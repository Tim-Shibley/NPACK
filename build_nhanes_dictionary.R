## build_nhanes_dictionary.R
## =========================
## Builds a comprehensive NHANES variable dictionary by querying the CDC
## via the nhanesA package, then merges with the existing Duke CSV.
##
## Usage:
##   Rscript build_nhanes_dictionary.R
##
## Runtime: ~2-4 hours. Saves progress after each data group/cycle so
##          it can be safely interrupted and resumed.
##
## Output: nhanes_full_dictionary.csv — drop-in replacement for
##         duke_variable_lookup.csv

library(nhanesA)
library(here)

DUKE_CSV      <- here("data", "duke_variable_lookup.csv")
OUTPUT_FILE   <- here("data", "nhanes_full_dictionary.csv")
PROGRESS_FILE <- here("data", "nhanes_scrape_progress.rds")

CYCLES <- c("1999-2000","2001-2002","2003-2004","2005-2006","2007-2008",
            "2009-2010","2011-2012","2013-2014","2015-2016","2017-2018")

DATA_GROUPS <- c("DEMO","DIETARY","EXAM","LAB","Q")

cat("NHANES Variable Dictionary Builder\n")
cat(strrep("=", 50), "\n")
cat("Output:", OUTPUT_FILE, "\n\n")

## ── Load progress ────────────────────────────────────────────────────────────
if (file.exists(PROGRESS_FILE)) {
  progress <- readRDS(PROGRESS_FILE)
  cat(sprintf("Resuming from progress file (%d rows cached)\n",
              nrow(progress$rows)))
} else {
  progress <- list(
    completed = character(0),
    rows      = data.frame(
      variable=character(), cycle=character(), table=character(),
      raw_name=character(), weight=character(), description=character(),
      category=character(), in_dataset=character(), units=character(),
      chemical_family=character(), stringsAsFactors=FALSE
    )
  )
}

## ── Scrape loop ───────────────────────────────────────────────────────────────
total <- length(DATA_GROUPS) * length(CYCLES)
done  <- 0

for (group in DATA_GROUPS) {
  for (cycle in CYCLES) {
    done     <- done + 1
    task_key <- paste0(group, "_", cycle)

    if (task_key %in% progress$completed) {
      cat(sprintf("[%d/%d] Skip %s %s (cached)\n", done, total, group, cycle))
      next
    }

    cat(sprintf("[%d/%d] Fetching %s %s... ", done, total, group, cycle),
        file=stderr())

    rows <- tryCatch({
      ## nhanesCodebook returns all variables for a given data group + cycle
      ## nhanesSearchVarName searches by name; nhanesTables lists all tables
      tables <- nhanesTables(data_group=group, year=as.integer(sub("-.*","",cycle)))

      if (is.null(tables) || nrow(tables) == 0) {
        message("no tables")
        data.frame()
      } else {
        ## For each table, get the variable list
        all_var_rows <- lapply(seq_len(nrow(tables)), function(ti) {
          tbl_name <- tables$Data.File.Name[ti]
          tbl_desc <- tables$Data.File.Description[ti]

          vars <- tryCatch(
            nhanesTableVars(data_group=group, nh_table=tbl_name, details=TRUE),
            error=function(e) NULL
          )
          if (is.null(vars) || nrow(vars) == 0) return(NULL)

          Sys.sleep(0.3)  ## be polite to CDC servers

          data.frame(
            variable    = toupper(vars$Variable.Name),
            cycle       = cycle,
            table       = tbl_name,
            raw_name    = "",
            weight      = "",
            description = vars$Variable.Description,
            category    = tbl_desc,
            in_dataset  = group,
            units       = "",
            chemical_family = "",
            stringsAsFactors = FALSE
          )
        })
        do.call(rbind, Filter(Negate(is.null), all_var_rows))
      }
    }, error = function(e) {
      message(sprintf("ERROR: %s", conditionMessage(e)))
      data.frame()
    })

    n_rows <- if (is.data.frame(rows)) nrow(rows) else 0
    message(sprintf("%d variables", n_rows))

    if (n_rows > 0) {
      progress$rows <- rbind(progress$rows, rows)
    }
    progress$completed <- c(progress$completed, task_key)
    saveRDS(progress, PROGRESS_FILE)

    Sys.sleep(0.5)
  }
}

## ── Verify 1999-2000 coverage ─────────────────────────────────────────────────
## 1999-2000 tables have no letter suffix (SMQ not SMQ_A) which can cause
## the scrape to miss them. Check and re-fetch any group where 1999-2000
## has far fewer variables than other cycles.

cat("\nVerifying 1999-2000 coverage...\n")
scraped <- progress$rows

for (group in DATA_GROUPS) {
  n_1999 <- nrow(scraped[scraped$in_dataset == group & scraped$cycle == "1999-2000", ])
  n_other <- median(sapply(CYCLES[-1], function(cy)
    nrow(scraped[scraped$in_dataset == group & scraped$cycle == cy, ])))

  if (n_1999 < n_other * 0.5) {
    cat(sprintf("  %s 1999-2000 has only %d vars vs median %d — re-fetching...\n",
                group, n_1999, round(n_other)))

    tables <- tryCatch(
      nhanesTables(data_group=group, year=1999),
      error=function(e) NULL
    )
    if (is.null(tables) || nrow(tables) == 0) next

    new_rows <- lapply(seq_len(nrow(tables)), function(ti) {
      tbl_name <- tables$Data.File.Name[ti]
      tbl_desc <- tables$Data.File.Description[ti]
      vars <- tryCatch(
        nhanesTableVars(data_group=group, nh_table=tbl_name, details=TRUE),
        error=function(e) NULL
      )
      if (is.null(vars) || nrow(vars) == 0) return(NULL)
      Sys.sleep(0.3)
      data.frame(
        variable=toupper(vars$Variable.Name), cycle="1999-2000",
        table=tbl_name, raw_name="", weight="",
        description=vars$Variable.Description,
        category=tbl_desc, in_dataset=group,
        units="", chemical_family="", stringsAsFactors=FALSE
      )
    })
    new_rows <- do.call(rbind, Filter(Negate(is.null), new_rows))
    if (!is.null(new_rows) && nrow(new_rows) > 0) {
      ## Remove old 1999-2000 entries for this group and add fresh ones
      scraped <- scraped[!(scraped$in_dataset == group & scraped$cycle == "1999-2000"), ]
      scraped <- rbind(scraped, new_rows)
      cat(sprintf("    Re-fetched %d variables\n", nrow(new_rows)))
    }
  } else {
    cat(sprintf("  %s 1999-2000: %d vars OK\n", group, n_1999))
  }
}
progress$rows <- scraped
cat("\nMerging with Duke CSV...\n")

scraped <- progress$rows

if (file.exists(DUKE_CSV)) {
  duke <- read.csv(DUKE_CSV, stringsAsFactors=FALSE)
  cat(sprintf("  Duke CSV: %d rows\n", nrow(duke)))

  ## Duke takes precedence — has weight/chemical_family info
  ## Only add scraped rows not already covered by Duke
  duke_keys    <- paste(duke$variable, duke$cycle)
  scraped_keys <- paste(scraped$variable, scraped$cycle)
  new_only     <- scraped[!scraped_keys %in% duke_keys, ]
  cat(sprintf("  New from scrape: %d rows\n", nrow(new_only)))

  combined <- rbind(duke, new_only)
} else {
  cat("  Duke CSV not found, using scraped data only\n")
  combined <- scraped
}

## Sort, deduplicate
combined <- combined[order(combined$variable, combined$cycle), ]
combined <- combined[!duplicated(paste(combined$variable, combined$cycle)), ]
cat(sprintf("  Combined total: %d rows\n", nrow(combined)))

## Save
write.csv(combined, OUTPUT_FILE, row.names=FALSE)
cat(sprintf("\nSaved to %s\n", OUTPUT_FILE))
cat("Copy to nhanes-gui/data/duke_variable_lookup.csv to use.\n")

## Clean up progress
if (file.exists(PROGRESS_FILE)) file.remove(PROGRESS_FILE)
cat("Done.\n")
