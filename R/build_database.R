## ============================================================================
## build_database.R
##
## ONE-TIME setup script. Builds the local NHANES database by fetching
## variables from CDC via nhanesA, using variable_lookup.R (backed by the
## Duke University NHANES harmonization crosswalk) to resolve table names
## automatically for all standard variables.
##
## PFAS chemicals are the only exception — they require custom handling for
## isomer summation (2013+) and per-cycle weight variables that go beyond
## what the Duke lookup encodes.
##
## Usage:
##   source("R/build_database.R")
##   build_nhanes_database()
##
## Estimated runtime: 15-30 minutes.
## ============================================================================

library(here)
library(nhanesA)
library(dplyr)

source(here("R", "variable_lookup.R"))
source(here("R", "derived_covariates.R"))
source(here("R", "derived_outcomes_registry.R"))

## Analysis cycles — includes 2001-2002 even though PFAS wasn't measured
## that cycle. PFAS columns will be NA for 2001-2002 participants and they
## will be excluded automatically by the implicit not_na exposure constraint
## in any PFAS analysis. Non-PFAS analyses can use 2001-2002 freely.
ANALYSIS_CYCLES <- c("1999-2000", "2001-2002", "2003-2004", "2005-2006",
                      "2007-2008", "2009-2010", "2011-2012", "2013-2014",
                      "2015-2016", "2017-2018")

## ============================================================================
## HELPER: fetch and stack a single standard variable across cycles
## using the Duke lookup for table name resolution
## ============================================================================

fetch_standard <- function(variable, cycles = ANALYSIS_CYCLES,
                            rename_to = NULL) {
  frames <- lapply(cycles, function(cyc) {
    fetch_variable_by_lookup(variable, cyc)
  })
  frames <- Filter(Negate(is.null), frames)
  if (length(frames) == 0) {
    warning(sprintf("'%s' not found in any requested cycle", variable))
    return(NULL)
  }
  result <- bind_rows(frames)
  if (!is.null(rename_to) && variable %in% names(result)) {
    names(result)[names(result) == variable] <- rename_to
  }
  result
}

## All NHANES continuous cycles — used for the demographic roster which
## must include every SEQN ever measured. This is fixed and complete;
## ANALYSIS_CYCLES controls which cycles other variables are fetched for.
ALL_NHANES_CYCLES <- c(
  "1999-2000", "2001-2002", "2003-2004", "2005-2006", "2007-2008",
  "2009-2010", "2011-2012", "2013-2014", "2015-2016", "2017-2018"
)

## ============================================================================
## STEP 1: DEMOGRAPHICS + DESIGN VARIABLES
## Always fetches ALL cycles to build the complete SEQN roster.
## Uses Duke lookup (now includes 2001-2002 after CSV rebuild).
## ============================================================================

fetch_demographics <- function(cycles = ALL_NHANES_CYCLES) {
  cat("  Demographics (all cycles — building complete SEQN roster)...\n")
  demo_vars <- list(
    RIDAGEYR = "age",
    RIAGENDR = "sex",
    RIDRETH1 = "race",
    DMDEDUC2 = "education",
    INDFMPIR = "income_ratio",
    SDMVSTRA = "sdmvstra",
    SDMVPSU  = "sdmvpsu",
    WTINT2YR = "wtint2yr",
    WTMEC2YR = "wtmec2yr",
    ## 4-year weights exist only for 1999-2000 and 2001-2002; NA for all later
    ## cycles. Used for CDC-correct pooling when both early cycles are combined.
    WTINT4YR = "wtint4yr",
    WTMEC4YR = "wtmec4yr",
    ## Dietary recall weights.
    ## WTDRD1 / WTDR4YR: day-1 recall — all cycles; WTDR4YR only 1999/2001.
    ## WTDRD2: day-2 recall — 2003+ only (1999/2001 have no day-2 weight).
    WTDRD1   = "wtdrd1",
    WTDR4YR  = "wtdr4yr",
    WTDRD2   = "wtdrd2"
  )

  frames <- lapply(cycles, function(cyc) {
    tbl <- get_table_for_variable("RIDAGEYR", cyc)
    if (is.na(tbl)) {
      warning(sprintf("Duke lookup returned NA for DEMO in cycle %s — skipping", cyc))
      return(NULL)
    }

    raw <- tryCatch(nhanesA::nhanes(tbl),
                    error = function(e) { warning(conditionMessage(e)); NULL })
    if (is.null(raw)) return(NULL)

    year_num <- as.integer(sub("-.*", "", cyc))
    out <- data.frame(seqn = raw$SEQN, year = year_num,
                       stringsAsFactors = FALSE)

    for (var in names(demo_vars)) {
      canonical <- demo_vars[[var]]
      col <- raw[[var]]
      if (is.null(col)) {
        out[[canonical]] <- NA_real_
      } else if (is.factor(col) || inherits(col, "haven_labelled") ||
                 inherits(col, "labelled")) {
        out[[canonical]] <- suppressWarnings(as.numeric(unclass(col)))
      } else {
        out[[canonical]] <- suppressWarnings(as.numeric(col))
      }
    }
    cat(sprintf("    %s (%s): %d participants\n", cyc, tbl, nrow(out)))
    out
  })
  bind_rows(Filter(Negate(is.null), frames))
}

## ============================================================================
## STEP 2: BODY MEASURES
## ============================================================================

fetch_bmx <- function(cycles = ANALYSIS_CYCLES) {
  cat("  BMI...\n")
  fetch_standard("BMXBMI", cycles, rename_to = "bmi")
}

## ============================================================================
## STEP 3: SMOKING
## SMQ020 = ever smoked 100 cigarettes (1=yes, 2=no)
## SMQ040 = do you now smoke (1=every day, 2=some days, 3=not at all)
## Three-level coding: never/former/current handled in derived_covariates
## ============================================================================

fetch_smq <- function(cycles = ANALYSIS_CYCLES) {
  cat("  Smoking (SMQ020, SMQ040)...\n")
  ## SMQ variables are stable across all cycles with no name changes, so
  ## they don't appear in the Duke inconsistencies file. Use direct suffix
  ## pattern: SMQ (1999-2000), SMQ_B (2001-2002), SMQ_C (2003-2004) etc.
  ## Confirmed via nhanesA live testing.
  cycle_suffix <- c(
    "1999-2000"="", "2001-2002"="_B", "2003-2004"="_C", "2005-2006"="_D",
    "2007-2008"="_E", "2009-2010"="_F", "2011-2012"="_G", "2013-2014"="_H",
    "2015-2016"="_I", "2017-2018"="_J"
  )
  frames <- lapply(cycles, function(cyc) {
    tbl <- paste0("SMQ", cycle_suffix[cyc])
    raw <- tryCatch(nhanesA::nhanes(tbl),
                    error = function(e) { warning(conditionMessage(e)); NULL })
    if (is.null(raw)) return(NULL)
    year_num <- as.integer(sub("-.*", "", cyc))
    out <- data.frame(seqn = raw$SEQN, year = year_num)
    for (var in c("SMQ020", "SMQ040")) {
      col <- raw[[var]]
      out[[tolower(var)]] <- if (!is.null(col)) {
        suppressWarnings(as.numeric(unclass(col)))
      } else NA_real_
    }
    out
  })
  bind_rows(Filter(Negate(is.null), frames))
}

## ============================================================================
## STEP 4: DIABETES QUESTIONNAIRE
## DIQ010 = doctor told you have diabetes
## ============================================================================

fetch_diq <- function(cycles = ANALYSIS_CYCLES) {
  cat("  Diabetes questionnaire (DIQ010)...\n")
  fetch_standard("DIQ010", cycles, rename_to = "diabetes_sr")
}

## ============================================================================
## STEP 5: CVD CONDITIONS (MCQ160B-F)
## All five items in the same MCQ table per cycle
## ============================================================================

fetch_mcq <- function(cycles = ANALYSIS_CYCLES) {
  cat("  CVD conditions (MCQ160B-F)...\n")
  cvd_vars <- c("MCQ160B", "MCQ160C", "MCQ160D", "MCQ160E", "MCQ160F")

  frames <- lapply(cycles, function(cyc) {
    tbl <- get_table_for_variable("MCQ160B", cyc)
    if (is.na(tbl)) return(NULL)
    raw <- tryCatch(nhanesA::nhanes(tbl),
                    error = function(e) { warning(conditionMessage(e)); NULL })
    if (is.null(raw)) return(NULL)

    year_num <- as.integer(sub("-.*", "", cyc))
    out <- data.frame(seqn = raw$SEQN, year = year_num)
    for (var in cvd_vars) {
      col <- raw[[var]]
      out[[tolower(var)]] <- if (!is.null(col)) {
        suppressWarnings(as.numeric(unclass(col)))
      } else NA_real_
    }
    out
  })
  bind_rows(Filter(Negate(is.null), frames))
}

## ============================================================================
## STEP 6: CVD MEDICATION FLAG (BPQ100D)
## ============================================================================

fetch_bpq <- function(cycles = ANALYSIS_CYCLES) {
  cat("  CVD medication (BPQ100D)...\n")
  fetch_standard("BPQ100D", cycles, rename_to = "oncvdmed_raw")
}

## ============================================================================
## STEP 7: LIPID OUTCOMES
## LDL (LBDLDL), Total Cholesterol (LBXTC), HDL (LBDHDD)
##
## Table names resolved via Duke lookup — covers all naming changes:
##   LDL:   LAB13AM (1999), L13AM_C (2003), TRIGLY_D+ (2005+)
##   TCHOL: LAB13   (1999), L13_C   (2003), TCHOL_D+  (2005+)
##   HDL:   LAB13   (1999), L13_C   (2003), HDL_D+    (2005+)
##
## Fasting weight (WTSAF2YR/WTSAF4YR) is fetched from the LDL table since
## it lives there per NHANES lab file documentation (confirmed via live test)
## ============================================================================

fetch_lipids <- function(cycles = ANALYSIS_CYCLES) {
  cat("  Lipid outcomes (LDL, total cholesterol, HDL) + fasting weight...\n")

  ## Per-cycle HDL variable name — confirmed via Duke Response sheet:
  ## 1999-2000: LBDHDL in Lab13     (harmonized to LBDHDD by Duke)
  ## 2003-2004: LBXHDD in L13_C    (harmonized to LBDHDD by Duke)
  ## 2005+:     LBDHDD in HDL_D+   (no change)
  hdl_varname <- list(
    "1999-2000" = "LBDHDL",
    "2003-2004" = "LBXHDD"
  )

  frames <- lapply(cycles, function(cyc) {
    ldl_tbl   <- get_table_for_variable("LBDLDL",  cyc)
    tchol_tbl <- get_table_for_variable("LBXTC",   cyc)
    hdl_tbl   <- get_table_for_variable("LBDHDD",  cyc)

    year_num <- as.integer(sub("-.*", "", cyc))

    fetch_one <- function(tbl, var, canonical) {
      if (is.na(tbl)) return(NULL)
      raw <- tryCatch(nhanesA::nhanes(tbl),
                      error = function(e) { warning(conditionMessage(e)); NULL })
      if (is.null(raw)) return(NULL)
      out <- data.frame(seqn = raw$SEQN, year = year_num)
      if (var %in% names(raw)) {
        out[[canonical]] <- suppressWarnings(as.numeric(unclass(raw[[var]])))
      }
      if (var == "LBDLDL") {
        ## Store 2yr and 4yr fasting weights as separate columns so the
        ## pooling logic can apply the correct CDC scaling per cycle.
        ## WTSAF4YR exists in 1999-2000 and 2001-2002 DEMO/LDL files.
        ## WTSAF2YR exists in 2001-2002 and all later cycles.
        ## app.R coalesces these into wtsaf (preferring 4yr for early cycles).
        if ("WTSAF4YR" %in% names(raw))
          out$wtsaf4yr <- suppressWarnings(as.numeric(raw[["WTSAF4YR"]]))
        if ("WTSAF2YR" %in% names(raw))
          out$wtsaf2yr <- suppressWarnings(as.numeric(raw[["WTSAF2YR"]]))
      }
      out
    }

    ## HDL uses cycle-specific raw variable name for early cycles
    hdl_raw_var <- hdl_varname[[cyc]] %||% "LBDHDD"

    ldl   <- fetch_one(ldl_tbl,   "LBDLDL",    "lbdldl")
    tchol <- fetch_one(tchol_tbl, "LBXTC",     "lbxtc")
    hdl   <- fetch_one(hdl_tbl,   hdl_raw_var, "lbdhdd")

    Reduce(function(a, b) left_join(a, b, by = c("seqn", "year")),
           Filter(Negate(is.null), list(ldl, tchol, hdl)))
  })

  bind_rows(Filter(Negate(is.null), frames))
}

## ============================================================================
## STEP 8: PFAS CHEMICALS
## Custom fetch — Duke lookup doesn't encode:
##   (a) isomer summation for PFOA/PFOS 2013+
##   (b) per-cycle subsample weight variable names
##   (c) the SSPFAS_H vs PFAS_H split for 2013-2014
##
## All table names and weight variables confirmed via Duke Chemicals sheet
## and live nhanesA testing (2026-06-30).
## ============================================================================

fetch_pfas <- function(cycles = ANALYSIS_CYCLES) {
  cat("  PFAS chemicals (PFOA, PFOS, PFHxS, PFNA)...\n")

  pfas_map <- list(
    "1999-2000" = list(
      table = "SSPFC_A",
      pfoa = "SPFOA", pfos = "SPFOS", pfhxs = "SPFHS", pfna = "SPFNA",
      weight_var = "WTMEC2YR", wt_table = "DEMO", isomer = FALSE
    ),
    ## 2001-2002: PFAS not measured — cycle included for non-PFAS analyses.
    ## Returning NULL here means fetch_pfas() produces no rows for this
    ## cycle, so left_join in main build leaves PFAS columns as NA for
    ## 2001-2002 participants (correct behavior).
    "2001-2002" = NULL,
    "2003-2004" = list(
      table = "L24PFC_C",
      pfoa = "LBXPFOA", pfos = "LBXPFOS", pfhxs = "LBXPFHS", pfna = "LBXPFNA",
      weight_var = "WTSA2YR", wt_table = NULL, isomer = FALSE
    ),
    "2005-2006" = list(
      table = "PFC_D",
      pfoa = "LBXPFOA", pfos = "LBXPFOS", pfhxs = "LBXPFHS", pfna = "LBXPFNA",
      weight_var = "WTSA2YR", wt_table = NULL, isomer = FALSE
    ),
    "2007-2008" = list(
      table = "PFC_E",
      pfoa = "LBXPFOA", pfos = "LBXPFOS", pfhxs = "LBXPFHS", pfna = "LBXPFNA",
      weight_var = "WTSC2YR", wt_table = NULL, isomer = FALSE
    ),
    "2009-2010" = list(
      table = "PFC_F",
      pfoa = "LBXPFOA", pfos = "LBXPFOS", pfhxs = "LBXPFHS", pfna = "LBXPFNA",
      weight_var = "WTSC2YR", wt_table = NULL, isomer = FALSE
    ),
    "2011-2012" = list(
      table = "PFC_G",
      pfoa = "LBXPFOA", pfos = "LBXPFOS", pfhxs = "LBXPFHS", pfna = "LBXPFNA",
      weight_var = "WTSA2YR", wt_table = NULL, isomer = FALSE
    ),
    ## 2013-2014: PFOA/PFOS isomers in SSPFAS_H; PFHxS/PFNA in PFAS_H
    ## Weight: WTSB2YR — confirmed via Duke Chemicals sheet
    "2013-2014" = list(
      table = "SSPFAS_H", table_pfhxs_na = "PFAS_H",
      pfoa = c("SSNPFOA","SSBPFOA"), pfos = c("SSNPFOS","SSMPFOS"),
      pfhxs = "LBXPFHS", pfna = "LBXPFNA",
      ## PFOA/PFOS from SSPFAS_H use surplus specimen weight WTSSBH2Y
      ## PFHxS/PFNA from PFAS_H use standard panel weight WTSB2YR
      ## Storing the PFOA/PFOS weight (WTSSBH2Y) as the primary pfas_weight
      ## since PFOA/PFOS are the primary exposures. WTSB2YR stored separately
      ## as pfas_weight_b for PFHxS/PFNA analyses in this cycle.
      weight_var = "WTSSBH2Y", wt_table = "SSPFAS_H",
      weight_var_b = "WTSB2YR", wt_table_b = "PFAS_H",
      isomer = TRUE
    ),
    "2015-2016" = list(
      table = "PFAS_I",
      pfoa = c("LBXNFOA","LBXBFOA"), pfos = c("LBXNFOS","LBXMFOS"),
      pfhxs = "LBXPFHS", pfna = "LBXPFNA",
      weight_var = "WTSB2YR", wt_table = NULL, isomer = TRUE
    ),
    "2017-2018" = list(
      table = "PFAS_J",
      pfoa = c("LBXNFOA","LBXBFOA"), pfos = c("LBXNFOS","LBXMFOS"),
      pfhxs = "LBXPFHS", pfna = "LBXPFNA",
      weight_var = "WTSB2YR", wt_table = NULL, isomer = TRUE
    )
  )

  sum_isomers <- function(raw, vars) {
    v1 <- suppressWarnings(as.numeric(unclass(raw[[vars[1]]])))
    v2 <- suppressWarnings(as.numeric(unclass(raw[[vars[2]]])))
    mapply(function(a, b) {
      if (is.na(a) && is.na(b)) NA_real_ else sum(c(a,b), na.rm=TRUE)
    }, v1, v2)
  }

  get_single <- function(raw, var) {
    if (is.null(var) || is.na(var) || !(var %in% names(raw)))
      return(rep(NA_real_, nrow(raw)))
    suppressWarnings(as.numeric(unclass(raw[[var]])))
  }

  frames <- lapply(cycles, function(cyc) {
    m <- pfas_map[[cyc]]
    if (is.null(m)) return(NULL)

    cat(sprintf("    %s (%s)... ", cyc, m$table))
    raw <- tryCatch(nhanesA::nhanes(m$table),
                    error = function(e) { warning(conditionMessage(e)); NULL })
    if (is.null(raw)) { cat("FAILED\n"); return(NULL) }

    year_num <- as.integer(sub("-.*", "", cyc))
    out <- data.frame(seqn = raw$SEQN, year = year_num)

    out$pfoa <- if (m$isomer) sum_isomers(raw, m$pfoa) else get_single(raw, m$pfoa)
    out$pfos <- if (m$isomer) sum_isomers(raw, m$pfos) else get_single(raw, m$pfos)

    ## PFHxS/PFNA — separate table for 2013-2014, different N than PFOA table
    ## Must join by seqn, not assign directly
    if (!is.null(m$table_pfhxs_na)) {
      src <- tryCatch(nhanesA::nhanes(m$table_pfhxs_na),
                      error = function(e) { warning(conditionMessage(e)); NULL })
      if (!is.null(src)) {
        pfhxs_vals <- get_single(src, m$pfhxs)
        pfna_vals  <- get_single(src, m$pfna)
        src_df <- data.frame(
          seqn  = src$SEQN,
          pfhxs = pfhxs_vals,
          pfna  = pfna_vals
        )
        out <- left_join(out, src_df, by = "seqn")
      } else {
        out$pfhxs <- NA_real_
        out$pfna  <- NA_real_
      }
    } else {
      out$pfhxs <- get_single(raw, m$pfhxs)
      out$pfna  <- get_single(raw, m$pfna)
    }

    ## Weight
    wt_src <- if (!is.null(m$wt_table) && m$wt_table != m$table) {
      nhanesA::nhanes(m$wt_table)
    } else raw
    if (!is.null(wt_src) && m$weight_var %in% names(wt_src)) {
      if (!is.null(m$wt_table) && m$wt_table != m$table) {
        wt_df <- data.frame(
          seqn = wt_src$SEQN,
          pfas_weight = suppressWarnings(as.numeric(unclass(wt_src[[m$weight_var]])))
        )
        out <- left_join(out, wt_df, by = "seqn")
      } else {
        out$pfas_weight <- suppressWarnings(as.numeric(unclass(wt_src[[m$weight_var]])))
      }
    } else {
      warning(sprintf("Weight '%s' not found for %s", m$weight_var, cyc))
      out$pfas_weight <- NA_real_
    }

    ## Secondary weight for 2013-2014: PFHxS/PFNA use WTSB2YR from PFAS_H
    ## while PFOA/PFOS use WTSSBH2Y from SSPFAS_H (surplus specimen)
    if (!is.null(m$weight_var_b) && !is.null(m$wt_table_b)) {
      wt_src_b <- nhanesA::nhanes(m$wt_table_b)
      if (!is.null(wt_src_b) && m$weight_var_b %in% names(wt_src_b)) {
        wt_df_b <- data.frame(
          seqn = wt_src_b$SEQN,
          pfas_weight_b = suppressWarnings(as.numeric(unclass(wt_src_b[[m$weight_var_b]])))
        )
        out <- left_join(out, wt_df_b, by = "seqn")
      } else {
        out$pfas_weight_b <- NA_real_
      }
    }

    cat(sprintf("%d rows (PFOA non-NA: %d)\n", nrow(out), sum(!is.na(out$pfoa))))
    out
  })

  bind_rows(Filter(Negate(is.null), frames))
}

## ============================================================================
## MAIN BUILD FUNCTION
## ============================================================================

build_nhanes_database <- function(
    output_path = here("data", "nhanes_pool.rds"),
    cycles      = ALL_NHANES_CYCLES
) {
  t_start <- proc.time()
  cat("================================================================\n")
  cat(" NHANES Database Build — Demographic Roster\n")
  cat(sprintf(" Cycles: %s\n", paste(cycles, collapse=", ")))
  cat(sprintf(" Output: %s\n", output_path))
  cat(" Builds the complete SEQN roster from DEMO tables.\n")
  cat(" All other variables are added via the Dataset tab.\n")
  cat("================================================================\n\n")

  cat("Fetching demographics (all cycles)...\n")
  demo <- fetch_demographics(cycles)
  cat(sprintf("  -> %d rows across %d cycles\n\n",
              nrow(demo), length(unique(demo$year))))

  ## Built-in derived variables (sex_bin, smoking_status, edu_clean, egfr, etc.)
  ## are NOT auto-computed here. Stage them from the Dataset tab to add them;
  ## prerequisites will be auto-staged and the builder runs during the build step.
  combined <- demo
  cat(sprintf("Demographics: %d rows, %d columns\n\n", nrow(combined), ncol(combined)))

  ## Save
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  cat(sprintf("Saving to %s...\n", output_path))
  saveRDS(combined, output_path)

  size_mb <- file.size(output_path) / 1024^2
  elapsed  <- (proc.time() - t_start)["elapsed"]

  cat("\n================================================================\n")
  cat(sprintf(" Build complete in %.1f seconds\n", elapsed))
  cat(sprintf(" File size:        %.1f MB\n",      size_mb))
  cat(sprintf(" Rows:             %d\n",            nrow(combined)))
  cat(sprintf(" Columns:          %d\n",            ncol(combined)))
  cat(sprintf(" Cycles:           %s\n",            paste(sort(unique(combined$year)), collapse=", ")))
  cat("================================================================\n")
  cat("\nNext: use the Dataset tab to add variables (BMI, lipids, PFAS, etc.)\n")

  invisible(combined)
}

## ============================================================================
## LOADER
## ============================================================================

load_nhanes_database <- function(path = here("data", "nhanes_pool.rds")) {
  if (!file.exists(path)) {
    stop(paste(
      "Local NHANES database not found at:", path,
      "\nRun build_nhanes_database() to build it (takes ~20 minutes, runs once)."
    ))
  }
  cat("Loading local NHANES database... ")
  data <- readRDS(path)

  ## Strip jackknife replicate weight columns if they snuck in from a previous
  ## build. Matches WTMREP01-52, WTIREP01-52, and their _pooled variants.
  ## We use TSL (sdmvstra/sdmvpsu) for variance — jackknife weights are never
  ## needed and bloat the database by ~104 columns per demographic variable.
  ## Jackknife replicates end in digits only (e.g. WTSPO01).
  ## Analytic subsample weights end in 2YR or 4YR (e.g. WTSPO4YR).
  ## Anchoring with [0-9]+$ keeps analytic weights like WTSPO4YR while
  ## stripping replicate columns like WTSPO01-WTSPO52.
  jk_cols <- grep(
    paste0("^wt[mi]rep[0-9]",   "|^wt[mi]rep[0-9].*_pooled$",
           "|^wtshm[0-9]+$",     "|^wtshm[0-9]+.*_pooled$",
           "|^wtsph[0-9]+$",     "|^wtspo[0-9]+$", "|^wtsau[0-9]+$", "|^wtsci[0-9]+$",
           "|^wtsba[0-9]+$", "|^wtspp[0-9]+$"),
    names(data), ignore.case = TRUE, value = TRUE)
  if (length(jk_cols) > 0) {
    data <- data[, !names(data) %in% jk_cols, drop = FALSE]
    cat(sprintf("(stripped %d jackknife weight columns) ", length(jk_cols)))
  }

  cat(sprintf("%d rows, %d columns\n", nrow(data), ncol(data)))
  invisible(data)
}
