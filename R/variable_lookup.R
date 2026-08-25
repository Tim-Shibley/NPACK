## ============================================================================
## variable_lookup.R
##
## Lookup engine built from the Duke University NHANES harmonization
## resource (nhanes_inconsistencies_documentation.xlsx + dictionary_nhanes.csv
## from figshare.com/articles/dataset/NHANES_1988-2018/21743372).
##
## Ground truth: the Duke file_name column contains exact nhanesA table
## names, confirmed via live testing against CDC servers (2026-06-30).
##
## This replaces the per-variable hardcoded table mappings that were
## previously scattered through variable_registry.R and build_database.R.
## PFAS chemicals (isomer summation, custom weight variables) are the one
## exception — they remain in build_database.R's pfas_map since the Duke
## file correctly shows LBXPFOA as only available 1999-2012 (pre-isomer)
## and doesn't encode the summation logic.
## ============================================================================

library(here)

## ---- Load the lookup table -----------------------------------------------

.duke_lookup      <- NULL  # module-level cache
.duke_lookup_mtime <- NULL  # modification time of cached file

load_duke_lookup <- function(path = here("data", "duke_variable_lookup.csv")) {
  if (!file.exists(path)) {
    stop(paste(
      "Duke variable lookup table not found at:", path,
      "\nThis file should be included with the package in data/.",
      "\nIf missing, regenerate it by running: source('scripts/build_duke_lookup.R')"
    ))
  }
  ## Reload if cache is empty or CSV has been modified since last load
  current_mtime <- file.mtime(path)
  if (!is.null(.duke_lookup) && !is.null(.duke_lookup_mtime) &&
      current_mtime <= .duke_lookup_mtime) {
    return(.duke_lookup)
  }
  tbl <- read.csv(path, stringsAsFactors = FALSE)
  .duke_lookup       <<- tbl
  .duke_lookup_mtime <<- current_mtime
  tbl
}


## ---- Core lookup functions -----------------------------------------------

#' Get the nhanesA table name for a variable in a specific cycle.
#'
#' @param variable NHANES variable name (uppercase, e.g. "LBDLDL")
#' @param cycle cycle ID string (e.g. "2007-2008")
#' @return table name string (e.g. "TRIGLY_E"), or NA if not available
#'
#' @examples
#' get_table_for_variable("LBDLDL", "2007-2008")  # -> "TRIGLY_E"
#' get_table_for_variable("RIDAGEYR", "1999-2000") # -> "DEMO"
#' get_table_for_variable("LBDLDL", "2001-2002")   # -> NA (not in target cycles)
get_table_for_variable <- function(variable, cycle) {
  lkp <- load_duke_lookup()
  result <- lkp[lkp$variable == toupper(variable) & lkp$cycle == cycle, ]
  if (nrow(result) == 0) return(NA_character_)
  result$table[1]
}

#' Get the raw variable name as it appears in the NHANES table for a
#' specific cycle. Returns the canonical name when no rename occurred.
get_raw_name_for_variable <- function(variable, cycle) {
  lkp <- load_duke_lookup()
  result <- lkp[lkp$variable == toupper(variable) & lkp$cycle == cycle, ]
  if (nrow(result) == 0) return(toupper(variable))
  raw <- result$raw_name[1]
  if (is.na(raw) || nchar(trimws(raw)) == 0) toupper(variable) else raw
}

#' Get the recommended analytic weight for a variable in a specific cycle,
#' as documented in the Duke NHANES Chemicals sheet. Returns empty string
#' if no weight is documented (use the caller's fallback logic).
get_weight_for_variable <- function(variable, cycle) {
  lkp <- load_duke_lookup()
  result <- lkp[lkp$variable == toupper(variable) & lkp$cycle == cycle, ]
  if (nrow(result) == 0 || !("weight" %in% names(result))) return("")
  wt <- result$weight[1]
  if (is.na(wt)) "" else trimws(wt)
}

#' Get the most common recommended weight across cycles for a variable.
#' Used to determine the weight category for an analysis pooling cycles.
get_predominant_weight <- function(variable, cycles) {
  weights <- sapply(cycles, function(cyc) get_weight_for_variable(variable, cyc))
  weights <- weights[nzchar(weights)]
  if (length(weights) == 0) return("")
  ## Return most frequent weight
  names(sort(table(weights), decreasing=TRUE))[1]
}

#' Get the category of a variable from the Duke lookup (e.g. "Demographics",
#' "Blood Pressure", "PFAS").  Returns NA_character_ when the variable is not
#' in the lookup.  Looks across all cycles and returns the most common value.
get_category_for_variable <- function(variable) {
  lkp <- load_duke_lookup()
  if (!"category" %in% names(lkp)) return(NA_character_)
  rows <- lkp[lkp$variable == toupper(variable), ]
  if (nrow(rows) == 0) return(NA_character_)
  cats <- rows$category[!is.na(rows$category) & nzchar(trimws(rows$category))]
  if (length(cats) == 0) return(NA_character_)
  names(sort(table(cats), decreasing = TRUE))[1]
}

#' Get all cycles a variable is available in (within the target cycle set).
#'
#' @param variable NHANES variable name
#' @return character vector of cycle strings
get_available_cycles <- function(variable) {
  lkp <- load_duke_lookup()
  sort(unique(lkp[lkp$variable == toupper(variable), "cycle"]))
}

#' Search for variables by description keyword.
#' Returns a data frame of matching variables with descriptions and categories.
#'
#' @param term search term (case-insensitive, partial match)
#' @param category optional filter e.g. "Cholesterol", "Medical Conditions"
#' @param cycles_required optional: only return variables available in ALL
#'        of these cycles (e.g. our 9-cycle analysis set)
search_variables <- function(term, category = NULL,
                              cycles_required = NULL) {
  lkp <- load_duke_lookup()

  ## Get unique variable metadata (one row per variable)
  meta <- unique(lkp[, c("variable", "description", "category",
                           "in_dataset", "units", "chemical_family")])

  ## Filter by term
  mask <- grepl(term, meta$description, ignore.case = TRUE) |
          grepl(term, meta$variable,    ignore.case = TRUE)
  meta <- meta[mask, ]

  ## Filter by category if specified
  if (!is.null(category)) {
    meta <- meta[grepl(category, meta$category, ignore.case = TRUE), ]
  }

  ## Filter by cycle availability if specified
  if (!is.null(cycles_required)) {
    available_in_all <- sapply(meta$variable, function(v) {
      avail <- get_available_cycles(v)
      all(cycles_required %in% avail)
    })
    meta <- meta[available_in_all, ]
  }

  meta[order(meta$category, meta$variable), ]
}

#' Fetch a single variable for a single cycle using the Duke lookup to
#' resolve the table name automatically.
#'
#' This is the general-purpose replacement for fetch_variable_for_cycle()
#' in data_loader.R — works for any of the 2,387 variables in the lookup,
#' without needing any entry in variable_registry.R.
#'
#' @param variable NHANES variable name (uppercase)
#' @param cycle cycle string e.g. "2007-2008"
#' @return data frame with columns (seqn, year, <variable>) or NULL
fetch_variable_by_lookup <- function(variable, cycle) {

  ## Special case: PFOA and PFOS changed to isomer measurements in 2013+
  ## PFOA = LBXNFOA (n-PFOA) + LBXBFOA (branched PFOA)
  ## PFOS = LBXNFOS (n-PFOS) + LBXMFOS (methyl/branched PFOS)
  ## These must be summed to get the total equivalent used in earlier cycles.
  cycle_year <- as.integer(sub("-.*", "", cycle))
  pfoa_isomer_cycles <- c(2013, 2015, 2017)
  pfos_isomer_cycles <- c(2013, 2015, 2017)

  if (toupper(variable) == "LBXPFOA" && cycle_year %in% pfoa_isomer_cycles) {
    message(sprintf(
      "  Note: LBXPFOA not measured as single analyte in %s — summing isomers LBXNFOA + LBXBFOA",
      cycle))
    frm_n <- fetch_variable_by_lookup("LBXNFOA", cycle)
    frm_b <- fetch_variable_by_lookup("LBXBFOA", cycle)
    if (is.null(frm_n)) return(NULL)
    out <- frm_n
    names(out)[names(out) == "LBXNFOA"] <- "LBXPFOA"
    if (!is.null(frm_b) && "LBXBFOA" %in% names(frm_b)) {
      b_vals <- frm_b$LBXBFOA[match(paste(frm_n$seqn, frm_n$year),
                                      paste(frm_b$seqn, frm_b$year))]
      both_na <- is.na(out$LBXPFOA) & is.na(b_vals)
      out$LBXPFOA <- ifelse(both_na, NA_real_,
                             rowSums(cbind(out$LBXPFOA, b_vals), na.rm=TRUE))
    }
    return(out)
  }

  if (toupper(variable) == "LBXPFOS" && cycle_year %in% pfos_isomer_cycles) {
    message(sprintf(
      "  Note: LBXPFOS not measured as single analyte in %s — summing isomers LBXNFOS + LBXMFOS",
      cycle))
    frm_n <- fetch_variable_by_lookup("LBXNFOS", cycle)
    frm_m <- fetch_variable_by_lookup("LBXMFOS", cycle)
    if (is.null(frm_n)) return(NULL)
    out <- frm_n
    names(out)[names(out) == "LBXNFOS"] <- "LBXPFOS"
    if (!is.null(frm_m) && "LBXMFOS" %in% names(frm_m)) {
      m_vals <- frm_m$LBXMFOS[match(paste(frm_n$seqn, frm_n$year),
                                      paste(frm_m$seqn, frm_m$year))]
      both_na <- is.na(out$LBXPFOS) & is.na(m_vals)
      out$LBXPFOS <- ifelse(both_na, NA_real_,
                             rowSums(cbind(out$LBXPFOS, m_vals), na.rm=TRUE))
    }
    return(out)
  }

  ## Standard lookup path
  table_name <- get_table_for_variable(variable, cycle)

  ## If Duke lookup misses, use nhanesSearchVarName as fallback.
  ## Variables with no inconsistencies are absent from the Duke file
  ## even though they exist in every cycle (e.g. LBXGH, SMQ020).
  ## nhanesSearchVarName returns ALL tables containing this variable
  ## across all cycles — we then match to the correct cycle via suffix.
  if (is.na(table_name)) {
    suffix_map <- c(
      "1999-2000"="",   "2001-2002"="_B", "2003-2004"="_C",
      "2005-2006"="_D", "2007-2008"="_E", "2009-2010"="_F",
      "2011-2012"="_G", "2013-2014"="_H", "2015-2016"="_I",
      "2017-2018"="_J"
    )
    suffix <- suffix_map[cycle]

    all_tables <- tryCatch(
      nhanesA::nhanesSearchVarName(toupper(variable)),
      error = function(e) character(0)
    )

    if (length(all_tables) > 0) {
      ## Match table whose suffix corresponds to this cycle
      ## e.g. cycle "2001-2002" suffix "_B" matches "L10_B" or "GHB_B"
      if (nchar(suffix) == 0) {
        ## 1999-2000: tables with no suffix (e.g. "LAB10", "GHB")
        matched <- all_tables[!grepl("_[A-Z]$|_[A-Z][0-9]$", all_tables)]
      } else {
        matched <- all_tables[endsWith(all_tables, suffix)]
      }
      if (length(matched) > 0) {
        table_name <- matched[1]
      }
    }

    if (is.na(table_name)) return(NULL)
  }

  raw <- tryCatch(
    nhanesA::nhanes(table_name),
    error = function(e) {
      warning(sprintf("Could not fetch table '%s' for %s (%s): %s",
                       table_name, variable, cycle, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(raw)) return(NULL)

  ## Standardise SEQN to lowercase seqn
  ## Use the raw variable name for this cycle (may differ from canonical
  ## due to codename changes documented in Duke inconsistencies file)
  raw_varname <- get_raw_name_for_variable(variable, cycle)

  seqn_col <- if ("SEQN" %in% names(raw)) "SEQN" else
              if ("seqn" %in% names(raw)) "seqn" else NULL
  if (is.null(seqn_col)) {
    warning(sprintf("Table '%s' has no SEQN column", table_name))
    return(NULL)
  }

  ## Try raw name first, then canonical, then uppercase canonical
  col_found <- NULL
  for (try_name in unique(c(raw_varname, toupper(variable), variable))) {
    if (try_name %in% names(raw)) { col_found <- try_name; break }
  }

  if (is.null(col_found)) {
    warning(sprintf(
      "Variable '%s' (raw: '%s') not found in table '%s' for cycle %s",
      variable, raw_varname, table_name, cycle))
    return(NULL)
  }

  year_num <- as.integer(sub("-.*", "", cycle))
  out <- data.frame(seqn = raw[[seqn_col]], year = year_num,
                     stringsAsFactors = FALSE)

  ## Coerce to numeric — handles factor/labelled types
  col <- raw[[col_found]]
  if (is.factor(col) || inherits(col, "haven_labelled") || inherits(col, "labelled")) {
    out[[variable]] <- suppressWarnings(as.numeric(unclass(col)))
  } else {
    out[[variable]] <- suppressWarnings(as.numeric(col))
  }

  ## Pull all weight columns present in this table — these are definitively
  ## the correct weights for this variable in this cycle since they come
  ## from the same XPT file. Store as attribute for weight map building.
  raw_names_lower <- tolower(names(raw))

  ## Jackknife replicate weights (WTMREP01-52, WTIREP01-52) are an alternative
  ## variance estimation approach — we use Taylor-series linearisation (TSL) via
  ## sdmvstra/sdmvpsu instead. Exclude them to prevent ~104 useless columns per
  ## variable fetched from the DEMO table (and their _pooled variants).
  ## Jackknife replicate patterns to exclude.
  ## CRITICAL: analytic subsample weights follow the pattern wts**2yr / wts**4yr
  ## (two letters then a year suffix). Jackknife replicates follow wts**NN where
  ## NN are digits only with NO trailing letters (e.g. WTSPO01-WTSPO52).
  ## Anchoring with [0-9]+$ ensures wtspo4yr is KEPT (has "yr" after the digit)
  ## while wtspo01-wtspo52 are excluded (digits at end of string).
  jk_pattern <- paste0(
    "^wt[mi]rep[0-9]",     ## WTMREP01-52, WTIREP01-52
    "|^wtshm[0-9]+$",       ## WTSHM##  (digits only at end)
    "|^wtsph[0-9]+$",       ## WTSPH##  (digits only at end — WTSPH4YR is kept)
    "|^wtspo[0-9]+$",        ## WTSPO##  (digits only at end — WTSPO4YR is kept)
    "|^wtsau[0-9]+$",        ## WTSAU##  (digits only at end — WTSAU4YR is kept)
    "|^wtsba[0-9]+$",        ## WTSBA##  (digits only at end — WTSBA4YR is kept)
    "|^wtsci[0-9]+$",        ## WTSCI##  (digits only at end — WTSCI4YR is kept)
    "|^wtspp[0-9]+$"        ## WTSPP##  (digits only at end — WTSPP4YR is kept)
    
  )
  wt_cols_in_table <- names(raw)[
    grepl("^wt[a-z0-9]", raw_names_lower) &
    !grepl(jk_pattern, raw_names_lower)
  ]

  ## Record which weight columns came from this table for this cycle
  ## Exclude wtmec2yr/wtint2yr — those are always present and are the
  ## default fallback, not a special subsample weight for this variable
  subsample_wts_found <- wt_cols_in_table[
    !tolower(wt_cols_in_table) %in% c("wtmec2yr","wtmec4yr","wtint2yr","wtint4yr")
  ]

  ## Store all weight columns in the frame; mark subsample ones as attribute
  for (wt_raw in wt_cols_in_table) {
    wt_db <- tolower(wt_raw)
    out[[wt_db]] <- suppressWarnings(as.numeric(raw[[wt_raw]]))
  }

  ## Dietary individual-food tables (e.g. DR1IFF_*, DR2IFF_*) have one row per
  ## food item per participant — SEQN is NOT unique. Aggregate by summing the
  ## nutrient variable across food items, and take the first value for weight
  ## columns (they are participant-level constants in those tables).
  if (anyDuplicated(paste(out$seqn, out$year))) {
    wt_db_cols <- tolower(wt_cols_in_table)
    out <- dplyr::group_by(out, seqn, year)
    out <- dplyr::summarise(
      out,
      dplyr::across(dplyr::all_of(variable),       ~ sum(.x, na.rm = TRUE)),
      dplyr::across(dplyr::any_of(wt_db_cols),     ~ dplyr::first(.x)),
      .groups = "drop"
    )
    out <- as.data.frame(out)
  }

  ## Normalise pre-press column name aliases used in NHANES 1999-2000 release.
  ## WTDRD1PP / WTDRD2PP are identical to WTDRD1 / WTDRD2 but were renamed
  ## after the initial data release; unify them so wt_map displays consistently.
  normalize_wt_names <- function(nms) {
    nms <- tolower(nms)
    nms[nms == "wtdrd1pp"] <- "wtdrd1"
    nms[nms == "wtdrd2pp"] <- "wtdrd2"
    nms
  }

  ## Attach metadata: which weights are subsample-specific for this variable/cycle
  attr(out, "subsample_weights") <- normalize_wt_names(subsample_wts_found)
  attr(out, "cycle_weight_map")  <- list(
    variable = tolower(variable),
    cycle    = cycle,
    weights  = if (length(subsample_wts_found) > 0)
                 normalize_wt_names(subsample_wts_found)
               else
                 "wtmec2yr"  ## default: MEC weight
  )

  out
}

#' Fetch a variable across multiple cycles, stacking into one data frame.
#' Cycles where the variable isn't available return no rows — the caller's
#' left_join will produce NA for participants in those cycles, which is the
#' correct behavior (participant exists in database, value simply wasn't
#' measured that cycle).
#'
#' @param variable NHANES variable name
#' @param cycles character vector of cycle strings
#' @param verbose whether to print per-cycle status
#' @return pooled data frame or NULL if unavailable in ALL cycles
fetch_variable_pooled <- function(variable, cycles, verbose = TRUE) {
  frames <- lapply(cycles, function(cyc) {
    result <- fetch_variable_by_lookup(variable, cyc)
    if (is.null(result) && verbose) {
      message(sprintf(
        "  Note: '%s' not available in %s — participants from this cycle will have NA",
        variable, cyc))
    }
    result
  })
  frames <- Filter(Negate(is.null), frames)
  if (length(frames) == 0) {
    if (verbose) warning(sprintf("'%s' not found in any requested cycle", variable))
    return(NULL)
  }

  ## Collect per-cycle weight maps before bind_rows drops attributes
  cycle_weight_maps <- lapply(frames, function(f) attr(f, "cycle_weight_map"))
  cycle_weight_maps <- Filter(Negate(is.null), cycle_weight_maps)

  combined <- dplyr::bind_rows(frames)

  ## Attach the consolidated weight map as attribute on the combined frame
  ## Structure: list of (variable, cycle, weights) — one entry per cycle
  attr(combined, "cycle_weight_maps") <- cycle_weight_maps

  combined
}

#' List all available variable categories (useful for Shiny UI dropdowns).
list_categories <- function() {
  lkp <- load_duke_lookup()
  sort(unique(lkp$category[lkp$category != ""]))
}

#' List all variables in a category.
list_variables_in_category <- function(category, cycles = NULL) {
  search_variables("", category = category, cycles_required = cycles)
}


## ============================================================================
## Weight group resolution
## ============================================================================

## Map a weight column name to its analytic group category.
## Groups (most to least restrictive): pfas > fasting > exam > interview.
weight_col_to_group <- function(wt_col) {
  wt <- tolower(trimws(wt_col))
  if (!nzchar(wt))                          return("exam")
  if (grepl("^wtsaf", wt))                  return("fasting")
  if (grepl("^wtss|^wts[abc]2yr", wt))      return("pfas")
  if (grepl("^wtmec", wt))                  return("exam")
  if (grepl("^wtint", wt))                  return("interview")
  "exam"
}

#' Determine the most restrictive analytic weight group for a set of NHANES variables.
#'
#' Uses the Duke lookup to find each variable's predominant weight column,
#' then applies the priority order pfas > fasting > exam > interview.
#'
#' @param variables NHANES variable names, e.g. c("LBXPFOA", "RIDAGEYR")
#' @param cycles cycle strings to consider; defaults to the full 10-cycle set
#' @return one of "pfas", "fasting", "exam", "interview"
resolve_weight_group <- function(variables, cycles = NULL) {
  if (is.null(cycles)) {
    cycles <- c("1999-2000", "2001-2002", "2003-2004", "2005-2006",
                "2007-2008", "2009-2010", "2011-2012", "2013-2014",
                "2015-2016", "2017-2018")
  }
  priority <- c(pfas = 4L, fasting = 3L, exam = 2L, interview = 1L)
  best <- "interview"
  for (v in variables) {
    wt  <- get_predominant_weight(v, cycles)
    grp <- weight_col_to_group(wt)
    if (priority[[grp]] > priority[[best]]) best <- grp
  }
  best
}
