## ============================================================================
## cli.R
##
## Main entry point for console use. run_analysis() loads the pre-built
## local database and runs the complete pipeline — population filter,
## survey design, regression, output formatting.
##
## The Shiny app will call the same function; the only difference is that
## Shiny collects arguments from UI inputs rather than from variables typed
## here. All column names follow the all-lowercase convention established
## in build_database.R.
## ============================================================================

library(here)
library(survey)
library(dplyr)

source(here("R", "build_database.R"))        # load_nhanes_database()
source(here("R", "derived_outcomes_registry.R"))
source(here("R", "derived_covariates.R"))
source(here("R", "population_filter.R"))
source(here("R", "variable_lookup.R"))
source(here("R", "weight_builder.R"))
source(here("R", "survey_design.R"))
source(here("R", "regression_engine.R"))
source(here("R", "output_formatters.R"))
source(here("R", "demographic_breakdown.R"))


## ============================================================================
## PRE-BUILT POPULATION DEFINITIONS
## All column references use lowercase (matching the database output).
## ============================================================================

population_definitions <- list(

  all_adults = list(
    label = "All adults (age >= 20)",
    constraints = list(
      make_constraint("age", ">=", 20)
    )
  ),

  cvd_medicated_fasting = list(
    label = "CVD-medicated fasting adults (age >= 20)",
    constraints = list(
      make_constraint("hascvd",   "==",  1),
      make_constraint("oncvdmed", "==",  1),
      make_constraint("lbdldl",   "not_na"),
      make_constraint("age",      ">=",  20),
      make_constraint("pfoa",     "not_na")
    )
  )
)


## ============================================================================
## COLUMN NAME MAP
## Maps user-facing canonical names to actual lowercase database column names.
## Users can refer to "PFOA" or "pfoa" interchangeably; this resolves both.
## ============================================================================

COLUMN_MAP <- c(
  ## Exposures
  "pfoa" = "pfoa", "PFOA" = "pfoa",
  "pfos" = "pfos", "PFOS" = "pfos",
  "pfhxs" = "pfhxs", "PFHxS" = "pfhxs",
  "pfna" = "pfna", "PFNA" = "pfna",

  ## Lipid outcomes
  "ldl" = "lbdldl", "LDL" = "lbdldl", "lbdldl" = "lbdldl",
  "non_hdl" = "non_hdl", "non_HDL" = "non_hdl", "NON_HDL" = "non_hdl",
  "remnant" = "remnant", "REMNANT" = "remnant",
  "hdl" = "lbdhdd", "HDL" = "lbdhdd", "lbdhdd" = "lbdhdd",
  "total_chol" = "lbxtc", "TOTAL_CHOL" = "lbxtc", "lbxtc" = "lbxtc",

  ## Covariates
  "age" = "age", "AGE" = "age",
  "sex_bin" = "sex_bin",
  "race_hispanic" = "race_hispanic",
  "race_black" = "race_black",
  "race_other" = "race_other",
  "bmi" = "bmi", "BMI" = "bmi",
  "education" = "education", "EDUCATION" = "education", "edu_clean" = "edu_clean",
  "income_ratio" = "income_ratio", "INCOME_RATIO" = "income_ratio",
  "diabetes_sr" = "diabetes_sr", "DIABETES" = "diabetes_sr",
  "smoking_status" = "smoking_status", "SMOKING_STATUS" = "smoking_status",
  "smq020" = "smq020", "smq040" = "smq040"
)

resolve_colname <- function(name) {
  resolved <- COLUMN_MAP[name]
  if (is.na(resolved)) name else resolved  # fall back to original if not mapped
}

resolve_colnames <- function(names) {
  vapply(names, resolve_colname, character(1), USE.NAMES = FALSE)
}


## ============================================================================
## WEIGHT SELECTION
## NHANES analytic guideline: use the weight for the most restrictive
## subsample involved in the analysis (lowest probability of selection).
## Each exposure-outcome pair independently determines its weight by comparing
## the weight maps of exposure, outcome, and all covariates across cycles.
##
## Priority (most to least restrictive):
##   WTSS* (1) > WTSA2YR (2) > WTSB2YR (3) > WTSC2YR (4)
##   > WTSAF2YR (5) > WTMEC2YR (6) > WTINT2YR (7)
##
## Weight maps are built at stage/build time from each variable's XPT file
## metadata and stored in attr(db, "variable_weight_map"). No variable type
## receives special preference — PFAS, lipids, and other exposures all follow
## the same per-cycle priority rules.
## ============================================================================

PFAS_VARS        <- c("pfoa","pfos","pfhxs","pfna")   ## used for log10 flag detection only
STANDARD_WEIGHTS <- c("wtint2yr","wtint4yr","wtmec2yr","wtmec4yr")

## Map from the database's canonical lowercase column names (built-in
## pseudonyms) to their NHANES source variable names for category lookup.
## Only covers the pseudonyms created in build_database.R's fetch_demographics().
.CANONICAL_TO_NHANES <- c(
  age          = "RIDAGEYR",
  sex          = "RIAGENDR",
  race         = "RIDRETH1",
  education    = "DMDEDUC2",
  income_ratio = "INDFMPIR"
)

## Columns that are never analytic variables — skip them in the check.
.NON_ANALYTIC_COLS <- c("year","sdmvstra","sdmvpsu",
                         "wtint2yr","wtmec2yr","pooled_weight")

## Resolve a single db column name to its NHANES source variable(s) for
## category lookup.  Returns character vector of uppercase NHANES names.
.to_nhanes_sources <- function(col) {
  v <- tolower(col)
  ## Built-in pseudonym (age, sex, race, etc.)
  mapped <- .CANONICAL_TO_NHANES[v]
  if (!is.na(mapped)) return(unname(mapped))
  ## Custom variable — collect source columns from both requires and or_groups,
  ## then recurse so nested custom variables are fully unwound.
  cv <- get_custom_variables()[[v]]
  if (!is.null(cv)) {
    srcs <- unique(c(
      tolower(cv$requires %||% character(0)),
      tolower(unlist(cv$or_groups %||% list()))
    ))
    srcs <- srcs[nzchar(srcs) & srcs != v]   ## avoid self-reference
    if (length(srcs) > 0)
      return(toupper(unique(unlist(lapply(srcs, .to_nhanes_sources)))))
  }
  ## Built-in derived outcomes (e.g. derived_outcomes_registry entries)
  srcs <- expand_to_sources(v)
  if (length(srcs) > 0 && !identical(srcs, v))
    return(toupper(unique(unlist(lapply(srcs, .to_nhanes_sources)))))
  ## Fall back to treating the column name itself as the NHANES variable name
  toupper(v)
}

## Return TRUE when every analytic variable in the study belongs to the
## "Demographics" category in the Duke lookup.  Non-analytic columns
## (year, sdmvstra, sdmvpsu, weight columns) are excluded from the check.
## Unknown variables (not in the Duke lookup) are treated as non-demographic
## so the safer WTMEC2YR remains the fallback.
all_demographic <- function(vars) {
  analytic <- tolower(vars)
  analytic <- analytic[!analytic %in% .NON_ANALYTIC_COLS]
  if (length(analytic) == 0) return(FALSE)
  all(vapply(analytic, function(col) {
    ## Shortcut: custom variable explicitly tagged "Demographics" by the user
    cv <- get_custom_variables()[[col]]
    if (!is.null(cv) && identical(cv$category, "Demographics")) return(TRUE)
    nhanes_vars <- .to_nhanes_sources(col)
    cats <- vapply(nhanes_vars, get_category_for_variable, character(1))
    ## All resolved sources must be Demographics; NA = unknown = not demographic
    length(cats) > 0 && all(!is.na(cats) & cats == "Demographics")
  }, logical(1)))
}

## Returns the priority of a weight column (lower = more restrictive).
## 4-year weights share the same priority as their 2-year counterparts;
## which one to use for a given row is decided by the CDC pooling formula
## (4yr for 1999/2001 when both early cycles are present, 2yr otherwise).
## Returns the priority of a weight column (lower = more restrictive).
## 4-year weights share the same priority as their 2-year counterparts.
## Priority: WTSS* (1) > WTSA (2) > WTSB (3) > WTSC (4)
##           > wts**[24]yr unknown (4) > WTSAF (5) > WTMEC (6) > WTINT (7)
## Any wts**2yr / wts**4yr not explicitly listed (e.g. wtspo4yr, wtssc2yr)
## is an unrecognised subsample weight — assigned priority 4 so it overrides
## both MEC and fasting weights.
get_weight_priority <- function(col) {
  col_l <- tolower(col)
  if (grepl("^wtss", col_l)) return(1L)
  switch(col_l,
    "wtsa2yr"  = 2L, "wtsa4yr"  = 2L,
    "wtsb2yr"  = 3L, "wtsb4yr"  = 3L,
    "wtsc2yr"  = 4L, "wtsc4yr"  = 4L,
    "wtsaf2yr" = 5L, "wtsaf4yr" = 5L, "wtsaf" = 5L,
    "wtmec2yr" = 6L, "wtmec4yr" = 6L,
    "wtint2yr" = 7L, "wtint4yr" = 7L,
    {
      if (grepl("^wts[a-z]{2}[24]yr$", col_l)) 4L else NA_integer_
    }
  )
}

## Named vector form for vectorised lookups (e.g. in app.R derived-var
## propagation). WTSS* and unknown wts**[24]yr weights are not listed here;
## callers should treat NA as priority 4 (overrides fasting and MEC).
.wt_priority_na_default <- 4L

WEIGHT_PRIORITY <- c(
  wtsa2yr  = 2L, wtsa4yr  = 2L,
  wtsb2yr  = 3L, wtsb4yr  = 3L,
  wtsc2yr  = 4L, wtsc4yr  = 4L,
  wtsaf2yr = 5L, wtsaf4yr = 5L, wtsaf = 5L,
  wtmec2yr = 6L, wtmec4yr = 6L,
  wtint2yr = 7L, wtint4yr = 7L
)


## ============================================================================
## resolve_population_def()
## Shared helper used by bkmr_engine.R, mixture_engine.R, mediation_engine.R,
## multinomial_engine.R, and timetrend_engine.R.
## ============================================================================

resolve_population_def <- function(population) {
  if (is.character(population)) {
    pop_def <- population_definitions[[population]]
    if (is.null(pop_def)) stop(sprintf(
      "Unknown population '%s'. Available: %s",
      population, paste(names(population_definitions), collapse=", ")))
    pop_def
  } else if (is.list(population) && !is.null(population$constraints)) {
    population
  } else {
    stop("population must be a named string or a list with $constraints.")
  }
}


## ============================================================================
## Shared weight helpers (used by run_analysis and multinomial_engine)
## ============================================================================

## Expand a variable to its raw NHANES source columns for weight lookup.
expand_to_sources <- function(var) {
  v <- tolower(var)
  cv <- get_custom_variables()[[v]]
  if (!is.null(cv) && length(cv$requires) > 0)
    return(tolower(cv$requires))
  dor <- derived_outcomes_registry[[v]]
  if (!is.null(dor) && length(dor$requires) > 0)
    return(tolower(dor$requires))
  v
}

## Choose the most restrictive per-cycle weight for an exposure-outcome pair.
## wt_map: attr(db, "variable_weight_map")
## Returns list(weight, raw_cols, cycle_wt_used, n_cycles)
build_pair_weight <- function(data, exposure, outcome, covariates = character(0),
                               wt_map = list(), dietary_weight = FALSE) {
  build_analysis_weight(data, c(exposure, outcome, covariates), wt_map,
                        dietary_weight = dietary_weight)
}

## Print the compact weight-map header used by all engines.
print_weight_map <- function(all_vars, wt_map) {
  cat("\n=== Weight maps (variable -> cycle -> weight column) ===\n")
  for (v in all_vars) {
    srcs   <- expand_to_sources(v)
    maps   <- lapply(srcs, function(s) wt_map[[s]] %||% list())
    merged <- do.call(c, maps)
    merged <- merged[!duplicated(names(merged))]
    if (length(merged) == 0) {
      cat(sprintf("  %-20s  (no subsample weight — wtmec2yr fallback for all cycles)\n", v))
    } else {
      years_sorted <- sort(as.integer(names(merged)))
      entries <- paste(
        sapply(years_sorted, function(yr)
          sprintf("%d:%s", yr, merged[[as.character(yr)]])),
        collapse = "  ")
      cat(sprintf("  %-20s  %s\n", v, entries))
    }
  }
  cat("========================================================\n\n")
}

## Print the per-pair weight audit table used by all engines.
print_weight_audit <- function(key, wt_info, pair_data) {
  parts <- strsplit(key, "__")[[1]]
  cat(sprintf("Weight audit — %s ~ %s (most restrictive: %s / %d cycles):\n",
              parts[2], parts[1], wt_info$raw_cols, wt_info$n_cycles))
  cat(sprintf("  %-12s %-20s %10s %10s\n", "Cycle", "Weight column", "N", "Mean weight"))
  cat(sprintf("  %s\n", paste(rep("-", 56), collapse = "")))
  for (yr in sort(unique(pair_data$year))) {
    yr_mask <- pair_data$year == yr
    yr_wt   <- pair_data$pooled_weight[yr_mask]
    n_yr    <- sum(!is.na(yr_wt) & yr_wt > 0)
    wt_src  <- wt_info$cycle_wt_used[[as.character(yr)]] %||% "wtmec2yr (fallback)"
    cat(sprintf("  %-12s %-20s %10d %10.1f\n",
                paste0(yr, "-", yr + 1), wt_src, n_yr,
                if (n_yr > 0) mean(yr_wt[!is.na(yr_wt) & yr_wt > 0]) else NA))
  }
  cat(sprintf("  %s\n\n", paste(rep("-", 56), collapse = "")))
}

## ============================================================================
## run_analysis()
## ============================================================================

#' Run a complete NHANES analysis against the pre-built local database.
#'
#' Orchestrates the full pipeline: load database → apply population filter →
#' select survey weight → build svydesign → run svyglm per exposure-outcome pair
#' → format output. All column names must match the lowercase convention used
#' in nhanes_pool.rds (see \code{COLUMN_MAP} for the canonical-name aliases).
#'
#' @param exposures character vector of exposure variable names (canonical or
#'   aliased, e.g. \code{c("pfoa","pfos","pfhxs","pfna")}).
#' @param outcomes character vector of outcome variable names
#'   (e.g. \code{c("ldl","non_hdl","remnant")}).
#' @param covariates character vector of covariate names to include in every
#'   regression model.
#' @param population either a named string matching an entry in
#'   \code{population_definitions} (e.g. \code{"all_adults"}) or a list with
#'   \code{$label} and \code{$constraints} elements produced by
#'   \code{make_constraint()}.
#' @param cycles character vector of NHANES cycle labels to pool,
#'   e.g. \code{c("1999-2000","2007-2008")}. Defaults to all ten cycles
#'   1999-2018. Participants outside these cycles are excluded.
#' @param output_type one of \code{"table"} (default), \code{"forest"}, or
#'   \code{"heatmap"}.
#' @param db_path path to the pre-built database RDS file produced by
#'   \code{build_nhanes_database()}. Defaults to \code{data/nhanes_pool.rds}
#'   relative to the project root.
#' @param exp_log10_flags logical vector, same length as \code{exposures}.
#'   \code{TRUE} applies a \code{log10()} transform to that exposure before
#'   fitting. Defaults to \code{TRUE} for PFAS variables, \code{FALSE} for all
#'   others.
#' @param out_log10_flags logical vector, same length as \code{outcomes}.
#'   \code{TRUE} log10-transforms the outcome. Defaults to all \code{FALSE}.
#' @param family GLM family object or string shortcut (\code{"gaussian"},
#'   \code{"binomial"}, \code{"logistic"}). Defaults to \code{gaussian()}.
#' @param spline_df integer degrees of freedom for a natural spline on the
#'   exposure (via \code{ns()}). \code{NULL} (default) fits a linear term.
#' @param spline_knots numeric vector of interior knot positions for the
#'   natural spline. Overrides \code{spline_df} when provided.
#' @param complete_cases_only logical. If \code{TRUE}, restricts to participants
#'   non-missing on ALL exposures and outcomes simultaneously, giving one
#'   shared analytic sample. Defaults to \code{FALSE} (per-pair filtering).
#' @param quartile_stratified logical. If \code{TRUE}, also runs a
#'   quartile-factor regression (Q1 = reference, Q2-Q4 as contrasts) for each
#'   pair and attaches results to \code{$quartile_results}. Defaults to
#'   \code{FALSE}.
#' @param logistic_ref_lowest logical. For logistic models only: if \code{TRUE},
#'   factors the exposure with its minimum observed value as the reference
#'   level. Defaults to \code{FALSE}.
#' @param verbose logical. Print filter traces, weight maps, and regression
#'   summaries to the console. Defaults to \code{TRUE}.
#'
#' @return An invisible list with components:
#' \describe{
#'   \item{type}{one of \code{"linear"}, \code{"logistic"}, or \code{"spline"}.}
#'   \item{results}{named list of standardised results objects, one per
#'     outcome-exposure pair (key: \code{"outcome__exposure"}).}
#'   \item{quartile_results}{named list of quartile data frames when
#'     \code{quartile_stratified = TRUE}, otherwise \code{NULL}.}
#'   \item{formatted}{table, ggplot, or list depending on \code{output_type}.}
#'   \item{pair_demographics}{list of Table-1 summaries, one per pair (or one
#'     shared entry when \code{complete_cases_only = TRUE}).}
#'   \item{n}{unweighted analytic N (first pair, or shared complete-cases N).}
#'   \item{population}{label string of the population definition used.}
#'   \item{cycles}{the \code{cycles} argument as passed.}
#' }
#'
#' @examples
#' \dontrun{
#' # Basic PFAS-lipid analysis in CVD-medicated fasting adults
#' result <- run_analysis(
#'   exposures  = c("pfoa", "pfos", "pfhxs", "pfna"),
#'   outcomes   = c("ldl", "non_hdl", "remnant"),
#'   covariates = c("age", "sex_bin", "race_hispanic", "race_black",
#'                  "race_other", "bmi", "education", "income_ratio",
#'                  "smq020", "diabetes_sr"),
#'   population  = "cvd_medicated_fasting",
#'   output_type = "table"
#' )
#' print(result$formatted)
#'
#' # Custom population using make_constraint()
#' result2 <- run_analysis(
#'   exposures  = "pfoa",
#'   outcomes   = "ldl",
#'   covariates = c("age", "sex_bin", "bmi"),
#'   population = list(
#'     label       = "Adults 40-65",
#'     constraints = list(
#'       make_constraint("age", ">=", 40),
#'       make_constraint("age", "<=", 65)
#'     )
#'   )
#' )
#' }
run_analysis <- function(
  exposures        = c("pfoa", "pfos", "pfhxs", "pfna"),
  outcomes         = c("ldl", "non_hdl", "remnant"),
  covariates       = c("age", "sex_bin", "race_hispanic", "race_black", "race_other",
                        "smq020", "diabetes_sr", "education", "income_ratio", "bmi"),
  population       = "cvd_medicated_fasting",
  cycles           = c("1999-2000", "2001-2002", "2003-2004", "2005-2006",
                        "2007-2008", "2009-2010", "2011-2012", "2013-2014",
                        "2015-2016", "2017-2018"),
  output_type      = "table",
  db_path          = here("data", "nhanes_pool.rds"),
  ## Per-variable transform flags
  exp_log10_flags  = NULL,
  out_log10_flags  = NULL,
  ## Regression family and spline options
  family                = gaussian(),
  spline_df             = NULL,
  spline_knots          = NULL,
  complete_cases_only   = FALSE,
  quartile_stratified   = FALSE,
  logistic_ref_lowest   = FALSE,
  dietary_weight        = FALSE,
  verbose               = TRUE
) {

  ## -- 1. Resolve population ------------------------------------------------
  if (is.character(population)) {
    pop_def <- population_definitions[[population]]
    if (is.null(pop_def)) stop(sprintf(
      "Unknown population '%s'. Available: %s",
      population, paste(names(population_definitions), collapse=", ")))
  } else if (is.list(population) && !is.null(population$constraints)) {
    pop_def <- population
  } else {
    stop("population must be a named string or list with $constraints.")
  }
  if (verbose) cat(sprintf("Population:  %s\n", pop_def$label %||% "custom"))

  ## -- 2. Resolve column names to lowercase database names ------------------
  exp_cols <- resolve_colnames(exposures)
  cov_cols <- resolve_colnames(covariates)

  ## Resolve outcomes
  out_cols <- resolve_colnames(outcomes)

  if (verbose) {
    cat(sprintf("Exposures:   %s\n", paste(exp_cols, collapse=", ")))
    cat(sprintf("Outcomes:    %s\n", paste(out_cols, collapse=", ")))
    cat(sprintf("Covariates:  %s\n\n", paste(cov_cols, collapse=", ")))
  }

  ## -- 3. Load database -----------------------------------------------------
  if (verbose) cat("Loading database... ")
  db <- load_nhanes_database(db_path)
  if (verbose) cat(sprintf("%d rows\n\n", nrow(db)))

  ## -- 3b. Validate all requested variables exist in the database -----------
  all_requested <- unique(c(exp_cols, out_cols, cov_cols))
  missing_vars  <- setdiff(all_requested, names(db))
  if (length(missing_vars) > 0) {
    stop(sprintf(
      "Sorry, the following variable(s) have not been added to the dataset: %s. Please add them in the Dataset tab to continue.",
      paste(missing_vars, collapse = ", ")
    ))
  }

  ## -- 4. Setup base constraints (per-pair filtering happens in loop) --------
  cycle_years      <- as.integer(sub("-.*", "", cycles))
  cycle_constraint <- list(make_constraint("year", "%in%", cycle_years))
  covariate_not_na <- lapply(cov_cols, function(col) make_constraint(col, "not_na"))
  base_constraints <- c(pop_def$constraints, cycle_constraint, covariate_not_na)

  ## If complete_cases_only, add not_na for ALL exposures and outcomes now so
  ## every pair in the loop uses the same restricted sample.
  if (isTRUE(complete_cases_only)) {
    all_var_not_na <- lapply(c(exp_cols, out_cols),
                             function(v) make_constraint(v, "not_na"))
    base_constraints <- c(base_constraints, all_var_not_na)
    if (verbose)
      cat(sprintf("Complete-cases mode: requiring non-missing for all %d exposures + %d outcomes\n",
                  length(exp_cols), length(out_cols)))
  }

  zscore_vars <- intersect(
    c(exp_cols, out_cols, cov_cols),
    names(Filter(function(e) isTRUE(e$is_zscore), get_custom_variables()))
  )

  ## -- 5. Weight helper -------------------------------------------------------
  wt_map <- attr(db, "variable_weight_map") %||% list()

  ## -- 6. Run regressions — each pair filtered independently ----------------
  pfas_cols <- c("pfoa","pfos","pfhxs","pfna")
  if (is.null(exp_log10_flags)) exp_log10_flags <- tolower(exp_cols) %in% pfas_cols
  if (is.null(out_log10_flags)) out_log10_flags  <- rep(FALSE, length(out_cols))
  if (length(exp_log10_flags) != length(exp_cols))
    stop("exp_log10_flags length must match exposures length")
  if (length(out_log10_flags) != length(out_cols))
    stop("out_log10_flags length must match outcomes length")

  if (verbose)
    print_weight_map(unique(c(exp_cols, out_cols, cov_cols)), wt_map)

  ## When complete_cases_only, pre-compute one shared weight from ALL variables
  ## so every pair uses the identical (most restrictive) weight map.
  shared_wt_info <- NULL
  if (isTRUE(complete_cases_only) && length(exp_cols) > 0 && length(out_cols) > 0) {
    shared_data    <- as.data.frame(apply_population_filter(db, base_constraints))
    shared_wt_info <- build_pair_weight(
      shared_data,
      exposure       = exp_cols[1],
      outcome        = out_cols[1],
      covariates     = c(cov_cols, exp_cols[-1], out_cols[-1]),
      wt_map         = wt_map,
      dietary_weight = dietary_weight
    )
    if (verbose)
      cat(sprintf("Shared weight (complete-cases): %s across %d cycle(s)\n\n",
                  shared_wt_info$raw_cols, shared_wt_info$n_cycles))
  }

  if (verbose) cat("Running regressions...\n")
  results_list          <- list()
  quartile_results_list <- list()
  weight_audits         <- list()
  pair_data_store       <- list()

  for (i in seq_along(out_cols)) {
    outcome <- out_cols[i]
    for (j in seq_along(exp_cols)) {
      exposure <- exp_cols[j]
      key      <- paste(outcome, exposure, sep="__")

      pair_constraints <- c(
        base_constraints,
        list(make_constraint(exposure, "not_na")),
        list(make_constraint(outcome,  "not_na"))
      )

      if (verbose) cat(sprintf("\n  %s ~ %s\n", exposure, outcome))
      pair_data <- apply_population_filter(db, pair_constraints)
      ## Force to plain data.frame to strip tibble/dplyr class attributes
      pair_data <- as.data.frame(pair_data)
      if (verbose) {
        print_filter_trace(pair_data)
        cat(sprintf("  Analytic N: %d\n", nrow(pair_data)))
      }

      if (nrow(pair_data) < 100) {
        warning(sprintf("N=%d for %s ~ %s — minimum 100 required, skipping",
                         nrow(pair_data), outcome, exposure))
        next
      }

      if (length(zscore_vars) > 0)
        pair_data <- apply_zscore_vars(pair_data, zscore_vars)

      ## Apply dropdown zscore transforms (regular NHANES variables, not custom).
      .zscore_col <- function(x) { m <- mean(x, na.rm=TRUE); s <- sd(x, na.rm=TRUE); (x - m) / s }
      if (identical(exp_log10_flags[j], "zscore") && exposure %in% names(pair_data))
        pair_data[[exposure]] <- .zscore_col(pair_data[[exposure]])
      if (identical(out_log10_flags[i], "zscore") && outcome %in% names(pair_data))
        pair_data[[outcome]] <- .zscore_col(pair_data[[outcome]])

      wt_info <- if (!is.null(shared_wt_info)) shared_wt_info else
        build_pair_weight(pair_data, exposure, outcome, cov_cols, wt_map,
                          dietary_weight = dietary_weight)
      pair_data$pooled_weight <- wt_info$weight
      pair_design <- build_survey_design(pair_data, exp_cols)
      weight_audits[[key]]   <- wt_info
      pair_data_store[[key]] <- pair_data

      results_list[[key]] <- tryCatch(
        run_weighted_regression(pair_design, outcome, exposure, cov_cols,
                                log10_exposure    = exp_log10_flags[j],
                                outcome_transform = out_log10_flags[i],
                                family            = family,
                                spline_df         = spline_df,
                                spline_knots      = spline_knots,
                                ref_lowest        = isTRUE(logistic_ref_lowest)),
        error = function(e) {
          warning(sprintf("Regression failed for %s ~ %s: %s",
                           outcome, exposure, conditionMessage(e)))
          NULL
        }
      )

      ## -- Quartile-factor regression (Q1 = reference, Q2–Q4 as contrasts) ----
      if (quartile_stratified && !is.null(results_list[[key]])) {
        qdf <- tryCatch(
          run_quartile_factor_regression(pair_design, outcome, exposure, cov_cols,
                                          log10_exposure    = exp_log10_flags[j],
                                          outcome_transform = out_log10_flags[i],
                                          family            = family),
          error = function(e) {
            warning(sprintf("Quartile regression failed for %s ~ %s: %s",
                             outcome, exposure, conditionMessage(e)))
            NULL
          }
        )
        if (!is.null(qdf)) quartile_results_list[[key]] <- qdf
      }
    }
  }
  results_list <- Filter(Negate(is.null), results_list)
  if (verbose) cat(sprintf("\nCompleted %d regressions\n\n", length(results_list)))

  if (verbose && length(weight_audits) > 0)
    for (key in names(weight_audits))
      print_weight_audit(key, weight_audits[[key]], pair_data_store[[key]])

  ## Build per-pair demographics or a single entry for complete-cases mode
  if (isTRUE(complete_cases_only) || length(pair_data_store) <= 1) {
    single_data <- if (length(pair_data_store) > 0)
                     as.data.frame(pair_data_store[[1]])
                   else
                     as.data.frame(apply_population_filter(db, base_constraints))
    pair_demo_label <- if (isTRUE(complete_cases_only)) "All (complete cases)" else "All"
    pair_demographics <- list(
      list(
        label  = pair_demo_label,
        n      = nrow(single_data),
        tables = build_demographic_table(single_data,
                   covariates = cov_cols,
                   outcomes   = out_cols,
                   exposures  = exp_cols)
      )
    )
  } else {
    pair_demographics <- lapply(names(pair_data_store), function(key) {
      parts   <- strsplit(key, "__")[[1]]
      out_col <- parts[1]
      exp_col <- parts[2]
      pd      <- as.data.frame(pair_data_store[[key]])
      list(
        label  = sprintf("%s → %s", exp_col, out_col),
        n      = nrow(pd),
        tables = build_demographic_table(pd,
                   covariates = cov_cols,
                   outcomes   = out_col,
                   exposures  = exp_col)
      )
    })
  }

  ## Legacy single pop_data reference for formatted output
  pop_data <- as.data.frame(
    if (length(pair_data_store) > 0) pair_data_store[[1]]
    else apply_population_filter(db, base_constraints)
  )

  ## -- 7. Format output -------------------------------------------------------
  formatted <- switch(output_type,
    "table"   = make_results_table(results_list),
    "forest"  = make_forest_plot(results_list),
    "heatmap" = make_heatmap(results_list),
    stop(sprintf("Unknown output_type '%s'", output_type))
  )

  ## Determine model type for results_ui branching
  any_logistic <- any(vapply(results_list, function(r)
    isTRUE(r$is_logistic), logical(1)))
  any_spline   <- any(vapply(results_list, function(r)
    !is.null(r$spline_df) || !is.null(r$spline_knots), logical(1)))
  result_type  <- if (any_logistic) "logistic" else
                  if (any_spline)   "spline"   else "linear"

  invisible(list(
    type             = result_type,
    results          = results_list,
    quartile_results = if (quartile_stratified) quartile_results_list else NULL,
    formatted        = formatted,
    pair_demographics = pair_demographics,
    n                = nrow(pop_data),
    population       = pop_def$label %||% "custom",
    cycles           = cycles
  ))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
