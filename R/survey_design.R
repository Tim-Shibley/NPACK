## ============================================================================
## survey_design.R
##
## Owns: constructing a svydesign object given a filtered population.
## Expects pooled_weight to be pre-set in the data (built by build_database.R).
## Knows nothing about what regression will eventually be run — that's
## regression_engine.R's job.
## ============================================================================

library(survey)

#' Build a survey design object for a given filtered dataset.
#'
#' Uses Taylor-series linearisation (TSL) variance estimation via strata +
#' PSU + nest — the standard NHANES-recommended approach (SDMVSTRA/SDMVPSU).
#' Expects pooled_weight to be pre-set in filtered_data by build_database.R.
#'
#' @param filtered_data output of apply_population_filter() — must contain
#'        pooled_weight, sdmvstra/SDMVSTRA, and sdmvpsu/SDMVPSU columns
#' @return a svydesign object ready for regression_engine.R
build_survey_design <- function(filtered_data, variables = NULL) {

  ## Resolve strata/PSU column names — accept uppercase or lowercase
  stra_col <- if ("SDMVSTRA" %in% names(filtered_data)) "SDMVSTRA" else
              if ("sdmvstra" %in% names(filtered_data)) "sdmvstra" else NULL
  psu_col  <- if ("SDMVPSU"  %in% names(filtered_data)) "SDMVPSU"  else
              if ("sdmvpsu"  %in% names(filtered_data)) "sdmvpsu"  else NULL

  required_cols <- c("pooled_weight")
  if (is.null(stra_col)) required_cols <- c(required_cols, "SDMVSTRA/sdmvstra")
  if (is.null(psu_col))  required_cols <- c(required_cols, "SDMVPSU/sdmvpsu")
  missing_cols <- setdiff(required_cols, names(filtered_data))
  if (length(missing_cols) > 0 || is.null(stra_col) || is.null(psu_col)) {
    stop(sprintf(
      "filtered_data is missing required column(s): %s",
      paste(c(if(is.null(stra_col)) "sdmvstra", if(is.null(psu_col)) "sdmvpsu",
              setdiff(required_cols, names(filtered_data))), collapse=", ")))
  }

  n_zero_weight <- sum(filtered_data$pooled_weight == 0, na.rm = TRUE)
  n_na_weight   <- sum(is.na(filtered_data$pooled_weight))
  if (n_zero_weight > 0 || n_na_weight > 0) {
    message(sprintf(
      "Note: dropping %d rows with zero weight and %d rows with missing weight before building design.",
      n_zero_weight, n_na_weight))
    filtered_data <- filtered_data[!is.na(filtered_data$pooled_weight) &
                                     filtered_data$pooled_weight > 0, ]
  }

  n_na_design <- sum(is.na(filtered_data[[stra_col]]) | is.na(filtered_data[[psu_col]]))
  if (n_na_design > 0) {
    message(sprintf(
      "Note: dropping %d rows with missing strata/PSU before building design.",
      n_na_design))
    filtered_data <- filtered_data[!is.na(filtered_data[[stra_col]]) &
                                     !is.na(filtered_data[[psu_col]]), ]
  }

  # Handle strata with only one PSU — a known issue with small NHANES
  # subpopulations where filtering leaves some strata with a single PSU.
  # Using "certainty" (treats single-PSU strata as certainty strata,
  # contributing zero variance) to match the "collapsing" convention used
  # in the published MATLAB TSL implementation.
  # NOTE: this option must remain set through the svyglm() call in
  # regression_engine.R — it is NOT reset here because on.exit() would
  # reset it before the regression runs. The caller is responsible for
  # managing this option if it needs to be restored afterward.
  options(survey.lonely.psu = "certainty")

  design <- svydesign(
    ids     = as.formula(paste0("~", psu_col)),
    strata  = as.formula(paste0("~", stra_col)),
    weights = ~pooled_weight,
    nest    = TRUE,
    data    = filtered_data
  )

  weight_group <- if (!is.null(variables)) resolve_weight_group(variables) else "interview"
  attr(design, "weight_group") <- weight_group
  design
}
