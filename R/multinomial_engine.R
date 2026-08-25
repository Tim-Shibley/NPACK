## ============================================================================
## multinomial_engine.R
##
## Survey-weighted multinomial logistic regression via repeated svyglm() calls.
## Used when the outcome is an unordered categorical variable with 3+ levels
## (e.g. BMI category, education level, self-reported health status).
##
## Returns one coefficient table per outcome level (vs. reference level),
## with odds ratios and 95% CIs.
##
## Depends on globals from cli.R:
##   resolve_colnames, %||%, load_nhanes_database, apply_population_filter,
##   make_constraint, resolve_population_def, build_demographic_table,
##   build_survey_design, PFAS_VARS, WEIGHT_PRIORITY
## ============================================================================

library(survey)

## Decide whether to treat a column as categorical. Returns TRUE for factors,
## character vectors, logical, and integer/numeric columns with <= max_levels
## distinct non-NA values.
.is_categorical_exposure <- function(x, max_levels = 10L) {
  if (is.factor(x) || is.character(x) || is.logical(x)) return(TRUE)
  n_uniq <- length(unique(na.omit(x)))
  is.integer(x) && n_uniq <= max_levels
}

#' Run a survey-weighted multinomial logistic regression.
#'
#' Fits one binary \code{svyglm()} per non-reference outcome level (each
#' contrasted against the reference level), which is equivalent to multinomial
#' logistic regression and works with any version of the \pkg{survey} package.
#' Outcomes must have three or more levels; continuous outcomes are coerced to
#' factors. Categorical exposures are automatically detected and factored with
#' the minimum observed value as the reference level.
#'
#' @param exposures character vector of exposure variable names.
#' @param outcomes character vector of outcome variable names. Each must have
#'   \eqn{\ge 3} distinct non-missing values after filtering; binary outcomes
#'   should use \code{run_analysis(family = "logistic")} instead.
#' @param covariates character vector of covariate names included in every model.
#' @param population named string from \code{population_definitions} or a list
#'   with \code{$label} and \code{$constraints}.
#' @param cycles character vector of NHANES cycle labels to include.
#' @param db_path path to the pre-built database RDS file.
#' @param seed integer random seed (reserved for future use).
#' @param complete_cases_only logical. If \code{TRUE}, restricts to participants
#'   non-missing on all exposures and all outcomes simultaneously.
#' @param verbose logical. Print filter traces, level counts, and per-level
#'   odds ratios to the console.
#'
#' @return An invisible list with components:
#' \describe{
#'   \item{type}{\code{"multinomial"}.}
#'   \item{multinomial_results}{named list keyed by
#'     \code{"outcome__exposure"}. Each entry contains: \code{ref_level}
#'     (reference outcome level), \code{exp_is_cat} (logical), \code{coef_table}
#'     (data frame with columns \code{level}, \code{term}, \code{OR},
#'     \code{ci_low}, \code{ci_high}, \code{p_value}), and \code{n}.}
#'   \item{pair_demographics}{Table-1 summaries.}
#'   \item{n}{unweighted analytic N.}
#'   \item{population}{population label string.}
#'   \item{cycles}{cycles used.}
#' }
#'
#' @examples
#' \dontrun{
#' # Treat education (5 ordinal levels) as a multinomial outcome
#' result <- run_multinomial_analysis(
#'   exposures  = "pfoa",
#'   outcomes   = "education",
#'   covariates = c("age", "sex_bin", "income_ratio"),
#'   population = "all_adults",
#'   cycles     = c("2003-2004", "2005-2006", "2007-2008", "2009-2010",
#'                  "2011-2012", "2013-2014", "2015-2016", "2017-2018"),
#'   db_path    = here::here("data", "nhanes_pool.rds")
#' )
#' # Odds ratios for each education level vs. the reference
#' print(result$multinomial_results[["education__pfoa"]]$coef_table)
#' }
run_multinomial_analysis <- function(exposures, outcomes, covariates,
                                      population, cycles, db_path,
                                      seed               = 42,
                                      complete_cases_only = FALSE,
                                      dietary_weight     = FALSE,
                                      verbose            = TRUE) {

  pop_def  <- resolve_population_def(population)
  exp_cols <- resolve_colnames(exposures)
  out_cols <- resolve_colnames(outcomes)
  cov_cols <- resolve_colnames(covariates)

  if (verbose) {
    cat(sprintf("Multinomial — Population: %s\n", pop_def$label %||% "custom"))
    cat(sprintf("Exposures: %s | Outcomes: %s\n\n",
                paste(exp_cols, collapse=", "), paste(out_cols, collapse=", ")))
  }

  db <- load_nhanes_database(db_path)
  missing <- setdiff(unique(c(exp_cols, out_cols, cov_cols)), names(db))
  if (length(missing) > 0)
    stop(sprintf("Variable(s) not in dataset: %s.", paste(missing, collapse=", ")))

  cycle_years      <- as.integer(sub("-.*", "", cycles))
  base_constraints <- c(pop_def$constraints,
                         list(make_constraint("year", "%in%", cycle_years)),
                         lapply(cov_cols, function(v) make_constraint(v, "not_na")))

  ## If complete_cases_only: require all exposures + outcomes non-missing in base
  ## (same sample for every pair, matching cli.R behaviour)
  if (isTRUE(complete_cases_only)) {
    base_constraints <- c(base_constraints,
      lapply(c(exp_cols, out_cols), function(v) make_constraint(v, "not_na")))
  }
  wt_map <- attr(db, "variable_weight_map") %||% list()

  if (verbose)
    print_weight_map(unique(c(exp_cols, out_cols, cov_cols)), wt_map)

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
  multinomial_results <- list()
  weight_audits       <- list()
  pair_data_store     <- list()
  cov_rhs <- if (length(cov_cols) > 0) paste(cov_cols, collapse=" + ") else "1"

  for (outcome in out_cols) {
    for (exposure in exp_cols) {
      key <- paste(outcome, exposure, sep="__")
      if (verbose) cat(sprintf("\n  %s ~ %s\n", outcome, exposure))

      constraints_i <- c(base_constraints,
        list(make_constraint(exposure, "not_na")),
        list(make_constraint(outcome,  "not_na")))

      filtered_i <- apply_population_filter(db, constraints_i)
      data_i     <- as.data.frame(filtered_i)
      if (verbose) {
        print_filter_trace(filtered_i)
        cat(sprintf("  Analytic N: %d\n", nrow(data_i)))
      }
      wt_info <- if (!is.null(shared_wt_info)) shared_wt_info else
        build_pair_weight(data_i, exposure, outcome, cov_cols, wt_map,
                          dietary_weight = dietary_weight)
      data_i$pooled_weight <- wt_info$weight

      ## Outcome must be a factor for svymultinom
      if (!is.factor(data_i[[outcome]]))
        data_i[[outcome]] <- as.factor(data_i[[outcome]])

      n_levels <- nlevels(data_i[[outcome]])
      if (n_levels < 3) {
        warning(sprintf(
          "'%s' has only %d level(s) — use logistic regression instead, skipping",
          outcome, n_levels))
        next
      }

      ## If exposure is categorical, factor it with minimum level as reference
      exp_is_cat <- .is_categorical_exposure(data_i[[exposure]])
      exp_col_in_formula <- exposure
      exp_ref_level <- NULL
      if (exp_is_cat) {
        raw_exp <- data_i[[exposure]]
        exp_levels <- sort(unique(na.omit(
          if (is.factor(raw_exp)) as.character(raw_exp) else raw_exp
        )))
        exp_ref_level <- as.character(exp_levels[1L])
        fac_col <- paste0(".fac_", exposure)
        data_i[[fac_col]] <- relevel(
          factor(raw_exp, levels = as.character(exp_levels)),
          ref = exp_ref_level
        )
        exp_col_in_formula <- fac_col
        if (verbose)
          cat(sprintf("Exposure '%s' treated as categorical (ref = '%s')\n",
                      exposure, exp_ref_level))
      }

      ## Drop rows missing pooled_weight or the factored exposure column
      cc_cols <- intersect(c(exp_col_in_formula, "pooled_weight"), names(data_i))
      data_cc <- data_i[complete.cases(data_i[, cc_cols, drop=FALSE]), ]

      if (nrow(data_cc) < 100) {
        warning(sprintf("N=%d for %s ~ %s (multinomial), skipping",
                         nrow(data_cc), outcome, exposure))
        next
      }
      if (verbose) cat(sprintf("N = %d | Levels: %s\n", nrow(data_cc),
                               paste(levels(data_cc[[outcome]]), collapse=", ")))

      svy       <- svydesign(ids=~1, weights=~pooled_weight, data=data_cc)
      out_lvls  <- levels(data_cc[[outcome]])
      ref_level <- out_lvls[1L]
      non_ref   <- out_lvls[-1L]
      fac_prefix <- if (exp_is_cat) paste0(".fac_", exposure) else NULL

      ## Fit one survey-weighted logistic model per non-reference outcome level
      ## (binary: this level vs reference level). This is equivalent to
      ## multinomial logistic regression and works with any version of survey.
      level_results <- lapply(non_ref, function(lv) {
        sub_idx  <- data_cc[[outcome]] %in% c(ref_level, lv)
        sub_data <- data_cc[sub_idx, , drop = FALSE]
        sub_data$.bin_outcome <- as.integer(sub_data[[outcome]] == lv)
        svy_sub <- svydesign(ids=~1, weights=~pooled_weight, data=sub_data)
        fml_bin <- as.formula(sprintf(".bin_outcome ~ %s + %s",
                                      exp_col_in_formula, cov_rhs))
        fit_bin <- tryCatch(
          svyglm(fml_bin, design=svy_sub, family=quasibinomial()),
          error = function(e)
            stop(sprintf("svyglm failed for %s (level '%s' vs '%s') ~ %s: %s",
                         outcome, lv, ref_level, exposure, conditionMessage(e)))
        )
        coefs <- summary(fit_bin)$coefficients
        terms <- rownames(coefs)
        if (exp_is_cat && !is.null(fac_prefix))
          terms <- sub(paste0("^", fac_prefix), exposure, terms, fixed = FALSE)
        data.frame(
          level    = lv,
          term     = terms,
          estimate = coefs[, "Estimate"],
          se       = coefs[, "Std. Error"],
          OR       = exp(coefs[, "Estimate"]),
          ci_low   = exp(coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"]),
          ci_high  = exp(coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"]),
          p_value  = coefs[, "Pr(>|t|)"],
          stringsAsFactors = FALSE
        )
      })
      coef_df <- do.call(rbind, level_results)

      weight_audits[[key]]       <- wt_info
      pair_data_store[[key]]     <- data_cc
      multinomial_results[[key]] <- list(
        outcome        = outcome,
        exposure       = exposure,
        ref_level      = ref_level,
        exp_is_cat     = exp_is_cat,
        exp_ref_level  = exp_ref_level,
        coef_table     = coef_df,
        n              = nrow(data_cc),
        fit            = NULL
      )

      if (verbose) {
        ## For categorical exposures, match all dummy terms; for continuous, exact match.
        exp_rows <- if (exp_is_cat)
          coef_df[startsWith(coef_df$term, exposure), ]
        else
          coef_df[coef_df$term == exposure, ]
        for (r in seq_len(nrow(exp_rows))) {
          cat(sprintf("  Level '%s' vs '%s': OR = %.3f (p = %.4f) [%s]\n",
                      exp_rows$level[r], ref_level,
                      exp_rows$OR[r], exp_rows$p_value[r],
                      exp_rows$term[r]))
        }
      }
    }
  }

  if (verbose) cat(sprintf("\nCompleted %d regressions\n\n", length(multinomial_results)))

  if (verbose && length(weight_audits) > 0)
    for (key in names(weight_audits))
      print_weight_audit(key, weight_audits[[key]], pair_data_store[[key]])

  pop_data <- as.data.frame(apply_population_filter(db, base_constraints))

  ## Build per-pair or single demographics
  pair_demographics <- if (isTRUE(complete_cases_only) || length(pair_data_store) <= 1) {
    single_data <- if (length(pair_data_store) > 0)
                     as.data.frame(pair_data_store[[1]])
                   else pop_data
    list(list(
      label  = if (isTRUE(complete_cases_only)) "All (complete cases)" else "All",
      n      = nrow(single_data),
      tables = build_demographic_table(single_data,
                 covariates = cov_cols,
                 outcomes   = out_cols,
                 exposures  = exp_cols)
    ))
  } else {
    lapply(names(pair_data_store), function(key) {
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

  invisible(list(
    type                = "multinomial",
    multinomial_results = multinomial_results,
    pair_demographics   = pair_demographics,
    n                   = nrow(pop_data),
    population          = pop_def$label %||% "custom",
    cycles              = cycles
  ))
}
