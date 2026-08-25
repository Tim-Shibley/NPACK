## ============================================================================
## bkmr_engine.R
##
## Owns: fitting a Bayesian Kernel Machine Regression (BKMR) model via
## kmbayes() and extracting standardized outputs (PIPs and exposure-response
## curves) suitable for display. Mirrors run_analysis() in cli.R but runs
## all exposures jointly as the Z matrix rather than as one-at-a-time pairs.
##
## Depends on globals sourced before this file:
##   cli.R              → resolve_colnames, COLUMN_MAP, PFAS_VARS,
##                         WEIGHT_PRIORITY, population_definitions, %||%
##   build_database.R   → load_nhanes_database
##   population_filter.R→ apply_population_filter, make_constraint,
##                         print_filter_trace
##   derived_outcomes_registry.R → apply_zscore_vars
##   variable_lookup.R  → get_custom_variables
##   demographic_breakdown.R → build_demographic_table
## ============================================================================

library(bkmr)

## bkmrhat and future are optional — loaded only when parallel chains are requested.
## Install with: install.packages(c("bkmrhat", "future"))

## ============================================================================
## Core fitting function
## ============================================================================

#' Fit a survey-weighted BKMR model for one outcome.
#'
#' @param data        Filtered, complete-case-ready data frame.
#' @param outcome     Column name of the outcome (y).
#' @param exposures   Character vector of exposure column names (Z matrix).
#' @param covariates  Character vector of covariate column names (X matrix).
#' @param weight_col  Name of the pooled weight column (used for complete-case
#'   selection only; \pkg{bkmr} v0.2.0 does not expose a \code{weights}
#'   argument to \code{kmbayes()}).
#' @param n_iter      Number of MCMC iterations.
#' @param varsel      Logical. Whether to perform variable selection via binary
#'   inclusion indicators. Recommended \code{TRUE}; produces posterior inclusion
#'   probabilities (PIPs) for each exposure.
#' @param seed        Integer random seed for reproducibility.
#' @param n_chains    Integer number of parallel chains. Values \code{> 1}
#'   require \pkg{bkmrhat} and \pkg{future}.
#'
#' @return A list with components:
#' \describe{
#'   \item{type}{\code{"bkmr"}.}
#'   \item{outcome}{outcome column name.}
#'   \item{exposures}{exposure column names.}
#'   \item{covariates}{covariate column names.}
#'   \item{n}{number of complete cases used.}
#'   \item{fit}{raw \code{kmbayes} object (or \code{bkmrplusfit} for
#'     multi-chain runs).}
#'   \item{pips}{data frame with columns \code{exposure} and \code{pip}
#'     (posterior inclusion probability). \code{NA} when \code{varsel = FALSE}.}
#'   \item{pip_conv}{convergence diagnostics for the \code{r} parameters
#'     (R-hat, Bulk ESS) when multi-chain; \code{NULL} otherwise.}
#'   \item{varsel}{the \code{varsel} argument as passed.}
#'   \item{convergence}{full convergence diagnostics data frame from
#'     \code{bkmrhat::kmbayes_diagnose()}, or \code{NULL}.}
#'   \item{n_chains}{number of chains run.}
#' }
run_bkmr <- function(data,
                      outcome,
                      exposures,
                      covariates,
                      weight_col = "pooled_weight",
                      n_iter     = 1000,
                      varsel     = TRUE,
                      seed       = 42,
                      n_chains   = 1L) {

  ## Drop rows where any required variable is NA
  vars_needed  <- intersect(c(outcome, exposures, covariates),
                             names(data))
  complete_idx <- complete.cases(data[, vars_needed, drop = FALSE])
  data_cc      <- data[complete_idx, , drop = FALSE]

  if (nrow(data_cc) < 50)
    stop(sprintf(
      "Insufficient complete cases for BKMR (N = %d; minimum 50 required).",
      nrow(data_cc)))

  y <- as.numeric(data_cc[[outcome]])
  Z <- as.matrix(data_cc[, exposures, drop = FALSE])
  X <- as.matrix(data_cc[, covariates, drop = FALSE])

  ## Note: the CRAN release of bkmr (v0.2.0) does not expose a weights
  ## argument in kmbayes(). The pooled survey weight is applied upstream
  ## during population filtering (complete-case selection and subsample
  ## weight construction) but is not passed into the MCMC sampler.
  ## If a future bkmr release adds native survey-weight support, re-enable:
  ##   weights = wt
  mode_label <- if (n_chains > 1L)
    sprintf("bkmrhat · %d chains", n_chains) else "single chain"
  message(sprintf(
    "[BKMR] Fitting: outcome=%s  N=%d  exposures=%d  iter=%d  mode=%s",
    outcome, nrow(data_cc), ncol(Z), n_iter, mode_label))

  ## 10-iteration pilot to estimate total run time (output suppressed)
  t0 <- proc.time()[["elapsed"]]
  set.seed(seed)
  suppressMessages(capture.output(
    kmbayes(y = y, Z = Z, X = X, iter = 10, varsel = varsel, verbose = FALSE)
  ))
  secs_per_iter <- (proc.time()[["elapsed"]] - t0) / 10
  est_secs      <- secs_per_iter * n_iter
  if (est_secs < 60) {
    message(sprintf("[BKMR] Estimated time: ~%.0f seconds", est_secs))
  } else {
    message(sprintf("[BKMR] Estimated time: ~%.1f minutes", est_secs / 60))
  }

  t_start <- proc.time()[["elapsed"]]

  if (n_chains > 1L) {
    ## --- Parallel chains via bkmrhat ----------------------------------------
    set.seed(seed)
    fit_parallel <- bkmrhat::kmbayes_parallel(
      y       = y, Z = Z, X = X,
      iter    = n_iter,
      varsel  = varsel,
      nchains = n_chains
    )

    ## Extract convergence diagnostics (R-hat, ESS) before collapsing chains
    conv <- tryCatch(
      bkmrhat::kmbayes_diagnose(fit_parallel),
      error = function(e) {
        message(sprintf("[BKMR] Convergence diagnostics failed: %s", conditionMessage(e)))
        NULL
      }
    )

    ## Collapse to a single kmbayes-compatible object for all downstream functions
    fit <- bkmrhat::kmbayes_combine(fit_parallel)

  } else {
    ## --- Single chain (standard bkmr) ----------------------------------------
    set.seed(seed)
    fit  <- kmbayes(y = y, Z = Z, X = X,
                     iter    = n_iter,
                     varsel  = varsel,
                     verbose = FALSE)
    conv <- NULL
  }

  elapsed <- proc.time()[["elapsed"]] - t_start
  if (elapsed < 60) {
    message(sprintf("[BKMR] Done:    outcome=%s  (%.0f seconds)", outcome, elapsed))
  } else {
    message(sprintf("[BKMR] Done:    outcome=%s  (%.1f minutes)", outcome, elapsed / 60))
  }

  if (!is.null(conv) && is.data.frame(conv))
    message(sprintf("[BKMR] Convergence: max R-hat = %.3f  min Bulk_ESS = %.0f",
                    max(conv$Rhat,     na.rm = TRUE),
                    min(conv$Bulk_ESS, na.rm = TRUE)))

  ## Extract posterior inclusion probabilities.
  ## bkmr stores binary inclusion indicators in fit$delta (n_iter x n_exposures);
  ## PIP for each exposure is the posterior mean of its delta column.
  pip_vals <- if (varsel && !is.null(fit$delta)) {
    colMeans(fit$delta, na.rm = TRUE)
  } else if (varsel && !is.null(fit$pips)) {
    colMeans(fit$pips, na.rm = TRUE)
  } else {
    rep(NA_real_, length(exposures))
  }

  ## Format convergence diagnostics as a display-ready data frame if available.
  ## ComputeRhat returns a data frame with row names as parameter names
  ## (beta1, r1, r2, ..., lambda, sigsq.eps) and columns Rhat, Bulk_ESS, Tail_ESS.
  conv_df <- if (!is.null(conv) && is.data.frame(conv)) {
    conv$Parameter <- rownames(conv)
    conv$`R-hat`   <- round(conv$Rhat,      3)
    conv$Bulk_ESS  <- round(conv$Bulk_ESS,  1)
    conv$Tail_ESS  <- round(conv$Tail_ESS,  1)
    conv$Converged <- ifelse(conv$Rhat < 1.1 & conv$Bulk_ESS >= 100, "Yes", "No")
    conv[, c("Parameter", "R-hat", "Bulk_ESS", "Tail_ESS", "Converged"),
         drop = FALSE]
  } else NULL

  ## Extract per-exposure R-hat and ESS from the r1, r2, ... rows
  pip_conv <- if (!is.null(conv_df)) {
    r_rows <- conv_df[grepl("^r\\d+$", conv_df$Parameter), , drop = FALSE]
    if (nrow(r_rows) == length(exposures)) {
      data.frame(
        exposure  = exposures,
        `R-hat`   = r_rows$`R-hat`,
        Bulk_ESS  = r_rows$Bulk_ESS,
        check.names = FALSE
      )
    } else NULL
  } else NULL

  list(
    type        = "bkmr",
    outcome     = outcome,
    exposures   = exposures,
    covariates  = covariates,
    n           = nrow(data_cc),
    fit         = fit,
    pips        = data.frame(
                    exposure = exposures,
                    pip      = as.numeric(pip_vals),
                    stringsAsFactors = FALSE
                  ),
    pip_conv    = pip_conv,
    varsel      = varsel,
    convergence = conv_df,
    n_chains    = n_chains
  )
}


## ============================================================================
## Curve extraction
## ============================================================================

#' Extract univariate exposure-response curves from a fitted BKMR model.
#'
#' Uses the second half of MCMC iterations (thinned by 2) to summarise h(z)
#' via \code{bkmr::PredictorResponseUnivar()}.
#'
#' @param bkmr_result Output of \code{run_bkmr()}.
#' @param q.fixed Quantile at which all other exposures are held fixed while
#'   the target exposure is varied. Defaults to \code{0.5} (median).
#'
#' @return A data frame with columns \code{exposure}, \code{z} (exposure value),
#'   \code{est} (posterior mean of h), \code{lower} and \code{upper} (95\%
#'   credible interval: est ± 1.96 × posterior SD). Returns \code{NULL} on
#'   failure with a message.
extract_bkmr_curves <- function(bkmr_result, q.fixed = 0.5) {
  fit <- bkmr_result$fit

  ## bkmrplusfit (from kmbayes_combine) stores iterations in $beta or $chain1$beta
  beta_mat <- if (!is.null(fit$beta)) fit$beta else fit$chain1$beta
  if (is.null(beta_mat)) {
    message("[BKMR] extract_bkmr_curves: could not locate $beta in fit object")
    return(NULL)
  }
  n_it <- nrow(beta_mat)
  ## Use second half of chain, thinned by 2, to reduce autocorrelation
  sel  <- seq(floor(n_it / 2) + 1, n_it, by = 2)

  tryCatch({
    curves <- PredictorResponseUnivar(fit, sel = sel, q.fixed = q.fixed)

    ## PredictorResponseUnivar returns: variable (integer index), z, est, se
    ## Map integer variable index back to exposure column name
    if (!is.null(curves$variable)) {
      curves$exposure <- bkmr_result$exposures[as.integer(curves$variable)]
    } else {
      curves$exposure <- bkmr_result$exposures[seq_len(nrow(curves))]
    }

    ## 95% credible interval: est ± 1.96 × se
    curves$lower <- curves$est - 1.96 * curves$se
    curves$upper <- curves$est + 1.96 * curves$se

    curves[, c("exposure", "z", "est", "lower", "upper")]
  }, error = function(e) {
    message(sprintf("[BKMR] extract_bkmr_curves failed: %s", conditionMessage(e)))
    NULL
  })
}


## ============================================================================
## Full analysis pipeline (parallel to run_analysis() in cli.R)
## ============================================================================

#' Run the complete BKMR pipeline: filter → weight → kmbayes → format.
#'
#' One kmbayes model is fit per outcome (all exposures enter jointly as Z).
#' The function signature mirrors run_analysis() so the Shiny server can call
#' it identically, just swapping in n_iter and seed instead of exp_log10_flags.
#'
#' @param exposures   Character vector of exposure names (resolved via COLUMN_MAP).
#' @param outcomes    Character vector of outcome names.
#' @param covariates  Character vector of covariate names.
#' @param population  Named string from population_definitions, or a list
#'                    with $label and $constraints.
#' @param cycles      Character vector of NHANES cycles to include.
#' @param db_path     Path to nhanes_pool.rds.
#' @param n_iter      MCMC iterations passed to kmbayes().
#' @param seed        Random seed.
#' @param verbose     Whether to cat() progress messages (captured by Shiny).
#'
#' @return An invisible list with components:
#' \describe{
#'   \item{type}{\code{"bkmr"}.}
#'   \item{bkmr_results}{named list (by outcome), each an output of
#'     \code{run_bkmr()}: \code{fit}, \code{pips}, \code{convergence}, etc.}
#'   \item{convergence}{combined convergence diagnostics across all outcomes
#'     (worst-case R-hat, minimum ESS), or \code{NULL} for single-chain runs.}
#'   \item{n_chains}{number of chains used.}
#'   \item{pair_demographics}{Table-1 summaries for the analytic population.}
#'   \item{n}{unweighted analytic N.}
#'   \item{population}{population label string.}
#'   \item{cycles}{cycles used.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- run_bkmr_analysis(
#'   exposures  = c("pfoa", "pfos", "pfhxs", "pfna"),
#'   outcomes   = "ldl",
#'   covariates = c("age", "sex_bin", "bmi", "education", "income_ratio"),
#'   population = "cvd_medicated_fasting",
#'   cycles     = c("2003-2004", "2005-2006", "2007-2008", "2009-2010",
#'                  "2011-2012", "2013-2014", "2015-2016", "2017-2018"),
#'   db_path    = here::here("data", "nhanes_pool.rds"),
#'   n_iter     = 1000
#' )
#' result$bkmr_results$ldl$pips        # posterior inclusion probabilities
#'
#' # Extract and plot exposure-response curves
#' curves <- extract_bkmr_curves(result$bkmr_results$ldl)
#' if (!is.null(curves)) {
#'   library(ggplot2)
#'   ggplot(curves, aes(x = z, y = est, ymin = lower, ymax = upper)) +
#'     geom_ribbon(alpha = 0.2) + geom_line() +
#'     facet_wrap(~exposure, scales = "free_x")
#' }
#' }
run_bkmr_analysis <- function(exposures,
                               outcomes,
                               covariates,
                               population,
                               cycles,
                               db_path             = here::here("data", "nhanes_pool.rds"),
                               n_iter              = 1000,
                               seed                = 42,
                               n_chains            = 1L,
                               complete_cases_only = FALSE,
                               verbose             = TRUE,
                               progress_callback   = NULL) {

  ## -- 1. Resolve population (same logic as run_analysis) --------------------
  if (is.character(population)) {
    pop_def <- population_definitions[[population]]
    if (is.null(pop_def)) stop(sprintf(
      "Unknown population '%s'. Available: %s",
      population, paste(names(population_definitions), collapse = ", ")))
  } else if (is.list(population) && !is.null(population$constraints)) {
    pop_def <- population
  } else {
    stop("population must be a named string or a list with $constraints.")
  }
  ## -- 2. Resolve column names -----------------------------------------------
  exp_cols <- resolve_colnames(exposures)
  out_cols <- resolve_colnames(outcomes)
  cov_cols <- resolve_colnames(covariates)
  if (verbose) {
    cat(sprintf("Population:  %s\n", pop_def$label %||% "custom"))
    cat(sprintf("Exposures:   %s\n", paste(exp_cols, collapse = ", ")))
    cat(sprintf("Outcomes:    %s\n", paste(out_cols, collapse = ", ")))
    cat(sprintf("Covariates:  %s\n\n", paste(cov_cols, collapse = ", ")))
    cat(sprintf("BKMR settings: iter=%d | chains=%d\n\n", n_iter, n_chains))
  }

  ## -- 3. Load database ------------------------------------------------------
  if (verbose) cat("Loading database... ")
  db <- load_nhanes_database(db_path)
  if (verbose) cat(sprintf("%d rows\n\n", nrow(db)))

  ## -- 4. Validate columns exist ---------------------------------------------
  all_req <- unique(c(exp_cols, out_cols, cov_cols))
  missing <- setdiff(all_req, names(db))
  if (length(missing) > 0)
    stop(sprintf(
      "Variable(s) not in dataset: %s. Add them in the Dataset tab.",
      paste(missing, collapse = ", ")))

  ## -- 5. Build base constraints ---------------------------------------------
  cycle_years      <- as.integer(sub("-.*", "", cycles))
  cycle_constraint <- list(make_constraint("year", "%in%", cycle_years))
  cov_not_na       <- lapply(cov_cols, function(col) make_constraint(col, "not_na"))
  exp_not_na       <- lapply(exp_cols, function(col) make_constraint(col, "not_na"))
  base_constraints <- c(pop_def$constraints, cycle_constraint,
                         cov_not_na, exp_not_na)

  ## -- 6. Weight map (from database attribute) --------------------------------
  wt_map <- attr(db, "variable_weight_map") %||% list()

  if (verbose)
    print_weight_map(unique(c(exp_cols, out_cols, cov_cols)), wt_map)

  ## -- 6b. Complete-cases shared SEQN mask -----------------------------------
  shared_seqn <- NULL
  if (isTRUE(complete_cases_only)) {
    all_constraints <- c(base_constraints,
                         lapply(out_cols, function(v) make_constraint(v, "not_na")))
    pool_all    <- as.data.frame(apply_population_filter(db, all_constraints))
    shared_seqn <- pool_all$seqn
    if (verbose)
      cat(sprintf("Complete-cases mode: requiring non-missing for all %d exposures + %d outcomes\n\n",
                  length(exp_cols), length(out_cols)))
  }

  ## Weight builder — generalises build_pair_weight() for multiple exposures.
  ## Priority: PFAS subsample > other subsample > MEC (wtmec2yr).
  ## Falls back to WTINT2YR when all variables are demographic-only.
  build_bkmr_weight <- function(data, exposures, outcome, covariates = character(0)) {
    build_analysis_weight(data, c(exposures, outcome, covariates), wt_map)
  }

  ## -- 7. Identify zscore variables ------------------------------------------
  zscore_vars <- intersect(
    c(exp_cols, out_cols, cov_cols),
    names(Filter(function(e) isTRUE(e$is_zscore), get_custom_variables()))
  )

  ## -- 8. Set up parallel plan if requested ---------------------------------
  if (n_chains > 1L) {
    if (!requireNamespace("bkmrhat", quietly = TRUE))
      stop("Package 'bkmrhat' is required for parallel chains. Install with: install.packages('bkmrhat')")
    if (!requireNamespace("future", quietly = TRUE))
      stop("Package 'future' is required for parallel chains. Install with: install.packages('future')")
    future::plan(future::multisession, workers = n_chains)
    on.exit(future::plan(future::sequential), add = TRUE)
    message(sprintf("[BKMR] Parallel plan active: %d workers (%s)",
                    n_chains, class(future::plan())[1]))
  }

  ## When complete_cases_only, pre-compute one shared weight from ALL outcomes
  ## so every outcome uses the identical (most restrictive) weight map.
  shared_bkmr_wt_info <- NULL
  if (!is.null(shared_seqn) && length(out_cols) > 0) {
    shared_bkmr_data    <- as.data.frame(apply_population_filter(db,
      c(base_constraints, lapply(out_cols, function(v) make_constraint(v, "not_na")))))
    shared_bkmr_wt_info <- build_bkmr_weight(shared_bkmr_data, exp_cols, out_cols[1],
                                              c(cov_cols, out_cols[-1]))
    if (verbose)
      cat(sprintf("Shared weight (complete-cases): %s across %d cycle(s)\n\n",
                  shared_bkmr_wt_info$raw_cols, shared_bkmr_wt_info$n_cycles))
  }

  ## -- 9. Fit one BKMR model per outcome -------------------------------------
  bkmr_results    <- list()
  bkmr_demo_store <- list()  ## per-outcome analytic data for demographics
  last_data       <- NULL

  for (oi in seq_along(out_cols)) {
    outcome <- out_cols[oi]
    if (!is.null(progress_callback))
      progress_callback(oi, length(out_cols), outcome)
    if (verbose) cat(sprintf("\n── BKMR: outcome = %s ──\n", outcome))

    out_constraints <- c(base_constraints,
                          list(make_constraint(outcome, "not_na")))
    bkmr_data <- as.data.frame(apply_population_filter(db, out_constraints))

    if (!is.null(shared_seqn))
      bkmr_data <- bkmr_data[bkmr_data$seqn %in% shared_seqn, , drop = FALSE]

    if (verbose) {
      print_filter_trace(bkmr_data)
      cat(sprintf("  Analytic N: %d\n", nrow(bkmr_data)))
    }

    if (length(zscore_vars) > 0)
      bkmr_data <- apply_zscore_vars(bkmr_data, zscore_vars)

    wt_info              <- if (!is.null(shared_bkmr_wt_info)) shared_bkmr_wt_info else
      build_bkmr_weight(bkmr_data, exp_cols, outcome, cov_cols)
    bkmr_data$pooled_weight <- wt_info$weight

    if (verbose)
      cat(sprintf("Weight: %s / %d cycle(s)\n",
                  paste(wt_info$raw_cols, collapse = "+"),
                  wt_info$n_cycles))

    bkmr_results[[outcome]] <- run_bkmr(
      data       = bkmr_data,
      outcome    = outcome,
      exposures  = exp_cols,
      covariates = cov_cols,
      weight_col = "pooled_weight",
      n_iter     = n_iter,
      varsel     = TRUE,
      seed       = seed,
      n_chains   = n_chains
    )

    if (verbose)
      cat(sprintf("Done. N used = %d · iterations = %d\n",
                  bkmr_results[[outcome]]$n, n_iter))

    bkmr_demo_store[[outcome]] <- bkmr_data
    last_data <- bkmr_data
  }

  ## -- 9. Demographics -------------------------------------------------------
  ## For complete_cases_only: all outcomes share the same SEQN set, so use
  ## the first outcome's data. Otherwise use the last (already in last_data).
  pop_data <- if (isTRUE(complete_cases_only) && length(out_cols) > 0)
    bkmr_demo_store[[out_cols[1]]]
  else if (!is.null(last_data))
    last_data
  else
    as.data.frame(apply_population_filter(db, base_constraints))

  ## Aggregate convergence across outcomes (worst-case R-hat, min ESS)
  all_conv <- Filter(Negate(is.null),
                     lapply(bkmr_results, `[[`, "convergence"))
  conv_summary <- if (length(all_conv) > 0) do.call(rbind, all_conv) else NULL

  invisible(list(
    type         = "bkmr",
    bkmr_results = bkmr_results,
    convergence  = conv_summary,
    n_chains     = n_chains,
    pair_demographics = if (isTRUE(complete_cases_only) || length(out_cols) <= 1) {
      list(list(
        label  = if (isTRUE(complete_cases_only)) "All (complete cases)" else "All",
        n      = nrow(pop_data),
        tables = build_demographic_table(pop_data,
                   covariates = cov_cols,
                   outcomes   = out_cols,
                   exposures  = exp_cols)
      ))
    } else {
      lapply(out_cols, function(outcome) {
        demo_data <- bkmr_demo_store[[outcome]] %||% pop_data
        list(
          label  = outcome,
          n      = nrow(demo_data),
          tables = build_demographic_table(demo_data,
                     covariates = cov_cols,
                     outcomes   = outcome,
                     exposures  = exp_cols)
        )
      })
    },
    n            = nrow(pop_data),
    population   = pop_def$label %||% "custom",
    cycles       = cycles
  ))
}
