## ============================================================================
## mixture_engine.R
##
## WQS (gWQS) and qgcomp mixture-regression pipelines.
## Both methods treat all exposures jointly, fitting one model per outcome.
## They are commonly paired with BKMR in mixture-epidemiology papers.
##
## Depends on globals from cli.R / bkmr_engine.R:
##   resolve_colnames, PFAS_VARS, WEIGHT_PRIORITY, population_definitions,
##   %||%, load_nhanes_database, apply_population_filter, make_constraint,
##   print_filter_trace, get_custom_variables, apply_zscore_vars,
##   build_demographic_table, resolve_population_def
## ============================================================================

library(gWQS)
library(qgcomp)

## Shared weight builder (mirrors bkmr_engine.R — kept local to avoid
## cross-file coupling; the closure captures wt_map via argument).
.mixture_weight <- function(data, exposures, outcome, wt_map, covariates = character(0),
                             dietary_weight = FALSE) {
  res <- build_analysis_weight(data, c(exposures, outcome, covariates), wt_map,
                               dietary_weight = dietary_weight)
  list(weight     = res$weight,
       cycle_cols = res$cycle_wt_used,
       n_cycles   = res$n_cycles)
}


## ============================================================================
## WQS
## ============================================================================

## Thin wrapper so gwqs() future workers cannot capture run_wqs_analysis's
## environment (outcome_frames, pop_data, etc.) via lexical scoping.
.call_gwqs <- function(fml, mix_name, data, q, b, b1_pos, family, weights, seed) {
  gwqs(fml, mix_name = mix_name, data = data,
       q = q, b = b, b1_pos = b1_pos,
       family = family, weights = weights, seed = seed)
}

#' Run a Weighted Quantile Sum (WQS) mixture analysis.
#'
#' Fits one WQS model per outcome using \code{gWQS::gwqs()}. All exposures are
#' treated jointly as the mixture; the WQS index coefficient represents the
#' combined mixture effect in the constrained direction. Parallel bootstrap
#' workers are managed by \pkg{future}; large databases are dropped from scope
#' before the bootstrap loop to stay within \code{future.globals.maxSize}.
#'
#' @param exposures character vector of exposure variable names.
#' @param outcomes character vector of outcome variable names.
#' @param covariates character vector of covariate names included in every model.
#' @param population named string from \code{population_definitions} or a list
#'   with \code{$label} and \code{$constraints}.
#' @param cycles character vector of NHANES cycle labels to include.
#' @param db_path path to the pre-built database RDS file.
#' @param directions character vector of mixture directions, one per outcome:
#'   \code{"positive"} (default) or \code{"negative"}. Recycled if length-1.
#' @param n_quantiles integer number of quantiles for exposure ranking.
#'   Defaults to \code{4} (quartiles).
#' @param n_boot integer number of bootstrap samples. Defaults to \code{100}.
#' @param seed integer random seed passed to \code{gwqs()} and \code{set.seed()}.
#' @param complete_cases_only logical. If \code{TRUE}, restricts to participants
#'   non-missing on all exposures and all outcomes simultaneously.
#' @param verbose logical. Print filter traces and per-outcome summaries.
#'
#' @return An invisible list with components:
#' \describe{
#'   \item{type}{\code{"wqs"}.}
#'   \item{wqs_results}{named list (by outcome) each containing \code{estimate},
#'     \code{se}, \code{ci_low}, \code{ci_high}, \code{p_value}, \code{direction},
#'     \code{weights} (data frame of component weights), \code{n}, and \code{fit}
#'     (raw \code{gwqs} object).}
#'   \item{pair_demographics}{Table-1 summaries for the analytic population.}
#'   \item{n}{unweighted analytic N.}
#'   \item{population}{population label string.}
#'   \item{cycles}{cycles used.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- run_wqs_analysis(
#'   exposures   = c("pfoa", "pfos", "pfhxs", "pfna"),
#'   outcomes    = c("ldl", "non_hdl"),
#'   covariates  = c("age", "sex_bin", "bmi", "education", "income_ratio"),
#'   population  = "cvd_medicated_fasting",
#'   cycles      = c("2003-2004", "2005-2006", "2007-2008", "2009-2010",
#'                   "2011-2012", "2013-2014", "2015-2016", "2017-2018"),
#'   db_path     = here::here("data", "nhanes_pool.rds"),
#'   directions  = "positive",
#'   n_boot      = 200
#' )
#' result$wqs_results$ldl$estimate   # WQS index coefficient for LDL
#' result$wqs_results$ldl$weights    # component weights data frame
#' }
run_wqs_analysis <- function(exposures, outcomes, covariates, population, cycles,
                               db_path,
                               directions        = "positive",
                               n_quantiles       = 4,
                               n_boot            = 100,
                               seed              = 42,
                               complete_cases_only = FALSE,
                               dietary_weight    = FALSE,
                               verbose           = TRUE) {

  pop_def  <- resolve_population_def(population)
  exp_cols <- resolve_colnames(exposures)
  out_cols <- resolve_colnames(outcomes)
  cov_cols <- resolve_colnames(covariates)

  ## Recycle directions to match number of outcomes
  directions <- rep_len(directions, length(out_cols))
  names(directions) <- out_cols

  if (verbose) {
    cat(sprintf("Population:  %s\n", pop_def$label %||% "custom"))
    cat(sprintf("Exposures:   %s\n", paste(exp_cols, collapse=", ")))
    cat(sprintf("Outcomes:    %s\n", paste(out_cols, collapse=", ")))
    cat(sprintf("Covariates:  %s\n\n", paste(cov_cols, collapse=", ")))
    cat(sprintf("WQS settings: q=%d | boot=%d\n\n", n_quantiles, n_boot))
  }

  if (verbose) cat("Loading database... ")
  db <- load_nhanes_database(db_path)
  if (verbose) cat(sprintf("%d rows\n\n", nrow(db)))
  missing <- setdiff(unique(c(exp_cols, out_cols, cov_cols)), names(db))
  if (length(missing) > 0)
    stop(sprintf("Variable(s) not in dataset: %s.", paste(missing, collapse=", ")))

  cycle_years      <- as.integer(sub("-.*", "", cycles))
  base_constraints <- c(pop_def$constraints,
                         list(make_constraint("year", "%in%", cycle_years)),
                         lapply(cov_cols, function(v) make_constraint(v, "not_na")),
                         lapply(exp_cols, function(v) make_constraint(v, "not_na")))
  wt_map <- attr(db, "variable_weight_map") %||% list()

  if (verbose)
    print_weight_map(unique(c(exp_cols, out_cols, cov_cols)), wt_map)

  ## Pre-filter every outcome dataset and the demographics frame WHILE db is
  ## still in scope, then drop db entirely. gwqs() uses future for parallelism
  ## and serialises every object reachable from the calling environment into
  ## each worker closure — if db is still alive that's ~150 MB × 3 closures =
  ## 450+ MB, which exceeds future's 500 MB default limit. Removing db before
  ## the loop ensures the workers only see the small analytic data frames.
  model_cols_base <- c(exp_cols, cov_cols, "pooled_weight")

  ## When complete_cases_only is TRUE, compute a shared SEQN mask: rows that
  ## are non-NA for every exposure AND every outcome.  Each per-outcome frame is
  ## then restricted to that shared set so all models use the same participants.
  shared_seqn <- NULL
  if (isTRUE(complete_cases_only)) {
    all_constraints <- c(base_constraints,
                         lapply(out_cols, function(v) make_constraint(v, "not_na")))
    pool_all <- as.data.frame(apply_population_filter(db, all_constraints))
    shared_seqn <- pool_all$seqn
    if (verbose)
      cat(sprintf("Complete-cases mode: requiring non-missing for all %d exposures + %d outcomes\n\n",
                  length(exp_cols), length(out_cols)))
  }

  ## When complete_cases_only, pre-compute one shared weight from ALL outcomes
  ## so every outcome frame uses the identical (most restrictive) weight map.
  shared_wqs_wt_info <- NULL
  if (!is.null(shared_seqn) && length(out_cols) > 0) {
    shared_wqs_data    <- as.data.frame(apply_population_filter(db,
      c(base_constraints, lapply(out_cols, function(v) make_constraint(v, "not_na")))))
    shared_wqs_wt_info <- .mixture_weight(shared_wqs_data, exp_cols, out_cols[1],
                                           wt_map, c(cov_cols, out_cols[-1]),
                                           dietary_weight = dietary_weight)
    if (verbose)
      cat(sprintf("Shared weight (complete-cases): %s across %d cycle(s)\n\n",
                  shared_wqs_wt_info$cycle_cols[[1]], shared_wqs_wt_info$n_cycles))
  }

  outcome_frames <- lapply(out_cols, function(outcome) {
    if (verbose) cat(sprintf("\n── WQS filter trace: %s ──\n", outcome))
    data_i <- as.data.frame(
      apply_population_filter(db,
        c(base_constraints, list(make_constraint(outcome, "not_na"))))
    )
    if (!is.null(shared_seqn))
      data_i <- data_i[data_i$seqn %in% shared_seqn, , drop = FALSE]
    if (verbose) {
      print_filter_trace(data_i)
      cat(sprintf("  Analytic N: %d\n", nrow(data_i)))
    }
    wt_info              <- if (!is.null(shared_wqs_wt_info)) shared_wqs_wt_info else
      .mixture_weight(data_i, exp_cols, outcome, wt_map, cov_cols,
                      dietary_weight = dietary_weight)
    data_i$pooled_weight <- wt_info$weight
    if (verbose) {
      cat(sprintf("  weights (divided by %d cycle(s)):\n", wt_info$n_cycles))
      for (yr in names(wt_info$cycle_cols))
        cat(sprintf("    cycle %s: %s\n", yr, wt_info$cycle_cols[[yr]]))
    }

    ## Trim to only the columns the model needs — exposure + outcome + covariates
    ## + weight. Nothing else. This is what gets copied by each bootstrap worker.
    keep <- intersect(c(model_cols_base, outcome), names(data_i))
    cc_mask <- complete.cases(data_i[, keep, drop=FALSE])
    data_cc  <- data_i[cc_mask, keep, drop=FALSE]
    ## Keep full-column analytic rows for demographics (computed after rm(db))
    demo_data <- data_i[cc_mask, , drop=FALSE]
    list(data_cc = data_cc, n_raw = nrow(data_i), demo_data = demo_data)
  })
  names(outcome_frames) <- out_cols

  ## Drop the full database — must happen before gwqs() is ever called
  rm(db); gc()

  wqs_results <- list()

  for (outcome in out_cols) {
    if (verbose) cat(sprintf("\n── WQS: %s ──\n", outcome))
    data_cc <- outcome_frames[[outcome]]$data_cc
    direction_i <- directions[[outcome]]
    if (verbose) cat(sprintf("N = %d | direction: %s\n", nrow(data_cc), direction_i))

    cov_rhs <- if (length(cov_cols) > 0) paste(cov_cols, collapse=" + ") else "1"
    fml     <- as.formula(sprintf("%s ~ wqs + %s", outcome, cov_rhs))

    set.seed(seed)
    old_max <- getOption("future.globals.maxSize")
    options(future.globals.maxSize = Inf)
    fit <- tryCatch(
      .call_gwqs(fml, mix_name = exp_cols, data = data_cc,
                 q = n_quantiles, b = n_boot,
                 b1_pos = (direction_i == "positive"),
                 family = "gaussian", weights = "pooled_weight", seed = seed),
      error = function(e)
        stop(sprintf("gwqs() failed for %s: %s", outcome, conditionMessage(e)))
    )
    options(future.globals.maxSize = old_max)

    ## Extract WQS index coefficient
    sm     <- summary(fit)$coef
    wqs_r  <- sm["wqs", , drop = FALSE]
    est    <- as.numeric(wqs_r[, "Estimate"])
    se_val <- as.numeric(wqs_r[, "Std. Error"])
    pval   <- as.numeric(wqs_r[, ncol(wqs_r)])

    ## Extract component weights — gWQS stores them in $final_weights
    wt_df <- tryCatch({
      fw    <- fit$final_weights
      nm_c  <- grep("mix_name|name|variable", names(fw), ignore.case=TRUE, value=TRUE)[1]
      wt_c  <- grep("mean_weight|weight|estimate", names(fw), ignore.case=TRUE, value=TRUE)[1]
      if (!is.na(nm_c) && !is.na(wt_c))
        data.frame(exposure=fw[[nm_c]], weight=as.numeric(fw[[wt_c]]),
                   stringsAsFactors=FALSE)
      else NULL
    }, error = function(e) NULL)

    ## Fallback: equal weights if extraction failed
    if (is.null(wt_df))
      wt_df <- data.frame(exposure=exp_cols,
                           weight=rep(1/length(exp_cols), length(exp_cols)))

    wqs_results[[outcome]] <- list(
      outcome   = outcome,
      estimate  = est,
      se        = se_val,
      ci_low    = est - 1.96 * se_val,
      ci_high   = est + 1.96 * se_val,
      p_value   = pval,
      direction = direction_i,
      weights   = wt_df,
      n         = nrow(data_cc),
      fit       = fit
    )
    if (verbose) cat(sprintf("WQS β = %.4f (p = %.4f)\n", est, pval))
  }

  invisible(list(
    type         = "wqs",
    wqs_results  = wqs_results,
    pair_demographics = if (isTRUE(complete_cases_only) || length(out_cols) <= 1) {
      demo_data <- outcome_frames[[out_cols[1]]]$demo_data
      list(list(
        label  = if (isTRUE(complete_cases_only)) "All (complete cases)" else "All",
        n      = nrow(demo_data),
        tables = build_demographic_table(demo_data,
                   covariates = cov_cols,
                   outcomes   = out_cols,
                   exposures  = exp_cols)
      ))
    } else {
      lapply(out_cols, function(outcome) {
        demo_data <- outcome_frames[[outcome]]$demo_data
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
    n            = nrow(outcome_frames[[out_cols[1]]]$demo_data),
    population   = pop_def$label %||% "custom",
    cycles       = cycles
  ))
}


## ============================================================================
## qgcomp
## ============================================================================

#' Run a quantile g-computation (qgcomp) mixture analysis.
#'
#' Fits one \code{qgcomp.glm.ee()} model per outcome using M-estimation
#' (estimating equations) with a sandwich variance estimator. All exposures
#' are ranked into \code{n_quantiles} groups before fitting; the mixture
#' effect \eqn{\psi} represents the expected change in the outcome for a
#' simultaneous one-quantile increase in all exposures. Survey weights are
#' passed as regression weights (partial adjustment; full survey-design-aware
#' qgcomp is not yet available in the released package).
#'
#' @param exposures character vector of exposure variable names.
#' @param outcomes character vector of outcome variable names.
#' @param covariates character vector of covariate names included in every model.
#' @param population named string from \code{population_definitions} or a list
#'   with \code{$label} and \code{$constraints}.
#' @param cycles character vector of NHANES cycle labels to include.
#' @param db_path path to the pre-built database RDS file.
#' @param n_quantiles integer number of quantile groups for exposure ranking.
#'   Defaults to \code{4}.
#' @param out_transforms character vector of outcome transforms, one per
#'   outcome: \code{"none"} (default), \code{"log10"}, \code{"log2"},
#'   \code{"sqrt"}, or \code{"ln"}. Recycled if length-1.
#' @param exp_transforms character vector of exposure transforms applied to the
#'   raw exposure values before quantile ranking. Same options as
#'   \code{out_transforms}. Recycled if length-1.
#' @param seed integer random seed.
#' @param complete_cases_only logical. If \code{TRUE}, restricts to participants
#'   non-missing on all exposures and all outcomes simultaneously.
#' @param verbose logical. Print filter traces, weight summaries, and
#'   per-outcome component weights.
#'
#' @return An invisible list with components:
#' \describe{
#'   \item{type}{\code{"qgcomp"}.}
#'   \item{qgcomp_results}{named list (by outcome) each containing \code{psi}
#'     (mixture effect estimate), \code{ci_low}, \code{ci_high}, \code{p_value},
#'     \code{weights} (data frame with \code{exposure}, \code{weight},
#'     \code{direction}), \code{n}, and \code{fit} (raw \code{qgcompfit} object).}
#'   \item{pair_demographics}{Table-1 summaries for the analytic population.}
#'   \item{n}{unweighted analytic N.}
#'   \item{population}{population label string.}
#'   \item{cycles}{cycles used.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- run_qgcomp_analysis(
#'   exposures  = c("pfoa", "pfos", "pfhxs", "pfna"),
#'   outcomes   = c("ldl", "non_hdl"),
#'   covariates = c("age", "sex_bin", "bmi", "education", "income_ratio"),
#'   population = "cvd_medicated_fasting",
#'   cycles     = c("2003-2004", "2005-2006", "2007-2008", "2009-2010",
#'                  "2011-2012", "2013-2014", "2015-2016", "2017-2018"),
#'   db_path    = here::here("data", "nhanes_pool.rds")
#' )
#' result$qgcomp_results$ldl$psi      # mixture effect estimate
#' result$qgcomp_results$ldl$weights  # directional component weights
#' }
run_qgcomp_analysis <- function(exposures, outcomes, covariates, population, cycles,
                                  db_path,
                                  n_quantiles         = 4,
                                  out_transforms      = NULL,
                                  exp_transforms      = NULL,
                                  seed                = 42,
                                  use_bootstrap       = FALSE,
                                  n_boot              = 200,
                                  complete_cases_only = FALSE,
                                  dietary_weight      = FALSE,
                                  verbose             = TRUE) {

  pop_def  <- resolve_population_def(population)
  exp_cols <- resolve_colnames(exposures)
  out_cols <- resolve_colnames(outcomes)
  cov_cols <- resolve_colnames(covariates)

  ## Recycle transform vectors to match number of outcomes / exposures
  out_transforms <- rep_len(out_transforms %||% "none", length(out_cols))
  exp_transforms <- rep_len(exp_transforms %||% "none", length(exp_cols))
  names(out_transforms) <- out_cols
  names(exp_transforms) <- exp_cols

  if (verbose) {
    cat(sprintf("Population:  %s\n", pop_def$label %||% "custom"))
    cat(sprintf("Exposures:   %s\n", paste(exp_cols, collapse=", ")))
    cat(sprintf("Outcomes:    %s\n", paste(out_cols, collapse=", ")))
    cat(sprintf("Covariates:  %s\n\n", paste(cov_cols, collapse=", ")))
    method_label <- if (use_bootstrap) sprintf("bootstrap (B=%d)", n_boot) else "EE (sandwich SE)"
    cat(sprintf("qgcomp settings: q=%d | method: %s\n\n", n_quantiles, method_label))
  }

  if (verbose) cat("Loading database... ")
  db <- load_nhanes_database(db_path)
  if (verbose) cat(sprintf("%d rows\n\n", nrow(db)))
  missing <- setdiff(unique(c(exp_cols, out_cols, cov_cols)), names(db))
  if (length(missing) > 0)
    stop(sprintf("Variable(s) not in dataset: %s.", paste(missing, collapse=", ")))

  cycle_years      <- as.integer(sub("-.*", "", cycles))
  base_constraints <- c(pop_def$constraints,
                         list(make_constraint("year", "%in%", cycle_years)),
                         lapply(cov_cols, function(v) make_constraint(v, "not_na")),
                         lapply(exp_cols, function(v) make_constraint(v, "not_na")))
  wt_map <- attr(db, "variable_weight_map") %||% list()

  if (verbose)
    print_weight_map(unique(c(exp_cols, out_cols, cov_cols)), wt_map)

  ## qgcomp.glm.ee() uses M-estimation (estimating equations) with a sandwich
  ## variance estimator — more robust than the delta-method (noboot) and avoids
  ## Monte Carlo noise from bootstrap.  Survey-design-aware qgcomp.svy() is not
  ## yet available in the released package; pooled_weight is passed as regression
  ## weights to partially account for NHANES sampling.

  shared_seqn <- NULL
  if (isTRUE(complete_cases_only)) {
    all_constraints <- c(base_constraints,
                         lapply(out_cols, function(v) make_constraint(v, "not_na")))
    pool_all <- as.data.frame(apply_population_filter(db, all_constraints))
    shared_seqn <- pool_all$seqn
    if (verbose)
      cat(sprintf("Complete-cases mode: requiring non-missing for all %d exposures + %d outcomes\n\n",
                  length(exp_cols), length(out_cols)))
  }

  ## When complete_cases_only, pre-compute one shared weight from ALL outcomes
  ## so every outcome uses the identical (most restrictive) weight map.
  shared_qgcomp_wt_info <- NULL
  if (!is.null(shared_seqn) && length(out_cols) > 0) {
    shared_qgcomp_data    <- as.data.frame(apply_population_filter(db,
      c(base_constraints, lapply(out_cols, function(v) make_constraint(v, "not_na")))))
    shared_qgcomp_wt_info <- .mixture_weight(shared_qgcomp_data, exp_cols, out_cols[1],
                                              wt_map, c(cov_cols, out_cols[-1]),
                                              dietary_weight = dietary_weight)
    if (verbose)
      cat(sprintf("Shared weight (complete-cases): %s across %d cycle(s)\n\n",
                  shared_qgcomp_wt_info$cycle_cols[[1]], shared_qgcomp_wt_info$n_cycles))
  }

  qgcomp_results  <- list()
  qgcomp_demo_store <- list()  ## per-outcome analytic data for demographics

  for (outcome in out_cols) {
    if (verbose) cat(sprintf("\n── qgcomp: %s ──\n", outcome))
    data_i <- as.data.frame(
      apply_population_filter(db,
        c(base_constraints, list(make_constraint(outcome, "not_na"))))
    )
    if (!is.null(shared_seqn))
      data_i <- data_i[data_i$seqn %in% shared_seqn, , drop = FALSE]
    if (verbose) {
      print_filter_trace(data_i)
      cat(sprintf("  Analytic N: %d\n", nrow(data_i)))
    }
    wt_info              <- if (!is.null(shared_qgcomp_wt_info)) shared_qgcomp_wt_info else
      .mixture_weight(data_i, exp_cols, outcome, wt_map, cov_cols,
                      dietary_weight = dietary_weight)
    data_i$pooled_weight <- wt_info$weight
    if (verbose) {
      cat(sprintf("  weights (divided by %d cycle(s)):\n", wt_info$n_cycles))
      for (yr in names(wt_info$cycle_cols))
        cat(sprintf("    cycle %s: %s\n", yr, wt_info$cycle_cols[[yr]]))
    }
    if (verbose) cat(sprintf("N (pre complete-cases) = %d\n", nrow(data_i)))

    keep_cols <- intersect(c(exp_cols, cov_cols, outcome, "pooled_weight",
                              "sdmvpsu", "sdmvstra"), names(data_i))
    data_cc   <- data_i[complete.cases(data_i[, keep_cols, drop = FALSE]), ]
    qgcomp_demo_store[[outcome]] <- data_cc  ## full columns, analytic N
    if (verbose) {
      cat(sprintf("N (complete cases)     = %d\n", nrow(data_cc)))
      w <- data_cc$pooled_weight
      w <- w[!is.na(w) & w > 0]
      if (length(w) > 0) {
        qs <- quantile(w, c(0, .25, .5, .75, 1))
        cat(sprintf(
          "  pooled_weight summary:  min=%.1f  Q1=%.1f  median=%.1f  Q3=%.1f  max=%.1f\n",
          qs[1], qs[2], qs[3], qs[4], qs[5]))
        cat(sprintf(
          "  weight outlier ratio:   max/median = %.2f  (>20 suggests extreme outlier)\n",
          qs[5] / qs[3]))
      }
    }

    ## Build data_fit: a copy of data_cc with all requested transforms applied.
    ## Exposure transforms are applied to the actual column values so qgcomp's
    ## internal quantile-ranking operates on the transformed scale.
    ## Outcome zscore is applied here too (no in-formula equivalent exists).
    .apply_col_transform <- function(x, tr) switch(tr %||% "none",
      "log10"  = log10(x),
      "log2"   = log2(x),
      "sqrt"   = sqrt(x),
      "ln"     = log(x),
      "zscore" = { m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE); (x - m) / s },
      x)

    data_fit <- data_cc
    for (v in exp_cols) {
      tr <- exp_transforms[[v]]
      if (!is.null(tr) && tr != "none") {
        data_fit[[v]] <- .apply_col_transform(data_cc[[v]], tr)
        if (verbose) cat(sprintf("  Exposure transform: %s(%s) applied\n", tr, v))
      }
    }

    out_tr <- out_transforms[[outcome]]
    if (!is.null(out_tr) && out_tr == "zscore") {
      data_fit[[outcome]] <- .apply_col_transform(data_cc[[outcome]], "zscore")
      if (verbose) cat(sprintf("  Outcome transform: zscore(%s) applied to data\n", outcome))
    }

    ## Build formula. Exposure columns in data_fit are already transformed, so
    ## the formula uses their plain names. Outcome log/sqrt go into the formula.
    .term <- function(var, tr) switch(tr %||% "none",
      "log10"  = sprintf("log10(%s)", var),
      "log2"   = sprintf("log2(%s)",  var),
      "sqrt"   = sprintf("sqrt(%s)",  var),
      "ln"     = sprintf("log(%s)",   var),
      var)
    out_term <- .term(outcome, out_tr)
    all_rhs  <- paste(c(exp_cols, cov_cols), collapse = " + ")
    fml      <- as.formula(sprintf("%s ~ %s", out_term, all_rhs))

    method_used <- if (use_bootstrap) sprintf("bootstrap (B=%d)", n_boot) else "EE"
    message(sprintf("[qgcomp] %s — method: %s", outcome, method_used))
    if (verbose) cat(sprintf("  Formula: %s\n", deparse(fml)))

    set.seed(seed)
    fit <- tryCatch({
      if (use_bootstrap) {
        qgcomp.glm.boot(fml, expnms = exp_cols, data = data_fit,
                        q = n_quantiles, family = gaussian(),
                        weights = data_fit$pooled_weight,
                        B = n_boot, seed = seed)
      } else {
        qgcomp.glm.ee(fml, expnms = exp_cols, data = data_fit,
                      q = n_quantiles, family = gaussian(),
                      weights = data_fit$pooled_weight)
      }
    }, error = function(e)
      stop(sprintf("qgcomp failed for %s: %s", outcome, conditionMessage(e))))

    ## Extract mixture effect and CI — slots differ between EE and bootstrap.
    psi <- as.numeric(fit$psi)[1L]
    if (use_bootstrap) {
      ## qgcomp.glm.boot stores bootstrap-derived CI in fit$ci (named vector)
      ## and p-value in fit$pval["psi1"].
      ci_lo <- tryCatch(as.numeric(fit$ci)[1L], error = function(e) NA_real_)
      ci_hi <- tryCatch(as.numeric(fit$ci)[2L], error = function(e) NA_real_)
      pval  <- tryCatch(as.numeric(fit$pval["psi1"]), error = function(e) {
        if (!is.na(ci_lo) && !is.na(ci_hi) && !is.na(psi)) {
          se_est <- (ci_hi - ci_lo) / (2 * 1.96)
          2 * pnorm(-abs(psi / se_est))
        } else NA_real_
      })
    } else {
      ## qgcomp.glm.ee() — derive CI and p from psi and var.psi (sandwich SE).
      se_psi <- sqrt(as.numeric(fit$var.psi)[1L])
      ci_lo  <- psi - 1.96 * se_psi
      ci_hi  <- psi + 1.96 * se_psi
      pval   <- 2 * pnorm(-abs(psi / se_psi))
    }

    ## qgcomp.glm.ee() stores directional weights in pos.weights / neg.weights
    ## (same slots as noboot). Fall back to the coef vector if slots are absent.
    pos_w <- fit$pos.weights %||% numeric(0)
    neg_w <- fit$neg.weights %||% numeric(0)
    if (length(pos_w) == 0 && length(neg_w) == 0) {
      ## Derive from the underlying coefficient vector (qgcomp.glm.ee does not
      ## populate pos.weights/neg.weights). Normalize within each direction so
      ## weights sum to 1, matching the convention of the bootstrap fit slots.
      raw_coef <- tryCatch(
        coef(fit$fit)[exp_cols],
        error = function(e) setNames(rep(NA_real_, length(exp_cols)), exp_cols)
      )
      raw_coef[is.na(raw_coef)] <- 0
      pos_w <- raw_coef[raw_coef >= 0]
      neg_w <- abs(raw_coef[raw_coef < 0])
      pos_sum <- sum(pos_w)
      neg_sum <- sum(neg_w)
      if (pos_sum > 0) pos_w <- pos_w / pos_sum
      if (neg_sum > 0) neg_w <- neg_w / neg_sum
    }
    wt_df <- data.frame(
      exposure  = c(names(pos_w), names(neg_w)),
      weight    = c(as.numeric(pos_w), -as.numeric(neg_w)),
      direction = c(rep("positive", length(pos_w)), rep("negative", length(neg_w))),
      stringsAsFactors = FALSE
    )

    qgcomp_results[[outcome]] <- list(
      outcome      = outcome,
      psi          = psi,
      ci_low       = ci_lo,
      ci_high      = ci_hi,
      p_value      = pval,
      weights      = wt_df,
      n            = nrow(data_cc),
      method       = method_used,
      survey_weighted = FALSE,
      fit          = fit
    )
    if (verbose) {
      se_label <- if (use_bootstrap) sprintf("bootstrap B=%d", n_boot) else "EE sandwich SE"
      cat(sprintf("qgcomp ψ = %.4f (95%% CI: %.4f, %.4f; p = %.4f) [%s]\n",
                  psi, ci_lo, ci_hi, pval, se_label))

      cat("\n  Exposure weights:\n")
      if (nrow(wt_df) == 0) {
        cat("    (none extracted)\n")
      } else {
        pos_rows <- wt_df[wt_df$direction == "positive", , drop = FALSE]
        neg_rows <- wt_df[wt_df$direction == "negative", , drop = FALSE]
        pos_rows <- pos_rows[order(-abs(pos_rows$weight)), , drop = FALSE]
        neg_rows <- neg_rows[order(-abs(neg_rows$weight)), , drop = FALSE]
        if (nrow(pos_rows) > 0) {
          cat("    Positive:\n")
          for (j in seq_len(nrow(pos_rows)))
            cat(sprintf("      %-35s  wt = %6.4f\n",
                        pos_rows$exposure[j], abs(pos_rows$weight[j])))
        }
        if (nrow(neg_rows) > 0) {
          cat("    Negative:\n")
          for (j in seq_len(nrow(neg_rows)))
            cat(sprintf("      %-35s  wt = %6.4f\n",
                        neg_rows$exposure[j], abs(neg_rows$weight[j])))
        }
      }
    }
  }

  invisible(list(
    type           = "qgcomp",
    qgcomp_results = qgcomp_results,
    pair_demographics = if (isTRUE(complete_cases_only) || length(out_cols) <= 1) {
      demo_data <- qgcomp_demo_store[[out_cols[1]]]
      list(list(
        label  = if (isTRUE(complete_cases_only)) "All (complete cases)" else "All",
        n      = nrow(demo_data),
        tables = build_demographic_table(demo_data,
                   covariates = cov_cols,
                   outcomes   = out_cols,
                   exposures  = exp_cols)
      ))
    } else {
      lapply(out_cols, function(outcome) {
        demo_data <- qgcomp_demo_store[[outcome]]
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
    n              = nrow(qgcomp_demo_store[[out_cols[1]]]),
    population     = pop_def$label %||% "custom",
    cycles         = cycles
  ))
}
