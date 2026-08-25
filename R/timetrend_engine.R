## ============================================================================
## timetrend_engine.R
##
## Time-trend analysis using a two-layer approach:
##   PLOT  — survey-weighted means per cycle via svyby() on a pooled design,
##            giving proper design-based SEs for error bars.
##   TREND — svyglm(log(outcome) ~ mid_year [+ covariates], design = pooled)
##            on person-level data, giving a design-based slope with correct
##            df and p-value.  mid_year = cycle_start_year + 1 (midpoint of
##            the 2-year NHANES cycle).
##
## Covariates are included in the svyglm trend model only; per-cycle means
## (svyby) remain unadjusted for cleaner visualization.
##
## Missing cycles at the edges are excluded from both layers.
## Missing middle cycles have no persons contributing, so the svyglm slope
## naturally spans the gap via the continuous mid_year predictor.
## They appear as hollow zero markers in the plot only.
##
## Depends on globals from cli.R:
##   resolve_colnames, %||%, load_nhanes_database, apply_population_filter,
##   make_constraint, resolve_population_def, build_demographic_table,
##   WEIGHT_PRIORITY, print_filter_trace, build_survey_design
## ============================================================================

library(survey)

.fmt3 <- function(x) formatC(x, digits = 3, format = "g")

## Wrap a variable name in the appropriate transform expression for a formula.
.tf_expr <- function(var, tf) {
  switch(tf,
    "ln"    = sprintf("log(%s)",   var),
    "log10" = sprintf("log10(%s)", var),
    "log2"  = sprintf("log2(%s)",  var),
    var   ## "none" or unrecognised — use raw variable
  )
}

## Back-transform a vector of model predictions to the original scale.
.tf_back <- function(x, tf) {
  switch(tf,
    "ln"    = exp(x),
    "log10" = 10^x,
    "log2"  = 2^x,
    x       ## "none" — already on original scale
  )
}

## Convert slope to % change per year (log transforms) or leave as Δ/year (none).
.tf_pct <- function(beta, tf) {
  switch(tf,
    "ln"    = (exp(beta)   - 1) * 100,
    "log10" = (10^beta     - 1) * 100,
    "log2"  = (2^beta      - 1) * 100,
    beta    ## "none" — absolute change per year
  )
}

#' Run a time-trend analysis across NHANES cycles.
#'
#' Uses a two-layer approach:
#' \enumerate{
#'   \item \strong{Per-cycle means} — survey-weighted means per cycle via
#'     \code{svyby()} on a pooled design, giving proper design-based SEs for
#'     plotting error bars.
#'   \item \strong{Overall trend} — weighted least squares (WLS) on the
#'     cycle-level means using \code{1/SE^2} as weights, regressing the
#'     (optionally transformed) mean on \code{mid_year} (cycle start + 1).
#'     The slope is back-transformed to a percent change per year when
#'     \code{pct_change = TRUE}.
#' }
#' Covariates are included in the svyglm trend model only; per-cycle means
#' remain unadjusted. Also exports Joinpoint-compatible cycle-mean tables for
#' external joinpoint regression.
#'
#' @param variables character vector of variable names to trend over time.
#'   One model is fit per variable.
#' @param population named string from \code{population_definitions} or a list
#'   with \code{$label} and \code{$constraints}.
#' @param cycles character vector of NHANES cycle labels defining the time span.
#' @param db_path path to the pre-built database RDS file.
#' @param covariates character vector of covariate names to adjust for in the
#'   svyglm trend model. Defaults to none (unadjusted trend).
#' @param pct_change logical. If \code{TRUE} (default), the outcome is
#'   log-transformed before fitting the slope and the result is reported as
#'   percent change per year. If \code{FALSE}, the slope is reported as an
#'   absolute change per year on the original scale.
#' @param complete_cases_only logical. If \code{TRUE}, restricts to participants
#'   non-missing on all variables simultaneously.
#' @param verbose logical. Print filter traces, weight audit, cycle-by-cycle
#'   changes, and trend summaries to the console.
#'
#' @return An invisible list with components:
#' \describe{
#'   \item{type}{\code{"timetrend"}.}
#'   \item{trend_results}{named list (by variable). Each entry contains:
#'     \code{cycle_data} (data frame of per-cycle means, SEs, CIs, and status),
#'     \code{trend_line} (fitted trend line for plotting),
#'     \code{joinpoint_data} (Joinpoint-ready export),
#'     \code{pct_change_per_year}, \code{pct_change_ci}, \code{trend_p},
#'     \code{weight_col}, and \code{n_analytic}.}
#'   \item{pair_demographics}{Table-1 summaries, one per variable.}
#'   \item{n}{unweighted analytic N for the first variable.}
#'   \item{population}{population label string.}
#'   \item{cycles}{cycles used.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- run_timetrend_analysis(
#'   variables  = c("pfoa", "pfos", "pfhxs", "pfna"),
#'   population = "all_adults",
#'   cycles     = c("1999-2000", "2003-2004", "2005-2006", "2007-2008",
#'                  "2009-2010", "2011-2012", "2013-2014", "2015-2016",
#'                  "2017-2018"),
#'   db_path    = here::here("data", "nhanes_pool.rds"),
#'   pct_change = TRUE
#' )
#' # Percent change per year and p-value for PFOA
#' r <- result$trend_results$pfoa
#' cat(sprintf("PFOA trend: %.2f%% per year (p = %.4f)\n",
#'             r$pct_change_per_year, r$trend_p))
#'
#' # Per-cycle means for plotting
#' print(r$cycle_data[, c("year", "mean", "ci_low", "ci_high")])
#' }
run_timetrend_analysis <- function(variables, population, cycles, db_path,
                                    covariates          = character(0),
                                    pct_change          = TRUE,
                                    complete_cases_only = FALSE,
                                    dietary_weight      = FALSE,
                                    verbose             = TRUE) {

  pop_def  <- resolve_population_def(population)
  var_cols <- resolve_colnames(variables)
  cov_cols <- if (length(covariates) > 0) resolve_colnames(covariates) else character(0)

  if (verbose) {
    cat(sprintf("Time-trend — Population: %s\n", pop_def$label %||% "custom"))
    cat(sprintf("Variables:   %s\n",   paste(var_cols, collapse=", ")))
    cat(sprintf("Covariates:  %s\n\n", if (length(cov_cols) > 0)
                                          paste(cov_cols, collapse=", ") else "(none)"))
  }

  db <- load_nhanes_database(db_path)
  missing_v <- setdiff(c(var_cols, cov_cols), names(db))
  if (length(missing_v) > 0)
    stop(sprintf("Variable(s) not in dataset: %s.", paste(missing_v, collapse=", ")))

  if (verbose)
    cat(sprintf("Database loaded: %d rows\n\n", nrow(db)))

  cycle_years <- as.integer(sub("-.*", "", cycles))
  wt_map      <- attr(db, "variable_weight_map") %||% list()

  ## Covariate not_na constraints applied to every variable's filter
  cov_not_na <- lapply(cov_cols, function(col) make_constraint(col, "not_na"))

  ## In complete-cases mode, require non-missing for every variable up front
  ## so all models share the same analytic sample.
  all_var_not_na <- if (isTRUE(complete_cases_only))
    lapply(var_cols, function(v) make_constraint(v, "not_na"))
  else
    list()

  if (verbose && isTRUE(complete_cases_only))
    cat(sprintf("Complete-cases mode: requiring non-missing for all %d variable(s)\n\n",
                length(var_cols)))

  ## Base constraints: population + cycle years + covariate completeness
  ## + (optionally) all-variable completeness for complete-cases mode
  base_constraints <- c(
    pop_def$constraints,
    list(make_constraint("year", "%in%", cycle_years)),
    cov_not_na,
    all_var_not_na
  )

  ## ── Weight maps block (matches linear regression verbose output) ─────────
  if (verbose) {
    all_analysis_vars <- unique(c(var_cols, cov_cols))
    cat("=== Weight maps (variable -> cycle -> weight column) ===\n")
    for (v in all_analysis_vars) {
      vm <- wt_map[[tolower(v)]] %||% list()
      if (length(vm) == 0) {
        cat(sprintf("  %-20s  (no subsample weight — wtmec2yr fallback for all cycles)\n", v))
      } else {
        years_sorted <- sort(as.integer(names(vm)))
        entries <- paste(
          sapply(years_sorted, function(yr) sprintf("%d:%s", yr, vm[[as.character(yr)]])),
          collapse = "  ")
        cat(sprintf("  %-20s  %s\n", v, entries))
      }
    }
    cat("========================================================\n\n")
  }

  ## When complete_cases_only, pre-compute one shared weight from ALL variables
  ## so every variable uses the identical (most restrictive) weight map.
  shared_wt_info <- NULL
  if (isTRUE(complete_cases_only) && length(var_cols) > 0) {
    shared_data    <- as.data.frame(apply_population_filter(db, base_constraints))
    shared_wt_info <- build_pair_weight(
      shared_data,
      exposure       = var_cols[1],
      outcome        = var_cols[1],
      covariates     = c(cov_cols, var_cols[-1]),
      wt_map         = wt_map,
      dietary_weight = dietary_weight
    )
    if (verbose)
      cat(sprintf("Shared weight (complete-cases): %s across %d cycle(s)\n\n",
                  shared_wt_info$raw_cols, shared_wt_info$n_cycles))
  }

  trend_results      <- list()
  analytic_data_store <- list()

  for (var in var_cols) {
    if (verbose) cat(sprintf("\n── Time-trend: %s ──\n", var))

    constraints_i <- c(base_constraints, list(make_constraint(var, "not_na")))
    data_i <- as.data.frame(apply_population_filter(db, constraints_i))

    ## ── Filter trace ─────────────────────────────────────────────────────────
    if (verbose) {
      cat(sprintf("  Total DB rows: %d\n", nrow(db)))
      print_filter_trace(data_i)
      cat(sprintf("  Analytic N after all filters: %d\n\n", nrow(data_i)))
    }

    if (nrow(data_i) == 0) {
      warning(sprintf("No data for time-trend of '%s'", var))
      next
    }

    ## ── Survey weight selection ──────────────────────────────────────────────
    wt_info       <- if (!is.null(shared_wt_info)) shared_wt_info else
                       build_analysis_weight(data_i, c(var, cov_cols), wt_map,
                                            dietary_weight = dietary_weight)
    combined_wt   <- wt_info$weight
    raw_wt_col    <- wt_info$raw_cols
    cycle_wt_used <- wt_info$cycle_wt_used
    n_cyc         <- wt_info$n_cycles
    data_i$pooled_weight <- combined_wt
    data_i$mid_year      <- data_i$year + 1L

    ## ── Pooled survey design ─────────────────────────────────────────────────
    data_valid <- data_i[!is.na(data_i$pooled_weight) & data_i$pooled_weight > 0 &
                           !is.na(data_i[[var]]), ]
    if (nrow(data_valid) < 10) {
      warning(sprintf("Insufficient valid observations for time-trend of '%s'", var))
      next
    }
    analytic_data_store[[var]] <- data_valid

    ## ── Weight audit ─────────────────────────────────────────────────────────
    if (verbose) {
      hdr_w <- 78
      cat(sprintf("  Weight audit — %s (most restrictive: %s / %d cycles):\n",
                  var, raw_wt_col, n_cyc))
      cat(sprintf("  %-12s %-20s %6s %12s %18s %22s\n",
                  "Cycle", "Weight col", "N", "Mean wt",
                  "Mean (SD)", "Geom Mean (IQR)"))
      cat(sprintf("  %s\n", paste(rep("-", hdr_w), collapse="")))
      for (yr in sort(unique(data_valid$year))) {
        yr_mask  <- data_valid$year == yr
        yr_wt    <- data_valid$pooled_weight[yr_mask]
        yr_vals  <- data_valid[[var]][yr_mask]
        n_yr     <- sum(yr_mask)
        wt_src   <- cycle_wt_used[[as.character(yr)]] %||% "wtmec2yr (fallback)"
        mean_wt  <- mean(yr_wt, na.rm = TRUE)
        mean_sd  <- sprintf("%s (%s)", .fmt3(mean(yr_vals)), .fmt3(sd(yr_vals)))
        pos_vals <- yr_vals[yr_vals > 0]
        if (length(pos_vals) > 0) {
          gm  <- exp(mean(log(pos_vals)))
          iqr <- quantile(pos_vals, probs = c(0.25, 0.75))
          gm_iqr <- sprintf("%s (%s–%s)", .fmt3(gm), .fmt3(iqr[1]), .fmt3(iqr[2]))
        } else {
          gm_iqr <- "—"
        }
        cat(sprintf("  %-12s %-20s %6d %12.1f %18s %22s\n",
                    paste0(yr, "-", yr+1), wt_src, n_yr, mean_wt,
                    mean_sd, gm_iqr))
      }
      cat(sprintf("  %s\n\n", paste(rep("-", hdr_w), collapse="")))
    }
    pooled_svy <- build_survey_design(data_valid)

    ## ── Per-cycle means for plot via svyby ───────────────────────────────────
    by_fml      <- as.formula(sprintf("~%s", var))
    cycle_means <- tryCatch(
      svyby(by_fml, ~year, design=pooled_svy, FUN=svymean,
            na.rm=TRUE, keep.names=FALSE),
      error=function(e) NULL
    )
    if (is.null(cycle_means) || nrow(cycle_means) < 2) {
      warning(sprintf("Insufficient cycle data for time-trend of '%s'", var))
      next
    }

    obs_years_raw <- as.integer(as.character(cycle_means$year))
    cycle_se      <- as.numeric(SE(cycle_means))
    n_per_cycle   <- tapply(!is.na(data_valid[[var]]), data_valid$year, sum)

    cycle_df <- data.frame(
      year   = obs_years_raw,
      mean   = as.numeric(cycle_means[[var]]),
      se     = cycle_se,
      n      = as.integer(n_per_cycle[as.character(obs_years_raw)]),
      status = "observed",
      stringsAsFactors = FALSE
    )
    cycle_df <- cycle_df[!is.na(cycle_df$mean) & !is.na(cycle_df$se) &
                           cycle_df$se > 0, ]
    if (nrow(cycle_df) < 2) {
      warning(sprintf("Insufficient cycle data for time-trend of '%s'", var))
      next
    }

    cycle_df$ci_low  <- cycle_df$mean - 1.96 * cycle_df$se
    cycle_df$ci_high <- cycle_df$mean + 1.96 * cycle_df$se

    ## ── Cycle-by-cycle change ─────────────────────────────────────────────────
    if (verbose && nrow(cycle_df) >= 2) {
      obs_cd_tmp <- cycle_df[cycle_df$status == "observed", ]
      obs_cd_tmp <- obs_cd_tmp[order(obs_cd_tmp$year), ]
      if (nrow(obs_cd_tmp) >= 2) {
        chg_label <- if (isTRUE(pct_change)) "% Change" else "Delta"
        cat(sprintf("  Cycle-by-cycle %s:\n", chg_label))
        cat(sprintf("  %-20s %12s\n", "Transition", chg_label))
        cat(sprintf("  %s\n", paste(rep("-", 34), collapse="")))
        for (i in seq_len(nrow(obs_cd_tmp) - 1)) {
          yr_a <- obs_cd_tmp$year[i];   m_a <- obs_cd_tmp$mean[i]
          yr_b <- obs_cd_tmp$year[i+1]; m_b <- obs_cd_tmp$mean[i+1]
          chg <- if (isTRUE(pct_change))
            ((m_b - m_a) / m_a) * 100
          else
            m_b - m_a
          fmt <- if (isTRUE(pct_change)) "%+.2f%%" else "%+.4f"
          transition <- sprintf("%d-%d → %d-%d", yr_a, yr_a+1, yr_b, yr_b+1)
          cat(sprintf("  %-20s %12s\n", transition, sprintf(fmt, chg)))
        }
        cat(sprintf("  %s\n\n", paste(rep("-", 34), collapse="")))
      }
    }

    ## ── Expand to all requested cycles (zero markers for missing) ────────────
    obs_years  <- cycle_df$year
    miss_years <- setdiff(cycle_years, obs_years)
    if (length(miss_years) > 0) {
      min_obs <- min(obs_years); max_obs <- max(obs_years)
      miss_rows <- lapply(miss_years, function(yr) {
        data.frame(
          year     = yr,
          mean     = 0,
          se       = NA_real_,
          n        = 0L,
          status   = if (yr < min_obs || yr > max_obs) "missing_edge" else "missing_middle",
          ci_low   = NA_real_,
          ci_high  = NA_real_,
          stringsAsFactors = FALSE
        )
      })
      cycle_df <- rbind(cycle_df, do.call(rbind, miss_rows))
      cycle_df <- cycle_df[order(cycle_df$year), ]
    }

    ## ── Transform: ln for % change, none for absolute Δ/year ─────────────────
    tf      <- if (isTRUE(pct_change)) "ln" else "none"
    tf_expr <- .tf_expr(var, tf)
    needs_positive <- tf == "ln"
    data_pos <- if (needs_positive)
      data_valid[data_valid[[var]] > 0, ]
    else
      data_valid

    ## ── Transformed means per cycle for OLS / Joinpoint (svyby on tf_expr) ──
    tf_fml  <- as.formula(sprintf("~%s", tf_expr))
    pos_svy <- build_survey_design(data_pos)
    tf_by   <- tryCatch(
      svyby(tf_fml, ~year, design=pos_svy, FUN=svymean,
            na.rm=TRUE, keep.names=FALSE),
      error=function(e) NULL
    )

    ## Build Joinpoint export: raw arithmetic means + transformed means + SEs.
    obs_cd <- cycle_df[cycle_df$status == "observed", ]
    joinpoint_data <- NULL
    if (!is.null(tf_by)) {
      tf_years   <- as.integer(as.character(tf_by$year))
      tf_mean    <- as.numeric(tf_by[[tf_expr]])
      tf_se      <- as.numeric(SE(tf_by))
      joinpoint_data <- data.frame(
        Cycle          = paste0(tf_years, "-", tf_years + 1),
        Mid_Year       = tf_years + 1L,
        N              = as.integer(n_per_cycle[as.character(tf_years)]),
        Raw_Mean       = obs_cd$mean[match(tf_years, obs_cd$year)],
        Raw_SE         = obs_cd$se[ match(tf_years, obs_cd$year)],
        Trans_Mean     = tf_mean,
        Trans_SE       = tf_se,
        Transform      = tf,
        stringsAsFactors = FALSE
      )
    }

    ## ── Trend estimation ─────────────────────────────────────────────────────
    pct_change_per_year <- NA_real_
    trend_p             <- NA_real_
    trend_ci            <- c(NA_real_, NA_real_)
    trend_line_df       <- NULL
    obs_mid             <- obs_cd$year + 1L

    ## Aggregate OLS on survey-weighted geometric cycle means; df = K-2.
    if (!is.null(tf_by) && nrow(tf_by) >= 2) {
      ols_df <- data.frame(
        mid_year = as.integer(as.character(tf_by$year)) + 1L,
        tf_mean  = as.numeric(tf_by[[tf_expr]]),
        tf_se    = as.numeric(SE(tf_by)),
        stringsAsFactors = FALSE
      )
      ols_df <- ols_df[!is.na(ols_df$tf_mean) & !is.na(ols_df$tf_se) &
                         ols_df$tf_se > 0, ]
      if (nrow(ols_df) >= 2) {
        ols_fit <- tryCatch(
          lm(tf_mean ~ mid_year, data=ols_df, weights=1/tf_se^2),
          error=function(e) NULL
        )
        if (!is.null(ols_fit)) {
          sm_ols <- summary(ols_fit)$coefficients
          if ("mid_year" %in% rownames(sm_ols)) {
            beta         <- sm_ols["mid_year", "Estimate"]
            se_b         <- sm_ols["mid_year", "Std. Error"]
            ols_df_resid <- nrow(ols_df) - 2L
            t_crit       <- qt(0.975, df = ols_df_resid)
            trend_p             <- sm_ols["mid_year", ncol(sm_ols)]
            pct_change_per_year <- .tf_pct(beta, tf)
            trend_ci            <- .tf_pct(c(beta - t_crit*se_b, beta + t_crit*se_b), tf)

            if (length(obs_mid) >= 2) {
              intercept_adj <- coef(ols_fit)["(Intercept)"]
              mid_seq <- seq(min(obs_mid), max(obs_mid), length.out=80)
              trend_line_df <- data.frame(
                year   = mid_seq - 1,
                fitted = .tf_back(intercept_adj + beta * mid_seq, tf)
              )
            }
          }
        }
      }
    }

    change_label <- if (tf == "none") "Δ/year (absolute)" else "% Change/Year"
    if (verbose) {
      cat(sprintf("  Transform: %s | N cycles: %d | %s: %.4f (p = %.4f)\n",
                  tf,
                  sum(cycle_df$status == "observed"),
                  change_label,
                  ifelse(is.na(pct_change_per_year), 0, pct_change_per_year),
                  ifelse(is.na(trend_p), 1, trend_p)))
    }

    trend_results[[var]] <- list(
      variable            = var,
      transform           = tf,
      change_label        = change_label,
      cycle_data          = cycle_df,
      trend_line          = trend_line_df,
      joinpoint_data      = joinpoint_data,
      n_analytic          = nrow(data_pos),
      pct_change_per_year = pct_change_per_year,
      pct_change_ci       = trend_ci,
      trend_p             = trend_p,
      weight_col          = raw_wt_col
    )
  }

  pair_demographics <- if (length(analytic_data_store) == 0) {
    list(list(label = "All", n = 0L, tables = NULL))
  } else {
    lapply(names(analytic_data_store), function(v) {
      list(
        label  = v,
        n      = nrow(analytic_data_store[[v]]),
        tables = build_demographic_table(analytic_data_store[[v]],
                   covariates = cov_cols,
                   outcomes   = v,
                   exposures  = NULL)
      )
    })
  }

  invisible(list(
    type              = "timetrend",
    trend_results     = trend_results,
    pair_demographics = pair_demographics,
    n                 = if (length(analytic_data_store) > 0)
                          nrow(analytic_data_store[[1]]) else 0L,
    population        = pop_def$label %||% "custom",
    cycles            = cycles
  ))
}
