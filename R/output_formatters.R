## ============================================================================
## output_formatters.R
##
## Owns: converting standardized results objects (from regression_engine.R)
## into structures ready for a specific output type. Knows nothing about how
## results were computed — pure presentation layer.
##
## Each formatter takes either one results object or a named list of them
## (as returned by run_regression_grid) and returns a tidy data frame, since
## that's the most flexible handoff point — ggplot2, a Shiny table, or a
## CLI print statement can all consume a tidy data frame.
## ============================================================================

library(dplyr)
library(ggplot2)

#' Flatten a list of results objects (from run_regression_grid) into one
#' tidy data frame, one row per outcome x exposure combination, pulling
#' just the headline exposure coefficient from each.
#'
#' This is the shared foundation every other formatter builds on.
results_grid_to_tidy <- function(results_list) {
  rows <- lapply(names(results_list), function(key) {
    res <- results_list[[key]]
    if (is.null(res)) return(NULL)

    ## Spline models: report global Wald p and non-linearity p — not individual
    ## basis-column coefficients, which have no interpretable meaning on their own.
    if (isTRUE(res$model_type == "svyglm_spline")) {
      pval <- res$spline_global_p %||% NA_real_
      return(data.frame(
        outcome          = res$outcome,
        exposure         = res$exposure,
        stratum          = res$stratum %||% "Full",
        estimate         = NA_real_,   ## no single β for a spline
        ci_low           = NA_real_,
        ci_high          = NA_real_,
        p_value          = pval,
        nonlin_p         = res$spline_nonlin_p  %||% NA_real_,
        aic_linear       = res$spline_aic_linear %||% NA_real_,
        aic_spline       = res$spline_aic_spline %||% NA_real_,
        n                = res$n,
        significant      = isTRUE(pval < 0.05),
        exp_coef         = FALSE,
        is_spline        = TRUE,
        stringsAsFactors = FALSE
      ))
    }

    ## ref_lowest logistic: return one row per exposure level (ref + all contrasts)
    if (isTRUE(res$ref_lowest)) {
      pat      <- paste0("^", regexEscape(res$exposure), "=")
      level_rows <- res$coefficients[grepl(pat, res$coefficients$term), ]
      if (nrow(level_rows) == 0) return(NULL)

      is_log <- isTRUE(res$exp_coef)

      ## Reference row (OR = 1, no CI, no p)
      ref_label <- paste0(res$exposure, "=", res$ref_min_label)
      ref_df <- data.frame(
        outcome     = res$outcome,
        exposure    = res$exposure,
        stratum     = ref_label,
        estimate    = if (is_log) 1 else 0,
        ci_low      = NA_real_,
        ci_high     = NA_real_,
        p_value     = NA_real_,
        nonlin_p    = NA_real_,
        aic_linear  = NA_real_,
        aic_spline  = NA_real_,
        n           = res$n,
        significant = FALSE,
        exp_coef    = is_log,
        is_spline   = FALSE,
        stringsAsFactors = FALSE
      )

      contrast_dfs <- lapply(seq_len(nrow(level_rows)), function(j) {
        r    <- level_rows[j, ]
        est  <- r$estimate
        cilo <- r$ci_low
        cihi <- r$ci_high
        pv   <- r$p_value
        if (is_log) { est <- exp(est); cilo <- exp(cilo); cihi <- exp(cihi) }
        data.frame(
          outcome     = res$outcome,
          exposure    = res$exposure,
          stratum     = r$term,
          estimate    = est,
          ci_low      = cilo,
          ci_high     = cihi,
          p_value     = pv,
          nonlin_p    = NA_real_,
          aic_linear  = NA_real_,
          aic_spline  = NA_real_,
          n           = res$n,
          significant = isTRUE(pv < 0.05),
          exp_coef    = is_log,
          is_spline   = FALSE,
          stringsAsFactors = FALSE
        )
      })

      return(dplyr::bind_rows(c(list(ref_df), contrast_dfs)))
    }

    coef_row <- tryCatch(
      get_exposure_coefficient(res),
      error = function(e) {
        warning(sprintf("Could not extract coefficient for %s: %s", key, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(coef_row) || nrow(coef_row) == 0) return(NULL)

    ## Defensive column access — coef_row should have estimate/ci_low/ci_high/p_value
    est    <- coef_row$estimate[1]
    ci_lo  <- coef_row$ci_low[1]
    ci_hi  <- coef_row$ci_high[1]
    pval   <- coef_row$p_value[1]

    if (any(is.na(c(est, ci_lo, ci_hi, pval)))) {
      warning(sprintf("NA values in coefficients for %s — skipping", key))
      return(NULL)
    }

    ## For logistic models, report on OR scale
    is_log <- isTRUE(res$exp_coef)
    if (is_log) {
      est   <- exp(est)
      ci_lo <- exp(ci_lo)
      ci_hi <- exp(ci_hi)
    }

    data.frame(
      outcome          = res$outcome,
      exposure         = res$exposure,
      stratum          = res$stratum %||% "Full",
      estimate         = est,
      ci_low           = ci_lo,
      ci_high          = ci_hi,
      p_value          = pval,
      nonlin_p         = NA_real_,
      aic_linear       = NA_real_,
      aic_spline       = NA_real_,
      n                = res$n,
      significant      = pval < 0.05,
      exp_coef         = is_log,
      is_spline        = FALSE,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(Filter(Negate(is.null), rows))
}

#' Forest plot: one panel per outcome, one row per exposure, mirroring the
#' structure of CVD_Fasting_Lipid_Regression.m's forest plot but built with
#' ggplot2 instead of raw MATLAB figure/axes calls.
#'
#' @param results_list output of run_regression_grid()
#' @return a ggplot object (facetted by outcome)
make_forest_plot <- function(results_list) {
  tidy <- results_grid_to_tidy(results_list)

  ggplot(tidy, aes(x = estimate, y = exposure, color = significant)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2, linewidth = 0.8) +
    geom_point(size = 3) +
    scale_color_manual(values = c(`TRUE` = "#D9534F", `FALSE` = "#999999"),
                        labels = c(`TRUE` = "p < 0.05", `FALSE` = "n.s."),
                        name = NULL) +
    facet_wrap(~outcome, scales = "free_x") +
    labs(x = "Coefficient (95% CI)", y = NULL,
         title = "Exposure-outcome associations") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
}

#' Standard results table — one row per outcome x exposure, formatted for
#' direct display or export (e.g. to a Shiny DT table or a CSV for
#' supplementary materials).
make_results_table <- function(results_list, digits = 3) {
  tidy <- results_grid_to_tidy(results_list)

  fmt_p <- function(p) ifelse(is.na(p), "—",
                       ifelse(p < 0.001, "<0.001", sprintf("%.4f", p)))
  stars <- function(p) ifelse(is.na(p), "",
                       ifelse(p < 0.001, "***",
                       ifelse(p < 0.01,  "**",
                       ifelse(p < 0.05,  "*", ""))))

  ## Spline results: show global p and non-linearity p instead of β
  if (nrow(tidy) > 0 && "is_spline" %in% names(tidy) && all(isTRUE(tidy$is_spline) | tidy$is_spline %in% TRUE)) {
    out <- data.frame(
      outcome          = tidy$outcome,
      exposure         = tidy$exposure,
      `Global p`       = fmt_p(tidy$p_value),
      ` `              = stars(tidy$p_value),
      `Nonlinearity p` = fmt_p(tidy$nonlin_p),
      `  `             = stars(tidy$nonlin_p),
      `AIC (linear)`   = round(as.numeric(tidy$aic_linear), 1),
      `AIC (spline)`   = round(as.numeric(tidy$aic_spline), 1),
      N                = tidy$n,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    return(out)
  }

  ## Linear / logistic results: standard β or OR table
  if (nrow(tidy) == 0) return(data.frame())

  has_levels <- "stratum" %in% names(tidy) &&
                any(!is.na(tidy$stratum) & tidy$stratum != "Full")

  tidy <- dplyr::mutate(tidy,
    coef_label   = ifelse(isTRUE(exp_coef), "OR", "β"),
    estimate_fmt = ifelse(is.na(estimate), "Ref",
                     sprintf(paste0("%.", digits, "f"), estimate)),
    ci_fmt       = ifelse(is.na(ci_low), "—",
                     sprintf(paste0("[%.", digits, "f, %.", digits, "f]"), ci_low, ci_high)),
    p_fmt        = fmt_p(p_value),
    sig_stars    = stars(p_value)
  )

  if (has_levels) {
    out <- tidy[, c("outcome","exposure","stratum","coef_label","estimate_fmt",
                     "ci_fmt","p_fmt","sig_stars","n")]
  } else {
    out <- tidy[, c("outcome","exposure","coef_label","estimate_fmt",
                     "ci_fmt","p_fmt","sig_stars","n")]
  }
  names(out)[names(out) == "estimate_fmt"] <- "estimate"
  out
}

#' Heatmap: outcome x exposure grid, cell color = effect size, cell label =
#' significance. Useful for the "many outcomes x many exposures" case where
#' a forest plot would have too many panels to read.
make_heatmap <- function(results_list) {
  tidy <- results_grid_to_tidy(results_list)

  ggplot(tidy, aes(x = exposure, y = outcome, fill = estimate)) +
    geom_tile(color = "white") +
    geom_text(aes(label = {
                    s <- ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", "")))
                    paste0(sprintf("%.2f", estimate), s)
                  }),
              size = 3) +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                          midpoint = 0, name = "Estimate") +
    labs(x = NULL, y = NULL, title = "Exposure-outcome effect size heatmap") +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

#' Combined results table — full population first, then Q1 (Ref) / Q2 / Q3 / Q4
#' rows per outcome x exposure pair. quartile_results_list is a named list of
#' plain data frames produced by run_quartile_factor_regression().
make_combined_results_table <- function(results_list, quartile_results_list, digits = 3) {
  fmt_p <- function(p) ifelse(is.na(p), "—",
                       ifelse(p < 0.001, "<0.001", sprintf("%.4f", p)))
  stars <- function(p) ifelse(is.na(p), "",
                       ifelse(p < 0.001, "***",
                       ifelse(p < 0.01,  "**",
                       ifelse(p < 0.05,  "*", ""))))

  main_tidy <- results_grid_to_tidy(results_list)
  if (nrow(main_tidy) == 0) return(data.frame())

  ## Combine quartile data frames (already have outcome/exposure/stratum/estimate/…)
  q_rows <- do.call(rbind, quartile_results_list)

  stratum_order <- c("Full", "Q1 (Ref)", "Q2", "Q3", "Q4")

  fmt_block <- function(df) {
    df$coef_label   <- ifelse(isTRUE(df$exp_coef) | df$exp_coef %in% TRUE, "OR", "β")
    df$estimate_fmt <- ifelse(is.na(df$estimate), "Ref",
                        sprintf(paste0("%.", digits, "f"), df$estimate))
    df$ci_fmt       <- ifelse(is.na(df$ci_low), "—",
                        sprintf(paste0("[%.", digits, "f, %.", digits, "f]"),
                                df$ci_low, df$ci_high))
    df$p_fmt        <- fmt_p(df$p_value)
    df$sig_stars    <- stars(df$p_value)
    df
  }

  main_fmt <- fmt_block(main_tidy)
  q_fmt    <- if (!is.null(q_rows) && nrow(q_rows) > 0) fmt_block(q_rows) else NULL

  combined <- dplyr::bind_rows(main_fmt, q_fmt)
  combined$stratum_rank <- match(combined$stratum, stratum_order)
  combined$stratum_rank[is.na(combined$stratum_rank)] <- 99L
  combined <- combined[order(combined$outcome, combined$exposure, combined$stratum_rank), ]
  combined$stratum_rank <- NULL

  out <- combined[, c("outcome","exposure","stratum","coef_label",
                       "estimate_fmt","ci_fmt","p_fmt","sig_stars","n")]
  names(out)[names(out) == "estimate_fmt"] <- "estimate"
  out
}

#' XY scatter of two continuous variables, colored by a grouping variable
#' (e.g. medication class) — for exploratory plots outside the regression
#' grid structure, operating directly on a filtered dataset rather than a
#' results object.
make_xy_plot <- function(filtered_data, x_var, y_var, group_var = NULL) {
  p <- ggplot(filtered_data, aes(x = .data[[x_var]], y = .data[[y_var]]))
  if (!is.null(group_var)) {
    p <- p + aes(color = .data[[group_var]])
  }
  p + geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(x = x_var, y = y_var) +
    theme_minimal(base_size = 11)
}
