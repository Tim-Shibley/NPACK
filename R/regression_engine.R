## ============================================================================
## regression_engine.R
##
## Owns: running a regression given a survey design, an outcome, and a
## covariate list, and returning a standardized results object. Knows
## nothing about populations or how the design was built — just consumes it.
##
## The standardized results object is the contract every output_formatters.R
## function depends on. Keep this stable; everything downstream assumes
## this exact shape.
## ============================================================================

library(survey)
library(dplyr)
library(splines)

#' Standard results object:
#' list(
#'   outcome       = character,
#'   exposure      = character (primary variable of interest, e.g. "PFOA"),
#'   covariates    = character vector,
#'   n             = integer (unweighted N used in model),
#'   coefficients  = data frame: term, estimate, std_error, ci_low, ci_high, p_value
#'   model_type    = "svyglm_gaussian" | "svyglm_binomial" | ...
#'   weight_group  = character (carried through from the design, for audit trail)
#' )

#' Run a survey-weighted linear regression.
#'
#' @param design svydesign object from build_survey_design()
#' @param outcome canonical name of the outcome variable
#' @param exposure canonical name of the primary exposure (the variable
#'        whose coefficient is the headline result — e.g. "PFOA")
#' @param covariates character vector of additional covariate canonical names
#' @param log10_exposure whether to log10-transform the exposure before
#'        fitting (defaults to the registry's log10_transform flag if NULL)
#' @param log10_exposure TRUE for log10 (default for PFAS), or a transform
#'        string: "log10", "log2", "sqrt", FALSE/NULL for raw
run_weighted_regression <- function(design, outcome, exposure, covariates,
                                     log10_exposure    = NULL,
                                     outcome_transform = NULL,
                                     family            = gaussian(),
                                     spline_df         = NULL,
                                     spline_knots      = NULL,
                                     ref_lowest        = FALSE) {
  ## Resolve transform — accept boolean (backward compat) or string
  transform <- if (isTRUE(log10_exposure) || identical(log10_exposure, "log10")) {
    "log10"
  } else if (identical(log10_exposure, "log2")) {
    "log2"
  } else if (identical(log10_exposure, "sqrt")) {
    "sqrt"
  } else if (identical(log10_exposure, "ln")) {
    "ln"
  } else {
    NULL
  }

  exposure_term <- switch(transform %||% "raw",
    "log10" = sprintf("log10(%s)", exposure),
    "log2"  = sprintf("log2(%s)",  exposure),
    "sqrt"  = sprintf("sqrt(%s)",  exposure),
    "ln"    = sprintf("log(%s)",   exposure),
    exposure  ## raw
  )

  ## Save inner term (with transform but before spline wrapping) so we can
  ## build the coefficient-name grep pattern later, e.g. "log10(lbxpfoa)".
  inner_exposure_term <- exposure_term

  ## Wrap exposure in natural spline if spline_df or spline_knots requested
  use_spline <- (!is.null(spline_knots) && length(spline_knots) >= 1) ||
                (!is.null(spline_df) && spline_df >= 2)
  if (use_spline) {
    if (!is.null(spline_knots) && length(spline_knots) >= 1) {
      knot_str <- paste(spline_knots, collapse = ", ")
      exposure_term <- sprintf("ns(%s, knots=c(%s))", exposure_term, knot_str)
    } else {
      exposure_term <- sprintf("ns(%s, df=%d)", exposure_term, as.integer(spline_df))
    }
  }

  ## Resolve family — accept string shortcuts (needed early for ref_lowest check)
  fam_obj <- if (is.character(family)) {
    switch(tolower(family),
      "gaussian"     = gaussian(),
      "binomial"     = quasibinomial(),
      "quasibinomial"= quasibinomial(),
      "logistic"     = quasibinomial(),
      gaussian()
    )
  } else family

  is_logistic_fam <- inherits(fam_obj, "family") &&
                     fam_obj$family %in% c("binomial","quasibinomial")

  ## If ref_lowest requested: factor the exposure with its minimum value as the
  ## reference level.  Only applies to logistic; ignored for continuous/spline.
  ref_factor_col <- NULL
  ref_min_label  <- NULL
  if (isTRUE(ref_lowest) && is_logistic_fam && !use_spline) {
    raw_vals <- design$variables[[exposure]]
    sorted_lvls <- sort(unique(na.omit(raw_vals)))
    if (length(sorted_lvls) >= 2L) {
      ref_min_label  <- as.character(sorted_lvls[1L])
      ref_factor_col <- paste0(".ref_", exposure)
      design$variables[[ref_factor_col]] <- relevel(
        factor(raw_vals, levels = as.character(sorted_lvls)),
        ref = ref_min_label
      )
      exposure_term       <- ref_factor_col
      inner_exposure_term <- ref_factor_col
    }
  }

  cov_rhs <- if (length(covariates) > 0) paste(covariates, collapse = " + ") else "1"

  outcome_term <- switch(outcome_transform %||% "none",
    "log10" = sprintf("log10(%s)", outcome),
    "log2"  = sprintf("log2(%s)",  outcome),
    "sqrt"  = sprintf("sqrt(%s)",  outcome),
    "ln"    = sprintf("log(%s)",   outcome),
    outcome  ## raw / none / zscore (zscore applied pre-model on the column)
  )

  formula_str   <- sprintf("%s ~ %s + %s", outcome_term, exposure_term, cov_rhs)
  model_formula <- as.formula(formula_str)

  ## Guard: if the base exposure variable is constant in this analytic sample,
  ## svyglm silently drops it from the model (collinear with intercept), causing
  ## a confusing "could not find exposure term" error downstream. Fail early.
  exp_vals <- design$variables[[exposure]]
  if (!is.null(exp_vals) && length(unique(na.omit(exp_vals))) <= 1L) {
    stop(sprintf(
      paste0("Exposure '%s' has zero variance in this analytic sample (all values identical). ",
             "Likely all observations are at the detection limit. ",
             "Cannot fit regression with a constant predictor."),
      exposure))
  }

  fit <- svyglm(model_formula, design = design, family = fam_obj)

  coefs <- summary(fit)$coefficients
  if (is.null(coefs) || nrow(coefs) == 0)
    stop("svyglm returned empty coefficients — check for rank-deficient model")

  ci <- tryCatch(
    confint(fit),
    error = function(e) {
      warning(sprintf("confint() failed for %s ~ %s: %s — using NA CIs",
                       outcome, exposure_term, conditionMessage(e)))
      matrix(NA_real_, nrow=nrow(coefs), ncol=2,
             dimnames=list(rownames(coefs), c("2.5 %","97.5 %")))
    }
  )

  ## Ensure ci has correct dimensions even if confint returned unexpectedly
  if (is.null(ci) || nrow(ci) != nrow(coefs)) {
    ci <- matrix(NA_real_, nrow=nrow(coefs), ncol=2,
                 dimnames=list(rownames(coefs), c("2.5 %","97.5 %")))
  }

  is_logistic <- is_logistic_fam  ## alias (already computed above)

  term_names <- rownames(coefs)

  ## When ref_lowest was applied, coefficient rows are named ".ref_<exposure><level>".
  ## Replace that prefix with a cleaner "<exposure>=" label so the results table
  ## and forest plots show readable level names.
  if (!is.null(ref_factor_col)) {
    term_names <- sub(
      paste0("^", regexEscape(ref_factor_col)),
      paste0(exposure, "="),
      term_names
    )
  }

  coef_df <- data.frame(
    term      = term_names,
    estimate  = coefs[, "Estimate"],
    std_error = coefs[, "Std. Error"],
    ci_low    = ci[, 1],
    ci_high   = ci[, 2],
    p_value   = coefs[, ncol(coefs)],
    row.names = NULL
  )

  ## For logistic: also store exponentiated columns (OR scale)
  if (is_logistic) {
    coef_df$OR      <- exp(coef_df$estimate)
    coef_df$OR_low  <- exp(coef_df$ci_low)
    coef_df$OR_high <- exp(coef_df$ci_high)
  }

  ## ---- Spline-specific outputs ---------------------------------------------
  spline_curve       <- NULL
  spline_global_p    <- NA_real_   ## Wald p-value: joint test of all ns() terms
  spline_nonlin_p    <- NA_real_   ## non-linearity p: spline vs linear model
  spline_aic_linear  <- NA_real_
  spline_aic_spline  <- NA_real_

  if (use_spline) {

    ## Find all ns() basis column indices in the fitted model.
    ## Use fixed=TRUE on the literal "ns(" so R's spacing/formatting around
    ## the = sign (e.g. "df = 3" vs "df=3") cannot break the match.
    ## Then filter to only columns that also contain the raw exposure name,
    ## so we don't accidentally pick up ns() terms for other variables.
    all_coef_names <- names(coef(fit))
    ns_idx <- grep("ns(", all_coef_names, fixed = TRUE)
    if (length(ns_idx) > 0)
      ns_idx <- ns_idx[grepl(exposure, all_coef_names[ns_idx], fixed = TRUE)]

    ## Wald chi-squared test for a subset of model coefficients.
    ## Accepts the model explicitly to avoid any closure-capture ambiguity.
    ## Uses vcov() which incorporates the survey design weighting.
    wald_chi2_p <- function(model, idx) {
      if (length(idx) == 0) return(NA_real_)
      b <- coef(model)[idx]
      if (any(is.na(b))) return(NA_real_)
      V <- vcov(model)[idx, idx, drop = FALSE]
      if (any(!is.finite(V))) return(NA_real_)
      W <- tryCatch(as.numeric(t(b) %*% solve(V) %*% b), error = function(e) NA_real_)
      if (is.null(W) || !is.finite(W)) return(NA_real_)
      pchisq(W, df = length(idx), lower.tail = FALSE)
    }

    ## 1. Global Wald test: are ALL ns() basis coefficients jointly zero?
    spline_global_p <- wald_chi2_p(fit, ns_idx)

    ## 2. Non-linearity test: are the EXTRA df beyond linear jointly zero?
    ##    ns(x, df=k) creates k columns; column 1 ≈ linear trend, columns 2..k
    ##    capture curvature. Testing 2..k asks: does spline beat a straight line?
    spline_nonlin_p <- wald_chi2_p(fit, ns_idx[-1L])

    ## 3. AIC comparison: fit a plain linear model for reference ΔAIC.
    linear_exposure_term <- switch(transform %||% "raw",
      "log10" = sprintf("log10(%s)", exposure),
      "log2"  = sprintf("log2(%s)",  exposure),
      "sqrt"  = sprintf("sqrt(%s)",  exposure),
      "ln"    = sprintf("log(%s)",   exposure),
      exposure
    )
    linear_fml <- as.formula(
      sprintf("%s ~ %s + %s", outcome, linear_exposure_term, cov_rhs))

    fit_lin <- tryCatch(
      svyglm(linear_fml, design = design, family = fam_obj),
      error = function(e) NULL
    )

    if (!is.null(fit_lin)) {
      ## extractAIC() always returns exactly c(edf, AIC) — taking [2L] gives a
      ## guaranteed scalar. AIC() on svyglm can return a multi-element vector
      ## (edf + AIC + other components) which would recycle rows in data.frame().
      spline_aic_linear <- tryCatch(extractAIC(fit_lin)[2L], error = function(e) NA_real_)
      spline_aic_spline <- tryCatch(extractAIC(fit)[2L],     error = function(e) NA_real_)

    }

    ## 4. Predicted values curve (covariates held at median/mode).
    ##    Uses manual matrix arithmetic rather than predict.svyglm newdata,
    ##    which has inconsistent se.fit support across survey package versions.
    spline_curve <- tryCatch({
      dv    <- design$variables
      rng   <- range(dv[[exposure]], na.rm = TRUE)
      x_seq <- seq(rng[1], rng[2], length.out = 100)

      ## Build a minimal prediction frame with just the variables in the formula
      newdata <- dv[rep(1L, 100L), , drop = FALSE]
      newdata[[exposure]] <- x_seq
      for (cv in covariates) {
        col <- dv[[cv]]
        newdata[[cv]] <- if (is.numeric(col)) median(col, na.rm = TRUE) else
                           names(which.max(table(col)))
      }

      ## Build model matrix using the fitted model's terms (applies ns() automatically)
      tt    <- delete.response(terms(fit))
      X_new <- model.matrix(tt, data = newdata)

      ## Point estimates and SE via delta method: Var(Xb) = diag(X V X')
      beta  <- coef(fit)
      V     <- vcov(fit)

      ## Keep only columns present in both X_new and beta (guard against aliased coefs)
      common <- intersect(colnames(X_new), names(beta))
      X_new  <- X_new[, common, drop = FALSE]
      beta   <- beta[common]
      V      <- V[common, common, drop = FALSE]

      yhat  <- as.numeric(X_new %*% beta)
      yvar  <- rowSums((X_new %*% V) * X_new)   ## diag(X V X') without forming n×n matrix
      yse   <- sqrt(pmax(yvar, 0))

      data.frame(
        x     = x_seq,
        y     = yhat,
        lower = yhat - 1.96 * yse,
        upper = yhat + 1.96 * yse
      )
    }, error = function(e) {
      warning(sprintf("spline_curve failed for %s ~ %s: %s", outcome, exposure,
                      conditionMessage(e)))
      NULL
    })
  }

  list(
    outcome            = outcome,
    exposure           = exposure,
    exposure_term      = exposure_term,
    covariates         = covariates,
    n                  = nrow(model.frame(fit)),
    coefficients       = coef_df,
    model_type         = if (is_logistic) "svyglm_logistic" else
                         if (use_spline) "svyglm_spline" else "svyglm_gaussian",
    is_logistic        = is_logistic,
    exp_coef           = is_logistic,
    ref_lowest         = isTRUE(ref_lowest) && !is.null(ref_factor_col),
    ref_min_label      = ref_min_label,
    spline_df          = spline_df,
    spline_knots       = spline_knots,
    spline_curve       = spline_curve,
    spline_global_p    = spline_global_p,
    spline_nonlin_p    = spline_nonlin_p,
    spline_aic_linear  = spline_aic_linear,
    spline_aic_spline  = spline_aic_spline,
    weight_group       = attr(design, "weight_group"),
    formula            = formula_str
  )
}

get_exposure_coefficient <- function(results) {
  ## For ref_lowest models: multiple exposure=<level> rows exist.
  ## Return the first non-reference row (the lowest non-ref level).
  if (isTRUE(results$ref_lowest)) {
    pat  <- paste0("^", regexEscape(results$exposure), "=")
    rows <- results$coefficients[grepl(pat, results$coefficients$term), ]
    if (nrow(rows) > 0) return(rows[1L, , drop=FALSE])
  }

  ## For spline models there are multiple exposure terms (ns(x, df=k) basis columns).
  ## Return the first spline component; forest plots will use the overall p-value
  ## from the joint test when available, or just the first term.
  term <- results$exposure_term %||% results$exposure

  ## Exact match first
  row <- results$coefficients[results$coefficients$term == term, ]
  if (nrow(row) > 0) return(row[1, , drop=FALSE])

  ## Partial match for spline terms, e.g. "ns(pfoa, df=3)1"
  base_exp <- results$exposure
  pat      <- paste0("^ns\\(", regexEscape(base_exp))
  rows     <- results$coefficients[grepl(pat, results$coefficients$term), ]
  if (nrow(rows) > 0) return(rows[1, , drop=FALSE])

  ## Fallback: any term containing exposure name
  rows <- results$coefficients[grepl(base_exp, results$coefficients$term, fixed=TRUE), ]
  if (nrow(rows) > 0) return(rows[1, , drop=FALSE])

  stop(sprintf(
    "Could not find exposure term '%s' in coefficients. Available terms: [%s]",
    term, paste(results$coefficients$term, collapse = ", ")))
}

## Helper: escape regex metacharacters in variable name
regexEscape <- function(s) gsub("([.+*?^${}()|\\[\\]\\\\])", "\\\\\\1", s)

#' Quartile-factor regression: treats the exposure as a 4-level ordered factor
#' with Q1 as the reference, returning one row per quartile (Q1 = Ref, Q2–Q4
#' as contrasts vs Q1).  Returned value is a plain data frame — not a
#' regression results object — intended for direct use by make_combined_results_table().
run_quartile_factor_regression <- function(design, outcome, exposure, covariates,
                                            log10_exposure    = NULL,
                                            outcome_transform = NULL,
                                            family            = gaussian()) {
  raw_vals <- design$variables[[exposure]]
  if (is.null(raw_vals))
    stop(sprintf("Exposure '%s' not found in design variables", exposure))

  if (length(unique(na.omit(raw_vals))) < 4L)
    stop(sprintf(
      "Exposure '%s' has fewer than 4 distinct values — cannot form 4 quartiles",
      exposure))

  q_labels <- c("Q1", "Q2", "Q3", "Q4")
  ## Use rank-based assignment so ties at percentile boundaries don't create
  ## unequal group sizes (unlike cut() which dumps tied boundary values into
  ## a single interval).
  qf <- factor(q_labels[dplyr::ntile(raw_vals, 4)], levels = q_labels)
  qf <- relevel(qf, ref = "Q1")
  n_per_q <- table(qf)   ## named integer: Q1/Q2/Q3/Q4

  design$variables[[".q_strat"]] <- qf

  fam_obj <- if (is.character(family)) {
    switch(tolower(family),
      "gaussian"      = gaussian(),
      "binomial"      = quasibinomial(),
      "quasibinomial" = quasibinomial(),
      "logistic"      = quasibinomial(),
      gaussian())
  } else family
  is_logistic <- inherits(fam_obj, "family") &&
                 fam_obj$family %in% c("binomial", "quasibinomial")

  cov_rhs <- if (length(covariates) > 0) paste(covariates, collapse = " + ") else "1"
  outcome_term <- switch(outcome_transform %||% "none",
    "log10" = sprintf("log10(%s)", outcome),
    "log2"  = sprintf("log2(%s)",  outcome),
    "sqrt"  = sprintf("sqrt(%s)",  outcome),
    "ln"    = sprintf("log(%s)",   outcome),
    outcome
  )
  fml     <- as.formula(sprintf("%s ~ .q_strat + %s", outcome_term, cov_rhs))
  fit     <- svyglm(fml, design = design, family = fam_obj)

  coefs <- summary(fit)$coefficients
  ci    <- tryCatch(
    confint(fit),
    error = function(e) matrix(NA_real_, nrow = nrow(coefs), ncol = 2,
                                dimnames = list(rownames(coefs), c("2.5 %", "97.5 %"))))

  ## Q1 reference row
  rows <- list(data.frame(
    outcome    = outcome,
    exposure   = exposure,
    stratum    = "Q1 (Ref)",
    estimate   = if (is_logistic) 1 else 0,
    ci_low     = NA_real_,
    ci_high    = NA_real_,
    p_value    = NA_real_,
    n          = as.integer(n_per_q["Q1"]),
    exp_coef   = is_logistic,
    stringsAsFactors = FALSE
  ))

  ## Q2–Q4 contrast rows
  for (q in 2:4) {
    term_nm <- sprintf(".q_stratQ%d", q)
    idx     <- which(rownames(coefs) == term_nm)
    if (length(idx) == 0L) next

    est <- coefs[idx, "Estimate"]
    cil <- ci[idx, 1]
    cih <- ci[idx, 2]
    pv  <- coefs[idx, ncol(coefs)]

    if (is_logistic) { est <- exp(est); cil <- exp(cil); cih <- exp(cih) }

    rows[[q]] <- data.frame(
      outcome  = outcome,
      exposure = exposure,
      stratum  = sprintf("Q%d", q),
      estimate = est,
      ci_low   = cil,
      ci_high  = cih,
      p_value  = pv,
      n        = as.integer(n_per_q[sprintf("Q%d", q)]),
      exp_coef = is_logistic,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, Filter(Negate(is.null), rows))
}

#' Run the same regression across multiple outcomes and/or multiple
#' exposures, returning a list of results objects. This is the function
#' the CLI/Shiny layer will actually call for a typical multi-outcome,
#' multi-chemical analysis (mirrors the existing MATLAB forest-plot loop
#' structure, generalized).
run_regression_grid <- function(design, outcomes, exposures, covariates,
                                 min_n = 20) {
  results <- list()
  for (outcome in outcomes) {
    for (exposure in exposures) {
      n_available <- sum(
        !is.na(design$variables[[outcome]]) &
        !is.na(design$variables[[exposure]])
      )
      if (n_available < min_n) {
        message(sprintf(
          "Skipping outcome='%s' exposure='%s': insufficient N (%d < %d)",
          outcome, exposure, n_available, min_n))
        next
      }
      key <- paste(outcome, exposure, sep = "__")
      results[[key]] <- tryCatch(
        run_weighted_regression(design, outcome, exposure, covariates),
        error = function(e) {
          warning(sprintf("Regression failed for outcome='%s' exposure='%s': %s",
                           outcome, exposure, conditionMessage(e)))
          NULL
        }
      )
    }
  }
  Filter(Negate(is.null), results)
}
