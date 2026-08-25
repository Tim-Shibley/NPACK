## ============================================================================
## mediation_engine.R
##
## Survey-weighted mediation analysis using the product-of-coefficients
## (Baron & Kenny) approach with delta-method variance estimation.
##
## Both component models (mediator and outcome) are fitted with svyglm using
## a proper NHANES survey design (strata + PSU + weights via build_survey_design),
## giving correct TSL standard errors for each path coefficient.
##
## The indirect effect (ACME = a*b) variance is computed via the delta method:
##   Var(a*b) ≈ b²·Var(a) + a²·Var(b)
## where Var(a) and Var(b) come from the survey-design-aware vcov() matrices.
## The cross-model covariance term is zero because the two models are fitted
## on independent estimating equations. This is methodologically equivalent to
## what mediate() does for its quasi-Bayesian approximation, but is exact under
## the delta method and respects the survey design throughout.
##
## Depends on globals from cli.R / bkmr_engine.R:
##   resolve_colnames, PFAS_VARS, WEIGHT_PRIORITY, population_definitions,
##   %||%, load_nhanes_database, apply_population_filter, make_constraint,
##   print_filter_trace, build_demographic_table, resolve_population_def,
##   build_survey_design
## ============================================================================

library(survey)

#' Run a survey-weighted mediation analysis.
#'
#' Implements the Baron-Kenny product-of-coefficients approach using two
#' \code{svyglm()} models (mediator model and outcome model) fitted with a
#' proper NHANES survey design (strata + PSU + weights). The indirect effect
#' (ACME = a*b) variance is estimated via the delta method; direct and total
#' effects use TSL standard errors from the outcome model. One triplet
#' (exposure, mediator, outcome) is fitted per combination of the three input
#' vectors.
#'
#' @param exposures character vector of exposure variable names.
#' @param outcomes character vector of outcome variable names.
#' @param mediators character vector of mediator variable names.
#' @param covariates character vector of covariate names included in both
#'   the mediator model and the outcome model.
#' @param population named string from \code{population_definitions} or a list
#'   with \code{$label} and \code{$constraints}.
#' @param cycles character vector of NHANES cycle labels to include.
#' @param db_path path to the pre-built database RDS file.
#' @param exp_transforms character vector of exposure transforms, one per
#'   exposure: \code{"none"} (default), \code{"log10"}, \code{"log2"},
#'   \code{"sqrt"}, or \code{"ln"}. Recycled if length-1.
#' @param out_transforms character vector of outcome transforms, same options.
#'   Recycled if length-1.
#' @param seed integer random seed (currently unused; reserved for future
#'   bootstrap extensions).
#' @param complete_cases_only logical. If \code{TRUE}, restricts to participants
#'   non-missing on all exposures, outcomes, and mediators simultaneously.
#' @param verbose logical. Print filter traces, weight maps, and path
#'   coefficients to the console.
#'
#' @return An invisible list with components:
#' \describe{
#'   \item{type}{\code{"mediation"}.}
#'   \item{mediation_results}{named list keyed by
#'     \code{"outcome__exposure__mediator"}. Each entry contains:
#'     \code{path_a} (X→M coefficient), \code{path_b} (M→Y coefficient),
#'     \code{acme} / \code{acme_se} / \code{acme_ci} / \code{acme_p}
#'     (indirect effect and delta-method inference),
#'     \code{ade} / \code{ade_se} / \code{ade_ci} / \code{ade_p}
#'     (direct effect), \code{total}, \code{prop_mediated}, \code{n},
#'     \code{med_fit} and \code{out_fit} (raw \code{svyglm} objects).}
#'   \item{pair_demographics}{Table-1 summaries.}
#'   \item{n}{unweighted analytic N.}
#'   \item{population}{population label string.}
#'   \item{cycles}{cycles used.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- run_mediation_analysis(
#'   exposures  = "pfoa",
#'   outcomes   = "ldl",
#'   mediators  = "bmi",
#'   covariates = c("age", "sex_bin", "education", "income_ratio"),
#'   population = "cvd_medicated_fasting",
#'   cycles     = c("2003-2004", "2005-2006", "2007-2008", "2009-2010",
#'                  "2011-2012", "2013-2014", "2015-2016", "2017-2018"),
#'   db_path    = here::here("data", "nhanes_pool.rds"),
#'   exp_transforms = "log10"
#' )
#' r <- result$mediation_results[["ldl__pfoa__bmi"]]
#' cat(sprintf("ACME = %.4f (p = %.4f)  |  ADE = %.4f  |  Prop. mediated = %.2f\n",
#'             r$acme, r$acme_p, r$ade, r$prop_mediated))
#' }
run_mediation_analysis <- function(exposures, outcomes, mediators, covariates,
                                    population, cycles, db_path,
                                    exp_transforms      = NULL,
                                    out_transforms      = NULL,
                                    seed                = 42,
                                    complete_cases_only = FALSE,
                                    dietary_weight      = FALSE,
                                    verbose             = TRUE) {

  pop_def  <- resolve_population_def(population)
  exp_cols <- resolve_colnames(exposures)
  out_cols <- resolve_colnames(outcomes)
  med_cols <- resolve_colnames(mediators)
  cov_cols <- resolve_colnames(covariates)

  ## Recycle transform vectors (same pattern as mixture_engine.R)
  exp_transforms <- rep_len(exp_transforms %||% "none", length(exp_cols))
  out_transforms <- rep_len(out_transforms %||% "none", length(out_cols))
  names(exp_transforms) <- exp_cols
  names(out_transforms) <- out_cols

  .term <- function(var, tr) switch(tr %||% "none",
    "log10" = sprintf("log10(%s)", var),
    "log2"  = sprintf("log2(%s)",  var),
    "sqrt"  = sprintf("sqrt(%s)",  var),
    "ln"    = sprintf("log(%s)",   var),
    var
  )

  if (verbose) {
    cat(sprintf("Mediation — Population: %s\n",
                pop_def$label %||% "custom"))
    cat(sprintf("Exposures: %s\nMediators: %s\nOutcomes: %s\n\n",
                paste(exp_cols, collapse=", "),
                paste(med_cols, collapse=", "),
                paste(out_cols, collapse=", ")))
  }

  db <- load_nhanes_database(db_path)
  all_vars <- unique(c(exp_cols, out_cols, med_cols, cov_cols))
  missing  <- setdiff(all_vars, names(db))
  if (length(missing) > 0)
    stop(sprintf("Variable(s) not in dataset: %s.", paste(missing, collapse=", ")))

  cycle_years      <- as.integer(sub("-.*", "", cycles))
  base_constraints <- c(pop_def$constraints,
                         list(make_constraint("year", "%in%", cycle_years)),
                         lapply(cov_cols, function(v) make_constraint(v, "not_na")))

  ## If complete_cases_only, restrict the base sample to rows non-missing on
  ## ALL exposures, outcomes, and mediators so every triplet shares one sample.
  if (isTRUE(complete_cases_only)) {
    all_var_not_na <- lapply(c(exp_cols, out_cols, med_cols),
                             function(v) make_constraint(v, "not_na"))
    base_constraints <- c(base_constraints, all_var_not_na)
    if (verbose)
      cat(sprintf("Complete-cases mode: requiring non-missing for all %d exposures, %d outcomes, %d mediators\n",
                  length(exp_cols), length(out_cols), length(med_cols)))
  }

  wt_map <- attr(db, "variable_weight_map") %||% list()

  ## Build pooled weight; tracks the chosen weight column and the variable
  ## that drove that choice, per cycle.
  .build_wt <- function(data, exp, out, covs = character(0)) {
    res <- build_analysis_weight(data, c(exp, out, covs), wt_map,
                                 dietary_weight = dietary_weight)
    ## mediation callers expect $cycle_wt and $cycle_wt_src fields
    list(weight       = res$weight,
         cycle_wt     = res$cycle_wt_used,
         cycle_wt_src = setNames(rep("weight_builder", length(res$cycle_wt_used)),
                                 names(res$cycle_wt_used)),
         n_cycles     = res$n_cycles)
  }

  ## When complete_cases_only, pre-compute one shared weight from ALL variables
  ## so every triplet uses the identical (most restrictive) weight map.
  shared_wt_info <- NULL
  if (isTRUE(complete_cases_only) &&
      length(exp_cols) > 0 && length(out_cols) > 0 && length(med_cols) > 0) {
    shared_data    <- as.data.frame(apply_population_filter(db, base_constraints))
    shared_wt_info <- .build_wt(shared_data, exp_cols[1], out_cols[1],
                                 c(cov_cols, exp_cols[-1], out_cols[-1], med_cols))
    if (verbose)
      cat(sprintf("Shared weight (complete-cases): %s across %d cycle(s)\n\n",
                  shared_wt_info$cycle_wt[[1]], shared_wt_info$n_cycles))
  }

  mediation_results <- list()
  cov_rhs <- if (length(cov_cols) > 0) paste(cov_cols, collapse=" + ") else "1"

  for (outcome in out_cols) {
    for (exposure in exp_cols) {
      for (mediator in med_cols) {
        key <- paste(outcome, exposure, mediator, sep="__")
        if (verbose) cat(sprintf("\n── Mediation: %s -> %s -> %s ──\n",
                                  exposure, mediator, outcome))

        constraints_i <- c(base_constraints,
          list(make_constraint(exposure, "not_na"),
               make_constraint(outcome,  "not_na"),
               make_constraint(mediator, "not_na")))

        data_i <- as.data.frame(apply_population_filter(db, constraints_i))
        if (verbose) print_filter_trace(data_i)

        wt_info <- if (!is.null(shared_wt_info)) shared_wt_info else
          .build_wt(data_i, exposure, outcome, cov_cols)
        data_i$pooled_weight <- wt_info$weight

        ## All analytic variables were already constrained not_na above.
        ## The only remaining drop is rows where the pooled weight is NA/zero
        ## (can happen when a subsample weight has no coverage for a row).
        data_cc <- data_i[!is.na(data_i$pooled_weight) & data_i$pooled_weight > 0, ]

        if (verbose) {
          cat(sprintf("  after [pooled weight not NA/zero]: N=%d\n", nrow(data_cc)))

          ## Print per-variable weight map for all analytic variables.
          analytic_vars <- unique(c(exposure, outcome, mediator, cov_cols))
          sep_width <- 56
          cat(sprintf("\n%s Weight maps (variable -> cycle -> weight column) %s\n",
                      strrep("=", 3), strrep("=", 3)))
          for (v in analytic_vars) {
            v_map   <- wt_map[[tolower(v)]] %||% list()
            entries <- vapply(as.character(sort(cycle_years)), function(yr) {
              wc <- v_map[[yr]]
              if (!is.null(wc) && wc %in% names(data_i)) {
                sprintf("%s:%s", yr, wc)
              } else NA_character_
            }, character(1))
            entries <- entries[!is.na(entries)]
            label   <- formatC(v, width = -22, flag = "-")
            if (length(entries) == 0) {
              cat(sprintf("  %s(no subsample weight — wtmec2yr fallback for all cycles)\n",
                          label))
            } else {
              cat(sprintf("  %s%s\n", label, paste(entries, collapse = "  ")))
            }
          }
          cat(sprintf("%s\n\n", strrep("=", sep_width)))
        }

        if (nrow(data_cc) < 100) {
          warning(sprintf("N=%d for mediation %s -> %s -> %s, skipping",
                           nrow(data_cc), exposure, mediator, outcome))
          next
        }

        ## Apply dropdown zscore transforms in-place before building the design.
        .zscore_col <- function(x) { m <- mean(x, na.rm=TRUE); s <- sd(x, na.rm=TRUE); (x - m) / s }
        if (identical(exp_transforms[[exposure]], "zscore"))
          data_cc[[exposure]] <- .zscore_col(data_cc[[exposure]])
        if (identical(out_transforms[[outcome]], "zscore"))
          data_cc[[outcome]] <- .zscore_col(data_cc[[outcome]])

        ## Proper NHANES survey design with strata + PSU + weights.
        ## build_survey_design() handles lonely PSU, sets survey.lonely.psu,
        ## and uses sdmvstra/sdmvpsu for correct TSL variance.
        svy <- tryCatch(
          build_survey_design(data_cc, c(exposure, mediator)),
          error = function(e) stop(sprintf("Survey design failed: %s", conditionMessage(e)))
        )

        exp_term <- .term(exposure, exp_transforms[[exposure]])
        out_term <- .term(outcome,  out_transforms[[outcome]])

        ## Path a: X → M  (exposure effect on mediator)
        med_fml <- as.formula(sprintf("%s ~ %s + %s", mediator, exp_term, cov_rhs))
        ## Path b + direct: Y ~ X + M + covariates
        out_fml <- as.formula(sprintf("%s ~ %s + %s + %s", out_term, exp_term, mediator, cov_rhs))
        ## Total effect model: Y ~ X + covariates (no mediator)
        tot_fml <- as.formula(sprintf("%s ~ %s + %s", out_term, exp_term, cov_rhs))

        med_fit <- tryCatch(
          svyglm(med_fml, design = svy, family = gaussian()),
          error = function(e) stop(sprintf("Mediator model failed: %s", conditionMessage(e)))
        )
        out_fit <- tryCatch(
          svyglm(out_fml, design = svy, family = gaussian()),
          error = function(e) stop(sprintf("Outcome model failed: %s", conditionMessage(e)))
        )
        tot_fit <- tryCatch(
          svyglm(tot_fml, design = svy, family = gaussian()),
          error = function(e) NULL   ## non-fatal; total estimated as ACME + ADE
        )

        ## ---- Delta-method indirect effect (ACME = a * b) --------------------
        ## a = exposure → mediator coefficient (from med_fit)
        ## b = mediator → outcome coefficient  (from out_fit)
        ## c = exposure → outcome direct effect (from out_fit)
        ## Var(a*b) ≈ b²·Var(a) + a²·Var(b)  [cross-model covariance = 0]

        a     <- as.numeric(coef(med_fit)[exp_term])
        b     <- as.numeric(coef(out_fit)[mediator])
        c_dir <- as.numeric(coef(out_fit)[exp_term])

        var_a <- as.numeric(vcov(med_fit)[exp_term, exp_term])
        var_b <- as.numeric(vcov(out_fit)[mediator,  mediator])
        var_c <- as.numeric(vcov(out_fit)[exp_term,  exp_term])

        acme_est <- a * b
        acme_se  <- sqrt(b^2 * var_a + a^2 * var_b)
        acme_ci  <- c(acme_est - 1.96 * acme_se,
                      acme_est + 1.96 * acme_se)
        acme_p   <- 2 * pnorm(abs(acme_est / acme_se), lower.tail = FALSE)

        ## ADE: direct effect of exposure on outcome (from out_fit, survey-aware)
        ade_est  <- c_dir
        ade_se   <- sqrt(var_c)
        ade_ci   <- tryCatch(as.numeric(confint(out_fit)[exp_term, ]),
                             error = function(e)
                               c(ade_est - 1.96*ade_se, ade_est + 1.96*ade_se))
        ade_p    <- tryCatch({
          sc <- summary(out_fit)$coefficients
          as.numeric(sc[exp_term, ncol(sc)])
        }, error = function(e) 2 * pnorm(abs(ade_est / ade_se), lower.tail = FALSE))

        ## Total effect: from total model if available, else ACME + ADE
        total_est <- if (!is.null(tot_fit)) {
          as.numeric(coef(tot_fit)[exp_term])
        } else {
          acme_est + ade_est
        }

        prop_med <- if (abs(total_est) > 1e-10) acme_est / total_est else NA_real_

        mediation_results[[key]] <- list(
          outcome       = outcome,
          exposure      = exposure,
          mediator      = mediator,
          ## Path coefficients
          path_a        = a,          ## X → M
          path_b        = b,          ## M → Y (adjusted)
          ## ACME = indirect effect (a * b), delta-method SE
          acme          = acme_est,
          acme_se       = acme_se,
          acme_ci       = acme_ci,
          acme_p        = acme_p,
          ## ADE = direct effect (X → Y adjusted for M), TSL SE from svyglm
          ade           = ade_est,
          ade_se        = ade_se,
          ade_ci        = ade_ci,
          ade_p         = ade_p,
          ## Total and proportion mediated
          total         = total_est,
          prop_mediated = prop_med,
          n             = nrow(data_cc),
          cycle_wt      = wt_info$cycle_wt,
          cycle_wt_src  = wt_info$cycle_wt_src,
          med_fit       = med_fit,
          out_fit       = out_fit
        )

        if (verbose)
          cat(sprintf(
            "a=%.4f  b=%.4f  ACME=%.4f (p=%.4f) | ADE=%.4f (p=%.4f) | Prop.med=%.2f\n",
            a, b, acme_est, acme_p, ade_est, ade_p,
            prop_med %||% NA_real_))
      }
    }
  }

  pop_data <- as.data.frame(apply_population_filter(db, base_constraints))

  ## Build per-(exposure, outcome) pair demographics using the analytic data
  ## from the first mediator for each pair (keys: "outcome__exposure__mediator").
  seen_pairs <- character(0)
  demo_entries <- list()
  for (key in names(mediation_results)) {
    parts   <- strsplit(key, "__")[[1]]
    out_col <- parts[1]; exp_col <- parts[2]
    pair_id <- paste(out_col, exp_col, sep="__")
    if (pair_id %in% seen_pairs) next
    seen_pairs <- c(seen_pairs, pair_id)
    r <- mediation_results[[key]]
    demo_n <- r$n %||% nrow(pop_data)
    demo_entries[[pair_id]] <- list(
      label  = sprintf("%s → %s", exp_col, out_col),
      n      = demo_n,
      tables = NULL  ## populated below using pop_data fallback
    )
  }

  pair_demographics <- if (isTRUE(complete_cases_only) || length(seen_pairs) <= 1) {
    list(list(
      label  = if (isTRUE(complete_cases_only)) "All (complete cases)" else "All",
      n      = nrow(pop_data),
      tables = build_demographic_table(pop_data,
                 covariates = cov_cols,
                 outcomes   = out_cols,
                 exposures  = exp_cols)
    ))
  } else {
    lapply(seen_pairs, function(pair_id) {
      parts   <- strsplit(pair_id, "__")[[1]]
      out_col <- parts[1]; exp_col <- parts[2]
      entry <- demo_entries[[pair_id]]
      list(
        label  = entry$label,
        n      = entry$n,
        tables = build_demographic_table(pop_data,
                   covariates = cov_cols,
                   outcomes   = out_col,
                   exposures  = exp_col)
      )
    })
  }

  invisible(list(
    type               = "mediation",
    mediation_results  = mediation_results,
    pair_demographics  = pair_demographics,
    n                  = nrow(pop_data),
    population         = pop_def$label %||% "custom",
    cycles             = cycles
  ))
}
