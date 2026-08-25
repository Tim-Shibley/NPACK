## ============================================================================
## weight_builder.R
##
## Single source of truth for NHANES survey weight selection and CDC pooling.
## All analysis engines call build_analysis_weight() instead of maintaining
## their own weight logic.
##
## CDC pooling rules implemented:
##   - When both 1999 and 2001 cycles are present: use 4-year standard weight
##     (wtmec4yr / wtint4yr) for those rows, scaled by 2/n_cycles.
##   - All other cycle rows: 2-year standard weight / n_cycles.
##   - If only one early cycle is present: fall back to 2-year weight.
##   - Subsample weights (wtsaf, wtsa*, etc.) override the standard weight for
##     the cycle(s) where they apply; priority order enforced via
##     get_weight_priority().
##   - When both 1999 and 2001 are present, any *2yr subsample weight selected
##     for those rows is upgraded to its *4yr counterpart if available
##     (e.g. wtsaf2yr → wtsaf4yr).
##   - If all analytic variables are demographic-only: use wtint2yr / wtint4yr
##     instead of wtmec2yr / wtmec4yr.
## ============================================================================

#' Build survey weights for a pooled NHANES analysis.
#'
#' @param data        Data frame containing all NHANES columns (including weight
#'                    columns and a \code{year} column of integer cycle start years).
#' @param all_vars    Character vector of all analytic variable names
#'                    (exposures + outcomes + covariates + mediators).
#' @param wt_map      Named list: variable -> cycle_year_string -> weight_col_name.
#'                    Only subsample weights are stored here; standard MEC/interview
#'                    weights are handled automatically.
#'
#' @return A list with:
#' \describe{
#'   \item{weight}{Numeric vector (same length as \code{nrow(data)}) of
#'     CDC-correct pooled survey weights ready to pass to \code{svydesign()}.}
#'   \item{raw_cols}{Name of the most restrictive weight column selected
#'     (used for audit headers and downstream reporting).}
#'   \item{cycle_wt_used}{Named character vector: cycle year string ->
#'     weight column name used for that cycle. Used to render weight audit tables.}
#'   \item{n_cycles}{Number of distinct cycles with valid weight data.}
#' }
build_analysis_weight <- function(data, all_vars, wt_map = list(),
                                   dietary_weight = FALSE) {

  ## -- 1. Default weight column (interview vs MEC) ----------------------------
  default_wt_col  <- if (all_demographic(all_vars)) "wtint2yr" else "wtmec2yr"
  combined_wt     <- data[[default_wt_col]]
  years_present   <- sort(unique(data$year))
  cycle_wt_used   <- setNames(
    rep(paste0(default_wt_col, " (fallback)"), length(years_present)),
    as.character(years_present)
  )
  selected_wt_col <- default_wt_col

  ## -- 2. CDC 4-year seeding for early cycles ---------------------------------
  ## When both 1999 and 2001 are present, initialise those rows from the
  ## 4-year standard weight so the later pooling formula (× 2/n_cycles) is
  ## numerically correct.
  both_early <- all(c(1999L, 2001L) %in% years_present)
  wt4yr_col  <- sub("2yr", "4yr", default_wt_col)   ## e.g. "wtmec4yr" / "wtint4yr"
  if (both_early && wt4yr_col %in% names(data)) {
    for (yr4 in c(1999L, 2001L)) {
      if (!yr4 %in% years_present) next
      m4 <- data$year == yr4
      v4 <- data[[wt4yr_col]][m4]
      if (any(!is.na(v4) & v4 > 0)) {
        combined_wt[m4]                    <- v4
        cycle_wt_used[[as.character(yr4)]] <- wt4yr_col
        if (get_weight_priority(wt4yr_col) < get_weight_priority(selected_wt_col))
          selected_wt_col <- wt4yr_col
      }
    }
  }

  ## -- 3. Subsample weight override (most restrictive wins per cycle) ----------
  ## Resolve each analytic variable to its NHANES source columns, then look up
  ## the variable_weight_map to find any subsample weight for each cycle.
  gather_maps <- function(var) {
    v  <- tolower(var)
    pre <- wt_map[[v]]
    if (!is.null(pre) && length(pre) > 0) return(list(pre))
    cv <- get_custom_variables()[[v]]
    if (!is.null(cv) && !is.null(cv$or_groups)) return(list())
    lapply(expand_to_sources(var), function(s) wt_map[[s]] %||% list())
  }
  source_maps <- unique(unlist(lapply(all_vars, gather_maps), recursive = FALSE))

  for (yr in years_present) {
    yr_str  <- as.character(yr)
    yr_mask <- data$year == yr
    candidates <- unique(unlist(lapply(source_maps, function(m) m[[yr_str]])))
    candidates <- candidates[nzchar(candidates) & candidates %in% names(data)]
    if (length(candidates) == 0) next
    priorities <- vapply(candidates, get_weight_priority, integer(1))
    candidates <- candidates[!is.na(priorities)]
    if (length(candidates) == 0) next
    candidates <- candidates[order(priorities[!is.na(priorities)])]
    for (wc in candidates) {
      wt_vals <- data[[wc]][yr_mask]
      if (any(!is.na(wt_vals) & wt_vals > 0)) {
        combined_wt[yr_mask]    <- wt_vals
        cycle_wt_used[[yr_str]] <- wc
        if (get_weight_priority(wc) < get_weight_priority(selected_wt_col))
          selected_wt_col <- wc
        break
      }
    }
  }

  ## -- 3b. Upgrade 2yr subsample weights to 4yr for early cycles ---------------
  ## After the subsample loop, if a *2yr subsample weight was selected for 1999
  ## or 2001 and a *4yr counterpart exists, replace it. This covers wtsaf2yr →
  ## wtsaf4yr and any other subsample weight that has a 4yr variant.
  if (both_early) {
    for (yr4 in c(1999L, 2001L)) {
      if (!yr4 %in% years_present) next
      yr4_str <- as.character(yr4)
      cur_col <- cycle_wt_used[[yr4_str]]
      ## Only upgrade if a *2yr subsample weight won (not already 4yr or fallback)
      if (grepl("2yr$", cur_col) && !grepl("^wtmec|^wtint", cur_col)) {
        col4 <- sub("2yr$", "4yr", cur_col)
        if (col4 %in% names(data)) {
          m4   <- data$year == yr4
          v4   <- data[[col4]][m4]
          if (any(!is.na(v4) & v4 > 0)) {
            combined_wt[m4]              <- v4
            cycle_wt_used[[yr4_str]]     <- col4
            if (get_weight_priority(col4) < get_weight_priority(selected_wt_col))
              selected_wt_col <- col4
          }
        }
      }
    }
  }

  ## -- 3c. Dietary recall weight override -------------------------------------
  ## dietary_weight is FALSE (off), "1day", or "2day".
  ##
  ## 1-day (WTDRD1 / WTDR4YR):
  ##   All cycles included. For 1999/2001 when both are present, WTDR4YR is
  ##   used (CDC 4yr rule); all other cycles use WTDRD1.
  ##
  ## 2-day (WTDRD2):
  ##   1999 and 2001 have no day-2 recall weight — those rows are excluded
  ##   by setting their combined_wt to NA. All other cycles use WTDRD2.
  if (identical(dietary_weight, "1day") || identical(dietary_weight, "2day")) {
    drd1_col <- Filter(function(x) x %in% names(data), c("wtdrd1", "WTDRD1"))[1]
    dr4_col  <- Filter(function(x) x %in% names(data), c("wtdr4yr","WTDR4YR"))[1]
    drd2_col <- Filter(function(x) x %in% names(data), c("wtdrd2", "WTDRD2"))[1]

    if (identical(dietary_weight, "1day")) {
      if (is.na(drd1_col))
        warning("1-day dietary recall weight requested but WTDRD1 not found — ",
                "rebuild the database to include dietary recall weights.")
      for (yr in years_present) {
        yr_str  <- as.character(yr)
        yr_mask <- data$year == yr
        use_col <- if (yr %in% c(1999L, 2001L) && both_early && !is.na(dr4_col))
                     dr4_col else drd1_col
        if (is.na(use_col)) next
        wt_vals <- data[[use_col]][yr_mask]
        if (any(!is.na(wt_vals) & wt_vals > 0)) {
          combined_wt[yr_mask]    <- wt_vals
          cycle_wt_used[[yr_str]] <- use_col
          selected_wt_col         <- use_col
        }
      }
    } else {
      ## 2-day: drop 1999/2001 entirely; apply WTDRD2 for all other cycles
      if (is.na(drd2_col))
        warning("2-day dietary recall weight requested but WTDRD2 not found — ",
                "rebuild the database to include dietary recall weights.")
      for (yr in years_present) {
        yr_str  <- as.character(yr)
        yr_mask <- data$year == yr
        if (yr %in% c(1999L, 2001L)) {
          combined_wt[yr_mask]    <- NA_real_
          cycle_wt_used[[yr_str]] <- "excluded (no 2-day recall weight)"
          next
        }
        if (is.na(drd2_col)) next
        wt_vals <- data[[drd2_col]][yr_mask]
        if (any(!is.na(wt_vals) & wt_vals > 0)) {
          combined_wt[yr_mask]    <- wt_vals
          cycle_wt_used[[yr_str]] <- drd2_col
          selected_wt_col         <- drd2_col
        }
      }
    }
  }

  ## -- 4. CDC pooling formula -------------------------------------------------
  n_cycles <- length(unique(data$year[!is.na(combined_wt) & combined_wt > 0]))
  weight <- if (n_cycles <= 1L) {
    combined_wt
  } else {
    w <- combined_wt / n_cycles
    if (both_early) {
      em    <- data$year %in% c(1999L, 2001L)
      w[em] <- combined_wt[em] * (2 / n_cycles)
    }
    w
  }

  list(
    weight        = weight,
    raw_cols      = selected_wt_col,
    cycle_wt_used = cycle_wt_used,
    n_cycles      = n_cycles
  )
}
