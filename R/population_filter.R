## ============================================================================
## population_filter.R
##
## Owns: turning a list of user-specified constraints into a filtered/flagged
## dataset. Knows nothing about regression, weights, or output formatting.
##
## A "constraint" is a small, composable unit: a condition on one column.
## A "population definition" is a list of constraints, ANDed together.
## This mirrors the project's existing 12-population structure (condition
## flags x medication status) but generalizes it to arbitrary user-specified
## constraints rather than the hardcoded set in the MATLAB pipeline.
## ============================================================================

#' Define a single constraint.
#'
#' @param variable canonical variable name (must exist in the data frame)
#' @param op one of "==", "!=", ">", ">=", "<", "<=", "%in%", "not_na"
#' @param value the comparison value (ignored for "not_na")
make_constraint <- function(variable, op, value = NULL) {
  list(variable = variable, op = op, value = value)
}

#' Apply a single constraint to a data frame, returning a logical mask.
apply_constraint <- function(data, constraint) {
  if (!(constraint$variable %in% names(data))) {
    stop(sprintf(
      "Constraint references variable '%s' which is not in the dataset — was it included in the variable list passed to build_harmonized_dataset()?",
      constraint$variable))
  }
  col <- data[[constraint$variable]]

  switch(constraint$op,
    "=="        = col == constraint$value,
    "!="        = col != constraint$value,
    ">"         = col >  constraint$value,
    ">="        = col >= constraint$value,
    "<"         = col <  constraint$value,
    "<="        = col <= constraint$value,
    "%in%"      = col %in% constraint$value,
    "not_na"    = !is.na(col),
    # na_or_neq: keep if NA OR not equal to value.
    # Use case: pregnancy exclusion — keep participants who are confirmed
    # non-pregnant (ridexprg != 1) OR have no pregnancy assessment (NA),
    # since NA means the question wasn't asked (e.g. males, older females),
    # not that the participant is pregnant.
    "na_or_neq" = is.na(col) | (!is.na(col) & col != constraint$value),
    stop(sprintf("Unknown constraint operator '%s'", constraint$op))
  )
}

#' Apply a list of constraints (ANDed) to a dataset.
#'
#' @param data harmonized dataset from build_harmonized_dataset()
#' @param constraints list of constraints from make_constraint()
#' @return data filtered to rows satisfying ALL constraints, plus an
#'         attribute recording how many rows each constraint individually
#'         removed (useful for debugging population definitions — a
#'         constraint that unexpectedly zeroes out the population is much
#'         easier to spot this way than from a single final N).
apply_population_filter <- function(data, constraints) {
  if (length(constraints) == 0) {
    attr(data, "filter_trace") <- data.frame(
      step = "no constraints", n_remaining = nrow(data))
    return(data)
  }

  mask <- rep(TRUE, nrow(data))
  trace <- data.frame(step = character(0), n_remaining = integer(0))

  for (i in seq_along(constraints)) {
    c_mask <- apply_constraint(data, constraints[[i]])
    # IMPORTANT: comparisons against NA (e.g. AGE >= 18 when AGE is NA)
    # return NA, not FALSE. Left unhandled, `mask & NA` propagates NA
    # through the combined mask, and subsetting a data frame with a
    # logical vector containing NA silently inserts a phantom all-NA row.
    # We explicitly coerce NA to FALSE here (i.e. "exclude rows where this
    # constraint cannot be evaluated"), matching standard complete-case
    # epidemiology convention. This was caught by the test suite — see
    # tests/smoke_test_no_network.R Test 5.
    c_mask[is.na(c_mask)] <- FALSE
    mask <- mask & c_mask
    trace <- rbind(trace, data.frame(
      step = sprintf("%s %s %s", constraints[[i]]$variable,
                      constraints[[i]]$op,
                      paste(constraints[[i]]$value, collapse = ",")),
      n_remaining = sum(mask)
    ))
  }

  out <- data[mask, ]
  attr(out, "filter_trace") <- trace
  out
}

#' Convenience: print the filter trace from apply_population_filter().
#' Mirrors the fprintf("...subsample: N=%d") pattern in the existing
#' MATLAB pipeline, generalized to an arbitrary constraint list.
print_filter_trace <- function(filtered_data) {
  trace <- attr(filtered_data, "filter_trace")
  if (is.null(trace)) {
    message("No filter trace attached to this dataset.")
    return(invisible(NULL))
  }
  cat("Population filter trace:\n")
  for (i in seq_len(nrow(trace))) {
    cat(sprintf("  after [%s]: N=%d\n", trace$step[i], trace$n_remaining[i]))
  }
}
