## ============================================================================
## derived_outcomes_registry.R
##
## Owns: the registry of derived outcome variables — outcomes computed from
## combinations of raw NHANES columns rather than fetched as a single column.
##
## Each entry specifies:
##   requires : character vector of canonical variable names that must be
##              fetched from NHANES before this outcome can be computed
##   compute  : function(data) -> numeric vector, applied to the harmonized
##              dataset after fetching and before population filtering
##   label    : human-readable name for plots and tables
##   log10_transform : whether to log10-transform before regression
##              (almost always FALSE for lipid outcomes)
##
## In Shiny, users will be able to add entries to this registry interactively
## by picking source variables from a dropdown and writing a formula. The
## structure here is the same structure the UI would produce — building a new
## entry here is the console equivalent of what the GUI will let users do.
## ============================================================================

derived_outcomes_registry <- list(

  non_hdl = list(
    label           = "Non-HDL Cholesterol (mg/dL)",
    requires        = c("lbxtc", "lbdhdd"),
    compute         = function(d) d$lbxtc - d$lbdhdd,
    log10_transform = FALSE
  ),

  remnant = list(
    label           = "Remnant Cholesterol (mg/dL)",
    requires        = c("lbxtc", "lbdhdd", "lbdldl"),
    compute         = function(d) d$lbxtc - d$lbdhdd - d$lbdldl,
    log10_transform = FALSE
  ),

  usfli = list(
    label           = "US Fatty Liver Index (USFLI)",
    requires        = c("ridreth1", "ridageyr", "lbxsgtsi", "bmxwaist", "lbxin", "lbxglu"),
    compute         = function(d) {
      black   <- as.integer(d[["ridreth1"]] == 4)
      mexican <- as.integer(d[["ridreth1"]] == 1)
      X <- (-0.8073 * black) +
           ( 0.3458 * mexican) +
           ( 0.0093 * d[["ridageyr"]]) +
           ( 0.6151 * log(d[["lbxsgtsi"]])) +
           ( 0.0249 * d[["bmxwaist"]]) +
           ( 1.1792 * log(d[["lbxin"]])) +
           ( 0.8242 * log(d[["lbxglu"]])) -
           14.7812
      (exp(X) / (1 + exp(X))) * 100
    },
    log10_transform = FALSE
  )

  # To add a new built-in derived outcome:
  #   label           : human-readable name shown in the UI
  #   requires        : lowercase column names that must already exist in the database
  #   compute         : function(d) returning a numeric vector, one value per row
  #   log10_transform : set TRUE only if this outcome should be log10-transformed
  #                     before regression (rare; almost always FALSE for clinical units)
  #
  # Example:
  # tc_hdl_ratio = list(
  #   label           = "Total Cholesterol / HDL Ratio",
  #   requires        = c("lbxtc", "lbdhdd"),
  #   compute         = function(d) d$lbxtc / d$lbdhdd,
  #   log10_transform = FALSE
  # )
)

#' Resolve a list of derived outcome names from derived_outcomes_registry.
#' Accepts case-insensitive names. Returns a list with:
#'   $fetched_outcomes  : always empty (fetched variables are raw NHANES columns now)
#'   $derived_outcomes  : names of derived outcomes to compute after fetching
#'   $all_requires      : union of all source NHANES columns needed
resolve_outcomes <- function(outcome_names) {
  derived  <- character(0)
  requires <- character(0)

  derived_keys_lower <- tolower(names(derived_outcomes_registry))

  for (nm in outcome_names) {
    if (tolower(nm) %in% derived_keys_lower) {
      canonical <- names(derived_outcomes_registry)[derived_keys_lower == tolower(nm)]
      derived  <- c(derived, canonical)
      requires <- c(requires, derived_outcomes_registry[[canonical]]$requires)
    } else {
      stop(sprintf(
        "Unknown outcome '%s' — not in derived_outcomes_registry.",
        nm))
    }
  }

  list(
    fetched_outcomes = character(0),
    derived_outcomes = derived,
    all_requires     = unique(requires)
  )
}

#' Apply all requested derived outcomes to a dataset, adding one new column
#' per derived outcome.
#'
#' @param data harmonized dataset from build_harmonized_dataset()
#' @param derived_outcome_names character vector of names from
#'        derived_outcomes_registry to compute and add
#' @return data with new columns added, one per derived outcome
apply_derived_outcomes <- function(data, derived_outcome_names) {
  for (nm in derived_outcome_names) {
    entry <- derived_outcomes_registry[[nm]]
    if (is.null(entry)) {
      stop(sprintf("Unknown derived outcome '%s'.", nm))
    }
    missing_src <- setdiff(entry$requires, names(data))
    if (length(missing_src) > 0) {
      stop(sprintf(
        "Cannot compute derived outcome '%s': source variable(s) missing from dataset: %s",
        nm, paste(missing_src, collapse = ", ")))
    }
    data[[nm]] <- entry$compute(data)
  }
  data
}

#' List all available derived outcomes with labels.
list_available_outcomes <- function() {
  lapply(names(derived_outcomes_registry), function(nm) {
    list(name  = nm,
         label = derived_outcomes_registry[[nm]]$label,
         type  = "derived")
  })
}

## ============================================================================
## USER-DEFINED CUSTOM VARIABLES
## Persistent across sessions via data/custom_variables.rds
## Formula syntax:
##   arithmetic: lbxtc - lbdhdd, lbxtc / lbdhdd
##   functions:  log(pfoa), sqrt(bmi)
##   deferred:   zscore(age) — computed at analysis time on filtered population
## ============================================================================

## Path where user-defined custom variables are saved
custom_vars_path <- function() here::here("data", "custom_variables.rds")

## In-memory registry of user custom variables (loaded at startup)
.custom_var_registry <- list()

## Load custom variables from disk into memory
load_custom_variables <- function() {
  path <- custom_vars_path()
  if (file.exists(path)) {
    .custom_var_registry <<- readRDS(path)
  }
  invisible(.custom_var_registry)
}

## Save current custom variable registry to disk
save_custom_variables <- function() {
  path <- custom_vars_path()
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(.custom_var_registry, path)
  invisible(TRUE)
}

## Parse a formula string and extract the column names it references.
## Supports single-line and multi-line R scripts. Column names are referenced
## directly by name — the script runs inside with(d, { ... }) so all columns
## in the data frame are in scope without any $ prefix. The value of the last
## expression is returned as the computed column.
## Special: zscore(col) is still recognised as a deferred single-column transform.
## Returns list(requires=character, is_zscore=logical, formula=string, r_expr=string)
parse_custom_formula <- function(formula_str) {
  f <- trimws(formula_str)

  ## Detect zscore wrapping — zscore(col) is deferred to analysis time
  is_zscore <- grepl("^zscore\\s*\\(", f)

  ## Parse # temp: and # functions: declarations — names listed here are
  ## excluded from the missing-column check.
  parse_directive <- function(tag) {
    lines <- regmatches(f, gregexpr(sprintf("(?m)^#\\s*%s:\\s*[^\n]+", tag), f, perl=TRUE))[[1]]
    nms   <- character(0)
    for (l in lines) {
      declared <- sub(sprintf("^#\\s*%s:\\s*", tag), "", l)
      nms      <- c(nms, trimws(strsplit(declared, ",")[[1]]))
    }
    nms
  }
  temp_vars <- c(parse_directive("temp"), parse_directive("functions"))

  ## Auto-exclude assignment targets (left-hand side of <-) so users don't
  ## need to declare intermediate variables manually.
  lhs_tokens <- regmatches(f, gregexpr("[A-Za-z_][A-Za-z0-9_.]*(?=\\s*<-)", f, perl=TRUE))[[1]]
  temp_vars  <- unique(c(temp_vars, lhs_tokens))

  ## Extract word tokens for dependency tracking.
  ## Regex starts with [A-Za-z_] (not dot) to avoid matching decimal literals like .013
  reserved <- c(
    "log", "sqrt", "zscore", "log2", "log10", "exp", "abs", "round", "floor",
    "ceiling", "ifelse", "is.na", "is.numeric", "is.character", "as.numeric",
    "as.integer", "as.character", "as", "is", "with", "within", "local",
    "TRUE", "FALSE", "NA", "NA_real_", "NA_integer_", "NA_complex_",
    "NA_character_", "NULL", "Inf", "NaN", "c", "sum", "mean", "sd", "var",
    "min", "max", "pmin", "pmax", "cumsum", "which", "length", "nrow",
    "seq", "seq_len", "seq_along", "rep", "paste", "paste0", "sprintf",
    "return", "function", "for", "while", "if", "else", "in", "next", "break"
  )
  f_no_comments <- gsub("(?m)#[^\n]*", "", f, perl=TRUE)
  tokens   <- regmatches(f_no_comments, gregexpr("[A-Za-z_][A-Za-z0-9_.]*", f_no_comments))[[1]]
  col_refs <- unique(setdiff(tokens, c(reserved, temp_vars)))

  ## The r_expr runs the user's code inside with(d, { ... }) so column names
  ## are in scope directly. Multi-line scripts are supported; the value of the
  ## last expression is the computed column.
  r_expr <- if (is_zscore) f else sprintf("with(d, {\n%s\n})", f)

  list(
    requires   = col_refs,
    is_zscore  = is_zscore,
    is_logical = FALSE,   ## logical detection no longer needed; user controls output type
    formula    = f,
    r_expr     = r_expr
  )
}

## Register a new user-defined custom variable
## name: column name to create (lowercase, no spaces)
## label: human readable description
## category: user-defined category string
## formula_str: formula as typed by user
## Returns list(success, message)
register_custom_variable <- function(name, label, category, formula_str,
                                      label_map = NULL, or_groups = NULL) {
  name <- tolower(trimws(gsub("[^A-Za-z0-9_]", "_", name)))

  if (nchar(name) == 0)
    return(list(success=FALSE, message="Variable name cannot be empty"))
  if (name %in% names(derived_outcomes_registry))
    return(list(success=FALSE,
                message=sprintf("'%s' is a built-in variable and cannot be overwritten", name)))

  parsed <- tryCatch(
    parse_custom_formula(formula_str),
    error = function(e) list(error=conditionMessage(e))
  )
  if (!is.null(parsed$error))
    return(list(success=FALSE, message=paste("Formula error:", parsed$error)))

  ## Validate the R expression compiles
  if (!parsed$is_zscore) {
    test_result <- tryCatch({
      parse(text=parsed$r_expr)
      TRUE
    }, error=function(e) conditionMessage(e))
    if (!isTRUE(test_result))
      return(list(success=FALSE, message=paste("Invalid formula:", test_result)))
  }

  ## Build compute function
  compute_fn <- if (parsed$is_zscore) {
    zscore_col <- parsed$requires[1]
    function(d) {
      vals <- d[[zscore_col]]
      (vals - mean(vals, na.rm=TRUE)) / sd(vals, na.rm=TRUE)
    }
  } else {
    r_expr <- parsed$r_expr
    function(d) eval(parse(text=r_expr), list(d=d))
  }

  .custom_var_registry[[name]] <<- list(
    label           = if (nchar(trimws(label)) > 0) label else name,
    category        = if (nchar(trimws(category)) > 0) category else "Custom",
    formula         = formula_str,
    requires        = parsed$requires,
    is_zscore       = parsed$is_zscore,
    compute         = compute_fn,
    log10_transform = FALSE,
    label_map       = label_map,  ## named list: code -> display label
    or_groups       = or_groups   ## list of char vectors: variables per AND-group, groups OR'd
  )
  save_custom_variables()
  list(success=TRUE, message=sprintf("Custom variable '%s' registered successfully", name))
}

## Register a merge variable: coalesces two source variables (left takes precedence)
## For each participant, the left variable value is used if not NA, otherwise
## the right variable value is used. The analytic weight is the least restrictive
## (broadest-coverage) weight of the two source variables per cycle.
register_merge_variable <- function(name, label, category, var_left, var_right) {
  name      <- tolower(trimws(gsub("[^A-Za-z0-9_]", "_", name)))
  var_left  <- tolower(trimws(var_left))
  var_right <- tolower(trimws(var_right))

  if (nchar(name) == 0)
    return(list(success=FALSE, message="Variable name cannot be empty"))
  if (!nzchar(var_left) || !nzchar(var_right))
    return(list(success=FALSE, message="Both source variables must be specified"))
  if (var_left == var_right)
    return(list(success=FALSE, message="Left and right variables must be different"))
  if (name %in% names(derived_outcomes_registry))
    return(list(success=FALSE,
                message=sprintf("'%s' is a built-in variable and cannot be overwritten", name)))

  vl <- var_left
  vr <- var_right
  compute_fn <- function(d) {
    l <- d[[vl]]
    r <- d[[vr]]
    ifelse(!is.na(l), l, r)
  }

  .custom_var_registry[[name]] <<- list(
    label           = if (nchar(trimws(label)) > 0) label else name,
    category        = if (nchar(trimws(category)) > 0) category else "Custom",
    type            = "merge",
    merge_left      = var_left,
    merge_right     = var_right,
    requires        = c(var_left, var_right),
    is_zscore       = FALSE,
    compute         = compute_fn,
    log10_transform = FALSE,
    label_map       = NULL
  )
  save_custom_variables()
  list(success=TRUE,
       message=sprintf("Merge variable '%s' registered: %s ← %s (fallback: %s)",
                       name, name, var_left, var_right))
}

## Delete a custom variable
delete_custom_variable <- function(name) {
  if (!(name %in% names(.custom_var_registry)))
    return(list(success=FALSE, message=sprintf("'%s' not found", name)))
  .custom_var_registry[[name]] <<- NULL
  save_custom_variables()
  list(success=TRUE, message=sprintf("'%s' deleted", name))
}

## Get all custom variables (for display or dependency checking)
get_custom_variables <- function() .custom_var_registry

## Apply a custom variable to a data frame (non-zscore only)
## zscore variables are applied at analysis time via apply_zscore_vars()
apply_custom_variable <- function(data, name) {
  entry <- .custom_var_registry[[name]]
  if (is.null(entry)) stop(sprintf("Custom variable '%s' not found", name))
  if (!is.list(entry)) stop(sprintf("Custom variable '%s' registry entry is corrupted", name))
  if (isTRUE(entry$is_zscore)) return(data)

  requires <- entry$requires %||% character(0)
  missing_src <- setdiff(requires, names(data))
  if (length(missing_src) > 0) {
    warning(sprintf(
      "Custom variable '%s': columns %s not in database — treating as NA",
      name, paste(missing_src, collapse=", ")))
    for (col in missing_src) data[[col]] <- NA_real_
  }

  ## Force to plain data.frame so formula evaluation produces plain vectors
  ## not tibble columns with compound class attributes
  d_plain <- as.data.frame(data)

  result <- tryCatch({
    raw <- entry$compute(d_plain)
    ## Strip any remaining class attributes — coerce to plain atomic vector
    plain <- suppressWarnings(as.numeric(raw))
    if (all(is.na(plain) | plain == floor(plain), na.rm=TRUE)) {
      as.integer(plain)
    } else {
      plain
    }
  }, error = function(e) {
    warning(sprintf("Failed to compute '%s': %s", name, conditionMessage(e)))
    rep(NA_real_, nrow(data))
  })

  data[[name]] <- result
  if (length(missing_src) > 0) data[, missing_src] <- NULL
  data
}

## Apply zscore variables at analysis time (on filtered population)
apply_zscore_vars <- function(data, var_names) {
  for (nm in var_names) {
    entry <- .custom_var_registry[[nm]]
    if (!is.null(entry) && isTRUE(entry$is_zscore)) {
      data <- tryCatch(
        { data[[nm]] <- entry$compute(data); data },
        error = function(e) {
          warning(sprintf("zscore computation failed for '%s': %s", nm, conditionMessage(e)))
          data
        }
      )
    }
  }
  data
}

## Load on source
load_custom_variables()
