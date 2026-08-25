## ============================================================================
## project_io.R
##
## Save / load project state for the NHANES Analysis Tool.
##
## A "project" is a plain R list serialised as an RDS file.  It captures
## every piece of analysis configuration so the user can re-open the app and
## pick up exactly where they left off.
##
## Structure of a saved project (version 2):
##   $version          integer
##   $saved_at         POSIXct
##   $analysis_type    character
##   $settings         named list  (type-specific inputs)
##   $complete_cases   logical
##   $exposure_rows    list of list(var, transform)
##   $outcome_rows     list of list(var, transform)
##   $shared_covariates character vector
##   $n_pops           integer
##   $populations      list (length n_pops), each element:
##       $label         character
##       $cycles        character vector
##       $custom_covars logical
##       $covariates    character vector (only meaningful when custom_covars TRUE)
##       $constraints   list of list(var, op, val)
##   $custom_var_defs  named list of custom variable snapshots (compute stripped):
##       formula vars:  label, category, formula, requires, is_zscore,
##                      log10_transform, label_map, or_groups
##       merge vars:    label, category, type="merge", merge_left, merge_right,
##                      requires, is_zscore, log10_transform
## ============================================================================

PROJECT_VERSION <- 2L

## ---------------------------------------------------------------------------
## .project_var_names()
##   Internal helper: collect all variable names referenced in a project list.
## ---------------------------------------------------------------------------
.project_var_names <- function(proj_list) {
  exp_vars     <- Filter(nzchar, vapply(proj_list$exposure_rows %||% list(),
                                        `[[`, character(1), "var"))
  out_vars     <- Filter(nzchar, vapply(proj_list$outcome_rows  %||% list(),
                                        `[[`, character(1), "var"))
  cov_vars     <- proj_list$shared_covariates %||% character(0)
  med_vars     <- (proj_list$settings %||% list())$mediators %||% character(0)
  pop_cov_vars <- unique(unlist(lapply(proj_list$populations %||% list(), function(p) {
    if (isTRUE(p$custom_covars)) p$covariates %||% character(0) else character(0)
  })))
  con_vars <- unique(unlist(lapply(proj_list$populations %||% list(), function(p) {
    Filter(nzchar, vapply(p$constraints %||% list(),
                          function(cc) cc$var %||% "", character(1)))
  })))
  unique(tolower(c(exp_vars, out_vars, cov_vars, med_vars, pop_cov_vars, con_vars)))
}

## ---------------------------------------------------------------------------
## collect_project()
##   Called inside the Shiny server; gathers all reactive inputs and
##   rv values into a plain serialisable list.
##
## Arguments:
##   input           — the Shiny input object
##   rv              — the Shiny reactiveValues object
##   pop_constraints — the per-population constraint reactiveValues
## ---------------------------------------------------------------------------
collect_project <- function(input, rv, pop_constraints) {
  n <- rv$n_pops

  ## ---- Per-population state -----------------------------------------------
  populations <- lapply(seq_len(n), function(i) {
    pop_id <- paste0("pop", i)
    list(
      label         = input[[paste0("pop_label_",   i)]] %||% rv$pop_labels[i],
      cycles        = input[[paste0("pop_cycles_",  i)]] %||% character(0),
      custom_covars = isTRUE(input[[paste0("custom_covars_", i)]]),
      covariates    = input[[paste0("pop_covariates_", i)]] %||% character(0),
      constraints   = isolate(pop_constraints[[pop_id]]) %||% list()
    )
  })

  ## ---- Analysis-type-specific settings ------------------------------------
  settings <- list(
    spline_df        = input$spline_df        %||% 3L,
    wqs_q            = input$wqs_q            %||% 4L,
    wqs_boot         = input$wqs_boot         %||% 100L,
    qgcomp_q         = input$qgcomp_q         %||% 4L,
    qgcomp_boot      = isTRUE(input$qgcomp_boot),
    qgcomp_nboot     = input$qgcomp_nboot     %||% 200L,
    bkmr_iter        = input$bkmr_iter        %||% 1000L,
    bkmr_seed        = input$bkmr_seed        %||% 42L,
    mediators           = input$mediators           %||% character(0),
    mediation_nsims     = input$mediation_nsims     %||% 500L,
    logistic_ref_lowest = isTRUE(input$logistic_ref_lowest)
  )

  proj_list <- list(
    version            = PROJECT_VERSION,
    saved_at           = Sys.time(),
    project_name       = input$project_name_input %||% "nhanes_project",
    analysis_type      = input$analysis_type %||% "linear",
    settings           = settings,
    complete_cases     = isTRUE(input$complete_cases_only),
    exposure_rows      = rv$exposure_rows,
    outcome_rows       = rv$outcome_rows,
    shared_covariates  = input$shared_covariates %||% character(0),
    n_pops             = n,
    populations        = populations
  )

  ## Snapshot custom variable definitions used by this project so the file is
  ## self-contained. compute functions are stripped (re-derived on registration).
  used_cv_names <- intersect(.project_var_names(proj_list), names(get_custom_variables()))
  cv_registry   <- get_custom_variables()
  proj_list$custom_var_defs <- lapply(cv_registry[used_cv_names], function(cv) {
    cv$compute <- NULL
    cv
  })

  proj_list
}

## ---------------------------------------------------------------------------
## save_project()
##   Serialises the project list to an RDS file at `path`.
## ---------------------------------------------------------------------------
save_project <- function(project, path) {
  saveRDS(project, path)
  invisible(path)
}

## ---------------------------------------------------------------------------
## load_project()
##   Reads and validates an RDS file, returning the project list.
##   Throws if the file is not a valid NHANES project.
## ---------------------------------------------------------------------------
load_project <- function(path) {
  proj <- tryCatch(
    readRDS(path),
    error = function(e) stop(sprintf("Cannot read project file: %s", conditionMessage(e)))
  )
  if (!is.list(proj) || is.null(proj$version))
    stop("File does not appear to be a valid NHANES project.")
  if (proj$version > PROJECT_VERSION)
    warning(sprintf(
      "Project was saved with a newer version (%d); some settings may be ignored.",
      proj$version))
  proj
}

## ---------------------------------------------------------------------------
## check_project_custom_vars()
##   Compares the custom variable definitions embedded in a project against
##   the current registry. Returns a named list of variables that are either
##   missing from or formulaically different in the current registry.
##
##   Each element: list(status = "missing"|"mismatch", def = <saved entry>,
##                       existing = <current entry or NULL>)
## ---------------------------------------------------------------------------
check_project_custom_vars <- function(proj) {
  defs <- proj$custom_var_defs %||% list()
  if (length(defs) == 0) return(list())
  current <- get_custom_variables()

  needs_action <- list()
  for (nm in names(defs)) {
    saved    <- defs[[nm]]
    existing <- current[[nm]]

    if (is.null(existing)) {
      needs_action[[nm]] <- list(status = "missing", def = saved, existing = NULL)
    } else {
      ## Detect mismatch: compare formula (formula vars) or left/right (merge vars)
      saved_type    <- saved$type    %||% "formula"
      existing_type <- existing$type %||% "formula"
      mismatch <- saved_type != existing_type ||
        (!identical(saved_type, "merge") &&
         !identical(saved$formula %||% "", existing$formula %||% "")) ||
        (identical(saved_type, "merge") &&
         (!identical(saved$merge_left,  existing$merge_left) ||
          !identical(saved$merge_right, existing$merge_right)))
      if (mismatch)
        needs_action[[nm]] <- list(status = "mismatch", def = saved, existing = existing)
    }
  }
  needs_action
}

## ---------------------------------------------------------------------------
## restore_project()
##   Applies a loaded project back to the Shiny session.
##   Must be called inside server() where session, input, rv, etc. are in scope.
##
## Arguments:
##   proj             — result of load_project()
##   session          — Shiny session object
##   rv               — reactiveValues
##   pop_constraints  — per-population constraint reactiveValues
##   pop_constraint_counts — per-population constraint count reactiveValues
## ---------------------------------------------------------------------------
restore_project <- function(proj, session, rv,
                             pop_constraints, pop_constraint_counts) {

  ## ---- Analysis type -------------------------------------------------------
  updateSelectInput(session, "analysis_type",
                    selected = proj$analysis_type %||% "linear")

  ## ---- Analysis settings ---------------------------------------------------
  s <- proj$settings %||% list()
  if (!is.null(s$spline_df))
    updateNumericInput(session, "spline_df",       value = s$spline_df)
  if (!is.null(s$wqs_q))
    updateNumericInput(session, "wqs_q",           value = s$wqs_q)
  if (!is.null(s$wqs_boot))
    updateNumericInput(session, "wqs_boot",        value = s$wqs_boot)
  if (!is.null(s$qgcomp_q))
    updateNumericInput(session, "qgcomp_q",        value = s$qgcomp_q)
  if (!is.null(s$qgcomp_boot))
    updateCheckboxInput(session,"qgcomp_boot",     value = s$qgcomp_boot)
  if (!is.null(s$qgcomp_nboot))
    updateNumericInput(session, "qgcomp_nboot",    value = s$qgcomp_nboot)
  if (!is.null(s$bkmr_iter))
    updateNumericInput(session, "bkmr_iter",       value = s$bkmr_iter)
  if (!is.null(s$bkmr_seed))
    updateNumericInput(session, "bkmr_seed",       value = s$bkmr_seed)
  if (!is.null(s$mediators) && length(s$mediators) > 0)
    updateSelectizeInput(session,"mediators",      selected = s$mediators)
  if (!is.null(s$mediation_nsims))
    updateNumericInput(session, "mediation_nsims", value = s$mediation_nsims)

  ## ---- Complete cases / logistic ref-lowest checkboxes ---------------------
  updateCheckboxInput(session, "complete_cases_only",
                      value = isTRUE(proj$complete_cases))
  if (!is.null(s$logistic_ref_lowest))
    updateCheckboxInput(session, "logistic_ref_lowest",
                        value = isTRUE(s$logistic_ref_lowest))

  ## ---- Exposure / outcome rows --------------------------------------------
  rv$exposure_rows <- if (!is.null(proj$exposure_rows) &&
                           length(proj$exposure_rows) > 0)
    proj$exposure_rows else list(list(var="", transform="none"))

  rv$outcome_rows  <- if (!is.null(proj$outcome_rows) &&
                           length(proj$outcome_rows) > 0) {
    lapply(proj$outcome_rows, function(r) {
      if (is.null(r$wqs_direction)) r$wqs_direction <- "positive"
      r
    })
  } else list(list(var="", transform="none", wqs_direction="positive"))

  ## ---- Shared covariates --------------------------------------------------
  if (!is.null(proj$shared_covariates) && length(proj$shared_covariates) > 0)
    updateSelectizeInput(session, "shared_covariates",
                         selected = proj$shared_covariates)

  ## ---- Populations --------------------------------------------------------
  n <- proj$n_pops %||% 1L
  rv$n_pops     <- n
  rv$pop_labels <- vapply(seq_len(n), function(i) {
    proj$populations[[i]]$label %||% paste("Population", i)
  }, character(1))

  ## Restore per-population state after a brief delay so population tabs
  ## have rendered.  We use shinyjs::delay or invalidateLater; here we
  ## rely on the observer firing after rv$n_pops updates.
  for (i in seq_len(n)) {
    local({
      ii   <- i
      pop  <- proj$populations[[ii]]
      pop_id <- paste0("pop", ii)

      ## Constraint rows — set reactive values so renderUI picks them up
      cons <- pop$constraints %||% list()
      pop_constraints[[pop_id]]       <- cons
      pop_constraint_counts[[pop_id]] <- length(cons)

      ## Persist cycle selection in rv so re-renders don't reset it
      saved_cycles <- pop$cycles %||% c("1999-2000","2001-2002","2003-2004","2005-2006",
                                         "2007-2008","2009-2010","2011-2012","2013-2014",
                                         "2015-2016","2017-2018")
      rv$pop_cycles[[pop_id]]        <- saved_cycles
      rv$pop_custom_covars[[pop_id]] <- isTRUE(pop$custom_covars)

      ## These updates fire after the population tabs UI re-renders
      ## (they are deferred by the Shiny event loop automatically since
      ## rv$n_pops assignment triggers a re-render first).
      updateTextInput(session, paste0("pop_label_", ii),
                      value = pop$label %||% paste("Population", ii))
      updateCheckboxGroupInput(session, paste0("pop_cycles_", ii),
                               selected = saved_cycles)
      updateCheckboxInput(session, paste0("custom_covars_", ii),
                          value = isTRUE(pop$custom_covars))
      if (isTRUE(pop$custom_covars) && length(pop$covariates) > 0)
        updateSelectizeInput(session, paste0("pop_covariates_", ii),
                             selected = pop$covariates)
    })
  }

  ## Trigger a full rebuild of the population tabset so tab count, labels,
  ## cycles, and custom-covars all reflect the restored state.
  rv$pop_tabs_reset <- isolate(rv$pop_tabs_reset) + 1L

  invisible(NULL)
}

## ---------------------------------------------------------------------------
## generate_r_script()
##   Generates a fully self-contained R script that reproduces the current
##   analysis.  The output has NO dependency on the nhanes-gui app — it only
##   requires the database RDS file and standard CRAN packages.
##
## Arguments:
##   input           — Shiny input object
##   rv              — Shiny reactiveValues
##   pop_constraints — per-population constraint reactiveValues
## ---------------------------------------------------------------------------
generate_r_script <- function(input, rv, pop_constraints) {

  lines <- character(0)
  add   <- function(...) lines <<- c(lines, paste0(...))
  emit  <- function(x)   lines <<- c(lines, Filter(Negate(is.null), as.character(x)))
  blank <- function()    lines <<- c(lines, "")
  r_vec <- function(x) {
    if (length(x) == 0) return("character(0)")
    paste0("c(", paste(sprintf('"%s"', x), collapse = ", "), ")")
  }

  atype       <- input$analysis_type %||% "linear"
  valid_exp   <- Filter(function(r) nzchar(r$var), rv$exposure_rows)
  valid_out   <- Filter(function(r) nzchar(r$var), rv$outcome_rows)
  exp_vars    <- vapply(valid_exp, `[[`, character(1), "var")
  out_vars    <- vapply(valid_out, `[[`, character(1), "var")
  shared_covs <- input$shared_covariates %||% character(0)
  n_pops      <- rv$n_pops %||% 1L
  med_vars    <- if (atype == "mediation") input$mediators %||% character(0) else character(0)

  ## Collect ALL covariates used across populations
  all_covs <- shared_covs
  for (i in seq_len(n_pops)) {
    if (isTRUE(input[[paste0("custom_covars_", i)]])) {
      all_covs <- union(all_covs,
                        input[[paste0("pop_covariates_", i)]] %||% character(0))
    }
  }

  all_var_names <- tolower(unique(c(exp_vars, out_vars, all_covs, med_vars)))

  ## Which built-in derived variables are referenced?
  BUILTIN_DERIVED <- c("sex_bin","race_hispanic","race_black","race_other",
                        "smoking_status","edu_clean","hascvd","oncvdmed",
                        "non_hdl","remnant","egfr","usfli")
  needed_builtin <- intersect(all_var_names, BUILTIN_DERIVED)

  ## Which user-defined custom variables are referenced?
  cvars         <- get_custom_variables()
  needed_custom <- intersect(all_var_names, names(cvars))

  need_race_sex <- any(c("sex_bin","race_hispanic","race_black","race_other") %in% needed_builtin)
  need_cvd      <- "hascvd"         %in% needed_builtin
  need_cvdmed   <- "oncvdmed"       %in% needed_builtin
  need_smoking  <- "smoking_status" %in% needed_builtin
  need_edu      <- "edu_clean"      %in% needed_builtin
  need_non_hdl  <- "non_hdl"        %in% needed_builtin
  need_remnant  <- "remnant"        %in% needed_builtin
  need_egfr     <- "egfr"           %in% needed_builtin
  need_usfli    <- "usfli"          %in% needed_builtin

  has_log10 <- atype %in% c("linear","logistic","spline") &&
    any(vapply(valid_exp, function(r) isTRUE(r$transform == "log10"), logical(1)))

  ## ---- Header ---------------------------------------------------------------
  type_label <- switch(atype,
    linear="Linear Regression", logistic="Logistic Regression",
    spline="Spline (Non-linear)", multinomial="Multinomial Regression",
    wqs="WQS Mixture", qgcomp="qgcomp Mixture", bkmr="BKMR Mixture",
    mediation="Mediation Analysis", timetrend="Time-Trend", atype)

  add("## ============================================================================")
  add("## NHANES Analysis — ", type_label)
  add("## Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M"))
  add("## Self-contained — no nhanes-gui dependency.")
  add("## Packages needed: survey, dplyr, splines",
      if (atype == "wqs")         ", gWQS"    else "",
      if (atype == "qgcomp")      ", qgcomp"  else "",
      if (atype == "bkmr")        ", bkmr"    else "",
      if (atype == "multinomial") ", nnet"    else "")
  add("## ============================================================================")
  blank()

  ## ---- Libraries ------------------------------------------------------------
  add("library(survey)")
  add("library(dplyr)")
  add("library(splines)")
  if (atype == "wqs")         add("library(gWQS)")
  if (atype == "qgcomp")      add("library(qgcomp)")
  if (atype == "bkmr")        add("library(bkmr)")
  if (atype == "multinomial") add("library(nnet)")
  blank()

  ## ---- Infrastructure helpers (always emitted) ------------------------------
  add("## ---- Infrastructure ---------------------------------------------------------")
  emit(c(
    '`%||%` <- function(a, b) if (!is.null(a)) a else b',
    '',
    'make_constraint <- function(variable, op, value = NULL)',
    '  list(variable = variable, op = op, value = value)',
    '',
    'apply_constraint <- function(data, con) {',
    '  col <- data[[con$variable]]',
    '  switch(con$op,',
    '    "=="        = col == con$value,',
    '    "!="        = col != con$value,',
    '    ">"         = col >  con$value,',
    '    ">="        = col >= con$value,',
    '    "<"         = col <  con$value,',
    '    "<="        = col <= con$value,',
    '    "%in%"      = col %in% con$value,',
    '    "not_na"    = !is.na(col),',
    '    "na_or_neq" = is.na(col) | (!is.na(col) & col != con$value),',
    '    stop(sprintf("Unknown constraint op \'%s\'", con$op))',
    '  )',
    '}',
    '',
    'apply_population_filter <- function(data, constraints) {',
    '  if (length(constraints) == 0) return(data)',
    '  mask <- rep(TRUE, nrow(data))',
    '  for (con in constraints) {',
    '    m <- apply_constraint(data, con)',
    '    m[is.na(m)] <- FALSE',
    '    mask <- mask & m',
    '  }',
    '  data[mask, , drop = FALSE]',
    '}',
    '',
    '## Apply constraints one-by-one, printing N and dropped count after each.',
    'apply_filter_trace <- function(data, constraints) {',
    '  if (length(constraints) == 0) return(data)',
    '  for (con in constraints) {',
    '    before <- nrow(data)',
    '    data   <- as.data.frame(apply_population_filter(data, list(con)))',
    '    val_str <- if (is.null(con$value)) ""',
    '               else paste(con$value, collapse = ",")',
    '    cat(sprintf("  Constraint [%s %s %s]: N = %d (dropped %d)\\n",',
    '                con$variable, con$op, val_str, nrow(data), before - nrow(data)))',
    '  }',
    '  data',
    '}',
    '',
    'build_survey_design <- function(data) {',
    '  options(survey.lonely.psu = "certainty")',
    '  stra <- if ("SDMVSTRA" %in% names(data)) "SDMVSTRA" else "sdmvstra"',
    '  psu  <- if ("SDMVPSU"  %in% names(data)) "SDMVPSU"  else "sdmvpsu"',
    '  data <- data[!is.na(data$pooled_weight) & data$pooled_weight > 0, ]',
    '  data <- data[!is.na(data[[stra]]) & !is.na(data[[psu]]), ]',
    '  svydesign(ids     = as.formula(paste0("~", psu)),',
    '            strata  = as.formula(paste0("~", stra)),',
    '            weights = ~pooled_weight, nest = TRUE, data = data)',
    '}',
    '',
    '## Assign the correct NHANES analytic weight for an exposure-outcome pair,',
    '## matching the app weight hierarchy (most to least restrictive):',
    '##   pfas_weight_pooled  — PFAS subsample exposures',
    '##   wtsaf               — fasting lab outcomes (LDL, cholesterol)',
    '##   wtmec2yr            — standard MEC exam weight (no cycle division)',
    '##   wtint2yr            — interview-only fallback',
    '## Subsample weights (PFAS) are already pre-divided by n_cycles in the',
    '## saved database (pfas_weight_pooled). wtmec2yr is NOT divided because',
    '## each 2-year cycle weight is already calibrated for that cycle.',
    'resolve_survey_weight <- function(data, exposures, outcome) {',
    '  pfas_vars    <- c("lbxpfoa","lbxpfos","lbxpfhs","lbxpfna",',
    '                     "pfoa","pfos","pfhxs","pfhs","pfna")',
    '  fasting_vars <- c("lbdldl","lbxtc","lbdhdd","non_hdl","remnant")',
    '  all_vars     <- tolower(c(exposures, outcome))',
    '  is_pfas      <- any(all_vars %in% pfas_vars)',
    '  is_fasting   <- any(all_vars %in% fasting_vars)',
    '  if (is_pfas && "pfas_weight_pooled" %in% names(data)) {',
    '    ## Pre-divided pooled PFAS weight already in database',
    '    data$pooled_weight <- data$pfas_weight_pooled',
    '  } else if (is_pfas && "pfas_weight" %in% names(data)) {',
    '    ## Fallback: divide raw PFAS weight by number of PFAS cycles',
    '    n_cyc <- length(unique(data$year[!is.na(data$pfas_weight) & data$pfas_weight > 0]))',
    '    data$pooled_weight <- data$pfas_weight / max(n_cyc, 1L)',
    '  } else if (is_fasting && "wtsaf" %in% names(data)) {',
    '    ## Fasting subsample weight for LDL/cholesterol outcomes',
    '    data$pooled_weight <- data$wtsaf',
    '  } else if ("wtmec2yr" %in% names(data)) {',
    '    ## Standard MEC weight — NOT divided by n_cycles per NHANES guidelines',
    '    data$pooled_weight <- data$wtmec2yr',
    '  } else if ("wtint2yr" %in% names(data)) {',
    '    data$pooled_weight <- data$wtint2yr',
    '  } else {',
    '    stop("No survey weight found (expected wtmec2yr, wtsaf, or pfas_weight_pooled)")',
    '  }',
    '  data',
    '}'
  ))
  blank()

  ## ---- Derived variable builders (only what is needed) ----------------------
  if (length(needed_builtin) > 0) {
    add("## ---- Derived variable builders ----------------------------------------------")
    blank()
  }

  if (need_race_sex) {
    emit(c(
      'add_matlab_equivalent_covariates <- function(data) {',
      '  ## sex_bin: NHANES 1=male 2=female -> 0=male 1=female',
      '  sx <- if ("SEX"  %in% names(data)) data$SEX  else data$sex',
      '  data$sex_bin <- as.numeric(sx) - 1',
      '  ## race dummies (RIDRETH1: 1-2=Hispanic, 3=NHW ref, 4=NHB, 5=Other)',
      '  rc <- if ("RACE" %in% names(data)) data$RACE else data$race',
      '  data$race_hispanic <- as.numeric(rc == 1 | rc == 2)',
      '  data$race_black    <- as.numeric(rc == 4)',
      '  data$race_other    <- as.numeric(rc == 5)',
      '  data',
      '}'
    ))
    blank()
  }

  if (need_cvd) {
    emit(c(
      'add_has_cvd <- function(data) {',
      '  mcq_u <- c("CVD_MCQ160B","CVD_MCQ160C","CVD_MCQ160D","CVD_MCQ160E","CVD_MCQ160F")',
      '  mcq_l <- c("mcq160b","mcq160c","mcq160d","mcq160e","mcq160f")',
      '  cols  <- if (all(mcq_u %in% names(data))) mcq_u',
      '           else if (all(mcq_l %in% names(data))) mcq_l',
      '           else { message("add_has_cvd: MCQ160 columns not found"); return(data) }',
      '  m      <- as.matrix(data[, cols])',
      '  any_y  <- apply(m == 1, 1, any, na.rm = TRUE)',
      '  all_na <- apply(is.na(m), 1, all)',
      '  data$hascvd <- ifelse(all_na, NA_real_, as.numeric(any_y))',
      '  data',
      '}'
    ))
    blank()
  }

  if (need_cvdmed) {
    emit(c(
      'add_on_cvd_med <- function(data) {',
      '  col <- if ("ONCVDMED_RAW" %in% names(data)) "ONCVDMED_RAW"',
      '         else if ("oncvdmed_raw" %in% names(data)) "oncvdmed_raw"',
      '         else if ("BPQ100D"      %in% names(data)) "BPQ100D"',
      '         else if ("bpq100d"      %in% names(data)) "bpq100d"',
      '         else NULL',
      '  if (is.null(col)) { message("add_on_cvd_med: source column not found"); return(data) }',
      '  data$oncvdmed <- as.numeric(data[[col]] == 1)',
      '  data',
      '}'
    ))
    blank()
  }

  if (need_smoking) {
    emit(c(
      'add_smoking_status <- function(data) {',
      '  if (!all(c("smq020","smq040") %in% names(data))) return(data)',
      '  s020 <- suppressWarnings(as.numeric(as.character(data$smq020)))',
      '  s040 <- suppressWarnings(as.numeric(as.character(data$smq040)))',
      '  s    <- rep(NA_real_, nrow(data))',
      '  s[!is.na(s020) & s020 == 2]                                       <- 0',
      '  s[!is.na(s020) & s020 == 1 & !is.na(s040) & s040 == 3]           <- 1',
      '  s[!is.na(s020) & s020 == 1 & !is.na(s040) & s040 %in% c(1, 2)]   <- 2',
      '  data$smoking_status <- s',
      '  data',
      '}'
    ))
    blank()
  }

  if (need_edu) {
    emit(c(
      'add_edu_clean <- function(data) {',
      '  if (!"education" %in% names(data)) return(data)',
      '  edu <- data$education',
      '  edu[!is.na(edu) & edu > 5] <- NA_real_',
      '  data$edu_clean <- edu',
      '  data',
      '}'
    ))
    blank()
  }

  if (need_non_hdl || need_remnant) {
    emit(c(
      'add_non_hdl <- function(data) {',
      '  if (!all(c("lbxtc","lbdhdd") %in% names(data))) return(data)',
      '  data$non_hdl <- data$lbxtc - data$lbdhdd',
      '  data',
      '}',
      '',
      'add_remnant <- function(data) {',
      '  if (!all(c("lbxtc","lbdhdd","lbdldl") %in% names(data))) return(data)',
      '  data$remnant <- data$lbxtc - data$lbdhdd - data$lbdldl',
      '  data',
      '}'
    ))
    blank()
  }

  if (need_egfr) {
    emit(c(
      '## CKD-EPI 2009 eGFR with Selvin 2007 creatinine recalibration',
      'add_egfr <- function(data) {',
      '  req <- c("lbxscr","age","sex","race","year")',
      '  if (!all(req %in% names(data))) return(data)',
      '  scr  <- as.numeric(data$lbxscr)',
      '  age  <- as.numeric(data$age)',
      '  sex  <- as.numeric(data$sex)',
      '  race <- as.numeric(data$race)',
      '  yr   <- as.integer(data$year)',
      '  ## Cycle-specific recalibration to Roche enzymatic standard',
      '  scr[!is.na(yr) & yr == 1999] <- 1.013 * scr[!is.na(yr) & yr == 1999] + 0.147',
      '  scr[!is.na(yr) & yr == 2005] <- 0.978 * scr[!is.na(yr) & yr == 2005] + 0.006',
      '  egfr <- rep(NA_real_, nrow(data))',
      '  for (i in seq_len(nrow(data))) {',
      '    if (is.na(scr[i]) || is.na(age[i]) || is.na(sex[i])) next',
      '    if (sex[i] == 2) { k <- 0.7; a <- -0.329 } else { k <- 0.9; a <- -0.411 }',
      '    r       <- scr[i] / k',
      '    val     <- 141 * min(r,1)^a * max(r,1)^(-1.209) * 0.993^age[i]',
      '    if (sex[i]  == 2)              val <- val * 1.018',
      '    if (!is.na(race[i]) && race[i] == 4) val <- val * 1.159',
      '    egfr[i] <- val',
      '  }',
      '  data$egfr <- egfr',
      '  data',
      '}'
    ))
    blank()
  }

  if (need_usfli) {
    emit(c(
      '## US Fatty Liver Index (Ruhl & Everhart 2012)',
      'add_usfli <- function(data) {',
      '  req <- c("ridreth1","ridageyr","lbxsgtsi","bmxwaist","lbxin","lbxglu")',
      '  if (!all(req %in% names(data))) return(data)',
      '  black   <- as.integer(data$ridreth1 == 4)',
      '  mexican <- as.integer(data$ridreth1 == 1)',
      '  X <- (-0.8073 * black) + (0.3458 * mexican) +',
      '       ( 0.0093 * data$ridageyr) + (0.6151 * log(data$lbxsgtsi)) +',
      '       ( 0.0249 * data$bmxwaist) + (1.1792 * log(data$lbxin)) +',
      '       ( 0.8242 * log(data$lbxglu)) - 14.7812',
      '  data$usfli <- (exp(X) / (1 + exp(X))) * 100',
      '  data',
      '}'
    ))
    blank()
  }

  ## ---- Custom variable compute functions ------------------------------------
  if (length(needed_custom) > 0) {
    add("## ---- Custom variable builders -----------------------------------------------")
    blank()
    for (nm in needed_custom) {
      cv <- cvars[[nm]]
      if (is.null(cv)) next
      add("## ", nm, " — ", cv$label %||% nm)
      if (isTRUE(cv$is_zscore)) {
        src <- cv$requires[1L]
        emit(c(
          sprintf('compute_%s <- function(d) {', nm),
          sprintf('  vals <- d[["%s"]]', src),
          '  (vals - mean(vals, na.rm = TRUE)) / sd(vals, na.rm = TRUE)',
          '}'
        ))
      } else {
        r_expr <- cv$r_expr %||% paste0("d$", nm)
        emit(c(
          sprintf('compute_%s <- function(d) {', nm),
          sprintf('  %s', r_expr),
          '}'
        ))
      }
      blank()
    }
  }

  ## ---- Analysis function (type-specific) ------------------------------------
  add("## ---- Analysis function ------------------------------------------------------")
  blank()

  if (atype %in% c("linear","logistic","spline")) {
    emit(c(
      '## Per-pair regression matching run_analysis() in cli.R exactly:',
      '## each exposure-outcome pair gets its own population filter (exposure + outcome',
      '## not_na), its own survey weight (PFAS/fasting/MEC hierarchy), and its own',
      '## svydesign built with strata + PSU + weight for proper TSL variance.',
      'run_regression_grid <- function(pop_data, outcomes, exposures, covariates,',
      '                                log10_flags = NULL, family = "gaussian",',
      '                                spline_df = NULL, min_n = 100) {',
      '  fam_obj <- switch(tolower(family),',
      '    "logistic" = , "binomial" = quasibinomial(), gaussian())',
      '  cov_rhs <- if (length(covariates) > 0) paste(covariates, collapse = " + ") else "1"',
      '  results <- list()',
      '  for (outcome in outcomes) {',
      '    for (j in seq_along(exposures)) {',
      '      exposure  <- exposures[j]',
      '      ## Filter to complete cases for this specific pair',
      '      pair_vars <- intersect(c(exposure, outcome, covariates), names(pop_data))',
      '      pair_data <- pop_data[complete.cases(pop_data[, pair_vars, drop=FALSE]), ]',
      '      pair_data <- as.data.frame(pair_data)',
      '      if (nrow(pair_data) < min_n) {',
      '        message(sprintf("Skipping %s ~ %s: N=%d < %d",',
      '                        outcome, exposure, nrow(pair_data), min_n))',
      '        next',
      '      }',
      '      ## Assign correct survey weight for this pair (PFAS > fasting > MEC)',
      '      pair_data <- resolve_survey_weight(pair_data, exposure, outcome)',
      '      ## Build proper TSL survey design: strata + PSU + pooled_weight',
      '      design    <- build_survey_design(pair_data)',
      '      flag      <- if (!is.null(log10_flags)) isTRUE(log10_flags[j]) else FALSE',
      '      exp_term  <- if (flag) sprintf("log10(%s)", exposure) else exposure',
      '      if (!is.null(spline_knots) && length(spline_knots) >= 1) {',
      '        exp_term <- sprintf("ns(%s, knots=c(%s))", exp_term, paste(spline_knots, collapse=", "))',
      '      } else if (!is.null(spline_df) && spline_df >= 2) {',
      '        exp_term <- sprintf("ns(%s, df = %d)", exp_term, as.integer(spline_df))',
      '      }',
      '      fml <- as.formula(sprintf("%s ~ %s + %s", outcome, exp_term, cov_rhs))',
      '      fit <- tryCatch(',
      '        svyglm(fml, design = design, family = fam_obj),',
      '        error = function(e) { message("Error: ", e$message); NULL })',
      '      if (is.null(fit)) next',
      '      coefs <- as.data.frame(summary(fit)$coefficients)',
      '      ci    <- tryCatch(confint(fit), error = function(e) matrix(NA, nrow(coefs), 2))',
      '      coefs$ci_low  <- ci[, 1]',
      '      coefs$ci_high <- ci[, 2]',
      '      results[[paste(outcome, exposure, sep = " ~ ")]] <-',
      '        list(outcome=outcome, exposure=exposure,',
      '             formula=deparse(fml), n=nrow(model.frame(fit)), coefs=coefs)',
      '    }',
      '  }',
      '  results',
      '}',
      '',
      'print_regression_results <- function(results) {',
      '  for (nm in names(results)) {',
      '    r     <- results[[nm]]',
      '    p_col <- grep("Pr\\\\(", names(r$coefs), value = TRUE)[1]',
      '    cols  <- c("Estimate", "Std. Error", "ci_low", "ci_high", p_col)',
      '    cat(sprintf("\\n## %s   N = %d\\n", nm, r$n))',
      '    print(r$coefs[, intersect(cols, names(r$coefs)), drop = FALSE], digits = 4)',
      '  }',
      '}'
    ))

  } else if (atype == "wqs") {
    emit(c(
      'run_wqs_outcome <- function(data, exposures, outcome, covariates,',
      '                             direction, n_q, n_boot, seed = 42) {',
      '  keep    <- intersect(c(exposures, outcome, covariates, "pooled_weight"),',
      '                       names(data))',
      '  data_cc <- data[complete.cases(data[, keep, drop = FALSE]), keep, drop = FALSE]',
      '  cov_rhs <- if (length(covariates) > 0) paste(covariates, collapse = " + ") else "1"',
      '  fml     <- as.formula(sprintf("%s ~ wqs + %s", outcome, cov_rhs))',
      '  set.seed(seed)',
      '  gwqs(fml, mix_name = exposures, data = data_cc,',
      '       q = n_q, b = n_boot,',
      '       b1_pos  = (direction == "positive"),',
      '       family  = "gaussian",',
      '       weights = "pooled_weight",',
      '       seed    = seed)',
      '}'
    ))

  } else if (atype == "qgcomp") {
    emit(c(
      'run_qgcomp_outcome <- function(data, exposures, outcome, covariates,',
      '                                n_q, seed = 42) {',
      '  keep    <- intersect(c(exposures, outcome, covariates, "pooled_weight",',
      '                          "sdmvpsu", "sdmvstra"), names(data))',
      '  data_cc <- data[complete.cases(data[, keep, drop = FALSE]), , drop = FALSE]',
      '  all_rhs <- paste(c(exposures, covariates), collapse = " + ")',
      '  fml     <- as.formula(sprintf("%s ~ %s", outcome, all_rhs))',
      '  set.seed(seed)',
      '  if (existsFunction("qgcomp.svy")) {',
      '    options(survey.lonely.psu = "certainty")',
      '    des <- svydesign(ids = ~sdmvpsu, strata = ~sdmvstra,',
      '                     weights = ~pooled_weight, nest = TRUE, data = data_cc)',
      '    qgcomp.svy(fml, expnms = exposures, design = des,',
      '               q = n_q, family = gaussian())',
      '  } else {',
      '    qgcomp.noboot(fml, expnms = exposures, data = data_cc,',
      '                  q = n_q, family = gaussian(),',
      '                  weights = data_cc$pooled_weight)',
      '  }',
      '}'
    ))

  } else if (atype == "bkmr") {
    emit(c(
      'run_bkmr_outcome <- function(data, exposures, outcome, covariates,',
      '                              n_iter, seed = 42) {',
      '  vars    <- c(outcome, exposures, covariates)',
      '  data_cc <- data[complete.cases(data[, intersect(vars, names(data)),',
      '                                      drop = FALSE]), , drop = FALSE]',
      '  if (nrow(data_cc) < 50)',
      '    stop(sprintf("Insufficient complete cases for BKMR (N=%d)", nrow(data_cc)))',
      '  y <- as.numeric(data_cc[[outcome]])',
      '  Z <- as.matrix(data_cc[, exposures, drop = FALSE])',
      '  X <- as.matrix(data_cc[, covariates, drop = FALSE])',
      '  set.seed(seed)',
      '  kmbayes(y = y, Z = Z, X = X, iter = n_iter, varsel = TRUE, verbose = FALSE)',
      '}'
    ))

  } else if (atype == "mediation") {
    emit(c(
      '## Delta-method survey-weighted mediation (Baron & Kenny product-of-coefficients)',
      'run_mediation_path <- function(design, exposure, mediator, outcome, covariates) {',
      '  cov_rhs <- if (length(covariates) > 0) paste(covariates, collapse = " + ") else "1"',
      '  ## Path a: exposure -> mediator',
      '  fml_a  <- as.formula(sprintf("%s ~ %s + %s", mediator, exposure, cov_rhs))',
      '  fit_a  <- svyglm(fml_a, design = design, family = gaussian())',
      '  a      <- coef(fit_a)[exposure]',
      '  va     <- vcov(fit_a)[exposure, exposure]',
      '  ## Paths b + c\': exposure + mediator -> outcome',
      '  fml_bc <- as.formula(sprintf("%s ~ %s + %s + %s", outcome, exposure, mediator, cov_rhs))',
      '  fit_bc <- svyglm(fml_bc, design = design, family = gaussian())',
      '  b      <- coef(fit_bc)[mediator]',
      '  vb     <- vcov(fit_bc)[mediator, mediator]',
      '  c_p    <- coef(fit_bc)[exposure]',
      '  ab     <- a * b',
      '  se_ab  <- sqrt(b^2 * va + a^2 * vb)',
      '  list(exposure = exposure, mediator = mediator, outcome = outcome,',
      '       indirect = ab, se = se_ab,',
      '       ci_low   = ab - 1.96 * se_ab,',
      '       ci_high  = ab + 1.96 * se_ab,',
      '       p        = 2 * pnorm(-abs(ab / se_ab)),',
      '       direct   = c_p, a = a, b = b)',
      '}'
    ))

  } else if (atype == "timetrend") {
    emit(c(
      'run_timetrend_var <- function(design, variable, covariates) {',
      '  cov_rhs <- if (length(covariates) > 0) paste(covariates, collapse = " + ") else "1"',
      '  fml     <- as.formula(sprintf("%s ~ year + %s", variable, cov_rhs))',
      '  fit     <- svyglm(fml, design = design, family = gaussian())',
      '  list(variable = variable,',
      '       beta_year = coef(fit)["year"],',
      '       se        = sqrt(vcov(fit)["year","year"]),',
      '       n         = nrow(model.frame(fit)))',
      '}'
    ))

  } else if (atype == "multinomial") {
    emit(c(
      'run_multinomial_model <- function(data, exposure, outcome, covariates) {',
      '  cov_rhs <- if (length(covariates) > 0) paste(covariates, collapse = " + ") else "1"',
      '  fml     <- as.formula(sprintf("%s ~ %s + %s", outcome, exposure, cov_rhs))',
      '  nnet::multinom(fml, data = data, weights = data$pooled_weight, trace = FALSE)',
      '}'
    ))
  }
  blank()

  ## ---- Configuration --------------------------------------------------------
  add("## ---- Configuration ---------------------------------------------------------")
  blank()

  ## Database path — defaults to the standard project location
  db_path_val <- tryCatch(here::here("data", "nhanes_pool.rds"), error = function(e) "data/nhanes_pool.rds")
  add(sprintf('db_path <- "%s"', gsub("\\\\", "/", db_path_val)))
  blank()

  add(sprintf("exposures  <- %s", r_vec(exp_vars)))
  add(sprintf("outcomes   <- %s", r_vec(out_vars)))
  add(sprintf("covariates <- %s", r_vec(shared_covs)))
  if (atype == "mediation")
    add(sprintf("mediators  <- %s", r_vec(med_vars)))

  if (has_log10) {
    flags    <- vapply(valid_exp, function(r) isTRUE(r$transform == "log10"), logical(1))
    flag_str <- paste(ifelse(flags, "TRUE", "FALSE"), collapse = ", ")
    add(sprintf("exp_log10_flags <- c(%s)  ## one flag per exposure", flag_str))
  }
  blank()

  ## Analysis-specific settings
  add("## Analysis settings")
  if (atype == "spline") {
    add(sprintf("spline_df     <- %dL", as.integer(input$spline_df %||% 3)))
  } else if (atype == "wqs") {
    dir_vec <- vapply(valid_out, function(r) r$wqs_direction %||% "positive", character(1))
    add(sprintf('wqs_directions <- c(%s)',
                paste(sprintf('"%s"', dir_vec), collapse = ", ")))
    add(sprintf("wqs_q         <- %dL", as.integer(input$wqs_q %||% 4)))
    add(sprintf("wqs_boot      <- %dL", as.integer(input$wqs_boot %||% 100)))
  } else if (atype == "qgcomp") {
    add(sprintf("qgcomp_q      <- %dL", as.integer(input$qgcomp_q %||% 4)))
  } else if (atype == "bkmr") {
    add(sprintf("bkmr_iter     <- %dL", as.integer(input$bkmr_iter %||% 1000)))
    add(sprintf("bkmr_seed     <- %dL", as.integer(input$bkmr_seed %||% 42)))
  } else if (atype == "mediation") {
    add(sprintf("mediation_nsims <- %dL", as.integer(input$mediation_nsims %||% 500)))
  }
  blank()

  ## ---- Per-population definitions -------------------------------------------
  add("## ---- Population definitions ------------------------------------------------")
  blank()
  for (i in seq_len(n_pops)) {
    pop_id   <- paste0("pop", i)
    lbl      <- rv$pop_labels[i] %||% paste("Population", i)
    cycles   <- input[[paste0("pop_cycles_", i)]] %||% character(0)
    cons     <- isolate(pop_constraints[[pop_id]]) %||% list()
    use_cov  <- isTRUE(input[[paste0("custom_covars_", i)]])
    pop_covs <- if (use_cov)
      input[[paste0("pop_covariates_", i)]] %||% shared_covs
    else shared_covs

    add(sprintf('## Population %d: %s', i, lbl))
    add(sprintf('label_%d    <- "%s"', i, lbl))
    add(sprintf('cycles_%d   <- %s',   i, r_vec(cycles)))

    if (identical(pop_covs, shared_covs)) {
      add(sprintf('covariates_%d <- covariates', i))
    } else {
      add(sprintf('covariates_%d <- %s', i, r_vec(pop_covs)))
    }

    if (length(cons) > 0) {
      add(sprintf('constraints_%d <- list(', i))
      for (j in seq_along(cons)) {
        cc  <- cons[[j]]
        sep <- if (j < length(cons)) "," else ""
        if (isTRUE(cc$op == "not_na")) {
          add(sprintf('  make_constraint("%s", "not_na")%s', cc$var, sep))
        } else {
          val_num <- suppressWarnings(as.numeric(cc$val))
          val_r   <- if (!is.na(val_num)) as.character(val_num)
                     else sprintf('"%s"', cc$val %||% "")
          add(sprintf('  make_constraint("%s", "%s", %s)%s',
                      cc$var, cc$op, val_r, sep))
        }
      }
      add(')')
    } else {
      add(sprintf('constraints_%d <- list()', i))
    }
    blank()
  }

  ## ---- Load and prepare database --------------------------------------------
  add("## ---- Load and prepare database ---------------------------------------------")
  blank()
  add('db <- readRDS(db_path)')
  blank()

  if (length(needed_builtin) > 0 || length(needed_custom) > 0) {
    add('## Compute derived / custom variables on the full database before filtering')
    if (need_race_sex)  add('db <- add_matlab_equivalent_covariates(db)')
    if (need_cvd)       add('db <- add_has_cvd(db)')
    if (need_cvdmed)    add('db <- add_on_cvd_med(db)')
    if (need_smoking)   add('db <- add_smoking_status(db)')
    if (need_edu)       add('db <- add_edu_clean(db)')
    if (need_non_hdl)   add('db <- add_non_hdl(db)')
    if (need_remnant)   add('db <- add_remnant(db)')
    if (need_egfr)      add('db <- add_egfr(db)')
    if (need_usfli)     add('db <- add_usfli(db)')
    for (nm in needed_custom) {
      cv <- cvars[[nm]]
      if (!is.null(cv) && !isTRUE(cv$is_zscore))
        add(sprintf('db$%s <- compute_%s(db)', nm, nm))
    }
    blank()
  }

  ## ---- Per-population analysis loop -----------------------------------------
  add('## ---- Run analysis ----------------------------------------------------------')
  blank()

  for (i in seq_len(n_pops)) {
    pop_id_i <- paste0("pop", i)
    lbl      <- rv$pop_labels[i] %||% paste("Population", i)
    cons_i   <- isolate(pop_constraints[[pop_id_i]]) %||% list()
    add('cat("\\n## =========================================\\n")')
    add(sprintf('cat("## Population %d: %s\\n")', i, lbl))
    add('cat("## =========================================\\n")')
    blank()

    ## Build filtered data block (shared across all types).
    ## apply_filter_trace() applies constraints one-by-one at runtime, printing
    ## N and dropped count after each. Survey weight is assigned per-pair later.
    filter_block <- c(
      sprintf('cycle_years <- as.integer(sub("-.*", "", cycles_%d))', i),
      'pop_data <- as.data.frame(db)',
      'cat(sprintf("  Starting N = %d (all cycles)\\n", nrow(pop_data)))',
      ## cycle filter — skip if no cycles selected (include all years)
      'if (length(cycle_years) > 0) {',
      '  before   <- nrow(pop_data)',
      '  pop_data <- as.data.frame(apply_population_filter(pop_data,',
      '    list(make_constraint("year", "%in%", cycle_years))))',
      '  cat(sprintf("  After cycle filter: N = %d (dropped %d)\\n",',
      '              nrow(pop_data), before - nrow(pop_data)))',
      '}',
      ## per-constraint trace via runtime helper
      sprintf('pop_data <- apply_filter_trace(pop_data, constraints_%d)', i),
      ## weight missingness report
      'wt_cols_present <- intersect(',
      '  c("wtmec2yr","wtsaf","pfas_weight_pooled","pfas_weight"), names(pop_data))',
      'if (length(wt_cols_present) > 0) {',
      '  wt_check     <- wt_cols_present[1]',
      '  n_missing_wt <- sum(is.na(pop_data[[wt_check]]) | pop_data[[wt_check]] <= 0,',
      '                      na.rm = TRUE)',
      '  cat(sprintf("  Missing/zero survey weight (%s): %d excluded per pair\\n",',
      '              wt_check, n_missing_wt))',
      '}',
      'cat(sprintf("  Final population N = %d\\n", nrow(pop_data)))'
    )

    if (atype %in% c("linear","logistic","spline")) {
      cc_line <- if (isTRUE(input$complete_cases_only)) {
        c(sprintf('all_mod_vars <- unique(c(exposures, outcomes, covariates_%d))', i),
          'pop_data     <- pop_data[complete.cases(',
          '  pop_data[, intersect(all_mod_vars, names(pop_data)), drop=FALSE]), ]')
      } else NULL
      emit(c(
        'local({',
        paste0('  ', filter_block),
        if (!is.null(cc_line)) paste0('  ', cc_line),
        '  ## Weight and survey design are built per-pair inside run_regression_grid',
        '  results <- run_regression_grid(',
        '    pop_data   = pop_data,',
        '    outcomes   = outcomes,',
        '    exposures  = exposures,',
        sprintf('    covariates = covariates_%d,', i),
        if (has_log10)            '    log10_flags = exp_log10_flags,' else NULL,
        if (atype == "logistic")  '    family      = "logistic",' else NULL,
        if (atype == "spline")    '    spline_df   = spline_df,'   else NULL,
        '  )',
        '  print_regression_results(results)',
        '})'
      ))

    } else if (atype == "wqs") {
      for (oi in seq_along(out_vars)) {
        ov <- out_vars[[oi]]
        emit(c(
          'local({',
          paste0('  ', filter_block),
          sprintf('  pop_data <- resolve_survey_weight(pop_data, exposures, "%s")', ov),
          sprintf('  cat(sprintf("  WQS outcome: %s\\n"))', ov),
          sprintf('  fit <- run_wqs_outcome(pop_data, exposures, "%s",', ov),
          sprintf('           covariates_%d, wqs_directions[%dL], wqs_q, wqs_boot)', i, oi),
          '  sm  <- summary(fit)$coef',
          '  cat(sprintf("  WQS beta=%.4f  p=%.4f\\n",',
          '      sm["wqs","Estimate"], sm["wqs", ncol(sm)]))',
          '  cat("  Component weights:\\n")',
          '  print(fit$final_weights)',
          '})'
        ))
      }

    } else if (atype == "qgcomp") {
      for (ov in out_vars) {
        emit(c(
          'local({',
          paste0('  ', filter_block),
          sprintf('  pop_data <- resolve_survey_weight(pop_data, exposures, "%s")', ov),
          sprintf('  cat(sprintf("  qgcomp outcome: %s\\n"))', ov),
          sprintf('  fit <- run_qgcomp_outcome(pop_data, exposures, "%s",', ov),
          sprintf('           covariates_%d, qgcomp_q)', i),
          '  psi   <- as.numeric(fit$psi)[1L]',
          '  ci    <- tryCatch(as.numeric(fit$ci), error=function(e) c(NA,NA))',
          '  pval  <- tryCatch(as.numeric(fit$pval)[1L], error=function(e) NA)',
          '  cat(sprintf("  psi=%.4f  95%%CI [%.4f, %.4f]  p=%.4f\\n",',
          '              psi, ci[1], ci[2], pval))',
          '  cat("  Positive weights:\\n"); print(fit$pos.weights)',
          '  cat("  Negative weights:\\n"); print(fit$neg.weights)',
          '})'
        ))
      }

    } else if (atype == "bkmr") {
      for (ov in out_vars) {
        emit(c(
          'local({',
          paste0('  ', filter_block),
          sprintf('  pop_data <- resolve_survey_weight(pop_data, exposures, "%s")', ov),
          sprintf('  cat(sprintf("  BKMR outcome: %s\\n"))', ov),
          sprintf('  fit  <- run_bkmr_outcome(pop_data, exposures, "%s",', ov),
          sprintf('            covariates_%d, bkmr_iter, bkmr_seed)', i),
          '  pips <- sort(colMeans(fit$pips, na.rm=TRUE), decreasing=TRUE)',
          '  cat("  Posterior inclusion probabilities (PIPs):\\n")',
          '  print(round(pips, 4))',
          '})'
        ))
      }

    } else if (atype == "mediation") {
      for (exp_v in exp_vars) {
        for (med_v in med_vars) {
          for (ov in out_vars) {
            emit(c(
              'local({',
              paste0('  ', filter_block),
              sprintf('  ## Weight for mediation: use most restrictive for exposure + outcome'),
              sprintf('  pop_data <- resolve_survey_weight(pop_data, c("%s", "%s"), "%s")',
                      exp_v, med_v, ov),
              '  design <- build_survey_design(pop_data)',
              sprintf('  res    <- run_mediation_path(design, "%s", "%s", "%s", covariates_%d)',
                      exp_v, med_v, ov, i),
              sprintf('  cat(sprintf("  %s -> %s -> %s\\n", "%s", "%s", "%s"))', exp_v, med_v, ov, exp_v, med_v, ov),
              '  cat(sprintf("    Indirect (a*b) = %.4f  SE=%.4f  p=%.4f  95%%CI [%.4f, %.4f]\\n",',
              '              res$indirect, res$se, res$p, res$ci_low, res$ci_high))',
              '  cat(sprintf("    Direct  (c\')   = %.4f\\n", res$direct))',
              '})'
            ))
          }
        }
      }

    } else if (atype == "timetrend") {
      emit(c(
        'local({',
        paste0('  ', filter_block),
        '  ## For time-trend, use MEC weight (no specific exposure-outcome pairing)',
        '  pop_data <- resolve_survey_weight(pop_data, exposures, outcomes[1])',
        '  design   <- build_survey_design(pop_data)',
        '  all_vars <- c(exposures, outcomes)',
        '  for (v in all_vars) {',
        '    res <- tryCatch(',
        sprintf('      run_timetrend_var(design, v, covariates_%d),', i),
        '      error = function(e) { message("  Error for ", v, ": ", e$message); NULL })',
        '    if (!is.null(res))',
        '      cat(sprintf("  %-20s  beta_year = %7.4f  SE = %.4f  N = %d\\n",',
        '                  res$variable, res$beta_year, res$se, res$n))',
        '  }',
        '})'
      ))

    } else if (atype == "multinomial") {
      for (ov in out_vars) {
        for (exp_v in exp_vars) {
          emit(c(
            'local({',
            paste0('  ', filter_block),
            sprintf('  pop_data <- resolve_survey_weight(pop_data, "%s", "%s")', exp_v, ov),
            sprintf('  cat(sprintf("  Multinomial: %s ~ %s\\n"))', ov, exp_v),
            sprintf('  fit <- run_multinomial_model(pop_data, "%s", "%s", covariates_%d)',
                    exp_v, ov, i),
            '  print(summary(fit)$coefficients)',
            '})'
          ))
        }
      }
    }
    blank()
  }

  paste(lines, collapse = "\n")
}
