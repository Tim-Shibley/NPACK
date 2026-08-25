## ============================================================================
## demographic_breakdown.R
##
## Dynamic Table 1-style demographic summary.
## Determines display type by heuristic:
##   - Unique non-NA values <= 10 AND all whole numbers -> categorical (N, %)
##   - Otherwise -> continuous (Mean, SD)
## Known variables get human-readable labels for their categories.
## ============================================================================

## Known categorical label maps: variable -> c(display_label = code)
KNOWN_LABEL_MAPS <- list(
  sex = c("Male"=1, "Female"=2),
  sex_bin = c("Male"=0, "Female"=1),
  race = c("Hispanic"=1, "Non-Hispanic White"=3,
            "Non-Hispanic Black"=4, "Other/Multiracial"=5),
  education = c("Less than 9th grade"=1, "9th-11th grade"=2,
                 "High school / GED"=3, "Some college"=4,
                 "College graduate+"=5),
  edu_clean = c("Less than 9th grade"=1, "9th-11th grade"=2,
                 "High school / GED"=3, "Some college"=4,
                 "College graduate+"=5),
  smoking_status = c("Never"=0, "Former"=1, "Current"=2),
  race_hispanic = c("Non-Hispanic"=0, "Hispanic"=1),
  race_black    = c("Non-Black"=0, "Non-Hispanic Black"=1),
  race_other    = c("Other/Multi"=1),
  hascvd   = c("No CVD"=0, "Has CVD"=1),
  oncvdmed = c("Not on CVD med"=0, "On CVD med"=1),
  hasdb = c("No diabetes"=0, "Has diabetes"=1),
  hasdiabetes = c("No diabetes"=0, "Has diabetes"=1)
)

## Known display names for variables
KNOWN_VAR_LABELS <- c(
  age="Age (years)", sex="Sex", sex_bin="Sex",
  race="Race/Ethnicity", race_hispanic="Hispanic", race_black="Non-Hispanic Black",
  race_other="Other/Multiracial", education="Education", edu_clean="Education",
  smoking_status="Smoking status", income_ratio="Income-to-poverty ratio",
  bmxbmi="BMI (kg/m²)", bmi="BMI (kg/m²)",
  hascvd="CVD status", oncvdmed="On CVD medication",
  hasdb="Diabetes status", hasdiabetes="Diabetes status",
  lbdldl="LDL cholesterol (mg/dL)", lbxtc="Total cholesterol (mg/dL)",
  lbdhdd="HDL cholesterol (mg/dL)", non_hdl="Non-HDL cholesterol (mg/dL)",
  remnant="Remnant cholesterol (mg/dL)", lbxglu="Fasting glucose (mg/dL)",
  lbxgh="HbA1c (%)", lbxin="Insulin (μU/mL)", lbxhgb="Hemoglobin (g/dL)",
  lbxtr="Triglycerides (mg/dL)", egfr="eGFR (mL/min/1.73m²)"
)

## Safe label lookup — [[ on named vector throws error for missing names,
## [ returns NA which we convert to NULL for %||% compatibility
get_var_label <- function(col) {
  lbl <- KNOWN_VAR_LABELS[col]
  if (length(lbl) == 0 || is.na(lbl)) NULL else unname(lbl)
}

#' Determine if a variable should be shown as categorical or continuous
is_categorical_var <- function(vals) {
  ## Force to plain numeric — handles integer, haven_labelled, multi-class
  vals <- suppressWarnings(as.numeric(vals))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(FALSE)
  n_unique  <- length(unique(vals))
  all_whole <- isTRUE(all(vals == floor(vals)))
  n_unique <= 10 && all_whole
}

#' Format a categorical breakdown
cat_rows <- function(data, col, label, levels_map = NULL) {
  vals <- data[[col]]
  num_vals <- suppressWarnings(as.numeric(vals))
  use_numeric <- !any(is.na(num_vals[!is.na(vals)]))

  if (is.null(levels_map)) {
    uvals <- sort(unique(if (use_numeric) num_vals[!is.na(num_vals)]
                         else vals[!is.na(vals)]))
    levels_map <- setNames(as.list(uvals), as.character(uvals))
  }

  total_nonmissing <- sum(!is.na(vals))
  rows <- lapply(names(levels_map), function(lbl) {
    code <- levels_map[[lbl]]
    if (is.null(code) || (length(code) == 1 && is.na(code))) {
      n <- sum(is.na(vals))
    } else {
      ## Match by numeric value regardless of whether code is stored as
      ## string "1" or numeric 1
      code_num <- suppressWarnings(as.numeric(code))
      if (!is.na(code_num) && use_numeric) {
        n <- sum(!is.na(num_vals) & num_vals == code_num)
      } else {
        n <- sum(!is.na(vals) & as.character(vals) == as.character(code))
      }
    }
    pct <- if (total_nonmissing > 0) 100 * n / total_nonmissing else NA_real_
    data.frame(characteristic=label, subcategory=lbl,
               value=sprintf("%d (%.1f%%)", n, pct),
               stringsAsFactors=FALSE)
  })
  do.call(rbind, rows)
}

#' Format a continuous variable as mean (SD)
cont_row <- function(data, col, label, digits = 1) {
  vals <- data[[col]][!is.na(data[[col]])]
  if (length(vals) == 0)
    return(data.frame(characteristic=label, subcategory="Mean (SD)",
                       value="—", stringsAsFactors=FALSE))
  data.frame(characteristic=label, subcategory="Mean (SD)",
              value=sprintf(paste0("%.", digits, "f (%.", digits, "f)"),
                             mean(vals), sd(vals)),
              stringsAsFactors=FALSE)
}

.fmt3 <- function(x) formatC(x, digits = 3, format = "g")

#' Format an outcome variable as mean (SD) + geometric mean (IQR)
outcome_rows <- function(data, col, label) {
  vals     <- data[[col]][!is.na(data[[col]])]
  pos_vals <- vals[vals > 0]
  if (length(vals) == 0)
    return(data.frame(
      characteristic = label,
      subcategory    = c("Mean (SD)", "Geometric Mean (IQR)"),
      value          = c("—", "—"),
      stringsAsFactors = FALSE))
  mean_sd <- sprintf("%s (%s)", .fmt3(mean(vals)), .fmt3(sd(vals)))
  if (length(pos_vals) > 0) {
    gm  <- exp(mean(log(pos_vals)))
    iqr <- quantile(pos_vals, probs = c(0.25, 0.75), na.rm = TRUE)
    gm_iqr <- sprintf("%s (%s–%s)", .fmt3(gm), .fmt3(iqr[1]), .fmt3(iqr[2]))
  } else {
    gm_iqr <- "—"
  }
  data.frame(
    characteristic  = label,
    subcategory     = c("Mean (SD)", "Geometric Mean (IQR)"),
    value           = c(mean_sd, gm_iqr),
    stringsAsFactors = FALSE)
}

#' Section header row
header_row <- function(label) {
  data.frame(characteristic=label, subcategory="", value="",
              stringsAsFactors=FALSE)
}

#' Build a geometric-mean or level-breakdown row for one exposure variable.
build_exposure_summary <- function(data, exposure_cols) {
  if (is.null(exposure_cols) || length(exposure_cols) == 0) return(NULL)
  cvars <- tryCatch(get_custom_variables(), error = function(e) list())
  rows <- list()
  for (col in exposure_cols) {
    if (!col %in% names(data)) next
    raw <- data[[col]]
    if (inherits(raw, "haven_labelled") || inherits(raw, "labelled") || is.factor(raw))
      raw <- suppressWarnings(as.numeric(as.character(raw)))
    label <- get_var_label(col) %||% col
    data[[col]] <- raw

    if (is_categorical_var(raw)) {
      custom_lmap <- cvars[[col]]$label_map
      known_lmap  <- KNOWN_LABEL_MAPS[[col]]
      lmap <- custom_lmap %||% known_lmap
      rows[[paste0("exp_", col)]] <- cat_rows(data, col, label, lmap)
    } else {
      pos_vals <- raw[!is.na(raw) & raw > 0]
      if (length(pos_vals) == 0) {
        rows[[paste0("exp_", col)]] <- data.frame(
          characteristic = label, subcategory = "Geometric Mean",
          value = "—", stringsAsFactors = FALSE)
      } else {
        gm <- exp(mean(log(pos_vals)))
        rows[[paste0("exp_", col)]] <- data.frame(
          characteristic = label, subcategory = "Geometric Mean",
          value = sprintf("%.4g", gm), stringsAsFactors = FALSE)
      }
    }
  }
  if (length(rows) == 0) return(NULL)
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

#' Build a dynamic Table 1 demographic summary split into four tables.
#'
#' @param data        Filtered population data frame
#' @param covariates  Character vector of covariate column names to summarize
#' @param outcomes    Character vector of outcome column names to summarize
#' @param exposures   Character vector of exposure column names to summarize
#' @return list with four data.frames: categorical, continuous, exposure_summary, outcomes
build_demographic_table <- function(data, covariates = NULL, outcomes = NULL, exposures = NULL) {
  n_total <- nrow(data)

  cat_rows_list  <- list()
  cont_rows_list <- list()
  out_rows_list  <- list()

  ## Header row for categorical table
  cat_rows_list$header <- header_row(sprintf("Total N = %d", n_total))

  ## ---- Setup variable list ------------------------------------------------
  core_vars <- c("sex","sex_bin","age","race","edu_clean","education","smoking_status","income_ratio")

  alias_groups <- list(c("sex","sex_bin"), c("education","edu_clean"))

  all_covs <- unique(c(
    intersect(core_vars, names(data)),
    if (!is.null(covariates)) intersect(covariates, names(data)) else character(0)
  ))

  ## Remove aliased duplicates
  seen_env <- new.env(parent=emptyenv()); seen_env$g <- character(0)
  all_covs <- Filter(function(col) {
    grp <- which(sapply(alias_groups, function(g) col %in% g))
    if (length(grp) > 0) {
      key <- paste0("grp_", grp[1])
      if (key %in% seen_env$g) return(FALSE)
      seen_env$g <- c(seen_env$g, key)
    }
    TRUE
  }, all_covs)

  if ("race" %in% names(data))
    all_covs <- setdiff(all_covs, c("race_hispanic","race_black","race_other","race_white"))
  if (!is.null(outcomes))
    all_covs <- setdiff(all_covs, outcomes)
  if (!is.null(exposures))
    all_covs <- setdiff(all_covs, exposures)

  ## Load custom variable label maps
  cvars <- tryCatch(get_custom_variables(), error=function(e) list())

  ## ---- Summarize each covariate into the right table ----------------------
  for (col in all_covs) {
    if (!col %in% names(data)) next
    raw <- data[[col]]
    if (inherits(raw, "haven_labelled") || inherits(raw, "labelled") || is.factor(raw))
      raw <- suppressWarnings(as.numeric(as.character(raw)))
    vals  <- raw
    label <- get_var_label(col) %||% col
    data[[col]] <- raw

    custom_lmap <- cvars[[col]]$label_map
    known_lmap  <- KNOWN_LABEL_MAPS[[col]]
    lmap <- custom_lmap %||% known_lmap

    if (col == "race") {
      total_r <- sum(!is.na(vals))
      cat_rows_list[[paste0("demo_", col)]] <- data.frame(
        characteristic = rep("Race/Ethnicity", 4),
        subcategory    = c("Hispanic","Non-Hispanic White",
                           "Non-Hispanic Black","Other/Multiracial"),
        value = c(
          sprintf("%d (%.1f%%)", sum(!is.na(vals) & vals %in% 1:2),
                  100*sum(!is.na(vals) & vals %in% 1:2)/max(total_r,1)),
          sprintf("%d (%.1f%%)", sum(!is.na(vals) & vals==3),
                  100*sum(!is.na(vals) & vals==3)/max(total_r,1)),
          sprintf("%d (%.1f%%)", sum(!is.na(vals) & vals==4),
                  100*sum(!is.na(vals) & vals==4)/max(total_r,1)),
          sprintf("%d (%.1f%%)", sum(!is.na(vals) & vals==5),
                  100*sum(!is.na(vals) & vals==5)/max(total_r,1))
        ), stringsAsFactors=FALSE)
    } else if (is_categorical_var(vals)) {
      cat_rows_list[[paste0("demo_", col)]] <- cat_rows(data, col, label, lmap)
    } else {
      cont_rows_list[[paste0("cov_", col)]] <- cont_row(data, col, label)
    }
  }

  ## ---- Outcomes -----------------------------------------------------------
  if (!is.null(outcomes) && length(outcomes) > 0) {
    out_cols_present <- intersect(outcomes, names(data))
    for (col in out_cols_present) {
      raw <- data[[col]]
      if (inherits(raw, "haven_labelled") || inherits(raw, "labelled") || is.factor(raw))
        raw <- suppressWarnings(as.numeric(as.character(raw)))
      data[[col]] <- raw
      label       <- get_var_label(col) %||% col
      custom_lmap <- cvars[[col]]$label_map
      known_lmap  <- KNOWN_LABEL_MAPS[[col]]
      lmap        <- custom_lmap %||% known_lmap
      if (is_categorical_var(raw)) {
        out_rows_list[[paste0("out_", col)]] <- cat_rows(data, col, label, lmap)
      } else {
        out_rows_list[[paste0("out_", col)]] <- outcome_rows(data, col, label)
      }
    }
  }

  ## ---- Assemble tables ----------------------------------------------------
  make_table <- function(rows) {
    if (length(rows) == 0) return(NULL)
    result <- do.call(rbind, rows)
    rownames(result) <- NULL
    result
  }

  exp_summary <- if (!is.null(exposures) && length(exposures) > 0)
    build_exposure_summary(data, intersect(exposures, names(data)))
  else
    NULL

  list(
    categorical      = make_table(cat_rows_list),
    continuous       = make_table(cont_rows_list),
    exposure_summary = exp_summary,
    outcomes         = make_table(out_rows_list)
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
