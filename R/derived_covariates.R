## ============================================================================
## derived_covariates.R
##
## Owns: constructing derived/dummy-coded covariates from raw NHANES codes,
## matching the exact coding scheme used in the existing MATLAB pipeline so
## validate_against_matlab.R has a fair, apples-to-apples comparison.
##
## Kept as its own module rather than folded into population_filter.R or
## regression_engine.R, since this is neither filtering nor regression — it's
## a third kind of transformation (recoding), and a future user adding a new
## derived covariate shouldn't need to touch filtering or regression logic.
##
## CRITICAL: these encodings are NOT arbitrary choices — they were reverse-
## engineered directly from the existing MATLAB pipeline (PanEtAl2024_
## MedicationEfficacy.m, PanEtAl2024_MedClassAnalysis.m) so that R results
## are comparable to already-published/in-progress MATLAB results. Do not
## "clean up" or change these encodings without re-validating against MATLAB.
## ============================================================================

#' Add race dummy variables matching MATLAB's exact scheme.
#'
#' MATLAB source (PanEtAl2024_MedicationEfficacy.m, confirmed via
#' PanEtAl2024_Replication.m's RIDRETH1 comment):
#'   RIDRETH1: 1=Mexican American, 2=Other Hispanic, 3=Non-Hispanic White,
#'             4=Non-Hispanic Black, 5=Other (incl. multi-racial)
#'   race_hispanic = (race==1 | race==2)
#'   race_black    = (race==4)
#'   race_other    = (race==5)
#'   [reference group = Non-Hispanic White, race==3, no dummy needed]
#'
#' @param data a data frame containing a RACE column (raw RIDRETH1 codes,
#'        1-5, as numeric — NOT translated text labels)
#' @return data with three new columns added: race_hispanic, race_black,
#'         race_other
add_race_dummies <- function(data) {
  ## Accept either uppercase RACE or lowercase race (build_database.R renames to lowercase)
  race_col <- if ("RACE" %in% names(data)) "RACE" else if ("race" %in% names(data)) "race" else NULL
  if (is.null(race_col)) {
    stop("add_race_dummies() requires a RACE column — was RACE included in the variable list passed to build_harmonized_dataset()?")
  }
  if (!is.numeric(data[[race_col]])) {
    stop("RACE column is not numeric — check that fetch_variable_for_cycle() correctly coerced it to raw numeric codes, not translated text labels.")
  }

  data$race_hispanic <- as.numeric(data[[race_col]] == 1 | data[[race_col]] == 2)
  data$race_black    <- as.numeric(data[[race_col]] == 4)
  data$race_other    <- as.numeric(data[[race_col]] == 5)
  data
}

#' Recode SEX from NHANES's 1/2 coding to MATLAB's 0/1 coding.
#'
#' MATLAB source: sex_bin = TPool.sex - 1
#' NHANES RIAGENDR: 1=Male, 2=Female -> sex_bin: 0=Male, 1=Female
#'
#' @param data a data frame containing a SEX column (raw RIAGENDR codes,
#'        1 or 2, as numeric)
#' @return data with a new sex_bin column (0/1)
add_sex_bin <- function(data) {
  sex_col <- if ("SEX" %in% names(data)) "SEX" else if ("sex" %in% names(data)) "sex" else NULL
  if (is.null(sex_col)) {
    stop("add_sex_bin() requires a SEX column.")
  }
  if (!is.numeric(data[[sex_col]])) {
    stop("SEX column is not numeric — check raw numeric coercion in fetch_variable_for_cycle().")
  }
  data$sex_bin <- data[[sex_col]] - 1
  data
}

#' Convenience wrapper applying all derived covariates used by the
#' CVD-medicated LDL validation target in one call.
add_matlab_equivalent_covariates <- function(data) {
  data <- add_race_dummies(data)
  data <- add_sex_bin(data)
  data
}

#' Construct the hasCVD composite flag from the five MCQ160 questionnaire
#' items (B/C/D/E/F — congestive heart failure, coronary heart disease,
#' angina, heart attack, stroke), matching the project's documented CVD
#' definition (memory: "CVD defined via five NHANES questionnaire variables
#' MCQ160B/C/D/E/F"). A participant is CVD-positive if ANY of the five
#' items is coded 1 (yes).
#'
#' @param data a data frame containing CVD_MCQ160B through CVD_MCQ160F
#'        columns (raw numeric NHANES codes: 1=yes, 2=no, 7/9=refused/dk)
#' @return data with a new hasCVD column (1/0), NA if all five items are NA
add_has_cvd <- function(data) {
  mcq_cols_upper <- c("CVD_MCQ160B","CVD_MCQ160C","CVD_MCQ160D","CVD_MCQ160E","CVD_MCQ160F")
  mcq_cols_lower <- c("mcq160b","mcq160c","mcq160d","mcq160e","mcq160f")

  if (all(mcq_cols_upper %in% names(data))) {
    mcq_cols <- mcq_cols_upper
  } else if (all(mcq_cols_lower %in% names(data))) {
    mcq_cols <- mcq_cols_lower
  } else {
    ## Source columns not yet in database — skip silently
    ## hascvd will be computed when MCQ160 variables are added
    return(data)
  }

  mcq_matrix <- as.matrix(data[, mcq_cols])
  any_yes  <- apply(mcq_matrix == 1, 1, any, na.rm = TRUE)
  all_na   <- apply(is.na(mcq_matrix), 1, all)
  data$hascvd <- ifelse(all_na, NA_real_, as.numeric(any_yes))
  data
}

#' Construct the onCVDmed flag, matching MATLAB exactly:
#' TPool.onCVDmed = (BPQ100D == 1)
#'
#' @param data a data frame containing an ONCVDMED_RAW column
#' @return data with a new oncvdmed column (1/0)
add_on_cvd_med <- function(data) {
  med_col <- if ("ONCVDMED_RAW" %in% names(data)) "ONCVDMED_RAW" else
             if ("oncvdmed_raw" %in% names(data)) "oncvdmed_raw" else
             if ("BPQ100D"      %in% names(data)) "BPQ100D"      else
             if ("bpq100d"      %in% names(data)) "bpq100d"      else NULL
  if (is.null(med_col)) {
    ## Source column not yet in database — skip silently
    return(data)
  }
  data$oncvdmed <- as.numeric(data[[med_col]] == 1)
  data
}

#' Construct 3-level smoking status matching MATLAB pipeline:
#'   0 = Never smoker  (smq020 == 2)
#'   1 = Former smoker (smq020 == 1 & smq040 == 3)
#'   2 = Current smoker(smq020 == 1 & smq040 %in% 1:2)
#'   NA = refused/don't know (smq020 %in% 7:9 or inconsistent)
#'
#' Replaces the binary smq020 covariate used in earlier analyses.
add_smoking_status <- function(data) {
  if (!all(c("smq020","smq040") %in% names(data))) return(data)

  ## Coerce to plain numeric — handles haven_labelled, factor, character
  smq020 <- suppressWarnings(as.numeric(as.character(data$smq020)))
  smq040 <- suppressWarnings(as.numeric(as.character(data$smq040)))

  s <- rep(NA_real_, nrow(data))
  s[!is.na(smq020) & smq020 == 2]                                 <- 0  # Never
  s[!is.na(smq020) & smq020 == 1 & !is.na(smq040) & smq040 == 3] <- 1  # Former
  s[!is.na(smq020) & smq020 == 1 & !is.na(smq040) & smq040 %in% c(1,2)] <- 2  # Current

  data$smoking_status <- s
  data
}

#' Clean education variable — recode refused (7) and don't know (9) to NA,
#' matching MATLAB: vals(vals > 5) = NaN
#' DMDEDUC2: 1=<9th grade, 2=9-11th, 3=HS/GED, 4=some college, 5=college+
add_edu_clean <- function(data) {
  if (!"education" %in% names(data)) return(data)
  edu <- data$education
  edu[!is.na(edu) & edu > 5] <- NA_real_
  data$edu_clean <- edu
  data
}

#' Compute eGFR using CKD-EPI 2009 creatinine equation (Levey et al., Ann
#' Intern Med 2009) with cycle-specific creatinine recalibration per
#' Selvin et al. 2007 (Am J Kidney Dis).
#'
#' Recalibration applied to raw Jaffe method creatinine values:
#'   1999-2000 (year=1999): lbxscr = 1.013 * lbxscr + 0.147
#'   2005-2006 (year=2005): lbxscr = 0.978 * lbxscr + 0.006
#'   All other cycles: no recalibration needed (Roche enzymatic method)
#'
#' CKD-EPI equation uses NHANES coding: sex 1=male 2=female, race 4=NH Black
add_egfr <- function(data) {
  required <- c("lbxscr","age","sex","race","year")
  if (!all(required %in% names(data))) return(data)

  scr  <- as.numeric(data$lbxscr)
  age  <- as.numeric(data$age)
  sex  <- as.numeric(data$sex)
  race <- as.numeric(data$race)
  yr   <- as.integer(data$year)

  ## Cycle-specific creatinine recalibration to Roche enzymatic standard
  scr_cal <- scr
  scr_cal[!is.na(yr) & yr == 1999] <-
    1.013 * scr[!is.na(yr) & yr == 1999] + 0.147
  scr_cal[!is.na(yr) & yr == 2005] <-
    0.978 * scr[!is.na(yr) & yr == 2005] + 0.006

  ## CKD-EPI 2009 — vectorised
  n    <- nrow(data)
  egfr <- rep(NA_real_, n)

  for (i in seq_len(n)) {
    if (is.na(scr_cal[i]) || is.na(age[i]) || is.na(sex[i])) next

    ## Sex-specific kappa and alpha
    if (sex[i] == 2) {          ## female
      kappa <- 0.7;  alpha <- -0.329
    } else {                    ## male
      kappa <- 0.9;  alpha <- -0.411
    }

    ratio   <- scr_cal[i] / kappa
    val     <- 141 * min(ratio, 1)^alpha * max(ratio, 1)^(-1.209) *
               0.993^age[i]

    ## Female multiplier
    if (sex[i] == 2) val <- val * 1.018

    ## Non-Hispanic Black multiplier
    if (!is.na(race[i]) && race[i] == 4) val <- val * 1.159

    egfr[i] <- val
  }

  data$egfr <- egfr
  data
}
