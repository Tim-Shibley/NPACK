# NEWS

## nhanes-gui 1.0.0 (2026-08-24)



### Core analysis engines

- `run_analysis()` — survey-weighted linear and logistic regression with Taylor-series
  linearisation variance, automatic NHANES weight selection, and optional natural spline
  or quartile-stratified models
- `run_wqs_analysis()` — Weighted Quantile Sum (WQS) regression via **gWQS**
- `run_qgcomp_analysis()` — Quantile g-computation via **qgcomp** with
  empirical-efficiency sandwich variance
- `run_bkmr()` / `run_bkmr_analysis()` — Bayesian Kernel Machine Regression via **bkmr**
  with convergence diagnostics and PIP extraction
- `run_mediation_analysis()` — Survey-weighted causal mediation via **mediation**
  (ACME / ADE decomposition)
- `run_multinomial_analysis()` — Survey-weighted multinomial logistic regression
  via repeated binary `svyglm()` calls (one model per non-reference level)
- `run_timetrend_analysis()` — Per-cycle survey-weighted means plus overall trend
  (percent change per year), with Joinpoint-compatible export

### Data infrastructure

- Pre-built pooled database (`nhanes_pool.rds`) covering NHANES cycles 1999–2018
  across demographics, PFAS biomarkers, lipid outcomes, and lifestyle covariates
- `variable_registry.R` — canonical variable definitions with per-cycle NHANES
  variable name mappings, weight group assignments, and log10-transform flags
- `derived_outcomes_registry.R` — computed outcomes (non-HDL, remnant cholesterol,
  TC/HDL ratio) calculated directly from NHANES raw variables
- `build_database.R` — reproducible database construction pipeline with PFAS isomer
  summation (linear + branched for 2013+ cycles), cross-cycle harmonisation, and
  `variable_weight_map` attribute for downstream weight selection
- `survey_design.R` — Taylor-series linearisation via strata + PSU + pooled weights;
  supports both local-database and live-fetch pipelines

### Population filtering

- `population_definitions` — named list of 11 analysis-ready NHANES subpopulations
  (fasting, non-CVD-medicated, pregnant exclusions, etc.)
- `apply_population_filter()` — constraint-based filter engine used by all analysis
  engines; population selection is orthogonal to weight selection

### User interface

- Shiny GUI (`app.R`) covering all seven analysis modes with reactive outputs,
  forest-plot and heatmap rendering, and Table-1 demographic summaries
- `demographic_breakdown.R` — Table-1 generator with survey-weighted means/proportions
  and SMD calculations

### Weight system

Seven-tier weight priority hierarchy (WTSS* > WTSA2YR > WTSB2YR > WTSC2YR >
WTSAF2YR > WTMEC2YR > WTINT2YR) resolved per exposure-outcome pair at analysis time
from `attr(db, "variable_weight_map")`.
