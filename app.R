## ============================================================================
## app.R — NHANES Analysis Tool v2
## Two-tab UI: Dataset builder + Analysis
## ============================================================================

library(shiny)
library(shinyjs)
library(jsonlite)
library(here)
library(dplyr)

source(here("R", "variable_lookup.R"))
source(here("R", "demographic_breakdown.R"))
source(here("R", "cli.R"))
source(here("R", "build_database.R"))
source(here("R", "bkmr_engine.R"))
source(here("R", "mixture_engine.R"))
source(here("R", "mediation_engine.R"))
source(here("R", "multinomial_engine.R"))
source(here("R", "timetrend_engine.R"))
source(here("R", "project_io.R"))

## ============================================================================
## HELPERS
## ============================================================================

## Load dataset column metadata for autocomplete
get_dataset_columns <- function(db) {
  lkp  <- load_duke_lookup()
  cols <- names(db)
  meta <- unique(lkp[, c("variable","description","category")])
  result <- data.frame(
    column      = cols,
    label       = cols,
    description = "",
    category    = "",
    num_cycles  = NA_integer_,
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(result))) {
    col <- toupper(result$column[i])
    m   <- meta[meta$variable == col, ]
    if (nrow(m) > 0) {
      result$label[i]       <- sprintf("%s — %s", result$column[i], m$description[1])
      result$description[i] <- m$description[1]
      result$category[i]    <- m$category[1]
    }
    result$num_cycles[i] <- length(unique(lkp[lkp$variable == col, "cycle"]))
  }
  result
}

parse_var_lines <- function(txt) {
  lines <- strsplit(txt, "\n")[[1]]
  lines <- trimws(lines[nzchar(trimws(lines))])
  lapply(lines, function(line) {
    parts <- strsplit(trimws(line), "\\s+")[[1]]
    var   <- parts[1]
    transform <- if (length(parts) >= 2) {
      switch(tolower(parts[2]),
        "log" = "log10", "log10" = "log10",
        "log2" = "log2", "sqrt" = "sqrt",
        "ln" = "ln",
        "raw" = NULL, NULL)
    } else NULL
    list(var = var, transform = transform)
  })
}
vars_only    <- function(p) vapply(p, `[[`, character(1), "var")
log10_flags  <- function(p) vapply(p, function(x) identical(x$transform,"log10"), logical(1))

## ============================================================================
## CSS
## ============================================================================

app_css <- "
/* ---- tokens ---- */
:root {
  --navy:   #1a2035;
  --slate:  #3a4a6b;
  --teal:   #2a7f7f;
  --teal-lt:#e8f4f4;
  --sand:   #f5f3ee;
  --white:  #ffffff;
  --border: #dddbd4;
  --text:   #1a1a1a;
  --muted:  #6b7280;
  --danger: #b91c1c;
  --success:#15803d;
}

/* ---- Wolfpack theme overrides ---- */
body.wolfpack {
  --navy:   #CC0000;
  --slate:  #990000;
  --teal:   #111111;
  --teal-lt:#f0f0f0;
  --sand:   #f5f5f5;
  --white:  #ffffff;
  --border: #dddddd;
  --text:   #1a1a1a;
  --muted:  #555555;
  --danger: #990000;
  --success:#15803d;
}
body.wolfpack .app-header { color: #000000; }
body.wolfpack .app-header span { color: #000000; opacity: 0.65; }
body.wolfpack .results-tbl th { background: #f0f0f0; }
body.wolfpack .var-row { border-bottom-color: #e8e8e8; }
body.wolfpack .staged-tag { background:#f0f0f0; border-color:#bbbbbb; color:#111111; }
body.wolfpack .project-bar { background:#f0f0f0; border-bottom-color:#dddddd; }
body.wolfpack .var-code { color: #111111; }
body.wolfpack .btn-connector { border-color: #111111; color: #111111; }
body.wolfpack .btn-connector:hover { background:#e8e8e8; border-color:#000000; color:#000000; }
body.wolfpack .dcv-gutter-and-start::before,
body.wolfpack .dcv-gutter-and-inner::before,
body.wolfpack .dcv-gutter-and-connector::before,
body.wolfpack .dcv-gutter-and-end::before { border-color: #111111; }

body { background: var(--sand); color: var(--text);
       font-family: 'Inter', 'Segoe UI', sans-serif; font-size:14px; }

/* ---- header ---- */
.app-header {
  background: var(--navy); color: #e8e4d9;
  padding: 14px 24px; display:flex; align-items:baseline; gap:16px;
  box-shadow: 0 2px 8px rgba(0,0,0,.25);
}
.app-header h2 { margin:0; font-size:1.15em; font-weight:600;
                  letter-spacing:.02em; font-family:'Georgia',serif; }
.app-header span { font-size:.8em; color:#8899bb; }

/* ---- nav tabs ---- */
.nav-tabs { border-bottom: 2px solid var(--border); margin-bottom:0; }
.nav-tabs > li > a {
  border:none; border-bottom:2px solid transparent;
  color: var(--muted); font-weight:500; padding:10px 20px;
  margin-bottom:-2px; border-radius:0;
}
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:focus,
.nav-tabs > li.active > a:hover {
  border:none; border-bottom:2px solid var(--teal);
  color: var(--teal); background:transparent;
}

/* ---- panels ---- */
.panel-box {
  background: var(--white); border:1px solid var(--border);
  border-radius:6px; padding:18px; margin-bottom:14px;
}
.panel-box h4 {
  margin:0 0 12px; font-size:.78em; font-weight:600;
  text-transform:uppercase; letter-spacing:.08em;
  color: var(--muted); border-bottom:1px solid var(--border);
  padding-bottom:8px;
}

/* ---- buttons ---- */
.btn-primary {
  background: var(--teal) !important; border-color: var(--teal) !important;
  color: white !important; font-weight:500;
}
.btn-primary:hover { background: #1f6060 !important; }
.btn-run {
  background: var(--navy); color:white; border:none;
  padding:10px 24px; border-radius:4px; font-size:.95em;
  font-weight:500; width:100%; cursor:pointer;
}
.btn-run:hover { background: var(--slate); }
.btn-sm-teal {
  background: var(--teal-lt); color: var(--teal);
  border:1px solid var(--teal); border-radius:3px;
  padding:3px 10px; font-size:.82em; cursor:pointer;
}
.btn-danger-sm {
  background:transparent; color:var(--danger);
  border:none; font-size:.85em; cursor:pointer; padding:2px 6px;
}

/* ---- dataset tab ---- */
.db-stat { text-align:center; padding:12px; }
.db-stat .num { font-size:1.8em; font-weight:700; color:var(--teal);
                font-family:'Georgia',serif; }
.db-stat .lbl { font-size:.75em; color:var(--muted); text-transform:uppercase;
                 letter-spacing:.06em; }
.var-row { display:flex; align-items:center; padding:6px 0;
           border-bottom:1px solid #f0ede6; gap:8px; }
.var-row:last-child { border-bottom:none; }
.var-code { font-family:monospace; font-size:.85em; color:var(--navy);
            min-width:90px; }
.var-desc { font-size:.82em; color:var(--muted); flex:1; }
.var-cat  { font-size:.75em; color:var(--teal); background:var(--teal-lt);
            padding:2px 6px; border-radius:10px; white-space:nowrap; }
.staged-tag {
  display:inline-flex; align-items:center; gap:4px;
  background:#f0f4ff; border:1px solid #c7d2fe;
  border-radius:12px; padding:3px 10px; margin:3px;
  font-size:.82em; color:#3730a3;
}

/* ---- constraint builder ---- */
.constraint-row { display:flex; gap:6px; align-items:center; margin-bottom:0; }
.constraint-row .form-control { font-size:.85em; }

/* Two-column flag builder: wide left gutter + right conditions column */
.dcv-row-wrap { display:flex; align-items:stretch; min-height:36px; }
.dcv-gutter {
  width:96px; flex-shrink:0;
  display:flex; align-items:center;
  position:relative;
}
/* OR connectors: left-aligned in gutter */
.dcv-gutter-or { justify-content:flex-start; padding-left:4px; }
.dcv-condition-col { flex:1; display:flex; align-items:center; padding:3px 0; }

/* [ bracket: all segments pin their spine to left:64px so they join flush.
   Cap width (10px) extends rightward from the spine toward the conditions.
   ::before is position:absolute inside the position:relative .dcv-gutter.  */
.dcv-gutter-and-start::before {
  content:''; position:absolute;
  left:64px; top:50%; bottom:0; width:10px;
  border-left:2px solid var(--teal); border-top:2px solid var(--teal);
}
.dcv-gutter-and-inner::before {
  content:''; position:absolute;
  left:64px; top:0; bottom:0; width:10px;
  border-left:2px solid var(--teal);
}
.dcv-gutter-and-connector::before {
  content:''; position:absolute;
  left:64px; top:0; bottom:0; width:10px;
  border-left:2px solid var(--teal);
}
.dcv-gutter-and-end::before {
  content:''; position:absolute;
  left:64px; top:0; bottom:50%; width:10px;
  border-left:2px solid var(--teal); border-bottom:2px solid var(--teal);
}
/* AND button sits exactly on the spine: absolutely centered at left:64px */
.dcv-gutter-and-connector .btn-connector {
  position:absolute; left:64px; transform:translateX(-50%); z-index:1;
}

.btn-connector {
  background:#fff; color:#222;
  border:2px solid #333; border-radius:3px;
  padding:1px 8px; font-size:.78em; font-weight:700;
  cursor:pointer; min-width:42px; letter-spacing:.05em;
}
.btn-connector:hover { background:#f5f5f5; border-color:#000; }

/* ---- variable row (exposure/outcome picker) ---- */
.var-pick-row { display:flex; gap:6px; align-items:center; margin-bottom:6px; }
.var-pick-row .selectize-control { flex:1; font-size:.85em; }
.transform-sel {
  width:80px !important; font-size:.82em; height:34px;
  border:1px solid var(--border); border-radius:3px;
  background:var(--white); color:var(--text); padding:4px 6px;
  flex-shrink:0;
}
body.analysis-timetrend .transform-sel { display:none !important; }
.btn-remove-var {
  background:transparent; border:none; color:var(--danger);
  font-size:1.1em; cursor:pointer; padding:2px 4px; flex-shrink:0;
}
.wqs-dir-sel {
  width:90px !important; font-size:.82em; height:34px;
  border:1px solid var(--border); border-radius:3px;
  background:var(--white); color:var(--text); padding:4px 6px;
  flex-shrink:0; display:none;
}
body.wqs-active .wqs-dir-sel { display:block; }

/* ---- population tabs ---- */
.pop-tabs .nav-tabs { border-bottom:1px solid var(--border); }
.pop-tabs .nav-tabs > li > a { font-size:.85em; padding:6px 14px; }

/* ---- results table ---- */
.results-tbl { width:100%; border-collapse:collapse; font-size:.88em; }
.results-tbl th {
  background:#f0ede6; padding:8px 10px; text-align:left;
  font-weight:600; font-size:.78em; text-transform:uppercase;
  letter-spacing:.04em; border-bottom:2px solid var(--border);
}
.results-tbl td { padding:7px 10px; border-bottom:1px solid #f5f3ee; }
.results-tbl tr:last-child td { border-bottom:none; }
.results-tbl .sig { color:var(--teal); font-weight:600; }
.results-tbl .ns  { color:var(--muted); }

/* ---- status pills ---- */
.status-running { background:#e0f2fe; color:#0369a1;
  padding:8px 14px; border-radius:4px; font-size:.88em; }
.status-done    { background:#dcfce7; color:var(--success);
  padding:8px 14px; border-radius:4px; font-size:.88em; }
.status-error   { background:#fee2e2; color:var(--danger);
  padding:8px 14px; border-radius:4px; font-size:.88em; }

/* ---- misc ---- */
.hint { font-size:.76em; color:var(--muted); margin-top:3px; }
.section-label { font-size:.8em; font-weight:600; color:var(--muted);
  text-transform:uppercase; letter-spacing:.06em; margin-bottom:6px; }

/* ---- NPACK loading screen ---- */
#npack-splash {
  position: fixed; inset: 0; z-index: 9999;
  background: rgba(20,20,20,.55);
  display: flex; align-items: center; justify-content: center;
  transition: opacity .5s ease;
}
#npack-splash.fade-out { opacity: 0; pointer-events: none; }
#npack-splash-box {
  background: #CC0000;
  border: 4px solid #000;
  border-radius: 18px;
  padding: 48px 64px;
  text-align: center;
  box-shadow: 0 8px 40px rgba(0,0,0,.55);
  max-width: 520px;
  width: 88vw;
}
#npack-splash-box .npack-title {
  font-family: 'Impact', 'Arial Black', 'Franklin Gothic Heavy', sans-serif;
  font-size: 5.2em;
  font-weight: 900;
  color: #000;
  letter-spacing: .06em;
  line-height: 1;
  text-shadow:
    -2px -2px 0 #fff,  2px -2px 0 #fff,
    -2px  2px 0 #fff,  2px  2px 0 #fff,
    -3px  0   0 #fff,  3px  0   0 #fff,
     0   -3px 0 #fff,  0    3px 0 #fff;
  display: block;
  margin-bottom: 10px;
}
#npack-splash-box .npack-subtitle {
  font-family: 'Impact', 'Arial Black', 'Franklin Gothic Heavy', sans-serif;
  font-size: 1.45em;
  font-weight: 900;
  color: #000;
  letter-spacing: .14em;
  line-height: 1.3;
  text-shadow:
    -1px -1px 0 #fff,  1px -1px 0 #fff,
    -1px  1px 0 #fff,  1px  1px 0 #fff;
  display: block;
  text-transform: uppercase;
  margin-bottom: 32px;
}
#npack-splash-box .npack-bar-wrap {
  background: rgba(0,0,0,.18);
  border-radius: 8px;
  height: 8px;
  overflow: hidden;
  margin-bottom: 14px;
}
#npack-splash-box .npack-bar {
  height: 100%;
  width: 0%;
  background: #000;
  border-radius: 8px;
  animation: npack-pulse 1.6s ease-in-out infinite alternate;
  transition: width .3s ease;
}
#npack-splash-box .npack-loading-text {
  font-family: 'Impact', 'Arial Black', sans-serif;
  font-size: .82em;
  color: #000;
  letter-spacing: .1em;
  text-transform: uppercase;
  opacity: .75;
}
@keyframes npack-pulse {
  from { opacity: .6; }
  to   { opacity: 1; }
}

/* ---- R script preview ---- */
#rscript_preview_ui {
  background: transparent !important;
  color: #cdd6f4;
  font-family: 'Consolas', 'Fira Mono', 'Monaco', monospace;
  font-size: .82em;
  white-space: pre;
  border: none !important;
  padding: 0 !important;
  margin: 0;
}

/* ---- project bar ---- */
.project-bar {
  background: #f0ede6; border-bottom: 1px solid var(--border);
  padding: 6px 24px; display:flex; align-items:center; gap:10px;
  font-size:.82em; color:var(--muted);
}
.project-bar .shiny-input-container { margin:0 !important; }

/* Hide the filename text box and progress bar from fileInput */
#load_project_file .form-control       { display:none !important; }
#load_project_file_progress            { display:none !important; }
#load_project_file .input-group        { display:inline-flex; }

/* Style the file-chooser button to match other project-bar buttons */
#load_project_file .btn-file {
  background: white;
  color: var(--text);
  border: 1px solid #ccc;
  border-radius: 4px;
  padding: 3px 12px;
  font-size: .8em;
  font-weight: 400;
  cursor: pointer;
  white-space: nowrap;
}
#load_project_file .btn-file:hover { background: #e8e8e8; }
"

## ============================================================================
## UI
## ============================================================================

ui <- fluidPage(
  useShinyjs(),

  ## NPACK loading splash — hidden by JS once data is ready (min 5 s)
  tags$div(id = "npack-splash",
    tags$div(id = "npack-splash-box",
      tags$span(class = "npack-title",    "NPACK"),
      tags$span(class = "npack-subtitle", "NHANES Practical Analysis Creator Kit"),
      tags$div(class = "npack-bar-wrap",
        tags$div(class = "npack-bar", id = "npack-bar")
      ),
      tags$span(class = "npack-loading-text", "Loading dataset…")
    )
  ),

  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML("
      // No beforeunload warning — the browser's close-tab WebSocket teardown
      // disrupts Shiny's session in a way that cannot be cleanly recovered,
      // so the warning was causing more harm (permanently broken buttons) than
      // the accidental-close risk it was meant to prevent.

      // Restore focus after Shiny re-renders outputs.
      // When Shiny patches the DOM (e.g. search results updating), the
      // currently focused element can be destroyed and recreated, losing
      // focus. We save the focused element's ID before each update and
      // restore it after.
      var _savedFocusId   = null;
      var _savedFocusPos  = null;

      $(document).on('shiny:outputinvalidated', function(e) {
        var el = document.activeElement;
        if (!el) return;
        _savedFocusId = el.id || null;
        // Save cursor position for text inputs
        if (el.tagName === 'INPUT' || el.tagName === 'TEXTAREA') {
          try { _savedFocusPos = el.selectionStart; } catch(e) {}
        }
      });

      $(document).on('shiny:value', function(e) {
        if (!_savedFocusId) return;
        setTimeout(function() {
          var el = document.getElementById(_savedFocusId);
          if (el) {
            el.focus();
            if (el.tagName === 'INPUT' || el.tagName === 'TEXTAREA') {
              try {
                // Always restore cursor to end of current value — the saved
                // position is stale when the user typed more chars during render.
                var pos = el.value.length;
                el.setSelectionRange(pos, pos);
              } catch(e) {}
            }
          }
          _savedFocusId  = null;
          _savedFocusPos = null;
        }, 0);
      });

      // Toggle body class so CSS can hide transform dropdowns for time-trend.
      $(document).on('shiny:inputchanged', function(e) {
        if (e.name === 'analysis_type') {
          if (e.value === 'timetrend') {
            document.body.classList.add('analysis-timetrend');
          } else {
            document.body.classList.remove('analysis-timetrend');
          }
        }
      });

      // Filter .var-row elements inside a container by a search input's value.
      // Re-applied automatically after Shiny re-renders via MutationObserver.
      function filterVarRows(inputId, containerId) {
        var q = document.getElementById(inputId).value.toLowerCase().trim();
        var container = document.getElementById(containerId);
        if (!container) return;
        var rows = container.querySelectorAll('.var-row');
        rows.forEach(function(row) {
          row.style.display = (!q || row.textContent.toLowerCase().includes(q)) ? '' : 'none';
        });
      }
      window.filterVarRows = filterVarRows;

      // Re-apply active filters whenever Shiny pushes new content into either list.
      var _filterObserver = new MutationObserver(function() {
        ['cv_db_search','dataset_search'].forEach(function(id) {
          var inp = document.getElementById(id);
          if (inp && inp.value) {
            var cid = id === 'cv_db_search' ? 'cv_db_list' : 'dataset_list';
            filterVarRows(id, cid);
          }
        });
      });
      document.addEventListener('DOMContentLoaded', function() {
        ['cv_db_list','dataset_list'].forEach(function(cid) {
          var el = document.getElementById(cid);
          if (el) _filterObserver.observe(el, {childList: true, subtree: true});
        });
      });

      // Toggle wqs-active class on body so per-outcome direction dropdowns show/hide
      $(document).on('shiny:inputchanged', function(e) {
        if (e.name === 'analysis_type') {
          document.body.classList.toggle('wqs-active', e.value === 'wqs');
        }
      });

      // ---- NPACK splash logic ----
      // Animate the progress bar and dismiss once server signals ready AND 5 s elapsed.
      var _npackStart   = Date.now();
      var _npackReady   = false;
      var _npackMinMs   = 5000;
      var _npackDismissed = false;

      function npackDismiss() {
        if (_npackDismissed) return;
        _npackDismissed = true;
        var el = document.getElementById('npack-splash');
        if (!el) return;
        el.classList.add('fade-out');
        setTimeout(function() { el.style.display = 'none'; }, 520);
      }

      function npackTryDismiss() {
        if (!_npackReady) return;
        var elapsed = Date.now() - _npackStart;
        var remaining = _npackMinMs - elapsed;
        if (remaining <= 0) {
          npackDismiss();
        } else {
          setTimeout(npackDismiss, remaining);
        }
      }

      // Animate bar: crawl to ~85% while waiting, jump to 100% on ready signal
      (function animateBar() {
        var bar = document.getElementById('npack-bar');
        if (!bar) return;
        var pct = 0;
        var iv = setInterval(function() {
          if (_npackDismissed) { clearInterval(iv); return; }
          if (_npackReady) {
            pct = 100;
            bar.style.width = '100%';
            clearInterval(iv);
          } else {
            // ease toward 85% asymptotically
            pct += (85 - pct) * 0.03;
            bar.style.width = pct + '%';
          }
        }, 80);
      })();

      // Server fires a custom message when dataset load is complete
      Shiny.addCustomMessageHandler('npack_ready', function(msg) {
        _npackReady = true;
        npackTryDismiss();
      });
    "))
  ),

  ## Header
  div(class="app-header",
    h2("NPACK"),
    span("NHANES Practical Analysis Creator Kit · NHANES 1999–2018")
  ),

  ## Project bar — save / load / new
  div(class="project-bar",
    span("Project:"),
    textInput("project_name_input", label=NULL,
              value="New Project",
              width="180px",
              placeholder="Project name..."),
    downloadButton("save_project_btn", "Save Project",
                   class="btn btn-default btn-sm",
                   style="padding:3px 12px;font-size:.8em;"),
    fileInput("load_project_file", label=NULL,
              accept=".rds",
              buttonLabel="Open Project",
              placeholder=""),
    actionButton("new_project_btn", "New Project",
                 class="btn btn-default btn-sm",
                 style="padding:3px 12px;font-size:.8em;"),
    div(style="margin-left:auto;display:flex;align-items:center;gap:6px;",
      span(style="font-size:.82em;color:inherit;", "Theme:"),
      tags$select(
        id       = "color_theme",
        style    = "font-size:.8em;padding:2px 6px;border:1px solid #ccc;border-radius:4px;background:white;color:#1a1a1a;cursor:pointer;",
        onchange = "document.body.classList.toggle('wolfpack', this.value === 'wolfpack');",
        tags$option(value="classic", "Classic"),
        tags$option(value="wolfpack", "Wolfpack")
      )
    )
  ),

  ## Top-level tabs
  navbarPage(title=NULL, id="main_tabs",
    windowTitle="NPACK",

    ## ==================================================================
    ## TAB 1: DATASET
    ## ==================================================================
    tabPanel("Dataset", value="tab_dataset",

      br(),
      fluidRow(

        ## LEFT: search + stage
        column(5,
          div(class="panel-box",
            h4("NHANES Variable Lookup"),
            fluidRow(
              column(8,
                textInput("var_search", label=NULL,
                          placeholder="Search by name or description...")
              ),
              column(4,
                selectInput("var_category", label=NULL,
                  choices=c("All categories"=""),
                  selected=""
                )
              )
            ),
            div(style="max-height:360px;overflow-y:auto;",
              uiOutput("search_results_ui")
            )
          ),

          div(class="panel-box",
            h4("Staged for Download"),
            uiOutput("staged_vars_ui"),
            br(),
            actionButton("build_btn", "Build / Update Dataset",
                          class="btn-run", icon=icon("database"))
          ),

          div(class="panel-box",
            h4("Custom Variable Builder"),
            tabsetPanel(id="cv_tabs", type="tabs",

              ## --- Tab 1: Default (Composite Flag) -------------------------
              tabPanel("Composite Flag",
                br(),
                p(class="hint", style="margin-bottom:8px;",
                  "Build composite disease flags. Use AND to combine criteria within a group, OR to separate groups. Result = 1 if the combined condition is true, NA only if ALL source variables are missing, otherwise 0."),
                fluidRow(
                  column(6, textInput("dcv_name", label="Variable name", placeholder="e.g. hasdb")),
                  column(6, textInput("dcv_category", label="Category", placeholder="e.g. Diabetes"))
                ),
                textInput("dcv_label", label="Description",
                           placeholder="e.g. Diabetes composite flag", width="100%"),
                uiOutput("dcv_criteria_ui"),
                actionButton("dcv_add_criterion", "+ Add Criterion",
                              class="btn-sm-teal", style="margin-top:4px;"),
                br(), br(),
                fluidRow(
                  column(6, textInput("dcv_true_label",  label="Label when TRUE (1)",
                                       placeholder="e.g. Has diabetes", width="100%")),
                  column(6, textInput("dcv_false_label", label="Label when FALSE (0)",
                                       placeholder="e.g. No diabetes", width="100%"))
                ),
                actionButton("dcv_register_btn", "Register Composite Variable",
                              class="btn-sm-teal", icon=icon("plus")),
                br(), br(),
                uiOutput("dcv_status_ui")
              ),

              ## --- Tab 2: Bin Builder --------------------------------------
              tabPanel("Bin Variable",
                br(),
                p(class="hint", style="margin-bottom:8px;",
                  "Assign integer bin labels based on value ranges. Participants not matching any bin get NA."),
                fluidRow(
                  column(6, textInput("bcv_name", label="Variable name", placeholder="e.g. a1c_cat")),
                  column(6, textInput("bcv_category", label="Category", placeholder="e.g. Diabetes"))
                ),
                fluidRow(
                  column(8,
                    textInput("bcv_label", label="Description",
                               placeholder="e.g. HbA1c category", width="100%")
                  ),
                  column(4,
                    selectizeInput("bcv_var", label="Variable",
                      choices=NULL, selected=NULL,
                      options=list(create=FALSE, placeholder="variable...",
                                   maxOptions=20, searchField=c("value","label")))
                  )
                ),
                uiOutput("bcv_bins_ui"),
                actionButton("bcv_add_bin", "+ Add Bin",
                              class="btn-sm-teal", style="margin-top:4px;"),
                br(), br(),
                actionButton("bcv_register_btn", "Register Bin Variable",
                              class="btn-sm-teal", icon=icon("plus")),
                br(), br(),
                uiOutput("bcv_status_ui")
              ),

              ## --- Tab 3: Advanced Bin Variable ------------------------------------
              tabPanel("Advanced Bin Variable",
                br(),
                p(class="hint", style="margin-bottom:8px;",
                  "Assign ordinal bin values using per-bin variable conditions. Each bin can reference a different variable with one comparison operator. The participant is assigned the HIGHEST bin number where the condition is TRUE. Define a condition for Bin 0 to restrict who qualifies as the default — participants who fail all conditions (including Bin 0) receive NA and are excluded."),
                fluidRow(
                  column(6, textInput("abv_name", label="Variable name", placeholder="e.g. cvd_risk_tier")),
                  column(6, textInput("abv_category", label="Category", placeholder="e.g. CVD"))
                ),
                textInput("abv_label", label="Description",
                           placeholder="e.g. CVD risk tier", width="100%"),
                uiOutput("abv_bin0_ui"),
                uiOutput("abv_bins_ui"),
                actionButton("abv_add_bin", "+ Add Bin",
                              class="btn-sm-teal", style="margin-top:4px;"),
                br(), br(),
                actionButton("abv_register_btn", "Register Advanced Bin Variable",
                              class="btn-sm-teal", icon=icon("plus")),
                br(), br(),
                uiOutput("abv_status_ui")
              ),

              ## --- Tab 4: Advanced (Formula) --------------------------------
              tabPanel("Formula",
                br(),
                fluidRow(
                  column(6, textInput("cv_name", label="Variable name", placeholder="e.g. tc_hdl_ratio")),
                  column(6, textInput("cv_category", label="Category", placeholder="e.g. Lipids"))
                ),
                textInput("cv_label", label="Description",
                           placeholder="e.g. Total cholesterol / HDL ratio", width="100%"),
                textAreaInput("cv_formula", label="Formula",
                           placeholder="e.g. LBXTC / LBDHDD",
                           width="100%", rows=5),
                div(class="hint", style="margin-bottom:8px;",
                  "Write any R expression or multi-line script. Database column names are in scope directly (no $ needed).",
                  br(),
                  "Functions: log()  sqrt()  ifelse()  is.na()  zscore(col)",
                  br(),
                  "The value of the ", tags$strong("last expression"), " is used as the variable.",
                  br(),
                  "Variables assigned with ", tags$code("<-"), " are automatically treated as intermediates.",
                  " Use ", tags$code("# functions: fn1, fn2"), " to suppress warnings for unlisted R functions.",
                  br(),
                  tags$em("Example:"),
                  tags$pre(style="margin:4px 0 0 0;font-size:.78em;",
                    "# functions: cummax, any\nkappa <- ifelse(sex == 2, 0.7, 0.9)\nratio <- lbxscr / kappa\nifelse(ratio > 5, NA, ratio)")
                ),
                actionButton("cv_register_btn", "Register Custom Variable",
                              class="btn-sm-teal", icon=icon("plus")),
                br(), br(),
                uiOutput("cv_status_ui")
              ),

              ## --- Tab 4: Merge Variables ------------------------------------
              tabPanel("Merge Variables",
                br(),
                p(class="hint", style="margin-bottom:8px;",
                  "Combine two variables measuring the same concept. The left variable takes precedence: its value is used when available; the right variable fills in when the left is NA. The analytic weight will be the least restrictive (broadest-coverage) weight of the two sources per cycle."),
                fluidRow(
                  column(6, textInput("mv_name", label="Variable name", placeholder="e.g. sleep_time")),
                  column(6, textInput("mv_category", label="Category", placeholder="e.g. Sleep"))
                ),
                textInput("mv_label", label="Description",
                           placeholder="e.g. Sleep duration (hours)", width="100%"),
                fluidRow(
                  column(6,
                    selectizeInput("mv_var_left", label="Left variable (primary)",
                      choices=NULL, selected=NULL,
                      options=list(create=FALSE, placeholder="primary variable...",
                                   maxOptions=20, searchField=c("value","label")))
                  ),
                  column(6,
                    selectizeInput("mv_var_right", label="Right variable (fallback)",
                      choices=NULL, selected=NULL,
                      options=list(create=FALSE, placeholder="fallback variable...",
                                   maxOptions=20, searchField=c("value","label")))
                  )
                ),
                div(class="hint", style="margin-bottom:8px;",
                  "Result = left value if not NA, otherwise right value. Use when two survey instruments measure the same construct across different cycles or sub-populations."
                ),
                actionButton("mv_register_btn", "Register Merge Variable",
                              class="btn-sm-teal", icon=icon("plus")),
                br(), br(),
                uiOutput("mv_status_ui")
              )
            ),

          ),

          div(class="panel-box",
            h4("Custom Variable Database"),
            tags$input(
              type        = "text",
              id          = "cv_db_search",
              placeholder = "Search variables...",
              oninput     = "filterVarRows('cv_db_search','cv_db_list')",
              style       = "width:100%;margin-bottom:8px;padding:4px 2px;border:none;border-bottom:1px solid #9ca3af;border-radius:0;background:#ffffff;color:#111827;font-size:.85em;outline:none;"
            ),
            div(id="cv_db_list", style="max-height:320px;overflow-y:auto;",
              uiOutput("cv_list_ui")
            )
          )
        ),

        ## RIGHT: current dataset
        column(7,
          div(class="panel-box",
            div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:12px;",
              h4(style="margin:0;padding:0;border:none;", "Current Dataset"),
              actionButton("reset_dataset_btn", "Reset Dataset",
                           class="btn btn-default btn-sm",
                           style="padding:3px 12px;font-size:.8em;color:var(--danger);border-color:var(--danger);")
            ),
            uiOutput("dataset_stats_ui"),
            br(),
            tags$input(
              type        = "text",
              id          = "dataset_search",
              placeholder = "Search variables...",
              oninput     = "filterVarRows('dataset_search','dataset_list')",
              style       = "width:100%;margin-bottom:8px;padding:4px 2px;border:none;border-bottom:1px solid #9ca3af;border-radius:0;background:#ffffff;color:#111827;font-size:.85em;outline:none;"
            ),
            div(id="dataset_list", style="max-height:480px;overflow-y:auto;",
              uiOutput("dataset_vars_ui")
            )
          ),
          uiOutput("build_status_ui"),

          div(class="panel-box", style="margin-top:14px;",
            h4("Analysis Variables Status"),
            div(class="hint", style="margin-bottom:10px;",
              "Variables required by the current Analysis tab configuration."),
            uiOutput("analysis_vars_status_ui")
          ),
          uiOutput("cv_detail_ui")
        )
      )
    ),

    ## ==================================================================
    ## TAB 2: ANALYSIS
    ## ==================================================================
    tabPanel("Analysis", value="tab_analysis",

      br(),
      fluidRow(

        ## LEFT: shared inputs
        column(4,

          div(class="panel-box",
            h4("Analysis Type"),
            selectInput("analysis_type", label=NULL,
              choices  = c(
                "Linear Regression"       = "linear",
                "Logistic Regression"     = "logistic",
                "Spline (Non-linear)"     = "spline",
                "Multinomial Regression"  = "multinomial",
                "WQS (Mixture)"           = "wqs",
                "qgcomp (Mixture)"        = "qgcomp",
                "BKMR (Mixture)"          = "bkmr",
                "Mediation Analysis"      = "mediation",
                "Time-Trend"              = "timetrend"
              ),
              selected = "linear"
            ),

            ## -- Logistic options --
            conditionalPanel(
              condition = "input.analysis_type == 'logistic'",
              div(class="hint", style="margin-bottom:6px;",
                "Outcome must be binary (0/1). Reports odds ratios (OR) with 95% CI."),
              checkboxInput("logistic_categorical",
                label = tags$span(style="white-space:nowrap;",
                  "Treat exposure as categorical"),
                value = FALSE),
              conditionalPanel(
                condition = "input.logistic_categorical == true",
                div(class="hint", style="margin-top:2px;margin-bottom:2px;",
                  "Each non-reference level gets its own OR vs. the lowest level. ",
                  "No linearity assumption across levels.")
              ),
              conditionalPanel(
                condition = "input.logistic_categorical == false",
                div(class="hint", style="margin-top:2px;margin-bottom:2px;",
                  "One OR per unit increase in the exposure. Assumes a linear ",
                  "relationship on the log-odds scale.")
              )
            ),

            ## -- Spline options --
            conditionalPanel(
              condition = "input.analysis_type == 'spline'",
              selectInput("spline_knot_mode", "Knot placement",
                choices = c(
                  "Automatic (df = 3)"  = "auto",
                  "Quantile (choose df)" = "quantile",
                  "Fixed (enter values)" = "fixed"
                ), selected = "auto"),
              conditionalPanel(
                condition = "input.analysis_type == 'spline' && input.spline_knot_mode == 'quantile'",
                numericInput("spline_df", "Degrees of freedom",
                              value = 3, min = 2, max = 10, step = 1),
                div(class="hint", "Knots placed at equally-spaced quantiles of the exposure.")
              ),
              conditionalPanel(
                condition = "input.analysis_type == 'spline' && input.spline_knot_mode == 'fixed'",
                textInput("spline_knots_fixed", "Knot positions (comma-separated)",
                          placeholder = "e.g. 1.5, 3.0, 6.0"),
                div(class="hint", "Enter the exact predictor values where knots should be placed.")
              ),
              conditionalPanel(
                condition = "input.analysis_type == 'spline' && input.spline_knot_mode == 'auto'",
                div(class="hint", "Natural cubic spline with df = 3; knots at data quantiles.")
              )
            ),

            ## -- Multinomial options --
            conditionalPanel(
              condition = "input.analysis_type == 'multinomial'",
              div(class="hint", style="margin-bottom:6px;",
                "Outcome must be a factor with 3+ levels (e.g. BMI category). Reports ORs vs. reference level.")
            ),

            ## -- WQS options --
            conditionalPanel(
              condition = "input.analysis_type == 'wqs'",
              div(class="hint", style="margin-bottom:8px;",
                "Weighted Quantile Sum: all exposures enter jointly as a weighted mixture index. Set the direction constraint per outcome variable in the Outcomes section."),
              numericInput("wqs_q",    "Quantiles (q)", value=4, min=2, max=10, step=1),
              numericInput("wqs_boot", "Bootstrap iterations", value=100, min=10, max=2000, step=50),
              numericInput("wqs_seed", "Random seed", value=42, min=1, step=1)
            ),

            ## -- qgcomp options --
            conditionalPanel(
              condition = "input.analysis_type == 'qgcomp'",
              div(class="hint", style="margin-bottom:8px;",
                "Quantile-based g-computation: flexible directional weights per exposure."),
              numericInput("qgcomp_q",    "Quantiles (q)", value=4, min=2, max=10, step=1),
              numericInput("qgcomp_seed", "Random seed",   value=42, min=1, step=1),
              checkboxInput("qgcomp_bootstrap", "Use bootstrap (instead of EE)", value=FALSE),
              conditionalPanel(
                condition = "input.qgcomp_bootstrap == true",
                numericInput("qgcomp_boot", "Bootstrap iterations (B)", value=200, min=50, max=5000, step=50)
              ),
              conditionalPanel(
                condition = "input.qgcomp_bootstrap == false",
                div(class="hint", style="font-size:11px; color:#888;",
                  "Estimating equations (EE) with sandwich SE — robust, no bootstrap needed.")
              )
            ),

            ## -- BKMR options --
            conditionalPanel(
              condition = "input.analysis_type == 'bkmr'",
              div(class="hint", style="margin-bottom:8px;",
                "BKMR fits one joint model per outcome. All exposures enter",
                "together as the Z matrix; covariates enter as X."),
              numericInput("bkmr_iter", "MCMC iterations",
                            value=1000, min=100, max=50000, step=500),
              numericInput("bkmr_seed", "Random seed",
                            value=42, min=1, step=1),
              checkboxInput("bkmr_parallel", "Run parallel chains (bkmrhat)",
                             value=FALSE),
              conditionalPanel(
                condition = "input.bkmr_parallel == true",
                numericInput("bkmr_chains", "Number of chains",
                              value=4, min=2, max=8, step=1),
                div(class="hint",
                  "Runs multiple chains in parallel and reports R-hat and ESS",
                  "convergence diagnostics. Requires the bkmrhat and future packages.")
              )
            ),

            ## -- Mediation options --
            conditionalPanel(
              condition = "input.analysis_type == 'mediation'",
              div(class="hint", style="margin-bottom:8px;",
                "Tests whether a mediator explains the exposure → outcome pathway (ACME + ADE)."),
              selectizeInput("mediators", "Mediator variable(s)", choices=NULL, multiple=TRUE,
                options=list(create=TRUE, placeholder="Type to search...",
                             plugins=list("remove_button"))),
            ),

            ## -- Time-trend note --
            conditionalPanel(
              condition = "input.analysis_type == 'timetrend'",
              div(class="hint", style="margin-bottom:6px;",
                "Plots survey-weighted means per NHANES cycle. Exposures and outcomes are both trended."),
              checkboxInput("trend_pct_change",
                "Report % change per year (log-transform internally)",
                value = TRUE),
            )
          ),

          div(class="panel-box",
            h4("Exposures"),
            uiOutput("exposures_ui"),
            actionButton("add_exposure_btn", "+ Add Exposure",
                          class="btn-sm-teal", style="margin-top:6px;")
          ),

          div(class="panel-box",
            h4("Outcomes"),
            uiOutput("outcomes_ui"),
            actionButton("add_outcome_btn", "+ Add Outcome",
                          class="btn-sm-teal", style="margin-top:6px;")
          ),

          div(class="panel-box",
            h4("Shared Covariates"),
            div(class="hint", style="margin-bottom:6px;",
              "Applied to all populations unless overridden"),
            selectizeInput("shared_covariates", label=NULL,
              choices=NULL, multiple=TRUE,
              options=list(
                create=TRUE, placeholder="Type to search or add...",
                plugins=list("remove_button")
              )
            )
          ),

          checkboxInput("complete_cases_only",
            "Restrict to complete cases across all exposures & outcomes",
            value = FALSE),

          conditionalPanel(
            condition = "input.analysis_type != 'bkmr'",
            checkboxInput("use_dietary_weight",
              label = tags$span(style = "white-space:nowrap;",
                "Use dietary recall weights"),
              value = FALSE),
            conditionalPanel(
              condition = "input.use_dietary_weight == true",
              selectInput("dietary_recall_days",
                label = NULL,
                choices = c(
                  "1-day recall (WTDRD1 / WTDR4YR)" = "1day",
                  "2-day recall (WTDRD2)"            = "2day"
                ),
                selected = "1day"
              ),
              conditionalPanel(
                condition = "input.dietary_recall_days == '1day'",
                div(class="hint", style="margin-top:2px;",
                  "Uses WTDRD1 for all cycles. When both 1999 and 2001 are present,",
                  "WTDR4YR is used for those cycles per CDC 4-year pooling rules.")
              ),
              conditionalPanel(
                condition = "input.dietary_recall_days == '2day'",
                div(class="hint", style="margin-top:2px;",
                  "Uses WTDRD2. The 1999 and 2001 cycles are excluded — no 2-day",
                  "recall weight exists for those cycles.")
              )
            )
          ),

          conditionalPanel(
            condition = "input.analysis_type == 'linear' || input.analysis_type == 'logistic' || input.analysis_type == 'spline'",
            checkboxInput("quartile_stratified",
              label = tags$span(style = "white-space:nowrap;",
                "Also run stratified by exposure quartile (Q1–Q4)"),
              value = FALSE)
          ),

          actionButton("run_btn", "Run Analysis",
                        class="btn-run", icon=icon("play"))
        ),

        ## RIGHT: populations + results
        column(8,

          ## Population tabs
          div(class="panel-box",
            div(style="display:flex;justify-content:space-between;align-items:center;",
              div(class="section-label", "Populations"),
              actionButton("add_pop_btn", "+ Add Population",
                            class="btn-sm-teal")
            ),
            br(),
            uiOutput("population_tabs_ui")
          ),

          ## Status
          uiOutput("analysis_status_ui"),

          ## Results tabs (one per population)
          uiOutput("results_ui")
        )
      )
    ),

    ## ==================================================================
    ## TAB 3: PLOTS
    ## ==================================================================
    tabPanel("Plots", value="tab_plots",

      br(),
      fluidRow(

        ## LEFT: settings
        column(3,
          div(class="panel-box",
            h4("Plot Settings"),
            uiOutput("plot_pop_selector_ui"),
            uiOutput("trend_plot_sliders_ui"),
            uiOutput("linear_plot_sliders_ui"),
            div(class="hint", style="margin-top:6px;",
              "Run an analysis first to generate plots.")
          )
        ),

        ## RIGHT: one forest plot per outcome
        column(9,
          uiOutput("forest_plots_ui")
        )
      )
    ),

    ## ==================================================================
    ## TAB 4: R SCRIPT (hidden from UI — tab commented out, code preserved below)
    ## ==================================================================
    # tabPanel("R Script", value="tab_rscript",
    #
    #   br(),
    #   fluidRow(
    #     column(3,
    #       div(class="panel-box",
    #         h4("Export R Script"),
    #         p(class="hint",
    #           "Generates a standalone R script that reproduces the current ",
    #           "analysis configuration. Run it from the project root directory."),
    #         br(),
    #         downloadButton("download_rscript_btn", "Download .R File",
    #                        class="btn-run",
    #                        style="width:100%;padding:10px;font-size:.95em;"),
    #         br(), br(),
    #         div(class="hint",
    #           "The script sources the same R files used by the app and calls ",
    #           "the appropriate analysis function with your current settings, ",
    #           "populations, and variable selections.")
    #       )
    #     ),
    #     column(9,
    #       div(class="panel-box",
    #         h4("Preview"),
    #         div(style="background:#1e1e2e;border-radius:4px;padding:16px;
    #                     overflow:auto;max-height:680px;",
    #           verbatimTextOutput("rscript_preview_ui")
    #         )
    #       )
    #     )
    #   )
    # )
  )
)

## ============================================================================
## SERVER
## ============================================================================

## Built-in derived variables — computed from source columns during a build,
## exactly like user custom variables.  Never fetched from NHANES directly.
BUILTIN_DERIVED_VARS <- c(
  "sex_bin", "race_hispanic", "race_black", "race_other",
  "smoking_status", "edu_clean",
  "hascvd", "oncvdmed",
  "non_hdl", "remnant", "egfr", "usfli"
)

## Builder functions for each built-in derived variable.
## Called during the build loop when the variable is staged.
## Note: add_matlab_equivalent_covariates() computes all four race/sex columns
## at once — staging any one of them runs the same builder (idempotent).
## apply_derived_outcomes("remnant") also recomputes non_hdl as a side-effect.
BUILTIN_BUILDERS <- list(
  "sex_bin"        = add_matlab_equivalent_covariates,
  "race_hispanic"  = add_matlab_equivalent_covariates,
  "race_black"     = add_matlab_equivalent_covariates,
  "race_other"     = add_matlab_equivalent_covariates,
  "edu_clean"      = add_edu_clean,
  "smoking_status" = add_smoking_status,
  "hascvd"         = add_has_cvd,
  "oncvdmed"       = add_on_cvd_med,
  "non_hdl"        = function(db) apply_derived_outcomes(db, "non_hdl"),
  "remnant"        = function(db) apply_derived_outcomes(db, c("non_hdl","remnant")),
  "egfr"           = add_egfr,
  "usfli"          = function(db) apply_derived_outcomes(db, "usfli")
)

server <- function(input, output, session) {

  ## ---- Reactive state -----------------------------------------------------
  .all_cycles <- c("1999-2000","2001-2002","2003-2004","2005-2006",
                   "2007-2008","2009-2010","2011-2012","2013-2014",
                   "2015-2016","2017-2018")

  rv <- reactiveValues(
    db            = NULL,          # loaded database data frame
    db_cols       = NULL,          # column metadata for autocomplete
    staged        = character(0),  # variables staged for download
    n_pops        = 1,             # number of populations
    pop_labels    = "Population 1",
    pop_cycles        = list(pop1 = c("1999-2000","2001-2002","2003-2004","2005-2006",
                                       "2007-2008","2009-2010","2011-2012","2013-2014",
                                       "2015-2016","2017-2018")),
    pop_custom_covars = list(pop1 = FALSE),
    pop_tabs_reset    = 0L,           # increment to force full population-tabs re-render
    results       = list(),        # named list of result objects per population
    analysis_type = "linear",      # "linear"|"logistic"|"spline"|"multinomial"|"wqs"|"qgcomp"|"bkmr"|"mediation"|"timetrend"
    build_status  = NULL,
    build_error   = NULL,
    run_status    = NULL,
    run_error     = NULL
  )

  ## ---- Load database on startup -------------------------------------------
  observe({
    path <- here("data", "nhanes_pool.rds")
    if (file.exists(path)) {
      rv$db      <- load_nhanes_database(path)
      rv$db_cols <- get_dataset_columns(rv$db)
      update_autocomplete()
    }
    ## Signal JS that loading is complete (also fires when no file found)
    session$sendCustomMessage("npack_ready", list())
  })

  update_autocomplete <- function() {
    req(rv$db_cols)
    choices <- setNames(rv$db_cols$column, rv$db_cols$label)
    ## Update shared covariates choices but preserve whatever the user has selected
    current_covs <- isolate(input$shared_covariates)
    updateSelectizeInput(session, "shared_covariates",
      choices=choices, server=TRUE,
      selected=if (length(current_covs) > 0) current_covs else NULL)
    updateSelectizeInput(session, "mediators",
      choices=choices, server=TRUE)
    ## Refresh exposure/outcome row dropdowns
    rv$var_choices_updated <- Sys.time()
  }

  ## ---- Project bar --------------------------------------------------------

  ## Save: collect state and offer download
  output$save_project_btn <- downloadHandler(
    filename = function() {
      nm <- gsub("[^A-Za-z0-9_-]", "_",
                 trimws(isolate(input$project_name_input) %||% "nhanes_project"))
      if (!nzchar(nm)) nm <- "nhanes_project"
      paste0(nm, "_", format(Sys.Date(), "%Y%m%d"), ".rds")
    },
    content = function(file) {
      proj <- collect_project(input, rv, pop_constraints)
      ## Persist the project name inside the file too
      proj$project_name <- isolate(input$project_name_input) %||% "nhanes_project"
      save_project(proj, file)
      showNotification("Project saved.", type="message", duration=5)
    }
  )

  ## Loaded project stored temporarily so the staging modal can access it
  rv$loaded_proj    <- NULL
  rv$pending_proj   <- NULL   ## awaiting custom-var confirmation
  rv$pending_proj_nm <- NULL

  ## Helper: collect every analysis variable from a project object
  project_vars <- function(proj) {
    exp_vars  <- Filter(nzchar, vapply(proj$exposure_rows %||% list(),
                                       `[[`, character(1), "var"))
    out_vars  <- Filter(nzchar, vapply(proj$outcome_rows  %||% list(),
                                       `[[`, character(1), "var"))
    cov_vars  <- proj$shared_covariates %||% character(0)
    med_vars  <- proj$settings$mediators %||% character(0)
    pop_cov_vars <- unique(unlist(lapply(proj$populations %||% list(), function(p) {
      if (isTRUE(p$custom_covars)) p$covariates %||% character(0) else character(0)
    })))
    con_vars <- unique(unlist(lapply(proj$populations %||% list(), function(p) {
      Filter(nzchar, vapply(p$constraints %||% list(),
                            function(c) c$var %||% "", character(1)))
    })))
    unique(c(exp_vars, out_vars, cov_vars, med_vars, pop_cov_vars, con_vars))
  }

  ## Internal helper: perform the actual project restore + show missing-vars modal.
  ## Called either directly (no custom-var issues) or after the user confirms
  ## custom-var registration.
  .do_restore_project <- function(proj, display_nm) {
    restore_project(proj, session, rv, pop_constraints, pop_constraint_counts)
    updateTextInput(session, "project_name_input", value = display_nm)

    ## Re-apply covariate selections explicitly after autocomplete refresh,
    ## since updateSelectizeInput in restore_project fires before choices load.
    saved_covs <- proj$shared_covariates %||% character(0)
    if (length(saved_covs) > 0 && !is.null(rv$db_cols)) {
      updateSelectizeInput(session, "shared_covariates",
        choices  = setNames(rv$db_cols$column, rv$db_cols$label),
        selected = saved_covs,
        server   = TRUE)
    }
    ## Per-population covariates
    for (i in seq_along(proj$populations %||% list())) {
      pop <- proj$populations[[i]]
      if (isTRUE(pop$custom_covars) && length(pop$covariates %||% character(0)) > 0
          && !is.null(rv$db_cols)) {
        updateSelectizeInput(session, paste0("pop_covariates_", i),
          choices  = setNames(rv$db_cols$column, rv$db_cols$label),
          selected = pop$covariates,
          server   = TRUE)
      }
    }

    rv$results     <- list()
    rv$run_status  <- NULL
    rv$loaded_proj <- proj   ## keep for staging modal

    ## Find variables missing from the current database
    all_vars <- project_vars(proj)
    in_db    <- if (!is.null(rv$db)) tolower(names(rv$db)) else character(0)
    cvars    <- names(get_custom_variables())
    missing  <- all_vars[!tolower(all_vars) %in% in_db]

    if (length(missing) == 0) {
      showNotification(sprintf("Project '%s' loaded. All variables present.",
                               display_nm), type = "message", duration = 5)
    } else {
      ## Classify missing vars for display
      make_badge <- function(v) {
        vl <- tolower(v)
        role <- if (vl %in% BUILTIN_DERIVED_VARS) "built-in"
                else if (vl %in% cvars)           "custom"
                else                               "NHANES"
        tags$li(
          span(style = "font-family:monospace;font-weight:600;", v), " ",
          span(style = sprintf("font-size:.78em;color:%s;",
                               switch(role, "built-in" = "#f59e0b",
                                           "custom"   = "#6366f1", "#6b7280")),
               sprintf("(%s)", role))
        )
      }

      showModal(modalDialog(
        title = sprintf("Project '%s' loaded", display_nm),
        div(
          p(sprintf("%d variable(s) required by this project are not in your current dataset:",
                    length(missing))),
          tags$ul(style = "max-height:240px;overflow-y:auto;margin-bottom:12px;",
                  lapply(missing, make_badge)),
          p(class = "hint",
            "Stage & Build will queue all missing variables (auto-staging prerequisites) ",
            "and immediately run the dataset build.")
        ),
        footer = tagList(
          modalButton("Skip"),
          actionButton("load_stage_only_btn",  "Stage Only",
                       class = "btn btn-default"),
          actionButton("load_stage_build_btn", "Stage & Build",
                       class = "btn btn-primary")
        ),
        size = "m"
      ))
    }
  }

  ## Load: read uploaded RDS; if custom variables need registration, ask first.
  observeEvent(input$load_project_file, {
    req(input$load_project_file)
    path <- input$load_project_file$datapath
    nm   <- tools::file_path_sans_ext(input$load_project_file$name)
    tryCatch({
      proj       <- load_project(path)
      display_nm <- proj$project_name %||% nm

      ## Check for custom variable issues before restoring
      cv_issues <- check_project_custom_vars(proj)

      if (length(cv_issues) > 0) {
        ## Store project and show confirmation modal — do NOT restore yet
        rv$pending_proj    <- proj
        rv$pending_proj_nm <- display_nm

        make_cv_row <- function(nm_cv, issue) {
          status_color <- if (issue$status == "missing") "#ef4444" else "#f59e0b"
          status_label <- if (issue$status == "missing") "missing from registry" else "formula mismatch"
          tagList(
            tags$li(
              span(style = "font-family:monospace;font-weight:600;", nm_cv), " ",
              span(style = sprintf("font-size:.78em;color:%s;", status_color),
                   sprintf("(%s)", status_label)),
              if (!is.null(issue$def$formula) && nzchar(issue$def$formula %||% ""))
                div(style = "font-size:.78em;color:#6b7280;margin-left:1em;",
                    sprintf("Formula: %s", issue$def$formula))
              else if (identical(issue$def$type %||% "", "merge"))
                div(style = "font-size:.78em;color:#6b7280;margin-left:1em;",
                    sprintf("Merge: %s ← %s (fallback: %s)",
                            nm_cv, issue$def$merge_left, issue$def$merge_right))
            )
          )
        }

        showModal(modalDialog(
          title = sprintf("Project '%s': Custom Variables Required", display_nm),
          div(
            p(sprintf(
              "%d custom variable(s) used by this project need to be registered before loading:",
              length(cv_issues))),
            tags$ul(style = "max-height:240px;overflow-y:auto;margin-bottom:12px;",
                    mapply(make_cv_row, names(cv_issues), cv_issues,
                           SIMPLIFY = FALSE, USE.NAMES = FALSE)),
            p(class = "hint",
              "Clicking Yes will add these custom variables to your registry. ",
              "You will then be prompted to stage and build any missing dataset variables.")
          ),
          footer = tagList(
            actionButton("load_cv_no_btn",  "No",
                         class = "btn btn-default"),
            actionButton("load_cv_yes_btn", "Yes",
                         class = "btn btn-primary")
          ),
          size = "m",
          easyClose = FALSE
        ))
        return()
      }

      ## No custom-var issues — restore immediately
      .do_restore_project(proj, display_nm)

    }, error = function(e) {
      showNotification(sprintf("Load failed: %s", conditionMessage(e)),
                       type = "error", duration = 8)
    })
  })

  ## User confirmed custom-var registration: register vars then restore project
  observeEvent(input$load_cv_yes_btn, {
    removeModal()
    proj       <- rv$pending_proj
    display_nm <- rv$pending_proj_nm
    rv$pending_proj    <- NULL
    rv$pending_proj_nm <- NULL
    req(!is.null(proj))

    defs <- proj$custom_var_defs %||% list()
    for (nm_cv in names(defs)) {
      d <- defs[[nm_cv]]
      if (identical(d$type %||% "formula", "merge")) {
        register_merge_variable(nm_cv,
                                d$label    %||% nm_cv,
                                d$category %||% "Custom",
                                d$merge_left,
                                d$merge_right)
      } else {
        register_custom_variable(nm_cv,
                                 d$label    %||% nm_cv,
                                 d$category %||% "Custom",
                                 d$formula  %||% "",
                                 label_map = d$label_map,
                                 or_groups = d$or_groups)
      }
    }

    .do_restore_project(proj, display_nm)
  })

  ## User declined custom-var registration: cancel load entirely
  observeEvent(input$load_cv_no_btn, {
    removeModal()
    rv$pending_proj    <- NULL
    rv$pending_proj_nm <- NULL
    showNotification("Project load cancelled.", type = "warning", duration = 4)
  })

  ## Helper: recursively collect all transitive dependencies for a variable.
  ## Returns a character vector ordered so prerequisites come before dependents.
  .collect_deps_recursive <- function(var, cvars, seen = character(0)) {
    vl <- tolower(var)
    if (vl %in% seen) return(character(0))   # cycle guard
    seen <- c(seen, vl)

    deps_static <- variable_dependencies[[vl]] %||% character(0)
    cv_entry    <- cvars[[vl]]
    deps_cv <- if (!is.null(cv_entry) && length(cv_entry$requires) > 0) {
      vapply(cv_entry$requires, function(r) {
        rl <- tolower(r)
        if (rl %in% BUILTIN_DERIVED_VARS || rl %in% names(cvars)) rl
        else toupper(r)
      }, character(1))
    } else character(0)

    direct_deps <- unique(c(deps_static, deps_cv))
    result <- character(0)
    for (d in direct_deps) {
      result <- c(result, .collect_deps_recursive(d, cvars, seen))
      seen   <- c(seen, tolower(d))
      result <- c(result, d)
    }
    unique(result)
  }

  ## Helper: stage all missing variables from the loaded project
  .stage_missing_from_project <- function() {
    proj    <- rv$loaded_proj
    if (is.null(proj)) return()
    all_vars <- project_vars(proj)
    in_db    <- if (!is.null(rv$db)) tolower(names(rv$db)) else character(0)
    missing  <- all_vars[!tolower(all_vars) %in% in_db]
    cvars    <- get_custom_variables()

    for (v in missing) {
      vl <- tolower(v)
      ## Determine correct stage value (lowercase for derived, uppercase for NHANES)
      is_derived <- vl %in% BUILTIN_DERIVED_VARS || vl %in% names(cvars)
      stage_val  <- if (is_derived) vl else toupper(v)

      ## Recursively collect all transitive dependencies
      all_deps     <- .collect_deps_recursive(v, cvars)
      staged_lower <- tolower(rv$staged)
      db_lower     <- tolower(if (!is.null(rv$db)) names(rv$db) else character(0))
      new_deps     <- all_deps[!tolower(all_deps) %in% c(db_lower, staged_lower)]

      to_add <- if (is_derived) {
        unique(c(new_deps, if (!vl %in% staged_lower) stage_val else NULL))
      } else {
        unique(c(if (!vl %in% staged_lower) stage_val else NULL, new_deps))
      }
      if (length(to_add) > 0)
        rv$staged <- c(rv$staged, to_add)
    }
  }

  observeEvent(input$load_stage_only_btn, {
    removeModal()
    .stage_missing_from_project()
    showNotification(
      sprintf("%d variable(s) staged. Click 'Build / Update Dataset' to fetch them.",
              length(rv$staged)),
      type="message", duration=5)
  })

  observeEvent(input$load_stage_build_btn, {
    removeModal()
    .stage_missing_from_project()
    ## Trigger the build button programmatically
    shinyjs::click("build_btn")
  })

  ## New project: reset everything to defaults
  observeEvent(input$new_project_btn, {
    showModal(modalDialog(
      title = "New Project",
      "This will clear all current settings. Continue?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("new_project_confirm", "Yes, start new",
                     class="btn btn-danger")
      )
    ))
  })

  observeEvent(input$new_project_confirm, {
    removeModal()
    rv$analysis_type  <- "linear"
    rv$exposure_rows  <- list(list(var="", transform="none"))
    rv$outcome_rows   <- list(list(var="", transform="none"))
    rv$n_pops         <- 1L
    rv$pop_labels     <- "Population 1"
    rv$pop_cycles        <- list(pop1 = .all_cycles)
    rv$pop_custom_covars <- list(pop1 = FALSE)
    rv$pop_tabs_reset    <- isolate(rv$pop_tabs_reset) + 1L
    rv$results        <- list()
    rv$run_status     <- NULL
    rv$run_error      <- NULL
    ## Clear constraint rows for all populations
    for (i in 1:10) {
      pid <- paste0("pop", i)
      pop_constraints[[pid]]       <- list()
      pop_constraint_counts[[pid]] <- 0L
    }
    ## Reset UI inputs to defaults
    updateTextInput(session, "project_name_input", value="Unsaved project")
    updateSelectInput(session, "analysis_type",    selected="linear")
    updateSelectizeInput(session, "shared_covariates", selected=character(0))
    updateCheckboxInput(session, "complete_cases_only",    value=FALSE)
    updateCheckboxInput(session, "logistic_categorical",    value=FALSE)
    updateSelectInput(session,  "spline_knot_mode", selected="auto")
    updateNumericInput(session, "spline_df",        value=3)
    updateTextInput(session,    "spline_knots_fixed", value="")
    updateNumericInput(session, "wqs_q",           value=4)
    updateNumericInput(session, "wqs_boot",        value=100)
    updateNumericInput(session, "wqs_seed",        value=42)
    updateNumericInput(session, "qgcomp_q",         value=4)
    updateNumericInput(session, "qgcomp_seed",      value=42)
    updateCheckboxInput(session, "qgcomp_bootstrap", value=FALSE)
    updateNumericInput(session, "qgcomp_boot",      value=200)
    updateNumericInput(session,  "bkmr_iter",     value=1000)
    updateNumericInput(session,  "bkmr_seed",     value=42)
    updateCheckboxInput(session, "bkmr_parallel", value=FALSE)
    updateNumericInput(session,  "bkmr_chains",   value=4)
    updateSelectizeInput(session, "mediators",     selected=character(0))
    updateTextInput(session, "pop_label_1",        value="Population 1")
    updateCheckboxGroupInput(session, "pop_cycles_1",
      selected=c("1999-2000","2001-2002","2003-2004","2005-2006",
                 "2007-2008","2009-2010","2011-2012","2013-2014",
                 "2015-2016","2017-2018"))
    updateCheckboxInput(session, "custom_covars_1", value=FALSE)
    showNotification("New project started.", type="message", duration=5)
  })

  ## ---- Reset dataset -------------------------------------------------------

  observeEvent(input$reset_dataset_btn, {
    showModal(modalDialog(
      title = "Reset Dataset",
      div(
        p("This will clear all staged variables and rebuild the base dataset from scratch by re-fetching the NHANES demographic roster from CDC."),
        p(class="hint", "This takes ~1–2 minutes. Any additional variables (lipids, PFAS, etc.) will need to be re-staged and built afterward.")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("reset_dataset_confirm", "Yes, reset",
                     class="btn btn-danger")
      )
    ))
  })

  observeEvent(input$reset_dataset_confirm, {
    removeModal()
    rv$db           <- NULL
    rv$db_cols      <- NULL
    rv$staged       <- character(0)
    rv$build_status <- "running"
    rv$build_error  <- NULL

    withProgress(message = "Rebuilding demographic roster from NHANES...", value = 0, {
      tryCatch({
        setProgress(value = 0.2, detail = "Fetching demographics...")
        build_nhanes_database()
        setProgress(value = 0.9, detail = "Loading...")
        path <- here("data", "nhanes_pool.rds")
        rv$db      <- load_nhanes_database(path)
        rv$db_cols <- get_dataset_columns(rv$db)
        update_autocomplete()
        rv$build_status <- "done"
        showNotification(
          sprintf("Dataset reset complete. %d participants loaded. Stage variables and click Build to add them.",
                  nrow(rv$db)),
          type = "message", duration = 7)
      }, error = function(e) {
        rv$build_status <- "error"
        rv$build_error  <- conditionMessage(e)
        showNotification(
          sprintf("Reset failed: %s", conditionMessage(e)),
          type = "error", duration = 10)
      })
    })
  })

  ## ---- Exposure / Outcome row state ----------------------------------------
  rv$exposure_rows <- list(
    list(var="", transform="none")
  )
  rv$outcome_rows <- list(
    list(var="", transform="none", wqs_direction="positive")
  )

  ## Build a single variable picker row
  make_var_row <- function(prefix, idx, var_val, transform_val, choices, wqs_dir_val = "positive") {
    transforms <- c("none"="raw", "ln"="ln", "log10"="log10", "log2"="log2",
                     "sqrt"="sqrt", "zscore"="zscore")
    opts <- lapply(names(transforms), function(val) {
      args <- list(value=val, transforms[[val]])
      if (val == transform_val) args$selected <- "selected"
      do.call(tags$option, args)
    })

    dir_sel <- if (prefix == "out") {
      dir_choices <- c("positive" = "Positive", "negative" = "Negative")
      dir_opts <- lapply(names(dir_choices), function(val) {
        args <- list(value = val, dir_choices[[val]])
        if (val == wqs_dir_val) args$selected <- "selected"
        do.call(tags$option, args)
      })
      tags$select(
        id       = paste0("out_dir_", idx),
        class    = "wqs-dir-sel",
        onchange = sprintf(
          "Shiny.setInputValue('out_dir_change', {idx:%d, val:this.value}, {priority:'event'})",
          idx),
        dir_opts
      )
    } else NULL

    div(class = "var-pick-row",
      selectizeInput(
        inputId  = paste0(prefix, "_var_", idx),
        label    = NULL,
        choices  = choices,
        selected = var_val,
        options  = list(
          create=FALSE, placeholder="variable...",
          maxOptions=50, searchField=c("value","label"),
          sortField=list(list(field="value", direction="asc")),
          ## onInitialize fires once on load — mark as initialized so
          ## onChange knows to ignore the first programmatic selection
          onInitialize=I(sprintf(
            "function(){ this._initialized_%s_%d = true; }",
            prefix, idx)),
          ## onChange only fires Shiny event after initialization is complete
          ## and only when the user actually makes a selection
          onChange=I(sprintf(
            "function(val){ if(this._initialized_%s_%d && val){ Shiny.setInputValue('%s_var_change', {idx:%d, val:val}, {priority:'event'}); } }",
            prefix, idx, prefix, idx))
        )
      ),
      tags$select(
        id       = paste0(prefix, "_tfm_", idx),
        class    = "transform-sel",
        onchange = sprintf(
          "Shiny.setInputValue('%s_tfm_change', {idx:%d, val:this.value}, {priority:'event'})",
          prefix, idx),
        opts
      ),
      dir_sel,
      tags$button("\u00d7", class="btn-remove-var",
        onclick=sprintf(
          "Shiny.setInputValue('%s_remove_row', %d, {priority:'event'})",
          prefix, idx))
    )
  }

  ## Render exposure rows — only rebuild when row count changes, not on
  ## every choices update. Choices are updated in-place via updateSelectizeInput.
  output$exposures_ui <- renderUI({
    n <- length(rv$exposure_rows)  ## only depend on row count
    choices <- if (!is.null(rv$db_cols))
      setNames(rv$db_cols$column, rv$db_cols$label) else character(0)
    rows <- lapply(seq_len(n), function(i) {
      r <- isolate(rv$exposure_rows[[i]])
      make_var_row("exp", i, r$var, r$transform, choices)
    })
    tagList(rows)
  })

  ## Render outcome rows — same pattern
  output$outcomes_ui <- renderUI({
    n <- length(rv$outcome_rows)
    choices <- if (!is.null(rv$db_cols))
      setNames(rv$db_cols$column, rv$db_cols$label) else character(0)
    rows <- lapply(seq_len(n), function(i) {
      r <- isolate(rv$outcome_rows[[i]])
      make_var_row("out", i, r$var, r$transform, choices,
                   wqs_dir_val = r$wqs_direction %||% "positive")
    })
    tagList(rows)
  })

  ## When choices update (new variable added to dataset), update selectize
  ## choices in-place without rebuilding the whole row
  observeEvent(rv$var_choices_updated, {
    choices <- if (!is.null(rv$db_cols))
      setNames(rv$db_cols$column, rv$db_cols$label) else character(0)
    for (i in seq_along(rv$exposure_rows)) {
      updateSelectizeInput(session, paste0("exp_var_", i),
        choices=choices, selected=isolate(rv$exposure_rows[[i]]$var),
        server=TRUE)
    }
    for (i in seq_along(rv$outcome_rows)) {
      updateSelectizeInput(session, paste0("out_var_", i),
        choices=choices, selected=isolate(rv$outcome_rows[[i]]$var),
        server=TRUE)
    }
  }, ignoreInit=FALSE)

  ## Add exposure row
  observeEvent(input$add_exposure_btn, {
    rv$exposure_rows <- c(rv$exposure_rows, list(list(var="", transform="none")))
  })

  ## Add outcome row
  observeEvent(input$add_outcome_btn, {
    rv$outcome_rows <- c(rv$outcome_rows, list(list(var="", transform="none", wqs_direction="positive")))
  })

  ## Remove exposure row
  observeEvent(input$exp_remove_row, {
    idx <- input$exp_remove_row
    if (length(rv$exposure_rows) > 1)
      rv$exposure_rows <- rv$exposure_rows[-idx]
  })

  ## Remove outcome row
  observeEvent(input$out_remove_row, {
    idx <- input$out_remove_row
    if (length(rv$outcome_rows) > 1)
      rv$outcome_rows <- rv$outcome_rows[-idx]
  })

  ## Update variable when selectize changes
  observeEvent(input$exp_var_change, {
    ch <- input$exp_var_change
    if (!is.null(ch) && ch$idx <= length(rv$exposure_rows))
      rv$exposure_rows[[ch$idx]]$var <- ch$val
  })
  observeEvent(input$out_var_change, {
    ch <- input$out_var_change
    if (!is.null(ch) && ch$idx <= length(rv$outcome_rows))
      rv$outcome_rows[[ch$idx]]$var <- ch$val
  })

  ## Update transform when dropdown changes
  observeEvent(input$exp_tfm_change, {
    ch <- input$exp_tfm_change
    if (!is.null(ch) && ch$idx <= length(rv$exposure_rows)) {
      rv$exposure_rows[[ch$idx]]$transform <- ch$val
    }
  })
  observeEvent(input$out_tfm_change, {
    ch <- input$out_tfm_change
    if (!is.null(ch) && ch$idx <= length(rv$outcome_rows)) {
      rv$outcome_rows[[ch$idx]]$transform <- ch$val
    }
  })
  observeEvent(input$out_dir_change, {
    ch <- input$out_dir_change
    if (!is.null(ch) && ch$idx <= length(rv$outcome_rows)) {
      rv$outcome_rows[[ch$idx]]$wqs_direction <- ch$val
    }
  })

  ## Helper: collect current exposure/outcome vars and flags from reactive state
  get_exposures <- function() {
    vars       <- character(0)
    transforms <- character(0)
    for (i in seq_along(rv$exposure_rows)) {
      r <- rv$exposure_rows[[i]]
      v <- r$var
      ## Also check input in case user typed but onChange hasn't fired
      v_input <- input[[paste0("exp_var_", i)]]
      if (!is.null(v_input) && nzchar(v_input)) v <- v_input
      ## Also check input in case user changed transform but observeEvent hasn't fired
      tfm_input <- input[[paste0("exp_tfm_", i)]]
      tfm <- if (!is.null(tfm_input) && nzchar(tfm_input)) tfm_input else (r$transform %||% "none")
      if (nzchar(v)) {
        vars       <- c(vars, v)
        transforms <- c(transforms, tfm)
      }
    }
    list(vars=vars, log10_flags=transforms == "log10", transforms=transforms)
  }

  get_outcomes <- function() {
    vars       <- character(0)
    transforms <- character(0)
    directions <- character(0)
    for (i in seq_along(rv$outcome_rows)) {
      r <- rv$outcome_rows[[i]]
      v <- r$var
      v_input <- input[[paste0("out_var_", i)]]
      if (!is.null(v_input) && nzchar(v_input)) v <- v_input
      if (nzchar(v)) {
        vars       <- c(vars, v)
        transforms <- c(transforms, r$transform %||% "none")
        directions <- c(directions, r$wqs_direction %||% "positive")
      }
    }
    list(vars=vars, log10_flags=transforms == "log10", transforms=transforms,
         wqs_directions=directions)
  }

  ## ---- Category filter for search -----------------------------------------
  observe({
    lkp  <- load_duke_lookup()
    cats <- sort(unique(lkp$category[nzchar(lkp$category)]))
    updateSelectInput(session, "var_category",
      choices=c("All categories"="", setNames(cats, cats)))
  })

  ## ---- Variable search results --------------------------------------------
  output$search_results_ui <- renderUI({
    lkp  <- load_duke_lookup()
    term <- trimws(input$var_search)
    cat_filter <- input$var_category

    results <- if (nchar(term) >= 2) {
      mask <- grepl(term, lkp$variable,     ignore.case=TRUE) |
              grepl(term, lkp$description,  ignore.case=TRUE)
      lkp[mask, ]
    } else {
      lkp[0, ]
    }
    if (nchar(cat_filter) > 0)
      results <- results[results$category == cat_filter, ]

    ## Unique variables
    results <- results[!duplicated(results$variable), ]
    results <- head(results, 30)

    if (nrow(results) == 0 && nchar(term) >= 2) {
      ## ---- Live NHANES API fallback -----------------------------------------
      ## Duke CSV only covers harmonized variables. When it returns nothing,
      ## try two nhanesA paths and show results with an "API" badge.

      ## Helper: normalise whatever column names nhanesA returns for a given
      ## column concept (column names differ slightly across nhanesA versions).
      pick_col <- function(df, patterns) {
        m <- grep(paste(patterns, collapse="|"), names(df),
                  ignore.case=TRUE, value=TRUE)
        if (length(m) > 0) df[[m[1]]] else rep(NA_character_, nrow(df))
      }

      nhanes_rows <- tryCatch({
        found <- list()

        ## Path A — exact/prefix variable-code search (e.g. "URXUMA", "LBXGH").
        ## Only makes sense when there are no spaces in the term.
        if (!grepl("\\s", term)) {
          vn_raw <- tryCatch(
            nhanesA::nhanesSearchVarName(toupper(term), includelabels = TRUE),
            ## Some nhanesA builds don't accept includelabels; fall back to
            ## the no-label form (returns table-name vector) so we at least
            ## know the variable exists.
            error = function(e)
              tryCatch(nhanesA::nhanesSearchVarName(toupper(term)),
                       error = function(e2) NULL)
          )

          if (is.data.frame(vn_raw) && nrow(vn_raw) > 0) {
            found[["varname"]] <- data.frame(
              variable    = toupper(pick_col(vn_raw,
                              c("Variable\\.Name","Variable Name","VarName","varname"))),
              description = pick_col(vn_raw,
                              c("Variable\\.Description","Variable Description",
                                "VarDesc","description")),
              category    = pick_col(vn_raw,
                              c("Component","component","category","Data\\.File")),
              stringsAsFactors = FALSE
            )
          } else if (is.character(vn_raw) && length(vn_raw) > 0) {
            ## Only table names returned — the variable exists but we have no
            ## description yet. Show a placeholder; the stage/build step will
            ## pull the real data.
            found[["varname"]] <- data.frame(
              variable    = toupper(term),
              description = "(NHANES variable — description fetched on build)",
              category    = "NHANES",
              stringsAsFactors = FALSE
            )
          }
        }

        ## Path B — keyword / description search (handles partial names and
        ## plain-English queries like "albumin urine").
        ns_raw <- tryCatch(
          nhanesA::nhanesSearch(
            search_terms  = term,
            ystart        = 1999,
            ystop         = 2018,
            ignore.case   = TRUE,
            includelabels = TRUE
          ),
          error = function(e) NULL
        )
        if (is.data.frame(ns_raw) && nrow(ns_raw) > 0) {
          found[["search"]] <- data.frame(
            variable    = toupper(pick_col(ns_raw,
                            c("Variable\\.Name","Variable Name","VarName"))),
            description =        pick_col(ns_raw,
                            c("Variable\\.Description","Variable Description","VarDesc")),
            category    =        pick_col(ns_raw,
                            c("Component","component","Data\\.File\\.Description")),
            stringsAsFactors = FALSE
          )
        }

        ## Combine, deduplicate on variable code, cap at 30 rows
        combined <- do.call(rbind, found)
        if (is.null(combined) || nrow(combined) == 0) return(data.frame())
        combined <- combined[!duplicated(toupper(combined$variable)), ]
        combined <- combined[!is.na(combined$variable) & nzchar(combined$variable), ]
        head(combined, 30)
      }, error = function(e) {
        warning("NHANES API fallback: ", conditionMessage(e))
        data.frame()
      })

      if (!is.null(nhanes_rows) && nrow(nhanes_rows) > 0) {
        in_db <- if (!is.null(rv$db)) tolower(names(rv$db)) else character(0)
        return(tagList(
          div(style="font-size:.78em;color:#6366f1;margin-bottom:8px;
                      padding:6px 10px;background:#f0f4ff;border-radius:4px;
                      border:1px solid #c7d2fe;",
            "⚡ Live NHANES search — not in local catalog"),
          lapply(seq_len(nrow(nhanes_rows)), function(ii) {
            var  <- nhanes_rows$variable[ii]
            desc <- nhanes_rows$description[ii] %||% ""
            cat  <- nhanes_rows$category[ii]    %||% ""
            if (is.na(desc)) desc <- ""
            if (is.na(cat))  cat  <- ""
            already_in_db  <- tolower(var) %in% in_db
            already_staged <- var %in% rv$staged
            div(class="var-row",
              span(class="var-code", var),
              span(class="var-desc", desc),
              if (nchar(cat) > 0) span(class="var-cat", cat),
              if (already_in_db) {
                span(style="color:#15803d;font-size:.78em;", "✓ in dataset")
              } else if (already_staged) {
                span(style="color:#6366f1;font-size:.78em;", "staged")
              } else {
                actionButton(
                  ## Sanitise variable name to produce a valid Shiny input id
                  inputId = paste0("stage_api_",
                                    gsub("[^A-Za-z0-9]", "_", var)),
                  label   = "+ Stage",
                  class   = "btn-sm-teal",
                  onclick = sprintf(
                    "Shiny.setInputValue('stage_var','%s',{priority:'event'})",
                    var)
                )
              }
            )
          })
        ))
      } else {
        return(div(style="color:#888;padding:12px;font-size:.85em;",
          "No variables found in local catalog or NHANES API. ",
          "Check the variable code at ",
          tags$a(href="https://wwwn.cdc.gov/nchs/nhanes/",
                 target="_blank", "CDC NHANES"),
          "."))
      }
    }
    if (nrow(results) == 0) {
      return(div(style="color:#aaa;padding:12px;font-size:.85em;",
                  "Type at least 2 characters to search."))
    }

    ## Check which are already in dataset
    in_db <- if (!is.null(rv$db)) tolower(names(rv$db)) else character(0)

    tagList(lapply(seq_len(nrow(results)), function(i) {
      var  <- results$variable[i]
      desc <- results$description[i]
      cat  <- results$category[i]
      already_in_db <- tolower(var) %in% in_db
      already_staged <- var %in% rv$staged

      cycles_for_var <- sort(unique(lkp[lkp$variable == toupper(var), "cycle"]))
      n_cyc <- length(cycles_for_var)
      cycles_label <- if (n_cyc > 0) {
        paste0(n_cyc, " cycle", if (n_cyc != 1) "s", ": ", paste(cycles_for_var, collapse=", "))
      } else NULL

      div(class="var-row",
        span(class="var-code", var),
        span(class="var-desc", desc),
        span(class="var-cat",  cat),
        if (!is.null(cycles_label))
          span(style="font-size:.73em;color:#64748b;display:block;", cycles_label),
        ## Note for PFOA/PFOS that isomers are summed automatically for 2013+
        if (var %in% c("LBXPFOA","LBXPFOS")) {
          span(style="font-size:.75em;color:#6366f1;",
               "⚡ isomers summed for 2013+")
        },
        if (already_in_db) {
          span(style="color:#15803d;font-size:.78em;", "✓ in dataset")
        } else if (already_staged) {
          span(style="color:#6366f1;font-size:.78em;", "staged")
        } else {
          actionButton(
            inputId = paste0("stage_", var),
            label   = "+ Stage",
            class   = "btn-sm-teal",
            onclick = sprintf(
              "Shiny.setInputValue('stage_var', '%s', {priority: 'event'})", var)
          )
        }
      )
    }))
  })

  ## ---- Handle stage button clicks -----------------------------------------
  ## Dependency map: when staging a derived/dependent variable, auto-stage
  ## its source variables too so they're fetched before computation runs
  variable_dependencies <- list(
    ## Built-in derived variables — source NHANES variables auto-staged
    "hascvd"         = c("MCQ160B","MCQ160C","MCQ160D","MCQ160E","MCQ160F"),
    "oncvdmed"       = "BPQ100D",
    "non_hdl"        = c("LBXTC","LBDHDD"),
    "remnant"        = c("LBXTC","LBDHDD","LBDLDL"),
    "sex_bin"        = "RIAGENDR",
    "race_hispanic"  = "RIDRETH1",
    "race_black"     = "RIDRETH1",
    "race_other"     = "RIDRETH1",
    "smoking_status" = c("SMQ020","SMQ040"),
    "edu_clean"      = "DMDEDUC2",
    "egfr"           = c("LBXSCR","RIDAGEYR","RIAGENDR"),
    "usfli"          = c("RIDRETH1","RIDAGEYR","LBXSGTSI","BMXWAIST","LBXIN","LBXGLU")
  )

  observeEvent(input$stage_var, {
    var <- input$stage_var
    if (is.null(var) || nchar(var) == 0) return()

    db_cols_present <- if (!is.null(rv$db)) tolower(names(rv$db)) else character(0)
    var_lower       <- tolower(var)

    ## Recursively collect all transitive dependencies (handles custom vars
    ## that depend on other custom vars).
    cvars    <- get_custom_variables()
    cv_entry <- cvars[[var_lower]]
    all_deps <- .collect_deps_recursive(var, cvars)

    ## Drop anything already in the database or already staged
    new_deps <- setdiff(
      tolower(all_deps),
      c(db_cols_present, tolower(rv$staged))
    )
    ## Re-apply correct casing from all_deps
    new_deps <- all_deps[tolower(all_deps) %in% new_deps]

    ## For derived variables (built-in or custom with requires), prerequisites
    ## must be processed BEFORE the builder runs — put deps first in the queue.
    is_derived <- var_lower %in% BUILTIN_DERIVED_VARS ||
                  (!is.null(cv_entry) && length(cv_entry$requires) > 0)
    var_entry  <- if (!var_lower %in% tolower(rv$staged)) var else NULL

    to_add <- if (is_derived) {
      unique(c(new_deps, var_entry))   ## deps first, then derived var
    } else {
      unique(c(var_entry, new_deps))
    }

    if (length(to_add) > 0) {
      rv$staged <- c(rv$staged, to_add)
      if (length(new_deps) > 0) {
        showNotification(
          sprintf("Auto-staged dependencies for %s: %s",
                   var, paste(new_deps, collapse=", ")),
          type = "message", duration = 5
        )
      }
    }
  })

  ## ---- Staged variables display -------------------------------------------
  output$staged_vars_ui <- renderUI({
    if (length(rv$staged) == 0) {
      return(div(style="color:#aaa;font-size:.85em;",
                  "No variables staged. Search and add variables above."))
    }
    tagList(
      div(style="margin-bottom:8px;",
        lapply(rv$staged, function(v) {
          span(class="staged-tag",
            v,
            tags$span(
              style="cursor:pointer;color:#6b7280;margin-left:2px;",
              "×",
              onclick=sprintf(
                "Shiny.setInputValue('unstage_var','%s',{priority:'event'})", v)
            )
          )
        })
      )
    )
  })

  observeEvent(input$unstage_var, {
    rv$staged <- setdiff(rv$staged, input$unstage_var)
  })

  ## ---- Custom variable handlers -------------------------------------------
  rv$cv_status  <- NULL  ## status message from last register/delete (list with $success, $message)
  rv$cv_refresh <- 0L   ## increment to trigger cv_list_ui refresh

  ## ---- Default Custom Variable Builder ------------------------------------
  ## connector: AND/OR sitting between row j and row j+1; NA on last row
  rv$dcv_criteria       <- list(list(var="", op="==", val="1", connector=NA))
  rv$dcv_criteria_count <- 1L
  rv$dcv_render_tick    <- 0L  ## incremented on connector toggle to force re-render

  output$dcv_criteria_ui <- renderUI({
    rv$dcv_render_tick   ## reactive dependency for connector toggles
    n <- rv$dcv_criteria_count
    col_choices <- if (!is.null(rv$db_cols)) {
      labels <- ifelse(nzchar(rv$db_cols$description),
                       paste0(rv$db_cols$column, " — ", rv$db_cols$description),
                       rv$db_cols$column)
      setNames(rv$db_cols$column, labels)
    } else character(0)
    op_choices <- c("=="="==","!="="!=",">="=">=","<="="<=",">"=">","<"="<","na_or_ne"="na_or_ne")
    crits <- isolate(rv$dcv_criteria)

    ## Compute bracket role for each condition row.
    ## prev_and[j]: connector arriving FROM row j-1 is AND
    ## next_and[j]: connector leaving  FROM row j   is AND
    prev_and <- logical(n)
    next_and <- logical(n)
    for (j in seq_len(n)) {
      if (j > 1) prev_and[j] <- isTRUE(crits[[j-1]]$connector == "AND")
      if (j < n) next_and[j] <- isTRUE(crits[[j]]$connector   == "AND")
    }
    and_status <- function(j) {
      if (!prev_and[j] && !next_and[j]) "none"
      else if (!prev_and[j] &&  next_and[j]) "start"
      else if ( prev_and[j] &&  next_and[j]) "inner"
      else                                    "end"
    }

    items <- vector("list", 2 * n - 1)

    for (j in seq_len(n)) {
      crit   <- if (j <= length(crits)) crits[[j]] else list(var="", op="==", val="1", connector=NA)
      row_id <- paste0("dcv_", j)

      ## Left gutter for this condition row: bracket segment, no button
      cond_gutter_class <- switch(and_status(j),
        "none"  = "dcv-gutter",
        "start" = "dcv-gutter dcv-gutter-and-start",
        "inner" = "dcv-gutter dcv-gutter-and-inner",
        "end"   = "dcv-gutter dcv-gutter-and-end"
      )

      condition_row <- div(class="dcv-row-wrap",
        div(class=cond_gutter_class),
        div(class="dcv-condition-col",
          div(class="constraint-row",
            selectizeInput(paste0(row_id,"_var"), label=NULL,
              selected=crit$var, choices=col_choices, width="200px",
              options=list(create=FALSE, placeholder="variable...",
                           maxOptions=20, searchField=c("value","label"))),
            selectInput(paste0(row_id,"_op"), label=NULL,
              selected=crit$op, choices=op_choices, width="80px"),
            textInput(paste0(row_id,"_val"), label=NULL,
              value=crit$val, width="80px", placeholder="value"),
            tags$button("×", class="btn-danger-sm",
              onclick=sprintf(
                "Shiny.setInputValue('dcv_remove_row',%d,{priority:'event'})", j))
          )
        )
      )
      items[[2 * j - 1]] <- condition_row

      ## Connector button between row j and row j+1
      if (j < n) {
        conn_label <- if (!is.na(crit$connector)) crit$connector else "OR"
        is_and     <- conn_label == "AND"
        ## AND: right-aligned gutter (close to conditions) + bracket spine
        ## OR:  left-aligned gutter (far from conditions, visually dominant)
        conn_gutter_class <- if (is_and) "dcv-gutter dcv-gutter-and-connector"
                             else        "dcv-gutter dcv-gutter-or"
        connector_row <- div(class="dcv-row-wrap",
          div(class=conn_gutter_class,
            tags$button(conn_label, class="btn-connector",
              onclick=sprintf(
                "Shiny.setInputValue('dcv_toggle_connector',%d,{priority:'event'})", j))
          ),
          div(class="dcv-condition-col")
        )
        items[[2 * j]] <- connector_row
      }
    }

    tagList(items)
  })

  lapply(1:30, function(j) {
    local({
      jj <- j
      observeEvent(input[[paste0("dcv_",jj,"_var")]], {
        isolate({
          if (jj <= length(rv$dcv_criteria))
            rv$dcv_criteria[[jj]]$var <- input[[paste0("dcv_",jj,"_var")]]
        })
      }, ignoreInit=TRUE, ignoreNULL=TRUE)
      observeEvent(input[[paste0("dcv_",jj,"_op")]], {
        isolate({
          if (jj <= length(rv$dcv_criteria))
            rv$dcv_criteria[[jj]]$op <- input[[paste0("dcv_",jj,"_op")]]
        })
      }, ignoreInit=TRUE, ignoreNULL=TRUE)
      observeEvent(input[[paste0("dcv_",jj,"_val")]], {
        isolate({
          if (jj <= length(rv$dcv_criteria))
            rv$dcv_criteria[[jj]]$val <- input[[paste0("dcv_",jj,"_val")]]
        })
      }, ignoreInit=TRUE, ignoreNULL=TRUE)
    })
  })

  observeEvent(input$dcv_toggle_connector, {
    idx <- input$dcv_toggle_connector
    isolate({
      if (idx >= 1 && idx < length(rv$dcv_criteria)) {
        current <- rv$dcv_criteria[[idx]]$connector
        rv$dcv_criteria[[idx]]$connector <- if (isTRUE(current == "AND")) "OR" else "AND"
        rv$dcv_render_tick <- rv$dcv_render_tick + 1L  ## trigger re-render
      }
    })
  })

  observeEvent(input$dcv_add_criterion, {
    isolate({
      n <- length(rv$dcv_criteria)
      ## Give the current last row a default OR connector before appending
      if (n > 0 && is.na(rv$dcv_criteria[[n]]$connector))
        rv$dcv_criteria[[n]]$connector <- "OR"
      rv$dcv_criteria <- c(rv$dcv_criteria, list(list(var="", op="==", val="1", connector=NA)))
      rv$dcv_criteria_count <- length(rv$dcv_criteria)
    })
  })

  observeEvent(input$dcv_remove_row, {
    idx <- input$dcv_remove_row
    isolate({
      if (length(rv$dcv_criteria) > 1) {
        rv$dcv_criteria <- rv$dcv_criteria[-idx]
        ## Ensure the new last row has no connector
        n <- length(rv$dcv_criteria)
        rv$dcv_criteria[[n]]$connector <- NA
        rv$dcv_criteria_count <- n
      }
    })
  })

  observeEvent(input$dcv_register_btn, {
    shinyjs::disable("dcv_register_btn")
    shinyjs::html("dcv_register_btn", "<i class='fa fa-spinner fa-spin'></i> Registering...")
    on.exit({
      shinyjs::enable("dcv_register_btn")
      shinyjs::html("dcv_register_btn", "Register Composite Variable")
    })
    nm  <- tolower(trimws(input$dcv_name))
    lbl <- trimws(input$dcv_label)
    cat <- trimws(input$dcv_category)

    if (!nzchar(nm)) {
      output$dcv_status_ui <- renderUI(
        div(style="color:var(--danger);", "Please enter a variable name."))
      return()
    }

    crits <- lapply(seq_along(rv$dcv_criteria), function(j) {
      var  <- input[[paste0("dcv_",j,"_var")]] %||% rv$dcv_criteria[[j]]$var
      op   <- input[[paste0("dcv_",j,"_op")]]  %||% rv$dcv_criteria[[j]]$op
      val  <- input[[paste0("dcv_",j,"_val")]] %||% rv$dcv_criteria[[j]]$val
      conn <- rv$dcv_criteria[[j]]$connector
      list(var=var, op=op, val=val, connector=conn)
    })
    crits <- Filter(function(c) nzchar(c$var) && nzchar(c$val), crits)

    if (length(crits) == 0) {
      output$dcv_status_ui <- renderUI(
        div(style="color:var(--danger);", "Add at least one criterion."))
      return()
    }

    ## Build per-row condition terms
    make_term <- function(c) {
      if (c$op == "na_or_ne")
        sprintf("(is.na(%s) | %s != %s)", c$var, c$var, c$val)
      else
        sprintf("(!is.na(%s) & %s %s %s)", c$var, c$var, c$op, c$val)
    }

    ## Split into OR-separated groups using each row's connector.
    ## Row j's connector links row j to row j+1; AND keeps them in the same
    ## group, OR (or NA on the last row) starts a new group.
    groups      <- list()
    current_grp <- list(crits[[1]])
    for (j in seq_along(crits)) {
      conn <- crits[[j]]$connector
      if (j < length(crits)) {
        if (isTRUE(conn == "AND")) {
          current_grp <- c(current_grp, list(crits[[j+1]]))
        } else {
          groups      <- c(groups, list(current_grp))
          current_grp <- list(crits[[j+1]])
        }
      }
    }
    groups <- c(groups, list(current_grp))

    ## Each group becomes an AND expression; groups are OR'd together
    group_exprs <- vapply(groups, function(grp) {
      terms <- vapply(grp, make_term, character(1))
      if (length(terms) == 1) terms else sprintf("(%s)", paste(terms, collapse=" & "))
    }, character(1))

    pos_expr <- paste(group_exprs, collapse=" | ")

    ## NA guard: result is NA only when NO OR-group is fully complete.
    ## A group is complete when every variable in it is non-NA for that row.
    ## If at least one group is complete we can evaluate; NAs in other groups
    ## already collapse to FALSE via the !is.na() guards inside make_term().
    group_complete_exprs <- vapply(groups, function(grp) {
      ## na_or_ne is satisfied by NA itself, so exclude those vars from completeness
      grp_vars <- unique(vapply(Filter(function(c) c$op != "na_or_ne", grp),
                                `[[`, character(1), "var"))
      if (length(grp_vars) == 0) return("TRUE")
      terms    <- vapply(grp_vars, function(v) sprintf("!is.na(%s)", v), character(1))
      if (length(terms) == 1) terms
      else sprintf("(%s)", paste(terms, collapse=" & "))
    }, character(1))
    any_complete_expr <- if (length(group_complete_exprs) == 1)
      group_complete_exprs
    else sprintf("(%s)", paste(group_complete_exprs, collapse=" | "))

    formula <- sprintf("ifelse(%s, 1, ifelse(!%s, NA, 0))",
      pos_expr,
      any_complete_expr)

    ## Build label map from true/false text inputs
    true_lbl  <- trimws(input$dcv_true_label  %||% "")
    false_lbl <- trimws(input$dcv_false_label %||% "")
    ## Store as display_label=code to match KNOWN_LABEL_MAPS convention
    label_map <- list()
    label_map[[if (nzchar(true_lbl))  true_lbl  else "Yes"]] <- 1
    label_map[[if (nzchar(false_lbl)) false_lbl else "No"]]  <- 0

    ## Build or_groups: list of char vectors (one per AND-group), used for
    ## two-level weight resolution (most-restrictive within AND, least-restrictive
    ## across OR groups).
    or_groups_vars <- lapply(groups, function(grp) {
      unique(vapply(grp, `[[`, character(1), "var"))
    })

    tryCatch({
      register_custom_variable(nm, formula, label=lbl, category=cat,
                                label_map=label_map, or_groups=or_groups_vars)
      if (!nm %in% rv$staged) rv$staged <- c(rv$staged, nm)
      rv$cv_refresh <- isolate(rv$cv_refresh) + 1L
      output$dcv_status_ui <- renderUI(
        div(style="color:var(--success);",
            sprintf("✓ '%s' registered and staged.", nm)))
    }, error = function(e) {
      output$dcv_status_ui <- renderUI(
        div(style="color:var(--danger);", sprintf("Error: %s", conditionMessage(e))))
    })
  })  ## end observeEvent dcv_register_btn

  ## ---- Custom Variable Bin Builder ----------------------------------------
  ## Each bin: list(lo_op=">=", lo_val="0", hi_op="<", hi_val="6.5", label="0")
  rv$bcv_bins       <- list(
    list(lo_op=">=", lo_val="",    hi_op="<",  hi_val="", bin_label="0"),
    list(lo_op=">=", lo_val="",    hi_op="<",  hi_val="", bin_label="1")
  )
  rv$bcv_bins_count <- 2L

  ## Keep bcv_var choices updated and re-render bins when var changes
  observe({
    req(rv$db_cols)
    choices <- setNames(rv$db_cols$column, rv$db_cols$label)
    updateSelectizeInput(session, "bcv_var", choices=choices, server=TRUE)
  })

  ## Trigger bins UI to re-render when variable changes
  observeEvent(input$bcv_var, {
    rv$bcv_bins_count <- isolate(rv$bcv_bins_count)  ## touch to invalidate
  }, ignoreInit=TRUE, ignoreNULL=TRUE)

  output$bcv_bins_ui <- renderUI({
    n      <- rv$bcv_bins_count
    bins   <- isolate(rv$bcv_bins)
    var    <- input$bcv_var %||% "x"
    var_display <- if (nzchar(var)) var else "x"
    op_choices <- c("<"="<", "<="="<=", ">"=">", ">="=">=", "=="="==")

    tagList(lapply(seq_len(n), function(j) {
      b      <- if (j <= length(bins)) bins[[j]] else
                  list(lo_op=">=", lo_val="", hi_op="<", hi_val="",
                       bin_label=as.character(j-1), bin_text="")
      row_id <- paste0("bcv_", j)
      div(style="display:flex;align-items:center;gap:6px;margin-bottom:8px;",
        ## Bin label badge
        div(style="min-width:44px;text-align:center;font-weight:bold;
                   background:var(--teal);color:#fff;border-radius:3px;
                   padding:4px 6px;font-size:.82em;flex-shrink:0;",
            paste0("Bin ", j-1)),
        ## Lower bound value
        textInput(paste0(row_id,"_lo_val"), label=NULL,
          value=b$lo_val, width="65px", placeholder="lower"),
        ## Lower bound operator
        selectInput(paste0(row_id,"_lo_op"), label=NULL,
          selected=b$lo_op, choices=c("<"="<","<="="<="), width="58px"),
        ## Variable name display
        div(style="font-weight:bold;font-size:.88em;color:var(--teal);
                   padding:0 2px;white-space:nowrap;flex-shrink:0;",
            var_display),
        ## Upper bound operator
        selectInput(paste0(row_id,"_hi_op"), label=NULL,
          selected=b$hi_op, choices=c("<"="<","<="="<="), width="58px"),
        ## Upper bound value
        textInput(paste0(row_id,"_hi_val"), label=NULL,
          value=b$hi_val, width="65px", placeholder="upper"),
        ## Label text — shown in demographics table
        textInput(paste0(row_id,"_text"), label=NULL,
          value=b$bin_text %||% "", width="90px", placeholder="label"),
        ## Remove button
        tags$button("×", class="btn-danger-sm",
          onclick=sprintf(
            "Shiny.setInputValue('bcv_remove_bin',%d,{priority:'event'})", j))
      )
    }))
  })

  ## Save bin field changes without triggering re-render
  lapply(1:20, function(j) {
    local({
      jj <- j
      for (field in c("lo_op","lo_val","hi_op","hi_val","text")) {
        local({
          ff <- field
          observeEvent(input[[paste0("bcv_",jj,"_",ff)]], {
            isolate({
              if (jj <= length(rv$bcv_bins)) {
                current <- rv$bcv_bins[[jj]]
                if (!is.list(current)) current <- list(lo_op="<",lo_val="",hi_op="<",hi_val="",bin_label=as.character(jj-1),bin_text="")
                current[[if(ff=="text") "bin_text" else ff]] <- input[[paste0("bcv_",jj,"_",ff)]]
                rv$bcv_bins[[jj]] <- current
              }
            })
          }, ignoreInit=TRUE, ignoreNULL=TRUE)
        })
      }
    })
  })

  observeEvent(input$bcv_add_bin, {
    isolate({
      n <- length(rv$bcv_bins)
      rv$bcv_bins <- c(rv$bcv_bins,
        list(list(lo_op="<", lo_val="", hi_op="<", hi_val="",
                  bin_label=as.character(n), bin_text="")))
      rv$bcv_bins_count <- length(rv$bcv_bins)
    })
  })

  observeEvent(input$bcv_remove_bin, {
    idx <- input$bcv_remove_bin
    isolate({
      if (length(rv$bcv_bins) > 1) {
        rv$bcv_bins <- rv$bcv_bins[-idx]
        rv$bcv_bins_count <- length(rv$bcv_bins)
      }
    })
  })

  observeEvent(input$bcv_register_btn, {
    shinyjs::disable("bcv_register_btn")
    shinyjs::html("bcv_register_btn", "<i class='fa fa-spinner fa-spin'></i> Registering...")
    on.exit({
      shinyjs::enable("bcv_register_btn")
      shinyjs::html("bcv_register_btn", "Register Bin Variable")
    })
    nm  <- tolower(trimws(input$bcv_name))
    lbl <- trimws(input$bcv_label)
    cat <- trimws(input$bcv_category)
    var <- tolower(trimws(input$bcv_var))

    if (!nzchar(nm) || !nzchar(var)) {
      output$bcv_status_ui <- renderUI(
        div(style="color:var(--danger);", "Please enter a variable name and select a source variable."))
      return()
    }

    ## Read current bin state safely — always build proper named lists
    n_bins <- isolate(rv$bcv_bins_count)
    bins <- lapply(seq_len(n_bins), function(j) {
      stored <- isolate(rv$bcv_bins[[j]])
      get_field <- function(field, default="") {
        v <- input[[paste0("bcv_",j,"_",field)]]
        if (!is.null(v) && length(v) == 1) v
        else if (is.list(stored) && !is.null(stored[[field]])) stored[[field]]
        else default
      }
      list(
        lo_op     = get_field("lo_op",  "<"),
        lo_val    = get_field("lo_val", ""),
        hi_op     = get_field("hi_op",  "<"),
        hi_val    = get_field("hi_val", ""),
        bin_label = as.character(j - 1),
        bin_text  = get_field("text",   "")
      )
    })

    ## Build nested ifelse formula from bins
    ## Each bin: (lo_op lo_val & hi_op hi_val) → bin_label
    ## NA-safe: if source variable is NA → NA
    make_bin_condition <- function(b, var) {
      has_lo <- nzchar(b$lo_val)
      has_hi <- nzchar(b$hi_val)
      if (!has_lo && !has_hi) return(NULL)
      parts <- character(0)
      ## lo_val < var means: var > lo_val, rewrite as var op lo_val
      if (has_lo) {
        lo_op_flipped <- switch(b$lo_op, "<"=">", "<="=">=", b$lo_op)
        parts <- c(parts, sprintf("(!is.na(%s) & %s %s %s)", var, var, lo_op_flipped, b$lo_val))
      }
      if (has_hi) {
        parts <- c(parts, sprintf("(!is.na(%s) & %s %s %s)", var, var, b$hi_op, b$hi_val))
      }
      paste(parts, collapse=" & ")
    }

    ## Build nested ifelse chain from last bin to first
    valid_bins <- Filter(function(b) nzchar(b$lo_val) || nzchar(b$hi_val), bins)
    if (length(valid_bins) == 0) {
      output$bcv_status_ui <- renderUI(
        div(style="color:var(--danger);", "Please define at least one bin with a value."))
      return()
    }

    ## Build from inside out: ifelse(cond_n, n, NA)
    formula <- sprintf("ifelse(!is.na(%s), NA_real_, NA_real_)", var)  ## base: NA
    ## Override: start from last bin and wrap outward
    expr <- "NA_real_"
    for (b in rev(valid_bins)) {
      cond <- make_bin_condition(b, var)
      if (!is.null(cond))
        expr <- sprintf("ifelse(%s, %s, %s)", cond, b$bin_label, expr)
    }
    ## Wrap in outer NA check for missing source variable
    formula <- sprintf("ifelse(is.na(%s), NA_real_, %s)", var, expr)

    ## Build label map: display_label -> code (matching KNOWN_LABEL_MAPS convention)
    label_map <- setNames(
      as.list(as.numeric(vapply(valid_bins, `[[`, character(1), "bin_label"))),
      vapply(valid_bins, function(b) {
        txt <- trimws(b$bin_text %||% "")
        if (nzchar(txt)) txt else paste0("Bin ", b$bin_label)
      }, character(1))
    )

    tryCatch({
      register_custom_variable(nm, formula, label=lbl, category=cat,
                                label_map=label_map)
      if (!nm %in% rv$staged) rv$staged <- c(rv$staged, nm)
      rv$cv_refresh <- isolate(rv$cv_refresh) + 1L
      output$bcv_status_ui <- renderUI(
        div(style="color:var(--success);",
            sprintf("✓ '%s' registered and staged.", nm)))
    }, error = function(e) {
      output$bcv_status_ui <- renderUI(
        div(style="color:var(--danger);", sprintf("Error: %s", conditionMessage(e))))
    })
  })  ## end observeEvent bcv_register_btn

  ## ---- Advanced Bin Variable Builder --------------------------------------
  ## Each bin: list(var, op, val, bin_label, bin_text)
  rv$abv_bins       <- list(
    list(var="", op=">=", val="", bin_label="1", bin_text="")
  )
  rv$abv_bins_count <- 1L

  ## Bin 0 (default) criteria — optional; if set, participants failing it get NA
  rv$abv_bin0 <- list(var="", op=">=", val="", bin_text="")

  output$abv_bin0_ui <- renderUI({
    col_choices <- if (!is.null(rv$db_cols))
                     setNames(rv$db_cols$column, rv$db_cols$label)
                   else character(0)
    op_choices  <- c("==" = "==", "!=" = "!=", ">" = ">", "<" = "<",
                     ">=" = ">=", "<=" = "<=", "NA or ≠" = "na_or_ne")
    b <- isolate(rv$abv_bin0)
    tagList(
      div(style="font-size:.8em;font-weight:600;color:var(--text-muted);
                 margin-bottom:4px;margin-top:4px;",
          "Bin 0 — Default (leave variable blank to allow everyone not in a higher bin)"),
      div(style="display:flex;align-items:center;gap:6px;margin-bottom:8px;flex-wrap:wrap;",
        div(style="min-width:44px;text-align:center;font-weight:bold;
                   background:#6b7280;color:#fff;border-radius:3px;
                   padding:4px 6px;font-size:.82em;flex-shrink:0;",
            "Bin 0"),
        selectizeInput("abv_bin0_var", label=NULL,
          choices=col_choices, selected=b$var,
          options=list(create=FALSE, placeholder="variable (optional)...",
                       maxOptions=20, searchField=c("value","label")),
          width="180px"),
        selectInput("abv_bin0_op", label=NULL,
          selected=b$op, choices=op_choices, width="105px"),
        textInput("abv_bin0_val", label=NULL,
          value=b$val, width="80px", placeholder="value"),
        textInput("abv_bin0_text", label=NULL,
          value=b$bin_text %||% "", width="100px", placeholder="label")
      ),
      hr(style="margin:6px 0 10px 0;")
    )
  })

  ## Persist bin0 inputs to rv$abv_bin0
  for (ff in c("var", "op", "val", "text")) {
    local({
      field <- ff
      input_id <- paste0("abv_bin0_", field)
      observeEvent(input[[input_id]], {
        isolate({
          current <- rv$abv_bin0
          current[[if (field == "text") "bin_text" else field]] <- input[[input_id]]
          rv$abv_bin0 <- current
        })
      }, ignoreInit=TRUE, ignoreNULL=TRUE)
    })
  }

  output$abv_bins_ui <- renderUI({
    n           <- rv$abv_bins_count
    bins        <- isolate(rv$abv_bins)
    col_choices <- if (!is.null(rv$db_cols))
                     setNames(rv$db_cols$column, rv$db_cols$label)
                   else character(0)
    op_choices  <- c("==" = "==", "!=" = "!=", ">" = ">", "<" = "<",
                     ">=" = ">=", "<=" = "<=", "NA or ≠" = "na_or_ne")

    tagList(lapply(seq_len(n), function(j) {
      b      <- if (j <= length(bins)) bins[[j]] else
                  list(var="", op=">=", val="", bin_label=as.character(j), bin_text="")
      row_id <- paste0("abv_", j)
      div(style="display:flex;align-items:center;gap:6px;margin-bottom:8px;flex-wrap:wrap;",
        div(style="min-width:44px;text-align:center;font-weight:bold;
                   background:var(--teal);color:#fff;border-radius:3px;
                   padding:4px 6px;font-size:.82em;flex-shrink:0;",
            paste0("Bin ", j)),
        selectizeInput(paste0(row_id,"_var"), label=NULL,
          choices=col_choices, selected=b$var,
          options=list(create=FALSE, placeholder="variable...",
                       maxOptions=20, searchField=c("value","label")),
          width="180px"),
        selectInput(paste0(row_id,"_op"), label=NULL,
          selected=b$op, choices=op_choices, width="105px"),
        textInput(paste0(row_id,"_val"), label=NULL,
          value=b$val, width="80px", placeholder="value"),
        textInput(paste0(row_id,"_text"), label=NULL,
          value=b$bin_text %||% "", width="100px", placeholder="label"),
        tags$button("×", class="btn-danger-sm",
          onclick=sprintf(
            "Shiny.setInputValue('abv_remove_bin',%d,{priority:'event'})", j))
      )
    }))
  })

  lapply(1:20, function(j) {
    local({
      jj <- j
      for (field in c("var", "op", "val", "text")) {
        local({
          ff <- field
          observeEvent(input[[paste0("abv_",jj,"_",ff)]], {
            isolate({
              if (jj <= length(rv$abv_bins)) {
                current <- rv$abv_bins[[jj]]
                if (!is.list(current))
                  current <- list(var="", op=">=", val="",
                                  bin_label=as.character(jj), bin_text="")
                current[[if(ff=="text") "bin_text" else ff]] <-
                  input[[paste0("abv_",jj,"_",ff)]]
                rv$abv_bins[[jj]] <- current
              }
            })
          }, ignoreInit=TRUE, ignoreNULL=TRUE)
        })
      }
    })
  })

  observeEvent(input$abv_add_bin, {
    isolate({
      n <- length(rv$abv_bins)
      rv$abv_bins <- c(rv$abv_bins,
        list(list(var="", op=">=", val="",
                  bin_label=as.character(n + 1L), bin_text="")))
      rv$abv_bins_count <- length(rv$abv_bins)
    })
  })

  observeEvent(input$abv_remove_bin, {
    idx <- input$abv_remove_bin
    isolate({
      if (length(rv$abv_bins) > 1) {
        rv$abv_bins <- rv$abv_bins[-idx]
        rv$abv_bins <- lapply(seq_along(rv$abv_bins), function(k) {
          b <- rv$abv_bins[[k]]
          b$bin_label <- as.character(k)
          b
        })
        rv$abv_bins_count <- length(rv$abv_bins)
      }
    })
  })

  observeEvent(input$abv_register_btn, {
    shinyjs::disable("abv_register_btn")
    shinyjs::html("abv_register_btn", "<i class='fa fa-spinner fa-spin'></i> Registering...")
    on.exit({
      shinyjs::enable("abv_register_btn")
      shinyjs::html("abv_register_btn", "Register Advanced Bin Variable")
    })
    nm  <- tolower(trimws(input$abv_name))
    lbl <- trimws(input$abv_label)
    cat <- trimws(input$abv_category)

    if (!nzchar(nm)) {
      output$abv_status_ui <- renderUI(
        div(style="color:var(--danger);", "Please enter a variable name."))
      return()
    }

    n_bins <- isolate(rv$abv_bins_count)
    bins <- lapply(seq_len(n_bins), function(j) {
      stored <- isolate(rv$abv_bins[[j]])
      get_field <- function(field, default="") {
        v <- input[[paste0("abv_",j,"_",field)]]
        if (!is.null(v) && length(v) == 1) v
        else if (is.list(stored) && !is.null(stored[[field]])) stored[[field]]
        else default
      }
      list(
        var       = tolower(trimws(get_field("var",  ""))),
        op        = get_field("op",  ">="),
        val       = trimws(get_field("val", "")),
        bin_label = as.character(j),
        bin_text  = get_field("text", "")
      )
    })

    valid_bins <- Filter(function(b) nzchar(b$var) && nzchar(b$val), bins)
    if (length(valid_bins) == 0) {
      output$abv_status_ui <- renderUI(
        div(style="color:var(--danger);",
            "Please define at least one bin with a variable and value."))
      return()
    }

    make_cond <- function(b) {
      if (b$op == "na_or_ne") {
        sprintf("(is.na(%s) | %s != %s)", b$var, b$var, b$val)
      } else {
        sprintf("(!is.na(%s) & %s %s %s)", b$var, b$var, b$op, b$val)
      }
    }

    ## Bin 0 condition — read from inputs or stored reactive
    bin0_stored <- isolate(rv$abv_bin0)
    get_bin0 <- function(field, default="") {
      v <- input[[paste0("abv_bin0_", field)]]
      if (!is.null(v) && length(v) == 1) v
      else if (is.list(bin0_stored) && !is.null(bin0_stored[[field]])) bin0_stored[[field]]
      else default
    }
    bin0 <- list(
      var      = tolower(trimws(get_bin0("var", ""))),
      op       = get_bin0("op", ">="),
      val      = trimws(get_bin0("val", "")),
      bin_text = get_bin0("text", "")
    )

    ## Build nested ifelse, highest bin outermost so it wins when true.
    ## Use plain integers (no L suffix) — the L suffix tokenises as a bare
    ## identifier after tolower() and confuses parse_custom_formula.
    ## If bin0 has a condition, participants who fail it get NA (excluded).
    has_bin0_cond <- nzchar(bin0$var) && nzchar(bin0$val)
    if (has_bin0_cond) {
      bin0_cond <- make_cond(bin0)
      expr <- sprintf("ifelse(%s, 0, NA_real_)", bin0_cond)
    } else {
      expr <- "0"
    }
    for (b in rev(valid_bins)) {
      cond <- make_cond(b)
      expr <- sprintf("ifelse(%s, %s, %s)", cond, b$bin_label, expr)
    }

    bin0_text <- trimws(bin0$bin_text %||% "")
    bin0_name <- if (nzchar(bin0_text)) bin0_text else "None"
    label_map <- setNames(
      as.list(c(0, as.numeric(vapply(valid_bins, `[[`, character(1), "bin_label")))),
      c(bin0_name, vapply(valid_bins, function(b) {
        txt <- trimws(b$bin_text %||% "")
        if (nzchar(txt)) txt else paste0("Bin ", b$bin_label)
      }, character(1)))
    )

    result <- register_custom_variable(nm, expr, label=lbl, category=cat,
                                        label_map=label_map)
    if (!isTRUE(result$success)) {
      output$abv_status_ui <- renderUI(
        div(style="color:var(--danger);",
            result$message %||% "Registration failed."))
      return()
    }

    if (!nm %in% rv$staged) rv$staged <- c(rv$staged, nm)
    rv$cv_refresh <- isolate(rv$cv_refresh) + 1L

    ## Add to db_cols so the variable appears in autocomplete dropdowns
    if (!is.null(rv$db_cols) && !nm %in% rv$db_cols$column) {
      rv$db_cols <- rbind(rv$db_cols, data.frame(
        column      = nm,
        label       = sprintf("%s — %s (custom)", nm, lbl),
        description = lbl,
        category    = if (nzchar(cat)) cat else "Custom",
        num_cycles  = NA_integer_,
        stringsAsFactors = FALSE
      ))
      update_autocomplete()
    }

    output$abv_status_ui <- renderUI(
      div(style="color:var(--success);",
          sprintf("✓ '%s' registered and staged.", nm)))
  })  ## end observeEvent abv_register_btn

  ## ---- Advanced Custom Variable Builder (Register) ------------------------
  observeEvent(input$cv_register_btn, {
    shinyjs::disable("cv_register_btn")
    shinyjs::html("cv_register_btn", "<i class='fa fa-spinner fa-spin'></i> Registering...")
    on.exit({
      shinyjs::enable("cv_register_btn")
      shinyjs::html("cv_register_btn", "Register Custom Variable")
    })
    result <- register_custom_variable(
      name        = input$cv_name,
      label       = input$cv_label,
      category    = input$cv_category,
      formula_str = input$cv_formula
    )
    ## Warn if referenced columns are not in the loaded dataset
    if (result$success && !is.null(rv$db_cols)) {
      known_cols  <- c(rv$db_cols$column,
                       names(get_custom_variables()),
                       names(derived_outcomes_registry))
      parsed_refs <- tryCatch(
        parse_custom_formula(input$cv_formula)$requires,
        error = function(e) character(0)
      )
      unknown <- setdiff(tolower(parsed_refs), tolower(known_cols))
      if (length(unknown) > 0) {
        result$message <- paste0(
          result$message,
          sprintf(" (warning: column(s) not found in database: %s — will be NA at compute time)",
                  paste(unknown, collapse=", "))
        )
        result$success <- "warn"
      }
    }
    rv$cv_status  <- result
    rv$cv_refresh <- isolate(rv$cv_refresh) + 1L
    if (isTRUE(result$success) || identical(result$success, "warn")) {
      ## Auto-stage the new variable
      nm_clean <- tolower(gsub("[^A-Za-z0-9_]","_", input$cv_name))
      if (!nm_clean %in% rv$staged) rv$staged <- c(rv$staged, nm_clean)

      ## Update the db_cols metadata so the new variable appears in autocomplete
      if (!is.null(rv$db_cols)) {
        new_row <- data.frame(
          column      = nm_clean,
          label       = sprintf("%s — %s (custom)", nm_clean, input$cv_label),
          description = input$cv_label,
          category    = if (nchar(trimws(input$cv_category))>0) input$cv_category else "Custom",
          num_cycles  = NA_integer_,
          stringsAsFactors = FALSE
        )
        rv$db_cols <- rbind(rv$db_cols, new_row)
        update_autocomplete()
      }
      ## Clear inputs
      updateTextInput(session, "cv_name",    value="")
      updateTextInput(session, "cv_label",   value="")
      updateTextInput(session, "cv_category",value="")
      updateTextAreaInput(session, "cv_formula", value="")
    }
  })

  ## ---- Merge Variable Builder -----------------------------------------------
  ## Keep mv_var_left / mv_var_right choices in sync with available db columns
  observe({
    choices <- if (!is.null(rv$db_cols)) {
      setNames(rv$db_cols$column, rv$db_cols$label)
    } else character(0)
    updateSelectizeInput(session, "mv_var_left",  choices=choices, server=TRUE)
    updateSelectizeInput(session, "mv_var_right", choices=choices, server=TRUE)
  })

  observeEvent(input$mv_register_btn, {
    shinyjs::disable("mv_register_btn")
    shinyjs::html("mv_register_btn", "<i class='fa fa-spinner fa-spin'></i> Registering...")
    on.exit({
      shinyjs::enable("mv_register_btn")
      shinyjs::html("mv_register_btn", "Register Merge Variable")
    })
    nm    <- tolower(trimws(input$mv_name))
    lbl   <- trimws(input$mv_label)
    cat   <- trimws(input$mv_category)
    vl    <- tolower(trimws(input$mv_var_left  %||% ""))
    vr    <- tolower(trimws(input$mv_var_right %||% ""))

    result <- register_merge_variable(nm, lbl, cat, vl, vr)
    rv$cv_refresh <- isolate(rv$cv_refresh) + 1L

    if (result$success) {
      if (!nm %in% rv$staged) rv$staged <- c(rv$staged, nm)
      if (!is.null(rv$db_cols)) {
        new_row <- data.frame(
          column      = nm,
          label       = sprintf("%s — %s (merge)", nm, lbl),
          description = lbl,
          category    = if (nchar(cat) > 0) cat else "Custom",
          num_cycles  = NA_integer_,
          stringsAsFactors = FALSE
        )
        rv$db_cols <- rbind(rv$db_cols, new_row)
        update_autocomplete()
      }
      updateTextInput(session, "mv_name",    value="")
      updateTextInput(session, "mv_label",   value="")
      updateTextInput(session, "mv_category",value="")
    }

    output$mv_status_ui <- renderUI({
      if (result$success) {
        div(class="status-done", style="padding:6px 10px;font-size:.82em;margin-bottom:6px;",
            "✓ ", result$message)
      } else {
        div(class="status-error", style="padding:6px 10px;font-size:.82em;margin-bottom:6px;",
            "✗ ", result$message)
      }
    })
  })

  ## Status message for custom variable registration
  output$cv_status_ui <- renderUI({
    s <- rv$cv_status
    if (is.null(s)) return(NULL)
    if (isTRUE(s$success)) {
      div(class="status-done", style="padding:6px 10px;font-size:.82em;margin-bottom:6px;",
          "✓ ", s$message)
    } else if (identical(s$success, "warn")) {
      div(class="status-warn",
          style="padding:6px 10px;font-size:.82em;margin-bottom:6px;color:var(--warning,#b45309);",
          "⚠ ", s$message)
    } else {
      div(class="status-error", style="padding:6px 10px;font-size:.82em;margin-bottom:6px;",
          "✗ ", s$message)
    }
  })

  ## List of registered custom variables with delete buttons
  output$cv_list_ui <- renderUI({
    rv$cv_refresh  ## trigger on any registration or deletion
    rv$db_cols     ## trigger when dataset is built/updated
    cvars <- get_custom_variables()

    ## Built-in derived variables — must be staged like user custom variables
    builtin <- list(
      list(name="sex_bin",       label="Sex binary (0=male, 1=female)",               category="Demographics",       formula="sex - 1"),
      list(name="race_hispanic", label="Hispanic race dummy (0/1)",                   category="Demographics",       formula="race == 1 | race == 2"),
      list(name="race_black",    label="Non-Hispanic Black dummy (0/1)",              category="Demographics",       formula="race == 4"),
      list(name="race_other",    label="Other/multiracial race dummy (0/1)",          category="Demographics",       formula="race == 5"),
      list(name="smoking_status",label="Smoking status (0=never,1=former,2=current)",category="Smoking",            formula="smq020 + smq040 logic"),
      list(name="edu_clean",     label="Education (7/9 recoded to NA)",               category="Demographics",       formula="ifelse(education > 5, NA, education)"),
      list(name="hascvd",        label="CVD composite flag (0/1)",                    category="Medical Conditions", formula="any MCQ160B-F == 1"),
      list(name="oncvdmed",      label="On CVD medication (0/1)",                     category="Medical Conditions", formula="BPQ100D == 1"),
      list(name="non_hdl",       label="Non-HDL cholesterol (mg/dL)",                 category="Cholesterol",        formula="lbxtc - lbdhdd"),
      list(name="remnant",       label="Remnant cholesterol (mg/dL)",                 category="Cholesterol",        formula="non_hdl - lbdldl"),
      list(name="egfr",          label="eGFR — CKD-EPI 2009 (mL/min/1.73m²)",       category="Kidney",             formula="CKD-EPI 2009 + Selvin recalibration"),
      list(name="usfli",         label="US Fatty Liver Index (USFLI, 0–100)",         category="Liver",              formula="logistic(race + age + GGT + WC + insulin + glucose)")
    )

    in_db_cols     <- if (!is.null(rv$db_cols)) rv$db_cols$column else character(0)
    staged_lower   <- tolower(rv$staged)

    builtin_ui <- tagList(
      div(class="section-label", style="margin-top:10px;margin-bottom:6px;",
          "Built-in derived variables"),
      lapply(builtin, function(v) {
        in_db          <- v$name %in% in_db_cols
        already_staged <- v$name %in% staged_lower
        div(class="var-row",
          span(class="var-code", v$name),
          span(class="var-desc", v$label),
          span(class="var-cat",  v$category),
          span(style="font-size:.75em;color:#888;font-family:monospace;flex:1;",
               v$formula),
          if (in_db)
            span(style="color:#15803d;font-size:.78em;", "✓ in dataset")
          else if (already_staged)
            span(style="color:#6366f1;font-size:.78em;", "staged")
          else
            actionButton(
              inputId = paste0("stage_builtin_", v$name),
              label   = "+ Stage",
              class   = "btn-sm-teal",
              onclick = sprintf(
                "Shiny.setInputValue('stage_var','%s',{priority:'event'})", v$name)
            )
        )
      })
    )

    if (length(cvars) == 0) {
      tagList(
        builtin_ui,
        div(class="section-label", style="margin-top:10px;", "User-defined custom variables"),
        div(style="color:#aaa;font-size:.82em;", "No custom variables defined yet.")
      )
    } else {
      tagList(
        builtin_ui,
        div(class="section-label", style="margin-top:10px;margin-bottom:6px;",
            "User-defined custom variables"),
        lapply(names(cvars), function(nm) {
          entry      <- cvars[[nm]]
          in_db      <- !is.null(rv$db_cols) && nm %in% rv$db_cols$column
          formula_display <- if (isTRUE(entry$type == "merge")) {
            sprintf("merge: %s ← %s (fallback: %s)",
                    nm, entry$merge_left, entry$merge_right)
          } else {
            sprintf("%s  [%s]", entry$formula %||% nm,
                    if(isTRUE(entry$is_zscore)) "zscore at analysis time" else "computed")
          }
          div(class="var-row",
            span(class="var-code", nm),
            span(class="var-cat", entry$category),
            tags$button("View details",
              class = "btn-sm-teal",
              style = "font-size:.72em;padding:2px 7px;",
              onclick = sprintf(
                "Shiny.setInputValue('view_cv','%s',{priority:'event'})", nm)),
            if (in_db)
              span(style="color:#15803d;font-size:.78em;", "✓ in dataset")
            else
              actionButton(
                inputId = paste0("stage_cv_", nm),
                label   = "+ Stage",
                class   = "btn-sm-teal",
                onclick = sprintf(
                  "Shiny.setInputValue('stage_var','%s',{priority:'event'})", nm)
              ),
            tags$button("×", class="btn-danger-sm",
              onclick=sprintf(
                "Shiny.setInputValue('delete_cv','%s',{priority:'event'})", nm))
          )
        })
      )
    }
  })

  ## Delete a custom variable
  observeEvent(input$delete_cv, {
    nm <- input$delete_cv
    result <- delete_custom_variable(nm)
    rv$cv_status  <- result
    rv$cv_refresh <- isolate(rv$cv_refresh) + 1L
    if (!is.null(rv$db_cols)) {
      rv$db_cols <- rv$db_cols[rv$db_cols$column != nm, ]
      update_autocomplete()
    }
  })

  ## ---- Custom Variable Detail Panel ----------------------------------------
  rv$cv_detail_var <- NULL

  observeEvent(input$view_cv, {
    nm <- input$view_cv
    rv$cv_detail_var <- if (nzchar(nm)) nm else NULL
  })

  ## Clear detail when the variable is deleted
  observeEvent(input$delete_cv, {
    if (identical(rv$cv_detail_var, input$delete_cv))
      rv$cv_detail_var <- NULL
  }, priority = -1)

  output$cv_detail_ui <- renderUI({
    nm <- rv$cv_detail_var
    if (is.null(nm) || !nzchar(nm)) return(NULL)
    cvars <- get_custom_variables()
    entry <- cvars[[nm]]
    if (is.null(entry)) return(NULL)

    op_sym <- c("=="="=", "!="="≠", ">="="≥", "<="="≤", ">"=">", "<"="<",
                "na_or_ne"="NA or ≠")
    bin_colors <- c("#0d9488","#6366f1","#f59e0b","#ef4444","#8b5cf6",
                    "#0ea5e9","#84cc16","#f97316")

    ## ---- helpers to render criteria/bins into the builder visual --------

    render_criteria <- function(crits, label_map) {
      ## Collapse into OR-separated groups of AND-connected criteria
      groups <- list()
      i <- 1L
      while (i <= length(crits)) {
        grp <- list(crits[[i]])
        while (isTRUE(crits[[i]]$connector == "AND")) {
          i <- i + 1L
          grp <- c(grp, list(crits[[i]]))
        }
        groups <- c(groups, list(grp))
        i <- i + 1L
      }

      ## Compact row: variable  op  value — items sit close together
      crit_row <- function(cr) {
        op <- op_sym[cr$op] %||% cr$op
        div(style="display:flex;align-items:center;gap:8px;background:#f9fafb;border:1px solid #e5e7eb;border-radius:5px;padding:5px 10px;font-size:.82em;margin-bottom:3px;",
          span(style="font-weight:600;color:#111827;", cr$var),
          span(style="color:#6366f1;font-weight:700;", op),
          span(style="font-family:monospace;color:#374151;background:#fff;border:1px solid #e5e7eb;border-radius:3px;padding:2px 5px;", cr$val)
        )
      }

      or_badge  <- div(style="font-size:.7em;font-weight:700;color:#6b7280;padding:2px 8px;border:1px solid #d1d5db;border-radius:3px;background:#f9fafb;display:inline-block;margin:4px 0;", "OR")
      and_badge <- div(style="font-size:.7em;font-weight:700;color:#0d9488;padding:2px 8px;border:1.5px solid #0d9488;border-radius:3px;background:#f0fdfa;display:inline-block;margin:4px 0;", "AND")

      rows <- lapply(seq_along(groups), function(gi) {
        grp     <- groups[[gi]]
        is_last <- gi == length(groups)

        group_ui <- if (length(grp) == 1L) {
          ## Single criterion — no bracket
          crit_row(grp[[1]])
        } else {
          ## AND group: three-segment bracket column beside content column.
          ## bracket column = top-corner (fixed h) + middle (flex:1) + bottom-corner (fixed h)
          ## All three are in one flex column — physically impossible to have gaps.
          inner_items <- vector("list", 2L * length(grp) - 1L)
          for (ai in seq_along(grp)) {
            inner_items[[2L*ai-1L]] <- crit_row(grp[[ai]])
            if (ai < length(grp)) inner_items[[2L*ai]] <- and_badge
          }
          div(style="display:flex;align-items:stretch;margin-bottom:3px;",
            div(style="display:flex;flex-direction:column;width:14px;flex-shrink:0;margin-left:4px;margin-right:8px;",
              div(style="height:14px;border-left:2px solid #0d9488;border-top:2px solid #0d9488;border-radius:2px 0 0 0;"),
              div(style="flex:1;border-left:2px solid #0d9488;"),
              div(style="height:14px;border-left:2px solid #0d9488;border-bottom:2px solid #0d9488;border-radius:0 0 0 2px;")
            ),
            div(style="flex:1;", tagList(inner_items))
          )
        }

        tagList(group_ui, if (!is_last) or_badge)
      })

      lbl_row <- if (!is.null(label_map) && length(label_map) > 0) {
        div(style="margin-top:12px;padding-top:8px;border-top:1px solid #e5e7eb;",
          lapply(names(label_map), function(lbl) {
            cv <- label_map[[lbl]]
            div(style="display:flex;align-items:center;gap:6px;margin-bottom:3px;font-size:.8em;",
              div(style=sprintf("background:%s;color:white;font-weight:700;font-size:.75em;padding:1px 8px;border-radius:3px;min-width:18px;text-align:center;",
                    if (isTRUE(cv == 1)) "#0d9488" else "#6b7280"),
                  as.character(cv)),
              span(style="color:#374151;", lbl))
          })
        )
      } else NULL
      tagList(tagList(rows), lbl_row)
    }

    render_bins_standard <- function(bins, var_nm, label_map) {
      valid <- Filter(function(b) nzchar(b$lo_val %||% "") || nzchar(b$hi_val %||% ""), bins)
      ## Build label lookup: bin_label -> display text
      lbl_lookup <- if (!is.null(label_map) && length(label_map) > 0) {
        setNames(names(label_map), as.character(unlist(label_map)))
      } else character(0)
      tagList(lapply(seq_along(valid), function(i) {
        b      <- valid[[i]]
        lo_val <- b$lo_val %||% ""; hi_val <- b$hi_val %||% ""
        lo_op  <- switch(b$lo_op %||% ">=", "<"=">", "<="=">=", b$lo_op %||% ">=")
        hi_op  <- b$hi_op %||% "<"
        lbl    <- lbl_lookup[as.character(b$bin_label)] %||% paste0("Bin ", b$bin_label)
        col    <- bin_colors[(i-1) %% length(bin_colors) + 1]
        div(style="display:flex;align-items:center;gap:8px;margin-bottom:6px;",
          div(style=sprintf("background:%s;color:white;font-size:.72em;font-weight:700;padding:3px 8px;border-radius:4px;min-width:42px;text-align:center;", col),
              lbl),
          div(style="flex:1;background:#f9fafb;border:1px solid #e5e7eb;border-radius:5px;padding:5px 10px;font-size:.82em;display:flex;align-items:center;gap:4px;flex-wrap:wrap;",
            if (nzchar(lo_val)) span(style="font-family:monospace;color:#374151;", lo_val),
            if (nzchar(lo_val)) span(style="color:#6366f1;font-weight:700;padding:0 2px;", op_sym[lo_op] %||% lo_op),
            span(style="font-weight:600;color:#0d9488;", var_nm),
            if (nzchar(hi_val)) span(style="color:#6366f1;font-weight:700;padding:0 2px;", op_sym[hi_op] %||% hi_op),
            if (nzchar(hi_val)) span(style="font-family:monospace;color:#374151;", hi_val)
          )
        )
      }))
    }

    ## ---- Parse composite flag formula ------------------------------------
    ## Formula structure produced by the default builder:
    ##   ifelse(POS_EXPR, 1, ifelse(!COMPLETE_EXPR, NA, 0))
    ##
    ## POS_EXPR is top-level OR-separated items where each item is either:
    ##   Single criterion : (!is.na(VAR) & VAR OP VAL)   -- outer parens from make_term
    ##                    : (is.na(VAR) | VAR != VAL)     -- na_or_ne variant
    ##   AND group        : (TERM1 & TERM2 & ...)         -- extra outer parens from builder
    ##
    ## After stripping one layer of outer parens from an OR-item:
    ##   starts with "!is.na" or "is.na" → single criterion
    ##   starts with "("                  → AND group; split on " & " at top level
    parse_composite_formula <- function(formula) {
      ## 1. Confirm and strip the outer ifelse shell
      if (!grepl("^ifelse\\(", formula, perl=TRUE)) return(NULL)
      ## Extract the positive expression: everything before ", 1, ifelse(!..."
      ## Find the top-level comma that precedes ", 1,"
      inner <- sub("^ifelse\\(", "", formula, perl=TRUE)
      depth <- 0L; pos_end <- NA_integer_
      for (i in seq_len(nchar(inner))) {
        ch <- substr(inner, i, i)
        if      (ch == "(") depth <- depth + 1L
        else if (ch == ")") depth <- depth - 1L
        else if (ch == "," && depth == 0L) { pos_end <- i; break }
      }
      if (is.na(pos_end)) return(NULL)
      pos_str <- trimws(substr(inner, 1L, pos_end - 1L))

      ## 2. Split pos_str on top-level " | "
      split_top <- function(s, sep) {
        sep_len <- nchar(sep)
        depth <- 0L; parts <- character(0); start <- 1L
        for (i in seq_len(nchar(s))) {
          ch <- substr(s, i, i)
          if      (ch == "(") depth <- depth + 1L
          else if (ch == ")") depth <- depth - 1L
          else if (depth == 0L && i + sep_len - 1L <= nchar(s) &&
                   substr(s, i, i + sep_len - 1L) == sep) {
            parts <- c(parts, trimws(substr(s, start, i - 1L)))
            start <- i + sep_len
            i     <- start  ## advance (loop will increment)
          }
        }
        c(parts, trimws(substr(s, start, nchar(s))))
      }

      or_items <- split_top(pos_str, " | ")

      ## 3. Parse one term (with outer parens already stripped)
      ##    Always numeric values, two fixed patterns.
      parse_term <- function(t) {
        t <- trimws(t)
        ## na_or_ne: is.na(VAR) | VAR != VAL
        if (grepl("^is\\.na\\(", t, perl=TRUE)) {
          var <- sub("^is\\.na\\((\\w+)\\).*$",        "\\1", t, perl=TRUE)
          val <- sub("^.*!=\\s*([0-9.]+)\\s*$",        "\\1", t, perl=TRUE)
          return(list(var=var, op="na_or_ne", val=val))
        }
        ## standard: !is.na(VAR) & VAR OP VAL
        if (grepl("^!is\\.na\\(", t, perl=TRUE)) {
          var <- sub("^!is\\.na\\((\\w+)\\).*$",       "\\1", t, perl=TRUE)
          op  <- sub("^.*&\\s*\\w+\\s*(==|!=|>=|<=|>|<)\\s*[0-9.].*$",
                     "\\1", t, perl=TRUE)
          val <- sub("^.*(?:==|!=|>=|<=|>|<)\\s*([0-9.]+)\\s*$",
                     "\\1", t, perl=TRUE)
          return(list(var=var, op=op, val=val))
        }
        NULL
      }

      ## 4. For each OR-item: strip one layer of outer parens, classify, parse
      strip_outer <- function(s) {
        s <- trimws(s)
        if (startsWith(s, "(") && endsWith(s, ")")) substr(s, 2L, nchar(s) - 1L)
        else s
      }

      crits <- list()
      for (gi in seq_along(or_items)) {
        item    <- strip_outer(or_items[[gi]])
        is_last_or <- gi == length(or_items)

        if (startsWith(item, "(")) {
          ## AND group: split on top-level " & ", each piece is a (term)
          and_parts <- split_top(item, " & ")
          for (ai in seq_along(and_parts)) {
            term <- parse_term(strip_outer(and_parts[[ai]]))
            if (is.null(term)) next
            term$connector <- if (ai < length(and_parts)) "AND"
                              else if (!is_last_or)       "OR"
                              else                         NA
            crits <- c(crits, list(term))
          }
        } else {
          ## Single criterion
          term <- parse_term(item)
          if (is.null(term)) next
          term$connector <- if (!is_last_or) "OR" else NA
          crits <- c(crits, list(term))
        }
      }
      if (length(crits) == 0) NULL else crits
    }

    ## ---- Parse bin formula -----------------------------------------------
    ## Pattern: ifelse(is.na(VAR), NA_real_, ifelse(COND, N, ifelse(...)))
    ## Each COND: (!is.na(VAR) & VAR OP VAL) & (!is.na(VAR) & VAR OP VAL)
    parse_bin_formula <- function(formula) {
      ## Must start with outer NA guard
      m <- regmatches(formula,
        regexpr("^ifelse\\(is\\.na\\((\\w+)\\),\\s*NA_real_,", formula, perl=TRUE))
      if (length(m) == 0) return(NULL)
      var_nm <- sub("^ifelse\\(is\\.na\\((\\w+)\\),.*$", "\\1", formula, perl=TRUE)

      bins <- list()
      ## Walk nested ifelses
      rest <- sub("^ifelse\\(is\\.na\\(\\w+\\),\\s*NA_real_,\\s*", "", formula, perl=TRUE)
      rest <- sub("\\)$", "", rest)  ## strip trailing )
      bin_idx <- 0L

      while (grepl("^ifelse\\(", rest)) {
        bin_idx <- bin_idx + 1L
        ## Extract condition (everything up to the first top-level comma after the condition)
        inner <- sub("^ifelse\\(", "", rest, perl=TRUE)
        ## find the comma that separates cond from true-branch
        depth <- 0L; split_at <- NA_integer_
        for (i in seq_len(nchar(inner))) {
          ch <- substr(inner, i, i)
          if (ch == "(") depth <- depth + 1L
          else if (ch == ")") { depth <- depth - 1L; if (depth < 0L) break }
          else if (ch == "," && depth == 0L) { split_at <- i; break }
        }
        if (is.na(split_at)) break
        cond_str  <- trimws(substr(inner, 1L, split_at - 1L))
        after     <- trimws(substr(inner, split_at + 1L, nchar(inner)))
        ## true-branch is next token before comma
        comma2 <- regexpr(",", after)
        if (comma2 < 0) break
        bin_label <- trimws(substr(after, 1L, comma2 - 1L))
        rest      <- trimws(substr(after, comma2 + 1L, nchar(after)))
        ## strip trailing ) if rest ends with it (from outer ifelse wrapper)
        rest <- sub("\\)+$", "", rest)

        ## parse cond_str into lo/hi bounds
        ## each part: (!is.na(VAR) & VAR OP VAL)
        parts <- strsplit(cond_str, "\\s*&\\s*(?=\\()", perl=TRUE)[[1]]
        lo_val <- ""; lo_op <- ">="; hi_val <- ""; hi_op <- "<"
        for (p in parts) {
          p <- trimws(gsub("^\\(|\\)$", "", p))
          m2 <- regmatches(p, regexpr(
            "!is\\.na\\(\\w+\\)\\s*&\\s*\\w+\\s*(>=|<=|>|<)\\s*([0-9.]+)",
            p, perl=TRUE))
          if (length(m2) == 0) next
          op2  <- sub(".*&\\s*\\w+\\s*(>=|<=|>|<)\\s*.*", "\\1", p, perl=TRUE)
          val2 <- sub(".*&\\s*\\w+\\s*(?:>=|<=|>|<)\\s*([0-9.]+).*", "\\1", p, perl=TRUE)
          ## lo bound: var >= X or var > X (lo_op stored as-is, builder flips for display)
          if (op2 %in% c(">=", ">")) { lo_val <- val2; lo_op <- op2 }
          else                        { hi_val <- val2; hi_op <- op2 }
        }
        bins <- c(bins, list(list(lo_op=lo_op, lo_val=lo_val,
                                  hi_op=hi_op, hi_val=hi_val,
                                  bin_label=bin_label)))
      }
      if (length(bins) == 0) return(NULL)
      list(var=var_nm, bins=bins)
    }

    ## ---- Determine display mode and render --------------------------------
    formula <- entry$formula %||% ""

    body_content <- if (isTRUE(entry$type == "merge")) {
      ## ---- Merge variable -----------------------------------------------
      div(
        div(class="hint", style="margin-bottom:10px;",
          "Uses the primary variable; falls back to the secondary when primary is missing."),
        div(style="display:flex;flex-direction:column;gap:6px;",
          div(style="display:flex;align-items:center;gap:8px;",
            div(style="font-size:.72em;font-weight:700;color:#0d9488;padding:2px 8px;border:1.5px solid #0d9488;border-radius:4px;background:#f0fdfa;min-width:58px;text-align:center;",
                "Primary"),
            div(style="flex:1;background:#f9fafb;border:1px solid #e5e7eb;border-radius:5px;padding:5px 10px;font-size:.82em;font-weight:600;color:#111827;",
                entry$merge_left)
          ),
          div(style="padding-left:30px;font-size:.75em;color:#9ca3af;", "↓ if missing"),
          div(style="display:flex;align-items:center;gap:8px;",
            div(style="font-size:.72em;font-weight:700;color:#6b7280;padding:2px 8px;border:1.5px solid #d1d5db;border-radius:4px;background:#f9fafb;min-width:58px;text-align:center;",
                "Fallback"),
            div(style="flex:1;background:#f9fafb;border:1px solid #e5e7eb;border-radius:5px;padding:5px 10px;font-size:.82em;color:#374151;",
                entry$merge_right)
          )
        )
      )

    } else if (isTRUE(entry$is_zscore)) {
      ## ---- Z-score ------------------------------------------------------
      div(style="background:#f9fafb;border:1px solid #e5e7eb;border-radius:6px;padding:10px;font-size:.82em;",
        div(style="color:#6b7280;margin-bottom:4px;", "Standardized (z-score) of:"),
        div(style="font-weight:600;color:#111827;", entry$requires[1])
      )

    } else {
      ## Try composite flag first
      crits <- tryCatch(parse_composite_formula(formula), error=function(e) NULL)
      if (!is.null(crits) && length(crits) > 0) {
        render_criteria(crits, entry$label_map)
      } else {
        ## Try bin variable
        parsed_bin <- tryCatch(parse_bin_formula(formula), error=function(e) NULL)
        if (!is.null(parsed_bin)) {
          render_bins_standard(parsed_bin$bins, parsed_bin$var, entry$label_map)
        } else {
          ## Fallback: show formula
          div(style="background:#f9fafb;border:1px solid #e5e7eb;border-radius:6px;padding:10px;font-size:.76em;font-family:monospace;word-break:break-all;color:#374151;",
            formula)
        }
      }
    }

    ## Outer panel-box
    div(class="panel-box", style="margin-top:14px;",
      div(style="display:flex;justify-content:space-between;align-items:flex-start;margin-bottom:12px;",
        div(
          tags$code(style="font-size:.9em;", nm),
          if (nzchar(entry$label %||% ""))
            div(style="font-size:.8em;color:#6b7280;margin-top:3px;", entry$label)
        ),
        tags$button("×",
          style=paste0("background:none;border:none;color:#9ca3af;font-size:1.2em;",
                       "cursor:pointer;padding:0 2px;line-height:1;margin-top:-2px;"),
          onclick="Shiny.setInputValue('view_cv','',{priority:'event'})")
      ),
      body_content
    )
  })

  ## ---- Analysis variables status -------------------------------------------
  output$analysis_vars_status_ui <- renderUI({

    ## Collect all variables referenced in the Analysis tab
    exp_vars  <- Filter(nzchar, vapply(rv$exposure_rows, `[[`, character(1), "var"))
    out_vars  <- Filter(nzchar, vapply(rv$outcome_rows,  `[[`, character(1), "var"))
    cov_vars  <- input$shared_covariates %||% character(0)
    med_vars  <- input$mediators         %||% character(0)

    ## Per-population custom covariates
    pop_cov_vars <- unique(unlist(lapply(seq_len(rv$n_pops), function(i) {
      if (isTRUE(input[[paste0("custom_covars_", i)]])) {
        input[[paste0("pop_covariates_", i)]] %||% character(0)
      } else character(0)
    })))

    ## Constraint variables
    con_vars <- unique(unlist(lapply(seq_len(rv$n_pops), function(i) {
      pop_id <- paste0("pop", i)
      cons   <- isolate(pop_constraints[[pop_id]]) %||% list()
      Filter(nzchar, vapply(cons, function(c) c$var %||% "", character(1)))
    })))

    ## Tag each variable with its role — preserve original casing alongside
    ## the lowercase key used for lookups.
    roles <- list(
      list(vars = exp_vars,     role = "Exposure"),
      list(vars = out_vars,     role = "Outcome"),
      list(vars = cov_vars,     role = "Covariate"),
      list(vars = med_vars,     role = "Mediator"),
      list(vars = pop_cov_vars, role = "Pop. covariate"),
      list(vars = con_vars,     role = "Filter constraint")
    )

    ## Build a deduplicated table: lowercase key -> list(roles, original_name)
    var_role_map <- list()
    for (entry in roles) {
      for (v in entry$vars) {
        vl <- tolower(v)
        if (is.null(var_role_map[[vl]]))
          var_role_map[[vl]] <- list(roles = character(0), original = v)
        var_role_map[[vl]]$roles <- unique(c(var_role_map[[vl]]$roles, entry$role))
      }
    }

    if (length(var_role_map) == 0) {
      return(div(style="color:#aaa;font-size:.85em;",
                 "No variables selected in the Analysis tab yet."))
    }

    ## Check against database
    in_db        <- if (!is.null(rv$db)) tolower(names(rv$db)) else character(0)
    custom_vars  <- names(get_custom_variables())

    rows <- lapply(names(var_role_map), function(vl) {
      entry      <- var_role_map[[vl]]
      roles_str  <- paste(entry$roles, collapse=", ")
      in_dataset <- vl %in% in_db
      is_custom  <- vl %in% custom_vars
      is_builtin <- vl %in% BUILTIN_DERIVED_VARS
      already_staged <- vl %in% tolower(rv$staged)

      ## Derived variables (custom or built-in) stay lowercase when staged;
      ## real NHANES variables are uppercased for fetch_variable_pooled().
      is_derived <- is_custom || is_builtin
      stage_val  <- if (is_derived) vl else toupper(vl)

      status_ui <- if (in_dataset) {
        span(style="color:#15803d;font-size:.78em;font-weight:600;", "✓ in dataset")
      } else if (is_builtin && !in_dataset) {
        tagList(
          span(style="color:#f59e0b;font-size:.78em;",
               "⚠ built-in — stage to fetch prerequisites  "),
          if (!already_staged)
            tags$button("+ Stage", class="btn-sm-teal",
              style="font-size:.76em;padding:1px 7px;",
              onclick=sprintf(
                "Shiny.setInputValue('stage_var','%s',{priority:'event'})", stage_val))
          else
            span(style="color:#6366f1;font-size:.78em;", "staged")
        )
      } else if (is_custom && !in_dataset) {
        tagList(
          span(style="color:#f59e0b;font-size:.78em;",
               "⚠ custom — build dataset to compute  "),
          if (!already_staged)
            tags$button("+ Stage", class="btn-sm-teal",
              style="font-size:.76em;padding:1px 7px;",
              onclick=sprintf(
                "Shiny.setInputValue('stage_var','%s',{priority:'event'})", stage_val))
        )
      } else if (already_staged) {
        span(style="color:#6366f1;font-size:.78em;", "staged — build to add")
      } else {
        tagList(
          span(style="color:#b91c1c;font-size:.78em;font-weight:600;", "✗ missing  "),
          tags$button("+ Stage", class="btn-sm-teal",
            style="font-size:.76em;padding:1px 7px;",
            onclick=sprintf(
              "Shiny.setInputValue('stage_var','%s',{priority:'event'})", stage_val))
        )
      }

      div(class="var-row",
        span(class="var-code", vl),
        span(class="var-cat",
             style=if (!in_dataset && !is_custom) "background:#fee2e2;color:#b91c1c;" else "",
             roles_str),
        status_ui
      )
    })

    ## Summary line
    n_total   <- length(var_role_map)
    n_present <- sum(vapply(names(var_role_map),
                            function(vl) vl %in% in_db, logical(1)))
    n_missing <- n_total - n_present

    summary_style <- if (n_missing == 0)
      "color:#15803d;font-size:.82em;font-weight:600;margin-bottom:8px;"
    else
      "color:#b91c1c;font-size:.82em;font-weight:600;margin-bottom:8px;"

    summary_msg <- if (n_missing == 0) {
      sprintf("✓ All %d variable(s) present in dataset.", n_total)
    } else {
      sprintf("✗ %d of %d variable(s) missing from dataset.", n_missing, n_total)
    }

    tagList(
      div(style=summary_style, summary_msg),
      tagList(rows)
    )
  })

  ## ---- Dataset stats -------------------------------------------------------
  output$dataset_stats_ui <- renderUI({
    if (is.null(rv$db)) {
      return(div(style="color:#888;font-size:.88em;",
                  "No dataset loaded. Build one to get started."))
    }
    db   <- rv$db
    years <- sort(unique(db$year))
    fluidRow(
      column(3, div(class="db-stat",
        div(class="num", format(nrow(db), big.mark=",")),
        div(class="lbl", "Participants")
      )),
      column(3, div(class="db-stat",
        div(class="num", ncol(db)),
        div(class="lbl", "Variables")
      )),
      column(3, div(class="db-stat",
        div(class="num", length(years)),
        div(class="lbl", "Cycles")
      )),
      column(3, div(class="db-stat",
        div(class="num", sprintf("%s–%s", min(years), max(years))),
        div(class="lbl", "Years")
      ))
    )
  })

  ## ---- Dataset column list -------------------------------------------------
  output$dataset_vars_ui <- renderUI({
    req(rv$db_cols)
    cols <- rv$db_cols
    tagList(lapply(seq_len(nrow(cols)), function(i) {
      n_cyc <- cols$num_cycles[i]
      div(class="var-row",
        span(class="var-code", cols$column[i]),
        span(class="var-desc", cols$description[i]),
        if (nchar(cols$category[i]) > 0)
          span(class="var-cat", cols$category[i]),
        if (!is.na(n_cyc) && n_cyc > 0)
          span(style="font-size:.75em;color:#64748b;",
               paste0(n_cyc, " cycle", if (n_cyc != 1) "s"))
      )
    }))
  })

  ## ---- Build / update dataset ---------------------------------------------
  observeEvent(input$build_btn, {
    req(length(rv$staged) > 0)
    rv$build_status <- "running"
    rv$build_error  <- NULL
    shinyjs::disable("build_btn")
    shinyjs::html("build_btn", "<i class='fa fa-spinner fa-spin'></i> Building...")
    on.exit({
      shinyjs::enable("build_btn")
      shinyjs::html("build_btn", "Build / Update Dataset")
    })

    withProgress(message="Fetching variables...", value=0, {
      tryCatch({
        cycles <- c("1999-2000","2001-2002","2003-2004","2005-2006",
                     "2007-2008","2009-2010","2011-2012","2013-2014",
                     "2015-2016","2017-2018")

        ## Database already contains all SEQNs across all cycles since
        ## fetch_demographics pulls ALL_NHANES_CYCLES at build time.
        ## New variables either fetch from CDC or compute from existing columns.
        db <- rv$db
        cvars <- get_custom_variables()
        fetched_frames <- list()  ## store for weight map building

        for (var in rv$staged) {
          var_lower <- tolower(var)
          setProgress(detail=sprintf("Processing %s...", var_lower))

          if (var_lower %in% names(BUILTIN_BUILDERS)) {
            ## Built-in derived variable — compute from existing columns
            builder <- BUILTIN_BUILDERS[[var_lower]]
            db <- tryCatch(
              suppressWarnings(builder(db)),
              error = function(e) {
                showNotification(
                  sprintf("Error computing built-in '%s': %s", var_lower, conditionMessage(e)),
                  type="error")
                db
              }
            )
          } else if (var_lower %in% names(cvars)) {
            entry <- cvars[[var_lower]]
            if (isTRUE(entry$is_zscore)) {
              showNotification(
                sprintf("'%s' is a zscore variable — computed at analysis time, not stored.", var_lower),
                type="message", duration=5)
              next
            }
            missing_src <- setdiff(entry$requires, names(db))
            if (length(missing_src) > 0) {
              showNotification(
                sprintf("Note: '%s' will treat %s as NA (not in database yet).",
                         var_lower, paste(missing_src, collapse=", ")),
                type="warning", duration=6)
            }
            db <- tryCatch(
              apply_custom_variable(db, var_lower),
              error = function(e) {
                showNotification(sprintf("Error computing '%s': %s", var_lower, conditionMessage(e)),
                                  type="error")
                db
              }
            )
          } else {
            frm <- fetch_variable_pooled(var, cycles)
            if (!is.null(frm)) {
              fetched_frames[[var_lower]] <- frm  ## save for weight map

              if (var %in% names(frm)) names(frm)[names(frm) == var] <- var_lower
              new_cols <- setdiff(names(frm), c("seqn","year"))
              wt_pattern <- "^wt[a-z0-9]"
              for (col in new_cols) {
                if (grepl(wt_pattern, col) && col %in% names(db)) {
                  existing <- db[[col]]
                  new_vals <- frm[[col]][match(paste(db$seqn, db$year),
                                                paste(frm$seqn, frm$year))]
                  db[[col]] <- ifelse(is.na(existing), new_vals, existing)
                } else {
                  if (col %in% names(db)) db[[col]] <- NULL
                  db <- left_join(db,
                    frm[, c("seqn","year",col), drop=FALSE],
                    by=c("seqn","year"))
                }
              }
            }
          }
        }

        ## Build variable->weight map from fetch-time metadata.
        ## Structure: wt_map[[variable]][[cycle_year]] = weight_col_name
        ## This records exactly which weight came from each variable's own
        ## XPT file for each cycle — used at analysis time to assign the
        ## correct weight per variable per cycle.
        wt_map <- attr(db, "variable_weight_map") %||% list()

        standard_wt_names <- c("wtmec2yr","wtmec4yr","wtint2yr","wtint4yr")

        for (var_lower in names(fetched_frames)) {
          frm_stored <- fetched_frames[[var_lower]]
          frm_maps   <- attr(frm_stored, "cycle_weight_maps")
          if (is.null(frm_maps) || length(frm_maps) == 0) next

          cycle_wt <- list()
          for (cm in frm_maps) {
            yr  <- as.integer(sub("-.*", "", cm$cycle))
            wts <- cm$weights[!cm$weights %in% standard_wt_names]
            if (length(wts) == 0) next
            ## Always store the *2yr variant for 1999/2001.
            ## build_analysis_weight step 3b upgrades to *4yr when both early
            ## cycles are present; using 2yr here ensures the correct weight
            ## is used when only one of 1999 or 2001 is in the analysis.
            wts2 <- wts[grepl("2yr$", wts)]
            chosen <- if (length(wts2) > 0) wts2[1] else wts[1]
            cycle_wt[[as.character(yr)]] <- chosen
          }
          if (length(cycle_wt) > 0) {
            wt_map[[var_lower]] <- cycle_wt
            ## Also index by the resolved db column name (e.g. "ldl" → "lbdldl")
            ## so engine lookups using resolve_colnames() find the same entry.
            resolved_key <- tolower(resolve_colname(var_lower))
            if (resolved_key != var_lower) wt_map[[resolved_key]] <- cycle_wt
          }
        }

        ## Compute pooled versions of subsample weights added during this build.
        ## Standard weights (MEC, interview) don't need pooling.
        ## Fasting weights (wtsaf2yr, wtsaf4yr) → coalesce into single wtsaf column.
        ## Exclude already-pooled columns to prevent _pooled_pooled accumulation.
        standard_wts <- c("wtint2yr","wtmec2yr","wtmec4yr","wtint4yr",
                           "wtsaf","wtsaf2yr","wtsaf4yr")
        all_wt_cols   <- grep("^wt[a-z0-9]", names(db), value=TRUE)
        subsample_wts <- all_wt_cols[
          !all_wt_cols %in% standard_wts &
          !grepl("_pooled$",    all_wt_cols) &
          !grepl("^wt[mi]rep|^wtshm[0-9]|^wtsph|^wtspo[0-9]|^wtsci[0-9]|^wtsau[0-9]|^wtsba[0-9]|^wtspp[0-9]", all_wt_cols)   ## exclude jackknife replicates
        ]

        ## Coalesce fasting weights into wtsaf
        ## Prefer 4yr for 1999-2002 rows (CDC requirement); 2yr fills later cycles.
        fasting_wt_cols <- intersect(c("wtsaf4yr","wtsaf2yr"), names(db))
        if (length(fasting_wt_cols) > 0) {
          db$wtsaf <- apply(db[, fasting_wt_cols, drop=FALSE], 1, function(row) {
            vals <- as.numeric(row)
            nna  <- vals[!is.na(vals) & vals > 0]
            if (length(nna) == 0) NA_real_ else nna[1]
          })
        }

        ## Create/update pooled versions for genuine subsample weights
        for (wt in subsample_wts) {
          pooled_col <- paste0(wt, "_pooled")
          n_cycles <- length(unique(db$year[!is.na(db[[wt]]) & db[[wt]] > 0]))
          if (n_cycles > 0) {
            db[[pooled_col]] <- db[[wt]] / n_cycles
          }
        }

        ## Propagate weight mappings to all derived variables — both built-in
        ## derived_outcomes_registry entries (like remnant, non_hdl) and user
        ## custom variables — so that every engine can find the correct
        ## subsample weight regardless of whether it expands sources like
        ## cli.R does or looks up the variable name directly.
        ## Done AFTER weight coalescing so wtsaf exists in db.
        ##
        ## Priority: lower WEIGHT_PRIORITY number = more restrictive.
        ## Unknown wts**[24]yr columns (not in WEIGHT_PRIORITY) get
        ## .wt_priority_na_default (4L) — overrides both MEC and fasting.
        all_derived_vars <- c(derived_outcomes_registry, cvars)
        mec_pri <- WEIGHT_PRIORITY["wtmec2yr"]   ## = 6; beat this to qualify
        for (dv_nm in names(all_derived_vars)) {
          entry <- all_derived_vars[[dv_nm]]
          if (is.null(entry$requires) || length(entry$requires) == 0) next
          best_map <- list()

          ## Composite flags (or_groups set): two-level resolution.
          ## Within each AND-group take the most-restrictive weight (lowest number);
          ## across OR-groups take the least-restrictive (highest number) — a fasting
          ## OR MEC composite can use MEC weight, but a fasting AND MEC composite needs
          ## fasting weight.
          ##
          ## wt_map only contains subsample weights (PFAS, fasting). Standard weights
          ## (MEC = 6, interview = 7) have no wt_map entry and are inferred from
          ## the Duke lookup weight column for the variable.
          if (!is.null(entry$or_groups)) {
            ## Map weight_group → priority for variables without a wt_map entry.
            ## Values align with WEIGHT_PRIORITY; no subsample map is stored because
            ## the analysis engine uses its MEC/interview default for these.
            wg_to_pri <- c(
              pfas      = unname(mec_pri) - 2L,
              fasting   = unname(WEIGHT_PRIORITY["wtsaf2yr"]),
              exam      = unname(mec_pri),
              interview = unname(WEIGHT_PRIORITY["wtint2yr"])
            )

            ## Returns list(pri, map) for one required variable.
            ## map is NULL when the variable uses a standard (non-subsample) weight.
            var_weight_info <- function(req) {
              req_map <- wt_map[[req]]
              if (!is.null(req_map) && length(req_map) > 0) {
                rep_col  <- req_map[[1L]]
                p        <- WEIGHT_PRIORITY[rep_col]
                if (is.na(p)) p <- .wt_priority_na_default
                return(list(pri = unname(p), map = req_map))
              }
              ## No subsample map — infer weight group from Duke lookup
              wt_col <- get_predominant_weight(req, ANALYSIS_CYCLES)
              if (nzchar(wt_col)) {
                grp <- weight_col_to_group(wt_col)
                p   <- wg_to_pri[[grp]]
                if (!is.na(p)) return(list(pri = unname(p), map = NULL))
              }
              list(pri = unname(mec_pri), map = NULL)  ## default: MEC
            }

            best_pri <- -Inf  ## OR takes the max across groups
            for (grp_vars in entry$or_groups) {
              grp_pri <- Inf   ## AND takes the min within group
              grp_map <- list()
              for (req in tolower(grp_vars)) {
                vi <- var_weight_info(req)
                if (vi$pri < grp_pri) {
                  grp_pri <- vi$pri
                  grp_map <- if (!is.null(vi$map)) vi$map else list()
                }
              }
              ## Least restrictive across OR groups = highest priority number
              if (is.finite(grp_pri) && grp_pri > best_pri) {
                best_pri <- grp_pri
                best_map <- grp_map
              }
            }
            ## Only store a wt_map entry when the winning group has a subsample weight.
            ## When best_map is empty the winning group uses MEC/interview (default).
            if (length(best_map) > 0) wt_map[[dv_nm]] <- best_map
            next
          }

          ## Merge variables use least-restrictive weight (highest priority number =
          ## broadest coverage). All other derived variables use most-restrictive.
          is_merge <- isTRUE(entry$type == "merge")
          best_pri <- if (is_merge) -Inf else mec_pri
          for (req in tolower(entry$requires)) {
            req_map <- wt_map[[req]]
            if (is.null(req_map) || length(req_map) == 0) next
            rep_col  <- req_map[[1L]]             ## weight col for first stored cycle
            cand_pri <- WEIGHT_PRIORITY[rep_col]  ## NA if not in known list
            if (is.na(cand_pri)) cand_pri <- .wt_priority_na_default
            ## Merge: pick least restrictive (highest number); others: most restrictive (lowest)
            if (is_merge && cand_pri > best_pri) { best_map <- req_map; best_pri <- cand_pri }
            if (!is_merge && cand_pri < best_pri) { best_map <- req_map; best_pri <- cand_pri }
          }
          if (length(best_map) > 0) wt_map[[dv_nm]] <- best_map
        }

        attr(db, "variable_weight_map") <- wt_map

        ## Save updated database
        path <- here("data","nhanes_pool.rds")
        saveRDS(db, path)
        rv$db      <- db
        rv$db_cols <- get_dataset_columns(db)
        rv$staged  <- character(0)
        update_autocomplete()

        ## Re-apply covariate selections from any loaded project now that the
        ## newly built variables are present in rv$db_cols.
        if (!is.null(rv$loaded_proj)) {
          lp         <- rv$loaded_proj
          new_choices <- setNames(rv$db_cols$column, rv$db_cols$label)
          saved_covs  <- lp$shared_covariates %||% character(0)
          valid_covs  <- saved_covs[saved_covs %in% rv$db_cols$column]
          if (length(valid_covs) > 0)
            updateSelectizeInput(session, "shared_covariates",
                                 choices  = new_choices,
                                 selected = valid_covs,
                                 server   = TRUE)
          for (i in seq_along(lp$populations %||% list())) {
            pop <- lp$populations[[i]]
            if (isTRUE(pop$custom_covars)) {
              pop_covs       <- pop$covariates %||% character(0)
              valid_pop_covs <- pop_covs[pop_covs %in% rv$db_cols$column]
              if (length(valid_pop_covs) > 0)
                updateSelectizeInput(session, paste0("pop_covariates_", i),
                                     choices  = new_choices,
                                     selected = valid_pop_covs,
                                     server   = TRUE)
            }
          }
        }

        rv$build_status <- "done"
        setProgress(1)
      }, error=function(e) {
        rv$build_error  <- conditionMessage(e)
        rv$build_status <- "error"
      })
    })
  })

  output$build_status_ui <- renderUI({
    if (is.null(rv$build_status)) return(NULL)
    switch(rv$build_status,
      "running" = div(class="status-running", "Fetching and merging variables..."),
      "done"    = div(class="status-done",    "✓ Dataset updated successfully"),
      "error"   = div(class="status-error",
                       "Error: ", rv$build_error)
    )
  })

  ## ---- Population tabs UI -------------------------------------------------
  ## Helper: build one tabPanel for population i.
  ## All per-population state is passed explicitly so the function can be used
  ## both at initial render and from appendTab (where rv reads must be isolate'd).
  make_pop_tab_panel <- function(i, pop_label, pop_cycs, pop_cust_covs, db_cols_now) {
    pop_id <- paste0("pop", i)
    tabPanel(
      title = div(
        ## Span lets JS update the title text without a Shiny re-render
        tags$span(id = paste0("pop_tab_title_", i), pop_label),
        ## Always show ×; server guards against removing the last population
        tags$span("×", style="margin-left:6px;color:#888;cursor:pointer;",
          onclick = sprintf(
            "Shiny.setInputValue('remove_pop',%d,{priority:'event'})", i))
      ),
      value = pop_id,
      br(),

      ## Population name
      fluidRow(
        column(6,
          textInput(paste0("pop_label_", i),
            label = "Population name",
            value = pop_label)
        )
      ),

      ## Cycle selector
      div(class="section-label", style="margin-top:8px;", "NHANES cycles"),
      checkboxGroupInput(
        inputId  = paste0("pop_cycles_", i),
        label    = NULL,
        choices  = .all_cycles,
        selected = pop_cycs,
        inline   = TRUE
      ),

      ## Constraint builder
      div(class="section-label", style="margin-top:8px;", "Inclusion criteria"),
      uiOutput(paste0("constraints_ui_", i)),
      actionButton(paste0("add_constraint_", i),
                    "+ Add constraint", class="btn-sm-teal",
                    style="margin-top:4px;"),

      ## Custom covariates
      br(),
      checkboxInput(paste0("custom_covars_", i),
                     "Override shared covariates for this population",
                     value = pop_cust_covs),
      conditionalPanel(
        condition = sprintf("input['custom_covars_%d']", i),
        selectizeInput(paste0("pop_covariates_", i),
          label   = "Custom covariates",
          choices = if (!is.null(db_cols_now))
                      setNames(db_cols_now$column, db_cols_now$label) else NULL,
          multiple = TRUE,
          options  = list(create=TRUE, placeholder="Type to search...",
                           plugins=list("remove_button"))
        )
      )
    )
  }

  ## Full (re-)render of the population tabset — only triggered by pop_tabs_reset.
  ## Adding a population uses appendTab instead so existing panels are never
  ## destroyed (which would wipe uiOutput constraint containers).
  output$population_tabs_ui <- renderUI({
    rv$pop_tabs_reset          ## sole reactive dependency — changing this forces rebuild
    n           <- isolate(rv$n_pops)
    db_cols_now <- isolate(rv$db_cols)
    if (n == 0) return(NULL)

    tab_panels <- lapply(seq_len(n), function(i) {
      pop_id <- paste0("pop", i)
      make_pop_tab_panel(
        i,
        pop_label     = isolate(rv$pop_labels[i]),
        pop_cycs      = isolate(rv$pop_cycles[[pop_id]])      %||% .all_cycles,
        pop_cust_covs = isolate(rv$pop_custom_covars[[pop_id]]) %||% FALSE,
        db_cols_now   = db_cols_now
      )
    })

    div(class="pop-tabs",
      do.call(tabsetPanel, c(list(id="pop_tabset"), tab_panels))
    )
  })

  ## Per-population constraint rows
  pop_constraints      <- reactiveValues()  # stores constraint lists per population
  pop_constraint_counts <- reactiveValues() # only the counts — renderUI depends on this

  lapply(1:10, function(i) {
    local({
      ii <- i
      pop_id <- paste0("pop", ii)

      ## Initialize constraints for this pop — start empty
      observe({
        if (is.null(pop_constraints[[pop_id]])) {
          pop_constraints[[pop_id]]      <- list()
          pop_constraint_counts[[pop_id]] <- 0L
        }
      })

      ## Add constraint button — updates count to trigger re-render
      observeEvent(input[[paste0("add_constraint_", ii)]], {
        current <- isolate(pop_constraints[[pop_id]])
        pop_constraints[[pop_id]] <- c(current, list(list(var="", op="==", val="")))
        pop_constraint_counts[[pop_id]] <- length(pop_constraints[[pop_id]])
      }, ignoreInit=TRUE)

      ## Constraint rows UI — depends ONLY on count, never on field values
      output[[paste0("constraints_ui_", ii)]] <- renderUI({
        n <- pop_constraint_counts[[pop_id]] %||% 0L
        if (n == 0) return(NULL)

        col_choices <- if (!is.null(rv$db_cols)) {
          labels <- ifelse(nzchar(rv$db_cols$description),
            paste0(rv$db_cols$column, " — ", rv$db_cols$description),
            rv$db_cols$column)
          setNames(rv$db_cols$column, labels)
        } else character(0)
        op_choices <- c("=="="==","!="="!=",">="=">=","<="="<=",
                        ">"=">","<"="<","not_na"="not_na","na_or_neq"="na_or_neq")

        ## Read current values without creating reactive dependency
        constraints <- isolate(pop_constraints[[pop_id]])

        tagList(lapply(seq_len(n), function(j) {
          con    <- if (!is.null(constraints) && j <= length(constraints))
                      constraints[[j]] else list(var="", op="==", val="")
          row_id <- paste0("con_", ii, "_", j)
          div(class="constraint-row",
            selectizeInput(paste0(row_id, "_var"),
              label=NULL, selected=con$var,
              choices=col_choices, width="220px",
              options=list(create=FALSE, placeholder="type to filter...",
                           maxOptions=20, searchField=c("value","label"),
                           sortField=list(list(field="value", direction="asc")))),
            selectInput(paste0(row_id, "_op"),
              label=NULL, selected=con$op,
              choices=op_choices, width="100px"),
            conditionalPanel(
              condition=sprintf("input['%s_op'] != 'not_na'", row_id),
              textInput(paste0(row_id, "_val"),
                label=NULL, value=con$val, width="100px", placeholder="value")),
            tags$button("×", class="btn-danger-sm",
              onclick=sprintf(
                "Shiny.setInputValue('remove_constraint','%s',{priority:'event'})",
                paste0(ii,"_",j)))
          )
        }))
      })

      ## Save constraint field changes back to pop_constraints immediately.
      ## Use isolate() on the write so we don't trigger constraints_ui to
      ## re-render — the UI already shows the correct value since the user
      ## just typed it; we only need to persist it for when the UI rebuilds.
      lapply(1:20, function(j) {
        local({
          jj     <- j
          row_id <- paste0("con_", ii, "_", jj)

          observeEvent(input[[paste0(row_id, "_var")]], {
            val <- input[[paste0(row_id, "_var")]]
            if (!nzchar(val %||% "")) return()  # ignore empty from selectize re-init
            isolate({
              current <- pop_constraints[[pop_id]]
              if (!is.null(current) && jj <= length(current)) {
                current[[jj]]$var <- val
                pop_constraints[[pop_id]] <- current
              }
            })
          }, ignoreInit=TRUE, ignoreNULL=TRUE)

          observeEvent(input[[paste0(row_id, "_op")]], {
            isolate({
              current <- pop_constraints[[pop_id]]
              if (!is.null(current) && jj <= length(current)) {
                current[[jj]]$op <- input[[paste0(row_id, "_op")]]
                pop_constraints[[pop_id]] <- current
              }
            })
          }, ignoreInit=TRUE, ignoreNULL=TRUE)

          observeEvent(input[[paste0(row_id, "_val")]], {
            isolate({
              current <- pop_constraints[[pop_id]]
              if (!is.null(current) && jj <= length(current)) {
                current[[jj]]$val <- input[[paste0(row_id, "_val")]]
                pop_constraints[[pop_id]] <- current
              }
            })
          }, ignoreInit=TRUE, ignoreNULL=TRUE)
        })
      })
    })
  })

  ## Remove constraint
  observeEvent(input$remove_constraint, {
    parts <- strsplit(input$remove_constraint, "_")[[1]]
    ii    <- as.integer(parts[1])
    jj    <- as.integer(parts[2])
    pop_id <- paste0("pop", ii)
    current <- isolate(pop_constraints[[pop_id]])
    if (length(current) > 0 && jj <= length(current)) {
      pop_constraints[[pop_id]] <- current[-jj]
      pop_constraint_counts[[pop_id]] <- length(current) - 1L
    }
  })

  ## Add population — appendTab so existing panels (and their constraint uiOutputs)
  ## are never destroyed.
  observeEvent(input$add_pop_btn, {
    new_i         <- isolate(rv$n_pops) + 1L
    new_label     <- sprintf("Population %d", new_i)
    new_pop_id    <- paste0("pop", new_i)
    rv$n_pops     <- new_i
    rv$pop_labels <- c(isolate(rv$pop_labels), new_label)
    rv$pop_cycles[[new_pop_id]]        <- .all_cycles
    rv$pop_custom_covars[[new_pop_id]] <- FALSE

    appendTab("pop_tabset",
      make_pop_tab_panel(
        new_i,
        pop_label     = new_label,
        pop_cycs      = .all_cycles,
        pop_cust_covs = FALSE,
        db_cols_now   = isolate(rv$db_cols)
      ),
      select = TRUE
    )
  })

  ## Remove population — rebuild the whole tabset via pop_tabs_reset so indices
  ## stay consistent (tab "popN" always matches rv$pop_labels[N]).
  observeEvent(input$remove_pop, {
    i <- input$remove_pop
    if (isolate(rv$n_pops) <= 1) {
      showNotification("Cannot remove the last population.", type="warning", duration=5)
      return()
    }
    pop_id <- paste0("pop", i)
    rv$n_pops                      <- isolate(rv$n_pops) - 1L
    rv$pop_labels                  <- isolate(rv$pop_labels)[-i]
    pop_constraints[[pop_id]]      <- NULL
    rv$pop_cycles[[pop_id]]        <- NULL
    rv$pop_custom_covars[[pop_id]] <- NULL
    rv$pop_tabs_reset              <- isolate(rv$pop_tabs_reset) + 1L
  })

  ## Update population labels from text inputs.
  ## Also patch the tab-title span directly via JS so no re-render is needed.
  lapply(1:10, function(i) {
    local({
      ii <- i
      observeEvent(input[[paste0("pop_label_", ii)]], {
        lbl <- input[[paste0("pop_label_", ii)]]
        if (!is.null(lbl) && ii <= isolate(rv$n_pops)) {
          rv$pop_labels[ii] <- lbl
          runjs(sprintf(
            "var el=document.getElementById('pop_tab_title_%d'); if(el) el.textContent=%s;",
            ii, jsonlite::toJSON(lbl, auto_unbox=TRUE)
          ))
        }
      }, ignoreInit=TRUE)

      ## Save cycle selections so they survive dataset rebuilds
      observeEvent(input[[paste0("pop_cycles_", ii)]], {
        if (ii <= rv$n_pops)
          rv$pop_cycles[[paste0("pop", ii)]] <- input[[paste0("pop_cycles_", ii)]]
      }, ignoreInit=TRUE, ignoreNULL=FALSE)

      ## Save custom-covars checkbox state so it survives re-renders
      observeEvent(input[[paste0("custom_covars_", ii)]], {
        if (ii <= rv$n_pops)
          rv$pop_custom_covars[[paste0("pop", ii)]] <- isTRUE(input[[paste0("custom_covars_", ii)]])
      }, ignoreInit=TRUE)
    })
  })

  ## When db_cols update (after a build), push new choices to the custom-covariate
  ## selectize inputs without triggering a full population-tabs re-render.
  observeEvent(rv$db_cols, {
    req(rv$db_cols)
    choices <- setNames(rv$db_cols$column, rv$db_cols$label)
    for (i in seq_len(isolate(rv$n_pops))) {
      updateSelectizeInput(session, paste0("pop_covariates_", i),
        choices=choices, server=TRUE)
    }
  }, ignoreInit=TRUE)

  ## ---- Build constraints from UI ------------------------------------------
  build_constraints <- function(i) {
    pop_id      <- paste0("pop", i)
    constraints <- isolate(pop_constraints[[pop_id]])
    if (is.null(constraints) || length(constraints) == 0) return(list())

    Filter(Negate(is.null), lapply(seq_along(constraints), function(j) {
      row_id <- paste0("con_", i, "_", j)
      ## Read from input first, fall back to stored value
      var <- input[[paste0(row_id, "_var")]]
      op  <- input[[paste0(row_id, "_op")]]
      val <- input[[paste0(row_id, "_val")]]

      ## Fall back to stored values if inputs not rendered
      stored <- constraints[[j]]
      if (is.null(var) || length(var) == 0 || !nzchar(trimws(var)))
        var <- if (is.list(stored)) stored$var else NULL
      if (is.null(op) || length(op) == 0)
        op  <- if (is.list(stored)) stored$op  else NULL
      if (is.null(val) || length(val) == 0)
        val <- if (is.list(stored)) stored$val else ""

      if (is.null(var) || !nzchar(trimws(var))) return(NULL)
      if (is.null(op))                           return(NULL)

      if (op == "not_na") {
        make_constraint(var, "not_na")
      } else {
        val_num <- suppressWarnings(as.numeric(val))
        val_use <- if (length(val_num) == 1 && !is.na(val_num)) val_num else val %||% ""
        make_constraint(var, op, val_use)
      }
    }))
  }

  ## ---- Run analysis -------------------------------------------------------
  observeEvent(input$run_btn, {
    req(!is.null(rv$db))
    rv$results    <- list()
    rv$run_status <- "running"
    rv$run_error  <- NULL
    shinyjs::disable("run_btn")
    shinyjs::html("run_btn", "<i class='fa fa-spinner fa-spin'></i> Running...")
    on.exit({
      shinyjs::enable("run_btn")
      shinyjs::html("run_btn", "Run Analysis")
    })

    withProgress(message="Running analysis...", value=0, {
      tryCatch({
        ## Resolve exposures and transforms
        exp_info          <- get_exposures()
        out_info          <- get_outcomes()
        exposures         <- exp_info$vars
        outcomes          <- out_info$vars
        exp_log10         <- exp_info$transforms
        out_transforms    <- out_info$transforms
        shared_covs       <- input$shared_covariates
        analysis_type_sel <- input$analysis_type %||% "linear"
        rv$analysis_type  <- analysis_type_sel

        for (i in seq_len(rv$n_pops)) {
          setProgress(i / rv$n_pops,
                       detail=sprintf("Running %s...", rv$pop_labels[i]))

          covariates <- if (isTRUE(input[[paste0("custom_covars_", i)]])) {
            input[[paste0("pop_covariates_", i)]]
          } else {
            shared_covs
          }

          ## Cycles — default to all if none selected
          selected_cycles <- input[[paste0("pop_cycles_", i)]]
          if (is.null(selected_cycles) || length(selected_cycles) == 0) {
            selected_cycles <- c("1999-2000","2001-2002","2003-2004","2005-2006",
                                  "2007-2008","2009-2010","2011-2012","2013-2014",
                                  "2015-2016","2017-2018")
          }

          constraints <- build_constraints(i)
          pop_def <- list(
            label       = rv$pop_labels[i],
            constraints = constraints
          )

          result <- tryCatch({
            ## Capture verbose output using textConnection instead of
            ## capture.output — avoids Shiny output stack interference
            ## which causes errors to bypass tryCatch
            trace_lines <- character(0)
            tc <- textConnection("trace_lines", open="w", local=TRUE)
            sink(tc, type="output")

            db_path_val <- here("data", "nhanes_pool.rds")

            captured_warnings <- character(0)
            r <- withCallingHandlers(
              tryCatch({
              switch(analysis_type_sel,

                "linear" = run_analysis(
                  exposures             = exposures,
                  outcomes              = outcomes,
                  covariates            = covariates,
                  population            = pop_def,
                  cycles                = selected_cycles,
                  exp_log10_flags       = exp_log10,
                  out_log10_flags       = out_transforms,
                  db_path               = db_path_val,
                  complete_cases_only   = isTRUE(input$complete_cases_only),
                  quartile_stratified   = isTRUE(input$quartile_stratified),
                  dietary_weight        = if (isTRUE(input$use_dietary_weight)) input$dietary_recall_days %||% "1day" else FALSE,
                  verbose               = TRUE
                ),

                "logistic" = run_analysis(
                  exposures             = exposures,
                  outcomes              = outcomes,
                  covariates            = covariates,
                  population            = pop_def,
                  cycles                = selected_cycles,
                  exp_log10_flags       = exp_log10,
                  out_log10_flags       = out_transforms,
                  db_path               = db_path_val,
                  family                = "logistic",
                  complete_cases_only   = isTRUE(input$complete_cases_only),
                  quartile_stratified   = isTRUE(input$quartile_stratified),
                  logistic_ref_lowest   = isTRUE(input$logistic_categorical),
                  dietary_weight        = if (isTRUE(input$use_dietary_weight)) input$dietary_recall_days %||% "1day" else FALSE,
                  verbose               = TRUE
                ),

                "spline" = {
                  knot_mode <- input$spline_knot_mode %||% "auto"
                  fixed_knots <- if (knot_mode == "fixed") {
                    raw <- trimws(strsplit(input$spline_knots_fixed %||% "", ",")[[1]])
                    vals <- suppressWarnings(as.numeric(raw[nchar(raw) > 0]))
                    if (length(vals) == 0 || any(is.na(vals))) {
                      showNotification("Fixed knots must be comma-separated numbers. Using df=3.",
                                       type = "warning", duration = 5)
                      NULL
                    } else sort(vals)
                  } else NULL
                  spline_df_val <- if (knot_mode == "quantile") as.integer(input$spline_df %||% 3) else 3L

                  run_analysis(
                    exposures             = exposures,
                    outcomes              = outcomes,
                    covariates            = covariates,
                    population            = pop_def,
                    cycles                = selected_cycles,
                    exp_log10_flags       = exp_log10,
                    out_log10_flags       = out_transforms,
                    db_path               = db_path_val,
                    spline_df             = if (is.null(fixed_knots)) spline_df_val else NULL,
                    spline_knots          = fixed_knots,
                    complete_cases_only   = isTRUE(input$complete_cases_only),
                    quartile_stratified   = isTRUE(input$quartile_stratified),
                    dietary_weight        = if (isTRUE(input$use_dietary_weight)) input$dietary_recall_days %||% "1day" else FALSE,
                    verbose               = TRUE
                  )
                },

                "multinomial" = run_multinomial_analysis(
                  exposures           = exposures,
                  outcomes            = outcomes,
                  covariates          = covariates,
                  population          = pop_def,
                  cycles              = selected_cycles,
                  db_path             = db_path_val,
                  complete_cases_only = isTRUE(input$complete_cases_only),
                  dietary_weight      = if (isTRUE(input$use_dietary_weight)) input$dietary_recall_days %||% "1day" else FALSE,
                  verbose             = TRUE
                ),

                "wqs" = run_wqs_analysis(
                  exposures           = exposures,
                  outcomes            = outcomes,
                  covariates          = covariates,
                  population          = pop_def,
                  cycles              = selected_cycles,
                  db_path             = db_path_val,
                  directions          = out_info$wqs_directions,
                  n_quantiles         = as.integer(input$wqs_q    %||% 4),
                  n_boot              = as.integer(input$wqs_boot %||% 100),
                  seed                = as.integer(input$wqs_seed %||% 42),
                  complete_cases_only = isTRUE(input$complete_cases_only),
                  dietary_weight      = if (isTRUE(input$use_dietary_weight)) input$dietary_recall_days %||% "1day" else FALSE,
                  verbose             = TRUE
                ),

                "qgcomp" = run_qgcomp_analysis(
                  exposures           = exposures,
                  outcomes            = outcomes,
                  covariates          = covariates,
                  population          = pop_def,
                  cycles              = selected_cycles,
                  db_path             = db_path_val,
                  n_quantiles         = as.integer(input$qgcomp_q         %||% 4),
                  seed                = as.integer(input$qgcomp_seed      %||% 42),
                  use_bootstrap       = isTRUE(input$qgcomp_bootstrap),
                  n_boot              = as.integer(input$qgcomp_boot       %||% 200),
                  out_transforms      = out_transforms,
                  exp_transforms      = exp_log10,
                  complete_cases_only = isTRUE(input$complete_cases_only),
                  dietary_weight      = if (isTRUE(input$use_dietary_weight)) input$dietary_recall_days %||% "1day" else FALSE,
                  verbose             = TRUE
                ),

                "bkmr" = run_bkmr_analysis(
                  exposures           = exposures,
                  outcomes            = outcomes,
                  covariates          = covariates,
                  population          = pop_def,
                  cycles              = selected_cycles,
                  db_path             = db_path_val,
                  n_iter              = as.integer(input$bkmr_iter %||% 1000),
                  seed                = as.integer(input$bkmr_seed %||% 42),
                  n_chains            = if (isTRUE(input$bkmr_parallel))
                                          as.integer(input$bkmr_chains %||% 4L) else 1L,
                  complete_cases_only = isTRUE(input$complete_cases_only),
                  verbose             = TRUE,
                  progress_callback = function(current, total, outcome) {
                    setProgress(
                      value  = current / total,
                      detail = sprintf("Outcome %d/%d: %s", current, total, outcome)
                    )
                  }
                ),

                "mediation" = run_mediation_analysis(
                  exposures           = exposures,
                  outcomes            = outcomes,
                  mediators           = input$mediators %||% character(0),
                  covariates          = covariates,
                  population          = pop_def,
                  cycles              = selected_cycles,
                  db_path             = db_path_val,
                  exp_transforms      = exp_log10,
                  out_transforms      = out_transforms,
                  complete_cases_only = isTRUE(input$complete_cases_only),
                  dietary_weight      = if (isTRUE(input$use_dietary_weight)) input$dietary_recall_days %||% "1day" else FALSE,
                  verbose             = TRUE
                ),

                "timetrend" = run_timetrend_analysis(
                  variables           = c(exposures, outcomes),
                  covariates          = covariates,
                  pct_change          = isTRUE(input$trend_pct_change),
                  population          = pop_def,
                  cycles              = selected_cycles,
                  db_path             = db_path_val,
                  complete_cases_only = isTRUE(input$complete_cases_only),
                  dietary_weight      = if (isTRUE(input$use_dietary_weight)) input$dietary_recall_days %||% "1day" else FALSE,
                  verbose             = TRUE
                ),

                stop(sprintf("Unknown analysis type: '%s'", analysis_type_sel))
              )
            }, error = function(e) {
              sink(type="output"); close(tc); stop(e)
            }),
            warning = function(w) {
              msg <- conditionMessage(w)
              ## Suppress known benign svyglm/survey messages that are not skip events
              benign <- c(
                "observations with zero weight not used for calculating dispersion",
                "In svyglm"
              )
              if (!any(vapply(benign, function(p) grepl(p, msg, fixed=TRUE), logical(1))))
                captured_warnings <<- c(captured_warnings, msg)
              invokeRestart("muffleWarning")
            })

            sink(type="output")
            close(tc)

            r$trace <- paste(trace_lines, collapse="\n")
            r$warnings  <- captured_warnings
            r
          }, error = function(e) {
            ## Ensure sink is closed even on error
            tryCatch({ sink(type="output") }, error=function(x) NULL)
            cat(sprintf("\n=== ERROR in %s analysis ===\n", analysis_type_sel))
            cat("Message:", conditionMessage(e), "\n")
            list(error=conditionMessage(e))
          })
          rv$results[[i]] <- result
        }

        rv$run_status <- "done"
        setProgress(1)
      }, error=function(e) {
        rv$run_error  <- conditionMessage(e)
        rv$run_status <- "error"
      })
    })
  })

  output$analysis_status_ui <- renderUI({
    if (is.null(rv$run_status)) return(NULL)
    switch(rv$run_status,
      "running" = div(class="status-running", "Running regressions..."),
      "done"    = div(class="status-done",
                       sprintf("✓ Analysis complete — %d population(s)",
                               rv$n_pops)),
      "error"   = div(class="status-error", "Error: ", rv$run_error)
    )
  })

  ## ---- Results UI ---------------------------------------------------------
  output$results_ui <- renderUI({
    req(length(rv$results) > 0)
    req(rv$run_status == "done")

    tab_panels <- lapply(seq_along(rv$results), function(i) {
      res <- rv$results[[i]]
      lbl <- rv$pop_labels[i] %||% paste("Population", i)

      ## Shared demographics / trace tabs — defined before tabPanel() call
      demo_tabs <- tagList(
        tabPanel("Weight Audit & Filter Trace", br(),
          verbatimTextOutput(paste0("trace_output_", i))),
        tabPanel("Demographics", br(),
          uiOutput(paste0("demo_ui_", i)))
      )

      ## Choose inner content based on result type
      inner_ui <- if (!is.null(res$error)) {
        div(class="status-error", "Error: ", res$error)

      } else if (isTRUE(res$type == "bkmr")) {
        ## ---- BKMR ----------------------------------------------------------
        do.call(tabsetPanel, c(
          list(
            tabPanel("Posterior Inclusion Probabilities", br(),
              div(style="font-size:.82em;color:var(--muted);margin-bottom:10px;",
                sprintf("N = %d · %d outcome(s) · %d exposure(s)",
                  res$n %||% 0L, length(res$bkmr_results %||% list()),
                  length(((res$bkmr_results %||% list())[[1]])$exposures %||% character(0)))),
              tableOutput(paste0("bkmr_pip_table_", i)),
              div(class="hint", style="font-size:.78em;margin-top:4px;margin-bottom:6px;",
                "* p < 0.05  ** p < 0.01  *** p < 0.001"), br(),
              plotOutput(paste0("bkmr_pip_plot_", i), height="280px")),
            tabPanel("Exposure-Response Curves", br(),
              uiOutput(paste0("bkmr_curves_ui_", i))),
            if (!is.null(res$convergence))
              tabPanel("Convergence Diagnostics", br(),
                div(class="hint", style="margin-bottom:8px;",
                  "R-hat < 1.1 and ESS > 100 per parameter indicate adequate convergence.",
                  " Computed across ", res$n_chains %||% 1L, " chains via bkmrhat."),
                tableOutput(paste0("bkmr_conv_table_", i)))
          ),
          demo_tabs
        ))

      } else if (res$type == "wqs") {
        ## ---- WQS -----------------------------------------------------------
        do.call(tabsetPanel, c(
          list(tabPanel("WQS Results", br(),
            tableOutput(paste0("wqs_table_", i)),
            div(class="hint", style="font-size:.78em;margin-top:4px;margin-bottom:6px;",
              "* p < 0.05  ** p < 0.01  *** p < 0.001"), br(),
            plotOutput(paste0("wqs_weights_plot_", i), height="250px"))),
          demo_tabs
        ))

      } else if (res$type == "qgcomp") {
        ## ---- qgcomp --------------------------------------------------------
        do.call(tabsetPanel, c(
          list(tabPanel("qgcomp Results", br(),
            tableOutput(paste0("qgcomp_table_", i)),
            div(class="hint", style="font-size:.78em;margin-top:4px;margin-bottom:6px;",
              "* p < 0.05  ** p < 0.01  *** p < 0.001"), br(),
            plotOutput(paste0("qgcomp_weights_plot_", i), height="260px"))),
          demo_tabs
        ))

      } else if (res$type == "mediation") {
        ## ---- Mediation -----------------------------------------------------
        do.call(tabsetPanel, c(
          list(tabPanel("Mediation Results", br(),
            div(class="hint", style="margin-bottom:8px;",
              "ACME = indirect effect · ADE = direct effect · Total = ACME + ADE"),
            tableOutput(paste0("med_table_", i)),
            div(class="hint", style="font-size:.78em;margin-top:4px;",
              "* p < 0.05  ** p < 0.01  *** p < 0.001"))),
          demo_tabs
        ))

      } else if (res$type == "multinomial") {
        ## ---- Multinomial ---------------------------------------------------
        do.call(tabsetPanel, c(
          list(tabPanel("Multinomial Results", br(),
            div(style="font-size:.82em;color:var(--muted);margin-bottom:6px;",
              "Odds ratios vs. reference level."),
            tableOutput(paste0("multi_table_", i)),
            div(class="hint", style="font-size:.78em;margin-top:4px;",
              "* p < 0.05  ** p < 0.01  *** p < 0.001"))),
          demo_tabs
        ))

      } else if (res$type == "timetrend") {
        ## ---- Time-Trend ----------------------------------------------------
        do.call(tabsetPanel, c(
          list(
            tabPanel("Trend Summary", br(),
              tableOutput(paste0("trend_table_", i)),
              div(class="hint", style="font-size:.78em;margin-top:4px;margin-bottom:6px;",
                "* p < 0.05  ** p < 0.01  *** p < 0.001"), br(),
              downloadButton(paste0("joinpoint_dl_", i),
                             "Download Joinpoint CSV",
                             class="btn-sm-teal"))
          ),
          demo_tabs
        ))

      } else {
        ## ---- Linear / Logistic / Spline ------------------------------------
        ## Spline curves live in the Plots tab; only the summary table here.
        do.call(tabsetPanel, c(
          list(tabPanel("Regression Results", br(),
            div(style="font-size:.82em;color:var(--muted);margin-bottom:10px;",
              sprintf("N = %d · %d regressions · type: %s",
                      res$n %||% 0L, nrow(res$formatted %||% data.frame()),
                      res$type %||% "linear")),
            tableOutput(paste0("results_table_", i)),
            div(class="hint", style="font-size:.78em;margin-top:4px;",
              "* p < 0.05  ** p < 0.01  *** p < 0.001"))),
          demo_tabs
        ))
      }

      warn_ui <- if (length(res$warnings) > 0) {
        div(class="status-error",
          style="margin-bottom:12px;padding:8px 12px;font-size:.83em;",
          tags$b("⚠ Warnings:"),
          tags$ul(style="margin:4px 0 0 0;padding-left:18px;",
            lapply(res$warnings, tags$li))
        )
      } else NULL

      tabPanel(lbl, br(), warn_ui, inner_ui)
    })

    div(class="panel-box",
      div(class="section-label", "Results"),
      do.call(tabsetPanel, tab_panels)
    )
  })

  lapply(1:10, function(i) {
    output[[paste0("results_table_", i)]] <- renderTable({
      req(length(rv$results) >= i)
      res <- rv$results[[i]]

      ## Use combined table when quartile results are available
      if (!is.null(res$quartile_results) && length(res$quartile_results) > 0) {
        fmt <- make_combined_results_table(res$results, res$quartile_results)
      } else {
        req(!is.null(res$formatted))
        fmt <- res$formatted
      }

      ## Determine estimate column label from coef_label, then drop it
      if ("coef_label" %in% names(fmt)) {
        est_label <- if (any(fmt$coef_label == "OR", na.rm=TRUE)) "OR" else "β₁"
        fmt$coef_label <- NULL
      } else {
        est_label <- "β₁"
      }
      ## Rename all display columns using base R (avoids any dplyr masking)
      col_map <- c(outcome="Outcome", exposure="Exposure", stratum="Stratum",
                   estimate=est_label, ci_fmt="95% CI",
                   p_fmt="p", sig_stars=" ", n="N")
      for (old in names(col_map)) {
        idx <- which(names(fmt) == old)
        if (length(idx) == 1L) names(fmt)[idx] <- col_map[[old]]
      }
      fmt
    }, striped=TRUE, hover=TRUE, bordered=FALSE)

    fmt_demo_tbl <- function(tbl) {
      req(!is.null(tbl), is.data.frame(tbl))
      tbl |> rename(Characteristic=characteristic, ` `=subcategory, Value=value)
    }

    ## Dynamic demographics UI — nested tabsetPanel when multiple pairs
    output[[paste0("demo_ui_", i)]] <- renderUI({
      req(length(rv$results) >= i)
      pd_list <- rv$results[[i]]$pair_demographics
      req(!is.null(pd_list), length(pd_list) > 0)

      make_pair_demo_ui <- function(pd, pop_idx, pair_idx) {
        n_label <- sprintf("N = %d", pd$n %||% 0L)
        tbl <- pd$tables
        tagList(
          div(style="font-size:.82em;color:var(--muted);margin-bottom:10px;", n_label),
          h5("Categorical Variables"),
          tableOutput(paste0("pdemo_cat_",  pop_idx, "_", pair_idx)), br(),
          h5("Continuous Covariates — Mean (SD)"),
          tableOutput(paste0("pdemo_cont_", pop_idx, "_", pair_idx)), br(),
          if (!is.null(tbl$exposure_summary)) tagList(
            h5("Exposure Summary"),
            tableOutput(paste0("pdemo_exp_",  pop_idx, "_", pair_idx)), br()
          ),
          h5("Outcome Variables"),
          tableOutput(paste0("pdemo_out_",  pop_idx, "_", pair_idx))
        )
      }

      if (length(pd_list) == 1) {
        make_pair_demo_ui(pd_list[[1]], i, 1)
      } else {
        pair_panels <- lapply(seq_along(pd_list), function(p) {
          pd <- pd_list[[p]]
          tabPanel(pd$label %||% paste("Pair", p),
            br(), make_pair_demo_ui(pd, i, p))
        })
        do.call(tabsetPanel, pair_panels)
      }
    })

    ## Pre-define per-pair table renderers for up to 25 pairs per population
    lapply(1:25, function(p) {
      get_pair_tables <- function(pop_i, pair_p) {
        req(length(rv$results) >= pop_i)
        pd_list <- rv$results[[pop_i]]$pair_demographics
        req(!is.null(pd_list), length(pd_list) >= pair_p)
        pd_list[[pair_p]]$tables
      }

      output[[paste0("pdemo_cat_",  i, "_", p)]] <- renderTable({
        tbl <- get_pair_tables(i, p); req(!is.null(tbl$categorical))
        fmt_demo_tbl(tbl$categorical)
      }, striped=FALSE, hover=FALSE, bordered=FALSE, na="")

      output[[paste0("pdemo_cont_", i, "_", p)]] <- renderTable({
        tbl <- get_pair_tables(i, p); req(!is.null(tbl$continuous))
        fmt_demo_tbl(tbl$continuous)
      }, striped=FALSE, hover=FALSE, bordered=FALSE, na="")

      output[[paste0("pdemo_exp_",  i, "_", p)]] <- renderTable({
        tbl <- get_pair_tables(i, p); req(!is.null(tbl$exposure_summary))
        fmt_demo_tbl(tbl$exposure_summary)
      }, striped=FALSE, hover=FALSE, bordered=FALSE, na="")

      output[[paste0("pdemo_out_",  i, "_", p)]] <- renderTable({
        tbl <- get_pair_tables(i, p); req(!is.null(tbl$outcomes))
        fmt_demo_tbl(tbl$outcomes)
      }, striped=FALSE, hover=FALSE, bordered=FALSE, na="")
    })

    output[[paste0("trace_output_", i)]] <- renderText({
      req(length(rv$results) >= i)
      res <- rv$results[[i]]
      req(!is.null(res$trace))
      res$trace
    })

    ## ---- Spline curves UI --------------------------------------------------
    output[[paste0("spline_curves_ui_", i)]] <- renderUI({
      req(length(rv$results) >= i)
      res <- rv$results[[i]]
      req(isTRUE(res$type == "spline"), !is.null(res$results))
      keys <- names(res$results)
      tagList(lapply(seq_along(keys), function(k) {
        r <- res$results[[keys[k]]]
        if (is.null(r$spline_curve)) return(NULL)

        ## Format test statistics for the header badge
        fmt_p <- function(p) if (is.na(p)) "—" else
                              if (p < 0.001) "<0.001" else sprintf("%.4f", p)
        aic_diff <- (r$spline_aic_linear %||% NA_real_) -
                    (r$spline_aic_spline  %||% NA_real_)
        aic_lbl  <- if (is.na(aic_diff)) "—" else sprintf("%.1f", aic_diff)

        pid <- paste0("spline_curve_", i, "_", k)
        div(class="panel-box", style="margin-bottom:12px;",
          ## Title row with test statistics
          div(style="display:flex;align-items:baseline;gap:18px;margin-bottom:6px;",
            h5(style="margin:0;",
               sprintf("%s ~ RCS(%s, df=%d)", r$outcome, r$exposure,
                       r$spline_df %||% 3L)),
            div(style="font-size:.82em;color:var(--muted);",
              sprintf("Global p = %s", fmt_p(r$spline_global_p))),
            div(style="font-size:.82em;color:var(--muted);",
              sprintf("Non-linearity p = %s", fmt_p(r$spline_nonlin_p))),
            div(style="font-size:.82em;color:var(--muted);",
              sprintf("ΔAIC (linear − spline) = %s", aic_lbl))
          ),
          plotOutput(pid, height="260px"),
          div(style="display:flex;gap:12px;flex-wrap:wrap;margin-top:6px;align-items:flex-end;",
            div(style="display:flex;flex-direction:column;gap:2px;",
              tags$label(style="font-size:.78em;color:var(--muted);", "X min"),
              numericInput(paste0(pid, "_xmin"), label=NULL, value=NA, width="90px")),
            div(style="display:flex;flex-direction:column;gap:2px;",
              tags$label(style="font-size:.78em;color:var(--muted);", "X max"),
              numericInput(paste0(pid, "_xmax"), label=NULL, value=NA, width="90px")),
            div(style="display:flex;flex-direction:column;gap:2px;",
              tags$label(style="font-size:.78em;color:var(--muted);", "Y min"),
              numericInput(paste0(pid, "_ymin"), label=NULL, value=NA, width="90px")),
            div(style="display:flex;flex-direction:column;gap:2px;",
              tags$label(style="font-size:.78em;color:var(--muted);", "Y max"),
              numericInput(paste0(pid, "_ymax"), label=NULL, value=NA, width="90px")),
            div(class="hint", style="margin:0;line-height:2.4;", "Leave blank for auto")
          )
        )
      }))
    })
    lapply(1:20, function(k) {
      output[[paste0("spline_curve_", i, "_", k)]] <- renderPlot({
        req(length(rv$results) >= i)
        res <- rv$results[[i]]
        req(isTRUE(res$type == "spline"), !is.null(res$results))
        keys <- names(res$results)
        req(k <= length(keys))
        r  <- res$results[[keys[k]]]
        req(!is.null(r$spline_curve))
        sc <- r$spline_curve

        pid      <- paste0("spline_curve_", i, "_", k)
        x_min    <- input[[paste0(pid, "_xmin")]]
        x_max    <- input[[paste0(pid, "_xmax")]]
        y_min    <- input[[paste0(pid, "_ymin")]]
        y_max    <- input[[paste0(pid, "_ymax")]]
        xlim_val <- c(if (is.na(x_min %||% NA_real_)) NA_real_ else x_min,
                      if (is.na(x_max %||% NA_real_)) NA_real_ else x_max)
        ylim_val <- c(if (is.na(y_min %||% NA_real_)) NA_real_ else y_min,
                      if (is.na(y_max %||% NA_real_)) NA_real_ else y_max)

        fmt_p <- function(p) if (is.na(p)) "—" else
                              if (p < 0.001) "<0.001" else sprintf("%.4f", p)
        subtitle_txt <- sprintf(
          "Global p = %s  |  Non-linearity p = %s  |  covariates held at median/mode",
          fmt_p(r$spline_global_p), fmt_p(r$spline_nonlin_p))

        ggplot(sc, aes(x=x, y=y)) +
          geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.18, fill="#2563eb") +
          geom_line(color="#2563eb", linewidth=0.9) +
          coord_cartesian(xlim = xlim_val, ylim = ylim_val) +
          labs(x     = r$exposure,
               y     = sprintf("Predicted %s", r$outcome),
               title = sprintf("Restricted cubic spline: %s ~ %s  (df = %d)",
                               r$outcome, r$exposure, r$spline_df %||% 3L),
               subtitle = subtitle_txt) +
          theme_bw(base_size=12) +
          theme(plot.background    = element_rect(fill="white", color=NA),
                panel.background   = element_rect(fill="white"),
                panel.border       = element_rect(color="black", linewidth=0.5),
                panel.grid.major   = element_line(color="#e5e7eb", linewidth=0.3),
                panel.grid.minor   = element_blank(),
                plot.subtitle      = element_text(size=9, color="grey40"),
                axis.text          = element_text(color="black"))
      }, bg="white")
    })

    ## ---- WQS results table -------------------------------------------------
    output[[paste0("wqs_table_", i)]] <- renderTable({
      req(length(rv$results) >= i)
      res <- rv$results[[i]]
      req(isTRUE(res$type == "wqs"), !is.null(res$wqs_results))
      rows <- lapply(names(res$wqs_results), function(out) {
        r <- res$wqs_results[[out]]
        data.frame(
          Outcome   = out,
          `β (WQS)` = round(r$estimate, 4),
          `95% CI`  = sprintf("[%.4f, %.4f]", r$ci_low, r$ci_high),
          p         = ifelse(r$p_value < 0.001, "<0.001", sprintf("%.4f", r$p_value)),
          ` `       = ifelse(r$p_value < 0.001, "***", ifelse(r$p_value < 0.01, "**", ifelse(r$p_value < 0.05, "*", ""))),
          Direction = r$direction,
          N         = r$n,
          check.names = FALSE, stringsAsFactors = FALSE)
      })
      do.call(rbind, rows)
    }, striped=TRUE, hover=TRUE, bordered=FALSE)

    ## ---- WQS weights bar chart ---------------------------------------------
    output[[paste0("wqs_weights_plot_", i)]] <- renderPlot({
      req(length(rv$results) >= i)
      res <- rv$results[[i]]
      req(isTRUE(res$type == "wqs"), !is.null(res$wqs_results))
      all_wt <- do.call(rbind, lapply(names(res$wqs_results), function(out) {
        wdf <- res$wqs_results[[out]]$weights
        if (is.null(wdf)) return(NULL)
        cbind(outcome=out, wdf)
      }))
      req(!is.null(all_wt), nrow(all_wt) > 0)
      ggplot(all_wt, aes(x=reorder(exposure, weight), y=weight, fill=outcome)) +
        geom_col(position="dodge", width=0.7) +
        geom_hline(yintercept=1/length(unique(all_wt$exposure)),
                   linetype="dashed", color="black", linewidth=0.4) +
        coord_flip() +
        labs(x=NULL, y="WQS weight", title="Component weights") +
        theme_bw(base_size=12) +
        theme(plot.background=element_rect(fill="white", color=NA),
              panel.background=element_rect(fill="white"),
              panel.border=element_rect(color="black", linewidth=0.5),
              panel.grid.major.x=element_line(color="#e5e7eb", linewidth=0.3),
              panel.grid.major.y=element_blank(),
              panel.grid.minor=element_blank(),
              legend.position="bottom")
    }, bg="white")

    ## ---- qgcomp results table ----------------------------------------------
    output[[paste0("qgcomp_table_", i)]] <- renderTable({
      req(length(rv$results) >= i)
      res <- rv$results[[i]]
      req(isTRUE(res$type == "qgcomp"), !is.null(res$qgcomp_results))
      rows <- lapply(names(res$qgcomp_results), function(out) {
        r <- res$qgcomp_results[[out]]
        data.frame(
          Outcome  = out,
          Method   = r$method %||% "EE",
          `ψ`      = round(r$psi, 4),
          `95% CI` = sprintf("[%.4f, %.4f]", r$ci_low, r$ci_high),
          p        = ifelse(r$p_value < 0.001, "<0.001", sprintf("%.4f", r$p_value)),
          ` `      = ifelse(r$p_value < 0.001, "***", ifelse(r$p_value < 0.01, "**", ifelse(r$p_value < 0.05, "*", ""))),
          N        = r$n,
          check.names = FALSE, stringsAsFactors = FALSE)
      })
      do.call(rbind, rows)
    }, striped=TRUE, hover=TRUE, bordered=FALSE)

    ## ---- qgcomp weights plot -----------------------------------------------
    output[[paste0("qgcomp_weights_plot_", i)]] <- renderPlot({
      req(length(rv$results) >= i)
      res <- rv$results[[i]]
      req(isTRUE(res$type == "qgcomp"), !is.null(res$qgcomp_results))
      all_wt <- do.call(rbind, lapply(names(res$qgcomp_results), function(out) {
        wdf <- res$qgcomp_results[[out]]$weights
        if (is.null(wdf) || nrow(wdf) == 0) return(NULL)
        cbind(outcome=out, wdf)
      }))
      req(!is.null(all_wt), nrow(all_wt) > 0)
      all_wt$dir_label <- ifelse(all_wt$direction == "positive", "Positive", "Negative")
      ggplot(all_wt, aes(x=reorder(exposure, weight), y=weight, fill=dir_label)) +
        geom_col(width=0.7) +
        coord_flip() +
        scale_fill_manual(values=c("Positive"="#f97316","Negative"="#2563eb"), name=NULL) +
        geom_hline(yintercept=0, color="black", linewidth=0.4) +
        facet_wrap(~outcome) +
        labs(x=NULL, y="qgcomp weight", title="Directional component weights") +
        theme_bw(base_size=12) +
        theme(plot.background=element_rect(fill="white", color=NA),
              panel.background=element_rect(fill="white"),
              panel.border=element_rect(color="black", linewidth=0.5),
              panel.grid.major.x=element_line(color="#e5e7eb", linewidth=0.3),
              panel.grid.major.y=element_blank(),
              panel.grid.minor=element_blank(),
              legend.position="bottom")
    }, bg="white")

    ## ---- Mediation results table -------------------------------------------
    output[[paste0("med_table_", i)]] <- renderTable({
      req(length(rv$results) >= i)
      res <- rv$results[[i]]
      req(isTRUE(res$type == "mediation"), !is.null(res$mediation_results))
      rows <- lapply(names(res$mediation_results), function(key) {
        r <- res$mediation_results[[key]]
        data.frame(
          Outcome   = r$outcome,
          Exposure  = r$exposure,
          Mediator  = r$mediator,
          `ACME`    = round(r$acme, 4),
          `ACME CI` = sprintf("[%.4f, %.4f]", r$acme_ci[1], r$acme_ci[2]),
          `ACME p`  = ifelse(r$acme_p < 0.001, "<0.001", sprintf("%.4f", r$acme_p)),
          ` `       = ifelse(r$acme_p < 0.001, "***", ifelse(r$acme_p < 0.01, "**", ifelse(r$acme_p < 0.05, "*", ""))),
          `ADE`     = round(r$ade,  4),
          `ADE p`   = ifelse(r$ade_p  < 0.001, "<0.001", sprintf("%.4f", r$ade_p)),
          `  `      = ifelse(r$ade_p  < 0.001, "***", ifelse(r$ade_p  < 0.01, "**", ifelse(r$ade_p  < 0.05, "*", ""))),
          `Total`   = round(r$total, 4),
          `Prop. mediated` = round(r$prop_mediated, 3),
          N         = r$n,
          check.names = FALSE, stringsAsFactors = FALSE)
      })
      do.call(rbind, rows)
    }, striped=TRUE, hover=TRUE, bordered=FALSE)

    ## ---- Multinomial results table -----------------------------------------
    output[[paste0("multi_table_", i)]] <- renderTable({
      req(length(rv$results) >= i)
      res <- rv$results[[i]]
      req(isTRUE(res$type == "multinomial"), !is.null(res$multinomial_results))
      rows <- lapply(names(res$multinomial_results), function(key) {
        r <- res$multinomial_results[[key]]

        ## Select only rows belonging to the exposure variable (skip intercept/covariates).
        ## For categorical exposures the terms are like "VARNAME<level>"; for continuous
        ## there is exactly one term equal to the exposure name.
        is_cat <- isTRUE(r$exp_is_cat)
        if (is_cat) {
          exp_rows <- r$coef_table[startsWith(r$coef_table$term, r$exposure), ]
          ## Parse the exposure level out of the term name (strip the variable prefix)
          exp_level_vals <- sub(paste0("^", r$exposure), "", exp_rows$term)
        } else {
          exp_rows <- r$coef_table[r$coef_table$term == r$exposure, ]
          exp_level_vals <- rep("(continuous)", nrow(exp_rows))
        }
        if (nrow(exp_rows) == 0) return(NULL)

        ## Build clear column labels:
        ##   Outcome variable / Outcome level (vs ref) / Exposure variable /
        ##   Exposure level (vs ref) / OR / CI / p / N
        data.frame(
          `Outcome var`       = r$outcome,
          `Outcome level`     = sprintf("%s  (ref = %s)", exp_rows$level, r$ref_level),
          `Exposure var`      = r$exposure,
          `Exposure level`    = if (is_cat)
                                  sprintf("%s  (ref = %s)", exp_level_vals, r$exp_ref_level)
                                else
                                  "(continuous)",
          OR                  = round(exp_rows$OR, 3),
          `95% CI`            = sprintf("[%.3f, %.3f]", exp_rows$ci_low, exp_rows$ci_high),
          p                   = ifelse(exp_rows$p_value < 0.001, "<0.001",
                                       sprintf("%.4f", exp_rows$p_value)),
          ` `                 = ifelse(exp_rows$p_value < 0.001, "***",
                                  ifelse(exp_rows$p_value < 0.01, "**",
                                  ifelse(exp_rows$p_value < 0.05, "*", ""))),
          N                   = r$n,
          check.names = FALSE, stringsAsFactors = FALSE)
      })
      do.call(rbind, Filter(Negate(is.null), rows))
    }, striped=TRUE, hover=TRUE, bordered=FALSE)

    ## ---- Time-trend: summary table -----------------------------------------
    output[[paste0("trend_table_", i)]] <- renderTable({
      req(length(rv$results) >= i)
      res <- rv$results[[i]]
      req(isTRUE(res$type == "timetrend"), !is.null(res$trend_results))
      rows <- lapply(names(res$trend_results), function(vv) {
        r   <- res$trend_results[[vv]]
        lbl <- r$change_label %||% "% Change/Year"
        is_pct <- r$transform %||% "none" != "none"
        ci_fmt <- if (is_pct)
          sprintf("[%.2f%%, %.2f%%]", r$pct_change_ci[1], r$pct_change_ci[2])
        else
          sprintf("[%.4f, %.4f]",     r$pct_change_ci[1], r$pct_change_ci[2])
        df <- data.frame(
          Variable       = vv,
          `Trend p`      = ifelse(r$trend_p < 0.001, "<0.001",
                                  sprintf("%.4f", r$trend_p)),
          ` `            = ifelse(r$trend_p < 0.001, "***",
                            ifelse(r$trend_p < 0.01, "**",
                            ifelse(r$trend_p < 0.05, "*", ""))),
          `N cycles`     = sum(r$cycle_data$status == "observed"),
          `N (analytic)` = r$n_analytic %||% NA_integer_,
          check.names = FALSE, stringsAsFactors = FALSE
        )
        df[[lbl]]      <- round(r$pct_change_per_year, 4)
        df[["95% CI"]] <- ci_fmt
        df[, c("Variable", lbl, "95% CI", "Trend p", " ",
               "N cycles", "N (analytic)")]
      })
      do.call(rbind, rows)
    }, striped=TRUE, hover=TRUE, bordered=FALSE)

    ## ---- Time-trend: Joinpoint CSV download --------------------------------
    output[[paste0("joinpoint_dl_", i)]] <- downloadHandler(
      filename = function() {
        sprintf("joinpoint_export_pop%d_%s.csv", i,
                format(Sys.Date(), "%Y%m%d"))
      },
      content = function(file) {
        res <- rv$results[[i]]
        req(isTRUE(res$type == "timetrend"), !is.null(res$trend_results))
        rows <- lapply(names(res$trend_results), function(vv) {
          jp <- res$trend_results[[vv]]$joinpoint_data
          if (is.null(jp)) return(NULL)
          jp$Variable <- vv
          jp[, c("Variable", "Cycle", "Mid_Year", "N",
                 "Arith_Mean", "Arith_SE", "Geom_Mean", "SE_log_GM")]
        })
        combined <- do.call(rbind, Filter(Negate(is.null), rows))
        write.csv(combined, file, row.names=FALSE)
      }
    )

    ## ---- BKMR: PIP table ---------------------------------------------------
    output[[paste0("bkmr_pip_table_", i)]] <- renderTable({
      req(length(rv$results) >= i)
      res <- rv$results[[i]]
      req(isTRUE(res$type == "bkmr"), !is.null(res$bkmr_results))

      rows <- lapply(names(res$bkmr_results), function(out) {
        r <- res$bkmr_results[[out]]
        if (is.null(r$pips)) return(NULL)
        df <- data.frame(
          Outcome  = out,
          Exposure = r$pips$exposure,
          PIP      = round(r$pips$pip, 3),
          stringsAsFactors = FALSE
        )
        ## Merge per-exposure R-hat and ESS when bkmrhat was used
        if (!is.null(r$pip_conv)) {
          df <- merge(df, r$pip_conv, by.x = "Exposure", by.y = "exposure",
                      all.x = TRUE, sort = FALSE)
          df <- df[match(r$pips$exposure, df$Exposure), ]
        }
        df
      })
      do.call(rbind, Filter(Negate(is.null), rows))
    }, striped=TRUE, hover=TRUE, bordered=FALSE)

    ## ---- BKMR: PIP bar chart -----------------------------------------------
    output[[paste0("bkmr_pip_plot_", i)]] <- renderPlot({
      req(length(rv$results) >= i)
      res <- rv$results[[i]]
      req(isTRUE(res$type == "bkmr"), !is.null(res$bkmr_results))

      rows <- lapply(names(res$bkmr_results), function(out) {
        r <- res$bkmr_results[[out]]
        if (is.null(r$pips)) return(NULL)
        data.frame(outcome  = out,
                   exposure = r$pips$exposure,
                   pip      = r$pips$pip,
                   stringsAsFactors = FALSE)
      })
      pip_df <- do.call(rbind, Filter(Negate(is.null), rows))
      req(!is.null(pip_df), nrow(pip_df) > 0)

      ggplot(pip_df, aes(x = reorder(exposure, pip),
                          y = pip,
                          fill = pip >= 0.5)) +
        geom_col(width = 0.65) +
        geom_hline(yintercept = 0.5, linetype = "dashed",
                   color = "black", linewidth = 0.4) +
        coord_flip() +
        scale_fill_manual(
          values = c("FALSE" = "#9ca3af", "TRUE" = "#f97316"),
          labels = c("FALSE" = "PIP < 0.5", "TRUE" = "PIP ≥ 0.5"),
          name   = NULL
        ) +
        scale_y_continuous(limits = c(0, 1),
                           breaks = c(0, 0.25, 0.5, 0.75, 1)) +
        facet_wrap(~outcome) +
        labs(x = NULL, y = "Posterior Inclusion Probability") +
        theme_bw(base_size = 12) +
        theme(
          plot.background    = element_rect(fill = "white", color = NA),
          panel.background   = element_rect(fill = "white"),
          panel.border       = element_rect(color = "black", linewidth = 0.5),
          panel.grid.major.x = element_line(color = "#e5e7eb", linewidth = 0.3),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text          = element_text(color = "black", size = 10),
          legend.position    = "bottom",
          legend.text        = element_text(size = 9)
        )
    }, bg = "white")

    ## ---- BKMR: exposure-response container (one plot per outcome) ----------
    output[[paste0("bkmr_curves_ui_", i)]] <- renderUI({
      req(length(rv$results) >= i)
      res <- rv$results[[i]]
      req(isTRUE(res$type == "bkmr"), !is.null(res$bkmr_results))
      outcomes <- names(res$bkmr_results)
      n_exp    <- length(
        (res$bkmr_results[[outcomes[1]]])$exposures %||% character(0))
      height_px <- paste0(max(260, n_exp * 160 + 80), "px")

      tagList(lapply(seq_along(outcomes), function(oi) {
        div(class = "panel-box", style = "margin-bottom:10px;",
          h5(outcomes[oi]),
          plotOutput(paste0("bkmr_curve_", i, "_", oi), height = height_px)
        )
      }))
    })

    ## Register up to 10 curve plots per population
    lapply(1:10, function(oi) {
      output[[paste0("bkmr_curve_", i, "_", oi)]] <- renderPlot({
        req(length(rv$results) >= i)
        res      <- rv$results[[i]]
        req(isTRUE(res$type == "bkmr"), !is.null(res$bkmr_results))
        outcomes <- names(res$bkmr_results)
        req(oi <= length(outcomes))

        bkmr_r <- res$bkmr_results[[outcomes[oi]]]
        curves  <- extract_bkmr_curves(bkmr_r)
        req(!is.null(curves), nrow(curves) > 0)

        ggplot(curves, aes(x = z, y = est)) +
          geom_ribbon(aes(ymin = lower, ymax = upper),
                      alpha = 0.2, fill = "#2563eb") +
          geom_line(color = "#2563eb", linewidth = 0.8) +
          geom_hline(yintercept = 0, linetype = "dashed",
                     color = "black", linewidth = 0.4) +
          facet_wrap(~exposure, scales = "free_x") +
          labs(x = "Exposure value",
               y = sprintf("h(%s)", outcomes[oi])) +
          theme_bw(base_size = 12) +
          theme(
            plot.background    = element_rect(fill = "white", color = NA),
            panel.background   = element_rect(fill = "white"),
            panel.border       = element_rect(color = "black", linewidth = 0.5),
            panel.grid.major   = element_line(color = "#e5e7eb", linewidth = 0.3),
            panel.grid.minor   = element_blank(),
            axis.text          = element_text(color = "black", size = 10),
            strip.text         = element_text(color = "black", face = "bold"),
            strip.background   = element_rect(fill = "#f0ede6")
          )
      }, bg = "white")
    })

    ## ---- BKMR: convergence diagnostics table ---------------------------------
    output[[paste0("bkmr_conv_table_", i)]] <- renderTable({
      req(length(rv$results) >= i)
      res <- rv$results[[i]]
      req(isTRUE(res$type == "bkmr"), !is.null(res$convergence))
      res$convergence
    }, digits = 3)

  })

  outputOptions(output, "results_ui", suspendWhenHidden=FALSE)

  ## ---- Plots tab -----------------------------------------------------------

  ## Population selector (only shown when multiple populations exist)
  output$plot_pop_selector_ui <- renderUI({
    req(rv$run_status == "done", length(rv$results) > 0)
    if (rv$n_pops <= 1) return(NULL)
    selectInput("plot_pop_idx", "Population",
      choices  = setNames(seq_len(rv$n_pops), rv$pop_labels[seq_len(rv$n_pops)]),
      selected = 1
    )
  })

  ## Time-trend plot size sliders (only shown when result type is timetrend)
  output$trend_plot_sliders_ui <- renderUI({
    req(rv$run_status == "done", length(rv$results) > 0)
    idx <- as.integer(input$plot_pop_idx %||% 1)
    req(idx >= 1, idx <= length(rv$results))
    res <- rv$results[[idx]]
    if (!isTRUE(res$type == "timetrend")) return(NULL)
    tagList(
      sliderInput("trend_plot_height", "Plot height (px)",
        min=200, max=700, value=380, step=20, ticks=FALSE),
      sliderInput("trend_plot_width", "Plot width (%)",
        min=30, max=100, value=100, step=5, ticks=FALSE),
      sliderInput("trend_plot_font_size", "Font size (pt)",
        min=8, max=24, value=12, step=1, ticks=FALSE)
    )
  })

  ## Linear/logistic forest plot size sliders
  output$linear_plot_sliders_ui <- renderUI({
    req(rv$run_status == "done", length(rv$results) > 0)
    idx <- as.integer(input$plot_pop_idx %||% 1)
    req(idx >= 1, idx <= length(rv$results))
    res <- rv$results[[idx]]
    if (!isTRUE(res$type %in% c("linear", "logistic"))) return(NULL)
    tagList(
      sliderInput("linear_plot_height", "Plot height (px)",
        min=150, max=900, value=380, step=20, ticks=FALSE),
      sliderInput("linear_plot_width", "Plot width (%)",
        min=30, max=100, value=100, step=5, ticks=FALSE)
    )
  })

  ## Render the container divs — branches on result type
  output$forest_plots_ui <- renderUI({
    req(rv$run_status == "done", length(rv$results) > 0)
    idx <- as.integer(input$plot_pop_idx %||% 1)
    req(idx >= 1, idx <= length(rv$results))
    res <- rv$results[[idx]]
    req(is.null(res$error))

    if (isTRUE(res$type == "bkmr")) {
      ## ---- BKMR: PIP bar + exposure-response per outcome -------------------
      req(!is.null(res$bkmr_results), length(res$bkmr_results) > 0)
      outcomes <- names(res$bkmr_results)
      n_exp    <- length((res$bkmr_results[[outcomes[1]]])$exposures %||% character(0))
      curve_h  <- paste0(max(260, n_exp * 160 + 80), "px")
      tagList(lapply(seq_along(outcomes), function(k) {
        tagList(
          div(class="panel-box",
            h4(sprintf("Posterior Inclusion Probabilities — %s", outcomes[k])),
            plotOutput(paste0("plots_pip_", k), height="230px")),
          div(class="panel-box",
            h4(sprintf("Exposure-Response Curves — %s", outcomes[k])),
            plotOutput(paste0("plots_curves_", k), height=curve_h))
        )
      }))

    } else if (res$type %in% c("wqs", "qgcomp")) {
      ## ---- WQS / qgcomp: weights plots per outcome -------------------------
      result_key <- if (res$type == "wqs") "wqs_results" else "qgcomp_results"
      r_list <- res[[result_key]]
      req(!is.null(r_list), length(r_list) > 0)
      outcomes <- names(r_list)
      tagList(lapply(seq_along(outcomes), function(k) {
        div(class="panel-box",
          h4(sprintf("%s weights — %s", toupper(res$type), outcomes[k])),
          plotOutput(paste0("plots_mixture_wt_", k), height="250px"))
      }))

    } else if (res$type == "timetrend") {
      ## ---- Time-trend: one plot per variable --------------------------------
      req(!is.null(res$trend_results), length(res$trend_results) > 0)
      vars <- names(res$trend_results)
      ph <- paste0(input$trend_plot_height %||% 380L, "px")
      pw <- paste0(input$trend_plot_width  %||% 100L, "%")
      tagList(lapply(seq_along(vars), function(k) {
        div(class="panel-box",
          h5(vars[k]),
          div(style=paste0("width:", pw, ";"),
            plotOutput(paste0("plots_trend_", k), height=ph, width="100%")))
      }))

    } else if (isTRUE(res$type == "spline")) {
      ## ---- Spline: one curve panel per exposure × outcome --------------------
      req(!is.null(res$results), length(res$results) > 0)
      keys <- names(res$results)
      keys_with_curve <- keys[vapply(keys, function(k)
        !is.null(res$results[[k]]$spline_curve), logical(1))]
      if (length(keys_with_curve) == 0)
        return(div(class="panel-box",
          p(style="color:var(--muted);",
            "Spline curves could not be computed — check the R console for warnings.")))

      tagList(lapply(seq_along(keys_with_curve), function(k) {
        r <- res$results[[keys_with_curve[k]]]
        fmt_p <- function(p) if (is.na(p %||% NA_real_)) "—" else
                              if (p < 0.001) "<0.001" else sprintf("%.4f", p)
        aic_diff <- (r$spline_aic_linear %||% NA_real_) -
                    (r$spline_aic_spline  %||% NA_real_)
        aic_lbl  <- if (is.na(aic_diff)) "—" else sprintf("%.1f", aic_diff)
        pid <- paste0("plots_spline_curve_", k)
        div(class="panel-box", style="margin-bottom:12px;",
          div(style="display:flex;align-items:baseline;gap:18px;margin-bottom:6px;",
            h5(style="margin:0;",
               sprintf("%s ~ RCS(%s, df=%d)", r$outcome, r$exposure,
                       r$spline_df %||% 3L)),
            div(style="font-size:.82em;color:var(--muted);",
              sprintf("Global p = %s", fmt_p(r$spline_global_p))),
            div(style="font-size:.82em;color:var(--muted);",
              sprintf("Non-linearity p = %s", fmt_p(r$spline_nonlin_p))),
            div(style="font-size:.82em;color:var(--muted);",
              sprintf("ΔAIC (linear − spline) = %s", aic_lbl))
          ),
          plotOutput(pid, height="260px"),
          div(style="display:flex;gap:12px;flex-wrap:wrap;margin-top:6px;align-items:flex-end;",
            div(style="display:flex;flex-direction:column;gap:2px;",
              tags$label(style="font-size:.78em;color:var(--muted);", "X min"),
              numericInput(paste0(pid, "_xmin"), label=NULL, value=NA, width="90px")),
            div(style="display:flex;flex-direction:column;gap:2px;",
              tags$label(style="font-size:.78em;color:var(--muted);", "X max"),
              numericInput(paste0(pid, "_xmax"), label=NULL, value=NA, width="90px")),
            div(style="display:flex;flex-direction:column;gap:2px;",
              tags$label(style="font-size:.78em;color:var(--muted);", "Y min"),
              numericInput(paste0(pid, "_ymin"), label=NULL, value=NA, width="90px")),
            div(style="display:flex;flex-direction:column;gap:2px;",
              tags$label(style="font-size:.78em;color:var(--muted);", "Y max"),
              numericInput(paste0(pid, "_ymax"), label=NULL, value=NA, width="90px")),
            div(class="hint", style="margin:0;line-height:2.4;", "Leave blank for auto")
          )
        )
      }))

    } else {
      ## ---- Linear / Logistic: forest plots per outcome ----------------------
      req(!is.null(res$results), length(res$results) > 0)
      tidy      <- results_grid_to_tidy(res$results)
      req(nrow(tidy) > 0)
      outcomes  <- unique(tidy$outcome)
      n_exp     <- length(unique(tidy$exposure))
      auto_h    <- max(220, n_exp * 55 + 100)
      ph        <- paste0(input$linear_plot_height %||% auto_h, "px")
      pw        <- paste0(input$linear_plot_width  %||% 100L, "%")
      tagList(lapply(seq_along(outcomes), function(k) {
        div(class="panel-box",
          div(style=paste0("width:", pw, ";"),
            plotOutput(paste0("forest_plot_", k), height=ph, width="100%")))
      }))
    }
  })

  ## Pre-register up to 20 slots — handles both linear forest plots
  ## and BKMR PIP / curve plots depending on the current result type.
  lapply(1:20, function(k) {

    ## -- Linear regression: forest plot -------------------------------------
    output[[paste0("forest_plot_", k)]] <- renderPlot({
      req(rv$run_status == "done", length(rv$results) > 0)
      idx <- as.integer(input$plot_pop_idx %||% 1)
      req(idx >= 1, idx <= length(rv$results))
      res <- rv$results[[idx]]
      req(is.null(res$error))
      req(is.null(res$type) || res$type %in% c("linear","logistic","spline"))
      req(!is.null(res$results), length(res$results) > 0)

      tidy     <- results_grid_to_tidy(res$results)
      req(nrow(tidy) > 0)
      outcomes <- unique(tidy$outcome)
      req(k <= length(outcomes))

      out <- outcomes[k]
      sub <- tidy[tidy$outcome == out, ]
      sub$color_group <- ifelse(sub$p_value >= 0.05, "ns",
                          ifelse(sub$estimate  < 0,  "neg", "pos"))

      null_val <- if (isTRUE(res$type == "logistic")) 1 else 0
      ggplot(sub, aes(x=estimate, y=exposure, color=color_group)) +
        geom_vline(xintercept=null_val, linetype="dashed",
                   color="black", linewidth=0.4) +
        geom_errorbarh(aes(xmin=ci_low, xmax=ci_high),
                       height=0.25, linewidth=0.7) +
        geom_point(size=3.5) +
        scale_color_manual(
          values = c("ns"  = "#9ca3af",
                     "neg" = "#2563eb",
                     "pos" = "#f97316"),
          labels = c("ns"  = "p ≥ 0.05",
                     "neg" = "p < 0.05  (negative β)",
                     "pos" = "p < 0.05  (positive β)"),
          name = NULL
        ) +
        labs(x=if(isTRUE(res$type=="logistic")) "OR (95% CI)" else "β (95% CI)",
             y=NULL, title=out) +
        theme_bw(base_size=12) +
        theme(
          plot.background    = element_rect(fill="white", color=NA),
          panel.background   = element_rect(fill="white"),
          panel.grid.major.x = element_line(color="#e5e7eb", linewidth=0.3),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          panel.border       = element_rect(color="black", linewidth=0.5),
          axis.text          = element_text(color="black", size=10),
          axis.title.x       = element_text(color="black", size=10),
          plot.title         = element_text(color="black", size=13, face="bold",
                                            margin=margin(b=8)),
          legend.position    = "bottom",
          legend.text        = element_text(size=9),
          legend.key         = element_rect(fill="white")
        )
    }, bg="white")

    ## -- Spline: exposure-response curve (Plots tab) -------------------------
    output[[paste0("plots_spline_curve_", k)]] <- renderPlot({
      req(rv$run_status == "done", length(rv$results) > 0)
      idx <- as.integer(input$plot_pop_idx %||% 1)
      req(idx >= 1, idx <= length(rv$results))
      res <- rv$results[[idx]]
      req(isTRUE(res$type == "spline"), !is.null(res$results))
      keys <- names(res$results)
      keys_with_curve <- keys[vapply(keys, function(kk)
        !is.null(res$results[[kk]]$spline_curve), logical(1))]
      req(k <= length(keys_with_curve))
      r  <- res$results[[keys_with_curve[k]]]
      sc <- r$spline_curve
      req(!is.null(sc))

      pid      <- paste0("plots_spline_curve_", k)
      x_min    <- input[[paste0(pid, "_xmin")]]
      x_max    <- input[[paste0(pid, "_xmax")]]
      y_min    <- input[[paste0(pid, "_ymin")]]
      y_max    <- input[[paste0(pid, "_ymax")]]
      xlim_val <- c(if (is.na(x_min %||% NA_real_)) NA_real_ else x_min,
                    if (is.na(x_max %||% NA_real_)) NA_real_ else x_max)
      ylim_val <- c(if (is.na(y_min %||% NA_real_)) NA_real_ else y_min,
                    if (is.na(y_max %||% NA_real_)) NA_real_ else y_max)

      fmt_p <- function(p) if (is.na(p %||% NA_real_)) "—" else
                            if (p < 0.001) "<0.001" else sprintf("%.4f", p)
      subtitle_txt <- sprintf(
        "Global p = %s  |  Non-linearity p = %s  |  covariates held at median/mode",
        fmt_p(r$spline_global_p), fmt_p(r$spline_nonlin_p))
      ggplot(sc, aes(x=x, y=y)) +
        geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.18, fill="#2563eb") +
        geom_line(color="#2563eb", linewidth=0.9) +
        coord_cartesian(xlim = xlim_val, ylim = ylim_val) +
        labs(x     = r$exposure,
             y     = sprintf("Predicted %s", r$outcome),
             title = sprintf("Restricted cubic spline: %s ~ %s  (df = %d)",
                             r$outcome, r$exposure, r$spline_df %||% 3L),
             subtitle = subtitle_txt) +
        theme_bw(base_size=12) +
        theme(plot.background    = element_rect(fill="white", color=NA),
              panel.background   = element_rect(fill="white"),
              panel.border       = element_rect(color="black", linewidth=0.5),
              panel.grid.major   = element_line(color="#e5e7eb", linewidth=0.3),
              panel.grid.minor   = element_blank(),
              plot.subtitle      = element_text(size=9, color="grey40"),
              plot.title         = element_text(size=12, face="bold"))
    }, bg="white")

    ## -- WQS / qgcomp: mixture weights per outcome (Plots tab) -------------
    output[[paste0("plots_mixture_wt_", k)]] <- renderPlot({
      req(rv$run_status == "done", length(rv$results) > 0)
      idx <- as.integer(input$plot_pop_idx %||% 1)
      req(idx >= 1, idx <= length(rv$results))
      res <- rv$results[[idx]]
      req(res$type %in% c("wqs", "qgcomp"))
      r_list <- if (res$type == "wqs") res$wqs_results else res$qgcomp_results
      req(!is.null(r_list)); outcomes <- names(r_list); req(k <= length(outcomes))
      r   <- r_list[[outcomes[k]]]
      wdf <- r$weights
      req(!is.null(wdf), nrow(wdf) > 0)
      fill_col <- if (res$type == "wqs") "#f97316" else
                   ifelse(wdf$direction == "positive", "#f97316", "#2563eb")
      ggplot(wdf, aes(x=reorder(exposure, weight), y=weight)) +
        geom_col(fill=fill_col, width=0.7) +
        coord_flip() +
        labs(x=NULL, y=if (res$type == "wqs") "WQS weight" else "qgcomp weight",
             title=outcomes[k]) +
        theme_bw(base_size=12) +
        theme(plot.background=element_rect(fill="white", color=NA),
              panel.background=element_rect(fill="white"),
              panel.border=element_rect(color="black", linewidth=0.5),
              panel.grid.major.x=element_line(color="#e5e7eb", linewidth=0.3),
              panel.grid.major.y=element_blank(),
              panel.grid.minor=element_blank())
    }, bg="white")

    ## -- Time-trend: plots (Plots tab) --------------------------------------
    output[[paste0("plots_trend_", k)]] <- renderPlot({
      req(rv$run_status == "done", length(rv$results) > 0)
      idx <- as.integer(input$plot_pop_idx %||% 1)
      req(idx >= 1, idx <= length(rv$results))
      res <- rv$results[[idx]]
      req(isTRUE(res$type == "timetrend"), !is.null(res$trend_results))
      vars <- names(res$trend_results); req(k <= length(vars))
      r   <- res$trend_results[[vars[k]]]
      cd  <- r$cycle_data
      tl  <- r$trend_line
      has_tl  <- !is.null(tl) && nrow(tl) > 0
      base_fs <- input$trend_plot_font_size %||% 12L

      pct_lbl <- if (!is.na(r$pct_change_per_year %||% NA_real_))
                   sprintf("%.2f%% per year (p = %.4f)",
                           r$pct_change_per_year, r$trend_p %||% NA_real_)
                 else "trend unavailable"

      cd_obs  <- cd[!is.na(cd$status) & cd$status == "observed", ]
      cd_miss <- cd[!is.na(cd$status) & cd$status == "missing_middle", ]

      ## Build bridge segments: connect last observed before gap to first after
      ## by drawing straight lines through the missing years at interpolated y.
      bridge_segs <- NULL
      if (nrow(cd_miss) > 0) {
        all_years  <- cd$year[order(cd$year)]
        all_means  <- cd$mean  ## NA for missing
        obs_idx    <- which(!is.na(all_means))
        bridge_rows <- lapply(which(is.na(all_means)), function(mi) {
          prev_obs <- max(obs_idx[obs_idx < mi], na.rm=TRUE)
          next_obs <- min(obs_idx[obs_idx > mi], na.rm=TRUE)
          if (is.infinite(prev_obs) || is.infinite(next_obs)) return(NULL)
          x0 <- all_years[prev_obs]; y0 <- all_means[prev_obs]
          x1 <- all_years[next_obs]; y1 <- all_means[next_obs]
          xm <- all_years[mi]
          data.frame(x=xm, xend=xm, y=y0, yend=y1,
                     xprev=x0, yprev=y0, xnext=x1, ynext=y1)
        })
        bridge_rows <- Filter(Negate(is.null), bridge_rows)
        if (length(bridge_rows) > 0)
          bridge_segs <- do.call(rbind, bridge_rows)
      }

      ## Dummy data frames for manual legend entries
      leg_obs  <- data.frame(x=NA_real_, y=NA_real_, series="Cycle mean (survey-wtd)")
      leg_tl   <- data.frame(x=NA_real_, y=NA_real_, series="Linear trend (regression)")
      leg_miss <- data.frame(x=NA_real_, y=NA_real_, series="Missing cycle (gap marker)")

      col_map   <- c("Cycle mean (survey-wtd)"="blue",
                     "Linear trend (regression)"="#f97316",
                     "Missing cycle (gap marker)"="#9ca3af")
      shape_map <- c("Cycle mean (survey-wtd)"=19,
                     "Linear trend (regression)"=NA,
                     "Missing cycle (gap marker)"=1)
      ltype_map <- c("Cycle mean (survey-wtd)"="solid",
                     "Linear trend (regression)"="solid",
                     "Missing cycle (gap marker)"="blank")
      active <- c("Cycle mean (survey-wtd)",
                  if (has_tl)          "Linear trend (regression)",
                  if (nrow(cd_miss)>0) "Missing cycle (gap marker)")

      p <- ggplot() +
        ## CI ribbon for observed cycles only
        geom_ribbon(data=cd_obs,
                    aes(x=year, ymin=ci_low, ymax=ci_high),
                    alpha=0.15, fill="blue", show.legend=FALSE) +
        ## Bridge lines connecting across missing middle cycles
        {if (!is.null(bridge_segs))
           geom_segment(data=bridge_segs,
                        aes(x=xprev, xend=xnext, y=yprev, yend=ynext),
                        color="blue", linewidth=0.6, linetype="dashed",
                        show.legend=FALSE)
         else list()} +
        ## Blue observed line
        geom_line(data=cd_obs,
                  aes(x=year, y=mean, color="Cycle mean (survey-wtd)",
                      linetype="Cycle mean (survey-wtd)"),
                  linewidth=0.8) +
        ## Blue observed points
        geom_point(data=cd_obs,
                   aes(x=year, y=mean, color="Cycle mean (survey-wtd)",
                       shape="Cycle mean (survey-wtd)"),
                   size=3) +
        ## Grey open circles at y=0 for missing middle cycles
        {if (nrow(cd_miss) > 0)
           geom_point(data=cd_miss,
                      aes(x=year, y=0,
                          color="Missing cycle (gap marker)",
                          shape="Missing cycle (gap marker)"),
                      size=3)
         else list()}

      if (has_tl)
        p <- p + geom_line(data=tl,
                           aes(x=year, y=fitted,
                               color="Linear trend (regression)",
                               linetype="Linear trend (regression)"),
                           linewidth=1)

      p +
        scale_color_manual(name=NULL, values=col_map[active],   breaks=active) +
        scale_shape_manual(name=NULL, values=shape_map[active], breaks=active, guide="none") +
        scale_linetype_manual(name=NULL, values=ltype_map[active], breaks=active, guide="none") +
        guides(color = guide_legend(override.aes=list(
          shape    = unname(shape_map[active]),
          linetype = unname(ltype_map[active]),
          linewidth = c(0.8, if (has_tl) 1, if (nrow(cd_miss)>0) 0)[seq_along(active)]
        ))) +
        labs(x="NHANES Cycle (start year)", y=vars[k],
             title=vars[k], subtitle=paste("Avg. change:", pct_lbl)) +
        scale_x_continuous(breaks=cd$year) +
        theme_bw(base_size=base_fs) +
        theme(plot.background=element_rect(fill="white", color=NA),
              panel.background=element_rect(fill="white"),
              panel.border=element_rect(color="black", linewidth=0.5),
              panel.grid.major=element_line(color="#e5e7eb", linewidth=0.3),
              panel.grid.minor=element_blank(),
              axis.text.x=element_text(angle=45, hjust=1),
              legend.position="bottom",
              legend.key=element_rect(fill="white"),
              legend.text=element_text(size=base_fs * 0.75))
    }, bg="white")

    ## -- BKMR: PIP bar chart (Plots tab) ------------------------------------
    output[[paste0("plots_pip_", k)]] <- renderPlot({
      req(rv$run_status == "done", length(rv$results) > 0)
      idx <- as.integer(input$plot_pop_idx %||% 1)
      req(idx >= 1, idx <= length(rv$results))
      res <- rv$results[[idx]]
      req(isTRUE(res$type == "bkmr"), !is.null(res$bkmr_results))
      outcomes <- names(res$bkmr_results)
      req(k <= length(outcomes))

      bkmr_r <- res$bkmr_results[[outcomes[k]]]
      pip_df <- bkmr_r$pips
      req(!is.null(pip_df), nrow(pip_df) > 0)

      ggplot(pip_df, aes(x=reorder(exposure, pip), y=pip, fill=pip >= 0.5)) +
        geom_col(width=0.65) +
        geom_hline(yintercept=0.5, linetype="dashed",
                   color="black", linewidth=0.4) +
        coord_flip() +
        scale_fill_manual(
          values = c("FALSE"="#9ca3af", "TRUE"="#f97316"),
          labels = c("FALSE"="PIP < 0.5", "TRUE"="PIP ≥ 0.5"),
          name   = NULL
        ) +
        scale_y_continuous(limits=c(0,1),
                           breaks=c(0, 0.25, 0.5, 0.75, 1)) +
        labs(x=NULL, y="Posterior Inclusion Probability",
             title=outcomes[k]) +
        theme_bw(base_size=12) +
        theme(
          plot.background    = element_rect(fill="white", color=NA),
          panel.background   = element_rect(fill="white"),
          panel.border       = element_rect(color="black", linewidth=0.5),
          panel.grid.major.x = element_line(color="#e5e7eb", linewidth=0.3),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text          = element_text(color="black", size=10),
          plot.title         = element_text(color="black", size=13, face="bold"),
          legend.position    = "bottom",
          legend.text        = element_text(size=9)
        )
    }, bg="white")

    ## -- BKMR: exposure-response curves (Plots tab) -------------------------
    output[[paste0("plots_curves_", k)]] <- renderPlot({
      req(rv$run_status == "done", length(rv$results) > 0)
      idx <- as.integer(input$plot_pop_idx %||% 1)
      req(idx >= 1, idx <= length(rv$results))
      res <- rv$results[[idx]]
      req(isTRUE(res$type == "bkmr"), !is.null(res$bkmr_results))
      outcomes <- names(res$bkmr_results)
      req(k <= length(outcomes))

      bkmr_r <- res$bkmr_results[[outcomes[k]]]
      curves  <- extract_bkmr_curves(bkmr_r)
      req(!is.null(curves), nrow(curves) > 0)

      ggplot(curves, aes(x=z, y=est)) +
        geom_ribbon(aes(ymin=lower, ymax=upper),
                    alpha=0.2, fill="#2563eb") +
        geom_line(color="#2563eb", linewidth=0.8) +
        geom_hline(yintercept=0, linetype="dashed",
                   color="black", linewidth=0.4) +
        facet_wrap(~exposure, scales="free_x") +
        labs(x="Exposure value",
             y=sprintf("h(%s)", outcomes[k]),
             title=sprintf("Exposure-response — %s", outcomes[k])) +
        theme_bw(base_size=12) +
        theme(
          plot.background    = element_rect(fill="white", color=NA),
          panel.background   = element_rect(fill="white"),
          panel.border       = element_rect(color="black", linewidth=0.5),
          panel.grid.major   = element_line(color="#e5e7eb", linewidth=0.3),
          panel.grid.minor   = element_blank(),
          axis.text          = element_text(color="black", size=10),
          strip.text         = element_text(color="black", face="bold"),
          strip.background   = element_rect(fill="#f0ede6"),
          plot.title         = element_text(color="black", size=13, face="bold")
        )
    }, bg="white")
  })

  ## ---- R Script tab -------------------------------------------------------

  ## Reactive script — regenerates whenever any analysis input changes
  rscript_text <- reactive({
    ## Touch enough reactive values to invalidate when settings change
    rv$exposure_rows; rv$outcome_rows; rv$n_pops; rv$pop_labels
    input$analysis_type; input$shared_covariates
    input$spline_df; input$wqs_q; input$wqs_boot; input$wqs_seed
    input$qgcomp_q; input$qgcomp_seed; input$qgcomp_bootstrap; input$qgcomp_boot
    input$bkmr_iter; input$bkmr_seed
    input$mediators
    input$complete_cases_only
    tryCatch(
      generate_r_script(input, rv, pop_constraints),
      error = function(e) paste("## Error generating script:\n##", conditionMessage(e))
    )
  })

  output$rscript_preview_ui <- renderText({
    rscript_text()
  })

  output$download_rscript_btn <- downloadHandler(
    filename = function() {
      nm <- gsub("[^A-Za-z0-9_-]", "_",
                 trimws(input$project_name_input %||% "nhanes_analysis"))
      if (!nzchar(nm)) nm <- "nhanes_analysis"
      paste0(nm, "_", format(Sys.Date(), "%Y%m%d"), ".R")
    },
    content = function(file) {
      writeLines(isolate(rscript_text()), file)
    }
  )
}

## ============================================================================
shinyApp(ui=ui, server=server)
