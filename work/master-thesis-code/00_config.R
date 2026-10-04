# Configuration for the strict 2026-05-31 FRM replication.
# The statistical settings match the user's FRM_SC_Study code.

resolve_project_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg)) {
    arg_path <- sub("^--file=", "", file_arg[1])
    if (!identical(arg_path, "-") && file.exists(arg_path)) {
      candidate <- dirname(normalizePath(arg_path))
      if (file.exists(file.path(candidate, "00_config.R"))) return(candidate)
    }
  }
  candidate <- normalizePath(getwd())
  if (file.exists(file.path(candidate, "00_config.R"))) return(candidate)
  stop("Open the project folder or run run_all.R from that folder.")
}

project_dir <- resolve_project_dir()
input_dir <- file.path(project_dir, "data", "input")
results_dir <- file.path(project_dir, "results")
figures_dir <- file.path(project_dir, "figures")
logs_dir <- file.path(project_dir, "logs")
adjacency_dir <- file.path(results_dir, "adjacency")
invisible(lapply(
  c(input_dir, results_dir, figures_dir, logs_dir, adjacency_dir),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

price_file <- file.path(input_dir, "Stable_Price_20260531.csv")
mktcap_file <- file.path(input_dir, "Stable_Mktcap_20260531.csv")
macro_file <- file.path(input_dir, "Stable_Macro_20260531.csv")
algorithm_file <- file.path(project_dir, "FRM_Statistics_Algorithm.R")

date_start_source <- as.Date("2020-01-02")
date_end_source <- as.Date("2026-05-31")
window_length <- 90L
tau <- 0.05
path_steps <- 25L
n_stablecoins <- 11L
n_macro_factors <- 5L

stablecoin_names <- c(
  "usd_coin", "binance_usd", "gemini_dollar", "stasis_eurs",
  "rupiah_token", "tether", "nusd", "pax_gold", "true_usd",
  "paxos_standard", "dai"
)
macro_names <- c(
  "BV010082.Index", "CVIX.Index", "DXY.Curncy", "SPX.Index",
  "VIX.Index"
)

# The original algorithm adds random jitter only when target observations tie.
# A fixed date-specific seed makes the same formula reproducible across runs.
seed_base <- 20260531L

# This threshold is used only for the separately labelled numerical-sensitivity
# series. It never changes the strict minimum-GACV baseline.
numerical_sensitivity_ratio <- 20

requested_cores <- suppressWarnings(as.integer(Sys.getenv("FRM_CORES", "")))
if (!is.finite(requested_cores) || requested_cores < 1L) {
  detected <- parallel::detectCores(logical = FALSE)
  if (!is.finite(detected)) detected <- 1L
  requested_cores <- max(1L, min(4L, detected - 1L))
}
if (.Platform$OS.type == "windows") requested_cores <- 1L

# A single RDS containing all daily adjacency matrices is always produced.
# Set FRM_EXPORT_ADJ_CSV=true only when downstream legacy scripts require one
# CSV per day; this creates 2,252 additional files.
export_adjacency_csv <- identical(
  tolower(Sys.getenv("FRM_EXPORT_ADJ_CSV", "false")),
  "true"
)

options(stringsAsFactors = FALSE, digits = 15, warn = 1)
