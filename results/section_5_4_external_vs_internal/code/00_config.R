options(stringsAsFactors = FALSE, digits = 15, warn = 1, scipen = 999)

resolve_bundle_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg)) {
    code_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1L])))
    return(dirname(code_dir))
  }
  candidate <- normalizePath(getwd())
  if (basename(candidate) == "code") return(dirname(candidate))
  candidate
}

bundle_dir <- resolve_bundle_dir()
code_dir <- file.path(bundle_dir, "code")
input_dir <- file.path(bundle_dir, "source_data", "input")
source_dir <- file.path(bundle_dir, "source_data")
table_dir <- file.path(bundle_dir, "tables")
figure_dir <- file.path(bundle_dir, "figures")
qa_dir <- file.path(bundle_dir, "qa")
reference_dir <- file.path(bundle_dir, "reference_code")
invisible(lapply(
  c(source_dir, table_dir, figure_dir, qa_dir, reference_dir),
  dir.create, recursive = TRUE, showWarnings = FALSE
))

peg_file <- file.path(input_dir, "peg_panel_reference_adjusted.csv")
macro_file <- file.path(input_dir, "Stable_Macro_20260531.csv")
frozen_coefficient_file <- file.path(input_dir, "selected_coefficients_strict.csv.gz")
imputation_file <- file.path(input_dir, "output_window_imputation_flags.csv")
algorithm_file <- file.path(reference_dir, "FRM_Statistics_Algorithm.R")

required_inputs <- c(
  peg_file, macro_file, frozen_coefficient_file, imputation_file,
  algorithm_file
)
if (!all(file.exists(required_inputs))) {
  stop("Missing required input(s): ",
       paste(required_inputs[!file.exists(required_inputs)], collapse = "; "))
}

local_library <- Sys.getenv("FRM_R_LIBRARY", "")
if (!nzchar(local_library)) {
  workspace_root <- normalizePath(file.path(bundle_dir, "..", ".."),
                                  mustWork = FALSE)
  local_library <- file.path(
    workspace_root, "work", "section_4_5_stable_crypto", "r_library"
  )
}
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))

coin_order <- c(
  "USDC", "BUSD", "GUSD", "EURS", "IDRT", "USDT",
  "sUSD", "PAXG", "TUSD", "USDP", "DAI"
)
macro_order <- c(
  "BV010082.Index", "CVIX.Index", "DXY.Curncy", "SPX.Index",
  "VIX.Index"
)

date_start <- as.Date("2020-01-01")
date_end <- as.Date("2026-05-31")
output_start <- as.Date("2020-04-01")
window_length <- 90L
tau <- 0.05
path_steps <- 25L
active_tolerance <- 1e-10
hac_lag <- 90L
seed_base <- 20260531L

requested_cores <- suppressWarnings(as.integer(Sys.getenv("FRM_CORES", "")))
if (!is.finite(requested_cores) || requested_cores < 1L) {
  detected <- parallel::detectCores(logical = FALSE)
  if (!is.finite(detected)) detected <- 1L
  requested_cores <- max(1L, min(4L, detected - 1L))
}
if (.Platform$OS.type == "windows") requested_cores <- 1L

max_dates <- suppressWarnings(as.integer(Sys.getenv("FRM_MAX_DATES", "")))
if (!is.finite(max_dates) || max_dates < 1L) max_dates <- Inf

write_csv <- function(x, path, quote = TRUE) {
  write.csv(
    x, path, row.names = FALSE, quote = quote, na = "",
    fileEncoding = "UTF-8"
  )
}

write_csv_gz <- function(x, path, quote = TRUE) {
  con <- gzfile(path, open = "wt", compression = 9)
  on.exit(close(con), add = TRUE)
  write.csv(x, con, row.names = FALSE, quote = quote, na = "")
}
