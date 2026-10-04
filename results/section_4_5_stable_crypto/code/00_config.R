options(stringsAsFactors = FALSE, digits = 15, warn = 1, scipen = 999)

resolve_bundle_dir <- function() {
  args <- commandArgs(FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (length(hit)) {
    p <- normalizePath(sub("^--file=", "", hit[1]), mustWork = FALSE)
    candidate <- if (basename(dirname(p)) == "code") dirname(dirname(p)) else dirname(p)
    if (file.exists(file.path(candidate, "code", "00_config.R"))) return(candidate)
  }
  candidate <- normalizePath(getwd(), mustWork = TRUE)
  if (file.exists(file.path(candidate, "code", "00_config.R"))) return(candidate)
  stop("Run from the Section 4.5 bundle or call a script inside code/.")
}

bundle_dir <- resolve_bundle_dir()
code_dir <- file.path(bundle_dir, "code")
config_dir <- file.path(bundle_dir, "config")
input_dir <- file.path(bundle_dir, "source_data", "input")
prepared_dir <- file.path(bundle_dir, "source_data", "prepared")
crypto_frm_dir <- file.path(bundle_dir, "source_data", "frm_crypto")
results_dir <- file.path(bundle_dir, "results")
tables_dir <- file.path(bundle_dir, "tables")
figures_dir <- file.path(bundle_dir, "figures")
qa_dir <- file.path(bundle_dir, "qa")
invisible(lapply(
  c(prepared_dir, crypto_frm_dir, results_dir, tables_dir, figures_dir, qa_dir),
  dir.create, recursive = TRUE, showWarnings = FALSE
))

repository_root <- normalizePath(file.path(bundle_dir, "..", ".."), mustWork = TRUE)
coingecko_root <- Sys.getenv(
  "COINGECKO_DATA_ROOT",
  file.path(repository_root, "data", "raw", "coingecko_subset")
)
asset_file <- file.path(config_dir, "crypto_asset_universe.csv")
algorithm_file <- file.path(code_dir, "FRM_Statistics_Algorithm.R")

sample_start <- as.Date("2020-01-01")
# The author's non-stablecoin local export ends on 6 April 2026. This still
# contains the complete 0-to-30-day Hormuz window ending 30 March 2026.
sample_end <- as.Date("2026-04-06")
window_length <- 90L
tau <- 0.05
path_steps <- 25L
seed_base <- 20260531L
numerical_sensitivity_ratio <- 20

forecast_horizons <- c(`10 days` = 10L, `5 weeks` = 35L,
                       `3 months` = 90L, `5 months` = 150L)
oos_initial_window <- 63L
oos_schemes <- c("expanding", "rolling_63")

requested_cores <- suppressWarnings(as.integer(Sys.getenv("FRM_CORES", "")))
if (!is.finite(requested_cores) || requested_cores < 1L) {
  detected <- parallel::detectCores(logical = FALSE)
  if (!is.finite(detected)) detected <- 1L
  requested_cores <- max(1L, min(4L, detected - 1L))
}
if (.Platform$OS.type == "windows") requested_cores <- 1L

write_csv <- function(x, path, quote = TRUE) {
  write.csv(x, path, row.names = FALSE, quote = quote, na = "", fileEncoding = "UTF-8")
}

hac_mean_test <- function(x, lag) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 10L) return(c(estimate = NA, se = NA, z = NA, p_two = NA, p_one = NA))
  u <- x - mean(x)
  lag <- min(as.integer(lag), n - 1L)
  gamma0 <- sum(u * u) / n
  lrv <- gamma0
  if (lag > 0L) {
    for (k in seq_len(lag)) {
      g <- sum(u[(k + 1L):n] * u[1L:(n - k)]) / n
      lrv <- lrv + 2 * (1 - k / (lag + 1)) * g
    }
  }
  se <- sqrt(max(lrv, 0) / n)
  z <- if (se > 0) mean(x) / se else NA_real_
  c(
    estimate = mean(x), se = se, z = z,
    p_two = if (is.finite(z)) 2 * pnorm(-abs(z)) else NA_real_,
    p_one = if (is.finite(z)) pnorm(z, lower.tail = FALSE) else NA_real_
  )
}

message("Section 4.5 bundle: ", bundle_dir)
