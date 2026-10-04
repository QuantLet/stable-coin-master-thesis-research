# ==============================================================================
# FRM_SC_peg_descriptives.R
# Chapter 4.1: Descriptive Evidence on Stablecoin Peg Stability
# ==============================================================================
# Purpose
#   1. Read and harmonise the author's local CoinGecko stablecoin files.
#   2. Construct reference-adjusted peg deviations at daily frequency.
#   3. Measure typical deviation, downside-tail deviation, depeg frequency,
#      episode count, and maximum episode duration.
#   4. Export thesis-ready source data, Table 4.1, diagnostics, and figures.
#
# Important methodological boundary
#   This script analyses PRICE LEVELS relative to the designated reference asset.
#   It does not estimate the return-based Financial Risk Meter (FRM), Quantile
#   Lasso, lambda, active sets, or adjacency matrices. Those remain in the
#   original FRM_SC pipeline and begin in Chapter 4.2.
#
# Dependencies
#   Base R only. Tested with R 4.4.2. No package installation is required.
#
# Final-mode safeguards
#   By default, the script stops unless:
#     - all 11 stablecoin series are usable through 31 May 2026;
#     - EURUSD, IDRUSD/USDIDR, and XAUUSD benchmark CSVs are available;
#     - every CoinGecko identifier has been mapped to the intended thesis asset.
#   For a preliminary audit of currently available data, run with:
#     FRM_SC_ALLOW_PARTIAL=1 Rscript FRM_SC_peg_descriptives.R
#   Preliminary output is labelled and must not be reported as the final
#   eleven-coin analysis.
#
# Optional environment variables
#   FRM_SC_DATA_ROOT              CoinGecko root directory; defaults to the
#                                 project-local copy when it is available
#   FRM_SC_PROJECT_ROOT           project/output root
#   FRM_SC_BENCHMARK_DIR          benchmark CSV directory
#   FRM_SC_OUTPUT_DIR             Chapter 4.1 output directory
#   FRM_SC_ALLOW_PARTIAL          1 = preliminary partial output; default 0
#   FRM_SC_MAIN_THRESHOLD         main downside threshold; default 0.01 (1%)
#   FRM_SC_THRESHOLD_SET          comma-separated sensitivity thresholds
#                                 default 0.005,0.01,0.02
#   FRM_SC_MAX_BENCHMARK_FFILL    maximum forward-fill gap in days; default 5
# ============================================================================== 

options(stringsAsFactors = FALSE, scipen = 999, warn = 1)

# ---- 1. Configuration ---------------------------------------------------------

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (length(hit)) {
    return(dirname(normalizePath(sub("^--file=", "", hit[1]), mustWork = FALSE)))
  }
  normalizePath(getwd(), mustWork = FALSE)
}

env_flag <- function(name, default = FALSE) {
  raw <- Sys.getenv(name, unset = if (default) "1" else "0")
  tolower(trimws(raw)) %in% c("1", "true", "yes", "y")
}

env_num <- function(name, default) {
  raw <- Sys.getenv(name, unset = as.character(default))
  val <- suppressWarnings(as.numeric(raw))
  if (!is.finite(val)) stop(name, " must be numeric; received: ", raw)
  val
}

parse_threshold_set <- function(x) {
  out <- suppressWarnings(as.numeric(strsplit(x, ",", fixed = TRUE)[[1]]))
  out <- sort(unique(out[is.finite(out) & out > 0 & out < 1]))
  if (!length(out)) stop("FRM_SC_THRESHOLD_SET contains no valid threshold.")
  out
}

script_dir <- get_script_dir()
project_root <- Sys.getenv("FRM_SC_PROJECT_ROOT", unset = script_dir)
local_data_root <- file.path(project_root, "Input", "Stable", "CoinGecko_Local")
data_root <- Sys.getenv(
  "FRM_SC_DATA_ROOT",
  unset = if (dir.exists(local_data_root)) {
    local_data_root
  } else {
    file.path(project_root, "..", "..", "data", "raw", "coingecko_subset")
  }
)
benchmark_dir <- Sys.getenv(
  "FRM_SC_BENCHMARK_DIR",
  unset = file.path(project_root, "Input", "Stable", "Benchmarks")
)
output_dir <- Sys.getenv(
  "FRM_SC_OUTPUT_DIR",
  unset = file.path(project_root, "Output", "Stable", "Chapter4_1")
)

sample_start <- as.Date("2020-01-01")
sample_end <- as.Date("2026-05-31")
main_threshold <- env_num("FRM_SC_MAIN_THRESHOLD", 0.01)
thresholds <- parse_threshold_set(Sys.getenv(
  "FRM_SC_THRESHOLD_SET", unset = "0.005,0.01,0.02"
))
if (!main_threshold %in% thresholds) thresholds <- sort(unique(c(thresholds, main_threshold)))

max_benchmark_ffill_days <- as.integer(env_num("FRM_SC_MAX_BENCHMARK_FFILL", 5))
allow_partial <- env_flag("FRM_SC_ALLOW_PARTIAL", FALSE)
minimum_price_coverage <- 0.98

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(benchmark_dir, recursive = TRUE, showWarnings = FALSE)

message("Chapter 4.1 peg-stability analysis")
message("  sample:       ", sample_start, " to ", sample_end)
message("  data root:    ", normalizePath(data_root, mustWork = FALSE))
message("  benchmark dir:", normalizePath(benchmark_dir, mustWork = FALSE))
message("  output dir:   ", normalizePath(output_dir, mustWork = FALSE))
message("  final mode:   ", if (allow_partial) "NO (preliminary partial audit)" else "YES")

if (!dir.exists(data_root)) stop("CoinGecko data root does not exist: ", data_root)
if (!(main_threshold > 0 && main_threshold < 1)) {
  stop("FRM_SC_MAIN_THRESHOLD must lie strictly between 0 and 1.")
}
if (max_benchmark_ffill_days < 0) {
  stop("FRM_SC_MAX_BENCHMARK_FFILL must be non-negative.")
}

# CoinGecko IDs and economic reference assets used in the thesis.
coin_map <- data.frame(
  coin_id = c(
    "tether", "usd-coin", "dai", "binance-usd", "true-usd",
    "paxos-standard", "gemini-dollar", "pax-gold", "stasis-eurs",
    "nusd", "rupiah-token"
  ),
  symbol = c(
    "USDT", "USDC", "DAI", "BUSD", "TUSD", "USDP", "GUSD",
    "PAXG", "EURS", "sUSD", "IDRT"
  ),
  benchmark_key = c(
    "USD", "USD", "USD", "USD", "USD", "USD", "USD",
    "XAUUSD", "EURUSD", "USD", "IDRUSD"
  ),
  benchmark_definition = c(
    rep("1 US dollar", 7),
    "US dollars per fine troy ounce of gold",
    "US dollars per euro",
    "1 US dollar",
    "US dollars per Indonesian rupiah"
  ),
  identifier_status = rep("confirmed", 11),
  stringsAsFactors = FALSE
)

# Explicit sources prevent accidental inclusion of thousands of unrelated coins.
# Higher priority wins when the same coin/date appears in multiple downloads.
source_map <- data.frame(
  source_name = c(
    "history_to_2025_03_02",
    "extended_to_2025_09_09",
    "segment_2025_09_10_to_2026_01_10",
    "segment_2026_01_11_to_2026_04_06",
    "daily_update_to_2026_05_31",
    "official_coin_gecko_historical_supplement"
  ),
  directory = c(
    file.path(data_root, "coin_gecko"),
    file.path(data_root, "coin_gecko_20250910_112201"),
    file.path(data_root, "20260110_133530_start_20250909_end_20260110"),
    file.path(data_root, "20260408_133530_coingecko_start_20260111_end_20260406"),
    file.path(data_root, "20260531_130117_stablecoins"),
    file.path(project_root, "Input", "Stable", "CoinGecko_Supplement")
  ),
  priority = c(10L, 20L, 30L, 40L, 100L, 200L),
  stringsAsFactors = FALSE
)

# ---- 2. Reusable helpers ------------------------------------------------------

write_csv <- function(x, path) {
  write.csv(x, path, row.names = FALSE, na = "", quote = TRUE, fileEncoding = "UTF-8")
}

safe_numeric <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  suppressWarnings(as.numeric(gsub(",", "", trimws(as.character(x)), fixed = TRUE)))
}

parse_datetime_utc <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
  formats <- c(
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%dT%H:%M:%OS%z",
    "%Y-%m-%d",
    "%m/%d/%Y %H:%M:%OS",
    "%m/%d/%Y"
  )
  for (fmt in formats) {
    miss <- is.na(out) & !is.na(x)
    if (!any(miss)) break
    candidate <- suppressWarnings(as.POSIXct(x[miss], format = fmt, tz = "UTC"))
    out[which(miss)] <- candidate
  }
  out
}

last_valid_row <- function(df) {
  ok <- is.finite(df$price_usd) & df$price_usd > 0
  if (!any(ok)) return(NULL)
  df <- df[ok, , drop = FALSE]
  df <- df[order(df$timestamp_utc, df$row_in_file), , drop = FALSE]
  df[nrow(df), , drop = FALSE]
}

read_coingecko_file <- function(path, coin_id, source_name, priority) {
  if (!file.exists(path)) return(NULL)
  raw <- tryCatch(
    read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) stop("Could not read ", path, ": ", conditionMessage(e))
  )
  required <- c("date", "prices")
  if (!all(required %in% names(raw))) {
    stop("CoinGecko file lacks required columns date/prices: ", path)
  }
  timestamp <- parse_datetime_utc(raw$date)
  date <- as.Date(timestamp, tz = "UTC")
  out <- data.frame(
    coin_id = coin_id,
    timestamp_utc = timestamp,
    date = date,
    price_usd = safe_numeric(raw$prices),
    market_cap_usd = if ("market_caps" %in% names(raw)) safe_numeric(raw$market_caps) else NA_real_,
    volume_usd = if ("total_volumes" %in% names(raw)) safe_numeric(raw$total_volumes) else NA_real_,
    source_name = source_name,
    source_file = normalizePath(path, mustWork = FALSE),
    source_priority = as.integer(priority),
    row_in_file = seq_len(nrow(raw)),
    stringsAsFactors = FALSE
  )
  out <- out[
    !is.na(out$date) & out$date >= sample_start & out$date <= sample_end,
    , drop = FALSE
  ]
  if (!nrow(out)) return(NULL)

  # CoinGecko short-range downloads may be hourly. Select the final valid UTC
  # observation for each date. total_volumes is a rolling 24-hour measure and
  # is therefore selected from the same row, not summed across the day.
  by_day <- split(out, out$date, drop = TRUE)
  daily <- lapply(by_day, last_valid_row)
  daily <- daily[!vapply(daily, is.null, logical(1))]
  if (!length(daily)) return(NULL)
  daily <- do.call(rbind, daily)
  rownames(daily) <- NULL
  daily
}

combine_coin_sources <- function(coin_id) {
  pieces <- vector("list", nrow(source_map))
  for (i in seq_len(nrow(source_map))) {
    path <- file.path(source_map$directory[i], paste0(coin_id, ".csv"))
    pieces[[i]] <- read_coingecko_file(
      path = path,
      coin_id = coin_id,
      source_name = source_map$source_name[i],
      priority = source_map$priority[i]
    )
  }
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (!length(pieces)) return(NULL)
  all <- do.call(rbind, pieces)
  all <- all[order(all$date, all$source_priority, all$timestamp_utc), , drop = FALSE]

  # The highest-priority source wins for overlapping dates. This ensures the
  # final daily update supersedes older or hourly extracts without averaging
  # incompatible sampling timestamps.
  keep <- !duplicated(all$date, fromLast = TRUE)
  out <- all[keep, , drop = FALSE]
  out <- out[order(out$date), , drop = FALSE]
  rownames(out) <- NULL
  out
}

detect_date_column <- function(df) {
  nms <- names(df)
  candidates <- c("date", "Date", "DATE", "time", "Time", "timestamp", "Timestamp")
  hit <- candidates[candidates %in% nms]
  if (length(hit)) return(hit[1])
  nms[1]
}

detect_value_column <- function(df, date_col) {
  nms <- setdiff(names(df), date_col)
  preferred <- c(
    "value", "Value", "VALUE", "price", "Price", "PRICE", "close", "Close",
    "CLOSE", "adj_close", "Adj.Close", "rate", "Rate", "RATE",
    "EURUSD", "IDRUSD", "USDIDR", "XAUUSD"
  )
  hit <- preferred[preferred %in% nms]
  if (length(hit)) return(hit[1])
  numeric_counts <- vapply(nms, function(nm) sum(is.finite(safe_numeric(df[[nm]]))), integer(1))
  if (!length(numeric_counts) || max(numeric_counts) == 0) {
    stop("No numeric benchmark value column found.")
  }
  names(which.max(numeric_counts))
}

write_benchmark_template <- function(file_name, definition) {
  path <- file.path(benchmark_dir, file_name)
  if (!file.exists(path)) {
    template <- data.frame(
      date = c("2019-12-31", "2020-01-02"),
      value = c(NA_real_, NA_real_),
      definition = c(definition, definition),
      stringsAsFactors = FALSE
    )
    write_csv(template, path)
  }
  path
}

benchmark_candidates <- list(
  EURUSD = c("EURUSD.csv", "eurusd.csv", "EUR_USD.csv"),
  IDRUSD = c("IDRUSD.csv", "idrusd.csv", "IDR_USD.csv"),
  USDIDR = c("USDIDR.csv", "usdidr.csv", "USD_IDR.csv"),
  XAUUSD = c("XAUUSD.csv", "xauusd.csv", "XAU_USD.csv")
)

find_first_file <- function(names) {
  paths <- file.path(benchmark_dir, names)
  hit <- paths[file.exists(paths)]
  if (length(hit)) hit[1] else NA_character_
}

read_one_benchmark_file <- function(path, key, invert = FALSE) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  raw <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(raw)) return(NULL)
  date_col <- detect_date_column(raw)
  value_col <- detect_value_column(raw, date_col)
  date <- as.Date(parse_datetime_utc(raw[[date_col]]), tz = "UTC")
  value <- safe_numeric(raw[[value_col]])
  if (invert) value <- ifelse(is.finite(value) & value > 0, 1 / value, NA_real_)
  out <- data.frame(
    date = date,
    benchmark_value = value,
    benchmark_observed = is.finite(value) & value > 0,
    benchmark_source = normalizePath(path, mustWork = FALSE),
    benchmark_key = key,
    stringsAsFactors = FALSE
  )
  out <- out[
    !is.na(out$date) & is.finite(out$benchmark_value) & out$benchmark_value > 0,
    , drop = FALSE
  ]
  if (!nrow(out)) return(NULL)
  out <- out[order(out$date), , drop = FALSE]
  out[!duplicated(out$date, fromLast = TRUE), , drop = FALSE]
}

read_benchmark_raw <- function(key) {
  if (key == "USD") {
    return(data.frame(
      date = seq(sample_start, sample_end, by = "day"),
      benchmark_value = 1,
      benchmark_observed = TRUE,
      benchmark_source = "Fixed USD reference",
      benchmark_key = "USD",
      stringsAsFactors = FALSE
    ))
  }

  out <- NULL
  if (key == "IDRUSD") {
    # Try direct IDRUSD files first. Empty templates do not block a valid
    # USDIDR file; the latter is inverted to obtain US dollars per rupiah.
    for (nm in benchmark_candidates$IDRUSD) {
      out <- read_one_benchmark_file(file.path(benchmark_dir, nm), key, invert = FALSE)
      if (!is.null(out)) break
    }
    if (is.null(out)) {
      for (nm in benchmark_candidates$USDIDR) {
        out <- read_one_benchmark_file(file.path(benchmark_dir, nm), key, invert = TRUE)
        if (!is.null(out)) break
      }
    }
  } else {
    for (nm in benchmark_candidates[[key]]) {
      out <- read_one_benchmark_file(file.path(benchmark_dir, nm), key, invert = FALSE)
      if (!is.null(out)) break
    }
  }
  if (is.null(out)) return(NULL)

  # Economic orientation checks prevent silent reciprocal errors.
  med <- median(out$benchmark_value, na.rm = TRUE)
  if (key == "EURUSD" && !(med > 0.5 && med < 2.0)) {
    stop("EURUSD must be US dollars per euro; implausible median: ", signif(med, 6))
  }
  if (key == "IDRUSD" && !(med > 0 && med < 0.01)) {
    stop("IDRUSD must be US dollars per rupiah; implausible median: ", signif(med, 6))
  }
  if (key == "XAUUSD" && !(med > 100 && med < 20000)) {
    stop("XAUUSD must be US dollars per fine troy ounce; implausible median: ", signif(med, 6))
  }
  out
}

forward_fill_benchmark <- function(observed, key) {
  calendar <- data.frame(date = seq(sample_start, sample_end, by = "day"))
  x <- merge(calendar, observed, by = "date", all.x = TRUE, sort = TRUE)
  if (!"benchmark_key" %in% names(x)) x$benchmark_key <- key
  x$benchmark_key[is.na(x$benchmark_key)] <- key

  actual <- is.finite(x$benchmark_value) & x$benchmark_value > 0
  last_value <- NA_real_
  last_date <- as.Date(NA)
  filled <- rep(NA_real_, nrow(x))
  days_since_observed <- rep(NA_integer_, nrow(x))
  for (i in seq_len(nrow(x))) {
    if (actual[i]) {
      last_value <- x$benchmark_value[i]
      last_date <- x$date[i]
      filled[i] <- last_value
      days_since_observed[i] <- 0L
    } else if (is.finite(last_value)) {
      gap <- as.integer(x$date[i] - last_date)
      if (gap <= max_benchmark_ffill_days) {
        filled[i] <- last_value
        days_since_observed[i] <- gap
      }
    }
  }
  x$benchmark_value <- filled
  x$benchmark_observed <- actual
  x$benchmark_forward_filled <- !actual & is.finite(filled)
  x$days_since_benchmark_observation <- days_since_observed
  x$benchmark_source[is.na(x$benchmark_source)] <- ifelse(
    x$benchmark_forward_filled[is.na(x$benchmark_source)],
    "forward-filled from preceding observation",
    NA_character_
  )
  x
}

safe_quantile <- function(x, p) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE, type = 7))
}

safe_median <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else median(x)
}

safe_iqr <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else IQR(x, type = 7)
}

extract_depeg_events <- function(df, threshold) {
  df <- df[order(df$date), , drop = FALSE]
  flag <- is.finite(df$signed_deviation) & df$signed_deviation <= -threshold
  if (!any(flag)) {
    return(data.frame(
      coin_id = character(), symbol = character(), threshold = numeric(),
      start_date = as.Date(character()), end_date = as.Date(character()),
      duration_days = integer(), minimum_deviation = numeric(),
      minimum_deviation_bps = numeric(), stringsAsFactors = FALSE
    ))
  }
  prior_flag <- c(FALSE, head(flag, -1))
  prior_date <- c(as.Date(NA), head(df$date, -1))
  new_run <- flag & (!prior_flag | is.na(prior_date) | as.integer(df$date - prior_date) != 1L)
  run_id <- cumsum(new_run)
  event_rows <- df[flag, , drop = FALSE]
  event_rows$run_id <- run_id[flag]
  groups <- split(event_rows, event_rows$run_id)
  out <- lapply(groups, function(g) {
    data.frame(
      coin_id = g$coin_id[1],
      symbol = g$symbol[1],
      threshold = threshold,
      start_date = min(g$date),
      end_date = max(g$date),
      duration_days = nrow(g),
      minimum_deviation = min(g$signed_deviation, na.rm = TRUE),
      minimum_deviation_bps = 10000 * min(g$signed_deviation, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

# ---- 3. Create benchmark templates and load available benchmarks -------------

template_paths <- c(
  write_benchmark_template("EURUSD.csv", "US dollars per euro"),
  write_benchmark_template("IDRUSD.csv", "US dollars per Indonesian rupiah; alternatively provide USDIDR.csv"),
  write_benchmark_template("XAUUSD.csv", "US dollars per fine troy ounce of gold")
)

benchmark_keys <- unique(coin_map$benchmark_key)
benchmarks <- setNames(vector("list", length(benchmark_keys)), benchmark_keys)
benchmark_diagnostics <- list()

for (key in benchmark_keys) {
  raw <- read_benchmark_raw(key)
  if (is.null(raw)) {
    benchmarks[[key]] <- NULL
    benchmark_diagnostics[[key]] <- data.frame(
      benchmark_key = key,
      status = "missing",
      source = NA_character_,
      first_observation = as.Date(NA),
      last_observation = as.Date(NA),
      n_observed = 0L,
      n_usable_after_forward_fill = 0L,
      stringsAsFactors = FALSE
    )
  } else {
    full <- forward_fill_benchmark(raw, key)
    benchmarks[[key]] <- full
    benchmark_diagnostics[[key]] <- data.frame(
      benchmark_key = key,
      status = "available",
      source = paste(unique(raw$benchmark_source), collapse = "; "),
      first_observation = min(raw$date),
      last_observation = max(raw$date),
      n_observed = nrow(raw),
      n_usable_after_forward_fill = sum(is.finite(full$benchmark_value)),
      stringsAsFactors = FALSE
    )
  }
}

benchmark_diagnostics <- do.call(rbind, benchmark_diagnostics)
rownames(benchmark_diagnostics) <- NULL
write_csv(benchmark_diagnostics, file.path(output_dir, "benchmark_diagnostics.csv"))

# ---- 4. Load and harmonise the 11 stablecoin series --------------------------

coin_panels <- vector("list", nrow(coin_map))
coverage_rows <- vector("list", nrow(coin_map))
expected_days <- as.integer(sample_end - sample_start) + 1L

for (i in seq_len(nrow(coin_map))) {
  info <- coin_map[i, , drop = FALSE]
  panel <- combine_coin_sources(info$coin_id)
  if (is.null(panel) || !nrow(panel)) {
    coverage_rows[[i]] <- data.frame(
      coin_id = info$coin_id,
      symbol = info$symbol,
      benchmark_key = info$benchmark_key,
      identifier_status = info$identifier_status,
      first_price_date = as.Date(NA),
      last_price_date = as.Date(NA),
      n_price_days = 0L,
      expected_days = expected_days,
      price_coverage_rate = 0,
      internal_missing_days = expected_days,
      reaches_sample_end = FALSE,
      benchmark_available = !is.null(benchmarks[[info$benchmark_key]]),
      analysis_status = "missing_stablecoin_price",
      stringsAsFactors = FALSE
    )
    next
  }

  panel$symbol <- info$symbol
  panel$benchmark_key <- info$benchmark_key
  panel$identifier_status <- info$identifier_status
  benchmark_ok <- !is.null(benchmarks[[info$benchmark_key]])
  coverage_rate <- nrow(panel) / expected_days
  reaches_end <- max(panel$date) >= sample_end
  status <- "eligible"
  if (!benchmark_ok) status <- "missing_benchmark"
  if (!reaches_end || coverage_rate < minimum_price_coverage) {
    status <- if (status == "eligible") "incomplete_price_coverage" else paste(status, "incomplete_price_coverage", sep = ";")
  }

  coverage_rows[[i]] <- data.frame(
    coin_id = info$coin_id,
    symbol = info$symbol,
    benchmark_key = info$benchmark_key,
    identifier_status = info$identifier_status,
    first_price_date = min(panel$date),
    last_price_date = max(panel$date),
    n_price_days = nrow(panel),
    expected_days = expected_days,
    price_coverage_rate = coverage_rate,
    internal_missing_days = expected_days - nrow(panel),
    reaches_sample_end = reaches_end,
    benchmark_available = benchmark_ok,
    analysis_status = status,
    stringsAsFactors = FALSE
  )
  coin_panels[[i]] <- panel
}

coverage <- do.call(rbind, coverage_rows)
rownames(coverage) <- NULL
write_csv(coverage, file.path(output_dir, "stablecoin_coverage_diagnostics.csv"))

# Export every missing stablecoin-price date so the author can request or
# download precisely the observations required for the final sample.
full_calendar <- seq(sample_start, sample_end, by = "day")
missing_price_rows <- list()
missing_counter <- 0L
for (i in seq_len(nrow(coin_map))) {
  observed_dates <- if (is.null(coin_panels[[i]])) as.Date(character()) else coin_panels[[i]]$date
  missing_dates <- setdiff(full_calendar, observed_dates)
  if (length(missing_dates)) {
    missing_counter <- missing_counter + 1L
    missing_price_rows[[missing_counter]] <- data.frame(
      coin_id = coin_map$coin_id[i],
      symbol = coin_map$symbol[i],
      missing_date = as.Date(missing_dates, origin = "1970-01-01"),
      stringsAsFactors = FALSE
    )
  }
}
if (length(missing_price_rows)) {
  missing_price_dates <- do.call(rbind, missing_price_rows)
} else {
  missing_price_dates <- data.frame(
    coin_id = character(), symbol = character(),
    missing_date = as.Date(character()), stringsAsFactors = FALSE
  )
}
write_csv(missing_price_dates, file.path(output_dir, "missing_stablecoin_price_dates.csv"))

unresolved <- coverage$analysis_status != "eligible"
if (any(unresolved)) {
  message("Data-integrity checks identified unresolved series:")
  for (i in which(unresolved)) {
    message("  - ", coverage$symbol[i], ": ", coverage$analysis_status[i])
  }
}

if (!allow_partial && any(unresolved)) {
  stop(
    "Final-mode analysis stopped because the 11-coin sample is not fully verified. ",
    "See stablecoin_coverage_diagnostics.csv and benchmark_diagnostics.csv. ",
    "Add the required data/confirmation, or use FRM_SC_ALLOW_PARTIAL=1 only for a preliminary audit."
  )
}

# In partial mode, retain only series with an available benchmark and price data.
usable_idx <- which(
  !vapply(coin_panels, is.null, logical(1)) &
    vapply(coin_map$benchmark_key, function(k) !is.null(benchmarks[[k]]), logical(1))
)
if (!length(usable_idx)) stop("No stablecoin series has both prices and a valid benchmark.")

# ---- 5. Construct reference-adjusted peg deviations -------------------------

analysis_panels <- vector("list", length(usable_idx))

for (z in seq_along(usable_idx)) {
  i <- usable_idx[z]
  info <- coin_map[i, , drop = FALSE]
  panel <- coin_panels[[i]]
  benchmark <- benchmarks[[info$benchmark_key]]
  keep_b <- benchmark[, c(
    "date", "benchmark_value", "benchmark_observed",
    "benchmark_forward_filled", "days_since_benchmark_observation"
  )]
  x <- merge(panel, keep_b, by = "date", all.x = TRUE, sort = TRUE)
  valid <- is.finite(x$price_usd) & x$price_usd > 0 &
    is.finite(x$benchmark_value) & x$benchmark_value > 0
  x$reference_adjusted_price <- NA_real_
  x$signed_deviation <- NA_real_
  x$absolute_deviation <- NA_real_
  x$downside_deviation <- NA_real_
  x$reference_adjusted_price[valid] <- x$price_usd[valid] / x$benchmark_value[valid]
  x$signed_deviation[valid] <- x$reference_adjusted_price[valid] - 1
  x$absolute_deviation[valid] <- abs(x$signed_deviation[valid])
  x$downside_deviation[valid] <- pmin(x$signed_deviation[valid], 0)
  x$signed_deviation_bps <- 10000 * x$signed_deviation
  x$absolute_deviation_bps <- 10000 * x$absolute_deviation
  x$downside_deviation_bps <- 10000 * x$downside_deviation
  x$main_downside_depeg <- is.finite(x$signed_deviation) & x$signed_deviation <= -main_threshold
  x$main_two_sided_depeg <- is.finite(x$absolute_deviation) & x$absolute_deviation >= main_threshold
  x$preliminary_output <- allow_partial
  x$identifier_confirmed <- info$identifier_status == "confirmed"
  analysis_panels[[z]] <- x
}

peg_panel <- do.call(rbind, analysis_panels)
peg_panel <- peg_panel[order(peg_panel$date, peg_panel$symbol), , drop = FALSE]
rownames(peg_panel) <- NULL

# Sanity check: reference-adjusted medians far from parity normally indicate a
# wrong benchmark orientation or wrong CoinGecko identifier. Do not auto-delete.
ratio_medians <- tapply(peg_panel$reference_adjusted_price, peg_panel$symbol, median, na.rm = TRUE)
bad_ratio <- names(ratio_medians)[!is.finite(ratio_medians) | ratio_medians < 0.8 | ratio_medians > 1.2]
if (length(bad_ratio)) {
  warning(
    "Reference-adjusted median is outside [0.8, 1.2] for: ",
    paste(bad_ratio, collapse = ", "),
    ". Verify identifiers and benchmark orientation before reporting."
  )
}

write_csv(peg_panel, file.path(output_dir, "peg_panel_daily.csv"))

# ---- 6. Detect depeg episodes at all pre-specified thresholds ----------------

events_all <- list()
counter <- 0L
for (thr in thresholds) {
  for (sym in unique(peg_panel$symbol)) {
    counter <- counter + 1L
    events_all[[counter]] <- extract_depeg_events(
      peg_panel[peg_panel$symbol == sym, , drop = FALSE],
      threshold = thr
    )
  }
}
events_all <- events_all[vapply(events_all, nrow, integer(1)) > 0]
if (length(events_all)) {
  depeg_events <- do.call(rbind, events_all)
  depeg_events <- depeg_events[order(depeg_events$threshold, depeg_events$symbol, depeg_events$start_date), , drop = FALSE]
  rownames(depeg_events) <- NULL
} else {
  depeg_events <- data.frame(
    coin_id = character(), symbol = character(), threshold = numeric(),
    start_date = as.Date(character()), end_date = as.Date(character()),
    duration_days = integer(), minimum_deviation = numeric(),
    minimum_deviation_bps = numeric(), stringsAsFactors = FALSE
  )
}
write_csv(depeg_events, file.path(output_dir, "depeg_events_all_thresholds.csv"))
write_csv(
  depeg_events[abs(depeg_events$threshold - main_threshold) < .Machine$double.eps^0.5, , drop = FALSE],
  file.path(output_dir, "depeg_events_main_threshold.csv")
)

# ---- 7. Table 4.1 and sensitivity table --------------------------------------

symbols <- unique(coin_map$symbol[coin_map$symbol %in% peg_panel$symbol])
summary_rows <- vector("list", length(symbols))

for (i in seq_along(symbols)) {
  sym <- symbols[i]
  x <- peg_panel[peg_panel$symbol == sym & is.finite(peg_panel$signed_deviation), , drop = FALSE]
  ev <- depeg_events[
    depeg_events$symbol == sym &
      abs(depeg_events$threshold - main_threshold) < .Machine$double.eps^0.5,
    , drop = FALSE
  ]
  threshold_flag <- x$signed_deviation <= -main_threshold
  summary_rows[[i]] <- data.frame(
    coin = sym,
    coingecko_id = x$coin_id[1],
    benchmark = x$benchmark_key[1],
    identifier_confirmed = x$identifier_confirmed[1],
    n_daily_observations = nrow(x),
    start_date = min(x$date),
    end_date = max(x$date),
    median_signed_deviation_bps = 10000 * safe_median(x$signed_deviation),
    median_absolute_deviation_bps = 10000 * safe_median(x$absolute_deviation),
    iqr_signed_deviation_bps = 10000 * safe_iqr(x$signed_deviation),
    lower_5pct_quantile_bps = 10000 * safe_quantile(x$signed_deviation, 0.05),
    absolute_95pct_quantile_bps = 10000 * safe_quantile(x$absolute_deviation, 0.95),
    minimum_signed_deviation_bps = 10000 * min(x$signed_deviation, na.rm = TRUE),
    main_threshold_bps = 10000 * main_threshold,
    downside_depeg_days = sum(threshold_flag, na.rm = TRUE),
    downside_depeg_frequency_pct = 100 * mean(threshold_flag, na.rm = TRUE),
    number_of_downside_episodes = nrow(ev),
    maximum_downside_duration_days = if (nrow(ev)) max(ev$duration_days) else 0L,
    preliminary_output = allow_partial,
    stringsAsFactors = FALSE
  )
}

table_4_1 <- do.call(rbind, summary_rows)
table_4_1 <- table_4_1[order(table_4_1$median_absolute_deviation_bps), , drop = FALSE]
rownames(table_4_1) <- NULL
write_csv(table_4_1, file.path(output_dir, "Table_4_1_Peg_Stability.csv"))

sensitivity_rows <- list()
counter <- 0L
for (thr in thresholds) {
  for (sym in symbols) {
    counter <- counter + 1L
    x <- peg_panel[peg_panel$symbol == sym & is.finite(peg_panel$signed_deviation), , drop = FALSE]
    ev <- depeg_events[
      depeg_events$symbol == sym & abs(depeg_events$threshold - thr) < .Machine$double.eps^0.5,
      , drop = FALSE
    ]
    flag <- x$signed_deviation <= -thr
    sensitivity_rows[[counter]] <- data.frame(
      coin = sym,
      threshold = thr,
      threshold_bps = 10000 * thr,
      n_daily_observations = nrow(x),
      depeg_days = sum(flag, na.rm = TRUE),
      depeg_frequency_pct = 100 * mean(flag, na.rm = TRUE),
      number_of_episodes = nrow(ev),
      maximum_duration_days = if (nrow(ev)) max(ev$duration_days) else 0L,
      stringsAsFactors = FALSE
    )
  }
}
sensitivity <- do.call(rbind, sensitivity_rows)
write_csv(sensitivity, file.path(output_dir, "depeg_threshold_sensitivity.csv"))

# ---- 8. Thesis-ready figures (R; PNG, TIFF, PDF, and editable SVG) -----------

palette <- c(
  USDT = "#1B9E77", USDC = "#377EB8", DAI = "#E6AB02", BUSD = "#F4A261",
  TUSD = "#4DAF4A", USDP = "#984EA3", GUSD = "#A65628", PAXG = "#B8860B",
  EURS = "#00A6D6", sUSD = "#E7298A", IDRT = "#666666"
)

open_figure_device <- function(path, device, width_in, height_in) {
  if (device == "png") {
    ragg::agg_png(path, width = width_in, height = height_in, units = "in", res = 600, background = "white")
  } else if (device == "tiff") {
    ragg::agg_tiff(
      path, width = width_in, height = height_in, units = "in", res = 600,
      compression = "lzw", background = "white"
    )
  } else if (device == "pdf") {
    grDevices::pdf(
      path, width = width_in, height = height_in, family = "Helvetica",
      bg = "white", useDingbats = FALSE, paper = "special"
    )
  } else if (device == "svg") {
    svglite::svglite(path, width = width_in, height = height_in, bg = "white")
  } else {
    stop("Unsupported figure device: ", device)
  }
}

plot_absolute_distribution <- function(device = c("png", "tiff", "pdf", "svg")) {
  device <- match.arg(device)
  path <- file.path(output_dir, paste0("Figure_4_1_Peg_Deviation_Distributions.", device))
  open_figure_device(path, device, width_in = 183 / 25.4, height_in = 112 / 25.4)
  on.exit(dev.off(), add = TRUE)

  order_symbols <- table_4_1$coin
  values <- lapply(order_symbols, function(sym) {
    x <- peg_panel$absolute_deviation_bps[peg_panel$symbol == sym]
    log10(1 + x[is.finite(x) & x >= 0])
  })
  names(values) <- order_symbols
  cols <- unname(palette[order_symbols])
  cols[is.na(cols)] <- "#808080"
  par(
    mar = c(6.2, 6.2, 1.2, 0.8), mgp = c(3.0, 1.0, 0),
    las = 1, family = "Helvetica", cex = 0.85
  )
  boxplot(
    values,
    col = grDevices::adjustcolor(cols, alpha.f = 0.60),
    border = cols,
    outline = TRUE,
    pch = 16,
    cex = 0.25,
    xaxt = "n",
    yaxt = "n",
    ylab = "",
    xlab = "",
    main = ""
  )
  axis(1, at = seq_along(order_symbols), labels = order_symbols, las = 2, cex.axis = 0.75)
  tick_bps <- c(0, 1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000)
  at <- log10(1 + tick_bps)
  usr <- par("usr")
  keep <- at >= usr[3] & at <= usr[4]
  axis(
    2, at = at[keep], labels = format(tick_bps[keep], big.mark = ",", scientific = FALSE),
    las = 1, cex.axis = 0.80
  )
  mtext("Absolute deviation (bp; log scale)", side = 2, line = 3.0, cex = 0.75, las = 0)
  abline(h = log10(1 + 10000 * main_threshold), lty = 2, lwd = 1.2, col = "#B2182B")
  legend(
    "topright",
    legend = paste0("Reference magnitude: ", 100 * main_threshold, "%"),
    lty = 2, lwd = 1.2, col = "#B2182B", bty = "n", cex = 0.65
  )
  box(bty = "l")
}

plot_depeg_frequency <- function(device = c("png", "tiff", "pdf", "svg")) {
  device <- match.arg(device)
  path <- file.path(output_dir, paste0("Figure_4_1_Downside_Depeg_Frequency.", device))
  open_figure_device(path, device, width_in = 183 / 25.4, height_in = 107 / 25.4)
  on.exit(dev.off(), add = TRUE)

  tbl <- table_4_1[order(table_4_1$downside_depeg_frequency_pct), , drop = FALSE]
  cols <- unname(palette[tbl$coin])
  cols[is.na(cols)] <- "#808080"
  par(mar = c(6.2, 5.2, 1.2, 0.8), las = 1, family = "Helvetica", cex = 0.90)
  bp <- barplot(
    tbl$downside_depeg_frequency_pct,
    names.arg = tbl$coin,
    las = 2,
    col = grDevices::adjustcolor(cols, alpha.f = 0.75),
    border = cols,
    ylab = "Downside depeg frequency (%)",
    xlab = "",
    main = ""
  )
  text(
    bp,
    tbl$downside_depeg_frequency_pct,
    labels = formatC(tbl$downside_depeg_frequency_pct, format = "f", digits = 2),
    pos = 3,
    cex = 0.65,
    xpd = TRUE
  )
  mtext(paste0("Threshold: deviation <= -", 100 * main_threshold, "%"), side = 3, line = 0.2, adj = 0, cex = 0.78)
  box(bty = "l")
}

plot_absolute_distribution("png")
plot_absolute_distribution("tiff")
plot_absolute_distribution("pdf")
plot_absolute_distribution("svg")
plot_depeg_frequency("png")
plot_depeg_frequency("tiff")
plot_depeg_frequency("pdf")
plot_depeg_frequency("svg")

# ---- 9. Reproducibility metadata ---------------------------------------------

source_inventory <- source_map
source_inventory$directory_exists <- dir.exists(source_inventory$directory)
write_csv(source_inventory, file.path(output_dir, "source_inventory.csv"))
write_csv(coin_map, file.path(output_dir, "coin_reference_map.csv"))

metadata <- c(
  paste0("run_time_utc=", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste0("r_version=", R.version.string),
  paste0("script=", normalizePath(file.path(script_dir, "FRM_SC_peg_descriptives.R"), mustWork = FALSE)),
  paste0("sample_start=", sample_start),
  paste0("sample_end=", sample_end),
  paste0("main_threshold=", format(main_threshold, scientific = FALSE)),
  paste0("threshold_set=", paste(thresholds, collapse = ",")),
  paste0("max_benchmark_forward_fill_days=", max_benchmark_ffill_days),
  paste0("allow_partial=", allow_partial),
  "coingecko_id_nusd_display_symbol=sUSD",
  paste0("n_coins_in_output=", length(unique(peg_panel$symbol))),
  paste0("coins_in_output=", paste(unique(peg_panel$symbol), collapse = ",")),
  "stablecoin_price_missing_values_are_not_imputed=true",
  "benchmark_missing_values_use_past_only_forward_fill=true",
  "outliers_removed=false",
  "inference_test_applied=false"
)
writeLines(metadata, file.path(output_dir, "run_metadata.txt"), useBytes = TRUE)

caption <- c(
  "Figure 4.1. Distribution of absolute reference-adjusted peg deviations.",
  "For each stablecoin, the market price is divided by the contemporaneous US-dollar value of its designated reference asset before subtracting one.",
  "Absolute deviations are reported in basis points on a log10(1+x) scale so that both routine observations and tail events remain visible.",
  "Boxes show the interquartile range and median; whiskers follow the conventional 1.5 x IQR rule; points outside the whiskers are retained.",
  paste0("The dashed line marks an absolute deviation of ", 100 * main_threshold, "%; downside event frequencies use the signed threshold."),
  if (allow_partial) "PRELIMINARY: the figure contains only series with currently available benchmarks and must not be presented as the final eleven-coin result." else "The final figure contains the verified eleven-coin sample."
)
writeLines(caption, file.path(output_dir, "Figure_4_1_caption.txt"), useBytes = TRUE)

message("Completed Chapter 4.1 outputs:")
message("  ", normalizePath(output_dir, mustWork = FALSE))
message("  coins in output: ", paste(unique(peg_panel$symbol), collapse = ", "))
if (allow_partial) {
  message("PRELIMINARY PARTIAL MODE: do not report these outputs as the final 11-coin analysis.")
}
