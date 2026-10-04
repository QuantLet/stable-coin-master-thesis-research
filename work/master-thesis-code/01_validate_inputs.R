#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else {
  normalizePath(getwd())
}
source(file.path(script_dir, "00_config.R"))

# Base-R replacement for zoo::na.locf(x, fromLast = TRUE, na.rm = FALSE),
# retained because that is the missing-value rule in the supplied code.
locf_from_last <- function(x) {
  x <- as.numeric(x)
  next_value <- NA_real_
  for (i in rev(seq_along(x))) {
    if (is.finite(x[i])) next_value <- x[i] else if (is.finite(next_value)) x[i] <- next_value
  }
  x
}

read_panel <- function(path) {
  if (!file.exists(path)) stop("Input file not found: ", path)
  x <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(x) || ncol(x) < 2L) stop("Empty or malformed panel: ", path)
  parsed <- as.Date(x[[1]], "%m/%d/%Y")
  if (anyNA(parsed)) parsed <- as.Date(x[[1]])
  if (anyNA(parsed)) stop("Date parsing failed in: ", path)
  x[[1]] <- parsed
  names(x)[1] <- "ticker"
  for (nm in names(x)[-1]) x[[nm]] <- suppressWarnings(as.numeric(x[[nm]]))
  x
}

validation <- data.frame(
  check = character(), status = character(), detail = character(),
  stringsAsFactors = FALSE
)
add_check <- function(check, ok, detail) {
  validation <<- rbind(
    validation,
    data.frame(check = check, status = if (ok) "PASS" else "FAIL", detail = detail)
  )
}

stock_prices <- read_panel(price_file)
mktcap <- read_panel(mktcap_file)
macro <- read_panel(macro_file)

add_check("price_dates_unique", !anyDuplicated(stock_prices$ticker),
          sprintf("%d rows", nrow(stock_prices)))
add_check("mktcap_dates_unique", !anyDuplicated(mktcap$ticker),
          sprintf("%d rows", nrow(mktcap)))
add_check("macro_dates_unique", !anyDuplicated(macro$ticker),
          sprintf("%d rows", nrow(macro)))
add_check("dates_aligned",
          identical(stock_prices$ticker, mktcap$ticker) && identical(stock_prices$ticker, macro$ticker),
          paste(format(min(stock_prices$ticker)), "to", format(max(stock_prices$ticker))))
add_check("daily_calendar",
          identical(stock_prices$ticker, seq(min(stock_prices$ticker), max(stock_prices$ticker), by = "day")),
          "No calendar dates omitted")
add_check("sample_end", max(stock_prices$ticker) == date_end_source,
          format(max(stock_prices$ticker)))
add_check("stablecoin_columns", identical(names(stock_prices)[-1], stablecoin_names),
          paste(names(stock_prices)[-1], collapse = ", "))
add_check("mktcap_columns", identical(names(mktcap)[-1], stablecoin_names),
          paste(names(mktcap)[-1], collapse = ", "))
add_check("macro_columns", identical(names(macro)[-1], macro_names),
          paste(names(macro)[-1], collapse = ", "))

price_values <- as.matrix(stock_prices[, -1, drop = FALSE])
mktcap_values <- as.matrix(mktcap[, -1, drop = FALSE])
macro_values_raw <- as.matrix(macro[, -1, drop = FALSE])
add_check("observed_prices_positive",
          all(price_values[is.finite(price_values)] > 0),
          sprintf("%d missing cells retained", sum(!is.finite(price_values))))
add_check("observed_market_caps_nonnegative",
          all(mktcap_values[is.finite(mktcap_values)] >= 0),
          sprintf("%d missing cells converted to zero for ranking", sum(!is.finite(mktcap_values))))
add_check("observed_macro_levels_positive",
          all(macro_values_raw[is.finite(macro_values_raw)] > 0),
          sprintf("%d missing cells before repository fill rule", sum(!is.finite(macro_values_raw))))

write.csv(
  validation,
  file.path(results_dir, "input_validation_20260531.csv"),
  row.names = FALSE,
  quote = TRUE
)
if (any(validation$status == "FAIL")) {
  stop("Input validation failed; inspect results/input_validation_20260531.csv")
}

macro_values <- apply(macro_values_raw, 2L, locf_from_last)
if (is.null(dim(macro_values))) macro_values <- matrix(macro_values, ncol = 1L)
colnames(macro_values) <- macro_names
if (any(!is.finite(macro_values))) stop("Macro panel still contains missing values after supplied fill rule.")

mktcap_values[!is.finite(mktcap_values)] <- 0
colnames(mktcap_values) <- stablecoin_names
all_levels <- cbind(price_values, macro_values)
all_return_unfiltered <- diff(log(all_levels))
nonfinite_return_counts <- colSums(!is.finite(all_return_unfiltered))
all_return <- all_return_unfiltered
all_return[!is.finite(all_return)] <- 0
stock_return <- all_return[, stablecoin_names, drop = FALSE]
macro_return <- all_return[, macro_names, drop = FALSE]

dates <- stock_prices$ticker
ticker <- as.integer(format(dates, "%Y%m%d"))
mktcap_index <- t(apply(mktcap_values, 1L, function(v) order(v, decreasing = TRUE)))
storage.mode(mktcap_index) <- "integer"

coverage <- data.frame(
  series = c(stablecoin_names, macro_names),
  type = c(rep("stablecoin", length(stablecoin_names)), rep("macro", length(macro_names))),
  first_date = format(min(dates)),
  last_date = format(max(dates)),
  input_observations = length(dates),
  missing_level_cells = c(colSums(!is.finite(price_values)), colSums(!is.finite(macro_values_raw))),
  nonfinite_returns_set_to_zero = as.integer(nonfinite_return_counts),
  stringsAsFactors = FALSE
)
write.csv(
  coverage,
  file.path(results_dir, "FRM_sample_coverage_20260531.csv"),
  row.names = FALSE,
  quote = TRUE
)

parameters <- data.frame(
  parameter = c(
    "Input start", "Input end", "Return transformation", "Rolling window",
    "Conditional quantile", "Maximum path steps", "Stablecoins",
    "Macro factors", "Daily aggregation", "GACV selection",
    "Missing stablecoin return rule", "Macro timing"
  ),
  value = c(
    format(min(dates)), format(max(dates)), "Daily log difference", window_length,
    tau, path_steps, n_stablecoins, n_macro_factors,
    "Equal-weight mean of coin-specific selected lambdas",
    "Minimum finite GACV on each fitted path",
    "Non-finite log returns set to zero, as in supplied code",
    "Contemporaneous daily log changes, as in supplied code"
  ),
  stringsAsFactors = FALSE
)
write.csv(
  parameters,
  file.path(results_dir, "Table_4_2_FRM_Parameters.csv"),
  row.names = FALSE,
  quote = TRUE
)

stage <- list(
  dates = dates,
  ticker = ticker,
  stock_return = stock_return,
  macro_return = macro_return,
  mktcap_index = mktcap_index,
  stablecoin_names = stablecoin_names,
  macro_names = macro_names,
  validation = validation,
  coverage = coverage,
  input_md5 = unname(tools::md5sum(c(price_file, mktcap_file, macro_file, algorithm_file)))
)
names(stage$input_md5) <- c("price", "market_cap", "macro", "algorithm")
saveRDS(stage, file.path(results_dir, "stage_20_validated_inputs.rds"), compress = "xz")
message("Input validation passed. Prepared model matrices through 2026-05-31.")
