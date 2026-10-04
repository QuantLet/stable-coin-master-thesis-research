#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else normalizePath(getwd())
source(file.path(script_dir, "00_config.R"))

if (!dir.exists(coingecko_root)) stop("CoinGecko root not found: ", coingecko_root)
assets <- read.csv(asset_file, check.names = FALSE, stringsAsFactors = FALSE)
required_asset_fields <- c("order", "ticker", "coingecko_id", "continuation_id", "ppt_label")
if (!all(required_asset_fields %in% names(assets))) stop("Malformed asset universe file.")
assets <- assets[order(assets$order), ]

source_map <- data.frame(
  source_name = c(
    "history_to_2025_03_02",
    "extended_to_2025_09_09",
    "segment_2025_09_10_to_2026_01_10",
    "segment_2026_01_11_to_2026_04_06"
  ),
  directory = file.path(coingecko_root, c(
    "coin_gecko",
    "coin_gecko_20250910_112201",
    "20260110_133530_start_20250909_end_20260110",
    "20260408_133530_coingecko_start_20260111_end_20260406"
  )),
  priority = c(10L, 20L, 30L, 40L),
  stringsAsFactors = FALSE
)

safe_numeric <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x), fixed = TRUE)))

parse_time <- function(x) {
  x <- trimws(as.character(x))
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
  formats <- c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%d")
  for (fmt in formats) {
    miss <- is.na(out) & nzchar(x)
    if (!any(miss)) break
    out[miss] <- suppressWarnings(as.POSIXct(x[miss], format = fmt, tz = "UTC"))
  }
  out
}

read_one <- function(path, source_name, priority, id) {
  if (!file.exists(path)) return(NULL)
  raw <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (!all(c("date", "prices", "market_caps") %in% names(raw))) {
    stop("Required CoinGecko fields missing: ", path)
  }
  timestamp <- parse_time(raw$date)
  x <- data.frame(
    date = as.Date(timestamp, tz = "UTC"),
    timestamp = timestamp,
    price = safe_numeric(raw$prices),
    market_cap = safe_numeric(raw$market_caps),
    volume = if ("total_volumes" %in% names(raw)) safe_numeric(raw$total_volumes) else NA_real_,
    source_name = source_name,
    source_file = normalizePath(path),
    priority = as.integer(priority),
    source_row = seq_len(nrow(raw)),
    raw_id = id,
    stringsAsFactors = FALSE
  )
  x <- x[!is.na(x$date) & x$date >= sample_start & x$date <= sample_end &
           is.finite(x$price) & x$price > 0, ]
  if (!nrow(x)) return(NULL)
  x <- x[order(x$date, x$timestamp, x$source_row), ]
  x <- x[!duplicated(x$date, fromLast = TRUE), ]
  rownames(x) <- NULL
  x
}

combine_id <- function(id) {
  pieces <- vector("list", nrow(source_map))
  for (i in seq_len(nrow(source_map))) {
    pieces[[i]] <- read_one(
      file.path(source_map$directory[i], paste0(id, ".csv")),
      source_map$source_name[i], source_map$priority[i], id
    )
  }
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (!length(pieces)) stop("No local data found for CoinGecko id: ", id)
  x <- do.call(rbind, pieces)
  x <- x[order(x$date, x$priority, x$timestamp, x$source_row), ]
  x <- x[!duplicated(x$date, fromLast = TRUE), ]
  rownames(x) <- NULL
  x
}

daily_list <- vector("list", nrow(assets))
manifest <- list()
for (i in seq_len(nrow(assets))) {
  primary <- combine_id(assets$coingecko_id[i])
  primary$continuation_rule <- "primary_id"
  continuation_id <- trimws(assets$continuation_id[i])
  if (nzchar(continuation_id)) {
    continuation <- combine_id(continuation_id)
    continuation$continuation_rule <- "continuation_id"
    # Polygon's MATIC-to-POL upgrade was 1:1. From 4 September 2024 onward,
    # prefer POL when present and retain MATIC only as a local-data gap filler.
    migration <- as.Date("2024-09-04")
    pre <- primary[primary$date < migration, ]
    post <- rbind(
      primary[primary$date >= migration, ],
      continuation[continuation$date >= migration, ]
    )
    post$continuation_rank <- ifelse(post$continuation_rule == "continuation_id", 2L, 1L)
    post <- post[order(post$date, post$continuation_rank, post$priority, post$timestamp), ]
    post <- post[!duplicated(post$date, fromLast = TRUE), ]
    post$continuation_rank <- NULL
    primary <- rbind(pre, post)
    primary <- primary[order(primary$date), ]
  }
  primary$ticker <- assets$ticker[i]
  daily_list[[i]] <- primary
  manifest[[i]] <- unique(primary[, c(
    "ticker", "raw_id", "source_name", "source_file", "priority", "continuation_rule"
  )])
}

calendar <- data.frame(date = seq(sample_start, sample_end, by = "day"))
price_panel <- calendar
mcap_panel <- calendar
volume_panel <- calendar
coverage <- list()
for (i in seq_along(daily_list)) {
  z <- daily_list[[i]]
  ticker_i <- assets$ticker[i]
  p <- z[, c("date", "price")]; names(p)[2] <- ticker_i
  m <- z[, c("date", "market_cap")]; names(m)[2] <- ticker_i
  v <- z[, c("date", "volume")]; names(v)[2] <- ticker_i
  price_panel <- merge(price_panel, p, by = "date", all.x = TRUE, sort = TRUE)
  mcap_panel <- merge(mcap_panel, m, by = "date", all.x = TRUE, sort = TRUE)
  volume_panel <- merge(volume_panel, v, by = "date", all.x = TRUE, sort = TRUE)
  ok <- is.finite(z$price) & z$price > 0
  coverage[[i]] <- data.frame(
    ticker = ticker_i,
    first_price_date = format(min(z$date[ok])),
    last_price_date = format(max(z$date[ok])),
    observed_price_days = sum(ok),
    calendar_days = nrow(calendar),
    price_coverage = sum(ok) / nrow(calendar),
    missing_price_days = nrow(calendar) - sum(ok),
    observed_market_cap_days = sum(is.finite(z$market_cap) & z$market_cap >= 0),
    stringsAsFactors = FALSE
  )
}

expected <- assets$ticker
stopifnot(identical(names(price_panel)[-1], expected))
stopifnot(identical(names(mcap_panel)[-1], expected))
stopifnot(identical(names(volume_panel)[-1], expected))

write_csv(price_panel, file.path(prepared_dir, "Crypto_Price_15_PPT.csv"), quote = FALSE)
write_csv(mcap_panel, file.path(prepared_dir, "Crypto_MarketCap_15_PPT.csv"), quote = FALSE)
write_csv(volume_panel, file.path(prepared_dir, "Crypto_Volume_15_PPT.csv"), quote = FALSE)
write_csv(do.call(rbind, coverage), file.path(qa_dir, "crypto_asset_coverage.csv"))
write_csv(do.call(rbind, manifest), file.path(qa_dir, "crypto_source_manifest.csv"))

qa <- data.frame(
  check = c(
    "ppt_asset_count", "daily_calendar", "sample_start", "sample_end",
    "all_observed_prices_positive", "all_assets_have_90_observations",
    "polygon_continuation_present"
  ),
  status = c(
    if (nrow(assets) == 15L) "PASS" else "FAIL",
    if (identical(price_panel$date, seq(sample_start, sample_end, by = "day"))) "PASS" else "FAIL",
    if (min(price_panel$date) == sample_start) "PASS" else "FAIL",
    if (max(price_panel$date) == sample_end) "PASS" else "FAIL",
    if (all(as.matrix(price_panel[-1])[is.finite(as.matrix(price_panel[-1]))] > 0)) "PASS" else "FAIL",
    if (all(vapply(daily_list, nrow, integer(1)) >= 90L)) "PASS" else "FAIL",
    if (any(daily_list[[which(assets$ticker == "MATIC_POL")]]$continuation_rule == "continuation_id")) "PASS" else "FAIL"
  ),
  detail = c(
    paste(assets$ticker, collapse = ", "),
    paste(min(price_panel$date), "to", max(price_panel$date)),
    format(min(price_panel$date)), format(max(price_panel$date)),
    paste(sum(is.finite(as.matrix(price_panel[-1]))), "observed cells"),
    paste(range(vapply(daily_list, nrow, integer(1))), collapse = " to "),
    "MATIC retained only where post-migration POL is absent"
  ),
  stringsAsFactors = FALSE
)
write_csv(qa, file.path(qa_dir, "crypto_panel_QA.csv"))
if (any(qa$status == "FAIL")) stop("Crypto panel QA failed.")

metadata <- c(
  "source=PPT slide 7 asset list and author's local CoinGecko exports",
  paste0("sample=", sample_start, " to ", sample_end),
  "daily_rule=last valid UTC observation per asset/date within highest-priority source",
  "missing_rule=no price interpolation; missing market capitalization retained",
  "PDT_label_note=PPT label PDT is implemented as the standard Polkadot ticker DOT",
  "polygon_rule=MATIC before 2024-09-04; prefer 1:1 POL continuation thereafter; MATIC fills local POL gaps",
  paste0("asset_file_md5=", unname(tools::md5sum(asset_file)))
)
writeLines(metadata, file.path(qa_dir, "crypto_panel_metadata.txt"), useBytes = TRUE)
message("Prepared the PPT-defined 15-asset crypto panel through ", sample_end)
