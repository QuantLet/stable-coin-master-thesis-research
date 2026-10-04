#!/usr/bin/env Rscript

# OPTIONAL: rebuild the three repository-format input panels from the user's
# CoinGecko exports and the documented macro source components. The project
# already contains the exact panels used for the delivered strict run, so this
# script is not called by run_all.R.

script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg)) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  normalizePath(getwd())
}
project_dir <- script_dir()
if (!file.exists(file.path(project_dir, "FRM_SC_Study_20260531_strict.Rproj"))) {
  project_dir <- normalizePath(getwd())
}

data_root <- Sys.getenv("COINGECKO_DATA_ROOT", "")
if (!nzchar(data_root) || !dir.exists(data_root)) {
  stop(
    "CoinGecko folder not found. Set COINGECKO_DATA_ROOT to the folder ",
    "that contains the CoinGecko export subfolders, then rerun this script."
  )
}

repo_input_dir <- Sys.getenv(
  "FRM_REPO_INPUT_DIR",
  file.path(dirname(project_dir), "FRM_Stable_Coins", "Input", "Stable", "20200102-20250302")
)
public_macro_path <- file.path(project_dir, "data", "support", "macro_public_components_weekdays.csv")
model_ready_macro_path <- file.path(
  project_dir,
  "data",
  "support",
  "macro_model_ready_calendar_daily_20200101_20260531.csv"
)
macro_extension_audit_path <- file.path(
  project_dir,
  "data",
  "support",
  "macro_extension_method_audit.csv"
)
paxg_supplement <- file.path(project_dir, "data", "support", "pax-gold_supplement_to_20260531.csv")
output_dir <- file.path(project_dir, "data", "input")
audit_dir <- file.path(project_dir, "audit")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

start_date <- as.Date("2020-01-01")
end_date <- as.Date("2026-05-31")

coins <- data.frame(
  repository_name = c(
    "usd_coin", "binance_usd", "gemini_dollar", "stasis_eurs", "rupiah_token",
    "tether", "nusd", "pax_gold", "true_usd", "paxos_standard", "dai"
  ),
  coin_id = c(
    "usd-coin", "binance-usd", "gemini-dollar", "stasis-eurs", "rupiah-token",
    "tether", "nusd", "pax-gold", "true-usd", "paxos-standard", "dai"
  ),
  stringsAsFactors = FALSE
)

# Compatibility note: the repository column `nusd` is retained because the
# supplied FRM code expects it. In thesis labels it should be displayed as sUSD.

source_priority <- function(path) {
  group <- basename(dirname(path))
  if (grepl("^20260531_", group)) return(500L)
  if (grepl("^20260408_", group)) return(400L)
  if (grepl("^20260110_", group)) return(300L)
  if (grepl("^coin_gecko_20250910_", group)) return(200L)
  if (identical(group, "coin_gecko")) return(100L)
  if (identical(group, "coin_gecko 2")) return(90L)
  10L
}

read_coin_file <- function(path, coin_id, repository_name, supplement = FALSE) {
  x <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  date_name <- intersect(c("date", "event_date"), names(x))[1]
  price_name <- intersect(c("prices", "close_price_usd"), names(x))[1]
  mcap_name <- intersect(c("market_caps", "market_cap_usd"), names(x))[1]
  if (is.na(date_name) || is.na(price_name)) stop("Date/price columns missing in ", path)
  clean_number <- function(v) suppressWarnings(as.numeric(gsub(",", "", v, fixed = TRUE)))
  data.frame(
    date = as.Date(substr(x[[date_name]], 1, 10)),
    repository_name = repository_name,
    coin_id = coin_id,
    price = clean_number(x[[price_name]]),
    market_cap = if (!is.na(mcap_name)) clean_number(x[[mcap_name]]) else NA_real_,
    source_group = basename(dirname(path)),
    source_file = basename(path),
    # Supplemental PAXG rows have the lowest priority and only fill dates that
    # are absent from the user's own exports.
    priority = if (supplement) 1L else source_priority(path),
    source_row = seq_len(nrow(x)),
    supplement = supplement,
    stringsAsFactors = FALSE
  )
}

all_rows <- list()
manifest <- list()
for (i in seq_len(nrow(coins))) {
  coin_id <- coins$coin_id[i]
  repository_name <- coins$repository_name[i]
  paths <- list.files(
    data_root,
    pattern = paste0("^", gsub("-", "\\\\-", coin_id), "\\.csv$"),
    recursive = TRUE,
    full.names = TRUE
  )
  if (!length(paths)) stop("No CoinGecko file found for ", coin_id)
  rows <- do.call(
    rbind,
    lapply(paths, read_coin_file, coin_id = coin_id, repository_name = repository_name)
  )

  # The user's PAXG files have a 2025-03-03–2025-09-09 gap and the final
  # 2026-05-31 folder omits PAXG. A supplemental CoinGecko export fills only
  # missing dates because its priority is lower than every user source.
  if (coin_id == "pax-gold" && file.exists(paxg_supplement)) {
    extra <- read_coin_file(
      paxg_supplement,
      coin_id = coin_id,
      repository_name = repository_name,
      supplement = TRUE
    )
    rows <- rbind(rows, extra)
  }
  rows <- rows[rows$date >= start_date & rows$date <= end_date, , drop = FALSE]
  all_rows[[repository_name]] <- rows
  manifest[[repository_name]] <- unique(rows[, c(
    "repository_name", "coin_id", "source_group", "source_file", "priority", "supplement"
  )])
}

source_rows <- do.call(rbind, all_rows)
ord <- order(
  source_rows$repository_name,
  source_rows$date,
  source_rows$priority,
  source_rows$source_row
)
source_rows <- source_rows[ord, , drop = FALSE]
key <- paste(source_rows$repository_name, source_rows$date)
resolved <- source_rows[!duplicated(key, fromLast = TRUE), , drop = FALSE]

calendar <- data.frame(Date = seq(start_date, end_date, by = "day"))
price_panel <- calendar
mcap_panel <- calendar
coverage <- list()
for (repository_name in coins$repository_name) {
  z <- resolved[resolved$repository_name == repository_name, , drop = FALSE]
  price_piece <- z[, c("date", "price")]
  mcap_piece <- z[, c("date", "market_cap")]
  names(price_piece) <- c("Date", repository_name)
  names(mcap_piece) <- c("Date", repository_name)
  price_panel <- merge(price_panel, price_piece, by = "Date", all.x = TRUE, sort = TRUE)
  mcap_panel <- merge(mcap_panel, mcap_piece, by = "Date", all.x = TRUE, sort = TRUE)
  coverage[[repository_name]] <- data.frame(
    series = repository_name,
    first_observation = format(min(z$date[is.finite(z$price)])),
    last_observation = format(max(z$date[is.finite(z$price)])),
    price_missing_days = sum(!is.finite(price_panel[[repository_name]])),
    market_cap_missing_days = sum(!is.finite(mcap_panel[[repository_name]])),
    stringsAsFactors = FALSE
  )
}

format_date <- function(x) format(x, "%m/%d/%Y")
price_out <- price_panel
mcap_out <- mcap_panel
price_out$Date <- format_date(price_out$Date)
mcap_out$Date <- format_date(mcap_out$Date)

write.csv(
  price_out,
  file.path(output_dir, "Stable_Price_20260531.csv"),
  row.names = FALSE,
  quote = FALSE,
  na = ""
)
write.csv(
  mcap_out,
  file.path(output_dir, "Stable_Mktcap_20260531.csv"),
  row.names = FALSE,
  quote = FALSE,
  na = ""
)
write.csv(
  unique(do.call(rbind, manifest)),
  file.path(audit_dir, "stablecoin_source_manifest.csv"),
  row.names = FALSE,
  quote = FALSE
)
write.csv(
  do.call(rbind, coverage),
  file.path(audit_dir, "stablecoin_coverage.csv"),
  row.names = FALSE,
  quote = FALSE
)

# Build the macro input in the repository's exact five-column order.
repo_macro_path <- file.path(repo_input_dir, "Stable_Macro_20250302.csv")
if (!file.exists(repo_macro_path)) {
  repo_macro_path <- file.path(
    project_dir,
    "data",
    "support",
    "repo_Stable_Macro_20250302.csv"
  )
}
if (!file.exists(repo_macro_path)) stop("Repository macro input not found: ", repo_macro_path)
if (!file.exists(public_macro_path)) stop("Public macro support file not found: ", public_macro_path)
if (!file.exists(model_ready_macro_path)) {
  stop("Model-ready macro extension not found: ", model_ready_macro_path)
}
if (!file.exists(macro_extension_audit_path)) {
  stop("Macro extension audit not found: ", macro_extension_audit_path)
}

repo_macro <- read.csv(repo_macro_path, check.names = FALSE, stringsAsFactors = FALSE)
repo_macro$Date <- as.Date(repo_macro$Date, "%m/%d/%Y")
public_macro <- read.csv(public_macro_path, check.names = FALSE, stringsAsFactors = FALSE)
public_macro$date <- as.Date(public_macro$date)
model_ready_macro <- read.csv(
  model_ready_macro_path,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
model_ready_macro$date <- as.Date(model_ready_macro$date)

overlap <- merge(repo_macro, public_macro, by.x = "Date", by.y = "date")
repo_exact_end <- as.Date("2022-05-20")
overlap_exact <- overlap[overlap$Date <= repo_exact_end, , drop = FALSE]
fit_usd_yield <- lm(BV010082.Index ~ DGS1_pct, data = overlap_exact)
fit_cvix <- lm(CVIX.Index ~ CVIX_realized_proxy_63d, data = overlap_exact)

macro_calendar <- data.frame(Date = seq(start_date, end_date, by = "day"))
macro <- merge(macro_calendar, repo_macro, by = "Date", all.x = TRUE, sort = TRUE)
macro[macro$Date > repo_exact_end, -1] <- NA_real_
extension <- model_ready_macro[
  model_ready_macro$date > repo_exact_end & model_ready_macro$date <= end_date,
  ,
  drop = FALSE
]
names(extension)[1] <- "Date"
for (nm in names(extension)[-1]) {
  idx <- match(extension$Date, macro$Date)
  macro[idx, nm] <- extension[[nm]]
}

macro_out <- macro
macro_out$Date <- format_date(macro_out$Date)
write.csv(
  macro_out,
  file.path(output_dir, "Stable_Macro_20260531.csv"),
  row.names = FALSE,
  quote = FALSE,
  na = ""
)

macro_audit <- read.csv(
  macro_extension_audit_path,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
write.csv(
  macro_audit,
  file.path(audit_dir, "macro_method_audit.csv"),
  row.names = FALSE,
  quote = FALSE
)

message("Prepared repository-format inputs in: ", normalizePath(output_dir))
