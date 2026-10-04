#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE, scipen = 999)

project_root <- normalizePath(getwd(), mustWork = TRUE)
out_dir <- file.path(project_root, "outputs", "section_6_1_market_concentration")
dirs <- file.path(out_dir, c("source_data", "tables", "figures", "qa"))
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

local_library <- file.path(project_root, "work", "section_4_5_stable_crypto", "r_library")
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))

required <- c("ggplot2", "svglite", "ragg", "jsonlite")
missing_packages <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Missing R packages: ", paste(missing_packages, collapse = ", "))

library(ggplot2)

data_root <- Sys.getenv(
  "FRM_COINGECKO_ROOT",
  unset = file.path(project_root, "data", "raw", "coingecko_subset")
)

source_dirs <- c(
  "coin_gecko",
  "coin_gecko_20250910_112201",
  "20260110_133530_start_20250909_end_20260110",
  "20260408_133530_coingecko_start_20260111_end_20260406",
  "20260531_130117_stablecoins"
)

coin_map <- data.frame(
  file_id = c(
    "tether", "usd-coin", "binance-usd", "dai", "true-usd",
    "paxos-standard", "pax-gold", "gemini-dollar", "nusd",
    "stasis-eurs", "rupiah-token"
  ),
  coin = c("USDT", "USDC", "BUSD", "DAI", "TUSD", "USDP", "PAXG", "GUSD", "sUSD", "EURS", "IDRT")
)

start_date <- as.Date("2020-01-01")
end_date <- as.Date("2026-05-31")

read_source <- function(file_path, source_order, source_label, file_id, coin) {
  x <- read.csv(file_path, check.names = FALSE)
  needed <- c("date", "market_caps")
  if (!all(needed %in% names(x))) stop("Required columns are missing from ", file_path)
  out <- data.frame(
    date = as.Date(substr(as.character(x$date), 1, 10)),
    coin = coin,
    market_cap_usd = suppressWarnings(as.numeric(x$market_caps)),
    source_order = source_order,
    source_label = source_label,
    source_file = paste0(file_id, ".csv")
  )
  out[out$date >= start_date & out$date <= end_date & !is.na(out$date), ]
}

pieces <- list()
manifest <- list()
k <- 0L
for (j in seq_along(source_dirs)) {
  source_label <- source_dirs[j]
  for (i in seq_len(nrow(coin_map))) {
    file_path <- file.path(data_root, source_label, paste0(coin_map$file_id[i], ".csv"))
    if (!file.exists(file_path)) next
    k <- k + 1L
    z <- read_source(file_path, j, source_label, coin_map$file_id[i], coin_map$coin[i])
    pieces[[k]] <- z
    manifest[[k]] <- data.frame(
      source_order = j,
      source_label = source_label,
      source_file = basename(file_path),
      coin = coin_map$coin[i],
      first_date = if (nrow(z)) min(z$date) else as.Date(NA),
      last_date = if (nrow(z)) max(z$date) else as.Date(NA),
      rows_in_range = nrow(z),
      positive_market_cap_rows = sum(is.finite(z$market_cap_usd) & z$market_cap_usd > 0)
    )
  }
}
frozen_selected_file <- Sys.getenv(
  "FRM_CONCENTRATION_FROZEN_PANEL",
  unset = file.path(out_dir, "source_data", "market_cap_selected_long.csv")
)

if (length(pieces)) {
  # A frozen public-API extract fills the only identified end-of-sample gap:
  # PAXG after 6 April 2026. The hourly response is reduced to the first UTC
  # observation per date, matching the daily timestamp convention of the files.
  paxg_api_file <- file.path(
    out_dir, "source_data", "input", "paxg_market_chart_20260407_20260531.json"
  )
  if (file.exists(paxg_api_file)) {
    api <- jsonlite::fromJSON(paxg_api_file)
    api_cap <- as.data.frame(api$market_caps)
    names(api_cap) <- c("timestamp_ms", "market_cap_usd")
    api_cap$date <- as.Date(
      as.POSIXct(api_cap$timestamp_ms / 1000, origin = "1970-01-01", tz = "UTC"),
      tz = "UTC"
    )
    api_cap <- api_cap[order(api_cap$date, api_cap$timestamp_ms), ]
    api_cap <- api_cap[!duplicated(api_cap$date), ]
    api_cap <- api_cap[api_cap$date >= as.Date("2026-04-07") & api_cap$date <= end_date, ]
    api_piece <- data.frame(
      date = api_cap$date,
      coin = "PAXG",
      market_cap_usd = suppressWarnings(as.numeric(api_cap$market_cap_usd)),
      source_order = length(source_dirs) + 1L,
      source_label = "coingecko_api_paxg_gap",
      source_file = basename(paxg_api_file)
    )
    k <- k + 1L
    pieces[[k]] <- api_piece
    manifest[[k]] <- data.frame(
      source_order = length(source_dirs) + 1L,
      source_label = "coingecko_api_paxg_gap",
      source_file = basename(paxg_api_file),
      coin = "PAXG",
      first_date = min(api_piece$date),
      last_date = max(api_piece$date),
      rows_in_range = nrow(api_piece),
      positive_market_cap_rows = sum(is.finite(api_piece$market_cap_usd) & api_piece$market_cap_usd > 0)
    )
  }

  raw_all <- do.call(rbind, pieces)
  source_manifest <- do.call(rbind, manifest)
  raw_all <- raw_all[order(raw_all$coin, raw_all$date, raw_all$source_order), ]

  # The most recent local download has priority on overlapping dates. No missing
  # market capitalisation is filled, interpolated, carried forward, or set to zero.
  selected <- raw_all[!duplicated(raw_all[c("coin", "date")], fromLast = TRUE), ]
  selected <- selected[order(selected$date, selected$coin), ]
} else {
  # Portable fallback: the package contains the exact selected input panel.
  if (!file.exists(frozen_selected_file)) {
    stop("No CoinGecko inputs found and the frozen selected panel is unavailable.")
  }
  selected <- read.csv(frozen_selected_file, check.names = FALSE)
  selected$date <- as.Date(selected$date)
  selected$market_cap_usd <- suppressWarnings(as.numeric(selected$market_cap_usd))
  selected <- selected[selected$date >= start_date & selected$date <= end_date, ]
  selected <- selected[order(selected$date, selected$coin), ]
  manifest_file <- file.path(out_dir, "qa", "source_manifest.csv")
  source_manifest <- if (file.exists(manifest_file)) {
    read.csv(manifest_file, check.names = FALSE)
  } else {
    data.frame(
      source_order = NA_integer_, source_label = "frozen_selected_panel",
      source_file = basename(frozen_selected_file), coin = NA_character_,
      first_date = start_date, last_date = end_date, rows_in_range = nrow(selected),
      positive_market_cap_rows = sum(is.finite(selected$market_cap_usd) & selected$market_cap_usd > 0)
    )
  }
}

calendar <- expand.grid(
  date = seq(start_date, end_date, by = "day"),
  coin = coin_map$coin,
  KEEP.OUT.ATTRS = FALSE
)
panel <- merge(calendar, selected[c("date", "coin", "market_cap_usd", "source_label")],
               by = c("date", "coin"), all.x = TRUE, sort = TRUE)
panel$observed_positive <- is.finite(panel$market_cap_usd) & panel$market_cap_usd > 0

calc_day <- function(d) {
  z <- panel[panel$date == d & panel$observed_positive, c("coin", "market_cap_usd")]
  n <- nrow(z)
  if (n < 2L || sum(z$market_cap_usd) <= 0) {
    return(data.frame(
      date = d, active_coins = n, sample_market_cap_usd = NA_real_,
      hhi = NA_real_, nhhi = NA_real_, top1_share = NA_real_,
      top2_share = NA_real_, effective_number = NA_real_, top1_coin = NA_character_
    ))
  }
  z$share <- z$market_cap_usd / sum(z$market_cap_usd)
  z <- z[order(z$share, decreasing = TRUE), ]
  hhi <- sum(z$share^2)
  nhhi <- (hhi - 1 / n) / (1 - 1 / n)
  data.frame(
    date = d,
    active_coins = n,
    sample_market_cap_usd = sum(z$market_cap_usd),
    hhi = hhi,
    nhhi = nhhi,
    top1_share = z$share[1],
    top2_share = sum(head(z$share, 2)),
    effective_number = 1 / hhi,
    top1_coin = z$coin[1]
  )
}

daily <- do.call(rbind, lapply(seq(start_date, end_date, by = "day"), calc_day))
daily$date <- as.Date(daily$date, origin = "1970-01-01")
daily <- daily[order(daily$date), ]

share_panel <- merge(
  panel,
  daily[c("date", "sample_market_cap_usd")],
  by = "date",
  all.x = TRUE,
  sort = TRUE
)
share_panel$market_share <- ifelse(
  share_panel$observed_positive & share_panel$sample_market_cap_usd > 0,
  share_panel$market_cap_usd / share_panel$sample_market_cap_usd,
  NA_real_
)

roll_median <- function(x, k = 31L) {
  if (length(x) < k) return(rep(NA_real_, length(x)))
  stats::runmed(x, k = k, endrule = "median")
}
daily$nhhi_roll31 <- roll_median(daily$nhhi)
daily$top2_roll31 <- roll_median(daily$top2_share)

year <- format(daily$date, "%Y")
annual <- do.call(rbind, lapply(split(daily, year), function(z) {
  data.frame(
    year = unique(format(z$date, "%Y")),
    days = nrow(z),
    median_active_coins = median(z$active_coins, na.rm = TRUE),
    median_sample_market_cap_usd = median(z$sample_market_cap_usd, na.rm = TRUE),
    median_hhi = median(z$hhi, na.rm = TRUE),
    median_nhhi = median(z$nhhi, na.rm = TRUE),
    q25_nhhi = unname(quantile(z$nhhi, 0.25, na.rm = TRUE)),
    q75_nhhi = unname(quantile(z$nhhi, 0.75, na.rm = TRUE)),
    median_top1_share = median(z$top1_share, na.rm = TRUE),
    median_top2_share = median(z$top2_share, na.rm = TRUE),
    modal_top1_coin = names(sort(table(z$top1_coin), decreasing = TRUE))[1]
  )
}))

coin_summary <- do.call(rbind, lapply(split(share_panel, share_panel$coin), function(z) {
  valid <- is.finite(z$market_share)
  end_row <- z[which(valid & z$date == max(z$date[valid]))[1], ]
  data.frame(
    coin = unique(z$coin),
    observed_positive_days = sum(z$observed_positive),
    first_positive_date = if (any(z$observed_positive)) min(z$date[z$observed_positive]) else as.Date(NA),
    last_positive_date = if (any(z$observed_positive)) max(z$date[z$observed_positive]) else as.Date(NA),
    mean_share = mean(z$market_share, na.rm = TRUE),
    median_share = median(z$market_share, na.rm = TRUE),
    end_sample_share = if (nrow(end_row)) end_row$market_share else NA_real_
  )
}))
coin_summary <- coin_summary[order(coin_summary$mean_share, decreasing = TRUE), ]

first30 <- daily[daily$date <= start_date + 29, ]
last30 <- daily[daily$date >= end_date - 29, ]
overall <- data.frame(
  sample_start = start_date,
  sample_end = end_date,
  calendar_days = nrow(daily),
  median_active_coins = median(daily$active_coins, na.rm = TRUE),
  min_active_coins = min(daily$active_coins, na.rm = TRUE),
  max_active_coins = max(daily$active_coins, na.rm = TRUE),
  median_nhhi = median(daily$nhhi, na.rm = TRUE),
  first30_median_nhhi = median(first30$nhhi, na.rm = TRUE),
  last30_median_nhhi = median(last30$nhhi, na.rm = TRUE),
  first30_median_top2 = median(first30$top2_share, na.rm = TRUE),
  last30_median_top2 = median(last30$top2_share, na.rm = TRUE),
  min_nhhi = min(daily$nhhi, na.rm = TRUE),
  min_nhhi_date = daily$date[which.min(daily$nhhi)],
  max_nhhi = max(daily$nhhi, na.rm = TRUE),
  max_nhhi_date = daily$date[which.max(daily$nhhi)],
  median_effective_number = median(daily$effective_number, na.rm = TRUE)
)

share_sum <- aggregate(market_share ~ date, share_panel, sum, na.rm = TRUE)
checks <- data.frame(
  check = c(
    "complete_calendar", "all_daily_metrics_finite", "shares_sum_to_one",
    "hhi_bounds", "nhhi_bounds", "top_share_order", "no_imputed_market_cap"
  ),
  status = c(
    nrow(daily) == length(seq(start_date, end_date, by = "day")),
    all(is.finite(daily$hhi)) && all(is.finite(daily$nhhi)),
    max(abs(share_sum$market_share - 1), na.rm = TRUE) < 1e-10,
    all(daily$hhi >= 1 / daily$active_coins - 1e-10 & daily$hhi <= 1 + 1e-10),
    all(daily$nhhi >= -1e-10 & daily$nhhi <= 1 + 1e-10),
    all(daily$top2_share + 1e-10 >= daily$top1_share & daily$top2_share <= 1 + 1e-10),
    TRUE
  ),
  detail = c(
    paste(nrow(daily), "daily rows"),
    paste(sum(!is.finite(daily$nhhi)), "non-finite NHHI values"),
    sprintf("maximum absolute share-sum error %.3g", max(abs(share_sum$market_share - 1), na.rm = TRUE)),
    sprintf("HHI range %.4f to %.4f", min(daily$hhi), max(daily$hhi)),
    sprintf("NHHI range %.4f to %.4f", min(daily$nhhi), max(daily$nhhi)),
    "top-1 <= top-2 <= 1",
    "missing and non-positive values excluded day by day; no zero fill, interpolation or carry-forward"
  )
)
checks$status <- ifelse(checks$status, "PASS", "FAIL")
if (any(checks$status == "FAIL")) stop("Concentration QA failed; inspect qa/concentration_QA.csv")

write.csv(selected, file.path(out_dir, "source_data", "market_cap_selected_long.csv"), row.names = FALSE)
write.csv(panel, file.path(out_dir, "source_data", "market_cap_calendar_panel.csv"), row.names = FALSE)
write.csv(share_panel, file.path(out_dir, "source_data", "market_cap_share_panel.csv"), row.names = FALSE)
write.csv(daily, file.path(out_dir, "tables", "Table_6_1_Daily_Concentration.csv"), row.names = FALSE)
write.csv(annual, file.path(out_dir, "tables", "Table_S6_1_Annual_Concentration.csv"), row.names = FALSE)
write.csv(coin_summary, file.path(out_dir, "tables", "Table_S6_1_Coin_Shares.csv"), row.names = FALSE)
write.csv(overall, file.path(out_dir, "tables", "Table_S6_1_Overall_Summary.csv"), row.names = FALSE)
write.csv(source_manifest, file.path(out_dir, "qa", "source_manifest.csv"), row.names = FALSE)
write.csv(checks, file.path(out_dir, "qa", "concentration_QA.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(out_dir, "qa", "session_info.txt"))

plot_data <- rbind(
  data.frame(date = daily$date, series = "Normalized HHI", value = daily$nhhi_roll31),
  data.frame(date = daily$date, series = "Top-2 market share", value = daily$top2_roll31)
)

palette <- c("Normalized HHI" = "#245B8A", "Top-2 market share" = "#C66A2B")
p <- ggplot(plot_data, aes(date, value, colour = series)) +
  geom_line(linewidth = 0.72, lineend = "round") +
  scale_colour_manual(values = palette, name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Concentration (0-1)") +
  theme_classic(base_size = 8, base_family = "Helvetica") +
  theme(
    axis.line = element_line(linewidth = 0.35, colour = "black"),
    axis.ticks = element_line(linewidth = 0.35, colour = "black"),
    axis.text = element_text(colour = "black", size = 7),
    axis.title = element_text(colour = "black", size = 8),
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(0, 0, 2, 0),
    legend.key.width = grid::unit(11, "mm"),
    legend.text = element_text(size = 7),
    plot.margin = margin(4, 7, 4, 4)
  )

figure_base <- file.path(out_dir, "figures", "Figure_6_1_Stablecoin_Market_Concentration")
width_mm <- 160
height_mm <- 82
w <- width_mm / 25.4
h <- height_mm / 25.4

svglite::svglite(paste0(figure_base, ".svg"), width = w, height = h)
print(p)
dev.off()
cairo_ok <- tryCatch(
  withCallingHandlers(
    {
      grDevices::cairoVersion()
      TRUE
    },
    warning = function(warning_condition) stop(warning_condition)
  ),
  error = function(error_condition) FALSE
)
if (cairo_ok) {
  grDevices::cairo_pdf(paste0(figure_base, ".pdf"), width = w, height = h,
                       family = "Helvetica")
} else {
  grDevices::pdf(paste0(figure_base, ".pdf"), width = w, height = h,
                 family = "Helvetica", useDingbats = FALSE)
}
print(p)
dev.off()
ragg::agg_tiff(paste0(figure_base, ".tiff"), width = w, height = h, units = "in", res = 600)
print(p)
dev.off()
ragg::agg_png(paste0(figure_base, ".png"), width = w, height = h, units = "in", res = 300)
print(p)
dev.off()

write.csv(plot_data, file.path(out_dir, "figures", "Figure_6_1_source_data.csv"), row.names = FALSE)
message("Section 6.1 market-concentration outputs written to: ", out_dir)
