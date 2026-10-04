#!/usr/bin/env Rscript
# Descriptive, pre-specified SVB cross-check for Section 5.5.
# This script does not re-estimate the Quantile-Lasso network or DPI.
options(stringsAsFactors = FALSE, digits = 15)

arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(arg) != 1L) stop("Run with Rscript.")
section_root <- dirname(dirname(normalizePath(sub("^--file=", "", arg))))
study_root <- dirname(dirname(section_root))

node_file <- file.path(section_root, "source_data", "Daily_Node_Centrality.csv")
onchain_file <- file.path(study_root, "outputs", "onchain_microdata_20260919",
                          "stablecoin_onchain_daily_20200101_20260531.csv")
stopifnot(file.exists(node_file), file.exists(onchain_file))

nodes <- read.csv(node_file, check.names = FALSE)
onchain <- read.csv(onchain_file, check.names = FALSE)
nodes$date <- as.Date(nodes$date)
onchain$date <- as.Date(onchain$date)
stopifnot(!anyDuplicated(onchain$date),
          !anyDuplicated(nodes[c("date", "coin")]),
          all(c("USDC", "DAI", "USDT") %in% nodes$coin))

usdc <- nodes[nodes$coin == "USDC",
              c("date", "in_strength", "out_strength")]
names(usdc)[-1] <- c("usdc_in_strength", "usdc_out_strength")
panel <- merge(onchain, usdc, by = "date", all = FALSE, sort = TRUE)

# Section 4.4 fixed the SVB event date as 2023-03-11. We exclude 2023-03-10
# from the pre-event baseline because stress had already begun that day.
baseline <- panel[panel$date >= as.Date("2023-02-08") &
                    panel$date <= as.Date("2023-03-09"), ]
shock <- panel[panel$date >= as.Date("2023-03-11") &
                 panel$date <= as.Date("2023-03-12"), ]
stopifnot(nrow(baseline) == 30L, nrow(shock) == 2L,
          all(baseline$curve_3pool_hours == 24L),
          all(shock$curve_3pool_hours == 24L),
          all(baseline$uni_usdc_usdt_hours == 24L),
          all(shock$uni_usdc_usdt_hours == 24L),
          all(baseline$uni_dai_usdc_hours == 24L),
          all(shock$uni_dai_usdc_hours == 24L),
          all(baseline$uni_usdc_usdt_liquidity_snapshot_hours == 24L),
          all(shock$uni_usdc_usdt_liquidity_snapshot_hours == 24L))

metrics <- c(
  "usdc_in_strength",
  "usdc_out_strength",
  "curve_3pool_imbalance_eod",
  "curve_3pool_swap_count",
  "uni_usdc_usdt_swap_count",
  "uni_dai_usdc_swap_count",
  "uni_usdc_usdt_inventory_pm25ticks_usd_eod"
)
stopifnot(all(metrics %in% names(panel)))
stopifnot(all(vapply(baseline[metrics], function(x) all(is.finite(x)), logical(1))),
          all(vapply(shock[metrics], function(x) all(is.finite(x)), logical(1))))

baseline_median <- vapply(baseline[metrics], median, numeric(1))
shock_median <- vapply(shock[metrics], median, numeric(1))
table <- data.frame(
  metric = metrics,
  baseline_start = "2023-02-08",
  baseline_end = "2023-03-09",
  baseline_days = nrow(baseline),
  shock_start = "2023-03-11",
  shock_end = "2023-03-12",
  shock_days = nrow(shock),
  baseline_median = unname(baseline_median),
  shock_median = unname(shock_median),
  shock_to_baseline_ratio = unname(shock_median / baseline_median)
)
write.csv(table,
          file.path(section_root, "tables", "Table_S5_5_SVB_Onchain_Crosscheck.csv"),
          row.names = FALSE, na = "")

event_daily <- panel[panel$date >= as.Date("2023-03-09") &
                       panel$date <= as.Date("2023-03-15"),
                     c("date", metrics)]
write.csv(event_daily,
          file.path(section_root, "source_data", "SVB_Onchain_Crosscheck_Daily.csv"),
          row.names = FALSE, na = "")
print(table, row.names = FALSE)
