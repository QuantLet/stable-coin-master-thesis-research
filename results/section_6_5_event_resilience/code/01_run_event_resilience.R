#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE, scipen = 999)

project_root <- normalizePath(getwd(), mustWork = TRUE)
bundle_dir <- file.path(project_root, "outputs", "section_6_5_event_resilience")
dirs <- file.path(bundle_dir, c("source_data", "source_data/input", "tables", "figures", "qa"))
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

local_library <- file.path(project_root, "work", "section_4_5_stable_crypto", "r_library")
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))

required <- c("ggplot2", "svglite", "ragg")
missing_packages <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Missing R packages: ", paste(missing_packages, collapse = ", "))
library(ggplot2)

input_dir <- file.path(bundle_dir, "source_data", "input")
daily_file <- file.path(input_dir, "Portfolio_Daily_Returns.csv")
events_file <- file.path(input_dir, "events_primary.csv")
if (!all(file.exists(c(daily_file, events_file)))) stop("Frozen Section 6.5 inputs are incomplete.")

daily <- read.csv(daily_file, check.names = FALSE)
events <- read.csv(events_file, check.names = FALSE)
daily$date <- as.Date(daily$date)
events$event_date <- as.Date(events$event_date)

strategies <- c("EW", "GMV", "LTEC", "QTEC")
comparators <- c("GMV", "LTEC", "QTEC")
if (!all(strategies %in% names(daily))) stop("Portfolio return columns are missing.")
if (anyDuplicated(daily$date) || is.unsorted(daily$date)) stop("Daily dates must be unique and sorted.")
if (any(!is.finite(as.matrix(daily[, strategies])))) stop("Portfolio returns contain non-finite values.")
if (nrow(events) != 6L || anyDuplicated(events$event_id)) stop("The frozen event family must contain six unique events.")

events$event_short <- c(
  kucoin = "KuCoin",
  china_crash = "May 2021 sell-off",
  terra = "Terra/UST",
  ftx = "FTX",
  svb = "SVB/USDC",
  hormuz = "Iran-Hormuz"
)[events$event_id]
if (anyNA(events$event_short)) stop("Short event labels are incomplete.")

annualized_volatility <- function(r) 100 * stats::sd(log1p(r)) * sqrt(365)
expected_shortfall_5 <- function(r) {
  cutoff <- as.numeric(stats::quantile(r, 0.05, type = 8, names = FALSE))
  -10000 * mean(r[r <= cutoff])
}
maximum_drawdown <- function(r) {
  wealth <- c(1, cumprod(1 + r))
  100 * max(1 - wealth / cummax(wealth))
}
cumulative_return <- function(r) 100 * (prod(1 + r) - 1)
worst_daily_loss <- function(r) -10000 * min(r)
metric_value <- function(r, metric) {
  switch(metric,
         AnnVol = annualized_volatility(r),
         ES5 = expected_shortfall_5(r),
         stop("Unknown metric: ", metric))
}

event_rows <- lapply(events$event_date, function(d) which(daily$date >= d & daily$date <= d + 30L))
names(event_rows) <- events$event_id
acute_rows <- lapply(events$event_date, function(d) which(daily$date >= d & daily$date <= d + 7L))
names(acute_rows) <- events$event_id
coverage <- data.frame(
  event_id = events$event_id,
  event_date = events$event_date,
  stress_days_0_30 = lengths(event_rows),
  acute_days_0_7 = lengths(acute_rows),
  stringsAsFactors = FALSE
)
if (any(coverage$stress_days_0_30 != 31L) || any(coverage$acute_days_0_7 != 8L)) {
  stop("At least one frozen event window lacks complete portfolio-return coverage.")
}
all_stress_rows <- unlist(event_rows, use.names = FALSE)
if (anyDuplicated(all_stress_rows)) stop("Event windows overlap.")

event_long <- do.call(rbind, lapply(seq_len(nrow(events)), function(i) {
  rows_30 <- event_rows[[events$event_id[i]]]
  rows_7 <- acute_rows[[events$event_id[i]]]
  do.call(rbind, lapply(strategies, function(strategy) {
    r30 <- daily[[strategy]][rows_30]
    r7 <- daily[[strategy]][rows_7]
    data.frame(
      event_id = events$event_id[i],
      event_label = events$event_label[i],
      event_short = events$event_short[i],
      event_date = events$event_date[i],
      shock_type = events$shock_type[i],
      origin = events$origin[i],
      strategy = strategy,
      n_days_0_30 = length(r30),
      cumulative_return_0_30_pct = cumulative_return(r30),
      annualized_volatility_0_30_pct = annualized_volatility(r30),
      maximum_drawdown_0_30_pct = maximum_drawdown(r30),
      worst_daily_loss_0_30_bps = worst_daily_loss(r30),
      cumulative_return_0_7_pct = cumulative_return(r7),
      maximum_drawdown_0_7_pct = maximum_drawdown(r7),
      stringsAsFactors = FALSE
    )
  }))
}))
write.csv(event_long, file.path(bundle_dir, "tables", "Table_S6_5_Event_Level_Performance.csv"), row.names = FALSE)

ew_dd <- event_long[event_long$strategy == "EW", c("event_id", "maximum_drawdown_0_30_pct")]
names(ew_dd)[2] <- "ew_maximum_drawdown_0_30_pct"
figure_source <- merge(event_long[event_long$strategy %in% comparators, ], ew_dd,
                       by = "event_id", all.x = TRUE, sort = FALSE)
figure_source$drawdown_difference_pp <-
  figure_source$maximum_drawdown_0_30_pct - figure_source$ew_maximum_drawdown_0_30_pct
figure_source <- figure_source[match(
  paste(rep(events$event_id, each = length(comparators)), rep(comparators, times = nrow(events))),
  paste(figure_source$event_id, figure_source$strategy)
), ]
write.csv(figure_source[, c(
  "event_id", "event_label", "event_short", "event_date", "shock_type", "origin", "strategy",
  "maximum_drawdown_0_30_pct", "ew_maximum_drawdown_0_30_pct", "drawdown_difference_pp"
)], file.path(bundle_dir, "figures", "Figure_6_5_source_data.csv"), row.names = FALSE)

stress_returns <- daily[all_stress_rows, c("date", strategies), drop = FALSE]
write.csv(stress_returns, file.path(bundle_dir, "source_data", "Pooled_Event_Window_Returns.csv"), row.names = FALSE)

pooled_performance <- do.call(rbind, lapply(strategies, function(strategy) {
  r <- stress_returns[[strategy]]
  event_dd <- event_long$maximum_drawdown_0_30_pct[event_long$strategy == strategy]
  data.frame(
    strategy = strategy,
    n_event_days = length(r),
    annualized_volatility_pct = annualized_volatility(r),
    expected_shortfall_5_bps = expected_shortfall_5(r),
    mean_event_maximum_drawdown_pct = mean(event_dd),
    worst_event_maximum_drawdown_pct = max(event_dd),
    stringsAsFactors = FALSE
  )
}))

stratified_block_indices <- function(rows_by_event, block_length) {
  unlist(lapply(rows_by_event, function(rows) {
    n <- length(rows)
    n_blocks <- ceiling(n / block_length)
    starts <- sample.int(n - block_length + 1L, n_blocks, replace = TRUE)
    local_idx <- unlist(lapply(starts, function(s) s:(s + block_length - 1L)), use.names = FALSE)
    rows[local_idx[seq_len(n)]]
  }), use.names = FALSE)
}

run_bootstrap <- function(block_length, B = 4999L, seed = 6505L,
                          rows_by_event = event_rows) {
  metrics <- c("AnnVol", "ES5")
  pooled_rows <- unlist(rows_by_event, use.names = FALSE)
  observed <- expand.grid(strategy = comparators, metric = metrics, stringsAsFactors = FALSE)
  observed$estimate <- mapply(function(strategy, metric) {
    metric_value(daily[[strategy]][pooled_rows], metric) -
      metric_value(daily$EW[pooled_rows], metric)
  }, observed$strategy, observed$metric)

  boot_store <- array(NA_real_, dim = c(B, length(comparators), length(metrics)),
                      dimnames = list(NULL, comparators, metrics))
  set.seed(seed + block_length)
  for (b in seq_len(B)) {
    idx <- stratified_block_indices(rows_by_event, block_length)
    for (strategy in comparators) {
      for (metric in metrics) {
        boot_store[b, strategy, metric] <-
          metric_value(daily[[strategy]][idx], metric) - metric_value(daily$EW[idx], metric)
      }
    }
  }

  out <- observed
  out$ci95_low <- NA_real_
  out$ci95_high <- NA_real_
  out$p_value <- NA_real_
  for (i in seq_len(nrow(out))) {
    x <- boot_store[, out$strategy[i], out$metric[i]]
    out$ci95_low[i] <- stats::quantile(x, 0.025, names = FALSE, type = 8)
    out$ci95_high[i] <- stats::quantile(x, 0.975, names = FALSE, type = 8)
    p_lower <- (1 + sum(x <= 0)) / (B + 1)
    p_upper <- (1 + sum(x >= 0)) / (B + 1)
    out$p_value[i] <- min(1, 2 * min(p_lower, p_upper))
  }
  out$p_holm <- p.adjust(out$p_value, method = "holm")
  out$block_length <- block_length
  out$bootstrap_repetitions <- B
  out
}

inference_main <- run_bootstrap(7L, B = 4999L, seed = 6505L)
inference_sensitivity <- do.call(rbind, lapply(c(3L, 14L), function(bl) {
  run_bootstrap(bl, B = 4999L, seed = 6505L)
}))
leave_one_event_out <- do.call(rbind, lapply(seq_len(nrow(events)), function(i) {
  kept_rows <- event_rows[-i]
  out <- run_bootstrap(7L, B = 1999L, seed = 6515L + i, rows_by_event = kept_rows)
  out$omitted_event_id <- events$event_id[i]
  out$omitted_event_label <- events$event_label[i]
  out$n_event_days <- sum(lengths(kept_rows))
  out
}))
write.csv(inference_main, file.path(bundle_dir, "tables", "Table_6_5_Pooled_Event_Inference.csv"), row.names = FALSE)
write.csv(inference_sensitivity, file.path(bundle_dir, "tables", "Table_S6_5_Block_Length_Sensitivity.csv"), row.names = FALSE)
write.csv(leave_one_event_out, file.path(bundle_dir, "tables", "Table_S6_5_Leave_One_Event_Out.csv"), row.names = FALSE)

table_main <- pooled_performance
table_main$annvol_difference_pp <- NA_real_
table_main$annvol_ci <- "--"
table_main$annvol_p_holm <- NA_real_
table_main$es_difference_bps <- NA_real_
table_main$es_ci <- "--"
table_main$es_p_holm <- NA_real_
for (strategy in comparators) {
  iv <- inference_main[inference_main$strategy == strategy & inference_main$metric == "AnnVol", ]
  ie <- inference_main[inference_main$strategy == strategy & inference_main$metric == "ES5", ]
  j <- which(table_main$strategy == strategy)
  table_main$annvol_difference_pp[j] <- iv$estimate
  table_main$annvol_ci[j] <- sprintf("%.2f to %.2f", iv$ci95_low, iv$ci95_high)
  table_main$annvol_p_holm[j] <- iv$p_holm
  table_main$es_difference_bps[j] <- ie$estimate
  table_main$es_ci[j] <- sprintf("%.2f to %.2f", ie$ci95_low, ie$ci95_high)
  table_main$es_p_holm[j] <- ie$p_holm
}
write.csv(table_main, file.path(bundle_dir, "tables", "Table_6_5_Pooled_Event_Performance.csv"), row.names = FALSE)

fmt_p <- function(x) ifelse(is.na(x), "--", ifelse(x < 0.001, "<0.001", sprintf("%.3f", x)))
table_md <- c(
  "**Table 6.5 | Portfolio risk during major tail-event windows.**",
  "",
  "| Strategy | Ann. volatility (%) | Difference vs EW (95% CI), pp | Holm p | 5% ES (bp) | Difference vs EW (95% CI), bp | Holm p |",
  "|---|---:|---:|---:|---:|---:|---:|",
  vapply(seq_len(nrow(table_main)), function(i) {
    vol_diff <- if (table_main$strategy[i] == "EW") "--" else
      sprintf("%.2f (%s)", table_main$annvol_difference_pp[i], table_main$annvol_ci[i])
    es_diff <- if (table_main$strategy[i] == "EW") "--" else
      sprintf("%.2f (%s)", table_main$es_difference_bps[i], table_main$es_ci[i])
    sprintf("| %s | %.2f | %s | %s | %.2f | %s | %s |",
            table_main$strategy[i], table_main$annualized_volatility_pct[i], vol_diff,
            fmt_p(table_main$annvol_p_holm[i]), table_main$expected_shortfall_5_bps[i], es_diff,
            fmt_p(table_main$es_p_holm[i]))
  }, character(1)),
  "",
  paste0("The analysis pools the six non-overlapping days 0–30 event windows (n = ",
         nrow(stress_returns), " daily observations). Differences are paired against EW. ",
         "Intervals and p values use 4,999 event-stratified moving-block bootstrap replications ",
         "with seven-day blocks; Holm adjustment covers the six strategy–endpoint comparisons. ",
         "Negative differences indicate lower risk.")
)
writeLines(table_md, file.path(bundle_dir, "tables", "Table_6_5_Manuscript_Ready.md"))

plot_data <- figure_source
plot_data$strategy <- factor(plot_data$strategy, levels = comparators)
plot_data$event_short <- factor(plot_data$event_short, levels = rev(events$event_short))
palette <- c(GMV = "#315B8A", LTEC = "#C56A36", QTEC = "#76558A")
shape_values <- c(GMV = 16, LTEC = 17, QTEC = 15)
pd <- position_dodge(width = 0.52)

plot_obj <- ggplot(plot_data,
                   aes(x = drawdown_difference_pp, y = event_short,
                       colour = strategy, shape = strategy)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45, colour = "#707070") +
  geom_point(size = 2.6, stroke = 0.45, position = pd) +
  scale_colour_manual(values = palette, name = NULL) +
  scale_shape_manual(values = shape_values, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.08))) +
  labs(x = "Maximum-drawdown difference relative to EW (percentage points)", y = NULL) +
  theme_classic(base_size = 7.5, base_family = "Helvetica") +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(linewidth = 0.4, colour = "black"),
    axis.ticks.x = element_line(linewidth = 0.35, colour = "black"),
    axis.text = element_text(colour = "black", size = 7),
    axis.text.y = element_text(size = 7.2),
    axis.title.x = element_text(size = 7.4, margin = margin(t = 6)),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.text = element_text(size = 7),
    legend.key.width = grid::unit(8, "pt"),
    plot.margin = margin(t = 4, r = 7, b = 4, l = 4)
  )

figure_base <- file.path(bundle_dir, "figures", "Figure_6_5_Portfolio_Drawdowns_Tail_Events")
width_mm <- 150
height_mm <- 92
width_in <- width_mm / 25.4
height_in <- height_mm / 25.4

svglite::svglite(paste0(figure_base, ".svg"), width = width_in, height = height_in)
print(plot_obj)
grDevices::dev.off()

cairo_runtime_available <- isTRUE(capabilities("cairo")) &&
  file.exists("/opt/X11/lib/libXrender.1.dylib")
if (cairo_runtime_available) {
  grDevices::cairo_pdf(paste0(figure_base, ".pdf"), width = width_in, height = height_in,
                       family = "Helvetica")
} else {
  grDevices::pdf(paste0(figure_base, ".pdf"), width = width_in, height = height_in,
                 family = "Helvetica", useDingbats = FALSE)
}
print(plot_obj)
grDevices::dev.off()

ragg::agg_tiff(paste0(figure_base, ".tiff"), width = width_in, height = height_in,
               units = "in", res = 600, background = "white")
print(plot_obj)
grDevices::dev.off()

ragg::agg_png(paste0(figure_base, ".png"), width = width_in, height = height_in,
              units = "in", res = 300, background = "white")
print(plot_obj)
grDevices::dev.off()

caption <- paste0(
  "Figure 6.5 | Portfolio drawdowns across major tail events. Points show the difference in maximum drawdown between each strategy and the equal-weight portfolio (EW) during days 0–30 after each event. ",
  "Negative values indicate a smaller drawdown than EW. Portfolio weights are the leakage-free weights defined in Section 6.3 and are not re-estimated using event information. ",
  "The six events and dates are identical to the frozen event family in Section 4.4. Event-level points are descriptive; pooled stress-window inference is reported in Table 6.5. Source data are provided with the figure."
)
writeLines(caption, file.path(bundle_dir, "figures", "Figure_6_5_caption.txt"))

qa <- data.frame(
  check = c(
    "event_count", "stress_days_per_event", "acute_days_per_event", "pooled_stress_days",
    "event_windows_non_overlapping", "strategy_count", "return_panel_complete",
    "bootstrap_repetitions", "main_block_length", "primary_comparison_count",
    "holm_values_complete", "block_sensitivity_rows", "leave_one_event_out_rows",
    "figure_source_rows", "svg_exists", "pdf_exists",
    "tiff_exists", "png_exists"
  ),
  value = c(
    nrow(events), paste(coverage$stress_days_0_30, collapse = ";"),
    paste(coverage$acute_days_0_7, collapse = ";"), length(all_stress_rows),
    !anyDuplicated(all_stress_rows), length(strategies),
    all(is.finite(as.matrix(daily[, strategies]))), 4999L, 7L, nrow(inference_main),
    all(is.finite(inference_main$p_holm)), nrow(inference_sensitivity), nrow(leave_one_event_out),
    nrow(figure_source),
    file.exists(paste0(figure_base, ".svg")), file.exists(paste0(figure_base, ".pdf")),
    file.exists(paste0(figure_base, ".tiff")), file.exists(paste0(figure_base, ".png"))
  ),
  stringsAsFactors = FALSE
)
write.csv(coverage, file.path(bundle_dir, "qa", "event_window_coverage.csv"), row.names = FALSE)
write.csv(qa, file.path(bundle_dir, "qa", "analysis_QA.csv"), row.names = FALSE)
manifest <- data.frame(
  file = normalizePath(c(daily_file, events_file), mustWork = TRUE),
  md5 = unname(tools::md5sum(c(daily_file, events_file))),
  stringsAsFactors = FALSE
)
write.csv(manifest, file.path(bundle_dir, "qa", "input_manifest_md5.csv"), row.names = FALSE)
capture.output(sessionInfo(), file = file.path(bundle_dir, "qa", "session_info.txt"))

cat("Section 6.5 outputs created.\n\nPooled event-window performance:\n")
print(pooled_performance, row.names = FALSE)
cat("\nPrimary pooled event-window inference (negative is lower risk):\n")
print(inference_main, row.names = FALSE)
cat("\nEvent-level maximum-drawdown differences versus EW:\n")
print(figure_source[, c("event_short", "strategy", "drawdown_difference_pp")], row.names = FALSE)
