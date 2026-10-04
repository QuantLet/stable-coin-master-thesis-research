#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE, scipen = 999)

project_root <- normalizePath(getwd(), mustWork = TRUE)
bundle_dir <- file.path(project_root, "outputs", "section_6_4_oos_portfolio_performance")
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
weights_file <- file.path(input_dir, "Portfolio_Weights_Long.csv")
performance_file <- file.path(input_dir, "OOS_Performance.csv")
inference_file <- file.path(input_dir, "Block_Bootstrap_Differences.csv")
input_files <- c(daily_file, weights_file, performance_file, inference_file)
if (any(!file.exists(input_files))) stop("Frozen Section 6.3 inputs are incomplete.")

daily <- read.csv(daily_file, check.names = FALSE)
daily$date <- as.Date(daily$date)
performance <- read.csv(performance_file, check.names = FALSE)
inference <- read.csv(inference_file, check.names = FALSE)
weights <- read.csv(weights_file, check.names = FALSE)
weights$date <- as.Date(weights$date)

strategies <- c("EW", "GMV", "LTEC", "QTEC")
comparators <- c("GMV", "LTEC", "QTEC")
if (!all(strategies %in% names(daily))) stop("Portfolio return columns are missing.")
if (anyDuplicated(daily$date) || is.unsorted(daily$date)) stop("Daily dates must be unique and sorted.")
if (any(!is.finite(as.matrix(daily[, strategies])))) stop("Portfolio returns contain non-finite values.")
if (!identical(sort(unique(weights$strategy)), sort(strategies))) stop("Weight strategies differ from return strategies.")

annualized_volatility <- function(r) 100 * stats::sd(log1p(r)) * sqrt(365)
expected_shortfall_5 <- function(r) {
  cutoff <- as.numeric(stats::quantile(r, 0.05, type = 8, names = FALSE))
  -10000 * mean(r[r <= cutoff])
}
metric_value <- function(r, metric) {
  if (metric == "AnnVol") annualized_volatility(r) else expected_shortfall_5(r)
}
moving_block_indices <- function(n, block_length) {
  n_blocks <- ceiling(n / block_length)
  starts <- sample.int(n - block_length + 1L, n_blocks, replace = TRUE)
  unlist(lapply(starts, function(s) s:(s + block_length - 1L)), use.names = FALSE)[seq_len(n)]
}

metrics <- c("AnnVol", "ES5")
n_days <- nrow(daily)
block_length <- 90L
B <- 1999L

ratio_results <- expand.grid(strategy = comparators, metric = metrics, stringsAsFactors = FALSE)
ratio_results$risk_ratio <- mapply(function(strategy, metric) {
  metric_value(daily[[strategy]], metric) / metric_value(daily$EW, metric)
}, ratio_results$strategy, ratio_results$metric)

set.seed(6404)
boot_ratio <- array(NA_real_, dim = c(B, length(comparators), length(metrics)),
                    dimnames = list(NULL, comparators, metrics))
for (b in seq_len(B)) {
  idx <- moving_block_indices(n_days, block_length)
  for (strategy in comparators) {
    for (metric in metrics) {
      boot_ratio[b, strategy, metric] <-
        metric_value(daily[[strategy]][idx], metric) /
        metric_value(daily$EW[idx], metric)
    }
  }
}

ratio_results$ci95_low <- NA_real_
ratio_results$ci95_high <- NA_real_
for (i in seq_len(nrow(ratio_results))) {
  x <- boot_ratio[, ratio_results$strategy[i], ratio_results$metric[i]]
  ratio_results$ci95_low[i] <- stats::quantile(x, 0.025, names = FALSE, type = 8)
  ratio_results$ci95_high[i] <- stats::quantile(x, 0.975, names = FALSE, type = 8)
}

primary_inference <- inference[inference$metric %in% metrics,
                               c("strategy", "metric", "estimate", "ci95_low", "ci95_high",
                                 "p_value", "p_holm_primary"), drop = FALSE]
names(primary_inference)[names(primary_inference) == "estimate"] <- "difference"
names(primary_inference)[names(primary_inference) == "ci95_low"] <- "difference_ci95_low"
names(primary_inference)[names(primary_inference) == "ci95_high"] <- "difference_ci95_high"
ratio_results <- merge(ratio_results, primary_inference,
                       by = c("strategy", "metric"), all.x = TRUE, sort = FALSE)
ratio_results <- ratio_results[match(
  paste(rep(comparators, each = length(metrics)), rep(metrics, times = length(comparators))),
  paste(ratio_results$strategy, ratio_results$metric)
), ]
ratio_results$n_days <- n_days
ratio_results$metric_label <- ifelse(
  ratio_results$metric == "AnnVol", "Annualised volatility", "5% Expected Shortfall"
)
ratio_results$strategy <- factor(ratio_results$strategy, levels = c("QTEC", "LTEC", "GMV"))
ratio_results$metric_label <- factor(
  ratio_results$metric_label,
  levels = c("Annualised volatility", "5% Expected Shortfall")
)

figure_source <- ratio_results[, c(
  "strategy", "metric", "metric_label", "risk_ratio", "ci95_low", "ci95_high",
  "difference", "difference_ci95_low", "difference_ci95_high",
  "p_value", "p_holm_primary", "n_days"
)]
write.csv(figure_source,
          file.path(bundle_dir, "figures", "Figure_6_4_source_data.csv"),
          row.names = FALSE)

performance$strategy <- factor(performance$strategy, levels = strategies)
performance <- performance[order(performance$strategy), ]
table_main <- data.frame(
  Strategy = as.character(performance$strategy),
  `Annualised return (%)` = round(performance$annualized_return_pct, 2),
  `Annualised volatility (%)` = round(performance$annualized_volatility_pct, 2),
  `5% ES (bp)` = round(performance$expected_shortfall_5_bps, 2),
  `Maximum drawdown (%)` = round(performance$maximum_drawdown_pct, 2),
  `Turnover per rebalance (%)` = round(performance$mean_turnover_per_rebalance_pct, 2),
  `Net annualised return, 10 bp (%)` = round(performance$net_annualized_return_10bp_pct, 2),
  check.names = FALSE
)
write.csv(table_main, file.path(bundle_dir, "tables", "Table_6_4_OOS_Performance.csv"), row.names = FALSE)

table_md <- c(
  "**Table 6.4 | Leakage-free out-of-sample portfolio performance.**",
  "",
  "| Strategy | Ann. return (%) | Ann. volatility (%) | 5% ES (bp) | Max. drawdown (%) | Turnover/rebalance (%) | Net ann. return, 10 bp (%) |",
  "|---|---:|---:|---:|---:|---:|---:|",
  vapply(seq_len(nrow(table_main)), function(i) {
    sprintf("| %s | %.2f | %.2f | %.2f | %.2f | %.2f | %.2f |",
            table_main$Strategy[i], table_main[i, 2], table_main[i, 3], table_main[i, 4],
            table_main[i, 5], table_main[i, 6], table_main[i, 7])
  }, character(1)),
  "",
  "Portfolio returns use reference-adjusted prices. The 10 bp column is a proportional one-way transaction-cost scenario rather than an observed execution-cost estimate."
)
writeLines(table_md, file.path(bundle_dir, "tables", "Table_6_4_Manuscript_Ready.md"))

palette <- c("Annualised volatility" = "#315B8A", "5% Expected Shortfall" = "#C56A36")
shape_values <- c("Annualised volatility" = 16, "5% Expected Shortfall" = 18)
pd <- position_dodge(width = 0.36)

plot_obj <- ggplot(ratio_results,
                   aes(x = risk_ratio, y = strategy, colour = metric_label, shape = metric_label)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.45, colour = "#707070") +
  geom_errorbar(aes(xmin = ci95_low, xmax = ci95_high),
                orientation = "y", width = 0.15, linewidth = 0.65, position = pd) +
  geom_point(size = 2.7, stroke = 0.4, position = pd) +
  annotate("text", x = 1, y = 3.62, label = "EW benchmark", hjust = 0.5,
           colour = "#555555", size = 2.35, family = "Helvetica") +
  scale_colour_manual(values = palette, name = NULL) +
  scale_shape_manual(values = shape_values, name = NULL) +
  scale_x_continuous(
    breaks = seq(0.25, 1.50, 0.25),
    limits = c(0.12, max(1.52, max(ratio_results$ci95_high) + 0.04)),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(x = "Risk ratio relative to equal weight", y = NULL) +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = 7.5, base_family = "Helvetica") +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(linewidth = 0.4, colour = "black"),
    axis.ticks.x = element_line(linewidth = 0.35, colour = "black"),
    axis.text = element_text(colour = "black", size = 7),
    axis.text.y = element_text(face = "bold", size = 7.4),
    axis.title.x = element_text(size = 7.5, margin = margin(t = 6)),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.text = element_text(size = 7),
    legend.key.width = grid::unit(7, "pt"),
    legend.margin = margin(b = 3),
    plot.margin = margin(t = 5, r = 7, b = 4, l = 4)
  )

figure_base <- file.path(bundle_dir, "figures", "Figure_6_4_Out_of_Sample_Portfolio_Risk")
width_mm <- 140
height_mm <- 82
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
  "Figure 6.4 | Out-of-sample portfolio risk relative to equal weight. ",
  "Points report the ratio of each strategy's annualised volatility or 5% Expected Shortfall to the corresponding equal-weight value; horizontal bars are 95% paired moving-block bootstrap intervals. ",
  "Values below one indicate lower realised risk than equal weight. All strategies are evaluated on the same 2,162 days from 30 June 2020 to 31 May 2026 using reference-adjusted returns. ",
  "The bootstrap uses 1,999 replications and 90-day blocks. Holm adjustment is applied across the six strategy-metric comparisons; the two GMV comparisons have adjusted p = 0.006, while none of the LTEC or QTEC comparisons is significant after adjustment. Source data are provided with the figure."
)
writeLines(caption, file.path(bundle_dir, "figures", "Figure_6_4_caption.txt"))

qa <- data.frame(
  check = c(
    "daily_observations", "first_date", "last_date", "strategy_count",
    "return_panel_complete", "same_dates_all_strategies", "bootstrap_repetitions",
    "block_length", "primary_comparison_count", "holm_values_complete",
    "figure_source_rows", "svg_exists", "pdf_exists", "tiff_exists", "png_exists"
  ),
  value = c(
    n_days, as.character(min(daily$date)), as.character(max(daily$date)), length(strategies),
    all(is.finite(as.matrix(daily[, strategies]))), TRUE, B, block_length,
    nrow(ratio_results), all(is.finite(ratio_results$p_holm_primary)),
    nrow(figure_source), file.exists(paste0(figure_base, ".svg")),
    file.exists(paste0(figure_base, ".pdf")), file.exists(paste0(figure_base, ".tiff")),
    file.exists(paste0(figure_base, ".png"))
  ),
  stringsAsFactors = FALSE
)
write.csv(qa, file.path(bundle_dir, "qa", "analysis_QA.csv"), row.names = FALSE)

manifest <- data.frame(
  file = normalizePath(input_files, mustWork = TRUE),
  md5 = unname(tools::md5sum(input_files)),
  stringsAsFactors = FALSE
)
write.csv(manifest, file.path(bundle_dir, "qa", "input_manifest_md5.csv"), row.names = FALSE)
capture.output(sessionInfo(), file = file.path(bundle_dir, "qa", "session_info.txt"))

cat("Section 6.4 outputs created.\n")
print(ratio_results[, c("strategy", "metric", "risk_ratio", "ci95_low", "ci95_high",
                        "p_value", "p_holm_primary")], row.names = FALSE)
