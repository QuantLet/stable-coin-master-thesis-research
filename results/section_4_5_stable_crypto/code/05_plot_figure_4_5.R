#!/usr/bin/env Rscript

# Figure 4.5 is a single heatmap of the new predictive evidence.
# Both FRMs remain constructed from 90-calendar-day rolling windows; the
# rolling-63 label applies only to the subsequent forecast regression.

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else normalizePath(getwd())
bundle_guess <- dirname(script_dir)
local_library <- file.path(bundle_guess, "r_library")
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))

required_packages <- c("ggplot2", "svglite", "ragg", "scales")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install the following R packages before plotting: ", paste(missing_packages, collapse = ", "))
}
suppressPackageStartupMessages(library(ggplot2))
source(file.path(script_dir, "00_config.R"))

oos <- read.csv(file.path(tables_dir, "Table_4_5d_OOS_R2.csv"), stringsAsFactors = FALSE)
plot_data <- oos[
  oos$outcome %in% c("stable_downside", "crypto_volatility") &
    oos$scheme %in% c("expanding", "rolling_63") &
    oos$model %in% c("stable", "crypto", "joint"),
]
if (nrow(plot_data) != 48L || any(!is.finite(plot_data$oos_r_squared))) {
  stop("Figure 4.5 requires all 48 pre-specified OOS R-squared values.")
}

model_order <- c("stable", "crypto", "joint")
horizon_order <- c(10L, 35L, 90L, 150L)
plot_data$x <- (match(plot_data$model, model_order) - 1L) * 4L +
  match(plot_data$horizon_days, horizon_order)
plot_data$y <- with(plot_data, ifelse(
  scheme == "expanding" & outcome == "stable_downside", 4,
  ifelse(scheme == "expanding" & outcome == "crypto_volatility", 3,
         ifelse(scheme == "rolling_63" & outcome == "stable_downside", 2, 1))
))
plot_data$value_label <- sprintf("%.2f", plot_data$oos_r_squared)
plot_data$text_colour <- ifelse(abs(plot_data$oos_r_squared) >= 0.24, "white", "#202020")

row_labels <- c(
  "Crypto volatility (rolling 63)" = 1,
  "Stablecoin downside (rolling 63)" = 2,
  "Crypto volatility (expanding)" = 3,
  "Stablecoin downside (expanding)" = 4
)
horizon_labels <- rep(c("10 d", "5 wk", "3 mo", "5 mo"), 3)

theme_set(
  theme_minimal(base_size = 7.5, base_family = "Helvetica") +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_text(size = 6.8, colour = "#252525", margin = margin(t = 3)),
      axis.text.y = element_text(size = 7.2, colour = "#252525", hjust = 1),
      axis.ticks = element_blank(),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 6.5),
      legend.position = "bottom",
      legend.key.width = grid::unit(32, "mm"),
      legend.key.height = grid::unit(2.8, "mm"),
      plot.title = element_text(size = 8.6, face = "bold", margin = margin(b = 2)),
      plot.subtitle = element_text(size = 7, colour = "#555555", margin = margin(b = 12)),
      plot.margin = margin(8, 7, 5, 7)
    )
)

figure <- ggplot(plot_data, aes(x, y, fill = oos_r_squared)) +
  geom_tile(width = 0.94, height = 0.78, colour = "white", linewidth = 0.55) +
  geom_text(aes(label = value_label, colour = text_colour), size = 2.35,
            family = "Helvetica", fontface = "bold") +
  scale_colour_identity() +
  scale_fill_gradient2(
    low = "#D98968", mid = "#F7F7F5", high = "#3778B8", midpoint = 0,
    limits = c(-0.46, 0.46), breaks = c(-0.4, -0.2, 0, 0.2, 0.4),
    name = expression(R[OOS]^2)
  ) +
  scale_x_continuous(
    limits = c(0.45, 12.55), breaks = 1:12, labels = horizon_labels,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.45, 5.05), breaks = unname(row_labels), labels = names(row_labels),
    expand = c(0, 0)
  ) +
  annotate("segment", x = 4.5, xend = 4.5, y = 0.55, yend = 4.4,
           linewidth = 0.5, colour = "#737373") +
  annotate("segment", x = 8.5, xend = 8.5, y = 0.55, yend = 4.4,
           linewidth = 0.5, colour = "#737373") +
  annotate("segment", x = 0.55, xend = 12.45, y = 2.5, yend = 2.5,
           linewidth = 0.45, colour = "#737373") +
  annotate("text", x = 2.5, y = 4.78, label = "Stable FRM predictor",
           size = 2.55, fontface = "bold", family = "Helvetica", colour = "#303030") +
  annotate("text", x = 6.5, y = 4.78, label = "Crypto FRM predictor",
           size = 2.55, fontface = "bold", family = "Helvetica", colour = "#303030") +
  annotate("text", x = 10.5, y = 4.78, label = "Joint predictors",
           size = 2.55, fontface = "bold", family = "Helvetica", colour = "#303030") +
  labs(
    title = "Out-of-Sample Forecast Performance",
    subtitle = "Positive values indicate lower forecast error than the historical-mean benchmark"
  ) +
  guides(fill = guide_colourbar(title.position = "left", title.hjust = 0.5,
                                barwidth = grid::unit(42, "mm")))

write_csv(
  plot_data[, c(
    "outcome", "scheme", "model", "horizon_days", "forecasts",
    "first_origin", "last_origin", "oos_r_squared", "rmse", "mae"
  )],
  file.path(figures_dir, "Figure_4_5_source_data.csv")
)

base_name <- file.path(figures_dir, "Figure_4_5_Stablecoin_Crypto_Risk")
width_mm <- 183
height_mm <- 90
width_in <- width_mm / 25.4
height_in <- height_mm / 25.4

svglite::svglite(paste0(base_name, ".svg"), width = width_in, height = height_in)
print(figure)
dev.off()
if (isTRUE(capabilities("cairo")) && file.exists("/opt/X11/lib/libXrender.1.dylib")) {
  grDevices::cairo_pdf(
    paste0(base_name, ".pdf"), width = width_in, height = height_in,
    family = "Helvetica"
  )
} else {
  grDevices::pdf(
    paste0(base_name, ".pdf"), width = width_in, height = height_in,
    family = "Helvetica", useDingbats = FALSE
  )
}
print(figure)
dev.off()
ragg::agg_tiff(
  paste0(base_name, ".tiff"), width = width_in, height = height_in,
  units = "in", res = 600, background = "white"
)
print(figure)
dev.off()
ragg::agg_png(
  paste0(base_name, ".png"), width = width_in, height = height_in,
  units = "in", res = 300, background = "white"
)
print(figure)
dev.off()

caption <- paste0(
  "Fig. 4.5 | Out-of-sample predictive content of Stable and Crypto FRM. ",
  "Cells report Campbell–Thompson out-of-sample R-squared relative to the historical-mean forecast for future reference-adjusted stablecoin downside pressure and future broad-crypto volatility. ",
  "Columns distinguish the Stable-only, Crypto-only and joint predictor models at horizons of 10, 35, 90 and 150 calendar days. ",
  "The upper rows use expanding estimation and the lower rows use the most recent 63 completely realised forecast observations. ",
  "Both FRM predictors are themselves constructed using 90-calendar-day rolling windows; the 63-observation window applies only to the forecast regression. ",
  "Positive values indicate improvement over the benchmark. Forecast comparisons, Holm-adjusted p values and definition robustness are reported in the accompanying tables."
)
writeLines(caption, file.path(figures_dir, "Figure_4_5_caption.txt"), useBytes = TRUE)

writeLines(c(
  "backend=R only",
  "layout=single heatmap; no panel assembly",
  "final_size_mm=183x90",
  "cells=48; all two outcomes x four horizons x three predictor models x two schemes",
  "metric=Campbell-Thompson out-of-sample R-squared relative to historical mean",
  "frm_construction=90-calendar-day rolling window for both Stable and Crypto FRM",
  "rolling_63=forecast-regression estimation window only",
  "values=exact values printed in every cell",
  "vector_exports=SVG and PDF with editable text",
  "raster_exports=TIFF 600 dpi; PNG 300 dpi preview"
), file.path(qa_dir, "Figure_4_5_export_and_integrity_QA.txt"), useBytes = TRUE)

message("Single-heatmap Figure 4.5 exported in R.")
