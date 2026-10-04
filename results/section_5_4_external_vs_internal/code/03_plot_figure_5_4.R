#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1L])))
} else normalizePath(getwd())
source(file.path(script_dir, "00_config.R"))

needed <- c("ggplot2", "svglite", "ragg")
missing <- needed[!vapply(needed, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing)) stop("Missing R packages: ", paste(missing, collapse = ", "))

source_path <- file.path(source_dir, "Figure_5_4_Source_Data.csv")
if (!file.exists(source_path)) stop("Run 02_analyse_block_models.R first.")
x <- read.csv(source_path, check.names = FALSE, stringsAsFactors = FALSE)
evidence_order <- c("conditional_same_day", "oos_same_day", "oos_all_lagged")
evidence_labels <- c(
  conditional_same_day = "Conditional GACV",
  oos_same_day = "OOS · same-day",
  oos_all_lagged = "OOS · all lagged"
)
model_order_plot <- c("macro_only", "coin_only", "joint")
model_labels_plot <- c(
  macro_only = "Macro only", coin_only = "Coin only", joint = "Joint"
)
if (!all(c("target", "evidence_id", "model", "skill_score") %in% names(x))) {
  stop("Figure source columns are incomplete.")
}
x$evidence_id <- factor(x$evidence_id, levels = evidence_order)
x$model <- factor(x$model, levels = model_order_plot)
x$target <- factor(x$target, levels = rev(c("System", coin_order)))
x$x_id <- (as.integer(x$evidence_id) - 1L) * length(model_order_plot) +
  as.integer(x$model)
x$display_value <- 100 * x$skill_score
x_breaks <- seq_len(length(evidence_order) * length(model_order_plot))
x_labels <- rep(unname(model_labels_plot), length(evidence_order))
max_abs <- max(abs(x$display_value), na.rm = TRUE)
max_abs <- ceiling(max_abs / 5) * 5
if (!is.finite(max_abs) || max_abs <= 0) max_abs <- 5

palette <- c(negative = "#B44A3E", neutral = "#F3F1EC", positive = "#256C89")

p <- ggplot2::ggplot(x, ggplot2::aes(x = x_id, y = target, fill = display_value)) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.45) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.1f", display_value)),
    size = 2.25, family = "Helvetica", colour = "#1F1F1F"
  ) +
  ggplot2::geom_vline(xintercept = c(3.5, 6.5), linewidth = 0.65,
                      colour = "#303030") +
  ggplot2::geom_hline(yintercept = 11.5, linewidth = 0.65,
                      colour = "#303030") +
  ggplot2::annotate(
    "text", x = 2, y = length(coin_order) + 1.82,
    label = "Conditional GACV", family = "Helvetica", fontface = "bold",
    size = 2.65
  ) +
  ggplot2::annotate(
    "text", x = 5, y = length(coin_order) + 1.82,
    label = "OOS · same-day", family = "Helvetica", fontface = "bold",
    size = 2.65
  ) +
  ggplot2::annotate(
    "text", x = 8, y = length(coin_order) + 1.82,
    label = "OOS · all lagged", family = "Helvetica", fontface = "bold",
    size = 2.65
  ) +
  ggplot2::scale_x_continuous(
    breaks = x_breaks, labels = x_labels,
    position = "bottom", expand = ggplot2::expansion(add = c(0.02, 0.02))
  ) +
  ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0.02, 0.9))) +
  ggplot2::scale_fill_gradient2(
    low = palette["negative"], mid = palette["neutral"], high = palette["positive"],
    midpoint = 0, limits = c(-max_abs, max_abs),
    name = "Loss reduction\n(% of benchmark)"
  ) +
  ggplot2::labs(
    title = "Conditional Fit and Tail Forecast Performance",
    x = NULL, y = NULL
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::theme_minimal(base_size = 7, base_family = "Helvetica") +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 6.2, angle = 0, hjust = 0.5),
    axis.text.y = ggplot2::element_text(size = 6.5, colour = "#202020"),
    plot.title = ggplot2::element_text(size = 8.2, face = "bold", hjust = 0),
    plot.margin = ggplot2::margin(12, 8, 5, 4),
    legend.position = "right",
    legend.title = ggplot2::element_text(size = 6.2),
    legend.text = ggplot2::element_text(size = 5.8),
    legend.key.height = grid::unit(18, "mm")
  )

base <- file.path(figure_dir, "Figure_5_4_Relative_Tail_Risk_Information")
width_mm <- 183
height_mm <- 118
width_in <- width_mm / 25.4
height_in <- height_mm / 25.4

svglite::svglite(paste0(base, ".svg"), width = width_in, height = height_in)
print(p)
grDevices::dev.off()
cairo_ready <- isTRUE(capabilities("cairo"))
if (identical(Sys.info()[["sysname"]], "Darwin") &&
    !file.exists("/opt/X11/lib/libXrender.1.dylib")) cairo_ready <- FALSE
if (cairo_ready) {
  grDevices::cairo_pdf(paste0(base, ".pdf"), width = width_in,
                       height = height_in, family = "Helvetica")
} else {
  # The base PDF device preserves editable Helvetica text when Cairo is not
  # available in the local R build.
  grDevices::pdf(paste0(base, ".pdf"), width = width_in, height = height_in,
                 family = "Helvetica", useDingbats = FALSE)
}
print(p)
grDevices::dev.off()
ragg::agg_tiff(paste0(base, ".tiff"), width = width_in, height = height_in,
               units = "in", res = 600, compression = "lzw")
print(p)
grDevices::dev.off()
ragg::agg_png(paste0(base, ".png"), width = width_in, height = height_in,
              units = "in", res = 300)
print(p)
grDevices::dev.off()

caption <- paste0(
  "Figure 5.4 | Conditional fit and tail forecast performance. Cells report ",
  "the percentage reduction in target-specific, MAD-standardized loss relative ",
  "to the rolling intercept-only benchmark. The first block reports the minimum ",
  "finite GACV selected within each 90-day conditional model. The second block ",
  "reports held-out check loss when stablecoin deviations are observed on the ",
  "evaluation date and macro-financial changes are lagged by one day. The third ",
  "block is a strict one-day-ahead test in which both information blocks are ",
  "dated no later than t-1. Positive values denote lower loss. The System ",
  "row uses the ratio of pooled date-level mean losses; the remaining rows show ",
  "the 11 target stablecoins."
)
writeLines(caption, file.path(figure_dir, "Figure_5_4_caption.txt"), useBytes = TRUE)

figure_qa <- data.frame(
  check = c("source_rows", "display_cells", "all_values_finite", "svg_nonempty",
            "pdf_nonempty", "tiff_nonempty", "png_nonempty"),
  value = c(nrow(x), nrow(x), sum(is.finite(x$display_value)),
            file.info(paste0(base, ".svg"))$size,
            file.info(paste0(base, ".pdf"))$size,
            file.info(paste0(base, ".tiff"))$size,
            file.info(paste0(base, ".png"))$size),
  expected = c(108, 108, 108, ">0", ">0", ">0", ">0"),
  status = c(
    if (nrow(x) == 108L) "PASS" else "FAIL",
    if (nrow(x) == 108L) "PASS" else "FAIL",
    if (all(is.finite(x$display_value))) "PASS" else "FAIL",
    rep(if (all(file.info(paste0(base, c(".svg", ".pdf", ".tiff", ".png")))$size > 0))
      "PASS" else "FAIL", 4L)
  ), stringsAsFactors = FALSE
)
write_csv(figure_qa, file.path(qa_dir, "figure_QA.csv"))
if (any(figure_qa$status == "FAIL")) stop("Figure QA failed.")
message("Figure 5.4 exported: ", figure_dir)
