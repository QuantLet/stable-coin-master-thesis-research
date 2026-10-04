#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(file_arg) && sub("^--file=", "", file_arg[1]) != "-") {
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else {
  normalizePath(getwd())
}
source(file.path(script_dir, "00_config.R"))

strict_file <- file.path(results_dir, "FRM_Stable_index_20260531.csv")
sensitivity_file <- file.path(
  results_dir,
  "FRM_Stable_index_20260531_numerical_sensitivity.csv"
)
if (!file.exists(strict_file)) stop("Run 02_estimate_frm.R first.")

read_index <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE)
  x$date <- as.Date(x$date)
  if (anyNA(x$date) || any(!is.finite(x$frm))) stop("Invalid FRM series: ", path)
  x
}

# Figure contract: one quantitative time-series panel; it documents temporal
# variation in the estimated index. The strict and numerical-sensitivity
# results are exported separately so that the visual cannot obscure the
# distinction between the supplied formula and the diagnostic path rule.
width_mm = 183
height_mm = 106
width_in <- width_mm / 25.4
height_in <- height_mm / 25.4
font_family <- "Helvetica"
fontsize = 8

draw_frm <- function(x, y_label) {
  old <- par(no.readonly = TRUE)
  on.exit(par(old), add = TRUE)
  par(
    mar = c(4.5, 4.9, 0.6, 0.6), las = 1, family = font_family,
    mgp = c(2.7, 0.75, 0), tcl = -0.25
  )
  plot(
    x$date, x$frm, type = "l", col = "#1769E0", lwd = 1.25,
    xlab = "Date", ylab = y_label, xaxt = "n", bty = "l",
    cex.axis = 0.72, cex.lab = 0.78, xaxs = "i", yaxs = "i",
    ylim = c(0, max(x$frm) * 1.035)
  )
  ticks <- seq(as.Date("2020-07-01"), as.Date("2026-07-01"), by = "1 year")
  axis.Date(1, at = ticks, format = "%Y", cex.axis = 0.72)
  abline(h = 0, col = "#777777", lwd = 0.45)
}

xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

# R-only editable-SVG fallback for systems without svglite/Cairo.
write_svg_fallback <- function(x, path, y_label) {
  w <- 720
  h <- 417
  left <- 76
  right <- 20
  top <- 18
  bottom <- 58
  plot_w <- w - left - right
  plot_h <- h - top - bottom
  xd <- as.numeric(x$date)
  ymax <- max(x$frm) * 1.035
  px <- left + (xd - min(xd)) / diff(range(xd)) * plot_w
  py <- top + plot_h - x$frm / ymax * plot_h
  points <- paste(sprintf("%.2f,%.2f", px, py), collapse = " ")
  xt <- seq(as.Date("2020-07-01"), as.Date("2026-07-01"), by = "1 year")
  xt <- xt[xt >= min(x$date) & xt <= max(x$date)]
  xtx <- left + (as.numeric(xt) - min(xd)) / diff(range(xd)) * plot_w
  yt <- pretty(c(0, ymax), n = 5)
  yt <- yt[yt >= 0 & yt <= ymax]
  yty <- top + plot_h - yt / ymax * plot_h
  lines <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%gmm" height="%gmm" viewBox="0 0 %d %d">', width_mm, height_mm, w, h),
    '<rect width="100%" height="100%" fill="white"/>',
    sprintf('<g font-family="Arial, Helvetica, sans-serif" font-size="10" fill="#111111">'),
    sprintf('<line x1="%d" y1="%d" x2="%d" y2="%d" stroke="#111111" stroke-width="1"/>', left, top + plot_h, left + plot_w, top + plot_h),
    sprintf('<line x1="%d" y1="%d" x2="%d" y2="%d" stroke="#111111" stroke-width="1"/>', left, top, left, top + plot_h),
    sprintf('<polyline points="%s" fill="none" stroke="#1769E0" stroke-width="1.5"/>', points)
  )
  for (i in seq_along(xt)) {
    lines <- c(
      lines,
      sprintf('<line x1="%.2f" y1="%d" x2="%.2f" y2="%d" stroke="#111111"/>', xtx[i], top + plot_h, xtx[i], top + plot_h + 4),
      sprintf('<text x="%.2f" y="%d" text-anchor="middle">%s</text>', xtx[i], top + plot_h + 18, format(xt[i], "%Y"))
    )
  }
  for (i in seq_along(yt)) {
    lines <- c(
      lines,
      sprintf('<line x1="%d" y1="%.2f" x2="%d" y2="%.2f" stroke="#111111"/>', left - 4, yty[i], left, yty[i]),
      sprintf('<text x="%d" y="%.2f" text-anchor="end" dominant-baseline="middle">%s</text>', left - 8, yty[i], format(yt[i], digits = 3))
    )
  }
  lines <- c(
    lines,
    sprintf('<text x="%.2f" y="%d" text-anchor="middle" font-size="11">Date</text>', left + plot_w / 2, h - 12),
    sprintf('<text x="16" y="%.2f" text-anchor="middle" font-size="11" transform="rotate(-90 16 %.2f)">%s</text>', top + plot_h / 2, top + plot_h / 2, xml_escape(y_label)),
    '</g>',
    '</svg>'
  )
  writeLines(lines, path, useBytes = TRUE)
}

export_one <- function(x, stem, y_label) {
  png(
    file.path(figures_dir, paste0(stem, ".png")),
    width = width_in, height = height_in, units = "in", res = 300,
    bg = "white", family = font_family, pointsize = fontsize
  )
  draw_frm(x, y_label)
  dev.off()

  pdf_path <- file.path(figures_dir, paste0(stem, ".pdf"))
  # The current macOS R build reports Cairo capability but lacks its X11
  # libraries, so the reproducible default is the editable base-R PDF device.
  use_cairo_pdf <- FALSE
  if (use_cairo_pdf && capabilities("cairo")) {
    grDevices::cairo_pdf(pdf_path, width = width_in, height = height_in,
                        family = font_family)
  } else {
    grDevices::pdf(pdf_path, width = width_in, height = height_in,
                   family = font_family, useDingbats = FALSE)
  }
  draw_frm(x, y_label)
  dev.off()

  tiff(
    file.path(figures_dir, paste0(stem, ".tiff")),
    width = width_in, height = height_in, units = "in", res = 600,
    bg = "white", family = font_family, pointsize = fontsize
  )
  draw_frm(x, y_label)
  dev.off()

  svg_path <- file.path(figures_dir, paste0(stem, ".svg"))
  if (requireNamespace("svglite", quietly = TRUE)) {
    svglite::svglite(svg_path, width = width_in, height = height_in,
                     bg = "white")
    draw_frm(x, y_label)
    dev.off()
  } else {
    write_svg_fallback(x, svg_path, y_label)
  }
}

strict <- read_index(strict_file)
export_one(
  strict,
  "Figure_FRM_Index_Strict_20260531",
  "FRM-based depegging pressure index"
)

if (file.exists(sensitivity_file)) {
  sensitivity <- read_index(sensitivity_file)
  export_one(
    sensitivity,
    "Figure_FRM_Index_NumericalSensitivity_20260531",
    "FRM index (numerical sensitivity)"
  )
}

message("FRM figures written to: ", figures_dir)
