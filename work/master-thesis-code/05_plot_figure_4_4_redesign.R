#!/usr/bin/env Rscript

# Redesigned Figure 4.4: a claim-led, asymmetric forest-plot composition.
# The noisy event-aligned trajectories and non-essential bubble plot are omitted
# from the main figure and remain available in the earlier exploratory export.

options(stringsAsFactors = FALSE, digits = 15, warn = 1, scipen = 999)

script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.")
  normalizePath(sub("^--file=", "", hit[1]), mustWork = TRUE)
}

code_dir <- dirname(script_path())
bundle_dir <- dirname(code_dir)
table_dir <- file.path(bundle_dir, "tables")
figure_dir <- file.path(bundle_dir, "figures")
source_dir <- file.path(bundle_dir, "source_data")
qa_dir <- file.path(bundle_dir, "qa")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

read_checked <- function(path, required) {
  if (!file.exists(path)) stop("Missing input: ", path)
  x <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  absent <- setdiff(required, names(x))
  if (length(absent)) stop("Missing columns in ", basename(path), ": ", paste(absent, collapse = ", "))
  x
}

main <- read_checked(
  file.path(table_dir, "Table_4_4b_FRM_Definition_Robustness_HAC90.csv"),
  c("series_id", "event_id", "effect_pct", "ci95_low_pct", "ci95_high_pct")
)
embedded <- read_checked(
  file.path(table_dir, "Table_4_4d_Embedded_USD_Target_Subindex_HAC90.csv"),
  c("event_id", "effect_pct", "ci95_low_pct", "ci95_high_pct")
)
leave_one <- read_checked(
  file.path(table_dir, "Table_4_5b_Cross_Definition_Breadth_Concentration_and_Leave_One_Out.csv"),
  c("definition_id", "event_id", "top_lambda_contributor",
    "top_lambda_contribution_share_0_30", "leave_top_hac90_effect_0_30_pct",
    "leave_top_hac90_ci95_low_pct", "leave_top_hac90_ci95_high_pct")
)

numeric_main <- c("effect_pct", "ci95_low_pct", "ci95_high_pct")
for (nm in numeric_main) main[[nm]] <- as.numeric(main[[nm]])
for (nm in numeric_main) embedded[[nm]] <- as.numeric(embedded[[nm]])
numeric_leave <- c("top_lambda_contribution_share_0_30", "leave_top_hac90_effect_0_30_pct",
                   "leave_top_hac90_ci95_low_pct", "leave_top_hac90_ci95_high_pct")
for (nm in numeric_leave) leave_one[[nm]] <- as.numeric(leave_one[[nm]])

event_order <- c("kucoin", "china_crash", "terra", "ftx", "svb")
event_labels <- c(
  kucoin = "KuCoin", china_crash = "China crash", terra = "Terra",
  ftx = "FTX", svb = "SVB/USDC"
)
series_order <- c("adjusted_screened", "usd_only_screened", "raw_usd_screened")
series_labels <- c(
  adjusted_screened = "Reference-adjusted 11-coin",
  usd_only_screened = "USD-only 8-coin",
  raw_usd_screened = "Raw USD-quoted 11-coin"
)

hist_data <- main[main$event_id %in% event_order & main$series_id %in% series_order, ]
hist_data$event_id <- factor(hist_data$event_id, levels = event_order)
hist_data$series_id <- factor(hist_data$series_id, levels = series_order)
hist_data <- hist_data[order(hist_data$event_id, hist_data$series_id), ]
if (nrow(hist_data) != 15L) stop("Historical panel does not contain 5 events x 3 definitions.")

hormuz_main <- main[main$event_id == "hormuz" & main$series_id %in% series_order, ]
hormuz_main <- hormuz_main[match(series_order, hormuz_main$series_id), ]
hormuz_embedded <- embedded[embedded$event_id == "hormuz", ]
if (nrow(hormuz_main) != 3L || nrow(hormuz_embedded) != 1L) stop("Incomplete Hormuz decomposition.")

panel_b <- data.frame(
  label = c("Adjusted 11", "USD targets (full)", "USD-only 8", "Raw-USD 11"),
  effect = c(hormuz_main$effect_pct[1], hormuz_embedded$effect_pct, hormuz_main$effect_pct[2], hormuz_main$effect_pct[3]),
  low = c(hormuz_main$ci95_low_pct[1], hormuz_embedded$ci95_low_pct, hormuz_main$ci95_low_pct[2], hormuz_main$ci95_low_pct[3]),
  high = c(hormuz_main$ci95_high_pct[1], hormuz_embedded$ci95_high_pct, hormuz_main$ci95_high_pct[2], hormuz_main$ci95_high_pct[3]),
  family = c("adjusted", "embedded", "usd", "raw"),
  stringsAsFactors = FALSE
)

scaled <- main[
  main$event_id == "hormuz" & main$series_id %in% c("adjusted_robust_scaled", "usd_only_robust_scaled"),
]
scaled <- scaled[match(c("adjusted_robust_scaled", "usd_only_robust_scaled"), scaled$series_id), ]
loo <- leave_one[
  leave_one$event_id == "hormuz" & leave_one$definition_id %in% c("reference_adjusted_11", "usd_only_8"),
]
loo <- loo[match(c("reference_adjusted_11", "usd_only_8"), loo$definition_id), ]
if (nrow(scaled) != 2L || nrow(loo) != 2L) stop("Incomplete Hormuz sensitivity inputs.")

panel_c <- data.frame(
  label = c("Adjusted, scaled", "USD-only, scaled", "Adjusted, leave GUSD", "USD-only, leave GUSD"),
  effect = c(scaled$effect_pct, loo$leave_top_hac90_effect_0_30_pct),
  low = c(scaled$ci95_low_pct, loo$leave_top_hac90_ci95_low_pct),
  high = c(scaled$ci95_high_pct, loo$leave_top_hac90_ci95_high_pct),
  family = c("adjusted", "usd", "adjusted", "usd"),
  stringsAsFactors = FALSE
)

source_out <- rbind(
  data.frame(panel = "a", item = paste(as.character(hist_data$event_id), as.character(hist_data$series_id), sep = "__"),
             effect_pct = hist_data$effect_pct, ci95_low_pct = hist_data$ci95_low_pct,
             ci95_high_pct = hist_data$ci95_high_pct, stringsAsFactors = FALSE),
  data.frame(panel = "b", item = panel_b$label, effect_pct = panel_b$effect,
             ci95_low_pct = panel_b$low, ci95_high_pct = panel_b$high, stringsAsFactors = FALSE),
  data.frame(panel = "c", item = panel_c$label, effect_pct = panel_c$effect,
             ci95_low_pct = panel_c$low, ci95_high_pct = panel_c$high, stringsAsFactors = FALSE)
)
write.csv(source_out, file.path(source_dir, "Source_Data_Figure_4_4_Redesigned.csv"),
          row.names = FALSE, quote = TRUE, na = "", fileEncoding = "UTF-8")

COL <- c(
  adjusted = "#1F4E79",
  embedded = "#6BAED6",
  usd = "#2A9D8F",
  raw = "#5E5E5E",
  ink = "#202020",
  mid = "#777777",
  pale = "#F4F6F8",
  pale_blue = "#EAF1F7",
  grid = "#D9D9D9"
)

draw_ci <- function(effect, low, high, y, colour, pch = 21, bg = colour, cex = 0.9, open = FALSE) {
  segments(low, y, high, y, col = colour, lwd = 1.45, lend = "round")
  segments(low, y - 0.055, low, y + 0.055, col = colour, lwd = 1.0)
  segments(high, y - 0.055, high, y + 0.055, col = colour, lwd = 1.0)
  points(effect, y, pch = pch, col = colour, bg = if (open) "white" else bg, cex = cex, lwd = 1.25)
}

panel_header <- function(letter, title, title_adj = 0.07) {
  mtext(letter, side = 3, line = 0.35, adj = 0, font = 2, cex = 0.92)
  mtext(title, side = 3, line = 0.35, adj = title_adj, font = 2, cex = 0.74)
}

draw_figure <- function() {
  old <- par(no.readonly = TRUE)
  on.exit(par(old), add = TRUE)
  layout(matrix(c(1, 2, 1, 3), nrow = 2, byrow = TRUE), widths = c(1.48, 1), heights = c(1, 1))
  par(family = "sans", ps = base_size, fg = unname(COL["ink"]), col.axis = unname(COL["ink"]),
      col.lab = unname(COL["ink"]),
      xaxs = "i", yaxs = "i", lend = "round")

  # a | Five historical events. The Iran-Hormuz case is isolated in b-c so it
  # does not compress the scale of the other events.
  par(mar = c(3.1, 4.15, 2.15, 0.7), mgp = c(1.75, 0.45, 0), tcl = -0.22,
      cex.axis = 0.72, cex.lab = 0.76)
  xlim_a <- c(-50, 90)
  ylim_a <- c(0.45, 6.15)
  plot(NA, xlim = xlim_a, ylim = ylim_a, axes = FALSE,
       xlab = "Change in FRM, days 0-30 (%)", ylab = "")
  y_event <- setNames(c(5, 4, 3, 2, 1), event_order)
  for (i in seq_along(event_order)) {
    if (i %% 2L == 1L) rect(xlim_a[1], y_event[i] - 0.43, xlim_a[2], y_event[i] + 0.43,
                            col = COL["pale"], border = NA)
  }
  for (gx in c(-40, 40, 80)) abline(v = gx, col = COL["grid"], lwd = 0.45)
  abline(v = 0, col = COL["mid"], lty = 2, lwd = 0.85)
  axis(1, at = c(-40, 0, 40, 80))
  axis(2, at = y_event, labels = event_labels[event_order], tick = FALSE, las = 1, cex.axis = 0.72)
  box(bty = "l", lwd = 0.7)
  offsets <- c(adjusted_screened = 0.20, usd_only_screened = 0, raw_usd_screened = -0.20)
  pchs <- c(adjusted_screened = 21, usd_only_screened = 22, raw_usd_screened = 24)
  cols <- c(
    adjusted_screened = unname(COL["adjusted"]),
    usd_only_screened = unname(COL["usd"]),
    raw_usd_screened = unname(COL["raw"])
  )
  for (sid in series_order) {
    zz <- hist_data[as.character(hist_data$series_id) == sid, ]
    zz <- zz[match(event_order, as.character(zz$event_id)), ]
    yy <- y_event[event_order] + offsets[sid]
    for (j in seq_along(yy)) {
      draw_ci(zz$effect_pct[j], zz$ci95_low_pct[j], zz$ci95_high_pct[j], yy[j],
              cols[sid], pch = pchs[sid],
              bg = if (sid == "raw_usd_screened") "#D0D0D0" else cols[sid],
              cex = 0.82, open = FALSE)
    }
  }
  primary <- hist_data[as.character(hist_data$series_id) == "adjusted_screened", ]
  primary <- primary[match(event_order, as.character(primary$event_id)), ]
  for (j in seq_along(event_order)) {
    text(primary$effect_pct[j], y_event[j] + offsets["adjusted_screened"] + 0.13,
         sprintf("%+.1f", primary$effect_pct[j]), pos = 3, offset = 0.15,
         cex = 0.54, col = COL["adjusted"], font = 2)
  }
  legend("top", inset = c(0, 0.005), horiz = TRUE, bty = "n", xpd = FALSE,
         legend = c("Adjusted 11", "USD-only 8", "Raw-USD 11"), pch = c(21, 22, 24),
         pt.bg = c(COL["adjusted"], COL["usd"], "#D0D0D0"),
         col = c(COL["adjusted"], COL["usd"], COL["raw"]),
         pt.cex = 0.9, cex = 0.58, x.intersp = 0.7)
  panel_header("a", "Historical events", title_adj = 0.055)

  # b | The three-step Hormuz definition path plus the raw-USD comparator.
  par(mar = c(2.9, 4.4, 2.25, 0.6), mgp = c(1.65, 0.42, 0), tcl = -0.20,
      cex.axis = 0.66, cex.lab = 0.70)
  xlim_b <- c(-55, 190)
  yb <- c(4.0, 3.0, 2.0, 0.85)
  plot(NA, xlim = xlim_b, ylim = c(0.35, 4.75), axes = FALSE,
       xlab = "Hormuz FRM change (%)", ylab = "")
  rect(xlim_b[1], 1.55, xlim_b[2], 4.45, col = COL["pale_blue"], border = NA)
  for (gx in c(-50, 50, 100, 150)) abline(v = gx, col = COL["grid"], lwd = 0.42)
  abline(v = 0, col = COL["mid"], lty = 2, lwd = 0.85)
  axis(1, at = c(-50, 0, 50, 100, 150))
  axis(2, at = yb, labels = panel_b$label, tick = FALSE, las = 1, cex.axis = 0.62)
  box(bty = "l", lwd = 0.7)
  bcols <- unname(COL[panel_b$family])
  bpch <- c(21, 23, 22, 24)
  bopen <- c(FALSE, FALSE, FALSE, TRUE)
  for (i in seq_len(nrow(panel_b))) {
    draw_ci(panel_b$effect[i], panel_b$low[i], panel_b$high[i], yb[i], bcols[i],
            pch = bpch[i], bg = bcols[i], cex = 0.87, open = bopen[i])
    text(panel_b$effect[i], yb[i] + 0.13, sprintf("%+.1f%%", panel_b$effect[i]),
         pos = 3, offset = 0.15, cex = 0.54, col = bcols[i], font = 2)
  }
  text(186, 4.60, "Aggregation contrast +112.3%\nNetwork re-estimation +42.5%",
       adj = c(1, 1), cex = 0.52, col = COL["mid"])
  panel_header("b", "Hormuz definition path", title_adj = 0.18)

  # c | Scale and single-coin sensitivity. Only the two economically central
  # definitions are retained; the raw-USD leave-one-out result is supplementary.
  par(mar = c(2.9, 4.4, 2.25, 0.6), mgp = c(1.65, 0.42, 0), tcl = -0.20,
      cex.axis = 0.66, cex.lab = 0.70)
  xlim_c <- c(-60, 45)
  yc <- c(4.0, 3.1, 1.75, 0.85)
  plot(NA, xlim = xlim_c, ylim = c(0.35, 4.75), axes = FALSE,
       xlab = "Hormuz sensitivity estimate (%)", ylab = "")
  rect(xlim_c[1], 2.55, xlim_c[2], 4.45, col = COL["pale"], border = NA)
  for (gx in c(-50, -25, 25)) abline(v = gx, col = COL["grid"], lwd = 0.42)
  abline(v = 0, col = COL["mid"], lty = 2, lwd = 0.85)
  axis(1, at = c(-50, -25, 0, 25))
  axis(2, at = yc, labels = panel_c$label, tick = FALSE, las = 1, cex.axis = 0.60)
  box(bty = "l", lwd = 0.7)
  ccols <- unname(COL[panel_c$family])
  cpch <- c(23, 25, 21, 22)
  for (i in seq_len(nrow(panel_c))) {
    draw_ci(panel_c$effect[i], panel_c$low[i], panel_c$high[i], yc[i], ccols[i],
            pch = cpch[i], bg = "white", cex = 0.84, open = TRUE)
    text(panel_c$effect[i], yc[i] + 0.13, sprintf("%+.1f", panel_c$effect[i]),
         pos = 3, offset = 0.15, cex = 0.53, col = ccols[i], font = 2)
  }
  text(43, 4.62, "GUSD lambda share: 28% adjusted; 74% USD-only",
       adj = c(1, 1), cex = 0.50, col = COL["mid"])
  panel_header("c", "Hormuz robustness", title_adj = 0.18)
}

base_size <- 8
width_mm = 183
height_mm = 118
width_in <- width_mm / 25.4
height_in <- height_mm / 25.4
prefix <- file.path(figure_dir, "Figure_4_4_FRM_Event_Responses_Redesigned")

if (!requireNamespace("svglite", quietly = TRUE)) stop("Package 'svglite' is required.")
if (!requireNamespace("ragg", quietly = TRUE)) stop("Package 'ragg' is required.")

svglite::svglite(paste0(prefix, ".svg"), width = width_in, height = height_in)
draw_figure()
dev.off()

if (identical(Sys.getenv("FRM_USE_CAIRO_PDF", "0"), "1")) {
  # Preferred when Cairo/X11 libraries are installed.
  grDevices::cairo_pdf(paste0(prefix, ".pdf"), width = width_in,
                       height = height_in, family = "Helvetica")
} else {
  # Editable-vector fallback for systems without Cairo/X11.
  grDevices::pdf(paste0(prefix, ".pdf"), width = width_in, height = height_in,
                 family = "Helvetica", useDingbats = FALSE)
}
draw_figure()
dev.off()

ragg::agg_png(paste0(prefix, ".png"), width = width_in, height = height_in,
              units = "in", res = 600, background = "white")
draw_figure()
dev.off()

ragg::agg_tiff(paste0(prefix, ".tiff"), width = width_in, height = height_in,
               units = "in", res = 600, compression = "lzw", background = "white")
draw_figure()
dev.off()

caption <- paste(
  "Fig. 4.4 | Event-specific FRM responses and definition sensitivity.",
  "a, Estimated changes in the FRM over days 0–30 relative to the pre-event baseline for five historical events.",
  "b, Decomposition of the Iran–Hormuz response across the full reference-adjusted network, the embedded USD-target subindex, the independently re-estimated USD-only network and the raw USD-quoted comparator.",
  "c, Window-robust scaling and leave-GUSD sensitivity checks for the two economically central network definitions.",
  "Points denote estimated percentage changes and horizontal lines denote 95% confidence intervals based on Newey–West covariance estimates with lag 90.",
  "The calendar day is the observational unit. Source data are provided as a Source Data file."
)
writeLines(caption, file.path(figure_dir, "Figure_4_4_FRM_Event_Responses_Redesigned_caption.txt"), useBytes = TRUE)

qa_notes <- c(
  "Core conclusion: SVB and FTX show the most stable positive event responses, whereas the Hormuz response reverses with network definition.",
  "Archetype: asymmetric quantitative figure with one hero forest panel and two subordinate Hormuz diagnostic panels.",
  "Backend: R only (base graphics, svglite, cairo_pdf and ragg).",
  "Final size: 183 mm x 118 mm; SVG/PDF editable vector plus 600-dpi PNG/TIFF.",
  "Panel a includes all 15 estimates for five historical events across the three main FRM definitions.",
  "Hormuz is excluded from panel a solely to prevent its 177% upper confidence bound from compressing the historical comparisons; it is shown in panel b.",
  "The event-aligned daily trajectories are omitted because six overlaid 90-day rolling series did not provide an identifiable additional claim.",
  "The bubble plot is omitted because lambda concentration, breadth and peg breadth could not be read precisely on one compact axis.",
  "Panel c retains adjusted and USD-only scale and leave-GUSD checks; the raw-USD leave-GUSD result remains in the source table as a secondary comparator.",
  "Confidence intervals are pointwise 95% Newey-West HAC intervals; Holm-adjusted p values remain in the accompanying source tables.",
  "No observations were sampled or altered for visual presentation."
)
writeLines(qa_notes, file.path(qa_dir, "Figure_4_4_Redesign_QA_Notes.txt"), useBytes = TRUE)

message("Redesigned Figure 4.4 exported to: ", figure_dir)
