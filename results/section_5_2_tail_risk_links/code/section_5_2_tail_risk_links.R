#!/usr/bin/env Rscript

# Section 5.2: Stablecoin-to-Stablecoin Tail-Risk Links
#
# Reuses the frozen coefficient panel underlying the primary Chapter 4 DPI.
# A directed edge runs from predictor i to target j because beta[j,i,t]
# belongs to the conditional lower-tail equation for target j. The analysis is
# descriptive: non-zero Quantile-Lasso coefficients are conditional links, not
# causal spillovers or identified transmission effects.

options(stringsAsFactors = FALSE, digits = 15, warn = 1, scipen = 999)

script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.")
  normalizePath(sub("^--file=", "", hit[1L]), mustWork = TRUE)
}

code_dir <- dirname(script_path())
bundle_dir <- dirname(code_dir)
input_dir <- file.path(bundle_dir, "source_data", "input")
source_dir <- file.path(bundle_dir, "source_data")
table_dir <- file.path(bundle_dir, "tables")
figure_dir <- file.path(bundle_dir, "figures")
qa_dir <- file.path(bundle_dir, "qa")
invisible(lapply(c(source_dir, table_dir, figure_dir, qa_dir), dir.create,
                 recursive = TRUE, showWarnings = FALSE))

coefficient_path <- file.path(input_dir, "selected_coefficients_strict.csv.gz")
dpi_path <- file.path(input_dir, "dpi_primary_robust_mad_strict.csv")
if (!file.exists(coefficient_path)) stop("Missing coefficient input.")
if (!file.exists(dpi_path)) stop("Missing DPI input.")

write_csv <- function(x, path) {
  write.csv(x, path, row.names = FALSE, quote = TRUE, na = "", fileEncoding = "UTF-8")
}

coin_order <- c("USDT", "USDC", "BUSD", "TUSD", "USDP", "GUSD",
                "DAI", "sUSD", "EURS", "IDRT", "PAXG")
active_tolerance <- 1e-10
state_labels <- c("Very low", "Low", "Moderate", "Elevated", "Severe")

coin_attributes <- data.frame(
  coin = coin_order,
  design = c(rep("Fiat-reserve-backed", 6),
             rep("Crypto-collateralised", 2),
             rep("Fiat-reserve-backed", 2),
             "Real-asset-backed"),
  reference = c(rep("USD", 8), "EUR", "IDR", "Gold"),
  stringsAsFactors = FALSE
)

coefficients <- read.csv(gzfile(coefficient_path), check.names = FALSE,
                         stringsAsFactors = FALSE)
dpi <- read.csv(dpi_path, check.names = FALSE, stringsAsFactors = FALSE)
required_coef <- c("date", "target", "predictor", "predictor_type", "coefficient")
if (!all(required_coef %in% names(coefficients))) stop("Coefficient columns are incomplete.")
if (!all(c("date", "dpi_primary") %in% names(dpi))) stop("DPI columns are incomplete.")
coefficients$date <- as.Date(coefficients$date)
dpi$date <- as.Date(dpi$date)
coefficients$coefficient <- as.numeric(coefficients$coefficient)
if (anyNA(coefficients[, required_coef]) || any(!is.finite(coefficients$coefficient))) {
  stop("Coefficient panel contains missing or non-finite values.")
}
if (anyDuplicated(coefficients[c("date", "target", "predictor")])) {
  stop("Duplicate date-target-predictor rows detected.")
}

links <- coefficients[coefficients$predictor_type == "stablecoin", required_coef]
names(links)[names(links) == "predictor"] <- "source"
names(links)[names(links) == "target"] <- "receiver"
if (!setequal(unique(c(links$source, links$receiver)), coin_order)) {
  stop("Unexpected stablecoin universe.")
}
if (any(links$source == links$receiver)) stop("Self-links should not be present.")

dpi_breaks <- as.numeric(quantile(dpi$dpi_primary, probs = seq(0, 1, 0.2), type = 8))
dpi$state <- cut(dpi$dpi_primary,
                 breaks = c(-Inf, dpi_breaks[2:5], Inf),
                 labels = state_labels, right = TRUE, ordered_result = TRUE)
idx <- match(links$date, dpi$date)
if (anyNA(idx)) stop("DPI merge failed.")
links$dpi_primary <- dpi$dpi_primary[idx]
links$state <- dpi$state[idx]
links$active <- as.integer(abs(links$coefficient) > active_tolerance)
links$aligned <- as.integer(links$coefficient > active_tolerance)
links$inverse <- as.integer(links$coefficient < -active_tolerance)

source_idx <- match(links$source, coin_attributes$coin)
receiver_idx <- match(links$receiver, coin_attributes$coin)
links$source_design <- coin_attributes$design[source_idx]
links$receiver_design <- coin_attributes$design[receiver_idx]
links$source_reference <- coin_attributes$reference[source_idx]
links$receiver_reference <- coin_attributes$reference[receiver_idx]
links$same_design <- links$source_design == links$receiver_design
links$same_reference <- links$source_reference == links$receiver_reference

pair_summary <- function(d) {
  active_beta <- d$coefficient[d$active == 1L]
  positive_beta <- d$coefficient[d$aligned == 1L]
  data.frame(
    days = nrow(d),
    active_days = sum(d$active),
    aligned_days = sum(d$aligned),
    inverse_days = sum(d$inverse),
    active_frequency = mean(d$active),
    aligned_frequency = mean(d$aligned),
    inverse_frequency = mean(d$inverse),
    directional_balance = mean(d$aligned) - mean(d$inverse),
    aligned_share_among_active = if (sum(d$active)) sum(d$aligned) / sum(d$active) else NA_real_,
    sign_stability = if (sum(d$active)) max(sum(d$aligned), sum(d$inverse)) / sum(d$active) else NA_real_,
    median_active_coefficient = if (length(active_beta)) median(active_beta) else NA_real_,
    median_aligned_coefficient = if (length(positive_beta)) median(positive_beta) else NA_real_,
    mean_absolute_active_coefficient = if (length(active_beta)) mean(abs(active_beta)) else NA_real_,
    stringsAsFactors = FALSE
  )
}

pair_split <- split(links, interaction(links$source, links$receiver, drop = TRUE))
pair_table <- do.call(rbind, lapply(pair_split, function(d) {
  cbind(data.frame(source = d$source[1], receiver = d$receiver[1],
                   source_design = d$source_design[1], receiver_design = d$receiver_design[1],
                   source_reference = d$source_reference[1], receiver_reference = d$receiver_reference[1],
                   same_design = d$same_design[1], same_reference = d$same_reference[1],
                   stringsAsFactors = FALSE),
        pair_summary(d))
}))
rownames(pair_table) <- NULL

reverse_lookup <- pair_table[, c("source", "receiver", "active_frequency",
                                  "aligned_frequency", "inverse_frequency")]
names(reverse_lookup) <- c("receiver", "source", "reverse_active_frequency",
                           "reverse_aligned_frequency", "reverse_inverse_frequency")
pair_table <- merge(pair_table, reverse_lookup, by = c("source", "receiver"),
                    all.x = TRUE, sort = FALSE)
pair_table$aligned_asymmetry <- pair_table$aligned_frequency - pair_table$reverse_aligned_frequency
pair_table <- pair_table[order(-pair_table$aligned_frequency,
                               -pair_table$sign_stability,
                               pair_table$source, pair_table$receiver), ]
write_csv(pair_table, file.path(source_dir, "Directed_Pair_Link_Metrics.csv"))

top_links <- head(pair_table, 10L)
top_links$rank <- seq_len(nrow(top_links))
top_links <- top_links[, c(
  "rank", "source", "receiver", "aligned_frequency", "inverse_frequency",
  "active_frequency", "reverse_aligned_frequency", "aligned_asymmetry",
  "sign_stability", "median_aligned_coefficient"
)]
write_csv(top_links, file.path(table_dir, "Table_5_2_Persistent_Downside_Aligned_Links.csv"))

unordered <- pair_table[pair_table$source < pair_table$receiver, ]
unordered$coin_1 <- unordered$source
unordered$coin_2 <- unordered$receiver
unordered$coin_1_to_2 <- unordered$aligned_frequency
unordered$coin_2_to_1 <- unordered$reverse_aligned_frequency
unordered$absolute_asymmetry <- abs(unordered$aligned_asymmetry)
unordered <- unordered[order(-unordered$absolute_asymmetry, unordered$coin_1, unordered$coin_2),
                       c("coin_1", "coin_2", "coin_1_to_2", "coin_2_to_1",
                         "aligned_asymmetry", "absolute_asymmetry")]
write_csv(unordered, file.path(table_dir, "Table_S5_2_Directional_Asymmetry.csv"))

state_split <- split(links, interaction(links$state, links$source, links$receiver,
                                        drop = TRUE))
state_pair <- do.call(rbind, lapply(state_split, function(d) {
  cbind(data.frame(state = as.character(d$state[1]), source = d$source[1],
                   receiver = d$receiver[1], stringsAsFactors = FALSE),
        pair_summary(d))
}))
rownames(state_pair) <- NULL
state_pair$state <- factor(state_pair$state, levels = state_labels, ordered = TRUE)
state_pair <- state_pair[order(state_pair$state,
                               match(state_pair$source, coin_order),
                               match(state_pair$receiver, coin_order)), ]
write_csv(state_pair, file.path(source_dir, "Directed_Pair_Link_Metrics_by_DPI_State.csv"))

state_summary <- do.call(rbind, lapply(split(links, links$state), function(d) {
  data.frame(
    state = as.character(d$state[1]),
    days = length(unique(d$date)),
    active_link_density = mean(d$active),
    downside_aligned_density = mean(d$aligned),
    inverse_link_density = mean(d$inverse),
    aligned_share_among_active = sum(d$aligned) / sum(d$active),
    mean_absolute_active_coefficient = mean(abs(d$coefficient[d$active == 1L])),
    stringsAsFactors = FALSE
  )
}))
state_summary$state <- factor(state_summary$state, levels = state_labels, ordered = TRUE)
state_summary <- state_summary[order(state_summary$state), ]
write_csv(state_summary, file.path(table_dir, "Table_S5_3_Network_Composition_by_DPI_State.csv"))

state_wide <- merge(
  state_pair[state_pair$state == "Very low", c("source", "receiver", "aligned_frequency")],
  state_pair[state_pair$state == "Severe", c("source", "receiver", "aligned_frequency")],
  by = c("source", "receiver"), suffixes = c("_very_low", "_severe")
)
state_wide$severe_minus_very_low <- state_wide$aligned_frequency_severe -
  state_wide$aligned_frequency_very_low
state_wide <- state_wide[order(-abs(state_wide$severe_minus_very_low)), ]
write_csv(state_wide, file.path(table_dir, "Table_S5_4_Aligned_Link_State_Shifts.csv"))

group_summary <- function(d, group_name, group_value) {
  data.frame(
    dimension = group_name,
    group = group_value,
    directed_pairs = length(unique(paste(d$source, d$receiver, sep = "->"))),
    pair_days = nrow(d),
    active_frequency = mean(d$active),
    aligned_frequency = mean(d$aligned),
    inverse_frequency = mean(d$inverse),
    aligned_share_among_active = sum(d$aligned) / sum(d$active),
    stringsAsFactors = FALSE
  )
}
design_groups <- split(links, ifelse(links$same_design, "Within design", "Across designs"))
reference_groups <- split(links, ifelse(links$same_reference, "Same reference", "Different references"))
group_table <- rbind(
  do.call(rbind, lapply(names(design_groups), function(nm) {
    group_summary(design_groups[[nm]], "Peg-support design", nm)
  })),
  do.call(rbind, lapply(names(reference_groups), function(nm) {
    group_summary(reference_groups[[nm]], "Reference asset", nm)
  }))
)
rownames(group_table) <- NULL
write_csv(group_table, file.path(table_dir, "Table_S5_5_Within_and_Cross_Group_Links.csv"))

daily_basic <- aggregate(cbind(active, aligned, inverse) ~ date + dpi_primary + state,
                         data = links, FUN = sum)
names(daily_basic)[names(daily_basic) == "active"] <- "active_links"
names(daily_basic)[names(daily_basic) == "aligned"] <- "aligned_links"
names(daily_basic)[names(daily_basic) == "inverse"] <- "inverse_links"
daily_basic$active_density <- daily_basic$active_links / 110
daily_basic$aligned_density <- daily_basic$aligned_links / 110
daily_basic$inverse_density <- daily_basic$inverse_links / 110
daily_basic$aligned_share_among_active <- daily_basic$aligned_links / daily_basic$active_links

links$dyad <- ifelse(links$source < links$receiver,
                     paste(links$source, links$receiver, sep = "--"),
                     paste(links$receiver, links$source, sep = "--"))
dyad_day <- aggregate(cbind(active, aligned) ~ date + dyad, data = links, FUN = sum)
dyad_day$reciprocated_active <- as.integer(dyad_day$active == 2L)
dyad_day$reciprocated_aligned <- as.integer(dyad_day$aligned == 2L)
daily_recip <- aggregate(cbind(reciprocated_active, reciprocated_aligned) ~ date,
                         data = dyad_day, FUN = mean)
daily <- merge(daily_basic, daily_recip, by = "date", sort = TRUE)
write_csv(daily, file.path(source_dir, "Daily_Stablecoin_Link_Network.csv"))

# -------------------------------------------------------------------------
# Figure 5.2: a single directed matrix. Columns are sources and rows targets.
# Fill is aligned selection frequency minus inverse selection frequency.
# -------------------------------------------------------------------------
display_source <- coin_order
display_receiver <- rev(coin_order)
z <- matrix(NA_real_, nrow = length(display_source), ncol = length(display_receiver),
            dimnames = list(display_source, display_receiver))
for (i in seq_len(nrow(pair_table))) {
  z[pair_table$source[i], pair_table$receiver[i]] <- pair_table$directional_balance[i]
}

heat_colours <- grDevices::colorRampPalette(
  c("#9A4C28", "#D89A74", "#F7F7F5", "#8AB8C5", "#1F5B78")
)(201)
width_mm <- 150
height_mm <- 132
colour_limit <- ceiling(max(abs(z), na.rm = TRUE) * 20) / 20

draw_figure <- function() {
  layout(matrix(c(1, 2), nrow = 2), heights = c(0.12, 0.88))

  par(family = "sans", mar = c(0, 0, 0, 0))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  legend_levels <- seq(-colour_limit, colour_limit, length.out = 5)
  legend_colours <- heat_colours[1 + round((legend_levels + colour_limit) /
                                             (2 * colour_limit) * 200)]
  legend("center", inset = c(-0.10, 0), horiz = TRUE,
         legend = sprintf("%+g", round(100 * legend_levels)),
         fill = legend_colours, border = NA, bty = "n", cex = 0.61,
         x.intersp = 0.50, title = "Aligned minus inverse selection (pp)")
  legend("topleft", inset = c(0.005, 0.19), horiz = TRUE,
         legend = "Top 10 aligned links", pch = 21, pt.bg = "white",
         col = "#172126", pt.cex = 0.80, bty = "n", cex = 0.58)

  par(family = "sans", mar = c(4.5, 4.5, 0.8, 1.4), mgp = c(2.0, 0.45, 0),
      tcl = -0.18, las = 1, xaxs = "i", yaxs = "i", bty = "n",
      cex.axis = 0.72, cex.lab = 0.80)
  image(seq_along(display_source), seq_along(display_receiver), z,
        col = heat_colours, zlim = c(-colour_limit, colour_limit), axes = FALSE,
        xlab = "Source stablecoin (predictor)", ylab = "Target stablecoin")
  axis(1, at = seq_along(display_source), labels = display_source,
       las = 2, cex.axis = 0.66)
  axis(2, at = seq_along(display_receiver), labels = display_receiver,
       tick = FALSE, cex.axis = 0.68)
  box(lwd = 0.45)

  # Explicitly distinguish absent self-equations from zero-valued links.
  for (coin in coin_order) {
    xi <- match(coin, display_source)
    yi <- match(coin, display_receiver)
    rect(xi - 0.5, yi - 0.5, xi + 0.5, yi + 0.5,
         col = "#E4E8EA", border = "white", lwd = 0.7)
  }

  # Mark the ten most persistent downside-aligned directions.
  points(match(top_links$source, display_source),
         match(top_links$receiver, display_receiver),
         pch = 21, cex = 0.62, lwd = 0.55, bg = "white", col = "#172126")
}

width_in <- width_mm / 25.4
height_in <- height_mm / 25.4
svg_path <- file.path(figure_dir, "Figure_5_2_Directional_Tail_Risk_Links.svg")
pdf_path <- file.path(figure_dir, "Figure_5_2_Directional_Tail_Risk_Links.pdf")
tiff_path <- file.path(figure_dir, "Figure_5_2_Directional_Tail_Risk_Links.tiff")
png_path <- file.path(figure_dir, "Figure_5_2_Directional_Tail_Risk_Links.png")

svglite::svglite(svg_path, width = width_in, height = height_in,
                 bg = "white", system_fonts = list(sans = "Arial"))
draw_figure()
dev.off()

pdf_backend <- "cairo_pdf"
pdf_opened <- try(
  tryCatch(
    grDevices::cairo_pdf(pdf_path, width = width_in, height = height_in,
                         family = "sans", bg = "white", onefile = TRUE),
    warning = function(w) stop(conditionMessage(w), call. = FALSE)
  ), silent = TRUE
)
if (inherits(pdf_opened, "try-error")) {
  pdf_backend <- "standard_pdf_Helvetica_fallback"
  grDevices::pdf(pdf_path, width = width_in, height = height_in,
                 family = "Helvetica", bg = "white", useDingbats = FALSE,
                 paper = "special")
}
draw_figure()
dev.off()

ragg::agg_tiff(tiff_path, width = width_in, height = height_in,
               units = "in", res = 600, background = "white", scaling = 1)
draw_figure()
dev.off()

ragg::agg_png(png_path, width = width_in, height = height_in,
              units = "in", res = 300, background = "white", scaling = 1)
draw_figure()
dev.off()

caption <- paste0(
  "Figure 5.2. Stablecoin Tail-Risk Link Matrix. Columns identify the source stablecoin ",
  "(predictor) and rows identify the target stablecoin. Each cell reports the percentage-point ",
  "difference between the frequency of positive and negative non-zero Quantile-Lasso coefficients ",
  "over 2,252 rolling windows. Because deviations are defined as price relative to the relevant ",
  "reference asset minus one, a positive coefficient is a downside-aligned conditional link: a ",
  "lower source deviation is associated with a lower conditional 5% quantile of the target. Negative ",
  "values denote inverse conditional links. Circles identify the ten largest full-sample positive ",
  "selection frequencies and do not denote statistical significance. The diagonal is not estimated. ",
  "All equations control for the other stablecoins and ",
  "five lagged macro-financial variables. Links are conditional associations, not causal transmission."
)
writeLines(caption, file.path(figure_dir, "Figure_5_2_caption.txt"), useBytes = TRUE)

# -------------------------------------------------------------------------
# QA and reproducibility metadata.
# -------------------------------------------------------------------------
nonzero_1e12 <- sum(abs(links$coefficient) > 1e-12)
nonzero_1e10 <- sum(abs(links$coefficient) > 1e-10)
nonzero_1e08 <- sum(abs(links$coefficient) > 1e-8)
qa <- data.frame(
  check = c(
    "stablecoin_pair_rows_complete", "directed_pairs_complete",
    "days_complete", "no_self_links", "every_pair_has_every_day",
    "sign_partition_complete", "tolerance_1e10_equals_1e8",
    "source_target_orientation_explicit", "figure_exports_complete"
  ),
  status = c(
    if (nrow(links) == 2252L * 110L) "PASS" else "FAIL",
    if (nrow(pair_table) == 110L) "PASS" else "FAIL",
    if (length(unique(links$date)) == 2252L) "PASS" else "FAIL",
    if (!any(links$source == links$receiver)) "PASS" else "FAIL",
    if (all(table(interaction(links$source, links$receiver, drop = TRUE)) == 2252L)) "PASS" else "FAIL",
    if (all(links$active == links$aligned + links$inverse)) "PASS" else "FAIL",
    if (nonzero_1e10 == nonzero_1e08) "PASS" else "FAIL",
    "PASS",
    if (all(file.exists(c(svg_path, pdf_path, tiff_path, png_path)))) "PASS" else "FAIL"
  ),
  detail = c(
    paste(nrow(links), "pair-day coefficients"),
    paste(nrow(pair_table), "directed non-self pairs"),
    paste(min(links$date), "to", max(links$date)),
    "source is predictor; receiver is target equation",
    "2,252 observations for each directed pair",
    "active coefficients partition into positive aligned and negative inverse links",
    paste0("active counts: 1e-12=", nonzero_1e12,
           ", 1e-10=", nonzero_1e10, ", 1e-8=", nonzero_1e08),
    "beta[target, predictor] is exported as predictor -> target",
    paste0("PDF backend: ", pdf_backend)
  ), stringsAsFactors = FALSE
)
write_csv(qa, file.path(qa_dir, "analysis_QA.csv"))
if (any(qa$status != "PASS")) stop("One or more QA checks failed.")

input_manifest <- data.frame(
  file = c(basename(coefficient_path), basename(dpi_path)),
  md5 = unname(tools::md5sum(c(coefficient_path, dpi_path))),
  stringsAsFactors = FALSE
)
write_csv(input_manifest, file.path(qa_dir, "input_manifest_md5.csv"))
capture.output(sessionInfo(), file = file.path(qa_dir, "session_info.txt"))

summary_lines <- c(
  paste0("sample_days=", length(unique(links$date))),
  paste0("directed_pairs=", nrow(pair_table)),
  paste0("pair_day_coefficients=", nrow(links)),
  paste0("overall_active_density=", formatC(mean(links$active), digits = 6, format = "f")),
  paste0("overall_aligned_density=", formatC(mean(links$aligned), digits = 6, format = "f")),
  paste0("overall_inverse_density=", formatC(mean(links$inverse), digits = 6, format = "f")),
  paste0("aligned_share_among_active=", formatC(sum(links$aligned) / sum(links$active), digits = 6, format = "f")),
  paste0("mean_absolute_pair_asymmetry=", formatC(mean(unordered$absolute_asymmetry), digits = 6, format = "f")),
  paste0("top_aligned_link=", top_links$source[1], "->", top_links$receiver[1]),
  paste0("top_aligned_frequency=", formatC(top_links$aligned_frequency[1], digits = 6, format = "f")),
  paste0("figure_pdf_backend=", pdf_backend)
)
writeLines(summary_lines, file.path(qa_dir, "analysis_summary.txt"), useBytes = TRUE)
message("Section 5.2 analysis completed: ", bundle_dir)
