#!/usr/bin/env Rscript

# Section 5.1: Composition of Active Risk Drivers
#
# This script uses the frozen primary Quantile-Lasso outputs from Chapter 4.
# It does not re-estimate the DPI and does not treat active-set membership as
# causal evidence. All 11 target equations and all 15 eligible predictors are
# retained for every date.

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
if (!file.exists(coefficient_path)) stop("Missing selected-coefficient input.")
if (!file.exists(dpi_path)) stop("Missing primary DPI input.")

write_csv <- function(x, path) {
  write.csv(x, path, row.names = FALSE, quote = TRUE, na = "", fileEncoding = "UTF-8")
}

coin_names <- c("USDC", "BUSD", "GUSD", "EURS", "IDRT", "USDT",
                "sUSD", "PAXG", "TUSD", "USDP", "DAI")
macro_names <- paste0("L1_", c("BV010082.Index", "CVIX.Index", "DXY.Curncy",
                               "SPX.Index", "VIX.Index"))
predictor_names <- c(coin_names, macro_names)
state_labels <- c("Very low", "Low", "Moderate", "Elevated", "Severe")
active_tolerance <- 1e-10

display_labels <- c(
  USDC = "USDC", BUSD = "BUSD", GUSD = "GUSD", EURS = "EURS",
  IDRT = "IDRT", USDT = "USDT", sUSD = "sUSD", PAXG = "PAXG",
  TUSD = "TUSD", USDP = "USDP", DAI = "DAI",
  `L1_BV010082.Index` = "1-year Treasury yield",
  `L1_CVIX.Index` = "CVIX", `L1_DXY.Curncy` = "DXY",
  `L1_SPX.Index` = "S&P 500", `L1_VIX.Index` = "VIX"
)
display_order <- c("USDC", "USDT", "BUSD", "TUSD", "USDP", "GUSD",
                   "DAI", "sUSD", "EURS", "IDRT", "PAXG",
                   "L1_BV010082.Index", "L1_CVIX.Index", "L1_DXY.Curncy",
                   "L1_SPX.Index", "L1_VIX.Index")

coefficients <- read.csv(gzfile(coefficient_path), check.names = FALSE,
                         stringsAsFactors = FALSE)
dpi <- read.csv(dpi_path, check.names = FALSE, stringsAsFactors = FALSE)
required_coef <- c("date", "target", "predictor", "predictor_type", "coefficient")
required_dpi <- c("date", "dpi_primary")
if (!all(required_coef %in% names(coefficients))) stop("Coefficient columns are incomplete.")
if (!all(required_dpi %in% names(dpi))) stop("DPI columns are incomplete.")
coefficients$date <- as.Date(coefficients$date)
dpi$date <- as.Date(dpi$date)
coefficients$coefficient <- as.numeric(coefficients$coefficient)

if (anyNA(coefficients[, required_coef]) || any(!is.finite(coefficients$coefficient))) {
  stop("Coefficient panel contains missing or non-finite values.")
}
if (anyDuplicated(coefficients[c("date", "target", "predictor")])) {
  stop("Duplicate date-target-predictor rows detected.")
}
if (anyDuplicated(dpi$date) || any(!is.finite(dpi$dpi_primary))) {
  stop("DPI series is incomplete or duplicated.")
}
if (!setequal(unique(coefficients$target), coin_names)) stop("Unexpected target universe.")
if (!setequal(unique(coefficients$predictor), predictor_names)) stop("Unexpected predictor universe.")
if (!identical(sort(unique(coefficients$date)), sort(dpi$date))) stop("DPI and coefficient dates differ.")

rows_per_model <- table(interaction(coefficients$date, coefficients$target, drop = TRUE))
rows_per_day <- table(coefficients$date)
if (!all(rows_per_model == 15L)) stop("Each target-date must contain 15 predictors.")
if (!all(rows_per_day == 165L)) stop("Each date must contain 11 x 15 coefficients.")

coefficients$active <- as.integer(abs(coefficients$coefficient) > active_tolerance)
coefficients$active_stablecoin <- coefficients$active *
  as.integer(coefficients$predictor_type == "stablecoin")
coefficients$active_macro <- coefficients$active *
  as.integer(coefficients$predictor_type == "lagged_macro")

dpi_breaks <- as.numeric(quantile(dpi$dpi_primary, probs = seq(0, 1, 0.2), type = 8))
dpi$state <- cut(dpi$dpi_primary,
                 breaks = c(-Inf, dpi_breaks[2:5], Inf),
                 labels = state_labels, right = TRUE, ordered_result = TRUE)
state_lookup <- dpi[, c("date", "dpi_primary", "state")]
match_index <- match(coefficients$date, state_lookup$date)
if (anyNA(match_index)) stop("State merge failed.")
coefficients$dpi_primary <- state_lookup$dpi_primary[match_index]
coefficients$state <- state_lookup$state[match_index]

target_day <- aggregate(
  cbind(active, active_stablecoin, active_macro) ~ date + target,
  data = coefficients, FUN = sum
)
names(target_day)[names(target_day) == "active"] <- "active_count"
names(target_day)[names(target_day) == "active_stablecoin"] <- "stablecoin_count"
names(target_day)[names(target_day) == "active_macro"] <- "macro_count"
target_day$stablecoin_inclusion_rate <- target_day$stablecoin_count / 10
target_day$macro_inclusion_rate <- target_day$macro_count / 5
target_day$active_share <- target_day$active_count / 15
target_day$dpi_primary <- state_lookup$dpi_primary[match(target_day$date, state_lookup$date)]
target_day$state <- state_lookup$state[match(target_day$date, state_lookup$date)]
target_day <- target_day[order(target_day$date, match(target_day$target, coin_names)), ]
write_csv(target_day, file.path(source_dir, "Target_Day_Active_Set_Metrics.csv"))

daily_counts <- aggregate(
  cbind(active_count, stablecoin_count, macro_count) ~ date + dpi_primary + state,
  data = target_day, FUN = sum
)
daily_counts$mean_active_count <- daily_counts$active_count / 11
daily_counts$stablecoin_inclusion_rate <- daily_counts$stablecoin_count / (11 * 10)
daily_counts$macro_inclusion_rate <- daily_counts$macro_count / (11 * 5)
daily_counts$stablecoin_share_of_selected <- daily_counts$stablecoin_count / daily_counts$active_count
daily_counts$year <- as.integer(format(daily_counts$date, "%Y"))
daily_counts <- daily_counts[order(daily_counts$date), ]
write_csv(daily_counts, file.path(source_dir, "Daily_Active_Set_Composition.csv"))

predictor_summary_for_rows <- function(z) {
  by_predictor <- aggregate(active ~ predictor + predictor_type, data = z,
                            FUN = function(x) c(active = sum(x), eligible = length(x), rate = mean(x)))
  values <- by_predictor$active
  if (!is.matrix(values)) values <- do.call(rbind, values)
  data.frame(
    predictor = by_predictor$predictor,
    predictor_type = by_predictor$predictor_type,
    active_coefficients = values[, "active"],
    eligible_coefficients = values[, "eligible"],
    selection_rate = values[, "rate"],
    stringsAsFactors = FALSE
  )
}

state_row <- function(state_name) {
  z <- target_day[target_day$state == state_name, ]
  cz <- coefficients[coefficients$state == state_name, ]
  ps <- predictor_summary_for_rows(cz)
  top_coin <- ps$predictor[which.max(ifelse(ps$predictor_type == "stablecoin",
                                           ps$selection_rate, -Inf))]
  top_macro <- ps$predictor[which.max(ifelse(ps$predictor_type == "lagged_macro",
                                            ps$selection_rate, -Inf))]
  data.frame(
    state = state_name,
    days = length(unique(z$date)),
    target_models = nrow(z),
    dpi_min = min(z$dpi_primary),
    dpi_max = max(z$dpi_primary),
    mean_active_count_of_15 = mean(z$active_count),
    median_active_count_of_15 = median(z$active_count),
    active_count_q25 = unname(quantile(z$active_count, .25, type = 8)),
    active_count_q75 = unname(quantile(z$active_count, .75, type = 8)),
    stablecoin_inclusion_rate = sum(z$stablecoin_count) / (nrow(z) * 10),
    macro_inclusion_rate = sum(z$macro_count) / (nrow(z) * 5),
    stablecoin_share_of_selected = sum(z$stablecoin_count) / sum(z$active_count),
    top_stablecoin_predictor = top_coin,
    top_macro_predictor = top_macro,
    stringsAsFactors = FALSE
  )
}

overall_ps <- predictor_summary_for_rows(coefficients)
overall_top_coin <- overall_ps$predictor[which.max(ifelse(
  overall_ps$predictor_type == "stablecoin", overall_ps$selection_rate, -Inf))]
overall_top_macro <- overall_ps$predictor[which.max(ifelse(
  overall_ps$predictor_type == "lagged_macro", overall_ps$selection_rate, -Inf))]
overall_row <- data.frame(
  state = "Overall",
  days = length(unique(target_day$date)),
  target_models = nrow(target_day),
  dpi_min = min(target_day$dpi_primary),
  dpi_max = max(target_day$dpi_primary),
  mean_active_count_of_15 = mean(target_day$active_count),
  median_active_count_of_15 = median(target_day$active_count),
  active_count_q25 = unname(quantile(target_day$active_count, .25, type = 8)),
  active_count_q75 = unname(quantile(target_day$active_count, .75, type = 8)),
  stablecoin_inclusion_rate = sum(target_day$stablecoin_count) / (nrow(target_day) * 10),
  macro_inclusion_rate = sum(target_day$macro_count) / (nrow(target_day) * 5),
  stablecoin_share_of_selected = sum(target_day$stablecoin_count) / sum(target_day$active_count),
  top_stablecoin_predictor = overall_top_coin,
  top_macro_predictor = overall_top_macro,
  stringsAsFactors = FALSE
)
state_table <- rbind(overall_row, do.call(rbind, lapply(state_labels, state_row)))
write_csv(state_table, file.path(table_dir, "Table_5_1_Active_Set_Composition_by_DPI_State.csv"))

# Predictor-level selection rates, normalized by the number of equations in
# which each predictor is eligible. A stablecoin predictor is eligible in ten
# target equations; each macro predictor is eligible in all eleven.
predictor_table <- overall_ps
for (state_name in state_labels) {
  z <- predictor_summary_for_rows(coefficients[coefficients$state == state_name, ])
  predictor_table[[paste0(gsub(" ", "_", tolower(state_name)), "_selection_rate")]] <-
    z$selection_rate[match(predictor_table$predictor, z$predictor)]
}
predictor_table$severe_minus_very_low_percentage_points <- 100 * (
  predictor_table$severe_selection_rate - predictor_table$very_low_selection_rate
)
predictor_table$display_label <- unname(display_labels[predictor_table$predictor])
predictor_table <- predictor_table[match(display_order, predictor_table$predictor), ]

coefficients$month <- as.Date(paste0(format(coefficients$date, "%Y-%m"), "-01"))
monthly <- aggregate(active ~ month + predictor + predictor_type,
                     data = coefficients, FUN = function(x) c(active = sum(x),
                                                              eligible = length(x),
                                                              rate = mean(x)))
monthly_values <- monthly$active
if (!is.matrix(monthly_values)) monthly_values <- do.call(rbind, monthly_values)
monthly$active_coefficients <- monthly_values[, "active"]
monthly$eligible_coefficients <- monthly_values[, "eligible"]
monthly$selection_frequency <- monthly_values[, "rate"]
monthly$active <- NULL
monthly$display_label <- unname(display_labels[monthly$predictor])
monthly$overall_selection_frequency <- predictor_table$selection_rate[
  match(monthly$predictor, predictor_table$predictor)
]
monthly <- monthly[order(monthly$month, match(monthly$predictor, display_order)), ]
write_csv(monthly, file.path(source_dir, "Figure_5_1_Active_Risk_Drivers.csv"))

monthly_range <- aggregate(selection_frequency ~ predictor, data = monthly,
                           FUN = function(x) c(min = min(x), max = max(x), sd = sd(x)))
monthly_range_values <- monthly_range$selection_frequency
if (!is.matrix(monthly_range_values)) {
  monthly_range_values <- do.call(rbind, monthly_range_values)
}
predictor_table$monthly_min_selection_rate <- monthly_range_values[
  match(predictor_table$predictor, monthly_range$predictor), "min"
]
predictor_table$monthly_max_selection_rate <- monthly_range_values[
  match(predictor_table$predictor, monthly_range$predictor), "max"
]
predictor_table$monthly_sd_selection_rate <- monthly_range_values[
  match(predictor_table$predictor, monthly_range$predictor), "sd"
]
write_csv(predictor_table,
          file.path(table_dir, "Table_S5_1_Predictor_Selection_Frequencies.csv"))

target_table <- do.call(rbind, lapply(coin_names, function(target_name) {
  z <- target_day[target_day$target == target_name, ]
  data.frame(
    target = target_name,
    observations = nrow(z),
    mean_active_count_of_15 = mean(z$active_count),
    median_active_count_of_15 = median(z$active_count),
    stablecoin_inclusion_rate = mean(z$stablecoin_inclusion_rate),
    macro_inclusion_rate = mean(z$macro_inclusion_rate),
    stringsAsFactors = FALSE
  )
}))
write_csv(target_table, file.path(table_dir, "Table_S5_2_Target_Active_Set_Size.csv"))

year_table <- aggregate(
  cbind(mean_active_count, stablecoin_inclusion_rate, macro_inclusion_rate,
        stablecoin_share_of_selected) ~ year,
  data = daily_counts, FUN = mean
)
year_days <- aggregate(date ~ year, data = daily_counts, FUN = length)
names(year_days)[2] <- "days"
year_table <- merge(year_days, year_table, by = "year", sort = TRUE)
write_csv(year_table, file.path(table_dir, "Table_S5_3_Active_Set_Composition_by_Year.csv"))

# -------------------------------------------------------------------------
# Figure 5.1: one integrated heatmap, not a multi-panel composition.
# -------------------------------------------------------------------------
month_values <- sort(unique(monthly$month))
heat_matrix <- matrix(NA_real_, nrow = length(month_values), ncol = length(display_order),
                      dimnames = list(format(month_values), display_order))
for (i in seq_len(nrow(monthly))) {
  heat_matrix[format(monthly$month[i]), monthly$predictor[i]] <-
    monthly$selection_frequency[i]
}
if (anyNA(heat_matrix)) stop("Monthly heatmap matrix is incomplete.")

heat_colours <- grDevices::colorRampPalette(
  c("#F5F7F7", "#D6E4E7", "#9FC4CB", "#4F8FA1", "#174A67")
)(101)
width_mm <- 183
height_mm <- 118

draw_figure <- function() {
  par(family = "sans", mar = c(3.6, 7.3, 2.5, 6.0), mgp = c(1.8, 0.45, 0),
      tcl = -0.18, las = 1, xaxs = "i", yaxs = "i", bty = "n",
      cex.axis = 0.72, cex.lab = 0.82)
  bottom_to_top <- rev(display_order)
  z <- heat_matrix[, bottom_to_top, drop = FALSE]
  image(seq_along(month_values), seq_along(bottom_to_top), z,
        col = heat_colours, zlim = c(0, 1), axes = FALSE,
        xlab = "Date", ylab = "")
  month_year <- format(month_values, "%Y")
  year_values <- unique(month_year)
  year_midpoints <- vapply(year_values, function(y) {
    mean(which(month_year == y))
  }, numeric(1))
  year_boundaries <- vapply(year_values[-1L], function(y) {
    min(which(month_year == y)) - 0.5
  }, numeric(1))
  axis(1, at = year_midpoints, labels = year_values, cex.axis = 0.72)
  axis(2, at = seq_along(bottom_to_top),
       labels = unname(display_labels[bottom_to_top]), tick = FALSE,
       las = 1, cex.axis = 0.66)
  abline(v = year_boundaries, col = grDevices::adjustcolor("white", alpha.f = 0.72),
         lwd = 0.55)
  abline(h = 5.5, col = "white", lwd = 1.4)
  box(lwd = 0.45)
  mtext("Monthly selection frequency", side = 3, line = 0.65,
        adj = 0, font = 2, cex = 0.88)
  mtext("Stablecoin predictors", side = 2, line = 5.9, at = 11,
        las = 3, cex = 0.58, col = "#4F5B60")
  mtext("Macro-financial predictors", side = 2, line = 5.9, at = 3,
        las = 3, cex = 0.58, col = "#4F5B60")

  overall_rates <- predictor_table$selection_rate[
    match(bottom_to_top, predictor_table$predictor)
  ]
  usr <- par("usr")
  text(usr[2] + 2.0, seq_along(bottom_to_top),
       labels = sprintf("%.1f%%", 100 * overall_rates),
       adj = c(0, 0.5), cex = 0.58, xpd = NA, col = "#263238")
  text(usr[2] + 2.0, usr[4] + 0.65, "Full sample",
       adj = c(0, 0.5), cex = 0.58, font = 2, xpd = NA, col = "#263238")

  legend_levels <- c(0, .25, .50, .75, 1)
  legend_colours <- heat_colours[1 + round(legend_levels * 100)]
  legend("topright", inset = c(0.00, -0.105), xpd = NA, horiz = TRUE,
         legend = paste0(100 * legend_levels, "%"), fill = legend_colours,
         border = NA, bty = "n", cex = 0.57, x.intersp = 0.55,
         title = "Selection frequency")
}

width_in <- width_mm / 25.4
height_in <- height_mm / 25.4
svg_path <- file.path(figure_dir, "Figure_5_1_Active_Risk_Drivers.svg")
pdf_path <- file.path(figure_dir, "Figure_5_1_Active_Risk_Drivers.pdf")
tiff_path <- file.path(figure_dir, "Figure_5_1_Active_Risk_Drivers.tiff")
png_path <- file.path(figure_dir, "Figure_5_1_Active_Risk_Drivers.png")

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
  ),
  silent = TRUE
)
if (inherits(pdf_opened, "try-error")) {
  # Some macOS R installations expose Cairo support but require XQuartz at runtime.
  # The svglite export above remains fully editable; use a portable vector-PDF fallback.
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
  "Figure 5.1. Selection Frequency of Active Risk Drivers. ",
  "Cells report the monthly proportion of eligible target equations in which each predictor ",
  "has an absolute Quantile-Lasso coefficient above 1e-10. Stablecoin predictors are eligible ",
  "for ten target equations and lagged macro-financial predictors for eleven. The estimates use ",
  "the primary 90-day, 5% Quantile-Lasso specification with window-local median/MAD scaling and ",
  "strict minimum finite GACV. Monthly aggregation is used only for display; all 2,252 daily ",
  "windows and 24,772 target equations enter the calculations. Selection indicates a conditional ",
  "association within the rolling lower-tail equation, not causal transmission or economic magnitude."
)
writeLines(caption, file.path(figure_dir, "Figure_5_1_caption.txt"), useBytes = TRUE)

# -------------------------------------------------------------------------
# Reproducibility and QA.
# -------------------------------------------------------------------------
nonzero_1e12 <- sum(abs(coefficients$coefficient) > 1e-12)
nonzero_1e10 <- sum(abs(coefficients$coefficient) > 1e-10)
nonzero_1e08 <- sum(abs(coefficients$coefficient) > 1e-8)
qa <- data.frame(
  check = c(
    "coefficient_rows_complete", "target_date_models_complete",
    "dates_match_primary_dpi", "target_universe_complete",
    "predictor_universe_complete", "coefficients_finite",
    "tolerance_1e10_equals_1e8", "monthly_source_complete",
    "figure_exports_complete"
  ),
  status = c(
    if (nrow(coefficients) == 2252L * 11L * 15L) "PASS" else "FAIL",
    if (nrow(target_day) == 2252L * 11L) "PASS" else "FAIL",
    "PASS", "PASS", "PASS", "PASS",
    if (nonzero_1e10 == nonzero_1e08) "PASS" else "FAIL",
    if (nrow(monthly) == length(month_values) * length(predictor_names)) "PASS" else "FAIL",
    if (all(file.exists(c(svg_path, pdf_path, tiff_path, png_path)))) "PASS" else "FAIL"
  ),
  detail = c(
    paste0(nrow(coefficients), " rows"), paste0(nrow(target_day), " models"),
    paste0(min(dpi$date), " to ", max(dpi$date)),
    paste(length(unique(coefficients$target)), "targets"),
    paste(length(unique(coefficients$predictor)), "predictors"),
    paste(sum(is.finite(coefficients$coefficient)), "finite coefficients"),
    paste0("active counts: 1e-12=", nonzero_1e12,
           ", 1e-10=", nonzero_1e10, ", 1e-8=", nonzero_1e08),
    paste0(nrow(monthly), " predictor-month rows"),
    paste0("PDF backend: ", pdf_backend)
  ),
  stringsAsFactors = FALSE
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
  paste0("sample_days=", nrow(dpi)),
  paste0("target_models=", nrow(target_day)),
  paste0("candidate_coefficients=", nrow(coefficients)),
  paste0("active_coefficients_tolerance_1e10=", nonzero_1e10),
  paste0("mean_active_count_of_15=", formatC(overall_row$mean_active_count_of_15, digits = 6, format = "f")),
  paste0("median_active_count_of_15=", overall_row$median_active_count_of_15),
  paste0("stablecoin_inclusion_rate=", formatC(overall_row$stablecoin_inclusion_rate, digits = 6, format = "f")),
  paste0("macro_inclusion_rate=", formatC(overall_row$macro_inclusion_rate, digits = 6, format = "f")),
  paste0("stablecoin_share_of_selected=", formatC(overall_row$stablecoin_share_of_selected, digits = 6, format = "f")),
  paste0("top_stablecoin_predictor=", overall_top_coin),
  paste0("top_macro_predictor=", overall_top_macro),
  paste0("dpi_quintile_breaks=", paste(formatC(dpi_breaks[2:5], digits = 6, format = "f"), collapse = ",")),
  paste0("figure_pdf_backend=", pdf_backend)
)
writeLines(summary_lines, file.path(qa_dir, "analysis_summary.txt"), useBytes = TRUE)
message("Section 5.1 analysis completed: ", bundle_dir)
