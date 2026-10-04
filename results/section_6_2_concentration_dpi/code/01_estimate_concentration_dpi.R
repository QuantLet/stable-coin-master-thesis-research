#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE, scipen = 999)

project_root <- normalizePath(getwd(), mustWork = TRUE)
bundle_dir <- file.path(project_root, "outputs", "section_6_2_concentration_dpi")
dirs <- file.path(bundle_dir, c("source_data", "tables", "figures", "qa"))
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

local_library <- file.path(project_root, "work", "section_4_5_stable_crypto", "r_library")
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))

required <- c("sandwich", "ggplot2", "svglite", "ragg")
missing_packages <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Missing R packages: ", paste(missing_packages, collapse = ", "))

library(ggplot2)

frozen_input_dir <- file.path(bundle_dir, "source_data", "input")
resolve_input <- function(filename, canonical_path) {
  frozen_path <- file.path(frozen_input_dir, filename)
  if (file.exists(frozen_path)) frozen_path else canonical_path
}

dpi_file <- resolve_input(
  "dpi_primary_robust_mad_strict.csv",
  file.path(project_root, "outputs", "section_5_1_active_risk_drivers", "source_data",
            "input", "dpi_primary_robust_mad_strict.csv")
)
concentration_file <- resolve_input(
  "Table_6_1_Daily_Concentration.csv",
  file.path(project_root, "outputs", "section_6_1_market_concentration", "tables",
            "Table_6_1_Daily_Concentration.csv")
)
crypto_file <- resolve_input(
  "Section_4_5_daily_analysis_panel.csv",
  file.path(project_root, "outputs", "section_4_5_stable_crypto", "source_data",
            "prepared", "Section_4_5_daily_analysis_panel.csv")
)
macro_file <- resolve_input(
  "Stable_Macro_20260531.csv",
  file.path(project_root, "outputs", "section_5_4_external_vs_internal", "source_data",
            "input", "Stable_Macro_20260531.csv")
)
onchain_file <- resolve_input(
  "stablecoin_onchain_daily_20200101_20260531.csv",
  file.path(project_root, "outputs", "onchain_microdata_20260919",
            "stablecoin_onchain_daily_20200101_20260531.csv")
)
input_files <- c(dpi_file, concentration_file, crypto_file, macro_file, onchain_file)
if (any(!file.exists(input_files))) stop("Required inputs are missing: ", paste(input_files[!file.exists(input_files)], collapse = ", "))

hac_lag <- 90L
alpha <- 0.05

parse_date <- function(x) {
  x <- as.character(x)
  slash <- grepl("/", x, fixed = TRUE)
  out <- as.Date(rep(NA_character_, length(x)))
  if (any(slash)) out[slash] <- as.Date(x[slash], "%m/%d/%Y")
  if (any(!slash)) out[!slash] <- as.Date(x[!slash], "%Y-%m-%d")
  if (anyNA(out)) stop("Date parsing failed.")
  out
}

lag_vec <- function(x, k = 1L) {
  k <- as.integer(k)
  if (k <= 0L) return(x)
  c(rep(NA, k), head(x, -k))
}

locf_past <- function(x) {
  x <- as.numeric(x)
  last_value <- NA_real_
  for (i in seq_along(x)) {
    if (is.finite(x[i])) last_value <- x[i]
    else if (is.finite(last_value)) x[i] <- last_value
  }
  x
}

z_full <- function(x) as.numeric((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))

dpi <- read.csv(dpi_file, check.names = FALSE)
dpi$date <- parse_date(dpi$date)
stopifnot(!anyDuplicated(dpi$date), all(is.finite(dpi$dpi_primary)), all(dpi$dpi_primary > 0))

concentration <- read.csv(concentration_file, check.names = FALSE)
concentration$date <- parse_date(concentration$date)
stopifnot(!anyDuplicated(concentration$date))

crypto <- read.csv(crypto_file, check.names = FALSE)
crypto$date <- parse_date(crypto$date)
crypto <- crypto[c("date", "frm_crypto_dynamic_strict")]
stopifnot(!anyDuplicated(crypto$date), all(crypto$frm_crypto_dynamic_strict > 0, na.rm = TRUE))

macro <- read.csv(macro_file, check.names = FALSE)
names(macro)[1L] <- "date"
macro$date <- parse_date(macro$date)
macro_order <- c("BV010082.Index", "CVIX.Index", "DXY.Curncy", "SPX.Index", "VIX.Index")
stopifnot(identical(names(macro)[-1L], macro_order), !anyDuplicated(macro$date))
macro_level <- as.matrix(macro[, macro_order, drop = FALSE])
storage.mode(macro_level) <- "numeric"
macro_level <- apply(macro_level, 2L, locf_past)
if (is.null(dim(macro_level))) macro_level <- matrix(macro_level, ncol = length(macro_order))
colnames(macro_level) <- macro_order
stopifnot(all(is.finite(macro_level)), all(macro_level > 0))
macro_change <- rbind(rep(NA_real_, ncol(macro_level)), diff(log(macro_level)))
lagged_macro <- apply(macro_change, 2L, lag_vec, k = 1L)
if (is.null(dim(lagged_macro))) lagged_macro <- matrix(lagged_macro, ncol = length(macro_order))
colnames(lagged_macro) <- paste0("L1_dlog_", macro_order)
macro_predictors <- data.frame(date = macro$date, lagged_macro, check.names = FALSE)

panel <- merge(
  dpi[c("date", "dpi_primary")],
  concentration[c("date", "active_coins", "sample_market_cap_usd", "nhhi", "top2_share")],
  by = "date", all = FALSE, sort = TRUE
)
panel <- merge(panel, crypto, by = "date", all.x = TRUE, sort = TRUE)
panel <- merge(panel, macro_predictors, by = "date", all.x = TRUE, sort = TRUE)

onchain <- read.csv(onchain_file, check.names = FALSE)
onchain$date <- parse_date(onchain$date)
stopifnot(!anyDuplicated(onchain$date))
source_ok <- onchain$curve_3pool_hours == 24 &
  onchain$uni_usdc_usdt_liquidity_snapshot_hours == 24
micro_raw <- data.frame(
  date = onchain$date,
  curve_imbalance = as.numeric(onchain$curve_3pool_imbalance_eod),
  log_swap_count = log1p(as.numeric(onchain$curve_3pool_swap_count)),
  log_near_inventory = log1p(as.numeric(onchain$uni_usdc_usdt_inventory_pm25ticks_usd_eod))
)
micro_raw[!source_ok | is.na(source_ok), c("curve_imbalance", "log_swap_count", "log_near_inventory")] <- NA_real_
micro_raw$L1_curve_imbalance <- lag_vec(micro_raw$curve_imbalance)
micro_raw$L1_log_swap_count <- lag_vec(micro_raw$log_swap_count)
micro_raw$L1_log_near_inventory <- lag_vec(micro_raw$log_near_inventory)
panel <- merge(
  panel,
  micro_raw[c("date", "L1_curve_imbalance", "L1_log_swap_count", "L1_log_near_inventory")],
  by = "date", all.x = TRUE, sort = TRUE
)

panel$log_dpi <- log(panel$dpi_primary)
panel$log_mcap <- log(panel$sample_market_cap_usd)
panel$log_crypto_frm <- log(panel$frm_crypto_dynamic_strict)
panel$z_nhhi <- z_full(panel$nhhi)
panel$z_top2 <- z_full(panel$top2_share)
panel$year <- factor(format(panel$date, "%Y"))

panel$L1_log_dpi <- lag_vec(panel$log_dpi)
panel$L1_log_mcap <- lag_vec(panel$log_mcap)
panel$L1_log_crypto_frm <- lag_vec(panel$log_crypto_frm)
panel$L1_active_coins <- lag_vec(panel$active_coins)
panel$L1_z_nhhi <- lag_vec(panel$z_nhhi)
panel$L1_z_top2 <- lag_vec(panel$z_top2)
panel$L30_z_nhhi <- lag_vec(panel$z_nhhi, 30L)
panel$L30_z_top2 <- lag_vec(panel$z_top2, 30L)

panel$d30_log_dpi <- panel$log_dpi - lag_vec(panel$log_dpi, 30L)
panel$L1_d30_log_dpi <- lag_vec(panel$d30_log_dpi)
panel$d30_z_nhhi <- panel$z_nhhi - lag_vec(panel$z_nhhi, 30L)
panel$d30_z_top2 <- panel$z_top2 - lag_vec(panel$z_top2, 30L)
panel$L1_d30_z_nhhi <- lag_vec(panel$d30_z_nhhi)
panel$L1_d30_z_top2 <- lag_vec(panel$d30_z_top2)
panel$d30_log_mcap <- panel$log_mcap - lag_vec(panel$log_mcap, 30L)
panel$d30_log_crypto_frm <- panel$log_crypto_frm - lag_vec(panel$log_crypto_frm, 30L)
panel$L1_d30_log_mcap <- lag_vec(panel$d30_log_mcap)
panel$L1_d30_log_crypto_frm <- lag_vec(panel$d30_log_crypto_frm)

event_dates <- as.Date(c("2022-11-08", "2023-03-11"))
panel$major_event_window <- Reduce(
  `|`,
  lapply(event_dates, function(d) panel$date >= d - 60 & panel$date <= d + 60)
)

macro_terms <- colnames(lagged_macro)
level_controls <- c(
  "L1_log_dpi", "L1_log_mcap",
  macro_terms, "L1_active_coins", "year"
)
change_controls <- c(
  "L1_d30_log_dpi", "L1_d30_log_mcap",
  macro_terms, "year"
)
micro_terms <- c("L1_curve_imbalance", "L1_log_swap_count", "L1_log_near_inventory")

fit_hac <- function(data, response, predictor, controls, specification, metric,
                    subset_rows = rep(TRUE, nrow(data))) {
  variables <- unique(c(response, predictor, controls))
  keep <- subset_rows & stats::complete.cases(data[, variables, drop = FALSE])
  d <- data[keep, , drop = FALSE]
  if (nrow(d) < 150L) stop("Insufficient observations for ", specification, " / ", metric)
  rhs <- paste(c(predictor, controls), collapse = " + ")
  fit <- lm(as.formula(paste(response, "~", rhs)), data = d)
  V <- sandwich::NeweyWest(fit, lag = hac_lag, prewhite = FALSE, adjust = TRUE)
  b <- coef(fit)[predictor]
  se <- sqrt(diag(V))[predictor]
  z <- b / se
  p <- 2 * pnorm(-abs(z))
  q <- qnorm(1 - alpha / 2)
  ci <- b + c(-1, 1) * q * se
  data.frame(
    metric = metric,
    specification = specification,
    response = response,
    predictor = predictor,
    estimate_log_points = unname(b),
    hac90_se = unname(se),
    ci95_low_log_points = ci[1L],
    ci95_high_log_points = ci[2L],
    p_value = unname(p),
    effect_pct = 100 * (exp(b) - 1),
    ci95_low_pct = 100 * (exp(ci[1L]) - 1),
    ci95_high_pct = 100 * (exp(ci[2L]) - 1),
    n = nobs(fit),
    r_squared = summary(fit)$r.squared,
    adjusted_r_squared = summary(fit)$adj.r.squared,
    first_date = min(d$date),
    last_date = max(d$date),
    stringsAsFactors = FALSE
  )
}

metric_map <- list(
  NHHI = list(level = "L1_z_nhhi", lag30 = "L30_z_nhhi", change = "L1_d30_z_nhhi"),
  `Top-2 market share` = list(level = "L1_z_top2", lag30 = "L30_z_top2", change = "L1_d30_z_top2")
)

results <- list()
k <- 0L
for (metric in names(metric_map)) {
  pred <- metric_map[[metric]]
  k <- k + 1L
  results[[k]] <- fit_hac(panel, "log_dpi", pred$level, "L1_log_dpi",
                          "Dynamic baseline", metric)
  k <- k + 1L
  results[[k]] <- fit_hac(
    panel, "log_dpi", pred$level,
    c("L1_log_dpi", "L1_log_mcap", "L1_active_coins"),
    "Market-size adjusted", metric
  )
  k <- k + 1L
  results[[k]] <- fit_hac(
    panel, "log_dpi", pred$level,
    c("L1_log_dpi", "L1_log_mcap", macro_terms, "L1_active_coins"),
    "Macro adjusted", metric
  )
  k <- k + 1L
  results[[k]] <- fit_hac(panel, "log_dpi", pred$level, level_controls,
                          "Adjusted primary", metric)
  k <- k + 1L
  results[[k]] <- fit_hac(panel, "log_dpi", pred$level,
                          c(level_controls, "L1_log_crypto_frm"),
                          "Crypto FRM control", metric)
  k <- k + 1L
  results[[k]] <- fit_hac(panel, "log_dpi", pred$lag30, level_controls,
                          "30-day concentration lag", metric)
  k <- k + 1L
  results[[k]] <- fit_hac(panel, "d30_log_dpi", pred$change, change_controls,
                          "30-day changes", metric)
  k <- k + 1L
  results[[k]] <- fit_hac(
    panel, "log_dpi", pred$level, setdiff(level_controls, "L1_active_coins"),
    "Complete 11-coin dates", metric,
    subset_rows = panel$active_coins == 11 & panel$L1_active_coins == 11
  )
  k <- k + 1L
  results[[k]] <- fit_hac(
    panel, "log_dpi", pred$level, level_controls,
    "FTX/SVB windows excluded", metric,
    subset_rows = !panel$major_event_window
  )
  k <- k + 1L
  results[[k]] <- fit_hac(
    panel, "log_dpi", pred$level, c(level_controls, micro_terms),
    "Additional on-chain controls", metric,
    subset_rows = panel$date >= as.Date("2022-04-02")
  )
}
results <- do.call(rbind, results)

results$p_holm_within_specification <- ave(
  results$p_value,
  results$specification,
  FUN = function(p) p.adjust(p, method = "holm")
)

primary_i <- results$specification == "Adjusted primary"
results$p_holm_primary_pair <- NA_real_
results$p_holm_primary_pair[primary_i] <- p.adjust(results$p_value[primary_i], method = "holm")

block_bootstrap_primary <- function(data, predictor, metric, controls = level_controls,
                                    block_length = 90L, repetitions = 999L, seed = 6202L) {
  variables <- unique(c("log_dpi", predictor, controls))
  keep <- complete.cases(data[, variables, drop = FALSE])
  d <- data[keep, , drop = FALSE]
  formula <- as.formula(paste("log_dpi ~", paste(c(predictor, controls), collapse = " + ")))
  fit <- lm(formula, data = d)
  X <- model.matrix(fit)
  yhat <- fitted(fit)
  residual <- resid(fit) - mean(resid(fit))
  n <- length(residual)
  block_length <- min(block_length, n)
  set.seed(seed)
  boot_beta <- numeric(repetitions)
  predictor_col <- match(predictor, colnames(X))
  if (is.na(predictor_col)) stop("Bootstrap predictor is absent from the model matrix.")
  max_start <- n - block_length + 1L
  n_blocks <- ceiling(n / block_length)
  for (b in seq_len(repetitions)) {
    starts <- sample.int(max_start, n_blocks, replace = TRUE)
    index <- unlist(lapply(starts, function(s) s:(s + block_length - 1L)), use.names = FALSE)
    e_star <- residual[index[seq_len(n)]]
    fit_star <- lm.fit(X, yhat + e_star)
    boot_beta[b] <- fit_star$coefficients[predictor_col]
  }
  ci <- unname(quantile(boot_beta, c(0.025, 0.975), na.rm = TRUE, names = FALSE))
  data.frame(
    metric = metric,
    predictor = predictor,
    block_length = block_length,
    repetitions = repetitions,
    estimate_log_points = unname(coef(fit)[predictor]),
    bootstrap_ci95_low_log_points = ci[1L],
    bootstrap_ci95_high_log_points = ci[2L],
    effect_pct = 100 * (exp(coef(fit)[predictor]) - 1),
    bootstrap_ci95_low_pct = 100 * (exp(ci[1L]) - 1),
    bootstrap_ci95_high_pct = 100 * (exp(ci[2L]) - 1),
    n = nrow(d),
    stringsAsFactors = FALSE
  )
}

bootstrap_results <- rbind(
  block_bootstrap_primary(panel, "L1_z_nhhi", "NHHI", seed = 62021L),
  block_bootstrap_primary(panel, "L1_z_top2", "Top-2 market share", seed = 62022L)
)

# Descriptive dependence and persistence diagnostics.
descriptive <- data.frame(
  item = c(
    "Pearson correlation: NHHI and Top-2 share",
    "Spearman correlation: NHHI and Top-2 share",
    "AR(1): log DPI",
    "AR(1): NHHI",
    "AR(1): Top-2 share",
    "DPI observations",
    "On-chain complete observations"
  ),
  value = c(
    cor(panel$nhhi, panel$top2_share, use = "complete.obs", method = "pearson"),
    cor(panel$nhhi, panel$top2_share, use = "complete.obs", method = "spearman"),
    cor(panel$log_dpi, panel$L1_log_dpi, use = "complete.obs"),
    cor(panel$nhhi, lag_vec(panel$nhhi), use = "complete.obs"),
    cor(panel$top2_share, lag_vec(panel$top2_share), use = "complete.obs"),
    nrow(panel),
    sum(complete.cases(panel[, micro_terms]))
  )
)

# A compact manuscript table contains only the pre-specified baseline and adjusted models.
main_specs <- c("Dynamic baseline", "Market-size adjusted", "Macro adjusted", "Adjusted primary")
main_table <- results[results$specification %in% main_specs, ]
main_table <- main_table[order(main_table$metric, match(main_table$specification,
                                                        main_specs)), ]

figure_specs <- c(
  "Adjusted primary", "Crypto FRM control", "30-day concentration lag", "30-day changes",
  "Complete 11-coin dates", "FTX/SVB windows excluded", "Additional on-chain controls"
)
figure_data <- results[results$specification %in% figure_specs, ]
figure_data$specification <- factor(figure_data$specification, levels = rev(figure_specs))
figure_data$metric <- factor(figure_data$metric, levels = c("NHHI", "Top-2 market share"))

write.csv(panel, file.path(bundle_dir, "source_data", "Section_6_2_analysis_panel.csv"), row.names = FALSE)
write.csv(results, file.path(bundle_dir, "tables", "Table_S6_2_All_Specifications.csv"), row.names = FALSE)
write.csv(main_table, file.path(bundle_dir, "tables", "Table_6_2_Primary_Models.csv"), row.names = FALSE)
write.csv(descriptive, file.path(bundle_dir, "tables", "Table_S6_2_Descriptive_Diagnostics.csv"), row.names = FALSE)
write.csv(bootstrap_results, file.path(bundle_dir, "tables", "Table_S6_2_Block_Bootstrap.csv"), row.names = FALSE)
write.csv(figure_data, file.path(bundle_dir, "figures", "Figure_6_2_source_data.csv"), row.names = FALSE)

input_manifest <- data.frame(
  input = c("Primary DPI", "Daily concentration", "Crypto FRM", "Macro-financial panel", "On-chain extension"),
  file = basename(input_files),
  md5 = unname(tools::md5sum(input_files)),
  stringsAsFactors = FALSE
)
write.csv(input_manifest, file.path(bundle_dir, "qa", "input_manifest_md5.csv"), row.names = FALSE)

checks <- data.frame(
  check = c(
    "dpi_dates_unique", "aligned_sample_size", "positive_dpi", "concentration_bounds",
    "primary_pair_present", "primary_holm_present", "all_hac_statistics_finite",
    "block_bootstrap_complete",
    "onchain_matched_sample", "figure_rows"
  ),
  status = c(
    !anyDuplicated(dpi$date),
    nrow(panel) == 2252L,
    all(panel$dpi_primary > 0),
    all(panel$nhhi >= 0 & panel$nhhi <= 1 & panel$top2_share >= 0 & panel$top2_share <= 1),
    sum(primary_i) == 2L,
    all(is.finite(results$p_holm_primary_pair[primary_i])),
    all(is.finite(results$estimate_log_points)) && all(is.finite(results$hac90_se)) && all(is.finite(results$p_value)),
    nrow(bootstrap_results) == 2L && all(is.finite(unlist(bootstrap_results[c("bootstrap_ci95_low_pct", "bootstrap_ci95_high_pct")]))),
    all(results$n[results$specification == "Additional on-chain controls"] == 1521L),
    nrow(figure_data) == 14L
  ),
  detail = c(
    paste(nrow(dpi), "unique DPI dates"),
    paste(nrow(panel), "aligned daily observations"),
    sprintf("DPI range %.4f to %.4f", min(panel$dpi_primary), max(panel$dpi_primary)),
    sprintf("NHHI %.3f-%.3f; Top-2 %.3f-%.3f", min(panel$nhhi), max(panel$nhhi), min(panel$top2_share), max(panel$top2_share)),
    paste(sum(primary_i), "primary concentration coefficients"),
    "Holm correction across NHHI and Top-2 primary coefficients",
    paste(nrow(results), "finite model estimates"),
    "999 moving-block residual-bootstrap replications per primary model; block length 90 days",
    paste(unique(results$n[results$specification == "Additional on-chain controls"]), collapse = ", "),
    paste(nrow(figure_data), "coefficient-plot rows")
  ),
  stringsAsFactors = FALSE
)
checks$status <- ifelse(checks$status, "PASS", "FAIL")
write.csv(checks, file.path(bundle_dir, "qa", "analysis_QA.csv"), row.names = FALSE)
if (any(checks$status == "FAIL")) stop("Section 6.2 QA failed; inspect qa/analysis_QA.csv")
writeLines(capture.output(sessionInfo()), file.path(bundle_dir, "qa", "session_info.txt"))

palette <- c("NHHI" = "#245B8A", "Top-2 market share" = "#C66A2B")
p <- ggplot(figure_data,
            aes(x = effect_pct, y = specification, colour = metric, shape = metric)) +
  geom_vline(xintercept = 0, colour = "#777777", linewidth = 0.35, linetype = "dashed") +
  geom_errorbar(aes(xmin = ci95_low_pct, xmax = ci95_high_pct),
                orientation = "y", position = position_dodge(width = 0.48),
                width = 0, linewidth = 0.55) +
  geom_point(position = position_dodge(width = 0.48), size = 2.0, stroke = 0.5) +
  scale_colour_manual(values = palette, name = NULL) +
  scale_shape_manual(values = c("NHHI" = 16, "Top-2 market share" = 17), name = NULL) +
  labs(x = "Change in DPI per 1-SD increase in concentration (%)", y = NULL) +
  theme_classic(base_size = 8, base_family = "Helvetica") +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(linewidth = 0.35, colour = "black"),
    axis.ticks.x = element_line(linewidth = 0.35, colour = "black"),
    axis.text = element_text(size = 7, colour = "black"),
    axis.title = element_text(size = 8, colour = "black"),
    legend.position = "top",
    legend.justification = "left",
    legend.key.width = grid::unit(8, "mm"),
    legend.text = element_text(size = 7),
    plot.margin = margin(4, 7, 4, 4)
  )

figure_base <- file.path(bundle_dir, "figures", "Figure_6_2_Concentration_and_Depegging_Pressure")
width_mm <- 160
height_mm <- 92
w <- width_mm / 25.4
h <- height_mm / 25.4

svglite::svglite(paste0(figure_base, ".svg"), width = w, height = h)
print(p)
dev.off()

cairo_ok <- tryCatch(
  withCallingHandlers({ grDevices::cairoVersion(); TRUE },
                       warning = function(warning_condition) stop(warning_condition)),
  error = function(error_condition) FALSE
)
if (cairo_ok) {
  grDevices::cairo_pdf(paste0(figure_base, ".pdf"), width = w, height = h, family = "Helvetica")
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

message("Section 6.2 outputs written to: ", bundle_dir)
