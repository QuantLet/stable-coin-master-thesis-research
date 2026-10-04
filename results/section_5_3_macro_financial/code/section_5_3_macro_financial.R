#!/usr/bin/env Rscript

# Section 5.3: Macro-Financial Conditions and Stablecoin Tail Risk
#
# This script analyses the five lagged macro-financial predictors in the frozen
# primary Quantile-Lasso coefficient panel. The calendar date is the sampling
# unit. Newey-West HAC inference uses a Bartlett kernel and lag 90 to reflect
# the 89-day overlap of adjacent rolling windows. Selection and coefficient
# weights are conditional penalised-regression summaries, not causal effects.

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

primary_path <- file.path(input_dir, "selected_coefficients_strict.csv.gz")
screened_path <- file.path(input_dir, "selected_coefficients_screened.csv.gz")
raw_path <- file.path(input_dir, "selected_coefficients_raw_strict.csv.gz")
dpi_path <- file.path(input_dir, "dpi_primary_robust_mad_strict.csv")
imputation_path <- file.path(input_dir, "output_window_imputation_flags.csv")
required_inputs <- c(primary_path, screened_path, raw_path, dpi_path, imputation_path)
if (!all(file.exists(required_inputs))) stop("One or more required inputs are missing.")

write_csv <- function(x, path) {
  write.csv(x, path, row.names = FALSE, quote = TRUE, na = "", fileEncoding = "UTF-8")
}

active_tolerance <- 1e-10
hac_lag <- 90L
coin_order <- c("USDT", "USDC", "BUSD", "TUSD", "USDP", "GUSD",
                "DAI", "sUSD", "EURS", "IDRT", "PAXG")
macro_order <- c("L1_CVIX.Index", "L1_BV010082.Index", "L1_DXY.Curncy",
                 "L1_SPX.Index", "L1_VIX.Index")
macro_labels <- c(
  `L1_CVIX.Index` = "CVIX",
  `L1_BV010082.Index` = "1-year Treasury yield",
  `L1_DXY.Curncy` = "DXY",
  `L1_SPX.Index` = "S&P 500",
  `L1_VIX.Index` = "VIX"
)

target_reference <- c(
  USDT = "USD", USDC = "USD", BUSD = "USD", TUSD = "USD",
  USDP = "USD", GUSD = "USD", DAI = "USD", sUSD = "USD",
  EURS = "EUR", IDRT = "IDR", PAXG = "Gold"
)

read_coefficients <- function(path) {
  x <- read.csv(gzfile(path), check.names = FALSE, stringsAsFactors = FALSE)
  needed <- c("date", "target", "predictor", "predictor_type", "coefficient")
  if (!all(needed %in% names(x))) stop("Coefficient columns are incomplete: ", basename(path))
  x$date <- as.Date(x$date)
  x$coefficient <- as.numeric(x$coefficient)
  if (anyNA(x[, needed]) || any(!is.finite(x$coefficient))) {
    stop("Coefficient panel contains missing or non-finite values: ", basename(path))
  }
  if (anyDuplicated(x[c("date", "target", "predictor")])) {
    stop("Duplicate date-target-predictor keys: ", basename(path))
  }
  x
}

primary_all <- read_coefficients(primary_path)
screened_all <- read_coefficients(screened_path)
raw_all <- read_coefficients(raw_path)

dates <- sort(unique(primary_all$date))
if (length(dates) != 2252L || min(dates) != as.Date("2020-04-01") ||
    max(dates) != as.Date("2026-05-31")) {
  stop("Primary date universe does not match the declared Chapter 4 system.")
}
if (!setequal(unique(primary_all$target), coin_order)) stop("Unexpected target universe.")
if (!setequal(unique(primary_all$predictor[primary_all$predictor_type == "lagged_macro"]),
              macro_order)) stop("Unexpected macro predictor universe.")

rows_per_model <- table(interaction(primary_all$date, primary_all$target, drop = TRUE))
if (!all(rows_per_model == 15L)) stop("Every target-date equation must contain 15 predictors.")

grid <- expand.grid(target = coin_order, predictor = macro_order,
                    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
grid$key <- paste(grid$target, grid$predictor, sep = "|")
cell_ids <- grid$key

prepare_macro <- function(all_x) {
  if (!identical(sort(unique(all_x$date)), dates)) stop("Sensitivity dates differ from primary dates.")
  x <- all_x[all_x$predictor_type == "lagged_macro", ]
  x$key <- paste(x$target, x$predictor, sep = "|")
  if (!setequal(unique(x$key), cell_ids)) stop("Incomplete target-factor universe.")
  x$active <- as.integer(abs(x$coefficient) > active_tolerance)
  x$positive <- as.integer(x$coefficient > active_tolerance)
  x$negative <- as.integer(x$coefficient < -active_tolerance)
  x
}

primary <- prepare_macro(primary_all)
screened <- prepare_macro(screened_all)
raw <- prepare_macro(raw_all)

to_matrix <- function(x, value, allow_na = FALSE) {
  out <- matrix(NA_real_, nrow = length(dates), ncol = length(cell_ids),
                dimnames = list(format(dates), cell_ids))
  out[cbind(match(x$date, dates), match(x$key, cell_ids))] <- x[[value]]
  if (!allow_na && anyNA(out)) stop("Incomplete target-factor matrix for ", value)
  out
}

active <- to_matrix(primary, "active")
positive <- to_matrix(primary, "positive")
negative <- to_matrix(primary, "negative")
coefficient <- to_matrix(primary, "coefficient")
screened_active <- to_matrix(screened, "active")
raw_active <- to_matrix(raw, "active")

# Absolute standardized coefficient shares. Window-local scaling makes these
# coefficients comparable within a target-date equation. The measure remains a
# penalised coefficient-mass summary and is not an ablation or causal effect.
equation_key_all <- paste(primary_all$date, primary_all$target, sep = "|")
equation_key_macro <- paste(primary$date, primary$target, sep = "|")
total_abs_by_equation <- tapply(abs(primary_all$coefficient), equation_key_all, sum)
macro_abs_by_equation <- tapply(abs(primary$coefficient), equation_key_macro, sum)
total_denom <- unname(total_abs_by_equation[equation_key_macro])
macro_denom <- unname(macro_abs_by_equation[equation_key_macro])
if (any(!is.finite(total_denom))) stop("Coefficient-mass denominators are incomplete.")
primary$total_abs_weight_share <- ifelse(total_denom > 0,
                                         abs(primary$coefficient) / total_denom,
                                         0)
primary$macro_abs_weight_share <- ifelse(macro_denom > 0,
                                         abs(primary$coefficient) / macro_denom,
                                         NA_real_)
total_weight_share <- to_matrix(primary, "total_abs_weight_share")
macro_weight_share <- to_matrix(primary, "macro_abs_weight_share", allow_na = TRUE)

hac_cov_mean <- function(X, lag = hac_lag) {
  X <- as.matrix(X)
  if (any(!is.finite(X))) stop("HAC input must be complete and finite.")
  n <- nrow(X)
  U <- sweep(X, 2L, colMeans(X), FUN = "-")
  lag <- min(as.integer(lag), n - 1L)
  omega <- crossprod(U) / n
  if (lag > 0L) {
    for (ell in seq_len(lag)) {
      weight <- 1 - ell / (lag + 1)
      gamma <- crossprod(U[(ell + 1L):n, , drop = FALSE],
                         U[seq_len(n - ell), , drop = FALSE]) / n
      omega <- omega + weight * (gamma + t(gamma))
    }
  }
  list(mean = colMeans(X), cov_mean = (omega + t(omega)) / (2 * n), n = n)
}

hac_mean <- function(x, lag = hac_lag) {
  fit <- hac_cov_mean(matrix(as.numeric(x), ncol = 1L), lag = lag)
  mu <- unname(fit$mean[1L])
  se <- sqrt(max(fit$cov_mean[1L, 1L], 0))
  z <- if (se > 0) mu / se else if (mu == 0) 0 else sign(mu) * Inf
  data.frame(estimate = mu, hac90_se = se,
             ci_low = mu - 1.96 * se, ci_high = mu + 1.96 * se,
             z = z, p_value = 2 * pnorm(-abs(z)), stringsAsFactors = FALSE)
}

psd_inverse <- function(V, tolerance = 1e-10) {
  V <- (V + t(V)) / 2
  eig <- eigen(V, symmetric = TRUE)
  cutoff <- max(abs(eig$values)) * tolerance
  keep <- eig$values > cutoff
  if (!any(keep)) stop("Wald covariance has no positive eigenvalues.")
  inv <- eig$vectors[, keep, drop = FALSE] %*%
    diag(1 / eig$values[keep], nrow = sum(keep)) %*%
    t(eig$vectors[, keep, drop = FALSE])
  list(inverse = inv, rank = sum(keep), min_eigenvalue = min(eig$values))
}

wald_test <- function(mu, V, R) {
  delta <- as.numeric(R %*% mu)
  VR <- R %*% V %*% t(R)
  inv <- psd_inverse(VR)
  statistic <- as.numeric(t(delta) %*% inv$inverse %*% delta)
  data.frame(statistic = statistic, df = inv$rank,
             p_value = pchisq(statistic, df = inv$rank, lower.tail = FALSE),
             minimum_covariance_eigenvalue = inv$min_eigenvalue,
             stringsAsFactors = FALSE)
}

factor_columns <- lapply(macro_order, function(m) which(grid$predictor == m))
names(factor_columns) <- macro_order
target_columns <- lapply(coin_order, function(s) which(grid$target == s))
names(target_columns) <- coin_order

daily_factor <- function(M, na_rm = FALSE) {
  out <- vapply(factor_columns, function(cols) rowMeans(M[, cols, drop = FALSE],
                                                        na.rm = na_rm),
                numeric(nrow(M)))
  colnames(out) <- macro_order
  out
}

daily_active_factor <- daily_factor(active)
daily_positive_factor <- daily_factor(positive)
daily_negative_factor <- daily_factor(negative)
daily_total_weight_factor <- daily_factor(total_weight_share)
daily_macro_weight_factor <- daily_factor(macro_weight_share, na_rm = TRUE)
if (any(!is.finite(daily_macro_weight_factor))) {
  stop("At least one date has no selected macro coefficient in any target equation.")
}

factor_summary <- do.call(rbind, lapply(macro_order, function(m) {
  sel <- hac_mean(daily_active_factor[, m])
  sign_balance <- hac_mean(daily_positive_factor[, m] - daily_negative_factor[, m])
  total_share <- hac_mean(daily_total_weight_factor[, m])
  macro_share <- hac_mean(daily_macro_weight_factor[, m])
  data.frame(
    predictor = m,
    factor = unname(macro_labels[m]),
    selection_frequency = sel$estimate,
    selection_hac90_se = sel$hac90_se,
    selection_ci_low = sel$ci_low,
    selection_ci_high = sel$ci_high,
    positive_frequency = mean(daily_positive_factor[, m]),
    negative_frequency = mean(daily_negative_factor[, m]),
    positive_share_among_active = mean(daily_positive_factor[, m]) /
      mean(daily_active_factor[, m]),
    sign_balance = sign_balance$estimate,
    sign_balance_hac90_se = sign_balance$hac90_se,
    sign_balance_ci_low = sign_balance$ci_low,
    sign_balance_ci_high = sign_balance$ci_high,
    sign_balance_p_value = sign_balance$p_value,
    total_abs_weight_share = total_share$estimate,
    total_abs_weight_ci_low = total_share$ci_low,
    total_abs_weight_ci_high = total_share$ci_high,
    macro_abs_weight_share = macro_share$estimate,
    macro_abs_weight_ci_low = macro_share$ci_low,
    macro_abs_weight_ci_high = macro_share$ci_high,
    stringsAsFactors = FALSE
  )
}))
factor_summary$sign_balance_p_holm_5 <- p.adjust(factor_summary$sign_balance_p_value,
                                                 method = "holm")
factor_summary$stress_aligned_share_among_active <- NA_real_
factor_summary$stress_alignment_rule <- "not assigned"
for (m in c("L1_CVIX.Index", "L1_VIX.Index")) {
  i <- factor_summary$predictor == m
  factor_summary$stress_aligned_share_among_active[i] <-
    factor_summary$negative_frequency[i] / factor_summary$selection_frequency[i]
  factor_summary$stress_alignment_rule[i] <- "negative coefficient"
}
i <- factor_summary$predictor == "L1_SPX.Index"
factor_summary$stress_aligned_share_among_active[i] <-
  factor_summary$positive_frequency[i] / factor_summary$selection_frequency[i]
factor_summary$stress_alignment_rule[i] <- "positive coefficient"
write_csv(factor_summary, file.path(table_dir, "Table_5_3_Macro_Factor_Summary.csv"))

# Equality of mean selection frequencies across the five factors.
factor_hac <- hac_cov_mean(daily_active_factor)
R_factor <- cbind(-1, diag(4L))
global_factor_equality <- wald_test(factor_hac$mean, factor_hac$cov_mean, R_factor)
global_factor_equality$test <- "equality of the five macro-factor selection frequencies"
write_csv(global_factor_equality,
          file.path(table_dir, "Table_S5_12_Global_Factor_Equality_HAC90.csv"))

# Ten paired factor contrasts, adjusted as one prespecified family.
factor_pairs <- t(combn(macro_order, 2L))
pairwise_factors <- do.call(rbind, lapply(seq_len(nrow(factor_pairs)), function(i) {
  a <- factor_pairs[i, 1L]
  b <- factor_pairs[i, 2L]
  h <- hac_mean(daily_active_factor[, a] - daily_active_factor[, b])
  data.frame(factor_1 = unname(macro_labels[a]), factor_2 = unname(macro_labels[b]),
             difference = h$estimate, hac90_se = h$hac90_se,
             ci_low = h$ci_low, ci_high = h$ci_high,
             z = h$z, p_value = h$p_value, stringsAsFactors = FALSE)
}))
pairwise_factors$p_holm_10 <- p.adjust(pairwise_factors$p_value, method = "holm")
pairwise_factors <- pairwise_factors[order(pairwise_factors$p_holm_10), ]
write_csv(pairwise_factors,
          file.path(table_dir, "Table_S5_13_Pairwise_Factor_Selection_HAC90_Holm.csv"))

# Factor-specific target heterogeneity. Each test compares the 11 target means;
# Holm adjustment is applied across the five macro factors.
target_heterogeneity <- do.call(rbind, lapply(macro_order, function(m) {
  cols <- factor_columns[[m]]
  X <- active[, cols, drop = FALSE]
  colnames(X) <- grid$target[cols]
  fit <- hac_cov_mean(X)
  R_target <- cbind(-1, diag(length(coin_order) - 1L))
  w <- wald_test(fit$mean, fit$cov_mean, R_target)
  data.frame(predictor = m, factor = unname(macro_labels[m]),
             statistic = w$statistic, df = w$df, p_value = w$p_value,
             minimum_covariance_eigenvalue = w$minimum_covariance_eigenvalue,
             max_minus_min_selection_frequency = max(fit$mean) - min(fit$mean),
             stringsAsFactors = FALSE)
}))
target_heterogeneity$p_holm_5 <- p.adjust(target_heterogeneity$p_value, method = "holm")
write_csv(target_heterogeneity,
          file.path(table_dir, "Table_S5_14_Target_Heterogeneity_HAC90_Holm.csv"))

# Full target-factor source data for Figure 5.3.
cell_summary <- data.frame(
  target = grid$target,
  reference_asset = unname(target_reference[grid$target]),
  predictor = grid$predictor,
  factor = unname(macro_labels[grid$predictor]),
  selection_frequency = colMeans(active),
  positive_frequency = colMeans(positive),
  negative_frequency = colMeans(negative),
  sign_balance = colMeans(positive - negative),
  total_abs_weight_share = colMeans(total_weight_share),
  macro_abs_weight_share = colMeans(macro_weight_share, na.rm = TRUE),
  stringsAsFactors = FALSE
)
write_csv(cell_summary, file.path(source_dir, "Figure_5_3_Macro_Financial_Selection.csv"))

# Selection frequencies by DPI state are descriptive because both objects come
# from the same rolling system.
dpi <- read.csv(dpi_path, stringsAsFactors = FALSE)
dpi$date <- as.Date(dpi$date)
dpi <- dpi[match(dates, dpi$date), ]
if (anyNA(dpi$date)) stop("DPI dates do not match coefficient dates.")
state_breaks <- as.numeric(quantile(dpi$dpi_primary, probs = seq(0, 1, 0.2), type = 8))
states <- cut(dpi$dpi_primary, breaks = c(-Inf, state_breaks[2:5], Inf),
              labels = c("Very low", "Low", "Moderate", "Elevated", "Severe"),
              right = TRUE, ordered_result = TRUE)
state_table <- do.call(rbind, lapply(levels(states), function(s) {
  data.frame(state = s,
             factor = unname(macro_labels[macro_order]),
             selection_frequency = colMeans(daily_active_factor[states == s, , drop = FALSE]),
             days = sum(states == s), stringsAsFactors = FALSE)
}))
write_csv(state_table, file.path(table_dir, "Table_S5_15_Macro_Selection_by_DPI_State.csv"))

# Reference-aware DXY summary. EUR, IDR and Gold groups each contain one target,
# so this output is descriptive and not used for population-level inference.
dxy_cells <- cell_summary[cell_summary$predictor == "L1_DXY.Curncy", ]
dxy_reference <- aggregate(cbind(selection_frequency, positive_frequency,
                                 negative_frequency, sign_balance) ~ reference_asset,
                           data = dxy_cells, FUN = mean)
dxy_reference$number_of_targets <- as.integer(table(dxy_cells$reference_asset)[
  dxy_reference$reference_asset])
write_csv(dxy_reference, file.path(table_dir, "Table_S5_16_DXY_by_Reference_Asset.csv"))

# Robustness: target-factor rank stability across selection rules, scaling,
# past-fill exclusion, and chronological halves.
imputation <- read.csv(imputation_path, stringsAsFactors = FALSE)
imputation$date <- as.Date(imputation$date)
imputation <- imputation[match(dates, imputation$date), ]
if (anyNA(imputation$date)) stop("Imputation flags do not match coefficient dates.")
clean_dates <- imputation$imputed_cells_in_window == 0
half_cut <- floor(length(dates) / 2L)

ranking_comparison <- function(reference_values, alternative_values,
                               comparison, top_n = 10L) {
  r1 <- rank(-reference_values, ties.method = "average")
  r2 <- rank(-alternative_values, ties.method = "average")
  top1 <- order(-reference_values)[seq_len(top_n)]
  top2 <- order(-alternative_values)[seq_len(top_n)]
  data.frame(
    comparison = comparison,
    spearman_rho = cor(reference_values, alternative_values, method = "spearman"),
    top_10_overlap = length(intersect(top1, top2)),
    cells = length(reference_values),
    stringsAsFactors = FALSE
  )
}

primary_frequency <- colMeans(active)
robustness <- rbind(
  ranking_comparison(primary_frequency, colMeans(screened_active),
                     "strict minimum GACV vs screened GACV"),
  ranking_comparison(primary_frequency, colMeans(raw_active),
                     "robust-scaled vs unscaled strict"),
  ranking_comparison(primary_frequency, colMeans(active[clean_dates, , drop = FALSE]),
                     "all dates vs dates without past-filled deviation cells"),
  ranking_comparison(colMeans(active[seq_len(half_cut), , drop = FALSE]),
                     colMeans(active[(half_cut + 1L):length(dates), , drop = FALSE]),
                     "first chronological half vs second chronological half")
)
write_csv(robustness, file.path(table_dir, "Table_S5_17_Macro_Rank_Robustness.csv"))

# Figure 5.3: one unspliced heatmap. No smoothing or cell selection.
heat <- matrix(cell_summary$selection_frequency,
               nrow = length(coin_order), ncol = length(macro_order),
               dimnames = list(coin_order, unname(macro_labels[macro_order])))
heat_colours <- grDevices::colorRampPalette(
  c("#F5F7F7", "#DCE8EA", "#AFCFD5", "#6FA8B6", "#2D738A", "#0D425E")
)(121)
zlim <- c(0.45, 0.85)
width_mm <- 183
height_mm <- 112

draw_figure <- function() {
  layout(matrix(c(1, 2), nrow = 1L), widths = c(8.2, 1.05))
  par(family = "Helvetica", mar = c(1.1, 5.0, 3.0, 0.7), mgp = c(1.8, 0.45, 0),
      tcl = -0.18, xaxs = "i", yaxs = "i", bty = "n")
  row_order <- rev(seq_along(coin_order))
  z <- t(heat[row_order, , drop = FALSE])
  image(seq_along(macro_order), seq_along(coin_order), z,
        col = heat_colours, zlim = zlim, axes = FALSE, xlab = "", ylab = "")
  figure_labels <- c("CVIX", "1-year yield", "DXY", "S&P 500", "VIX")
  axis(3, at = seq_along(macro_order), labels = figure_labels,
       las = 1, tick = FALSE, cex.axis = 0.72, line = -0.15)
  axis(2, at = seq_along(coin_order), labels = rownames(heat)[row_order],
       las = 1, tick = FALSE, cex.axis = 0.78)
  abline(v = seq(0.5, length(macro_order) + 0.5, by = 1), col = "white", lwd = 0.8)
  abline(h = seq(0.5, length(coin_order) + 0.5, by = 1), col = "white", lwd = 0.8)
  for (ix in seq_along(macro_order)) {
    for (iy in seq_along(coin_order)) {
      value <- z[ix, iy]
      text(ix, iy, sprintf("%.1f", 100 * value),
           cex = 0.67, col = if (value >= 0.70) "white" else "#1D2A30")
    }
  }
  box(lwd = 0.45)
  mtext("Stablecoin target", side = 2, line = 3.7, cex = 0.82)

  par(family = "Helvetica", mar = c(1.1, 0.6, 3.0, 3.0), xaxs = "i", yaxs = "i")
  legend_values <- seq(zlim[1], zlim[2], length.out = length(heat_colours))
  image(1, legend_values, matrix(legend_values, nrow = 1L),
        col = heat_colours, zlim = zlim, axes = FALSE, xlab = "", ylab = "")
  tick_values <- seq(0.45, 0.85, by = 0.10)
  axis(4, at = tick_values, labels = sprintf("%d", round(100 * tick_values)),
       las = 1, tcl = -0.25, cex.axis = 0.70)
  mtext("Selection frequency (%)", side = 4, line = 2.0, cex = 0.75)
}

base_name <- file.path(figure_dir, "Figure_5_3_Macro_Financial_Selection")
svglite::svglite(paste0(base_name, ".svg"), width = width_mm / 25.4,
                 height = height_mm / 25.4)
draw_figure()
dev.off()
grDevices::pdf(paste0(base_name, ".pdf"), width = width_mm / 25.4,
               height = height_mm / 25.4, family = "Helvetica",
               useDingbats = FALSE, paper = "special")
draw_figure()
dev.off()
ragg::agg_tiff(paste0(base_name, ".tiff"), width = width_mm,
               height = height_mm, units = "mm", res = 600,
               background = "white")
draw_figure()
dev.off()
ragg::agg_png(paste0(base_name, ".png"), width = width_mm,
              height = height_mm, units = "mm", res = 300,
              background = "white")
draw_figure()
dev.off()

caption <- paste(
  "Figure 5.3 | Macro-financial selection across stablecoins.",
  "Cells report the percentage of 2,252 rolling windows in which each one-calendar-day-lagged macro-financial predictor has an absolute Quantile-Lasso coefficient above 10^-10 for the indicated target stablecoin.",
  "The estimates use the primary 90-day, 5% conditional-quantile specification with window-local median/MAD scaling and strict minimum finite GACV.",
  "All 11 targets and five factors are shown; no smoothing or post-hoc cell selection is applied.",
  "Selection frequency measures conditional inclusion, not causal transmission or incremental predictive contribution."
)
writeLines(caption, file.path(figure_dir, "Figure_5_3_caption.txt"), useBytes = TRUE)

# Compact manuscript table in Markdown.
fmt_pct <- function(x, digits = 1L) sprintf(paste0("%.", digits, "f"), 100 * x)
table_lines <- c(
  "**Table 5.3 | Macro-financial selection, sign and standardized coefficient weight.**",
  "",
  "| Factor | Selection, % (HAC 95% CI) | Positive / negative, % | Stress-aligned among active, % | Absolute coefficient weight in macro block, % | Absolute coefficient weight in full model, % |",
  "|---|---:|---:|---:|---:|---:|"
)
for (i in seq_len(nrow(factor_summary))) {
  stress <- if (is.na(factor_summary$stress_aligned_share_among_active[i])) "n.a." else
    fmt_pct(factor_summary$stress_aligned_share_among_active[i])
  table_lines <- c(table_lines, sprintf(
    "| %s | %s (%s-%s) | %s / %s | %s | %s | %s |",
    factor_summary$factor[i],
    fmt_pct(factor_summary$selection_frequency[i]),
    fmt_pct(factor_summary$selection_ci_low[i]),
    fmt_pct(factor_summary$selection_ci_high[i]),
    fmt_pct(factor_summary$positive_frequency[i]),
    fmt_pct(factor_summary$negative_frequency[i]),
    stress,
    fmt_pct(factor_summary$macro_abs_weight_share[i]),
    fmt_pct(factor_summary$total_abs_weight_share[i])
  ))
}
table_lines <- c(
  table_lines, "",
  "Notes: Positive and negative frequencies are fractions of all eligible target-window equations. For VIX and CVIX, a negative coefficient is classified as stress-aligned; for the S&P 500, a positive coefficient is stress-aligned because an equity decline then lowers the conditional tail. No universal stress direction is assigned to DXY or the one-year Treasury yield. Absolute coefficient weights use window-standardized coefficients and are descriptive; they do not equal incremental predictive content. HAC intervals use a Bartlett kernel with lag 90 and calendar date as the sampling unit."
)
writeLines(table_lines, file.path(table_dir, "Table_5_3_Manuscript_Ready.md"), useBytes = TRUE)

# QA and provenance.
input_manifest <- data.frame(
  file = basename(required_inputs),
  bytes = file.info(required_inputs)$size,
  md5 = unname(tools::md5sum(required_inputs)),
  stringsAsFactors = FALSE
)
write_csv(input_manifest, file.path(qa_dir, "input_manifest_md5.csv"))

qa <- data.frame(
  check = c("primary_dates", "target_count", "macro_factor_count",
            "target_factor_cells", "macro_coefficient_rows", "all_model_rows",
            "past_filled_windows", "zero_total_mass_equations",
            "zero_macro_mass_equations",
            "factor_selection_sum_check", "macro_weight_sum_check"),
  value = c(length(dates), length(unique(primary$target)),
            length(unique(primary$predictor)), ncol(active), nrow(primary),
            nrow(primary_all), sum(!clean_dates), sum(total_abs_by_equation <= 0),
            sum(macro_abs_by_equation <= 0),
            max(abs(colMeans(active) - cell_summary$selection_frequency)),
            abs(sum(factor_summary$macro_abs_weight_share) - 1)),
  expected = c("2252", "11", "5", "55", "123860", "371580", "539", "37", "263",
               "0", "0"),
  pass = c(length(dates) == 2252L, length(unique(primary$target)) == 11L,
           length(unique(primary$predictor)) == 5L, ncol(active) == 55L,
           nrow(primary) == 123860L, nrow(primary_all) == 371580L,
           sum(!clean_dates) == 539L, sum(total_abs_by_equation <= 0) == 37L,
           sum(macro_abs_by_equation <= 0) == 263L,
           max(abs(colMeans(active) - cell_summary$selection_frequency)) < 1e-12,
           abs(sum(factor_summary$macro_abs_weight_share) - 1) < 1e-12),
  stringsAsFactors = FALSE
)
write_csv(qa, file.path(qa_dir, "analysis_QA.csv"))
if (!all(qa$pass)) stop("At least one analysis QA check failed.")

summary_lines <- c(
  sprintf("Sample: %d dates, %d targets, %d macro factors, %d target-factor coefficients.",
          length(dates), length(coin_order), length(macro_order), nrow(primary)),
  sprintf("Global equality of macro selection frequencies: HAC Wald chi-square(%d) = %.3f, p = %.8g.",
          global_factor_equality$df, global_factor_equality$statistic,
          global_factor_equality$p_value),
  sprintf("Macro block share of total absolute standardized coefficient mass: %.4f.",
          sum(factor_summary$total_abs_weight_share)),
  sprintf("Chronological half-sample target-factor rank correlation: %.4f; top-10 overlap: %d.",
          robustness$spearman_rho[4L], robustness$top_10_overlap[4L]),
  sprintf("Strict versus screened target-factor rank correlation: %.4f.",
          robustness$spearman_rho[1L]),
  sprintf("Robust-scaled versus unscaled target-factor rank correlation: %.4f.",
          robustness$spearman_rho[2L])
)
writeLines(summary_lines, file.path(qa_dir, "analysis_summary.txt"), useBytes = TRUE)
capture.output(sessionInfo(), file = file.path(qa_dir, "session_info.txt"))

message("Section 5.3 analysis complete: ", bundle_dir)
