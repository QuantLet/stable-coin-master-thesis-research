#!/usr/bin/env Rscript

# Section 5.2 inference and robustness
#
# The primary object is the signed coefficient panel from the reference-adjusted,
# robust-MAD, strict minimum-GACV Quantile-Lasso system. Dependence-adjusted
# uncertainty treats calendar dates as the sampling dimension and preserves all
# 110 directed pairs jointly. HAC lag and moving-block length are both 90 days,
# matching the overlap of the primary rolling estimator.

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
qa_dir <- file.path(bundle_dir, "qa")
invisible(lapply(c(source_dir, table_dir, qa_dir), dir.create,
                 recursive = TRUE, showWarnings = FALSE))

primary_path <- file.path(input_dir, "selected_coefficients_strict.csv.gz")
screened_path <- file.path(input_dir, "selected_coefficients_screened.csv.gz")
raw_path <- file.path(input_dir, "selected_coefficients_raw_strict.csv.gz")
dpi_path <- file.path(input_dir, "dpi_primary_robust_mad_strict.csv")
imputation_path <- file.path(input_dir, "output_window_imputation_flags.csv")
required_inputs <- c(primary_path, screened_path, raw_path, dpi_path, imputation_path)
if (!all(file.exists(required_inputs))) stop("One or more robustness inputs are missing.")

write_csv <- function(x, path) {
  write.csv(x, path, row.names = FALSE, quote = TRUE, na = "", fileEncoding = "UTF-8")
}

coin_order <- c("USDT", "USDC", "BUSD", "TUSD", "USDP", "GUSD",
                "DAI", "sUSD", "EURS", "IDRT", "PAXG")
active_tolerance <- 1e-10
hac_lag <- 90L
block_length <- 90L
bootstrap_repetitions <- 1999L
bootstrap_seed <- 20260908L

coin_attributes <- data.frame(
  coin = coin_order,
  design = c(rep("Fiat-reserve-backed", 6),
             rep("Crypto-collateralised", 2),
             rep("Fiat-reserve-backed", 2),
             "Real-asset-backed"),
  reference = c(rep("USD", 8), "EUR", "IDR", "Gold"),
  stringsAsFactors = FALSE
)

pair_map <- do.call(rbind, lapply(coin_order, function(source) {
  data.frame(source = source, receiver = coin_order[coin_order != source],
             stringsAsFactors = FALSE)
}))
pair_map$pair <- paste(pair_map$source, pair_map$receiver, sep = "->")
pair_map$source_design <- coin_attributes$design[match(pair_map$source, coin_attributes$coin)]
pair_map$receiver_design <- coin_attributes$design[match(pair_map$receiver, coin_attributes$coin)]
pair_map$source_reference <- coin_attributes$reference[match(pair_map$source, coin_attributes$coin)]
pair_map$receiver_reference <- coin_attributes$reference[match(pair_map$receiver, coin_attributes$coin)]
pair_map$same_design <- pair_map$source_design == pair_map$receiver_design
pair_map$same_reference <- pair_map$source_reference == pair_map$receiver_reference
pair_ids <- pair_map$pair

read_links <- function(path) {
  x <- read.csv(gzfile(path), check.names = FALSE, stringsAsFactors = FALSE)
  need <- c("date", "target", "predictor", "predictor_type", "coefficient")
  if (!all(need %in% names(x))) stop("Coefficient input has incomplete columns: ", basename(path))
  x <- x[x$predictor_type == "stablecoin", need]
  x$date <- as.Date(x$date)
  x$coefficient <- as.numeric(x$coefficient)
  names(x)[names(x) == "predictor"] <- "source"
  names(x)[names(x) == "target"] <- "receiver"
  x$pair <- paste(x$source, x$receiver, sep = "->")
  x$active <- as.integer(abs(x$coefficient) > active_tolerance)
  x$aligned <- as.integer(x$coefficient > active_tolerance)
  x$inverse <- as.integer(x$coefficient < -active_tolerance)
  if (anyNA(x[, c("date", "source", "receiver", "coefficient")])) {
    stop("Missing coefficient fields: ", basename(path))
  }
  if (anyDuplicated(x[c("date", "pair")])) stop("Duplicate pair-date keys: ", basename(path))
  if (!setequal(unique(x$pair), pair_ids)) stop("Unexpected directed pair universe: ", basename(path))
  x
}

primary <- read_links(primary_path)
screened <- read_links(screened_path)
raw <- read_links(raw_path)
dates <- sort(unique(primary$date))

to_matrix <- function(x, value) {
  if (!identical(sort(unique(x$date)), dates)) stop("Specification dates do not match primary dates.")
  out <- matrix(NA_real_, nrow = length(dates), ncol = length(pair_ids),
                dimnames = list(format(dates), pair_ids))
  i <- match(x$date, dates)
  j <- match(x$pair, pair_ids)
  out[cbind(i, j)] <- x[[value]]
  if (anyNA(out)) stop("Incomplete pair-date matrix for ", value)
  out
}

primary_active <- to_matrix(primary, "active")
primary_aligned <- to_matrix(primary, "aligned")
primary_inverse <- to_matrix(primary, "inverse")
screened_active <- to_matrix(screened, "active")
screened_aligned <- to_matrix(screened, "aligned")
screened_inverse <- to_matrix(screened, "inverse")
raw_active <- to_matrix(raw, "active")
raw_aligned <- to_matrix(raw, "aligned")
raw_inverse <- to_matrix(raw, "inverse")

dpi <- read.csv(dpi_path, stringsAsFactors = FALSE)
dpi$date <- as.Date(dpi$date)
dpi <- dpi[match(dates, dpi$date), ]
if (anyNA(dpi$date)) stop("DPI dates do not match coefficient dates.")
state_breaks <- as.numeric(quantile(dpi$dpi_primary, probs = seq(0, 1, 0.2), type = 8))
dpi_state <- cut(dpi$dpi_primary, breaks = c(-Inf, state_breaks[2:5], Inf),
                 labels = c("Very low", "Low", "Moderate", "Elevated", "Severe"),
                 right = TRUE, ordered_result = TRUE)

hac_mean <- function(x, lag = hac_lag) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3L) return(rep(NA_real_, 6L))
  mu <- mean(x)
  u <- x - mu
  lag <- min(as.integer(lag), n - 1L)
  lrv <- sum(u * u) / n
  if (lag > 0L) {
    for (ell in seq_len(lag)) {
      weight <- 1 - ell / (lag + 1)
      gamma <- sum(u[(ell + 1L):n] * u[seq_len(n - ell)]) / n
      lrv <- lrv + 2 * weight * gamma
    }
  }
  se <- sqrt(max(lrv, 0) / n)
  z <- if (se > 0) mu / se else if (mu == 0) 0 else sign(mu) * Inf
  p <- 2 * stats::pnorm(-abs(z))
  c(estimate = mu, se = se, ci_low = mu - 1.96 * se,
    ci_high = mu + 1.96 * se, z = z, p_value = p)
}

circular_block_indices <- function(n, block) {
  starts <- sample.int(n, ceiling(n / block), replace = TRUE)
  idx <- unlist(lapply(starts, function(s) ((s - 1L + seq_len(block) - 1L) %% n) + 1L),
                use.names = FALSE)
  idx[seq_len(n)]
}

# Moving-block bootstrap CIs preserve all directed pairs jointly by resampling dates.
set.seed(bootstrap_seed)
boot_aligned <- matrix(NA_real_, nrow = bootstrap_repetitions, ncol = length(pair_ids))
boot_overall <- matrix(NA_real_, nrow = bootstrap_repetitions, ncol = 3L,
                       dimnames = list(NULL, c("active", "aligned", "inverse")))
boot_group <- matrix(NA_real_, nrow = bootstrap_repetitions, ncol = 2L,
                     dimnames = list(NULL, c("same_minus_different_reference",
                                             "within_minus_across_design")))
boot_state <- rep(NA_real_, bootstrap_repetitions)

daily_aligned <- rowMeans(primary_aligned)
same_reference_cols <- which(pair_map$same_reference)
different_reference_cols <- which(!pair_map$same_reference)
within_design_cols <- which(pair_map$same_design)
across_design_cols <- which(!pair_map$same_design)

for (b in seq_len(bootstrap_repetitions)) {
  idx <- circular_block_indices(length(dates), block_length)
  boot_aligned[b, ] <- colMeans(primary_aligned[idx, , drop = FALSE])
  boot_overall[b, ] <- c(mean(primary_active[idx, ]),
                         mean(primary_aligned[idx, ]),
                         mean(primary_inverse[idx, ]))
  boot_group[b, 1] <- mean(primary_aligned[idx, same_reference_cols]) -
    mean(primary_aligned[idx, different_reference_cols])
  boot_group[b, 2] <- mean(primary_aligned[idx, within_design_cols]) -
    mean(primary_aligned[idx, across_design_cols])
  sampled_states <- dpi_state[idx]
  if (any(sampled_states == "Severe") && any(sampled_states == "Very low")) {
    boot_state[b] <- mean(daily_aligned[idx][sampled_states == "Severe"]) -
      mean(daily_aligned[idx][sampled_states == "Very low"])
  }
}

aligned_frequency <- colMeans(primary_aligned)
aligned_ci <- t(apply(boot_aligned, 2L, stats::quantile,
                      probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE, type = 8))
pair_uncertainty <- cbind(pair_map,
                          aligned_frequency = aligned_frequency,
                          aligned_ci_low = aligned_ci[, 1],
                          aligned_ci_high = aligned_ci[, 2],
                          inverse_frequency = colMeans(primary_inverse),
                          active_frequency = colMeans(primary_active))
pair_uncertainty <- pair_uncertainty[order(-pair_uncertainty$aligned_frequency,
                                           pair_uncertainty$source,
                                           pair_uncertainty$receiver), ]
pair_uncertainty$rank <- seq_len(nrow(pair_uncertainty))
write_csv(pair_uncertainty,
          file.path(source_dir, "Directed_Pair_Link_Metrics_with_Block_CI.csv"))

# Directional asymmetry: HAC(90) tests with Holm adjustment across all 55 dyads.
dyads <- t(combn(coin_order, 2L))
direction_tests <- do.call(rbind, lapply(seq_len(nrow(dyads)), function(k) {
  a <- dyads[k, 1]
  b <- dyads[k, 2]
  ab <- primary_aligned[, match(paste(a, b, sep = "->"), pair_ids)]
  ba <- primary_aligned[, match(paste(b, a, sep = "->"), pair_ids)]
  h <- hac_mean(ab - ba)
  data.frame(
    coin_1 = a,
    coin_2 = b,
    coin_1_to_2 = mean(ab),
    coin_2_to_1 = mean(ba),
    directional_difference = unname(h["estimate"]),
    hac90_se = unname(h["se"]),
    ci_low = unname(h["ci_low"]),
    ci_high = unname(h["ci_high"]),
    z = unname(h["z"]),
    p_value = unname(h["p_value"]),
    stringsAsFactors = FALSE
  )
}))
direction_tests$p_holm_55 <- p.adjust(direction_tests$p_value, method = "holm")
direction_tests$larger_direction <- ifelse(
  direction_tests$directional_difference >= 0,
  paste(direction_tests$coin_1, direction_tests$coin_2, sep = "->"),
  paste(direction_tests$coin_2, direction_tests$coin_1, sep = "->")
)
direction_tests$absolute_difference <- abs(direction_tests$directional_difference)
direction_tests <- direction_tests[order(-direction_tests$absolute_difference), ]
write_csv(direction_tests,
          file.path(table_dir, "Table_S5_6_Directional_Asymmetry_HAC90_Holm.csv"))

# Main table joins dependence-adjusted persistence intervals and full-family asymmetry tests.
top <- head(pair_uncertainty, 8L)
top$reverse_aligned_frequency <- vapply(seq_len(nrow(top)), function(i) {
  aligned_frequency[match(paste(top$receiver[i], top$source[i], sep = "->"), pair_ids)]
}, numeric(1))
top$directional_difference <- top$aligned_frequency - top$reverse_aligned_frequency
top$p_holm_55 <- vapply(seq_len(nrow(top)), function(i) {
  z <- direction_tests[
    (direction_tests$coin_1 == top$source[i] & direction_tests$coin_2 == top$receiver[i]) |
      (direction_tests$coin_1 == top$receiver[i] & direction_tests$coin_2 == top$source[i]),
    "p_holm_55"
  ]
  if (length(z) != 1L) stop("Dyad inference lookup failed.")
  z
}, numeric(1))
main_table <- top[, c("rank", "source", "receiver", "aligned_frequency",
                      "aligned_ci_low", "aligned_ci_high", "inverse_frequency",
                      "reverse_aligned_frequency", "directional_difference", "p_holm_55")]
write_csv(main_table,
          file.path(table_dir, "Table_5_2_Persistent_Links_with_Uncertainty.csv"))

# Daily network-density HAC summaries.
density_series <- list(
  active = rowMeans(primary_active),
  aligned = rowMeans(primary_aligned),
  inverse = rowMeans(primary_inverse)
)
density_hac <- do.call(rbind, lapply(names(density_series), function(nm) {
  h <- hac_mean(density_series[[nm]])
  data.frame(metric = nm, estimate = unname(h["estimate"]),
             hac90_se = unname(h["se"]), ci_low = unname(h["ci_low"]),
             ci_high = unname(h["ci_high"]), stringsAsFactors = FALSE)
}))
write_csv(density_hac, file.path(table_dir, "Table_S5_9_Network_Density_HAC90.csv"))

# Primary system-level contrast: are downside-aligned selections more frequent
# than inverse selections? The unit is one cross-sectional difference per date.
systemwide_alignment_series <- density_series$aligned - density_series$inverse
systemwide_alignment_hac <- hac_mean(systemwide_alignment_series)
systemwide_alignment <- data.frame(
  contrast = "downside-aligned minus inverse selection frequency",
  aligned_frequency = mean(density_series$aligned),
  inverse_frequency = mean(density_series$inverse),
  difference = unname(systemwide_alignment_hac["estimate"]),
  hac90_se = unname(systemwide_alignment_hac["se"]),
  ci_low = unname(systemwide_alignment_hac["ci_low"]),
  ci_high = unname(systemwide_alignment_hac["ci_high"]),
  z = unname(systemwide_alignment_hac["z"]),
  p_value = unname(systemwide_alignment_hac["p_value"]),
  block_ci_low = unname(stats::quantile(
    boot_overall[, "aligned"] - boot_overall[, "inverse"],
    probs = 0.025, names = FALSE, type = 8)),
  block_ci_high = unname(stats::quantile(
    boot_overall[, "aligned"] - boot_overall[, "inverse"],
    probs = 0.975, names = FALSE, type = 8)),
  stringsAsFactors = FALSE
)

# Group contrasts are performed on daily group means, not on 247,720 pair-days.
group_series <- list(
  same_minus_different_reference =
    rowMeans(primary_aligned[, same_reference_cols, drop = FALSE]) -
    rowMeans(primary_aligned[, different_reference_cols, drop = FALSE]),
  within_minus_across_design =
    rowMeans(primary_aligned[, within_design_cols, drop = FALSE]) -
    rowMeans(primary_aligned[, across_design_cols, drop = FALSE])
)
group_tests <- do.call(rbind, lapply(names(group_series), function(nm) {
  h <- hac_mean(group_series[[nm]])
  data.frame(contrast = nm, estimate = unname(h["estimate"]),
             hac90_se = unname(h["se"]), ci_low = unname(h["ci_low"]),
             ci_high = unname(h["ci_high"]), z = unname(h["z"]),
             p_value = unname(h["p_value"]), stringsAsFactors = FALSE)
}))
group_tests$block_ci_low <- apply(boot_group, 2L, stats::quantile, probs = 0.025,
                                  na.rm = TRUE, names = FALSE, type = 8)
group_tests$block_ci_high <- apply(boot_group, 2L, stats::quantile, probs = 0.975,
                                   na.rm = TRUE, names = FALSE, type = 8)

# The system-wide sign contrast and two taxonomy contrasts form one primary
# three-test family. Dyad-level directional tests remain a separate 55-test
# family because they answer a different, pair-specific question.
primary_structure_p <- c(systemwide_alignment$p_value, group_tests$p_value)
primary_structure_holm <- p.adjust(primary_structure_p, method = "holm")
systemwide_alignment$p_holm_primary_3 <- primary_structure_holm[1L]
group_tests$p_holm_primary_3 <- primary_structure_holm[-1L]
write_csv(systemwide_alignment,
          file.path(table_dir, "Table_5_2_Systemwide_Downside_Alignment.csv"))
write_csv(group_tests, file.path(table_dir, "Table_S5_7_Group_Contrasts_HAC90.csv"))

# DPI-state contrast remains descriptive because DPI and links share an estimator.
state_estimate <- mean(daily_aligned[dpi_state == "Severe"]) -
  mean(daily_aligned[dpi_state == "Very low"])
state_table <- data.frame(
  contrast = "Severe minus very-low aligned-link density",
  estimate = state_estimate,
  block_ci_low = unname(quantile(boot_state, 0.025, na.rm = TRUE, type = 8)),
  block_ci_high = unname(quantile(boot_state, 0.975, na.rm = TRUE, type = 8)),
  inferential_status = "descriptive; DPI and links are generated by the same penalised system",
  stringsAsFactors = FALSE
)
write_csv(state_table,
          file.path(table_dir, "Table_S5_10_State_Contrast_Block_Bootstrap.csv"))

compare_specification <- function(label, active_alt, aligned_alt, inverse_alt) {
  primary_balance <- colMeans(primary_aligned) - colMeans(primary_inverse)
  alt_balance <- colMeans(aligned_alt) - colMeans(inverse_alt)
  union_active <- sum((primary_active + active_alt) > 0)
  union_aligned <- sum((primary_aligned + aligned_alt) > 0)
  data.frame(
    comparison = label,
    active_frequency_rank_spearman = cor(colMeans(primary_active), colMeans(active_alt),
                                         method = "spearman"),
    aligned_frequency_rank_spearman = cor(colMeans(primary_aligned), colMeans(aligned_alt),
                                          method = "spearman"),
    directional_balance_rank_spearman = cor(primary_balance, alt_balance, method = "spearman"),
    sign_state_agreement = mean((primary_aligned - primary_inverse) ==
                                  (aligned_alt - inverse_alt)),
    active_jaccard = if (union_active) sum((primary_active * active_alt) > 0) / union_active else NA_real_,
    aligned_jaccard = if (union_aligned) sum((primary_aligned * aligned_alt) > 0) / union_aligned else NA_real_,
    stringsAsFactors = FALSE
  )
}

specification_table <- rbind(
  compare_specification("robust-MAD strict vs robust-MAD screened",
                        screened_active, screened_aligned, screened_inverse),
  compare_specification("robust-MAD strict vs unscaled strict",
                        raw_active, raw_aligned, raw_inverse)
)

imputation <- read.csv(imputation_path, stringsAsFactors = FALSE)
imputation$date <- as.Date(imputation$date)
imputation <- imputation[match(dates, imputation$date), ]
if (anyNA(imputation$date)) stop("Imputation flags do not match coefficient dates.")
clean <- imputation$imputed_cells_in_window == 0
full_aligned_frequency <- colMeans(primary_aligned)
clean_aligned_frequency <- colMeans(primary_aligned[clean, , drop = FALSE])
imputation_result <- data.frame(
  comparison = "all windows vs windows without past-filled deviations",
  active_frequency_rank_spearman = cor(colMeans(primary_active),
    colMeans(primary_active[clean, , drop = FALSE]), method = "spearman"),
  aligned_frequency_rank_spearman = cor(full_aligned_frequency,
    clean_aligned_frequency, method = "spearman"),
  directional_balance_rank_spearman = cor(
    colMeans(primary_aligned) - colMeans(primary_inverse),
    colMeans(primary_aligned[clean, , drop = FALSE]) -
      colMeans(primary_inverse[clean, , drop = FALSE]), method = "spearman"),
  sign_state_agreement = NA_real_,
  active_jaccard = NA_real_,
  aligned_jaccard = NA_real_,
  stringsAsFactors = FALSE
)
specification_table <- rbind(specification_table, imputation_result)
write_csv(specification_table,
          file.path(table_dir, "Table_S5_8_Specification_Robustness.csv"))

# Chronological split is a transparent diagnostic of temporal ranking stability.
half <- floor(length(dates) / 2L)
first_frequency <- colMeans(primary_aligned[seq_len(half), , drop = FALSE])
second_frequency <- colMeans(primary_aligned[(half + 1L):length(dates), , drop = FALSE])
first_balance <- colMeans(primary_aligned[seq_len(half), , drop = FALSE]) -
  colMeans(primary_inverse[seq_len(half), , drop = FALSE])
second_balance <- colMeans(primary_aligned[(half + 1L):length(dates), , drop = FALSE]) -
  colMeans(primary_inverse[(half + 1L):length(dates), , drop = FALSE])
temporal_table <- data.frame(
  split = "chronological halves",
  first_start = format(dates[1]),
  first_end = format(dates[half]),
  second_start = format(dates[half + 1L]),
  second_end = format(dates[length(dates)]),
  aligned_frequency_rank_spearman = cor(first_frequency, second_frequency,
                                         method = "spearman"),
  directional_balance_rank_spearman = cor(first_balance, second_balance,
                                           method = "spearman"),
  top_10_overlap = length(intersect(order(first_frequency, decreasing = TRUE)[1:10],
                                    order(second_frequency, decreasing = TRUE)[1:10])),
  stringsAsFactors = FALSE
)
write_csv(temporal_table,
          file.path(table_dir, "Table_S5_11_Chronological_Stability.csv"))

qa <- data.frame(
  check = c(
    "primary_pair_matrix_complete", "alternative_keys_match",
    "bootstrap_repetitions_complete", "hac_direction_tests_complete",
    "holm_family_is_55_dyads", "systemwide_alignment_uses_daily_units",
    "holm_primary_family_is_3", "group_tests_use_daily_units",
    "imputation_exclusion_is_past_fill_only", "temporal_split_is_chronological"
  ),
  status = c(
    if (all(dim(primary_aligned) == c(2252L, 110L))) "PASS" else "FAIL",
    if (all(dim(screened_aligned) == dim(primary_aligned)) &&
        all(dim(raw_aligned) == dim(primary_aligned))) "PASS" else "FAIL",
    if (nrow(boot_aligned) == bootstrap_repetitions && all(is.finite(boot_aligned))) "PASS" else "FAIL",
    if (nrow(direction_tests) == 55L && all(is.finite(direction_tests$p_value))) "PASS" else "FAIL",
    if (length(direction_tests$p_holm_55) == 55L) "PASS" else "FAIL",
    if (length(systemwide_alignment_series) == 2252L &&
        all(is.finite(systemwide_alignment_series))) "PASS" else "FAIL",
    if (length(primary_structure_holm) == 3L &&
        all(is.finite(primary_structure_holm))) "PASS" else "FAIL",
    if (all(vapply(group_series, length, integer(1)) == 2252L)) "PASS" else "FAIL",
    if (sum(clean) == 1713L && sum(!clean) == 539L) "PASS" else "FAIL",
    if (half == 1126L && length(dates) - half == 1126L) "PASS" else "FAIL"
  ),
  detail = c(
    "2,252 dates by 110 directed non-self pairs",
    "robust screened and unscaled strict use the same pair-date keys",
    paste0(bootstrap_repetitions, " circular moving-block draws; block length=", block_length),
    paste0("HAC Bartlett lag=", hac_lag),
    "two-sided asymmetry p values adjusted jointly across 55 unordered pairs",
    "one aligned-minus-inverse cross-sectional difference per date",
    "aligned-minus-inverse and two taxonomy contrasts adjusted jointly",
    "each group contrast reduces the cross-section to one difference per date",
    "539 flagged windows excluded; no future fill was used in the source estimator",
    "equal first and second halves"
  ),
  stringsAsFactors = FALSE
)
write_csv(qa, file.path(qa_dir, "inference_robustness_QA.csv"))
if (any(qa$status != "PASS")) stop("Inference or robustness QA failed.")

input_manifest <- data.frame(
  file = basename(required_inputs),
  md5 = unname(tools::md5sum(required_inputs)),
  stringsAsFactors = FALSE
)
write_csv(input_manifest, file.path(qa_dir, "inference_input_manifest_md5.csv"))
capture.output(sessionInfo(), file = file.path(qa_dir, "inference_session_info.txt"))

summary_lines <- c(
  paste0("bootstrap_repetitions=", bootstrap_repetitions),
  paste0("block_length=", block_length),
  paste0("hac_lag=", hac_lag),
  paste0("holm_asymmetric_dyads=", sum(direction_tests$p_holm_55 < 0.05)),
  paste0("systemwide_aligned_minus_inverse=",
         formatC(systemwide_alignment$difference, digits = 6, format = "f")),
  paste0("systemwide_alignment_hac_ci=[",
         formatC(systemwide_alignment$ci_low, digits = 6, format = "f"), ",",
         formatC(systemwide_alignment$ci_high, digits = 6, format = "f"), "]"),
  paste0("systemwide_alignment_raw_p=",
         format(systemwide_alignment$p_value, scientific = TRUE, digits = 6)),
  paste0("systemwide_alignment_holm_p=",
         format(systemwide_alignment$p_holm_primary_3, scientific = TRUE, digits = 6)),
  paste0("same_reference_contrast=", formatC(group_tests$estimate[1], digits = 6, format = "f")),
  paste0("same_reference_holm_p=", format(group_tests$p_holm_primary_3[1], scientific = TRUE, digits = 6)),
  paste0("within_design_contrast=", formatC(group_tests$estimate[2], digits = 6, format = "f")),
  paste0("within_design_holm_p=", format(group_tests$p_holm_primary_3[2], scientific = TRUE, digits = 6)),
  paste0("state_severe_minus_very_low=", formatC(state_estimate, digits = 6, format = "f")),
  paste0("strict_screened_aligned_rank_spearman=",
         formatC(specification_table$aligned_frequency_rank_spearman[1], digits = 6, format = "f")),
  paste0("raw_scaled_aligned_rank_spearman=",
         formatC(specification_table$aligned_frequency_rank_spearman[2], digits = 6, format = "f")),
  paste0("exclude_imputed_aligned_rank_spearman=",
         formatC(specification_table$aligned_frequency_rank_spearman[3], digits = 6, format = "f")),
  paste0("chronological_half_rank_spearman=",
         formatC(temporal_table$aligned_frequency_rank_spearman, digits = 6, format = "f")),
  paste0("chronological_half_balance_rank_spearman=",
         formatC(temporal_table$directional_balance_rank_spearman, digits = 6, format = "f")),
  paste0("chronological_top10_overlap=", temporal_table$top_10_overlap)
)
writeLines(summary_lines, file.path(qa_dir, "inference_robustness_summary.txt"), useBytes = TRUE)
message("Section 5.2 inference and robustness completed: ", bundle_dir)
