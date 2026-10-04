#!/usr/bin/env Rscript
# Paired, date-level analysis of the matched baseline and on-chain extension.
options(stringsAsFactors = FALSE, digits = 15, scipen = 999)

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(file_arg) != 1L) stop("Run this file with Rscript.")
code_dir <- dirname(normalizePath(sub("^--file=", "", file_arg)))
bundle_dir <- dirname(code_dir)
input_dir <- file.path(bundle_dir, "source_data", "input")
main_dpi_file <- file.path(input_dir, "dpi_primary_robust_mad_strict.csv")
aug_file <- file.path(bundle_dir, "source_data", "Augmented_Predictions.rds")
base_file <- file.path(input_dir, "OOS_Block_Model_Predictions.rds")
stopifnot(all(file.exists(c(aug_file, base_file, main_dpi_file))))

source_dir <- file.path(bundle_dir, "source_data")
table_dir <- file.path(bundle_dir, "tables")
qa_dir <- file.path(bundle_dir, "qa")
invisible(lapply(c(source_dir, table_dir, qa_dir), dir.create,
                 recursive = TRUE, showWarnings = FALSE))

write_data <- function(x, path) {
  write.csv(x, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

aug <- readRDS(aug_file)
base_all <- readRDS(base_file)
aug$date <- as.Date(aug$date)
base_all$date <- as.Date(base_all$date)
base <- base_all[base_all$model == "joint" &
                   base_all$date %in% aug$date, ]
benchmark <- base_all[base_all$model == "benchmark" &
                        base_all$date %in% aug$date,
                      c("date", "target", "specification", "check_loss")]
names(benchmark)[4L] <- "benchmark_loss"
keys <- c("date", "target", "specification")
stopifnot(!anyDuplicated(aug[keys]), !anyDuplicated(base[keys]),
          !anyDuplicated(benchmark[keys]))
paired <- merge(base, aug, by = keys, suffixes = c("_base", "_aug"),
                all = FALSE, sort = FALSE)
paired <- merge(paired, benchmark, by = keys, all = FALSE, sort = FALSE)
paired <- paired[order(paired$date, paired$target, paired$specification), ]
rownames(paired) <- NULL
expected_rows <- 1521L * 11L * 2L
stopifnot(nrow(paired) == expected_rows, nrow(base) == expected_rows,
          nrow(aug) == expected_rows, nrow(benchmark) == expected_rows,
          all(is.finite(paired$check_loss_base)),
          all(is.finite(paired$check_loss_aug)),
          all(is.finite(paired$benchmark_loss)),
          max(abs(paired$observed_deviation_base -
                    paired$observed_deviation_aug)) < 1e-12,
          max(abs(paired$target_center_base - paired$target_center_aug)) < 1e-12,
          max(abs(paired$target_scale_base - paired$target_scale_aug)) < 1e-12)

paired$loss_reduction <- paired$check_loss_base - paired$check_loss_aug
paired$gacv_reduction <- paired$selected_gacv_base - paired$selected_gacv_aug
paired$lambda_change <- paired$selected_lambda_aug - paired$selected_lambda_base
write_data(paired[, c(keys, "check_loss_base", "check_loss_aug",
                       "benchmark_loss", "loss_reduction",
                       "selected_gacv_base", "selected_gacv_aug",
                       "gacv_reduction", "selected_lambda_base",
                       "selected_lambda_aug", "lambda_change",
                       "active_size_base", "active_size_aug", "active_micro",
                       "beta_curve_imbalance", "beta_curve_swap_count",
                       "beta_uni_near_inventory")],
           file.path(source_dir, "Paired_Target_Diagnostics.csv"))

date_mean <- function(x, value) {
  aggregate(x[[value]], by = x[c("date", "specification")], FUN = mean)
}
daily <- aggregate(
  paired[c("check_loss_base", "check_loss_aug", "benchmark_loss",
           "selected_gacv_base", "selected_gacv_aug", "loss_reduction",
           "gacv_reduction", "selected_lambda_base", "selected_lambda_aug",
           "active_size_base", "active_size_aug", "active_micro")],
  by = paired[c("date", "specification")], FUN = mean
)
daily <- daily[order(daily$specification, daily$date), ]
write_data(daily, file.path(source_dir, "Daily_Matched_Model_Comparison.csv"))

# One calendar date, after averaging the 11 targets, is the inference unit.
# A 90-day Bartlett bandwidth tracks the overlap of adjacent model windows.
hac_mean <- function(x, lag = 90L) {
  x <- as.numeric(x)
  stopifnot(all(is.finite(x)), length(x) > lag)
  n <- length(x)
  m <- mean(x)
  u <- x - m
  long_run_var <- sum(u * u) / n
  for (ell in seq_len(lag)) {
    gamma <- sum(u[(ell + 1L):n] * u[seq_len(n - ell)]) / n
    long_run_var <- long_run_var + 2 * (1 - ell / (lag + 1)) * gamma
  }
  se <- sqrt(max(long_run_var, 0) / n)
  c(estimate = m, se = se, ci_low = m - 1.96 * se,
    ci_high = m + 1.96 * se,
    p_value = if (se > 0) 2 * pnorm(-abs(m / se)) else NA_real_)
}

block_ci <- function(x, block_length = 90L, replicates = 1999L, seed = 5606L) {
  x <- as.numeric(x)
  n <- length(x)
  n_blocks <- ceiling(n / block_length)
  circular <- c(x, x[seq_len(block_length - 1L)])
  set.seed(seed)
  draws <- numeric(replicates)
  for (b in seq_len(replicates)) {
    starts <- sample.int(n, n_blocks, replace = TRUE)
    idx <- unlist(lapply(starts, function(s) s:(s + block_length - 1L)),
                  use.names = FALSE)
    draws[b] <- mean(circular[idx[seq_len(n)]])
  }
  c(ci_low = unname(quantile(draws, .025, type = 6)),
    ci_high = unname(quantile(draws, .975, type = 6)))
}

spec_order <- c("same_day_conditional", "all_predictors_lagged")
endpoint_order <- c("gacv_reduction", "loss_reduction")
table_rows <- list()
k <- 0L
for (spec in spec_order) {
  z <- daily[daily$specification == spec, ]
  stopifnot(nrow(z) == 1521L, !anyDuplicated(z$date))
  for (endpoint in endpoint_order) {
    k <- k + 1L
    base_name <- if (endpoint == "gacv_reduction") "selected_gacv_base" else
      "check_loss_base"
    aug_name <- if (endpoint == "gacv_reduction") "selected_gacv_aug" else
      "check_loss_aug"
    h <- hac_mean(z[[endpoint]])
    b <- block_ci(z[[endpoint]], seed = 5606L + k)
    table_rows[[k]] <- data.frame(
      timing = spec, endpoint = if (endpoint == "gacv_reduction")
        "selected_GACV" else "held_out_check_loss",
      n_dates = nrow(z), n_target_equations = nrow(z) * 11L,
      baseline_mean = mean(z[[base_name]]),
      augmented_mean = mean(z[[aug_name]]),
      improvement = unname(h["estimate"]),
      relative_improvement_pct = 100 * unname(h["estimate"]) /
        mean(z[[base_name]]),
      hac90_se = unname(h["se"]),
      hac90_ci_low = unname(h["ci_low"]),
      hac90_ci_high = unname(h["ci_high"]),
      block90_ci_low = unname(b["ci_low"]),
      block90_ci_high = unname(b["ci_high"]),
      p_value = unname(h["p_value"]),
      benchmark_mean_loss = if (endpoint == "loss_reduction")
        mean(z$benchmark_loss) else NA_real_,
      baseline_qss_vs_benchmark = if (endpoint == "loss_reduction")
        1 - mean(z$check_loss_base) / mean(z$benchmark_loss) else NA_real_,
      augmented_qss_vs_benchmark = if (endpoint == "loss_reduction")
        1 - mean(z$check_loss_aug) / mean(z$benchmark_loss) else NA_real_
    )
  }
}
comparison <- do.call(rbind, table_rows)
comparison$p_holm_4 <- p.adjust(comparison$p_value, method = "holm")
comparison$bootstrap_replicates <- 1999L
write_data(comparison, file.path(table_dir, "Table_5_6_Matched_Model_Comparison.csv"))
manuscript_table <- comparison[, c(
  "timing", "endpoint", "n_dates", "baseline_mean", "augmented_mean",
  "relative_improvement_pct", "hac90_ci_low", "hac90_ci_high",
  "p_holm_4", "baseline_qss_vs_benchmark", "augmented_qss_vs_benchmark"
)]
write_data(manuscript_table,
           file.path(table_dir, "Table_5_6_Manuscript_Comparison.csv"))

coef_map <- c(curve_3pool_imbalance = "beta_curve_imbalance",
              curve_3pool_swap_count = "beta_curve_swap_count",
              uni_usdc_usdt_near_inventory_25ticks = "beta_uni_near_inventory")
selection <- do.call(rbind, lapply(spec_order, function(spec) {
  z <- paired[paired$specification == spec, ]
  do.call(rbind, lapply(names(coef_map), function(variable) {
    beta <- z[[coef_map[[variable]]]]
    active <- abs(beta) > 1e-10
    data.frame(timing = spec, variable = variable,
               n_target_equations = length(beta),
               active_frequency = mean(active),
               positive_frequency = mean(beta > 1e-10),
               negative_frequency = mean(beta < -1e-10),
               median_active_coefficient = if (any(active))
                 median(beta[active]) else NA_real_)
  }))
}))
write_data(selection, file.path(table_dir, "Table_S5_6_Micro_Selection.csv"))

target_selection <- do.call(rbind, lapply(spec_order, function(spec) {
  z <- paired[paired$specification == spec, ]
  do.call(rbind, lapply(sort(unique(z$target)), function(target) {
    zz <- z[z$target == target, ]
    data.frame(timing = spec, target = target,
               n_dates = nrow(zz),
               any_micro_active_frequency = mean(zz$active_micro > 0),
               mean_active_micro = mean(zz$active_micro),
               curve_imbalance_frequency = mean(abs(zz$beta_curve_imbalance) > 1e-10),
               curve_swap_count_frequency = mean(abs(zz$beta_curve_swap_count) > 1e-10),
               uni_near_inventory_frequency = mean(abs(zz$beta_uni_near_inventory) > 1e-10))
  }))
}))
write_data(target_selection,
           file.path(table_dir, "Table_S5_6_Target_Level_Micro_Selection.csv"))

target_performance <- aggregate(
  paired[c("check_loss_base", "check_loss_aug", "loss_reduction",
           "selected_gacv_base", "selected_gacv_aug")],
  by = paired[c("specification", "target")], FUN = mean
)
target_performance$relative_loss_improvement_pct <-
  100 * target_performance$loss_reduction / target_performance$check_loss_base
target_performance <- target_performance[
  order(target_performance$specification, target_performance$target), ]
write_data(target_performance,
           file.path(table_dir, "Table_S5_6_Target_Level_Performance.csv"))

# The three coins directly represented in the supplied pools were fixed by
# data coverage, not chosen after seeing model performance.
paired$pool_coverage_group <- ifelse(paired$target %in% c("DAI", "USDC", "USDT"),
                                     "pool_represented_3", "other_8")
group_daily <- aggregate(
  paired[c("check_loss_base", "check_loss_aug", "loss_reduction")],
  by = paired[c("date", "specification", "pool_coverage_group")], FUN = mean
)
group_table <- do.call(rbind, lapply(spec_order, function(spec) {
  z <- group_daily[group_daily$specification == spec, ]
  group_means <- aggregate(z[c("check_loss_base", "check_loss_aug",
                               "loss_reduction")],
                           by = z["pool_coverage_group"], FUN = mean)
  group_means$relative_improvement_pct <-
    100 * group_means$loss_reduction / group_means$check_loss_base
  group_means$timing <- spec
  group_means
}))
write_data(group_table,
           file.path(table_dir, "Table_S5_6_Pool_Coverage_Groups.csv"))
group_contrast <- do.call(rbind, lapply(spec_order, function(spec) {
  z <- group_daily[group_daily$specification == spec,
                   c("date", "pool_coverage_group", "loss_reduction")]
  w <- reshape(z, idvar = "date", timevar = "pool_coverage_group",
               direction = "wide")
  w <- w[order(w$date), ]
  d <- w$loss_reduction.pool_represented_3 - w$loss_reduction.other_8
  h <- hac_mean(d)
  data.frame(timing = spec, n_dates = nrow(w),
             represented_minus_other_gain = unname(h["estimate"]),
             hac90_ci_low = unname(h["ci_low"]),
             hac90_ci_high = unname(h["ci_high"]),
             p_value = unname(h["p_value"]))
}))
group_contrast$p_holm_2 <- p.adjust(group_contrast$p_value, method = "holm")
write_data(group_contrast,
           file.path(table_dir, "Table_S5_6_Pool_Coverage_Contrast.csv"))

index <- daily[daily$specification == "same_day_conditional",
               c("date", "selected_lambda_base", "selected_lambda_aug")]
index$window_end_date <- index$date - 1L
main_dpi <- read.csv(main_dpi_file)
main_dpi$date <- as.Date(main_dpi$date)
index$frozen_chapter4_dpi <- main_dpi$dpi_primary[
  match(index$window_end_date, main_dpi$date)]
stopifnot(!anyNA(index$frozen_chapter4_dpi))
names(index)[names(index) == "selected_lambda_base"] <- "matched_baseline_dpi"
names(index)[names(index) == "selected_lambda_aug"] <- "augmented_lambda_index"
write_data(index, file.path(source_dir, "Daily_Auxiliary_Lambda_Index.csv"))
index_summary <- data.frame(
  n_windows = nrow(index),
  mean_matched_baseline_dpi = mean(index$matched_baseline_dpi),
  mean_augmented_lambda_index = mean(index$augmented_lambda_index),
  spearman_rank_correlation = cor(index$matched_baseline_dpi,
                                  index$augmented_lambda_index,
                                  method = "spearman"),
  pearson_correlation = cor(index$matched_baseline_dpi,
                            index$augmented_lambda_index),
  median_absolute_pct_change = median(abs(index$augmented_lambda_index /
                                           index$matched_baseline_dpi - 1)) * 100,
  fraction_exactly_matching_frozen_chapter4 = mean(
    abs(index$matched_baseline_dpi - index$frozen_chapter4_dpi) < 1e-12)
)
write_data(index_summary, file.path(table_dir, "Table_S5_6_Auxiliary_Index.csv"))

qa <- data.frame(
  check = c("matched_rows", "same_dates_and_targets", "same_observed_response",
            "same_target_scaling", "finite_losses", "full_output_dates"),
  status = rep("PASS", 6L),
  detail = c(nrow(paired), length(unique(paired$date)),
             max(abs(paired$observed_deviation_base -
                       paired$observed_deviation_aug)),
             max(abs(paired$target_scale_base - paired$target_scale_aug)),
             sum(is.finite(paired$check_loss_aug)),
             paste(min(paired$date), max(paired$date), sep = " to "))
)
write_data(qa, file.path(qa_dir, "comparison_QA.csv"))
writeLines(capture.output(sessionInfo()),
           file.path(qa_dir, "session_info_analysis.txt"))
print(comparison, row.names = FALSE)
print(selection, row.names = FALSE)
print(index_summary, row.names = FALSE)
