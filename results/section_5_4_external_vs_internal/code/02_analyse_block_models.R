#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1L])))
} else normalizePath(getwd())
source(file.path(script_dir, "00_config.R"))

prediction_path <- file.path(source_dir, "OOS_Block_Model_Predictions.rds")
if (!file.exists(prediction_path)) stop("Run 01_estimate_block_models.R first.")
pred <- readRDS(prediction_path)
pred$date <- as.Date(pred$date)
if (any(!is.finite(pred$check_loss)) || anyNA(pred$hit)) {
  stop("Prediction panel contains incomplete losses or hit indicators.")
}

model_order <- c("benchmark", "macro_only", "coin_only", "joint")
spec_order <- c("same_day_conditional", "all_predictors_lagged")
model_labels <- c(
  benchmark = "Intercept-only benchmark",
  macro_only = "Macro-only",
  coin_only = "Coin-only",
  joint = "Joint"
)
spec_labels <- c(
  same_day_conditional = "Same-day conditional",
  all_predictors_lagged = "All predictors lagged"
)

hac_cov_mean <- function(X, lag = hac_lag) {
  X <- as.matrix(X)
  if (any(!is.finite(X))) stop("HAC input must be complete and finite.")
  n <- nrow(X)
  U <- sweep(X, 2L, colMeans(X), "-")
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

hac_mean <- function(x, null = 0, lag = hac_lag) {
  fit <- hac_cov_mean(matrix(as.numeric(x), ncol = 1L), lag = lag)
  estimate <- unname(fit$mean[1L])
  se <- sqrt(max(fit$cov_mean[1L, 1L], 0))
  z <- if (se > 0) (estimate - null) / se else if (estimate == null) 0 else
    sign(estimate - null) * Inf
  data.frame(
    estimate = estimate, hac90_se = se,
    ci_low = estimate - 1.96 * se, ci_high = estimate + 1.96 * se,
    z = z, p_value = 2 * pnorm(-abs(z)), n_dates = fit$n,
    stringsAsFactors = FALSE
  )
}

ratio_hac <- function(numerator, denominator, lag = hac_lag) {
  X <- cbind(numerator = as.numeric(numerator), denominator = as.numeric(denominator))
  fit <- hac_cov_mean(X, lag = lag)
  a <- fit$mean[1L]
  b <- fit$mean[2L]
  estimate <- a / b
  gradient <- c(1 / b, -a / b^2)
  variance <- as.numeric(t(gradient) %*% fit$cov_mean %*% gradient)
  se <- sqrt(max(variance, 0))
  data.frame(
    estimate = estimate, hac90_se = se,
    ci_low = estimate - 1.96 * se, ci_high = estimate + 1.96 * se,
    stringsAsFactors = FALSE
  )
}

block_bootstrap_mean <- function(x, block_length = 90L, replicates = 1999L,
                                 seed = 5402026L) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L) stop("Bootstrap series is too short.")
  block_length <- min(as.integer(block_length), n)
  n_blocks <- ceiling(n / block_length)
  set.seed(seed)
  boot <- numeric(replicates)
  circular <- c(x, x[seq_len(block_length - 1L)])
  for (b in seq_len(replicates)) {
    starts <- sample.int(n, n_blocks, replace = TRUE)
    sampled <- unlist(lapply(starts, function(s) {
      circular[s:(s + block_length - 1L)]
    }), use.names = FALSE)
    boot[b] <- mean(sampled[seq_len(n)])
  }
  lower_count <- sum(boot <= 0)
  upper_count <- sum(boot >= 0)
  c(
    ci_low = unname(quantile(boot, 0.025, type = 6)),
    ci_high = unname(quantile(boot, 0.975, type = 6)),
    p_value = min(1, 2 * (min(lower_count, upper_count) + 1) /
                    (replicates + 1)),
    replicates = replicates
  )
}

daily_loss <- aggregate(
  check_loss ~ date + specification + model,
  data = pred, FUN = mean
)
daily_hit <- aggregate(
  hit ~ date + specification + model,
  data = pred, FUN = mean
)
daily_gacv <- aggregate(
  selected_gacv ~ date + specification + model,
  data = pred, FUN = mean
)
names(daily_loss)[names(daily_loss) == "check_loss"] <- "mean_check_loss"
names(daily_hit)[names(daily_hit) == "hit"] <- "hit_rate"
names(daily_gacv)[names(daily_gacv) == "selected_gacv"] <- "mean_selected_gacv"
write_csv(daily_loss, file.path(source_dir, "Daily_Block_Model_Losses.csv"))
write_csv(daily_gacv, file.path(source_dir, "Daily_Block_Model_GACV.csv"))

wide_daily <- function(specification) {
  z <- daily_loss[daily_loss$specification == specification, ]
  w <- reshape(z, idvar = "date", timevar = "model", direction = "wide")
  names(w) <- sub("^mean_check_loss\\.", "", names(w))
  w <- w[order(w$date), ]
  if (!all(model_order %in% names(w))) stop("Incomplete daily model-loss matrix.")
  w
}

wide_gacv <- function(specification) {
  z <- daily_gacv[daily_gacv$specification == specification, ]
  w <- reshape(z, idvar = "date", timevar = "model", direction = "wide")
  names(w) <- sub("^mean_selected_gacv\\.", "", names(w))
  w <- w[order(w$date), ]
  if (!all(model_order %in% names(w))) stop("Incomplete daily GACV matrix.")
  w
}

model_summary <- do.call(rbind, lapply(spec_order, function(specification) {
  w <- wide_daily(specification)
  hit_spec <- daily_hit[daily_hit$specification == specification, ]
  do.call(rbind, lapply(model_order, function(model) {
    loss_fit <- hac_mean(w[[model]])
    hit_vec <- hit_spec$hit_rate[match(w$date,
                                      hit_spec$date[hit_spec$model == model])]
    # The explicit subset avoids depending on reshape ordering.
    hit_rows <- hit_spec[hit_spec$model == model, ]
    hit_vec <- hit_rows$hit_rate[match(w$date, hit_rows$date)]
    hit_fit <- hac_mean(hit_vec, null = tau)
    if (model == "benchmark") {
      skill <- data.frame(estimate = 0, hac90_se = 0, ci_low = 0, ci_high = 0)
    } else {
      # QSS = (benchmark loss - model loss) / benchmark loss.
      skill <- ratio_hac(w$benchmark - w[[model]], w$benchmark)
    }
    data.frame(
      specification = specification,
      specification_label = unname(spec_labels[specification]),
      model = model, model_label = unname(model_labels[model]),
      n_dates = nrow(w), n_target_forecasts = nrow(w) * length(coin_order),
      mean_standardized_check_loss = loss_fit$estimate,
      loss_hac90_se = loss_fit$hac90_se,
      loss_ci_low = loss_fit$ci_low, loss_ci_high = loss_fit$ci_high,
      quantile_skill_score = skill$estimate,
      qss_hac90_se = skill$hac90_se,
      qss_ci_low = skill$ci_low, qss_ci_high = skill$ci_high,
      hit_rate = hit_fit$estimate, hit_hac90_se = hit_fit$hac90_se,
      hit_ci_low = hit_fit$ci_low, hit_ci_high = hit_fit$ci_high,
      hit_test_p_value = hit_fit$p_value,
      stringsAsFactors = FALSE
    )
  }))
}))
write_csv(model_summary, file.path(table_dir, "Table_5_4_OOS_Model_Performance.csv"))

conditional_summary <- do.call(rbind, lapply(spec_order, function(specification) {
  w <- wide_gacv(specification)
  do.call(rbind, lapply(model_order, function(model) {
    fit <- hac_mean(w[[model]])
    if (model == "benchmark") {
      skill <- data.frame(estimate = 0, hac90_se = 0, ci_low = 0, ci_high = 0)
    } else {
      skill <- ratio_hac(w$benchmark - w[[model]], w$benchmark)
    }
    data.frame(
      specification = specification,
      specification_label = unname(spec_labels[specification]),
      model = model, model_label = unname(model_labels[model]),
      n_dates = nrow(w), n_target_windows = nrow(w) * length(coin_order),
      mean_selected_gacv = fit$estimate,
      gacv_hac90_se = fit$hac90_se,
      gacv_ci_low = fit$ci_low, gacv_ci_high = fit$ci_high,
      conditional_skill_score = skill$estimate,
      skill_hac90_se = skill$hac90_se,
      skill_ci_low = skill$ci_low, skill_ci_high = skill$ci_high,
      stringsAsFactors = FALSE
    )
  }))
}))
write_csv(conditional_summary,
          file.path(table_dir, "Table_5_4_Conditional_Model_Performance.csv"))

# One compact main-text table. The detailed inference tables below remain the
# source for block contrasts and multiple-testing adjustment.
main_conditional <- conditional_summary[
  conditional_summary$specification == "same_day_conditional",
  c("model", "model_label", "mean_selected_gacv",
    "conditional_skill_score")
]
main_oos <- model_summary[
  model_summary$specification == "same_day_conditional",
  c("model", "mean_standardized_check_loss", "quantile_skill_score")
]
main_table <- merge(main_conditional, main_oos, by = "model", all = FALSE,
                    sort = FALSE)
main_table <- main_table[match(model_order, main_table$model), ]
main_table <- main_table[, c(
  "model", "model_label", "mean_selected_gacv",
  "conditional_skill_score", "mean_standardized_check_loss",
  "quantile_skill_score"
)]
write_csv(main_table, file.path(table_dir, "Table_5_4_Main_Results.csv"))

contrast_definitions <- list(
  coin_vs_macro = list(
    label = "Coin-only relative to macro-only",
    better = "coin_only", baseline = "macro_only"
  ),
  incremental_coin = list(
    label = "Incremental coin information in joint model",
    better = "joint", baseline = "macro_only"
  ),
  incremental_macro = list(
    label = "Incremental macro information in joint model",
    better = "joint", baseline = "coin_only"
  )
)

contrast_table <- do.call(rbind, lapply(seq_along(spec_order), function(s) {
  specification <- spec_order[s]
  w <- wide_daily(specification)
  tab <- do.call(rbind, lapply(seq_along(contrast_definitions), function(k) {
    id <- names(contrast_definitions)[k]
    def <- contrast_definitions[[k]]
    differential <- w[[def$baseline]] - w[[def$better]]
    h <- hac_mean(differential)
    boot <- block_bootstrap_mean(
      differential, block_length = 90L, replicates = 1999L,
      seed = 5402026L + s * 100L + k
    )
    data.frame(
      specification = specification,
      specification_label = unname(spec_labels[specification]),
      contrast_id = id, contrast = def$label,
      baseline_model = def$baseline, comparison_model = def$better,
      loss_reduction = h$estimate,
      relative_loss_reduction = h$estimate / mean(w[[def$baseline]]),
      hac90_se = h$hac90_se, ci_low = h$ci_low, ci_high = h$ci_high,
      z = h$z, p_value = h$p_value,
      block90_ci_low = boot["ci_low"], block90_ci_high = boot["ci_high"],
      block90_p_value = boot["p_value"], bootstrap_replicates = boot["replicates"],
      n_dates = nrow(w), stringsAsFactors = FALSE
    )
  }))
  tab$p_holm_3 <- p.adjust(tab$p_value, method = "holm")
  tab
}))
write_csv(contrast_table,
          file.path(table_dir, "Table_5_4_OOS_Block_Contrasts.csv"))

conditional_contrasts <- do.call(rbind, lapply(seq_along(spec_order), function(s) {
  specification <- spec_order[s]
  w <- wide_gacv(specification)
  tab <- do.call(rbind, lapply(seq_along(contrast_definitions), function(k) {
    id <- names(contrast_definitions)[k]
    def <- contrast_definitions[[k]]
    differential <- w[[def$baseline]] - w[[def$better]]
    h <- hac_mean(differential)
    boot <- block_bootstrap_mean(
      differential, block_length = 90L, replicates = 1999L,
      seed = 5402500L + s * 100L + k
    )
    data.frame(
      specification = specification,
      specification_label = unname(spec_labels[specification]),
      contrast_id = id, contrast = def$label,
      baseline_model = def$baseline, comparison_model = def$better,
      gacv_reduction = h$estimate,
      relative_gacv_reduction = h$estimate / mean(w[[def$baseline]]),
      hac90_se = h$hac90_se, ci_low = h$ci_low, ci_high = h$ci_high,
      z = h$z, p_value = h$p_value,
      block90_ci_low = boot["ci_low"], block90_ci_high = boot["ci_high"],
      block90_p_value = boot["p_value"], bootstrap_replicates = boot["replicates"],
      n_dates = nrow(w), stringsAsFactors = FALSE
    )
  }))
  tab$p_holm_3 <- p.adjust(tab$p_value, method = "holm")
  tab
}))
write_csv(conditional_contrasts,
          file.path(table_dir, "Table_5_4_Conditional_Block_Contrasts.csv"))

# Paired timing test on the common 2,251 dates. A positive value means that
# contemporaneous stablecoin deviations reduce check loss more than their
# one-day lags. Because the macro-only information set is identical in both
# specifications, the joint-model timing contrast is also the direct contrast
# in incremental coin information conditional on macro variables.
same_wide <- wide_daily("same_day_conditional")
lagged_wide <- wide_daily("all_predictors_lagged")
common_dates <- as.Date(intersect(same_wide$date, lagged_wide$date),
                        origin = "1970-01-01")
same_wide <- same_wide[match(common_dates, same_wide$date), ]
lagged_wide <- lagged_wide[match(common_dates, lagged_wide$date), ]
timing_series <- list(
  coin_only_same_day_advantage = lagged_wide$coin_only - same_wide$coin_only,
  joint_same_day_advantage = lagged_wide$joint - same_wide$joint
)
timing_labels <- c(
  coin_only_same_day_advantage = "Same-day advantage in the coin-only model",
  joint_same_day_advantage = "Same-day coin-information advantage in the joint model"
)
timing_contrasts <- do.call(rbind, lapply(seq_along(timing_series), function(k) {
  id <- names(timing_series)[k]
  h <- hac_mean(timing_series[[k]])
  boot <- block_bootstrap_mean(timing_series[[k]], block_length = 90L,
                               replicates = 1999L, seed = 5402400L + k)
  data.frame(
    contrast_id = id, contrast = unname(timing_labels[id]),
    loss_reduction = h$estimate,
    relative_to_lagged_loss = h$estimate /
      mean(if (k == 1L) lagged_wide$coin_only else lagged_wide$joint),
    hac90_se = h$hac90_se, ci_low = h$ci_low, ci_high = h$ci_high,
    z = h$z, p_value = h$p_value,
    block90_ci_low = boot["ci_low"], block90_ci_high = boot["ci_high"],
    block90_p_value = boot["p_value"], bootstrap_replicates = boot["replicates"],
    n_dates = length(common_dates), stringsAsFactors = FALSE
  )
}))
timing_contrasts$p_holm_2 <- p.adjust(timing_contrasts$p_value, method = "holm")
write_csv(timing_contrasts,
          file.path(table_dir, "Table_5_4_Timing_Contrasts.csv"))

shapley_table <- do.call(rbind, lapply(spec_order, function(specification) {
  w <- wide_gacv(specification)
  phi_macro <- 0.5 * ((w$benchmark - w$macro_only) +
                      (w$coin_only - w$joint))
  phi_coin <- 0.5 * ((w$benchmark - w$coin_only) +
                     (w$macro_only - w$joint))
  total <- w$benchmark - w$joint
  do.call(rbind, lapply(c("macro", "coin"), function(block) {
    phi <- if (block == "macro") phi_macro else phi_coin
    h <- hac_mean(phi)
    share <- ratio_hac(phi, total)
    data.frame(
      specification = specification,
      specification_label = unname(spec_labels[specification]),
      information_block = block,
      shapley_loss_reduction = h$estimate,
      hac90_se = h$hac90_se, ci_low = h$ci_low, ci_high = h$ci_high,
      p_value = h$p_value,
      share_of_joint_improvement = share$estimate,
      share_hac90_se = share$hac90_se,
      share_ci_low = share$ci_low, share_ci_high = share$ci_high,
      total_joint_improvement = mean(total), n_dates = nrow(w),
      stringsAsFactors = FALSE
    )
  }))
}))
write_csv(shapley_table,
          file.path(table_dir, "Table_S5_18_Conditional_GACV_Decomposition.csv"))

# Target-level evidence for the main figure. All values are normalized by the
# target-specific benchmark loss, so each stablecoin receives equal display
# weight even when its peg-deviation scale differs.
target_loss <- aggregate(
  check_loss ~ specification + target + model,
  data = pred, FUN = mean
)
target_oos <- do.call(rbind, lapply(spec_order, function(specification) {
  do.call(rbind, lapply(coin_order, function(target) {
    z <- target_loss[target_loss$specification == specification &
                       target_loss$target == target, ]
    loss <- setNames(z$check_loss, z$model)
    if (!all(model_order %in% names(loss))) stop("Incomplete target-level losses.")
    data.frame(
      specification = specification,
      specification_label = unname(spec_labels[specification]),
      target = target,
      benchmark_loss = loss["benchmark"],
      macro_only_qss = 1 - loss["macro_only"] / loss["benchmark"],
      coin_only_qss = 1 - loss["coin_only"] / loss["benchmark"],
      joint_qss = 1 - loss["joint"] / loss["benchmark"],
      incremental_macro = (loss["coin_only"] - loss["joint"]) / loss["benchmark"],
      incremental_coin = (loss["macro_only"] - loss["joint"]) / loss["benchmark"],
      stringsAsFactors = FALSE
    )
  }))
}))

target_gacv_loss <- aggregate(
  selected_gacv ~ specification + target + model,
  data = pred, FUN = mean
)
target_conditional <- do.call(rbind, lapply(spec_order, function(specification) {
  do.call(rbind, lapply(coin_order, function(target) {
    z <- target_gacv_loss[target_gacv_loss$specification == specification &
                            target_gacv_loss$target == target, ]
    loss <- setNames(z$selected_gacv, z$model)
    data.frame(
      specification = specification,
      specification_label = unname(spec_labels[specification]),
      target = target,
      benchmark_loss = loss["benchmark"],
      macro_only_qss = 1 - loss["macro_only"] / loss["benchmark"],
      coin_only_qss = 1 - loss["coin_only"] / loss["benchmark"],
      joint_qss = 1 - loss["joint"] / loss["benchmark"],
      stringsAsFactors = FALSE
    )
  }))
}))

figure_rows <- list()
counter <- 0L
append_figure_rows <- function(data, evidence_id, evidence_label,
                               specification) {
  z <- data[data$specification == specification, ]
  out <- do.call(rbind, lapply(seq_len(nrow(z)), function(i) {
    do.call(rbind, lapply(c("macro_only", "coin_only", "joint"), function(model) {
      data.frame(
        target = z$target[i], evidence_id = evidence_id,
        evidence_label = evidence_label,
        specification = specification, model = model,
        model_label = unname(model_labels[model]),
        skill_score = z[[paste0(model, "_qss")]][i],
        stringsAsFactors = FALSE
      )
    }))
  }))
  out
}
figure_rows[[1L]] <- append_figure_rows(
  target_conditional, "conditional_same_day", "Conditional GACV · same-day",
  "same_day_conditional"
)
figure_rows[[2L]] <- append_figure_rows(
  target_oos, "oos_same_day", "OOS check loss · same-day",
  "same_day_conditional"
)
figure_rows[[3L]] <- append_figure_rows(
  target_oos, "oos_all_lagged", "OOS check loss · all lagged",
  "all_predictors_lagged"
)
figure_source <- do.call(rbind, figure_rows)

# Add one system row based on the ratio of date-level mean losses. This row is
# not the arithmetic average of target QSS values.
system_sources <- list(
  list(id = "conditional_same_day", label = "Conditional GACV · same-day",
       table = conditional_summary,
       spec = "same_day_conditional", skill = "conditional_skill_score"),
  list(id = "oos_same_day", label = "OOS check loss · same-day",
       table = model_summary,
       spec = "same_day_conditional", skill = "quantile_skill_score"),
  list(id = "oos_all_lagged", label = "OOS check loss · all lagged",
       table = model_summary,
       spec = "all_predictors_lagged", skill = "quantile_skill_score")
)
system_rows <- do.call(rbind, lapply(system_sources, function(s) {
  z <- s$table[s$table$specification == s$spec &
                 s$table$model != "benchmark", ]
  data.frame(
    target = "System", evidence_id = s$id, evidence_label = s$label,
    specification = s$spec, model = z$model,
    model_label = z$model_label, skill_score = z[[s$skill]],
    stringsAsFactors = FALSE
  )
}))
figure_source <- rbind(system_rows, figure_source)
write_csv(figure_source, file.path(source_dir, "Figure_5_4_Source_Data.csv"))
write_csv(target_oos, file.path(source_dir, "Target_Level_OOS_Performance.csv"))
write_csv(target_conditional,
          file.path(source_dir, "Target_Level_Conditional_Performance.csv"))

imputation <- read.csv(imputation_file, check.names = FALSE,
                       stringsAsFactors = FALSE)
imputation$date <- as.Date(imputation$date)
clean_dates <- imputation$date[imputation$imputed_cells_in_window == 0L]
clean_robustness <- do.call(rbind, lapply(c("conditional_gacv", "oos_check_loss"),
                                         function(metric) {
  do.call(rbind, lapply(spec_order, function(specification) {
    w <- if (metric == "conditional_gacv") wide_gacv(specification) else
      wide_daily(specification)
    w <- w[w$date %in% clean_dates, ]
    do.call(rbind, lapply(names(contrast_definitions), function(id) {
      def <- contrast_definitions[[id]]
      differential <- w[[def$baseline]] - w[[def$better]]
      data.frame(
        metric = metric, specification = specification,
        specification_label = unname(spec_labels[specification]),
        contrast_id = id, contrast = def$label,
        loss_reduction = mean(differential),
        relative_loss_reduction = mean(differential) / mean(w[[def$baseline]]),
        clean_dates = nrow(w), stringsAsFactors = FALSE
      )
    }))
  }))
}))
write_csv(clean_robustness,
          file.path(table_dir, "Table_S5_19_Clean_Window_Robustness.csv"))

calibration_table <- model_summary[, c(
  "specification", "specification_label", "model", "model_label",
  "n_dates", "hit_rate", "hit_hac90_se", "hit_ci_low", "hit_ci_high",
  "hit_test_p_value"
)]
calibration_table$p_holm_4 <- ave(
  calibration_table$hit_test_p_value, calibration_table$specification,
  FUN = function(p) p.adjust(p, method = "holm")
)
write_csv(calibration_table,
          file.path(table_dir, "Table_S5_20_Quantile_Calibration.csv"))

input_manifest <- data.frame(
  file = basename(required_inputs),
  size_bytes = as.numeric(file.info(required_inputs)$size),
  md5 = unname(tools::md5sum(required_inputs)),
  stringsAsFactors = FALSE
)
write_csv(input_manifest, file.path(qa_dir, "input_manifest_md5.csv"))

primary_contrasts <- contrast_table[
  contrast_table$specification == "same_day_conditional", ]
strict_contrasts <- contrast_table[
  contrast_table$specification == "all_predictors_lagged", ]
qa <- data.frame(
  check = c(
    "primary_date_count", "strict_date_count", "target_count",
    "primary_contrast_family", "strict_contrast_family", "timing_contrasts",
    "macro_predictions_identical_on_common_dates",
    "finite_model_summary", "finite_target_source", "shapley_additivity"
  ),
  value = c(
    length(unique(pred$date[pred$specification == "same_day_conditional"])),
    length(unique(pred$date[pred$specification == "all_predictors_lagged"])),
    length(unique(pred$target)), nrow(primary_contrasts), nrow(strict_contrasts),
    nrow(timing_contrasts),
    max(abs(same_wide$macro_only - lagged_wide$macro_only)),
    sum(is.finite(model_summary$mean_standardized_check_loss)),
    sum(is.finite(figure_source$skill_score)),
    max(abs(with(shapley_table,
                 ave(shapley_loss_reduction, specification, FUN = sum) -
                   total_joint_improvement)))
  ),
  expected = c(2252, 2251, 11, 3, 3, 2, 0, 8, 108, 0),
  status = c(
    if (length(unique(pred$date[pred$specification == "same_day_conditional"])) == 2252L) "PASS" else "FAIL",
    if (length(unique(pred$date[pred$specification == "all_predictors_lagged"])) == 2251L) "PASS" else "FAIL",
    if (length(unique(pred$target)) == 11L) "PASS" else "FAIL",
    if (nrow(primary_contrasts) == 3L) "PASS" else "FAIL",
    if (nrow(strict_contrasts) == 3L) "PASS" else "FAIL",
    if (nrow(timing_contrasts) == 2L) "PASS" else "FAIL",
    if (max(abs(same_wide$macro_only - lagged_wide$macro_only)) < 1e-14)
      "PASS" else "FAIL",
    if (all(is.finite(model_summary$mean_standardized_check_loss))) "PASS" else "FAIL",
    if (nrow(figure_source) == 108L && all(is.finite(figure_source$skill_score)))
      "PASS" else "FAIL",
    if (max(abs(with(shapley_table,
                     ave(shapley_loss_reduction, specification, FUN = sum) -
                       total_joint_improvement))) < 1e-12) "PASS" else "FAIL"
  ),
  stringsAsFactors = FALSE
)
write_csv(qa, file.path(qa_dir, "analysis_QA.csv"))
if (any(qa$status == "FAIL")) stop("Analysis QA failed.")

writeLines(capture.output(sessionInfo()), file.path(qa_dir, "session_info_analysis.txt"))
message("Section 5.4 statistical analysis completed: ", bundle_dir)
