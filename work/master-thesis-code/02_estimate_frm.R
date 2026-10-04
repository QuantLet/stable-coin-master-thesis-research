#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else {
  normalizePath(getwd())
}
source(file.path(script_dir, "00_config.R"))
source(algorithm_file)

stage_file <- file.path(results_dir, "stage_20_validated_inputs.rds")
if (!file.exists(stage_file)) stop("Run 01_validate_inputs.R first.")
stage <- readRDS(stage_file)
dates <- stage$dates
ticker <- stage$ticker
stock_return <- stage$stock_return
macro_return <- stage$macro_return
mktcap_index <- stage$mktcap_index

N0 <- which(dates >= date_start_source)[1]
N1 <- tail(which(dates <= date_end_source), 1)
if (!is.finite(N0) || !is.finite(N1) || N0 + window_length > N1) stop("Invalid estimation range.")
day_indices <- seq.int(N0 + window_length, N1)
J_eff <- min(n_stablecoins, ncol(stock_return))

process_day <- function(t) {
  set.seed(seed_base + ticker[t])
  biggest_index <- as.integer(mktcap_index[t, seq_len(J_eff)])
  rows_ret <- (t - window_length):(t - 1L)
  selected_coin_names <- colnames(stock_return)[biggest_index]
  X <- cbind(
    stock_return[rows_ret, biggest_index, drop = FALSE],
    macro_return[rows_ret, , drop = FALSE]
  )
  X[!is.finite(X)] <- 0
  X <- X[, colSums(X != 0) > 0, drop = FALSE]
  M_t <- ncol(X)
  coin_pos <- which(colnames(X) %in% selected_coin_names)

  adjacency <- matrix(0, nrow = M_t, ncol = M_t,
                      dimnames = list(colnames(X), colnames(X)))
  adjacency_sensitivity <- adjacency
  selected_lambdas <- setNames(rep(NA_real_, M_t), colnames(X))
  sensitivity_lambdas <- setNames(rep(NA_real_, M_t), colnames(X))
  diagnostics <- vector("list", M_t)

  for (k in seq_len(M_t)) {
    fit_error <- NA_character_
    est <- tryCatch(
      suppressWarnings(FRM_Quantile_Regression(as.matrix(X), k, tau, path_steps)),
      error = function(e) {
        fit_error <<- conditionMessage(e)
        NULL
      }
    )

    selected_row <- NA_integer_
    selected_step <- NA_integer_
    selected_gacv <- NA_real_
    selected_lambda <- NA_real_
    sensitivity_row <- NA_integer_
    sensitivity_lambda <- NA_real_
    sensitivity_rejected_raw <- FALSE
    path_length <- 0L
    condition_number <- NA_real_
    reversal_count <- NA_integer_

    if (!is.null(est)) {
      gacv <- as.numeric(est$Cgacv)
      lambda_path <- abs(as.numeric(est$lambda))
      finite_rows <- which(is.finite(gacv) & is.finite(lambda_path))
      path_length <- length(lambda_path)
      condition_number <- as.numeric(est$FRM_Condition)
      reversal_count <- if (length(lambda_path) > 1L) {
        sum(diff(lambda_path) > 1e-12, na.rm = TRUE)
      } else 0L

      # This is the intended minimum-GACV rule. Mapping through finite_rows
      # repairs the filtered-index bug in the supplied wrapper without changing
      # the quantile-Lasso estimator or its objective.
      if (length(finite_rows)) {
        selected_row <- finite_rows[which.min(gacv[finite_rows])]
        selected_step <- selected_row - 1L
        selected_gacv <- gacv[selected_row]
        selected_lambda <- lambda_path[selected_row]
        beta <- as.numeric(est$beta[selected_row, ])
        if (length(beta) == M_t - 1L) adjacency[k, -k] <- beta
        selected_lambdas[k] <- selected_lambda

        # Separately labelled diagnostic sensitivity. Candidates are ranked by
        # GACV; a later candidate is rejected only when its lambda exceeds 20
        # times the smallest finite lambda already reached earlier in the path.
        # This rule is never used for the strict baseline above.
        ranked_rows <- finite_rows[order(gacv[finite_rows], finite_rows)]
        sensitivity_row <- ranked_rows[1]
        for (candidate in ranked_rows) {
          if (candidate == 1L) {
            sensitivity_row <- candidate
            break
          }
          prior <- lambda_path[seq_len(candidate - 1L)]
          prior <- prior[is.finite(prior)]
          if (!length(prior) ||
              lambda_path[candidate] <= numerical_sensitivity_ratio * max(min(prior), 1e-12)) {
            sensitivity_row <- candidate
            break
          }
        }
        sensitivity_lambda <- lambda_path[sensitivity_row]
        sensitivity_rejected_raw <- !identical(sensitivity_row, selected_row)
        beta_sensitivity <- as.numeric(est$beta[sensitivity_row, ])
        if (length(beta_sensitivity) == M_t - 1L) {
          adjacency_sensitivity[k, -k] <- beta_sensitivity
        }
        sensitivity_lambdas[k] <- sensitivity_lambda
      } else {
        fit_error <- "No finite GACV/lambda pair"
      }
    }

    diagnostics[[k]] <- data.frame(
      date = format(dates[t]),
      target = colnames(X)[k],
      target_type = if (colnames(X)[k] %in% selected_coin_names) "stablecoin" else "macro",
      selected_row = selected_row,
      selected_path_step = selected_step,
      path_length = path_length,
      selected_gacv = selected_gacv,
      selected_lambda = selected_lambda,
      sensitivity_row = sensitivity_row,
      sensitivity_lambda = sensitivity_lambda,
      sensitivity_rejected_raw = sensitivity_rejected_raw,
      condition_number = condition_number,
      lambda_path_reversals = reversal_count,
      error = fit_error,
      stringsAsFactors = FALSE
    )
  }

  coin_lambdas <- selected_lambdas[coin_pos]
  coin_lambdas_sensitivity <- sensitivity_lambdas[coin_pos]
  list(
    date = dates[t],
    coin_lambdas = coin_lambdas,
    coin_lambdas_sensitivity = coin_lambdas_sensitivity,
    adjacency = adjacency,
    adjacency_sensitivity = adjacency_sensitivity,
    diagnostics = do.call(rbind, diagnostics),
    selected_coins = selected_coin_names
  )
}

message(
  "Estimating ", length(day_indices), " daily windows with ", requested_cores,
  " core(s); window=", window_length, ", tau=", tau,
  ", path steps=", path_steps, "."
)
batch_size <- 100L
batches <- split(day_indices, ceiling(seq_along(day_indices) / batch_size))
daily <- list()
completed <- 0L
for (b in seq_along(batches)) {
  idx <- batches[[b]]
  piece <- if (requested_cores > 1L) {
    parallel::mclapply(
      idx, process_day, mc.cores = requested_cores,
      mc.preschedule = TRUE, mc.set.seed = FALSE
    )
  } else {
    lapply(idx, process_day)
  }
  daily <- c(daily, piece)
  completed <- completed + length(piece)
  message("  ", completed, "/", length(day_indices), " through ", format(dates[max(idx)]))
}
names(daily) <- vapply(daily, function(x) format(x$date), character(1))

lambda_matrix <- matrix(
  NA_real_, nrow = length(daily), ncol = length(stablecoin_names),
  dimnames = list(names(daily), stablecoin_names)
)
sensitivity_matrix <- lambda_matrix
for (i in seq_along(daily)) {
  v <- daily[[i]]$coin_lambdas
  if (length(v)) lambda_matrix[i, names(v)] <- as.numeric(v)
  vs <- daily[[i]]$coin_lambdas_sensitivity
  if (length(vs)) sensitivity_matrix[i, names(vs)] <- as.numeric(vs)
}
effective_coins <- rowSums(is.finite(lambda_matrix))
frm <- apply(lambda_matrix, 1L, function(v) {
  v <- v[is.finite(v)]
  if (length(v)) mean(v) else NA_real_
})
frm_sensitivity <- apply(sensitivity_matrix, 1L, function(v) {
  v <- v[is.finite(v)]
  if (length(v)) mean(v) else NA_real_
})

FRM_index <- data.frame(
  date = as.Date(rownames(lambda_matrix)),
  frm = as.numeric(frm),
  effective_coins = as.integer(effective_coins),
  stringsAsFactors = FALSE
)
lambdas_wide <- data.frame(
  date = FRM_index$date,
  lambda_matrix,
  check.names = FALSE,
  row.names = NULL
)
lambdas_sensitivity_wide <- data.frame(
  date = FRM_index$date,
  sensitivity_matrix,
  check.names = FALSE,
  row.names = NULL
)
FRM_index_sensitivity <- data.frame(
  date = FRM_index$date,
  frm = as.numeric(frm_sensitivity),
  effective_coins = rowSums(is.finite(sensitivity_matrix)),
  stringsAsFactors = FALSE
)
target_diagnostics <- do.call(rbind, lapply(daily, `[[`, "diagnostics"))
adjacency <- lapply(daily, `[[`, "adjacency")
adjacency_sensitivity <- lapply(daily, `[[`, "adjacency_sensitivity")
selected_coins <- lapply(daily, `[[`, "selected_coins")

write.csv(FRM_index, file.path(results_dir, "FRM_Stable_index_20260531.csv"),
          row.names = FALSE, quote = FALSE)
write.csv(lambdas_wide, file.path(results_dir, "lambdas_wide_20260531.csv"),
          row.names = FALSE, quote = FALSE, na = "")
write.csv(FRM_index_sensitivity,
          file.path(results_dir, "FRM_Stable_index_20260531_numerical_sensitivity.csv"),
          row.names = FALSE, quote = FALSE)
write.csv(lambdas_sensitivity_wide,
          file.path(results_dir, "lambdas_wide_20260531_numerical_sensitivity.csv"),
          row.names = FALSE, quote = FALSE, na = "")
write.csv(target_diagnostics,
          file.path(results_dir, "FRM_target_diagnostics_20260531.csv"),
          row.names = FALSE, quote = TRUE, na = "")
saveRDS(adjacency, file.path(results_dir, "adjacency_matrices_20260531.rds"),
        compress = "xz")
saveRDS(adjacency_sensitivity,
        file.path(results_dir, "adjacency_matrices_20260531_numerical_sensitivity.rds"),
        compress = "xz")
saveRDS(selected_coins, file.path(results_dir, "selected_coins_20260531.rds"),
        compress = "xz")

if (export_adjacency_csv) {
  for (nm in names(adjacency)) {
    write.csv(
      adjacency[[nm]],
      file.path(adjacency_dir, paste0("adj_matrix_", gsub("-", "", nm), ".csv")),
      row.names = TRUE,
      quote = FALSE
    )
  }
}
write.csv(
  adjacency[[length(adjacency)]],
  file.path(adjacency_dir, "adj_matrix_20260531.csv"),
  row.names = TRUE,
  quote = FALSE
)
write.csv(
  adjacency_sensitivity[[length(adjacency_sensitivity)]],
  file.path(adjacency_dir, "adj_matrix_20260531_numerical_sensitivity.csv"),
  row.names = TRUE,
  quote = FALSE
)

finite_frm <- is.finite(FRM_index$frm)
if (!any(finite_frm)) stop("No finite FRM estimates were produced.")
max_i <- which.max(FRM_index$frm)
min_i <- which.min(FRM_index$frm)
summary_table <- data.frame(
  metric = c(
    "first_estimation_date", "last_estimation_date", "observations",
    "missing_frm_days", "minimum", "minimum_date", "first_quartile",
    "median", "mean", "third_quartile", "maximum", "maximum_date",
    "minimum_effective_coins", "maximum_effective_coins"
  ),
  value = c(
    format(min(FRM_index$date)), format(max(FRM_index$date)), nrow(FRM_index),
    sum(!finite_frm), format(FRM_index$frm[min_i], digits = 15),
    format(FRM_index$date[min_i]),
    format(unname(quantile(FRM_index$frm, 0.25, na.rm = TRUE)), digits = 15),
    format(median(FRM_index$frm, na.rm = TRUE), digits = 15),
    format(mean(FRM_index$frm, na.rm = TRUE), digits = 15),
    format(unname(quantile(FRM_index$frm, 0.75, na.rm = TRUE)), digits = 15),
    format(FRM_index$frm[max_i], digits = 15), format(FRM_index$date[max_i]),
    min(FRM_index$effective_coins), max(FRM_index$effective_coins)
  ),
  stringsAsFactors = FALSE
)
write.csv(summary_table, file.path(results_dir, "FRM_summary_20260531.csv"),
          row.names = FALSE, quote = TRUE)

sensitivity_max_i <- which.max(FRM_index_sensitivity$frm)
sensitivity_summary <- data.frame(
  metric = c(
    "label", "observations", "minimum", "median", "mean", "maximum",
    "maximum_date", "target_paths_changed", "index_days_changed"
  ),
  value = c(
    "Numerical sensitivity only; not the strict baseline",
    nrow(FRM_index_sensitivity),
    format(min(FRM_index_sensitivity$frm), digits = 15),
    format(median(FRM_index_sensitivity$frm), digits = 15),
    format(mean(FRM_index_sensitivity$frm), digits = 15),
    format(FRM_index_sensitivity$frm[sensitivity_max_i], digits = 15),
    format(FRM_index_sensitivity$date[sensitivity_max_i]),
    sum(target_diagnostics$sensitivity_rejected_raw, na.rm = TRUE),
    sum(abs(FRM_index_sensitivity$frm - FRM_index$frm) > 1e-15, na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)
write.csv(
  sensitivity_summary,
  file.path(results_dir, "FRM_numerical_sensitivity_summary_20260531.csv"),
  row.names = FALSE,
  quote = TRUE
)

risk_ecdf <- ecdf(FRM_index$frm[finite_frm])
risk_percentile <- 100 * risk_ecdf(FRM_index$frm)
risk_label <- cut(
  risk_percentile,
  breaks = c(-Inf, 20, 40, 60, 80, Inf), right = FALSE,
  labels = c("1. Low risk", "2. General risk", "3. Elevated risk",
             "4. High risk", "5. Severe risk")
)
risk_out <- data.frame(
  date = FRM_index$date,
  frm = FRM_index$frm,
  empirical_percentile = round(risk_percentile, 2),
  risk_level = as.character(risk_label),
  stringsAsFactors = FALSE
)
write.csv(risk_out, file.path(results_dir, "FRM_risk_levels_20260531.csv"),
          row.names = FALSE, quote = TRUE)
risk_thresholds <- data.frame(
  percentile = c(20, 40, 60, 80),
  frm_threshold = as.numeric(quantile(FRM_index$frm, c(.2, .4, .6, .8),
                                      na.rm = TRUE, type = 7))
)
write.csv(risk_thresholds,
          file.path(results_dir, "FRM_risk_thresholds_20260531.csv"),
          row.names = FALSE, quote = FALSE)

model_failures <- sum(nzchar(target_diagnostics$error[!is.na(target_diagnostics$error)]))
qa <- data.frame(
  check = c(
    "all_daily_frm_finite", "eleven_coin_lambdas_each_day",
    "no_target_model_failures", "output_end_is_2026_05_31",
    "minimum_gacv_used_without_post_filter"
  ),
  status = c(
    if (all(finite_frm)) "PASS" else "FAIL",
    if (all(effective_coins == n_stablecoins)) "PASS" else "FAIL",
    if (model_failures == 0L) "PASS" else "FAIL",
    if (max(FRM_index$date) == date_end_source) "PASS" else "FAIL",
    "PASS"
  ),
  detail = c(
    sprintf("%d/%d finite", sum(finite_frm), nrow(FRM_index)),
    sprintf("effective coin count range %d-%d", min(effective_coins), max(effective_coins)),
    sprintf("%d failed target fits", model_failures),
    format(max(FRM_index$date)),
    "No clipping, winsorization, smoothing, rescaling, or custom path rejection"
  ),
  stringsAsFactors = FALSE
)
write.csv(qa, file.path(results_dir, "FRM_estimation_QA_20260531.csv"),
          row.names = FALSE, quote = TRUE)

run_object <- list(
  FRM_index = FRM_index,
  lambdas_wide = lambdas_wide,
  target_diagnostics = target_diagnostics,
  adjacency = adjacency,
  adjacency_sensitivity = adjacency_sensitivity,
  selected_coins = selected_coins,
  summary = summary_table,
  qa = qa,
  settings = list(
    date_start_source = date_start_source,
    date_end_source = date_end_source,
    window_length = window_length,
    tau = tau,
    path_steps = path_steps,
    n_stablecoins = n_stablecoins,
    n_macro_factors = n_macro_factors,
    seed_base = seed_base,
    numerical_sensitivity_ratio = numerical_sensitivity_ratio,
    cores = requested_cores
  )
)
saveRDS(run_object, file.path(results_dir, "stage_30_frm_strict_20260531.rds"),
        compress = "xz")

metadata <- c(
  paste0("run_time_utc=", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste0("r_version=", R.version.string),
  paste0("input_start=", format(min(dates))),
  paste0("input_end=", format(max(dates))),
  paste0("estimation_start=", format(min(FRM_index$date))),
  paste0("estimation_end=", format(max(FRM_index$date))),
  paste0("rolling_window=", window_length),
  paste0("tau=", tau),
  paste0("path_steps=", path_steps),
  paste0("stablecoins=", n_stablecoins),
  paste0("macro_factors=", n_macro_factors),
  "macro_timing=contemporaneous_daily_log_changes_as_in_supplied_code",
  "gacv_rule=minimum_finite_gacv_with_correct_original_row_mapping",
  "post_estimation_filtering=none",
  paste0("separate_numerical_sensitivity_ratio=", numerical_sensitivity_ratio),
  paste0("algorithm_md5=", unname(tools::md5sum(algorithm_file))),
  paste0("price_md5=", unname(tools::md5sum(price_file))),
  paste0("market_cap_md5=", unname(tools::md5sum(mktcap_file))),
  paste0("macro_md5=", unname(tools::md5sum(macro_file)))
)
writeLines(metadata, file.path(results_dir, "run_metadata_20260531.txt"), useBytes = TRUE)
message("Strict FRM estimation completed and written to: ", results_dir)
