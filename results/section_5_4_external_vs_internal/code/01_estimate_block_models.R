#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1L])))
} else normalizePath(getwd())
source(file.path(script_dir, "00_config.R"))
source(algorithm_file)

locf_past <- function(x) {
  x <- as.numeric(x)
  last_value <- NA_real_
  for (i in seq_along(x)) {
    if (is.finite(x[i])) last_value <- x[i]
    else if (is.finite(last_value)) x[i] <- last_value
  }
  x
}

robust_location_scale <- function(x) {
  center <- median(x, na.rm = TRUE)
  scale <- mad(x, center = center, constant = 1.4826, na.rm = TRUE)
  if (!is.finite(scale) || scale <= 1e-12) scale <- sd(x, na.rm = TRUE)
  if (!is.finite(scale) || scale <= 1e-12) scale <- 1
  c(center = center, scale = scale)
}

check_loss <- function(y, q, p = tau) {
  u <- y - q
  (p - as.numeric(u < 0)) * u
}

parse_date <- function(x) {
  x <- as.character(x)
  slash <- grepl("/", x, fixed = TRUE)
  out <- as.Date(rep(NA_character_, length(x)))
  if (any(slash)) out[slash] <- suppressWarnings(as.Date(x[slash], "%m/%d/%Y"))
  if (any(!slash)) out[!slash] <- suppressWarnings(as.Date(x[!slash], "%Y-%m-%d"))
  if (anyNA(out)) stop("Date parsing failed.")
  out
}

message("Building the frozen Chapter 4 data universe.")
calendar <- seq(date_start, date_end, by = "day")

peg <- read.csv(peg_file, check.names = FALSE, stringsAsFactors = FALSE)
needed_peg <- c("date", "symbol", "signed_deviation_bps")
if (!all(needed_peg %in% names(peg))) stop("Peg panel columns are incomplete.")
peg$date <- parse_date(peg$date)
peg$signed_deviation_bps <- suppressWarnings(as.numeric(peg$signed_deviation_bps))
if (!setequal(unique(peg$symbol), coin_order)) stop("Unexpected stablecoin universe.")

deviation_raw <- matrix(
  NA_real_, nrow = length(calendar), ncol = length(coin_order),
  dimnames = list(format(calendar), coin_order)
)
row_id <- match(peg$date, calendar)
col_id <- match(peg$symbol, coin_order)
if (anyNA(row_id) || anyNA(col_id)) stop("Peg observations fall outside the declared universe.")
if (anyDuplicated(data.frame(row_id, col_id))) stop("Duplicate stablecoin-date observations.")
deviation_raw[cbind(row_id, col_id)] <- peg$signed_deviation_bps / 10000
imputed_cell <- !is.finite(deviation_raw)
deviation <- apply(deviation_raw, 2L, locf_past)
if (is.null(dim(deviation))) deviation <- matrix(deviation, ncol = length(coin_order))
colnames(deviation) <- coin_order
rownames(deviation) <- format(calendar)
if (any(!is.finite(deviation[-1L, , drop = FALSE]))) {
  stop("Past-only stablecoin filling leaves non-finite observations after the first day.")
}

macro <- read.csv(macro_file, check.names = FALSE, stringsAsFactors = FALSE)
if (ncol(macro) != length(macro_order) + 1L) stop("Unexpected macro panel width.")
macro[[1L]] <- parse_date(macro[[1L]])
names(macro)[1L] <- "date"
if (!identical(macro$date, calendar)) stop("Macro dates do not match the daily calendar.")
if (!identical(names(macro)[-1L], macro_order)) stop("Unexpected macro ordering.")
macro_level <- as.matrix(macro[, macro_order, drop = FALSE])
storage.mode(macro_level) <- "numeric"
macro_level <- apply(macro_level, 2L, locf_past)
if (is.null(dim(macro_level))) macro_level <- matrix(macro_level, ncol = length(macro_order))
colnames(macro_level) <- macro_order
if (any(!is.finite(macro_level)) || any(macro_level <= 0)) {
  stop("Past-filled macro levels must be finite and positive.")
}
macro_change <- rbind(rep(0, ncol(macro_level)), diff(log(macro_level)))
lagged_macro <- rbind(rep(0, ncol(macro_change)),
                      macro_change[-nrow(macro_change), , drop = FALSE])
colnames(lagged_macro) <- paste0("L1_", macro_order)
rownames(lagged_macro) <- format(calendar)

imputation_check <- read.csv(imputation_file, check.names = FALSE,
                             stringsAsFactors = FALSE)
imputation_check$date <- as.Date(imputation_check$date)
declared_output_dates <- seq(output_start, date_end, by = "day")
reconstructed_imputation <- data.frame(
  date = declared_output_dates,
  imputed_cells_in_window = vapply(declared_output_dates, function(d) {
    t <- match(d, calendar)
    sum(imputed_cell[(t - window_length + 1L):t, , drop = FALSE])
  }, integer(1L)),
  imputed_days_in_window = vapply(declared_output_dates, function(d) {
    t <- match(d, calendar)
    sum(rowSums(imputed_cell[(t - window_length + 1L):t, , drop = FALSE]) > 0)
  }, integer(1L)),
  stringsAsFactors = FALSE
)
imputation_check <- imputation_check[match(declared_output_dates, imputation_check$date), ]
if (anyNA(imputation_check$date) ||
    !identical(reconstructed_imputation$imputed_cells_in_window,
               imputation_check$imputed_cells_in_window) ||
    !identical(reconstructed_imputation$imputed_days_in_window,
               imputation_check$imputed_days_in_window)) {
  stop("Reconstructed missing-data windows do not match the frozen Chapter 4 audit.")
}

scale_matrix <- function(X) {
  pars <- vapply(seq_len(ncol(X)), function(j) robust_location_scale(X[, j]),
                 numeric(2L))
  centers <- pars["center", ]
  scales <- pars["scale", ]
  Z <- sweep(sweep(X, 2L, centers, "-"), 2L, scales, "/")
  list(z = Z, center = centers, scale = scales)
}

fit_one <- function(y_train, x_train, x_test, seed) {
  X <- cbind(target = y_train, x_train)
  scaled <- scale_matrix(X)
  y_test_scale <- unname(scaled$scale[1L])
  y_test_center <- unname(scaled$center[1L])
  x_test_scaled <- (as.numeric(x_test) - scaled$center[-1L]) / scaled$scale[-1L]
  set.seed(seed)
  fit_error <- NA_character_
  est <- tryCatch(
    suppressWarnings(FRM_Quantile_Regression(
      as.matrix(scaled$z), 1L, tau, path_steps
    )),
    error = function(e) {
      fit_error <<- conditionMessage(e)
      NULL
    }
  )
  if (is.null(est)) {
    return(list(
      q_scaled = NA_real_, q_raw = NA_real_, selected_row = NA_integer_,
      selected_gacv = NA_real_, selected_lambda = NA_real_, active_size = NA_integer_,
      target_center = y_test_center, target_scale = y_test_scale,
      error = fit_error
    ))
  }
  finite_rows <- which(is.finite(est$Cgacv) & is.finite(est$lambda))
  if (!length(finite_rows)) {
    return(list(
      q_scaled = NA_real_, q_raw = NA_real_, selected_row = NA_integer_,
      selected_gacv = NA_real_, selected_lambda = NA_real_, active_size = NA_integer_,
      target_center = y_test_center, target_scale = y_test_scale,
      error = "No finite GACV/lambda row"
    ))
  }
  selected_row <- finite_rows[which.min(est$Cgacv[finite_rows])]
  beta <- as.numeric(est$beta[selected_row, ])
  q_scaled <- as.numeric(est$beta0[selected_row] + sum(beta * x_test_scaled))
  list(
    q_scaled = q_scaled,
    q_raw = y_test_center + y_test_scale * q_scaled,
    selected_row = selected_row,
    selected_gacv = as.numeric(est$Cgacv[selected_row]),
    selected_lambda = abs(as.numeric(est$lambda[selected_row])),
    active_size = sum(abs(beta) > active_tolerance),
    target_center = y_test_center,
    target_scale = y_test_scale,
    error = NA_character_
  )
}

benchmark_one <- function(y_train) {
  ls <- robust_location_scale(y_train)
  yz <- (y_train - ls["center"]) / ls["scale"]
  q_scaled <- sort(yz)[floor(length(yz) * tau) + 1L]
  list(
    q_scaled = unname(q_scaled),
    q_raw = unname(ls["center"] + ls["scale"] * q_scaled),
    selected_row = 1L, selected_gacv = mean(check_loss(yz, q_scaled)),
    selected_lambda = NA_real_, active_size = 0L,
    target_center = unname(ls["center"]), target_scale = unname(ls["scale"]),
    error = NA_character_
  )
}

prediction_row <- function(date, target, specification, model, observed,
                           estimate) {
  y_scaled <- (observed - estimate$target_center) / estimate$target_scale
  data.frame(
    date = date,
    target = target,
    specification = specification,
    model = model,
    observed_deviation = observed,
    observed_standardized = y_scaled,
    predicted_quantile = estimate$q_raw,
    predicted_standardized = estimate$q_scaled,
    check_loss = check_loss(y_scaled, estimate$q_scaled),
    hit = as.integer(y_scaled <= estimate$q_scaled),
    selected_row = estimate$selected_row,
    selected_path_step = estimate$selected_row - 1L,
    selected_gacv = estimate$selected_gacv,
    selected_lambda = estimate$selected_lambda,
    active_size = estimate$active_size,
    target_center = estimate$target_center,
    target_scale = estimate$target_scale,
    error = estimate$error,
    stringsAsFactors = FALSE
  )
}

process_date <- function(t) {
  d <- calendar[t]
  train <- (t - window_length):(t - 1L)
  y_test_all <- deviation[t, ]
  primary_coin_train <- deviation[train, , drop = FALSE]
  primary_coin_test <- deviation[t, ]
  lagged_coin_train <- deviation[train - 1L, , drop = FALSE]
  lagged_coin_test <- deviation[t - 1L, ]
  macro_train <- lagged_macro[train, , drop = FALSE]
  macro_test <- lagged_macro[t, ]
  available_specifications <- "same_day_conditional"
  if (all(is.finite(lagged_coin_train)) && all(is.finite(lagged_coin_test))) {
    available_specifications <- c(available_specifications, "all_predictors_lagged")
  }
  out <- vector("list", length(coin_order) * 4L * length(available_specifications))
  counter <- 0L

  for (j in seq_along(coin_order)) {
    target <- coin_order[j]
    y_train <- deviation[train, j]
    y_test <- y_test_all[j]
    other <- setdiff(seq_along(coin_order), j)
    common_seed <- as.integer((seed_base + as.integer(format(d, "%Y%m%d")) +
      j * 100003L) %% .Machine$integer.max)
    benchmark <- benchmark_one(y_train)
    macro_fit <- fit_one(y_train, macro_train, macro_test, common_seed)

    for (specification in available_specifications) {
      coin_train <- if (specification == "same_day_conditional") {
        primary_coin_train[, other, drop = FALSE]
      } else lagged_coin_train[, other, drop = FALSE]
      coin_test <- if (specification == "same_day_conditional") {
        primary_coin_test[other]
      } else lagged_coin_test[other]

      coin_fit <- fit_one(y_train, coin_train, coin_test, common_seed)
      joint_fit <- fit_one(
        y_train,
        cbind(coin_train, macro_train),
        c(coin_test, macro_test),
        common_seed
      )
      fits <- list(
        benchmark = benchmark, macro_only = macro_fit,
        coin_only = coin_fit, joint = joint_fit
      )
      for (model in names(fits)) {
        counter <- counter + 1L
        out[[counter]] <- prediction_row(
          d, target, specification, model, y_test, fits[[model]]
        )
      }
    }
  }
  do.call(rbind, out)
}

# Exact one-window replication of the frozen coefficient panel. This establishes
# that the reconstructed deviations, one-day-lagged macro changes, scaling,
# window labelling and GACV row mapping reproduce the Chapter 4 system.
validate_frozen_alignment <- function() {
  d <- output_start
  t <- match(d, calendar)
  rows <- (t - window_length + 1L):t
  X <- cbind(deviation[rows, , drop = FALSE],
             lagged_macro[rows, , drop = FALSE])
  Z <- scale_matrix(X)$z
  set.seed(seed_base + as.integer(format(d, "%Y%m%d")))
  est <- suppressWarnings(FRM_Quantile_Regression(Z, 1L, tau, path_steps))
  finite_rows <- which(is.finite(est$Cgacv) & is.finite(est$lambda))
  selected_row <- finite_rows[which.min(est$Cgacv[finite_rows])]
  reproduced <- setNames(as.numeric(est$beta[selected_row, ]), colnames(Z)[-1L])
  frozen <- read.csv(gzfile(frozen_coefficient_file), nrows = 15L,
                     check.names = FALSE, stringsAsFactors = FALSE)
  frozen_vector <- setNames(frozen$coefficient, frozen$predictor)
  error <- max(abs(reproduced[names(frozen_vector)] - frozen_vector))
  data.frame(
    check = "first target-window coefficients reproduce frozen primary panel",
    date = d, target = "USDC", selected_row = selected_row,
    maximum_absolute_error = error,
    status = if (is.finite(error) && error < 1e-12) "PASS" else "FAIL",
    stringsAsFactors = FALSE
  )
}

alignment_qa <- validate_frozen_alignment()
write_csv(alignment_qa, file.path(qa_dir, "frozen_input_alignment_QA.csv"))
if (alignment_qa$status != "PASS") stop("Frozen Chapter 4 alignment check failed.")

output_dates <- declared_output_dates
if (is.finite(max_dates)) output_dates <- head(output_dates, max_dates)
output_indices <- match(output_dates, calendar)
batch_size <- 25L
batches <- split(output_indices, ceiling(seq_along(output_indices) / batch_size))
checkpoint_dir <- file.path(source_dir, "checkpoints")
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

message(
  "Estimating ", length(output_indices), " out-of-sample dates, ",
  length(coin_order), " targets, two information-timing specifications and ",
  "three Quantile-Lasso block models with ", requested_cores, " core(s)."
)

batch_paths <- character(length(batches))
for (b in seq_along(batches)) {
  idx <- batches[[b]]
  batch_path <- file.path(
    checkpoint_dir,
    sprintf("predictions_%04d_%s_%s.rds", b, format(calendar[min(idx)]),
            format(calendar[max(idx)]))
  )
  batch_paths[b] <- batch_path
  if (!file.exists(batch_path)) {
    piece <- if (requested_cores > 1L) {
      parallel::mclapply(
        idx, process_date, mc.cores = requested_cores,
        mc.preschedule = TRUE, mc.set.seed = FALSE
      )
    } else lapply(idx, process_date)
    saveRDS(do.call(rbind, piece), batch_path, compress = "xz")
  }
  message("  ", sum(lengths(batches[seq_len(b)])), "/", length(output_indices),
          " through ", format(calendar[max(idx)]))
}

predictions <- do.call(rbind, lapply(batch_paths, readRDS))
predictions$date <- as.Date(predictions$date, origin = "1970-01-01")
predictions <- predictions[order(
  predictions$specification, predictions$date,
  match(predictions$target, coin_order),
  match(predictions$model, c("benchmark", "macro_only", "coin_only", "joint"))
), ]
rownames(predictions) <- NULL

strict_dates <- sum(output_dates > output_start)
expected_rows <- length(coin_order) * 4L * (length(output_dates) + strict_dates)
failures <- !is.na(predictions$error) & nzchar(predictions$error)
qa <- data.frame(
  check = c(
    "expected_prediction_rows", "all_losses_finite", "no_model_failures",
    "eleven_targets", "four_models", "two_timing_specifications",
    "full_sample_dates", "frozen_alignment"
  ),
  status = c(
    if (nrow(predictions) == expected_rows) "PASS" else "FAIL",
    if (all(is.finite(predictions$check_loss))) "PASS" else "FAIL",
    if (!any(failures)) "PASS" else "FAIL",
    if (length(unique(predictions$target)) == 11L) "PASS" else "FAIL",
    if (length(unique(predictions$model)) == 4L) "PASS" else "FAIL",
    if ((max(output_dates) == output_start &&
         identical(unique(predictions$specification), "same_day_conditional")) ||
        (max(output_dates) > output_start &&
         length(unique(predictions$specification)) == 2L)) "PASS" else "FAIL",
    if (is.infinite(max_dates) && length(unique(predictions$date)) == 2252L) "PASS" else
      if (length(unique(predictions$date)) == length(output_dates)) "PASS_TEST_RUN" else "FAIL",
    alignment_qa$status
  ),
  detail = c(
    paste(nrow(predictions), "of", expected_rows),
    paste(sum(is.finite(predictions$check_loss)), "of", nrow(predictions)),
    paste(sum(failures), "failed fits"),
    paste(length(unique(predictions$target)), "targets"),
    paste(unique(predictions$model), collapse = "; "),
    paste(unique(predictions$specification), collapse = "; "),
    paste(min(predictions$date), "to", max(predictions$date)),
    format(alignment_qa$maximum_absolute_error, scientific = TRUE)
  ),
  stringsAsFactors = FALSE
)

write_csv_gz(predictions, file.path(source_dir, "OOS_Block_Model_Predictions.csv.gz"))
saveRDS(predictions, file.path(source_dir, "OOS_Block_Model_Predictions.rds"),
        compress = "xz")
write_csv(qa, file.path(qa_dir, "estimation_QA.csv"))
writeLines(capture.output(sessionInfo()), file.path(qa_dir, "session_info_estimation.txt"))

if (any(qa$status == "FAIL")) stop("Estimation QA failed.")
message("Block-model estimation completed: ", bundle_dir)
