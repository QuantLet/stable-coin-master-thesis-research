#!/usr/bin/env Rscript
# Matched-sample microstructure extension of the frozen Chapter 5.4 joint model.
# The Quantile-Lasso algorithm, peg panel, macro transforms, 90-day windows,
# median/MAD scaling and strict finite-GACV selection are unchanged.
options(stringsAsFactors = FALSE, digits = 15, warn = 1, scipen = 999)

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(file_arg) != 1L) stop("Run this file with Rscript.")
code_dir <- dirname(normalizePath(sub("^--file=", "", file_arg)))
bundle_dir <- dirname(code_dir)
study_dir <- dirname(dirname(bundle_dir))
input_dir <- file.path(bundle_dir, "source_data", "input")
out_dir <- file.path(bundle_dir, "source_data")
qa_dir <- file.path(bundle_dir, "qa")
checkpoint_dir <- file.path(out_dir, "checkpoints")
invisible(lapply(c(out_dir, qa_dir, checkpoint_dir), dir.create,
                 recursive = TRUE, showWarnings = FALSE))

peg_file <- file.path(input_dir, "peg_panel_reference_adjusted.csv")
macro_file <- file.path(input_dir, "Stable_Macro_20260531.csv")
baseline_file <- file.path(input_dir, "OOS_Block_Model_Predictions.rds")
algorithm_file <- file.path(input_dir, "FRM_Statistics_Algorithm.R")
onchain_file <- file.path(input_dir, "stablecoin_onchain_daily_20200101_20260531.csv")
required <- c(peg_file, macro_file, baseline_file, algorithm_file, onchain_file)
if (!all(file.exists(required))) {
  stop("Missing input: ", paste(required[!file.exists(required)], collapse = "; "))
}

local_library <- file.path(study_dir, "work", "section_4_5_stable_crypto", "r_library")
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))
source(algorithm_file)

coin_order <- c("USDC", "BUSD", "GUSD", "EURS", "IDRT", "USDT",
                "sUSD", "PAXG", "TUSD", "USDP", "DAI")
macro_order <- c("BV010082.Index", "CVIX.Index", "DXY.Curncy",
                 "SPX.Index", "VIX.Index")
micro_order <- c("L1_curve_3pool_imbalance",
                 "L1_log1p_curve_3pool_swap_count",
                 "L1_log1p_uni_usdc_usdt_inventory_pm25ticks_usd")
date_start <- as.Date("2020-01-01")
date_end <- as.Date("2026-05-31")
first_test_date <- as.Date("2022-04-02")
window_length <- 90L
tau <- 0.05
path_steps <- 25L
active_tolerance <- 1e-10
seed_base <- 20260531L

requested_cores <- suppressWarnings(as.integer(Sys.getenv("FRM_MICRO_CORES", "4")))
if (!is.finite(requested_cores) || requested_cores < 1L) requested_cores <- 1L
if (.Platform$OS.type == "windows") requested_cores <- 1L
max_dates <- suppressWarnings(as.integer(Sys.getenv("FRM_MICRO_MAX_DATES", "")))
if (!is.finite(max_dates) || max_dates < 1L) max_dates <- Inf

parse_date <- function(x) {
  x <- as.character(x)
  slash <- grepl("/", x, fixed = TRUE)
  out <- as.Date(rep(NA_character_, length(x)))
  if (any(slash)) out[slash] <- as.Date(x[slash], "%m/%d/%Y")
  if (any(!slash)) out[!slash] <- as.Date(x[!slash], "%Y-%m-%d")
  if (anyNA(out)) stop("Date parsing failed.")
  out
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

robust_location_scale <- function(x) {
  center <- median(x, na.rm = TRUE)
  scale <- mad(x, center = center, constant = 1.4826, na.rm = TRUE)
  if (!is.finite(scale) || scale <= 1e-12) scale <- sd(x, na.rm = TRUE)
  if (!is.finite(scale) || scale <= 1e-12) scale <- 1
  c(center = center, scale = scale)
}

scale_matrix <- function(X) {
  pars <- vapply(seq_len(ncol(X)), function(j) robust_location_scale(X[, j]),
                 numeric(2L))
  Z <- sweep(sweep(X, 2L, pars["center", ], "-"), 2L, pars["scale", ], "/")
  list(z = Z, center = pars["center", ], scale = pars["scale", ])
}

check_loss <- function(y, q, p = tau) {
  u <- y - q
  (p - as.numeric(u < 0)) * u
}

calendar <- seq(date_start, date_end, by = "day")
peg <- read.csv(peg_file, check.names = FALSE)
stopifnot(all(c("date", "symbol", "signed_deviation_bps") %in% names(peg)),
          setequal(unique(peg$symbol), coin_order))
peg$date <- parse_date(peg$date)
peg$signed_deviation_bps <- as.numeric(peg$signed_deviation_bps)
deviation_raw <- matrix(NA_real_, length(calendar), length(coin_order),
                        dimnames = list(as.character(calendar), coin_order))
row_id <- match(peg$date, calendar)
col_id <- match(peg$symbol, coin_order)
stopifnot(!anyNA(row_id), !anyNA(col_id),
          !anyDuplicated(data.frame(row_id, col_id)))
deviation_raw[cbind(row_id, col_id)] <- peg$signed_deviation_bps / 10000
deviation <- apply(deviation_raw, 2L, locf_past)
colnames(deviation) <- coin_order
rownames(deviation) <- as.character(calendar)

macro <- read.csv(macro_file, check.names = FALSE)
macro[[1L]] <- parse_date(macro[[1L]])
names(macro)[1L] <- "date"
stopifnot(identical(macro$date, calendar),
          identical(names(macro)[-1L], macro_order))
macro_level <- as.matrix(macro[, macro_order, drop = FALSE])
storage.mode(macro_level) <- "numeric"
macro_level <- apply(macro_level, 2L, locf_past)
stopifnot(all(is.finite(macro_level)), all(macro_level > 0))
macro_change <- rbind(rep(0, ncol(macro_level)), diff(log(macro_level)))
lagged_macro <- rbind(rep(0, ncol(macro_change)),
                      macro_change[-nrow(macro_change), , drop = FALSE])
colnames(lagged_macro) <- paste0("L1_", macro_order)

onchain <- read.csv(onchain_file, check.names = FALSE)
onchain$date <- as.Date(onchain$date)
stopifnot(!anyNA(onchain$date), !anyDuplicated(onchain$date))
onchain <- onchain[match(calendar, onchain$date), ]
stopifnot(identical(onchain$date, calendar))
source_ok <- onchain$curve_3pool_hours == 24 &
  onchain$uni_usdc_usdt_liquidity_snapshot_hours == 24
micro_raw <- cbind(
  curve_3pool_imbalance = onchain$curve_3pool_imbalance_eod,
  log1p_curve_3pool_swap_count = log1p(onchain$curve_3pool_swap_count),
  log1p_uni_usdc_usdt_inventory_pm25ticks_usd =
    log1p(onchain$uni_usdc_usdt_inventory_pm25ticks_usd_eod)
)
micro_raw[!source_ok | is.na(source_ok), ] <- NA_real_
lagged_micro <- rbind(rep(NA_real_, ncol(micro_raw)),
                      micro_raw[-nrow(micro_raw), , drop = FALSE])
colnames(lagged_micro) <- micro_order

all_test_dates <- seq(first_test_date, date_end, by = "day")
test_dates <- if (is.finite(max_dates)) head(all_test_dates, max_dates) else all_test_dates
test_indices <- match(test_dates, calendar)
stopifnot(!anyNA(test_indices),
          all(vapply(test_indices, function(t) {
            train <- (t - window_length):(t - 1L)
            all(is.finite(lagged_micro[c(train, t), , drop = FALSE])) &&
              all(is.finite(deviation[c(train - 1L, train, t), , drop = FALSE]))
          }, logical(1L))))

input_audit <- data.frame(
  item = c("stablecoins", "baseline_predictors_per_target",
           "added_micro_predictors", "window_days", "quantile",
           "first_test_date", "last_test_date", "test_dates",
           "onchain_source_days_24_hours"),
  value = c(length(coin_order), length(coin_order) - 1L + length(macro_order),
            length(micro_order), window_length, tau,
            as.character(min(test_dates)), as.character(max(test_dates)),
            length(test_dates), sum(source_ok & onchain$date >= as.Date("2022-01-01"),
                                   na.rm = TRUE))
)
write.csv(input_audit, file.path(qa_dir, "input_coverage.csv"), row.names = FALSE)

fit_one <- function(y_train, x_train, x_test, observed, seed) {
  X <- cbind(target = y_train, x_train)
  scaled <- scale_matrix(X)
  x_test_z <- (as.numeric(x_test) - scaled$center[-1L]) / scaled$scale[-1L]
  set.seed(seed)
  error <- NA_character_
  fit <- tryCatch(
    suppressWarnings(FRM_Quantile_Regression(as.matrix(scaled$z), 1L,
                                             tau, path_steps)),
    error = function(e) { error <<- conditionMessage(e); NULL }
  )
  if (is.null(fit)) return(list(error = error))
  finite_rows <- which(is.finite(fit$Cgacv) & is.finite(fit$lambda))
  if (!length(finite_rows)) return(list(error = "No finite GACV/lambda row"))
  selected_row <- finite_rows[which.min(fit$Cgacv[finite_rows])]
  beta <- setNames(as.numeric(fit$beta[selected_row, ]), colnames(x_train))
  q_scaled <- as.numeric(fit$beta0[selected_row] + sum(beta * x_test_z))
  y_scaled <- as.numeric((observed - scaled$center[1L]) / scaled$scale[1L])
  list(
    error = NA_character_,
    selected_row = selected_row,
    selected_gacv = as.numeric(fit$Cgacv[selected_row]),
    selected_lambda = abs(as.numeric(fit$lambda[selected_row])),
    active_size = sum(abs(beta) > active_tolerance),
    target_center = as.numeric(scaled$center[1L]),
    target_scale = as.numeric(scaled$scale[1L]),
    predicted_quantile = as.numeric(scaled$center[1L] +
                                      scaled$scale[1L] * q_scaled),
    check_loss = as.numeric(check_loss(y_scaled, q_scaled)),
    hit = as.integer(y_scaled <= q_scaled),
    micro_beta = beta[micro_order]
  )
}

process_date <- function(t) {
  d <- calendar[t]
  train <- (t - window_length):(t - 1L)
  out <- vector("list", length(coin_order) * 2L)
  k <- 0L
  for (j in seq_along(coin_order)) {
    target <- coin_order[j]
    other <- setdiff(seq_along(coin_order), j)
    y_train <- deviation[train, j]
    observed <- deviation[t, j]
    seed <- as.integer((seed_base + as.integer(format(d, "%Y%m%d")) +
                          j * 100003L) %% .Machine$integer.max)
    for (specification in c("same_day_conditional", "all_predictors_lagged")) {
      coin_train <- if (specification == "same_day_conditional") {
        deviation[train, other, drop = FALSE]
      } else deviation[train - 1L, other, drop = FALSE]
      coin_test <- if (specification == "same_day_conditional") {
        deviation[t, other]
      } else deviation[t - 1L, other]
      X <- cbind(coin_train, lagged_macro[train, , drop = FALSE],
                 lagged_micro[train, , drop = FALSE])
      x_test <- c(coin_test, lagged_macro[t, ], lagged_micro[t, ])
      estimate <- fit_one(y_train, X, x_test, observed, seed)
      k <- k + 1L
      if (!is.na(estimate$error)) {
        out[[k]] <- data.frame(date = d, target = target,
                               specification = specification,
                               error = estimate$error)
      } else {
        out[[k]] <- data.frame(
          date = d, target = target, specification = specification,
          observed_deviation = observed,
          predicted_quantile = estimate$predicted_quantile,
          check_loss = estimate$check_loss, hit = estimate$hit,
          selected_row = estimate$selected_row,
          selected_gacv = estimate$selected_gacv,
          selected_lambda = estimate$selected_lambda,
          active_size = estimate$active_size,
          target_center = estimate$target_center,
          target_scale = estimate$target_scale,
          active_micro = sum(abs(estimate$micro_beta) > active_tolerance),
          beta_curve_imbalance = unname(estimate$micro_beta[1L]),
          beta_curve_swap_count = unname(estimate$micro_beta[2L]),
          beta_uni_near_inventory = unname(estimate$micro_beta[3L]),
          error = NA_character_
        )
      }
    }
  }
  do.call(rbind, out)
}

batch_size <- 14L
batches <- split(test_indices, ceiling(seq_along(test_indices) / batch_size))
batch_paths <- character(length(batches))
message("Estimating ", length(test_indices), " dates, ", length(coin_order),
        " targets, two timing specifications; ", requested_cores, " core(s).")
for (b in seq_along(batches)) {
  idx <- batches[[b]]
  path <- file.path(checkpoint_dir,
                    sprintf("augmented_%04d_%s_%s.rds", b,
                            format(calendar[min(idx)]), format(calendar[max(idx)])))
  batch_paths[b] <- path
  if (!file.exists(path)) {
    piece <- if (requested_cores > 1L) {
      parallel::mclapply(idx, process_date, mc.cores = requested_cores,
                         mc.preschedule = TRUE, mc.set.seed = FALSE)
    } else lapply(idx, process_date)
    saveRDS(do.call(rbind, piece), path, compress = "xz")
  }
  message("  ", sum(lengths(batches[seq_len(b)])), "/", length(test_indices),
          " through ", format(calendar[max(idx)]))
}

predictions <- do.call(rbind, lapply(batch_paths, readRDS))
predictions$date <- as.Date(predictions$date, origin = "1970-01-01")
predictions <- predictions[order(predictions$date,
                                  match(predictions$target, coin_order),
                                  predictions$specification), ]
rownames(predictions) <- NULL
failures <- !is.na(predictions$error) & nzchar(predictions$error)
expected_rows <- length(test_dates) * length(coin_order) * 2L
qa <- data.frame(
  check = c("expected_rows", "two_timing_specs", "all_losses_finite",
            "no_fit_failures", "no_missing_micro_coefficients"),
  status = c(if (nrow(predictions) == expected_rows) "PASS" else "FAIL",
             if (setequal(unique(predictions$specification),
                          c("same_day_conditional", "all_predictors_lagged")))
               "PASS" else "FAIL",
             if (all(is.finite(predictions$check_loss))) "PASS" else "FAIL",
             if (!any(failures)) "PASS" else "FAIL",
             if (all(is.finite(as.matrix(predictions[, c(
               "beta_curve_imbalance", "beta_curve_swap_count",
               "beta_uni_near_inventory")])))) "PASS" else "FAIL"),
  detail = c(paste(nrow(predictions), "of", expected_rows),
             paste(unique(predictions$specification), collapse = "; "),
             paste(sum(is.finite(predictions$check_loss)), "finite"),
             paste(sum(failures), "failed fits"),
             paste(nrow(predictions), "coefficient rows"))
)
write.csv(qa, file.path(qa_dir, "estimation_QA.csv"), row.names = FALSE)
saveRDS(predictions, file.path(out_dir, "Augmented_Predictions.rds"), compress = "xz")
write.csv(predictions, gzfile(file.path(out_dir, "Augmented_Predictions.csv.gz")),
          row.names = FALSE, na = "")
writeLines(capture.output(sessionInfo()), file.path(qa_dir, "session_info.txt"))
if (any(qa$status == "FAIL")) stop("Estimation QA failed; inspect estimation_QA.csv.")
message("Estimation completed: ", bundle_dir)
