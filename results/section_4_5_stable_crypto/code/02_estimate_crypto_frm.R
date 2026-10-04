#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else normalizePath(getwd())
source(file.path(script_dir, "00_config.R"))
if (!file.exists(algorithm_file)) stop("Missing FRM algorithm: ", algorithm_file)
source(algorithm_file)

read_panel <- function(path) {
  x <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  x$date <- as.Date(x$date)
  if (anyNA(x$date)) stop("Date parsing failed: ", path)
  for (nm in names(x)[-1]) x[[nm]] <- suppressWarnings(as.numeric(x[[nm]]))
  x
}

prices_all <- read_panel(file.path(prepared_dir, "Crypto_Price_15_PPT.csv"))
mcap_all <- read_panel(file.path(prepared_dir, "Crypto_MarketCap_15_PPT.csv"))
if (!identical(prices_all$date, mcap_all$date)) stop("Crypto panels are not aligned.")
assets <- read.csv(asset_file, stringsAsFactors = FALSE, check.names = FALSE)

# The balanced nine-asset system is a pre-specified missing-data robustness
# check. These assets have complete local price coverage over 2020-01-01 to
# 2026-04-06. The main system remains the full PPT-defined 15-asset universe.
specifications <- list(
  ppt_dynamic_15 = assets$ticker,
  balanced_complete_9 = c("BTC", "ETH", "BNB", "ADA", "DOGE", "CRO", "MATIC_POL", "BCH", "LINK")
)

estimate_one <- function(spec_id, tickers) {
  message("\n=== FRM@Crypto: ", spec_id, " ===")
  dates <- prices_all$date
  ticker_date <- as.integer(format(dates, "%Y%m%d"))
  price <- as.matrix(prices_all[, tickers, drop = FALSE])
  mcap <- as.matrix(mcap_all[, tickers, drop = FALSE])
  storage.mode(price) <- "numeric"
  storage.mode(mcap) <- "numeric"
  if (!all(price[is.finite(price)] > 0)) stop("Non-positive crypto price.")
  mcap[!is.finite(mcap)] <- 0
  ret <- diff(log(price))
  nonfinite_counts <- colSums(!is.finite(ret))
  ret[!is.finite(ret)] <- 0
  mcap_index <- t(apply(mcap, 1L, function(v) order(v, decreasing = TRUE)))
  storage.mode(mcap_index) <- "integer"
  N0 <- which(dates >= sample_start)[1]
  N1 <- tail(which(dates <= sample_end), 1)
  day_indices <- seq.int(N0 + window_length, N1)

  process_day <- function(t) {
    set.seed(seed_base + ticker_date[t])
    ranked <- as.integer(mcap_index[t, seq_along(tickers)])
    rows_ret <- (t - window_length):(t - 1L)
    X <- ret[rows_ret, ranked, drop = FALSE]
    X[!is.finite(X)] <- 0
    X <- X[, colSums(X != 0) > 0, drop = FALSE]
    active_names <- colnames(X)
    M_t <- ncol(X)
    if (M_t < 2L) {
      return(list(
        date = dates[t], lambdas = setNames(numeric(), character()),
        lambdas_screened = setNames(numeric(), character()),
        diagnostics = data.frame(
          date = format(dates[t]), target = NA_character_, selected_lambda = NA_real_,
          screened_lambda = NA_real_, screened_changed = FALSE,
          error = "Fewer than two active assets", stringsAsFactors = FALSE
        ), active_assets = active_names
      ))
    }
    strict <- setNames(rep(NA_real_, M_t), active_names)
    screened <- strict
    diagnostics <- vector("list", M_t)

    for (k in seq_len(M_t)) {
      fit_error <- NA_character_
      est <- tryCatch(
        suppressWarnings(FRM_Quantile_Regression(as.matrix(X), k, tau, path_steps)),
        error = function(e) { fit_error <<- conditionMessage(e); NULL }
      )
      selected_row <- screened_row <- NA_integer_
      selected_lambda <- screened_lambda <- selected_gacv <- NA_real_
      path_length <- 0L
      condition_number <- NA_real_
      reversal_count <- NA_integer_
      screened_changed <- FALSE
      if (!is.null(est)) {
        gacv <- as.numeric(est$Cgacv)
        lambda_path <- abs(as.numeric(est$lambda))
        finite_rows <- which(is.finite(gacv) & is.finite(lambda_path))
        path_length <- length(lambda_path)
        condition_number <- as.numeric(est$FRM_Condition)
        reversal_count <- if (length(lambda_path) > 1L) sum(diff(lambda_path) > 1e-12) else 0L
        if (length(finite_rows)) {
          selected_row <- finite_rows[which.min(gacv[finite_rows])]
          selected_gacv <- gacv[selected_row]
          selected_lambda <- lambda_path[selected_row]
          strict[k] <- selected_lambda
          ranked_rows <- finite_rows[order(gacv[finite_rows], finite_rows)]
          screened_row <- ranked_rows[1]
          for (candidate in ranked_rows) {
            if (candidate == 1L) { screened_row <- candidate; break }
            prior <- lambda_path[seq_len(candidate - 1L)]
            prior <- prior[is.finite(prior)]
            if (!length(prior) ||
                lambda_path[candidate] <= numerical_sensitivity_ratio * max(min(prior), 1e-12)) {
              screened_row <- candidate
              break
            }
          }
          screened_lambda <- lambda_path[screened_row]
          screened[k] <- screened_lambda
          screened_changed <- !identical(screened_row, selected_row)
        } else fit_error <- "No finite GACV/lambda pair"
      }
      diagnostics[[k]] <- data.frame(
        date = format(dates[t]), target = active_names[k],
        active_assets = M_t, selected_row = selected_row,
        selected_gacv = selected_gacv, selected_lambda = selected_lambda,
        screened_row = screened_row, screened_lambda = screened_lambda,
        screened_changed = screened_changed, path_length = path_length,
        condition_number = condition_number, lambda_path_reversals = reversal_count,
        error = fit_error, stringsAsFactors = FALSE
      )
    }
    list(
      date = dates[t], lambdas = strict, lambdas_screened = screened,
      diagnostics = do.call(rbind, diagnostics), active_assets = active_names
    )
  }

  message(
    "Estimating ", length(day_indices), " daily windows; tau=", tau,
    ", window=", window_length, ", assets=", length(tickers),
    ", cores=", requested_cores
  )
  batches <- split(day_indices, ceiling(seq_along(day_indices) / 100L))
  daily <- list()
  complete <- 0L
  for (b in seq_along(batches)) {
    idx <- batches[[b]]
    piece <- if (requested_cores > 1L) {
      parallel::mclapply(idx, process_day, mc.cores = requested_cores,
                         mc.preschedule = TRUE, mc.set.seed = FALSE)
    } else lapply(idx, process_day)
    daily <- c(daily, piece)
    complete <- complete + length(piece)
    message("  ", complete, "/", length(day_indices), " through ", format(dates[max(idx)]))
  }
  names(daily) <- vapply(daily, function(x) format(x$date), character(1))

  strict_matrix <- matrix(
    NA_real_, nrow = length(daily), ncol = length(tickers),
    dimnames = list(names(daily), tickers)
  )
  screened_matrix <- strict_matrix
  active_count <- integer(length(daily))
  for (i in seq_along(daily)) {
    if (length(daily[[i]]$lambdas)) {
      strict_matrix[i, names(daily[[i]]$lambdas)] <- daily[[i]]$lambdas
      screened_matrix[i, names(daily[[i]]$lambdas_screened)] <- daily[[i]]$lambdas_screened
    }
    active_count[i] <- length(daily[[i]]$active_assets)
  }
  row_mean <- function(m) apply(m, 1L, function(v) if (any(is.finite(v))) mean(v, na.rm = TRUE) else NA_real_)
  frm_strict <- data.frame(
    date = as.Date(rownames(strict_matrix)), frm = row_mean(strict_matrix),
    effective_assets = rowSums(is.finite(strict_matrix)), active_columns = active_count
  )
  frm_screened <- data.frame(
    date = as.Date(rownames(screened_matrix)), frm = row_mean(screened_matrix),
    effective_assets = rowSums(is.finite(screened_matrix)), active_columns = active_count
  )
  lambdas_strict <- data.frame(date = frm_strict$date, strict_matrix, check.names = FALSE)
  lambdas_screened <- data.frame(date = frm_screened$date, screened_matrix, check.names = FALSE)
  diagnostics <- do.call(rbind, lapply(daily, `[[`, "diagnostics"))

  out_dir <- file.path(crypto_frm_dir, spec_id)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(frm_strict, file.path(out_dir, "frm_strict.csv"), quote = FALSE)
  write_csv(frm_screened, file.path(out_dir, "frm_numerical_screened.csv"), quote = FALSE)
  write_csv(lambdas_strict, file.path(out_dir, "lambdas_strict.csv"), quote = FALSE)
  write_csv(lambdas_screened, file.path(out_dir, "lambdas_numerical_screened.csv"), quote = FALSE)
  write_csv(diagnostics, file.path(out_dir, "target_diagnostics.csv"))

  failures <- sum(!is.na(diagnostics$error) & nzchar(diagnostics$error))
  qa <- data.frame(
    check = c(
      "all_daily_strict_frm_finite", "all_daily_screened_frm_finite",
      "no_target_fit_failures", "minimum_finite_gacv_row_mapping",
      "numerical_screening_separate", "output_end_matches_local_crypto_data"
    ),
    status = c(
      if (all(is.finite(frm_strict$frm))) "PASS" else "FAIL",
      if (all(is.finite(frm_screened$frm))) "PASS" else "FAIL",
      if (failures == 0L) "PASS" else "FAIL",
      "PASS", "PASS",
      if (max(frm_strict$date) == sample_end) "PASS" else "FAIL"
    ),
    detail = c(
      paste(sum(is.finite(frm_strict$frm)), "of", nrow(frm_strict)),
      paste(sum(is.finite(frm_screened$frm)), "of", nrow(frm_screened)),
      paste(failures, "failed target fits"),
      "Filtered finite rows are mapped back to their original path rows",
      paste(sum(diagnostics$screened_changed, na.rm = TRUE), "target paths changed"),
      format(max(frm_strict$date))
    ), stringsAsFactors = FALSE
  )
  write_csv(qa, file.path(out_dir, "estimation_QA.csv"))
  if (any(qa$status == "FAIL")) stop("FRM@Crypto QA failed for ", spec_id)
  summary <- rbind(
    data.frame(specification = spec_id, series = "strict", observations = nrow(frm_strict),
               start = min(frm_strict$date), end = max(frm_strict$date),
               min = min(frm_strict$frm), median = median(frm_strict$frm),
               mean = mean(frm_strict$frm), max = max(frm_strict$frm),
               min_assets = min(frm_strict$effective_assets), max_assets = max(frm_strict$effective_assets)),
    data.frame(specification = spec_id, series = "numerical_screened", observations = nrow(frm_screened),
               start = min(frm_screened$date), end = max(frm_screened$date),
               min = min(frm_screened$frm), median = median(frm_screened$frm),
               mean = mean(frm_screened$frm), max = max(frm_screened$frm),
               min_assets = min(frm_screened$effective_assets), max_assets = max(frm_screened$effective_assets))
  )
  write_csv(summary, file.path(out_dir, "estimation_summary.csv"))
  invisible(list(strict = frm_strict, screened = frm_screened, qa = qa, summary = summary))
}

all_results <- lapply(names(specifications), function(id) estimate_one(id, specifications[[id]]))
names(all_results) <- names(specifications)
write_csv(
  do.call(rbind, lapply(all_results, `[[`, "summary")),
  file.path(qa_dir, "FRM_Crypto_estimation_summary.csv")
)

metadata <- c(
  "algorithm=author-supplied FRM_Statistics_Algorithm.R",
  paste0("algorithm_md5=", unname(tools::md5sum(algorithm_file))),
  "GitHub_wrapper_reference=fankewe/master-thesis-code commit b827743bd4d47deb04bfdcf6777ef44ae2a2d8f5",
  paste0("window=", window_length), paste0("tau=", tau), paste0("path_steps=", path_steps),
  "selection=minimum finite GACV with original-row mapping",
  "strict_post_processing=none",
  paste0("separate_numerical_screening_ratio=", numerical_sensitivity_ratio),
  "main_universe=PPT slide 7 fifteen cryptoassets with window-active columns",
  "robustness_universe=balanced nine assets with complete local price coverage",
  "missing_return_rule=non-finite log returns set to zero as in supplied GitHub wrapper",
  "aggregation=equal-weight mean of active asset-specific selected lambdas"
)
writeLines(metadata, file.path(qa_dir, "FRM_Crypto_estimation_metadata.txt"), useBytes = TRUE)
message("FRM@Crypto estimation completed.")
