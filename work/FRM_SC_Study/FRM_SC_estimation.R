# ==== FRM_SC_estimation.R ==============================================
N0 <- idx_start_at_or_after(ticker, date_start)
N1 <- idx_end_at_or_before(ticker, date_end)
stopifnot(!is.na(N0), !is.na(N1), N1 >= N0)

FRM_individ <- list()
J_dynamic   <- numeric(0)
idx_out <- 0L

for (t in N0:N1) {
  if (t < (N0 + s)) next
  idx_out <- idx_out + 1L
  J_eff <- min(J, ncol(stock_return))
  biggest_index <- as.integer(mktcap_index[t, 2:(J_eff + 1), drop = FALSE])
  rows_ret <- (t - s):(t - 1)
  data <- cbind(stock_return[rows_ret, biggest_index, drop = FALSE],
                if (!is.null(macro_return)) macro_return[rows_ret, , drop = FALSE])
  data[!is.finite(data)] <- 0
  data <- data[, colSums(data != 0) > 0, drop = FALSE]
  M_t <- ncol(data); J_t <- M_t - M_macro; J_dynamic[idx_out] <- J_t
  if (J_t <= 0) { FRM_individ[[idx_out]] <- matrix(NA_real_, 1, 0); next }
  adj_matrix <- matrix(0, M_t, M_t)
  est_lambda_t <- vector()
  for (k in 1:M_t) {
    est <- tryCatch(FRM_Quantile_Regression(as.matrix(data), k, tau, I), error = function(e) NULL)
    if (is.null(est)) { est_lambda_t <- c(est_lambda_t, NA_real_); next }
    gacv <- est$Cgacv
    k_best <- if (!any(is.finite(gacv))) length(gacv) else which(gacv == min(gacv[is.finite(gacv)], na.rm = TRUE))[1]
    est_lambda <- abs(data.matrix(est$lambda[k_best]))
    est_beta   <- t(as.matrix(est$beta[k_best, ]))
    adj_matrix[k, -k] <- est_beta
    est_lambda_t      <- c(est_lambda_t, est_lambda)
  }
  est_lambda_t <- t(data.frame(est_lambda_t[1:J_t]))
  colnames(est_lambda_t) <- colnames(data)[1:J_t]
  FRM_individ[[idx_out]] <- est_lambda_t
  colnames(adj_matrix) <- colnames(data); rownames(adj_matrix) <- colnames(data)
  write.csv(adj_matrix, file.path(output_path, 'Adj_Matrices', paste0('adj_matrix_', ticker[t], '.csv')), quote = FALSE)
}

# Name snapshots
if (length(FRM_individ)) {
  vary_start <- (N0 + s)
  snap_dates <- as.Date(as.character(ticker[vary_start:N1]), '%Y%m%d')
  names(FRM_individ) <- format(snap_dates[seq_along(FRM_individ)], '%Y-%m-%d')
}

# ---- Fixed universe for GIF ----
N0_fixed <- idx_start_at_or_after(ticker, date_start_fixed)
N1_fixed <- idx_end_at_or_before(ticker, date_end_fixed)
FRM_individ_fixed <- NULL; J_eff_fixed <- NULL

if (!is.na(N0_fixed) && !is.na(N1_fixed) && (N1_fixed - N0_fixed + 1) >= s) {
  J_eff_fixed <- min(J, ncol(stock_return))
  biggest_index_fixed <- as.integer(mktcap_index[N0_fixed, 2:(J_eff_fixed + 1), drop = FALSE])
  M_J <- J_eff_fixed + M_macro
  N_fixed <- N1_fixed - N0_fixed + 1
  FRM_individ_fixed <- matrix(NA_real_, N_fixed, J_eff_fixed + 1)
  FRM_individ_fixed[, 1] <- ticker[N0_fixed:N1_fixed]
  for (t in N0_fixed:N1_fixed) {
    rows_ret <- (t - s):(t - 1)
    data_fixed <- cbind(
      stock_return[rows_ret, biggest_index_fixed, drop = FALSE],
      if (!is.null(macro_return)) macro_return[rows_ret, , drop = FALSE]
    )
    data_fixed[!is.finite(data_fixed)] <- 0
    if (all(colSums(data_fixed != 0) > 0)) {
      adj_matrix_fixed <- matrix(0, M_J, M_J)
      for (k in 1:M_J) {
        est_fixed <- tryCatch(FRM_Quantile_Regression(as.matrix(data_fixed), k, tau, I), error = function(e) NULL)
        if (is.null(est_fixed)) next
        gacv <- est_fixed$Cgacv
        k_best <- if (!any(is.finite(gacv))) length(gacv) else which(gacv == min(gacv[is.finite(gacv)], na.rm = TRUE))[1]
        est_lambda_fixed <- abs(data.matrix(est_fixed$lambda[k_best]))
        est_beta_fixed   <- t(as.matrix(est_fixed$beta[k_best, ]))
        adj_matrix_fixed[k, -k] <- est_beta_fixed
        if (k <= J_eff_fixed) FRM_individ_fixed[t - N0_fixed + 1, k + 1] <- est_lambda_fixed
      }
      colnames(adj_matrix_fixed) <- colnames(data_fixed); rownames(adj_matrix_fixed) <- colnames(data_fixed)
      write.csv(adj_matrix_fixed, file.path(output_path, 'Adj_Matrices/Fixed', paste0('adj_matrix_', ticker[t], '.csv')), quote = FALSE)
    }
  }
  colnames(FRM_individ_fixed) <- c('date', colnames(data_fixed)[1:J_eff_fixed])
  write.csv(FRM_individ_fixed, file.path(output_path, 'Lambda/Fixed',
          paste0('lambdas_fixed_', date_start_fixed, '_', date_end_fixed, '.csv')),
          row.names = FALSE, quote = FALSE)
} else {
  warning('Fixed window insufficient for s; skipping fixed-company estimation.')
}

# ---- Save FRM history RDS (append if exists) ----
rds_path <- file.path(output_path, 'Lambda', paste0('FRM_', channel, '.rds'))
FRM_history <- if (file.exists(rds_path)) c(readRDS(rds_path), FRM_individ) else FRM_individ
FRM_history <- FRM_history[order(as.Date(names(FRM_history)))]
saveRDS(FRM_history, rds_path)
