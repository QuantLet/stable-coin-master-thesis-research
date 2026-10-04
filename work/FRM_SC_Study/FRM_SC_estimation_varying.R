
load(file.path(output_path, "stage_20_loaded.RData"))
FRM_individ <- list(); J_dynamic <- numeric(0); idx_out <- 0L
N0 <- idx_start_at_or_after(ticker, date_start); N1 <- idx_end_at_or_before(ticker, date_end)

for (t in N0:N1) {
  if (t < (N0 + s)) next
  idx_out <- idx_out + 1L
  J_eff <- min(J, M_stock); biggest_index <- as.integer(mktcap_index[t, 2:(J_eff+1), drop=FALSE])
  rows_ret <- (t - s):(t - 1)
  data <- cbind(stock_return[rows_ret, biggest_index, drop=FALSE], macro_return[rows_ret, , drop=FALSE])
  data[!is.finite(data)] <- 0; data <- data[, colSums(data != 0) > 0, drop=FALSE]
  M_t <- ncol(data); J_t <- M_t - M_macro; J_dynamic[idx_out] <- J_t
  if (J_t <= 0) { FRM_individ[[idx_out]] <- matrix(numeric(0), nrow=1); next }
  adj_matrix <- matrix(0, M_t, M_t); est_lambda_t <- c()
  for (k in 1:M_t) {
    est <- tryCatch(FRM_Quantile_Regression(as.matrix(data), k, tau, I), error=function(e) NULL)
    if (is.null(est)) { est_lambda_t <- c(est_lambda_t, NA); next }
    gacv <- est$Cgacv; k_best <- if (!any(is.finite(gacv))) length(gacv) else which.min(gacv[is.finite(gacv)])
    est_lambda <- abs(data.matrix(est$lambda[k_best]))
    est_beta   <- t(as.matrix(est$beta[k_best, ]))
    adj_matrix[k, -k] <- est_beta; est_lambda_t <- c(est_lambda_t, est_lambda)
  }
  est_lambda_t <- t(data.frame(est_lambda_t[1:J_t]))
  colnames(est_lambda_t) <- colnames(data)[1:J_t]; FRM_individ[[idx_out]] <- est_lambda_t
  colnames(adj_matrix) <- colnames(data); rownames(adj_matrix) <- colnames(data)
  write.csv(adj_matrix, file.path(output_path, "Adj_Matrices", paste0("adj_matrix_", ticker[t], ".csv")), quote=FALSE)
}
if (length(FRM_individ)) {
  vary_start <- (N0 + s); snap_dates <- as.Date(as.character(ticker[vary_start:N1]), "%Y%m%d")
  names(FRM_individ) <- format(snap_dates[seq_along(FRM_individ)], "%Y-%m-%d")
}
save(FRM_individ, file = file.path(output_path, "stage_30_varying.RData"))

