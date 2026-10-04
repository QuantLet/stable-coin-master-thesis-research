# ==== FRM_SC_portfolios.R ==============================================
# Dynamic long-only portfolios vs singles
s_opt <- 63; rf <- 0; rebal_every <- 1
dates_ret <- dates[-1]
R <- as.matrix(stock_return); mode(R) <- 'numeric'; tickers <- colnames(R); P <- ncol(R)

ANN_DAYS <- 252
ann_mu   <- function(x) mean(x, na.rm = TRUE) * ANN_DAYS
ann_vol  <- function(x) sd(x,   na.rm = TRUE) * sqrt(ANN_DAYS)
sharpe   <- function(x) if (sd(x, na.rm=TRUE) > 0) ann_mu(x)/ann_vol(x) else NA_real_

safe_inv <- function(S){ S <- as.matrix(S); S[!is.finite(S)] <- 0; S + diag(1e-6, ncol(S)) }

solve_gmv_longonly <- function(S){
  p <- ncol(S); Dmat <- 2 * (S + t(S))/2; dvec <- rep(0, p); Amat <- cbind(rep(1, p), diag(p)); bvec <- c(1, rep(0, p)); meq <- 1
  out <- tryCatch(solve.QP(Dmat, dvec, Amat, bvec, meq = meq), error=function(e) NULL)
  if (is.null(out)) return(rep(1/p, p))
  w <- out$solution; w[w < 0] <- 0; w/sum(w)
}
solve_tangency_longonly <- function(mu, S, rf=0){
  p <- length(mu); ex <- mu - rf; S <- safe_inv(S); w <- tryCatch(as.numeric(ginv(S) %*% ex), error=function(e) rep(1, p))
  w[!is.finite(w)] <- 0; w[w < 0] <- 0; if (sum(w) == 0) w <- rep(1/p, p); w/sum(w)
}

w_gmv <- matrix(NA_real_, nrow = nrow(R), ncol = P)
w_tan <- matrix(NA_real_, nrow = nrow(R), ncol = P)
for (t in seq_len(nrow(R))) {
  if (t < s_opt) next
  if (((t - s_opt) %% rebal_every) != 0) { w_gmv[t, ] <- w_gmv[t-1, ]; w_tan[t, ] <- w_tan[t-1, ]; next }
  Rw <- R[(t - s_opt + 1):t, , drop = FALSE]; mu <- colMeans(Rw, na.rm = TRUE); S <- stats::cov(Rw, use = 'pairwise.complete.obs'); S[!is.finite(S)] <- 0
  w_gmv[t, ] <- solve_gmv_longonly(S); w_tan[t, ] <- solve_tangency_longonly(mu, S, rf)
}
w0 <- matrix(1/P, nrow = nrow(R), ncol = P); w_gmv[is.na(w_gmv)] <- w0[is.na(w_gmv)]; w_tan[is.na(w_tan)] <- w0[is.na(w_tan)]

rp_gmv <- rowSums(R * w_gmv); rp_tan <- rowSums(R * w_tan); rp_eq <- rowMeans(R)
to_wealth <- function(r) exp(cumsum(r))
W_eq <- to_wealth(rp_eq); W_gmv <- to_wealth(rp_gmv); W_tan <- to_wealth(rp_tan); W_single <- apply(R, 2, to_wealth)

finals <- W_single[nrow(W_single), ]; best_id <- which.max(finals); worst_id <- which.min(finals)
best_tk <- tickers[best_id]; worst_tk <- tickers[worst_id]
W_best  <- W_single[, best_id]; W_worst <- W_single[, worst_id]
qband <- t(apply(W_single, 1, function(x) stats::quantile(x, c(0.10, 0.50, 0.90), na.rm = TRUE)))
colnames(qband) <- c('p10','p50','p90')

cols <- c('Equal-weight'='grey40','GMV (long-only)'='blue','Tangency (long-only)'='red','Singles (median)'='grey60')
cols[paste0('Best single: ',  best_tk)]  <- '#2ca02c'; cols[paste0('Worst single: ', worst_tk)] <- '#d62728'

# Wealth
png(file.path(output_path, 'CAPM_Portfolio_vs_Singles_Wealth.png'), width = 1400, height = 900, bg = 'transparent')
print(
  ggplot() +
    geom_ribbon(aes(x = dates_ret, ymin = qband[, 'p10'], ymax = qband[, 'p90'], fill = 'Singles (10–90%)'), alpha = 0.15, inherit.aes = FALSE) +
    geom_line(aes(x = dates_ret, y = qband[, 'p50'], color = 'Singles (median)'), linewidth = 0.7, linetype = 'dashed') +
    geom_line(aes(x = dates_ret, y = W_best,  color = paste0('Best single: ',  best_tk)),  linewidth = 0.8) +
    geom_line(aes(x = dates_ret, y = W_worst, color = paste0('Worst single: ', worst_tk)), linewidth = 0.6) +
    geom_line(aes(x = dates_ret, y = W_eq,  color = 'Equal-weight'),        linewidth = 1.1) +
    geom_line(aes(x = dates_ret, y = W_gmv, color = 'GMV (long-only)'),     linewidth = 1.1) +
    geom_line(aes(x = dates_ret, y = W_tan, color = 'Tangency (long-only)'),linewidth = 1.1) +
    scale_color_manual(values = cols, breaks = names(cols)) +
    scale_fill_manual(values = c('Singles (10–90%)' = 'grey60')) +
    scale_x_date(date_breaks = '3 months', date_labels = '%b %Y') +
    labs(title = 'Cumulative Wealth — Dynamic Stablecoin Portfolios vs Single Coins', x = 'Date', y = 'Wealth (start = 1)', color = NULL, fill = NULL) +
    theme_transparent_bottom + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
); dev.off()

# Rolling volatility
roll_vol <- function(x, k){ n <- length(x); out <- rep(NA_real_, n); if (n < k) return(out); for (i in k:n) out[i] <- sd(x[(i-k+1):i], na.rm = TRUE); out }
k_vol <- 21
vol_eq  <- roll_vol(rp_eq,  k_vol); vol_gmv <- roll_vol(rp_gmv, k_vol); vol_tan <- roll_vol(rp_tan, k_vol)
vol_single_med <- apply(R, 2, function(col) roll_vol(col, k_vol)); vol_single_med <- apply(vol_single_med, 1, median, na.rm = TRUE)

png(file.path(output_path, 'CAPM_Portfolio_vs_Singles_RollVol.png'), width = 1400, height = 800, bg = 'transparent')
print(
  ggplot() +
    geom_line(aes(x = dates_ret, y = vol_single_med, color = 'Singles (median)'), linewidth = 0.8, linetype = 'dashed') +
    geom_line(aes(x = dates_ret, y = vol_eq,  color = 'Equal-weight'),        linewidth = 1.0) +
    geom_line(aes(x = dates_ret, y = vol_gmv, color = 'GMV (long-only)'),     linewidth = 1.0) +
    geom_line(aes(x = dates_ret, y = vol_tan, color = 'Tangency (long-only)'),linewidth = 1.0) +
    scale_color_manual(values = c('Equal-weight'='grey40','GMV (long-only)'='blue','Tangency (long-only)'='red','Singles (median)'='grey60')) +
    scale_x_date(date_breaks = '3 months', date_labels = '%b %Y') +
    labs(title = paste0('Rolling ', k_vol, '-day Realized Volatility — Portfolios vs Single Coins'), x = 'Date', y = 'σ (daily)', color = NULL) +
    theme_transparent_bottom + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
); dev.off()

# Stats
DD <- function(wealth){ peak <- cummax(wealth); draw <- (wealth/peak) - 1; min(draw, na.rm = TRUE) }
stats_tbl <- data.frame(
  Strategy  = c('Equal-weight', 'GMV (long-only)', 'Tangency (long-only)', paste0('Best single: ', best_tk)),
  AnnReturn = c(ann_mu(rp_eq), ann_mu(rp_gmv), ann_mu(rp_tan), ann_mu(R[, best_id])),
  AnnVol    = c(ann_vol(rp_eq), ann_vol(rp_gmv), ann_vol(rp_tan), ann_vol(R[, best_id])),
  Sharpe    = c(sharpe(rp_eq),  sharpe(rp_gmv),  sharpe(rp_tan),  sharpe(R[, best_id])),
  MaxDD     = c(DD(W_eq),       DD(W_gmv),       DD(W_tan),       DD(W_best))
)
stats_tbl_out <- stats_tbl; num_cols <- vapply(stats_tbl_out, is.numeric, logical(1))
stats_tbl_out[num_cols] <- lapply(stats_tbl_out[num_cols], function(x) round(x, 4))
write.csv(stats_tbl_out, file.path(output_path, 'CAPM_Portfolio_vs_Singles_Stats.csv'), row.names = FALSE)
