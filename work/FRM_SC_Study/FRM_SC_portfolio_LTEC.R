
source("FRM_SC_utils.R"); load(file.path(output_path,"stage_20_loaded.RData"))

suppressPackageStartupMessages({ library(quadprog); library(ggplot2); library(zoo) })

# settings (inherit your global s & tau from config)
s_opt        <- s         # rolling window (e.g., 63)
rebal_every  <- 1         # rebalance every day
Er_target    <- 0.008     # MV target daily return (relaxed if infeasible)
alpha_ltec   <- 0.20      # LTEC variance vs tail exposure weight
tail_q       <- tau       # use your global tau (e.g., 0.05)
min_frac_obs <- 0.95      # asset must have >=95% finite obs in window
MAX_N        <- Inf       # cap number of assets per window (Inf = all)
ga_qtec      <- 0.80      # QTEC-lite: variance vs mean(lambda) tradeoff

dates_ret <- dates[-1]
R_full    <- as.matrix(stock_return); mode(R_full) <- "numeric"
tickers   <- colnames(R_full); if (is.null(tickers)) tickers <- paste0("A", seq_len(ncol(R_full)))
N         <- ncol(R_full); T_all <- nrow(R_full)

# optional lambdas_wide for QTEC-lite
lambda_path_try <- file.path(output_path, "Lambda", "lambdas_wide.csv")
has_lambda <- file.exists(lambda_path_try)
if (has_lambda) {
  lam_raw <- tryCatch(read.csv(lambda_path_try, check.names=FALSE), error=function(e) NULL)
  if (!is.null(lam_raw) && "date" %in% names(lam_raw) && all(tickers %in% names(lam_raw))) {
    lam_raw$date <- as.Date(lam_raw$date)
    lam_idx <- as.Date(dates_ret)
    lam_aligned <- matrix(NA_real_, nrow=length(lam_idx), ncol=N); colnames(lam_aligned) <- tickers
    rowmap <- match(lam_idx, lam_raw$date); matched <- which(is.finite(rowmap))
    if (length(matched)) lam_aligned[matched, ] <- as.matrix(lam_raw[rowmap[matched], tickers, drop=FALSE])
    # forward fill per column
    for (j in seq_len(N)) {
      v <- lam_aligned[, j]; last <- NA_real_
      for (i in seq_along(v)) { if (is.finite(v[i])) last <- v[i] else v[i] <- last }
      lam_aligned[, j] <- v
    }
  } else { has_lambda <- FALSE }
}

# helpers
safe_cov <- function(X){
  S <- stats::cov(X, use="pairwise.complete.obs"); S[!is.finite(S)] <- 0
  S <- (S + t(S))/2; S + diag(1e-6, ncol(S))
}
solve_mv_longonly <- function(mu,S,Er,relax=TRUE){
  n <- length(mu); Dmat <- 2*((S + t(S))/2 + diag(1e-8,n)); dvec <- rep(0,n)
  Amat <- cbind(rep(1,n), mu, diag(n)); bvec <- c(1, Er, rep(0,n)); meq <- 1
  try_solve <- function(tEr){
    bvec[2] <- tEr
    out <- tryCatch(quadprog::solve.QP(Dmat,dvec,Amat,bvec,meq=meq), error=function(e) NULL)
    if (is.null(out)) return(NULL)
    w <- out$solution; w[w<0] <- 0; sw <- sum(w); if (!is.finite(sw) || sw<=0) sw <- 1; w/sw
  }
  w <- try_solve(Er); if (!is.null(w)) return(w)
  if (relax) for (g in seq(Er, stats::median(mu,na.rm=TRUE), length.out=5)) { w <- try_solve(g); if (!is.null(w)) return(w) }
  rep(1/n, n)
}
tail_betas <- function(Rw, tail_q=0.05, ref=NULL){
  if (is.null(ref)) ref <- rowMeans(Rw, na.rm=TRUE)
  qv <- stats::quantile(ref, probs=tail_q, na.rm=TRUE, type=7)
  idx <- which(is.finite(ref) & ref <= qv)
  if (length(idx) < max(20, ncol(Rw)+5)) { k <- max(ceiling(0.10*sum(is.finite(ref))), 20L); ord <- order(ref); idx <- ord[seq_len(min(k,length(ord)))] }
  ref_t <- ref[idx]; vref <- stats::var(ref_t, na.rm=TRUE); if (!is.finite(vref) || vref<=0) vref <- 1e-6
  sapply(seq_len(ncol(Rw)), function(j){ y <- Rw[idx, j]; cxy <- stats::cov(y, ref_t, use="pairwise.complete.obs"); if (!is.finite(cxy)) cxy <- 0; b <- cxy/vref; if (!is.finite(b)) 0 else b })
}
solve_ltec_longonly <- function(S, beta_tail, alpha=0.2){
  n <- length(beta_tail); Dmat <- 2*(alpha*((S + t(S))/2) + diag(1e-8,n)); dvec <- -(1-alpha)*beta_tail
  Amat <- cbind(rep(1,n), diag(n)); bvec <- c(1, rep(0,n)); meq <- 1
  out <- tryCatch(quadprog::solve.QP(Dmat,dvec,Amat,bvec,meq=meq), error=function(e) NULL)
  if (is.null(out)) return(rep(1/n,n))
  w <- out$solution; w[w<0] <- 0; sw <- sum(w); if (!is.finite(sw) || sw<=0) sw <- 1; w/sw
}
solve_qtec_lite <- function(S, l_mu, ga=0.8){
  n <- length(l_mu); Dmat <- 2*(ga*((S + t(S))/2) + diag(1e-8,n)); dvec <- -(1-ga)*as.numeric(l_mu)
  Amat <- cbind(rep(1,n), diag(n)); bvec <- c(1, rep(0,n)); meq <- 1
  out <- tryCatch(quadprog::solve.QP(Dmat,dvec,Amat,bvec,meq=meq), error=function(e) NULL)
  if (is.null(out)) return(rep(1/n,n))
  w <- out$solution; w[w<0] <- 0; sw <- sum(w); if (!is.finite(sw) || sw<=0) sw <- 1; w/sw
}
to_wealth <- function(r) exp(cumsum(r))
roll_vol <- function(x,k){ n <- length(x); out <- rep(NA_real_, n); if (n<k) return(out); for (i in k:n) out[i] <- sd(x[(i-k+1):i], na.rm=TRUE); out }

# storage
W_mv   <- matrix(NA_real_, nrow=T_all, ncol=N); colnames(W_mv)   <- tickers
W_ltec <- matrix(NA_real_, nrow=T_all, ncol=N); colnames(W_ltec) <- tickers
W_qtec <- matrix(NA_real_, nrow=T_all, ncol=N); colnames(W_qtec) <- tickers

for (t in seq_len(T_all)) {
  if (t < s_opt) next
  if (((t - s_opt) %% rebal_every) != 0) {
    if (t > 1) { W_mv[t,] <- W_mv[t-1,]; W_ltec[t,] <- W_ltec[t-1,]; W_qtec[t,] <- W_qtec[t-1,] }
    next
  }
  Rw <- R_full[(t - s_opt + 1):t, , drop=FALSE]

  # largest feasible pool: enough finite obs and non-zero variance
  ok_frac <- vapply(as.data.frame(Rw), function(x) mean(is.finite(x)) >= min_frac_obs, logical(1))
  ok_var  <- vapply(as.data.frame(Rw), function(x){ v <- stats::var(x, na.rm=TRUE); is.finite(v) && v>0 }, logical(1))
  ok <- ok_frac & ok_var
  tick_now <- colnames(Rw)[ok]
  if (length(tick_now) > MAX_N) tick_now <- tick_now[seq_len(MAX_N)]

  if (length(tick_now) < 2) {
    W_mv[t,] <- rep(1/N, N); W_ltec[t,] <- W_mv[t,]; W_qtec[t,] <- W_mv[t,]
    next
  }

  Rsub <- Rw[, tick_now, drop=FALSE]
  Sret <- safe_cov(Rsub)
  mu   <- colMeans(Rsub, na.rm=TRUE)

  # MV
  w_mv <- solve_mv_longonly(mu, Sret, Er=Er_target)

  # LTEC
  btail <- tail_betas(Rsub, tail_q=tail_q, ref=rowMeans(Rsub, na.rm=TRUE))
  w_lt  <- solve_ltec_longonly(Sret, btail, alpha=alpha_ltec)

  # QTEC-lite (only if lambdas exist)
  if (has_lambda) {
    Lsub <- lam_aligned[(t - s_opt + 1):t, tick_now, drop=FALSE]
    l_mu <- colMeans(Lsub, na.rm=TRUE)
    w_qt <- solve_qtec_lite(Sret, l_mu, ga=ga_qtec)
  } else {
    w_qt <- rep(1/length(tick_now), length(tick_now))
  }

  # map to full
  map_full <- function(w_sub){
    v <- rep(0, N); names(v) <- tickers; v[tick_now] <- w_sub; v
  }
  W_mv[t,]   <- map_full(w_mv)
  W_ltec[t,] <- map_full(w_lt)
  W_qtec[t,] <- map_full(w_qt)
}

# forward-fill weights, initialize with equal-weight
ffill <- function(W){
  for (i in seq_len(nrow(W))) {
    if (i==1 && any(!is.finite(W[i,]))) W[i,] <- rep(1/ncol(W), ncol(W))
    if (i>1  && any(!is.finite(W[i,])))  W[i,] <- W[i-1,]
  }
  W
}
W_mv   <- ffill(W_mv)
W_ltec <- ffill(W_ltec)
W_qtec <- ffill(W_qtec)

# portfolio returns & wealth
rp_mv   <- rowSums(R_full * W_mv)
rp_ltec <- rowSums(R_full * W_ltec)
rp_qtec <- rowSums(R_full * W_qtec)

We_mv   <- to_wealth(rp_mv)
We_ltec <- to_wealth(rp_ltec)
We_qtec <- to_wealth(rp_qtec)

# Individual Coins band (for context)
We_single <- apply(R_full, 2, to_wealth)
qband <- t(apply(We_single, 1, function(x) stats::quantile(x, c(0.10,0.50,0.90), na.rm=TRUE)))
colnames(qband) <- c("p10","p50","p90")

# Performance (whole sample)
ANN_DAYS <- 252
ann_mu <- function(x) mean(x, na.rm=TRUE)*ANN_DAYS
ann_vol <- function(x) sd(x,   na.rm=TRUE)*sqrt(ANN_DAYS)
sharpe  <- function(x) if (sd(x,na.rm=TRUE)>0) ann_mu(x)/ann_vol(x) else NA_real_
DD <- function(wealth){ peak <- cummax(wealth); draw <- (wealth/peak) - 1; min(draw, na.rm=TRUE) }

stats_tbl <- data.frame(
  Strategy  = c("QTEC-lite","MV (long-only)","LTEC (long-only)"),
  AnnReturn = c(ann_mu(rp_qtec), ann_mu(rp_mv), ann_mu(rp_ltec)),
  AnnVol    = c(ann_vol(rp_qtec), ann_vol(rp_mv), ann_vol(rp_ltec)),
  Sharpe    = c(sharpe(rp_qtec),  sharpe(rp_mv),  sharpe(rp_ltec)),
  MaxDD     = c(DD(We_qtec),      DD(We_mv),      DD(We_ltec))
)
num_cols <- vapply(stats_tbl, is.numeric, logical(1))
stats_tbl[num_cols] <- lapply(stats_tbl[num_cols], function(x) round(x,4))

# save
write.csv(
  data.frame(date=dates_ret, Wealth_QTEC=We_qtec, Wealth_MV=We_mv, Wealth_LTEC=We_ltec),
  file.path(output_path, "LTEC_like_PortWealth.csv"), row.names=FALSE
)
write.csv(stats_tbl, file.path(output_path, "LTEC_like_PortfolioPerformance.csv"), row.names=FALSE)

# plots (no titles, no x-axis label)
png(file.path(output_path, "LTEC_like_Wealth.png"), width=1400, height=900, bg="transparent")
print(ggplot() +
  geom_ribbon(aes(x=dates_ret, ymin=qband[,"p10"], ymax=qband[,"p90"], fill="Individual Coins (10–90%)"), alpha=0.15) +
  geom_line(aes(x=dates_ret, y=qband[,"p50"], color="Individual Coins (median)"), linewidth=0.7, linetype="dashed") +
  geom_line(aes(x=dates_ret, y=We_mv,   color="MV (long-only)"),     linewidth=1.1) +
  geom_line(aes(x=dates_ret, y=We_ltec, color="LTEC (long-only)"),   linewidth=1.1) +
  geom_line(aes(x=dates_ret, y=We_qtec, color="QTEC-lite"),          linewidth=1.1) +
  scale_color_manual(values=c("MV (long-only)"="grey40","LTEC (long-only)"="blue","QTEC-lite"="red","Individual Coins (median)"="grey60")) +
  scale_fill_manual(values=c("Individual Coins (10–90%)"="grey60")) +
  scale_x_date(date_breaks="3 months", date_labels="%b %Y") +
  labs(title=NULL, x=NULL, y="Wealth (start = 1)", color=NULL, fill=NULL) +
  theme_transparent_bottom + theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)))
dev.off()

k_vol <- 21
vol_q <- roll_vol(rp_qtec, k_vol); vol_m <- roll_vol(rp_mv, k_vol); vol_l <- roll_vol(rp_ltec, k_vol)

png(file.path(output_path, "LTEC_like_RollVol.png"), width=1400, height=800, bg="transparent")
print(ggplot() +
  geom_line(aes(x=dates_ret, y=vol_m, color="MV (long-only)"),   linewidth=1.0) +
  geom_line(aes(x=dates_ret, y=vol_l, color="LTEC (long-only)"), linewidth=1.0) +
  geom_line(aes(x=dates_ret, y=vol_q, color="QTEC-lite"),        linewidth=1.0) +
  scale_color_manual(values=c("MV (long-only)"="grey40","LTEC (long-only)"="blue","QTEC-lite"="red")) +
  scale_x_date(date_breaks="3 months", date_labels="%b %Y") +
  labs(title=NULL, x=NULL, y="σ (daily)", color=NULL) +
  theme_transparent_bottom + theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)))
  
# ==== GMV vs Individual Coins: Evidence & Tests ====

suppressPackageStartupMessages({ library(ggplot2) })

# 1) Risk-adjusted stats vs Individual Coins (per-coin and distribution)
ANN_DAYS <- 365
ann_mu  <- function(x) mean(x,na.rm=TRUE)*ANN_DAYS
ann_vol <- function(x) sd(x,na.rm=TRUE)*sqrt(ANN_DAYS)
sharpe  <- function(x) if (sd(x,na.rm=TRUE)>0) ann_mu(x)/ann_vol(x) else NA_real_
sortino <- function(x){ dn <- x; dn[dn>0] <- 0; d <- sd(dn,na.rm=TRUE)*sqrt(ANN_DAYS); if (!is.finite(d)||d==0) return(NA_real_); ann_mu(x)/d }
ES <- function(x, p=0.05){ # Expected Shortfall on daily returns
  xp <- sort(x[is.finite(x)]); if (!length(xp)) return(NA_real_)
  k <- max(1, floor(p*length(xp))); mean(xp[1:k], na.rm=TRUE)
}
DD <- function(wealth){ peak <- cummax(wealth); draw <- (wealth/peak) - 1; min(draw, na.rm=TRUE) }

# Individual Coins stats
single_names <- colnames(R)
single_stats <- data.frame(
  Coin      = single_names,
  AnnReturn = apply(R, 2, ann_mu),
  AnnVol    = apply(R, 2, ann_vol),
  Sharpe    = apply(R, 2, sharpe),
  Sortino   = apply(R, 2, sortino),
  ES_5      = apply(R, 2, ES, p=0.05),
  MaxDD     = apply(W_single, 2, DD),
  stringsAsFactors = FALSE
)

# GMV stats
gmv_stats <- data.frame(
  Strategy  = "GMV (long-only)",
  AnnReturn = ann_mu(rp_gmv),
  AnnVol    = ann_vol(rp_gmv),
  Sharpe    = sharpe(rp_gmv),
  Sortino   = sortino(rp_gmv),
  ES_5      = ES(rp_gmv, 0.05),
  MaxDD     = DD(W_gmv)
)

# Median single (by Sharpe) for reference in tests
med_single_idx <- order(single_stats$Sharpe)[ceiling(ncol(R)/2)]
rp_med_single  <- R[, med_single_idx]
W_med_single   <- W_single[, med_single_idx, drop=TRUE]

# 2) Time-series dominance: percentile rank of GMV wealth among Individual Coins
# For each day, rank GMV wealth vs the distribution of single-coin wealth
gmv_vs_single_rank <- vapply(
  seq_along(dates_ret),
  function(i){
    w_s <- W_single[i, ]; gm <- W_gmv[i]
    if (!is.finite(gm) || all(!is.finite(w_s))) return(NA_real_)
    # percentile rank within Individual Coins
    mean(w_s <= gm, na.rm=TRUE)
  }, numeric(1)
)

# 3) Statistical tests (one-sided)
# a) Paired t-test that GMV daily return > median single
ret_diff_med <- rp_gmv - rp_med_single
t_test_med <- tryCatch(t.test(ret_diff_med, alternative="greater"), error=function(e) NULL)

# b) Sign (hit-rate) test: P( GMV > median single ) > 0.5
hits <- sum(rp_gmv > rp_med_single, na.rm=TRUE)
nobs <- sum(is.finite(rp_gmv) & is.finite(rp_med_single))
binom_p_med <- if (nobs>0) stats::pbinom(hits-1, nobs, 0.5, lower.tail=FALSE) else NA_real_

# c) Jobson-Korkie with Memmel (Sharpe difference significance)
jk_memmel <- function(r1, r2){
  # both are daily return series
  mu1 <- mean(r1, na.rm=TRUE); mu2 <- mean(r2, na.rm=TRUE)
  s1  <- sd(r1,   na.rm=TRUE); s2  <- sd(r2,   na.rm=TRUE)
  if (!is.finite(s1) || !is.finite(s2) || s1==0 || s2==0) return(list(z=NA,p=NA))
  S1 <- mu1/s1; S2 <- mu2/s2
  rho <- suppressWarnings(cor(r1, r2, use="pairwise.complete.obs")); if (!is.finite(rho)) rho <- 0
  n <- sum(is.finite(r1) & is.finite(r2))
  # Memmel (2003) correction to Jobson-Korkie variance
  var_d <- (1 - rho) * (2 + (S1^2 + S2^2)/2 - rho*S1*S2) / n
  z <- (S1 - S2) / sqrt(var_d)
  p <- 2 * (1 - pnorm(abs(z)))
  list(z=z, p=p, Sharpe1=S1, Sharpe2=S2)
}
jk_med <- jk_memmel(rp_gmv, rp_med_single)

# 4) Effective diversification: Herfindahl index of GMV weights
# (use last rebalance weights as representative)
w_last <- W_gmv[nrow(W_gmv), ]
w_last <- w_last[is.finite(w_last)]
HHI <- sum(w_last^2)
EffN <- if (is.finite(HHI) && HHI>0) 1/HHI else NA_real_

# ---- Save evidence table ----
ev_tbl <- data.frame(
  Metric = c("AnnReturn","AnnVol","Sharpe","Sortino","ES_5","MaxDD","EffN",
             "Avg GMV wealth percentile vs Individual Coins",
             "t-test p (GMV > median single)",
             "Sign-test p (GMV > median single hit-rate > 0.5)",
             "JK-Memmel p (Sharpe_GMV vs Sharpe_medianSingle)"),
  Value  = c(
    round(gmv_stats$AnnReturn,4),
    round(gmv_stats$AnnVol,4),
    round(gmv_stats$Sharpe,4),
    round(gmv_stats$Sortino,4),
    round(gmv_stats$ES_5,4),
    round(gmv_stats$MaxDD,4),
    round(EffN,2),
    round(mean(gmv_vs_single_rank, na.rm=TRUE),3),
    if (is.null(t_test_med)) NA_real_ else round(t_test_med$p.value,4),
    round(binom_p_med,4),
    round(jk_med$p,4)
  )
)
write.csv(ev_tbl, file.path(output_path, "GMV_vs_Individual Coins_Evidence.csv"), row.names=FALSE)
write.csv(single_stats, file.path(output_path, "GMV_Individual Coins_PerCoinStats.csv"), row.names=FALSE)

# ---- Plots (no titles, no x label) ----

# A) GMV wealth percentile rank vs Individual Coins over time
png(file.path(output_path, "GMV_vs_Individual Coins_RelativeRank.png"), width=1400, height=500, bg="transparent")
print(
  ggplot(data.frame(date=dates_ret, pct=gmv_vs_single_rank), aes(x=date, y=pct)) +
    geom_hline(yintercept=0.5, linetype="dashed") +
    geom_line(linewidth=1) +
    scale_x_date(date_breaks="3 months", date_labels="%b %Y") +
    scale_y_continuous(limits=c(0,1), labels = scales::percent) +
    labs(title=NULL, x=NULL, y="GMV wealth percentile among Individual Coins") +
    theme_transparent_bottom + theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
)
dev.off()

# B) Drawdown comparison: GMV vs median single
DD_path <- function(wealth){ peak <- cummax(wealth); (wealth/peak) - 1 }
png(file.path(output_path, "GMV_vs_Individual Coins_Drawdowns.png"), width=1400, height=500, bg="transparent")
print(
  ggplot() +
    geom_line(aes(x=dates_ret, y=DD_path(W_gmv), color="GMV"), linewidth=1) +
    geom_line(aes(x=dates_ret, y=DD_path(W_med_single), color="Median single (by Sharpe)"), linewidth=0.8) +
    scale_color_manual(values=c("GMV"="blue","Median single (by Sharpe)"="grey40")) +
    scale_x_date(date_breaks="3 months", date_labels="%b %Y") +
    labs(title=NULL, x=NULL, y="Drawdown", color=NULL) +
    theme_transparent_bottom + theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
)
dev.off()



