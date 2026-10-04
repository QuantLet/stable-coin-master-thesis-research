
source("FRM_SC_utils.R"); load(file.path(output_path,"stage_20_loaded.RData"))

suppressPackageStartupMessages({
  library(quadprog); library(ggplot2)
  library(zoo); library(dplyr)
})

# ========== SETTINGS ==========
s_opt <- 90            # rolling window for GMV weights & rolling Sharpe
rebal_every <- 1       # rebalance every trading day
ANN_DAYS <- 365        # annualization days
K_VOL <- 90            # rolling volatility window
MIN_COV_FRAC <- 0.95   # require >=95% non-NA obs in window for a coin/date
KMIN_CROSS <- 5        # need at least 5 coins to compute x-sectional stat

# ========== HELPERS ==========
ann_mu   <- function(x) mean(x,na.rm=TRUE)*ANN_DAYS
ann_vol  <- function(x) sd(x,na.rm=TRUE)*sqrt(ANN_DAYS)
sharpe   <- function(x) if (sd(x,na.rm=TRUE)>0) ann_mu(x)/ann_vol(x) else NA_real_
to_wealth <- function(r) exp(cumsum(r))
roll_apply <- function(x,k,FUN) zoo::rollapply(x, k, FUN, by=1, align="right", fill=NA, na.rm=TRUE)
roll_vol    <- function(x,k) roll_apply(x,k,sd)
roll_mean   <- function(x,k) roll_apply(x,k,mean)
roll_sd     <- function(x,k) roll_apply(x,k,sd)
roll_sharpe <- function(x,k,ann=ANN_DAYS){
  m <- roll_mean(x,k); s <- roll_sd(x,k)
  out <- (m*ann)/(s*sqrt(ann)); out[!is.finite(out)] <- NA_real_; out
}
solve_gmv_longonly <- function(S){
  p <- ncol(S); Dmat <- 2 * (S + t(S))/2; dvec <- rep(0,p)
  Amat <- cbind(rep(1,p), diag(p)); bvec <- c(1, rep(0,p)); meq <- 1
  out <- tryCatch(quadprog::solve.QP(Dmat,dvec,Amat,bvec,meq=meq), error=function(e) NULL)
  if (is.null(out)) return(rep(1/p,p))
  w <- out$solution; w[w<0] <- 0; w/sum(w)
}
DD <- function(wealth){ peak <- cummax(wealth); draw <- (wealth/peak) - 1; min(draw, na.rm=TRUE) }
# finite-count in a rolling window
roll_count_finite <- function(x, k) rollapply(x, k, function(v) sum(is.finite(v)), by=1, align="right", fill=NA)
# robust row-wise stat requiring at least kmin values
row_stat <- function(M, FUN, kmin=KMIN_CROSS){
  apply(M, 1, function(row){
    v <- row[is.finite(row)]
    if (length(v) < kmin) NA_real_ else FUN(v)
  })
}

# ========== DATA ==========
dates_ret <- dates[-1]
R <- as.matrix(stock_return); mode(R) <- "numeric"
P <- ncol(R)

# ========== ROLLING GMV (DAILY) ==========
w_gmv <- matrix(NA_real_, nrow=nrow(R), ncol=P)
for (t in seq_len(nrow(R))) {
  if (t < s_opt) next
  if (((t - s_opt) %% rebal_every) != 0) { w_gmv[t,] <- w_gmv[t-1,]; next }
  Rw <- R[(t - s_opt + 1):t, , drop=FALSE]
  S  <- stats::cov(Rw, use="pairwise.complete.obs"); S[!is.finite(S)] <- 0
  w_gmv[t,] <- solve_gmv_longonly(S)
}
# forward-fill any gaps
w0 <- rep(1/P, P)
for (i in seq_len(nrow(w_gmv))) {
  if (i==1 && any(!is.finite(w_gmv[i,]))) w_gmv[i,] <- w0
  if (i>1  && any(!is.finite(w_gmv[i,])))  w_gmv[i,] <- w_gmv[i-1,]
}

# Portfolio returns & wealth
rp_gmv <- rowSums(R * w_gmv)
W_gmv  <- to_wealth(rp_gmv)

# Individual Coins wealth (for plots & MaxDD per coin)
W_single <- apply(R, 2, to_wealth)

# Cross-sectional wealth summaries each date (no coverage filter needed)
W_min <- apply(W_single, 1, function(x) min(x, na.rm=TRUE))
W_med <- apply(W_single, 1, function(x) stats::median(x, na.rm=TRUE))
W_max <- apply(W_single, 1, function(x) max(x, na.rm=TRUE))
W_avg <- rowMeans(W_single, na.rm=TRUE)

# Also track equal-weight average return across Individual Coins
rp_avg <- rowMeans(R, na.rm=TRUE)

# ========== ROLLING VOLATILITY (90d) with coverage filter ==========
vol_single_all <- apply(R, 2, function(col) roll_vol(col, K_VOL))
cover_mat <- sapply(seq_len(ncol(R)), function(j) roll_count_finite(R[, j], K_VOL))
MIN_COV <- ceiling(MIN_COV_FRAC * K_VOL)
vol_single_all[ cover_mat < MIN_COV ] <- NA

vol_min <- row_stat(vol_single_all, min)
vol_med <- row_stat(vol_single_all, median)
vol_max <- row_stat(vol_single_all, max)
vol_avg <- roll_vol(rp_avg, K_VOL)
vol_gmv <- roll_vol(rp_gmv, K_VOL)

# ========== ROLLING SHARPE (90d, ANN=365) with coverage filter ==========
# Compute per-coin rolling Sharpe first
sh_single_all <- apply(R, 2, function(col) roll_sharpe(col, s_opt, ann=ANN_DAYS))
# also enforce coverage for Sharpe (use the same cover_mat)
sh_single_all[ cover_mat < MIN_COV ] <- NA

sh_min <- row_stat(sh_single_all, min)
sh_med <- row_stat(sh_single_all, median)
sh_max <- row_stat(sh_single_all, max)
sh_avg <- roll_sharpe(rp_avg, s_opt, ann=ANN_DAYS)
sh_gmv <- roll_sharpe(rp_gmv, s_opt, ann=ANN_DAYS)

# ========== COLORS & STYLES ==========
col_GMV <- "dodgerblue"
col_min <- "firebrick2"
col_med <- "forestgreen"
col_max <- "purple"
col_avg <- "darkorange"
lw_gmv <- 2.2
lw_dash <- 1.3

# ========== PLOTS ==========
# 1) Cumulative wealth
png(file.path(output_path,"CAPM_Portfolio_vs_Individual Coins_Wealth.png"), width=1400, height=900, bg="transparent")
print(
  ggplot() +
    geom_line(aes(x=dates_ret, y=W_min, color="Individual Coins (min)"),     linewidth=lw_dash, linetype="dashed") +
    geom_line(aes(x=dates_ret, y=W_med, color="Individual Coins (median)"),  linewidth=lw_dash, linetype="dashed") +
    geom_line(aes(x=dates_ret, y=W_max, color="Individual Coins (max)"),     linewidth=lw_dash, linetype="dashed") +
    geom_line(aes(x=dates_ret, y=W_avg, color="Individual Coins (average)"), linewidth=lw_dash, linetype="dashed") +
    geom_line(aes(x=dates_ret, y=W_gmv, color="GMV"), linewidth=lw_gmv) +
    scale_color_manual(values=c("GMV"=col_GMV,
                                "Individual Coins (min)"=col_min,
                                "Individual Coins (median)"=col_med,
                                "Individual Coins (max)"=col_max,
                                "Individual Coins (average)"=col_avg)) +
    scale_x_date(date_breaks="3 months", date_labels="%b %Y") +
    labs(title=NULL, x=NULL, y="Wealth (start = 1)", color=NULL) +
    theme_transparent_bottom +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
)
dev.off()

# 2) Rolling volatility (90-day)
png(file.path(output_path,"CAPM_Portfolio_vs_Individual Coins_RollVol.png"), width=1400, height=800, bg="transparent")
print(
  ggplot() +
    geom_line(aes(x=dates_ret, y=vol_min, color="Individual Coins (min)"),     linewidth=lw_dash, linetype="dashed") +
    geom_line(aes(x=dates_ret, y=vol_med, color="Individual Coins (median)"),  linewidth=lw_dash, linetype="dashed") +
    geom_line(aes(x=dates_ret, y=vol_max, color="Individual Coins (max)"),     linewidth=lw_dash, linetype="dashed") +
    geom_line(aes(x=dates_ret, y=vol_avg, color="Individual Coins (average)"), linewidth=lw_dash, linetype="dashed") +
    geom_line(aes(x=dates_ret, y=vol_gmv, color="GMV"), linewidth=lw_gmv) +
    scale_color_manual(values=c("GMV"=col_GMV,
                                "Individual Coins (min)"=col_min,
                                "Individual Coins (median)"=col_med,
                                "Individual Coins (max)"=col_max,
                                "Individual Coins (average)"=col_avg)) +
    scale_x_date(date_breaks="3 months", date_labels="%b %Y") +
    labs(title=NULL, x=NULL, y="σ (90-day)", color=NULL) +
    theme_transparent_bottom +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
)
dev.off()

# 3) Rolling Sharpe (90-day, ANN=365)
png(file.path(output_path,"CAPM_Portfolio_vs_Individual Coins_RollSharpe.png"), width=1400, height=800, bg="transparent")
print(
  ggplot() +
    geom_line(aes(x=dates_ret, y=sh_min, color="Individual Coins (min)"),     linewidth=lw_dash, linetype="dashed") +
    geom_line(aes(x=dates_ret, y=sh_med, color="Individual Coins (median)"),  linewidth=lw_dash, linetype="dashed") +
    geom_line(aes(x=dates_ret, y=sh_max, color="Individual Coins (max)"),     linewidth=lw_dash, linetype="dashed") +
    geom_line(aes(x=dates_ret, y=sh_avg, color="Individual Coins (average)"), linewidth=lw_dash, linetype="dashed") +
    geom_line(aes(x=dates_ret, y=sh_gmv, color="GMV"), linewidth=lw_gmv) +
    scale_color_manual(values=c("GMV"=col_GMV,
                                "Individual Coins (min)"=col_min,
                                "Individual Coins (median)"=col_med,
                                "Individual Coins (max)"=col_max,
                                "Individual Coins (average)"=col_avg)) +
    scale_x_date(date_breaks="3 months", date_labels="%b %Y") +
    labs(title=NULL, x=NULL, y="Rolling Sharpe (ANN=365, 90-day)", color=NULL) +
    theme_transparent_bottom +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
)
dev.off()

# ========== STATS (ANN=365) — GMV + EVERY COIN ==========
stats_tbl <- data.frame(
  Strategy  = c("GMV", colnames(R)),
  AnnReturn = c(ann_mu(rp_gmv), apply(R, 2, ann_mu)),
  AnnVol    = c(ann_vol(rp_gmv), apply(R, 2, ann_vol)),
  Sharpe    = c(sharpe(rp_gmv),  apply(R, 2, sharpe)),
  MaxDD     = c(DD(W_gmv),       apply(W_single, 2, DD))
)
num_cols <- vapply(stats_tbl, is.numeric, logical(1))
stats_tbl[num_cols] <- lapply(stats_tbl[num_cols], function(x) round(x, 4))

# ====== SUMMARY TABLE (time-averages & medians across days) ======
avg <- function(x) mean(x, na.rm=TRUE)
med <- function(x) median(x, na.rm=TRUE)

summary_tbl <- data.frame(
  Indicator = rep(c("Volatility (90d)", "Sharpe (90d)"), each = 5),
  Series    = rep(c("GMV", "Singles (min)", "Singles (median)", "Singles (max)", "Singles (average)"), times = 2),
  Mean      = c(avg(vol_gmv), avg(vol_min), avg(vol_med), avg(vol_max), avg(vol_avg),
                avg(sh_gmv),  avg(sh_min),  avg(sh_med),  avg(sh_max),  avg(sh_avg)),
  Median    = c(med(vol_gmv), med(vol_min), med(vol_med), med(vol_max), med(vol_avg),
                med(sh_gmv),  med(sh_min),  med(sh_med),  med(sh_max),  med(sh_avg))
)

# round and save
summary_tbl$Mean   <- round(summary_tbl$Mean,   6)
summary_tbl$Median <- round(summary_tbl$Median, 6)
write.csv(summary_tbl, file.path(output_path, "CAPM_Rolling_Metrics_Summary.csv"), row.names = FALSE)

# ====== BOX PLOTS (full distribution across time) ======
suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyr)
  library(dplyr)
})

# --- Use a strong, system-like blue for GMV (you can change if you prefer) ---
col_GMV <- "#007AFF"   # iOS System Blue (fully blue fill + outline for GMV)

# If you already defined col_min / col_med / col_max / col_avg above, this will use them.
# Otherwise, give them quick defaults:
if (!exists("col_min"))  col_min  <- "#8E8E93"  # muted grey
if (!exists("col_med"))  col_med  <- "#FF9F0A"  # orange
if (!exists("col_max"))  col_max  <- "#FF3B30"  # red
if (!exists("col_avg"))  col_avg  <- "#34C759"  # green

# ---------- Common high-res PNG helper (transparent) ----------
png_hr <- function(path, width_px = 2400, height_px = 1600, res = 300) {
  png(filename = path, width = width_px, height = height_px, res = res,
      units = "px", bg = "transparent")
}

# ---------- Aggregate (GMV vs Singles stats) ----------
vol_df <- data.frame(
  GMV                = vol_gmv,
  `Singles (min)`    = vol_min,
  `Singles (median)` = vol_med,
  `Singles (max)`    = vol_max,
  `Singles (average)`= vol_avg
) |>
  pivot_longer(cols = everything(), names_to = "Series", values_to = "Value") |>
  filter(is.finite(Value))

sh_df <- data.frame(
  GMV                = sh_gmv,
  `Singles (min)`    = sh_min,
  `Singles (median)` = sh_med,
  `Singles (max)`    = sh_max,
  `Singles (average)`= sh_avg
) |>
  pivot_longer(cols = everything(), names_to = "Series", values_to = "Value") |>
  filter(is.finite(Value))

pal_agg <- c(
  "GMV"               = col_GMV,
  "Singles (min)"     = col_min,
  "Singles (median)"  = col_med,
  "Singles (max)"     = col_max,
  "Singles (average)" = col_avg
)

# 1) 90-day Volatility (aggregate)
png_hr(file.path(output_path, "CAPM_Boxplot_RollVol_90d.png"))
print(
  ggplot(vol_df, aes(x = Series, y = Value, color = Series, fill = Series)) +
    geom_boxplot(outlier.alpha = 0.35, linewidth = 1) +
    scale_color_manual(values = pal_agg, guide = "none") +
    scale_fill_manual(values  = pal_agg, guide = "none") +
    labs(title = NULL, x = NULL, y = "σ (90-day)") +
    theme_transparent_bottom +
    theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1))
)
dev.off()

# 2) 90-day Sharpe (aggregate)
png_hr(file.path(output_path, "CAPM_Boxplot_RollSharpe_90d.png"))
print(
  ggplot(sh_df, aes(x = Series, y = Value, color = Series, fill = Series)) +
    geom_boxplot(outlier.alpha = 0.35, linewidth = 1) +
    scale_color_manual(values = pal_agg, guide = "none") +
    scale_fill_manual(values  = pal_agg, guide = "none") +
    labs(title = NULL, x = NULL, y = "Rolling Sharpe (ANN=365, 90-day)") +
    theme_transparent_bottom +
    theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1))
)
dev.off()

# ====== BOX PLOTS PER COIN (+ GMV) FOR 90d VOL & 90d SHARPE ======

# --- Ensure coin names from your return matrix R ---
coin_names <- colnames(R)

# Long data for per-coin volatility + GMV
vol_df_coin <- as.data.frame(vol_single_all)
colnames(vol_df_coin) <- coin_names
vol_df_coin$GMV <- vol_gmv
vol_long <- vol_df_coin |>
  pivot_longer(cols = everything(), names_to = "Series", values_to = "Value") |>
  filter(is.finite(Value))

# Long data for per-coin Sharpe + GMV
sh_df_coin <- as.data.frame(sh_single_all)
colnames(sh_df_coin) <- coin_names
sh_df_coin$GMV <- sh_gmv
sh_long <- sh_df_coin |>
  pivot_longer(cols = everything(), names_to = "Series", values_to = "Value") |>
  filter(is.finite(Value))

# Order with GMV first, then coins alphabetically
lvl <- c("GMV", sort(coin_names))
vol_long$Series <- factor(vol_long$Series, levels = lvl)
sh_long$Series  <- factor(sh_long$Series,  levels = lvl)

# ---------- Distinct colors for individual coins (GMV stays full blue) ----------
# A quick, bright, distinct palette generator
distinct_cols <- function(n, exclude = NULL) {
  # HCL-based distinct hues
  base <- grDevices::hcl(
    h = seq(15, 375, length.out = n + 1)[1:n],
    c = 100, l = 55
  )
  # Make sure none equals the excluded blue (rare anyway)
  if (!is.null(exclude)) base[base == exclude] <- "#5856D6"  # swap if collision (purple)
  base
}

coin_cols <- distinct_cols(length(coin_names), exclude = col_GMV)
names(coin_cols) <- sort(coin_names)

pal_by_coin <- c("GMV" = col_GMV, coin_cols)

# ---------- Boxplot: 90d Rolling Volatility (per coin + GMV) ----------
png_hr(file.path(output_path, "CAPM_Boxplot_RollVol_90d_ByCoin.png"),
       width_px = 2800, height_px = 1600, res = 300)
print(
  ggplot(vol_long, aes(x = Series, y = Value, color = Series, fill = Series)) +
    geom_boxplot(outlier.alpha = 0.30, linewidth = 0.9) +
    scale_color_manual(values = pal_by_coin, guide = "none") +
    scale_fill_manual(values  = pal_by_coin, guide = "none") +
    labs(title = NULL, x = NULL, y = "σ (90-day)") +
    theme_transparent_bottom +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
)
dev.off()

# ---------- Boxplot: 90d Rolling Sharpe (per coin + GMV) ----------
png_hr(file.path(output_path, "CAPM_Boxplot_RollSharpe_90d_ByCoin.png"),
       width_px = 2800, height_px = 1600, res = 300)
print(
  ggplot(sh_long, aes(x = Series, y = Value, color = Series, fill = Series)) +
    geom_boxplot(outlier.alpha = 0.30, linewidth = 0.9) +
    scale_color_manual(values = pal_by_coin, guide = "none") +
    scale_fill_manual(values  = pal_by_coin, guide = "none") +
    labs(title = NULL, x = NULL, y = "Rolling Sharpe (ANN=365, 90-day)") +
    theme_transparent_bottom +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
)
dev.off()

# ---------- Per-coin summary tables (time-mean & time-median) ----------
avg <- function(x) mean(x, na.rm = TRUE)
med <- function(x) median(x, na.rm = TRUE)

# Volatility summary
vol_summary <- data.frame(
  Series = lvl,
  Mean   = c(avg(vol_gmv), sapply(vol_df_coin[ , sort(coin_names), drop = FALSE], avg)),
  Median = c(med(vol_gmv), sapply(vol_df_coin[ , sort(coin_names), drop = FALSE], med))
)
vol_summary$Mean   <- round(vol_summary$Mean,   6)
vol_summary$Median <- round(vol_summary$Median, 6)
write.csv(vol_summary, file.path(output_path, "CAPM_RollVol_90d_ByCoin_Summary.csv"), row.names = FALSE)

# Sharpe summary
sh_summary <- data.frame(
  Series = lvl,
  Mean   = c(avg(sh_gmv), sapply(sh_df_coin[ , sort(coin_names), drop = FALSE], avg)),
  Median = c(med(sh_gmv), sapply(sh_df_coin[ , sort(coin_names), drop = FALSE], med))
)
sh_summary$Mean   <- round(sh_summary$Mean,   6)
sh_summary$Median <- round(sh_summary$Median, 6)
write.csv(sh_summary, file.path(output_path, "CAPM_RollSharpe_90d_ByCoin_Summary.csv"), row.names = FALSE)

# If you computed a stats_tbl elsewhere, this persists it:
if (exists("stats_tbl")) {
  write.csv(stats_tbl,
            file.path(output_path, "CAPM_Portfolio_vs_Individual Coins_Stats.csv"),
            row.names = FALSE)
}

