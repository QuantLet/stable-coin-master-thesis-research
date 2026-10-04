
# ==========================================================
# HHI charts: Lambda + MarketCap (Daily & Cumulative-to-date)
# No normalization, transparent background, no titles,
# no x-axis label "Date".
# ==========================================================

suppressPackageStartupMessages({
  library(ggplot2)
})

# Expect: FRM_SC_utils.R defines theme_transparent_bottom
source("FRM_SC_utils.R")

# Data from earlier stages
load(file.path(output_path,"stage_20_loaded.RData"))   # mktcap, dates, etc.
load(file.path(output_path,"stage_30_varying.RData"))  # FRM_individ (list)

dir.create(file.path(output_path,"Lambda"), showWarnings=FALSE, recursive=TRUE)
dir.create(file.path(website_path, date_end), showWarnings=FALSE, recursive=TRUE)

# ---------------- Helpers ----------------
hhi_from_values <- function(x){
  x <- as.numeric(x); x[!is.finite(x) | x < 0] <- 0
  s <- sum(x)
  if (s <= 0) return(NA_real_)
  p <- x / s
  sum(p^2)
}

# ---------------- 1) HHI from lambdas ----------------
# names(FRM_individ) expected to be date strings (YYYY-MM-DD or YYYYMMDD)
lambda_dates <- suppressWarnings(as.Date(names(FRM_individ)))
if (any(is.na(lambda_dates))) {
  lambda_dates2 <- suppressWarnings(as.Date(names(FRM_individ), "%Y%m%d"))
  lambda_dates[is.na(lambda_dates)] <- lambda_dates2[is.na(lambda_dates)]
}

hhi_lambda <- vapply(
  FRM_individ,
  function(m){
    if (is.null(m) || !ncol(m)) return(NA_real_)
    # use first row of lambda vector as in your pipeline
    hhi_from_values(m[1, ])
  },
  numeric(1)
)

HHI_lambda_df <- data.frame(date = lambda_dates, HHI_lambda = round(hhi_lambda, 6))
HHI_lambda_df <- HHI_lambda_df[order(HHI_lambda_df$date), ]
write.csv(HHI_lambda_df, file.path(output_path,"Lambda","HHI_lambda.csv"), row.names=FALSE)

png(file.path(website_path, date_end, "HHI_Lambda.png"), width=1600, height=900, res=120, bg="transparent")
print(
  ggplot(HHI_lambda_df, aes(x=date, y=HHI_lambda, color="Lambda HHI")) +
    geom_line(linewidth=1.2, lineend="round") +
    scale_color_manual(values=c("Lambda HHI"="#007AFF"), name=NULL) +
    scale_x_date(date_breaks="3 months", date_labels="%b %Y") +
    labs(title=NULL, x=NULL, y="HHI") +
    theme_transparent_bottom +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
)
dev.off()

# ---------------- 2) HHI from Market Cap — Daily ----------------
mktcap_dates <- suppressWarnings(as.Date(mktcap$ticker))
if (any(is.na(mktcap_dates))) {
  mktcap_dates2 <- suppressWarnings(as.Date(mktcap$ticker, "%Y%m%d"))
  mktcap_dates[is.na(mktcap_dates)] <- mktcap_dates2[is.na(mktcap_dates)]
}

mcap_mat <- as.matrix(mktcap[, -1, drop=FALSE]); storage.mode(mcap_mat) <- "numeric"
mcap_mat[!is.finite(mcap_mat) | mcap_mat < 0] <- 0

hhi_mktcap_daily <- apply(mcap_mat, 1, hhi_from_values)
HHI_mktcap_daily_df <- data.frame(date = mktcap_dates, HHI_mktcap_daily = round(hhi_mktcap_daily, 6))
HHI_mktcap_daily_df <- HHI_mktcap_daily_df[order(HHI_mktcap_daily_df$date), ]

# Backward-compat file name expected elsewhere
write.csv(
  data.frame(date = HHI_mktcap_daily_df$date, HHI_mktcap = HHI_mktcap_daily_df$HHI_mktcap_daily),
  file.path(output_path,"Lambda","HHI_mktcap_all.csv"),
  row.names = FALSE
)
# Explicit daily file
write.csv(HHI_mktcap_daily_df, file.path(output_path,"Lambda","HHI_mktcap_daily.csv"), row.names=FALSE)

png(file.path(website_path, date_end, "HHI_MarketCap_Daily.png"), width=1600, height=900, res=120, bg="transparent")
print(
  ggplot(HHI_mktcap_daily_df, aes(x=date, y=HHI_mktcap_daily, color="Market-Cap HHI (Daily)")) +
    geom_line(linewidth=1.2, lineend="round") +
    scale_color_manual(values=c("Market-Cap HHI (Daily)"="#FF3B30"), name=NULL) +
    scale_x_date(date_breaks="3 months", date_labels="%b %Y") +
    labs(title=NULL, x=NULL, y="HHI") +
    theme_transparent_bottom +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
)
dev.off()

# ---------------- 3) HHI from Market Cap — CUMULATIVE-TO-DATE ----------------
mcap_cum_mat <- apply(mcap_mat, 2, function(col){
  col[!is.finite(col) | col < 0] <- 0
  cumsum(col)
})

hhi_mktcap_cum <- apply(mcap_cum_mat, 1, hhi_from_values)
HHI_mktcap_cum_df <- data.frame(date = mktcap_dates, HHI_mktcap_cum = round(hhi_mktcap_cum, 6))
HHI_mktcap_cum_df <- HHI_mktcap_cum_df[order(HHI_mktcap_cum_df$date), ]
write.csv(HHI_mktcap_cum_df, file.path(output_path,"Lambda","HHI_mktcap_cumulative.csv"), row.names=FALSE)

png(file.path(website_path, date_end, "HHI_MarketCap_Cumulative.png"), width=1600, height=900, res=120, bg="transparent")
print(
  ggplot(HHI_mktcap_cum_df, aes(x=date, y=HHI_mktcap_cum, color="Market-Cap HHI (Cumulative)")) +
    geom_line(linewidth=1.2, lineend="round") +
    scale_color_manual(values=c("Market-Cap HHI (Cumulative)"="#FF9500"), name=NULL) +
    scale_x_date(date_breaks="3 months", date_labels="%b %Y") +
    labs(title=NULL, x=NULL, y="HHI") +
    theme_transparent_bottom +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
)
dev.off()

# ---------------- 4) Combined CSV (optional) ----------------
HHI_combo <- merge(HHI_mktcap_daily_df, HHI_mktcap_cum_df, by="date", all=TRUE)
write.csv(HHI_combo, file.path(output_path,"Lambda","HHI_mktcap_variants.csv"), row.names=FALSE)

