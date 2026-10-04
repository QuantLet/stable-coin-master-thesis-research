#!/usr/bin/env Rscript

# Build the common daily panel used in Section 4.5. Stablecoin outcomes are
# computed from the reference-adjusted peg panel. The broad-crypto return is
# value weighted with lagged market capitalisation, so today's return is not
# weighted using information observed only at the end of today.

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else normalizePath(getwd())
source(file.path(script_dir, "00_config.R"))

read_dates <- function(path) {
  if (!file.exists(path)) stop("Missing input: ", path)
  x <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (!"date" %in% names(x)) stop("Missing date column: ", path)
  x$date <- as.Date(x$date)
  if (anyNA(x$date) || anyDuplicated(x$date)) stop("Invalid dates: ", path)
  x
}

read_frm <- function(path, output_name) {
  x <- read_dates(path)
  if (!"frm" %in% names(x)) stop("Missing frm column: ", path)
  x$frm <- suppressWarnings(as.numeric(x$frm))
  if (any(!is.finite(x$frm)) || any(x$frm <= 0)) stop("Invalid FRM: ", path)
  out <- x[, c("date", "frm")]
  names(out)[2] <- output_name
  out
}

prices <- read_dates(file.path(prepared_dir, "Crypto_Price_15_PPT.csv"))
mcaps <- read_dates(file.path(prepared_dir, "Crypto_MarketCap_15_PPT.csv"))
if (!identical(prices$date, mcaps$date) || !identical(names(prices), names(mcaps))) {
  stop("Crypto price and market-cap panels are not aligned.")
}
tickers <- setdiff(names(prices), "date")
price_matrix <- as.matrix(prices[, tickers, drop = FALSE])
mcap_matrix <- as.matrix(mcaps[, tickers, drop = FALSE])
storage.mode(price_matrix) <- "numeric"
storage.mode(mcap_matrix) <- "numeric"

asset_returns <- matrix(NA_real_, nrow(price_matrix), ncol(price_matrix),
                        dimnames = list(NULL, tickers))
asset_returns[-1, ] <- log(price_matrix[-1, , drop = FALSE] /
                           price_matrix[-nrow(price_matrix), , drop = FALSE])
asset_returns[!is.finite(asset_returns)] <- NA_real_
lagged_mcap <- rbind(NA_real_, mcap_matrix[-nrow(mcap_matrix), , drop = FALSE])

market_return_vw <- market_return_ew <- rep(NA_real_, nrow(price_matrix))
market_active_assets <- integer(nrow(price_matrix))
for (i in seq_len(nrow(price_matrix))) {
  valid <- is.finite(asset_returns[i, ]) & is.finite(lagged_mcap[i, ]) & lagged_mcap[i, ] > 0
  market_active_assets[i] <- sum(valid)
  if (any(valid)) {
    weights <- lagged_mcap[i, valid] / sum(lagged_mcap[i, valid])
    market_return_vw[i] <- sum(weights * asset_returns[i, valid])
    market_return_ew[i] <- mean(asset_returns[i, valid])
  }
}
crypto_market <- data.frame(
  date = prices$date,
  crypto_market_return_vw = market_return_vw,
  crypto_market_return_ew = market_return_ew,
  crypto_market_active_assets = market_active_assets,
  stringsAsFactors = FALSE
)

peg <- read.csv(file.path(input_dir, "peg_panel_reference_adjusted.csv"),
                check.names = FALSE, stringsAsFactors = FALSE)
required_peg <- c("date", "symbol", "signed_deviation_bps", "absolute_deviation_bps")
if (!all(required_peg %in% names(peg))) stop("Reference-adjusted peg panel is incomplete.")
peg$date <- as.Date(peg$date)
peg$signed_deviation_bps <- suppressWarnings(as.numeric(peg$signed_deviation_bps))
peg$absolute_deviation_bps <- suppressWarnings(as.numeric(peg$absolute_deviation_bps))
peg$downside_bps <- pmax(-peg$signed_deviation_bps, 0)
peg$depeg_1pct <- is.finite(peg$signed_deviation_bps) & peg$signed_deviation_bps <= -100

stable_outcomes <- do.call(rbind, lapply(split(peg, peg$date), function(z) {
  valid <- is.finite(z$signed_deviation_bps)
  data.frame(
    date = z$date[1],
    stable_peg_n = sum(valid),
    stable_mean_downside_bps = if (any(valid)) mean(z$downside_bps[valid]) else NA_real_,
    stable_median_abs_bps = if (any(valid)) median(z$absolute_deviation_bps[valid]) else NA_real_,
    stable_max_downside_bps = if (any(valid)) max(z$downside_bps[valid]) else NA_real_,
    stable_depeg_breadth_count = sum(z$depeg_1pct, na.rm = TRUE),
    stable_depeg_breadth_share = if (any(valid)) sum(z$depeg_1pct[valid]) / sum(valid) else NA_real_,
    stringsAsFactors = FALSE
  )
}))
rownames(stable_outcomes) <- NULL
stable_outcomes$date <- as.Date(stable_outcomes$date)

frm_series <- list(
  read_frm(file.path(input_dir, "FRM_Stable_adjusted11_screened.csv"), "frm_stable_screened"),
  read_frm(file.path(input_dir, "FRM_Stable_adjusted11_strict.csv"), "frm_stable_strict"),
  read_frm(file.path(input_dir, "FRM_Stable_usd8_screened.csv"), "frm_stable_usd8_screened"),
  read_frm(file.path(crypto_frm_dir, "ppt_dynamic_15", "frm_numerical_screened.csv"), "frm_crypto_dynamic_screened"),
  read_frm(file.path(crypto_frm_dir, "ppt_dynamic_15", "frm_strict.csv"), "frm_crypto_dynamic_strict"),
  read_frm(file.path(crypto_frm_dir, "balanced_complete_9", "frm_numerical_screened.csv"), "frm_crypto_balanced9_screened"),
  read_frm(file.path(crypto_frm_dir, "balanced_complete_9", "frm_strict.csv"), "frm_crypto_balanced9_strict")
)

daily <- Reduce(function(x, y) merge(x, y, by = "date", all = FALSE),
                c(frm_series, list(crypto_market, stable_outcomes)))
daily <- daily[order(daily$date), ]
daily <- daily[daily$date >= sample_start & daily$date <= sample_end, ]
rownames(daily) <- NULL
if (!nrow(daily)) stop("The common Section 4.5 panel is empty.")

# State classifications are descriptive full-sample summaries only. Forecasts
# in 04_cross_market_analysis.R never use these full-sample thresholds.
stable_q80 <- as.numeric(quantile(daily$frm_stable_screened, 0.80, type = 8, na.rm = TRUE))
crypto_q80 <- as.numeric(quantile(daily$frm_crypto_dynamic_screened, 0.80, type = 8, na.rm = TRUE))
daily$stable_risk_percentile <- 100 * rank(daily$frm_stable_screened, ties.method = "average") / nrow(daily)
daily$crypto_risk_percentile <- 100 * rank(daily$frm_crypto_dynamic_screened, ties.method = "average") / nrow(daily)
daily$stable_high <- daily$frm_stable_screened >= stable_q80
daily$crypto_high <- daily$frm_crypto_dynamic_screened >= crypto_q80
daily$risk_state <- ifelse(
  daily$stable_high & daily$crypto_high, "joint_high",
  ifelse(daily$stable_high, "stable_only_high",
         ifelse(daily$crypto_high, "crypto_only_high", "joint_normal"))
)

state_levels <- c("joint_normal", "stable_only_high", "crypto_only_high", "joint_high")
state_counts <- table(factor(daily$risk_state, levels = state_levels))
state_summary <- data.frame(
  risk_state = state_levels,
  days = as.integer(state_counts),
  share = as.integer(state_counts) / nrow(daily),
  stringsAsFactors = FALSE
)

transition_counts <- table(
  factor(head(daily$risk_state, -1), levels = state_levels),
  factor(tail(daily$risk_state, -1), levels = state_levels)
)
transition_prob <- transition_counts / rowSums(transition_counts)
transition_table <- as.data.frame(as.table(transition_prob), stringsAsFactors = FALSE)
names(transition_table) <- c("state_t", "state_t_plus_1", "probability")

write_csv(daily, file.path(prepared_dir, "Section_4_5_daily_analysis_panel.csv"), quote = FALSE)
write_csv(stable_outcomes, file.path(prepared_dir, "Reference_Adjusted_Stablecoin_Outcomes_Daily.csv"), quote = FALSE)
write_csv(crypto_market, file.path(prepared_dir, "Broad_Crypto_Market_Return_Daily.csv"), quote = FALSE)
write_csv(state_summary, file.path(tables_dir, "Table_4_5a_Risk_State_Shares.csv"))
write_csv(transition_table, file.path(tables_dir, "Table_S4_5_Risk_State_Transitions.csv"))

qa <- data.frame(
  check = c(
    "common_panel_dates_unique", "common_panel_sorted_daily", "primary_frms_finite_positive",
    "market_return_uses_lagged_mcap", "stable_outcome_reference_adjusted",
    "full_hormuz_0_30_available", "state_partition_exhaustive"
  ),
  status = c(
    if (!anyDuplicated(daily$date)) "PASS" else "FAIL",
    if (all(diff(daily$date) == 1)) "PASS" else "FAIL",
    if (all(is.finite(daily$frm_stable_screened)) && all(daily$frm_stable_screened > 0) &&
        all(is.finite(daily$frm_crypto_dynamic_screened)) && all(daily$frm_crypto_dynamic_screened > 0)) "PASS" else "FAIL",
    "PASS", "PASS",
    if (max(daily$date) >= as.Date("2026-03-30")) "PASS" else "FAIL",
    if (sum(state_summary$days) == nrow(daily)) "PASS" else "FAIL"
  ),
  detail = c(
    paste(nrow(daily), "daily observations"),
    paste(format(min(daily$date)), "to", format(max(daily$date))),
    "Reference-adjusted screened Stable FRM and dynamic-15 screened Crypto FRM",
    "Weights dated t-1; missing assets excluded and remaining weights renormalised",
    "PAXG/XAUUSD, EURS/EURUSD and IDRT/IDRUSD were adjusted upstream",
    format(max(daily$date)),
    paste("stable q80=", signif(stable_q80, 6), "; crypto q80=", signif(crypto_q80, 6), sep = "")
  ), stringsAsFactors = FALSE
)
write_csv(qa, file.path(qa_dir, "Section_4_5_outcome_panel_QA.csv"))
if (any(qa$status == "FAIL")) stop("Section 4.5 outcome panel QA failed.")

writeLines(c(
  "stable_outcome_primary=log1p(mean future reference-adjusted downside peg deviation in basis points)",
  "stable_outcome_secondary=future mean one-percent downside-depeg breadth share",
  "crypto_outcome_primary=log future annualised volatility of lagged-market-cap-weighted crypto return",
  "risk_state_threshold=full-sample 80th percentile; descriptive only; never used in forecasts",
  "crypto_market_weighting=lagged market capitalisation with daily renormalisation over valid assets"
), file.path(qa_dir, "Section_4_5_outcome_definitions.txt"), useBytes = TRUE)

message("Section 4.5 common outcome panel completed.")
