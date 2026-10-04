#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE, scipen = 999)

project_root <- normalizePath(getwd(), mustWork = TRUE)
bundle_dir <- file.path(project_root, "outputs", "section_6_3_portfolio_protocol")
dirs <- file.path(bundle_dir, c("source_data", "source_data/input", "tables", "qa"))
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

frozen_input_dir <- file.path(bundle_dir, "source_data", "input")
resolve_input <- function(filename, canonical_path) {
  frozen_path <- file.path(frozen_input_dir, filename)
  if (file.exists(frozen_path)) frozen_path else canonical_path
}

price_file <- resolve_input(
  "Stable_Price_reference_adjusted_11.csv",
  file.path(project_root, "outputs", "section_4_4_reference_adjusted_event_study",
            "source_data", "prepared", "Stable_Price_reference_adjusted_11.csv")
)
lambda_file <- resolve_input(
  "lambdas_strict.csv",
  file.path(project_root, "outputs", "section_4_4_reference_adjusted_event_study",
            "source_data", "frm", "reference_adjusted_11_robust_scaled",
            "lambdas_strict.csv")
)

if (!file.exists(price_file) || !file.exists(lambda_file)) {
  stop("Required price or lambda input is missing.")
}

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

project_capped_simplex <- function(v, cap = 0.30, tol = 1e-13) {
  p <- length(v)
  if (cap * p < 1 - tol) stop("Infeasible weight cap.")
  lo <- min(v - cap) - 1
  hi <- max(v) + 1
  for (iter in seq_len(80L)) {
    mid <- (lo + hi) / 2
    w <- pmin(cap, pmax(0, v - mid))
    if (sum(w) > 1) lo <- mid else hi <- mid
  }
  w <- pmin(cap, pmax(0, v - (lo + hi) / 2))
  w <- w / sum(w)
  if (max(w) > cap + 1e-9) stop("Projection violated the cap.")
  w
}

solve_linear_capped <- function(cvec, cap = 0.30) {
  p <- length(cvec)
  if (cap * p < 1 - 1e-12) stop("Infeasible weight cap.")
  w <- rep(0, p)
  remaining <- 1
  for (j in order(cvec, seq_along(cvec))) {
    add <- min(cap, remaining)
    w[j] <- add
    remaining <- remaining - add
    if (remaining <= 1e-12) break
  }
  if (remaining > 1e-8) stop("Linear allocation failed.")
  w / sum(w)
}

solve_qp_capped <- function(Q, cvec, cap = 0.30, w0 = NULL,
                            tolerance = 1e-10, max_iter = 1000L) {
  Q <- (Q + t(Q)) / 2
  p <- nrow(Q)
  ev <- eigen(Q, symmetric = TRUE, only.values = TRUE)$values
  ridge <- max(1e-12, -min(ev) + 1e-12)
  Q <- Q + diag(ridge, p)
  theta0 <- rep(1 / p, p - 1L)
  to_weights <- function(theta) c(theta, 1 - sum(theta))
  objective <- function(theta) {
    w <- to_weights(theta)
    as.numeric(t(w) %*% Q %*% w + sum(cvec * w))
  }
  gradient <- function(theta) {
    w <- to_weights(theta)
    gw <- 2 * as.vector(Q %*% w) + cvec
    gw[seq_len(p - 1L)] - gw[p]
  }
  ui <- rbind(
    diag(p - 1L),
    -diag(p - 1L),
    -matrix(1, nrow = 1L, ncol = p - 1L),
    matrix(1, nrow = 1L, ncol = p - 1L)
  )
  ci <- c(rep(0, p - 1L), rep(-cap, p - 1L), -1, 1 - cap)
  fit <- tryCatch(
    stats::constrOptim(
      theta0, objective, gradient, ui = ui, ci = ci,
      method = "BFGS", control = list(maxit = max_iter, reltol = tolerance),
      outer.iterations = 50L, outer.eps = 1e-8
    ),
    error = function(e) NULL
  )
  if (is.null(fit) || any(!is.finite(fit$par))) {
    return(project_capped_simplex(rep(1 / p, p), cap))
  }
  project_capped_simplex(to_weights(fit$par), cap)
}

annualized_return <- function(r) {
  100 * (exp(mean(log1p(r)) * 365) - 1)
}

annualized_volatility <- function(r) {
  100 * stats::sd(log1p(r)) * sqrt(365)
}

expected_shortfall_5 <- function(r) {
  cutoff <- as.numeric(stats::quantile(r, 0.05, type = 8, names = FALSE))
  -10000 * mean(r[r <= cutoff])
}

maximum_drawdown <- function(r) {
  wealth <- cumprod(1 + r)
  100 * (-min(wealth / cummax(wealth) - 1))
}

sortino_ratio <- function(r) {
  downside <- pmin(r, 0)
  denom <- sqrt(mean(downside^2)) * sqrt(365)
  if (!is.finite(denom) || denom <= 0) return(NA_real_)
  mean(r) * 365 / denom
}

metric_value <- function(r, metric) {
  switch(metric,
         AnnVol = annualized_volatility(r),
         ES5 = expected_shortfall_5(r),
         MaxDD = maximum_drawdown(r),
         stop("Unknown metric: ", metric))
}

moving_block_indices <- function(n, block_length) {
  n_blocks <- ceiling(n / block_length)
  starts <- sample.int(n - block_length + 1L, n_blocks, replace = TRUE)
  idx <- unlist(lapply(starts, function(s) s:(s + block_length - 1L)), use.names = FALSE)
  idx[seq_len(n)]
}

prices <- read.csv(price_file, check.names = FALSE)
names(prices)[1L] <- "date"
prices$date <- parse_date(prices$date)
if (anyDuplicated(prices$date) || is.unsorted(prices$date)) stop("Price dates must be unique and sorted.")

lambdas <- read.csv(lambda_file, check.names = FALSE)
lambdas$date <- parse_date(lambdas$date)
if (anyDuplicated(lambdas$date) || is.unsorted(lambdas$date)) stop("Lambda dates must be unique and sorted.")

coin_names <- setdiff(names(lambdas), "date")
if (!setequal(coin_names, setdiff(names(prices), "date"))) {
  stop("Price and lambda universes differ.")
}
prices <- prices[c("date", coin_names)]
lambdas <- lambdas[c("date", coin_names)]

price_matrix_raw <- as.matrix(prices[, coin_names, drop = FALSE])
storage.mode(price_matrix_raw) <- "numeric"
missing_price_cells <- sum(!is.finite(price_matrix_raw))
price_matrix <- apply(price_matrix_raw, 2L, locf_past)
if (is.null(dim(price_matrix))) price_matrix <- matrix(price_matrix, ncol = length(coin_names))
colnames(price_matrix) <- coin_names
if (any(!is.finite(price_matrix)) || any(price_matrix <= 0)) stop("Invalid prices after past-only carry.")

simple_returns <- price_matrix[-1L, , drop = FALSE] / price_matrix[-nrow(price_matrix), , drop = FALSE] - 1
log_returns <- log(price_matrix[-1L, , drop = FALSE]) - log(price_matrix[-nrow(price_matrix), , drop = FALSE])
return_dates <- prices$date[-1L]

lambda_matrix <- as.matrix(lambdas[, coin_names, drop = FALSE])
storage.mode(lambda_matrix) <- "numeric"
if (any(!is.finite(lambda_matrix)) || any(lambda_matrix < 0)) stop("Invalid lambda input.")

lambda_cols <- paste0(coin_names, "_lambda")
return_cols <- paste0(coin_names, "_return")
log_return_cols <- paste0(coin_names, "_log")
colnames(lambda_matrix) <- lambda_cols
colnames(simple_returns) <- return_cols
colnames(log_returns) <- log_return_cols
return_panel <- data.frame(date = return_dates, simple_returns, check.names = FALSE)
log_return_panel <- data.frame(date = return_dates, log_returns, check.names = FALSE)
lambda_panel <- data.frame(date = lambdas$date, lambda_matrix, check.names = FALSE)

panel <- merge(lambda_panel, return_panel, by = "date", sort = TRUE)
panel <- merge(panel, log_return_panel, by = "date", sort = TRUE)

if (!all(c(lambda_cols, return_cols, log_return_cols) %in% names(panel))) {
  stop("Merged panel columns are incomplete.")
}

L <- as.matrix(panel[, lambda_cols, drop = FALSE])
R <- as.matrix(panel[, return_cols, drop = FALSE])
Rlog <- as.matrix(panel[, log_return_cols, drop = FALSE])
colnames(L) <- colnames(R) <- colnames(Rlog) <- coin_names
storage.mode(L) <- storage.mode(R) <- storage.mode(Rlog) <- "numeric"

window <- 90L
rebalance_every <- 7L
weight_cap <- 0.30
gamma_qtec <- 0.50
strategies <- c("EW", "GMV", "LTEC", "QTEC")
p <- length(coin_names)
ew <- rep(1 / p, p)

start_index <- window + 1L
if (nrow(panel) < start_index) stop("Insufficient observations.")
analysis_rows <- start_index:nrow(panel)
n_oos <- length(analysis_rows)

returns_out <- matrix(NA_real_, nrow = n_oos, ncol = length(strategies),
                      dimnames = list(NULL, strategies))
turnover_out <- returns_out
weights_array <- array(
  NA_real_, dim = c(n_oos, length(coin_names), length(strategies)),
  dimnames = list(NULL, coin_names, strategies)
)
pretrade <- lapply(strategies, function(x) ew)
names(pretrade) <- strategies

for (kk in seq_along(analysis_rows)) {
  t <- analysis_rows[kk]
  rebalance <- ((kk - 1L) %% rebalance_every) == 0L

  if (rebalance) {
    hist <- (t - window):(t - 1L)
    S_return <- stats::cov(Rlog[hist, , drop = FALSE])
    S_return[!is.finite(S_return)] <- 0
    S_return <- (S_return + t(S_return)) / 2
    S_return <- S_return + diag(max(mean(diag(S_return)), 1e-12) * 1e-6, p)

    lambda_now <- as.numeric(L[t - 1L, ])
    lambda_mean <- mean(lambda_now)
    if (!is.finite(lambda_mean) || lambda_mean <= 0) stop("Non-positive lambda mean.")
    lambda_scaled <- lambda_now / lambda_mean

    S_lambda <- stats::cov(L[hist, , drop = FALSE])
    S_lambda[!is.finite(S_lambda)] <- 0
    S_lambda <- (S_lambda + t(S_lambda)) / 2
    q0 <- as.numeric(t(ew) %*% S_lambda %*% ew)
    if (!is.finite(q0) || q0 <= 1e-12) q0 <- max(mean(diag(S_lambda)) / p, 1e-12)

    target <- list(
      EW = ew,
      GMV = solve_qp_capped(S_return, rep(0, p), weight_cap, pretrade$GMV),
      LTEC = solve_linear_capped(lambda_scaled, weight_cap),
      QTEC = solve_qp_capped(
        gamma_qtec * S_lambda / q0,
        (1 - gamma_qtec) * lambda_scaled,
        weight_cap,
        pretrade$QTEC
      )
    )
  } else {
    target <- pretrade
  }

  asset_return <- as.numeric(R[t, ])
  if (any(!is.finite(asset_return))) stop("Non-finite evaluation return.")

  for (strategy in strategies) {
    w_start <- as.numeric(target[[strategy]])
    if (rebalance) {
      turnover_out[kk, strategy] <- 0.5 * sum(abs(w_start - pretrade[[strategy]]))
    } else {
      turnover_out[kk, strategy] <- 0
    }
    weights_array[kk, , strategy] <- w_start
    rp <- sum(w_start * asset_return)
    returns_out[kk, strategy] <- rp
    gross_weights <- w_start * (1 + asset_return)
    pretrade[[strategy]] <- gross_weights / sum(gross_weights)
  }
}

oos_dates <- panel$date[analysis_rows]
daily_output <- data.frame(date = oos_dates, returns_out, check.names = FALSE)
for (strategy in strategies) daily_output[[paste0(strategy, "_turnover")]] <- turnover_out[, strategy]
write.csv(daily_output, file.path(bundle_dir, "source_data", "Portfolio_Daily_Returns.csv"), row.names = FALSE)

weights_long <- do.call(rbind, lapply(seq_along(strategies), function(s) {
  data.frame(
    date = rep(oos_dates, each = p),
    strategy = strategies[s],
    coin = rep(coin_names, times = n_oos),
    weight = as.vector(t(weights_array[, , s])),
    stringsAsFactors = FALSE
  )
}))
write.csv(weights_long, file.path(bundle_dir, "source_data", "Portfolio_Weights_Long.csv"), row.names = FALSE)

performance <- do.call(rbind, lapply(strategies, function(strategy) {
  r <- returns_out[, strategy]
  tr <- turnover_out[, strategy]
  data.frame(
    strategy = strategy,
    n_days = length(r),
    first_date = min(oos_dates),
    last_date = max(oos_dates),
    annualized_return_pct = annualized_return(r),
    annualized_volatility_pct = annualized_volatility(r),
    expected_shortfall_5_bps = expected_shortfall_5(r),
    maximum_drawdown_pct = maximum_drawdown(r),
    sortino_ratio = sortino_ratio(r),
    mean_turnover_per_rebalance_pct = 100 * mean(tr[tr > 0]),
    annualized_turnover = mean(tr) * 365,
    net_annualized_return_10bp_pct = annualized_return(r - 0.0010 * tr),
    stringsAsFactors = FALSE
  )
}))
write.csv(performance, file.path(bundle_dir, "tables", "Table_S6_3_OOS_Performance.csv"), row.names = FALSE)

weight_diagnostics <- do.call(rbind, lapply(strategies, function(strategy) {
  W <- weights_array[, , strategy, drop = TRUE]
  rebalance_rows <- seq(1L, nrow(W), by = rebalance_every)
  data.frame(
    strategy = strategy,
    mean_largest_weight_pct = 100 * mean(apply(W, 1L, max)),
    maximum_start_of_day_weight_pct = 100 * max(W),
    maximum_rebalance_target_weight_pct = 100 * max(W[rebalance_rows, , drop = FALSE]),
    mean_effective_number = mean(1 / rowSums(W^2)),
    minimum_effective_number = min(1 / rowSums(W^2)),
    weight_sum_max_error = max(abs(rowSums(W) - 1)),
    minimum_weight = min(W),
    stringsAsFactors = FALSE
  )
}))
write.csv(weight_diagnostics, file.path(bundle_dir, "tables", "Table_S6_3_Weight_Diagnostics.csv"), row.names = FALSE)

design_table <- data.frame(
  strategy = strategies,
  objective = c(
    "Equal allocation across the eleven-coin sample",
    "Minimize trailing reference-adjusted return variance",
    "Minimize lagged coin-specific DPI lambda exposure",
    "Minimize an equal-weighted combination of lambda covariance and lagged lambda exposure"
  ),
  information_set = c(
    "Fixed universe",
    "Trailing 90 days through t-1",
    "Lambda estimated through t-1",
    "Trailing 90-day lambda covariance and lambda through t-1"
  ),
  constraints = rep("Long-only; weights sum to one; 30% maximum per coin", 4L),
  stringsAsFactors = FALSE
)
write.csv(design_table, file.path(bundle_dir, "tables", "Table_6_3_Portfolio_Design.csv"), row.names = FALSE)
design_markdown <- c(
  "**Table 6.3 | Portfolio strategies and ex ante evaluation protocol.**",
  "",
  "| Strategy | Objective | Information set | Constraints |",
  "|---|---|---|---|",
  paste0("| ", design_table$strategy, " | ", design_table$objective, " | ",
         design_table$information_set, " | ", design_table$constraints, " |")
)
writeLines(design_markdown,
           file.path(bundle_dir, "tables", "Table_6_3_Manuscript_Ready.md"))

set.seed(6303)
B <- 1999L
block_length <- 90L
metrics <- c("AnnVol", "ES5", "MaxDD")
comparators <- setdiff(strategies, "EW")
observed <- expand.grid(strategy = comparators, metric = metrics, stringsAsFactors = FALSE)
observed$estimate <- mapply(function(strategy, metric) {
  metric_value(returns_out[, strategy], metric) - metric_value(returns_out[, "EW"], metric)
}, observed$strategy, observed$metric)

boot_store <- array(NA_real_, dim = c(B, length(comparators), length(metrics)),
                    dimnames = list(NULL, comparators, metrics))
for (b in seq_len(B)) {
  idx <- moving_block_indices(n_oos, block_length)
  for (strategy in comparators) {
    for (metric in metrics) {
      boot_store[b, strategy, metric] <-
        metric_value(returns_out[idx, strategy], metric) -
        metric_value(returns_out[idx, "EW"], metric)
    }
  }
}

bootstrap_results <- observed
bootstrap_results$ci95_low <- NA_real_
bootstrap_results$ci95_high <- NA_real_
bootstrap_results$p_value <- NA_real_
for (i in seq_len(nrow(bootstrap_results))) {
  x <- boot_store[, bootstrap_results$strategy[i], bootstrap_results$metric[i]]
  bootstrap_results$ci95_low[i] <- stats::quantile(x, 0.025, names = FALSE, type = 8)
  bootstrap_results$ci95_high[i] <- stats::quantile(x, 0.975, names = FALSE, type = 8)
  p_lower <- (1 + sum(x <= 0)) / (B + 1)
  p_upper <- (1 + sum(x >= 0)) / (B + 1)
  bootstrap_results$p_value[i] <- min(1, 2 * min(p_lower, p_upper))
}
bootstrap_results$p_holm_primary <- NA_real_
primary_family <- bootstrap_results$metric %in% c("AnnVol", "ES5")
bootstrap_results$p_holm_primary[primary_family] <-
  p.adjust(bootstrap_results$p_value[primary_family], method = "holm")
bootstrap_results$lower_is_better <- TRUE
bootstrap_results$block_length <- block_length
bootstrap_results$bootstrap_repetitions <- B
write.csv(bootstrap_results,
          file.path(bundle_dir, "tables", "Table_S6_3_Block_Bootstrap.csv"),
          row.names = FALSE)
metric_label <- c(AnnVol = "Annualised volatility (percentage points)",
                  ES5 = "5% Expected Shortfall (basis points)",
                  MaxDD = "Maximum drawdown (percentage points)")
inference_markdown <- c(
  "**Supplementary Table S6.3 | Paired out-of-sample differences relative to EW.**",
  "",
  "Negative differences indicate lower risk than EW. Confidence intervals and p values use 1,999 paired moving-block bootstrap replications with 90-day blocks. Holm adjustment is applied to the six annualised-volatility and Expected-Shortfall comparisons; maximum drawdown is secondary.",
  "",
  "| Strategy | Metric | Difference | 95% interval | p | Holm p |",
  "|---|---|---:|---:|---:|---:|",
  vapply(seq_len(nrow(bootstrap_results)), function(i) {
    hp <- bootstrap_results$p_holm_primary[i]
    hp_text <- if (is.finite(hp)) sprintf("%.3f", hp) else "--"
    sprintf("| %s | %s | %.2f | %.2f to %.2f | %.3f | %s |",
            bootstrap_results$strategy[i], metric_label[bootstrap_results$metric[i]],
            bootstrap_results$estimate[i], bootstrap_results$ci95_low[i],
            bootstrap_results$ci95_high[i], bootstrap_results$p_value[i], hp_text)
  }, character(1))
)
writeLines(inference_markdown,
           file.path(bundle_dir, "tables", "Table_S6_3_Inference_Manuscript.md"))

qa <- data.frame(
  check = c(
    "price_lambda_universe_match",
    "past_only_price_carry_cells",
    "oos_first_date",
    "oos_last_date",
    "oos_days",
    "rebalance_frequency_days",
    "rolling_window_days",
    "weight_cap",
    "all_returns_finite",
    "weight_sums_equal_one",
    "weights_nonnegative",
    "rebalance_target_cap_respected",
    "maximum_between_rebalance_drift_weight",
    "bootstrap_repetitions",
    "bootstrap_block_length"
  ),
  value = c(
    identical(sort(coin_names), sort(setdiff(names(prices), "date"))),
    missing_price_cells,
    as.character(min(oos_dates)),
    as.character(max(oos_dates)),
    n_oos,
    rebalance_every,
    window,
    weight_cap,
    all(is.finite(returns_out)),
    max(abs(apply(weights_array, c(1L, 3L), sum) - 1)) < 1e-8,
    min(weights_array) >= -1e-10,
    max(weights_array[seq(1L, n_oos, by = rebalance_every), , , drop = FALSE]) <= weight_cap + 1e-8,
    max(weights_array),
    B,
    block_length
  ),
  stringsAsFactors = FALSE
)
write.csv(qa, file.path(bundle_dir, "qa", "analysis_QA.csv"), row.names = FALSE)

manifest <- data.frame(
  file = normalizePath(c(price_file, lambda_file), mustWork = TRUE),
  md5 = unname(tools::md5sum(c(price_file, lambda_file))),
  stringsAsFactors = FALSE
)
write.csv(manifest, file.path(bundle_dir, "qa", "input_manifest_md5.csv"), row.names = FALSE)
capture.output(sessionInfo(), file = file.path(bundle_dir, "qa", "session_info.txt"))

cat("Section 6.3 portfolio protocol completed.\n")
print(performance, row.names = FALSE)
cat("\nMoving-block bootstrap differences versus EW (negative is better):\n")
print(bootstrap_results, row.names = FALSE)
