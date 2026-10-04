
# ===============================================================
# Build FIXED adjacency matrices from CSV inputs (no RData needed)
# - Uses FRM_SC_config.R (paths, dates, s/tau/I/J, channel)
# - Reads your input CSVs via FRM_SC_load_data.R (if needed)
# - Writes: Output/<channel>/Adj_Matrices/Fixed/adj_matrix_YYYYMMDD.csv
# - Writes: Output/<channel>/Lambda/Fixed/lambdas_fixed_<start>_<end>.csv
# ===============================================================

rm(list = setdiff(ls(), c()))
suppressPackageStartupMessages({ library(zoo) })

# ---- config + utils + FRM algorithm ----
source("FRM_SC_config.R")
source("FRM_SC_utils.R")
if (!file.exists("FRM_Statistics_Algorithm.R"))
  stop("Missing FRM_Statistics_Algorithm.R")
source("FRM_Statistics_Algorithm.R")

# ---- load stage_20 or build it from CSVs once ----
st20 <- file.path(output_path, "stage_20_loaded.RData")
if (!file.exists(st20)) {
  message("stage_20_loaded.RData not found — running FRM_SC_load_data.R once...")
  source("FRM_SC_load_data.R")
}
load(file.path(output_path, "stage_20_loaded.RData"))
# objects we expect now: dates, ticker, stock_return, macro_return, M_stock, M_macro, mktcap_index, s, tau, I, J

# ---- ensure output folders ----
fixed_dir  <- file.path(output_path, "Adj_Matrices", "Fixed")
lambda_dir <- file.path(output_path, "Lambda", "Fixed")
dir.create(fixed_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(lambda_dir, recursive = TRUE, showWarnings = FALSE)

# ---- helpers for date indexing ----
idx_start_at_or_after <- function(key, target){ w <- which(key >= target); if (length(w)) w[1] else NA_integer_ }
idx_end_at_or_before  <- function(key, target){ w <- which(key <= target); if (length(w)) tail(w,1) else NA_integer_ }

# ---- fixed window from config ----
N0_fixed <- idx_start_at_or_after(ticker, date_start_fixed)
N1_fixed <- idx_end_at_or_before( ticker, date_end_fixed)
if (is.na(N0_fixed) || is.na(N1_fixed) || (N1_fixed - N0_fixed + 1) < s) {
  stop(sprintf("Fixed window [%s, %s] too short for s=%d.", date_start_fixed, date_end_fixed, s))
}

# ---- lock Top-J once at the START of the fixed window ----
J_eff <- min(J, M_stock)
biggest_index_fixed <- as.integer(mktcap_index[N0_fixed, 2:(J_eff + 1), drop = FALSE])
M_J <- J_eff + M_macro

coin_names  <- colnames(stock_return)
macro_names <- if (M_macro > 0) colnames(macro_return) else character(0)
col_full    <- c(coin_names[biggest_index_fixed], macro_names)

# ---- iterate days in fixed window and create CSV adjacencies ----
N_fixed <- N1_fixed - N0_fixed + 1
lambdas_fixed <- matrix(NA_real_, N_fixed, J_eff + 1)
lambdas_fixed[, 1] <- ticker[N0_fixed:N1_fixed]

n_written <- 0L
for (t in N0_fixed:N1_fixed) {
  rows_ret <- (t - s):(t - 1)
  if (min(rows_ret) < 1) next

  X <- cbind(
    stock_return[rows_ret, biggest_index_fixed, drop = FALSE],
    if (M_macro > 0) macro_return[rows_ret, , drop = FALSE] else NULL
  )
  X[!is.finite(X)] <- 0
  if (!all(colSums(X != 0) > 0)) next

  A <- matrix(0, M_J, M_J)
  lam_row <- rep(NA_real_, J_eff)

  for (k in 1:M_J) {
    est <- tryCatch(FRM_Quantile_Regression(as.matrix(X), k, tau, I), error = function(e) NULL)
    if (is.null(est)) next
    gacv   <- est$Cgacv
    k_best <- if (!any(is.finite(gacv))) length(gacv) else which(gacv == min(gacv[is.finite(gacv)], na.rm = TRUE))[1]
    est_lambda <- abs(data.matrix(est$lambda[k_best]))
    est_beta   <- t(as.matrix(est$beta[k_best, ]))
    A[k, -k] <- est_beta
    if (k <= J_eff) lam_row[k] <- est_lambda
  }

  colnames(A) <- col_full
  rownames(A) <- col_full

  out_csv <- file.path(fixed_dir, paste0("adj_matrix_", ticker[t], ".csv"))
  write.csv(A, out_csv, quote = FALSE)
  n_written <- n_written + 1L

  lambdas_fixed[t - N0_fixed + 1, 2:(J_eff + 1)] <- lam_row
}

# ---- save helper lambdas CSV for the fixed universe ----
colnames(lambdas_fixed) <- c("date", coin_names[biggest_index_fixed])
lam_csv <- file.path(lambda_dir, paste0("lambdas_fixed_", date_start_fixed, "_", date_end_fixed, ".csv"))
write.csv(lambdas_fixed, lam_csv, row.names = FALSE, quote = FALSE)

message("✅ Fixed adjacency CSVs written: ", n_written)
message("📁 Fixed folder: ", normalizePath(fixed_dir))
message("🧾 Lambdas CSV:  ", normalizePath(lam_csv))

