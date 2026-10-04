
# ===============================================================
# Network GIF (FIXED) — circular layout, bottom DATE — FRM label
# Nodes sized by lambda (global scaling), UPPERCASE labels
# Edges: into main coin = blue; out of main coin = orange; others grey
# Transparent background
# ===============================================================

suppressPackageStartupMessages({
  library(igraph)
  library(magick)
  library(scales)
})

# ---------- 0) Config ----------
stopifnot(file.exists("FRM_SC_config.R"))
source("FRM_SC_config.R")     # output_path, website_path, channel, date_end, date_start_fixed, date_end_fixed, etc.
if (file.exists("FRM_SC_utils.R")) source("FRM_SC_utils.R")  # pretty_coin(), optional

fixed_adj_dir <- file.path(output_path, "Adj_Matrices", "Fixed")
frm_csv       <- file.path(output_path, "Lambda", paste0("FRM_", channel, "_index.csv"))
out_dir       <- file.path(website_path, date_end, "Fixed")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
gif_path      <- file.path(out_dir, paste0("Network_", date_start_fixed, "_", date_end_fixed, "_", channel, ".gif"))

# ---------- 1) FRM index & lookup ----------
stopifnot(file.exists(frm_csv))
FRM_index <- read.csv(frm_csv, stringsAsFactors = FALSE, check.names = FALSE)
names(FRM_index) <- tolower(names(FRM_index))
stopifnot(all(c("date","frm") %in% names(FRM_index)))
FRM_index$date <- suppressWarnings(as.Date(FRM_index$date))
if (any(is.na(FRM_index$date))) {
  d1 <- suppressWarnings(as.Date(FRM_index$date, "%Y-%m-%d"))
  d2 <- suppressWarnings(as.Date(FRM_index$date, "%Y/%m/%d"))
  FRM_index$date[is.na(FRM_index$date)] <- d1[is.na(FRM_index$date)]
  FRM_index$date[is.na(FRM_index$date)] <- d2[is.na(FRM_index$date)]
}
frm_lookup <- function(d){  # d is Date
  if (is.na(d)) return(NA_real_)
  w <- which(FRM_index$date == d)
  if (!length(w)) return(NA_real_)
  FRM_index$frm[w[1]]
}

# ---------- 2) Ensure ticker/N0/N1 ----------
if (!(exists("ticker") && length(ticker) > 0)) {
  f <- file.path(output_path, "stage_20_loaded.RData")
  if (file.exists(f)) load(f)
}
stopifnot(exists("ticker") && length(ticker) > 0)

idx_start_at_or_after <- function(key, target){ w <- which(key >= target); if (length(w)) w[1] else NA_integer_ }
idx_end_at_or_before  <- function(key, target){ w <- which(key <= target); if (length(w)) tail(w,1) else NA_integer_ }

if (!exists("N0_fixed") || !is.numeric(N0_fixed)) N0_fixed <- idx_start_at_or_after(ticker, date_start_fixed)
if (!exists("N1_fixed") || !is.numeric(N1_fixed)) N1_fixed <- idx_end_at_or_before( ticker, date_end_fixed)
stopifnot(is.finite(N0_fixed), is.finite(N1_fixed), N1_fixed >= N0_fixed)

# ---------- 3) Lambda table (prefer in-memory; else latest CSV) ----------
if (!exists("FRM_individ_fixed")) {
  lf_dir <- file.path(output_path, "Lambda", "Fixed")
  lf_all <- list.files(lf_dir, pattern = "^lambdas_fixed_.*\\.csv$", full.names = TRUE)
  if (!length(lf_all)) stop("No lambdas_fixed_*.csv found in: ", normalizePath(lf_dir))
  lf_path <- lf_all[which.max(file.info(lf_all)$mtime)]
  LFR     <- read.csv(lf_path, stringsAsFactors = FALSE, check.names = FALSE)
  stopifnot("date" %in% names(LFR))
  key <- gsub("\\D", "", as.character(LFR[[1]]))  # keep digits only (YYYYMMDD)
  FRM_individ_fixed <- cbind(as.integer(key), LFR[ , setdiff(names(LFR), "date"), drop = FALSE])
  colnames(FRM_individ_fixed)[1] <- "date"
}
if (!is.numeric(FRM_individ_fixed[,1])) {
  FRM_individ_fixed[,1] <- as.integer(gsub("\\D","", as.character(FRM_individ_fixed[,1])))
}

lambdas_fixed_vals <- as.matrix(FRM_individ_fixed[, -1, drop = FALSE])
storage.mode(lambdas_fixed_vals) <- "numeric"
lambdas_fixed_vals[!is.finite(lambdas_fixed_vals)] <- 0

if (!exists("J_eff_fixed") || !is.numeric(J_eff_fixed) || J_eff_fixed < 1) {
  J_eff_fixed <- ncol(lambdas_fixed_vals)
}

# Global scaling for node sizes (stable across frames)
rng <- range(lambdas_fixed_vals, na.rm = TRUE)
rescale_safe <- function(v) {
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) return(rep(10, length(v)))
  scales::rescale(v, to = c(6, 22), from = rng)
}

# ---------- 4) Labels & main coin ----------
if (!exists("pretty_coin") || !is.function(pretty_coin)) pretty_coin <- function(x) x
if (!exists("stock_main") || !is.character(stock_main) || !length(stock_main)) {
  means <- colMeans(lambdas_fixed_vals, na.rm = TRUE)
  stock_main <- names(sort(means, decreasing = TRUE))[1]
  if (!length(stock_main)) stock_main <- colnames(lambdas_fixed_vals)[1]
}
label_vec <- function(cols){
  if (exists("COIN_LABELS")) {
    if (is.list(COIN_LABELS)) {
      return(vapply(cols, function(x) if (!is.null(COIN_LABELS[[x]])) as.character(COIN_LABELS[[x]]) else x, ""))
    } else {
      out <- as.character(COIN_LABELS[cols]); out[is.na(out)] <- cols; return(out)
    }
  }
  cols
}

# ---------- 5) Render frames (bottom margin text) ----------
stopifnot(dir.exists(fixed_adj_dir))
fig <- image_graph(width = 1200, height = 1200, res = 120, bg = "transparent")
options(show.error.messages = FALSE)

for (t in N0_fixed:N1_fixed) {
  fcsv <- file.path(fixed_adj_dir, paste0("adj_matrix_", ticker[t], ".csv"))
  if (!file.exists(fcsv)) next

  adj0 <- tryCatch(read.csv(fcsv, header = TRUE, sep = ",", row.names = 1, check.names = FALSE),
                   error = function(e) NULL)
  if (is.null(adj0)) next
  adj0 <- as.matrix(adj0)
  storage.mode(adj0) <- "numeric"
  adj0[!is.finite(adj0)] <- 0
  J_here <- min(J_eff_fixed, nrow(adj0), ncol(adj0))
  adj0   <- adj0[1:J_here, 1:J_here, drop = FALSE]
  diag(adj0) <- 0

  netw1 <- graph_from_adjacency_matrix(adj0, mode = "directed", weighted = TRUE)

  row_id <- which(as.integer(FRM_individ_fixed[, 1]) == as.integer(ticker[t]))
  if (length(row_id)) row_id <- row_id[1]
  sizes <- if (length(row_id) && row_id >= 1 && row_id <= nrow(lambdas_fixed_vals)) {
    v <- as.numeric(lambdas_fixed_vals[row_id, 1:J_here]); v[!is.finite(v)] <- 0; rescale_safe(v)
  } else rep(10, J_here)

  cols_now <- colnames(adj0)
  lbl_now  <- toupper(label_vec(cols_now))  # UPPERCASE
  main_lbl <- toupper(pretty_coin(stock_main))

  V(netw1)$label <- lbl_now
  V(netw1)$size  <- sizes
  V(netw1)$color <- ifelse(lbl_now == main_lbl, "orange", "lightgrey")
  V(netw1)$frame.color <- NA
  V(netw1)$label.cex <- 0.9

  ecols <- rep("gray", length(E(netw1)))
  if (ecount(netw1) > 0) {
    heads <- head_of(netw1, E(netw1))$name
    tails <- tail_of(netw1, E(netw1))$name
    ecols[heads == stock_main] <- "blue"    # into main coin
    ecols[tails == stock_main] <- "orange"  # out of main coin
  }
  E(netw1)$color <- ecols
  E(netw1)$width <- 0.8
  E(netw1)$arrow.size  <- 0.9
  E(netw1)$arrow.width <- 1

  # Bottom margin text (outside the circle)
  par(mar = c(7, 0, 0, 0), xpd = NA, bg = NA)
  plot(netw1,
       layout = layout_in_circle,
       edge.curved = 0.15,
       asp = 0)

  this_date <- as.Date(as.character(ticker[t]), "%Y%m%d")
  frm_val   <- frm_lookup(this_date)
  lab <- if (is.finite(frm_val)) {
    paste0(format(this_date, "%Y-%m-%d"), "  —  FRM: ", formatC(frm_val, format = "f", digits = 5))
  } else {
    format(this_date, "%Y-%m-%d")
  }
  mtext(lab, side = 1, line = 2.6, cex = 1.1, font = 2)
}

options(show.error.messages = TRUE)
dev.off()

# ---------- 6) Animate & save ----------
animation <- image_animate(fig, fps = 5, loop = 0)
image_write(animation, gif_path)
message("Network GIF saved to: ", normalizePath(gif_path))

