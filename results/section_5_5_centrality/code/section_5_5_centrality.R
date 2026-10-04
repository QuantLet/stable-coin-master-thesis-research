#!/usr/bin/env Rscript
# Section 5.5: centrality of the frozen reference-adjusted Quantile-Lasso network.
# Does not re-estimate the Chapter 4 DPI or change the Chapter 5.2 coefficients.

options(stringsAsFactors = FALSE, digits = 15, scipen = 999)
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(arg) != 1L) stop("Run with Rscript.")
root <- dirname(dirname(normalizePath(sub("^--file=", "", arg))))
input <- file.path(root, "source_data", "input")
out <- file.path(root, "source_data")
tables <- file.path(root, "tables")
figures <- file.path(root, "figures")
qa <- file.path(root, "qa")
invisible(lapply(c(out, tables, figures, qa), dir.create,
                 recursive = TRUE, showWarnings = FALSE))

coins <- c("USDT", "USDC", "BUSD", "TUSD", "USDP", "GUSD",
           "DAI", "sUSD", "EURS", "IDRT", "PAXG")
n <- length(coins)
tol <- 1e-10
coef_path <- file.path(input, "selected_coefficients_strict.csv.gz")
raw_path <- file.path(input, "selected_coefficients_raw_strict.csv.gz")
dpi_path <- file.path(input, "dpi_primary_robust_mad_strict.csv")
flag_path <- file.path(input, "output_window_imputation_flags.csv")
stopifnot(all(file.exists(c(coef_path, raw_path, dpi_path, flag_path))))

write_data <- function(x, path) {
  write.csv(x, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

read_coef <- function(path) {
  x <- read.csv(gzfile(path), check.names = FALSE)
  stopifnot(all(c("date", "target", "predictor", "predictor_type", "coefficient") %in% names(x)))
  x <- x[x$predictor_type == "stablecoin", ]
  x$date <- as.Date(x$date)
  stopifnot(!anyNA(x$date), !anyNA(x$coefficient),
            !anyDuplicated(x[c("date", "target", "predictor")]),
            setequal(unique(x$target), coins), setequal(unique(x$predictor), coins),
            !any(x$target == x$predictor))
  x
}

coefs <- read_coef(coef_path)
raw <- read_coef(raw_path)
dates <- sort(unique(coefs$date))
stopifnot(length(dates) == 2252L, nrow(coefs) == length(dates) * n * (n - 1L),
          setequal(unique(raw$date), dates), nrow(raw) == nrow(coefs))

make_array <- function(x) {
  a <- array(0, dim = c(n, n, length(dates)),
             dimnames = list(coins, coins, as.character(dates)))
  si <- match(x$predictor, coins)
  ti <- match(x$target, coins)
  di <- match(x$date, dates)
  a[cbind(si, ti, di)] <- x$coefficient
  a
}

beta <- make_array(coefs)
raw_beta <- make_array(raw)

# A[source,target] = positive selected coefficient in the target lower-tail equation.
# Negative coefficients describe inverse conditional links and are excluded from
# the downside-aligned main network. Self-links are structurally absent.
positive <- pmax(beta, 0)
positive[positive <= tol] <- 0
raw_positive <- pmax(raw_beta, 0)
raw_positive[raw_positive <= tol] <- 0
binary_positive <- (positive > 0) * 1
absolute <- abs(beta)
absolute[absolute <= tol] <- 0

centrality <- function(a) {
  out <- matrix(NA_real_, length(dates), n, dimnames = list(NULL, coins))
  inn <- out
  eig <- out
  close <- out
  density <- rep(NA_real_, length(dates))
  for (tt in seq_along(dates)) {
    A <- a[, , tt]
    diag(A) <- 0
    out[tt, ] <- rowSums(A) / (n - 1L)
    inn[tt, ] <- colSums(A) / (n - 1L)
    density[tt] <- sum(A > 0) / (n * (n - 1L))

    # Weighted inward eigenvector centrality: v_j proportional to sum_i A_ij v_i.
    if (any(A > 0)) {
      ee <- eigen(t(A))
      k <- which.max(Re(ee$values))
      vv <- Re(ee$vectors[, k])
      if (sum(vv) < 0) vv <- -vv
      vv[vv < 0 & vv > -1e-8] <- 0
      eig[tt, ] <- if (all(vv >= 0) && sum(vv) > 0) vv / sum(vv) else rep(NA_real_, n)
    } else {
      eig[tt, ] <- rep(1 / n, n)
    }

    # Weighted outward harmonic closeness remains defined in disconnected graphs.
    D <- matrix(Inf, n, n)
    D[A > 0] <- 1 / A[A > 0]
    diag(D) <- 0
    for (kk in seq_len(n)) D <- pmin(D, outer(D[, kk], D[kk, ], "+"))
    diag(D) <- Inf
    invD <- 1 / D
    invD[!is.finite(invD)] <- 0
    close[tt, ] <- rowSums(invD) / (n - 1L)
  }
  list(out = out, in_strength = inn, eigen_in = eig,
       harmonic_out = close, density = density)
}

main <- centrality(positive)
binary <- centrality(binary_positive)
raw_net <- centrality(raw_positive)
abs_net <- centrality(absolute)

dpi <- read.csv(dpi_path)
dpi$date <- as.Date(dpi$date)
dpi <- dpi[match(dates, dpi$date), ]
stopifnot(!anyNA(dpi$date))
flags <- read.csv(flag_path)
flags$date <- as.Date(flags$date)
flags <- flags[match(dates, flags$date), ]
stopifnot(!anyNA(flags$date))
clean <- flags$imputed_cells_in_window == 0

daily <- data.frame(date = rep(dates, each = n), coin = rep(coins, times = length(dates)),
                    out_strength = as.vector(t(main$out)),
                    in_strength = as.vector(t(main$in_strength)),
                    inward_eigenvector = as.vector(t(main$eigen_in)),
                    outward_harmonic_closeness = as.vector(t(main$harmonic_out)),
                    imputed_cells_in_window = rep(flags$imputed_cells_in_window, each = n))
write_data(daily, file.path(out, "Daily_Node_Centrality.csv"))

system <- data.frame(date = dates, dpi = dpi$dpi_primary,
                     aligned_density = main$density,
                     eigen_hhi = rowSums(main$eigen_in^2),
                     clean_window = clean)
write_data(system, file.path(out, "Daily_Network_Summary.csv"))

# Calendar dates are the sampling units. Circular 90-day blocks preserve the
# overlapping-window dependence, resampling all nodes jointly.
B <- 1999L
block <- 90L
set.seed(5505L)
boot_idx <- matrix(NA_integer_, nrow = length(dates), ncol = B)
for (b in seq_len(B)) {
  starts <- sample.int(length(dates), ceiling(length(dates) / block), replace = TRUE)
  idx <- unlist(lapply(starts, function(s) ((s - 1L + seq_len(block) - 1L) %% length(dates)) + 1L),
                use.names = FALSE)
  boot_idx[, b] <- idx[seq_along(dates)]
}

metrics <- main[c("out", "in_strength", "eigen_in", "harmonic_out")]
metric_names <- c(out = "out_strength", in_strength = "in_strength",
                  eigen_in = "inward_eigenvector", harmonic_out = "outward_harmonic_closeness")
summary_rows <- list()
for (name in names(metrics)) {
  X <- metrics[[name]]
  boot_mean <- vapply(seq_len(B), function(b) colMeans(X[boot_idx[, b], , drop = FALSE], na.rm = TRUE),
                      numeric(n))
  if (is.null(dim(boot_mean))) boot_mean <- matrix(boot_mean, nrow = n)
  avg <- colMeans(X, na.rm = TRUE)
  summary_rows[[name]] <- data.frame(
    coin = coins, metric = metric_names[[name]], mean = avg,
    ci_low = apply(boot_mean, 1, quantile, probs = 0.025, na.rm = TRUE),
    ci_high = apply(boot_mean, 1, quantile, probs = 0.975, na.rm = TRUE),
    full_sample_rank = rank(-avg, ties.method = "min"),
    bootstrap_top_rank_probability = rowMeans(boot_mean == rep(colMaxs <- apply(boot_mean, 2, max),
                                                              each = n)),
    first_half_mean = colMeans(X[seq_len(floor(nrow(X) / 2)), , drop = FALSE], na.rm = TRUE),
    second_half_mean = colMeans(X[(floor(nrow(X) / 2) + 1L):nrow(X), , drop = FALSE], na.rm = TRUE),
    clean_window_mean = colMeans(X[clean, , drop = FALSE], na.rm = TRUE)
  )
}
node_summary <- do.call(rbind, summary_rows)
rownames(node_summary) <- NULL
write_data(node_summary, file.path(tables, "Table_S5_5_All_Node_Centralities.csv"))

# Role asymmetry is compared within date; Holm correction covers all 11 coins.
diff_mat <- main$out - main$in_strength
hac_mean <- function(x, lag = block) {
  x <- x[is.finite(x)]
  m <- mean(x)
  u <- x - m
  nn <- length(x)
  L <- min(lag, nn - 1L)
  lrv <- sum(u * u) / nn
  for (ell in seq_len(L)) {
    gamma <- sum(u[(ell + 1L):nn] * u[seq_len(nn - ell)]) / nn
    lrv <- lrv + 2 * (1 - ell / (L + 1)) * gamma
  }
  se <- sqrt(max(lrv, 0) / nn)
  c(estimate = m, se = se, ci_low = m - 1.96 * se,
    ci_high = m + 1.96 * se, p = if (se > 0) 2 * pnorm(-abs(m / se)) else NA_real_)
}
role <- as.data.frame(t(apply(diff_mat, 2, hac_mean)))
role$coin <- coins
role$p_holm_11 <- p.adjust(role$p, method = "holm")
role <- role[, c("coin", "estimate", "se", "ci_low", "ci_high", "p", "p_holm_11")]
write_data(role, file.path(tables, "Table_S5_5_Out_Minus_In_HAC90.csv"))

rank_spearman <- function(x, y) cor(rank(x), rank(y), method = "pearson")
sensitivity <- do.call(rbind, lapply(c("out", "in_strength", "eigen_in", "harmonic_out"), function(name) {
  ref <- colMeans(main[[name]], na.rm = TRUE)
  data.frame(metric = metric_names[[name]],
             binary_rank_rho = rank_spearman(ref, colMeans(binary[[name]], na.rm = TRUE)),
             raw_unscaled_rank_rho = rank_spearman(ref, colMeans(raw_net[[name]], na.rm = TRUE)),
             absolute_signed_rank_rho = rank_spearman(ref, colMeans(abs_net[[name]], na.rm = TRUE)),
             clean_window_rank_rho = rank_spearman(ref, colMeans(main[[name]][clean, , drop = FALSE], na.rm = TRUE)),
             half_sample_rank_rho = rank_spearman(
               colMeans(main[[name]][seq_len(floor(length(dates) / 2)), , drop = FALSE], na.rm = TRUE),
               colMeans(main[[name]][(floor(length(dates) / 2) + 1L):length(dates), , drop = FALSE], na.rm = TRUE)))
}))
write_data(sensitivity, file.path(tables, "Table_S5_5_Specification_Rank_Sensitivity.csv"))

sensitivity_nodes <- do.call(rbind, lapply(c("out", "in_strength", "eigen_in", "harmonic_out"), function(name) {
  data.frame(coin = coins, metric = metric_names[[name]],
             positive_weighted = colMeans(main[[name]], na.rm = TRUE),
             positive_binary = colMeans(binary[[name]], na.rm = TRUE),
             unscaled_positive_weighted = colMeans(raw_net[[name]], na.rm = TRUE),
             absolute_weighted = colMeans(abs_net[[name]], na.rm = TRUE))
}))
write_data(sensitivity_nodes, file.path(tables, "Table_S5_5_Node_Sensitivity.csv"))

main_table <- merge(subset(node_summary, metric == "out_strength",
                           select = c(coin, mean, ci_low, ci_high, full_sample_rank)),
                    subset(node_summary, metric == "in_strength",
                           select = c(coin, mean, ci_low, ci_high, full_sample_rank)),
                    by = "coin", suffixes = c("_out", "_in"))
main_table <- main_table[order(main_table$full_sample_rank_out), ]
write_data(main_table, file.path(tables, "Table_5_5_Network_Roles.csv"))
write_data(main_table, file.path(out, "Figure_5_5_Source_Data.csv"))

# One-panel descriptive role map. Intervals and inference are in Table 5.5.
draw_figure <- function() {
  par(mar = c(4.2, 4.3, 1.1, 0.7), family = "sans", cex.axis = 0.85,
      cex.lab = 0.9, bty = "n")
  y <- rev(seq_len(n))
  lim <- range(c(main_table$mean_out, main_table$mean_in))
  plot(NA, xlim = c(0, lim[2] * 1.12), ylim = c(0.5, n + 0.5),
       xlab = "Mean positive coefficient per possible link", ylab = "",
       axes = FALSE)
  abline(v = pretty(c(0, lim[2])), col = "#E6E6E6", lwd = 0.7)
  segments(main_table$mean_out, y, main_table$mean_in, y,
           col = "#B6BEC7", lwd = 1.6)
  points(main_table$mean_out, y, pch = 16, cex = 0.95, col = "#1F5D7A")
  points(main_table$mean_in, y, pch = 17, cex = 0.95, col = "#B45A42")
  axis(1, cex.axis = 0.85)
  axis(2, at = y, labels = main_table$coin, las = 1, tick = FALSE, cex.axis = 0.9)
  legend("topleft", legend = c("Outgoing", "Incoming"),
         pch = c(16, 17), col = c("#1F5D7A", "#B45A42"),
         bty = "n", horiz = TRUE, cex = 0.8)
}
pdf_path <- file.path(figures, "Figure_5_5_Network_Roles.pdf")
if (identical(Sys.getenv("SECTION55_CAIRO_PDF"), "1")) {
  grDevices::cairo_pdf(pdf_path, width = 6.9, height = 4.1, family = "Helvetica")
} else {
  grDevices::pdf(pdf_path, width = 6.9, height = 4.1,
                 family = "Helvetica", useDingbats = FALSE)
}
draw_figure()
dev.off()
ragg::agg_png(file.path(figures, "Figure_5_5_Network_Roles.png"),
              width = 6.9, height = 4.1, units = "in", res = 300)
draw_figure()
dev.off()
svglite::svglite(file.path(figures, "Figure_5_5_Network_Roles.svg"),
                 width = 6.9, height = 4.1)
draw_figure()
dev.off()
ragg::agg_tiff(file.path(figures, "Figure_5_5_Network_Roles.tiff"),
               width = 6.9, height = 4.1, units = "in", res = 600)
draw_figure()
dev.off()

qa_table <- data.frame(check = c("windows", "coins", "directed_pairs_per_window",
                                 "coefficient_rows", "clean_windows", "positive_density",
                                 "negative_edges_excluded", "nonfinite_node_metrics"),
                       value = c(length(dates), n, n * (n - 1L), nrow(coefs),
                                 sum(clean), mean(main$density),
                                 sum(beta < -tol),
                                 sum(!is.finite(unlist(metrics)))))
write_data(qa_table, file.path(qa, "analysis_QA.csv"))
write_data(data.frame(file = basename(c(coef_path, raw_path, dpi_path, flag_path)),
                      md5 = unname(tools::md5sum(c(coef_path, raw_path, dpi_path, flag_path)))),
           file.path(qa, "input_md5.csv"))
writeLines(capture.output(sessionInfo()), file.path(qa, "R_session_info.txt"))
cat("Section 5.5 complete.\n")
