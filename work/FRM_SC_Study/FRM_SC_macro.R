# ==== FRM_SC_macro.R ====================================================
M_macro <- if (exists('macro_return') && !is.null(macro_return)) ncol(macro_return) else 0
if (M_macro > 0) {
  N_h <- length(FRM_history)
  macro_inf <- matrix(0, N_h, M_macro + 1)
  macro_inf[, 1] <- names(FRM_history)
  colnames(macro)[1] <- 'date'
  colnames(macro_inf) <- colnames(macro)

  for (t in 1:N_h) {
    fcsv <- file.path(output_path, 'Adj_Matrices', paste0('adj_matrix_', gsub('-', '', names(FRM_history)[t]), '.csv'))
    if (!file.exists(fcsv)) next
    adj0 <- read.csv(fcsv, header = TRUE, sep = ',', row.names = 1)
    k1 <- ncol(adj0) - M_macro
    if (k1 > 0) for (k in 1:M_macro) macro_inf[t, k+1] <- sum(adj0[1:k1, k1 + k] != 0) / k1
  }
  write.csv(macro_inf, file.path(output_path, 'Macro', 'macro_influence.csv'), row.names = FALSE, quote = FALSE)

  macro_inf_long <- tidyr::gather(as.data.frame(macro_inf), macro, inf_idx, -date, convert = TRUE, factor_key = TRUE)
  div <- max(1, floor(N_h/7)); labs_idx <- unique(pmin(c(1, div*(0:6)+1, N_h), N_h))
  plot_labels_macro <- names(FRM_history)[labs_idx]

  png(file.path(output_path, 'Macro', 'macro_inf.png'), width = 900, height = 600, bg = 'transparent')
  print(
    ggplot(macro_inf_long, aes(date, as.numeric(inf_idx), group = macro)) +
      geom_line(aes(color = macro), linewidth = 1) + ylab('normalised # of non-zero betas') +
      scale_x_discrete(breaks = plot_labels_macro, expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 1)) + theme_transparent_bottom
  ); dev.off()

  macro_inf_smooth <- macro_inf
  dates_num <- suppressWarnings(as.numeric(as.Date(macro_inf[, 'date'])))
  if (all(is.na(dates_num))) dates_num <- seq_len(nrow(macro_inf))
  if (nrow(macro_inf) > 3) {
    for (k in 1:M_macro) {
      y <- suppressWarnings(as.numeric(macro_inf[, k + 1])); ok <- is.finite(y) & is.finite(dates_num)
      if (sum(ok) >= 4 && sd(y[ok], na.rm = TRUE) > 0) {
        fit <- smooth.spline(x = dates_num[ok], y = y[ok]); ss <- predict(fit, x = dates_num)$y
      } else ss <- y
      ss[!is.finite(ss)] <- NA_real_; macro_inf_smooth[, k + 1] <- pmax(ss, 0)
    }
  }
  macro_inf_long_smooth <- tidyr::gather(as.data.frame(macro_inf_smooth), macro, inf_idx, -date, convert = TRUE, factor_key = TRUE)
  png(file.path(output_path, 'Macro', 'macro_inf_smooth.png'), width = 900, height = 600, bg = 'transparent')
  print(
    ggplot(macro_inf_long_smooth, aes(date, as.numeric(inf_idx), group = macro)) +
      geom_line(aes(color = macro), linewidth = 1) + ylab('normalised # of non-zero betas') +
      scale_x_discrete(breaks = plot_labels_macro, expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 1)) + theme_transparent_bottom
  ); dev.off()
}
