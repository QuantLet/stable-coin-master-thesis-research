# ==== FRM_SC_index_and_charts.R ========================================
iso_dates <- as.Date(names(FRM_history))
N_h <- length(FRM_history)

# FRM index (mean lambda)
FRM_index <- data.frame(
  date = iso_dates,
  frm  = vapply(FRM_history, function(m) {
    v <- suppressWarnings(as.numeric(m[1, ])); v <- v[is.finite(v) & !is.na(v)]
    if (length(v) == 0) NA_real_ else mean(v)
  }, numeric(1))
)
FRM_index$frm <- round(FRM_index$frm, 6)
write.csv(FRM_index, file.path(output_path, 'Lambda', paste0('FRM_', channel, '_index.csv')), row.names = FALSE, quote = FALSE)

# Risk PNG
good_frm <- FRM_index$frm[is.finite(FRM_index$frm)]
risk_ecdf <- if (length(good_frm)) ecdf(good_frm) else function(x) NA_real_
FRM_plot <- FRM_index
FRM_plot$risk_level <- round(100 * risk_ecdf(FRM_plot$frm), 2)
FRM_plot$`Risk level` <- factor(
  ifelse(FRM_plot$risk_level < 20, '1. Low risk',
         ifelse(FRM_plot$risk_level < 40, '2. General risk',
                ifelse(FRM_plot$risk_level < 60, '3. Elevated risk',
                       ifelse(FRM_plot$risk_level < 80, '4. High risk', '5. Severe risk')))),
  levels = names(risk_colors)
)
png(file.path(website_path, date_end, paste0('FRMColor_', channel,'.png')), width = 900, height = 600, bg = 'transparent')
print(
  ggplot(FRM_plot, aes(x = date, y = frm)) +
    labs(x = 'Date', y = paste0('FRM@', channel)) +
    scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
    geom_point(aes(color = `Risk level`), size = 1, na.rm = TRUE) +
    scale_color_manual(values = risk_colors) +
    theme_transparent_bottom
); dev.off()

# Clean blue line “FRM@Stable Coins”
out_png <- file.path(website_path, date_end, 'FRM_Stable_Index.png')
png(out_png, width = 1200, height = 700, bg = 'transparent')
print(
  ggplot(FRM_index, aes(x = date, y = frm)) +
    labs(title = 'FRM@Stable Coins', x = 'Date', y = 'FRM') +
    geom_line(linewidth = 0.9, color = 'blue') +
    scale_x_date(date_breaks = '3 months', date_labels = '%b %Y') +
    theme_transparent_bottom +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
); dev.off()
