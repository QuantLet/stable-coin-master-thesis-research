# ==== FRM_SC_compare_crypto.R ==========================================
# Load FRM@Crypto (daily)
crypto_path <- file.path(wdir, 'FRM_Crypto_index.csv')
FRM_crypto_raw <- read.csv(crypto_path, stringsAsFactors = FALSE)
names(FRM_crypto_raw) <- tolower(names(FRM_crypto_raw))
stopifnot(all(c('date','frm') %in% names(FRM_crypto_raw)))
parse_date_robust <- function(x) {
  d <- suppressWarnings(as.Date(x)); if (all(is.na(d))) d <- suppressWarnings(as.Date(x, '%m/%d/%Y'))
  if (all(is.na(d))) { dt <- suppressWarnings(as.POSIXct(x, tz = 'UTC')); if (!all(is.na(dt))) d <- as.Date(dt, tz = 'UTC') }
  d
}
FRM_crypto_raw$date <- parse_date_robust(FRM_crypto_raw$date)
FRM_crypto_daily <- FRM_crypto_raw |> dplyr::filter(is.finite(frm), !is.na(date)) |>
  dplyr::arrange(date) |> dplyr::group_by(date) |> dplyr::summarise(frm = dplyr::last(frm), .groups = 'drop')

FRM_stable_daily <- FRM_index |> dplyr::arrange(date)
aligned <- dplyr::inner_join(
  FRM_stable_daily |> dplyr::select(date, frm_stable = frm),
  FRM_crypto_daily |> dplyr::select(date,  frm_crypto = frm),
  by = 'date'
) |> dplyr::filter(is.finite(frm_stable), is.finite(frm_crypto))

norm01 <- function(x) { r <- range(x, na.rm = TRUE); if (r[1] == r[2]) return(rep(0, length(x))); (x - r[1])/(r[2]-r[1]) }
aligned$stable_norm <- norm01(aligned$frm_stable)
aligned$crypto_norm <- norm01(aligned$frm_crypto)

# Single-axis, each normalized 0–1
out_png_sep_norm <- file.path(website_path, date_end, 'FRM_Stable_vs_Crypto_singleY_separately_normalized.png')
png(out_png_sep_norm, width = 1400, height = 800, bg = 'transparent')
print(
  ggplot(aligned, aes(x = date)) +
    geom_line(aes(y = stable_norm, color = 'Stable Coins'), linewidth = 1) +
    geom_line(aes(y = crypto_norm,  color = 'Crypto'),       linewidth = 1) +
    scale_color_manual(values = c('Stable Coins' = 'blue', 'Crypto' = 'red')) +
    scale_x_date(date_breaks = '3 months', date_labels = '%b %Y') +
    scale_y_continuous(limits = c(0, 1), name = 'Normalized FRM (0–1, per series)') +
    labs(title = 'FRM@Stable Coins vs FRM@Crypto (single axis, each normalized 0–1)', x = 'Date', y = NULL, color = NULL) +
    theme_transparent_bottom + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
); dev.off()

# Dual-axis (both axes show 0–1 normalized series)
out_png_dual_norm <- file.path(website_path, date_end, 'FRM_Stable_vs_Crypto_dualY_normalized.png')
png(out_png_dual_norm, width = 1400, height = 800, bg = 'transparent')
print(
  ggplot(aligned, aes(x = date)) +
    geom_line(aes(y = stable_norm, color = 'Stable Coins'), linewidth = 1) +
    geom_line(aes(y = crypto_norm, color = 'Crypto'), linewidth = 1) +
    scale_color_manual(values = c('Stable Coins' = 'blue', 'Crypto' = 'red')) +
    scale_x_date(date_breaks = '3 months', date_labels = '%b %Y') +
    scale_y_continuous(
      name = 'FRM@Stable Coins (normalized 0–1)',
      sec.axis = sec_axis(~ ., name = 'FRM@Crypto (normalized 0–1)')
    ) + labs(title = 'FRM@Stable Coins vs FRM@Crypto (dual-axis, normalized separately)', x = 'Date', y = NULL) +
    theme_transparent_bottom + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = 'bottom')
); dev.off()
