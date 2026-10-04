
theme_transparent_bottom <- theme(
  panel.background      = element_rect(fill="transparent", colour=NA),
  plot.background       = element_rect(fill="transparent", colour=NA),
  legend.box.background = element_rect(fill="transparent"),
  legend.background     = element_rect(fill="transparent"),
  legend.position       = "bottom",
  legend.direction      = "horizontal",
  legend.box.just       = "center",
  axis.line             = element_line(colour="black"),
  axis.text             = element_text(size=12),
  axis.title            = element_text(size=14),
  panel.grid            = element_blank()
)

risk_colors <- c("1. Low risk"="green","2. General risk"="blue","3. Elevated risk"="yellow",
                 "4. High risk"="orange","5. Severe risk"="red")

pick_file_flexible <- function(path, strong_regex, weak_regex, role="file") {
  f <- list.files(path, pattern=strong_regex, full.names=TRUE, ignore.case=TRUE)
  if (!length(f)) f <- list.files(path, pattern=weak_regex, full.names=TRUE, ignore.case=TRUE)
  if (!length(f)) stop(sprintf("No %s found in %s", role, path))
  extract_max_date_from_name <- function(x) {
    b <- basename(x); ds <- gregexpr("[0-9]{8}", b, perl=TRUE); vals <- regmatches(b, ds)[[1]]
    if (length(vals)) as.integer(max(vals)) else NA_integer_ }
  name_dates <- vapply(f, extract_max_date_from_name, integer(1))
  if (any(!is.na(name_dates))) return(f[which.max(name_dates)])
  f[1]
}

COIN_NAME_MAP <- c(
  usd_coin="USDC", binance_usd="BUSD", gemini_dollar="GUSD", stasis_eurs="EURs",
  rupiah_token="IDRT", tether="USDT", nusd="nUSD", pax_gold="PAXG",
  true_usd="TUSD", paxos_standard="USDP", dai="DAI"
)
pretty_coin <- function(x){
  y <- ifelse(x %in% names(COIN_NAME_MAP), COIN_NAME_MAP[x], NA_character_)
  ifelse(is.na(y), stringr::str_to_title(gsub("_"," ",x, fixed=TRUE)), y)
}

# ================================================================
# Dual-axis overlay (FRM raw LEFT; bars on RIGHT)
# Use for FRM line + a spike/bar series (e.g., HHI daily).
# - df must have columns: date, frm, <right_col>
# - Right axis is scaled to its own min/max (or clamp later if needed).
# ================================================================
dualaxis_frm_line_vs_bars <- function(
  df, date_col = "date", frm_col = "frm", right_col, right_label = "Right",
  out_png, frm_colr = "#0A84FF", bar_colr = "#007AFF", bar_alpha = 0.60
){
  stopifnot(all(c(date_col, frm_col, right_col) %in% names(df)))
  D <- df[, c(date_col, frm_col, right_col)]
  names(D) <- c("date","frm","rgt")
  D$date <- suppressWarnings(as.Date(D$date))
  D <- D[is.finite(D$frm) & is.finite(D$rgt) & !is.na(D$date), ]
  if (!nrow(D)) return(invisible(NULL))

  # Left axis: FRM RAW
  fmin <- min(D$frm, na.rm = TRUE); fmax <- max(D$frm, na.rm = TRUE)
  if (!is.finite(fmin) || !is.finite(fmax) || fmax <= fmin) { fmin <- 0; fmax <- 1 }

  # Right axis: bar series own range
  rmin <- min(D$rgt, na.rm = TRUE); rmax <- max(D$rgt, na.rm = TRUE)
  if (!is.finite(rmin) || !is.finite(rmax) || rmax <= rmin) { rmin <- 0; rmax <- 1 }

  # map right→left and inverse (for sec_axis)
  to_left   <- function(x) (x - rmin) * (fmax - fmin) / (rmax - rmin) + fmin
  from_left <- function(y) (y - fmin) * (rmax - rmin) / (fmax - fmin) + rmin

  png(out_png, width = 1600, height = 900, bg = "transparent", res = 120)
  print(
    ggplot(D, aes(x = date)) +
      geom_line(aes(y = frm, color = "FRM@Stable"), linewidth = 1.4, lineend = "round") +
      geom_col(aes(y = to_left(rgt), fill = right_label), width = 1, alpha = bar_alpha) +
      scale_color_manual(values = c("FRM@Stable" = frm_colr), name = NULL) +
      scale_fill_manual(values  = c(right_label = bar_colr),  name = NULL) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      scale_y_continuous(
        name = "FRM@Stable (raw)",
        sec.axis = sec_axis(~ from_left(.),
                            breaks = scales::pretty_breaks(5),
                            labels = function(x) formatC(x, format="f", digits=2),
                            name = right_label)
      ) +
      theme_transparent_bottom +
      theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
  )
  dev.off()
}

