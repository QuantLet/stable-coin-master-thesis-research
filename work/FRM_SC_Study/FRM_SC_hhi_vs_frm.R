
# ================================================================
# HHI (MarketCap) vs FRM@Stable — FRM RAW (no normalization), dual-axis
# Left axis: FRM raw  — "#0A84FF"
# Right axis: HHI in [0,1]
#   Daily       "#FF3B30"
#   Cumulative  "#FF9500"
# Also saves a **bars** overlay for Daily HHI (to match spike style).
# ================================================================

suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })

# --- config / paths ---
if (!exists("output_path") || !exists("website_path") || !exists("date_end") || !exists("channel")) {
  stopifnot(file.exists("FRM_SC_config.R")); source("FRM_SC_config.R")
}
source("FRM_SC_utils.R")  # brings theme + dualaxis_frm_line_vs_bars

dir.create(file.path(output_path, "Lambda"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(website_path, date_end), showWarnings = FALSE, recursive = TRUE)

# --- FRM index (RAW; use column `frm` only) ---
stopifnot(file.exists(file.path(output_path, "stage_50_index.RData")))
load(file.path(output_path, "stage_50_index.RData"))  # brings FRM_index (date, frm)
FRM_index <- as.data.frame(FRM_index)
FRM_index$date <- suppressWarnings(as.Date(FRM_index$date))
if (!("frm" %in% names(FRM_index))) stop("FRM_index must contain column `frm` (raw FRM).")
FRM_index <- FRM_index[is.finite(FRM_index$frm) & !is.na(FRM_index$date), ] |> arrange(date)

# --- HHI files (guarantee [0,1]) ---
daily_file <- file.path(output_path, "Lambda", "HHI_mktcap_all.csv")
cum_file   <- file.path(output_path, "Lambda", "HHI_mktcap_cumulative.csv")

parse_date_robust <- function(x){
  d <- suppressWarnings(as.Date(x))
  if (all(is.na(d))) d <- suppressWarnings(as.Date(x, "%Y-%m-%d"))
  if (all(is.na(d))) d <- suppressWarnings(as.Date(x, "%m/%d/%Y"))
  if (all(is.na(d))) {
    dt <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
    if (!all(is.na(dt))) d <- as.Date(dt, tz = "UTC")
  }
  d
}
clamp01 <- function(v) pmin(pmax(v, 0), 1)

load_daily <- function(){
  if (!file.exists(daily_file)) stop("Daily HHI not found: ", daily_file)
  df <- read.csv(daily_file, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- tolower(names(df))
  if (!all(c("date","hhi_mktcap") %in% names(df))) stop("Daily HHI needs: date, hhi_mktcap")
  df$date <- parse_date_robust(df$date)
  df <- arrange(df, date)
  names(df)[names(df) == "hhi_mktcap"] <- "hhi_daily"
  df$hhi_daily <- clamp01(df$hhi_daily)
  df
}
load_cum <- function(){
  if (!file.exists(cum_file)) stop("Cumulative HHI not found: ", cum_file)
  df <- read.csv(cum_file, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- tolower(names(df))
  if (!all(c("date","hhi_mktcap_cum") %in% names(df))) stop("Cumulative HHI needs: date, hhi_mktcap_cum")
  df$date <- parse_date_robust(df$date)
  df <- arrange(df, date)
  df$hhi_mktcap_cum <- clamp01(df$hhi_mktcap_cum)
  df
}

# --- transparent theme (lines only) ---
if (!exists("theme_transparent_bottom")) {
  theme_transparent_bottom <- theme(
    panel.background      = element_rect(fill = "transparent", colour = NA),
    plot.background       = element_rect(fill = "transparent", colour = NA),
    legend.background     = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.position       = "bottom",
    legend.direction      = "horizontal",
    legend.box.just       = "center",
    axis.line             = element_line(colour = "black"),
    axis.text             = element_text(size = 12, colour = "black"),
    axis.title            = element_text(size = 14, colour = "black"),
    panel.grid            = element_blank()
  )
}

# --- generic dual-axis line+line (FRM raw left; HHI right [0,1]) ---
plot_dual_raw <- function(D, hhi_col, right_label, out_png,
                          frm_col = "#0A84FF",
                          hhi_colr = "#FF3B30") {

  stopifnot(all(c("date","frm", hhi_col) %in% names(D)))
  D <- D[, c("date","frm", hhi_col), drop = FALSE]
  names(D)[3] <- "hhi"
  D <- D[is.finite(D$frm) & is.finite(D$hhi) & !is.na(D$date), , drop = FALSE]
  if (!nrow(D)) return(invisible(NULL))

  # Left axis = FRM RAW
  fmin <- min(D$frm, na.rm = TRUE); fmax <- max(D$frm, na.rm = TRUE)
  if (!is.finite(fmin) || !is.finite(fmax) || fmax == fmin) { fmin <- 0; fmax <- 1 }

  # Right axis fixed to [0,1]
  hmin <- 0; hmax <- 1
  to_left   <- function(x) (x - hmin) * (fmax - fmin) / (hmax - hmin) + fmin
  from_left <- function(y) (y - fmin) * (hmax - hmin) / (fmax - fmin) + hmin

  png(out_png, width = 1600, height = 900, res = 120, bg = "transparent"); print(
    ggplot(D, aes(x = date)) +
      geom_line(aes(y = frm,          color = "FRM@Stable"), linewidth = 1.3, lineend = "round") +
      geom_line(aes(y = to_left(hhi), color = right_label),  linewidth = 1.3, lineend = "round") +
      scale_color_manual(values = c("FRM@Stable" = frm_col, right_label = hhi_colr), name = NULL) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      scale_y_continuous(
        limits = c(fmin, fmax),
        name   = "FRM@Stable (raw)",
        sec.axis = sec_axis(~ from_left(.),
                            breaks = seq(0, 1, 0.25),
                            labels = sprintf("%.2f", seq(0, 1, 0.25)),
                            name = "HHI (0–1)")
      ) +
      labs(title = NULL, x = NULL, y = NULL) +
      theme_transparent_bottom +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  ); dev.off()
}

# --- load HHI, align with FRM ---
HHI_d <- load_daily()
HHI_c <- load_cum()

frm_df <- FRM_index[, c("date","frm")] |> arrange(date)

aligned_d <- inner_join(frm_df, HHI_d[, c("date","hhi_daily")] |> arrange(date), by = "date")
aligned_c <- inner_join(frm_df, HHI_c[, c("date","hhi_mktcap_cum")] |> arrange(date), by = "date")

write.csv(aligned_d, file.path(output_path, "Lambda", "HHI_vs_FRM_Stable_DAILY_aligned_raw.csv"), row.names = FALSE)
write.csv(aligned_c, file.path(output_path, "Lambda", "HHI_vs_FRM_Stable_CUMULATIVE_aligned_raw.csv"), row.names = FALSE)

# --- plot: Daily (FRM raw vs HHI daily) — LINE+LINE ---
plot_dual_raw(
  aligned_d, hhi_col = "hhi_daily",
  right_label = "Market-Cap HHI (Daily)",
  out_png = file.path(website_path, date_end, "HHI_vs_FRM_Stable_DAILY_Raw_DualAxis.png"),
  frm_col = "#0A84FF", hhi_colr = "#FF3B30"
)

# --- plot: Cumulative (FRM raw vs HHI cum) — LINE+LINE ---
plot_dual_raw(
  aligned_c, hhi_col = "hhi_mktcap_cum",
  right_label = "Market-Cap HHI (Cumulative)",
  out_png = file.path(website_path, date_end, "HHI_vs_FRM_Stable_CUMULATIVE_Raw_DualAxis.png"),
  frm_col = "#0A84FF", hhi_colr = "#FF9500"
)

# --- plot: Combined (both HHI lines, FRM raw left) ---
combo <- inner_join(aligned_d, aligned_c, by = c("date","frm"))
if (nrow(combo)) {
  fmin <- min(combo$frm, na.rm = TRUE); fmax <- max(combo$frm, na.rm = TRUE)
  if (!is.finite(fmin) || !is.finite(fmax) || fmax == fmin) { fmin <- 0; fmax <- 1 }
  hmin <- 0; hmax <- 1
  to_left   <- function(x) (x - hmin) * (fmax - fmin) / (hmax - hmin) + fmin
  from_left <- function(y) (y - fmin) * (hmax - hmin) / (fmax - fmin) + hmin

  png(file.path(website_path, date_end, "HHI_vs_FRM_Stable_BOTH_Raw_DualAxis.png"),
      width = 1600, height = 900, res = 120, bg = "transparent"); print(
    ggplot(combo, aes(x = date)) +
      geom_line(aes(y = frm, color = "FRM@Stable"), linewidth = 1.3, lineend = "round") +
      geom_line(aes(y = to_left(hhi_daily),      color = "HHI (Daily)"),      linewidth = 1.3, lineend = "round") +
      geom_line(aes(y = to_left(hhi_mktcap_cum), color = "HHI (Cumulative)"), linewidth = 1.3, lineend = "round") +
      scale_color_manual(values = c("FRM@Stable" = "#0A84FF",
                                    "HHI (Daily)" = "#FF3B30",
                                    "HHI (Cumulative)" = "#FF9500"),
                         name = NULL) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      scale_y_continuous(
        limits = c(fmin, fmax),
        name   = "FRM@Stable (raw)",
        sec.axis = sec_axis(~ from_left(.),
                            breaks = seq(0, 1, 0.25),
                            labels = sprintf("%.2f", seq(0, 1, 0.25)),
                            name = "HHI (0–1)")
      ) +
      labs(title = NULL, x = NULL, y = NULL) +
      theme_transparent_bottom +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  ); dev.off()
}

# --- NEW: Daily HHI as **bars** over FRM line (true dual-axis, no normalization glitches) ---
dualaxis_frm_line_vs_bars(
  df = aligned_d, right_col = "hhi_daily", right_label = "HHI (Daily)",
  out_png = file.path(website_path, date_end, "HHI_vs_FRM_Stable_DAILY_Raw_DualAxis_Bars.png"),
  frm_colr = "#0A84FF", bar_colr = "#007AFF", bar_alpha = 0.65
)

