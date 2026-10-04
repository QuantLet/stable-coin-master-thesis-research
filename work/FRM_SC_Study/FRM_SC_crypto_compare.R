
# ===============================================================
# FRM_SC_crypto_compare.R
# Two separate charts (no normalization):
#   1) FRM@Stable (blue)
#   2) FRM Crypto  (red)
# Saves to: Website/<channel>/<date_end>/
# ===============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# --- config & data ---
source("FRM_SC_config.R")
source("FRM_SC_utils.R")
load(file.path(output_path, "stage_50_index.RData"))  # brings FRM_index (date, frm)

# Path to the all-crypto FRM CSV
crypto_path <- file.path(wdir, "FRM_Crypto_index.csv")
stopifnot(file.exists(crypto_path))

# Robust date parser
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

# Minimal transparent theme (axes only)
theme_transparent_bottom <- theme_classic(base_size = 12) +
  theme(
    panel.background       = element_rect(fill = "transparent", colour = NA),
    plot.background        = element_rect(fill = "transparent", colour = NA),
    legend.background      = element_rect(fill = "transparent", colour = NA),
    legend.box.background  = element_rect(fill = "transparent", colour = NA),
    legend.position        = "none",
    axis.text              = element_text(size = 12, colour = "black"),
    axis.title             = element_text(size = 14, colour = "black"),
    panel.grid             = element_blank()
  )

# --- load & clean FRM Crypto ---
FRM_crypto_raw <- read.csv(crypto_path, stringsAsFactors = FALSE, check.names = FALSE)
names(FRM_crypto_raw) <- tolower(names(FRM_crypto_raw))
stopifnot(all(c("date","frm") %in% names(FRM_crypto_raw)))
FRM_crypto_raw$date <- parse_date_robust(FRM_crypto_raw$date)
FRM_crypto_raw <- FRM_crypto_raw[is.finite(FRM_crypto_raw$frm) & !is.na(FRM_crypto_raw$date), ]

# ensure FRM_index is clean
names(FRM_index) <- tolower(names(FRM_index))
stopifnot(all(c("date","frm") %in% names(FRM_index)))
FRM_index$date <- parse_date_robust(FRM_index$date)
FRM_index <- FRM_index %>% filter(!is.na(date), is.finite(frm))

# collapse to one value per date if needed
FRM_crypto_daily <- FRM_crypto_raw %>%
  arrange(date) %>%
  group_by(date) %>%
  summarise(frm = dplyr::last(frm), .groups = "drop")

FRM_stable_daily <- FRM_index %>%
  arrange(date) %>%
  group_by(date) %>%
  summarise(frm = dplyr::last(frm), .groups = "drop")

# Optional: use the overlapping date range so x-axes match
dmin <- max(min(FRM_stable_daily$date, na.rm = TRUE),
            min(FRM_crypto_daily$date, na.rm = TRUE), na.rm = TRUE)
dmax <- min(max(FRM_stable_daily$date, na.rm = TRUE),
            max(FRM_crypto_daily$date, na.rm = TRUE), na.rm = TRUE)

FRM_stable_plot <- FRM_stable_daily %>% filter(date >= dmin, date <= dmax)
FRM_crypto_plot <- FRM_crypto_daily %>% filter(date >= dmin, date <= dmax)

# --- output folder ---
out_dir <- file.path(website_path, date_end)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Colors (Apple iOS blue + SF red)
ios_blue <- "blue"
sf_red   <- "red"

# --- plot helper (single-series line) ---
save_line <- function(df, ycol, color, outfile){
  df2 <- df[, c("date", ycol)]
  names(df2) <- c("date", "val")
  png(outfile, width = 1400, height = 800, bg = "transparent")
  print(
    ggplot(df2, aes(x = date, y = val)) +
      geom_line(linewidth = 1.3, color = color) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      labs(title = NULL, x = NULL, y = NULL) +
      theme_transparent_bottom +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  )
  dev.off()
}

# --- save the two separate charts (no normalization) ---
save_line(
  FRM_stable_plot, "frm", ios_blue,
  file.path(out_dir, "FRM_Stable_only.png")
)
save_line(
  FRM_crypto_plot, "frm", sf_red,
  file.path(out_dir, "FRM_Crypto_only.png")
)

message("✓ Saved:
  ",
        normalizePath(file.path(out_dir, "FRM_Stable_only.png")), "
  ",
        normalizePath(file.path(out_dir, "FRM_Crypto_only.png")))


