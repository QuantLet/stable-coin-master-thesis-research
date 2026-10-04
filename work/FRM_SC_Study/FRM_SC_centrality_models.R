
source("FRM_SC_utils.R")
suppressPackageStartupMessages({
  library(igraph); library(dplyr); library(ggplot2); library(readr); library(magick)
})

# --- Load FRM index (created by FRM_SC_history_outputs.R) ---
load(file.path(output_path, "stage_50_index.RData"))  # provides FRM_index (date, frm)

# --- Collect adjacency matrices ---
adj_dir   <- file.path(output_path, "Adj_Matrices")
adj_files <- list.files(
  adj_dir,
  full.names = TRUE,
  pattern = "^adj_matrix_[0-9]{8}\.csv$"
)

if (!length(adj_files)) {
  message("No adjacency matrices found in: ", adj_dir)
} else {
  message("Found ", length(adj_files), " adjacency matrices.")
  print(basename(head(adj_files, 5)))  # show first few for debug
}

# Helper: safe numeric matrix
as_numeric_matrix <- function(df) {
  m <- as.matrix(df)
  mode(m) <- "numeric"
  m[!is.finite(m)] <- 0
  m
}

centrality_list <- list()

for (f in adj_files) {
  # Extract YYYYMMDD from filename
  date_str <- gsub("adj_matrix_|\.csv", "", basename(f))
  d <- suppressWarnings(as.Date(date_str, "%Y%m%d"))
  if (!is.finite(d)) next

  mat_df <- read.csv(f, row.names = 1, check.names = FALSE)
  mat    <- as_numeric_matrix(mat_df)

  # Build directed, weighted graph
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "directed", weighted = TRUE, diag = FALSE)

  # Centralities
  deg_all   <- mean(igraph::degree(g, mode = "all", normalized = TRUE))
  btw       <- mean(igraph::betweenness(g, normalized = TRUE, weights = abs(E(g)$weight)))
  eig_vec   <- tryCatch(igraph::eigen_centrality(g, directed = TRUE, weights = abs(E(g)$weight))$vector,
                        error = function(e) rep(NA_real_, vcount(g)))
  eig_mean  <- mean(eig_vec, na.rm = TRUE)
  out_str   <- mean(igraph::strength(g, mode = "out", weights = abs(E(g)$weight)))
  in_str    <- mean(igraph::strength(g, mode = "in",  weights = abs(E(g)$weight)))

  centrality_list[[length(centrality_list) + 1L]] <- data.frame(
    date = d,
    degree_all    = deg_all,
    betweenness   = btw,
    eigen         = eig_mean,
    out_strength  = out_str,
    in_strength   = in_str
  )
}

if (length(centrality_list)) {
  centrality_df <- bind_rows(centrality_list) %>% arrange(date)
} else {
  centrality_df <- data.frame(date = as.Date(character()), degree_all = numeric(),
                              betweenness = numeric(), eigen = numeric(),
                              out_strength = numeric(), in_strength = numeric())
}

# Save centrality time series
dir.create(file.path(output_path, "Network"), showWarnings = FALSE, recursive = TRUE)
write.csv(centrality_df, file.path(output_path, "Network", "Centrality_TimeSeries.csv"), row.names = FALSE)

# --- Merge with FRM and HHI ---
hhi_file <- file.path(output_path, "Lambda", "HHI_mktcap_all.csv")
if (!file.exists(hhi_file)) {
  stop("Missing HHI_mktcap_all.csv at: ", hhi_file, call. = FALSE)
}
HHI <- read.csv(hhi_file, stringsAsFactors = FALSE)
HHI$date <- as.Date(HHI$date)

aligned <- FRM_index %>%
  dplyr::arrange(date) %>%
  dplyr::inner_join(HHI %>% dplyr::arrange(date), by = "date") %>%
  dplyr::inner_join(centrality_df %>% dplyr::arrange(date), by = "date")

# --- Regression: FRM ~ HHI + centralities (only if enough data) ---
reg_out_path <- file.path(output_path, "Lambda", "FRM_Centrality_Regression.csv")
dir.create(file.path(output_path, "Lambda"), showWarnings = FALSE, recursive = TRUE)

if (nrow(aligned) >= 10) {
  use <- stats::complete.cases(aligned[, c("frm", "HHI_mktcap", "eigen", "degree_all", "out_strength", "in_strength")])
  dfm <- aligned[use, , drop = FALSE]

  if (nrow(dfm) >= 10) {
    fit <- lm(frm ~ HHI_mktcap + eigen + degree_all + out_strength + in_strength, data = dfm)
    s   <- summary(fit)
    co  <- coef(s)

    reg_tbl <- data.frame(
      Term      = rownames(co),
      Estimate  = co[, "Estimate"],
      StdError  = co[, "Std. Error"],
      tValue    = co[, "t value"],
      pValue    = co[, "Pr(>|t|)"],
      R2        = s$r.squared,
      AdjR2     = s$adj.r.squared,
      N         = nrow(dfm),
      stringsAsFactors = FALSE
    )
    write.csv(reg_tbl, reg_out_path, row.names = FALSE)
  } else {
    write.csv(data.frame(Note = "Not enough complete cases for regression."), reg_out_path, row.names = FALSE)
  }
} else {
  write.csv(data.frame(Note = "Not enough rows to run regression."), reg_out_path, row.names = FALSE)
}

# --- Plot: FRM vs HHI (z-score) ---
png(file.path(website_path, date_end, "FRM_vs_HHI_zscore.png"), width = 1200, height = 700, bg = "transparent")
print(
  ggplot(aligned, aes(x = date)) +
    geom_line(aes(y = scale(frm),        color = "FRM"), linewidth = 1.1) +
    geom_line(aes(y = scale(HHI_mktcap), color = "HHI"), linewidth = 1.1) +
    scale_color_manual(values = c("FRM" = "blue", "HHI" = "red")) +
    labs(title = NULL, x = NULL, y = "Standardized (z-score)", color = NULL) +
    theme_transparent_bottom
)
dev.off()

# --- Plot: FRM vs Eigen centrality (z-score) ---
png(file.path(website_path, date_end, "FRM_vs_Eigen_zscore.png"), width = 1200, height = 700, bg = "transparent")
print(
  ggplot(aligned, aes(x = date)) +
    geom_line(aes(y = scale(frm),   color = "FRM"), linewidth = 1.1) +
    geom_line(aes(y = scale(eigen), color = "Eigen centrality"), linewidth = 1.1) +
    scale_color_manual(values = c("FRM" = "blue", "Eigen centrality" = "darkorange")) +
    labs(title = NULL, x = NULL, y = "Standardized (z-score)", color = NULL) +
    theme_transparent_bottom
)
dev.off()

