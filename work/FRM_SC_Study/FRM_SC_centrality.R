
# ================================================================
# Centrality indicators (CSV-only) for FIXED universe snapshots
# - Input : Output/<channel>/Adj_Matrices/Fixed/adj_matrix_YYYYMMDD.csv
# - FRM   : Output/<channel>/Lambda/FRM_<channel>_index.csv
# - Output: Output/<channel>/Network/Fixed/*.csv
#           Website/<channel>/<date_end>/Fixed/*.png (normalized overlays)
# ================================================================

suppressPackageStartupMessages({
  library(igraph); library(dplyr); library(ggplot2); library(readr); library(scales)
})

source("FRM_SC_config.R")
source("FRM_SC_utils.R")

# ---- Paths ----
adj_dir <- file.path(output_path, "Adj_Matrices", "Fixed")
net_dir <- file.path(output_path, "Network", "Fixed")
dir.create(net_dir, recursive = TRUE, showWarnings = FALSE)

web_dir <- file.path(website_path, date_end, "Fixed")
dir.create(web_dir, recursive = TRUE, showWarnings = FALSE)

# ---- FRM index (CSV) ----
frm_csv <- file.path(output_path, "Lambda", paste0("FRM_", channel, "_index.csv"))
if (!file.exists(frm_csv)) stop("Missing FRM index CSV at: ", frm_csv)
FRM_index <- read.csv(frm_csv, stringsAsFactors = FALSE)
FRM_index$date <- suppressWarnings(as.Date(FRM_index$date))
FRM_index <- FRM_index[is.finite(FRM_index$frm) & !is.na(FRM_index$date), ]

# ---- Find Fixed adjacency CSVs ----
if (!dir.exists(adj_dir)) stop("Missing folder: ", normalizePath(adj_dir))
files <- list.files(adj_dir, pattern = "^[Aa]dj_?matrix_[0-9]{8}\\.csv$", full.names = TRUE)
if (!length(files)) {
  # relaxed fallback (any csv with "adj" and "matrix" in name)
  files <- list.files(adj_dir, pattern = "\\.csv$", full.names = TRUE)
  files <- files[grepl("adj.*matr?ix", basename(files), ignore.case = TRUE)]
}
if (!length(files)) stop("No Fixed adjacency CSVs found in: ", normalizePath(adj_dir))

# ---- Helpers ----
date_from_name <- function(fp){
  b <- basename(fp)
  m <- regexpr("[0-9]{8}", b, perl = TRUE)
  if (m[1] == -1) return(as.Date(NA))
  ds <- substr(b, m[1], m[1] + attr(m, "match.length")[1] - 1)
  suppressWarnings(as.Date(ds, "%Y%m%d"))
}
numify_matrix <- function(df){
  for (j in seq_along(df)) {
    if (!is.numeric(df[[j]])) {
      v <- df[[j]]
      if (is.factor(v)) v <- as.character(v)
      v <- gsub("\\s+", "", v)
      df[[j]] <- suppressWarnings(as.numeric(v))
    }
  }
  as.matrix(df)
}
read_adj_csv <- function(fp){
  df <- tryCatch(read.csv(fp, row.names = 1, check.names = FALSE), error = function(e) NULL)
  if (!is.null(df)) {
    M <- numify_matrix(df); mode(M) <- "numeric"; M[!is.finite(M)] <- 0
    if (any(M != 0)) return(M)
  }
  df <- tryCatch(read.csv2(fp, row.names = 1, check.names = FALSE), error = function(e) NULL)
  if (!is.null(df)) {
    M <- numify_matrix(df); mode(M) <- "numeric"; M[!is.finite(M)] <- 0
    if (any(M != 0)) return(M)
  }
  stop("Cannot parse numeric adjacency: ", basename(fp))
}
inv_distance <- function(g) 1 / pmax(abs(E(g)$weight), 1e-8)
abs_weight  <- function(g) abs(E(g)$weight)
safe_mean   <- function(x) if (length(x)) mean(x, na.rm = TRUE) else NA_real_
norm01 <- function(x){
  r <- range(x, na.rm = TRUE)
  if (!is.finite(r[1]) || !is.finite(r[2]) || r[1] == r[2]) return(rep(0, length(x)))
  (x - r[1])/(r[2] - r[1])
}

# ---- Order files by date ----
map <- data.frame(file = files, stringsAsFactors = FALSE)
map$date <- vapply(map$file, date_from_name, as.Date(NA))
map <- map[!is.na(map$date), ]
map <- map[order(map$date), ]
if (!nrow(map)) stop("No parsable dates in Fixed adjacency filenames.")

# ---- Compute centralities per snapshot ----
by_node <- list()
avg_rows <- vector("list", nrow(map))

for (i in seq_len(nrow(map))) {
  A <- tryCatch(read_adj_csv(map$file[i]), error = function(e) NULL)
  if (is.null(A)) next
  mode(A) <- "numeric"
  A[!is.finite(A)] <- 0
  diag(A) <- 0
  if (ncol(A) < 2) next

  g <- igraph::graph_from_adjacency_matrix(A, mode = "directed", weighted = TRUE, diag = FALSE)

  if (ecount(g) == 0) {
    n <- vcount(g)
    outdeg <- indeg <- closeo <- betw <- eigv <- ostr <- istr <- rep(0, n)
  } else {
    w_d   <- inv_distance(g)              # distances ~ 1/|weight|
    w_abs <- abs_weight(g)                # strengths/eigen on |weight|

    outdeg <- tryCatch(degree(g, mode = "out", normalized = TRUE), error = function(e) rep(0, vcount(g)))
    indeg  <- tryCatch(degree(g, mode = "in",  normalized = TRUE), error = function(e) rep(0, vcount(g)))
    closeo <- tryCatch(closeness(g, mode = "out", normalized = TRUE, weights = w_d),
                       error = function(e) tryCatch(closeness(g, mode = "out", normalized = TRUE),
                                                    error = function(e2) rep(0, vcount(g))))
    betw   <- tryCatch(betweenness(g, directed = TRUE, normalized = TRUE, weights = w_d),
                       error = function(e) tryCatch(betweenness(g, directed = TRUE, normalized = TRUE),
                                                    error = function(e2) rep(0, vcount(g))))
    eigv   <- tryCatch(eigen_centrality(g, directed = TRUE, weights = w_abs)$vector,
                       error = function(e) rep(0, vcount(g)))
    ostr   <- tryCatch(strength(g, mode = "out", weights = w_abs), error = function(e) rep(0, vcount(g)))
    istr   <- tryCatch(strength(g, mode = "in",  weights = w_abs), error = function(e) rep(0, vcount(g)))

    fixv <- function(v){ v[!is.finite(v)] <- 0; as.numeric(v) }
    outdeg <- fixv(outdeg); indeg <- fixv(indeg); closeo <- fixv(closeo)
    betw   <- fixv(betw);   eigv  <- fixv(eigv);  ostr   <- fixv(ostr);  istr <- fixv(istr)
  }

  nodes <- igraph::V(g)$name
  by_node[[length(by_node) + 1L]] <- data.frame(
    date = map$date[i], node = nodes,
    outdegree = outdeg, indegree = indeg,
    closeness = closeo, betweenness = betw,
    eigenvector = eigv, out_strength = ostr, in_strength = istr,
    stringsAsFactors = FALSE
  )

  avg_rows[[i]] <- data.frame(
    date = map$date[i],
    outdegree_avg = safe_mean(outdeg),
    indegree_avg  = safe_mean(indeg),
    closeness_avg = safe_mean(closeo),
    betweenness_avg = safe_mean(betw),
    eigenvector_avg = safe_mean(eigv),
    out_strength_avg = safe_mean(ostr),
    in_strength_avg  = safe_mean(istr)
  )
}

centrality_by_node <- dplyr::bind_rows(by_node)
centrality_avg     <- dplyr::bind_rows(avg_rows) %>% dplyr::arrange(date)

# ---- Save CSV outputs ----
write.csv(centrality_by_node, file.path(net_dir, "Centrality_ByNode_Fixed.csv"), row.names = FALSE)
write.csv(centrality_avg,     file.path(net_dir, "Centrality_Averages_Fixed.csv"), row.names = FALSE)

# ---- Correlation with FRM (time series averages vs FRM) ----
aligned <- dplyr::inner_join(
  FRM_index %>% dplyr::arrange(date),
  centrality_avg %>% dplyr::arrange(date),
  by = "date"
)

vars <- c("frm","outdegree_avg","indegree_avg","closeness_avg","betweenness_avg",
          "eigenvector_avg","out_strength_avg","in_strength_avg")
C <- aligned[, vars, drop = FALSE]
C <- C[stats::complete.cases(C), , drop = FALSE]

corr_mat <- if (nrow(C) > 3) stats::cor(C, use = "pairwise.complete.obs", method = "pearson") else NA
p_mat <- matrix(NA_real_, ncol = ncol(C), nrow = ncol(C),
                dimnames = list(colnames(C), colnames(C)))
if (nrow(C) > 3) {
  for (i in 1:(ncol(C) - 1)) for (j in (i + 1):ncol(C)) {
    ct <- tryCatch(stats::cor.test(C[[i]], C[[j]], method = "pearson"), error = function(e) NULL)
    if (!is.null(ct)) { p_mat[i, j] <- p_mat[j, i] <- ct$p.value }
  }
  diag(p_mat) <- 0
}
write.csv(corr_mat, file.path(net_dir, "Centrality_FRM_Corr_Fixed.csv"),  row.names = TRUE)
write.csv(p_mat,    file.path(net_dir, "Centrality_FRM_CorrP_Fixed.csv"), row.names = TRUE)

# ---- Normalized overlay plots (FRM vs each centrality average) ----
plot_overlay <- function(df, colname, label){
  D <- df[, c("date","frm", colname), drop = FALSE]; names(D)[3] <- "cent"
  D <- D[stats::complete.cases(D), , drop = FALSE]; if (!nrow(D)) return(invisible(NULL))

  # left axis = FRM (raw)
  frm_min <- min(D$frm,  na.rm = TRUE); frm_max <- max(D$frm,  na.rm = TRUE)
  if (!is.finite(frm_min) || !is.finite(frm_max) || frm_max <= frm_min) { frm_min <- 0; frm_max <- 1 }

  # right axis = centrality (its own scale)
  cen_min <- min(D$cent, na.rm = TRUE); cen_max <- max(D$cent, na.rm = TRUE)
  if (!is.finite(cen_min) || !is.finite(cen_max) || cen_max <= cen_min) { cen_min <- 0; cen_max <- 1 }

  to_left   <- function(x) (x - cen_min) * (frm_max - frm_min) / (cen_max - cen_min) + frm_min
  from_left <- function(y) (y - frm_min) * (cen_max - cen_min) / (frm_max - frm_min) + cen_min

  out_png <- file.path(web_dir, paste0("FRM_vs_", gsub("_avg$","", colname), "_Fixed_DualAxis.png"))
  png(out_png, width = 1600, height = 900, bg = "transparent", res = 120)
  print(
    ggplot(D, aes(x = date)) +
      geom_line(aes(y = frm,           color = "FRM@Stable"), linewidth = 1.3, lineend = "round") +
      geom_line(aes(y = to_left(cent), color = label),        linewidth = 1.3, lineend = "round") +
      scale_color_manual(values = c("FRM@Stable" = "#007AFF", label = "#FF3B30"), name = NULL) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      scale_y_continuous(
        name = "FRM@Stable (raw)",
        sec.axis = sec_axis(~ from_left(.), name = label)
      ) +
      theme_transparent_bottom +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  )
  dev.off()
}


plot_overlay(aligned, "outdegree_avg",    "Out-degree")
plot_overlay(aligned, "indegree_avg",     "In-degree")
plot_overlay(aligned, "closeness_avg",    "Closeness")
plot_overlay(aligned, "betweenness_avg",  "Betweenness")
plot_overlay(aligned, "eigenvector_avg",  "Eigenvector")
plot_overlay(aligned, "out_strength_avg", "Out-strength")
plot_overlay(aligned, "in_strength_avg",  "In-strength")

message("✅ Centrality CSVs: ", normalizePath(net_dir))
message("✅ Overlay PNGs:    ", normalizePath(web_dir))

