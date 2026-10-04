
############################################################
# Centrality on FIXED adjacencies + Dual-axis overlays
# - Input 1: Output/<channel>/Adj_Matrices/Fixed/adj_matrix_YYYYMMDD.csv
# - Input 2: Output/<channel>/Lambda/FRM_<channel>_index.csv
# - Input 3: Output/<channel>/Lambda/Fixed/lambdas_fixed_*.csv (coin list)
# - Output CSVs: Output/<channel>/Network/Fixed/
# - Output PNGs: Website/<channel>/<date_end>/Fixed/*_DualAxis.png (transparent)
############################################################

suppressPackageStartupMessages({
  library(igraph); library(dplyr); library(ggplot2)
})

# ---------- 0) Config ----------
stopifnot(file.exists("FRM_SC_config.R"))
source("FRM_SC_config.R")  # brings: output_path, website_path, channel, date_end, etc.

fixed_dir <- file.path(output_path, "Adj_Matrices", "Fixed")
net_dir   <- file.path(output_path, "Network", "Fixed")
web_dir   <- file.path(website_path, date_end, "Fixed")  # website_path already includes channel
dir.create(net_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(web_dir, recursive = TRUE, showWarnings = FALSE)

# ---------- 1) FRM index ----------
frm_csv <- file.path(output_path, "Lambda", paste0("FRM_", channel, "_index.csv"))
if (!file.exists(frm_csv)) stop("Missing FRM index CSV at: ", frm_csv)
FRM_index <- read.csv(frm_csv, stringsAsFactors = FALSE)
FRM_index$date <- suppressWarnings(as.Date(FRM_index$date))
FRM_index <- FRM_index[is.finite(FRM_index$frm) & !is.na(FRM_index$date), ]

# ---------- 2) Fixed coin universe (from latest lambdas_fixed_*.csv) ----------
lf_dir  <- file.path(output_path, "Lambda", "Fixed")
lf_all  <- list.files(lf_dir, pattern = "^lambdas_fixed_.*\\.csv$", full.names = TRUE)
if (!length(lf_all)) stop("No lambdas_fixed_*.csv found in: ", normalizePath(lf_dir))
lf_path <- lf_all[which.max(file.info(lf_all)$mtime)]  # latest by modified time
lf_hdr  <- read.csv(lf_path, nrows = 1, check.names = FALSE)
coin_names_fixed <- setdiff(names(lf_hdr), "date")
if (!length(coin_names_fixed)) stop("Could not infer coin columns from: ", lf_path)

# ---------- 3) List & order Fixed adjacency CSVs ----------
if (!dir.exists(fixed_dir)) stop("Missing folder: ", normalizePath(fixed_dir))
all_csv <- list.files(fixed_dir, pattern = "\\.csv$", full.names = TRUE)
if (!length(all_csv)) stop("No Fixed adjacency CSVs in: ", normalizePath(fixed_dir))

date_from_name <- function(fp){
  b <- basename(fp)
  m <- regexpr("[0-9]{8}", b, perl = TRUE)
  if (m[1] == -1) return(as.Date(NA))
  ds <- substr(b, m[1], m[1] + attr(m, "match.length")[1] - 1)
  suppressWarnings(as.Date(ds, "%Y%m%d"))
}
map <- data.frame(file = all_csv, stringsAsFactors = FALSE)
map$date <- vapply(map$file, date_from_name, as.Date(NA))
map <- map[!is.na(map$date), ]
map <- map[order(map$date), ]
stopifnot(nrow(map) > 0)

# ---------- 4) Readers & helpers ----------
numify_df <- function(df){
  for (j in seq_along(df)) {
    if (!is.numeric(df[[j]])) {
      v <- df[[j]]; if (is.factor(v)) v <- as.character(v)
      v <- gsub("\\s+", "", v)
      v <- gsub(",", ".", v, fixed = TRUE)
      df[[j]] <- suppressWarnings(as.numeric(v))
    }
  }
  as.matrix(df)
}

read_adj_csv <- function(fp){
  df <- tryCatch(read.csv(fp, row.names = 1, check.names = FALSE), error = function(e) NULL)
  if (is.null(df)) stop("Cannot read adjacency CSV: ", fp)
  M <- numify_df(df)
  mode(M) <- "numeric"; M[!is.finite(M)] <- 0; diag(M) <- 0
  rn <- rownames(M); cn <- colnames(M)
  take <- intersect(intersect(coin_names_fixed, rn), cn)  # keep only fixed coins; drop macros
  if (!length(take)) stop("Adjacency file has no overlap with fixed coin set: ", basename(fp))
  M[take, take, drop = FALSE]
}

inv_distance <- function(g) 1 / pmax(abs(E(g)$weight), 1e-8)  # for closeness/betweenness
abs_weight  <- function(g) abs(E(g)$weight)                    # for strength/eigen
fixv        <- function(v){ v[!is.finite(v)] <- 0; as.numeric(v) }
safe_mean   <- function(x) if (length(x)) mean(x, na.rm = TRUE) else NA_real_

# ---------- 5) Compute node-level & average centralities ----------
by_node <- list()
avg_rows <- vector("list", nrow(map))

message("Computing centralities for ", nrow(map), " Fixed snapshots...")
for (i in seq_len(nrow(map))) {
  A <- read_adj_csv(map$file[i])
  if (ncol(A) < 2) next
  g <- igraph::graph_from_adjacency_matrix(A, mode = "directed", weighted = TRUE, diag = FALSE)

  if (ecount(g) == 0) {
    n <- vcount(g)
    outdeg <- indeg <- closeo <- betw <- eigv <- ostr <- istr <- rep(0, n)
  } else {
    w_d   <- inv_distance(g)
    w_abs <- abs_weight(g)
    outdeg <- tryCatch(degree(g, mode = "out", normalized = TRUE),      error = function(e) rep(0, vcount(g)))
    indeg  <- tryCatch(degree(g, mode = "in",  normalized = TRUE),      error = function(e) rep(0, vcount(g)))
    closeo <- tryCatch(closeness(g, mode = "out", normalized = TRUE, weights = w_d),
                       error = function(e) tryCatch(closeness(g, mode = "out", normalized = TRUE), error = function(e2) rep(0, vcount(g))))
    betw   <- tryCatch(betweenness(g, directed = TRUE, normalized = TRUE, weights = w_d),
                       error = function(e) tryCatch(betweenness(g, directed = TRUE, normalized = TRUE), error = function(e2) rep(0, vcount(g))))
    eigv   <- tryCatch(eigen_centrality(g, directed = TRUE, weights = w_abs)$vector,
                       error = function(e) rep(0, vcount(g)))
    ostr   <- tryCatch(strength(g, mode = "out", weights = w_abs),      error = function(e) rep(0, vcount(g)))
    istr   <- tryCatch(strength(g, mode = "in",  weights = w_abs),      error = function(e) rep(0, vcount(g)))
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
    outdegree_avg   = safe_mean(outdeg),
    indegree_avg    = safe_mean(indeg),
    closeness_avg   = safe_mean(closeo),
    betweenness_avg = safe_mean(betw),
    eigenvector_avg = safe_mean(eigv),
    out_strength_avg= safe_mean(ostr),
    in_strength_avg = safe_mean(istr)
  )
}

# --- assemble data frames BEFORE further use ---
centrality_by_node <- dplyr::bind_rows(by_node)
centrality_avg     <- dplyr::bind_rows(avg_rows) %>% dplyr::arrange(date)

# --- force proper Date before joining ---
to_Date <- function(x){
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(round(x), origin = "1970-01-01"))
  x1 <- suppressWarnings(as.Date(x))
  if (all(is.na(x1))) x1 <- suppressWarnings(as.Date(x, "%Y-%m-%d"))
  if (all(is.na(x1))) x1 <- suppressWarnings(as.Date(x, "%m/%d/%Y"))
  x1
}
FRM_index$date              <- to_Date(FRM_index$date)
centrality_avg$date         <- to_Date(centrality_avg$date)
centrality_by_node$date     <- to_Date(centrality_by_node$date)

# --- save CSVs ---
write.csv(centrality_by_node, file.path(net_dir, "Centrality_ByNode_Fixed.csv"), row.names = FALSE)
write.csv(centrality_avg,     file.path(net_dir, "Centrality_Averages_Fixed.csv"), row.names = FALSE)
message("Saved Fixed centrality CSVs to: ", normalizePath(net_dir))

# ---------- 6) Align with FRM ----------
aligned <- dplyr::inner_join(
  FRM_index %>% dplyr::arrange(date),
  centrality_avg %>% dplyr::arrange(date),
  by = "date"
)

# ---------- 7) Minimal transparent theme ----------
theme_transparent_min <- theme_classic(base_size = 12) +
  theme(
    panel.background       = element_rect(fill = "transparent", colour = NA),
    plot.background        = element_rect(fill = "transparent", colour = NA),
    legend.background      = element_rect(fill = "transparent", colour = NA),
    legend.box.background  = element_rect(fill = "transparent", colour = NA),
    legend.key             = element_rect(fill = "transparent", colour = NA),
    legend.position        = "bottom",
    legend.direction       = "horizontal",
    legend.box.just        = "center",
    axis.text              = element_text(size = 12, colour = "black"),
    axis.title             = element_text(size = 14, colour = "black"),
    panel.grid             = element_blank()
  )

# ---------- 8) Dual-axis overlay (transparent; #007AFF vs #FF3B30) ----------
# ---------- 8) Dual-axis overlays (line+line) and (line+bars) ----------
# FRM raw stays on the LEFT; centrality on RIGHT (own scale)

plot_overlay_dual <- function(
  df, colname, right_label, out_dir,
  frm_col   = "#007AFF",    # FRM blue
  cent_col  = "#FF3B30",    # centrality red
  lw_frm    = 1.4,
  lw_cent   = 1.4,
  filename  = NULL
){
  D <- df[, c("date","frm", colname), drop = FALSE]; names(D)[3] <- "cent"
  D <- D[stats::complete.cases(D), , drop = FALSE]
  if (!nrow(D)) return(invisible(NULL))

  frm_min <- min(D$frm,  na.rm = TRUE); frm_max <- max(D$frm,  na.rm = TRUE)
  cen_min <- min(D$cent, na.rm = TRUE); cen_max <- max(D$cent, na.rm = TRUE)
  if (frm_max <= frm_min) { frm_min <- 0; frm_max <- 1 }
  if (cen_max <= cen_min) { cen_min <- 0; cen_max <- 1 }
  to_left   <- function(x) (x - cen_min) * (frm_max - frm_min) / (cen_max - cen_min) + frm_min
  from_left <- function(y) (y - frm_min) * (cen_max - cen_min) / (frm_max - frm_min) + cen_min

  pal <- setNames(c(frm_col, cent_col), c("FRM@Stable", right_label))
  out_png <- file.path(out_dir, if (is.null(filename))
    paste0("FRM_vs_", gsub("_avg$","", colname), "_DualAxis.png") else filename)

  png(out_png, width = 1600, height = 900, bg = "transparent", res = 120); print(
    ggplot(D, aes(x = date)) +
      geom_line(aes(y = frm,           color = "FRM@Stable"), linewidth = lw_frm, lineend = "round") +
      geom_line(aes(y = to_left(cent), color = right_label),  linewidth = lw_cent, lineend = "round") +
      scale_color_manual(values = pal, breaks = names(pal), limits = names(pal), name = NULL) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
                   expand = expansion(mult = c(0.005, 0.01))) +
      scale_y_continuous(
        name = "FRM@Stable (raw)",
        sec.axis = sec_axis(~ from_left(.), name = right_label)
      ) +
      theme_transparent_min +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  ); dev.off()
}

# Bars version (FRM line + centrality bars on right axis)
plot_overlay_dual_bars <- function(df, colname, right_label, out_dir,
                                   frm_col = "#007AFF", bar_col = "#007AFF", bar_alpha = 0.65){
  D <- df[, c("date","frm", colname), drop = FALSE]; names(D)[3] <- "cent"
  D <- D[stats::complete.cases(D), , drop = FALSE]
  if (!nrow(D)) return(invisible(NULL))

  # reuse the utils helper to do proper transforms and labels
  out_png <- file.path(out_dir, paste0("FRM_vs_", gsub("_avg$","", colname), "_DualAxis_Bars.png"))
  dualaxis_frm_line_vs_bars(
    df = transform(D, date = as.Date(date)),
    date_col = "date", frm_col = "frm",
    right_col = "cent", right_label = right_label,
    out_png   = out_png,
    frm_colr  = frm_col,
    bar_colr  = bar_col,
    bar_alpha = bar_alpha
  )
}

# ---------- 9) Build all overlays (both styles) ----------
plot_overlay_dual(aligned, "outdegree_avg",    "Out-degree",   web_dir)
plot_overlay_dual(aligned, "indegree_avg",     "In-degree",    web_dir)
plot_overlay_dual(aligned, "closeness_avg",    "Closeness",    web_dir)
plot_overlay_dual(aligned, "betweenness_avg",  "Betweenness",  web_dir)
plot_overlay_dual(aligned, "eigenvector_avg",  "Eigenvector",  web_dir)
plot_overlay_dual(aligned, "out_strength_avg", "Out-strength", web_dir)
plot_overlay_dual(aligned, "in_strength_avg",  "In-strength",  web_dir)

# bars variants (names end with _DualAxis_Bars.png)
plot_overlay_dual_bars(aligned, "outdegree_avg",    "Out-degree",   web_dir)
plot_overlay_dual_bars(aligned, "indegree_avg",     "In-degree",    web_dir)
plot_overlay_dual_bars(aligned, "closeness_avg",    "Closeness",    web_dir)
plot_overlay_dual_bars(aligned, "betweenness_avg",  "Betweenness",  web_dir)
plot_overlay_dual_bars(aligned, "eigenvector_avg",  "Eigenvector",  web_dir)
plot_overlay_dual_bars(aligned, "out_strength_avg", "Out-strength", web_dir)
plot_overlay_dual_bars(aligned, "in_strength_avg",  "In-strength",  web_dir)

message("Done. CSVs -> ", normalizePath(net_dir),
        " | PNGs -> ", normalizePath(web_dir))

# ================= Correlation table (HTML, no colors) =================
# FRM vs. Closeness and Eigenvector (Fixed universe)
# ======================================================================

# 0) Inputs (reuse paths)
cent_csv <- file.path(net_dir, "Centrality_Averages_Fixed.csv")
if (!file.exists(cent_csv)) stop("Fixed centrality CSV not found at: ", cent_csv)

# 1) Load & align
FRM_index2 <- read.csv(frm_csv, stringsAsFactors = FALSE)
names(FRM_index2) <- tolower(names(FRM_index2))
CENT <- read.csv(cent_csv, stringsAsFactors = FALSE)
names(CENT) <- tolower(names(CENT))

parse_date <- function(x){
  d <- suppressWarnings(as.Date(x))
  if (all(is.na(d))) d <- suppressWarnings(as.Date(x, "%Y-%m-%d"))
  if (all(is.na(d))) d <- suppressWarnings(as.Date(x, "%m/%d/%Y"))
  if (all(is.na(d))) {
    dt <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
    if (!all(is.na(dt))) d <- as.Date(dt, tz = "UTC")
  }
  d
}
FRM_index2$date <- parse_date(FRM_index2$date)
CENT$date       <- parse_date(CENT$date)

dat <- merge(
  FRM_index2[, c("date","frm")],
  CENT[, c("date","closeness_avg","eigenvector_avg")],
  by = "date", all = FALSE
)
dat <- dat[is.finite(dat$frm) & is.finite(dat$closeness_avg) & is.finite(dat$eigenvector_avg), ]
dat$FRM       <- dat$frm
dat$closeness <- dat$closeness_avg
dat$eigen     <- dat$eigenvector_avg

vars <- c("FRM","closeness","eigen")
R <- matrix(NA_real_, 3, 3, dimnames = list(vars, vars))
P <- matrix(NA_real_, 3, 3, dimnames = list(vars, vars))
S <- matrix("",        3, 3, dimnames = list(vars, vars))

for (i in 1:3) for (j in i:3) {
  if (i == j) { R[i,j] <- 1; P[i,j] <- 0; next }
  x <- dat[[vars[i]]]; y <- dat[[vars[j]]]
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) >= 3) {
    ct <- suppressWarnings(stats::cor.test(x[ok], y[ok], method = "pearson"))
    r <- unname(ct$estimate); p <- ct$p.value
  } else { r <- NA_real_; p <- NA_real_ }
  R[i,j] <- R[j,i] <- r
  P[i,j] <- P[j,i] <- p
  S[i,j] <- S[j,i] <- if (is.na(p)) "" else if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
}

# Save CSVs (optional)
out_dir2 <- file.path(website_path, date_end, "Fixed")
dir.create(out_dir2, recursive = TRUE, showWarnings = FALSE)
write.csv(R, file.path(out_dir2, "Correlation_r_FRM_Closeness_Eigen.csv"), row.names = TRUE)
write.csv(P, file.path(out_dir2, "Correlation_p_FRM_Closeness_Eigen.csv"), row.names = TRUE)

# HTML table (no colors) — build with a quote variable that does not use single quotes
fmt_mat <- apply(R, 2, function(col) formatC(col, format = "f", digits = 3))
fmt_mat <- matrix(fmt_mat, nrow = nrow(R), ncol = ncol(R),
                  dimnames = dimnames(R), byrow = FALSE)
cell <- matrix(paste0(fmt_mat, S), nrow = nrow(R), ncol = ncol(R),
               dimnames = dimnames(R))

html_rows <- character(0)
header <- paste0("<tr><th></th>", paste0(sprintf("<th>%s</th>", vars), collapse = ""), "</tr>")
html_rows <- c(html_rows, header)
for (i in 1:nrow(cell)) {
  row_cells <- paste0(sprintf("<td>%s</td>", cell[i, ]), collapse = "")
  html_rows <- c(html_rows, sprintf("<tr><th>%s</th>%s</tr>", vars[i], row_cells))
}

q <- "\""
html <- paste0(
  "<!doctype html>
",
  "<html lang=", q, "en", q, ">
",
  "<head>
",
  "<meta charset=", q, "utf-8", q, ">
",
  "<title>Correlation: FRM, Closeness, Eigen</title>
",
  "<style>
",
  "  body { font-family: -apple-system, system-ui, Arial, sans-serif; color:#000; background:#fff; margin:24px; }
",
  "  h1   { font-size: 18px; margin: 0 0 8px 0; }
",
  "  p    { margin: 4px 0 12px 0; }
",
  "  table { border-collapse: collapse; }
",
  "  th, td { border: 1px solid #000; padding: 6px 10px; text-align: right; }
",
  "  th:first-child { text-align: left; }
",
  "</style>
",
  "</head>
",
  "<body>
",
  "<h1>Correlation (Pearson): FRM, Closeness, Eigenvector</h1>
",
  "<p>Entries are <em>r</em>; significance stars: <strong>***</strong> p &lt; 0.01, <strong>**</strong> p &lt; 0.05, <strong>*</strong> p &lt; 0.10.</p>
",
  "<table>
",
  paste0(html_rows, collapse = "
"),
  "
</table>
",
  "</body>
",
  "</html>
"
)

out_html <- file.path(out_dir2, "Correlation_FRM_Closeness_Eigen.html")
writeLines(html, out_html)
message("HTML written to: ", out_html)

