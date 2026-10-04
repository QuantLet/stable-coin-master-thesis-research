
source("FRM_SC_utils.R")
plot_adj_matrix <- function(adj_file, out_dir=file.path(output_path,"Network")){
  stopifnot(file.exists(adj_file))
  date_str <- gsub("adj_matrix_|\\.csv", "", basename(adj_file))
  date_fmt <- tryCatch(as.Date(date_str, "%Y%m%d"), error=function(e) date_str)
  mat <- as.matrix(read.csv(adj_file, row.names=1, check.names=FALSE))
  df_long <- reshape2::melt(mat, varnames=c("From","To"), value.name="Weight")
  p <- ggplot(df_long, aes(x=To, y=From, fill=Weight)) +
    geom_tile(color="white") + geom_text(aes(label=round(Weight,2)), size=3) +
    scale_fill_gradient2(low="red", mid="white", high="blue", midpoint=0) +
    labs(title=NULL, x="To", y="From") +
    theme_minimal(base_size=14) +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          panel.background=element_rect(fill="transparent", colour=NA),
          plot.background=element_rect(fill="transparent", colour=NA),
          legend.box.background=element_rect(fill="transparent"),
          legend.background=element_rect(fill="transparent"))
  dir.create(out_dir, showWarnings=FALSE, recursive=TRUE)
  out_png <- file.path(out_dir, paste0("AdjMatrix_", date_str, ".png"))
  ggsave(out_png, p, width=8, height=6, bg="transparent"); message("Saved: ", out_png)
  invisible(p)
}
# Example:
# f <- list.files(file.path(output_path,"Adj_Matrices"), full.names=TRUE, pattern="adj_matrix_\\d+\\.csv$")[1]
# plot_adj_matrix(f)

