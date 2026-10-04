# ==== FRM_SC_network.R ==================================================
if (exists('FRM_individ_fixed') && !is.null(FRM_individ_fixed) && exists('J_eff_fixed') && !is.null(J_eff_fixed)) {
  lambdas_fixed_vals <- FRM_individ_fixed[, -1, drop = FALSE]
  lambdas_fixed_vals[!is.finite(lambdas_fixed_vals)] <- 0
  rng <- range(lambdas_fixed_vals, na.rm = TRUE)
  rescale_safe <- function(v) { if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) return(rep(10, length(v))); scales::rescale(v, to = c(6, 22), from = rng) }

  fig <- image_graph(width = 1000, height = 1000, res = 96, bg = 'transparent')
  options(show.error.messages = FALSE)
  for (t in idx_start_at_or_after(ticker, date_start_fixed):idx_end_at_or_before(ticker, date_end_fixed)) try({
    fcsv <- file.path(output_path, 'Adj_Matrices/Fixed', paste0('adj_matrix_', ticker[t], '.csv'))
    if (!file.exists(fcsv)) next
    adj0 <- as.matrix(read.csv(file = fcsv, header = TRUE, sep = ',', row.names = 1))
    adj0 <- adj0[1:J_eff_fixed, 1:J_eff_fixed, drop = FALSE]; adj0 <- apply(adj0, 2, as.numeric)
    netw1 <- graph_from_adjacency_matrix(adj0, mode = 'directed', weighted = TRUE)
    row_id <- which(FRM_individ_fixed[, 1] == ticker[t])
    sizes <- if (length(row_id)) rescale_safe(as.numeric(lambdas_fixed_vals[row_id, ])) else rep(10, J_eff_fixed)
    V(netw1)$color <- ifelse(V(netw1)$name == stock_main, 'orange', 'lightgrey')
    colors <- rep('gray', length(E(netw1)))
    colors <- ifelse(head_of(netw1, E(netw1))$name == stock_main, 'blue',  colors)
    colors <- ifelse(tail_of(netw1, E(netw1))$name == stock_main, 'orange', colors)
    plot(netw1, layout = layout_in_circle, vertex.label = colnames(adj0),
         edge.width = 0.8, edge.color = colors, edge.arrow.size = 0.9, edge.arrow.width = 1,
         vertex.size = sizes)
    this_date <- as.Date(as.character(ticker[t]), '%Y%m%d')
    frm_val <- NA; w <- which(FRM_index$date == this_date); if (length(w)) frm_val <- FRM_index$frm[w[1]]
    title(xlab = paste0(this_date, if (is.finite(frm_val)) paste0('\n FRM: ', round(frm_val, 5)) else ''),
          cex.lab = 1.15, font.lab = 2, line = -0.5)
  })
  options(show.error.messages = TRUE); dev.off()
  animation <- image_animate(fig, fps = 5)
  image_write(animation, file.path(output_path, 'Network',
                                   paste0('Network_', date_start_fixed, '_', date_end_fixed, '_', channel, '.gif')))
}

# Quick adjacency heatmap for the latest matrix found
adj_files <- list.files(file.path(output_path, 'Adj_Matrices'), full.names = TRUE, pattern = '^adj_matrix_\d+\.csv$')
if (length(adj_files)) {
  adj_file <- adj_files[which.max(gsub('\u005C\u005CD', '', basename(adj_files)))]
  mat <- as.matrix(read.csv(adj_file, row.names = 1, check.names = FALSE))
  df_long <- melt(mat, varnames = c('From', 'To'), value.name = 'Weight')
  date_str <- gsub('adj_matrix_|\.csv', '', basename(adj_file))
  date_fmt <- tryCatch(as.Date(date_str, '%Y%m%d'), error = function(e) date_str)
  p <- ggplot(df_long, aes(x = To, y = From, fill = Weight)) +
    geom_tile(color = 'white') + geom_text(aes(label = round(Weight, 2)), size = 3) +
    scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue', midpoint = 0) +
    labs(title = paste('Adjacency Matrix —', date_fmt), x = 'To', y = 'From') +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = 'transparent', colour = NA),
          plot.background  = element_rect(fill = 'transparent', colour = NA),
          legend.background= element_rect(fill = 'transparent'),
          legend.box.background = element_rect(fill = 'transparent'))
  ggsave(file.path(output_path, 'Network', paste0('AdjMatrix_', date_str, '.png')), p, width = 8, height = 6, bg = 'transparent')
}
