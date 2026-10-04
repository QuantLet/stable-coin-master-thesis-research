
source("FRM_SC_utils.R")
load(file.path(output_path,"stage_20_loaded.RData"))
load(file.path(output_path,"stage_30_varying.RData"))

rds_path <- file.path(output_path, "Lambda", paste0("FRM_", channel, ".rds"))
FRM_history <- if (file.exists(rds_path)) c(readRDS(rds_path), FRM_individ) else FRM_individ
FRM_history <- FRM_history[order(as.Date(names(FRM_history)))]
saveRDS(FRM_history, rds_path)

N_h <- length(FRM_history); iso_dates <- as.Date(names(FRM_history))
stock_names <- unique(unlist(lapply(FRM_history, function(x) colnames(x))))
if (length(stock_names)) {
  lambdas_wide <- matrix(0, N_h, length(stock_names)+1)
  for (k in seq_along(stock_names)) for (t in 1:N_h)
    if (stock_names[k] %in% colnames(FRM_history[[t]])) lambdas_wide[t, k+1] <- FRM_history[[t]][, stock_names[k]]
  lambdas_wide[,1] <- format(iso_dates, "%Y-%m-%d"); colnames(lambdas_wide) <- c("date", stock_names)
  write.csv(lambdas_wide, file.path(output_path,"Lambda","lambdas_wide.csv"), row.names=FALSE)
}

FRM_index <- data.frame(
  date = iso_dates,
  frm  = vapply(FRM_history, function(m){ v <- suppressWarnings(as.numeric(m[1, ])); v <- v[is.finite(v)]; if (!length(v)) NA_real_ else mean(v) }, numeric(1))
)
write.csv(FRM_index, file.path(output_path,"Lambda", paste0("FRM_", channel, "_index.csv")), row.names=FALSE)

good_frm <- FRM_index$frm[is.finite(FRM_index$frm)]
risk_ecdf <- if (length(good_frm)) ecdf(good_frm) else function(x) NA_real_
FRM_plot <- FRM_index; FRM_plot$risk <- round(100 * risk_ecdf(FRM_plot$frm), 2)
FRM_plot$`Risk level` <- factor(ifelse(FRM_plot$risk<20,"1. Low risk",
                                ifelse(FRM_plot$risk<40,"2. General risk",
                                ifelse(FRM_plot$risk<60,"3. Elevated risk",
                                ifelse(FRM_plot$risk<80,"4. High risk","5. Severe risk")))),
                                levels=names(risk_colors))
png(file.path(website_path, date_end, paste0("FRMColor_", channel,".png")), width=900, height=600, bg="transparent")
print(ggplot(FRM_plot, aes(x=date, y=frm)) +
  geom_point(aes(color=`Risk level`), size=1, na.rm=TRUE) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  scale_color_manual(values=risk_colors) +
  labs(title=NULL, x=NULL, y=paste0("FRM@", channel)) + theme_transparent_bottom)
dev.off()
save(FRM_index, file = file.path(output_path, "stage_50_index.RData"))

