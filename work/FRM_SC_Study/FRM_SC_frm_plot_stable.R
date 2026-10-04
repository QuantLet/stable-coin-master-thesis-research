
source("FRM_SC_utils.R"); load(file.path(output_path,"stage_50_index.RData"))
out_png <- file.path(website_path, date_end, "FRM_Stable_Index.png")
png(out_png, width=1200, height=700, bg="transparent")
print(ggplot(FRM_index, aes(x=date, y=frm)) +
  geom_line(linewidth=0.9, color="blue") +
  scale_x_date(date_breaks="3 months", date_labels="%b %Y") +
  labs(title=NULL, x=NULL, y="FRM") +
  theme_transparent_bottom + theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)))
dev.off()

