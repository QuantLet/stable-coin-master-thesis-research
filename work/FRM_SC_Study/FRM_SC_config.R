
rm(list = ls(all = TRUE)); graphics.off(); options(digits=6, warn=-1)
#wdir <- '/Users/danielpele/Library/CloudStorage/GoogleDrive-danpele@ase.ro/Other computers/Asus/G/PROIECTE/2025 FRM Stable Coins'
wdir <- 'D:\\G\\PROIECTE\\2025 FRM Stable Coins'
setwd(wdir)

channel <- "Stable"
date_start_source <- 20200102; date_end_source <- 20250302
date_start <- 20200102; date_end <- 20250302
date_start_fixed <- 20200405; date_end_fixed <- 20250302

s <- 90; tau <- 0.05; I <- 25; L <- 5; J <- 11
stock_main <- "tether"
quantiles <- c(0.99,0.95,0.90,0.85,0.80,0.75,0.70,0.65,0.60,0.55,0.50,0.25)

input_path  <- file.path("Input", channel, paste0(date_start_source, "-", date_end_source))
output_path <- if (tau == 0.05 & s == 90) file.path("Output", channel) else
  file.path("Output", channel, paste0("Sensitivity/tau=", 100*tau, "/s=", s))
website_path <- file.path("Website", channel)

dirs <- c(output_path,
          file.path(output_path,"Adj_Matrices"),
          file.path(output_path,"Adj_Matrices/Fixed"),
          file.path(output_path,"Lambda"),
          file.path(output_path,"Lambda/Fixed"),
          file.path(output_path,"Lambda/Quantiles"),
          file.path(output_path,"Top"),
          file.path(output_path,"Network"),
          file.path(output_path,"Macro"),
          file.path(output_path,"Boxplot"),
          website_path,
          file.path(website_path, date_end))
invisible(lapply(dirs, dir.create, recursive=TRUE, showWarnings=FALSE))

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(lubridate); library(zoo)
  library(ggplot2); library(data.table); library(igraph); library(magick); library(scales)
  library(stringr); library(graphics); library(plotly); library(tidyverse); require(timeDate)
  library(reshape2); library(quadprog); library(MASS)
})
source("FRM_Statistics_Algorithm.R")

