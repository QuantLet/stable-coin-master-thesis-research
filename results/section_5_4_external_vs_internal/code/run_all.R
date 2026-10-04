#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1L])))
} else normalizePath(getwd())

scripts <- c(
  "01_estimate_block_models.R",
  "02_analyse_block_models.R",
  "03_plot_figure_5_4.R",
  "04_finalize_QA.R"
)
for (script in scripts) {
  status <- system2("Rscript", file.path(script_dir, script))
  if (!identical(status, 0L)) stop("Failed: ", script)
}
message("Section 5.4 pipeline completed.")
