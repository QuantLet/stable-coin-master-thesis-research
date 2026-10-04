#!/usr/bin/env Rscript

# Full reproduction driver. Set FRM_SKIP_ESTIMATION=1 only when the packaged
# FRM@Crypto outputs are retained and the user wants to rerun downstream steps.

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else normalizePath(getwd())
bundle_dir <- dirname(script_dir)

scripts <- character()
if (Sys.getenv("FRM_SKIP_RAW_BUILD", "0") != "1") scripts <- c(scripts, "01_build_crypto_panel.R")
if (Sys.getenv("FRM_SKIP_ESTIMATION", "0") != "1") scripts <- c(scripts, "02_estimate_crypto_frm.R")
scripts <- c(scripts, "03_build_outcomes.R", "04_cross_market_analysis.R",
             "05_plot_figure_4_5.R", "06_finalize_QA.R")

for (script in scripts) {
  message("\n>>> Running ", script)
  status <- system2(file.path(R.home("bin"), "Rscript"), file.path(script_dir, script))
  if (!identical(status, 0L)) stop("Pipeline failed in ", script)
}
message("Section 4.5 full pipeline completed: ", bundle_dir)
