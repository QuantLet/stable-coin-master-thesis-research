#!/usr/bin/env Rscript
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(file_arg) != 1L) stop("Run this file with Rscript.")
code_dir <- dirname(normalizePath(sub("^--file=", "", file_arg)))
bundle_dir <- dirname(code_dir)
study_dir <- dirname(dirname(bundle_dir))
input_dir <- file.path(bundle_dir, "source_data", "input")
origins <- c(
  peg_panel_reference_adjusted.csv =
    "outputs/section_5_4_external_vs_internal/source_data/input/peg_panel_reference_adjusted.csv",
  Stable_Macro_20260531.csv =
    "outputs/section_5_4_external_vs_internal/source_data/input/Stable_Macro_20260531.csv",
  OOS_Block_Model_Predictions.rds =
    "outputs/section_5_4_external_vs_internal/source_data/OOS_Block_Model_Predictions.rds",
  FRM_Statistics_Algorithm.R =
    "outputs/section_5_4_external_vs_internal/reference_code/FRM_Statistics_Algorithm.R",
  stablecoin_onchain_daily_20200101_20260531.csv =
    "outputs/onchain_microdata_20260919/stablecoin_onchain_daily_20200101_20260531.csv",
  dpi_primary_robust_mad_strict.csv =
    "outputs/section_5_5_centrality/source_data/input/dpi_primary_robust_mad_strict.csv"
)
source_files <- file.path(study_dir, unname(origins))
bundle_files <- file.path(input_dir, names(origins))
stopifnot(all(file.exists(source_files)), all(file.exists(bundle_files)))
source_md5 <- unname(tools::md5sum(source_files))
bundle_md5 <- unname(tools::md5sum(bundle_files))
manifest <- data.frame(file = names(origins), origin = unname(origins),
                       source_md5 = source_md5, bundle_md5 = bundle_md5,
                       exact_copy = source_md5 == bundle_md5)
write.csv(manifest, file.path(bundle_dir, "qa", "input_manifest_md5.csv"),
          row.names = FALSE)
if (!all(manifest$exact_copy)) stop("One or more copied inputs differ.")
print(manifest, row.names = FALSE)
