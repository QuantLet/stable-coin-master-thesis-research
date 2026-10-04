#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else normalizePath(getwd())
source(file.path(script_dir, "00_config.R"))

required_files <- c(
  "config/crypto_asset_universe.csv",
  "config/events_primary.csv",
  "code/FRM_Statistics_Algorithm.R",
  "source_data/prepared/Section_4_5_daily_analysis_panel.csv",
  "source_data/frm_crypto/ppt_dynamic_15/frm_strict.csv",
  "source_data/frm_crypto/ppt_dynamic_15/frm_numerical_screened.csv",
  "source_data/frm_crypto/balanced_complete_9/frm_numerical_screened.csv",
  "tables/Table_4_5b_FRM_Coupling.csv",
  "tables/Table_4_5c_High_Risk_State_Association.csv",
  "tables/Table_4_5d_OOS_R2.csv",
  "tables/Table_4_5e_OOS_Forecast_Tests.csv",
  "tables/Table_4_5f_Paired_Event_Effects_0_30_HAC90.csv",
  "tables/Table_S4_5_OOS_Definition_Robustness.csv",
  "figures/Figure_4_5_Stablecoin_Crypto_Risk.svg",
  "figures/Figure_4_5_Stablecoin_Crypto_Risk.pdf",
  "figures/Figure_4_5_Stablecoin_Crypto_Risk.tiff",
  "figures/Figure_4_5_Stablecoin_Crypto_Risk.png",
  "figures/Figure_4_5_caption.txt",
  "Section_4_5_Writing_Logic_and_Empirical_Design_zh.md",
  "Section_4_5_Manuscript_Ready_EN.md",
  "README_zh.md"
)

oos <- read.csv(file.path(tables_dir, "Table_4_5d_OOS_R2.csv"), stringsAsFactors = FALSE)
tests <- read.csv(file.path(tables_dir, "Table_4_5e_OOS_Forecast_Tests.csv"), stringsAsFactors = FALSE)
events_out <- read.csv(file.path(tables_dir, "Table_S4_5_All_Paired_Event_Effects_HAC90.csv"), stringsAsFactors = FALSE)
robust <- read.csv(file.path(tables_dir, "Table_S4_5_OOS_Definition_Robustness.csv"), stringsAsFactors = FALSE)
events <- read.csv(file.path(config_dir, "events_primary.csv"), stringsAsFactors = FALSE)

expected_horizons <- sort(as.integer(forecast_horizons))
primary_complete <- all(is.finite(oos$oos_r_squared)) &&
  identical(sort(unique(oos$horizon_days)), expected_horizons) &&
  setequal(unique(oos$scheme), oos_schemes) &&
  setequal(unique(oos$model), c("stable", "crypto", "joint"))
tests_complete <- all(is.finite(tests$p_value)) && all(is.finite(tests$p_holm_four_horizons))
event_primary <- events_out[
  events_out$contrast == "acute_to_transmission_0_30_vs_baseline" &
    events_out$series_id %in% c("stable_adjusted_screened", "crypto_dynamic15_screened", "stable_minus_crypto_screened"), ]
event_complete <- nrow(event_primary) == nrow(events) * 3L &&
  all(event_primary$window_complete) && all(is.finite(event_primary$effect_pct))
hormuz_persistence <- events_out[
  events_out$event_id == "hormuz" & events_out$contrast == "persistence_31_60_vs_baseline", ]
incomplete_guard <- nrow(hormuz_persistence) > 0L &&
  all(!hormuz_persistence$window_complete) && all(is.na(hormuz_persistence$effect_pct))
robust_complete <- setequal(unique(robust$specification), c("strict_both", "balanced9_crypto")) &&
  identical(sort(unique(robust$horizon_days)), expected_horizons) && all(is.finite(robust$oos_r_squared))

upstream_qa_files <- c(
  file.path(qa_dir, "crypto_panel_QA.csv"),
  file.path(qa_dir, "Section_4_5_outcome_panel_QA.csv"),
  file.path(qa_dir, "Section_4_5_analysis_QA.csv")
)
upstream_pass <- all(vapply(upstream_qa_files, function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE)
  all(x$status == "PASS")
}, logical(1)))

figure_paths <- file.path(bundle_dir, c(
  "figures/Figure_4_5_Stablecoin_Crypto_Risk.svg",
  "figures/Figure_4_5_Stablecoin_Crypto_Risk.pdf",
  "figures/Figure_4_5_Stablecoin_Crypto_Risk.tiff",
  "figures/Figure_4_5_Stablecoin_Crypto_Risk.png"
))
figure_nonempty <- all(file.exists(figure_paths)) && all(file.info(figure_paths)$size > 1000)

qa <- data.frame(
  check = c(
    "all_required_delivery_files_present", "all_upstream_qa_pass",
    "primary_oos_grid_complete", "forecast_tests_complete",
    "primary_0_30_event_grid_complete", "incomplete_hormuz_persistence_suppressed",
    "definition_robustness_grid_complete", "figure_exports_nonempty"
  ),
  status = c(
    if (all(file.exists(file.path(bundle_dir, required_files)))) "PASS" else "FAIL",
    if (upstream_pass) "PASS" else "FAIL",
    if (primary_complete) "PASS" else "FAIL",
    if (tests_complete) "PASS" else "FAIL",
    if (event_complete) "PASS" else "FAIL",
    if (incomplete_guard) "PASS" else "FAIL",
    if (robust_complete) "PASS" else "FAIL",
    if (figure_nonempty) "PASS" else "FAIL"
  ),
  detail = c(
    paste(length(required_files), "required files"),
    paste(basename(upstream_qa_files), collapse = "; "),
    "3 outcomes x 4 horizons x 2 schemes x 3 models",
    "Clark-West and DM-style HAC tests with four-horizon Holm correction",
    "6 events x Stable/Crypto/direct-difference",
    "31-60 day window ends after local Crypto data and is returned as NA",
    "strict-both and balanced-nine Crypto systems",
    paste(basename(figure_paths), collapse = "; ")
  ), stringsAsFactors = FALSE
)
write_csv(qa, file.path(qa_dir, "Section_4_5_final_QA.csv"))
if (any(qa$status == "FAIL")) stop("Section 4.5 final QA failed.")

all_files <- list.files(bundle_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE)
all_files <- all_files[file.info(all_files)$isdir %in% FALSE]
all_files <- all_files[!grepl("(^|/)r_library/", all_files)]
all_files <- all_files[!basename(all_files) %in% c("bundle_manifest_md5.csv", "Figure_4_5_preview_v2.png")]
manifest <- data.frame(
  relative_path = substring(all_files, nchar(bundle_dir) + 2L),
  bytes = file.info(all_files)$size,
  md5 = unname(tools::md5sum(all_files)),
  stringsAsFactors = FALSE
)
manifest <- manifest[order(manifest$relative_path), ]
write_csv(manifest, file.path(qa_dir, "bundle_manifest_md5.csv"))
message("Section 4.5 final QA completed.")
