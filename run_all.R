args <- commandArgs(trailingOnly = FALSE)
script_arg <- args[grep("^--file=", args)]
root <- if (length(script_arg)) dirname(normalizePath(sub("^--file=", "", script_arg[1]), mustWork = TRUE)) else normalizePath(getwd(), mustWork = TRUE)
setwd(root)
project_library <- file.path(root, "environment", "R-library")
if (dir.exists(project_library)) {
  .libPaths(c(project_library, .libPaths()))
  Sys.setenv(R_LIBS_USER = project_library)
}

mode <- tolower(Sys.getenv("REPRO_MODE", "check"))
if (!mode %in% c("check", "frozen", "full")) stop("REPRO_MODE must be check, frozen, or full.")

run_r <- function(path, env = character(), wd = root) {
  message("\n==> ", path)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(wd)
  inherited_env <- if (dir.exists(project_library)) paste0("R_LIBS_USER=", project_library) else character()
  status <- system2("Rscript", path, env = c(inherited_env, env), wait = TRUE)
  if (!identical(status, 0L)) stop("Command failed: Rscript ", path)
}

run_r("scripts/validate_repository.R")
if (mode == "check") quit(save = "no", status = 0)

if (mode == "full") {
  run_r("outputs/section_4_4_reference_adjusted_event_study/code/run_all.R")
} else {
  run_r("outputs/section_4_4_reference_adjusted_event_study/code/02_event_study_three_definitions.R")
  run_r("outputs/section_4_4_reference_adjusted_event_study/code/03_plot_reference_adjusted_event_study.R")
  run_r("outputs/section_4_4_reference_adjusted_event_study/code/05_plot_figure_4_4_redesign.R")
  run_r("outputs/section_4_4_reference_adjusted_event_study/code/04_finalize_bundle_QA.R")
}

crypto_env <- if (mode == "frozen") c("FRM_SKIP_RAW_BUILD=1", "FRM_SKIP_ESTIMATION=1") else character()
run_r("outputs/section_4_5_stable_crypto/code/run_all.R", env = crypto_env)
run_r("outputs/section_5_1_active_risk_drivers/code/section_5_1_active_sets.R")

run_r("code/section_5_2_tail_risk_links.R", wd = file.path(root, "outputs/section_5_2_tail_risk_links"))
run_r("code/section_5_2_inference_and_robustness.R", wd = file.path(root, "outputs/section_5_2_tail_risk_links"))
run_r("code/section_5_3_macro_financial.R", wd = file.path(root, "outputs/section_5_3_macro_financial"))

if (mode == "full") {
  run_r("outputs/section_5_4_external_vs_internal/code/run_all.R")
} else {
  run_r("outputs/section_5_4_external_vs_internal/code/02_analyse_block_models.R")
  run_r("outputs/section_5_4_external_vs_internal/code/03_plot_figure_5_4.R")
  run_r("outputs/section_5_4_external_vs_internal/code/04_finalize_QA.R")
}

run_r("code/section_5_5_centrality.R", wd = file.path(root, "outputs/section_5_5_centrality"))
run_r("code/section_5_5_onchain_crosscheck.R", wd = file.path(root, "outputs/section_5_5_centrality"))

if (mode == "full") run_r("outputs/section_5_6_microstructure_extension/code/01_estimate_micro_extension.R")
run_r("outputs/section_5_6_microstructure_extension/code/02_analyse_micro_extension.R")
run_r("outputs/section_6_1_market_concentration/code/01_build_market_concentration.R")
run_r("outputs/section_6_2_concentration_dpi/code/01_estimate_concentration_dpi.R")
run_r("outputs/section_6_3_portfolio_protocol/code/01_run_portfolio_protocol.R")
run_r("outputs/section_6_4_oos_portfolio_performance/code/01_build_oos_performance.R")
run_r("outputs/section_6_5_event_resilience/code/01_run_event_resilience.R")

message("\nReproduction sequence completed in mode: ", mode)
