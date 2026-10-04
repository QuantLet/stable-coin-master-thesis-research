args <- commandArgs(trailingOnly = FALSE)
script_arg <- args[grep("^--file=", args)]
root <- if (length(script_arg)) {
  dirname(dirname(normalizePath(sub("^--file=", "", script_arg[1]), mustWork = TRUE)))
} else {
  normalizePath(getwd(), mustWork = TRUE)
}

required <- c(
  "README.md",
  "DATA_AVAILABILITY.md",
  "REPRODUCTION_ORDER.md",
  "results/section_4_4_reference_adjusted_event_study/code/run_all.R",
  "results/section_4_5_stable_crypto/code/run_all.R",
  "results/section_5_1_active_risk_drivers/code/section_5_1_active_sets.R",
  "results/section_5_2_tail_risk_links/code/section_5_2_tail_risk_links.R",
  "results/section_5_3_macro_financial/code/section_5_3_macro_financial.R",
  "results/section_5_4_external_vs_internal/code/run_all.R",
  "results/section_5_5_centrality/code/section_5_5_centrality.R",
  "results/section_5_6_microstructure_extension/code/02_analyse_micro_extension.R",
  "results/section_6_1_market_concentration/code/01_build_market_concentration.R",
  "results/section_6_2_concentration_dpi/code/01_estimate_concentration_dpi.R",
  "results/section_6_3_portfolio_protocol/code/01_run_portfolio_protocol.R",
  "results/section_6_4_oos_portfolio_performance/code/01_build_oos_performance.R",
  "results/section_6_5_event_resilience/code/01_run_event_resilience.R"
)

missing <- required[!file.exists(file.path(root, required))]
if (length(missing)) stop("Missing required files:\n", paste(missing, collapse = "\n"))

r_files <- c(
  list.files(file.path(root, "results"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
  file.path(root, "run_all.R"),
  list.files(file.path(root, "scripts"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
  list.files(file.path(root, "environment"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
)
r_files <- unique(r_files[file.exists(r_files)])
r_files <- r_files[!grepl("/R-library/", r_files, fixed = TRUE)]
parse_errors <- character()
for (path in r_files) {
  ok <- tryCatch({ parse(path); TRUE }, error = function(e) FALSE)
  if (!ok) parse_errors <- c(parse_errors, path)
}
if (length(parse_errors)) stop("R parse failures:\n", paste(parse_errors, collapse = "\n"))

message("Repository validation passed: ", length(required), " required files and ", length(r_files), " R scripts checked.")
