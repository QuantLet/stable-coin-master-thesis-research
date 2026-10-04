#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(file_arg) && sub("^--file=", "", file_arg[1]) != "-") {
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else {
  normalizePath(getwd())
}
source(file.path(script_dir, "00_config.R"))

summary_file <- file.path(results_dir, "FRM_summary_20260531.csv")
if (!file.exists(summary_file)) stop("Run 02_estimate_frm.R first.")
summary_values <- read.csv(summary_file, stringsAsFactors = FALSE)
value_of <- function(metric) summary_values$value[match(metric, summary_values$metric)]

construction_table <- data.frame(
  item = c(
    "Input level sample", "FRM estimation sample", "Daily FRM observations",
    "Stablecoin targets", "Macro-financial factors", "Return transformation",
    "Rolling window", "Conditional quantile", "Maximum path steps",
    "Path selection", "Daily FRM aggregation", "Macro timing",
    "Strict baseline post-processing", "Separate sensitivity treatment"
  ),
  specification = c(
    "2020-01-01 to 2026-05-31",
    paste(value_of("first_estimation_date"), "to", value_of("last_estimation_date")),
    value_of("observations"),
    "11", "5", "Daily log differences", "90 calendar-day observations",
    "tau = 0.05", "25", "Minimum finite GACV on each fitted path",
    "Equal-weight mean of the 11 coin-specific selected lambdas",
    "Contemporaneous daily log changes, following the supplied code",
    "None: no clipping, winsorization, smoothing, rescaling, or path rejection",
    "A 20x path-jump diagnostic is exported separately and is not the baseline"
  ),
  stringsAsFactors = FALSE
)
write.csv(
  construction_table,
  file.path(results_dir, "Table_4_2_FRM_Construction_and_Coverage.csv"),
  row.names = FALSE,
  quote = TRUE
)

limitations <- data.frame(
  issue = c(
    "Macro weekend filling",
    "Macro source continuity",
    "Stablecoin level gaps",
    "Tied target observations",
    "Numerical lambda-path reversal",
    "sUSD naming compatibility"
  ),
  treatment_in_strict_baseline = c(
    "Next-observation carry backward on 248 weekend dates through 2022-05-15, exactly as supplied code",
    "Original QuantLet levels through 2022-05-20; documented public extensions thereafter",
    "IDRT: 3 days; sUSD/nusd: 1 day; affected non-finite log returns set to zero as supplied code",
    "Date-specific random seed makes the algorithm's tiny jitter reproducible",
    "Minimum-GACV value retained, including DAI on 2023-09-04",
    "Repository column remains nusd; manuscript label should be sUSD"
  ),
  implication = c(
    "Potential look-ahead information; disclose as a code-replication limitation",
    "USD yield and CVIX are proxies after 2022-05-20; do not describe all five extensions as Bloomberg data",
    "Four missing level observations are not interpolated",
    "Repeated runs return identical values",
    "Strict maximum is 0.105404; separately labelled numerical sensitivity removes only this index-day distortion",
    "Avoid presenting NUSD as a different sampled asset"
  ),
  stringsAsFactors = FALSE
)
write.csv(
  limitations,
  file.path(results_dir, "methodological_limitations_20260531.csv"),
  row.names = FALSE,
  quote = TRUE
)

manifest_path <- file.path(results_dir, "file_manifest_md5.csv")
files <- list.files(project_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE)
files <- files[file.info(files)$isdir %in% FALSE]
files <- files[normalizePath(files, mustWork = FALSE) != normalizePath(manifest_path, mustWork = FALSE)]
relative <- substring(files, nchar(project_dir) + 2L)
category <- sub("/.*$", "", relative)
category[!grepl("/", relative)] <- "code_or_documentation"
manifest <- data.frame(
  file = relative,
  category = category,
  bytes = as.numeric(file.info(files)$size),
  md5 = unname(tools::md5sum(files)),
  stringsAsFactors = FALSE
)
manifest <- manifest[order(manifest$file), ]
write.csv(manifest, manifest_path, row.names = FALSE, quote = FALSE)
message("Final audit tables and file manifest written to: ", results_dir)
