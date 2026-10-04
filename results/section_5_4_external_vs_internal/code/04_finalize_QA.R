#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1L])))
} else normalizePath(getwd())
source(file.path(script_dir, "00_config.R"))

required_outputs <- c(
  file.path(table_dir, "Table_5_4_Main_Results.csv"),
  file.path(table_dir, "Table_5_4_Conditional_Block_Contrasts.csv"),
  file.path(table_dir, "Table_5_4_OOS_Block_Contrasts.csv"),
  file.path(source_dir, "OOS_Block_Model_Predictions.rds"),
  file.path(source_dir, "Figure_5_4_Source_Data.csv"),
  file.path(figure_dir, "Figure_5_4_Relative_Tail_Risk_Information.svg"),
  file.path(figure_dir, "Figure_5_4_Relative_Tail_Risk_Information.pdf"),
  file.path(figure_dir, "Figure_5_4_Relative_Tail_Risk_Information.tiff"),
  file.path(figure_dir, "Figure_5_4_Relative_Tail_Risk_Information.png"),
  file.path(bundle_dir, "Section_5_4_Manuscript_Ready_EN.md")
)

qa_files <- c(
  file.path(qa_dir, "frozen_input_alignment_QA.csv"),
  file.path(qa_dir, "estimation_QA.csv"),
  file.path(qa_dir, "analysis_QA.csv"),
  file.path(qa_dir, "figure_QA.csv")
)

script_files <- list.files(code_dir, pattern = "\\.R$", full.names = TRUE)
parse_ok <- vapply(script_files, function(path) {
  isTRUE(tryCatch({ parse(path); TRUE }, error = function(e) FALSE))
}, logical(1L))

qa_status_ok <- vapply(qa_files, function(path) {
  if (!file.exists(path)) return(FALSE)
  x <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  "status" %in% names(x) && nrow(x) > 0L && all(x$status == "PASS")
}, logical(1L))

final_qa <- data.frame(
  check = c("required_outputs_exist", "required_outputs_nonempty",
            "all_R_scripts_parse", "all_component_QA_pass"),
  value = c(sum(file.exists(required_outputs)),
            sum(file.exists(required_outputs) &
                  file.info(required_outputs)$size > 0),
            sum(parse_ok), sum(qa_status_ok)),
  expected = c(length(required_outputs), length(required_outputs),
               length(script_files), length(qa_files)),
  status = c(
    if (all(file.exists(required_outputs))) "PASS" else "FAIL",
    if (all(file.exists(required_outputs)) &&
        all(file.info(required_outputs)$size > 0)) "PASS" else "FAIL",
    if (all(parse_ok)) "PASS" else "FAIL",
    if (all(qa_status_ok)) "PASS" else "FAIL"
  ),
  stringsAsFactors = FALSE
)
write_csv(final_qa, file.path(qa_dir, "final_bundle_QA.csv"))
if (any(final_qa$status == "FAIL")) stop("Final bundle QA failed.")

manifest_files <- list.files(bundle_dir, recursive = TRUE, full.names = TRUE,
                             include.dirs = FALSE)
manifest_files <- manifest_files[
  !grepl("/source_data/checkpoints/", manifest_files, fixed = TRUE) &
    !basename(manifest_files) %in% c("bundle_manifest_md5.csv",
                                    "final_bundle_QA.csv", "Rplots.pdf")
]
manifest <- data.frame(
  file = substring(manifest_files, nchar(bundle_dir) + 2L),
  size_bytes = as.numeric(file.info(manifest_files)$size),
  md5 = unname(tools::md5sum(manifest_files)),
  stringsAsFactors = FALSE
)
manifest <- manifest[order(manifest$file), ]
write_csv(manifest, file.path(qa_dir, "bundle_manifest_md5.csv"))
message("Final Section 5.4 bundle QA completed.")
