args <- commandArgs(trailingOnly = FALSE)
script_arg <- args[grep("^--file=", args)]
script_dir <- if (length(script_arg)) dirname(normalizePath(sub("^--file=", "", script_arg[1]), mustWork = TRUE)) else normalizePath(getwd(), mustWork = TRUE)
project_library <- normalizePath(file.path(script_dir, "R-library"), mustWork = FALSE)
dir.create(project_library, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(project_library, .libPaths()))

required <- c(
  "data.table", "dplyr", "ggplot2", "igraph", "jsonlite", "lubridate",
  "patchwork", "quadprog", "ragg", "readr", "reshape2", "sandwich",
  "scales", "stringr", "svglite", "tidyr", "timeDate", "zoo"
)

available <- rownames(installed.packages())
missing <- setdiff(required, available)

if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org", lib = project_library)
} else {
  message("All declared R packages are already installed.")
}

message("Project R library: ", project_library)
