#!/usr/bin/env Rscript
# Validate benchmark coverage and task input factories without building or timing runners.

library(jsonlite)
source("lib/task_manifest.R")

root_dir <- normalizePath(".")
manifest <- load_task_manifest(root_dir)

args <- commandArgs(trailingOnly = TRUE)
task_filter <- NULL
for (arg in args) {
  if (grepl("^--tasks=", arg)) task_filter <- as.integer(strsplit(sub("^--tasks=", "", arg), ",")[[1]])
}

config_paths <- sort(Sys.glob(file.path(root_dir, "runners", "*.json")))
if (length(config_paths) == 0L) stop("no runner configs found in runners/")

configs <- lapply(config_paths, fromJSON, simplifyVector = FALSE)
runner_names <- sub("\\.json$", "", basename(config_paths))
names(configs) <- runner_names
active <- vapply(configs, function(cfg) is.null(cfg$status) || cfg$status != "broken", logical(1))
for (runner in runner_names[active]) validate_runner_config(manifest, configs[[runner]], runner)

runner_args <- c("runner_subprocess.R", "--runner=r", "--check-only")
if (!is.null(task_filter)) runner_args <- c(runner_args, sprintf("--tasks=%s", paste(task_filter, collapse = ",")))
status <- system2("Rscript", args = runner_args)
if (!identical(status, 0L)) stop(sprintf("task-spec preflight failed with exit code %d", status))

cat(sprintf("Runner coverage is valid for %d active runners and %d manifest tasks.\n", sum(active), nrow(manifest)))
