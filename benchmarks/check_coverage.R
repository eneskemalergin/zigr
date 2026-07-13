#!/usr/bin/env Rscript

library(jsonlite)
source("lib/task_manifest.R")
source("lib/evidence_schema.R")

root_dir <- normalizePath(".")
manifest <- load_task_manifest(root_dir)
evidence <- load_evidence_manifest(root_dir, manifest)

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
for (runner in runner_names[active]) {
  configs[[runner]] <- hydrate_runner_config(manifest, configs[[runner]], runner, evidence)
}

schema_test <- system2("Rscript", args = file.path("tests", "test_evidence_schema.R"))
if (!identical(schema_test, 0L)) stop(sprintf("evidence schema tests failed with exit code %d", schema_test))

runner_args <- c("runner_subprocess.R", "--runner=r", "--check-only")
if (!is.null(task_filter)) runner_args <- c(runner_args, sprintf("--tasks=%s", paste(task_filter, collapse = ",")))
status <- system2("Rscript", args = runner_args)
if (!identical(status, 0L)) stop(sprintf("task-spec preflight failed with exit code %d", status))

cat(sprintf(
  "Runner coverage is valid for %d active runners, %d manifest tasks, and %d fixture cells.\n",
  sum(active), nrow(manifest), nrow(evidence$fixture_rows)
))
