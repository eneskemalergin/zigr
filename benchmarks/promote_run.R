#!/usr/bin/env Rscript
# Promote one completed full-matrix run to the explicit canonical directory.
# Usage: Rscript promote_run.R --run-dir=results/runs/<run_id>

library(jsonlite)
source("lib/run_manifest.R")

args <- commandArgs(trailingOnly = TRUE)
run_dir <- NULL
target_dir <- file.path("results", "canonical")
for (arg in args) {
  if (grepl("^--run-dir=", arg)) run_dir <- sub("^--run-dir=", "", arg)
  if (grepl("^--target-dir=", arg)) target_dir <- sub("^--target-dir=", "", arg)
}
if (is.null(run_dir)) stop("--run-dir= is required")

run_dir <- normalizePath(run_dir, mustWork = FALSE)
target_dir <- normalizePath(target_dir, mustWork = FALSE)
if (identical(run_dir, target_dir)) stop("--target-dir must differ from --run-dir")
metadata <- read_run_manifest(run_dir)
if (!identical(as.character(metadata$status), "complete")) stop("only complete runs can be promoted")
if (!isTRUE(metadata$full_matrix)) stop("only an unfiltered full-matrix run can be promoted")
validate_run_artifacts(run_dir, metadata)
for (name in c("comparative_metrics.csv", "task_comparisons.csv")) {
  if (!file.exists(file.path(run_dir, name))) stop(sprintf("run is missing %s", name))
}

if (dir.exists(target_dir)) {
  archive_id <- paste0("canonical-", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "-pid", Sys.getpid())
  archive_dir <- file.path("results", "archive", archive_id)
  dir.create(dirname(archive_dir), recursive = TRUE, showWarnings = FALSE)
  if (!file.rename(target_dir, archive_dir)) stop(sprintf("cannot archive existing canonical directory: %s", target_dir))
}
legacy_entries <- list.files("results", all.files = TRUE, full.names = TRUE, no.. = TRUE)
legacy_entries <- legacy_entries[!basename(legacy_entries) %in% c("runs", "canonical", "archive", "README.md", "CANONICAL_RUN.json")]
if (length(legacy_entries) > 0L) {
  legacy_dir <- file.path("results", "archive", "legacy-pre-p0.4")
  dir.create(legacy_dir, recursive = TRUE, showWarnings = FALSE)
  for (entry in legacy_entries) {
    destination <- file.path(legacy_dir, basename(entry))
    if (file.exists(destination) || dir.exists(destination)) stop(sprintf("legacy archive path already exists: %s", destination))
    if (!file.rename(entry, destination)) stop(sprintf("cannot archive legacy result: %s", entry))
  }
}
dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
entries <- list.files(run_dir, all.files = TRUE, full.names = TRUE, no.. = TRUE)
for (entry in entries) {
  destination <- file.path(target_dir, basename(entry))
  if (!file.copy(entry, destination, recursive = TRUE, overwrite = TRUE)) {
    stop(sprintf("cannot copy run artifact: %s", entry))
  }
}

pointer <- list(
  schema_version = 1L,
  run_id = as.character(metadata$run_id),
  source_run_dir = run_dir,
  canonical_dir = target_dir,
  promoted_at = run_manifest_timestamp()
)
jsonlite::write_json(pointer, file.path("results", "CANONICAL_RUN.json"), auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("Promoted run %s to %s\n", metadata$run_id, target_dir))
