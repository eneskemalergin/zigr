#!/usr/bin/env Rscript

library(jsonlite)
root_dir <- normalizePath(".")
source(file.path(root_dir, "lib", "specification.R"))
source(file.path(root_dir, "lib", "measurement.R"))
source(file.path(root_dir, "lib", "provenance.R"))
source(file.path(root_dir, "lib", "run_manifest.R"))
source(file.path(root_dir, "lib", "product_fixtures.R"))

args <- commandArgs(trailingOnly = TRUE)
validate_cli_arguments(args, value_options = "run-dir", flag_options = "dry-run", label = "promotion")
run_dir <- NULL
dry_run <- FALSE
for (arg in args) {
  if (grepl("^--run-dir=", arg)) {
    run_dir <- sub("^--run-dir=", "", arg)
  } else if (identical(arg, "--dry-run")) {
    dry_run <- TRUE
  } else {
    stop(sprintf("unknown promotion argument: %s", arg))
  }
}
if (is.null(run_dir)) stop("--run-dir= is required")

repository_root <- normalizePath("..")
resolve_path <- function(path) {
  expanded <- path.expand(path)
  absolute <- grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", expanded)
  normalizePath(if (absolute) expanded else file.path(root_dir, expanded), mustWork = FALSE)
}
run_dir <- resolve_path(run_dir)
receipt_path <- file.path(root_dir, "results", "CANONICAL_RUN.json")

metadata <- read_run_manifest(run_dir)
if (!identical(as.character(metadata$status), "complete")) stop("only complete runs can be promoted")
if (!isTRUE(metadata$full_matrix) || !identical(as.character(metadata$measurement_mode), "timed")) {
  stop("only an unfiltered timed full-matrix run can be promoted")
}
manifest <- load_task_manifest(root_dir)
evidence <- load_evidence_manifest(root_dir, manifest)
expected_runners <- sort(evidence_schema_vocabulary()$runners)
if (!identical(sort(run_manifest_values(metadata$runners)), expected_runners) ||
    !identical(sort(run_manifest_values(metadata$tasks)), sort(as.character(manifest$task)))) {
  stop("promotion requires the current seven-runner and complete task matrix")
}
if (is.null(metadata$boundary_budget_policy_version) ||
    !identical(as.character(metadata$boundary_budget_policy_version), boundary_budget_policy_version())) {
  stop("run boundary budget policy version differs from the current policy; collect a fresh run")
}
if (!identical(
  run_manifest_object_digest(metadata$timing_policy),
  run_manifest_object_digest(benchmark_timing_policy())
)) {
  stop("run timing policy differs from the current policy; collect a fresh run")
}

validate_run_completion_contract(metadata)
validate_run_completion_artifacts(run_dir, metadata)
validate_run_disposition_identity(
  metadata,
  run_disposition_records(evidence, expected_runners, as.character(manifest$task))
)
validate_source_tree_identity(repository_root, metadata$environment$source_tree)
validate_run_artifacts(run_dir, metadata)
correctness_identity <- validate_correctness_artifacts(run_dir, metadata, evidence)
validate_fixture_measurement_artifacts(run_dir, metadata, evidence)
invisible(verify_fixture_source_paths(root_dir, evidence))

stage_dir <- tempfile("zigr-benchmark-promotion-")
dir.create(stage_dir, recursive = TRUE)
on.exit(unlink(stage_dir, recursive = TRUE), add = TRUE)
sealed_paths <- vapply(metadata$completion_artifacts$files, function(record) as.character(record$path), character(1))
copy_run_artifact_set(run_dir, stage_dir, c("run_manifest.json", sealed_paths))
staged_metadata <- read_run_manifest(stage_dir)
validate_run_completion_contract(staged_metadata)
validate_run_completion_artifacts(stage_dir, staged_metadata)
invisible(validate_run_artifacts(stage_dir, staged_metadata))
invisible(validate_correctness_artifacts(stage_dir, staged_metadata, evidence))
invisible(validate_fixture_measurement_artifacts(stage_dir, staged_metadata, evidence))

run_exporter <- function(script, arguments = character(0)) {
  output <- system2(
    "Rscript",
    c(script, paste0("--run-dir=", stage_dir), arguments),
    stdout = TRUE,
    stderr = TRUE,
    env = c("OPENBLAS_NUM_THREADS=1", "OMP_NUM_THREADS=1", "MKL_NUM_THREADS=1")
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop(sprintf("%s failed with exit code %d: %s", script, status, paste(output, collapse = "\n")))
  }
  invisible(output)
}

run_exporter("export_comparative_metrics.R")
run_exporter("export_boundary_metrics.R")

report_files <- separated_report_files()
required_reports <- c(
  unname(report_files), "report_manifest.json", "boundary_metrics.csv", "boundary_budgets.csv",
  "representation_budgets.csv"
)
missing_reports <- required_reports[!file.exists(file.path(stage_dir, required_reports))]
if (length(missing_reports) > 0L) {
  stop(sprintf("promotion report regeneration is incomplete: %s", paste(missing_reports, collapse = ", ")))
}

report_manifest <- jsonlite::fromJSON(file.path(stage_dir, "report_manifest.json"), simplifyVector = FALSE)
if (!identical(as.character(report_manifest$schema_version), "separated-report-v2") ||
    !identical(as.character(report_manifest$run_id), as.character(metadata$run_id)) ||
    !identical(as.character(report_manifest$recorded_source_tree_digest), as.character(metadata$environment$source_tree$digest)) ||
    !identical(as.character(report_manifest$task_correctness_artifact_digest), as.character(correctness_identity$task_artifact_digest)) ||
    !identical(as.character(report_manifest$fixture_correctness_artifact_digest), as.character(correctness_identity$fixture_artifact_digest))) {
  stop("regenerated report manifest identity differs from the completed run")
}
for (record in report_manifest$report_code_sources) {
  path <- as.character(record$path)
  actual <- unname(as.character(tools::md5sum(file.path(root_dir, path))[[1L]]))
  if (!identical(actual, as.character(record$md5))) {
    stop(sprintf("regenerated report code identity differs for %s", path))
  }
}
for (record in report_manifest$reports) {
  path <- file.path(stage_dir, as.character(record$file))
  actual <- unname(as.character(tools::md5sum(path)[[1L]]))
  if (!identical(actual, as.character(record$md5))) {
    stop(sprintf("regenerated report digest differs for %s", basename(path)))
  }
}

product <- read.csv(file.path(stage_dir, report_files[["product"]]), stringsAsFactors = FALSE)
strategy <- read.csv(file.path(stage_dir, report_files[["strategy"]]), stringsAsFactors = FALSE)
r_baseline <- read.csv(file.path(stage_dir, report_files[["r_baseline"]]), stringsAsFactors = FALSE)
control <- read.csv(file.path(stage_dir, report_files[["control"]]), stringsAsFactors = FALSE)
diagnostic <- read.csv(file.path(stage_dir, report_files[["diagnostic"]]), stringsAsFactors = FALSE)
capability <- read.csv(file.path(stage_dir, report_files[["capability"]]), stringsAsFactors = FALSE)
safety <- read.csv(file.path(stage_dir, report_files[["safety"]]), stringsAsFactors = FALSE)
validate_product_metrics(product)
validate_strategy_metrics(strategy)
validate_r_baseline_metrics(r_baseline, as.character(manifest$task), c(evidence$fixtures, "F03_optimized_base_r", "F04_optimized_base_r"))
validate_control_metrics(control)
validate_diagnostic_metrics(
  diagnostic,
  unlist(lapply(expected_runners, function(runner) paste(runner, manifest$task, sep = "\r")), use.names = FALSE)
)
validate_capability_matrix(capability, paste(evidence$fixture_rows$runner, evidence$fixture_rows$fixture, sep = "\r"))
validate_safety_results(safety, expected_runners)

boundary_budgets <- read.csv(file.path(stage_dir, "boundary_budgets.csv"), stringsAsFactors = FALSE)
representation_budgets <- read.csv(file.path(stage_dir, "representation_budgets.csv"), stringsAsFactors = FALSE)
boundary_metrics <- read.csv(file.path(stage_dir, "boundary_metrics.csv"), stringsAsFactors = FALSE)
if (any(!grepl("^PASS", boundary_budgets$status)) || any(representation_budgets$status != "PASS") ||
    any(as.character(boundary_budgets$run_id) != as.character(metadata$run_id)) ||
    any(as.character(representation_budgets$run_id) != as.character(metadata$run_id))) {
  stop("regenerated boundary or representation report does not pass")
}
r_reference_gap <- boundary_metrics$r_reference_status != "PASS"
if (any(!(boundary_metrics$r_reference_status %in% c("PASS", "N/A"))) ||
    any(r_reference_gap & !nzchar(as.character(boundary_metrics$r_reference_reason))) ||
    any(r_reference_gap & !is.na(boundary_metrics$r_reference_median_ms)) ||
    any(!r_reference_gap & is.na(boundary_metrics$r_reference_median_ms))) {
  stop("regenerated boundary report hides or misstates an R reference gap")
}
analysis <- read.csv(file.path(stage_dir, "analysis_summary.csv"), stringsAsFactors = FALSE)
expected_analysis_rows <- length(expected_runners) * nrow(manifest)
if (nrow(analysis) != expected_analysis_rows ||
    !"run_id" %in% names(analysis) ||
    any(as.character(analysis$run_id) != as.character(metadata$run_id))) {
  stop("regenerated analysis report coverage or run identity differs")
}

task_correctness_files <- run_correctness_artifact_paths(stage_dir, staged_metadata, "task", expected_runners)
fixture_correctness_files <- run_correctness_artifact_paths(stage_dir, staged_metadata, "fixture", expected_runners)
task_correctness_rows <- sum(vapply(task_correctness_files, function(path) nrow(read.csv(path)), integer(1)))
fixture_correctness_rows <- sum(vapply(fixture_correctness_files, function(path) nrow(read.csv(path)), integer(1)))
derived_report_record <- function(role, file) list(
  role = role,
  file = file,
  rows = nrow(read.csv(file.path(stage_dir, file), stringsAsFactors = FALSE)),
  md5 = unname(as.character(tools::md5sum(file.path(stage_dir, file))[[1L]]))
)
derived_reports <- c(
  report_manifest$reports,
  list(
    derived_report_record("boundary", "boundary_metrics.csv"),
    derived_report_record("boundary_budget", "boundary_budgets.csv"),
    derived_report_record("representation_budget", "representation_budgets.csv")
  )
)
receipt <- list(
  schema_version = "benchmark-promotion-v2",
  run_id = as.character(metadata$run_id),
  promoted_at = run_manifest_timestamp(),
  source_tree_digest = as.character(metadata$environment$source_tree$digest),
  source_ledger_identity_digest = as.character(metadata$environment$tool_source_ledger$identity_digest),
  completion_contract_digest = as.character(metadata$completion_contract$digest),
  completion_artifact_digest = as.character(metadata$completion_artifacts$digest),
  report_manifest_md5 = unname(as.character(tools::md5sum(file.path(stage_dir, "report_manifest.json"))[[1L]])),
  report_code_identity_digest = run_manifest_object_digest(report_manifest$report_code_sources),
  correctness = list(
    task_rows = task_correctness_rows,
    fixture_rows = fixture_correctness_rows,
    task_artifact_digest = as.character(correctness_identity$task_artifact_digest),
    fixture_artifact_digest = as.character(correctness_identity$fixture_artifact_digest)
  ),
  coverage = list(
    runners = length(expected_runners),
    tasks = nrow(manifest),
    task_dispositions = length(expected_runners) * nrow(manifest),
    fixture_dispositions = nrow(evidence$fixture_rows),
    sealed_artifacts = length(metadata$completion_artifacts$files),
    analysis_rows = nrow(analysis),
    boundary_rows = nrow(boundary_metrics)
  ),
  acceptance = list(
    boundary_budget_rows = nrow(boundary_budgets),
    boundary_budgets_pass = all(grepl("^PASS", boundary_budgets$status)),
    representation_budget_rows = nrow(representation_budgets),
    representation_budgets_pass = all(representation_budgets$status == "PASS"),
    lifecycle_rows = nrow(safety),
    lifecycle_pass = all(safety$proof_status == "PASS")
  ),
  reports = derived_reports
)
receipt <- seal_run_promotion_receipt(receipt)
validate_run_promotion_receipt(receipt)

receipt_relative_path <- run_manifest_relative_artifact_path("benchmarks", "results", "CANONICAL_RUN.json")
ignore_status <- system2(
  "git",
  c("-C", repository_root, "check-ignore", "--quiet", "--no-index", receipt_relative_path)
)
if (identical(ignore_status, 0L)) stop("compact promotion receipt is ignored by Git")
if (!(ignore_status %in% c(1L))) stop("could not verify compact promotion receipt tracking policy")

if (dry_run) {
  cat(sprintf(
    "Promotion dry run passed for %s: %d sealed inputs, 11 regenerated reports, and one compact receipt.\n",
    metadata$run_id,
    length(metadata$completion_artifacts$files)
  ))
  quit(save = "no", status = 0L, runLast = FALSE)
}

write_run_manifest_json_atomic(receipt, receipt_path)
cat(sprintf("Promoted run %s with compact receipt %s\n", metadata$run_id, receipt_path))
