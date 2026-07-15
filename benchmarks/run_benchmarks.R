#!/usr/bin/env Rscript

library(jsonlite)
source("lib/task_manifest.R")
source("lib/evidence_schema.R")
source("lib/run_manifest.R")
source("lib/source_ledger.R")
source("lib/environment_manifest.R")
source("lib/input_contract.R")
source("lib/r_provenance.R")
source("lib/product_fixtures.R")
source("lib/fixture_measurement.R")

args <- commandArgs(trailingOnly = TRUE)
runners_filter <- NULL
tasks_filter   <- NULL
do_build       <- FALSE
correctness_only <- FALSE
run_dir_arg    <- NULL
master_seed    <- benchmark_master_seed()
for (a in args) {
  if (grepl("^--runners=", a)) runners_filter <- strsplit(sub("^--runners=", "", a), ",")[[1]]
  if (grepl("^--tasks=", a))  tasks_filter  <- as.integer(strsplit(sub("^--tasks=", "", a), ",")[[1]])
  if (a == "--build")         do_build      <- TRUE
  if (a == "--correctness-only") correctness_only <- TRUE
  if (grepl("^--run-dir=", a)) run_dir_arg <- sub("^--run-dir=", "", a)
  if (grepl("^--seed=", a)) master_seed <- input_scalar_integer(sub("^--seed=", "", a), "master seed")
}

root_dir <- normalizePath(".")
manifest <- load_task_manifest(root_dir)
evidence <- load_evidence_manifest(root_dir, manifest)
runner_files <- Sys.glob("runners/*.json")
if (length(runner_files) == 0) stop("no runner configs found in runners/")

all_runners <- list()
for (f in runner_files) {
  cfg <- fromJSON(f, simplifyVector = FALSE)
  if (!is.null(cfg$status) && cfg$status == "broken") next
  cfg <- hydrate_runner_config(manifest, cfg, cfg$name, evidence)
  all_runners[[cfg$name]] <- cfg
}

if (!is.null(runners_filter)) {
  all_runners <- all_runners[intersect(names(all_runners), runners_filter)]
}
if (length(all_runners) == 0L) stop("no active runners selected")

task_numbers <- as.integer(sub("([0-9]+).*", "\\1", manifest$task))
selected_tasks <- manifest$task
if (!is.null(tasks_filter)) {
  selected_tasks <- manifest$task[task_numbers %in% tasks_filter]
  if (length(selected_tasks) == 0L) stop("task filter selected no manifest tasks")
}

source(file.path(root_dir, "src", "r", "run_all.R"))
r_config <- fromJSON(file.path(root_dir, "runners", "r.json"), simplifyVector = FALSE)
r_runner_map <- r_config$exports
r_reference_exports <- r_reference_map(r_config)
r_evidence_rows <- evidence$tasks[evidence$tasks$runner == "r", , drop = FALSE]
r_provenance <- build_run_r_provenance(
  selected_tasks, r_runner_map, r_reference_exports, manifest, r_evidence_rows
)

coverage_args <- c("check_coverage.R")
if (!is.null(tasks_filter)) coverage_args <- c(coverage_args, sprintf("--tasks=%s", paste(tasks_filter, collapse = ",")))
blas_env <- c("OPENBLAS_NUM_THREADS=1")
coverage_code <- system2("Rscript", args = coverage_args, env = blas_env, stdout = "", stderr = "")
if (!identical(coverage_code, 0L)) stop(sprintf("coverage preflight failed with exit code %d", coverage_code))

run_dir <- if (is.null(run_dir_arg)) {
  run_id <- paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "-pid", Sys.getpid())
  file.path(root_dir, "results", "runs", run_id)
} else {
  normalizePath(run_dir_arg, mustWork = FALSE)
}
run_id <- basename(run_dir)
if (file.exists(run_manifest_path(run_dir))) stop(sprintf("run directory already exists: %s", run_dir))
existing_entries <- if (dir.exists(run_dir)) list.files(run_dir, all.files = TRUE, no.. = TRUE) else character(0)
if (length(existing_entries) > 0L) stop(sprintf("run directory is not empty: %s", run_dir))

project_runs_root <- normalizePath(file.path(root_dir, "results", "runs"), mustWork = FALSE)
if (identical(dirname(run_dir), project_runs_root)) {
  stale_runs <- reconcile_running_runs(
    file.path(root_dir, "results"),
    replacement_run_id = run_id
  )
  if (length(stale_runs) > 0L) {
    cat(sprintf("Reconciled stale runs: %s\n\n", paste(stale_runs, collapse = ", ")))
  }
}

run_metadata <- list(
  schema_version = 2L,
  run_id = run_id,
  status = "running",
  started_at = run_manifest_timestamp(),
  runners = sort(names(all_runners)),
  tasks = selected_tasks,
  master_seed = master_seed,
  input_manifest = list(relative_path = "input_manifest.json", digest = "pending"),
  runner_dispositions = run_disposition_records(evidence, sort(names(all_runners)), selected_tasks),
  r_provenance = r_provenance,
  timing_policy = benchmark_timing_policy(),
  boundary_budget_policy_version = boundary_budget_policy_version(),
  full_matrix = is.null(runners_filter) && is.null(tasks_filter),
  measurement_mode = if (correctness_only) "correctness_only" else "timed",
  command = commandArgs()
)
write_run_manifest(run_dir, run_metadata)
run_complete <- FALSE
run_error <- NULL
previous_error_handler <- getOption("error")
mark_run_incomplete <- function() {
  if (!run_complete) {
    message <- if (is.null(run_error)) geterrmessage() else run_error
    try(update_run_manifest(run_dir, "incomplete", message), silent = TRUE)
  }
}
options(error = function() {
  mark_run_incomplete()
  options(error = previous_error_handler)
  if (is.function(previous_error_handler)) previous_error_handler()
  quit(save = "no", status = 1L, runLast = FALSE)
})

input_manifest_path <- file.path(run_dir, run_metadata$input_manifest$relative_path)
prepare_args <- c(
  "runner_subprocess.R",
  sprintf("--prepare-inputs=%s", input_manifest_path),
  sprintf("--master-seed=%d", master_seed)
)
if (!is.null(tasks_filter)) {
  prepare_args <- c(prepare_args, sprintf("--tasks=%s", paste(tasks_filter, collapse = ",")))
}
prepare_code <- system2("Rscript", args = prepare_args, env = blas_env, stdout = "", stderr = "")
if (!identical(prepare_code, 0L)) stop(sprintf("canonical input preparation failed with exit code %d", prepare_code))
run_metadata$input_manifest$digest <- unname(as.character(tools::md5sum(input_manifest_path))[[1L]])
prepared_inputs <- read_input_recipe_manifest(input_manifest_path)
if (!identical(sort(names(prepared_inputs$tasks)), sort(as.character(selected_tasks)))) {
  stop("canonical input preparation produced the wrong task set")
}
run_metadata$task_inputs <- unname(prepared_inputs$tasks)
write_run_manifest(run_dir, run_metadata)

cat(sprintf("Runners: %s\n\n", paste(names(all_runners), collapse = ", ")))
cat(sprintf("Run: %s\n\n", run_id))
runner_failures <- character(0)

if (do_build) {
  cat("Building runners\n")
  code <- system("bash build_all.sh", ignore.stdout = FALSE, ignore.stderr = FALSE)
  if (code != 0) stop(sprintf("runner build failed with exit code %d", code))
  cat("\n")
}

checked_sexp_value <- tolower(Sys.getenv("ZIGR_CHECKED_SEXP", unset = "false"))
if (!(checked_sexp_value %in% c("1", "true", "yes", "on", "0", "false", "no", "off", ""))) {
  stop("ZIGR_CHECKED_SEXP must be a boolean value")
}

build_settings <- list(
  optimization = Sys.getenv("ZIGR_OPTIMIZE", unset = "ReleaseFast"),
  target = Sys.getenv("ZIGR_TARGET", unset = "native"),
  cpu_features = Sys.getenv("ZIGR_CPU_FEATURES", unset = "default"),
  checked_sexp = checked_sexp_value %in% c("1", "true", "yes", "on"),
  cache_dir = normalizePath(
    Sys.getenv("ZIG_CACHE_DIR", unset = file.path(root_dir, ".zig-cache")),
    mustWork = FALSE
  ),
  global_cache_dir = normalizePath(
    Sys.getenv("ZIG_GLOBAL_CACHE_DIR", unset = file.path(root_dir, ".zig-global-cache")),
    mustWork = FALSE
  ),
  command = if (do_build) "bash build_all.sh" else "prebuilt runner libraries",
  requested_rebuild = do_build
)
run_metadata$environment <- capture_environment_manifest(
  root_dir,
  all_runners,
  blas_env,
  build_settings,
  evidence,
  r_provenance,
  source_root = normalizePath("..")
)
validate_environment_manifest(run_metadata$environment)
write_run_manifest(run_dir, run_metadata)

runner_process_args <- function(runner_name, validation_only = FALSE, validation_output = NULL) {
  runner_args <- c(
    "runner_subprocess.R",
    sprintf("--runner=%s", runner_name),
    sprintf("--results-dir=%s", run_dir),
    sprintf("--input-manifest=%s", input_manifest_path),
    sprintf("--expected-input-manifest-digest=%s", run_metadata$input_manifest$digest),
    sprintf("--master-seed=%d", master_seed)
  )
  if (validation_only) {
    runner_args <- c(runner_args, "--validation-only", sprintf("--validation-output=%s", validation_output))
  }
  if (!is.null(tasks_filter)) {
    runner_args <- c(runner_args, sprintf("--tasks=%s", paste(tasks_filter, collapse = ",")))
  }
  runner_args
}

fixture_process_args <- function(runner_name, validation_only = FALSE, validation_output = NULL) {
  fixture_args <- c(
    "fixture_subprocess.R",
    sprintf("--runner=%s", runner_name),
    sprintf("--run-dir=%s", run_dir)
  )
  if (validation_only) {
    fixture_args <- c(fixture_args, "--validation-only", sprintf("--validation-output=%s", validation_output))
  }
  fixture_args
}

cat("Trust and correctness preflight\n")
correctness_root <- file.path(run_dir, "correctness")
task_correctness_root <- file.path(correctness_root, "tasks")
fixture_correctness_root <- file.path(correctness_root, "fixtures")
for (rn in names(all_runners)) {
  code <- system2(
    "Rscript",
    args = fixture_process_args(
      rn, validation_only = TRUE, validation_output = file.path(fixture_correctness_root, paste0(rn, ".csv"))
    ),
    env = blas_env, stdout = "", stderr = ""
  )
  if (code != 0L) {
    run_error <- sprintf("fixture validation preflight failed for %s with exit code %d", rn, code)
    stop(run_error)
  }
}
for (rn in names(all_runners)) {
  code <- system2(
    "Rscript",
    args = runner_process_args(
      rn, validation_only = TRUE, validation_output = file.path(task_correctness_root, paste0(rn, ".csv"))
    ),
    env = blas_env, stdout = "", stderr = ""
  )
  if (code != 0L) {
    run_error <- sprintf("runner validation preflight failed for %s with exit code %d", rn, code)
    stop(run_error)
  }
}
cat("\n")

correctness_evidence <- validate_correctness_artifacts(run_dir, run_metadata, evidence)
validate_source_tree_identity(normalizePath(".."), run_metadata$environment$source_tree)

run_metadata$correctness_stage <- c(list(
  completed_at = run_manifest_timestamp(),
  fixture_runners = sort(names(all_runners)),
  task_runners = sort(names(all_runners)),
  tasks = as.list(selected_tasks),
  source_tree_digest = as.character(run_metadata$environment$source_tree$digest),
  source_ledger_identity_digest = as.character(run_metadata$environment$tool_source_ledger$identity_digest)
), correctness_evidence)
write_run_manifest(run_dir, run_metadata)

if (correctness_only) {
  validate_source_tree_identity(normalizePath(".."), run_metadata$environment$source_tree)
  run_metadata$status <- "correctness_complete"
  write_run_manifest(run_dir, run_metadata)
  run_complete <- TRUE
  options(error = previous_error_handler)
  cat("Correctness-only stage completed without timing artifacts.\n")
  quit(save = "no", status = 0L, runLast = FALSE)
}

cat("Normalized fixture measurement\n")
for (rn in names(all_runners)) {
  code <- system2("Rscript", args = fixture_process_args(rn), env = blas_env, stdout = "", stderr = "")
  if (code != 0L) {
    runner_failures <- c(runner_failures, sprintf("fixture-%s:%d", rn, code))
  }
}
cat("\n")
if (length(runner_failures) > 0L) {
  run_error <- sprintf("fixture measurement subprocesses failed: %s", paste(runner_failures, collapse = ", "))
  stop(run_error)
}

for (rn in names(all_runners)) {
  cfg <- all_runners[[rn]]
  cat(sprintf("Runner: %s (%s)\n", rn, cfg$label))

  runner_args <- runner_process_args(rn)

  code <- system2("Rscript", args = runner_args, env = blas_env, stdout = "", stderr = "")
  if (code != 0) cat(sprintf("  [SUB] exited with code %d\n", code))
  if (code != 0) runner_failures <- c(runner_failures, sprintf("%s:%d", rn, code))
  cat("\n")
}

if (length(runner_failures) > 0L) {
  run_error <- sprintf("runner subprocesses failed: %s", paste(runner_failures, collapse = ", "))
  stop(run_error)
}

validate_source_tree_identity(normalizePath(".."), run_metadata$environment$source_tree)
validate_run_artifacts(run_dir, run_metadata)
validate_fixture_measurement_artifacts(run_dir, run_metadata, evidence)
validate_source_tree_identity(normalizePath(".."), run_metadata$environment$source_tree)
run_metadata$completion_artifacts <- capture_run_completion_artifacts(run_dir, run_metadata)
run_metadata$status <- "complete"
run_metadata$finished_at <- run_manifest_timestamp()
run_metadata$status_message <- NULL
run_metadata$completion_contract <- capture_run_completion_contract(run_metadata)
write_run_manifest(run_dir, run_metadata)
validate_run_completion_contract(run_metadata)
validate_run_completion_artifacts(run_dir, run_metadata)
run_complete <- TRUE
options(error = previous_error_handler)
cat("Report generation is deferred to the separate P4.7 stage.\n")
cat("Done.\n")
