#!/usr/bin/env Rscript

library(jsonlite)
source("lib/task_manifest.R")
source("lib/run_manifest.R")
source("lib/environment_manifest.R")

args <- commandArgs(trailingOnly = TRUE)
runners_filter <- NULL
tasks_filter   <- NULL
do_build       <- FALSE
run_dir_arg    <- NULL
for (a in args) {
  if (grepl("^--runners=", a)) runners_filter <- strsplit(sub("^--runners=", "", a), ",")[[1]]
  if (grepl("^--tasks=", a))  tasks_filter  <- as.integer(strsplit(sub("^--tasks=", "", a), ",")[[1]])
  if (a == "--build")         do_build      <- TRUE
  if (grepl("^--run-dir=", a)) run_dir_arg <- sub("^--run-dir=", "", a)
}

root_dir <- normalizePath(".")
manifest <- load_task_manifest(root_dir)
runner_files <- Sys.glob("runners/*.json")
if (length(runner_files) == 0) stop("no runner configs found in runners/")

all_runners <- list()
for (f in runner_files) {
  cfg <- fromJSON(f, simplifyVector = FALSE)
  if (!is.null(cfg$status) && cfg$status == "broken") next
  all_runners[[cfg$name]] <- cfg
}

if (!is.null(runners_filter)) {
  all_runners <- all_runners[intersect(names(all_runners), runners_filter)]
}
if (length(all_runners) == 0L) stop("no active runners selected")

optional_tasks <- unique(unlist(lapply(all_runners, function(cfg) {
  if (is.null(cfg$optional_tasks)) character(0) else as.character(unlist(cfg$optional_tasks, use.names = FALSE))
}), use.names = FALSE))

task_numbers <- as.integer(sub("([0-9]+).*", "\\1", manifest$task))
selected_tasks <- manifest$task
if (!is.null(tasks_filter)) {
  selected_tasks <- manifest$task[task_numbers %in% tasks_filter]
  if (length(selected_tasks) == 0L) stop("task filter selected no manifest tasks")
}

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
  schema_version = 1L,
  run_id = run_id,
  status = "running",
  started_at = run_manifest_timestamp(),
  runners = sort(names(all_runners)),
  tasks = selected_tasks,
  allowed_na_tasks = sort(intersect(selected_tasks, optional_tasks)),
  timing_policy = benchmark_timing_policy(),
  boundary_budget_policy_version = boundary_budget_policy_version(),
  full_matrix = is.null(runners_filter) && is.null(tasks_filter),
  command = commandArgs()
)
blas_env <- c("OPENBLAS_NUM_THREADS=1")
write_run_manifest(run_dir, run_metadata)
run_complete <- FALSE
run_error <- NULL
on.exit({
  if (!run_complete) {
    message <- if (is.null(run_error)) geterrmessage() else run_error
    try(update_run_manifest(run_dir, "incomplete", message), silent = TRUE)
  }
}, add = TRUE)

cat(sprintf("Runners: %s\n\n", paste(names(all_runners), collapse = ", ")))
cat(sprintf("Run: %s\n\n", run_id))
runner_failures <- character(0)

if (do_build) {
  cat("Building runners\n")
  code <- system("bash build_all.sh", ignore.stdout = FALSE, ignore.stderr = FALSE)
  if (code != 0) stop(sprintf("runner build failed with exit code %d", code))
  cat("\n")
}

build_settings <- list(
  optimization = Sys.getenv("ZIGR_OPTIMIZE", unset = "ReleaseFast"),
  target = Sys.getenv("ZIGR_TARGET", unset = "native"),
  cpu_features = Sys.getenv("ZIGR_CPU_FEATURES", unset = "default"),
  command = if (do_build) "bash build_all.sh" else "prebuilt runner libraries",
  requested_rebuild = do_build
)
run_metadata$environment <- capture_environment_manifest(
  root_dir,
  all_runners,
  blas_env,
  build_settings,
  source_root = normalizePath("..")
)
validate_environment_manifest(run_metadata$environment)
write_run_manifest(run_dir, run_metadata)

coverage_args <- c("check_coverage.R")
if (!is.null(tasks_filter)) coverage_args <- c(coverage_args, sprintf("--tasks=%s", paste(tasks_filter, collapse = ",")))
coverage_code <- system2("Rscript", args = coverage_args, env = blas_env, stdout = "", stderr = "")
if (!identical(coverage_code, 0L)) stop(sprintf("coverage preflight failed with exit code %d", coverage_code))

for (rn in names(all_runners)) {
  cfg <- all_runners[[rn]]
  cat(sprintf("Runner: %s (%s)\n", rn, cfg$label))

  runner_args <- c("runner_subprocess.R", sprintf("--runner=%s", rn), sprintf("--results-dir=%s", run_dir))
  if (!is.null(tasks_filter)) {
    runner_args <- c(runner_args, sprintf("--tasks=%s", paste(tasks_filter, collapse = ",")))
  }

  code <- system2("Rscript", args = runner_args, env = blas_env, stdout = "", stderr = "")
  if (code != 0) cat(sprintf("  [SUB] exited with code %d\n", code))
  if (code != 0) runner_failures <- c(runner_failures, sprintf("%s:%d", rn, code))
  cat("\n")
}

if (length(runner_failures) > 0L) {
  run_error <- sprintf("runner subprocesses failed: %s", paste(runner_failures, collapse = ", "))
  stop(run_error)
}

validate_run_artifacts(run_dir, run_metadata)
update_run_manifest(run_dir, "complete")

if (is.null(runners_filter) && is.null(tasks_filter)) {
  cat("Comparative metrics\n")
  code <- system2("Rscript", args = c("export_comparative_metrics.R", sprintf("--run-dir=%s", run_dir)),
                  stdout = "", stderr = "")
  if (code != 0) {
    run_error <- sprintf("comparative metrics export failed with exit code %d", code)
    stop(run_error)
  }
  code <- system2("Rscript", args = c("export_boundary_metrics.R", sprintf("--run-dir=%s", run_dir)),
                  stdout = "", stderr = "")
  if (code != 0) {
    run_error <- sprintf("boundary metrics export failed with exit code %d", code)
    stop(run_error)
  }
  cat("\n")
} else {
  cat("Skipping comparative export for filtered benchmark runs.\n\n")
}

run_complete <- TRUE
cat("Done.\n")
