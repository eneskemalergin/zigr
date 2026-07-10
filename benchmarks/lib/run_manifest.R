run_manifest_path <- function(run_dir) file.path(run_dir, "run_manifest.json")

run_manifest_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

run_manifest_values <- function(value) {
  if (is.null(value)) character(0) else as.character(unlist(value, use.names = FALSE))
}

benchmark_timing_policy <- function() {
  list(
    warmup_iterations = 10L,
    block_size = 10L,
    max_iterations = 500L,
    convergence_window_blocks = 5L,
    convergence_cv_threshold_pct = 1.0,
    timer_noise_floor_ms = 0.01,
    timer_noise_floor_method = "fixed 0.01 ms floor rounded above empty-eval p99 calibration",
    low_noise_cv_threshold_pct = 20.0,
    meaningful_margin_ratio = 1.05,
    median_ci_level = 0.95,
    median_ci_method = "exact order-statistic interval",
    rss_metric = "post_gc_endpoint_delta_kb",
    gc_policy = "full before cold/warmup and both RSS endpoints; no forced GC between timed samples"
  )
}

validate_timing_policy <- function(policy) {
  if (is.null(policy)) stop("run manifest has no timing policy")
  required <- c(
    "warmup_iterations", "block_size", "max_iterations", "convergence_window_blocks",
    "convergence_cv_threshold_pct", "timer_noise_floor_ms", "timer_noise_floor_method",
    "low_noise_cv_threshold_pct",
    "meaningful_margin_ratio", "median_ci_level", "median_ci_method", "rss_metric", "gc_policy"
  )
  missing <- required[vapply(required, function(name) is.null(policy[[name]]), logical(1))]
  if (length(missing) > 0L) stop(sprintf("timing policy missing fields: %s", paste(missing, collapse = ", ")))
  integer_fields <- c("warmup_iterations", "block_size", "max_iterations", "convergence_window_blocks")
  for (name in integer_fields) {
    value <- as.numeric(policy[[name]])
    if (length(value) != 1L || is.na(value) || value < 1 || value != as.integer(value)) {
      stop(sprintf("timing policy has invalid %s", name))
    }
  }
  numeric_fields <- c(
    "convergence_cv_threshold_pct", "timer_noise_floor_ms", "low_noise_cv_threshold_pct",
    "meaningful_margin_ratio", "median_ci_level"
  )
  for (name in numeric_fields) {
    value <- as.numeric(policy[[name]])
    if (length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0) {
      stop(sprintf("timing policy has invalid %s", name))
    }
  }
  if (as.numeric(policy$median_ci_level) >= 1) stop("timing policy median CI level must be below 1")
  for (name in c("timer_noise_floor_method", "median_ci_method", "rss_metric", "gc_policy")) {
    if (length(policy[[name]]) != 1L || !nzchar(as.character(policy[[name]]))) {
      stop(sprintf("timing policy has invalid %s", name))
    }
  }
  invisible(policy)
}

write_run_manifest <- function(run_dir, metadata) {
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  path <- run_manifest_path(run_dir)
  temporary <- tempfile("run_manifest-", tmpdir = run_dir)
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(metadata, temporary, auto_unbox = TRUE, pretty = TRUE)
  if (file.exists(path) && !file.remove(path)) stop(sprintf("cannot replace run manifest: %s", path))
  if (!file.rename(temporary, path)) stop(sprintf("cannot install run manifest: %s", path))
  invisible(path)
}

read_run_manifest <- function(run_dir) {
  path <- run_manifest_path(run_dir)
  if (!file.exists(path)) stop(sprintf("run manifest not found: %s", path))
  metadata <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (is.null(metadata$run_id) || !nzchar(as.character(metadata$run_id))) {
    stop(sprintf("run manifest has no run_id: %s", path))
  }
  metadata
}

update_run_manifest <- function(run_dir, status, message = NULL) {
  metadata <- read_run_manifest(run_dir)
  metadata$status <- status
  if (status %in% c("complete", "incomplete")) metadata$finished_at <- run_manifest_timestamp()
  if (is.null(message) || !nzchar(message)) {
    metadata$status_message <- NULL
  } else {
    metadata$status_message <- message
  }
  write_run_manifest(run_dir, metadata)
}

reconcile_running_runs <- function(results_root, replacement_run_id, stale_after_seconds = 6 * 60 * 60) {
  runs_root <- file.path(results_root, "runs")
  if (!dir.exists(runs_root)) return(character(0))

  run_dirs <- list.dirs(runs_root, full.names = TRUE, recursive = FALSE)
  reconciled <- character(0)
  for (run_dir in run_dirs) {
    manifest_path <- run_manifest_path(run_dir)
    if (!file.exists(manifest_path)) next
    metadata <- tryCatch(read_run_manifest(run_dir), error = function(e) NULL)
    if (is.null(metadata) || !identical(as.character(metadata$status), "running")) next

    started_at <- if (is.null(metadata$started_at)) "" else as.character(metadata$started_at)
    started_at <- sub("Z$", "", started_at)
    started <- suppressWarnings(as.POSIXct(started_at, format = "%Y-%m-%dT%H:%OS", tz = "UTC"))
    age_seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (is.na(age_seconds) || age_seconds < stale_after_seconds) next

    update_run_manifest(
      run_dir,
      "incomplete",
      sprintf("stale running run superseded by %s; artifacts retained", replacement_run_id)
    )
    reconciled <- c(reconciled, basename(run_dir))
  }
  reconciled
}

validate_environment_manifest <- function(environment) {
  if (is.null(environment)) stop("run manifest has no environment metadata")
  require_scalar <- function(container, name, label) {
    value <- if (is.null(container)) NULL else container[[name]]
    if (is.null(value) || length(value) != 1L || is.na(value) || !nzchar(as.character(value))) {
      stop(sprintf("environment metadata missing %s", label))
    }
  }
  source_tree <- environment$source_tree
  require_scalar(source_tree, "method", "source tree identity method")
  require_scalar(source_tree, "digest", "source tree digest")
  file_count <- source_tree$file_count
  if (is.null(file_count) || length(file_count) != 1L || is.na(file_count) || as.numeric(file_count) < 1) {
    stop("environment metadata has an invalid source tree file count")
  }
  if (identical(as.character(source_tree$method), "git-worktree-md5")) {
    for (field in c("included_prefixes", "excluded_patterns")) {
      values <- source_tree[[field]]
      if (is.null(values) || length(values) == 0L || any(!nzchar(as.character(values)))) {
        stop(sprintf("environment metadata missing source tree %s", field))
      }
    }
  }
  require_scalar(environment$host, "sysname", "host OS")
  require_scalar(environment$host, "release", "kernel release")
  require_scalar(environment$host, "machine", "host architecture")
  require_scalar(environment$host, "cpu_model", "CPU model")
  require_scalar(environment$r, "version", "R version")
  require_scalar(environment$r, "home", "R home")
  require_scalar(environment$zig, "executable", "Zig executable")
  require_scalar(environment$zig, "version", "Zig version")
  require_scalar(environment$build, "optimization", "optimization mode")
  require_scalar(environment$build, "target", "target triple")
  require_scalar(environment$build, "cpu_features", "CPU feature settings")
  require_scalar(environment$blas, "vendor", "BLAS vendor")
  require_scalar(environment$blas, "version_or_path", "BLAS version or path")
  require_scalar(environment$locale, "LC_ALL", "LC_ALL locale")
  configs <- environment$runner_configs
  if (is.null(configs) || length(configs) == 0L) stop("environment metadata has no runner configurations")
  for (config in configs) {
    require_scalar(config, "name", "runner name")
    require_scalar(config, "call_type", "runner call type")
  }
  invisible(environment)
}

validate_run_artifacts <- function(run_dir, metadata) {
  validate_environment_manifest(metadata$environment)
  expected_run_id <- as.character(metadata$run_id)
  expected_runners <- sort(run_manifest_values(metadata$runners))
  expected_tasks <- sort(run_manifest_values(metadata$tasks))
  allowed_na_tasks <- sort(run_manifest_values(metadata$allowed_na_tasks))
  if (!is.null(metadata$timing_policy)) validate_timing_policy(metadata$timing_policy)
  if (length(expected_runners) == 0L) stop("run manifest has no runners")
  if (length(expected_tasks) == 0L) stop("run manifest has no tasks")
  unknown_allowed_na <- setdiff(allowed_na_tasks, expected_tasks)
  if (length(unknown_allowed_na) > 0L) {
    stop(sprintf("run manifest allows N/A for undeclared tasks: %s", paste(unknown_allowed_na, collapse = ", ")))
  }
  staging_dir <- file.path(run_dir, ".staging")
  if (dir.exists(staging_dir) && length(list.files(staging_dir, all.files = TRUE, no.. = TRUE, recursive = TRUE)) > 0L) {
    stop("run contains unpromoted staging artifacts")
  }

  all_summary_files <- sort(list.files(run_dir, pattern = "^[^/]+_summary\\.csv$", full.names = TRUE))
  expected_files <- file.path(run_dir, paste0(expected_runners, "_summary.csv"))
  allowed_derived_files <- file.path(run_dir, "analysis_summary.csv")
  missing_files <- expected_files[!file.exists(expected_files)]
  unexpected_files <- setdiff(all_summary_files, c(expected_files, allowed_derived_files))
  if (length(missing_files) > 0L || length(unexpected_files) > 0L) {
    stop(sprintf(
      "run summary set differs from the run manifest; expected: %s; got: %s",
      paste(basename(expected_files), collapse = ", "),
      paste(basename(c(setdiff(expected_files, missing_files), unexpected_files)), collapse = ", ")
    ))
  }
  summary_files <- expected_files

  summaries <- do.call(rbind, lapply(summary_files, read.csv, stringsAsFactors = FALSE))
  required <- c(
    "run_id", "runner", "task", "status",
    "correctness_status", "correctness_policy", "correctness_message"
  )
  if (!is.null(metadata$timing_policy)) {
    required <- c(
      required,
      "warmup_iterations", "block_size", "max_iterations", "convergence_window_blocks",
      "convergence_cv_threshold_pct", "convergence_cv_pct", "stopping_condition", "converged",
      "timer_noise_floor_ms", "timer_noise_status", "rss_metric", "gc_policy"
    )
  }
  missing <- setdiff(required, names(summaries))
  if (length(missing) > 0L) stop(sprintf("run summaries missing columns: %s", paste(missing, collapse = ", ")))
  if (!all(as.character(summaries$run_id) == expected_run_id)) stop("run summaries contain mixed run IDs")

  if (anyNA(summaries$status) || any(!nzchar(as.character(summaries$status)))) {
    stop("run summaries contain blank or missing statuses")
  }
  allowed_statuses <- c("PASS", "N/A")
  invalid_status <- summaries[!(summaries$status %in% allowed_statuses), , drop = FALSE]
  if (nrow(invalid_status) > 0L) {
    stop(sprintf(
      "run summaries contain non-completable statuses: %s",
      paste(unique(invalid_status$status), collapse = ", ")
    ))
  }
  invalid_pass <- summaries[
    summaries$status == "PASS" & !(summaries$correctness_status %in% c("PASS", "REFERENCE")),
    , drop = FALSE
  ]
  if (nrow(invalid_pass) > 0L) {
    stop(sprintf(
      "run summaries contain unapproved correctness rows: %s",
      paste(unique(invalid_pass$task), collapse = ", ")
    ))
  }
  invalid_na <- summaries[
    summaries$status == "N/A" &
      (!(summaries$task %in% allowed_na_tasks) | summaries$correctness_status != "NOT_APPLICABLE"),
    , drop = FALSE
  ]
  if (nrow(invalid_na) > 0L) {
    stop(sprintf(
      "run summaries contain undeclared or malformed N/A rows: %s",
      paste(unique(invalid_na$task), collapse = ", ")
    ))
  }

  actual_runners <- sort(unique(as.character(summaries$runner)))
  if (!identical(expected_runners, actual_runners)) stop("run summaries contain an unexpected runner set")
  keys <- paste(summaries$runner, summaries$task, sep = "\r")
  if (anyDuplicated(keys)) stop("run summaries contain duplicate runner/task rows")

  for (runner in expected_runners) {
    runner_tasks <- sort(unique(as.character(summaries$task[summaries$runner == runner])))
    if (!identical(expected_tasks, runner_tasks)) {
      stop(sprintf("run summary coverage for %s differs from the run manifest", runner))
    }

    runner_dir <- file.path(run_dir, runner)
    raw_files <- sort(list.files(runner_dir, pattern = "^task_.*\\.csv$", full.names = TRUE))
    expected_raw_tasks <- sort(as.character(summaries$task[summaries$runner == runner & summaries$status == "PASS"]))
    actual_raw_tasks <- sort(sub("\\.csv$", "", sub("^task_", "", basename(raw_files))))
    if (!identical(expected_raw_tasks, actual_raw_tasks)) {
      stop(sprintf("raw timing coverage for %s differs from PASS summaries", runner))
    }
    for (raw_file in raw_files) {
      raw <- read.csv(raw_file, stringsAsFactors = FALSE)
      if (!"run_id" %in% names(raw)) stop(sprintf("raw result lacks run_id: %s", raw_file))
      if (!all(as.character(raw$run_id) == expected_run_id)) stop("raw results contain mixed run IDs")
    }
    if (!is.null(metadata$timing_policy)) {
      cold_file <- file.path(runner_dir, "cold_start.csv")
      if (!file.exists(cold_file)) stop(sprintf("cold-start results missing for %s", runner))
      cold <- read.csv(cold_file, stringsAsFactors = FALSE)
      missing_cold <- setdiff(c("runner", "task", "wall_ms", "run_id"), names(cold))
      if (length(missing_cold) > 0L) stop(sprintf("cold-start results missing columns: %s", paste(missing_cold, collapse = ", ")))
      if (!all(as.character(cold$run_id) == expected_run_id)) stop("cold-start results contain mixed run IDs")
      actual_cold_tasks <- sort(as.character(cold$task))
      if (!identical(expected_raw_tasks, actual_cold_tasks)) {
        stop(sprintf("cold-start coverage for %s differs from PASS summaries", runner))
      }
    }
  }

  invisible(TRUE)
}
