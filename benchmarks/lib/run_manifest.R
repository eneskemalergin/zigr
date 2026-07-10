run_manifest_path <- function(run_dir) file.path(run_dir, "run_manifest.json")

run_manifest_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

run_manifest_values <- function(value) {
  if (is.null(value)) character(0) else as.character(unlist(value, use.names = FALSE))
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

validate_environment_manifest <- function(environment) {
  if (is.null(environment)) stop("run manifest has no environment metadata")
  require_scalar <- function(container, name, label) {
    value <- if (is.null(container)) NULL else container[[name]]
    if (is.null(value) || length(value) != 1L || is.na(value) || !nzchar(as.character(value))) {
      stop(sprintf("environment metadata missing %s", label))
    }
  }
  require_scalar(environment$source_tree, "digest", "source tree digest")
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
  }

  invisible(TRUE)
}
