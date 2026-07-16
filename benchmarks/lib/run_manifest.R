run_manifest_path <- function(run_dir) file.path(run_dir, "run_manifest.json")

run_manifest_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

run_manifest_values <- function(value) {
  if (is.null(value)) character(0) else as.character(unlist(value, use.names = FALSE))
}

load_retained_correctness <- function(
    path, expected_path, runner, run_id, id_field, expected_ids, passing_ids = character(0),
    expected_identity = list()) {
  supplied <- normalizePath(path, mustWork = TRUE)
  expected <- normalizePath(expected_path, mustWork = TRUE)
  if (!identical(supplied, expected)) stop("retained correctness path differs from the run")
  rows <- read.csv(supplied, stringsAsFactors = FALSE)
  required <- c(
    "run_id", "runner", id_field, "status", "correctness_status", "correctness_message",
    names(expected_identity)
  )
  missing <- setdiff(required, names(rows))
  if (length(missing) > 0L) {
    stop(sprintf("retained correctness is missing columns: %s", paste(missing, collapse = ", ")))
  }
  rows <- rows[as.character(rows$runner) == runner, , drop = FALSE]
  ids <- as.character(rows[[id_field]])
  if (!identical(sort(ids), sort(as.character(expected_ids))) || anyDuplicated(ids) ||
      any(as.character(rows$runner) != runner) || any(as.character(rows$run_id) != run_id) ||
      any(as.character(rows$status) == "FAIL")) {
    stop("retained correctness does not match the selected run cells")
  }
  passing <- rows[ids %in% as.character(passing_ids), , drop = FALSE]
  if (nrow(passing) != length(passing_ids) || any(as.character(passing$status) != "PASS") ||
      any(!(as.character(passing$correctness_status) %in% c("PASS", "REFERENCE")))) {
    stop("retained correctness is not passing for every executable cell")
  }
  for (field in names(expected_identity)) {
    expected_value <- expected_identity[[field]]
    expected <- as.character(expected_value)
    names(expected) <- names(expected_value)
    actual <- as.character(rows[[field]])
    if (length(expected) == 1L) {
      expected <- rep(expected, length(ids))
    } else if (is.null(names(expected)) || !setequal(names(expected), expected_ids)) {
      stop(sprintf("retained correctness expected %s identity is not keyed by selected cells", field))
    } else {
      expected <- expected[match(ids, names(expected))]
    }
    if (anyNA(expected) || any(!nzchar(expected)) ||
        anyNA(actual) || any(!nzchar(actual)) || any(actual != expected)) {
      stop(sprintf("retained correctness %s differs from the selected run", field))
    }
  }
  rows
}

run_manifest_disposition <- function(metadata, runner, task) {
  runner_records <- metadata$runner_dispositions[[runner]]
  if (is.null(runner_records)) stop(sprintf("run manifest has no dispositions for runner %s", runner))
  matches <- runner_records[vapply(runner_records, function(record) identical(as.character(record$task), task), logical(1))]
  if (length(matches) != 1L) stop(sprintf("run manifest has no unique disposition for %s/%s", runner, task))
  matches[[1L]]
}

run_manifest_task_input <- function(metadata, task) {
  records <- metadata$task_inputs
  if (is.null(records)) stop("run manifest has no task input records")
  matches <- records[vapply(records, function(record) identical(as.character(record$task), task), logical(1))]
  if (length(matches) != 1L) stop(sprintf("run manifest has no unique task input record for %s", task))
  matches[[1L]]
}

run_manifest_artifact_digest <- function(paths) {
  paths <- sort(unique(normalizePath(as.character(paths), mustWork = FALSE)))
  if (length(paths) == 0L || any(!file.exists(paths))) stop("run artifact identity contains a missing file")
  identity_file <- tempfile("run-artifact-identity-")
  on.exit(unlink(identity_file), add = TRUE)
  writeLines(paste(paths, unname(as.character(tools::md5sum(paths))), sep = "\t"), identity_file, useBytes = TRUE)
  unname(as.character(tools::md5sum(identity_file))[[1L]])
}

run_manifest_object_digest <- function(value) {
  temporary <- tempfile("run-manifest-object-")
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(value, temporary, auto_unbox = TRUE, null = "null", digits = NA)
  unname(as.character(tools::md5sum(temporary))[[1L]])
}

run_manifest_completion_contract <- function(metadata) {
  required <- c(
    "schema_version", "run_id", "status", "started_at", "finished_at", "runners", "tasks",
    "master_seed", "input_manifest", "runner_dispositions", "r_provenance", "timing_policy",
    "boundary_budget_policy_version", "suite", "full_matrix", "promotion_eligible", "measurement_mode", "environment",
    "correctness_stage", "completion_artifacts"
  )
  if (!is.null(metadata$schema_version) && as.integer(metadata$schema_version) < 3L) {
    required <- c(required, "task_inputs")
  }
  missing <- required[vapply(required, function(field) is.null(metadata[[field]]), logical(1))]
  if (!is.null(metadata$schema_version) && as.integer(metadata$schema_version) >= 3L &&
      is.null(metadata$artifact_layout)) {
    missing <- c(missing, "artifact_layout")
  }
  if (length(missing) > 0L) {
    stop(sprintf("run completion contract is missing fields: %s", paste(missing, collapse = ", ")))
  }
  if (!identical(as.character(metadata$status), "complete")) {
    stop("run completion contract requires complete status")
  }
  fields <- sort(setdiff(names(metadata), "completion_contract"))
  metadata[fields]
}

capture_run_completion_contract <- function(metadata) {
  list(
    schema_version = "run-completion-contract-v1",
    digest = run_manifest_object_digest(run_manifest_completion_contract(metadata))
  )
}

validate_run_completion_contract <- function(metadata) {
  recorded <- metadata$completion_contract
  if (is.null(recorded) ||
      !identical(as.character(recorded$schema_version), "run-completion-contract-v1") ||
      is.null(recorded$digest) || !nzchar(as.character(recorded$digest))) {
    stop("run has no supported completion contract; collect a fresh run")
  }
  actual <- capture_run_completion_contract(metadata)
  if (!identical(as.character(actual$digest), as.character(recorded$digest))) {
    stop("run completion contract differs from the completed run")
  }
  invisible(actual)
}

run_manifest_relative_artifact_path <- function(...) {
  path <- gsub("\\\\", "/", do.call(file.path, list(...)))
  absolute <- grepl("^(/|[A-Za-z]:/|//)", path)
  if (length(path) != 1L || is.na(path) || !nzchar(path) || absolute ||
      grepl("(^|/)\\.\\.(/|$)", path)) {
    stop(sprintf("run completion artifact path is unsafe: %s", path))
  }
  path
}

run_manifest_relative_artifact_paths <- function(paths) {
  paths <- as.character(paths)
  if (length(paths) == 0L) return(character(0))
  vapply(paths, run_manifest_relative_artifact_path, character(1), USE.NAMES = FALSE)
}

copy_run_artifact_set <- function(source_dir, destination_dir, relative_paths) {
  source_root <- normalizePath(source_dir, mustWork = TRUE)
  source_prefix <- paste0(source_root, .Platform$file.sep)
  relative_paths <- sort(unique(run_manifest_relative_artifact_paths(relative_paths)))
  for (relative_path in relative_paths) {
    source <- file.path(source_root, relative_path)
    if (!file.exists(source)) stop(sprintf("promotion source file is missing: %s", source))
    resolved_source <- normalizePath(source, mustWork = TRUE)
    if (!startsWith(resolved_source, source_prefix)) {
      stop(sprintf("promotion source file escapes the completed run: %s", relative_path))
    }
    destination <- file.path(destination_dir, relative_path)
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(source, destination, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)) {
      stop(sprintf("cannot stage promotion file: %s", relative_path))
    }
  }
  invisible(relative_paths)
}

write_run_manifest_json_atomic <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  staged <- tempfile("run-manifest-json-", tmpdir = dirname(path))
  backup <- NULL
  on.exit({
    if (file.exists(staged)) unlink(staged)
    if (!is.null(backup) && file.exists(backup)) {
      if (!file.exists(path)) {
        file.rename(backup, path)
      } else {
        unlink(backup)
      }
    }
  }, add = TRUE)
  jsonlite::write_json(value, staged, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  if (file.exists(path)) {
    if (file.rename(staged, path)) return(invisible(path))
    backup <- tempfile("run-manifest-backup-", tmpdir = dirname(path))
    if (!file.rename(path, backup)) stop(sprintf("cannot stage existing JSON record: %s", path))
  }
  if (!file.rename(staged, path)) {
    if (!is.null(backup) && file.exists(backup)) file.rename(backup, path)
    stop(sprintf("cannot install JSON record: %s", path))
  }
  if (!is.null(backup)) unlink(backup)
  invisible(path)
}

run_relative_files <- function(root) {
  if (!dir.exists(root)) return(character(0))
  sort(gsub("\\\\", "/", list.files(
    root, recursive = TRUE, all.files = TRUE, no.. = TRUE,
    full.names = FALSE, include.dirs = FALSE
  )))
}

validate_run_cache_paths <- function(run_dir, paths) {
  run_root <- normalizePath(run_dir, mustWork = FALSE)
  prefix <- paste0(run_root, .Platform$file.sep)
  resolved <- vapply(paths, normalizePath, character(1), mustWork = FALSE)
  if (any(resolved == run_root | startsWith(resolved, prefix))) {
    stop("build caches must be outside the benchmark run directory")
  }
  invisible(resolved)
}

run_core_artifact_paths <- function(run_dir, metadata) {
  sort(unique(c("run_manifest.json", run_completion_artifact_paths(run_dir, metadata))))
}

validate_run_core_artifact_set <- function(run_dir, metadata) {
  expected <- run_core_artifact_paths(run_dir, metadata)
  actual <- run_relative_files(run_dir)
  missing <- setdiff(expected, actual)
  extra <- setdiff(actual, expected)
  if (length(missing) > 0L || length(extra) > 0L) {
    stop(sprintf(
      "run core artifact set differs; missing: %s; extra: %s",
      if (length(missing) == 0L) "none" else paste(missing, collapse = ", "),
      if (length(extra) == 0L) "none" else paste(extra, collapse = ", ")
    ))
  }
  invisible(expected)
}

validate_report_artifact_set <- function(run_dir, metadata, report_manifest) {
  declared <- sort(unique(run_manifest_relative_artifact_paths(
    run_manifest_values(report_manifest$declared_report_files)
  )))
  expected_declared <- sort(unname(declared_report_files()))
  if (length(declared) == 0L || anyDuplicated(run_manifest_values(report_manifest$declared_report_files)) ||
      !identical(declared, expected_declared)) {
    stop("report manifest declared filenames differ from the report contract")
  }
  records <- report_manifest$reports
  if (is.null(records) || length(records) != length(declared)) {
    stop("report manifest records do not cover every declared report")
  }
  recorded <- vapply(records, function(record) as.character(record$file), character(1))
  if (anyDuplicated(recorded) || !setequal(recorded, declared)) {
    stop("report manifest records differ from its declared filenames")
  }
  recorded_roles <- vapply(records, function(record) as.character(record$role), character(1))
  expected_roles <- names(declared_report_files())[match(recorded, unname(declared_report_files()))]
  if (anyNA(expected_roles) || anyDuplicated(recorded_roles) || !identical(recorded_roles, expected_roles)) {
    stop("report manifest roles differ from the report contract")
  }
  for (record in records) {
    relative <- run_manifest_relative_artifact_path(as.character(record$file))
    path <- file.path(run_dir, relative)
    actual <- if (file.exists(path)) unname(as.character(tools::md5sum(path)[[1L]])) else ""
    if (!identical(actual, as.character(record$md5))) {
      stop(sprintf("declared report digest differs for %s", relative))
    }
    rows <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(error) NULL)
    recorded_rows <- suppressWarnings(as.integer(record$rows))
    if (is.null(rows) || length(recorded_rows) != 1L || is.na(recorded_rows) ||
        recorded_rows < 0L || recorded_rows != nrow(rows)) {
      stop(sprintf("declared report row count differs for %s", relative))
    }
    tracks <- record$tracks
    if ("report_track" %in% names(rows)) {
      if (is.null(tracks) || length(tracks) == 0L) {
        stop(sprintf("consolidated report has no track counts for %s", relative))
      }
      if (anyNA(rows$report_track) || any(!nzchar(as.character(rows$report_track)))) {
        stop(sprintf("consolidated report has a blank track for %s", relative))
      }
      track_names <- vapply(tracks, function(track) as.character(track$track), character(1))
      track_rows <- vapply(tracks, function(track) suppressWarnings(as.integer(track$rows)), integer(1))
      actual_tracks <- table(as.character(rows$report_track), useNA = "always")
      actual_tracks <- actual_tracks[!is.na(names(actual_tracks))]
      if (anyDuplicated(track_names) || anyNA(track_rows) || any(track_rows < 1L) ||
          sum(track_rows) != recorded_rows ||
          !setequal(track_names, names(actual_tracks)) ||
          any(track_rows[match(names(actual_tracks), track_names)] != as.integer(actual_tracks))) {
        stop(sprintf("declared report track counts differ for %s", relative))
      }
    } else if (!is.null(tracks)) {
      stop(sprintf("non-consolidated report declares track counts for %s", relative))
    }
  }
  expected <- sort(c(run_core_artifact_paths(run_dir, metadata), "report_manifest.json", declared))
  actual <- run_relative_files(run_dir)
  missing <- setdiff(expected, actual)
  extra <- setdiff(actual, expected)
  if (length(missing) > 0L || length(extra) > 0L) {
    stop(sprintf(
      "published report artifact set differs; missing: %s; extra: %s",
      if (length(missing) == 0L) "none" else paste(missing, collapse = ", "),
      if (length(extra) == 0L) "none" else paste(extra, collapse = ", ")
    ))
  }
  invisible(declared)
}

record_run_failure <- function(run_dir, message) {
  metadata <- tryCatch(read_run_manifest(run_dir), error = function(error) NULL)
  if (is.null(metadata)) return(invisible(NULL))
  worker_errors <- character(0)
  staging <- file.path(run_dir, ".staging")
  if (dir.exists(staging)) {
    error_files <- list.files(
      staging, pattern = "^errors\\.csv$", recursive = TRUE, full.names = TRUE
    )
    for (path in error_files) {
      rows <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(error) NULL)
      if (!is.null(rows) && "error" %in% names(rows)) {
        worker_errors <- c(worker_errors, as.character(rows$error))
      }
    }
  }
  worker_errors <- unique(worker_errors[!is.na(worker_errors) & nzchar(worker_errors)])
  detail <- paste(c(as.character(message), head(worker_errors, 20L)), collapse = "; ")
  if (!nzchar(detail)) detail <- "benchmark run did not complete"
  if (nchar(detail, type = "bytes") > 4000L) detail <- substr(detail, 1L, 4000L)
  failure <- list(
    schema_version = as.integer(metadata$schema_version),
    artifact_layout = as.character(metadata$artifact_layout),
    run_id = as.character(metadata$run_id),
    status = "incomplete",
    started_at = as.character(metadata$started_at),
    finished_at = run_manifest_timestamp(),
    status_message = detail
  )
  entries <- list.files(run_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  unlink(entries, recursive = TRUE)
  write_run_manifest_json_atomic(failure, run_manifest_path(run_dir))
  invisible(failure)
}

seal_run_promotion_receipt <- function(receipt) {
  if (!is.null(receipt$receipt_digest)) stop("promotion receipt is already sealed")
  receipt$receipt_digest <- run_manifest_object_digest(receipt)
  receipt
}

validate_run_promotion_receipt <- function(receipt) {
  recorded <- receipt$receipt_digest
  if (is.null(recorded) || !nzchar(as.character(recorded))) stop("promotion receipt has no digest")
  receipt$receipt_digest <- NULL
  actual <- run_manifest_object_digest(receipt)
  if (!identical(as.character(recorded), actual)) stop("promotion receipt digest differs")
  invisible(receipt)
}

run_completion_artifact_paths <- function(run_dir, metadata) {
  runners <- sort(run_manifest_values(metadata$runners))
  paths <- character(0)
  task_summaries <- data.frame()
  fixture_summaries <- data.frame()
  if (benchmark_run_includes(metadata, "task")) {
    paths <- c(
      paths,
      as.character(metadata$input_manifest$relative_path),
      run_summary_artifact_paths("", metadata, "task", runners),
      run_correctness_artifact_paths("", metadata, "task", runners)
    )
    task_summaries <- read_run_summary_table(run_dir, metadata, "task", runners)
  }
  if (benchmark_run_includes(metadata, "fixture")) {
    paths <- c(
      paths,
      run_summary_artifact_paths("", metadata, "fixture", runners),
      run_correctness_artifact_paths("", metadata, "fixture", runners)
    )
    fixture_summaries <- read_run_summary_table(run_dir, metadata, "fixture", runners)
  }
  for (runner in runners) {
    if (benchmark_run_includes(metadata, "task")) {
      task_summary <- task_summaries[as.character(task_summaries$runner) == runner, , drop = FALSE]
      task_ids <- as.character(task_summary$task[task_summary$status == "PASS"])
      if (length(task_ids) > 0L) paths <- c(
        paths,
        run_sample_artifact_paths("", metadata, "task", runner, task_ids)
      )
    }
    if (benchmark_run_includes(metadata, "fixture")) {
      fixture_summary <- fixture_summaries[as.character(fixture_summaries$runner) == runner, , drop = FALSE]
      fixture_rows <- as.character(fixture_summary$row_id[fixture_summary$status == "PASS"])
      if (length(fixture_rows) > 0L) paths <- c(
        paths,
        run_sample_artifact_paths("", metadata, "fixture", runner, fixture_rows)
      )
    }
  }
  sort(unique(run_manifest_relative_artifact_paths(paths)))
}

capture_run_completion_artifacts <- function(run_dir, metadata) {
  relative_paths <- run_completion_artifact_paths(run_dir, metadata)
  paths <- file.path(run_dir, relative_paths)
  missing <- relative_paths[!file.exists(paths)]
  if (length(missing) > 0L) {
    stop(sprintf("run completion artifacts are missing: %s", paste(missing, collapse = ", ")))
  }
  records <- lapply(seq_along(paths), function(index) list(
    path = relative_paths[[index]],
    size = unname(file.info(paths[[index]])$size),
    md5 = unname(as.character(tools::md5sum(paths[[index]]))[[1L]])
  ))
  list(
    schema_version = "run-completion-artifacts-v1",
    digest = run_manifest_object_digest(records),
    files = records
  )
}

validate_run_completion_artifacts <- function(run_dir, metadata) {
  recorded <- metadata$completion_artifacts
  if (is.null(recorded) ||
      !identical(as.character(recorded$schema_version), "run-completion-artifacts-v1") ||
      is.null(recorded$digest) || !nzchar(as.character(recorded$digest)) ||
      is.null(recorded$files) || length(recorded$files) == 0L) {
    stop("run has no supported completion artifact seal; collect a fresh run")
  }
  actual <- capture_run_completion_artifacts(run_dir, metadata)
  if (!identical(as.character(actual$digest), as.character(recorded$digest))) {
    stop("run completion artifacts differ from the completed run")
  }
  invisible(actual)
}

validate_run_disposition_identity <- function(metadata, current_dispositions) {
  if (!identical(
    run_manifest_object_digest(metadata$runner_dispositions),
    run_manifest_object_digest(current_dispositions)
  )) {
    stop("current task dispositions differ from the completed run; collect a fresh run")
  }
  invisible(current_dispositions)
}

run_manifest_r_provenance_records <- function(metadata, field) {
  provenance <- metadata$r_provenance
  supported <- if (as.integer(metadata$schema_version) >= 3L) {
    run_r_provenance_schema_version()
  } else r_provenance_schema_version()
  if (is.null(provenance) || !identical(as.character(provenance$schema_version), supported)) {
    stop("run manifest has no supported R provenance")
  }
  records <- provenance[[field]]
  if (is.null(records)) return(list())
  task_ids <- vapply(records, function(record) as.character(record$task), character(1))
  if (anyDuplicated(task_ids)) stop(sprintf("run R provenance %s contains duplicate tasks", field))
  names(records) <- task_ids
  records
}

validate_summary_disposition <- function(status, correctness_status, disposition, disposition_reason, runner, task) {
  if (identical(as.character(status), "N/A")) {
    if (isTRUE(disposition$executable) || !identical(as.character(correctness_status), "NOT_APPLICABLE")) {
      stop(sprintf("run summary has a runner-specific N/A error for %s/%s", runner, task))
    }
    if (!identical(as.character(disposition_reason), as.character(disposition$reason)) ||
        !nzchar(as.character(disposition_reason))) {
      stop(sprintf("run summary N/A lacks its normalized disposition reason for %s/%s", runner, task))
    }
  } else if (!isTRUE(disposition$executable)) {
    stop(sprintf("run summary executes a non-executable disposition for %s/%s", runner, task))
  }
  invisible(disposition)
}

validate_timing_admission <- function(disposition, runner, task) {
  if (!isTRUE(disposition$executable) || !isTRUE(disposition$timing_eligible)) {
    stop(sprintf("runner %s task %s is not admitted for timing", runner, task))
  }
  invisible(disposition)
}

validate_timing_policy <- function(policy) {
  if (is.null(policy)) stop("run manifest has no timing policy")
  if (is.null(policy$policy_version)) {
    required <- c(
      "warmup_iterations", "block_size", "max_iterations", "convergence_window_blocks",
      "convergence_cv_threshold_pct", "timer_noise_floor_ms", "timer_noise_floor_method",
      "low_noise_cv_threshold_pct", "meaningful_margin_ratio", "median_ci_level",
      "median_ci_method", "rss_metric", "gc_policy"
    )
    missing <- required[vapply(required, function(name) is.null(policy[[name]]), logical(1))]
    if (length(missing) > 0L) stop(sprintf("legacy timing policy missing fields: %s", paste(missing, collapse = ", ")))
    integer_fields <- c("warmup_iterations", "block_size", "max_iterations", "convergence_window_blocks")
    numeric_fields <- c(
      "convergence_cv_threshold_pct", "timer_noise_floor_ms", "low_noise_cv_threshold_pct",
      "meaningful_margin_ratio", "median_ci_level"
    )
    for (name in integer_fields) {
      value <- as.numeric(policy[[name]])
      if (length(value) != 1L || is.na(value) || value < 1 || value != as.integer(value)) {
        stop(sprintf("legacy timing policy has invalid %s", name))
      }
    }
    for (name in numeric_fields) {
      value <- as.numeric(policy[[name]])
      if (length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0) {
        stop(sprintf("legacy timing policy has invalid %s", name))
      }
    }
    if (as.numeric(policy$median_ci_level) >= 1) stop("legacy timing policy median CI level must be below 1")
    return(invisible(policy))
  }
  if (!identical(as.character(policy$policy_version), "bounded-pilot-confirmation-v2")) {
    stop("run manifest has an unsupported timing policy version")
  }
  required <- c(
    "policy_version", "warmup_iterations", "pilot_iterations", "confirmation_min_iterations",
    "confirmation_max_iterations", "confirmation_target_cv_pct", "group_time_cap_ms",
    "batch_time_cap_ms", "batch_group_cap", "batch_timeout_seconds", "total_run_budget_seconds",
    "timer_noise_floor_ms", "timer_noise_floor_method",
    "low_noise_cv_threshold_pct",
    "meaningful_margin_ratio", "median_ci_level", "median_ci_method", "rss_endpoint_metric",
    "peak_rss_metric", "peak_rss_repetitions", "peak_rss_timeout_seconds",
    "peak_rss_fixture_ids", "gc_policy"
  )
  missing <- required[vapply(required, function(name) is.null(policy[[name]]), logical(1))]
  if (length(missing) > 0L) stop(sprintf("timing policy missing fields: %s", paste(missing, collapse = ", ")))
  integer_fields <- c(
    "warmup_iterations", "pilot_iterations", "confirmation_min_iterations",
    "confirmation_max_iterations", "batch_group_cap", "batch_timeout_seconds", "total_run_budget_seconds",
    "peak_rss_repetitions", "peak_rss_timeout_seconds"
  )
  for (name in integer_fields) {
    value <- as.numeric(policy[[name]])
    if (length(value) != 1L || is.na(value) || value < 1 || value != as.integer(value)) {
      stop(sprintf("timing policy has invalid %s", name))
    }
  }
  numeric_fields <- c(
    "confirmation_target_cv_pct", "group_time_cap_ms", "batch_time_cap_ms",
    "timer_noise_floor_ms", "low_noise_cv_threshold_pct",
    "meaningful_margin_ratio", "median_ci_level"
  )
  for (name in numeric_fields) {
    value <- as.numeric(policy[[name]])
    if (length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0) {
      stop(sprintf("timing policy has invalid %s", name))
    }
  }
  if (as.numeric(policy$median_ci_level) >= 1) stop("timing policy median CI level must be below 1")
  if (as.integer(policy$confirmation_min_iterations) > as.integer(policy$confirmation_max_iterations)) {
    stop("timing policy confirmation bounds are inverted")
  }
  for (name in c(
    "policy_version", "timer_noise_floor_method", "median_ci_method", "rss_endpoint_metric",
    "peak_rss_metric", "gc_policy"
  )) {
    if (length(policy[[name]]) != 1L || !nzchar(as.character(policy[[name]]))) {
      stop(sprintf("timing policy has invalid %s", name))
    }
  }
  fixture_ids <- as.character(unlist(policy$peak_rss_fixture_ids, use.names = FALSE))
  if (!identical(fixture_ids, c("F03", "F04", "F06"))) {
    stop("timing policy has invalid peak_rss_fixture_ids")
  }
  if (!identical(as.character(policy$rss_endpoint_metric), "post_gc_current_rss_endpoint_delta_kb") ||
      !identical(as.character(policy$peak_rss_metric), "linux_proc_status_vmhwm_kb")) {
    stop("timing policy has an invalid process-memory metric")
  }
  invisible(policy)
}

is_bounded_timing_policy <- function(policy) {
  !is.null(policy$policy_version) &&
    identical(as.character(policy$policy_version), "bounded-pilot-confirmation-v2")
}

timing_records_frame <- function(records, label) {
  if (is.data.frame(records)) return(records)
  if (!is.list(records) || length(records) == 0L) return(data.frame())
  fields <- unique(unlist(lapply(records, names), use.names = FALSE))
  rows <- lapply(records, function(record) {
    values <- lapply(fields, function(field) {
      value <- record[[field]]
      if (is.null(value)) NA else value
    })
    names(values) <- fields
    as.data.frame(values, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

validate_timing_execution <- function(execution, metadata) {
  required <- c(
    "schedule_seeds", "packing_hint_run_ids", "confirmation_budget", "task_plan", "fixture_plan",
    "task_pilot_schedule", "fixture_pilot_schedule",
    "task_pilot_batches", "fixture_pilot_batches", "task_confirmation_schedule", "fixture_confirmation_schedule",
    "task_confirmation_batches", "fixture_confirmation_batches", "frozen_at", "outcomes", "finished_at"
  )
  missing <- required[vapply(required, function(field) is.null(execution[[field]]), logical(1))]
  if (length(missing) > 0L) stop(sprintf("timing execution missing fields: %s", paste(missing, collapse = ", ")))
  available_ms <- as.numeric(execution$confirmation_budget$available_ms)
  decisions <- timing_records_frame(execution$confirmation_budget$decisions, "confirmation budget decisions")
  required_decisions <- c(
    "universe", "group_id", "group_order", "estimated_ms", "budget_order", "admitted", "remaining_after_ms"
  )
  if (length(available_ms) != 1L || !is.finite(available_ms) || available_ms < 0 ||
      available_ms > as.numeric(metadata$timing_policy$total_run_budget_seconds) * 1000 ||
      (nrow(decisions) > 0L && (
        length(setdiff(required_decisions, names(decisions))) > 0L ||
        anyDuplicated(paste(decisions$universe, decisions$group_id, sep = "\r")) ||
        any(!(as.character(decisions$universe) %in% c("task", "fixture"))) ||
        !identical(as.integer(decisions$budget_order), seq_len(nrow(decisions))) ||
        any(!is.finite(as.numeric(decisions$estimated_ms))) || any(as.numeric(decisions$estimated_ms) < 0) ||
        any(!is.finite(as.numeric(decisions$remaining_after_ms))) || any(as.numeric(decisions$remaining_after_ms) < 0)
      ))) {
    stop("confirmation budget plan is invalid")
  }
  if (nrow(decisions) > 0L) {
    remaining <- available_ms
    for (index in seq_len(nrow(decisions))) {
      expected_admitted <- as.numeric(decisions$estimated_ms[[index]]) <= remaining
      if (expected_admitted) remaining <- remaining - as.numeric(decisions$estimated_ms[[index]])
      if (!identical(as.logical(decisions$admitted[[index]]), expected_admitted) ||
          !isTRUE(all.equal(as.numeric(decisions$remaining_after_ms[[index]]), remaining))) {
        stop("confirmation budget decisions differ from the declared budget")
      }
    }
  }
  for (universe in c("task", "fixture")) {
    plan <- timing_records_frame(execution[[paste0(universe, "_plan")]], paste(universe, "timing plan"))
    required_plan <- c(
      "group_id", "pilot_complete", "pilot_median_group_ms", "pilot_max_cv_pct",
      "pilot_max_drift_pct", "confirmation_iterations", "estimated_confirmation_ms", "status"
    )
    selected <- benchmark_run_includes(metadata, universe)
    if ((!selected && nrow(plan) > 0L) ||
        (selected && length(setdiff(required_plan, names(plan))) > 0L) ||
        anyDuplicated(as.character(plan$group_id)) ||
        any(!(as.character(plan$status) %in% c("confirmation", "below_timer_floor", "unsupported", "incomplete")))) {
      stop(sprintf("%s timing plan is invalid", universe))
    }
    if (identical(universe, "task") && !identical(
      sort(as.character(plan$group_id)), sort(run_manifest_values(metadata$tasks))
    )) stop("task timing plan differs from the declared task set")
    confirmation <- plan[as.character(plan$status) == "confirmation", , drop = FALSE]
    universe_decisions <- decisions[as.character(decisions$universe) == universe, , drop = FALSE]
    admitted <- as.character(universe_decisions$group_id[as.logical(universe_decisions$admitted)])
    rejected <- as.character(universe_decisions$group_id[!as.logical(universe_decisions$admitted)])
    if (!setequal(admitted, as.character(confirmation$group_id)) ||
        any(!(as.character(universe_decisions$group_id) %in% as.character(plan$group_id))) ||
        any(plan$status[match(rejected, plan$group_id)] != "incomplete")) {
      stop(sprintf("%s timing plan differs from confirmation budget admission", universe))
    }
    if (nrow(confirmation) > 0L && (
        any(as.integer(confirmation$confirmation_iterations) < as.integer(metadata$timing_policy$confirmation_min_iterations)) ||
        any(as.integer(confirmation$confirmation_iterations) > as.integer(metadata$timing_policy$confirmation_max_iterations)) ||
        any(!is.finite(as.numeric(confirmation$estimated_confirmation_ms))))) {
      stop(sprintf("%s confirmation plan exceeds timing bounds", universe))
    }
    for (stage in c("pilot", "confirmation")) {
      expected_groups <- if (identical(stage, "pilot")) as.character(plan$group_id) else as.character(confirmation$group_id)
      schedule <- timing_records_frame(
        execution[[paste0(universe, "_", stage, "_schedule")]], paste(universe, stage, "schedule")
      )
      batches <- timing_records_frame(
        execution[[paste0(universe, "_", stage, "_batches")]], paste(universe, stage, "batches")
      )
      if (length(expected_groups) == 0L) {
        if (nrow(schedule) != 0L || nrow(batches) != 0L) {
          stop(sprintf("%s %s schedule has unexpected groups", universe, stage))
        }
      } else {
        required_schedule <- c("group_id", "group_order", "batch", "runner", "member_order")
        required_batches <- c("group_id", "group_order", "estimated_ms", "batch")
        keys <- if (nrow(schedule) > 0L) paste(schedule$group_id, schedule$runner, sep = "\r") else character(0)
        expected_keys <- as.vector(outer(expected_groups, run_manifest_values(metadata$runners), paste, sep = "\r"))
        if (length(setdiff(required_schedule, names(schedule))) > 0L || anyDuplicated(keys) ||
            !setequal(keys, expected_keys) ||
            length(setdiff(required_batches, names(batches))) > 0L ||
            anyDuplicated(as.character(batches$group_id)) ||
            !setequal(as.character(batches$group_id), expected_groups) ||
            anyDuplicated(as.integer(batches$group_order)) ||
            any(as.integer(batches$group_order) < 1L) || any(as.integer(batches$batch) < 1L) ||
            any(!is.finite(as.numeric(batches$estimated_ms))) || any(as.numeric(batches$estimated_ms) < 0)) {
          stop(sprintf("%s %s schedule is not complete and symmetric", universe, stage))
        }
        batch_rows <- match(as.character(schedule$group_id), as.character(batches$group_id))
        if (any(as.integer(schedule$group_order) != as.integer(batches$group_order[batch_rows])) ||
            any(as.integer(schedule$batch) != as.integer(batches$batch[batch_rows]))) {
          stop(sprintf("%s %s schedule differs from its frozen batches", universe, stage))
        }
        runners <- run_manifest_values(metadata$runners)
        for (group in expected_groups) {
          rows <- schedule[as.character(schedule$group_id) == group, , drop = FALSE]
          rows <- rows[order(as.integer(rows$member_order)), , drop = FALSE]
          batch <- as.integer(rows$batch[[1L]])
          offset <- (batch - 1L) %% length(runners)
          expected_runners <- runners[((seq_along(runners) - 1L + offset) %% length(runners)) + 1L]
          if (!identical(as.integer(rows$member_order), seq_along(runners)) ||
              !identical(as.character(rows$runner), as.character(expected_runners))) {
            stop(sprintf("%s %s tool order does not match its frozen rotation", universe, stage))
          }
        }
      }
    }
  }
  if (length(execution$frozen_at) != 1L || !nzchar(as.character(execution$frozen_at))) {
    stop("timing execution has no freeze timestamp")
  }
  for (universe in c("task", "fixture")) {
    plan <- timing_records_frame(execution[[paste0(universe, "_plan")]], paste(universe, "timing plan"))
    for (stage in c("pilot", "confirmation")) {
      name <- paste(universe, stage, sep = "_")
      outcomes <- timing_records_frame(execution$outcomes[[name]], paste(name, "timing outcomes"))
      expected <- if (identical(stage, "pilot")) {
        as.character(plan$group_id)
      } else as.character(plan$group_id[as.character(plan$status) == "confirmation"])
      invalid <- if (length(expected) == 0L) nrow(outcomes) != 0L else
        length(setdiff(c("group_id", "batch", "attempt", "batch_epoch", "status"), names(outcomes))) > 0L ||
        any(as.character(outcomes$status) != "complete") || anyDuplicated(as.character(outcomes$group_id)) ||
        !setequal(as.character(outcomes$group_id), expected)
      if (invalid) {
        stop(sprintf("%s timing outcomes are not complete", name))
      }
    }
  }
  if (length(execution$finished_at) != 1L || !nzchar(as.character(execution$finished_at))) {
    stop("timing execution has no finish timestamp")
  }
  invisible(execution)
}

write_run_manifest <- function(run_dir, metadata) {
  write_run_manifest_json_atomic(metadata, run_manifest_path(run_dir))
}

read_run_manifest <- function(run_dir) {
  path <- run_manifest_path(run_dir)
  if (!file.exists(path)) stop(sprintf("run manifest not found: %s", path))
  metadata <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (is.null(metadata$run_id) || !nzchar(as.character(metadata$run_id))) {
    stop(sprintf("run manifest has no run_id: %s", path))
  }
  if (is.null(metadata$schema_version) || as.integer(metadata$schema_version) != 3L) {
    stop(sprintf("unsupported run manifest schema version: %s", path))
  }
  benchmark_artifact_layout(metadata)
  metadata
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

    record_run_failure(
      run_dir,
      sprintf("stale running run superseded by %s", replacement_run_id)
    )
    reconciled <- c(reconciled, basename(run_dir))
  }
  reconciled
}

prune_local_runs <- function(results_root, keep, protected_run_ids = character(0), dry_run = FALSE) {
  keep_text <- as.character(keep)
  if (length(keep_text) != 1L || is.na(keep_text) || !grepl("^[0-9]+$", keep_text)) {
    stop("run retention count must be a non-negative integer")
  }
  keep <- suppressWarnings(as.integer(keep_text))
  if (is.na(keep)) stop("run retention count must be a non-negative integer")
  runs_root <- normalizePath(file.path(results_root, "runs"), mustWork = FALSE)
  if (!dir.exists(runs_root)) return(character(0))
  run_dirs <- list.dirs(runs_root, full.names = TRUE, recursive = FALSE)
  records <- lapply(run_dirs, function(run_dir) {
    manifest_path <- run_manifest_path(run_dir)
    if (!file.exists(manifest_path)) return(NULL)
    metadata <- tryCatch(jsonlite::fromJSON(manifest_path, simplifyVector = FALSE), error = function(error) NULL)
    if (is.null(metadata) || length(metadata$run_id) != 1L || length(metadata$status) != 1L ||
        !identical(as.character(metadata$run_id), basename(run_dir))) return(NULL)
    list(
      run_id = as.character(metadata$run_id),
      status = as.character(metadata$status),
      path = normalizePath(run_dir, mustWork = TRUE),
      modified = unname(file.info(run_dir)$mtime)
    )
  })
  records <- Filter(Negate(is.null), records)
  eligible <- Filter(function(record) {
    record$status %in% c("complete", "incomplete", "correctness_complete") &&
      !(record$run_id %in% as.character(protected_run_ids))
  }, records)
  if (length(eligible) <= keep) return(character(0))
  order_index <- order(vapply(eligible, function(record) as.numeric(record$modified), numeric(1)), decreasing = TRUE)
  remove <- eligible[order_index][seq.int(keep + 1L, length(eligible))]
  paths <- vapply(remove, function(record) record$path, character(1))
  expected_prefix <- paste0(runs_root, .Platform$file.sep)
  if (any(!startsWith(paths, expected_prefix))) stop("run pruning candidate escapes the local runs directory")
  if (!dry_run) {
    unlink(paths, recursive = TRUE)
    if (any(dir.exists(paths))) stop("run pruning could not remove every selected directory")
  }
  vapply(remove, function(record) record$run_id, character(1))
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
  environment_schema <- if (is.null(environment$schema_version)) 1L else as.integer(environment$schema_version)
  if (length(environment_schema) != 1L || is.na(environment_schema) || !(environment_schema %in% 1:3)) {
    stop("unsupported environment metadata schema version")
  }
  if (environment_schema >= 2L) {
    if (is.null(environment$build$checked_sexp) ||
        length(environment$build$checked_sexp) != 1L ||
        !is.logical(environment$build$checked_sexp) ||
        is.na(environment$build$checked_sexp)) {
      stop("environment metadata missing checked SEXP mode")
    }
  }
  if (environment_schema >= 3L) {
    require_scalar(environment$tool_source_ledger, "schema_version", "tool source ledger schema")
    require_scalar(environment$tool_source_ledger, "identity_digest", "tool source ledger identity")
    require_scalar(environment$tool_source_ledger, "source_verification_digest", "source verification identity")
    require_scalar(environment$tool_source_ledger, "benchmark_root", "tool source ledger root")
  }
  require_scalar(environment$blas, "vendor", "BLAS vendor")
  require_scalar(environment$blas, "version_or_path", "BLAS version or path")
  require_scalar(environment$locale, "LC_ALL", "LC_ALL locale")
  configs <- environment$runner_configs
  if (is.null(configs) || length(configs) == 0L) stop("environment metadata has no runner configurations")
  for (config in configs) {
    require_scalar(config, "name", "runner name")
    require_scalar(config, "call_type", "runner call type")
    require_scalar(config, "tool_identity", "runner tool identity")
    require_scalar(config, "runner_config_digest", "runner configuration digest")
    require_scalar(config, "generated_glue_kind", "runner generated-glue kind")
    require_scalar(config, "generated_glue_digest", "runner generated-glue digest")
    if (environment_schema >= 3L) {
      require_scalar(config, "source_digest", "runner source digest")
      require_scalar(config, "build_digest", "runner build digest")
      require_scalar(config, "dependency_digest", "runner dependency digest")
      require_scalar(config, "artifact_dependency_digest", "runner artifact dependency digest")
      require_scalar(config, "source_ledger_identity_digest", "runner source ledger identity")
      require_scalar(config, "fixture_source_digest", "fixture source digest")
      require_scalar(config, "fixture_build_digest", "fixture build digest")
      require_scalar(config, "fixture_generated_glue_kind", "fixture generated-glue kind")
      require_scalar(config, "fixture_generated_glue_digest", "fixture generated-glue digest")
      require_scalar(config, "fixture_artifact_digest", "fixture artifact digest")
      require_scalar(config, "fixture_dependency_digest", "fixture dependency digest")
      require_scalar(config, "fixture_artifact_dependency_digest", "fixture artifact dependency digest")
      fixture_artifact_paths <- run_manifest_values(config$fixture_artifact_paths)
      if (length(fixture_artifact_paths) == 0L || any(!nzchar(fixture_artifact_paths))) {
        stop("environment metadata missing fixture artifact paths")
      }
    }
    require_scalar(config, "artifact_path", "runner artifact path")
    require_scalar(config, "artifact_digest", "runner artifact digest")
    artifact_paths <- run_manifest_values(config$artifact_paths)
    if (length(artifact_paths) == 0L || any(!nzchar(artifact_paths))) {
      stop("environment metadata missing runner artifact paths")
    }
  }
  invisible(environment)
}

validate_run_artifacts <- function(run_dir, metadata) {
  if (is.null(metadata$schema_version) || as.integer(metadata$schema_version) != 3L) {
    stop("unsupported run manifest schema version")
  }
  benchmark_artifact_layout(metadata)
  validate_environment_manifest(metadata$environment)
  if (as.integer(metadata$environment$schema_version) >= 3L) {
    validate_tool_source_ledger(
      as.character(metadata$environment$tool_source_ledger$benchmark_root),
      metadata$environment$tool_source_ledger
    )
  }
  expected_run_id <- as.character(metadata$run_id)
  expected_runners <- sort(run_manifest_values(metadata$runners))
  expected_tasks <- sort(run_manifest_values(metadata$tasks))
  suite <- benchmark_run_suite(metadata)
  task_selected <- benchmark_run_includes(metadata, "task")
  promotion_eligible <- benchmark_run_promotion_eligible(metadata)
  if (!is.null(metadata$promotion_eligible) && !identical(isTRUE(metadata$promotion_eligible), promotion_eligible)) {
    stop("run promotion eligibility differs from its suite, matrix, or measurement mode")
  }
  if (!is.null(metadata$timing_policy)) validate_timing_policy(metadata$timing_policy)
  if (!is.null(metadata$timing_policy) &&
      identical(as.character(metadata$timing_policy$policy_version), "bounded-pilot-confirmation-v2")) {
    if (is.null(metadata$timing_execution)) stop("run manifest has no frozen timing execution")
    validate_timing_execution(metadata$timing_execution, metadata)
  }
  if (length(expected_runners) == 0L) stop("run manifest has no runners")
  if (task_selected && length(expected_tasks) == 0L) stop("task suite run manifest has no tasks")
  if (!task_selected && length(expected_tasks) > 0L) stop("fixture-only run manifest declares task work")
  master_seed <- suppressWarnings(as.integer(metadata$master_seed))
  if (length(master_seed) != 1L || is.na(master_seed) || master_seed < 1L) stop("run manifest has an invalid master seed")
  input_relative_path <- if (is.null(metadata$input_manifest)) NULL else metadata$input_manifest$relative_path
  input_digest <- if (is.null(metadata$input_manifest)) NULL else metadata$input_manifest$digest
  input_payload_records <- list()
  input_payload_tasks <- character(0)
  if (task_selected) {
    if (is.null(input_relative_path) || length(input_relative_path) != 1L || !nzchar(as.character(input_relative_path)) ||
        is.null(input_digest) || length(input_digest) != 1L || !nzchar(as.character(input_digest))) {
      stop("run manifest has no canonical input identity")
    }
    input_path <- file.path(run_dir, as.character(input_relative_path))
    if (!file.exists(input_path)) stop("run canonical input manifest is missing")
    actual_input_digest <- unname(as.character(tools::md5sum(input_path))[[1L]])
    if (!identical(actual_input_digest, as.character(input_digest))) stop("run canonical input manifest digest differs")
    input_payload <- jsonlite::fromJSON(input_path, simplifyVector = FALSE)
    input_payload_records <- input_payload$tasks
    input_payload_tasks <- vapply(input_payload_records, function(record) as.character(record$task), character(1))
    if (!identical(sort(input_payload_tasks), expected_tasks) || anyDuplicated(input_payload_tasks)) {
      stop("canonical input manifest task set differs from the run manifest")
    }
  }
  if (task_selected && as.integer(metadata$schema_version) < 3L) {
    input_tasks <- vapply(metadata$task_inputs, function(record) as.character(record$task), character(1))
    if (!identical(sort(input_tasks), expected_tasks) || anyDuplicated(input_tasks)) {
      stop("run task input records differ from the declared task set")
    }
    for (task in expected_tasks) {
      metadata_input <- run_manifest_task_input(metadata, task)
      payload_input <- input_payload_records[[match(task, input_payload_tasks)]]
      for (field in c(
        "master_seed", "task_seed", "fixture_version", "contract_version",
        "mutation_policy", "altrep_intent", "fingerprint"
      )) {
        if (!identical(as.character(metadata_input[[field]]), as.character(payload_input[[field]]))) {
          stop(sprintf("run task input field %s differs from the canonical artifact for %s", field, task))
        }
      }
    }
  }
  r_runner_provenance <- run_manifest_r_provenance_records(metadata, "runner_rows")
  r_reference_provenance <- run_manifest_r_provenance_records(metadata, "reference_rows")
  if (!identical(sort(names(r_runner_provenance)), expected_tasks)) {
    stop("run R runner provenance differs from the declared task set")
  }
  if (length(setdiff(names(r_reference_provenance), expected_tasks)) > 0L) {
    stop("run R reference provenance contains undeclared tasks")
  }
  required_provenance_fields <- if (as.integer(metadata$schema_version) >= 3L) {
    c(
      "schema_version", "task", "function_name", "implementation_class", "source_digest",
      "backend_identity_keys", "record_digest"
    )
  } else c(
    "schema_version", "task", "function_name", "implementation_class", "source_digest",
    "source_body", "ast_calls", "ast_allowlist_id", "ast_allowlist", "forbidden_call_result",
    "compiled_backend", "backend_calls", "backend_classes", "backend_identity_keys"
  )
  for (record in c(r_runner_provenance, r_reference_provenance)) {
    missing <- required_provenance_fields[vapply(required_provenance_fields, function(field) is.null(record[[field]]), logical(1))]
    if (length(missing) > 0L) stop(sprintf("run R provenance is missing fields for %s: %s", record$task, paste(missing, collapse = ", ")))
    if (!(as.character(record$implementation_class) %in% c("pure_r", "optimized_base_r", "pure_r_unrepresentable"))) {
      stop(sprintf("run R provenance has an invalid implementation class for %s", record$task))
    }
    if (!nzchar(as.character(record$source_digest))) stop(sprintf("run R provenance lacks a source digest for %s", record$task))
    backend_keys <- as.character(unlist(record$backend_identity_keys, use.names = FALSE))
    allowed_backend_keys <- c(
      "none", "not_applicable", "r_runtime", "blas", "lapack", "fortran_runtime",
      "stats_package", "methods_package"
    )
    if (length(setdiff(backend_keys, allowed_backend_keys)) > 0L) {
      stop(sprintf("run R provenance has an unknown backend identity key for %s", record$task))
    }
    if (as.integer(metadata$environment$schema_version) >= 3L) {
      r_build <- metadata$environment$tool_source_ledger$r_build
      available <- list(
        r_runtime = r_build$lib_r,
        blas = r_build$blas,
        lapack = r_build$lapack,
        fortran_runtime = r_build$fortran_runtime,
        stats_package = r_build$stats_package,
        methods_package = r_build$methods_package
      )
      for (key in setdiff(backend_keys, c("none", "not_applicable"))) {
        identity <- available[[key]]
        resolved <- if (key %in% c("stats_package", "methods_package")) {
          !is.null(identity) && dir.exists(as.character(identity$library_path)) &&
            nzchar(as.character(identity$source_tree$digest))
        } else {
          !is.null(identity) && isTRUE(identity$exists) && nzchar(as.character(identity$md5))
        }
        if (!resolved) {
          stop(sprintf("run R provenance cannot resolve backend %s for %s", key, record$task))
        }
      }
    }
  }
  if (is.null(metadata$runner_dispositions) || !identical(sort(names(metadata$runner_dispositions)), expected_runners)) {
    stop("run manifest disposition runner set differs from the declared runner set")
  }
  for (runner in expected_runners) {
    runner_dispositions <- metadata$runner_dispositions[[runner]]
    disposition_tasks <- vapply(runner_dispositions, function(record) as.character(record$task), character(1))
    if (!identical(sort(disposition_tasks), expected_tasks) || anyDuplicated(disposition_tasks)) {
      stop(sprintf("run dispositions for %s differ from the declared task set", runner))
    }
    required_disposition_fields <- if (as.integer(metadata$schema_version) >= 3L) c(
      "task", "status", "executable", "reason", "evidence_use", "path_kind",
      "representation_strategy", "kernel_id", "contract_version", "timing_eligible"
    ) else c(
      "task", "status", "executable", "reason", "owner", "implementation_role", "evidence_use",
      "path_kind", "public_path", "representation_strategy", "kernel_id", "contract_version",
      "fixture_version", "comparison_tier", "mutation_policy", "setup_policy", "comparison_group",
      "timing_eligible"
    )
    for (record in runner_dispositions) {
      missing <- required_disposition_fields[vapply(
        required_disposition_fields,
        function(field) is.null(record[[field]]),
        logical(1)
      )]
      if (length(missing) > 0L) {
        stop(sprintf("run disposition is missing fields for %s/%s: %s", runner, record$task, paste(missing, collapse = ", ")))
      }
    }
    environment_records <- metadata$environment$runner_configs
    environment_matches <- environment_records[vapply(
      environment_records,
      function(record) identical(as.character(record$name), runner),
      logical(1)
    )]
    if (length(environment_matches) != 1L) stop(sprintf("environment identity is missing for runner %s", runner))
    artifact <- environment_matches[[1L]]
    artifact_paths <- run_manifest_values(artifact$artifact_paths)
    actual_artifact_digest <- tryCatch(
      run_manifest_artifact_digest(artifact_paths),
      error = function(error) ""
    )
    if (!identical(actual_artifact_digest, as.character(artifact$artifact_digest))) {
      stop(sprintf("artifact drift detected while completing runner %s", runner))
    }
    validate_fixture_artifact_identity(artifact)
  }
  staging_dir <- file.path(run_dir, ".staging")
  if (dir.exists(staging_dir) && length(list.files(staging_dir, all.files = TRUE, no.. = TRUE, recursive = TRUE)) > 0L) {
    stop("run contains unpromoted staging artifacts")
  }
  suite_artifacts <- function(universe) c(
    run_summary_artifact_paths(run_dir, metadata, universe, expected_runners),
    run_correctness_artifact_paths(run_dir, metadata, universe, expected_runners),
    run_sample_artifact_paths(run_dir, metadata, universe, expected_runners[[1L]], ".")
  )
  for (universe in c("task", "fixture")) {
    if (!benchmark_run_includes(metadata, universe) && any(file.exists(suite_artifacts(universe)))) {
      stop(sprintf("run contains artifacts for unselected %s suite", universe))
    }
  }
  if (!task_selected) return(invisible(TRUE))

  all_summary_files <- sort(list.files(run_dir, pattern = "^[^/]+_summary\\.csv$", full.names = TRUE))
  expected_files <- run_summary_artifact_paths(run_dir, metadata, "task", expected_runners)
  allowed_derived_files <- run_summary_artifact_paths(run_dir, metadata, "fixture", expected_runners)
  missing_files <- expected_files[!file.exists(expected_files)]
  unexpected_files <- setdiff(all_summary_files, c(expected_files, allowed_derived_files))
  if (length(missing_files) > 0L || length(unexpected_files) > 0L) {
    stop(sprintf(
      "run summary set differs from the run manifest; expected: %s; got: %s",
      paste(basename(expected_files), collapse = ", "),
      paste(basename(c(setdiff(expected_files, missing_files), unexpected_files)), collapse = ", ")
    ))
  }
  summaries <- read_run_summary_table(run_dir, metadata, "task", expected_runners)
  required <- c(
    "run_id", "runner", "task", "status",
    "correctness_status", "correctness_policy", "correctness_message",
    "master_seed", "task_seed", "input_fingerprint", "contract_version", "path_kind", "evidence_use",
    "r_implementation_provenance", "r_source_digest", "kernel_id", "representation_strategy",
    "mutation_policy", "tool_identity", "generated_glue_kind", "generated_glue_digest", "artifact_digest",
    "source_digest", "build_digest", "dependency_digest", "artifact_dependency_digest",
    "source_ledger_identity_digest", "source_path_class", "source_verification_digest",
    "disposition", "disposition_reason"
  )
  if (!is.null(metadata$timing_policy)) {
    timing_required <- if (is_bounded_timing_policy(metadata$timing_policy)) c(
      "warmup_iterations", "sample_stage", "fixed_iterations", "timer_noise_floor_ms",
      "timer_noise_status", "rss_endpoint_metric", "rss_endpoint_support",
      "rss_endpoint_support_reason", "gc_policy", "peak_rss_kb", "loaded_process_rss_kb",
      "peak_rss_metric", "peak_rss_support", "peak_rss_support_reason", "peak_rss_repetitions"
    ) else c(
      "warmup_iterations", "block_size", "max_iterations", "convergence_window_blocks",
      "convergence_cv_threshold_pct", "convergence_cv_pct", "stopping_condition", "converged",
      "timer_noise_floor_ms", "timer_noise_status", "rss_metric", "gc_policy"
    )
    required <- c(required, timing_required)
  }
  missing <- setdiff(required, names(summaries))
  if (length(missing) > 0L) stop(sprintf("run summaries missing columns: %s", paste(missing, collapse = ", ")))
  if (!all(as.character(summaries$run_id) == expected_run_id)) stop("run summaries contain mixed run IDs")

  if (!is.null(metadata$timing_policy)) {
    policy <- metadata$timing_policy
    bounded <- is_bounded_timing_policy(policy)
    numeric_policy_fields <- if (bounded) c("warmup_iterations", "timer_noise_floor_ms") else c(
      "warmup_iterations", "block_size", "max_iterations", "convergence_window_blocks",
      "convergence_cv_threshold_pct", "timer_noise_floor_ms"
    )
    for (name in numeric_policy_fields) {
      if (any(as.numeric(summaries[[name]]) != as.numeric(policy[[name]]))) {
        stop(sprintf("run summaries disagree with timing policy field %s", name))
      }
    }
    exact_policy_fields <- if (bounded) "gc_policy" else c("rss_metric", "gc_policy")
    for (name in exact_policy_fields) {
      if (any(as.character(summaries[[name]]) != as.character(policy[[name]]))) {
        stop(sprintf("run summaries disagree with timing policy field %s", name))
      }
    }
    pass_rows <- summaries$status == "PASS"
    if (bounded) {
      validate_rss_endpoint_support(summaries, pass_rows, policy, "task summaries")
      validate_first_call_metric(summaries, pass_rows, "task summaries")
      validate_peak_rss_support(summaries, policy, "task", "task summaries")
      if (any(!summaries$sample_stage[pass_rows] %in% c("pilot", "confirmation")) ||
          any(as.integer(summaries$n_iterations[pass_rows]) != as.integer(summaries$fixed_iterations[pass_rows]))) {
        stop("PASS summaries contain an invalid measured sample count")
      }
      pilot_rows <- pass_rows & summaries$sample_stage == "pilot"
      confirmation_rows <- pass_rows & summaries$sample_stage == "confirmation"
      if (any(summaries$n_iterations[pilot_rows] != as.integer(policy$pilot_iterations)) ||
          any(summaries$n_iterations[confirmation_rows] < as.integer(policy$confirmation_min_iterations)) ||
          any(summaries$n_iterations[confirmation_rows] > as.integer(policy$confirmation_max_iterations))) {
        stop("PASS summaries disagree with bounded timing policy")
      }
    } else {
      if (any(!summaries$stopping_condition[pass_rows] %in% c("rolling_cv", "max_iterations")) ||
          any(summaries$n_iterations[pass_rows] < 1L |
              summaries$n_iterations[pass_rows] > as.integer(policy$max_iterations) |
              summaries$n_iterations[pass_rows] %% as.integer(policy$block_size) != 0L)) {
        stop("legacy PASS summaries contain invalid adaptive timing evidence")
      }
    }
  }

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
  for (index in seq_len(nrow(summaries))) {
    row <- summaries[index, , drop = FALSE]
    runner <- as.character(row$runner)
    task <- as.character(row$task)
    disposition <- run_manifest_disposition(metadata, runner, task)
    input <- if (as.integer(metadata$schema_version) >= 3L) {
      input_payload_records[[match(task, input_payload_tasks)]]
    } else run_manifest_task_input(metadata, task)
    environment_records <- metadata$environment$runner_configs
    environment_matches <- environment_records[vapply(
      environment_records,
      function(record) identical(as.character(record$name), runner),
      logical(1)
    )]
    if (length(environment_matches) != 1L) stop(sprintf("environment identity is missing for runner %s", runner))
    environment <- environment_matches[[1L]]
    source_verification <- source_ledger_verification_record(
      metadata$environment$tool_source_ledger,
      runner,
      task
    )
    exact_fields <- list(
      master_seed = as.character(master_seed),
      task_seed = as.character(input$task_seed),
      input_fingerprint = as.character(input$fingerprint),
      contract_version = as.character(disposition$contract_version),
      path_kind = as.character(disposition$path_kind),
      evidence_use = as.character(disposition$evidence_use),
      kernel_id = as.character(disposition$kernel_id),
      representation_strategy = as.character(disposition$representation_strategy),
      mutation_policy = as.character(input$mutation_policy),
      tool_identity = as.character(environment$tool_identity),
      generated_glue_kind = as.character(environment$generated_glue_kind),
      generated_glue_digest = as.character(environment$generated_glue_digest),
      artifact_digest = as.character(environment$artifact_digest),
      source_digest = as.character(environment$source_digest),
      build_digest = as.character(environment$build_digest),
      dependency_digest = as.character(environment$dependency_digest),
      artifact_dependency_digest = as.character(environment$artifact_dependency_digest),
      source_ledger_identity_digest = as.character(environment$source_ledger_identity_digest),
      source_path_class = as.character(source_verification$source_class),
      source_verification_digest = as.character(source_verification$verification_digest),
      disposition = as.character(disposition$status),
      disposition_reason = as.character(disposition$reason)
    )
    for (field in names(exact_fields)) {
      if (!identical(as.character(row[[field]]), exact_fields[[field]])) {
        stop(sprintf("run summary identity field %s differs for %s/%s", field, runner, task))
      }
    }
    validate_summary_disposition(
      row$status,
      row$correctness_status,
      disposition,
      row$disposition_reason,
      runner,
      task
    )
    expected_r_provenance <- if (identical(runner, "r")) {
      as.character(r_runner_provenance[[task]]$implementation_class)
    } else if (!is.null(r_reference_provenance[[task]])) {
      paste0("reference:", as.character(r_reference_provenance[[task]]$implementation_class))
    } else {
      "not_applicable"
    }
    expected_r_digest <- if (identical(runner, "r")) {
      as.character(r_runner_provenance[[task]]$source_digest)
    } else if (!is.null(r_reference_provenance[[task]])) {
      as.character(r_reference_provenance[[task]]$source_digest)
    } else {
      "not_applicable"
    }
    if (!identical(as.character(row$r_implementation_provenance), expected_r_provenance) ||
        !identical(as.character(row$r_source_digest), expected_r_digest)) {
      stop(sprintf("run summary R provenance differs for %s/%s", runner, task))
    }
  }

  actual_runners <- sort(unique(as.character(summaries$runner)))
  if (!identical(expected_runners, actual_runners)) stop("run summaries contain an unexpected runner set")
  keys <- paste(summaries$runner, summaries$task, sep = "\r")
  if (anyDuplicated(keys)) stop("run summaries contain duplicate runner/task rows")

  benchmark_artifact_layout(metadata)
  shared_samples <- run_sample_artifact_paths(run_dir, metadata, "task", expected_runners[[1L]], ".")
  if (!identical(file.exists(shared_samples), any(summaries$status == "PASS"))) {
    stop("shared task timing artifact presence differs from PASS summaries")
  }
  if (any(dir.exists(file.path(run_dir, expected_runners)))) {
    stop("grouped run retains per-runner task artifact directories")
  }
  for (runner in expected_runners) {
    runner_tasks <- sort(unique(as.character(summaries$task[summaries$runner == runner])))
    if (!identical(expected_tasks, runner_tasks)) {
      stop(sprintf("run summary coverage for %s differs from the run manifest", runner))
    }

    expected_raw_tasks <- sort(as.character(summaries$task[summaries$runner == runner & summaries$status == "PASS"]))
    raw_samples <- read_run_sample_table(run_dir, metadata, "task", runner, expected_raw_tasks)
    required_raw <- c("run_id", "runner", "task", "iteration", "wall_ms")
    if (is_bounded_timing_policy(metadata$timing_policy)) required_raw <- c(
      required_raw, "stage", "process_epoch", "batch", "attempt", "group_order", "member_order",
      "excluded", "exclusion_reason", "rss_endpoint_delta_kb"
    )
    required_raw <- c(required_raw, "phase")
    if (nrow(raw_samples) > 0L && length(setdiff(required_raw, names(raw_samples))) > 0L) {
      stop(sprintf("raw timing samples have missing columns for %s", runner))
    }
    if (nrow(raw_samples) > 0L && !setequal(unique(as.character(raw_samples$phase)), c("first_call", "timed"))) {
      stop(sprintf("raw timing samples have invalid phases for %s", runner))
    }
    actual_raw_tasks <- sort(unique(as.character(raw_samples$task)))
    if (!identical(expected_raw_tasks, actual_raw_tasks)) {
      stop(sprintf("raw timing coverage for %s differs from PASS summaries", runner))
    }
    if (nrow(raw_samples) > 0L && (!all(as.character(raw_samples$run_id) == expected_run_id) ||
        !all(as.character(raw_samples$runner) == runner))) {
      stop("raw results contain mixed run or runner IDs")
    }
    if (nrow(raw_samples) > 0L && any(!is.finite(raw_samples$wall_ms) | raw_samples$wall_ms < 0)) {
      stop(sprintf("raw result has invalid wall_ms for %s", runner))
    }
    if (is_bounded_timing_policy(metadata$timing_policy) && nrow(raw_samples) > 0L && (
        any(!(as.character(raw_samples$stage) %in% c("pilot", "confirmation"))) ||
        any(as.integer(raw_samples$process_epoch) < 1L) || any(as.integer(raw_samples$batch) < 1L) ||
        any(!(as.integer(raw_samples$attempt) %in% 1:2)) ||
        any(as.integer(raw_samples$group_order) < 1L) || any(as.integer(raw_samples$member_order) < 1L))) {
      stop(sprintf("raw result has invalid bounded timing metadata for %s", runner))
    }
    if (is_bounded_timing_policy(metadata$timing_policy)) {
      excluded <- as.logical(raw_samples$excluded) %in% TRUE
      if (anyNA(as.logical(raw_samples$excluded)) ||
          any(excluded & (is.na(raw_samples$exclusion_reason) | !nzchar(as.character(raw_samples$exclusion_reason)))) ||
          any(!excluded & !is.na(raw_samples$exclusion_reason) & nzchar(as.character(raw_samples$exclusion_reason)))) {
        stop(sprintf("raw result has invalid timing exclusion evidence for %s", runner))
      }
    }
    for (task in expected_raw_tasks) {
      summary_row <- summaries[summaries$runner == runner & summaries$task == task, , drop = FALSE]
      raw <- raw_samples[as.character(raw_samples$task) == task, , drop = FALSE]
      if (is_bounded_timing_policy(metadata$timing_policy)) {
        raw <- raw[!(as.logical(raw$excluded) %in% TRUE), , drop = FALSE]
      }
      raw <- raw[as.character(raw$phase) == "timed", , drop = FALSE]
      if (is_bounded_timing_policy(metadata$timing_policy)) raw <- raw[
        as.character(raw$stage) == as.character(summary_row$sample_stage[[1L]]), , drop = FALSE
      ]
      if (nrow(summary_row) != 1L || nrow(raw) != summary_row$n_iterations[[1]]) {
        stop(sprintf("raw result sample count differs from summary: %s/%s", runner, task))
      }
      if (!identical(as.integer(raw$iteration), seq_len(nrow(raw)))) {
        stop(sprintf("raw result iterations differ from summary: %s/%s", runner, task))
      }
      if (!is.null(metadata$timing_policy)) {
        raw_mean <- mean(raw$wall_ms)
        raw_median <- median(raw$wall_ms)
        raw_min <- min(raw$wall_ms)
        raw_max <- max(raw$wall_ms)
        raw_sd <- sd(raw$wall_ms)
        raw_cv <- if (raw_mean > 0) raw_sd / raw_mean * 100 else 0
        expected_statistics <- c(
          mean_ms = round(raw_mean, 4), median_ms = round(raw_median, 4),
          min_ms = round(raw_min, 4), max_ms = round(raw_max, 4),
          sd_ms = round(raw_sd, 4), cv_pct = round(raw_cv, 2)
        )
        actual_statistics <- vapply(names(expected_statistics), function(field) {
          as.numeric(summary_row[[field]][[1L]])
        }, numeric(1))
        if (!identical(unname(actual_statistics), unname(expected_statistics))) {
          stop(sprintf("raw result statistics differ from summary: %s/%s", runner, task))
        }
        expected_noise <- if (raw_median < as.numeric(metadata$timing_policy$timer_noise_floor_ms)) "below_floor" else "above_floor"
        if (summary_row$timer_noise_status[[1]] != expected_noise) {
          stop(sprintf("raw result disagrees with timer-noise policy: %s/%s", runner, task))
        }
        if (is_bounded_timing_policy(metadata$timing_policy)) {
          validate_rss_endpoint_raw(summary_row, raw, paste(runner, task, sep = "/"))
        }
      }
    }
    if (!is.null(metadata$timing_policy)) {
      first_call <- raw_samples[as.character(raw_samples$phase) == "first_call", , drop = FALSE]
      if (is_bounded_timing_policy(metadata$timing_policy)) first_call <- first_call[
        !(as.logical(first_call$excluded) %in% TRUE) &
          as.character(first_call$stage) == as.character(summaries$sample_stage[
            match(paste(first_call$runner, first_call$task, sep = "\r"), paste(summaries$runner, summaries$task, sep = "\r"))
          ]), , drop = FALSE
      ]
      actual_first_call_tasks <- sort(as.character(first_call$task))
      if (!identical(expected_raw_tasks, actual_first_call_tasks)) {
        stop(sprintf("first-call samples differ from PASS summaries for %s", runner))
      }
      for (task in expected_raw_tasks) {
        validate_first_call_raw(
          summaries[summaries$runner == runner & summaries$task == task, , drop = FALSE],
          first_call[as.character(first_call$task) == task, , drop = FALSE],
          paste(runner, task, sep = "/")
        )
      }
    }
  }

  confirmation <- if (is_bounded_timing_policy(metadata$timing_policy)) {
    summaries[summaries$status == "PASS" & summaries$sample_stage == "confirmation", , drop = FALSE]
  } else summaries[0, , drop = FALSE]
  if (nrow(confirmation) > 0L) {
    counts <- split(as.integer(confirmation$n_iterations), as.character(confirmation$task))
    if (any(vapply(counts, function(value) length(unique(value)) != 1L, logical(1)))) {
      stop("confirmation task groups do not use symmetric frozen sample counts")
    }
  }

  invisible(TRUE)
}
