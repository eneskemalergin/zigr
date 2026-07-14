run_manifest_path <- function(run_dir) file.path(run_dir, "run_manifest.json")

run_manifest_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

run_manifest_values <- function(value) {
  if (is.null(value)) character(0) else as.character(unlist(value, use.names = FALSE))
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

run_manifest_r_provenance_records <- function(metadata, field) {
  provenance <- metadata$r_provenance
  if (is.null(provenance) || !identical(as.character(provenance$schema_version), r_provenance_schema_version())) {
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
  if (is.null(metadata$schema_version) || as.integer(metadata$schema_version) != 2L) {
    stop(sprintf("unsupported run manifest schema version: %s", path))
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
  if (is.null(metadata$schema_version) || as.integer(metadata$schema_version) != 2L) {
    stop("unsupported run manifest schema version")
  }
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
  if (!is.null(metadata$timing_policy)) validate_timing_policy(metadata$timing_policy)
  if (length(expected_runners) == 0L) stop("run manifest has no runners")
  if (length(expected_tasks) == 0L) stop("run manifest has no tasks")
  master_seed <- suppressWarnings(as.integer(metadata$master_seed))
  if (length(master_seed) != 1L || is.na(master_seed) || master_seed < 1L) stop("run manifest has an invalid master seed")
  input_relative_path <- if (is.null(metadata$input_manifest)) NULL else metadata$input_manifest$relative_path
  input_digest <- if (is.null(metadata$input_manifest)) NULL else metadata$input_manifest$digest
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
  r_runner_provenance <- run_manifest_r_provenance_records(metadata, "runner_rows")
  r_reference_provenance <- run_manifest_r_provenance_records(metadata, "reference_rows")
  if (!identical(sort(names(r_runner_provenance)), expected_tasks)) {
    stop("run R runner provenance differs from the declared task set")
  }
  if (length(setdiff(names(r_reference_provenance), expected_tasks)) > 0L) {
    stop("run R reference provenance contains undeclared tasks")
  }
  required_provenance_fields <- c(
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
    required_disposition_fields <- c(
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

  all_summary_files <- sort(list.files(run_dir, pattern = "^[^/]+_summary\\.csv$", full.names = TRUE))
  expected_files <- file.path(run_dir, paste0(expected_runners, "_summary.csv"))
  allowed_derived_files <- c(
    file.path(run_dir, "analysis_summary.csv"),
    file.path(run_dir, paste0("fixture_", expected_runners, "_summary.csv"))
  )
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
    "correctness_status", "correctness_policy", "correctness_message",
    "master_seed", "task_seed", "input_fingerprint", "contract_version", "path_kind", "evidence_use",
    "r_implementation_provenance", "r_source_digest", "kernel_id", "representation_strategy",
    "mutation_policy", "tool_identity", "generated_glue_kind", "generated_glue_digest", "artifact_digest",
    "source_digest", "build_digest", "dependency_digest", "artifact_dependency_digest",
    "source_ledger_identity_digest", "source_path_class", "source_verification_digest",
    "disposition", "disposition_reason"
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

  if (!is.null(metadata$timing_policy)) {
    policy <- metadata$timing_policy
    numeric_policy_fields <- c(
      "warmup_iterations", "block_size", "max_iterations", "convergence_window_blocks",
      "convergence_cv_threshold_pct", "timer_noise_floor_ms"
    )
    for (name in numeric_policy_fields) {
      if (any(as.numeric(summaries[[name]]) != as.numeric(policy[[name]]))) {
        stop(sprintf("run summaries disagree with timing policy field %s", name))
      }
    }
    for (name in c("rss_metric", "gc_policy")) {
      if (any(as.character(summaries[[name]]) != as.character(policy[[name]]))) {
        stop(sprintf("run summaries disagree with timing policy field %s", name))
      }
    }
    pass_rows <- summaries$status == "PASS"
    if (any(!summaries$stopping_condition[pass_rows] %in% c("rolling_cv", "max_iterations"))) {
      stop("PASS summaries contain an invalid stopping condition")
    }
    if (any(summaries$n_iterations[pass_rows] < 1L |
            summaries$n_iterations[pass_rows] > as.integer(policy$max_iterations) |
            summaries$n_iterations[pass_rows] %% as.integer(policy$block_size) != 0L)) {
      stop("PASS summaries contain an invalid measured sample count")
    }
    max_rows <- pass_rows & summaries$stopping_condition == "max_iterations"
    if (any(summaries$n_iterations[max_rows] != as.integer(policy$max_iterations)) || any(summaries$converged[max_rows])) {
      stop("max-iteration summaries disagree with the timing policy")
    }
    converged_rows <- pass_rows & summaries$stopping_condition == "rolling_cv"
    if (any(!summaries$converged[converged_rows])) stop("rolling-CV summaries are not marked converged")
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
    input <- run_manifest_task_input(metadata, task)
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
      if (!"wall_ms" %in% names(raw) || any(!is.finite(raw$wall_ms))) stop(sprintf("raw result has invalid wall_ms: %s", raw_file))
      task <- sub("\\.csv$", "", sub("^task_", "", basename(raw_file)))
      summary_row <- summaries[summaries$runner == runner & summaries$task == task, , drop = FALSE]
      if (nrow(summary_row) != 1L || nrow(raw) != summary_row$n_iterations[[1]]) {
        stop(sprintf("raw result sample count differs from summary: %s", raw_file))
      }
      if (!is.null(metadata$boundary_budget_policy_version)) {
        raw_median <- median(raw$wall_ms)
        raw_mean <- mean(raw$wall_ms)
        raw_cv <- if (raw_mean > 0) sd(raw$wall_ms) / raw_mean * 100 else 0
        if (summary_row$median_ms[[1]] != round(raw_median, 4) || summary_row$cv_pct[[1]] != round(raw_cv, 2)) {
          stop(sprintf("raw result statistics differ from summary: %s", raw_file))
        }
        expected_noise <- if (raw_median < as.numeric(metadata$timing_policy$timer_noise_floor_ms)) "below_floor" else "above_floor"
        if (summary_row$timer_noise_status[[1]] != expected_noise) {
          stop(sprintf("raw result disagrees with timer-noise policy: %s", raw_file))
        }
      }
    }
    if (!is.null(metadata$timing_policy)) {
      cold_file <- file.path(runner_dir, "cold_start.csv")
      if (length(expected_raw_tasks) == 0L) {
        if (file.exists(cold_file)) stop(sprintf("cold-start results exist without PASS rows for %s", runner))
        next
      }
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
