fixture_measurement_specs <- function() {
  complex_values <- function() {
    values <- complex(
      real = as.double(seq_len(32768L)),
      imaginary = as.double(seq_len(32768L)) * 0.5
    )
    values[[1L]] <- NA_complex_
    values[[2L]] <- complex(real = NaN, imaginary = 3)
    values
  }
  list(
    F01 = list(function_name = "fixture_zero", arguments = function() list()),
    F02 = list(function_name = "fixture_scalar", arguments = function() list(2.5)),
    F03 = list(
      function_name = "fixture_numeric",
      arguments = function() list(c(as.double(seq_len(100000L)) + 0.0, NA_real_, NaN))
    ),
    F04 = list(
      function_name = "fixture_altrep_integer",
      arguments = function() list(seq_len(100000L)),
      altrep_intent = "compact_integer_altrep"
    ),
    F05 = list(
      function_name = "fixture_strings",
      arguments = function() list(rep(fixture_encoded_strings(), 2000L))
    ),
    F06 = list(
      function_name = "fixture_raw",
      arguments = function() list(as.raw((seq_len(262144L) - 1L) %% 251L))
    ),
    F07 = list(function_name = "fixture_complex", arguments = function() list(complex_values())),
    F08 = list(
      function_name = "fixture_logical_counts",
      arguments = function() list(rep(c(FALSE, TRUE, NA), length.out = 100000L))
    ),
    F09 = list(function_name = "fixture_schema", arguments = function() list(fixture_schema_value())),
    F10 = list(
      function_name = c("fixture_new", "fixture_method", "fixture_read"),
      arguments = function() list(7L),
      stateful = TRUE
    ),
    F12 = list(function_name = "fixture_outputs", arguments = function() list())
  )
}

fixture_measurement_optimized_specs <- function() {
  list(
    F03 = list(function_name = "F03", implementation_class = "optimized_base_r"),
    F04 = list(function_name = "F04", implementation_class = "optimized_base_r")
  )
}

fixture_measurement_altrep_intent <- function(spec) {
  if (is.null(spec$altrep_intent)) "ordinary_r_object" else as.character(spec$altrep_intent)
}

fixture_measurement_requires_fresh_input <- function(spec) {
  isTRUE(spec$stateful) || !identical(fixture_measurement_altrep_intent(spec), "ordinary_r_object")
}

fixture_measurement_input_fingerprint <- function(fixture, spec) {
  task_arguments_fingerprint(
    paste0("fixture:", fixture),
    spec$arguments(),
    fixture_measurement_altrep_intent(spec)
  )
}

fixture_measurement_context <- function(root_dir, runner, evidence) {
  supported <- as.character(evidence$fixture_rows$fixture[
    evidence$fixture_rows$runner == runner & evidence$fixture_rows$executable
  ])
  if (runner %in% c("zigr", "rcpp", "cpp11", "extendr", "savvy")) {
    package <- fixture_package_map(root_dir)[[runner]]
    old_paths <- .libPaths()
    .libPaths(c(package$library, old_paths))
    if (package$package %in% loadedNamespaces()) unloadNamespace(package$package)
    loadNamespace(package$package, lib.loc = package$library)
    namespace <- asNamespace(package$package)
    public_names <- unique(unlist(fixture_function_map(), use.names = FALSE))
    functions <- lapply(public_names, function(name) {
      if (exists(name, envir = namespace, mode = "function", inherits = FALSE)) {
        get(name, envir = namespace, mode = "function", inherits = FALSE)
      } else {
        NULL
      }
    })
    names(functions) <- public_names
    close <- function() {
      gc(full = TRUE)
      if (package$package %in% loadedNamespaces()) unloadNamespace(package$package)
      .libPaths(old_paths)
      invisible(NULL)
    }
    return(list(functions = functions, optimized = list(), supported = supported, close = close))
  }
  if (identical(runner, "r")) {
    reference <- fixture_r_functions(root_dir)
    return(list(
      functions = reference$functions,
      optimized = reference$optimized,
      supported = supported,
      close = function() invisible(NULL)
    ))
  }
  if (identical(runner, "c_call")) {
    control <- fixture_c_context(root_dir)
    return(list(
      functions = control$functions,
      optimized = list(),
      supported = supported,
      close = control$close
    ))
  }
  stop(sprintf("no fixture measurement context for runner %s", runner))
}

fixture_measurement_prepare <- function(functions, fixture, spec, arguments = spec$arguments()) {
  force(arguments)
  if (isTRUE(spec$stateful)) {
    state <- functions$fixture_new()
    amount <- arguments[[1L]]
    return(function() functions$fixture_method(state, amount))
  }
  fn <- functions[[spec$function_name]]
  if (!is.function(fn)) stop(sprintf("fixture %s has no callable implementation", fixture))
  function() do.call(fn, arguments)
}

fixture_measurement_validate_case <- function(runner, functions, reference_functions, fixture, spec) {
  if (isTRUE(spec$stateful)) {
    state <- functions$fixture_new()
    amount <- spec$arguments()[[1L]]
    actual <- functions$fixture_method(state, amount)
    if (!identical(as.integer(actual), amount) ||
        !identical(as.integer(functions$fixture_read(state)), amount)) {
      stop(sprintf("fixture timing correctness failed for %s/%s", runner, fixture))
    }
    return(invisible(TRUE))
  }
  actual <- fixture_measurement_prepare(functions, fixture, spec)()
  expected <- do.call(reference_functions[[spec$function_name]], spec$arguments())
  fixture_assert_same(expected, actual, sprintf("fixture timing %s/%s", runner, fixture))
  invisible(TRUE)
}

fixture_measurement_validate_optimized <- function(optimized, reference_functions, fixture, spec) {
  fn <- optimized[[fixture]]
  if (!is.function(fn)) stop(sprintf("optimized R fixture %s is missing", fixture))
  arguments <- spec$arguments()
  actual <- do.call(fn, arguments)
  expected <- do.call(reference_functions[[spec$function_name]], spec$arguments())
  fixture_assert_same(expected, actual, sprintf("optimized R fixture timing %s", fixture))
  invisible(TRUE)
}

correctness_artifact_set_digest <- function(paths) {
  paths <- sort(normalizePath(paths, mustWork = TRUE))
  records <- lapply(paths, function(path) list(
    name = basename(path),
    size = unname(file.info(path)$size),
    md5 = unname(as.character(tools::md5sum(path))[[1L]])
  ))
  source_ledger_object_digest(records)
}

validate_correctness_artifacts <- function(run_dir, metadata, evidence) {
  expected_runners <- sort(run_manifest_values(metadata$runners))
  expected_tasks <- sort(run_manifest_values(metadata$tasks))
  task_files <- file.path(run_dir, "correctness", "tasks", paste0(expected_runners, ".csv"))
  fixture_files <- file.path(run_dir, "correctness", "fixtures", paste0(expected_runners, ".csv"))
  if (any(!file.exists(task_files)) || any(!file.exists(fixture_files))) {
    stop("correctness evidence files are incomplete")
  }

  tasks <- do.call(rbind, lapply(task_files, read.csv, stringsAsFactors = FALSE))
  task_required <- c(
    "run_id", "runner", "task", "status", "correctness_status", "correctness_policy",
    "correctness_message", "source_tree_digest", "source_ledger_identity_digest",
    "artifact_digest", "input_manifest_digest"
  )
  if (length(setdiff(task_required, names(tasks))) > 0L) stop("task correctness evidence columns differ")
  task_keys <- paste(tasks$runner, tasks$task, sep = "\r")
  expected_task_keys <- unlist(lapply(expected_runners, function(runner) {
    paste(runner, expected_tasks, sep = "\r")
  }), use.names = FALSE)
  if (!setequal(task_keys, expected_task_keys) || anyDuplicated(task_keys)) {
    stop("task correctness evidence coverage differs from the run manifest")
  }
  for (index in seq_len(nrow(tasks))) {
    row <- tasks[index, , drop = FALSE]
    runner <- as.character(row$runner)
    task <- as.character(row$task)
    disposition <- run_manifest_disposition(metadata, runner, task)
    environment <- runner_environment_record(metadata$environment, runner)
    expected_status <- if (isTRUE(disposition$executable)) "PASS" else "N/A"
    expected_correctness <- if (isTRUE(disposition$executable)) c("PASS", "REFERENCE") else "NOT_APPLICABLE"
    if (!identical(as.character(row$status), expected_status) ||
        !(as.character(row$correctness_status) %in% expected_correctness) ||
        !nzchar(as.character(row$correctness_message))) {
      stop(sprintf("task correctness evidence failed for %s/%s", runner, task))
    }
    if (!isTRUE(disposition$executable) &&
        !identical(as.character(row$correctness_message), as.character(disposition$reason))) {
      stop(sprintf("task correctness gap reason differs for %s/%s", runner, task))
    }
    exact <- list(
      run_id = as.character(metadata$run_id),
      source_tree_digest = as.character(metadata$environment$source_tree$digest),
      source_ledger_identity_digest = as.character(environment$source_ledger_identity_digest),
      artifact_digest = as.character(environment$artifact_digest),
      input_manifest_digest = as.character(metadata$input_manifest$digest)
    )
    for (field in names(exact)) {
      if (!identical(as.character(row[[field]]), exact[[field]])) {
        stop(sprintf("task correctness identity field %s differs for %s/%s", field, runner, task))
      }
    }
  }

  fixtures <- do.call(rbind, lapply(fixture_files, read.csv, stringsAsFactors = FALSE))
  fixture_required <- c(
    "run_id", "runner", "fixture", "variant", "row_id", "status", "correctness_status",
    "correctness_message", "source_tree_digest", "source_ledger_identity_digest", "artifact_digest"
  )
  if (length(setdiff(fixture_required, names(fixtures))) > 0L) {
    stop("fixture correctness evidence columns differ")
  }
  fixture_keys <- paste(fixtures$runner, fixtures$row_id, sep = "\r")
  expected_fixture_keys <- unlist(lapply(expected_runners, function(runner) {
    base <- paste(runner, evidence$fixtures, sep = "\r")
    if (identical(runner, "r")) {
      c(base, paste(runner, paste0(c("F03", "F04"), "_optimized_base_r"), sep = "\r"))
    } else base
  }), use.names = FALSE)
  if (!setequal(fixture_keys, expected_fixture_keys) || anyDuplicated(fixture_keys)) {
    stop("fixture correctness evidence coverage differs from the normalized matrix")
  }
  for (index in seq_len(nrow(fixtures))) {
    row <- fixtures[index, , drop = FALSE]
    runner <- as.character(row$runner)
    fixture <- as.character(row$fixture)
    optimized <- identical(as.character(row$variant), "optimized_base_r")
    evidence_row <- evidence$fixture_rows[
      evidence$fixture_rows$runner == runner & evidence$fixture_rows$fixture == fixture,
      , drop = FALSE
    ]
    if (nrow(evidence_row) != 1L) stop(sprintf("fixture correctness evidence is missing for %s/%s", runner, fixture))
    environment <- runner_environment_record(metadata$environment, runner)
    executable <- optimized || isTRUE(evidence_row$executable)
    expected_status <- if (executable) "PASS" else "N/A"
    expected_correctness <- if (executable) c("PASS", "REFERENCE") else "NOT_APPLICABLE"
    expected_variant <- if (optimized) "optimized_base_r" else "public"
    expected_row_id <- if (optimized) paste0(fixture, "_optimized_base_r") else fixture
    if (!identical(as.character(row$variant), expected_variant) ||
        !identical(as.character(row$row_id), expected_row_id) ||
        !identical(as.character(row$status), expected_status) ||
        !(as.character(row$correctness_status) %in% expected_correctness) ||
        !nzchar(as.character(row$correctness_message))) {
      stop(sprintf("fixture correctness evidence failed for %s/%s", runner, row$row_id))
    }
    if (!executable &&
        !identical(as.character(row$correctness_message), as.character(evidence_row$reason))) {
      stop(sprintf("fixture correctness gap reason differs for %s/%s", runner, fixture))
    }
    exact <- list(
      run_id = as.character(metadata$run_id),
      source_tree_digest = as.character(metadata$environment$source_tree$digest),
      source_ledger_identity_digest = as.character(environment$source_ledger_identity_digest),
      artifact_digest = as.character(environment$fixture_artifact_digest)
    )
    for (field in names(exact)) {
      if (!identical(as.character(row[[field]]), exact[[field]])) {
        stop(sprintf("fixture correctness identity field %s differs for %s/%s", field, runner, row$row_id))
      }
    }
  }

  list(
    task_rows = nrow(tasks),
    fixture_rows = nrow(fixtures),
    task_artifact_digest = correctness_artifact_set_digest(task_files),
    fixture_artifact_digest = correctness_artifact_set_digest(fixture_files)
  )
}

validate_fixture_raw_statistics <- function(summary, raw, policy, label) {
  median_ms <- median(raw$wall_ms)
  mean_ms <- mean(raw$wall_ms)
  min_ms <- min(raw$wall_ms)
  max_ms <- max(raw$wall_ms)
  sd_ms <- sd(raw$wall_ms)
  cv_pct <- if (mean_ms > 0) sd_ms / mean_ms * 100 else 0
  if (!identical(as.numeric(summary$mean_ms), round(mean_ms, 4)) ||
      !identical(as.numeric(summary$median_ms), round(median_ms, 4)) ||
      !identical(as.numeric(summary$min_ms), round(min_ms, 4)) ||
      !identical(as.numeric(summary$max_ms), round(max_ms, 4)) ||
      !identical(as.numeric(summary$sd_ms), round(sd_ms, 4)) ||
      !identical(as.numeric(summary$cv_pct), round(cv_pct, 2))) {
    stop(sprintf("fixture raw statistics differ for %s", label))
  }
  expected_noise <- if (median_ms < as.numeric(policy$timer_noise_floor_ms)) "below_floor" else "above_floor"
  window_size <- as.integer(policy$block_size) * as.integer(policy$convergence_window_blocks)
  convergence_window <- tail(raw$wall_ms, window_size)
  convergence_mean <- mean(convergence_window)
  convergence_cv <- if (convergence_mean > 0) {
    sd(convergence_window) / convergence_mean * 100
  } else 0
  if (!identical(as.character(summary$timer_noise_status), expected_noise) ||
      !isTRUE(all.equal(as.numeric(summary$convergence_cv_pct), convergence_cv, tolerance = 1e-12))) {
    stop(sprintf("fixture convergence or timer-noise evidence differs for %s", label))
  }
  invisible(TRUE)
}

validate_fixture_measurement_artifacts <- function(run_dir, metadata, evidence) {
  expected_runners <- sort(run_manifest_values(metadata$runners))
  expected_files <- file.path(run_dir, paste0("fixture_", expected_runners, "_summary.csv"))
  if (any(!file.exists(expected_files))) stop("fixture measurement summaries are incomplete")
  summaries <- do.call(rbind, lapply(expected_files, read.csv, stringsAsFactors = FALSE))
  required <- c(
    "run_id", "runner", "fixture", "variant", "row_id", "status", "correctness_status",
    "correctness_message", "input_fingerprint", "implementation_role", "evidence_use",
    "path_kind", "representation_strategy", "comparison_tier", "setup_policy", "timing_eligible",
    "kernel_id", "contract_version", "fixture_version", "comparison_group", "tool_identity",
    "fixture_source_digest", "fixture_build_digest", "fixture_generated_glue_kind",
    "fixture_generated_glue_digest", "fixture_artifact_digest", "fixture_dependency_digest",
    "fixture_artifact_dependency_digest", "source_ledger_identity_digest", "mean_ms", "median_ms",
    "min_ms", "max_ms", "sd_ms", "cv_pct", "rss_kb", "cold_start_ms", "n_iterations",
    "warmup_iterations", "block_size", "max_iterations", "convergence_window_blocks",
    "convergence_cv_threshold_pct", "convergence_cv_pct", "stopping_condition", "converged",
    "timer_noise_floor_ms", "timer_noise_status", "rss_metric", "gc_policy"
  )
  missing <- setdiff(required, names(summaries))
  if (length(missing) > 0L) stop(sprintf("fixture summaries missing columns: %s", paste(missing, collapse = ", ")))
  if (!all(as.character(summaries$run_id) == as.character(metadata$run_id))) {
    stop("fixture summaries contain mixed run IDs")
  }
  expected_keys <- unlist(lapply(expected_runners, function(runner) {
    base <- paste(runner, evidence$fixtures, sep = "\r")
    if (identical(runner, "r")) c(base, paste(runner, paste0(c("F03", "F04"), "_optimized_base_r"), sep = "\r")) else base
  }), use.names = FALSE)
  actual_keys <- paste(summaries$runner, summaries$row_id, sep = "\r")
  if (!setequal(expected_keys, actual_keys) || anyDuplicated(actual_keys)) {
    stop("fixture summary coverage differs from the normalized matrix")
  }
  specs <- fixture_measurement_specs()
  for (index in seq_len(nrow(summaries))) {
    summary <- summaries[index, , drop = FALSE]
    runner <- as.character(summary$runner)
    fixture <- as.character(summary$fixture)
    variant <- as.character(summary$variant)
    optimized <- identical(variant, "optimized_base_r")
    evidence_row <- evidence$fixture_rows[
      evidence$fixture_rows$runner == runner & evidence$fixture_rows$fixture == fixture,
      , drop = FALSE
    ]
    if (nrow(evidence_row) != 1L) stop(sprintf("fixture evidence is missing for %s/%s", runner, fixture))
    environment <- runner_environment_record(metadata$environment, runner)
    validate_fixture_artifact_identity(environment)
    spec <- specs[[fixture]]
    expected_fingerprint <- if (is.null(spec)) {
      "not_applicable"
    } else {
      fixture_measurement_input_fingerprint(fixture, spec)
    }
    expected <- list(
      variant = if (optimized) "optimized_base_r" else "public",
      row_id = if (optimized) paste0(fixture, "_optimized_base_r") else fixture,
      input_fingerprint = expected_fingerprint,
      implementation_role = if (optimized) "optimized_base_r" else as.character(evidence_row$implementation_role),
      evidence_use = if (optimized) "timed_baseline" else as.character(evidence_row$evidence_use),
      path_kind = if (optimized) "optimized_base_r" else as.character(evidence_row$path_kind),
      representation_strategy = if (optimized) "runtime_service" else as.character(evidence_row$representation_strategy),
      comparison_tier = if (optimized) "tier_c" else as.character(evidence_row$comparison_tier),
      setup_policy = if (optimized) "setup_outside_timer" else as.character(evidence_row$setup_policy),
      kernel_id = if (optimized) paste0("normalized:", fixture, ":optimized-base-r-v1") else as.character(evidence_row$kernel_id),
      contract_version = as.character(evidence_row$contract_version),
      fixture_version = as.character(evidence_row$fixture_version),
      comparison_group = if (optimized) paste0("normalized:", fixture, ":optimized-base-r") else as.character(evidence_row$comparison_group),
      tool_identity = as.character(environment$tool_identity),
      fixture_source_digest = as.character(environment$fixture_source_digest),
      fixture_build_digest = as.character(environment$fixture_build_digest),
      fixture_generated_glue_kind = as.character(environment$fixture_generated_glue_kind),
      fixture_generated_glue_digest = as.character(environment$fixture_generated_glue_digest),
      fixture_artifact_digest = as.character(environment$fixture_artifact_digest),
      fixture_dependency_digest = as.character(environment$fixture_dependency_digest),
      fixture_artifact_dependency_digest = as.character(environment$fixture_artifact_dependency_digest),
      source_ledger_identity_digest = as.character(environment$source_ledger_identity_digest),
      timing_eligible = if (optimized) "TRUE" else as.character(isTRUE(evidence_row$timing_eligible))
    )
    for (field in names(expected)) {
      if (!identical(as.character(summary[[field]]), expected[[field]])) {
        stop(sprintf("fixture summary field %s differs for %s/%s", field, runner, summary$row_id))
      }
    }
    admitted <- optimized || isTRUE(evidence_row$timing_eligible)
    if (admitted) {
      if (!identical(as.character(summary$status), "PASS") ||
          !(as.character(summary$correctness_status) %in% c("PASS", "REFERENCE")) ||
          !nzchar(as.character(summary$correctness_message))) {
        stop(sprintf("timed fixture lacks passing correctness for %s/%s", runner, summary$row_id))
      }
    } else if (isTRUE(evidence_row$executable)) {
      if (!identical(as.character(summary$status), "CORRECTNESS_ONLY") ||
          !(as.character(summary$correctness_status) %in% c("PASS", "REFERENCE"))) {
        stop(sprintf("correctness-only fixture status differs for %s/%s", runner, fixture))
      }
    } else if (!identical(as.character(summary$status), "N/A") ||
               !identical(as.character(summary$correctness_status), "NOT_APPLICABLE")) {
      stop(sprintf("fixture gap status differs for %s/%s", runner, fixture))
    }
  }
  pass <- summaries[summaries$status == "PASS", , drop = FALSE]
  policy <- metadata$timing_policy
  if (any(as.integer(pass$warmup_iterations) != as.integer(policy$warmup_iterations)) ||
      any(as.integer(pass$block_size) != as.integer(policy$block_size)) ||
      any(as.integer(pass$max_iterations) != as.integer(policy$max_iterations)) ||
      any(as.integer(pass$convergence_window_blocks) != as.integer(policy$convergence_window_blocks)) ||
      any(as.numeric(pass$convergence_cv_threshold_pct) != as.numeric(policy$convergence_cv_threshold_pct)) ||
      any(as.numeric(pass$timer_noise_floor_ms) != as.numeric(policy$timer_noise_floor_ms)) ||
      any(as.character(pass$rss_metric) != as.character(policy$rss_metric)) ||
      any(as.character(pass$gc_policy) != as.character(policy$gc_policy))) {
    stop("fixture summaries disagree with the timing policy")
  }
  numeric_fields <- c(
    "mean_ms", "median_ms", "min_ms", "max_ms", "sd_ms", "cv_pct", "rss_kb", "cold_start_ms",
    "convergence_cv_pct"
  )
  if (any(!vapply(pass[numeric_fields], function(values) all(is.finite(values) & values >= 0), logical(1)))) {
    stop("fixture PASS summaries contain invalid timing statistics")
  }
  if (any(pass$n_iterations < 1L |
          pass$n_iterations > as.integer(policy$max_iterations) |
          pass$n_iterations %% as.integer(policy$block_size) != 0L)) {
    stop("fixture PASS summaries contain an invalid measured sample count")
  }
  if (any(!(pass$stopping_condition %in% c("rolling_cv", "max_iterations")))) {
    stop("fixture PASS summaries contain an invalid stopping condition")
  }
  max_rows <- pass$stopping_condition == "max_iterations"
  if (any(pass$n_iterations[max_rows] != as.integer(policy$max_iterations)) ||
      any(pass$converged[max_rows])) {
    stop("fixture max-iteration summaries disagree with the timing policy")
  }
  converged_rows <- pass$stopping_condition == "rolling_cv"
  if (any(!pass$converged[converged_rows])) {
    stop("fixture rolling-CV summaries are not marked converged")
  }
  not_measured <- summaries$status != "PASS"
  not_measured_numeric <- c(
    "mean_ms", "median_ms", "min_ms", "max_ms", "sd_ms", "cv_pct", "rss_kb", "cold_start_ms",
    "n_iterations", "convergence_cv_pct"
  )
  if (any(!vapply(summaries[not_measured, not_measured_numeric, drop = FALSE], function(values) {
    all(is.na(values))
  }, logical(1))) ||
      any(as.character(summaries$stopping_condition[not_measured]) != "not_measured") ||
      any(!is.na(summaries$converged[not_measured])) ||
      any(as.character(summaries$timer_noise_status[not_measured]) != "not_measured")) {
    stop("untimed fixture summaries contain measurement evidence")
  }
  for (runner in expected_runners) {
    runner_rows <- pass[pass$runner == runner, , drop = FALSE]
    raw_dir <- file.path(run_dir, "fixtures", runner)
    raw_files <- sort(list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE))
    expected_raw <- sort(paste0(as.character(runner_rows$row_id), ".csv"))
    if (!identical(sort(basename(raw_files)), expected_raw)) {
      stop(sprintf("fixture raw timing coverage differs for %s", runner))
    }
    for (path in raw_files) {
      raw <- read.csv(path, stringsAsFactors = FALSE)
      row_id <- sub("\\.csv$", "", basename(path))
      summary <- runner_rows[runner_rows$row_id == row_id, , drop = FALSE]
      required_raw <- c("run_id", "runner", "fixture", "variant", "row_id", "iteration", "wall_ms")
      if (length(setdiff(required_raw, names(raw))) > 0L) {
        stop(sprintf("fixture raw timing columns differ for %s/%s", runner, row_id))
      }
      identity_matches <- nrow(summary) == 1L &&
        all(as.character(raw$run_id) == as.character(metadata$run_id)) &&
        all(as.character(raw$runner) == runner) &&
        all(as.character(raw$fixture) == as.character(summary$fixture)) &&
        all(as.character(raw$variant) == as.character(summary$variant)) &&
        all(as.character(raw$row_id) == row_id)
      if (!identity_matches || nrow(raw) != as.integer(summary$n_iterations) ||
          !identical(as.integer(raw$iteration), seq_len(nrow(raw))) ||
          any(!is.finite(raw$wall_ms) | raw$wall_ms < 0)) {
        stop(sprintf("fixture raw sample count differs for %s/%s", runner, row_id))
      }
      validate_fixture_raw_statistics(summary, raw, policy, paste(runner, row_id, sep = "/"))
    }
  }
  invisible(TRUE)
}
