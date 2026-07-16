#!/usr/bin/env Rscript

library(methods)
library(jsonlite)

root_dir <- normalizePath(".")
source(file.path(root_dir, "lib", "specification.R"))
source(file.path(root_dir, "lib", "measurement.R"))
source(file.path(root_dir, "lib", "provenance.R"))
source(file.path(root_dir, "lib", "run_manifest.R"))
source(file.path(root_dir, "lib", "product_fixtures.R"))

parse_named_integer_map <- function(value, label) {
  entries <- strsplit(as.character(value), ",", fixed = TRUE)[[1L]]
  parts <- strsplit(entries, "=", fixed = TRUE)
  if (length(parts) == 0L || any(lengths(parts) != 2L)) stop(sprintf("%s must use id=integer entries", label))
  ids <- vapply(parts, `[[`, character(1), 1L)
  values <- suppressWarnings(as.integer(vapply(parts, `[[`, character(1), 2L)))
  if (any(!nzchar(ids)) || anyDuplicated(ids) || anyNA(values) || any(values < 1L)) {
    stop(sprintf("%s contains an invalid entry", label))
  }
  stats::setNames(values, ids)
}

timing_worker_options <- function(args) {
  value <- function(name) {
    matches <- grep(paste0("^--", name, "="), args, value = TRUE)
    if (length(matches) == 0L) return(NULL)
    sub(paste0("^--", name, "="), "", matches[[1L]])
  }
  stage <- value("timing-stage")
  counts <- value("timing-counts")
  output <- value("batch-output")
  if (sum(vapply(list(stage, counts, output), is.null, logical(1))) %in% c(1L, 2L)) {
    stop("timing-stage, timing-counts, and batch-output must be supplied together")
  }
  if (is.null(stage)) return(NULL)
  if (!(stage %in% c("pilot", "confirmation"))) stop("timing stage must be pilot or confirmation")
  scalar_integer <- function(name) input_scalar_integer(value(name), name)
  list(
    stage = stage, counts = parse_named_integer_map(counts, "timing counts"),
    output = normalizePath(output, mustWork = FALSE), batch = scalar_integer("batch"),
    attempt = scalar_integer("attempt"), process_epoch = scalar_integer("process-epoch"),
    member_order = scalar_integer("member-order"),
    group_orders = parse_named_integer_map(value("group-orders"), "group orders")
  )
}

run_task_worker <- function(cli) {
  timing_options <- timing_worker_options(cli)
  runner_name <- NA
  task_filter <- NULL
  task_id_filter <- NULL
  check_only <- FALSE
  validation_only <- FALSE
  validation_output_arg <- NULL
  validated_correctness_arg <- NULL
  results_dir_arg <- NULL
  prepare_inputs_arg <- NULL
  input_manifest_arg <- NULL
  expected_input_manifest_digest <- NULL
  master_seed_arg <- NULL
  for (a in cli) {
    if (grepl("^--runner=", a)) runner_name <- sub("^--runner=", "", a)
    if (grepl("^--tasks=", a)) task_filter <- parse_task_filter(sub("^--tasks=", "", a))
    if (grepl("^--task-ids=", a)) {
      task_id_filter <- parse_csv_option(sub("^--task-ids=", "", a), "task ID filter")
    }
    if (a == "--check-only") check_only <- TRUE
    if (a == "--validation-only") validation_only <- TRUE
    if (grepl("^--validation-output=", a)) validation_output_arg <- sub("^--validation-output=", "", a)
    if (grepl("^--validated-correctness=", a)) {
      validated_correctness_arg <- sub("^--validated-correctness=", "", a)
    }
    if (grepl("^--results-dir=", a)) results_dir_arg <- sub("^--results-dir=", "", a)
    if (grepl("^--prepare-inputs=", a)) prepare_inputs_arg <- sub("^--prepare-inputs=", "", a)
    if (grepl("^--input-manifest=", a)) input_manifest_arg <- sub("^--input-manifest=", "", a)
    if (grepl("^--expected-input-manifest-digest=", a)) {
      expected_input_manifest_digest <- sub("^--expected-input-manifest-digest=", "", a)
    }
    if (grepl("^--master-seed=", a)) master_seed_arg <- sub("^--master-seed=", "", a)
  }
  if (is.na(runner_name) && is.null(prepare_inputs_arg)) stop("--runner= required")
  if (!is.null(task_filter) && !is.null(task_id_filter)) {
    stop("--tasks and --task-ids cannot be combined")
  }
  if (is.na(runner_name)) runner_name <- "r"
  if (validation_only && is.null(validation_output_arg)) stop("--validation-output= is required with --validation-only")
  if (validation_only && !is.null(validated_correctness_arg)) {
    stop("validation-only mode cannot reuse retained correctness")
  }
  if (!check_only && !validation_only && is.null(prepare_inputs_arg) && is.null(timing_options)) {
    stop("timed task execution requires bounded timing options")
  }
  
  runner_configs <- load_runner_configs(root_dir)
  if (!(runner_name %in% names(runner_configs))) stop(sprintf("runner config not found: %s", runner_name))
  cfg <- runner_configs[[runner_name]]
  if (!check_only && is.null(prepare_inputs_arg) && is.null(results_dir_arg)) {
    stop("--results-dir= is required for benchmark execution")
  }
  if (!is.null(prepare_inputs_arg)) results_dir_arg <- NULL
  results_dir <- normalizePath(if (is.null(results_dir_arg)) file.path(root_dir, "results") else results_dir_arg, mustWork = FALSE)
  run_id <- NA_character_
  timing_policy <- benchmark_timing_policy()
  run_metadata <- NULL
  if (!check_only && is.null(prepare_inputs_arg)) {
    run_metadata <- read_run_manifest(results_dir)
    run_id <- as.character(run_metadata$run_id)
    if (!is.null(run_metadata$timing_policy)) timing_policy <- run_metadata$timing_policy
    validate_timing_policy(timing_policy)
    if (!(runner_name %in% run_manifest_values(run_metadata$runners))) {
      stop(sprintf("runner %s is not declared by run manifest %s", runner_name, run_manifest_path(results_dir)))
    }
    staging_results_dir <- if (validation_only) dirname(validation_output_arg) else timing_options$output
    dir.create(file.path(staging_results_dir, runner_name), recursive = TRUE, showWarnings = FALSE)
    unlink(file.path(staging_results_dir, runner_name, "errors.csv"))
  }
  
  all_tasks <- benchmark_task_specs()
  manifest <- load_task_manifest(root_dir)
  evidence <- load_evidence_manifest(root_dir, manifest)
  cfg <- hydrate_runner_config(manifest, cfg, runner_name, evidence)
  validate_task_specs(manifest, all_tasks)
  all_tasks <- order_task_specs(manifest, all_tasks)
  
  if (!is.null(task_filter)) {
    task_ids <- vapply(all_tasks, function(task) task$id, character(1))
    selected_ids <- select_task_ids(task_ids, task_filter)
    all_tasks <- all_tasks[task_ids %in% selected_ids]
  }
  if (!is.null(task_id_filter)) {
    task_ids <- vapply(all_tasks, function(task) task$id, character(1))
    all_tasks <- all_tasks[ordered_selection(task_ids, task_id_filter, "task ID filter")]
    if (length(all_tasks) == 0L) stop("task ID filter selected no manifest tasks")
  }
  
  if (!is.null(prepare_inputs_arg)) {
    master_seed <- if (is.null(master_seed_arg)) benchmark_master_seed() else input_scalar_integer(master_seed_arg, "master seed")
    write_input_recipe_manifest(
      normalizePath(prepare_inputs_arg, mustWork = FALSE),
      all_tasks,
      manifest,
      evidence,
      master_seed
    )
    cat(sprintf("Canonical input recipes written to %s\n", normalizePath(prepare_inputs_arg, mustWork = FALSE)))
    quit(save = "no", status = 0, runLast = FALSE)
  }
  
  input_recipes <- NULL
  master_seed <- NULL
  runner_environment <- NULL
  if (!check_only) {
    if (is.null(input_manifest_arg) || is.null(expected_input_manifest_digest) || is.null(master_seed_arg)) {
      stop("benchmark execution requires --input-manifest, --expected-input-manifest-digest, and --master-seed")
    }
    declared_input_path <- normalizePath(file.path(results_dir, as.character(run_metadata$input_manifest$relative_path)), mustWork = FALSE)
    supplied_input_path <- normalizePath(input_manifest_arg, mustWork = FALSE)
    if (!identical(declared_input_path, supplied_input_path)) stop("canonical input manifest location differs from the run manifest")
    if (!identical(as.character(run_metadata$input_manifest$digest), expected_input_manifest_digest)) {
      stop("expected canonical input manifest digest differs from the run manifest")
    }
    validate_input_manifest_digest(supplied_input_path, expected_input_manifest_digest)
    master_seed <- input_scalar_integer(master_seed_arg, "master seed")
    if (!identical(master_seed, input_scalar_integer(run_metadata$master_seed, "run manifest master seed"))) {
      stop("master seed differs from the run manifest")
    }
    input_recipes <- read_input_recipe_manifest(supplied_input_path)
    if (!identical(master_seed, input_scalar_integer(input_recipes$master_seed, "canonical input master seed"))) {
      stop("master seed differs from the canonical input manifest")
    }
    selected_ids <- run_manifest_values(run_metadata$tasks)
    if (!identical(sort(names(input_recipes$tasks)), sort(selected_ids))) {
      stop("canonical input task set differs from the runner task set")
    }
    runner_environment <- runner_environment_record(run_metadata$environment, runner_name)
    validate_runner_artifact_identity(root_dir, runner_environment)
    validate_tool_source_ledger(root_dir, run_metadata$environment$tool_source_ledger, runner_name)
  }
  
  retained_correctness <- NULL
  if (!is.null(validated_correctness_arg)) {
    expected_path <- normalizePath(
      run_correctness_artifact_paths(results_dir, run_metadata, "task", runner_name),
      mustWork = TRUE
    )
    expected_tasks <- run_manifest_values(run_metadata$tasks)
    executable_tasks <- expected_tasks[vapply(expected_tasks, function(task) {
      isTRUE(run_manifest_disposition(run_metadata, runner_name, task)$executable)
    }, logical(1))]
    contract_versions <- stats::setNames(vapply(expected_tasks, function(task) {
      as.character(run_manifest_disposition(run_metadata, runner_name, task)$contract_version)
    }, character(1)), expected_tasks)
    retained_correctness <- load_retained_correctness(
      validated_correctness_arg, expected_path, runner_name, run_id, "task", expected_tasks, executable_tasks,
      expected_identity = list(
        source_tree_digest = as.character(run_metadata$environment$source_tree$digest),
        source_ledger_identity_digest = as.character(runner_environment$source_ledger_identity_digest),
        artifact_digest = as.character(runner_environment$artifact_digest),
        input_manifest_digest = as.character(run_metadata$input_manifest$digest),
        contract_version = contract_versions,
        timing_policy_digest = run_manifest_object_digest(run_metadata$timing_policy)
      )
    )
  }
  
  cat(sprintf("Runner: %s (%s)\n", runner_name, cfg$label))
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
  call_type <- cfg$call_type %||% ".Call"
  
  timing_summary_fields <- function(bm = NULL) {
    if (is.null(bm)) {
      return(list(
        warmup_iterations = as.integer(timing_policy$warmup_iterations),
        sample_stage = "not_measured",
        fixed_iterations = NA_integer_,
        timer_noise_floor_ms = as.numeric(timing_policy$timer_noise_floor_ms),
        timer_noise_status = "not_measured",
        rss_endpoint_metric = as.character(timing_policy$rss_endpoint_metric),
        rss_endpoint_support = "not_measured",
        rss_endpoint_support_reason = "timing not measured",
        peak_rss_kb = NA_integer_, loaded_process_rss_kb = NA_integer_,
        peak_rss_metric = as.character(timing_policy$peak_rss_metric),
        peak_rss_support = "not_eligible",
        peak_rss_support_reason = "historical task is not declared memory eligible",
        peak_rss_repetitions = NA_integer_,
        gc_policy = as.character(timing_policy$gc_policy)
      ))
    }
    list(
      warmup_iterations = as.integer(bm$warmup_iterations),
      sample_stage = as.character(timing_options$stage),
      fixed_iterations = as.integer(bm$fixed_iterations),
      timer_noise_floor_ms = as.numeric(bm$timer_noise_floor_ms),
      timer_noise_status = as.character(bm$timer_noise_status),
      rss_endpoint_metric = as.character(bm$rss_endpoint_metric),
      rss_endpoint_support = as.character(bm$rss_endpoint_support),
      rss_endpoint_support_reason = as.character(bm$rss_endpoint_support_reason),
      peak_rss_kb = NA_integer_, loaded_process_rss_kb = NA_integer_,
      peak_rss_metric = as.character(timing_policy$peak_rss_metric),
      peak_rss_support = "not_eligible",
      peak_rss_support_reason = "historical task is not declared memory eligible",
      peak_rss_repetitions = NA_integer_,
      gc_policy = as.character(timing_policy$gc_policy)
    )
  }
  
  if (call_type != "r") {
    loaded_dlls <- list()
    so_path <- file.path(root_dir, cfg$so_path)
    if (!file.exists(so_path)) stop(sprintf("library not found: %s", so_path))
    main_dll <- tryCatch(dyn.load(so_path),
                         error = function(e) stop(sprintf("load error: %s", conditionMessage(e))))
    loaded_dlls[[main_dll[["name"]]]] <- main_dll
  }
  
  source(file.path(root_dir, "src/r/run_all.R"))
  r_config <- runner_configs$r
  r_runner_map <- r_config$exports
  r_ref <- r_reference_map(r_config)
  validate_r_reference_map(manifest, r_ref)
  r_evidence_rows <- evidence$tasks[evidence$tasks$runner == "r", , drop = FALSE]
  selected_task_ids <- vapply(all_tasks, function(task) task$id, character(1))
  live_r_provenance <- build_run_r_provenance(
    selected_task_ids, r_runner_map, r_ref, manifest, r_evidence_rows
  )
  r_runner_provenance <- named_r_provenance_records(live_r_provenance, "runner_rows")
  r_reference_provenance <- named_r_provenance_records(live_r_provenance, "reference_rows")
  if (!check_only) {
    expected_runner_provenance <- named_r_provenance_records(run_metadata$r_provenance, "runner_rows")
    expected_reference_provenance <- named_r_provenance_records(run_metadata$r_provenance, "reference_rows")
    expected_runner_provenance <- expected_runner_provenance[selected_task_ids]
    expected_reference_provenance <- expected_reference_provenance[intersect(
      names(expected_reference_provenance), selected_task_ids
    )]
    if (!identical(sort(names(expected_runner_provenance)), sort(names(r_runner_provenance))) ||
        !identical(sort(names(expected_reference_provenance)), sort(names(r_reference_provenance)))) {
      stop("R provenance task sets differ from the run manifest")
    }
    for (task_id in names(r_runner_provenance)) {
      compare_r_provenance_records(expected_runner_provenance[[task_id]], r_runner_provenance[[task_id]], task_id)
    }
    for (task_id in names(r_reference_provenance)) {
      compare_r_provenance_records(
        expected_reference_provenance[[task_id]],
        r_reference_provenance[[task_id]],
        paste0(task_id, " reference")
      )
    }
  }
  
  capture_result <- function(fn) {
    error <- NA_character_
    value <- tryCatch(fn(), error = function(e) {
      error <<- conditionMessage(e)
      NULL
    })
    list(ok = is.na(error), value = value, error = error)
  }
  
  resolve_registered_exports <- function(export_names, cfg) {
    package_name <- cfg$package_name %||% ""
    if (!nzchar(package_name)) stop(sprintf("runner %s enables registered_symbols but has no package_name", runner_name))
    resolved <- export_names
    validated_packages <- character(0)
    for (tid in names(export_names)) {
      dll <- loaded_dlls[[package_name]]
      if (is.null(dll)) stop(sprintf(
        "registered symbol lookup has no loaded DLL named %s for runner %s task %s",
        package_name, runner_name, tid
      ))
      if (!(package_name %in% validated_packages)) {
        validate_forced_registration(dll[["dynamicLookup"]], sprintf("%s package %s", runner_name, package_name))
        validated_packages <- c(validated_packages, package_name)
      }
      info <- tryCatch(
        getNativeSymbolInfo(export_names[[tid]], PACKAGE = dll, withRegistrationInfo = TRUE),
        error = function(e) stop(sprintf(
          "registered symbol lookup failed for runner %s task %s (%s in package %s): %s",
          runner_name, tid, export_names[[tid]], package_for_task, conditionMessage(e)
        ))
      )
      resolved[[tid]] <- info$address
    }
    resolved
  }
  
  validate_registration_fixture <- function(cfg) {
    fixture <- cfg$registration_fixture
    if (is.null(fixture)) return(invisible(NULL))
    recognized <- c(
      "scalar", "integer", "logical", "scalar_after_allocation",
      "optional", "optional_integer", "optional_logical",
      "vector", "new", "method", "error", "external", "wrong_tag", "cleared", "misaligned"
    )
    recognized <- c(recognized, "missing_metadata")
    unknown <- setdiff(names(fixture), recognized)
    if (length(unknown) > 0L) stop(sprintf(
      "runner %s registration_fixture has unknown checks: %s", runner_name, paste(unknown, collapse = ", ")
    ))
    blank <- names(fixture)[vapply(fixture, function(value) length(value) != 1L || !nzchar(value), logical(1))]
    if (length(blank) > 0L) stop(sprintf(
      "runner %s registration_fixture has blank symbols: %s", runner_name, paste(blank, collapse = ", ")
    ))
    has <- function(key) !is.null(fixture[[key]])
    if (xor(has("new"), has("method"))) stop(sprintf(
      "runner %s registration_fixture must declare new and method together", runner_name
    ))
    receiver_checks <- c("wrong_tag", "missing_metadata", "cleared", "misaligned")
    orphaned <- receiver_checks[vapply(receiver_checks, has, logical(1)) & !has("method")]
    if (length(orphaned) > 0L) stop(sprintf(
      "runner %s registration_fixture pointer checks require a method: %s",
      runner_name,
      paste(orphaned, collapse = ", ")
    ))
    package_name <- cfg$package_name
    dll <- loaded_dlls[[package_name]]
    if (is.null(dll)) stop(sprintf(
      "registration fixture has no loaded DLL named %s for runner %s", package_name, runner_name
    ))
    validate_forced_registration(dll[["dynamicLookup"]], runner_name)
    symbol <- function(key) {
      info <- tryCatch(
        getNativeSymbolInfo(fixture[[key]], PACKAGE = dll, withRegistrationInfo = TRUE),
        error = function(e) stop(sprintf(
          "registration fixture lookup failed for runner %s (%s in package %s): %s",
          runner_name, fixture[[key]], package_name, conditionMessage(e)
        ))
      )
      info$address
    }
    symbols <- setNames(lapply(names(fixture), symbol), names(fixture))
  
    expect_fixture_error <- function(result, label, expected_message = NULL) {
      if (result$ok) stop(sprintf("registration fixture accepted %s for %s", label, runner_name))
      if (!is.null(expected_message) && !grepl(expected_message, result$error, fixed = TRUE)) stop(sprintf(
        "registration fixture error message changed for %s (%s): %s",
        runner_name, label, result$error %||% "no message"
      ))
    }
  
    if (has("scalar")) {
      scalar <- capture_result(function() do.call(.Call, list(symbols$scalar, 3.5)))
      if (!scalar$ok || !isTRUE(all.equal(scalar$value, 3.5))) stop(sprintf(
        "registration fixture scalar check failed for %s: %s", runner_name, scalar$error %||% "wrong result"
      ))
      # Preflight only: repeated independent calls must not add a timed inner loop.
      for (value in c(-3.5, 0, 7.25)) {
        repeated_scalar <- capture_result(function() do.call(.Call, list(symbols$scalar, value)))
        if (!repeated_scalar$ok || !isTRUE(all.equal(repeated_scalar$value, value))) stop(sprintf(
          "registration fixture repeated scalar check failed for %s", runner_name
        ))
      }
      scalar_nan <- capture_result(function() do.call(.Call, list(symbols$scalar, NaN)))
      if (!scalar_nan$ok || !isTRUE(is.nan(scalar_nan$value))) stop(sprintf(
        "registration fixture scalar NaN check failed for %s", runner_name
      ))
      expect_fixture_error(
        capture_result(function() do.call(.Call, list(symbols$scalar, 1L))),
        "a wrong scalar type",
        if (identical(runner_name, "zigr")) "expected REALSXP" else NULL
      )
      expect_fixture_error(
        capture_result(function() do.call(.Call, list(symbols$scalar, c(1.0, 2.0)))),
        "an invalid scalar length",
        if (identical(runner_name, "zigr")) "scalar inputs must have length one" else NULL
      )
      expect_fixture_error(
        capture_result(function() do.call(.Call, list(symbols$scalar, numeric()))),
        "an empty scalar",
        if (identical(runner_name, "zigr")) "expected non-empty vector" else NULL
      )
      expect_fixture_error(
        capture_result(function() do.call(.Call, list(symbols$scalar, NA_real_))),
        "a required scalar NA",
        if (identical(runner_name, "zigr")) "scalar inputs must not be NA" else NULL
      )
      expect_fixture_error(
        capture_result(function() do.call(.Call, list(symbols$scalar, 3.5, 4.5))),
        "an invalid .Call arity"
      )
      expect_fixture_error(
        capture_result(function() getNativeSymbolInfo(
          fixture$scalar,
          PACKAGE = dll,
          type = ".External",
          withRegistrationInfo = TRUE
        )),
        "a .Call routine requested as .External"
      )
      expect_fixture_error(
        capture_result(function() .Call(fixture$scalar, 3.5, PACKAGE = package_name)),
        "character lookup when forced symbols are enabled"
      )
    }
  
    if (has("scalar_after_allocation")) {
      result <- capture_result(function() do.call(.Call, list(symbols$scalar_after_allocation, 3.5)))
      if (!result$ok || !isTRUE(all.equal(result$value, 3.5))) stop(sprintf(
        "registration fixture scalar-after-allocation check failed for %s", runner_name
      ))
    }
  
    if (has("integer")) {
      integer <- capture_result(function() do.call(.Call, list(symbols$integer, -7L)))
      if (!integer$ok || !identical(integer$value, -7L)) stop(sprintf(
        "registration fixture integer scalar check failed for %s", runner_name
      ))
      expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$integer, 3.5))), "a wrong integer scalar type")
      expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$integer, c(1L, 2L)))), "an overlong integer scalar")
      expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$integer, integer()))), "an empty integer scalar")
      expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$integer, NA_integer_))), "a required integer NA")
    }
  
    if (has("logical")) {
      logical_false <- capture_result(function() do.call(.Call, list(symbols$logical, FALSE)))
      logical_true <- capture_result(function() do.call(.Call, list(symbols$logical, TRUE)))
      if (!logical_false$ok || !identical(logical_false$value, FALSE) ||
          !logical_true$ok || !identical(logical_true$value, TRUE)) stop(sprintf(
        "registration fixture logical scalar check failed for %s", runner_name
      ))
      expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$logical, 1L))), "a wrong logical scalar type")
      expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$logical, c(TRUE, FALSE)))), "an overlong logical scalar")
      expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$logical, logical()))), "an empty logical scalar")
      expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$logical, NA))), "a required logical NA")
    }
  
    if (has("optional")) {
      optional_null <- capture_result(function() do.call(.Call, list(symbols$optional, NULL)))
      optional_na <- capture_result(function() do.call(.Call, list(symbols$optional, NA_real_)))
      optional_nan <- capture_result(function() do.call(.Call, list(symbols$optional, NaN)))
      if (!optional_null$ok || !identical(optional_null$value, 0L) ||
          !optional_na$ok || !identical(optional_na$value, 0L) ||
          !optional_nan$ok || !identical(optional_nan$value, 1L)) stop(sprintf(
        "registration fixture optional scalar check failed for %s", runner_name
      ))
      expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$optional, c(NA_real_, 1.0)))), "an invalid optional scalar length")
      expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$optional, numeric()))), "an empty optional scalar")
    }
  
    if (has("optional_integer")) {
      optional_null <- capture_result(function() do.call(.Call, list(symbols$optional_integer, NULL)))
      optional_na <- capture_result(function() do.call(.Call, list(symbols$optional_integer, NA_integer_)))
      optional_value <- capture_result(function() do.call(.Call, list(symbols$optional_integer, 7L)))
      if (!optional_null$ok || !identical(optional_null$value, 0L) ||
          !optional_na$ok || !identical(optional_na$value, 0L) ||
          !optional_value$ok || !identical(optional_value$value, 1L)) stop(sprintf(
        "registration fixture optional integer check failed for %s", runner_name
      ))
      expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$optional_integer, c(NA_integer_, 1L)))), "an overlong optional integer scalar")
      expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$optional_integer, NA_real_))), "a wrong optional integer scalar type")
    }
  
    if (has("optional_logical")) {
      optional_null <- capture_result(function() do.call(.Call, list(symbols$optional_logical, NULL)))
      optional_na <- capture_result(function() do.call(.Call, list(symbols$optional_logical, NA)))
      optional_value <- capture_result(function() do.call(.Call, list(symbols$optional_logical, FALSE)))
      if (!optional_null$ok || !identical(optional_null$value, 0L) ||
          !optional_na$ok || !identical(optional_na$value, 0L) ||
          !optional_value$ok || !identical(optional_value$value, 1L)) stop(sprintf(
        "registration fixture optional logical check failed for %s", runner_name
      ))
      expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$optional_logical, c(NA, TRUE)))), "an overlong optional logical scalar")
      expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$optional_logical, NA_integer_))), "a wrong optional logical scalar type")
    }
  
    invalid_result_contract <- validate_result_contract(1L, "real_scalar")
    if (invalid_result_contract$ok) stop(sprintf("result-contract preflight accepted an invalid shape for %s", runner_name))
  
    if (has("vector")) {
      vector <- capture_result(function() do.call(.Call, list(symbols$vector, c(1.0, 2.0, 3.0))))
      if (!vector$ok || !isTRUE(all.equal(vector$value, 6.0))) stop(sprintf(
        "registration fixture vector check failed for %s: %s", runner_name, vector$error %||% "wrong result"
      ))
    }
  
    if (has("new")) {
      receiver <- capture_result(function() do.call(.Call, list(symbols$new)))
      if (!receiver$ok || !identical(typeof(receiver$value), "externalptr")) stop(sprintf(
        "registration fixture constructor check failed for %s: %s", runner_name, receiver$error %||% "wrong result"
      ))
      method <- capture_result(function() do.call(.Call, list(symbols$method, receiver$value, 7L)))
      if (!method$ok || !isTRUE(all.equal(method$value, 7L))) stop(sprintf(
        "registration fixture method check failed for %s: %s", runner_name, method$error %||% "wrong result"
      ))
      expect_fixture_error(
        capture_result(function() do.call(.Call, list(symbols$method, 1L, 7L))),
        "an invalid method receiver"
      )
    }
  
    if (has("error")) {
      expected_error <- capture_result(function() do.call(.Call, list(symbols$error, 1.0)))
      if (expected_error$ok || !grepl("fixture error", expected_error$error, fixed = TRUE)) stop(sprintf(
        "registration fixture error check failed for %s", runner_name
      ))
    }
  
    if (has("external")) {
      external <- capture_result(function() do.call(.External, list(symbols$external, 4.0)))
      if (!external$ok || !isTRUE(all.equal(external$value, 5.0))) stop(sprintf(
        "registration fixture .External check failed for %s: %s", runner_name, external$error %||% "wrong result"
      ))
      expect_fixture_error(
        capture_result(function() do.call(.Call, list(symbols$external, 4.0))),
        "an external routine called through .Call"
      )
      expect_fixture_error(
        capture_result(function() do.call(.External, list(symbols$external))),
        "an invalid .External arity"
      )
    }
  
    if (has("scalar") && has("external")) {
      expect_fixture_error(
        capture_result(function() do.call(.External, list(symbols$scalar, 3.5))),
        "a .Call routine called through .External"
      )
    }
  
    pointer_case <- function(key, label, expected_message = NULL) {
      pointer <- capture_result(function() do.call(.Call, list(symbols[[key]])))
      if (!pointer$ok || !identical(typeof(pointer$value), "externalptr")) stop(sprintf(
        "registration fixture %s constructor check failed for %s", label, runner_name
      ))
      expect_fixture_error(
        capture_result(function() do.call(.Call, list(symbols$method, pointer$value, 7L))),
        sprintf("%s external pointer", label),
        expected_message
      )
    }
    if (has("wrong_tag")) pointer_case("wrong_tag", "wrong-tag")
    if (has("missing_metadata")) pointer_case(
      "missing_metadata",
      "missing-metadata",
      if (identical(runner_name, "zigr")) "external pointer is missing typed metadata" else NULL
    )
    if (has("cleared")) pointer_case("cleared", "cleared")
    if (has("misaligned")) pointer_case("misaligned", "misaligned")
  
    missing_symbol <- capture_result(function() getNativeSymbolInfo(
      paste0(package_name, "_fixture_missing"),
      PACKAGE = dll,
      withRegistrationInfo = TRUE
    ))
    if (missing_symbol$ok) stop(sprintf("registration fixture exposed an unregistered symbol for %s", runner_name))
    cat(sprintf("Registration preflight passed for %s\n", runner_name))
    invisible(NULL)
  }
  
  registered_export_names <- NULL
  if (isTRUE(cfg$registered_symbols)) {
    registered_export_names <- cfg$exports
    cfg$exports <- resolve_registered_exports(registered_export_names, cfg)
    validate_registration_fixture(cfg)
  }
  
  method_receiver <- function() {
    if (call_type == "r") return(NULL)
    fixture <- cfg$registration_fixture
    package_name <- cfg$package_name %||% ""
    if (is.null(fixture) || is.null(fixture$new) || !nzchar(package_name)) return(NULL)
    dll <- loaded_dlls[[package_name]]
    if (is.null(dll)) stop(sprintf("runner %s has no loaded package DLL for the method receiver", runner_name))
    new_address <- getNativeSymbolInfo(fixture$new, PACKAGE = dll, withRegistrationInfo = TRUE)$address
    do.call(.Call, list(new_address))
  }
  
  if (check_only) {
    validate_task_arguments(manifest, all_tasks)
    cat(sprintf("Coverage preflight passed for %s (%d task specs)\n", runner_name, length(all_tasks)))
    quit(save = "no", status = 0, runLast = FALSE)
  }
  
  same_attributes <- function(expected, actual) {
    expected_names <- names(expected)
    actual_names <- names(actual)
    if (!identical(sort(expected_names), sort(actual_names))) return(FALSE)
    if (length(expected_names) == 0L) return(TRUE)
    all(vapply(expected_names, function(name) identical(expected[[name]], actual[[name]]), logical(1)))
  }
  
  compare_correctness <- function(expected, actual, path = "result") {
    result <- function(ok, message = "") list(ok = isTRUE(ok), message = message)
    if (is.null(expected) && is.null(actual)) return(result(TRUE))
    if (is.null(expected) || is.null(actual)) return(result(FALSE, sprintf("%s is NULL on one side", path)))
    if (!identical(typeof(expected), typeof(actual))) {
      return(result(FALSE, sprintf("%s type differs", path)))
    }
    if (!same_attributes(attributes(expected), attributes(actual))) {
      return(result(FALSE, sprintf("%s attributes differ", path)))
    }
    if (length(expected) != length(actual)) return(result(FALSE, sprintf("%s length differs", path)))
  
    if (is.numeric(expected) || is.complex(expected)) {
      if (!identical(is.na(expected), is.na(actual))) return(result(FALSE, sprintf("%s NA positions differ", path)))
      if (!identical(is.nan(expected), is.nan(actual))) return(result(FALSE, sprintf("%s NA and NaN kinds differ", path)))
      if (!isTRUE(all.equal(
        expected,
        actual,
        tolerance = sqrt(.Machine$double.eps),
        check.attributes = FALSE
      ))) {
        return(result(FALSE, sprintf("%s values differ", path)))
      }
      return(result(TRUE))
    }
    if (is.character(expected)) {
      if (!identical(is.na(expected), is.na(actual))) return(result(FALSE, sprintf("%s NA positions differ", path)))
      if (!identical(Encoding(expected), Encoding(actual))) return(result(FALSE, sprintf("%s encodings differ", path)))
      if (!identical(expected[!is.na(expected)], actual[!is.na(actual)])) {
        return(result(FALSE, sprintf("%s string values differ", path)))
      }
      return(result(TRUE))
    }
    if (is.list(expected)) {
      for (index in seq_along(expected)) {
        nested <- compare_correctness(expected[[index]], actual[[index]], sprintf("%s[[%d]]", path, index))
        if (!isTRUE(nested$ok)) return(nested)
      }
      return(result(TRUE))
    }
    if (identical(expected, actual)) return(result(TRUE))
    result(FALSE, sprintf("%s values differ", path))
  }
  
  result_preview <- function(value) {
    gsub(",", " ", substr(paste(deparse(value), collapse = ""), 1, 120))
  }
  
  results_list <- list()
  raw_results <- list()
  exports <- cfg$exports
  n_pass <- 0; n_fail <- 0; n_na <- 0
  
  for (task in all_tasks) {
    tid <- task$id
    manifest_row <- match(tid, manifest$task)
    if (is.na(manifest_row)) stop(sprintf("task %s is absent from the manifest", tid))
    correctness_policy <- manifest$correctness_policy[[manifest_row]]
    expected_return <- manifest$expected_return[[manifest_row]]
    correctness_status <- if (call_type == "r") "REFERENCE" else "NOT_VALIDATED"
    correctness_message <- ""
    task_call_type <- if (call_type == "r") "r" else (task$call_type %||% call_type)
    cfun <- exports[[tid]]
    disposition <- run_manifest_disposition(run_metadata, runner_name, tid)
    input_record <- input_recipes$tasks[[tid]]
    mutation_policy <- as.character(input_record$mutation_policy)
    altrep_intent <- as.character(input_record$altrep_intent)
    task_seed <- input_scalar_integer(input_record$task_seed, sprintf("task seed for %s", tid))
    input_verified <- FALSE
    new_phase_arguments <- function() {
      materialized <- materialize_task_input(task, task_seed)
      if (!input_verified) {
        validate_materialized_task_input(task, input_record, master_seed, materialized)
        input_verified <<- TRUE
      }
      arguments <- materialize_runtime_task_arguments(
        materialized$arguments, mutation_policy, method_receiver
      )
      if (identical(mutation_policy, "rng_reset_required")) {
        set.seed(task_seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
      }
      arguments
    }
    expression_for_arguments <- function(arguments) {
      make_call_expr(cfun, arguments, task_call_type)
    }
    callable_for_arguments <- function(arguments) {
      force(arguments)
      force(cfun)
      force(task_call_type)
      if (identical(task_call_type, "r")) {
        return(function() do.call(cfun, arguments))
      }
      interface <- switch(task_call_type, ".C" = .C, ".External" = .External, .Call)
      function() do.call(interface, c(list(cfun), arguments))
    }
    if (is.null(cfun)) {
      n_na <- n_na + 1
      na_allowed <- !isTRUE(disposition$executable)
      if (!na_allowed) {
        n_fail <- n_fail + 1
        correctness_message <- "normalized disposition requires an executable but the runner has none"
      }
      cat(sprintf("  %-14s [N/A]\n", tid))
      results_list[[length(results_list) + 1]] <- data.frame(
        runner = runner_name, task = tid, status = "N/A",
        call_type = task_call_type,
        mean_ms = NA, median_ms = NA, min_ms = NA, max_ms = NA,
        sd_ms = NA, cv_pct = NA, rss_endpoint_delta_kb = NA,
        first_call_ms = NA, n_iterations = NA, error = NA_character_,
        correctness_status = "NOT_APPLICABLE",
        correctness_policy = correctness_policy,
        correctness_message = if (na_allowed) as.character(disposition$reason) else correctness_message,
        stringsAsFactors = FALSE,
        timing_summary_fields())
      next
    }
    if (!isTRUE(disposition$executable)) {
      stop(sprintf("runner %s exposes %s despite its non-executable disposition", runner_name, tid))
    }
  
    retained_row <- if (is.null(retained_correctness)) {
      NULL
    } else {
      retained_correctness[retained_correctness$task == tid, , drop = FALSE]
    }
    if (!is.null(retained_row)) {
      if (nrow(retained_row) != 1L || !identical(as.character(retained_row$status), "PASS") ||
          !(as.character(retained_row$correctness_status) %in% c("PASS", "REFERENCE"))) {
        stop(sprintf("retained task correctness is not passing for %s/%s", runner_name, tid))
      }
      correctness_status <- as.character(retained_row$correctness_status)
      correctness_message <- as.character(retained_row$correctness_message)
    } else if (task_call_type == "r") {
      r_arguments <- new_phase_arguments()
      before <- task_arguments_fingerprint(tid, r_arguments, altrep_intent)
      r_eval <- capture_result(function() do.call(get(cfun, mode = "function"), r_arguments))
      r_rng_state <- if (r_eval$ok && identical(mutation_policy, "rng_reset_required")) rng_state_snapshot() else NULL
      if (!r_eval$ok) {
        correctness_status <- "FAIL"
        correctness_message <- sprintf("R implementation failed: %s", r_eval$error)
      } else {
        r_contract <- validate_result_contract(r_eval$value, expected_return)
        if (!r_contract$ok) {
          correctness_status <- "FAIL"
          correctness_message <- paste("R result:", r_contract$message)
        } else {
          if (identical(mutation_policy, "immutable")) {
            mutation <- capture_result(function() assert_immutable_input(tid, r_arguments, before, altrep_intent))
            if (!mutation$ok) {
              correctness_status <- "FAIL"
              correctness_message <- mutation$error
            }
          }
          if (!identical(correctness_status, "FAIL")) {
            provenance <- r_runner_provenance[[tid]]
            correctness_status <- if (identical(provenance$implementation_class, "pure_r")) "REFERENCE" else "PASS"
            correctness_message <- sprintf("validated %s result contract", provenance$implementation_class)
            if (identical(mutation_policy, "rng_reset_required")) {
              reference_arguments <- new_phase_arguments()
              r_repeat <- capture_result(function() do.call(get(r_ref[[tid]], mode = "function"), reference_arguments))
              r_repeat_state <- if (r_repeat$ok) rng_state_snapshot() else NULL
              if (!r_repeat$ok) {
                correctness_status <- "FAIL"
                correctness_message <- sprintf("R RNG repeat failed: %s", r_repeat$error)
              } else {
                repeated_values <- compare_correctness(r_eval$value, r_repeat$value)
                repeated_state <- capture_result(function() assert_rng_state_equivalent(r_rng_state, r_repeat_state, tid))
                if (!isTRUE(repeated_values$ok) || !repeated_state$ok) {
                  correctness_status <- "FAIL"
                  correctness_message <- if (!isTRUE(repeated_values$ok)) {
                    sprintf("deterministic RNG value mismatch: %s", repeated_values$message)
                  } else {
                    repeated_state$error
                  }
                } else {
                  correctness_message <- sprintf(
                    "validated deterministic values and post-call RNG state for %s",
                    provenance$implementation_class
                  )
                }
              }
            }
          }
        }
      }
    } else {
      native_arguments <- new_phase_arguments()
      native_before <- task_arguments_fingerprint(tid, native_arguments, altrep_intent)
      invoke_native <- function() {
        if (task_call_type == ".Call") return(do.call(.Call, c(list(cfun), native_arguments)))
        if (task_call_type == ".C") return(do.call(.C, c(list(cfun), native_arguments)))
        if (task_call_type == ".External") return(do.call(.External, c(list(cfun), native_arguments)))
        stop(sprintf("unsupported correctness call type: %s", task_call_type))
      }
      native_eval <- capture_result(invoke_native)
      native_rng_state <- if (native_eval$ok && identical(mutation_policy, "rng_reset_required")) rng_state_snapshot() else NULL
      if (!native_eval$ok) {
        correctness_status <- "FAIL"
        correctness_message <- sprintf("native call failed: %s", native_eval$error)
      } else {
        native_contract <- validate_result_contract(native_eval$value, expected_return)
        if (!native_contract$ok) {
          correctness_status <- "FAIL"
          correctness_message <- paste("native result:", native_contract$message)
        } else if (identical(mutation_policy, "immutable")) {
          mutation <- capture_result(function() assert_immutable_input(tid, native_arguments, native_before, altrep_intent))
          if (!mutation$ok) {
            correctness_status <- "FAIL"
            correctness_message <- mutation$error
          }
        }
        if (!identical(correctness_status, "FAIL") && identical(correctness_policy, "r_reference")) {
          ref_name <- r_ref[[tid]]
          if (is.null(ref_name) || !nzchar(ref_name) || !exists(ref_name, mode = "function")) {
            correctness_status <- "NOT_VALIDATED"
            correctness_message <- "R reference function is missing"
          } else {
            reference_arguments <- new_phase_arguments()
            reference_before <- task_arguments_fingerprint(tid, reference_arguments, altrep_intent)
            ref_eval <- capture_result(function() do.call(get(ref_name, mode = "function"), reference_arguments))
            reference_rng_state <- if (ref_eval$ok && identical(mutation_policy, "rng_reset_required")) rng_state_snapshot() else NULL
            if (!ref_eval$ok) {
              correctness_status <- "FAIL"
              correctness_message <- sprintf("R reference failed: %s", ref_eval$error)
            } else {
              ref_contract <- validate_result_contract(ref_eval$value, expected_return)
              if (!ref_contract$ok) {
                correctness_status <- "FAIL"
                correctness_message <- paste("R reference result:", ref_contract$message)
              } else if (identical(mutation_policy, "immutable")) {
                mutation <- capture_result(function() assert_immutable_input(tid, reference_arguments, reference_before, altrep_intent))
                if (!mutation$ok) {
                  correctness_status <- "FAIL"
                  correctness_message <- mutation$error
                }
              }
              if (!identical(correctness_status, "FAIL")) {
                comparison <- compare_correctness(ref_eval$value, native_eval$value)
                if (!isTRUE(comparison$ok)) {
                  correctness_status <- "FAIL"
                  correctness_message <- sprintf(
                    "structural validation mismatch: %s; expected '%s' got '%s'",
                    comparison$message,
                    result_preview(ref_eval$value),
                    result_preview(native_eval$value)
                  )
                } else if (identical(mutation_policy, "rng_reset_required")) {
                  rng_comparison <- capture_result(function() {
                    assert_rng_state_equivalent(reference_rng_state, native_rng_state, tid)
                  })
                  if (!rng_comparison$ok) {
                    correctness_status <- "FAIL"
                    correctness_message <- rng_comparison$error
                  } else {
                    correctness_status <- "PASS"
                    correctness_message <- sprintf(
                      "validated deterministic values and post-call RNG state against %s R reference",
                      r_reference_provenance[[tid]]$implementation_class
                    )
                  }
                } else {
                  correctness_status <- "PASS"
                  correctness_message <- sprintf(
                    "validated against %s R reference",
                    r_reference_provenance[[tid]]$implementation_class
                  )
                }
              }
            }
          }
        } else if (!identical(correctness_status, "FAIL")) {
          correctness_status <- "PASS"
          correctness_message <- "validated declared result contract"
        }
      }
    }
  
    if (correctness_status %in% c("FAIL", "NOT_VALIDATED")) {
      n_fail <- n_fail + 1
      cat(sprintf("  %-14s [%s] correctness: %s\n", tid, correctness_status, correctness_message))
      log_error(runner_name, tid, correctness_message, dir = staging_results_dir)
      results_list[[length(results_list) + 1]] <- data.frame(
        runner = runner_name, task = tid, status = "FAIL",
        call_type = task_call_type,
        mean_ms = NA, median_ms = NA, min_ms = NA, max_ms = NA,
        sd_ms = NA, cv_pct = NA, rss_endpoint_delta_kb = NA,
        first_call_ms = NA, n_iterations = NA, error = correctness_message,
        correctness_status = correctness_status,
        correctness_policy = correctness_policy,
        correctness_message = correctness_message,
        stringsAsFactors = FALSE,
        timing_summary_fields())
      next
    }
  
    if (validation_only) {
      n_pass <- n_pass + 1L
      cat(sprintf("  %-14s [VALIDATED] %s\n", tid, correctness_message))
      results_list[[length(results_list) + 1L]] <- data.frame(
        runner = runner_name, task = tid, status = "PASS",
        call_type = task_call_type,
        mean_ms = NA, median_ms = NA, min_ms = NA, max_ms = NA,
        sd_ms = NA, cv_pct = NA, rss_endpoint_delta_kb = NA,
        first_call_ms = NA, n_iterations = NA, error = NA_character_,
        correctness_status = correctness_status,
        correctness_policy = correctness_policy,
        correctness_message = correctness_message,
        stringsAsFactors = FALSE,
        timing_summary_fields())
      next
    }
  
    validate_timing_admission(disposition, runner_name, tid)
  
    # Do not retain large correctness results while timing the same input.
    native_eval <- NULL
    ref_eval <- NULL
    native_contract <- NULL
    ref_contract <- NULL
    comparison <- NULL
  
    first_call_prepare <- function() expression_for_arguments(new_phase_arguments())
    first_call <- measure_first_call(first_call_prepare)
    raw_results[[length(raw_results) + 1L]] <- data.frame(
      runner = runner_name,
      task = tid,
      call_type = task_call_type,
      phase = "first_call",
      iteration = 1L,
      wall_ms = first_call$wall_ms,
      planning_ms = NA_real_,
      rss_endpoint_delta_kb = NA_integer_,
      error = first_call$error,
      run_id = run_id,
      correctness_status = correctness_status,
      correctness_policy = correctness_policy,
      correctness_message = correctness_message,
      stage = timing_options$stage,
      process_epoch = timing_options$process_epoch,
      batch = timing_options$batch,
      attempt = timing_options$attempt,
      group_order = timing_options$group_orders[[tid]],
      member_order = timing_options$member_order,
      excluded = FALSE,
      exclusion_reason = NA_character_,
      stringsAsFactors = FALSE
    )
    if (!is.na(first_call$error)) {
      n_fail <- n_fail + 1
      cat(sprintf("  %-14s [FAIL] %s\n", tid, first_call$error))
      log_error(runner_name, tid, first_call$error, dir = staging_results_dir)
      results_list[[length(results_list) + 1]] <- data.frame(
        runner = runner_name, task = tid, status = "FAIL",
        call_type = task_call_type,
        mean_ms = NA, median_ms = NA, min_ms = NA, max_ms = NA,
        sd_ms = NA, cv_pct = NA, rss_endpoint_delta_kb = NA,
        first_call_ms = round(first_call$wall_ms, 3), n_iterations = NA,
        error = first_call$error,
        correctness_status = correctness_status,
        correctness_policy = correctness_policy,
        correctness_message = correctness_message,
        stringsAsFactors = FALSE,
        timing_summary_fields())
      next
    }
  
    if (identical(mutation_policy, "immutable")) {
      warmup_arguments <- new_phase_arguments()
      timed_arguments <- new_phase_arguments()
      warmup_before <- task_arguments_fingerprint(tid, warmup_arguments, altrep_intent)
      timed_before <- task_arguments_fingerprint(tid, timed_arguments, altrep_intent)
      prepare_warmup <- function() expression_for_arguments(warmup_arguments)
      prepare_timed <- function() expression_for_arguments(timed_arguments)
    } else {
      prepare_warmup <- function() callable_for_arguments(new_phase_arguments())
      prepare_timed <- function() callable_for_arguments(new_phase_arguments())
    }
  
    bm <- benchmark_call(
      prepare_warmup,
      prepare_timed,
      iterations = as.integer(timing_options$counts[[tid]]),
      warmup = as.integer(timing_policy$warmup_iterations),
      fresh_each_iteration = !identical(mutation_policy, "immutable"),
      timer_noise_floor_ms = as.numeric(timing_policy$timer_noise_floor_ms),
      rss_endpoint_metric = as.character(timing_policy$rss_endpoint_metric)
    )
    if (!is.na(bm$error)) {
      n_fail <- n_fail + 1
      cat(sprintf("  %-14s [FAIL] %s\n", tid, bm$error))
      log_error(runner_name, tid, bm$error, dir = staging_results_dir)
      results_list[[length(results_list) + 1]] <- data.frame(
        runner = runner_name, task = tid, status = "FAIL",
        call_type = task_call_type,
        mean_ms = NA, median_ms = NA, min_ms = NA, max_ms = NA,
        sd_ms = NA, cv_pct = NA, rss_endpoint_delta_kb = NA,
        first_call_ms = round(first_call$wall_ms, 3), n_iterations = NA,
        error = bm$error,
        correctness_status = correctness_status,
        correctness_policy = correctness_policy,
        correctness_message = correctness_message,
        stringsAsFactors = FALSE,
        timing_summary_fields())
      next
    }
    if (identical(mutation_policy, "immutable")) {
      assert_immutable_input(tid, warmup_arguments, warmup_before, altrep_intent)
      assert_immutable_input(tid, timed_arguments, timed_before, altrep_intent)
    }
  
    n_pass <- n_pass + 1
    cat(sprintf("  %-14s mean=%8.4fms median=%8.4fms sd=%7.4fms cv=%5.2f%% endpoint-rss=%sKB runs=%d\n",
                tid, bm$mean_ms, bm$median_ms, bm$sd_ms, bm$cv_pct,
                as.character(bm$rss_endpoint_delta_kb), bm$n_runs))
  
    runs_df <- data.frame(
      runner      = runner_name,
      task        = tid,
      call_type   = task_call_type,
      phase       = "timed",
      iteration   = seq_len(length(bm$times)),
      wall_ms     = bm$times,
      planning_ms = bm$planning_times,
      rss_endpoint_delta_kb = c(rep(NA_integer_, length(bm$times) - 1), bm$rss_endpoint_delta_kb),
      error       = NA_character_,
      run_id      = run_id,
      correctness_status = correctness_status,
      correctness_policy = correctness_policy,
      correctness_message = correctness_message,
      stage = timing_options$stage,
      process_epoch = timing_options$process_epoch,
      batch = timing_options$batch,
      attempt = timing_options$attempt,
      group_order = timing_options$group_orders[[tid]],
      member_order = timing_options$member_order,
      excluded = FALSE,
      exclusion_reason = NA_character_,
      stringsAsFactors = FALSE
    )
    raw_results[[length(raw_results) + 1L]] <- runs_df
  
    results_list[[length(results_list) + 1]] <- data.frame(
      runner        = runner_name,
      task          = tid,
      status        = "PASS",
      call_type     = task_call_type,
      mean_ms       = round(bm$mean_ms, 4),
      median_ms     = round(bm$median_ms, 4),
      min_ms        = round(bm$min_ms, 4),
      max_ms        = round(bm$max_ms, 4),
      sd_ms         = round(bm$sd_ms, 4),
      cv_pct        = round(bm$cv_pct, 2),
      rss_endpoint_delta_kb = bm$rss_endpoint_delta_kb,
      first_call_ms = round(first_call$wall_ms, 3),
      n_iterations  = bm$n_runs,
      error         = NA_character_,
      correctness_status = correctness_status,
      correctness_policy = correctness_policy,
      correctness_message = correctness_message,
      stringsAsFactors = FALSE,
      timing_summary_fields(bm)
    )
  }
  
  if (validation_only) {
    correctness <- do.call(rbind, results_list)
    correctness$run_id <- run_id
    correctness$source_tree_digest <- as.character(run_metadata$environment$source_tree$digest)
    correctness$source_ledger_identity_digest <- as.character(
      run_metadata$environment$tool_source_ledger$identity_digest
    )
    correctness$artifact_digest <- as.character(runner_environment$artifact_digest)
    correctness$input_manifest_digest <- as.character(run_metadata$input_manifest$digest)
    correctness$contract_version <- vapply(correctness$task, function(task) {
      as.character(run_manifest_disposition(run_metadata, runner_name, task)$contract_version)
    }, character(1))
    correctness$timing_policy_digest <- run_manifest_object_digest(run_metadata$timing_policy)
    correctness <- correctness[, c(
      "run_id", "runner", "task", "status", "correctness_status", "correctness_policy",
      "correctness_message", "source_tree_digest", "source_ledger_identity_digest",
      "artifact_digest", "input_manifest_digest", "contract_version", "timing_policy_digest"
    )]
    output_path <- normalizePath(validation_output_arg, mustWork = FALSE)
    write_csv_once(correctness, output_path, "validation output")
    if (n_fail > 0L) stop(sprintf("runner %s failed validation for %d task(s)", runner_name, n_fail))
    cat(sprintf("Validation preflight passed for %s: %d executable and %d N/A task(s).\n", runner_name, n_pass, n_na))
    quit(save = "no", status = 0, runLast = FALSE)
  }

  if (length(raw_results) > 0L) {
    write_csv(
      do.call(rbind, raw_results),
      file.path(staging_results_dir, runner_name, "samples.csv")
    )
  }
  
  summary <- do.call(rbind, results_list)
  summary$run_id <- run_id
  summary_dispositions <- lapply(summary$task, function(task_id) {
    run_manifest_disposition(run_metadata, runner_name, as.character(task_id))
  })
  summary_inputs <- input_recipes$tasks[as.character(summary$task)]
  summary$master_seed <- master_seed
  summary$task_seed <- vapply(summary_inputs, function(record) input_scalar_integer(record$task_seed, "task seed"), integer(1))
  summary$input_fingerprint <- vapply(summary_inputs, function(record) as.character(record$fingerprint), character(1))
  summary$contract_version <- vapply(summary_dispositions, function(record) as.character(record$contract_version), character(1))
  summary$path_kind <- vapply(summary_dispositions, function(record) as.character(record$path_kind), character(1))
  summary$evidence_use <- vapply(summary_dispositions, function(record) as.character(record$evidence_use), character(1))
  summary$kernel_id <- vapply(summary_dispositions, function(record) as.character(record$kernel_id), character(1))
  summary$representation_strategy <- vapply(
    summary_dispositions,
    function(record) as.character(record$representation_strategy),
    character(1)
  )
  summary$mutation_policy <- vapply(summary_inputs, function(record) as.character(record$mutation_policy), character(1))
  summary$tool_identity <- as.character(runner_environment$tool_identity)
  summary$generated_glue_kind <- as.character(runner_environment$generated_glue_kind)
  summary$generated_glue_digest <- as.character(runner_environment$generated_glue_digest)
  summary$artifact_digest <- as.character(runner_environment$artifact_digest)
  summary$source_digest <- as.character(runner_environment$source_digest)
  summary$build_digest <- as.character(runner_environment$build_digest)
  summary$dependency_digest <- as.character(runner_environment$dependency_digest)
  summary$artifact_dependency_digest <- as.character(runner_environment$artifact_dependency_digest)
  summary$source_ledger_identity_digest <- as.character(runner_environment$source_ledger_identity_digest)
  summary_source_records <- lapply(as.character(summary$task), function(task_id) {
    source_ledger_verification_record(run_metadata$environment$tool_source_ledger, runner_name, task_id)
  })
  summary$source_path_class <- vapply(summary_source_records, function(record) as.character(record$source_class), character(1))
  summary$source_verification_digest <- vapply(
    summary_source_records,
    function(record) as.character(record$verification_digest),
    character(1)
  )
  summary$disposition <- vapply(summary_dispositions, function(record) as.character(record$status), character(1))
  summary$disposition_reason <- vapply(summary_dispositions, function(record) as.character(record$reason), character(1))
  summary$r_implementation_provenance <- vapply(as.character(summary$task), function(task_id) {
    if (identical(runner_name, "r")) {
      if (!is.null(r_runner_provenance[[task_id]])) return(as.character(r_runner_provenance[[task_id]]$implementation_class))
      return("pure_r_unrepresentable")
    }
    if (!is.null(r_reference_provenance[[task_id]])) {
      return(paste0("reference:", as.character(r_reference_provenance[[task_id]]$implementation_class)))
    }
    "not_applicable"
  }, character(1))
  summary$r_source_digest <- vapply(as.character(summary$task), function(task_id) {
    if (identical(runner_name, "r") && !is.null(r_runner_provenance[[task_id]])) {
      return(as.character(r_runner_provenance[[task_id]]$source_digest))
    }
    if (!is.null(r_reference_provenance[[task_id]])) return(as.character(r_reference_provenance[[task_id]]$source_digest))
    "not_applicable"
  }, character(1))
  staged_summary <- file.path(staging_results_dir, sprintf("%s_summary.csv", runner_name))
  write_csv(summary, staged_summary)
  if (n_fail > 0L) {
    stop(sprintf(
      "runner %s failed correctness or timing validation for %d task(s); run cannot be completed",
      runner_name, n_fail
    ))
  }
  cat(sprintf("  Batch output: %s\n", timing_options$output))
  invisible(summary)
}

run_fixture_worker <- function(args) {
  timing_options <- timing_worker_options(args)
  runner <- NULL
  run_dir <- NULL
  validation_only <- FALSE
  validation_output <- NULL
  validated_correctness <- NULL
  proof_only <- FALSE
  proof_output <- NULL
  memory_row <- NULL
  memory_output <- NULL
  fixture_filter <- NULL
  for (arg in args) {
    if (grepl("^--runner=", arg)) runner <- sub("^--runner=", "", arg)
    if (grepl("^--run-dir=", arg)) run_dir <- sub("^--run-dir=", "", arg)
    if (identical(arg, "--validation-only")) validation_only <- TRUE
    if (grepl("^--validation-output=", arg)) validation_output <- sub("^--validation-output=", "", arg)
    if (grepl("^--validated-correctness=", arg)) {
      validated_correctness <- sub("^--validated-correctness=", "", arg)
    }
    if (identical(arg, "--proof-only")) proof_only <- TRUE
    if (grepl("^--proof-output=", arg)) proof_output <- sub("^--proof-output=", "", arg)
    if (grepl("^--memory-row=", arg)) memory_row <- sub("^--memory-row=", "", arg)
    if (grepl("^--memory-output=", arg)) memory_output <- sub("^--memory-output=", "", arg)
    if (grepl("^--fixtures=", arg)) fixture_filter <- parse_csv_option(sub("^--fixtures=", "", arg), "fixture filter")
  }
  if (is.null(runner)) stop("--runner= is required")
  if (is.null(run_dir)) stop("--run-dir= is required")
  if (validation_only && is.null(validation_output)) stop("--validation-output= is required with --validation-only")
  if (validation_only && !is.null(validated_correctness)) {
    stop("validation-only mode cannot reuse retained correctness")
  }
  if (proof_only && is.null(proof_output)) stop("--proof-output= is required with --proof-only")
  memory_only <- !is.null(memory_row) || !is.null(memory_output)
  if (memory_only && (is.null(memory_row) || is.null(memory_output))) {
    stop("--memory-row= and --memory-output= must be supplied together")
  }
  if (sum(c(validation_only, proof_only, memory_only, !is.null(timing_options))) != 1L) {
    stop("fixture worker modes are mutually exclusive")
  }
  if (memory_only && (is.null(validated_correctness) || !is.null(fixture_filter))) {
    stop("memory measurement requires retained correctness and one --memory-row without --fixtures")
  }

  run_dir <- normalizePath(run_dir, mustWork = TRUE)
  source(file.path(root_dir, "src", "r", "run_all.R"))
  metadata <- read_run_manifest(run_dir)
  if (!(runner %in% run_manifest_values(metadata$runners))) {
    stop(sprintf("fixture runner %s is absent from the run manifest", runner))
  }
  manifest <- load_task_manifest(root_dir)
  evidence <- load_evidence_manifest(root_dir, manifest)
  environment <- runner_environment_record(metadata$environment, runner)
  validate_fixture_artifact_identity(environment)
  validate_tool_source_ledger(root_dir, metadata$environment$tool_source_ledger, runner)
  
  proof <- if (is.null(validated_correctness) || proof_only) {
    run_live_product_fixture_gate(root_dir, evidence, runner)
  } else {
    NULL
  }
  if (proof_only) {
    supported <- evidence$fixture_rows[
      evidence$fixture_rows$runner == runner & evidence$fixture_rows$executable,
      "fixture",
      drop = TRUE
    ]
    domain_status <- function(fixtures) {
      if (any(as.character(fixtures) %in% supported)) "PASS" else "NOT_APPLICABLE"
    }
    count_status <- function(value) {
      if (isTRUE(as.numeric(value) > 0)) "PASS" else "NOT_APPLICABLE"
    }
    proof_row <- data.frame(
      run_id = as.character(metadata$run_id),
      runner = runner,
      proof_status = "PASS",
      claim_eligible = FALSE,
      allocation_status = count_status(proof$values[["allocation"]]),
      protection_status = domain_status(c("F04", "F12")),
      recovery_status = domain_status("F11"),
      external_pointer_status = count_status(proof$state[["constructor"]]),
      altrep_callback_status = domain_status("F04"),
      finalizer_status = count_status(proof$state[["finalizer"]]),
      valid_case_count = unname(proof$values[["valid"]]),
      invalid_case_count = unname(proof$values[["invalid"]]),
      allocation_event_count = unname(proof$values[["allocation"]]),
      copy_event_count = unname(proof$values[["copy"]]),
      output_construction_count = unname(proof$output[["construction"]]),
      retained_output_count = unname(proof$output[["retention"]]),
      altrep_element_count = unname(proof$altrep[["element"]]),
      altrep_region_count = unname(proof$altrep[["region"]]),
      altrep_pointer_count = unname(proof$altrep[["pointer"]]),
      altrep_materialization_count = unname(proof$altrep[["materialization"]]),
      altrep_materialized_count = unname(proof$altrep[["is_materialized"]]),
      recovery_error_count = unname(proof$recovery[["error"]]),
      recovery_success_count = unname(proof$recovery[["recovery"]]),
      state_constructor_count = unname(proof$state[["constructor"]]),
      state_method_count = unname(proof$state[["method"]]),
      state_finalizer_count = unname(proof$state[["finalizer"]]),
      source_tree_digest = as.character(metadata$environment$source_tree$digest),
      source_ledger_identity_digest = as.character(environment$source_ledger_identity_digest),
      artifact_digest = as.character(environment$fixture_artifact_digest),
      stringsAsFactors = FALSE
    )
    output_path <- normalizePath(proof_output, mustWork = FALSE)
    write_csv_once(proof_row, output_path, "fixture proof output")
    cat(sprintf("Fixture safety proof passed for %s.\n", runner))
    quit(save = "no", status = 0L, runLast = FALSE)
  }
  context <- fixture_measurement_context(root_dir, runner, evidence)
  on.exit(context$close(), add = TRUE)
  reference <- fixture_r_functions(root_dir)
  specs <- fixture_measurement_specs()
  rows <- evidence$fixture_rows[evidence$fixture_rows$runner == runner, , drop = FALSE]
  rows <- rows[match(evidence$fixtures, rows$fixture), , drop = FALSE]
  
  retained_correctness_rows <- NULL
  if (!is.null(validated_correctness)) {
    expected_path <- normalizePath(
      run_correctness_artifact_paths(run_dir, metadata, "fixture", runner),
      mustWork = TRUE
    )
    expected_row_ids <- c(
      evidence$fixtures,
      if (identical(runner, "r")) c("F03_optimized_base_r", "F04_optimized_base_r") else character(0)
    )
    expected_fixtures <- sub("_optimized_base_r$", "", expected_row_ids)
    contract_versions <- stats::setNames(vapply(expected_fixtures, function(fixture) {
      as.character(rows$contract_version[match(fixture, rows$fixture)])
    }, character(1)), expected_row_ids)
    input_fingerprints <- stats::setNames(vapply(expected_fixtures, function(fixture) {
      spec <- specs[[fixture]]
      if (is.null(spec)) "not_applicable" else fixture_measurement_input_fingerprint(fixture, spec)
    }, character(1)), expected_row_ids)
    retained_correctness_rows <- load_retained_correctness(
      validated_correctness, expected_path, runner, as.character(metadata$run_id), "row_id", expected_row_ids,
      c(
        as.character(rows$fixture[rows$executable]),
        if (identical(runner, "r")) c("F03_optimized_base_r", "F04_optimized_base_r") else character(0)
      ),
      list(
        source_tree_digest = as.character(metadata$environment$source_tree$digest),
        source_ledger_identity_digest = as.character(environment$source_ledger_identity_digest),
        artifact_digest = as.character(environment$fixture_artifact_digest),
        input_fingerprint = input_fingerprints,
        contract_version = contract_versions,
        timing_policy_digest = run_manifest_object_digest(metadata$timing_policy)
      )
    )
  }
  if (!is.null(fixture_filter)) {
    rows <- rows[ordered_selection(rows$fixture, fixture_filter, "fixture filter"), , drop = FALSE]
  }
  
  if (is.null(retained_correctness_rows)) {
    for (index in seq_len(nrow(rows))) {
      row <- rows[index, , drop = FALSE]
      fixture <- as.character(row$fixture)
      if (isTRUE(row$executable) && fixture %in% names(specs)) {
        fixture_measurement_validate_case(
          runner, context$functions, reference$functions, fixture, specs[[fixture]]
        )
      }
    }
    if (identical(runner, "r")) {
      for (fixture in names(fixture_measurement_optimized_specs())) {
        fixture_measurement_validate_optimized(
          context$optimized, reference$functions, fixture, specs[[fixture]]
        )
      }
    }
  }
  if (validation_only) {
    correctness <- do.call(rbind, lapply(seq_len(nrow(rows)), function(index) {
      row <- rows[index, , drop = FALSE]
      fixture <- as.character(row$fixture)
      executable <- isTRUE(row$executable)
      data.frame(
        run_id = as.character(metadata$run_id), runner = runner, fixture = fixture,
        variant = "public", row_id = fixture, status = if (executable) "PASS" else "N/A",
        correctness_status = if (executable) {
          if (identical(runner, "r")) "REFERENCE" else "PASS"
        } else "NOT_APPLICABLE",
        correctness_message = if (executable) {
          "validated by the complete fixture semantic and lifecycle gate"
        } else as.character(row$reason),
        source_tree_digest = as.character(metadata$environment$source_tree$digest),
        source_ledger_identity_digest = as.character(environment$source_ledger_identity_digest),
        artifact_digest = as.character(environment$fixture_artifact_digest),
        stringsAsFactors = FALSE
      )
    }))
    if (identical(runner, "r")) {
      optimized <- do.call(rbind, lapply(names(fixture_measurement_optimized_specs()), function(fixture) {
        data.frame(
          run_id = as.character(metadata$run_id), runner = runner, fixture = fixture,
          variant = "optimized_base_r", row_id = paste0(fixture, "_optimized_base_r"), status = "PASS",
          correctness_status = "PASS",
          correctness_message = "validated optimized base-R result against the authored R oracle",
          source_tree_digest = as.character(metadata$environment$source_tree$digest),
          source_ledger_identity_digest = as.character(environment$source_ledger_identity_digest),
          artifact_digest = as.character(environment$fixture_artifact_digest),
          stringsAsFactors = FALSE
        )
      }))
      correctness <- rbind(correctness, optimized)
    }
    correctness$input_fingerprint <- vapply(correctness$fixture, function(fixture) {
      spec <- specs[[fixture]]
      if (is.null(spec)) "not_applicable" else fixture_measurement_input_fingerprint(fixture, spec)
    }, character(1))
    correctness$contract_version <- vapply(correctness$fixture, function(fixture) {
      as.character(rows$contract_version[match(fixture, rows$fixture)])
    }, character(1))
    correctness$timing_policy_digest <- run_manifest_object_digest(metadata$timing_policy)
    output_path <- normalizePath(validation_output, mustWork = FALSE)
    write_csv_once(correctness, output_path, "fixture validation output")
    cat(sprintf("Fixture correctness preflight passed for %s.\n", runner))
    quit(save = "no", status = 0L, runLast = FALSE)
  }
  
  timing_policy <- metadata$timing_policy
  validate_timing_policy(timing_policy)

  fixture_prepare_fresh <- function(fixture, variant = "public", functions = context$functions) {
    spec <- specs[[fixture]]
    if (identical(variant, "optimized_base_r")) {
      fn <- context$optimized[[fixture]]
      return(function() {
        arguments <- spec$arguments()
        function() do.call(fn, arguments)
      })
    }
    function() fixture_measurement_prepare(functions, fixture, spec)
  }

  if (memory_only) {
    optimized <- grepl("_optimized_base_r$", memory_row)
    fixture <- sub("_optimized_base_r$", "", memory_row)
    variant <- if (optimized) "optimized_base_r" else "public"
    row <- rows[rows$fixture == fixture, , drop = FALSE]
    if (nrow(row) != 1L || (optimized && !identical(runner, "r")) ||
        !isTRUE(row$timing_eligible) ||
        !peak_rss_fixture_eligible(fixture, variant, "PASS", timing_policy)) {
      stop(sprintf("fixture row %s is not declared memory eligible for %s", memory_row, runner))
    }
    retained <- retained_correctness_rows[
      retained_correctness_rows$runner == runner & retained_correctness_rows$row_id == memory_row,
      , drop = FALSE
    ]
    if (nrow(retained) != 1L || !identical(as.character(retained$status), "PASS")) {
      stop(sprintf("fixture row %s lacks retained passing correctness for %s", memory_row, runner))
    }
    memory <- measure_peak_process_rss(
      fixture_prepare_fresh(fixture, variant),
      as.integer(timing_policy$peak_rss_repetitions)
    )
    result <- data.frame(
      run_id = as.character(metadata$run_id), runner = runner, fixture = fixture,
      variant = variant, row_id = memory_row,
      peak_rss_kb = memory$peak_rss_kb,
      loaded_process_rss_kb = memory$loaded_process_rss_kb,
      peak_rss_metric = as.character(timing_policy$peak_rss_metric),
      peak_rss_support = as.character(memory$peak_rss_support),
      peak_rss_support_reason = as.character(memory$peak_rss_support_reason),
      peak_rss_repetitions = as.integer(memory$peak_rss_repetitions),
      stringsAsFactors = FALSE
    )
    write_csv_once(result, normalizePath(memory_output, mustWork = FALSE), "peak RSS output")
    cat(sprintf("Peak RSS measurement completed for %s/%s.\n", runner, memory_row))
    quit(save = "no", status = 0L, runLast = FALSE)
  }

  staging_root <- timing_options$output
  staging_runner <- file.path(staging_root, runner)
  dir.create(staging_runner, recursive = TRUE, showWarnings = FALSE)
  unlink(list.files(staging_runner, full.names = TRUE, all.files = TRUE, no.. = TRUE), recursive = TRUE)
  raw_results <- list()
  
  timing_fields <- function(bm = NULL) {
    if (is.null(bm)) return(list(
      warmup_iterations = as.integer(timing_policy$warmup_iterations),
      sample_stage = "not_measured", fixed_iterations = NA_integer_,
      timer_noise_floor_ms = as.numeric(timing_policy$timer_noise_floor_ms),
      timer_noise_status = "not_measured",
      rss_endpoint_metric = as.character(timing_policy$rss_endpoint_metric),
      rss_endpoint_support = "not_measured",
      rss_endpoint_support_reason = "timing not measured",
      peak_rss_kb = NA_integer_, loaded_process_rss_kb = NA_integer_,
      peak_rss_metric = as.character(timing_policy$peak_rss_metric),
      peak_rss_support = "not_eligible",
      peak_rss_support_reason = "workload is not declared memory eligible",
      peak_rss_repetitions = NA_integer_,
      gc_policy = as.character(timing_policy$gc_policy)
    ))
    list(
      warmup_iterations = as.integer(bm$warmup_iterations),
      sample_stage = as.character(timing_options$stage), fixed_iterations = as.integer(bm$fixed_iterations),
      timer_noise_floor_ms = as.numeric(bm$timer_noise_floor_ms),
      timer_noise_status = as.character(bm$timer_noise_status),
      rss_endpoint_metric = as.character(bm$rss_endpoint_metric),
      rss_endpoint_support = as.character(bm$rss_endpoint_support),
      rss_endpoint_support_reason = as.character(bm$rss_endpoint_support_reason),
      peak_rss_kb = NA_integer_, loaded_process_rss_kb = NA_integer_,
      peak_rss_metric = as.character(timing_policy$peak_rss_metric),
      peak_rss_support = "not_eligible",
      peak_rss_support_reason = "workload is not declared memory eligible",
      peak_rss_repetitions = NA_integer_,
      gc_policy = as.character(timing_policy$gc_policy)
    )
  }
  
  identity_fields <- function(row, fixture, variant, input_fingerprint) {
    optimized <- identical(variant, "optimized_base_r")
    list(
      run_id = as.character(metadata$run_id), runner = runner, fixture = fixture,
      variant = variant, row_id = if (optimized) paste0(fixture, "_optimized_base_r") else fixture,
      input_fingerprint = input_fingerprint,
      implementation_role = if (optimized) "optimized_base_r" else as.character(row$implementation_role),
      evidence_use = if (optimized) "timed_baseline" else as.character(row$evidence_use),
      path_kind = if (optimized) "optimized_base_r" else as.character(row$path_kind),
      representation_strategy = if (optimized) "runtime_service" else as.character(row$representation_strategy),
      comparison_tier = if (optimized) "tier_c" else as.character(row$comparison_tier),
      setup_policy = if (optimized) "setup_outside_timer" else as.character(row$setup_policy),
      timing_eligible = if (optimized) TRUE else isTRUE(row$timing_eligible),
      kernel_id = if (optimized) paste0("normalized:", fixture, ":optimized-base-r-v1") else as.character(row$kernel_id),
      contract_version = as.character(row$contract_version), fixture_version = as.character(row$fixture_version),
      comparison_group = if (optimized) paste0("normalized:", fixture, ":optimized-base-r") else as.character(row$comparison_group),
      tool_identity = as.character(environment$tool_identity),
      fixture_source_digest = as.character(environment$fixture_source_digest),
      fixture_build_digest = as.character(environment$fixture_build_digest),
      fixture_generated_glue_kind = as.character(environment$fixture_generated_glue_kind),
      fixture_generated_glue_digest = as.character(environment$fixture_generated_glue_digest),
      fixture_artifact_digest = as.character(environment$fixture_artifact_digest),
      fixture_dependency_digest = as.character(environment$fixture_dependency_digest),
      fixture_artifact_dependency_digest = as.character(environment$fixture_artifact_dependency_digest),
      source_ledger_identity_digest = as.character(environment$source_ledger_identity_digest)
    )
  }
  
  measure <- function(row, fixture, variant = "public", functions = context$functions) {
    spec <- specs[[fixture]]
    fingerprint <- fixture_measurement_input_fingerprint(fixture, spec)
    fields <- identity_fields(row, fixture, variant, fingerprint)
    prepare_fresh <- fixture_prepare_fresh(fixture, variant, functions)
    first_call <- measure_first_call(prepare_fresh)
    if (!is.na(first_call$error)) {
      stop(sprintf("fixture first call failed for %s/%s: %s", runner, fields$row_id, first_call$error))
    }
  
    reusable <- !fixture_measurement_requires_fresh_input(spec)
    if (reusable) {
      warmup_arguments <- spec$arguments()
      timed_arguments <- spec$arguments()
      intent <- fixture_measurement_altrep_intent(spec)
      warmup_before <- task_arguments_fingerprint(paste0("fixture:", fixture), warmup_arguments, intent)
      timed_before <- task_arguments_fingerprint(paste0("fixture:", fixture), timed_arguments, intent)
      if (identical(variant, "optimized_base_r")) {
        fn <- context$optimized[[fixture]]
        prepare_warmup <- function() function() do.call(fn, warmup_arguments)
        prepare_timed <- function() function() do.call(fn, timed_arguments)
      } else {
        prepare_warmup <- function() fixture_measurement_prepare(functions, fixture, spec, warmup_arguments)
        prepare_timed <- function() fixture_measurement_prepare(functions, fixture, spec, timed_arguments)
      }
    } else {
      prepare_warmup <- prepare_fresh
      prepare_timed <- prepare_fresh
    }
    bm <- benchmark_call(
      prepare_warmup, prepare_timed,
      iterations = as.integer(timing_options$counts[[fixture]]),
      warmup = as.integer(timing_policy$warmup_iterations),
      fresh_each_iteration = !reusable,
      timer_noise_floor_ms = as.numeric(timing_policy$timer_noise_floor_ms),
      rss_endpoint_metric = as.character(timing_policy$rss_endpoint_metric)
    )
    if (!is.na(bm$error)) stop(sprintf("fixture timing failed for %s/%s: %s", runner, fields$row_id, bm$error))
    if (reusable) {
      assert_immutable_input(paste0("fixture:", fixture), warmup_arguments, warmup_before, intent)
      assert_immutable_input(paste0("fixture:", fixture), timed_arguments, timed_before, intent)
    }
    first_call_raw <- data.frame(
      run_id = as.character(metadata$run_id), runner = runner, fixture = fixture,
      variant = variant, row_id = fields$row_id, phase = "first_call", iteration = 1L,
      wall_ms = first_call$wall_ms, planning_ms = NA_real_,
      rss_endpoint_delta_kb = NA_integer_, stage = timing_options$stage,
      process_epoch = timing_options$process_epoch, batch = timing_options$batch,
      attempt = timing_options$attempt, group_order = timing_options$group_orders[[fixture]],
      member_order = timing_options$member_order, excluded = FALSE,
      exclusion_reason = NA_character_, stringsAsFactors = FALSE
    )
    timed_raw <- data.frame(
      run_id = as.character(metadata$run_id), runner = runner, fixture = fixture,
      variant = variant, row_id = fields$row_id, phase = "timed", iteration = seq_along(bm$times),
      wall_ms = bm$times,
      planning_ms = bm$planning_times,
      rss_endpoint_delta_kb = c(rep(NA_integer_, length(bm$times) - 1L), bm$rss_endpoint_delta_kb),
      stage = timing_options$stage,
      process_epoch = timing_options$process_epoch, batch = timing_options$batch,
      attempt = timing_options$attempt, group_order = timing_options$group_orders[[fixture]],
      member_order = timing_options$member_order, excluded = FALSE,
      exclusion_reason = NA_character_, stringsAsFactors = FALSE
    )
    raw_results[[length(raw_results) + 1L]] <<- rbind(first_call_raw, timed_raw)
    data.frame(
      as.data.frame(fields, stringsAsFactors = FALSE), status = "PASS",
      correctness_status = if (identical(runner, "r") && !identical(variant, "optimized_base_r")) "REFERENCE" else "PASS",
      correctness_message = "validated by the complete fixture semantic and lifecycle gate",
      mean_ms = round(bm$mean_ms, 4), median_ms = round(bm$median_ms, 4),
      min_ms = round(bm$min_ms, 4), max_ms = round(bm$max_ms, 4), sd_ms = round(bm$sd_ms, 4),
      cv_pct = round(bm$cv_pct, 2), rss_endpoint_delta_kb = bm$rss_endpoint_delta_kb,
      first_call_ms = round(first_call$wall_ms, 3), n_iterations = bm$n_runs,
      stringsAsFactors = FALSE, as.data.frame(timing_fields(bm), stringsAsFactors = FALSE)
    )
  }
  
  summaries <- list()
  for (index in seq_len(nrow(rows))) {
    row <- rows[index, , drop = FALSE]
    fixture <- as.character(row$fixture)
    spec <- specs[[fixture]]
    fingerprint <- if (is.null(spec)) "not_applicable" else fixture_measurement_input_fingerprint(fixture, spec)
    if (isTRUE(row$timing_eligible)) {
      validate_timing_admission(as.list(row), runner, fixture)
      summaries[[length(summaries) + 1L]] <- measure(row, fixture)
    } else {
      fields <- identity_fields(row, fixture, "public", fingerprint)
      executable <- isTRUE(row$executable)
      summaries[[length(summaries) + 1L]] <- data.frame(
        as.data.frame(fields, stringsAsFactors = FALSE),
        status = if (executable) "CORRECTNESS_ONLY" else "N/A",
        correctness_status = if (executable) {
          if (identical(runner, "r")) "REFERENCE" else "PASS"
        } else "NOT_APPLICABLE",
        correctness_message = as.character(row$reason),
        mean_ms = NA_real_, median_ms = NA_real_, min_ms = NA_real_, max_ms = NA_real_,
        sd_ms = NA_real_, cv_pct = NA_real_, rss_endpoint_delta_kb = NA_integer_, first_call_ms = NA_real_,
        n_iterations = NA_integer_, stringsAsFactors = FALSE,
        as.data.frame(timing_fields(), stringsAsFactors = FALSE)
      )
    }
  }
  if (identical(runner, "r")) {
    for (fixture in intersect(names(fixture_measurement_optimized_specs()), as.character(rows$fixture))) {
      row <- rows[rows$fixture == fixture, , drop = FALSE]
      summaries[[length(summaries) + 1L]] <- measure(row, fixture, variant = "optimized_base_r")
    }
  }

  if (length(raw_results) > 0L) {
    write_csv(do.call(rbind, raw_results), file.path(staging_runner, "samples.csv"))
  }
  
  summary <- do.call(rbind, summaries)
  staged_summary <- file.path(staging_root, paste0("fixture_", runner, "_summary.csv"))
  write_csv(summary, staged_summary)
  cat(sprintf("Fixture batch output: %s\n", timing_options$output))
  invisible(summary)
}

worker_args <- commandArgs(trailingOnly = TRUE)
kind_args <- grep("^--kind=", worker_args, value = TRUE)
if (length(kind_args) != 1L) stop("exactly one --kind=task|fixture argument is required")
kind <- sub("^--kind=", "", kind_args[[1L]])
if (!(kind %in% c("task", "fixture"))) stop("--kind must be task or fixture")
worker_args <- worker_args[!grepl("^--kind=", worker_args)]
if (identical(kind, "task")) {
  validate_cli_arguments(
    worker_args,
    value_options = c(
      "runner", "tasks", "task-ids", "validation-output", "validated-correctness", "results-dir",
      "prepare-inputs", "input-manifest", "expected-input-manifest-digest", "master-seed",
      "timing-stage", "timing-counts", "batch-output", "batch", "attempt", "process-epoch",
      "member-order", "group-orders"
    ),
    flag_options = c("check-only", "validation-only"),
    label = "task worker"
  )
  run_task_worker(worker_args)
} else {
  validate_cli_arguments(
    worker_args,
    value_options = c(
      "runner", "run-dir", "validation-output", "validated-correctness", "proof-output", "fixtures",
      "memory-row", "memory-output",
      "timing-stage", "timing-counts", "batch-output", "batch", "attempt", "process-epoch",
      "member-order", "group-orders"
    ),
    flag_options = c("validation-only", "proof-only"),
    label = "fixture worker"
  )
  run_fixture_worker(worker_args)
}
