#!/usr/bin/env Rscript

library(jsonlite)
source("lib/harness.R")
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
runner <- NULL
run_dir <- NULL
validation_only <- FALSE
validation_output <- NULL
validated_correctness <- NULL
proof_only <- FALSE
proof_output <- NULL
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
}
if (is.null(runner)) stop("--runner= is required")
if (is.null(run_dir)) stop("--run-dir= is required")
if (validation_only && is.null(validation_output)) stop("--validation-output= is required with --validation-only")
if (validation_only && !is.null(validated_correctness)) {
  stop("validation-only mode cannot reuse retained correctness")
}
if (proof_only && is.null(proof_output)) stop("--proof-output= is required with --proof-only")
if (validation_only && proof_only) stop("validation-only and proof-only modes are mutually exclusive")

root_dir <- normalizePath(".")
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
  if (file.exists(output_path)) stop(sprintf("fixture proof output already exists: %s", output_path))
  staged_output <- paste0(output_path, ".tmp-", Sys.getpid())
  on.exit(unlink(staged_output), add = TRUE)
  write_csv(proof_row, staged_output)
  if (!file.rename(staged_output, output_path)) stop(sprintf("cannot promote fixture proof output: %s", output_path))
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
    file.path(run_dir, "correctness", "fixtures", paste0(runner, ".csv")),
    mustWork = TRUE
  )
  expected_row_ids <- c(
    evidence$fixtures,
    if (identical(runner, "r")) c("F03_optimized_base_r", "F04_optimized_base_r") else character(0)
  )
  retained_correctness_rows <- load_retained_correctness(
    validated_correctness, expected_path, runner, as.character(metadata$run_id), "row_id", expected_row_ids,
    c(
      as.character(rows$fixture[rows$executable]),
      if (identical(runner, "r")) c("F03_optimized_base_r", "F04_optimized_base_r") else character(0)
    ),
    list(
      source_tree_digest = as.character(metadata$environment$source_tree$digest),
      source_ledger_identity_digest = as.character(environment$source_ledger_identity_digest),
      artifact_digest = as.character(environment$fixture_artifact_digest)
    )
  )
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
  output_path <- normalizePath(validation_output, mustWork = FALSE)
  if (file.exists(output_path)) stop(sprintf("fixture validation output already exists: %s", output_path))
  staged_output <- paste0(output_path, ".tmp-", Sys.getpid())
  on.exit(unlink(staged_output), add = TRUE)
  write_csv(correctness, staged_output)
  if (!file.rename(staged_output, output_path)) {
    stop(sprintf("cannot promote fixture validation output: %s", output_path))
  }
  cat(sprintf("Fixture correctness preflight passed for %s.\n", runner))
  quit(save = "no", status = 0L, runLast = FALSE)
}

timing_policy <- metadata$timing_policy
validate_timing_policy(timing_policy)
staging_root <- file.path(run_dir, ".staging", "fixtures")
staging_runner <- file.path(staging_root, runner)
dir.create(staging_runner, recursive = TRUE, showWarnings = FALSE)
unlink(list.files(staging_runner, full.names = TRUE, all.files = TRUE, no.. = TRUE), recursive = TRUE)

timing_fields <- function(bm = NULL) {
  if (is.null(bm)) return(list(
    warmup_iterations = as.integer(timing_policy$warmup_iterations),
    block_size = as.integer(timing_policy$block_size),
    max_iterations = as.integer(timing_policy$max_iterations),
    convergence_window_blocks = as.integer(timing_policy$convergence_window_blocks),
    convergence_cv_threshold_pct = as.numeric(timing_policy$convergence_cv_threshold_pct),
    convergence_cv_pct = NA_real_, stopping_condition = "not_measured", converged = NA,
    timer_noise_floor_ms = as.numeric(timing_policy$timer_noise_floor_ms),
    timer_noise_status = "not_measured", rss_metric = as.character(timing_policy$rss_metric),
    gc_policy = as.character(timing_policy$gc_policy)
  ))
  list(
    warmup_iterations = as.integer(bm$warmup_iterations), block_size = as.integer(bm$block_size),
    max_iterations = as.integer(bm$max_iterations),
    convergence_window_blocks = as.integer(bm$convergence_window_blocks),
    convergence_cv_threshold_pct = as.numeric(bm$convergence_cv_threshold_pct),
    convergence_cv_pct = as.numeric(bm$convergence_cv_pct),
    stopping_condition = as.character(bm$stopping_condition), converged = isTRUE(bm$converged),
    timer_noise_floor_ms = as.numeric(bm$timer_noise_floor_ms),
    timer_noise_status = as.character(bm$timer_noise_status), rss_metric = as.character(bm$rss_metric),
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
  prepare_fresh <- if (identical(variant, "optimized_base_r")) {
    fn <- context$optimized[[fixture]]
    function() {
      arguments <- spec$arguments()
      function() do.call(fn, arguments)
    }
  } else {
    function() fixture_measurement_prepare(functions, fixture, spec)
  }
  cold <- timed_call(prepare_fresh)
  if (!is.na(cold$error)) stop(sprintf("fixture cold call failed for %s/%s: %s", runner, fields$row_id, cold$error))

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
    warmup = as.integer(timing_policy$warmup_iterations),
    block_size = as.integer(timing_policy$block_size),
    max_iter = as.integer(timing_policy$max_iterations),
    cv_threshold = as.numeric(timing_policy$convergence_cv_threshold_pct),
    convergence_blocks = as.integer(timing_policy$convergence_window_blocks),
    timer_noise_floor_ms = as.numeric(timing_policy$timer_noise_floor_ms),
    rss_metric = as.character(timing_policy$rss_metric)
  )
  if (!is.na(bm$error)) stop(sprintf("fixture timing failed for %s/%s: %s", runner, fields$row_id, bm$error))
  if (reusable) {
    assert_immutable_input(paste0("fixture:", fixture), warmup_arguments, warmup_before, intent)
    assert_immutable_input(paste0("fixture:", fixture), timed_arguments, timed_before, intent)
  }
  raw <- data.frame(
    run_id = as.character(metadata$run_id), runner = runner, fixture = fixture,
    variant = variant, row_id = fields$row_id, iteration = seq_along(bm$times),
    wall_ms = bm$times, stringsAsFactors = FALSE
  )
  write_csv(raw, file.path(staging_runner, paste0(fields$row_id, ".csv")))
  data.frame(
    as.data.frame(fields, stringsAsFactors = FALSE), status = "PASS",
    correctness_status = if (identical(runner, "r") && !identical(variant, "optimized_base_r")) "REFERENCE" else "PASS",
    correctness_message = "validated by the complete fixture semantic and lifecycle gate",
    mean_ms = round(bm$mean_ms, 4), median_ms = round(bm$median_ms, 4),
    min_ms = round(bm$min_ms, 4), max_ms = round(bm$max_ms, 4), sd_ms = round(bm$sd_ms, 4),
    cv_pct = round(bm$cv_pct, 2), rss_kb = bm$rss_delta_kb,
    cold_start_ms = round(cold$wall_ms, 3), n_iterations = bm$n_runs,
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
      sd_ms = NA_real_, cv_pct = NA_real_, rss_kb = NA_integer_, cold_start_ms = NA_real_,
      n_iterations = NA_integer_, stringsAsFactors = FALSE,
      as.data.frame(timing_fields(), stringsAsFactors = FALSE)
    )
  }
}
if (identical(runner, "r")) {
  for (fixture in names(fixture_measurement_optimized_specs())) {
    row <- rows[rows$fixture == fixture, , drop = FALSE]
    summaries[[length(summaries) + 1L]] <- measure(row, fixture, variant = "optimized_base_r")
  }
}

summary <- do.call(rbind, summaries)
staged_summary <- file.path(staging_root, paste0("fixture_", runner, "_summary.csv"))
write_csv(summary, staged_summary)
final_root <- file.path(run_dir, "fixtures")
dir.create(final_root, recursive = TRUE, showWarnings = FALSE)
final_runner <- file.path(final_root, runner)
final_summary <- file.path(run_dir, paste0("fixture_", runner, "_summary.csv"))
if (dir.exists(final_runner) || file.exists(final_summary)) {
  stop(sprintf("final fixture result path already exists for %s", runner))
}
if (!file.rename(staging_runner, final_runner)) stop(sprintf("cannot promote fixture raw results for %s", runner))
if (!file.rename(staged_summary, final_summary)) stop(sprintf("cannot promote fixture summary for %s", runner))
cat(sprintf("Fixture measurement completed for %s: %d rows.\n", runner, nrow(summary)))
