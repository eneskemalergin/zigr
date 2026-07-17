#!/usr/bin/env Rscript

library(methods)

root_dir <- normalizePath(".")
source(file.path(root_dir, "lib", "measurement.R"))
source(file.path(root_dir, "lib", "product_fixtures.R"))

option_value <- function(arguments, name, required = FALSE) {
  matches <- grep(paste0("^--", name, "="), arguments, value = TRUE)
  if (length(matches) > 1L) stop(sprintf("--%s may be supplied once", name))
  if (length(matches) == 0L) {
    if (required) stop(sprintf("--%s is required", name))
    return(NULL)
  }
  sub(paste0("^--", name, "="), "", matches[[1L]])
}

arguments <- commandArgs(trailingOnly = TRUE)
runner <- option_value(arguments, "runner", required = TRUE)
mode <- option_value(arguments, "mode", required = TRUE)
output_root <- normalizePath(option_value(arguments, "output-root", required = TRUE), mustWork = FALSE)
task_filter <- option_value(arguments, "tasks")
measurement_samples <- option_value(arguments, "measurement-samples")
batch_repetitions <- option_value(arguments, "batch-repetitions")
master_seed <- option_value(arguments, "master-seed")
skip_probes <- "--skip-probes" %in% arguments

runner_names <- c("r", "c_call", "zigr", "rcpp", "cpp11", "extendr", "savvy")
if (!(runner %in% runner_names)) stop(sprintf("unknown runner: %s", runner))
if (!(mode %in% c("correctness", "sizing", "timing", "memory"))) {
  stop("worker mode must be correctness, sizing, timing, or memory")
}
if (skip_probes && !identical(mode, "sizing")) {
  stop("--skip-probes is allowed only for later sizing rounds")
}
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
master_seed <- input_scalar_integer(master_seed, "master seed")
invisible(compiler::enableJIT(0L))

specs <- benchmark_revision_task_specs()
if (!is.null(task_filter)) {
  selected <- strsplit(task_filter, ",", fixed = TRUE)[[1L]]
  ids <- vapply(specs, `[[`, character(1), "id")
  if (any(!nzchar(selected)) || anyDuplicated(selected) || !all(selected %in% ids)) {
    stop("task selection contains an unknown or duplicate ID")
  }
  specs <- specs[match(selected, ids)]
}

if (identical(mode, "correctness")) {
  passed <- run_benchmark_revision_gate(
    root_dir, runner, vapply(specs, `[[`, character(1), "id"), master_seed
  )
  if (!identical(passed, length(specs))) stop(sprintf("correctness gate returned %s", passed))
  rows <- data.frame(
    runner = runner,
    task = vapply(specs, `[[`, character(1), "id"),
    status = "PASS",
    stringsAsFactors = FALSE
  )
  write_csv_once(rows, file.path(output_root, paste0(runner, ".csv")), "runner correctness")
  quit(save = "no", status = 0L, runLast = FALSE)
}

task_ids <- vapply(specs, `[[`, character(1), "id")
if (mode %in% c("sizing", "timing")) {
  if (is.null(batch_repetitions)) stop("--batch-repetitions is required for sizing and timing")
  if (identical(mode, "timing")) {
    measurement_samples <- input_scalar_integer(measurement_samples, "measurement samples")
  }
  batch_repetitions <- parse_named_integer_map(
    batch_repetitions, task_ids, "batch repetitions"
  )
  if (any(!batch_repetitions %in% validate_direct_sizing_policy(direct_sizing_policy())$ladder)) {
    stop("batch repetitions must use the declared sizing ladder")
  }
}

source(file.path(root_dir, "src", "r", "run_all.R"), local = .GlobalEnv)
if (!methods::isClass("BenchS4")) methods::setClass("BenchS4", slots = c(slot_x = "numeric"))
c_dll <- dyn.load(file.path(root_dir, "src", "c_call", "bench.so"), local = TRUE, now = TRUE)
on.exit(try(dyn.unload(c_dll[["path"]]), silent = TRUE), add = TRUE)
native_checks <- direct_revision_native_checks(c_dll)
if (!skip_probes && !identical(mode, "memory")) {
  probes <- run_direct_measurement_probes(c_dll)
  probe_rows <- transform(probes$samples, runner = runner)
  probe_rows <- probe_rows[c(
    "runner", "probe", "probe_sample", "batch_repetitions", "batch_elapsed_ms",
    "elapsed_per_event_ms", "gc_elapsed_ms"
  )]
  write_csv_once(
    probe_rows, file.path(output_root, paste0(runner, "-probes.csv")), "runner probes"
  )
  write_csv_once(data.frame(
    runner = runner,
    timer_floor_ms = probes$timer_floor_ms,
    nanotime_elapsed_ms = probes$nanotime_elapsed_ms,
    independent_elapsed_ms = probes$independent_elapsed_ms,
    stringsAsFactors = FALSE
  ), file.path(output_root, paste0(runner, "-probe-summary.csv")), "runner probe summary")
}

runner_environment <- direct_runner_environment(root_dir, runner)

runner_entry <- function(spec) {
  if (identical(runner, "c_call")) {
    symbol <- getNativeSymbolInfo(paste0("c_revision_", spec$id), c_dll)
    return(function(values) direct_native_call(".Call", symbol, values))
  }
  function_object <- get(
    spec$function_name,
    envir = if (identical(runner, "r")) .GlobalEnv else runner_environment,
    inherits = FALSE
  )
  function(values) direct_function_call(function_object, values)
}

r_call <- function(spec, values) {
  direct_function_call(get(spec$function_name, envir = .GlobalEnv, inherits = FALSE), values)
}

altrep_tasks <- task_ids[vapply(task_ids, direct_task_is_altrep, logical(1))]
rng_seed <- task_input_seed(master_seed, "rng", "direct-timing-v1")
reset_rng <- function(spec) {
  if (isTRUE(spec$rng)) {
    set.seed(
      rng_seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
      sample.kind = "Rejection"
    )
  }
}

prepare_phase <- function(spec) {
  values <- benchmark_revision_arguments(spec, master_seed)
  if (spec$id %in% altrep_tasks) {
    direct_assert_altrep_phase(
      spec, values[[1L]], native_checks$is_unmaterialized_altrep,
      sprintf("%s/%s", runner, spec$id), "before"
    )
  }
  list(values = values, call = runner_entries[[spec$id]](values))
}

assert_altrep_phase <- function(spec, values) {
  if (spec$id %in% altrep_tasks) {
    direct_assert_altrep_phase(
      spec, values[[1L]], native_checks$is_unmaterialized_altrep,
      sprintf("%s/%s", runner, spec$id), "after"
    )
  }
}

runner_entries <- setNames(lapply(specs, runner_entry), vapply(specs, `[[`, character(1), "id"))

phase_truth <- function(spec) {
  arguments <- benchmark_revision_arguments(spec, master_seed)
  reset_rng(spec)
  result <- eval(r_call(spec, arguments), envir = .GlobalEnv)
  list(
    result = result,
    rng = if (isTRUE(spec$rng)) rng_state_snapshot() else NULL
  )
}

run_prepared_phase <- function(spec, phase, repetitions, truth, verify_result = TRUE,
                               timed = TRUE) {
  prepared <- prepare_phase(spec)
  before <- if (length(prepared$values) && !spec$id %in% altrep_tasks) {
    task_arguments_fingerprint(spec$id, prepared$values, "ordinary_r_object")
  } else NULL
  reset_rng(spec)
  measured <- if (timed) {
    measure_direct_batch(prepared$call, runner_environment, repetitions)
  } else {
    list(result = eval(direct_batch_expression(prepared$call, repetitions), envir = runner_environment))
  }
  if (!is.null(before)) {
    assert_immutable_input(spec$id, prepared$values, before, "ordinary_r_object")
  }
  assert_altrep_phase(spec, prepared$values)
  if (verify_result) {
    direct_assert_result_parity(truth$result, measured$result, spec, paste0(spec$id, " ", phase))
    direct_assert_altrep_result(
      spec, measured$result, native_checks$is_altrep, sprintf("%s/%s", runner, spec$id)
    )
    if (isTRUE(spec$rng)) assert_rng_state_equivalent(truth$rng, rng_state_snapshot(), spec$id)
  }
  measured
}

if (identical(mode, "memory")) {
  if (length(specs) != 1L) stop("memory workers require exactly one task")
  spec <- specs[[1L]]
  truth <- phase_truth(spec)
  prepared <- prepare_phase(spec)
  before <- if (length(prepared$values) && !spec$id %in% altrep_tasks) {
    task_arguments_fingerprint(spec$id, prepared$values, "ordinary_r_object")
  } else NULL
  reset_rng(spec)
  gc(full = TRUE)
  baseline <- direct_process_memory_snapshot()
  result <- eval(prepared$call, envir = runner_environment)
  if (!is.null(before)) {
    assert_immutable_input(spec$id, prepared$values, before, "ordinary_r_object")
  }
  assert_altrep_phase(spec, prepared$values)
  direct_assert_result_parity(truth$result, result, spec, paste0(spec$id, " memory"))
  direct_assert_altrep_result(
    spec, result, native_checks$is_altrep, sprintf("%s/%s", runner, spec$id)
  )
  if (isTRUE(spec$rng)) assert_rng_state_equivalent(truth$rng, rng_state_snapshot(), spec$id)
  after <- direct_process_memory_snapshot()
  supported <- !is.null(baseline) && !is.null(after)
  memory_status <- direct_memory_event_status(baseline, after)
  row <- data.frame(
    runner = runner, task = spec$id,
    memory_status = memory_status$status,
    rss_metric = if (supported) direct_memory_policy()$rss_metric else "unsupported",
    loaded_process_rss_kb = if (supported) baseline$rss_kb else NA_real_,
    initial_process_high_water_rss_kb = if (supported) baseline$hwm_kb else NA_real_,
    process_high_water_rss_kb = if (supported) after$hwm_kb else NA_real_,
    swap_before_kb = if (supported) baseline$swap_kb else NA_real_,
    swap_after_kb = if (supported) after$swap_kb else NA_real_,
    reason = memory_status$reason,
    stringsAsFactors = FALSE
  )
  rm(truth, prepared, result)
  gc(FALSE)
  validate_direct_memory_summary(row, runner, spec$id)
  write_csv_once(row, file.path(output_root, paste0(runner, "-memory.csv")), "runner memory")
  cat(sprintf("Direct memory recorded for %s/%s.\n", runner, spec$id))
  quit(save = "no", status = 0L, runLast = FALSE)
}

if (mode %in% c("sizing", "timing") && any(vapply(task_ids, function(task) {
  identical(direct_task_batchability(task), "one") && batch_repetitions[[task]] != 1L
}, logical(1)))) {
  stop("mutable, stateful, RNG, and ALTREP tasks require batch repetitions of one")
}

if (identical(mode, "sizing")) {
  rows <- list()
  for (spec in specs) {
    count <- batch_repetitions[[spec$id]]
    truth <- phase_truth(spec)
    gc(full = TRUE)
    result <- run_prepared_phase(spec, "shared sizing", count, truth)
    rows[[length(rows) + 1L]] <- data.frame(
      runner = runner, task = spec$id, batch_repetitions = count,
      batch_elapsed_ms = result$batch_elapsed_ms, gc_elapsed_ms = result$gc_elapsed_ms,
      stringsAsFactors = FALSE
    )
    rm(truth, result)
    gc(FALSE)
  }
  write_csv_once(do.call(rbind, rows), file.path(output_root, paste0(runner, "-sizing.csv")), "runner sizing")
  cat(sprintf("Direct sizing passed for %s across %d retained tasks.\n", runner, length(specs)))
  quit(save = "no", status = 0L, runLast = FALSE)
}

sample_rows <- list()
first_rows <- list()

for (spec in specs) {
  truth <- phase_truth(spec)

  gc(full = TRUE)
  first_result <- run_prepared_phase(spec, "first call", 1L, truth)
  first_rows[[length(first_rows) + 1L]] <- data.frame(
    runner = runner, task = spec$id, first_call_ms = first_result$batch_elapsed_ms,
    stringsAsFactors = FALSE
  )
  rm(first_result)

  gc(full = TRUE)
  warmup_result <- run_prepared_phase(spec, "warmup", 1L, truth, timed = FALSE)
  rm(warmup_result)

  vector_heap_trigger_vcells <- direct_vector_heap_trigger_vcells(gc(full = TRUE))
  repetitions <- batch_repetitions[[spec$id]]
  calibration_result <- run_prepared_phase(spec, "local calibration", repetitions, truth)
  rm(calibration_result)

  last_result <- NULL
  last_rng <- NULL
  for (sample in seq_len(measurement_samples)) {
    result <- run_prepared_phase(spec, "measurement", repetitions, truth, verify_result = FALSE)
    if (isTRUE(spec$rng)) last_rng <- rng_state_snapshot()
    sample_rows[[length(sample_rows) + 1L]] <- data.frame(
      runner = runner, task = spec$id, phase = "measurement",
      measurement_sample = sample,
      batch_repetitions = result$batch_repetitions,
      batch_elapsed_ms = result$batch_elapsed_ms,
      elapsed_per_event_ms = result$elapsed_per_event_ms,
      gc_elapsed_ms = result$gc_elapsed_ms,
      vector_heap_trigger_vcells = vector_heap_trigger_vcells,
      stringsAsFactors = FALSE
    )
    last_result <- result$result
    rm(result)
  }

  direct_assert_result_parity(truth$result, last_result, spec, paste0(spec$id, " post-timing"))
  direct_assert_altrep_result(
    spec, last_result, native_checks$is_altrep, sprintf("%s/%s", runner, spec$id)
  )
  if (isTRUE(spec$rng)) assert_rng_state_equivalent(truth$rng, last_rng, spec$id)
  rm(truth, last_result, last_rng)
  gc(FALSE)
}

samples <- do.call(rbind, sample_rows)
first_calls <- do.call(rbind, first_rows)
validate_direct_timing_samples(
  samples, runner, vapply(specs, `[[`, character(1), "id"), measurement_samples
)
write_csv_once(samples, file.path(output_root, paste0(runner, "-samples.csv")), "runner samples")
write_csv_once(first_calls, file.path(output_root, paste0(runner, "-first-call.csv")), "runner first calls")

cat(sprintf("Direct timing passed for %s across %d retained tasks.\n", runner, length(specs)))
