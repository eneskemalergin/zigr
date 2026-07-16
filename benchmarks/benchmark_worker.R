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

runner_names <- c("r", "c_call", "zigr", "rcpp", "cpp11", "extendr", "savvy")
if (!(runner %in% runner_names)) stop(sprintf("unknown runner: %s", runner))
if (!(mode %in% c("correctness", "sizing", "timing"))) {
  stop("worker mode must be correctness, sizing, or timing")
}
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
master_seed <- input_scalar_integer(master_seed, "master seed")

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
if (identical(mode, "timing")) {
  measurement_samples <- input_scalar_integer(measurement_samples, "measurement samples")
  batch_repetitions <- parse_named_integer_map(
    batch_repetitions, task_ids, "batch repetitions"
  )
}

source(file.path(root_dir, "src", "r", "run_all.R"), local = .GlobalEnv)
if (!methods::isClass("BenchS4")) methods::setClass("BenchS4", slots = c(slot_x = "numeric"))
c_dll <- dyn.load(file.path(root_dir, "src", "c_call", "bench.so"), local = TRUE, now = TRUE)
on.exit(try(dyn.unload(c_dll[["path"]]), silent = TRUE), add = TRUE)
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

runner_environment <- .GlobalEnv
if (!runner %in% c("r", "c_call")) {
  package <- fixture_package_map(root_dir)[[runner]]
  runner_environment <- loadNamespace(package$package, lib.loc = package$library)
}

runner_call <- function(spec, values) {
  if (identical(runner, "c_call")) {
    symbol <- getNativeSymbolInfo(paste0("c_revision_", spec$id), c_dll)
    return(direct_native_call(".Call", symbol, values))
  }
  function_object <- get(
    spec$function_name,
    envir = if (identical(runner, "r")) .GlobalEnv else runner_environment,
    inherits = FALSE
  )
  direct_function_call(function_object, values)
}

r_call <- function(spec, values) {
  direct_function_call(get(spec$function_name, envir = .GlobalEnv, inherits = FALSE), values)
}

is_unmaterialized_altrep <- function(value) {
  isTRUE(revision_native_call(c_dll, "c_revision_altrep_unmaterialized", list(value)))
}

altrep_tasks <- c("altrep_sum", "altrep_index", "altrep_materialize")
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
  if (spec$id %in% altrep_tasks && !is_unmaterialized_altrep(values[[1L]])) {
    stop(sprintf("%s/%s phase input is not an unmaterialized compact ALTREP", runner, spec$id))
  }
  list(values = values, call = runner_call(spec, values))
}

assert_altrep_phase <- function(spec, values) {
  if (spec$id %in% c("altrep_sum", "altrep_index") &&
      !is_unmaterialized_altrep(values[[1L]])) {
    stop(sprintf("%s/%s materialized compact ALTREP inside the timed call", runner, spec$id))
  }
}

if (identical(mode, "timing") && any(vapply(task_ids, function(task) {
  identical(direct_task_batchability(task), "one") && batch_repetitions[[task]] != 1L
}, logical(1)))) {
  stop("mutable, stateful, RNG, and ALTREP tasks require batch repetitions of one")
}

if (identical(mode, "sizing")) {
  rows <- list()
  for (spec in specs) {
    counts <- if (identical(direct_task_batchability(spec$id), "one")) 1L else c(1L, 8L, 64L)
    truth_arguments <- benchmark_revision_arguments(spec, master_seed)
    reset_rng(spec)
    truth <- eval(r_call(spec, truth_arguments), envir = .GlobalEnv)
    truth_rng <- if (isTRUE(spec$rng)) rng_state_snapshot() else NULL
    for (count in counts) {
      gc(full = TRUE)
      measured <- prepare_phase(spec)
      before <- if (length(measured$values) && !spec$id %in% altrep_tasks) {
        task_arguments_fingerprint(spec$id, measured$values, "ordinary_r_object")
      } else NULL
      reset_rng(spec)
      result <- measure_direct_batch(measured$call, runner_environment, count)
      if (!is.null(before)) {
        assert_immutable_input(spec$id, measured$values, before, "ordinary_r_object")
      }
      assert_altrep_phase(spec, measured$values)
      revision_assert_same(truth, result$result, paste0(spec$id, " sizing"), isTRUE(spec$tolerance))
      if (isTRUE(spec$rng)) assert_rng_state_equivalent(truth_rng, rng_state_snapshot(), spec$id)
      rows[[length(rows) + 1L]] <- data.frame(
        runner = runner, task = spec$id, batch_repetitions = count,
        batch_elapsed_ms = result$batch_elapsed_ms, gc_elapsed_ms = result$gc_elapsed_ms,
        stringsAsFactors = FALSE
      )
    }
    rm(truth_arguments, truth, truth_rng)
    gc(FALSE)
  }
  write_csv_once(do.call(rbind, rows), file.path(output_root, paste0(runner, "-sizing.csv")), "runner sizing")
  cat(sprintf("Direct sizing passed for %s across %d retained tasks.\n", runner, length(specs)))
  quit(save = "no", status = 0L, runLast = FALSE)
}

sample_rows <- list()
first_rows <- list()

for (spec in specs) {
  gc(full = TRUE)
  first <- prepare_phase(spec)
  reset_rng(spec)
  first_result <- measure_direct_batch(first$call, runner_environment, 1L)
  assert_altrep_phase(spec, first$values)
  first_rows[[length(first_rows) + 1L]] <- data.frame(
    runner = runner, task = spec$id, first_call_ms = first_result$batch_elapsed_ms,
    stringsAsFactors = FALSE
  )
  rm(first, first_result)

  gc(full = TRUE)
  warmup <- prepare_phase(spec)
  reset_rng(spec)
  invisible(eval(warmup$call, envir = runner_environment))
  assert_altrep_phase(spec, warmup$values)
  rm(warmup)

  gc(full = TRUE)
  calibration <- prepare_phase(spec)
  reset_rng(spec)
  repetitions <- batch_repetitions[[spec$id]]
  invisible(measure_direct_batch(calibration$call, runner_environment, repetitions))
  assert_altrep_phase(spec, calibration$values)
  rm(calibration)

  last_result <- NULL
  last_rng <- NULL
  for (sample in seq_len(measurement_samples)) {
    measured <- prepare_phase(spec)
    before <- if (length(measured$values) && !spec$id %in% altrep_tasks) {
      task_arguments_fingerprint(spec$id, measured$values, "ordinary_r_object")
    } else NULL
    reset_rng(spec)
    result <- measure_direct_batch(
      measured$call, runner_environment,
      repetitions
    )
    if (!is.null(before)) {
      assert_immutable_input(spec$id, measured$values, before, "ordinary_r_object")
    }
    assert_altrep_phase(spec, measured$values)
    if (isTRUE(spec$rng)) last_rng <- rng_state_snapshot()
    sample_rows[[length(sample_rows) + 1L]] <- data.frame(
      runner = runner, task = spec$id, phase = "measurement",
      measurement_sample = sample,
      batch_repetitions = result$batch_repetitions,
      batch_elapsed_ms = result$batch_elapsed_ms,
      elapsed_per_event_ms = result$elapsed_per_event_ms,
      gc_elapsed_ms = result$gc_elapsed_ms,
      stringsAsFactors = FALSE
    )
    last_result <- result$result
    rm(measured, result)
  }

  truth_arguments <- benchmark_revision_arguments(spec, master_seed)
  reset_rng(spec)
  truth <- eval(r_call(spec, truth_arguments), envir = .GlobalEnv)
  truth_rng <- if (isTRUE(spec$rng)) rng_state_snapshot() else NULL
  revision_assert_same(truth, last_result, paste0(spec$id, " post-timing"), isTRUE(spec$tolerance))
  if (isTRUE(spec$rng)) assert_rng_state_equivalent(truth_rng, last_rng, spec$id)
  rm(truth_arguments, truth, truth_rng, last_result, last_rng)
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
