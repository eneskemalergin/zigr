#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("cannot determine measurement test location")
script_path <- normalizePath(sub("^--file=", "", script_argument[[1L]]))
root_dir <- normalizePath(file.path(dirname(script_path), ".."))

source(file.path(root_dir, "lib", "measurement.R"))

expect_true <- function(condition, label) {
  if (!isTRUE(condition)) stop(sprintf("assertion failed: %s", label), call. = FALSE)
}

expect_error <- function(label, expression, pattern) {
  error <- tryCatch({
    force(expression)
    NULL
  }, error = function(condition) condition)
  if (is.null(error)) stop(sprintf("negative test did not fail: %s", label), call. = FALSE)
  if (!grepl(pattern, conditionMessage(error), perl = TRUE)) {
    stop(sprintf(
      "negative test %s failed for the wrong reason: %s", label, conditionMessage(error)
    ), call. = FALSE)
  }
  invisible(error)
}

specs <- benchmark_revision_task_specs()
expect_true(
  length(specs) == 27L && !anyDuplicated(vapply(specs, `[[`, character(1), "id")) &&
    !anyDuplicated(vapply(specs, `[[`, character(1), "function_name")),
  "the direct path has one unique identity for each retained event"
)
suitability <- validate_direct_task_suitability()
task_set <- function(field) suitability$task[suitability[[field]]]
expect_true(
  all(suitability$immutable_input) && !any(suitability$input_mutating) &&
    identical(task_set("stateful"), "rng") &&
    identical(task_set("representation_changing"), c("altrep_sum", "altrep_index", "altrep_materialize")) &&
    identical(task_set("large_output"), c(
      "numeric_transform", "sort", "transpose", "matmul", "attributes", "raw_copy",
      "complex_conjugate", "altrep_materialize", "serialize", "rng"
    )) &&
    identical(task_set("gc_relevant"), c(
      "numeric_transform", "sort", "transpose", "matmul", "attributes", "raw_copy",
      "complex_conjugate", "altrep_materialize", "serialize", "rng"
    )) &&
    identical(task_set("small_output"), c(
      "rowcol", "dataframe", "string_concat", "string_metadata", "factor", "logical_counts",
      "outputs"
    )) &&
    identical(task_set("matrix_task"), c("transpose", "rowcol", "matmul")) &&
    identical(
      vapply(specs, function(spec) isTRUE(spec$altrep), logical(1)),
      suitability$representation_changing
    ) &&
    identical(
      vapply(specs, function(spec) isTRUE(spec$rng), logical(1)),
      suitability$stateful
    ) &&
    identical(
      vapply(specs, direct_task_altrep_input_postcondition, character(1)),
      c(rep("ordinary", 19L), "preserve", "preserve", "allow_change", rep("ordinary", 5L))
    ),
  "the task suitability map records the declared input, state, representation, output, matrix, and ALTREP contracts"
)

invalid_altrep_spec <- specs[[which(vapply(specs, `[[`, character(1), "id") == "altrep_sum")]]
invalid_altrep_spec$altrep_input_postcondition <- "invalid"
expect_error(
  "ALTREP postcondition rejects an unrecognized preservation policy",
  direct_task_altrep_input_postcondition(invalid_altrep_spec),
  "invalid input postcondition"
)
preserve_spec <- specs[[which(vapply(specs, `[[`, character(1), "id") == "altrep_sum")]]
materialize_spec <- specs[[which(vapply(specs, `[[`, character(1), "id") == "altrep_materialize")]]
expect_error(
  "ALTREP preserve policy rejects an observed materialized input",
  assert_direct_task_altrep_input(preserve_spec, FALSE, "rcpp/altrep_sum"),
  "rcpp/altrep_sum materialized compact ALTREP inside the timed call"
)
expect_true(
  identical(assert_direct_task_altrep_input(preserve_spec, TRUE), "preserve") &&
    identical(assert_direct_task_altrep_input(materialize_spec, FALSE), "allow_change"),
  "ALTREP post-event checks preserve required inputs and allow the declared materializer"
)

first <- benchmark_revision_arguments(specs[[1L]])
second <- benchmark_revision_arguments(specs[[1L]])
alternate_seed <- benchmark_master_seed() + 1L
alternate <- benchmark_revision_arguments(specs[[1L]], alternate_seed)
expect_true(
  identical(first, second) && !identical(first, benchmark_revision_arguments(specs[[2L]])) &&
    !identical(first, alternate),
  "task inputs are deterministic, task-specific, and controlled by the run seed"
)
set.seed(99173L)
caller_rng <- rng_state_snapshot()
invisible(benchmark_revision_arguments(specs[[1L]], alternate_seed))
expect_true(
  identical(rng_state_snapshot(), caller_rng),
  "task input construction preserves the caller RNG state"
)

fingerprint <- function(value) {
  task_arguments_fingerprint("fingerprint-test", list(value), "ordinary_r_object")
}
utf8_value <- enc2utf8("façade")
latin1_value <- iconv("façade", from = "UTF-8", to = "latin1")
Encoding(utf8_value) <- "UTF-8"
Encoding(latin1_value) <- "latin1"
fingerprints <- c(
  fingerprint(NA_real_), fingerprint(NaN), fingerprint(-0.0), fingerprint(0.0),
  fingerprint(c(value = 1)), fingerprint(1), fingerprint(utf8_value), fingerprint(latin1_value),
  fingerprint(list(list(1))), fingerprint(list(list(2)))
)
expect_true(
  !anyDuplicated(fingerprints),
  "input fingerprints distinguish missing kind, sign, attributes, encoding, and nested values"
)
immutable_arguments <- list(c(1, 2, 3))
immutable_before <- task_arguments_fingerprint(
  "immutable-test", immutable_arguments, "ordinary_r_object"
)
immutable_arguments[[1L]][[2L]] <- 9
expect_error(
  "immutable input mutation",
  assert_immutable_input(
    "immutable-test", immutable_arguments, immutable_before, "ordinary_r_object"
  ),
  "immutable input mutated"
)

clock_values <- c(10, 10.025)
clock_calls <- 0L
clock <- function() {
  clock_calls <<- clock_calls + 1L
  clock_values[[clock_calls]]
}
event_calls <- 0L
event <- function(value) {
  event_calls <<- event_calls + 1L
  value + event_calls
}
prepared_arguments <- list(40L)
prepared_call <- direct_function_call(event, prepared_arguments)
measured <- measure_direct_batch(prepared_call, environment(), 4L, clock)
expect_true(
  clock_calls == 2L && event_calls == 4L && identical(measured$result, 44L) &&
    identical(measured$batch_repetitions, 4L) &&
    abs(measured$batch_elapsed_ms - 25) < 1e-9 &&
    abs(measured$elapsed_per_event_ms - 6.25) < 1e-9,
  "one timer interval surrounds the exact declared event batch"
)

zero_calls <- 0L
zero <- function() {
  zero_calls <<- zero_calls + 1L
  1L
}
scalar_calls <- 0L
scalar <- function(value) {
  scalar_calls <<- scalar_calls + 1L
  value
}
vector_calls <- 0L
vector <- function(value) {
  vector_calls <<- vector_calls + 1L
  sum(value)
}
state_calls <- 0L
stateful <- function() {
  state_calls <<- state_calls + 1L
  state <- 0L
  for (index in seq_len(100L)) state <- state + 7L
  state
}
constant_clock <- local({
  value <- 0
  function() {
    value <<- value + 0.001
    value
  }
})
invisible(measure_direct_batch(direct_function_call(zero), environment(), 3L, constant_clock))
invisible(measure_direct_batch(
  direct_function_call(scalar, list(3.5)), environment(), 2L, constant_clock
))
invisible(measure_direct_batch(
  direct_function_call(vector, list(1:10)), environment(), 5L, constant_clock
))
state_result <- measure_direct_batch(
  direct_function_call(stateful), environment(), 1L, constant_clock
)
expect_true(
  zero_calls == 3L && scalar_calls == 2L && vector_calls == 5L && state_calls == 1L &&
    identical(state_result$result, 700L),
  "zero-argument, scalar, vector, and stateful calls execute exact event counts"
)

fake_symbol <- structure(list(address = new("externalptr")), class = "NativeSymbolInfo")
call_shape <- direct_native_call(".Call", fake_symbol, list(1L, 2L))
external_shape <- direct_native_call(".External", fake_symbol, list(1L))
expect_true(
  identical(as.character(call_shape[[1L]]), ".Call") && length(call_shape) == 4L &&
    identical(as.character(external_shape[[1L]]), ".External") && length(external_shape) == 3L,
  ".Call and .External expressions contain only the resolved symbol and declared arguments"
)

c_library <- file.path(root_dir, "src", "c_call", paste0("bench", .Platform$dynlib.ext))
if (!file.exists(c_library)) stop("direct native timing proof requires the built C benchmark library")
c_dll <- dyn.load(c_library, local = TRUE, now = TRUE)
on.exit(try(dyn.unload(c_dll[["path"]]), silent = TRUE), add = TRUE)
native_symbol <- function(name) getNativeSymbolInfo(name, c_dll)
invisible(eval(direct_native_call(".Call", native_symbol("c_benchmark_lifecycle_reset"))))
native_clock <- local({
  value <- 0
  function() {
    value <<- value + 0.001
    value
  }
})
native_result <- measure_direct_batch(
  direct_native_call(".Call", native_symbol("c_revision_external_state")),
  environment(), 3L, native_clock
)
lifecycle <- eval(direct_native_call(".Call", native_symbol("c_benchmark_lifecycle_snapshot")))
external_result <- measure_direct_batch(
  direct_native_call(".External", native_symbol("c_fixture_external"), list(2.5)),
  environment(), 4L, native_clock
)
expect_true(
  identical(native_result$result, 700L) && identical(unname(lifecycle[1:2]), c(3L, 303L)),
  "a live .Call batch executes exactly the declared number of native events"
)
expect_true(
  identical(external_result$result, 3.5) && identical(external_result$batch_repetitions, 4L),
  "a live .External batch uses the direct evaluator"
)
external_batch <- as.list(direct_batch_expression(
  direct_native_call(".External", native_symbol("c_fixture_external"), list(2.5)), 4L
))
expect_true(
  length(external_batch) == 5L &&
    all(vapply(external_batch[-1L], identical, logical(1), external_batch[[2L]])),
  "a .External batch expression contains exactly the declared number of events"
)

released_batch_results <- 0L
batch_result <- function() {
  value <- new.env(parent = emptyenv())
  reg.finalizer(value, function(...) released_batch_results <<- released_batch_results + 1L,
                onexit = FALSE)
  value
}
retained_batch_result <- measure_direct_batch(
  direct_function_call(batch_result), environment(), 4L, constant_clock
)
invisible(gc(full = TRUE))
expect_true(
  identical(released_batch_results, 3L),
  "a repeated batch retains only its final result while its intermediate results become unreachable"
)
rm(retained_batch_result)
invisible(gc(full = TRUE))
expect_true(
  identical(released_batch_results, 4L),
  "the harness releases the final batch result when its owning phase drops it"
)

preparations <- 0L
prepare <- function() {
  preparations <<- preparations + 1L
  list(7L)
}
phase_arguments <- prepare()
phase_call <- direct_function_call(identity, phase_arguments)
phase_clock_calls <- 0L
phase_clock <- local({
  value <- 0
  function() {
    phase_clock_calls <<- phase_clock_calls + 1L
    value <<- value + 0.001
    value
  }
})
phase_result <- measure_direct_batch(phase_call, environment(), 1L, phase_clock)
post_validation <- identical(phase_result$result, 7L)
expect_true(
  preparations == 1L && phase_clock_calls == 2L && post_validation,
  "argument preparation and post-measurement validation stay outside the timer"
)

samples <- data.frame(
  runner = rep(c("r", "c_call"), each = 3L), task = "vector_sum",
  phase = "measurement", measurement_sample = rep(1:3, 2L),
  batch_repetitions = 2L,
  batch_elapsed_ms = c(2, 4, 6, 3, 5, 7),
  elapsed_per_event_ms = c(1, 2, 3, 1.5, 2.5, 3.5),
  gc_elapsed_ms = 0,
  vector_heap_trigger_vcells = 8388608L,
  stringsAsFactors = FALSE
)
validate_direct_timing_samples(samples, c("r", "c_call"))
first_calls <- data.frame(
  runner = c("r", "c_call"), task = "vector_sum", first_call_ms = c(1.2, 1.4),
  stringsAsFactors = FALSE
)
summary <- summarize_direct_timing(samples, first_calls)
expect_true(
  nrow(summary) == 2L && identical(summary$measurement_samples, c(3L, 3L)) &&
    identical(summary$batch_repetitions, c(2L, 2L)) &&
    all(summary$median_ms == c(2.5, 2)) && all(summary$mean_ms == c(2.5, 2)),
  "direct summaries use only the declared measurement samples"
)
expect_true(
  identical(samples$measurement_sample, rep(1:3, 2L)) &&
    identical(summary$distribution_status, c("PASS", "PASS")),
  "raw sample order is retained and summary construction does not reorder the sequence"
)
batch_map <- direct_batch_repetition_map(c("vector_sum", "attributes"), 64L)
expect_true(
  identical(unname(batch_map), c(64L, 64L)),
  "fresh-output tasks may receive an expanded batch"
)
external_state_batch <- direct_batch_repetition_map("external_state", 64L)
expect_true(
  identical(unname(external_state_batch), 64L) &&
    identical(direct_task_batchability("external_state"), "repeat"),
  "fresh external state is recreated for every repeated event"
)
batchability <- vapply(
  c("altrep_sum", "altrep_index", "altrep_materialize", "external_state", "rng", "sort", "attributes"),
  direct_task_batchability, character(1)
)
expect_true(
  identical(
    unname(batchability),
    c("one", "one", "one", "repeat", "one", "repeat", "repeat")
  ),
  "only cross-event state, RNG, and representation-changing events require a single invocation"
)
allocation_classes <- vapply(
  c("complex_conjugate", "schema", "outputs"), direct_task_allocation_class, character(1)
)
expect_true(
  identical(unname(allocation_classes), c("large_output", "ordinary_result", "ordinary_result")) &&
    identical(direct_task_output_vcells("complex_conjugate"), 65536L) &&
    identical(direct_task_output_vcells("schema"), 0L),
  "only the declared large complex output has a GC-observation requirement"
)
expect_true(
  identical(remaining_direct_run_seconds(10, 609.9, 600L), 0) &&
    identical(remaining_direct_run_seconds(10, 610, 600L), 0),
  "one run-wide timeout budget covers both sizing and measurement elapsed time"
)
sizing_rows <- data.frame(
  runner = rep(c("r", "c_call"), each = 4L),
  task = rep(c("vector_sum", "vector_sum", "attributes", "attributes"), times = 2L),
  batch_repetitions = rep(c(1L, 8L, 1L, 8L), times = 2L),
  batch_elapsed_ms = c(0.2, 6, 0.2, 6, 0.2, 6, 0.2, 6),
  gc_elapsed_ms = 0,
  stringsAsFactors = FALSE
)
expect_true(
  identical(
    unname(select_direct_batch_repetitions(
      sizing_rows, c(r = 0.01, c_call = 0.01), c("r", "c_call"),
      c("vector_sum", "attributes")
    )),
    c(8L, 8L)
  ),
  "sizing selects the smallest shared safe batch for immutable and fresh-output tasks"
)
expect_error(
  "sizing rejects a favorable-runner-only ladder step",
  select_direct_batch_repetitions(
    sizing_rows[-6L, , drop = FALSE], c(r = 0.01, c_call = 0.01),
    c("r", "c_call"), c("vector_sum", "attributes")
  ),
  "coverage differs"
)
unnecessary_sizing <- sizing_rows
unnecessary_sizing$batch_elapsed_ms[unnecessary_sizing$batch_repetitions == 1L] <- 6
expect_error(
  "sizing rejects a later step after a shared success",
  select_direct_batch_repetitions(
    unnecessary_sizing, c(r = 0.01, c_call = 0.01),
    c("r", "c_call"), c("vector_sum", "attributes")
  ),
  "unnecessary or undeclared"
)
over_cap_sizing <- sizing_rows
over_cap_sizing$batch_elapsed_ms[over_cap_sizing$batch_repetitions == 8L] <- 251
expect_error(
  "sizing rejects a batch above the wall-time cap",
  select_direct_batch_repetitions(
    over_cap_sizing, c(r = 0.01, c_call = 0.01),
    c("r", "c_call"), c("vector_sum", "attributes")
  ),
  "cannot meet"
)
numeric_transform_sizing <- data.frame(
  runner = rep(c("r", "c_call"), times = 3L),
  task = "numeric_transform",
  batch_repetitions = rep(c(1L, 8L, 64L), each = 2L),
  batch_elapsed_ms = c(83, 0.5, 695, 3.9, 5416, 35),
  gc_elapsed_ms = c(5, 0, 74, 0, 464, 0),
  stringsAsFactors = FALSE
)
expect_error(
  "numeric transform cannot meet the shared sizing cap",
  select_direct_batch_repetitions(
    numeric_transform_sizing, c(r = 0.01, c_call = 0.01),
    c("r", "c_call"), "numeric_transform"
  ),
  "cannot meet the shared sizing target"
)
fractional_sizing <- sizing_rows
fractional_sizing$batch_repetitions[[1L]] <- 1.5
expect_error(
  "sizing rejects fractional repetitions",
  select_direct_batch_repetitions(
    fractional_sizing, c(r = 0.01, c_call = 0.01),
    c("r", "c_call"), c("vector_sum", "attributes")
  ),
  "invalid coverage or repetitions"
)
expect_true(
  !direct_sizing_count_accepted(
    fractional_sizing[fractional_sizing$task == "vector_sum", , drop = FALSE],
    c(r = 0.01, c_call = 0.01), c("r", "c_call")
  ),
  "a sizing round cannot advance from malformed repetitions"
)
expect_true(
  identical(
    classify_direct_distribution(c(0.001, 0.001), c(0.02, 0.02), c(0, 0), 0.01)$status,
    "PASS"
  ),
  "timer-floor admission uses the batch interval rather than its divided per-event value"
)

task86_values <- c(rep(0.04, 10L), rep(0.26, 99L), 8.5, rep(0.26, 98L), 8.7)
task86_gc <- numeric(length(task86_values))
task86_gc[c(110L, 209L)] <- c(8.1, 8.2)
task86_classification <- classify_direct_distribution(task86_values, task86_values, task86_gc, 0.01)
expect_true(
  identical(task86_classification$status, "BLOCK") &&
    isTRUE(task86_classification$metrics$regime_change),
  "the historical task 86 fast regime remains blocked even when periodic GC spikes are explained"
)
gc_only_values <- c(rep(0.26, 20L), 8.5, rep(0.26, 20L))
gc_only_time <- numeric(length(gc_only_values))
gc_only_time[[21L]] <- 8.1
expect_true(
  identical(classify_direct_distribution(
    gc_only_values, gc_only_values, gc_only_time, 0.01
  )$status, "PASS_GC"),
  "measured GC can explain a periodic allocating-task spike without deleting its sample"
)
complex_values <- c(
  0.321548375, 0.137349094, 0.224299906, 0.130848641, 0.11138675,
  0.121308, 0.053524547, 0.090369406, 0.091502375, 0.052623297, 0.087147359
)
complex_gc <- c(6, 0, 3, 0, 2, 3, 0, 2, 2, 0, 2)
complex_full_metrics <- direct_distribution_metrics(complex_values)
complex_without_gc <- complex_values[complex_gc == 0]
complex_residual <- direct_distribution_metrics(complex_without_gc)
complex_classification <- classify_direct_distribution(
  complex_values, complex_values, complex_gc, 0.01
)

expect_true(
  length(complex_values) == 11L &&
    isTRUE(all.equal(complex_classification$metrics, complex_full_metrics)) &&
    length(complex_without_gc) == 4L && complex_residual$cv_pct > 50 &&
    length(direct_distribution_triggers(complex_residual)) > 0L &&
    identical(complex_classification$status, "BLOCK") &&
    grepl("measured R GC did not explain", complex_classification$reason, fixed = TRUE),
  "complex output remains blocked when non-GC samples retain unexplained spread"
)
alternating_values <- rep(c(1, 10), length.out = 11L)
alternating_classification <- classify_direct_distribution(
  alternating_values, alternating_values, numeric(length(alternating_values)), 0.01
)
expect_true(
  identical(alternating_classification$status, "BLOCK") &&
    grepl("separated-mode ratio", alternating_classification$reason, fixed = TRUE) &&
    grepl("alternating mode sequence", alternating_classification$reason, fixed = TRUE) &&
    is.na(alternating_classification$metrics$ordered_regime_ratio),
  "alternating separated modes block without being mislabeled as one ordered transition"
)
separated_bimodal_values <- c(1, 1, 1, 10, 10, 1, 1, 10, 10, 10, 1)
separated_bimodal_classification <- classify_direct_distribution(
  separated_bimodal_values, separated_bimodal_values,
  numeric(length(separated_bimodal_values)), 0.01
)
expect_true(
  identical(separated_bimodal_classification$status, "BLOCK") &&
    separated_bimodal_classification$metrics$separated_mode_ratio >
      direct_distribution_policy()$regime_median_ratio_limit &&
    !isTRUE(separated_bimodal_classification$metrics$alternating_regime) &&
    is.na(separated_bimodal_classification$metrics$ordered_regime_ratio),
  "a separated bimodal sequence blocks even when it is neither alternating nor one ordered transition"
)
expect_true(
  identical(classify_direct_distribution(seq(1, 2, length.out = 11L), seq(1, 2, length.out = 11L),
                                         numeric(11L), 0.01)$status, "PASS"),
  "gradual drift below every declared threshold remains visible but does not create a false regime"
)
scheduler_spike <- classify_direct_distribution(c(rep(1, 10L), 30), c(rep(1, 10L), 30), numeric(11L), 0.01)
expect_true(
  identical(scheduler_spike$status, "BLOCK") &&
    grepl("max/median 30 exceeds 20", scheduler_spike$reason, fixed = TRUE),
  "an unexplained scheduler spike reports its exact threshold breach"
)
threshold_metrics <- list(
  median_ms = 1, mean_ms = 1, max_over_median = 20, p99_over_median = 5,
  cv_pct = 50, ordered_regime_ratio = 3, separated_mode_ratio = 3,
  alternating_switches = 0L, alternating_regime = FALSE
)
expect_true(
  length(direct_distribution_triggers(threshold_metrics)) == 0L &&
    length(direct_distribution_triggers(utils::modifyList(threshold_metrics, list(max_over_median = 20.001)))) == 1L &&
    length(direct_distribution_triggers(utils::modifyList(threshold_metrics, list(p99_over_median = 5.001)))) == 1L &&
    length(direct_distribution_triggers(utils::modifyList(threshold_metrics, list(cv_pct = 50.001)))) == 1L &&
    length(direct_distribution_triggers(utils::modifyList(threshold_metrics, list(ordered_regime_ratio = 3.001)))) == 1L,
  "all four inspection thresholds use strict declared boundaries"
)
zero_classification <- classify_direct_distribution(rep(0, 11L), rep(1, 11L), numeric(11L), 0.01)
expect_true(
  identical(zero_classification$status, "BLOCK") &&
    grepl("undefined ratio", zero_classification$reason, fixed = TRUE),
  "zero per-event medians receive a specific blocked reason"
)
manual_values <- seq(9.5, 10.5, length.out = 11L)
manual_samples <- data.frame(
  runner = "r", task = "manual", phase = "measurement", measurement_sample = seq_len(11L),
  batch_repetitions = 1L, batch_elapsed_ms = manual_values,
  elapsed_per_event_ms = manual_values, gc_elapsed_ms = 0,
  vector_heap_trigger_vcells = 8388608L, stringsAsFactors = FALSE
)
manual_summary <- summarize_direct_timing(
  manual_samples,
  data.frame(runner = "r", task = "manual", first_call_ms = 12, stringsAsFactors = FALSE),
  c(r = 0.01)
)
manual_expected <- data.frame(
  runner = "r", task = "manual", distribution_policy_digest = direct_distribution_policy_digest(),
  measurement_samples = 11L, batch_repetitions = 1L,
  median_ms = stats::median(manual_values), mean_ms = mean(manual_values),
  q1_ms = unname(stats::quantile(manual_values, 0.25, type = 7)),
  q3_ms = unname(stats::quantile(manual_values, 0.75, type = 7)),
  p95_ms = unname(stats::quantile(manual_values, 0.95, type = 7)),
  p99_ms = unname(stats::quantile(manual_values, 0.99, type = 7)),
  min_ms = min(manual_values), max_ms = max(manual_values), sd_ms = stats::sd(manual_values),
  cv_pct = stats::sd(manual_values) / mean(manual_values) * 100,
  max_over_median = max(manual_values) / stats::median(manual_values),
  p99_over_median = unname(stats::quantile(manual_values, 0.99, type = 7)) / stats::median(manual_values),
  allocation_class = "ordinary_result", fixed_sequence_output_vcells = 0,
  vector_heap_trigger_vcells = 8388608L, allocation_gc_status = "NOT_REQUIRED",
  regime_change = FALSE, separated_modes = FALSE, alternating_switches = 0L,
  alternating_regime = FALSE, timer_floor_ms = 0.01,
  distribution_status = "PASS", distribution_reason = "ordered samples pass the distribution gates",
  first_call_ms = 12, stringsAsFactors = FALSE
)
expect_true(
  isTRUE(all.equal(manual_summary, manual_expected, tolerance = 1e-12, check.attributes = FALSE)),
  "an independent 11-sample calculation matches every published summary field"
)
allocation_samples <- manual_samples
allocation_samples$task <- "complex_conjugate"
allocation_samples$batch_repetitions <- 64L
allocation_samples$batch_elapsed_ms <- 64
allocation_samples$elapsed_per_event_ms <- 1
allocation_samples$vector_heap_trigger_vcells <- 8388608L
allocation_blocked <- summarize_direct_timing(
  allocation_samples,
  data.frame(runner = "r", task = "complex_conjugate", first_call_ms = 12, stringsAsFactors = FALSE),
  c(r = 0.01)
)
expect_true(
  identical(allocation_blocked$allocation_class, "large_output") &&
    identical(allocation_blocked$allocation_gc_status, "GC_NOT_OBSERVED") &&
    identical(allocation_blocked$distribution_status, "BLOCK") &&
    grepl("GC not observed", allocation_blocked$distribution_reason, fixed = TRUE),
  "a large fixed sequence without measured GC cannot claim amortized allocation timing"
)
allocation_samples$vector_heap_trigger_vcells <- 50000000L
allocation_not_required <- summarize_direct_timing(
  allocation_samples,
  data.frame(runner = "r", task = "complex_conjugate", first_call_ms = 12, stringsAsFactors = FALSE),
  c(r = 0.01)
)
expect_true(
  identical(allocation_not_required$allocation_gc_status, "NOT_REQUIRED") &&
    identical(allocation_not_required$distribution_status, "PASS"),
  "a large output below its prephase vector-heap trigger does not require measured GC"
)
allocation_samples$vector_heap_trigger_vcells <- 8388608L
allocation_samples$gc_elapsed_ms[[1L]] <- 0.5
allocation_observed <- summarize_direct_timing(
  allocation_samples,
  data.frame(runner = "r", task = "complex_conjugate", first_call_ms = 12, stringsAsFactors = FALSE),
  c(r = 0.01)
)
expect_true(
  identical(allocation_observed$allocation_gc_status, "GC_OBSERVED") &&
    identical(allocation_observed$distribution_status, "PASS"),
  "a required large-output sequence records measured GC without changing its timing distribution"
)
extra_first_call <- rbind(
  first_calls,
  data.frame(runner = "zigr", task = "vector_sum", first_call_ms = 1, stringsAsFactors = FALSE)
)
expect_error(
  "first-call coverage",
  summarize_direct_timing(samples, extra_first_call),
  "first-call coverage differs"
)

calibration <- samples[1L, , drop = FALSE]
calibration$phase <- "calibration"
expect_error(
  "calibration exclusion",
  validate_direct_timing_samples(rbind(samples, calibration)),
  "only measurement rows"
)
mixed_first_call <- samples[1L, , drop = FALSE]
mixed_first_call$phase <- "first_call"
expect_error(
  "first-call exclusion",
  validate_direct_timing_samples(rbind(samples, mixed_first_call)),
  "only measurement rows"
)
asymmetric <- samples
asymmetric$batch_repetitions[asymmetric$runner == "c_call"] <- 3L
asymmetric$elapsed_per_event_ms <- asymmetric$batch_elapsed_ms / asymmetric$batch_repetitions
expect_error(
  "shared batch repetitions",
  validate_direct_timing_samples(asymmetric),
  "shared batch repetition"
)
inconsistent_elapsed <- samples
inconsistent_elapsed$elapsed_per_event_ms[[1L]] <- 99
expect_error(
  "elapsed-per-event identity",
  validate_direct_timing_samples(inconsistent_elapsed),
  "elapsed-per-event values differ"
)
missing_sample <- samples[-1L, , drop = FALSE]
expect_error(
  "fixed sample coverage",
  validate_direct_timing_samples(
    missing_sample, c("r", "c_call"), "vector_sum", measurement_samples = 3L
  ),
  "measurement-sample coverage is incomplete"
)
reordered <- samples[c(2L, 1L, 3:nrow(samples)), , drop = FALSE]
expect_error(
  "raw sample order",
  validate_direct_timing_samples(
    reordered, c("r", "c_call"), "vector_sum", measurement_samples = 3L
  ),
  "raw sample order is invalid"
)
distinct_samples <- samples
distinct_samples$batch_elapsed_ms <- c(2, 4, 6, 30, 50, 70)
distinct_samples$elapsed_per_event_ms <- distinct_samples$batch_elapsed_ms / distinct_samples$batch_repetitions
distinct_summary <- summarize_direct_timing(distinct_samples, first_calls)
expect_true(
  identical(distinct_summary$runner, c("c_call", "r")) &&
    identical(distinct_summary$median_ms, c(25, 2)),
  "summary grouping retains each runner's observed samples without cross-runner recycling"
)
fractional_sample <- samples
fractional_sample$measurement_sample[[1L]] <- 1.5
expect_error(
  "integer sample identity",
  validate_direct_timing_samples(fractional_sample),
  "invalid measurements"
)
duplicate <- rbind(samples, samples[1L, , drop = FALSE])
expect_error(
  "duplicate sample identity",
  validate_direct_timing_samples(duplicate),
  "duplicate identities"
)
bad_gc <- samples
bad_gc$gc_elapsed_ms[[1L]] <- bad_gc$batch_elapsed_ms[[1L]] + 1
expect_error(
  "GC time exceeds batch time",
  validate_direct_timing_samples(bad_gc),
  "invalid measurements"
)
bad_heap_trigger <- samples
bad_heap_trigger$vector_heap_trigger_vcells[[1L]] <- 0
expect_error(
  "nonpositive vector-heap trigger",
  validate_direct_timing_samples(bad_heap_trigger),
  "invalid measurements"
)
mixed_heap_trigger <- samples
mixed_heap_trigger$vector_heap_trigger_vcells[[2L]] <- 8388609L
expect_error(
  "mixed runner-task vector-heap trigger",
  validate_direct_timing_samples(mixed_heap_trigger),
  "one vector-heap trigger"
)

expect_error(
  "zero batch repetitions",
  direct_batch_expression(direct_function_call(identity, list(1L)), 0L),
  "positive"
)
expect_error(
  "unsupported native interface",
  direct_native_call(".C", fake_symbol, list()),
  "must be .Call or .External"
)

csv_root <- tempfile("direct-csv-")
dir.create(csv_root)
on.exit(unlink(csv_root, recursive = TRUE), add = TRUE)
csv_inputs <- file.path(csv_root, c("r.csv", "c.csv"))
write_csv_once(data.frame(runner = "r", value = 1L), csv_inputs[[1L]])
write_csv_once(data.frame(runner = "c_call", value = 2L), csv_inputs[[2L]])
csv_output <- file.path(csv_root, "combined.csv")
combine_csv_files_once(csv_inputs, csv_output, "direct CSV test")
expect_true(
  identical(read.csv(csv_output, stringsAsFactors = FALSE)$value, 1:2) &&
    !any(file.exists(csv_inputs)),
  "direct CSV consolidation writes once and removes staging inputs"
)

probes <- run_direct_measurement_probes(c_dll, 5L)
expect_true(
  nrow(probes$samples) == 20L &&
    identical(probes$samples$probe, rep(measurement_probe_names(), each = 5L)) &&
    identical(probes$samples$probe_sample, rep(1:5, times = 4L)) &&
    is.finite(probes$timer_floor_ms) && probes$timer_floor_ms >= 0 &&
    identical(probes$timer_floor_ms, measurement_probe_timer_floor(probes$samples)) &&
    probes$independent_elapsed_ms >= measurement_unit_minimum_ms() &&
    abs(probes$nanotime_elapsed_ms - probes$independent_elapsed_ms) <=
      measurement_unit_tolerance_ms(probes$independent_elapsed_ms) &&
    identical(benchmark_timing_policy()$measurement_probe_samples, 101L),
  paste(
    "focused probes retain both no-op shapes in order, prove CPU identity and fresh",
    "allocation, establish an empirical p99 floor, and verify time units"
  )
)
invalid_floor <- probes$samples[probes$samples$probe != "noop_r", , drop = FALSE]
expect_error(
  "timer floor requires both no-op call shapes",
  measurement_probe_timer_floor(invalid_floor),
  "invalid"
)
compact_real <- as.double(seq_len(1000000L))
expect_error(
  "CPU probe rejects an ALTREP input that REAL could materialize inside timing",
  eval(direct_native_call(
    ".Call", native_symbol("c_measurement_probe_cpu"), list(compact_real)
  )),
  "ordinary numeric vector"
)

status_lines <- c("VmRSS:\t  123 kB", "VmHWM:\t456 kB", "VmSwap:\t0 kB")
expect_true(
  identical(direct_proc_status_value_kb(status_lines, "VmRSS"), 123) &&
    identical(direct_proc_status_value_kb(status_lines, "VmHWM"), 456) &&
    identical(direct_proc_status_value_kb(status_lines, "VmSwap"), 0) &&
    is.na(direct_proc_status_value_kb(status_lines, "VmPeak")),
  "the Linux memory reader accepts only exact kilobyte status fields"
)
memory_rows <- data.frame(
  runner = c("r", "c_call"), task = "complex_conjugate", memory_status = "PASS",
  rss_metric = "VmRSS-and-VmHWM-kB", loaded_process_rss_kb = c(100, 110),
  initial_process_high_water_rss_kb = c(110, 120),
  process_high_water_rss_kb = c(120, 130), swap_before_kb = 0, swap_after_kb = 0,
  reason = "process high-water RSS recorded", stringsAsFactors = FALSE
)
validate_direct_memory_summary(memory_rows, c("r", "c_call"), "complex_conjugate")
bad_memory_policy <- direct_memory_policy()
bad_memory_policy$maximum_high_water_growth_kb <- 65535L
expect_error(
  "memory policy rejects an undeclared high-water cap",
  validate_direct_memory_policy(bad_memory_policy),
  "memory policy is invalid"
)
baseline_memory <- list(rss_kb = 100, hwm_kb = 150, swap_kb = 0)
after_memory <- list(rss_kb = 120, hwm_kb = 160, swap_kb = 0)
expect_true(
  identical(direct_memory_event_status(baseline_memory, after_memory)$status, "PASS") &&
    identical(direct_memory_event_status(
      baseline_memory, utils::modifyList(after_memory, list(swap_kb = 1))
    )$status, "BLOCK") &&
    identical(direct_memory_event_status(
      baseline_memory,
      utils::modifyList(after_memory, list(hwm_kb = 150 + 65537))
    )$status, "BLOCK"),
  "memory admission uses event high-water growth and blocks process swap"
)
bad_memory_rows <- memory_rows
bad_memory_rows$process_high_water_rss_kb[[1L]] <- 99
expect_error(
  "memory summary rejects a high-water value below its loaded baseline",
  validate_direct_memory_summary(bad_memory_rows, c("r", "c_call"), "complex_conjugate"),
  "invalid measurements"
)

cat("Direct measurement gate passed: direct intervals, exact counts, ordered distributions, GC attribution, timer floor, and independent units.\n")
