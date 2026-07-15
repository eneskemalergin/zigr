#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("cannot determine measurement test location")
script_path <- normalizePath(sub("^--file=", "", script_argument[[1L]]))
root_dir <- normalizePath(file.path(dirname(script_path), ".."))

source(file.path(root_dir, "lib", "specification.R"))
source(file.path(root_dir, "lib", "measurement.R"))
source(file.path(root_dir, "lib", "provenance.R"))
source(file.path(root_dir, "lib", "run_manifest.R"))
source(file.path(root_dir, "lib", "product_fixtures.R"))

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
    stop(sprintf("negative test %s failed for the wrong reason: %s", label, conditionMessage(error)), call. = FALSE)
  }
  invisible(error)
}

timing_policy <- benchmark_timing_policy()
validate_timing_policy(timing_policy)
legacy_timing_policy <- list(
  warmup_iterations = 10L, block_size = 10L, max_iterations = 500L,
  convergence_window_blocks = 5L, convergence_cv_threshold_pct = 1,
  timer_noise_floor_ms = 0.01, timer_noise_floor_method = "legacy calibration",
  low_noise_cv_threshold_pct = 20, meaningful_margin_ratio = 1.05,
  median_ci_level = 0.95, median_ci_method = "exact order-statistic interval",
  rss_metric = "post_gc_rss_delta_kb", gc_policy = "legacy"
)
validate_timing_policy(legacy_timing_policy)
expect_error(
  "unknown timing policy version",
  validate_timing_policy(within(timing_policy, policy_version <- "future-policy")),
  "unsupported timing policy version"
)
expect_error(
  "invalid peak RSS timeout",
  validate_timing_policy(within(timing_policy, peak_rss_timeout_seconds <- 0L)),
  "invalid peak_rss_timeout_seconds"
)
expect_error(
  "invalid peak RSS repetition policy",
  validate_timing_policy(within(timing_policy, peak_rss_repetitions <- 0L)),
  "invalid peak_rss_repetitions"
)
expect_error(
  "misnamed peak RSS metric",
  validate_timing_policy(within(timing_policy, peak_rss_metric <- "peak_memory_kb")),
  "invalid process-memory metric"
)
expect_error(
  "unknown peak RSS fixture",
  validate_timing_policy(within(timing_policy, peak_rss_fixture_ids <- c("F03", "F04", "F99"))),
  "invalid peak_rss_fixture_ids"
)
group_order_a <- timing_group_schedule(sprintf("G%02d", 1:8), 771L)
group_order_b <- timing_group_schedule(sprintf("G%02d", 1:8), 771L)
order_batches <- transform(group_order_a, estimated_ms = 1, batch = rep(1:3, length.out = 8L))
schedule_a <- timing_batch_schedule(order_batches, c("zigr", "r", "c_call"))
schedule_b <- timing_batch_schedule(order_batches, c("zigr", "r", "c_call"))
expect_true(
  identical(group_order_a, group_order_b) && identical(schedule_a, schedule_b) &&
    identical(sort(unique(group_order_a$group_order)), 1:8) &&
    all(vapply(split(schedule_a$runner, schedule_a$group_order), function(order) {
      length(unique(order)) == 3L
    }, logical(1))) &&
    length(unique(vapply(split(schedule_a$runner, schedule_a$group_order), paste, collapse = ":", character(1)))) == 3L,
  "timing schedule deterministically randomizes groups and rotates member order"
)
expect_true(
  identical(ordered_selection(c("a", "b", "c"), c("c", "a"), "test selection"), c(3L, 1L)),
  "worker selections preserve the scheduler's declared order"
)
expect_error(
  "worker selection rejects an unknown ID",
  ordered_selection(c("a", "b"), c("a", "missing"), "test selection"),
  "differs from available IDs"
)

synthetic_pilot <- function(group, member, values) data.frame(
  group_id = group, member_id = member, iteration = seq_along(values), wall_ms = values,
  stringsAsFactors = FALSE
)
stable <- rep(1, timing_policy$pilot_iterations)
noisy <- rep(c(0.25, 1.75), length.out = timing_policy$pilot_iterations)
drifting <- seq(0.5, 1.5, length.out = timing_policy$pilot_iterations)
floor_bound <- rep(timing_policy$timer_noise_floor_ms / 2, timing_policy$pilot_iterations)
slow <- rep(timing_policy$group_time_cap_ms, timing_policy$pilot_iterations)
pilot_samples <- do.call(rbind, list(
  synthetic_pilot("stable", "a", stable), synthetic_pilot("stable", "b", stable),
  synthetic_pilot("noisy", "a", noisy), synthetic_pilot("noisy", "b", noisy),
  synthetic_pilot("drifting", "a", drifting), synthetic_pilot("drifting", "b", drifting),
  synthetic_pilot("floor", "a", floor_bound), synthetic_pilot("floor", "b", floor_bound),
  synthetic_pilot("slow", "a", slow), synthetic_pilot("slow", "b", slow)
))
pilot_plan <- pilot_group_plan(pilot_samples, timing_policy)
pilot_by_group <- split(pilot_plan, pilot_plan$group_id)
expect_true(
  pilot_by_group$stable$status == "confirmation" &&
    pilot_by_group$stable$confirmation_iterations == timing_policy$confirmation_min_iterations &&
    pilot_by_group$noisy$confirmation_iterations > pilot_by_group$stable$confirmation_iterations &&
    pilot_by_group$drifting$pilot_max_drift_pct > pilot_by_group$stable$pilot_max_drift_pct &&
    pilot_by_group$floor$status == "below_timer_floor" &&
    pilot_by_group$slow$status == "incomplete",
  "pilot sizing handles stable, noisy, drifting, timer-floor, and slow groups within declared bounds"
)
expect_true(
  all(vapply(split(pilot_samples$iteration, paste(pilot_samples$group_id, pilot_samples$member_id)), length, integer(1)) ==
        timing_policy$pilot_iterations),
  "every eligible pilot member receives the same bounded floor"
)
duplicate_iteration <- pilot_samples[pilot_samples$group_id == "stable", , drop = FALSE]
duplicate_iteration$iteration[duplicate_iteration$member_id == "a" & duplicate_iteration$iteration == 2L] <- 1L
expect_true(
  identical(pilot_group_plan(duplicate_iteration, timing_policy)$status, "incomplete"),
  "a pilot with duplicated iteration numbers is incomplete even when its row count matches"
)

packed <- pack_timing_batches(data.frame(
  group_id = sprintf("G%02d", 1:5), group_order = 1:5,
  estimated_ms = rep(timing_policy$batch_time_cap_ms / 3, 5L)
), timing_policy)
packed_cost <- tapply(packed$estimated_ms, packed$batch, sum)
packed_count <- table(packed$batch)
expect_true(
  all(packed_cost <= timing_policy$batch_time_cap_ms) &&
    all(packed_count <= timing_policy$batch_group_cap),
  "batch packing respects declared time and group caps"
)
budget_admission <- admit_timing_budget(data.frame(
  universe = c("task", "task", "fixture"), group_id = c("a", "b", "c"),
  estimated_ms = c(60, 50, 30), stringsAsFactors = FALSE
), 100)
expect_true(
  identical(budget_admission$admitted, c(TRUE, FALSE, TRUE)) &&
    identical(budget_admission$remaining_after_ms, c(40, 40, 10)),
  "confirmation budget admission is ordered, bounded, and can admit later work that fits"
)
attempts <- new.env(parent = emptyenv())
executor <- function(rows, timeout_seconds, attempt, batch_epoch) {
  key <- paste(rows$group_id, collapse = "+")
  attempts[[key]] <- if (is.null(attempts[[key]])) 1L else attempts[[key]] + 1L
  stalled <- "G01" %in% rows$group_id
  list(ok = !stalled, timed_out = stalled)
}
queue_result <- run_timing_batches(
  packed, executor, timing_policy$batch_timeout_seconds, timing_policy$total_run_budget_seconds
)
expect_true(
  any(queue_result$group_id == "G01" & queue_result$status == "incomplete_timeout") &&
    all(queue_result$status[queue_result$group_id != "G01"] == "complete") &&
    max(queue_result$batch_epoch) > 1L && sum(grepl("G01", ls(attempts, all.names = TRUE))) == 2L,
  "a stalled batch receives one reduced retry and later batches still complete"
)
budget_executor_called <- FALSE
budget_result <- run_timing_batches(
  packed[1L, , drop = FALSE],
  function(...) {
    budget_executor_called <<- TRUE
    list(ok = TRUE, timed_out = FALSE)
  },
  timing_policy$batch_timeout_seconds, 1,
  started_at = proc.time()[["elapsed"]] - 2
)
expect_true(
  !budget_executor_called && identical(budget_result$status, "incomplete_budget"),
  "an exhausted total timing budget marks work incomplete without launching another worker"
)
received_timeout <- NA_real_
invisible(run_timing_batches(
  packed[1L, , drop = FALSE],
  function(rows, timeout_seconds, attempt, batch_epoch) {
    received_timeout <<- timeout_seconds
    list(ok = TRUE, timed_out = FALSE)
  },
  timing_policy$batch_timeout_seconds, 10,
  started_at = proc.time()[["elapsed"]] - 8
))
expect_true(
  is.finite(received_timeout) && received_timeout > 0 && received_timeout <= 2.1,
  "a batch timeout is capped by the remaining total timing budget"
)

execution_plan <- data.frame(
  group_id = c("G01", "G02"), pilot_complete = TRUE,
  pilot_median_group_ms = c(2, 0.001), pilot_max_cv_pct = c(1, 1),
  pilot_max_drift_pct = c(0, 0), confirmation_iterations = c(20L, NA_integer_),
  estimated_confirmation_ms = c(40, NA_real_),
  status = c("confirmation", "below_timer_floor"), stringsAsFactors = FALSE
)
pilot_schedule <- data.frame(
  group_id = rep(c("G01", "G02"), each = 2L), group_order = rep(1:2, each = 2L),
  batch = rep(1:2, each = 2L), runner = c("r", "zigr", "zigr", "r"),
  member_order = rep(1:2, 2L), stringsAsFactors = FALSE
)
confirmation_schedule <- pilot_schedule[pilot_schedule$group_id == "G01", , drop = FALSE]
confirmation_batches <- data.frame(
  group_id = "G01", group_order = 1L, estimated_ms = 40, batch = 1L,
  stringsAsFactors = FALSE
)
pilot_batches <- data.frame(
  group_id = c("G01", "G02"), group_order = 1:2, estimated_ms = 0,
  batch = 1:2, stringsAsFactors = FALSE
)
timing_execution <- list(
  schedule_seeds = list(task = 1L, fixture = 2L),
  packing_hint_run_ids = list(),
  confirmation_budget = list(
    available_ms = 100,
    decisions = data.frame(
      universe = c("task", "fixture"), group_id = "G01", group_order = 1L,
      estimated_ms = 40, budget_order = 1:2, admitted = TRUE,
      remaining_after_ms = c(60, 20), stringsAsFactors = FALSE
    )
  ),
  task_plan = execution_plan, fixture_plan = execution_plan,
  task_pilot_schedule = pilot_schedule, fixture_pilot_schedule = pilot_schedule,
  task_pilot_batches = pilot_batches, fixture_pilot_batches = pilot_batches,
  task_confirmation_schedule = confirmation_schedule,
  fixture_confirmation_schedule = confirmation_schedule,
  task_confirmation_batches = confirmation_batches,
  fixture_confirmation_batches = confirmation_batches,
  frozen_at = "2026-07-15T00:00:00.000Z",
  outcomes = list(
    task_pilot = data.frame(
      group_id = c("G01", "G02"), batch = 1:2, attempt = 1L, batch_epoch = 1:2,
      status = "complete"
    ),
    fixture_pilot = data.frame(
      group_id = c("G01", "G02"), batch = 1:2, attempt = 1L, batch_epoch = 3:4,
      status = "complete"
    ),
    task_confirmation = data.frame(
      group_id = "G01", batch = 1L, attempt = 1L, batch_epoch = 5L, status = "complete"
    ),
    fixture_confirmation = data.frame(
      group_id = "G01", batch = 1L, attempt = 1L, batch_epoch = 6L, status = "complete"
    )
  ),
  finished_at = "2026-07-15T00:01:00.000Z"
)
timing_metadata <- list(
  tasks = as.list(c("G01", "G02")), runners = as.list(c("r", "zigr")),
  timing_policy = timing_policy
)
validate_timing_execution(timing_execution, timing_metadata)
serialized_execution <- jsonlite::fromJSON(
  jsonlite::toJSON(timing_execution, auto_unbox = TRUE), simplifyVector = FALSE
)
validate_timing_execution(serialized_execution, timing_metadata)
task_only_execution <- unserialize(serialize(timing_execution, NULL))
task_only_execution$fixture_plan <- data.frame()
task_only_execution$fixture_pilot_schedule <- data.frame()
task_only_execution$fixture_pilot_batches <- data.frame()
task_only_execution$fixture_confirmation_schedule <- data.frame()
task_only_execution$fixture_confirmation_batches <- data.frame()
task_only_execution$outcomes$fixture_pilot <- data.frame()
task_only_execution$outcomes$fixture_confirmation <- data.frame()
task_only_execution$confirmation_budget$decisions <-
  task_only_execution$confirmation_budget$decisions[1L, , drop = FALSE]
task_only_execution$confirmation_budget$decisions$budget_order <- 1L
task_only_metadata <- timing_metadata
task_only_metadata$suite <- "tasks"
validate_timing_execution(
  jsonlite::fromJSON(jsonlite::toJSON(task_only_execution, auto_unbox = TRUE), simplifyVector = FALSE),
  task_only_metadata
)
fixture_only_execution <- unserialize(serialize(timing_execution, NULL))
fixture_only_execution$task_plan <- data.frame()
fixture_only_execution$task_pilot_schedule <- data.frame()
fixture_only_execution$task_pilot_batches <- data.frame()
fixture_only_execution$task_confirmation_schedule <- data.frame()
fixture_only_execution$task_confirmation_batches <- data.frame()
fixture_only_execution$outcomes$task_pilot <- data.frame()
fixture_only_execution$outcomes$task_confirmation <- data.frame()
fixture_only_execution$confirmation_budget$decisions <-
  fixture_only_execution$confirmation_budget$decisions[2L, , drop = FALSE]
fixture_only_execution$confirmation_budget$decisions$budget_order <- 1L
fixture_only_execution$confirmation_budget$decisions$remaining_after_ms <- 60
fixture_only_metadata <- timing_metadata
fixture_only_metadata$suite <- "fixtures"
fixture_only_metadata$tasks <- list()
validate_timing_execution(
  jsonlite::fromJSON(jsonlite::toJSON(fixture_only_execution, auto_unbox = TRUE), simplifyVector = FALSE),
  fixture_only_metadata
)
asymmetric_execution <- unserialize(serialize(timing_execution, NULL))
asymmetric_execution$task_confirmation_schedule <-
  asymmetric_execution$task_confirmation_schedule[-1L, , drop = FALSE]
expect_error(
  "asymmetric frozen confirmation schedule",
  validate_timing_execution(asymmetric_execution, timing_metadata),
  "not complete and symmetric"
)
invalid_order <- unserialize(serialize(timing_execution, NULL))
invalid_order$task_pilot_schedule$member_order[[2L]] <- 1L
expect_error(
  "invalid frozen member order",
  validate_timing_execution(invalid_order, timing_metadata),
  "tool order does not match"
)
tampered_budget <- unserialize(serialize(timing_execution, NULL))
tampered_budget$confirmation_budget$decisions$admitted[[1L]] <- FALSE
expect_error(
  "tampered confirmation budget admission",
  validate_timing_execution(tampered_budget, timing_metadata),
  "differ from the declared budget"
)

fixed_calls <- 0L
fixed_result <- benchmark_call(
  function() function() NULL,
  function() function() fixed_calls <<- fixed_calls + 1L,
  iterations = 7L, warmup = 2L
)
expect_true(
  fixed_calls == 7L && fixed_result$n_runs == 7L && fixed_result$fixed_iterations == 7L,
  "fixed timing takes exactly the predeclared confirmation count"
)
original_current_rss_kb <- current_rss_kb
current_rss_kb <- function() NA_integer_
unsupported_endpoint <- benchmark_call(
  function() function() NULL,
  function() function() NULL,
  iterations = 2L,
  warmup = 0L
)
current_rss_kb <- original_current_rss_kb
expect_true(
  is.na(unsupported_endpoint$rss_endpoint_delta_kb) &&
    identical(unsupported_endpoint$rss_endpoint_support, "unsupported") &&
    nzchar(unsupported_endpoint$rss_endpoint_support_reason),
  "unsupported endpoint RSS remains NA with a support reason"
)
metric_rows <- data.frame(
  first_call_ms = c(1, NA_real_), rss_endpoint_delta_kb = c(NA_integer_, NA_integer_),
  rss_endpoint_metric = timing_policy$rss_endpoint_metric,
  rss_endpoint_support = c("unsupported", "not_measured"),
  rss_endpoint_support_reason = c("unsupported test host", "timing not measured"),
  stringsAsFactors = FALSE
)
validate_first_call_metric(metric_rows, c(TRUE, FALSE), "synthetic metrics")
validate_rss_endpoint_support(metric_rows, c(TRUE, FALSE), timing_policy, "synthetic metrics")
bad_first_call <- metric_rows
bad_first_call$first_call_ms[[1L]] <- -1
expect_error(
  "negative first-call measurement",
  validate_first_call_metric(bad_first_call, c(TRUE, FALSE), "synthetic metrics"),
  "invalid first-call"
)
bad_endpoint_support <- metric_rows
bad_endpoint_support$rss_endpoint_delta_kb[[1L]] <- 0L
expect_error(
  "unsupported endpoint RSS cannot become zero",
  validate_rss_endpoint_support(bad_endpoint_support, c(TRUE, FALSE), timing_policy, "synthetic metrics"),
  "disagrees with its support state"
)
bad_endpoint_state <- metric_rows
bad_endpoint_state$rss_endpoint_support[[1L]] <- NA_character_
expect_error(
  "missing endpoint RSS support state",
  validate_rss_endpoint_support(bad_endpoint_state, c(TRUE, FALSE), timing_policy, "synthetic metrics"),
  "invalid endpoint RSS support state"
)
bad_endpoint_reason <- metric_rows
bad_endpoint_reason$rss_endpoint_support_reason[[1L]] <- "available"
expect_error(
  "unsupported endpoint RSS cannot claim availability",
  validate_rss_endpoint_support(bad_endpoint_reason, c(TRUE, FALSE), timing_policy, "synthetic metrics"),
  "disagrees with its support state"
)
bad_endpoint_text <- metric_rows
bad_endpoint_text$rss_endpoint_delta_kb[[1L]] <- "missing"
expect_error(
  "endpoint RSS rejects non-numeric missing placeholders",
  validate_rss_endpoint_support(bad_endpoint_text, c(TRUE, FALSE), timing_policy, "synthetic metrics"),
  "non-numeric value"
)
raw_metric_summary <- data.frame(
  rss_endpoint_delta_kb = 12, rss_endpoint_support = "supported", first_call_ms = 1,
  stringsAsFactors = FALSE
)
raw_metric_samples <- data.frame(
  iteration = 1:2, wall_ms = c(1, 1), rss_endpoint_delta_kb = c(NA, 12),
  stringsAsFactors = FALSE
)
validate_rss_endpoint_raw(raw_metric_summary, raw_metric_samples, "synthetic raw metrics")
validate_first_call_raw(
  raw_metric_summary,
  data.frame(iteration = 1L, wall_ms = 1, stringsAsFactors = FALSE),
  "synthetic raw metrics"
)
bad_raw_metric <- raw_metric_samples
bad_raw_metric$rss_endpoint_delta_kb <- c(12, NA)
expect_error(
  "endpoint RSS must occupy the final timed sample",
  validate_rss_endpoint_raw(raw_metric_summary, bad_raw_metric, "synthetic raw metrics"),
  "differs from its summary"
)
expect_error(
  "first-call raw timing must match its summary",
  validate_first_call_raw(
    raw_metric_summary,
    data.frame(iteration = 1L, wall_ms = 2, stringsAsFactors = FALSE),
    "synthetic raw metrics"
  ),
  "differs from its summary"
)
expect_error(
  "invalid fixed timing count",
  benchmark_call(function() function() NULL, function() function() NULL, iterations = 0L),
  "positive integer"
)

proc_snapshot <- parse_proc_status_memory(c(
  "Name:\tR", "VmHWM:\t 4096 kB", "VmRSS:\t 2048 kB"
))
expect_true(
  identical(proc_snapshot, list(loaded_process_rss_kb = 2048L, peak_rss_kb = 4096L)),
  "Linux process status parser keeps loaded and gross peak RSS separate"
)
malformed_snapshot <- parse_proc_status_memory(c("VmHWM: unknown", "VmRSS: 1 MB"))
expect_true(
  is.na(malformed_snapshot$loaded_process_rss_kb) && is.na(malformed_snapshot$peak_rss_kb),
  "malformed process memory readings remain unsupported"
)
peak_probe_calls <- 0L
peak_probe <- measure_peak_process_rss(function() {
  peak_probe_calls <<- peak_probe_calls + 1L
  function() raw(1024L * 1024L)
}, repetitions = 2L)
if (identical(peak_probe$peak_rss_support, "supported")) {
  expect_true(
    peak_probe_calls == 2L && is.finite(peak_probe$peak_rss_kb) &&
      peak_probe$peak_rss_kb >= peak_probe$loaded_process_rss_kb,
    "supported peak RSS uses the fixed repetition count and reports gross peak above loaded baseline"
  )
} else {
  expect_true(
    peak_probe_calls == 0L && is.na(peak_probe$peak_rss_kb) &&
      is.na(peak_probe$loaded_process_rss_kb) && nzchar(peak_probe$peak_rss_support_reason),
    "unsupported peak RSS does not run the workload and preserves NA with a reason"
  )
}

memory_summaries <- data.frame(
  run_id = "run", runner = c("zigr", "zigr"), fixture = c("F03", "F01"),
  variant = "public", row_id = c("F03", "F01"), status = "PASS",
  stringsAsFactors = FALSE
)
memory_results <- data.frame(
  run_id = "run", runner = "zigr", fixture = "F03", variant = "public", row_id = "F03",
  peak_rss_kb = 4096L, loaded_process_rss_kb = 2048L,
  peak_rss_metric = timing_policy$peak_rss_metric,
  peak_rss_support = "supported", peak_rss_support_reason = "available",
  peak_rss_repetitions = timing_policy$peak_rss_repetitions,
  stringsAsFactors = FALSE
)
supported_memory <- apply_peak_rss_results(
  memory_summaries, memory_results, timing_policy, list(supported = TRUE, reason = "available")
)
expect_true(
  supported_memory$peak_rss_kb[[1L]] == 4096L &&
    identical(supported_memory$peak_rss_support, c("supported", "not_eligible")),
  "declared memory fixture receives gross peak and non-eligible rows stay explicit"
)
validate_peak_rss_support(supported_memory, timing_policy, "fixture", "synthetic fixture metrics")
unsupported_memory <- apply_peak_rss_results(
  memory_summaries, data.frame(), timing_policy,
  list(supported = FALSE, reason = "unsupported test host")
)
expect_true(
  is.na(unsupported_memory$peak_rss_kb[[1L]]) &&
    identical(unsupported_memory$peak_rss_support[[1L]], "unsupported") &&
    identical(unsupported_memory$peak_rss_support_reason[[1L]], "unsupported test host"),
  "unsupported gross peak RSS remains NA with the host reason"
)
validate_peak_rss_support(unsupported_memory, timing_policy, "fixture", "synthetic fixture metrics")
unsupported_worker_result <- memory_results
unsupported_worker_result$peak_rss_kb <- NA_integer_
unsupported_worker_result$loaded_process_rss_kb <- NA_integer_
unsupported_worker_result$peak_rss_support <- "unsupported"
unsupported_worker_result$peak_rss_support_reason <- "worker /proc reading unavailable"
unsupported_worker_memory <- apply_peak_rss_results(
  memory_summaries, unsupported_worker_result, timing_policy,
  list(supported = TRUE, reason = "available")
)
expect_true(
  is.na(unsupported_worker_memory$peak_rss_kb[[1L]]) &&
    identical(unsupported_worker_memory$peak_rss_support[[1L]], "unsupported") &&
    identical(unsupported_worker_memory$peak_rss_support_reason[[1L]], "worker /proc reading unavailable"),
  "worker-level peak RSS support loss remains NA even on a supported host"
)
invalid_memory_results <- memory_results
invalid_memory_results$loaded_process_rss_kb <- 5000L
expect_error(
  "gross peak RSS below loaded baseline",
  apply_peak_rss_results(
    memory_summaries, invalid_memory_results, timing_policy,
    list(supported = TRUE, reason = "available")
  ),
  "value or identity is invalid"
)
zero_memory_results <- memory_results
zero_memory_results$peak_rss_kb <- 0L
zero_memory_results$loaded_process_rss_kb <- 0L
expect_error(
  "supported process RSS cannot be zero",
  apply_peak_rss_results(
    memory_summaries, zero_memory_results, timing_policy,
    list(supported = TRUE, reason = "available")
  ),
  "value or identity is invalid"
)
available_unsupported_memory <- unsupported_worker_result
available_unsupported_memory$peak_rss_support_reason <- "available"
expect_error(
  "unsupported peak RSS cannot claim availability",
  apply_peak_rss_results(
    memory_summaries, available_unsupported_memory, timing_policy,
    list(supported = TRUE, reason = "available")
  ),
  "value or identity is invalid"
)
invalid_unsupported_memory <- unsupported_worker_result
invalid_unsupported_memory$peak_rss_kb <- 0L
expect_error(
  "unsupported gross peak RSS cannot become zero",
  apply_peak_rss_results(
    memory_summaries, invalid_unsupported_memory, timing_policy,
    list(supported = TRUE, reason = "available")
  ),
  "value or identity is invalid"
)
no_memory_rows <- memory_summaries[2L, , drop = FALSE]
no_memory_result <- apply_peak_rss_results(
  no_memory_rows, NULL, timing_policy, list(supported = TRUE, reason = "available")
)
expect_true(
  identical(no_memory_result$peak_rss_support, "not_eligible") && is.na(no_memory_result$peak_rss_kb),
  "a supported host accepts a filtered suite with no memory-eligible fixture"
)
expect_error(
  "missing declared peak RSS row",
  apply_peak_rss_results(
    memory_summaries, memory_results[0, ], timing_policy,
    list(supported = TRUE, reason = "available")
  ),
  "coverage differs"
)

sample_file <- tempfile("wall-time-samples-", fileext = ".csv")
write.csv(data.frame(iteration = 1:5, wall_ms = c(0, 0.1, 0.2, 0.3, 0.4)), sample_file, row.names = FALSE)
sample_values <- read_wall_time_samples(sample_file, expected_n = 5L)
sample_interval <- median_confidence_interval(sample_values, 0.95)
expect_true(
  identical(sample_values, c(0, 0.1, 0.2, 0.3, 0.4)) &&
    sample_interval[["low"]] <= median(sample_values) && sample_interval[["high"]] >= median(sample_values),
  "shared report sample reader preserves valid zero-duration samples and interval coverage"
)
grouped_sample_dir <- tempfile("grouped-fixture-samples-")
dir.create(grouped_sample_dir)
grouped_metadata <- list(schema_version = 3L, artifact_layout = "grouped-v1")
write.csv(data.frame(
  runner = "zigr", row_id = "F03", phase = c("first_call", "timed", "timed"),
  stage = "confirmation", excluded = FALSE, iteration = c(1L, 1L, 2L),
  wall_ms = c(9, 1, 2), stringsAsFactors = FALSE
), file.path(grouped_sample_dir, "fixture_samples.csv"), row.names = FALSE)
grouped_fixture_values <- read_run_wall_time_samples(
  grouped_sample_dir, grouped_metadata, "fixture", "zigr", "F03",
  expected_n = 2L, stage = "confirmation"
)
expect_true(
  identical(grouped_fixture_values, c(1L, 2L)),
  "grouped fixture sample reads exclude first-call observations"
)
unlink(grouped_sample_dir, recursive = TRUE)
atomic_csv <- tempfile("atomic-csv-", fileext = ".csv")
write_csv_once(data.frame(value = 1L), atomic_csv, "test output")
expect_true(identical(read.csv(atomic_csv)$value, 1L), "atomic CSV output is promoted once")
expect_error(
  "atomic CSV overwrite",
  write_csv_once(data.frame(value = 2L), atomic_csv, "test output"),
  "already exists"
)
unlink(atomic_csv)
merge_root <- tempfile("csv-merge-")
dir.create(merge_root)
merge_inputs <- file.path(merge_root, c("a.csv", "b.csv"))
write.csv(data.frame(runner = "a", value = 1L), merge_inputs[[1L]], row.names = FALSE)
write.csv(data.frame(runner = "b", value = 2L), merge_inputs[[2L]], row.names = FALSE)
merge_output <- file.path(merge_root, "combined.csv")
combine_csv_files_once(merge_inputs, merge_output, "test merge")
expect_true(
  identical(read.csv(merge_output)$value, 1:2) && !any(file.exists(merge_inputs)),
  "CSV consolidation promotes one output and removes its staging inputs"
)
unlink(merge_root, recursive = TRUE)
expect_error(
  "raw timing sample count drift",
  read_wall_time_samples(sample_file, expected_n = 4L),
  "count differs from summary"
)
write.csv(
  data.frame(task = rep(c("a", "b"), each = 3L), phase = "timed", iteration = rep(1:3, 2L), wall_ms = 1:6),
  sample_file,
  row.names = FALSE
)
expect_true(
  identical(read_wall_time_samples(sample_file, expected_n = 3L, filters = list(task = "b")), 4:6),
  "grouped raw timing samples select exactly one task"
)
expect_error(
  "grouped raw timing missing identity",
  read_wall_time_samples(sample_file, filters = list(row_id = "missing")),
  "lack filter columns"
)
expect_error(
  "grouped raw timing empty identity",
  read_wall_time_samples(sample_file, filters = list(task = "missing")),
  "incomplete"
)
layout_root <- tempfile("artifact-layout-")
dir.create(layout_root, recursive = TRUE)
grouped_metadata <- list(schema_version = 3L, artifact_layout = "grouped-v1")
write.csv(
  rbind(
    data.frame(runner = "r", task = rep(c("a", "b"), each = 3L), phase = "timed", iteration = rep(1:3, 2L), wall_ms = 1:6),
    data.frame(runner = "other", task = "b", phase = "timed", iteration = 1:3, wall_ms = 101:103)
  ),
  file.path(layout_root, "task_samples.csv"),
  row.names = FALSE
)
expect_true(
  identical(read_run_wall_time_samples(layout_root, grouped_metadata, "task", "r", "b", expected_n = 3L), 4:6),
  "grouped artifact layout reads a selected task"
)
legacy_metadata <- list(schema_version = 2L, run_id = legacy_run_manifest_id())
dir.create(file.path(layout_root, "r"))
write.csv(
  data.frame(task = "b", iteration = 1:3, wall_ms = 7:9),
  file.path(layout_root, "r", "task_b.csv"),
  row.names = FALSE
)
expect_true(
  identical(read_run_wall_time_samples(layout_root, legacy_metadata, "task", "r", "b", expected_n = 3L), 7:9),
  "low-level schema-two artifact reader remains available for the accepted historical run"
)
unnamed_legacy_metadata <- legacy_metadata
unnamed_legacy_metadata$run_id <- "unaccepted-schema-two"
expect_error(
  "low-level schema-two reader rejects an unnamed run",
  read_run_wall_time_samples(layout_root, unnamed_legacy_metadata, "task", "r", "b", expected_n = 3L),
  "retained only for the named accepted historical run"
)
unlink(layout_root, recursive = TRUE)
legacy_manifest_root <- tempfile("legacy-run-manifest-")
dir.create(legacy_manifest_root)
jsonlite::write_json(
  list(schema_version = 2L, run_id = "unaccepted-schema-two"),
  run_manifest_path(legacy_manifest_root), auto_unbox = TRUE
)
expect_error(
  "unnamed schema-two run manifest",
  read_run_manifest(legacy_manifest_root),
  "retained only for the named accepted historical run"
)
jsonlite::write_json(
  list(schema_version = 2L, run_id = legacy_run_manifest_id()),
  run_manifest_path(legacy_manifest_root), auto_unbox = TRUE
)
expect_true(
  identical(as.character(read_run_manifest(legacy_manifest_root)$run_id), legacy_run_manifest_id()),
  "named accepted historical run retains schema-two compatibility"
)
unlink(legacy_manifest_root, recursive = TRUE)
write.csv(data.frame(wall_ms = c(0.1, Inf)), sample_file, row.names = FALSE)
expect_error(
  "raw timing non-finite drift",
  read_wall_time_samples(sample_file),
  "invalid wall_ms values"
)
expect_error(
  "invalid median confidence level",
  median_confidence_interval(1:5, 1),
  "between zero and one"
)
unlink(sample_file)

fixture_version <- "trust-test-v1"
small_task <- list(id = "fixture_task", args = function() list(runif(8L), c("ascii", enc2utf8("cafe\u0301"))))
record <- build_input_recipe_record(small_task, 12345L, fixture_version, "contract-v1")
expect_true(
  identical(record$task_seed, task_input_seed(12345L, small_task$id, fixture_version)),
  "stable per-task seed"
)

materialized_record <- materialize_task_input(small_task, record$task_seed)
expect_true(
  identical(
    validate_materialized_task_input(small_task, record, 12345L, materialized_record)$arguments,
    materialized_record$arguments
  ),
  "first phase materialization verifies the canonical input recipe"
)
expect_true(
  identical(
    record$fingerprint,
    build_input_recipe_record(small_task, 12345L, fixture_version, "contract-v1")$fingerprint
  ),
  "deterministic structural fingerprint"
)

bad_seed <- unserialize(serialize(record, NULL))
bad_seed$task_seed <- bad_seed$task_seed + 1L
expect_error(
  "deliberate task seed mismatch",
  validate_materialized_task_input(
    small_task, bad_seed, 12345L, materialize_task_input(small_task, bad_seed$task_seed)
  ),
  "task seed mismatch"
)

bad_master_seed <- unserialize(serialize(record, NULL))
bad_master_seed$master_seed <- bad_master_seed$master_seed + 1L
expect_error(
  "deliberate master seed mismatch",
  validate_materialized_task_input(
    small_task, bad_master_seed, 12345L, materialize_task_input(small_task, bad_master_seed$task_seed)
  ),
  "master seed mismatch"
)

bad_fingerprint <- unserialize(serialize(record, NULL))
bad_fingerprint$fingerprint <- "00000000000000000000000000000000"
expect_error(
  "deliberate input fingerprint mismatch",
  validate_materialized_task_input(
    small_task, bad_fingerprint, 12345L, materialize_task_input(small_task, bad_fingerprint$task_seed)
  ),
  "canonical input fingerprint mismatch"
)

first_phase <- materialize_task_input(small_task, record$task_seed)$arguments
second_phase <- materialize_task_input(small_task, record$task_seed)$arguments
first_phase[[1L]][[1L]] <- -1
expect_true(second_phase[[1L]][[1L]] != -1, "phase inputs do not share mutable state")

state_recipe <- structure("runner_registered_fixture_state", class = "benchmark_external_state_recipe")
state_arguments <- materialize_runtime_task_arguments(
  list(state_recipe, 7L), "stateful_reset_required", function() structure("live", class = "test_receiver")
)
expect_true(
  inherits(state_arguments[[1L]], "test_receiver") && identical(state_arguments[[2L]], 7L),
  "stateful runtime recipes become fresh runner-specific receivers"
)
expect_error(
  "missing stateful runtime recipe",
  materialize_runtime_task_arguments(list(NULL, 7L), "stateful_reset_required", function() "live"),
  "missing its external-state recipe"
)

encoded_cases <- benchmark_encoded_strings()
expect_true(
  identical(Encoding(encoded_cases), c("UTF-8", "latin1", "bytes", "unknown", "unknown")) &&
    identical(charToRaw(encoded_cases[[3L]]), as.raw(c(0x66, 0x61, 0xe7, 0x61, 0x64, 0x65))) &&
    identical(encoded_cases[[4L]], "") && is.na(encoded_cases[[5L]]),
  "F05 task inputs contain real UTF-8, Latin-1, bytes, empty, and missing cases"
)
for (task_id in c("17_string_concat", "18_string_nchar", "19_string_encoding")) {
  values <- benchmark_string_input(task_id)
  validate_special_task_input(task_id, list(values))
  expect_true(length(values) == 10000L, sprintf("%s uses the exact shared string input size", task_id))
}
expect_true(
  sum(is.na(benchmark_string_input("18_string_nchar"))) == 500L,
  "string byte-length input contains deterministic missing cases"
)

factor_values <- benchmark_factor_input()
validate_special_task_input("20_factor_ops", list(factor_values))
expect_true(
  length(factor_values) == 10000L &&
    length(unique(factor_values[!is.na(factor_values)])) == 100L &&
    sum(is.na(factor_values)) == 1L && is.na(factor_values[[10000L]]),
  "factor input contains 100 deterministic levels and one explicit trailing missing value"
)
expect_error(
  "old 26-letter factor vocabulary",
  validate_special_task_input("20_factor_ops", list(letters[seq_len(100L)])),
  "factor contract requires 100 deterministic levels"
)
expect_error(
  "old compact-vector size",
  validate_special_task_input("24_long_vector_idx", list(seq_len(1000000L))),
  "compact ALTREP indexing contract input differs"
)

set.seed(43L, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
first_rng_values <- rnorm(32L)
first_rng_state <- rng_state_snapshot()
set.seed(43L, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
second_rng_values <- rnorm(32L)
second_rng_state <- rng_state_snapshot()
expect_true(
  identical(first_rng_values, second_rng_values) &&
    identical(assert_rng_state_equivalent(first_rng_state, second_rng_state), second_rng_state),
  "RNG values and post-call state are reproducible under the declared generator"
)
bad_rng_state <- second_rng_state
bad_rng_state[[2L]] <- bad_rng_state[[2L]] + 1L
expect_error(
  "post-call RNG state mismatch",
  assert_rng_state_equivalent(first_rng_state, bad_rng_state),
  "post-call RNG state differs"
)

immutable_arguments <- list(c(1, 2, 3))
immutable_before <- task_arguments_fingerprint("immutable_fixture", immutable_arguments, "ordinary_r_object")
immutable_arguments[[1L]][[1L]] <- 99
expect_error(
  "deliberate immutable input mutation",
  assert_immutable_input("immutable_fixture", immutable_arguments, immutable_before, "ordinary_r_object"),
  "immutable input mutated"
)

bad_r_contract <- validate_result_contract(1L, "real_scalar")
expect_true(
  !isTRUE(bad_r_contract$ok) && identical(bad_r_contract$message, "expected result contract real_scalar"),
  "R result contract rejects a mismatched type"
)

pure_record <- function(task, fn, source_digest = r_body_digest(fn), backend = "none") {
  calls <- intersect(r_function_call_names(fn), r_pure_forbidden_calls())
  policy <- r_pure_contract_policy(task)
  list(
    schema_version = r_provenance_schema_version(),
    task = task,
    function_name = task,
    implementation_class = "pure_r",
    source_digest = source_digest,
    source_body = r_source_body(fn),
    ast_calls = as.list(r_function_call_names(fn)),
    ast_allowlist_id = policy$id,
    ast_allowlist = as.list(policy$allowed_calls),
    forbidden_call_result = if (length(calls) == 0L) "pass" else paste0("declared:", paste(sort(calls), collapse = "+")),
    compiled_backend = backend,
    backend_calls = list(),
    backend_classes = list("none"),
    backend_identity_keys = list("none")
  )
}

pure_test_task <- "52_boundary_scalar_generated"
native_entry <- function(x) .Call("native_entry", x)
expect_error(
  "pure R native entry",
  validate_r_provenance_record(pure_record(pure_test_task, native_entry), native_entry),
  "forbidden calls.*\\.Call"
)

compiled_kernel <- function(x) sum(x)
expect_error(
  "compiled kernel labeled pure R",
  validate_r_provenance_record(pure_record(pure_test_task, compiled_kernel), compiled_kernel),
  "forbidden calls.*sum"
)

namespace_escape <- function(x) base::sum(x)
expect_error(
  "namespace escape labeled pure R",
  validate_r_provenance_record(pure_record(pure_test_task, namespace_escape), namespace_escape),
  "forbidden calls.*::"
)

dynamic_dispatch <- function(fn, x) do.call(fn, list(x))
expect_error(
  "dynamic dispatch labeled pure R",
  validate_r_provenance_record(pure_record(pure_test_task, dynamic_dispatch), dynamic_dispatch),
  "forbidden calls.*do.call"
)

hidden_native_helper <- function(x) x
helper_dispatch <- function(x) hidden_native_helper(x)
expect_error(
  "undeclared helper dispatch labeled pure R",
  validate_r_provenance_record(pure_record(pure_test_task, helper_dispatch), helper_dispatch),
  "outside the AST allowlist.*hidden_native_helper"
)

wrong_contract_primitive <- function(x) seq_len(length(x))
expect_error(
  "pure call allowed only by another contract",
  validate_r_provenance_record(pure_record(pure_test_task, wrong_contract_primitive), wrong_contract_primitive),
  "outside the AST allowlist.*seq_len"
)

unmapped_record <- pure_record(pure_test_task, function(x) x)
unmapped_record$task <- "unmapped_pure_contract"
expect_error(
  "pure contract without an authored allowlist",
  validate_r_provenance_record(unmapped_record, function(x) x),
  "no authored AST allowlist"
)

plain_r <- function(x) x
expect_error(
  "missing R source digest",
  validate_r_provenance_record(pure_record(pure_test_task, plain_r, source_digest = ""), plain_r),
  "source digest is missing"
)
expect_error(
  "optimized backend labeled pure R",
  validate_r_provenance_record(pure_record(pure_test_task, plain_r, backend = "BLAS"), plain_r),
  "optimized backend row claims pure R"
)

gap <- list(executable = FALSE, reason = "source-backed product gap")
validate_summary_disposition("N/A", "NOT_APPLICABLE", gap, gap$reason, "cpp11", "fixture_task")
validate_timing_admission(
  list(executable = TRUE, timing_eligible = TRUE), "zigr", "fixture_task"
)
expect_error(
  "timing without normalized admission",
  validate_timing_admission(
    list(executable = TRUE, timing_eligible = FALSE), "zigr", "fixture_task"
  ),
  "not admitted for timing"
)
expect_error(
  "runner-specific N/A on executable row",
  validate_summary_disposition("N/A", "NOT_APPLICABLE", list(executable = TRUE, reason = ""), "", "zigr", "fixture_task"),
  "runner-specific N/A error"
)
expect_error(
  "runner-specific N/A reason drift",
  validate_summary_disposition("N/A", "NOT_APPLICABLE", gap, "different reason", "cpp11", "fixture_task"),
  "normalized disposition reason"
)

r_record <- list(
  name = "r",
  so_path = "",
  artifact_paths = list(file.path(root_dir, "src/r/run_all.R")),
  artifact_digest = absolute_file_identity_digest(file.path(root_dir, "src/r/run_all.R"), "R artifact"),
  runner_config_digest = source_ledger_object_digest(load_runner_configs(root_dir)$r),
  generated_glue_paths = list("src/r/run_all.R"),
  generated_glue_digest = file_identity_digest(root_dir, "src/r/run_all.R", "R source glue")
)
validate_runner_artifact_identity(root_dir, r_record)

input_artifact <- file.path(root_dir, "src/r/run_all.R")
input_artifact_digest <- unname(as.character(tools::md5sum(input_artifact))[[1L]])
validate_input_manifest_digest(input_artifact, input_artifact_digest)
expect_error(
  "canonical input artifact drift",
  validate_input_manifest_digest(input_artifact, "00000000000000000000000000000000"),
  "canonical input manifest digest differs"
)

drifted_record <- r_record
drifted_record$artifact_digest <- "00000000000000000000000000000000"
expect_error(
  "artifact drift",
  validate_runner_artifact_identity(root_dir, drifted_record),
  "artifact drift detected"
)

drifted_config <- r_record
drifted_config$runner_config_digest <- "00000000000000000000000000000000"
expect_error(
  "runner configuration drift",
  validate_runner_artifact_identity(root_dir, drifted_config),
  "runner config drift detected"
)

drifted_glue <- r_record
drifted_glue$generated_glue_digest <- "00000000000000000000000000000000"
expect_error(
  "generated glue drift",
  validate_runner_artifact_identity(root_dir, drifted_glue),
  "generated-glue drift detected"
)

sealed_run <- tempfile("sealed-run-")
dir.create(sealed_run, recursive = TRUE)
dir.create(file.path(sealed_run, "correctness"), recursive = TRUE)
writeLines("input", file.path(sealed_run, "input_manifest.json"))
sealed_task_summary <- data.frame(runner = "r", task = "fixture_task", status = "PASS", stringsAsFactors = FALSE)
sealed_fixture_summary <- data.frame(runner = "r", row_id = "F01", status = "PASS", stringsAsFactors = FALSE)
write.csv(sealed_task_summary, file.path(sealed_run, "task_summary.csv"), row.names = FALSE)
write.csv(sealed_fixture_summary, file.path(sealed_run, "fixture_summary.csv"), row.names = FALSE)
write.csv(
  data.frame(runner = "r", task = "fixture_task", phase = c("first_call", "timed"), wall_ms = 1, run_id = "sealed"),
  file.path(sealed_run, "task_samples.csv"),
  row.names = FALSE
)
write.csv(data.frame(runner = "r", row_id = "F01", wall_ms = 1), file.path(sealed_run, "fixture_samples.csv"), row.names = FALSE)
writeLines("task correctness", file.path(sealed_run, "correctness", "tasks.csv"))
writeLines("fixture correctness", file.path(sealed_run, "correctness", "fixtures.csv"))
sealed_metadata <- list(
  schema_version = 3L,
  artifact_layout = "grouped-v1",
  run_id = "sealed",
  status = "complete",
  started_at = "2026-07-15T00:00:00.000Z",
  finished_at = "2026-07-15T00:01:00.000Z",
  runners = list("r"),
  tasks = list("fixture_task"),
  master_seed = 1L,
  input_manifest = list(relative_path = "input_manifest.json", digest = "input"),
  runner_dispositions = list(r = list(list(task = "fixture_task", status = "applicable"))),
  r_provenance = list(schema_version = "test"),
  timing_policy = benchmark_timing_policy(),
  boundary_budget_policy_version = boundary_budget_policy_version(),
  suite = "all",
  full_matrix = TRUE,
  promotion_eligible = TRUE,
  measurement_mode = "timed",
  environment = list(identity = "test"),
  correctness_stage = list(status = "complete")
)
sealed_metadata$completion_artifacts <- capture_run_completion_artifacts(sealed_run, sealed_metadata)
sealed_metadata$completion_contract <- capture_run_completion_contract(sealed_metadata)
write_run_manifest(sealed_run, sealed_metadata)
validate_run_completion_contract(sealed_metadata)
validate_run_completion_artifacts(sealed_run, sealed_metadata)
validate_run_core_artifact_set(sealed_run, sealed_metadata)
cache_root <- tempfile("retained-cache-")
validate_run_cache_paths(sealed_run, c(cache_root, paste0(cache_root, "-global")))
expect_error(
  "cache nested in published run",
  validate_run_cache_paths(sealed_run, file.path(sealed_run, ".zig-cache")),
  "caches must be outside"
)
writeLines("undeclared", file.path(sealed_run, "undeclared.txt"))
expect_error(
  "extra completed core artifact",
  validate_run_core_artifact_set(sealed_run, sealed_metadata),
  "extra: undeclared.txt"
)
unlink(file.path(sealed_run, "undeclared.txt"))
report_names <- unname(declared_report_files())
report_roles <- names(declared_report_files())
report_tables <- lapply(report_roles, function(role) {
  if (identical(role, "comparative")) {
    data.frame(report_track = c("product", "strategy", "r_baseline", "control", "diagnostic"), value = 1:5)
  } else if (identical(role, "budget")) {
    data.frame(report_track = c("boundary", "boundary_budget", "representation_budget"), value = 1:3)
  } else {
    data.frame(value = role)
  }
})
for (index in seq_along(report_names)) {
  write.csv(report_tables[[index]], file.path(sealed_run, report_names[[index]]), row.names = FALSE)
}
report_records <- lapply(seq_along(report_names), function(index) {
  record <- list(
    role = report_roles[[index]], file = report_names[[index]], rows = nrow(report_tables[[index]]),
    md5 = unname(as.character(tools::md5sum(file.path(sealed_run, report_names[[index]]))[[1L]]))
  )
  if ("report_track" %in% names(report_tables[[index]])) {
    counts <- table(report_tables[[index]]$report_track)
    record$tracks <- lapply(names(counts), function(track) list(track = track, rows = as.integer(counts[[track]])))
  }
  record
})
report_manifest <- list(
  declared_report_files = as.list(report_names), reports = report_records
)
jsonlite::write_json(
  report_manifest, file.path(sealed_run, "report_manifest.json"), auto_unbox = TRUE
)
report_manifest <- jsonlite::fromJSON(
  file.path(sealed_run, "report_manifest.json"), simplifyVector = FALSE
)
validate_report_artifact_set(sealed_run, sealed_metadata, report_manifest)
wrong_report_rows <- unserialize(serialize(report_manifest, NULL))
wrong_report_rows$reports[[1L]]$rows <- wrong_report_rows$reports[[1L]]$rows + 1L
expect_error(
  "report manifest false row count",
  validate_report_artifact_set(sealed_run, sealed_metadata, wrong_report_rows),
  "row count differs"
)
wrong_track_rows <- unserialize(serialize(report_manifest, NULL))
wrong_track_rows$reports[[1L]]$tracks[[1L]]$rows <- 2L
expect_error(
  "report manifest false track count",
  validate_report_artifact_set(sealed_run, sealed_metadata, wrong_track_rows),
  "track counts differ"
)
wrong_report_role <- unserialize(serialize(report_manifest, NULL))
wrong_report_role$reports[[1L]]$role <- "diagnostic"
expect_error(
  "report manifest false role",
  validate_report_artifact_set(sealed_run, sealed_metadata, wrong_report_role),
  "roles differ"
)
writeLines("undeclared derived output", file.path(sealed_run, "debug_report.csv"))
expect_error(
  "undeclared derived report",
  validate_report_artifact_set(sealed_run, sealed_metadata, report_manifest),
  "extra: debug_report.csv"
)
unlink(file.path(sealed_run, "debug_report.csv"))
missing_declaration <- unserialize(serialize(report_manifest, NULL))
missing_declaration$declared_report_files <- missing_declaration$declared_report_files[-1L]
expect_error(
  "incomplete report declaration",
  validate_report_artifact_set(sealed_run, sealed_metadata, missing_declaration),
  "declared filenames differ"
)
unlink(file.path(sealed_run, c("report_manifest.json", report_names)))

task_suite_metadata <- unserialize(serialize(sealed_metadata, NULL))
task_suite_metadata$suite <- "tasks"
task_suite_metadata$full_matrix <- FALSE
task_suite_metadata$promotion_eligible <- FALSE
task_suite_paths <- run_completion_artifact_paths(sealed_run, task_suite_metadata)
fixture_suite_metadata <- unserialize(serialize(sealed_metadata, NULL))
fixture_suite_metadata$suite <- "fixtures"
fixture_suite_metadata$tasks <- list()
fixture_suite_metadata$input_manifest <- list(relative_path = "not_applicable", digest = "not_applicable")
fixture_suite_metadata$runner_dispositions <- list(r = list())
fixture_suite_metadata$full_matrix <- FALSE
fixture_suite_metadata$promotion_eligible <- FALSE
fixture_suite_paths <- run_completion_artifact_paths(sealed_run, fixture_suite_metadata)
expect_true(
  setequal(task_suite_paths, c(
    "correctness/tasks.csv", "input_manifest.json", "task_samples.csv", "task_summary.csv"
  )) &&
    setequal(fixture_suite_paths, c(
      "correctness/fixtures.csv", "fixture_samples.csv", "fixture_summary.csv"
    )),
  "suite completion seals include only the selected universe artifacts"
)

drifted_status <- unserialize(serialize(sealed_metadata, NULL))
drifted_status$status <- "incomplete"
expect_error(
  "completed manifest status drift",
  validate_run_completion_contract(drifted_status),
  "requires complete status"
)

edited_summary <- sealed_task_summary
edited_summary$extra <- "tampered"
write.csv(edited_summary, file.path(sealed_run, "task_summary.csv"), row.names = FALSE)
expect_error(
  "edited completed summary",
  validate_run_completion_artifacts(sealed_run, sealed_metadata),
  "completion artifacts differ"
)
write.csv(sealed_task_summary, file.path(sealed_run, "task_summary.csv"), row.names = FALSE)

unlink(file.path(sealed_run, "fixture_samples.csv"))
expect_error(
  "missing completed fixture sample",
  validate_run_completion_artifacts(sealed_run, sealed_metadata),
  "completion artifacts are missing"
)
write.csv(data.frame(runner = "r", row_id = "F01", wall_ms = 1), file.path(sealed_run, "fixture_samples.csv"), row.names = FALSE)

drifted_completion <- unserialize(serialize(sealed_metadata, NULL))
drifted_completion$timing_policy$confirmation_max_iterations <-
  drifted_completion$timing_policy$confirmation_max_iterations + 1L
expect_error(
  "completed manifest contract drift",
  validate_run_completion_contract(drifted_completion),
  "completion contract differs"
)
drifted_seal <- unserialize(serialize(sealed_metadata, NULL))
drifted_seal$completion_artifacts$files[[1L]]$md5 <- "00000000000000000000000000000000"
drifted_seal$completion_artifacts$digest <- run_manifest_object_digest(drifted_seal$completion_artifacts$files)
expect_error(
  "rewritten completion artifact seal",
  validate_run_completion_contract(drifted_seal),
  "completion contract differs"
)
drifted_disposition <- unserialize(serialize(sealed_metadata$runner_dispositions, NULL))
drifted_disposition$r[[1L]]$status <- "product_gap"
expect_error(
  "newly unsupported completed disposition",
  validate_run_disposition_identity(sealed_metadata, drifted_disposition),
  "current task dispositions differ"
)
expect_error(
  "unsafe completion artifact path",
  run_manifest_relative_artifact_path("..", "copied-run.csv"),
  "artifact path is unsafe"
)
unsafe_input_metadata <- unserialize(serialize(sealed_metadata, NULL))
unsafe_input_metadata$input_manifest$relative_path <- file.path("..", "input_manifest.json")
expect_error(
  "unsafe input artifact path reaches completion capture",
  capture_run_completion_artifacts(sealed_run, unsafe_input_metadata),
  "artifact path is unsafe"
)
unsafe_copy_dir <- tempfile("unsafe-copy-")
dir.create(unsafe_copy_dir)
expect_error(
  "unsafe sealed path reaches promotion copy",
  copy_run_artifact_set(sealed_run, unsafe_copy_dir, file.path("..", "input_manifest.json")),
  "artifact path is unsafe"
)
unlink(unsafe_copy_dir, recursive = TRUE)
failure_run <- tempfile("failed-run-")
dir.create(file.path(failure_run, ".staging", "timing", "runner"), recursive = TRUE)
failure_metadata <- list(
  schema_version = 3L, artifact_layout = "grouped-v1", run_id = basename(failure_run),
  status = "running", started_at = "2026-07-15T00:00:00.000Z"
)
write_run_manifest(failure_run, failure_metadata)
write.csv(
  data.frame(runner = "zigr", task = "fixture_task", error = "worker detail"),
  file.path(failure_run, ".staging", "timing", "runner", "errors.csv"), row.names = FALSE
)
writeLines("partial", file.path(failure_run, "task_summary.csv"))
record_run_failure(failure_run, "batch failed")
failure_record <- read_run_manifest(failure_run)
expect_true(
  identical(run_relative_files(failure_run), "run_manifest.json") &&
    identical(as.character(failure_record$status), "incomplete") &&
    grepl("batch failed", failure_record$status_message, fixed = TRUE) &&
    grepl("worker detail", failure_record$status_message, fixed = TRUE),
  "failed run removes partial publication and retains one compact failure record"
)
unlink(failure_run, recursive = TRUE)
stale_root <- tempfile("stale-runs-")
stale_run <- file.path(stale_root, "runs", "stale")
dir.create(file.path(stale_run, ".staging"), recursive = TRUE)
write_run_manifest(stale_run, list(
  schema_version = 3L, artifact_layout = "grouped-v1", run_id = "stale",
  status = "running", started_at = "2026-01-01T00:00:00.000Z"
))
writeLines("partial", file.path(stale_run, ".staging", "partial.csv"))
reconciled <- reconcile_running_runs(stale_root, "replacement", stale_after_seconds = 0)
stale_record <- read_run_manifest(stale_run)
expect_true(
  identical(reconciled, "stale") && identical(run_relative_files(stale_run), "run_manifest.json") &&
    identical(as.character(stale_record$status), "incomplete"),
  "stale run reconciliation retains only one compact failure record"
)
unlink(stale_root, recursive = TRUE)
atomic_record_dir <- tempfile("atomic-record-")
atomic_record_path <- file.path(atomic_record_dir, "record.json")
write_run_manifest_json_atomic(list(version = 1L), atomic_record_path)
write_run_manifest_json_atomic(list(version = 2L), atomic_record_path)
atomic_record <- jsonlite::fromJSON(atomic_record_path, simplifyVector = FALSE)
expect_true(
  identical(as.integer(atomic_record$version), 2L) &&
    length(list.files(atomic_record_dir, all.files = TRUE, no.. = TRUE)) == 1L,
  "atomic JSON replacement leaves only the current record"
)
sealed_receipt <- seal_run_promotion_receipt(list(schema_version = "test", run_id = "sealed"))
validate_run_promotion_receipt(sealed_receipt)
tampered_receipt <- unserialize(serialize(sealed_receipt, NULL))
tampered_receipt$run_id <- "tampered"
expect_error(
  "tampered compact promotion receipt",
  validate_run_promotion_receipt(tampered_receipt),
  "receipt digest differs"
)
canonical_receipt <- jsonlite::fromJSON(
  file.path(root_dir, "results", "CANONICAL_RUN.json"),
  simplifyVector = FALSE
)
validate_run_promotion_receipt(canonical_receipt)
canonical_shape <- if (identical(canonical_receipt$record_kind, "migrated_promotion_record")) {
  identical(canonical_receipt$schema_version, "benchmark-acceptance-receipt-v1") &&
    identical(canonical_receipt$origin$schema_version, "benchmark-promotion-v1") &&
    identical(canonical_receipt$origin$promotion_manifest_md5, "cd6b3f60c547af5ac3599b21574699fd")
} else {
  identical(canonical_receipt$schema_version, "benchmark-promotion-v3") &&
    length(canonical_receipt$promoted_at) == 1L && nzchar(as.character(canonical_receipt$promoted_at))
}
expect_true(
  canonical_shape &&
    !grepl(normalizePath(file.path(root_dir, "..")), jsonlite::toJSON(canonical_receipt), fixed = TRUE),
  "tracked canonical evidence is a sealed portable acceptance record"
)
prune_root <- tempfile("run-prune-")
dir.create(file.path(prune_root, "runs"), recursive = TRUE)
prune_records <- list(
  list(run_id = "old", status = "complete"),
  list(run_id = "new", status = "incomplete"),
  list(run_id = "protected", status = "complete"),
  list(run_id = "active", status = "running")
)
for (index in seq_along(prune_records)) {
  record <- prune_records[[index]]
  path <- file.path(prune_root, "runs", record$run_id)
  dir.create(path)
  jsonlite::write_json(record, run_manifest_path(path), auto_unbox = TRUE)
  Sys.setFileTime(path, as.POSIXct("2026-01-01", tz = "UTC") + index)
}
dry_candidates <- prune_local_runs(prune_root, 1L, "protected", dry_run = TRUE)
expect_true(
  identical(dry_candidates, "old") && dir.exists(file.path(prune_root, "runs", "old")),
  "run pruning dry run selects only old unprotected completed evidence"
)
removed_runs <- prune_local_runs(prune_root, 1L, "protected")
expect_true(
  identical(removed_runs, "old") &&
    !dir.exists(file.path(prune_root, "runs", "old")) &&
    dir.exists(file.path(prune_root, "runs", "new")) &&
    dir.exists(file.path(prune_root, "runs", "protected")) &&
    dir.exists(file.path(prune_root, "runs", "active")),
  "run pruning retains the newest, canonical, and active runs"
)
expect_error(
  "run pruning rejects fractional retention",
  prune_local_runs(prune_root, "1.5", dry_run = TRUE),
  "non-negative integer"
)
expect_error(
  "run pruning rejects negative retention",
  prune_local_runs(prune_root, "-1", dry_run = TRUE),
  "non-negative integer"
)
mismatched_path <- file.path(prune_root, "runs", "directory-name")
dir.create(mismatched_path)
jsonlite::write_json(
  list(run_id = "different-manifest-id", status = "complete"),
  run_manifest_path(mismatched_path), auto_unbox = TRUE
)
expect_true(
  !("different-manifest-id" %in% prune_local_runs(prune_root, 0L, dry_run = TRUE)) &&
    dir.exists(mismatched_path),
  "run pruning ignores a manifest whose run ID differs from its directory"
)
unlink(prune_root, recursive = TRUE)
unlink(atomic_record_dir, recursive = TRUE)
unlink(sealed_run, recursive = TRUE)

validate_forced_registration(FALSE, "registered fixture")
expect_true(
  identical(evaluate_prepared_call(function() 42L), 42L) &&
    identical(evaluate_prepared_call(quote(40L + 2L)), 42L),
  "timing harness accepts prepared closures and legacy expressions"
)
expect_error(
  "wrong registration mode",
  validate_forced_registration(TRUE, "dynamic fixture"),
  "dynamic symbol lookup is enabled"
)

source_root <- normalizePath(file.path(root_dir, ".."))
source_probe <- tempfile("source-identity-probe-", tmpdir = root_dir, fileext = ".R")
on.exit(unlink(source_probe), add = TRUE)
writeLines("probe <- 1L", source_probe)
recorded_source <- source_tree_identity(source_root)
validate_source_tree_identity(source_root, recorded_source)
writeLines("probe <- 2L", source_probe)
expect_error(
  "source tree changed after run capture",
  validate_source_tree_identity(source_root, recorded_source),
  "source tree identity field digest differs"
)
unlink(source_probe)


# Deterministic property fuzz: semantically identical clones must hash equally,
# while value, attribute, missing-kind, encoding, and nested mutations must drift.
set.seed(20260715L)
fuzz_values <- list(
  numeric(0),
  c(-0.0, NA_real_, NaN, runif(29L)),
  c(NA_integer_, sample.int(1000L, 63L, replace = TRUE)),
  c(enc2utf8("façade"), iconv("façade", from = "UTF-8", to = "latin1"), NA_character_, ""),
  as.raw(sample.int(256L, 257L, replace = TRUE) - 1L),
  complex(real = c(NA_real_, NaN, runif(15L)), imaginary = runif(17L)),
  list(empty = list(), nested = list(values = c(NA_real_, -0.0, 1.0)), flag = TRUE)
)
for (index in seq_along(fuzz_values)) {
  original <- list(fuzz_values[[index]])
  cloned <- unserialize(serialize(original, NULL))
  base <- task_arguments_fingerprint(sprintf("fuzz_%02d", index), original, "ordinary_r_object")
  same <- task_arguments_fingerprint(sprintf("fuzz_%02d", index), cloned, "ordinary_r_object")
  mutated <- unserialize(serialize(original, NULL))
  attr(mutated[[1L]], "fuzz_mutation") <- index
  changed <- task_arguments_fingerprint(sprintf("fuzz_%02d", index), mutated, "ordinary_r_object")
  expect_true(
    identical(base, same) && !identical(base, changed),
    sprintf("fingerprint fuzz case %d is clone-stable and attribute-sensitive", index)
  )
}
missing_kinds <- list(list(NA_real_), list(NaN), list(NA_integer_), list(NA_complex_))
missing_fingerprints <- vapply(seq_along(missing_kinds), function(index) {
  task_arguments_fingerprint("missing_kind", missing_kinds[[index]], "ordinary_r_object")
}, character(1))
expect_true(!anyDuplicated(missing_fingerprints), "fingerprints distinguish adjacent missing-value kinds")

manifest <- load_task_manifest(root_dir)
evidence <- load_evidence_manifest(root_dir, manifest)
specs <- fixture_measurement_specs()
expect_true(
  identical(names(specs), c(sprintf("F%02d", 1:10), "F12")),
  "measurement cases cover every timed fixture and exclude correctness-only F11"
)
expect_true(
  isTRUE(specs$F10$stateful) &&
    identical(fixture_measurement_altrep_intent(specs$F04), "compact_integer_altrep") &&
    fixture_measurement_requires_fresh_input(specs$F04) &&
    fixture_measurement_requires_fresh_input(specs$F10) &&
    !fixture_measurement_requires_fresh_input(specs$F03),
  "state and ALTREP measurement setup are explicit"
)
first <- vapply(names(specs), function(fixture) {
  fixture_measurement_input_fingerprint(fixture, specs[[fixture]])
}, character(1))
second <- vapply(names(specs), function(fixture) {
  fixture_measurement_input_fingerprint(fixture, specs[[fixture]])
}, character(1))
expect_true(identical(first, second) && !anyDuplicated(first), "fixture measurement inputs are deterministic and distinct")
expect_true(
  identical(names(fixture_measurement_optimized_specs()), c("F03", "F04")),
  "optimized base-R timing is limited to the declared fixture backends"
)
expect_true(
  all(evidence$fixture_rows$fixture[evidence$fixture_rows$timing_eligible] %in% names(specs)) &&
    !any(evidence$fixture_rows$fixture == "F11" & evidence$fixture_rows$timing_eligible),
  "fixture timing specs agree with normalized admission"
)

argument_builds <- 0L
calls <- 0L
prepared <- fixture_measurement_prepare(
  list(probe = function(value) {
    calls <<- calls + 1L
    value
  }),
  "probe",
  list(
    function_name = "probe",
    arguments = function() {
      argument_builds <<- argument_builds + 1L
      list(42L)
    }
  )
)
expect_true(argument_builds == 1L && calls == 0L, "ordinary fixture input is prepared outside timing")
expect_true(identical(prepared(), 42L) && argument_builds == 1L && calls == 1L, "prepared call times only the fixture")
reused <- fixture_measurement_prepare(
  list(probe = function(value) value),
  "probe",
  list(function_name = "probe", arguments = function() stop("reused fixture arguments were rebuilt")),
  list(42L)
)
expect_true(identical(reused(), 42L), "ordinary fixture input can be reused without rebuilding")

state_builds <- 0L
state_calls <- 0L
state_functions <- list(
  fixture_new = function() {
    state_builds <<- state_builds + 1L
    new.env(parent = emptyenv())
  },
  fixture_method = function(state, amount) {
    state_calls <<- state_calls + 1L
    amount
  }
)
state_spec <- list(stateful = TRUE, arguments = function() list(7L))
first_state_call <- fixture_measurement_prepare(state_functions, "F10", state_spec)
second_state_call <- fixture_measurement_prepare(state_functions, "F10", state_spec)
expect_true(
  state_builds == 2L && state_calls == 0L &&
    identical(first_state_call(), 7L) && identical(second_state_call(), 7L) && state_calls == 2L,
  "each F10 sample constructs a fresh receiver outside timing"
)

correctness_root <- tempfile("correctness-artifacts-")
dir.create(file.path(correctness_root, "correctness"), recursive = TRUE)
on.exit(unlink(correctness_root, recursive = TRUE), add = TRUE)
test_metadata <- list(
  schema_version = 3L,
  artifact_layout = "grouped-v1",
  run_id = "correctness-test",
  suite = "all",
  runners = list("zigr"),
  tasks = as.list(c("task_gap", "task_pass")),
  timing_policy = timing_policy,
  input_manifest = list(digest = "input-digest"),
  runner_dispositions = list(zigr = list(
    list(task = "task_gap", executable = FALSE, reason = "declared task gap", contract_version = "gap-v1"),
    list(task = "task_pass", executable = TRUE, reason = "", contract_version = "pass-v1")
  )),
  environment = list(
    source_tree = list(digest = "source-digest"),
    runner_configs = list(list(
      name = "zigr", source_ledger_identity_digest = "ledger-digest",
      artifact_digest = "task-artifact", fixture_artifact_digest = "fixture-artifact"
    ))
  )
)
test_evidence <- list(
  fixtures = c("F01", "F08", "F11"),
  fixture_rows = data.frame(
    runner = rep("zigr", 3L), fixture = c("F01", "F08", "F11"),
    executable = c(TRUE, FALSE, TRUE), reason = c("", "declared fixture gap", ""),
    contract_version = c("f01-v1", "f08-v1", "f11-v1"),
    stringsAsFactors = FALSE
  )
)
task_correctness <- data.frame(
  run_id = rep("correctness-test", 2L), runner = rep("zigr", 2L),
  task = c("task_gap", "task_pass"), status = c("N/A", "PASS"),
  correctness_status = c("NOT_APPLICABLE", "PASS"), correctness_policy = c("none", "oracle"),
  correctness_message = c("declared task gap", "validated"),
  source_tree_digest = rep("source-digest", 2L),
  source_ledger_identity_digest = rep("ledger-digest", 2L),
  artifact_digest = rep("task-artifact", 2L), input_manifest_digest = rep("input-digest", 2L),
  contract_version = c("gap-v1", "pass-v1"),
  timing_policy_digest = rep(run_manifest_object_digest(timing_policy), 2L),
  stringsAsFactors = FALSE
)
fixture_correctness <- data.frame(
  run_id = rep("correctness-test", 3L), runner = rep("zigr", 3L),
  fixture = c("F01", "F08", "F11"), variant = rep("public", 3L),
  row_id = c("F01", "F08", "F11"), status = c("PASS", "N/A", "PASS"),
  correctness_status = c("PASS", "NOT_APPLICABLE", "PASS"),
  correctness_message = c("validated", "declared fixture gap", "validated"),
  source_tree_digest = rep("source-digest", 3L),
  source_ledger_identity_digest = rep("ledger-digest", 3L),
  artifact_digest = rep("fixture-artifact", 3L),
  input_fingerprint = vapply(c("F01", "F08", "F11"), function(fixture) {
    spec <- fixture_measurement_specs()[[fixture]]
    if (is.null(spec)) "not_applicable" else fixture_measurement_input_fingerprint(fixture, spec)
  }, character(1)),
  contract_version = c("f01-v1", "f08-v1", "f11-v1"),
  timing_policy_digest = rep(run_manifest_object_digest(timing_policy), 3L),
  stringsAsFactors = FALSE
)
task_path <- file.path(correctness_root, "correctness", "tasks.csv")
fixture_path <- file.path(correctness_root, "correctness", "fixtures.csv")
write.csv(task_correctness, task_path, row.names = FALSE, na = "")
write.csv(fixture_correctness, fixture_path, row.names = FALSE, na = "")
retained <- load_retained_correctness(
  fixture_path,
  fixture_path,
  "zigr",
  "correctness-test",
  "row_id",
  c("F01", "F08", "F11"),
  c("F01", "F11"),
  list(
    source_tree_digest = "source-digest",
    source_ledger_identity_digest = "ledger-digest",
    artifact_digest = "fixture-artifact",
    input_fingerprint = stats::setNames(fixture_correctness$input_fingerprint, fixture_correctness$row_id),
    contract_version = stats::setNames(fixture_correctness$contract_version, fixture_correctness$row_id),
    timing_policy_digest = run_manifest_object_digest(timing_policy)
  )
)
expect_true(nrow(retained) == 3L, "retained correctness is reusable by measurement subprocesses")
expect_error(
  "retained correctness rejects timing policy drift",
  load_retained_correctness(
    fixture_path, fixture_path, "zigr", "correctness-test", "row_id",
    c("F01", "F08", "F11"), c("F01", "F11"),
    list(timing_policy_digest = "different-policy")
  ),
  "timing_policy_digest differs"
)
validated <- validate_correctness_artifacts(correctness_root, test_metadata, test_evidence)
expect_true(validated$task_rows == 2L && validated$fixture_rows == 3L, "structured correctness evidence validates")

unlink(fixture_path)
task_only_metadata <- unserialize(serialize(test_metadata, NULL))
task_only_metadata$suite <- "tasks"
task_only <- validate_correctness_artifacts(correctness_root, task_only_metadata, test_evidence)
write.csv(fixture_correctness, fixture_path, row.names = FALSE, na = "")
expect_error(
  "task-only correctness rejects fixture evidence",
  validate_correctness_artifacts(correctness_root, task_only_metadata, test_evidence),
  "unselected suite"
)
unlink(task_path)
fixture_only_metadata <- unserialize(serialize(test_metadata, NULL))
fixture_only_metadata$suite <- "fixtures"
fixture_only_metadata$tasks <- list()
fixture_only_metadata$runner_dispositions <- list(zigr = list())
fixture_only <- validate_correctness_artifacts(correctness_root, fixture_only_metadata, test_evidence)
write.csv(task_correctness, task_path, row.names = FALSE, na = "")
expect_error(
  "fixture-only correctness rejects task evidence",
  validate_correctness_artifacts(correctness_root, fixture_only_metadata, test_evidence),
  "unselected suite"
)
expect_true(
  task_only$task_rows == 2L && task_only$fixture_rows == 0L &&
    fixture_only$task_rows == 0L && fixture_only$fixture_rows == 3L,
  "correctness validation requires only the selected suite and does not reuse the other suite"
)

drifted_task <- task_correctness
drifted_task$source_tree_digest[[2L]] <- "changed-source"
write.csv(drifted_task, task_path, row.names = FALSE, na = "")
expect_error(
  "correctness source identity drift",
  validate_correctness_artifacts(correctness_root, test_metadata, test_evidence),
  "source_tree_digest differs"
)
write.csv(task_correctness, task_path, row.names = FALSE, na = "")
drifted_contract <- task_correctness
drifted_contract$contract_version[[2L]] <- "changed-contract"
write.csv(drifted_contract, task_path, row.names = FALSE, na = "")
expect_error(
  "correctness contract identity drift",
  validate_correctness_artifacts(correctness_root, test_metadata, test_evidence),
  "contract_version differs"
)
write.csv(task_correctness, task_path, row.names = FALSE, na = "")
drifted_fixture_input <- fixture_correctness
drifted_fixture_input$input_fingerprint[[1L]] <- "changed-input"
write.csv(drifted_fixture_input, fixture_path, row.names = FALSE, na = "")
expect_error(
  "correctness fixture input identity drift",
  validate_correctness_artifacts(correctness_root, test_metadata, test_evidence),
  "input_fingerprint differs"
)
write.csv(fixture_correctness, fixture_path, row.names = FALSE, na = "")
identity_drifted_fixture <- fixture_correctness
identity_drifted_fixture$source_tree_digest[[2L]] <- "changed-source"
write.csv(identity_drifted_fixture, fixture_path, row.names = FALSE, na = "")
expect_error(
  "retained correctness rejects source identity drift",
  load_retained_correctness(
    fixture_path, fixture_path, "zigr", "correctness-test", "row_id",
    c("F01", "F08", "F11"), c("F01", "F11"),
    list(source_tree_digest = "source-digest", artifact_digest = "fixture-artifact")
  ),
  "source_tree_digest differs"
)
write.csv(fixture_correctness, fixture_path, row.names = FALSE, na = "")
not_passing_fixture <- fixture_correctness
not_passing_fixture$status[[1L]] <- "N/A"
not_passing_fixture$correctness_status[[1L]] <- "NOT_APPLICABLE"
write.csv(not_passing_fixture, fixture_path, row.names = FALSE, na = "")
expect_error(
  "retained correctness requires executable cells to pass",
  load_retained_correctness(
    fixture_path, fixture_path, "zigr", "correctness-test", "row_id",
    c("F01", "F08", "F11"), c("F01", "F11")
  ),
  "not passing for every executable cell"
)
write.csv(fixture_correctness, fixture_path, row.names = FALSE, na = "")
drifted_fixture <- fixture_correctness
drifted_fixture$correctness_message[[2L]] <- "hidden gap"
failed_fixture <- fixture_correctness
failed_fixture$status[[1L]] <- "FAIL"
write.csv(failed_fixture, fixture_path, row.names = FALSE, na = "")
expect_error(
  "retained correctness rejects failures",
  load_retained_correctness(
    fixture_path,
    fixture_path,
    "zigr",
    "correctness-test",
    "row_id",
    c("F01", "F08", "F11")
  ),
  "does not match the selected run cells"
)
write.csv(drifted_fixture, fixture_path, row.names = FALSE, na = "")
expect_error(
  "correctness gap reason drift",
  validate_correctness_artifacts(correctness_root, test_metadata, test_evidence),
  "fixture correctness gap reason differs"
)

raw_times <- data.frame(wall_ms = seq(0.02, 0.069, length.out = 50L))
raw_mean <- mean(raw_times$wall_ms)
raw_window_cv <- sd(raw_times$wall_ms) / raw_mean * 100
timing_policy <- benchmark_timing_policy()
raw_summary <- data.frame(
  mean_ms = round(raw_mean, 4), median_ms = round(median(raw_times$wall_ms), 4),
  min_ms = round(min(raw_times$wall_ms), 4), max_ms = round(max(raw_times$wall_ms), 4),
  sd_ms = round(sd(raw_times$wall_ms), 4), cv_pct = round(raw_window_cv, 2),
  timer_noise_status = "above_floor",
  stringsAsFactors = FALSE
)
validate_fixture_raw_statistics(raw_summary, raw_times, timing_policy, "synthetic/F01")
bad_statistics <- raw_summary
bad_statistics$min_ms <- bad_statistics$min_ms + 1
expect_error(
  "fixture summary statistic drift",
  validate_fixture_raw_statistics(bad_statistics, raw_times, timing_policy, "synthetic/F01"),
  "raw statistics differ"
)
bad_noise <- raw_summary
bad_noise$timer_noise_status <- "below_floor"
expect_error(
  "fixture timer-noise drift",
  validate_fixture_raw_statistics(bad_noise, raw_times, timing_policy, "synthetic/F01"),
  "timer-noise evidence differs"
)

cat("Measurement trust passed: deterministic fuzz, phase isolation, sequence behavior, run seals, and fixture drift checks.\n")
