#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("cannot determine fixture measurement test location")
script_path <- normalizePath(sub("^--file=", "", script_argument[[1L]]))
root_dir <- normalizePath(file.path(dirname(script_path), ".."))

source(file.path(root_dir, "lib", "task_manifest.R"))
source(file.path(root_dir, "lib", "evidence_schema.R"))
source(file.path(root_dir, "lib", "input_contract.R"))
source(file.path(root_dir, "lib", "r_provenance.R"))
source(file.path(root_dir, "lib", "run_manifest.R"))
source(file.path(root_dir, "lib", "source_ledger.R"))
source(file.path(root_dir, "lib", "environment_manifest.R"))
source(file.path(root_dir, "lib", "product_fixtures.R"))
source(file.path(root_dir, "lib", "fixture_measurement.R"))

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
dir.create(file.path(correctness_root, "correctness", "tasks"), recursive = TRUE)
dir.create(file.path(correctness_root, "correctness", "fixtures"), recursive = TRUE)
on.exit(unlink(correctness_root, recursive = TRUE), add = TRUE)
test_metadata <- list(
  run_id = "correctness-test",
  runners = list("zigr"),
  tasks = as.list(c("task_gap", "task_pass")),
  input_manifest = list(digest = "input-digest"),
  runner_dispositions = list(zigr = list(
    list(task = "task_gap", executable = FALSE, reason = "declared task gap"),
    list(task = "task_pass", executable = TRUE, reason = "")
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
  artifact_digest = rep("fixture-artifact", 3L), stringsAsFactors = FALSE
)
task_path <- file.path(correctness_root, "correctness", "tasks", "zigr.csv")
fixture_path <- file.path(correctness_root, "correctness", "fixtures", "zigr.csv")
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
    artifact_digest = "fixture-artifact"
  )
)
expect_true(nrow(retained) == 3L, "retained correctness is reusable by measurement subprocesses")
validated <- validate_correctness_artifacts(correctness_root, test_metadata, test_evidence)
expect_true(validated$task_rows == 2L && validated$fixture_rows == 3L, "structured correctness evidence validates")

drifted_task <- task_correctness
drifted_task$source_tree_digest[[2L]] <- "changed-source"
write.csv(drifted_task, task_path, row.names = FALSE, na = "")
expect_error(
  "correctness source identity drift",
  validate_correctness_artifacts(correctness_root, test_metadata, test_evidence),
  "source_tree_digest differs"
)
write.csv(task_correctness, task_path, row.names = FALSE, na = "")
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
  timer_noise_status = "above_floor", convergence_cv_pct = raw_window_cv,
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

cat("Fixture measurement passed: deterministic F01-F10 and F12 inputs, state setup, ALTREP setup, and optimized R variants.\n")
