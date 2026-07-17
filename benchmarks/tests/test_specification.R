#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("cannot determine direct test location")
script_path <- normalizePath(sub("^--file=", "", script_argument[[1L]]))
root_dir <- normalizePath(file.path(dirname(script_path), ".."))

source(file.path(root_dir, "lib", "measurement.R"))
source(file.path(root_dir, "lib", "run_manifest.R"))
source(file.path(root_dir, "lib", "product_fixtures.R"))

expect_true <- function(condition, label) {
  if (!isTRUE(condition)) stop(sprintf("assertion failed: %s", label), call. = FALSE)
}

expect_error <- function(label, expression, pattern) {
  error <- tryCatch({ force(expression); NULL }, error = function(condition) condition)
  if (is.null(error)) stop(sprintf("negative test did not fail: %s", label), call. = FALSE)
  if (!grepl(pattern, conditionMessage(error), perl = TRUE)) {
    stop(sprintf("negative test %s failed for the wrong reason: %s", label, conditionMessage(error)), call. = FALSE)
  }
}

specs <- benchmark_revision_task_specs()
ids <- vapply(specs, `[[`, character(1), "id")
expect_true(
  length(specs) == 27L && !anyDuplicated(ids) &&
    !anyDuplicated(vapply(specs, `[[`, character(1), "function_name")),
  "one direct task registry owns the retained suite"
)
expect_true(
  identical(ids, validate_direct_task_suitability()$task),
  "task suitability has exactly the direct registry coverage"
)
expect_true(
  identical(
    validate_cli_arguments(c("--tasks=vector_sum", "--correctness-only"),
                           "tasks", "correctness-only", "benchmark"),
    c("--tasks=vector_sum", "--correctness-only")
  ),
  "direct CLI accepts declared arguments once"
)
expect_error(
  "direct CLI rejects removed suite selection",
  validate_cli_arguments("--suite=fixtures", "tasks", label = "benchmark"),
  "unknown benchmark argument"
)
expect_error(
  "direct CLI rejects duplicate task selection",
  validate_cli_arguments(c("--tasks=vector_sum", "--tasks=sort"), "tasks", label = "benchmark"),
  "repeated benchmark argument"
)

direct_package_runners <- names(direct_runner_packages(root_dir))
expect_true(
  identical(direct_package_runners, c("zigr", "rcpp", "cpp11", "extendr", "savvy")) &&
    identical(
      direct_runner_artifact_path(root_dir, "r"),
      file.path(root_dir, "src", "r", "run_all.R")
    ) &&
    identical(
      direct_runner_artifact_path(root_dir, "c_call"),
      file.path(root_dir, "src", "c_call", "bench.so")
    ) &&
    identical(direct_runner_environment(root_dir, "r"), .GlobalEnv) &&
    identical(direct_runner_environment(root_dir, "c_call"), .GlobalEnv),
  "one direct owner resolves package runners, artifacts, and non-package environments"
)
expect_error(
  "direct package lookup rejects an undeclared runner",
  direct_runner_package(root_dir, "unknown"),
  "no direct runner package"
)

parity_spec <- list(id = "parity-test", tolerance = FALSE, rng = FALSE)
expect_true(
  identical(direct_assert_result_parity(1L, 1L, parity_spec, "parity-test"), 1L),
  "direct parity helper accepts an identical result"
)
expect_error(
  "direct parity helper rejects a different result",
  direct_assert_result_parity(1L, 2L, parity_spec, "parity-test"),
  "parity-test result values differ"
)
expect_error(
  "runner parity helper rejects an R/C mismatch",
  direct_assert_runner_parity(parity_spec, 1L, 2L, 1L, "runner"),
  "parity-test R/C result values differ"
)
expect_error(
  "fresh-result helper rejects reused output",
  direct_assert_fresh_result(
    list(id = "numeric_transform", tolerance = FALSE), "runner", list(1:2), 1:2, 1:2,
    function(left, right) identical(left, right)
  ),
  "reused its prior allocating result"
)
expect_error(
  "fresh-result helper rejects input aliasing",
  direct_assert_fresh_result(
    list(id = "numeric_transform", tolerance = FALSE), "runner", list(1:2), 1:2, NULL,
    function(left, right) identical(left, right)
  ),
  "returned its input instead of a fresh result"
)
altrep_sum_spec <- specs[[which(ids == "altrep_sum")]]
expect_error(
  "ALTREP phase helper rejects materialized pre-event input",
  direct_assert_altrep_phase(altrep_sum_spec, 1:2, function(value) FALSE, "runner/altrep_sum", "before"),
  "phase input is not an unmaterialized compact ALTREP"
)
expect_error(
  "ALTREP phase helper rejects materialized preserve input",
  direct_assert_altrep_phase(altrep_sum_spec, 1:2, function(value) FALSE, "runner/altrep_sum", "after"),
  "materialized compact ALTREP inside the timed call"
)
expect_error(
  "ALTREP result helper rejects an undeclared ALTREP result",
  direct_assert_altrep_result(
    specs[[which(ids == "altrep_materialize")]], 1:2, function(value) TRUE, "runner"
  ),
  "returned an ALTREP result"
)
expect_error(
  "state reset helper rejects stale external state",
  direct_assert_state_reset(
    list(id = "external_state"), "runner", function(arguments) 699L, 700L,
    function(left, right) FALSE
  ),
  "external state did not reset"
)
expect_error(
  "state reset helper rejects reused output tree",
  direct_assert_state_reset(
    list(id = "outputs"), "runner", function(arguments) list(1:2), list(1:2),
    function(left, right) TRUE
  ),
  "reused an output SEXP"
)

policy <- benchmark_timing_policy()
expect_true(
  identical(policy$policy_version, "direct-batch-v6") &&
    identical(validate_direct_sizing_policy(policy$sizing_policy)$ladder, c(1L, 8L, 64L)) &&
    identical(policy$measurement_samples, 11L),
  "direct timing uses one bounded shared-count policy"
)
clone <- function(value) unserialize(serialize(value, NULL))
direct_runners <- c("r", "c_call", "zigr", "rcpp", "cpp11", "extendr", "savvy")
direct_seed <- benchmark_master_seed() + 17L
metadata <- list(
  schema_version = 4L,
  artifact_layout = "direct-v1",
  run_id = "direct-manifest-test",
  status = "running",
  started_at = run_manifest_timestamp(),
  runners = as.list(direct_runners),
  tasks = as.list(ids),
  master_seed = direct_seed,
  input_recipe_version = "revision-v1",
  input_seeds = setNames(lapply(ids, function(task) task_input_seed(direct_seed, task, "revision-v1")), ids),
  rng_event_seed = task_input_seed(direct_seed, "rng", "direct-timing-v1"),
  source_tree = list(method = "test", digest = "source-digest", file_count = 1L),
  artifacts = setNames(lapply(direct_runners, function(runner) {
    list(runner = runner, relative_path = paste0("artifact/", runner), md5 = paste0("digest-", runner))
  }), direct_runners),
  timing_policy = policy,
  measurement_mode = "timed",
  command = list("Rscript", "run_benchmarks.R")
)
validate_direct_run_manifest(metadata)
incomplete <- clone(metadata)
incomplete$status <- "incomplete"
incomplete$finished_at <- run_manifest_timestamp()
incomplete$status_message <- "sizing failure"
validate_direct_run_manifest(incomplete)
forged_seed <- clone(metadata)
forged_seed$input_seeds[[ids[[1L]]]] <- forged_seed$input_seeds[[ids[[1L]]]] + 1L
expect_error("manifest rejects forged input seed", validate_direct_run_manifest(forged_seed), "input seed differs")
bad_batch <- clone(metadata)
bad_batch$timing_policy$batch_repetitions$vector_sum <- 7L
expect_error("manifest rejects undeclared repetition", validate_direct_run_manifest(bad_batch), "outside the sizing ladder")
missing_diagnostic <- clone(incomplete)
missing_diagnostic$status_message <- NULL
expect_error("incomplete manifest requires diagnostic", validate_direct_run_manifest(missing_diagnostic), "incomplete status message")
forged_memory <- clone(metadata)
forged_memory$memory_task <- "vector_sum"
forged_memory$memory_policy <- direct_memory_policy()
expect_error("manifest rejects ineligible memory task", validate_direct_run_manifest(forged_memory), "not a selected large-output task")
expect_true(
  !file.exists(file.path(root_dir, "lib", "specification.R")) &&
    !file.exists(file.path(root_dir, "export_comparative_metrics.R")) &&
    !file.exists(file.path(root_dir, "export_boundary_metrics.R")) &&
    !file.exists(file.path(root_dir, "promote_run.R")),
  "historical suites, report exporters, and promotion entry point are absent"
)
expect_true(
  identical(
    direct_memory_summary_schema(),
    c("runner", "task", "memory_status", "rss_metric", "loaded_process_rss_kb",
      "initial_process_high_water_rss_kb", "process_high_water_rss_kb",
      "swap_before_kb", "swap_after_kb", "reason")
  ),
  "optional memory artifact has one direct schema"
)

cat("Direct specification gate passed: one suite, strict CLI, bounded policy, and removed historical entry points.\n")
