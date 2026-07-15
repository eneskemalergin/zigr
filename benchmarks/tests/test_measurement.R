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

sample_file <- tempfile("wall-time-samples-", fileext = ".csv")
write.csv(data.frame(iteration = 1:5, wall_ms = c(0, 0.1, 0.2, 0.3, 0.4)), sample_file, row.names = FALSE)
sample_values <- read_wall_time_samples(sample_file, expected_n = 5L)
sample_interval <- median_confidence_interval(sample_values, 0.95)
expect_true(
  identical(sample_values, c(0, 0.1, 0.2, 0.3, 0.4)) &&
    sample_interval[["low"]] <= median(sample_values) && sample_interval[["high"]] >= median(sample_values),
  "shared report sample reader preserves valid zero-duration samples and interval coverage"
)
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
legacy_metadata <- list(schema_version = 2L)
dir.create(file.path(layout_root, "r"))
write.csv(
  data.frame(task = "b", iteration = 1:3, wall_ms = 7:9),
  file.path(layout_root, "r", "task_b.csv"),
  row.names = FALSE
)
expect_true(
  identical(read_run_wall_time_samples(layout_root, legacy_metadata, "task", "r", "b", expected_n = 3L), 7:9),
  "schema-two artifact layout remains readable"
)
unlink(layout_root, recursive = TRUE)
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
  data.frame(runner = "r", task = "fixture_task", phase = c("cold", "timed"), wall_ms = 1, run_id = "sealed"),
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
  full_matrix = TRUE,
  measurement_mode = "timed",
  environment = list(identity = "test"),
  correctness_stage = list(status = "complete")
)
sealed_metadata$completion_artifacts <- capture_run_completion_artifacts(sealed_run, sealed_metadata)
sealed_metadata$completion_contract <- capture_run_completion_contract(sealed_metadata)
validate_run_completion_contract(sealed_metadata)
validate_run_completion_artifacts(sealed_run, sealed_metadata)

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
drifted_completion$timing_policy$max_iterations <- drifted_completion$timing_policy$max_iterations + 1L
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
  identical(canonical_receipt$schema_version, "benchmark-promotion-v2") &&
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

cat("Measurement trust passed: deterministic fuzz, phase isolation, sequence behavior, run seals, and fixture drift checks.\n")
