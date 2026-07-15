#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("cannot determine harness trust test location")
script_path <- normalizePath(sub("^--file=", "", script_argument[[1L]]))
root_dir <- normalizePath(file.path(dirname(script_path), ".."))

source(file.path(root_dir, "lib", "task_manifest.R"))
source(file.path(root_dir, "lib", "input_contract.R"))
source(file.path(root_dir, "lib", "r_provenance.R"))
source(file.path(root_dir, "lib", "source_ledger.R"))
source(file.path(root_dir, "lib", "run_manifest.R"))
source(file.path(root_dir, "lib", "environment_manifest.R"))
source(file.path(root_dir, "lib", "harness.R"))

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

fixture_version <- "trust-test-v1"
small_task <- list(id = "fixture_task", args = function() list(runif(8L), c("ascii", enc2utf8("cafe\u0301"))))
record <- build_input_recipe_record(small_task, 12345L, fixture_version, "contract-v1")
expect_true(
  identical(record$task_seed, task_input_seed(12345L, small_task$id, fixture_version)),
  "stable per-task seed"
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
  validate_task_input_recipe(small_task, bad_seed, 12345L),
  "task seed mismatch"
)

bad_master_seed <- unserialize(serialize(record, NULL))
bad_master_seed$master_seed <- bad_master_seed$master_seed + 1L
expect_error(
  "deliberate master seed mismatch",
  validate_task_input_recipe(small_task, bad_master_seed, 12345L),
  "master seed mismatch"
)

bad_fingerprint <- unserialize(serialize(record, NULL))
bad_fingerprint$fingerprint <- "00000000000000000000000000000000"
expect_error(
  "deliberate input fingerprint mismatch",
  validate_task_input_recipe(small_task, bad_fingerprint, 12345L),
  "canonical input fingerprint mismatch"
)

first_phase <- materialize_task_input(small_task, record$task_seed)$arguments
second_phase <- materialize_task_input(small_task, record$task_seed)$arguments
first_phase[[1L]][[1L]] <- -1
expect_true(second_phase[[1L]][[1L]] != -1, "phase inputs do not share mutable state")

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
  runner_config_digest = file_identity_digest(root_dir, "runners/r.json", "R runner config"),
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
dir.create(file.path(sealed_run, "r"), recursive = TRUE)
dir.create(file.path(sealed_run, "fixtures", "r"), recursive = TRUE)
dir.create(file.path(sealed_run, "correctness", "tasks"), recursive = TRUE)
dir.create(file.path(sealed_run, "correctness", "fixtures"), recursive = TRUE)
writeLines("input", file.path(sealed_run, "input_manifest.json"))
sealed_task_summary <- data.frame(task = "fixture_task", status = "PASS", stringsAsFactors = FALSE)
sealed_fixture_summary <- data.frame(row_id = "F01", status = "PASS", stringsAsFactors = FALSE)
write.csv(sealed_task_summary, file.path(sealed_run, "r_summary.csv"), row.names = FALSE)
write.csv(sealed_fixture_summary, file.path(sealed_run, "fixture_r_summary.csv"), row.names = FALSE)
write.csv(data.frame(wall_ms = 1), file.path(sealed_run, "r", "task_fixture_task.csv"), row.names = FALSE)
write.csv(
  data.frame(runner = "r", task = "fixture_task", wall_ms = 1, run_id = "sealed"),
  file.path(sealed_run, "r", "cold_start.csv"),
  row.names = FALSE
)
write.csv(data.frame(wall_ms = 1), file.path(sealed_run, "fixtures", "r", "F01.csv"), row.names = FALSE)
writeLines("task correctness", file.path(sealed_run, "correctness", "tasks", "r.csv"))
writeLines("fixture correctness", file.path(sealed_run, "correctness", "fixtures", "r.csv"))
sealed_metadata <- list(
  schema_version = 2L,
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
  task_inputs = list(list(task = "fixture_task")),
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
write.csv(edited_summary, file.path(sealed_run, "r_summary.csv"), row.names = FALSE)
expect_error(
  "edited completed summary",
  validate_run_completion_artifacts(sealed_run, sealed_metadata),
  "completion artifacts differ"
)
write.csv(sealed_task_summary, file.path(sealed_run, "r_summary.csv"), row.names = FALSE)

unlink(file.path(sealed_run, "fixtures", "r", "F01.csv"))
expect_error(
  "missing completed fixture sample",
  validate_run_completion_artifacts(sealed_run, sealed_metadata),
  "completion artifacts are missing"
)
write.csv(data.frame(wall_ms = 1), file.path(sealed_run, "fixtures", "r", "F01.csv"), row.names = FALSE)

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

cat("Harness trust passed: deterministic inputs, phase isolation, R provenance, dispositions, registration, and identity drift.\n")
