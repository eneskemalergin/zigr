#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("cannot determine harness trust test location")
script_path <- normalizePath(sub("^--file=", "", script_argument[[1L]]))
root_dir <- normalizePath(file.path(dirname(script_path), ".."))

source(file.path(root_dir, "lib", "task_manifest.R"))
source(file.path(root_dir, "lib", "input_contract.R"))
source(file.path(root_dir, "lib", "r_provenance.R"))
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
  list(
    schema_version = r_provenance_schema_version(),
    task = task,
    function_name = task,
    implementation_class = "pure_r",
    source_digest = source_digest,
    ast_allowlist_id = r_pure_contract_policy(task)$id,
    forbidden_call_result = if (length(calls) == 0L) "pass" else paste0("declared:", paste(sort(calls), collapse = "+")),
    compiled_backend = backend
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

validate_forced_registration(FALSE, "registered fixture")
expect_error(
  "wrong registration mode",
  validate_forced_registration(TRUE, "dynamic fixture"),
  "dynamic symbol lookup is enabled"
)

cat("Harness trust passed: deterministic inputs, phase isolation, R provenance, dispositions, registration, and identity drift.\n")
