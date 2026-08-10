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

direct_runners <- direct_runner_names(root_dir)
direct_specs <- direct_runner_registry(root_dir)
comparison_scope <- direct_comparison_scope()
expect_true(
  identical(
    names(comparison_scope),
    c("product_runners", "baseline_roles", "control_runners", "direct_runners",
      "pure_r_runner", "optimized_base_r_runner")
  ) &&
    identical(comparison_scope$product_runners,
              c("zigr", "rcpp", "cpp11", "extendr", "savvy")) &&
    identical(comparison_scope$baseline_roles, c("pure_r", "optimized_base_r")) &&
    identical(comparison_scope$control_runners, "c_call") &&
    identical(comparison_scope$direct_runners,
              c("r", "c_call", "zigr", "rcpp", "cpp11", "extendr", "savvy")) &&
    is.na(comparison_scope$pure_r_runner) &&
    identical(comparison_scope$optimized_base_r_runner, "r"),
  "comparison scope freezes products, baseline roles, and controls"
)
expect_true(
  identical(direct_runners, comparison_scope$direct_runners) &&
    !anyDuplicated(direct_runners) &&
    identical(names(direct_specs), direct_runners) &&
    identical(unname(vapply(direct_specs, `[[`, character(1), "name")), direct_runners) &&
    all(vapply(direct_specs, function(spec) {
      is.character(spec$artifact_path) && length(spec$artifact_path) == 1L && nzchar(spec$artifact_path)
    }, logical(1))) &&
    all(vapply(direct_specs, function(spec) {
      identical(names(spec), c("name", "label", "invocation", "artifact_path", "package"))
    }, logical(1))) &&
    identical(
      direct_runners[vapply(direct_specs, function(spec) !is.null(spec$package), logical(1))],
      c("zigr", "rcpp", "cpp11", "extendr", "savvy")
    ) &&
    identical(
      vapply(direct_specs, `[[`, character(1), "invocation"),
      c(r = "r_function", c_call = "registered_native", zigr = "package_function",
        rcpp = "package_function", cpp11 = "package_function", extendr = "package_function",
        savvy = "package_function")
    ) &&
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
original_comparison_scope <- direct_comparison_scope
forged_comparison_scope <- original_comparison_scope()
forged_comparison_scope$direct_runners <- c("r", "c_call")
direct_comparison_scope <- function() forged_comparison_scope
expect_error(
  "runtime runner lookup rejects registry drift",
  direct_runner_names(root_dir),
  "live runner registry differs from frozen comparison scope"
)
direct_comparison_scope <- original_comparison_scope
live_runner_sources <- c(
  file.path(root_dir, "run_benchmarks.R"),
  file.path(root_dir, "benchmark_worker.R"),
  file.path(root_dir, "check_coverage.R"),
  file.path(root_dir, "lib", "product_fixtures.R")
)
expect_true(
  !any(vapply(live_runner_sources, function(path) {
    any(grepl('c\\("r", "c_call", "zigr"', readLines(path, warn = FALSE), fixed = FALSE))
  }, logical(1))),
  "live runner callers do not define a second hard-coded registry"
)
expect_error(
  "direct runner lookup rejects an undeclared runner",
  direct_runner_package(root_dir, "unknown"),
  "unknown direct runner"
)
expect_error(
  "direct runner spec rejects an undeclared runner",
  direct_runner_spec(root_dir, "unknown"),
  "unknown direct runner"
)

manifest_source <- readLines(file.path(root_dir, "lib", "run_manifest.R"), warn = FALSE)
entry_source <- readLines(file.path(root_dir, "run_benchmarks.R"), warn = FALSE)
expect_true(
  any(grepl("^source_tree_identity[[:space:]]*<-[[:space:]]*function", manifest_source)) &&
    any(grepl("^validate_source_tree_identity[[:space:]]*<-[[:space:]]*function", manifest_source)) &&
    !file.exists(file.path(root_dir, "lib", "provenance.R")) &&
    !any(grepl("lib/provenance\\.R", entry_source, fixed = FALSE)),
  "direct manifest owner keeps source identity after legacy provenance deletion"
)
current_runtime_sources <- c(
  entry_source,
  readLines(file.path(root_dir, "benchmark_worker.R"), warn = FALSE),
  manifest_source,
  readLines(file.path(root_dir, "lib", "measurement.R"), warn = FALSE),
  readLines(file.path(root_dir, "lib", "product_fixtures.R"), warn = FALSE)
)
historical_receipt_pattern <- paste(
  c("CANONICAL_RUN", "benchmark-promotion-v1", "promotion_manifest", "source_ledger",
    "report_manifest", "product_metrics", "capability_matrix", "promote_run\\.R"),
  collapse = "|"
)
expect_true(
  !any(grepl(historical_receipt_pattern, current_runtime_sources, fixed = FALSE)),
  "current runtime owners do not consume historical receipt or promotion metadata"
)
source_identity <- source_tree_identity(root_dir)
expect_true(
  identical(validate_source_tree_identity(root_dir, source_identity)$digest, source_identity$digest),
  "direct source identity validates the current worktree"
)
forged_source_identity <- source_identity
forged_source_identity$digest <- paste0(source_identity$digest, "-changed")
expect_error(
  "direct source identity rejects a changed recorded digest",
  validate_source_tree_identity(root_dir, forged_source_identity),
  "source tree identity field digest differs"
)
forged_source_identity <- source_identity
forged_source_identity$method <- "changed-method"
expect_error(
  "direct source identity rejects a changed recorded method",
  validate_source_tree_identity(root_dir, forged_source_identity),
  "source tree identity field method differs"
)
forged_source_identity <- source_identity
forged_source_identity$file_count <- source_identity$file_count + 1L
expect_error(
  "direct source identity rejects a changed recorded file count",
  validate_source_tree_identity(root_dir, forged_source_identity),
  "source tree identity field file_count differs"
)

product_source <- readLines(file.path(root_dir, "lib", "product_fixtures.R"), warn = FALSE)
sort_oracle_lengths <- vapply(
  exact_task_arguments("sort"), function(arguments) length(arguments[[1L]]), integer(1)
)
expect_true(
  identical(sort_oracle_lengths, c(0L, 7L, 65535L, 65535L, 65536L)),
  "sort correctness oracles cover compact and large radix boundaries"
)
legacy_definition_pattern <- paste0(
  "^(", paste(c("fixture_", "verify_", "build_fixture_", "run_fixture_",
                 "validate_fixture_", "cpp11_fixture_"), collapse = "|"),
  ")[_A-Za-z0-9]*[[:space:]]*<-[[:space:]]*function"
)
expect_true(
  !any(grepl(legacy_definition_pattern, product_source)) &&
    any(grepl("^direct_assert_fresh_tree[[:space:]]*<-[[:space:]]*function", product_source)),
  "direct owner has no F01 through F12 verifier definitions"
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
    identical(policy$policy_version, "direct-batch-v12") &&
    identical(validate_direct_sizing_policy(policy$sizing_policy)$ladder, c(1L, 8L, 64L, 512L, 4096L, 8192L)) &&
    identical(validate_direct_comparison_policy(policy$comparison_policy), list(
      confidence_level = 0.90,
      required_order_pairs = 4L,
      equivalence_margin_pct = 2
    )) &&
    identical(policy$measurement_samples, 11L) &&
    identical(policy$sizing_policy$target_batch_ms, 1) &&
    identical(policy$sizing_policy$maximum_batch_ms, 500) &&
    grepl("each large-output measurement sample", policy$gc_policy, fixed = TRUE),
  "direct timing uses one bounded shared-count policy and explicit large-output GC boundaries"
)
clone <- function(value) unserialize(serialize(value, NULL))
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
forged_comparison_policy <- clone(metadata)
forged_comparison_policy$timing_policy$comparison_policy$equivalence_margin_pct <- 20
expect_error(
  "manifest rejects a widened comparison equivalence margin",
  validate_direct_run_manifest(forged_comparison_policy),
  "comparison policy has invalid limits"
)
forged_sizing_policy <- clone(metadata)
forged_sizing_policy$timing_policy$sizing_policy$maximum_batch_ms <- 5000
expect_error(
  "manifest rejects a widened sizing cap",
  validate_direct_run_manifest(forged_sizing_policy),
  "sizing policy has invalid batch bounds"
)
forged_sizing_policy <- clone(metadata)
forged_sizing_policy$timing_policy$sizing_policy$ladder[[1L]] <- 1.5
expect_error(
  "manifest rejects a fractional sizing ladder",
  validate_direct_run_manifest(forged_sizing_policy),
  "direct sizing policy is invalid"
)
missing_required <- clone(metadata)
missing_required$source_tree <- NULL
expect_error(
  "manifest rejects missing source identity",
  validate_direct_run_manifest(missing_required),
  "run manifest is missing: source_tree"
)
incomplete <- clone(metadata)
incomplete$status <- "incomplete"
incomplete$finished_at <- run_manifest_timestamp()
incomplete$status_message <- "sizing failure"
validate_direct_run_manifest(incomplete)
missing_output <- clone(metadata)
missing_output$status <- "correctness_complete"
missing_output$measurement_mode <- "correctness_only"
missing_output$finished_at <- run_manifest_timestamp()
missing_output$outputs <- list(correctness = list(
  relative_path = "correctness.csv", md5 = "missing-output-digest"
))
validate_direct_run_manifest(missing_output)
expect_error(
  "output validator rejects a missing correctness artifact",
  validate_direct_run_outputs(tempdir(), missing_output),
  "run output is missing"
)
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

failure_root <- file.path(tempdir(), "zigr-direct-failure-policy")
unlink(failure_root, recursive = TRUE)
wrapper_dir <- file.path(tempdir(), "zigr-direct-failure-rscript")
unlink(wrapper_dir, recursive = TRUE)
count_file <- file.path(tempdir(), "zigr-direct-failure-count")
unlink(count_file)
dir.create(wrapper_dir, recursive = TRUE, showWarnings = FALSE)
wrapper <- file.path(wrapper_dir, "Rscript")
real_rscript <- unname(Sys.which("Rscript")[[1L]])
writeLines(c(
  "#!/bin/sh",
  "for argument in \"$@\"; do",
  "  case \"$argument\" in",
  "    --mode=timing) echo timing >> \"$ZIGR_TIMING_FAILURE_COUNT\"; exit 42 ;;",
  "  esac",
  "done",
  sprintf("exec %s \"$@\"", shQuote(real_rscript))
), wrapper)
Sys.chmod(wrapper, mode = "0755")
failure_status <- system2(
  "Rscript",
  c(
    file.path(root_dir, "run_benchmarks.R"),
    "--runners=r", "--tasks=vector_sum",
    paste0("--run-dir=", failure_root)
  ),
  env = c(
    paste0("PATH=", wrapper_dir, .Platform$path.sep, Sys.getenv("PATH")),
    paste0("ZIGR_TIMING_FAILURE_COUNT=", count_file)
  ),
  stdout = FALSE,
  stderr = FALSE
)
expect_true(failure_status != 0L, "forced worker failure exits nonzero")
failure_manifest <- read_run_manifest(failure_root)
expect_true(
  identical(failure_manifest$status, "incomplete") &&
    grepl("timing worker failed for r with exit code 42", failure_manifest$status_message, fixed = TRUE) &&
    file.exists(file.path(failure_root, "correctness.csv")) &&
    identical(length(readLines(count_file, warn = FALSE)), 1L) &&
    !file.exists(file.path(failure_root, "timing_samples.csv")) &&
    !file.exists(file.path(failure_root, "timing_summary.csv")),
  "worker failure preserves an incomplete diagnostic manifest without timing outputs"
)
unlink(c(failure_root, wrapper_dir, count_file), recursive = TRUE)
expect_true(
  !file.exists(file.path(root_dir, "lib", "specification.R")) &&
    !file.exists(file.path(root_dir, "export_comparative_metrics.R")) &&
    !file.exists(file.path(root_dir, "export_boundary_metrics.R")) &&
    !file.exists(file.path(root_dir, "promote_run.R")) &&
    !file.exists(file.path(root_dir, "task_manifest.csv")) &&
    !file.exists(file.path(root_dir, "evidence_manifest.json")) &&
    !file.exists(file.path(root_dir, "runners.json")) &&
    !file.exists(file.path(root_dir, "source_ledger.json")) &&
    !file.exists(file.path(root_dir, "src", "zig", "main.zig")) &&
    !file.exists(file.path(root_dir, "src", "zig", "tasks.zig")) &&
    !file.exists(file.path(root_dir, "src", "cpp", "main.cpp")) &&
    !file.exists(file.path(root_dir, "src", "extendr", "Makefile")) &&
    !file.exists(file.path(root_dir, "src", "extendr", "entrypoint.c")) &&
    !file.exists(file.path(root_dir, "src", "extendr", "rust", "Cargo.toml")) &&
    !file.exists(file.path(root_dir, "src", "extendr", "rust", "Cargo.lock")) &&
    !file.exists(file.path(root_dir, "src", "extendr", "rust", "src", "lib.rs")) &&
    !file.exists(file.path(root_dir, "src", "savvy", "Makefile")) &&
    !file.exists(file.path(root_dir, "src", "savvy", "api.h")) &&
    !file.exists(file.path(root_dir, "src", "savvy", "init.c")) &&
    !file.exists(file.path(root_dir, "src", "savvy", "rust", "Cargo.toml")) &&
    !file.exists(file.path(root_dir, "src", "savvy", "rust", "Cargo.lock")) &&
    !file.exists(file.path(root_dir, "src", "savvy", "rust", "src", "lib.rs")),
  "historical suites, legacy metadata, standalone runners, report exporters, and promotion entry point are absent"
)
build_all_lines <- readLines(file.path(root_dir, "build_all.sh"), warn = FALSE)
build_all_lines <- build_all_lines[seq.int(match("ZIG_BIN=${ZIG:-}", build_all_lines), length(build_all_lines))]
build_sources <- c(file.path(root_dir, "build.zig"), build_all_lines)
expect_true(
  !any(vapply(build_sources, function(path) {
    lines <- if (length(path) == 1L && file.exists(path)) readLines(path, warn = FALSE) else path
    any(grepl("zigr_benchmarks|src/zig/(main|tasks)\\.zig|src/cpp/main\\.cpp|src/extendr/(Makefile|entrypoint\\.c|rust)|src/savvy/(Makefile|init\\.c|api\\.h|rust)", lines))
  }, logical(1))),
  "benchmark build only names retained sources and artifacts"
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
