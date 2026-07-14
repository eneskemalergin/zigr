#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("cannot determine source ledger test location")
script_path <- normalizePath(sub("^--file=", "", script_argument[[1L]]))
root_dir <- normalizePath(file.path(dirname(script_path), ".."))

source(file.path(root_dir, "lib", "task_manifest.R"))
source(file.path(root_dir, "lib", "evidence_schema.R"))
source(file.path(root_dir, "lib", "input_contract.R"))
source(file.path(root_dir, "lib", "r_provenance.R"))
source(file.path(root_dir, "lib", "source_ledger.R"))

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
configs <- list()
for (path in Sys.glob(file.path(root_dir, "runners", "*.json"))) {
  config <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  configs[[config$name]] <- hydrate_runner_config(manifest, config, config$name, evidence)
}

source(file.path(root_dir, "src", "r", "run_all.R"))
r_config <- jsonlite::fromJSON(file.path(root_dir, "runners", "r.json"), simplifyVector = FALSE)
r_map <- r_config$exports
r_references <- r_reference_map(r_config)
r_provenance <- build_run_r_provenance(
  manifest$task, r_map, r_references, manifest,
  evidence$tasks[evidence$tasks$runner == "r", , drop = FALSE]
)
r_provenance_by_task <- named_r_provenance_records(r_provenance, "runner_rows")
r_reference_by_task <- named_r_provenance_records(r_provenance, "reference_rows")
reference_classes <- table(factor(
  vapply(r_reference_by_task, function(record) record$implementation_class, character(1)),
  levels = c("pure_r", "optimized_base_r")
))
expect_true(
  identical(unname(as.integer(reference_classes)), c(28L, 46L)),
  "R correctness uses 28 source-validated pure oracles and 46 explicitly optimized references"
)
expect_true(
  identical(
    unname(vapply(r_pure_reference_tasks(), function(task_id) {
      as.character(r_reference_by_task[[task_id]]$function_name)
    }, character(1))),
    unname(unlist(r_config$reference_overrides, use.names = FALSE))
  ),
  "every new pure-R reference override has its exact source function"
)
expect_equivalent <- function(actual, expected, label) {
  expect_true(
    isTRUE(all.equal(actual, expected, tolerance = sqrt(.Machine$double.eps), check.attributes = TRUE)),
    label
  )
}
oracle_vector <- c(-3.5, 0.25, 7.0, NA_real_)
expect_equivalent(r_oracle_vectorsum(oracle_vector[1:3]), sum(oracle_vector[1:3]), "pure vector-sum oracle")
expect_equivalent(
  r_oracle_broadcast(oracle_vector[1:3], 0.75),
  sum(oracle_vector[1:3] + 0.75),
  "pure broadcast oracle"
)
oracle_matrix <- matrix(as.double(seq_len(12L)), nrow = 3L)
expect_equivalent(r_oracle_transpose(oracle_matrix), t(oracle_matrix), "pure transpose oracle")
expect_equivalent(r_oracle_rowsums(oracle_matrix), rowSums(oracle_matrix), "pure row-sum oracle")
expect_equivalent(
  r_oracle_rowcol_means(oracle_matrix),
  list(rowMeans(oracle_matrix), colSums(oracle_matrix)),
  "pure row and column oracle"
)
oracle_list <- list(c(1.5, 8.0), c(-2.0, 9.0), c(4.25, 10.0))
expect_equivalent(r_bench_list_access(oracle_list), 3.75, "pure nested-list runner")
expect_equivalent(r_oracle_na_mean(c(1.0, NA_real_, NaN, 5.0)), 3.0, "pure missing-value mean oracle")
expect_true(is.nan(r_oracle_na_mean(c(NA_real_, NaN))), "pure missing-value oracle preserves NaN for an empty denominator")
expect_equivalent(r_oracle_altrep_min_max(257L), 256L, "pure ALTREP min-max oracle")
expect_true(identical(r_oracle_altrep_no_na(257L), 0L), "pure ALTREP missingness oracle")
oracle_record <- list(
  id = 42L, count = 7L, level = 3L, flag = TRUE, enabled = FALSE,
  ratio = 1.25, offset = -3.5, scale = 9.75,
  weights = c(0.5, 1.5), indices = c(3L, 1L)
)
expect_true(identical(r_oracle_struct_convert(oracle_record), oracle_record), "pure fixed-record oracle")
expect_true(
  identical(
    sort(unlist(r_provenance_by_task[["22_s4_slot_access"]]$backend_identity_keys)),
    c("methods_package", "r_runtime")
  ),
  "methods runtime has an exact package identity"
)
expect_true(
  identical(
    sort(unlist(r_provenance_by_task[["29_lm_fit"]]$backend_identity_keys)),
    c("fortran_runtime", "r_runtime", "stats_package")
  ),
  "stats Fortran backend has package and runtime identities"
)
records <- verify_source_paths(root_dir, configs, evidence, r_provenance)
keys <- vapply(records, function(record) paste(record$runner, record$task, sep = "\r"), character(1))
names(records) <- keys
record <- function(runner, task) records[[paste(runner, task, sep = "\r")]]

expect_true(length(records) == 83L * 7L, "complete source verification matrix")
expect_true(
  sum(vapply(records, function(row) identical(row$runner, "zigr") && isTRUE(row$product_eligible), logical(1))) == 13L,
  "only thirteen generated zigr boundary rows are product eligible"
)
expect_true(
  sum(vapply(records, function(row) identical(row$runner, "cpp11") && isTRUE(row$product_eligible), logical(1))) == 11L,
  "all configured cpp11 rows use the generated typed path"
)
expect_true(
  all(vapply(records, function(row) {
    !(row$runner %in% c("rcpp", "extendr", "savvy")) || !isTRUE(row$product_eligible)
  }, logical(1))),
  "current Rcpp, extendr, and Savvy rows are not accepted as product evidence"
)

raw_extendr <- unname(sort(vapply(records[vapply(records, function(row) {
  identical(row$runner, "extendr") && identical(row$source_class, "raw_ffi_substitution")
}, logical(1))], function(row) row$task, character(1))))
expect_true(
  identical(raw_extendr, sort(c("26_matmul", "38_struct_convert", "42_external_ptr", "43_rng_stress"))),
  "exact four raw extendr substitutions"
)
expect_true(
  sum(vapply(records, function(row) identical(row$runner, "c_call") && isTRUE(row$accepted_control), logical(1))) == 70L,
  "all configured C rows are registered controls"
)
expect_true(
  all(vapply(records, function(row) {
    !identical(row$runner, "savvy") || !isTRUE(row$executable) || isTRUE(row$configured_symbol_present)
  }, logical(1))),
  "every configured Savvy symbol has a handwritten C wrapper, macro wrapper, or Rust definition"
)

read_source <- function(path) paste(readLines(file.path(root_dir, path), warn = FALSE), collapse = "\n")
source_region <- function(source, start_marker, end_marker) {
  start <- regexpr(start_marker, source, fixed = TRUE)[[1L]]
  if (start < 1L) stop(sprintf("source region start is missing: %s", start_marker))
  tail <- substring(source, start)
  end <- regexpr(end_marker, tail, fixed = TRUE)[[1L]]
  if (end < 1L) stop(sprintf("source region end is missing: %s", end_marker))
  substring(tail, 1L, end - 1L)
}
appears_before <- function(source, first, second) {
  first_position <- regexpr(first, source, fixed = TRUE)[[1L]]
  second_position <- regexpr(second, source, fixed = TRUE)[[1L]]
  first_position > 0L && second_position > 0L && first_position < second_position
}
c_task41 <- read_source("src/c_call/task_41_serialize_roundtrip.c")
rcpp_source <- read_source("src/cpp/main.cpp")
extendr_source <- read_source("src/extendr/rust/src/lib.rs")
savvy_source <- read_source("src/savvy/rust/src/lib.rs")
zigr_task09 <- read_source("src/zig/task_09_longjmp_safety.zig")
zigr_task11 <- read_source("src/zig/task_11_sexp_inspect.zig")
c_task09 <- read_source("src/c_call/task_09_longjmp_safety.c")
c_task11 <- read_source("src/c_call/task_11_sexp_inspect.c")
zigr_task15 <- read_source("src/zig/task_15_dataframe_filter.zig")
c_task15 <- read_source("src/c_call/task_15_dataframe_filter.c")
zigr_task39 <- read_source("src/zig/task_39_r_eval.zig")
c_task39 <- read_source("src/c_call/task_39_r_eval.c")
zigr_task41 <- read_source("src/zig/task_41_serialize_roundtrip.zig")
zigr_eval_source <- read_source("../src/eval.zig")
zigr_serialize_source <- read_source("../src/serialize.zig")
rcpp_task09 <- source_region(rcpp_source, "SEXP rcpp_bench_longjmp_safety", "SEXP rcpp_bench_sexp_create")
rcpp_task11 <- source_region(rcpp_source, "SEXP rcpp_bench_sexp_inspect", "#define BLOCK 32")
rcpp_task15 <- source_region(rcpp_source, "SEXP rcpp_bench_dataframe_filter", "SEXP rcpp_bench_list_access")
rcpp_task39 <- source_region(rcpp_source, "SEXP rcpp_bench_r_eval", "SEXP rcpp_bench_r_tryeval")
extendr_task09 <- source_region(extendr_source, "fn extendr_bench_longjmp_safety", "fn extendr_bench_sexp_create")
extendr_task11 <- source_region(extendr_source, "fn extendr_bench_sexp_inspect", "fn extendr_bench_matrix_transpose")
extendr_task15 <- source_region(extendr_source, "fn extendr_bench_dataframe_filter", "fn extendr_bench_list_access")
extendr_task39 <- source_region(extendr_source, "fn extendr_bench_r_eval", "fn extendr_bench_r_tryeval")
savvy_task09 <- source_region(savvy_source, "fn savvy_bench_longjmp_safety__ffi", "ffi_stub!(savvy_bench_translate_c_cost__ffi")
savvy_task11 <- source_region(savvy_source, "fn savvy_bench_sexp_inspect__ffi", "fn savvy_bench_sexp_inspect(")
savvy_task15 <- source_region(savvy_source, "fn savvy_bench_dataframe_filter__ffi", "fn savvy_bench_dataframe_filter(")
savvy_task39 <- source_region(savvy_source, "fn savvy_bench_r_eval__impl", "fn savvy_bench_r_tryeval__impl")
expect_true(
  appears_before(zigr_task09, "const stop_call", "for (0..repeats)") &&
    appears_before(c_task09, "for (int rep", "SEXP stop_call") &&
    appears_before(rcpp_task09, "for (int rep", "SEXP stop_call") &&
    appears_before(extendr_task09, "for rep in 0..EXTENDR_REPEATS", "let stop_call") &&
    appears_before(savvy_task09, "for rep in 0..SAVVY_REPEATS", "let stop_call"),
  "task 09 source supports the distinct hoisted and per-pass error-call kernel identities"
)
expect_true(
  appears_before(zigr_task11, "var per_elt", "for (0..10000)") &&
    appears_before(c_task11, "for (int iter", "TYPEOF(elts[i])") &&
    appears_before(rcpp_task11, "for (int iter", "TYPEOF(elts[i])") &&
    appears_before(extendr_task11, "for _ in 0..10000", "TYPEOF(s)") &&
    appears_before(savvy_task11, "for _ in 0..10000", "VECTOR_ELT(x, i)"),
  "task 11 source supports the cached, hoisted, and repeated list-element query identities"
)
expect_true(
  grepl("let (_, x_robj) = it.next()", extendr_task15, fixed = TRUE) &&
    grepl("columnIndex(\"x\")", zigr_task15, fixed = TRUE) &&
    all(vapply(c(c_task15, rcpp_task15, savvy_task15), grepl, logical(1), pattern = "STRING_ELT", fixed = TRUE)),
  "task 15 source supports the positional extendr and named-column kernel identities"
)
expect_true(
  grepl("callIn(\"sum\"", zigr_task39, fixed = TRUE) &&
    grepl("return R.Rf_eval(call_expr.get(), envir);", zigr_eval_source, fixed = TRUE) &&
    grepl("Rf_eval(sum_call", c_task39, fixed = TRUE) &&
    grepl("Rf_eval(sum_call", rcpp_task39, fixed = TRUE) &&
    grepl("R_tryEvalSilent(sum_call", extendr_task39, fixed = TRUE) &&
    grepl("R_tryEvalSilent(sum_call", savvy_task39, fixed = TRUE),
  "task 39 source supports direct-eval and silent-try-eval kernel identities"
)
rcpp_task41 <- source_region(rcpp_source, "SEXP rcpp_bench_serialize_roundtrip", "struct RcppBenchmarkState")
extendr_task41 <- source_region(extendr_source, "fn extendr_bench_serialize_roundtrip", "fn extendr_bench_external_ptr")
savvy_task41 <- source_region(
  savvy_source,
  "pub unsafe extern \"C\" fn savvy_bench_serialize_roundtrip__impl",
  "pub unsafe extern \"C\" fn savvy_bench_external_ptr__impl"
)
expect_true(
  grepl("serialize.toVector", zigr_task41, fixed = TRUE) &&
    grepl("serialize.fromVector", zigr_task41, fixed = TRUE) &&
    grepl("R.R_InitOutPStream", zigr_serialize_source, fixed = TRUE) &&
    grepl("R.R_InitInPStream", zigr_serialize_source, fixed = TRUE) &&
    grepl("Rf_eval(ser_call", c_task41, fixed = TRUE) &&
    grepl("Rf_eval(ser_call", rcpp_task41, fixed = TRUE) &&
    grepl("R_tryEvalSilent(ser_call", extendr_task41, fixed = TRUE) &&
    grepl("R_tryEvalSilent(ser_call", savvy_task41, fixed = TRUE),
  "task 41 source supports persistent-stream, direct-eval, and silent-try-eval kernel identities"
)
expect_true(
  grepl("SEXP conn = PROTECT", c_task41, fixed = TRUE) && grepl("UNPROTECT(4);", c_task41, fixed = TRUE) &&
    !grepl("UNPROTECT(1);", c_task41, fixed = TRUE) &&
    grepl("SEXP conn = PROTECT", rcpp_task41, fixed = TRUE) && grepl("UNPROTECT(4);", rcpp_task41, fixed = TRUE) &&
    !grepl("UNPROTECT(1);", rcpp_task41, fixed = TRUE) &&
    grepl("let conn = unsafe { extendr_ffi::Rf_protect", extendr_task41, fixed = TRUE) &&
      grepl("extendr_ffi::Rf_unprotect(4)", extendr_task41, fixed = TRUE) &&
      !grepl("extendr_ffi::Rf_unprotect(1)", extendr_task41, fixed = TRUE) &&
    grepl("let conn = savvy_ffi::Rf_protect", savvy_task41, fixed = TRUE) &&
      grepl("savvy_ffi::Rf_unprotect(4)", savvy_task41, fixed = TRUE) &&
      !grepl("savvy_ffi::Rf_unprotect(1)", savvy_task41, fixed = TRUE),
  "every repaired serialization control retains the complete protection stack through unserialization"
)

c_task42 <- read_source("src/c_call/task_42_external_ptr.c")
zigr_task42 <- read_source("src/zig/task_42_external_ptr.zig")
cpp11_source <- read_source("src/cpp11/src/fixture.cpp")
expect_true(
  all(vapply(c("malloc", "R_SetExternalPtrAddr", "R_RegisterCFinalizerEx", "free(state)", "R_ClearExternalPtr"),
    grepl, logical(1), x = c_task42, fixed = TRUE)) &&
    all(vapply(c("new (std::nothrow) RcppBenchmarkState", "R_SetExternalPtrAddr", "R_RegisterCFinalizerEx", "delete state", "R_ClearExternalPtr"),
      grepl, logical(1), x = rcpp_source, fixed = TRUE)) &&
    all(vapply(c("Box::into_raw", "R_SetExternalPtrAddr", "R_RegisterCFinalizerEx", "Box::from_raw", "R_ClearExternalPtr"),
      grepl, logical(1), x = extendr_source, fixed = TRUE)) &&
    all(vapply(c("Box::into_raw", "R_SetExternalPtrAddr", "R_RegisterCFinalizerEx", "Box::from_raw", "R_ClearExternalPtr"),
      grepl, logical(1), x = savvy_source, fixed = TRUE)) &&
    grepl("externalptr.createTyped", zigr_task42, fixed = TRUE) &&
    grepl("external_pointer<FixtureState>(new FixtureState(value[0]))", cpp11_source, fixed = TRUE),
  "every executable native task-42 control owns its state and declares a finalizer path"
)
expect_true(
  grepl("externalptr.createTyped(FixtureState", read_source("src/zig/main.zig"), fixed = TRUE) &&
    grepl("R_RegisterCFinalizerEx(result, c_fixture_state_finalizer", read_source("src/c_call/register.c"), fixed = TRUE),
  "historical task-72 constructors no longer return borrowed static state"
)

registration_only <- c(
  "extern SEXP missing_control(SEXP);",
  '{"missing_control", (DL_FUNC) &missing_control, 1}'
)
expect_true(
  !source_ledger_c_definition_present(registration_only, "missing_control"),
  "a C declaration and registration do not impersonate a definition"
)
expect_true(
  source_ledger_c_definition_present("SEXP present_control(SEXP value) {", "present_control"),
  "a C function definition is recognized"
)
expect_true(
  !source_ledger_cpp11_wrapper_present(
    '{"_fixture_missing", (DL_FUNC) &_fixture_missing, 1}',
    "_fixture_missing"
  ),
  "cpp11 registration does not impersonate a generated native wrapper"
)
expect_true(
  source_ledger_cpp11_wrapper_present(
    'extern "C" SEXP _fixture_present(SEXP value) {',
    "_fixture_present"
  ),
  "a cpp11 native wrapper definition is recognized"
)
expect_true(
  !source_ledger_r_call_present("registered <- '_fixture_missing'", "_fixture_missing"),
  "a cpp11 symbol mention does not impersonate a generated R wrapper"
)
expect_true(
  source_ledger_r_call_present(".Call(`_fixture_present`, value)", "_fixture_present"),
  "a cpp11 generated R call is recognized"
)
expect_true(
  !source_ledger_savvy_wrapper_present(
    c("extern SEXP savvy_missing__impl(SEXP);", '{"savvy_missing__impl", (DL_FUNC) &savvy_missing__impl, 1}'),
    character(0),
    "savvy_missing__impl"
  ),
  "Savvy registration without a C wrapper, macro invocation, or Rust definition is rejected"
)
expect_true(
  source_ledger_savvy_wrapper_present(
    "SAVVY_WRAP1(present, c_arg__value)",
    character(0),
    "savvy_present__impl"
  ),
  "a Savvy macro wrapper invocation is recognized"
)

r_classes <- table(factor(
  vapply(records[vapply(records, function(row) identical(row$runner, "r"), logical(1))],
    function(row) row$source_class, character(1)),
  levels = c("pure_r", "optimized_base_r", "pure_r_unrepresentable")
))
expect_true(
  identical(unname(as.integer(r_classes)), c(17L, 55L, 11L)),
  "R runner remains mixed rather than uniformly pure R"
)

validate_product_source_record(record("zigr", "52_boundary_scalar_generated"))
validate_product_source_record(record("cpp11", "52_boundary_scalar_generated"))
expect_error(
  "Rcpp product label",
  validate_product_source_record(record("rcpp", "05_fib_recursive")),
  "rejects rcpp/05_fib_recursive.*annotation_present"
)
expect_error(
  "Savvy product label",
  validate_product_source_record(record("savvy", "01_vectorsum")),
  "rejects savvy/01_vectorsum.*annotation_present"
)
expect_error(
  "extendr mixed fixture product label",
  validate_product_source_record(record("extendr", "01_vectorsum")),
  "generated_r_wrapper|forced_symbols"
)
expect_error(
  "direct zigr product label",
  validate_product_source_record(record("zigr", "51_boundary_zero_handwritten")),
  "rejects zigr/51_boundary_zero_handwritten"
)

damaged_zigr <- record("zigr", "52_boundary_scalar_generated")
damaged_zigr$generated_native_wrapper <- FALSE
expect_error(
  "missing zigr generated wrapper",
  validate_product_source_record(damaged_zigr),
  "generated_native_wrapper"
)

damaged_cpp11 <- record("cpp11", "52_boundary_scalar_generated")
damaged_cpp11$annotation_present <- FALSE
expect_error(
  "missing cpp11 annotation",
  validate_product_source_record(damaged_cpp11),
  "annotation_present"
)

drift_root <- tempfile("source-ledger-drift-")
dir.create(drift_root)
on.exit(unlink(drift_root, recursive = TRUE), add = TRUE)
writeLines("original source", file.path(drift_root, "fixture.c"))
recorded_source <- source_ledger_file_identity(drift_root, "fixture.c", "drift fixture")
writeLines("changed source", file.path(drift_root, "fixture.c"))
changed_source <- source_ledger_file_identity(drift_root, "fixture.c", "drift fixture")
expect_error(
  "real source content drift",
  source_ledger_require_digest(changed_source$digest, recorded_source$digest, "runner source"),
  "runner source drift detected"
)

writeLines("first installed file", file.path(drift_root, "DESCRIPTION"))
recorded_tree <- source_ledger_directory_identity(drift_root, "installed source tree")
writeLines("second installed file", file.path(drift_root, "NAMESPACE"))
changed_tree <- source_ledger_directory_identity(drift_root, "installed source tree")
expect_error(
  "real installed tree drift",
  source_ledger_require_digest(changed_tree$digest, recorded_tree$digest, "installed package source"),
  "installed package source drift detected"
)

recorded_dependency <- source_ledger_object_digest(list(name = "libR.so", md5 = "recorded"))
changed_dependency <- source_ledger_object_digest(list(name = "libR.so", md5 = "changed"))
expect_error(
  "real dependency identity drift",
  source_ledger_require_digest(changed_dependency, recorded_dependency, "artifact dependencies"),
  "artifact dependencies drift detected"
)

spec <- load_source_ledger_spec(root_dir)
spec_test_root <- tempfile("source-ledger-spec-")
dir.create(spec_test_root)
on.exit(unlink(spec_test_root, recursive = TRUE), add = TRUE)
write_spec <- function(value) {
  jsonlite::write_json(
    value[names(value) != "path"],
    file.path(spec_test_root, "source_ledger.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
}
bad_spec <- spec
bad_spec$unexpected <- "not allowed"
write_spec(bad_spec)
expect_error(
  "unexpected source ledger field",
  load_source_ledger_spec(spec_test_root),
  "specification fields differ from the schema"
)
bad_spec <- spec
bad_spec$runners$c_call$source_globs <- list("")
write_spec(bad_spec)
expect_error(
  "blank source selector",
  load_source_ledger_spec(spec_test_root),
  "field source_globs must be a unique nonblank list"
)
bad_spec <- spec
bad_spec$runners$savvy$role <- "product_reference"
write_spec(bad_spec)
expect_error(
  "drifted runner identity",
  load_source_ledger_spec(spec_test_root),
  "savvy identity differs from the frozen runner map"
)
bad_spec <- spec
bad_spec$fixture_runners$rcpp$verifier_kind <- "raw_fixture"
write_spec(bad_spec)
expect_error(
  "drifted fixture verifier",
  load_source_ledger_spec(spec_test_root),
  "fixture runner rcpp verifier differs from the frozen map"
)
for (runner in names(spec$runners)) {
  runner_spec <- spec$runners[[runner]]
  expect_true(
    source_ledger_file_identity(root_dir, runner_spec$source_globs, paste(runner, "source"))$file_count > 0L,
    paste(runner, "source selection")
  )
  expect_true(
    source_ledger_file_identity(root_dir, runner_spec$build_files, paste(runner, "build"))$file_count > 0L,
    paste(runner, "build selection")
  )
  expect_true(
    source_ledger_file_identity(root_dir, runner_spec$generated_glue$paths, paste(runner, "glue"))$file_count > 0L,
    paste(runner, "generated glue selection")
  )
}
for (runner in names(spec$fixture_runners)) {
  runner_spec <- spec$fixture_runners[[runner]]
  expect_true(
    source_ledger_file_identity(
      root_dir, runner_spec$source_globs, paste(runner, "fixture source")
    )$file_count > 0L,
    paste(runner, "fixture source selection")
  )
  expect_true(
    source_ledger_file_identity(
      root_dir, runner_spec$build_files, paste(runner, "fixture build")
    )$file_count > 0L,
    paste(runner, "fixture build selection")
  )
  expect_true(
    source_ledger_file_identity(
      root_dir, runner_spec$generated_glue$paths, paste(runner, "fixture glue")
    )$file_count > 0L,
    paste(runner, "fixture generated glue selection")
  )
}
build_settings <- list(
  optimization = "ReleaseFast",
  target = "native",
  cpu_features = "default",
  checked_sexp = FALSE,
  cache_dir = normalizePath(file.path(root_dir, ".zig-cache"), mustWork = FALSE),
  global_cache_dir = normalizePath(file.path(root_dir, ".zig-global-cache"), mustWork = FALSE),
  requested_rebuild = FALSE
)
for (runner in names(spec$runners)) {
  invocation <- source_ledger_build_invocation(root_dir, runner, build_settings)
  expect_true(nzchar(invocation$executable), paste(runner, "resolved build executable"))
  expect_true(dir.exists(invocation$working_directory), paste(runner, "resolved build working directory"))
  expect_true(!isTRUE(invocation$executed_in_run), paste(runner, "prebuilt invocation is not called rebuilt"))
}
zigr_invocation <- source_ledger_build_invocation(root_dir, "zigr", build_settings)
expect_true(
  identical(
    as.character(unlist(zigr_invocation$arguments, use.names = FALSE)),
    c(
      "build", "-Doptimize=ReleaseFast",
      paste0("-Dr-include=", source_ledger_effective_build_path("R_INCLUDE", R.home("include"))),
      paste0("-Dr-lib=", source_ledger_effective_build_path("R_LIB", R.home("lib"))),
      "--cache-dir", build_settings$cache_dir,
      "--global-cache-dir", build_settings$global_cache_dir
    )
  ),
  "resolved zigr invocation matches build_all.sh defaults"
)
expect_error(
  "one missing required path",
  source_ledger_file_identity(
    root_dir,
    c("source_ledger.json", "missing-required-source-ledger-path"),
    "required path test"
  ),
  "patterns select no regular files: missing-required-source-ledger-path"
)
expect_true(isTRUE(spec$runners$cpp11$generated_glue$retained_output), "cpp11 generated output is retained")
expect_true(!isTRUE(spec$runners$extendr$generated_glue$retained_output), "extendr macro output is not retained")
expect_true(!isTRUE(spec$runners$zigr$generated_glue$retained_output), "zigr comptime output is not retained")
expect_true(
  identical(spec$runners$savvy$generated_glue$kind, "handwritten_savvy_shaped_glue_not_generated"),
  "Savvy glue is not mislabeled as generated"
)

extendr_profile <- parse_cargo_release_profile(file.path(root_dir, spec$runners$extendr$cargo_manifest))
savvy_profile <- parse_cargo_release_profile(file.path(root_dir, spec$runners$savvy$cargo_manifest))
expect_true(identical(extendr_profile$lto, "true"), "extendr release LTO")
expect_true(identical(extendr_profile$panic, "unwind (Cargo release default)"), "extendr release panic strategy")
expect_true(identical(savvy_profile$lto, "true"), "Savvy release LTO")
expect_true(identical(savvy_profile$panic, "abort"), "Savvy release panic strategy")
for (runner in c("extendr", "savvy")) {
  locked <- parse_cargo_lock(file.path(root_dir, spec$runners[[runner]]$cargo_lock))
  expect_true(all(vapply(locked, function(package) nzchar(package$name) && nzchar(package$version), logical(1))),
              paste(runner, "locked package names and versions"))
  registry <- locked[vapply(locked, function(package) startsWith(package$source, "registry+"), logical(1))]
  expect_true(length(registry) > 0L && all(vapply(registry, function(package) nzchar(package$checksum), logical(1))),
              paste(runner, "registry package checksums"))
}

cat("Source ledger passed: 581 task cells, seven fixture ledgers, exact paths, mixed R provenance, and required drift rejection.\n")
