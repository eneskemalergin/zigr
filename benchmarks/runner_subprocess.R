#!/usr/bin/env Rscript

source("lib/harness.R")
library(methods)
library(jsonlite)

cli <- commandArgs(trailingOnly = TRUE)
runner_name <- NA
task_filter <- NULL
check_only <- FALSE
validation_only <- FALSE
results_dir_arg <- NULL
prepare_inputs_arg <- NULL
input_manifest_arg <- NULL
expected_input_manifest_digest <- NULL
master_seed_arg <- NULL
for (a in cli) {
  if (grepl("^--runner=", a)) runner_name <- sub("^--runner=", "", a)
  if (grepl("^--tasks=", a))  task_filter <- as.integer(strsplit(sub("^--tasks=", "", a), ",")[[1]])
  if (a == "--check-only") check_only <- TRUE
  if (a == "--validation-only") validation_only <- TRUE
  if (grepl("^--results-dir=", a)) results_dir_arg <- sub("^--results-dir=", "", a)
  if (grepl("^--prepare-inputs=", a)) prepare_inputs_arg <- sub("^--prepare-inputs=", "", a)
  if (grepl("^--input-manifest=", a)) input_manifest_arg <- sub("^--input-manifest=", "", a)
  if (grepl("^--expected-input-manifest-digest=", a)) {
    expected_input_manifest_digest <- sub("^--expected-input-manifest-digest=", "", a)
  }
  if (grepl("^--master-seed=", a)) master_seed_arg <- sub("^--master-seed=", "", a)
}
if (is.na(runner_name) && is.null(prepare_inputs_arg)) stop("--runner= required")
if (is.na(runner_name)) runner_name <- "r"

cfg_dir <- "runners"
cfg_path <- file.path(cfg_dir, paste0(runner_name, ".json"))
if (!file.exists(cfg_path)) stop(sprintf("runner config not found: %s", cfg_path))
cfg <- fromJSON(cfg_path, simplifyVector = FALSE)

root_dir <- normalizePath(file.path(cfg_dir, ".."))
source(file.path(root_dir, "lib", "run_manifest.R"))
source(file.path(root_dir, "lib", "input_contract.R"))
source(file.path(root_dir, "lib", "r_provenance.R"))
source(file.path(root_dir, "lib", "source_ledger.R"))
source(file.path(root_dir, "lib", "environment_manifest.R"))
if (!check_only && is.null(prepare_inputs_arg) && is.null(results_dir_arg)) {
  stop("--results-dir= is required for benchmark execution")
}
if (!is.null(prepare_inputs_arg)) results_dir_arg <- NULL
results_dir <- normalizePath(if (is.null(results_dir_arg)) file.path(root_dir, "results") else results_dir_arg, mustWork = FALSE)
run_id <- NA_character_
timing_policy <- benchmark_timing_policy()
run_metadata <- NULL
if (!check_only && is.null(prepare_inputs_arg)) {
  run_metadata <- read_run_manifest(results_dir)
  run_id <- as.character(run_metadata$run_id)
  if (!is.null(run_metadata$timing_policy)) timing_policy <- run_metadata$timing_policy
  validate_timing_policy(timing_policy)
  if (!(runner_name %in% run_manifest_values(run_metadata$runners))) {
    stop(sprintf("runner %s is not declared by run manifest %s", runner_name, run_manifest_path(results_dir)))
  }
  staging_results_dir <- file.path(results_dir, ".staging")
  dir.create(file.path(staging_results_dir, runner_name), recursive = TRUE, showWarnings = FALSE)
  unlink(file.path(staging_results_dir, runner_name, "errors.csv"))
}

# The parent input-recipe process cannot construct registered native state.
# The fingerprint normalizes this sentinel and real external pointers to the
# same declared recipe identity.
method_receiver <- function() structure("runner_registered_fixture_state", class = "benchmark_external_state_recipe")

string_input <- function() {
  rep(c("zigr", "boundary", NA_character_, ""), length.out = 32768L)
}

raw_input <- function() {
  as.raw((seq_len(262144L) - 1L) %% 251L)
}

complex_input <- function() {
  complex(real = as.double(seq_len(32768L)), imaginary = as.double(seq_len(32768L)) * 0.5)
}

all_tasks <- list(
  list(id = "01_vectorsum", name = "Vector Sum (1e7)",
       args = function() list(runif(1e7))),
  list(id = "02_elem_ops", name = "Element-wise ops (1e6)",
       args = function() list(runif(1e6, -5, 5))),
  list(id = "03_memcpy_bandwidth", name = "Memory Bandwidth (1e7)",
       args = function() list(rep.int(as.double(1:8), 131072L))),
  list(id = "04_sort", name = "Sort (1e6)",
       args = function() list(runif(1e6))),
  list(id = "05_fib_recursive", name = "Fibonacci (n=25)",
       args = function() list(25L)),
  list(id = "06_broadcast", name = "Vector + Scalar (1e7)",
       args = function() list(runif(1e7), 3.14)),
  list(id = "07a_protect_shallow", name = "PROTECT Shallow (10 items)",
       args = function() list(runif(10))),
  list(id = "07b_protect_scaling", name = "PROTECT Scaling (100k items)",
       args = function() list(runif(100000))),
  list(id = "08_type_dispatch", name = "Type Dispatch (3 types x 2048)",
       args = function() list(list(runif(2048), 1:2048, rep("x", 2048)))),
  list(id = "09_longjmp_safety", name = "Longjmp Safety (4x512x64)",
       args = function() list(runif(64))),
  list(id = "10_sexp_create", name = "SEXP Create (100k)",
       args = function() list(100000L)),
  list(id = "11_sexp_inspect", name = "SEXP Inspect (10k)",
       args = function() list(list(runif(1), 1L, "x", list(), NULL))),
  list(id = "12_matrix_transpose", name = "Matrix Transpose (500x500)",
       args = function() list(matrix(runif(500*500), 500, 500))),
  list(id = "13_matrix_rowsums", name = "Row Sums (1000x500)",
       args = function() list(matrix(runif(500000), 1000, 500))),
  list(id = "14_matrix_rowcol_means", name = "Row/Col Means (500x1000)",
       args = function() list(matrix(runif(500000), 500, 1000))),
  list(id = "15_dataframe_filter", name = "Data Frame Filter (100k rows)",
       args = function() list(data.frame(x = rnorm(1e5), y = abs(rnorm(1e5)) + 0.1,
                                         grp = factor(sample(letters[1:10], 1e5, T)),
                                         stringsAsFactors = FALSE))),
  list(id = "16_list_access", name = "List Access (1000 elements)",
       args = function() list(replicate(1000, runif(100), simplify = FALSE))),
  list(id = "17_string_concat", name = "String Concatenation (10k x 24 chars)",
       args = function() list(replicate(10000, paste0(sample(letters, 24, T), collapse = "")))),
  list(id = "18_string_nchar", name = "String Nchar (10k, 5% NA)",
       args = function() { x <- replicate(10000, paste0(sample(letters, 50, TRUE), collapse = "")); x[sample(10000, 500)] <- NA_character_; list(x) }),
  list(id = "19_string_encoding", name = "String Encoding (10k ASCII)",
       args = function() list(replicate(10000, paste0(sample(letters, 24, TRUE), collapse = "")))),
  list(id = "20_factor_ops", name = "Factor Ops (10k / 100 levels)",
       args = function() list(sample(letters[1:100], 10000, replace = TRUE))),
  list(id = "21_attrib_ops", name = "Attribute Ops (1M vector)",
       args = function() list(runif(1e6))),
  list(id = "22_s4_slot_access", name = "S4 Slot Access",
       args = function() list(1.0)),
  list(id = "23_na_propagation", name = "NA Propagation (1e6, 5% NA)",
       args = function() { x <- runif(1e6); x[sample(1e6, 5e4)] <- NA; list(x) }),
  list(id = "24_long_vector_idx", name = "Long Vector Index (2^31+1 ALTREP)",
       args = function() list(1:1e7)),
  list(id = "25_l1_arithmetic", name = "L1 Arithmetic (4000 x 2500 passes)",
       args = function() list(runif(4000))),
  list(id = "26_matmul", name = "Matmul (256x256)",
       args = function() { n <- 256L; list(matrix(runif(n * n), n, n),
                                           matrix(runif(n * n), n, n)) }),
  list(id = "27_crossprod", name = "Cross-product (500x50)",
       args = function() list(matrix(runif(25000), 500, 50))),
  list(id = "28_cholesky", name = "Cholesky (200x200)",
       args = function() { n <- 200L; A <- crossprod(matrix(runif(n * n), n, n)) + diag(n); list(A) }),
  list(id = "29_lm_fit", name = "Linear Model (n=5000, p=20)",
       args = function() { n <- 5000L; p <- 20L; X <- matrix(runif(n * p), n, p); y <- runif(n); list(X, y) }),
  list(id = "30_altrep_create", name = "ALTREP Create (n=1e6)",
       args = function() list(1000000L)),
  list(id = "31_altrep_materialize", name = "ALTREP Materialize (1e6)",
       args = function() list(1000000L)),
  list(id = "32_altrep_elt_walk", name = "ALTREP Elt Walk (1e6)",
       args = function() list(1000000L)),
  list(id = "33_altrep_region_read", name = "ALTREP Region Read (1e6)",
       args = function() list(1000000L)),
  list(id = "34_altrep_sum_via_R", name = "ALTREP Sum via R (1e6)",
       args = function() list(1000000L)),
  list(id = "35_altrep_sum_native", name = "ALTREP Sum Native (1e6)",
       args = function() list(1000000L)),
  list(id = "36_altrep_min_max", name = "ALTREP Min/Max (1e6)",
       args = function() list(1000000L)),
  list(id = "37_altrep_no_na_query", name = "ALTREP No-NA Query (1e6)",
       args = function() list(1000000L)),
  list(id = "38_struct_convert", name = "Struct Convert (10 fields)",
       args = function() list(list(id = 42L, count = 7L, level = 3L, flag = TRUE,
                                   enabled = FALSE, ratio = 1.25, offset = -3.5, scale = 9.75,
                                   weights = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5),
                                   indices = c(3L, 1L, 4L, 1L, 5L, 9L, 2L, 6L)))),
  list(id = "39_r_eval", name = "R Eval (sum(x) + mean(x))",
       args = function() list(runif(1e6))),
  list(id = "40_r_tryeval", name = "R TryEval (stop error catch)",
       args = function() list(runif(1))),
  list(id = "41_serialize_roundtrip", name = "Serialize Roundtrip (1e6)",
       args = function() list(runif(1e6))),
  list(id = "42_external_ptr", name = "External Pointer",
       args = function() list(1L)),
  list(id = "43_rng_stress", name = "RNG Stress (1M norm_rand)",
       args = function() list(1000000L)),
  list(id = "48_weakref_lifecycle", name = "Weak-reference Create/Access (4096)",
       args = function() list(4096L)),
  list(id = "49_owned_altrep_create", name = "Owned ALTREP Callbacks (n=1e6)",
       args = function() list(1000000L)),
  list(id = "50_boundary_zero_generated", name = "Generated zero-argument boundary",
       args = function() list()),
  list(id = "51_boundary_zero_handwritten", name = "Handwritten zero-argument boundary",
       args = function() list()),
  list(id = "52_boundary_scalar_generated", name = "Generated scalar boundary",
       args = function() list(3.5)),
  list(id = "53_boundary_scalar_handwritten", name = "Handwritten scalar boundary",
       args = function() list(3.5)),
  list(id = "54_boundary_optional_null_generated", name = "Generated optional NULL boundary",
       args = function() list(NULL)),
  list(id = "55_boundary_optional_null_handwritten", name = "Handwritten optional NULL boundary",
       args = function() list(NULL)),
  list(id = "56_boundary_optional_typed_na_generated", name = "Generated optional typed-NA boundary",
       args = function() list(NA_real_)),
  list(id = "57_boundary_optional_typed_na_handwritten", name = "Handwritten optional typed-NA boundary",
       args = function() list(NA_real_)),
  # as.double(seq_len()) is compact ALTREP in current R; arithmetic makes the
  # ordinary REALSXP required by the materialized numeric pair.
  list(id = "58_boundary_numeric_small_generated", name = "Generated small numeric boundary",
       args = function() list(as.double(seq_len(16L)) + 0.0)),
  list(id = "59_boundary_numeric_small_handwritten", name = "Handwritten small numeric boundary",
       args = function() list(as.double(seq_len(16L)) + 0.0)),
  list(id = "60_boundary_numeric_large_generated", name = "Generated large numeric boundary",
       args = function() list(as.double(seq_len(100000L)) + 0.0)),
  list(id = "61_boundary_numeric_large_handwritten", name = "Handwritten large numeric boundary",
       args = function() list(as.double(seq_len(100000L)) + 0.0)),
  list(id = "62_boundary_altrep_integer_generated", name = "Generated integer ALTREP copy",
       args = function() list(seq_len(100000L))),
  list(id = "63_boundary_altrep_integer_handwritten", name = "Handwritten integer ALTREP region stream",
       args = function() list(seq_len(100000L))),
  list(id = "64_boundary_string_view_generated", name = "Generated string-view boundary",
       args = function() list(rep(c("zigr", "boundary", NA_character_), length.out = 32L))),
  list(id = "65_boundary_string_view_handwritten", name = "Handwritten string-view boundary",
       args = function() list(rep(c("zigr", "boundary", NA_character_), length.out = 32L))),
  list(id = "66_boundary_raw_generated", name = "Generated raw boundary",
       args = function() list(as.raw(seq_len(64L) %% 251L))),
  list(id = "67_boundary_raw_handwritten", name = "Handwritten raw boundary",
       args = function() list(as.raw(seq_len(64L) %% 251L))),
  list(id = "68_boundary_complex_generated", name = "Generated complex boundary",
       args = function() list(complex(real = as.double(seq_len(32L)), imaginary = as.double(seq_len(32L)) * 0.5))),
  list(id = "69_boundary_complex_handwritten", name = "Handwritten complex boundary",
       args = function() list(complex(real = as.double(seq_len(32L)), imaginary = as.double(seq_len(32L)) * 0.5))),
  list(id = "70_boundary_schema_generated", name = "Generated fixed-schema boundary",
       args = function() list(list(id = 42L, count = 7L, ratio = 1.25, enabled = TRUE))),
  list(id = "71_boundary_schema_handwritten", name = "Handwritten fixed-schema boundary",
       args = function() list(list(id = 42L, count = 7L, ratio = 1.25, enabled = TRUE))),
  list(id = "72_boundary_external_method_generated", name = "Generated external-pointer method",
       args = function() list(method_receiver(), 7L)),
  list(id = "73_boundary_external_method_handwritten", name = "Handwritten external-pointer method",
       args = function() list(method_receiver(), 7L)),
  list(id = "74_boundary_external_generated", name = "Generated .External boundary",
       call_type = ".External",
       args = function() list(4.0)),
  list(id = "75_boundary_external_handwritten", name = "Handwritten .External boundary",
       call_type = ".External",
       args = function() list(4.0)),
  list(id = "76_string_view_one", name = "One-pass string view (32k)",
       args = function() list(string_input())),
  list(id = "77_string_cache_build", name = "String metadata cache build (32k)",
       args = function() list(string_input())),
  list(id = "78_string_cache_one", name = "One-pass cached strings (32k)",
       args = function() list(string_input())),
  list(id = "79_string_headers_one", name = "One-pass string headers (32k)",
       args = function() list(string_input())),
  list(id = "80_string_view_repeated", name = "Four-pass string view (32k)",
       args = function() list(string_input())),
  list(id = "81_string_cache_repeated", name = "Four-pass cached strings (32k)",
       args = function() list(string_input())),
  list(id = "82_string_headers_repeated", name = "Four-pass string headers (32k)",
       args = function() list(string_input())),
  list(id = "83_raw_view", name = "Borrowed raw view (256k)",
       args = function() list(raw_input())),
  list(id = "84_raw_copy", name = "Copied raw input (256k)",
       args = function() list(raw_input())),
  list(id = "85_complex_view", name = "Complex input view (32k)",
       args = function() list(complex_input())),
  list(id = "86_complex_return", name = "Complex return copy (32k)",
       args = function() list(complex_input()))
)

source(file.path(root_dir, "lib", "task_manifest.R"))
source(file.path(root_dir, "lib", "evidence_schema.R"))
manifest <- load_task_manifest(root_dir)
evidence <- load_evidence_manifest(root_dir, manifest)
cfg <- hydrate_runner_config(manifest, cfg, runner_name, evidence)
validate_task_specs(manifest, all_tasks)
all_tasks <- order_task_specs(manifest, all_tasks)

if (!is.null(task_filter)) {
  task_numbers <- vapply(
    all_tasks,
    function(task) as.integer(sub("([0-9]+).*", "\\1", task$id)),
    integer(1))
  all_tasks <- all_tasks[task_numbers %in% task_filter]
}

if (!is.null(prepare_inputs_arg)) {
  master_seed <- if (is.null(master_seed_arg)) benchmark_master_seed() else input_scalar_integer(master_seed_arg, "master seed")
  write_input_recipe_manifest(
    normalizePath(prepare_inputs_arg, mustWork = FALSE),
    all_tasks,
    manifest,
    evidence,
    master_seed
  )
  cat(sprintf("Canonical input recipes written to %s\n", normalizePath(prepare_inputs_arg, mustWork = FALSE)))
  quit(save = "no", status = 0, runLast = FALSE)
}

input_recipes <- NULL
master_seed <- NULL
runner_environment <- NULL
if (!check_only) {
  if (is.null(input_manifest_arg) || is.null(expected_input_manifest_digest) || is.null(master_seed_arg)) {
    stop("benchmark execution requires --input-manifest, --expected-input-manifest-digest, and --master-seed")
  }
  declared_input_path <- normalizePath(file.path(results_dir, as.character(run_metadata$input_manifest$relative_path)), mustWork = FALSE)
  supplied_input_path <- normalizePath(input_manifest_arg, mustWork = FALSE)
  if (!identical(declared_input_path, supplied_input_path)) stop("canonical input manifest location differs from the run manifest")
  if (!identical(as.character(run_metadata$input_manifest$digest), expected_input_manifest_digest)) {
    stop("expected canonical input manifest digest differs from the run manifest")
  }
  validate_input_manifest_digest(supplied_input_path, expected_input_manifest_digest)
  master_seed <- input_scalar_integer(master_seed_arg, "master seed")
  if (!identical(master_seed, input_scalar_integer(run_metadata$master_seed, "run manifest master seed"))) {
    stop("master seed differs from the run manifest")
  }
  input_recipes <- read_input_recipe_manifest(supplied_input_path)
  if (!identical(master_seed, input_scalar_integer(input_recipes$master_seed, "canonical input master seed"))) {
    stop("master seed differs from the canonical input manifest")
  }
  selected_ids <- vapply(all_tasks, function(task) task$id, character(1))
  if (!identical(sort(names(input_recipes$tasks)), sort(selected_ids))) {
    stop("canonical input task set differs from the runner task set")
  }
  for (task in all_tasks) validate_task_input_recipe(task, input_recipes$tasks[[task$id]], master_seed)
  runner_environment <- runner_environment_record(run_metadata$environment, runner_name)
  validate_runner_artifact_identity(root_dir, runner_environment)
  validate_tool_source_ledger(root_dir, run_metadata$environment$tool_source_ledger, runner_name)
}

cat(sprintf("Runner: %s (%s)\n", runner_name, cfg$label))

`%||%` <- function(x, y) if (is.null(x)) y else x
call_type <- cfg$call_type %||% ".Call"

timing_summary_fields <- function(bm = NULL) {
  if (is.null(bm)) {
    return(list(
      warmup_iterations = as.integer(timing_policy$warmup_iterations),
      block_size = as.integer(timing_policy$block_size),
      max_iterations = as.integer(timing_policy$max_iterations),
      convergence_window_blocks = as.integer(timing_policy$convergence_window_blocks),
      convergence_cv_threshold_pct = as.numeric(timing_policy$convergence_cv_threshold_pct),
      convergence_cv_pct = NA_real_,
      stopping_condition = "not_measured",
      converged = NA,
      timer_noise_floor_ms = as.numeric(timing_policy$timer_noise_floor_ms),
      timer_noise_status = "not_measured",
      rss_metric = as.character(timing_policy$rss_metric),
      gc_policy = as.character(timing_policy$gc_policy)
    ))
  }
  list(
    warmup_iterations = as.integer(bm$warmup_iterations),
    block_size = as.integer(bm$block_size),
    max_iterations = as.integer(bm$max_iterations),
    convergence_window_blocks = as.integer(bm$convergence_window_blocks),
    convergence_cv_threshold_pct = as.numeric(bm$convergence_cv_threshold_pct),
    convergence_cv_pct = as.numeric(bm$convergence_cv_pct),
    stopping_condition = as.character(bm$stopping_condition),
    converged = isTRUE(bm$converged),
    timer_noise_floor_ms = as.numeric(bm$timer_noise_floor_ms),
    timer_noise_status = as.character(bm$timer_noise_status),
    rss_metric = as.character(bm$rss_metric),
    gc_policy = as.character(timing_policy$gc_policy)
  )
}

if (call_type != "r") {
  loaded_dlls <- list()
  so_path <- file.path(root_dir, cfg$so_path)
  if (!file.exists(so_path)) stop(sprintf("library not found: %s", so_path))
  main_dll <- tryCatch(dyn.load(so_path),
                       error = function(e) stop(sprintf("load error: %s", conditionMessage(e))))
  loaded_dlls[[main_dll[["name"]]]] <- main_dll

  extra_so_paths <- cfg$extra_so_paths %||% list()
  for (extra_so in extra_so_paths) {
    extra_path <- file.path(root_dir, extra_so)
    if (!file.exists(extra_path)) stop(sprintf("library not found: %s", extra_path))
    extra_dll <- tryCatch(dyn.load(extra_path),
                          error = function(e) stop(sprintf("load error: %s", conditionMessage(e))))
    loaded_dlls[[extra_dll[["name"]]]] <- extra_dll
  }
}

source(file.path(root_dir, "src/r/run_all.R"))
r_cfg_path <- file.path(root_dir, "runners", "r.json")
r_ref <- fromJSON(r_cfg_path, simplifyVector = FALSE)$exports
validate_r_reference_map(manifest, r_ref)
r_evidence_rows <- evidence$tasks[evidence$tasks$runner == "r", , drop = FALSE]
selected_task_ids <- vapply(all_tasks, function(task) task$id, character(1))
live_r_provenance <- build_run_r_provenance(selected_task_ids, r_ref, manifest, r_evidence_rows)
r_runner_provenance <- named_r_provenance_records(live_r_provenance, "runner_rows")
r_reference_provenance <- named_r_provenance_records(live_r_provenance, "reference_rows")
if (!check_only) {
  expected_runner_provenance <- named_r_provenance_records(run_metadata$r_provenance, "runner_rows")
  expected_reference_provenance <- named_r_provenance_records(run_metadata$r_provenance, "reference_rows")
  if (!identical(sort(names(expected_runner_provenance)), sort(names(r_runner_provenance))) ||
      !identical(sort(names(expected_reference_provenance)), sort(names(r_reference_provenance)))) {
    stop("R provenance task sets differ from the run manifest")
  }
  for (task_id in names(r_runner_provenance)) {
    compare_r_provenance_records(expected_runner_provenance[[task_id]], r_runner_provenance[[task_id]], task_id)
  }
  for (task_id in names(r_reference_provenance)) {
    compare_r_provenance_records(
      expected_reference_provenance[[task_id]],
      r_reference_provenance[[task_id]],
      paste0(task_id, " reference")
    )
  }
}

capture_result <- function(fn) {
  error <- NA_character_
  value <- tryCatch(fn(), error = function(e) {
    error <<- conditionMessage(e)
    NULL
  })
  list(ok = is.na(error), value = value, error = error)
}

resolve_registered_exports <- function(export_names, cfg) {
  package_overrides <- cfg$package_overrides %||% list()
  package_name <- cfg$package_name %||% ""
  if (!nzchar(package_name)) stop(sprintf("runner %s enables registered_symbols but has no package_name", runner_name))
  resolved <- export_names
  validated_packages <- character(0)
  for (tid in names(export_names)) {
    package_for_task <- package_overrides[[tid]] %||% package_name
    dll <- loaded_dlls[[package_for_task]]
    if (is.null(dll)) stop(sprintf(
      "registered symbol lookup has no loaded DLL named %s for runner %s task %s",
      package_for_task, runner_name, tid
    ))
    if (!(package_for_task %in% validated_packages)) {
      validate_forced_registration(dll[["dynamicLookup"]], sprintf("%s package %s", runner_name, package_for_task))
      validated_packages <- c(validated_packages, package_for_task)
    }
    info <- tryCatch(
      getNativeSymbolInfo(export_names[[tid]], PACKAGE = dll, withRegistrationInfo = TRUE),
      error = function(e) stop(sprintf(
        "registered symbol lookup failed for runner %s task %s (%s in package %s): %s",
        runner_name, tid, export_names[[tid]], package_for_task, conditionMessage(e)
      ))
    )
    resolved[[tid]] <- info$address
  }
  resolved
}

validate_registration_fixture <- function(cfg) {
  fixture <- cfg$registration_fixture
  if (is.null(fixture)) return(invisible(NULL))
  recognized <- c(
    "scalar", "integer", "logical", "scalar_after_allocation",
    "optional", "optional_integer", "optional_logical",
    "vector", "new", "method", "error", "external", "wrong_tag", "cleared", "misaligned"
  )
  recognized <- c(recognized, "missing_metadata")
  unknown <- setdiff(names(fixture), recognized)
  if (length(unknown) > 0L) stop(sprintf(
    "runner %s registration_fixture has unknown checks: %s", runner_name, paste(unknown, collapse = ", ")
  ))
  blank <- names(fixture)[vapply(fixture, function(value) length(value) != 1L || !nzchar(value), logical(1))]
  if (length(blank) > 0L) stop(sprintf(
    "runner %s registration_fixture has blank symbols: %s", runner_name, paste(blank, collapse = ", ")
  ))
  has <- function(key) !is.null(fixture[[key]])
  if (xor(has("new"), has("method"))) stop(sprintf(
    "runner %s registration_fixture must declare new and method together", runner_name
  ))
  receiver_checks <- c("wrong_tag", "missing_metadata", "cleared", "misaligned")
  orphaned <- receiver_checks[vapply(receiver_checks, has, logical(1)) & !has("method")]
  if (length(orphaned) > 0L) stop(sprintf(
    "runner %s registration_fixture pointer checks require a method: %s",
    runner_name,
    paste(orphaned, collapse = ", ")
  ))
  package_name <- cfg$package_name
  dll <- loaded_dlls[[package_name]]
  if (is.null(dll)) stop(sprintf(
    "registration fixture has no loaded DLL named %s for runner %s", package_name, runner_name
  ))
  validate_forced_registration(dll[["dynamicLookup"]], runner_name)
  symbol <- function(key) {
    info <- tryCatch(
      getNativeSymbolInfo(fixture[[key]], PACKAGE = dll, withRegistrationInfo = TRUE),
      error = function(e) stop(sprintf(
        "registration fixture lookup failed for runner %s (%s in package %s): %s",
        runner_name, fixture[[key]], package_name, conditionMessage(e)
      ))
    )
    info$address
  }
  symbols <- setNames(lapply(names(fixture), symbol), names(fixture))

  expect_fixture_error <- function(result, label, expected_message = NULL) {
    if (result$ok) stop(sprintf("registration fixture accepted %s for %s", label, runner_name))
    if (!is.null(expected_message) && !grepl(expected_message, result$error, fixed = TRUE)) stop(sprintf(
      "registration fixture error message changed for %s (%s): %s",
      runner_name, label, result$error %||% "no message"
    ))
  }

  if (has("scalar")) {
    scalar <- capture_result(function() do.call(.Call, list(symbols$scalar, 3.5)))
    if (!scalar$ok || !isTRUE(all.equal(scalar$value, 3.5))) stop(sprintf(
      "registration fixture scalar check failed for %s: %s", runner_name, scalar$error %||% "wrong result"
    ))
    # Preflight only: repeated independent calls must not add a timed inner loop.
    for (value in c(-3.5, 0, 7.25)) {
      repeated_scalar <- capture_result(function() do.call(.Call, list(symbols$scalar, value)))
      if (!repeated_scalar$ok || !isTRUE(all.equal(repeated_scalar$value, value))) stop(sprintf(
        "registration fixture repeated scalar check failed for %s", runner_name
      ))
    }
    scalar_nan <- capture_result(function() do.call(.Call, list(symbols$scalar, NaN)))
    if (!scalar_nan$ok || !isTRUE(is.nan(scalar_nan$value))) stop(sprintf(
      "registration fixture scalar NaN check failed for %s", runner_name
    ))
    expect_fixture_error(
      capture_result(function() do.call(.Call, list(symbols$scalar, 1L))),
      "a wrong scalar type",
      if (identical(runner_name, "zigr")) "expected REALSXP" else NULL
    )
    expect_fixture_error(
      capture_result(function() do.call(.Call, list(symbols$scalar, c(1.0, 2.0)))),
      "an invalid scalar length",
      if (identical(runner_name, "zigr")) "scalar inputs must have length one" else NULL
    )
    expect_fixture_error(
      capture_result(function() do.call(.Call, list(symbols$scalar, numeric()))),
      "an empty scalar",
      if (identical(runner_name, "zigr")) "expected non-empty vector" else NULL
    )
    expect_fixture_error(
      capture_result(function() do.call(.Call, list(symbols$scalar, NA_real_))),
      "a required scalar NA",
      if (identical(runner_name, "zigr")) "scalar inputs must not be NA" else NULL
    )
    expect_fixture_error(
      capture_result(function() do.call(.Call, list(symbols$scalar, 3.5, 4.5))),
      "an invalid .Call arity"
    )
    expect_fixture_error(
      capture_result(function() getNativeSymbolInfo(
        fixture$scalar,
        PACKAGE = dll,
        type = ".External",
        withRegistrationInfo = TRUE
      )),
      "a .Call routine requested as .External"
    )
    expect_fixture_error(
      capture_result(function() .Call(fixture$scalar, 3.5, PACKAGE = package_name)),
      "character lookup when forced symbols are enabled"
    )
  }

  if (has("scalar_after_allocation")) {
    result <- capture_result(function() do.call(.Call, list(symbols$scalar_after_allocation, 3.5)))
    if (!result$ok || !isTRUE(all.equal(result$value, 3.5))) stop(sprintf(
      "registration fixture scalar-after-allocation check failed for %s", runner_name
    ))
  }

  if (has("integer")) {
    integer <- capture_result(function() do.call(.Call, list(symbols$integer, -7L)))
    if (!integer$ok || !identical(integer$value, -7L)) stop(sprintf(
      "registration fixture integer scalar check failed for %s", runner_name
    ))
    expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$integer, 3.5))), "a wrong integer scalar type")
    expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$integer, c(1L, 2L)))), "an overlong integer scalar")
    expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$integer, integer()))), "an empty integer scalar")
    expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$integer, NA_integer_))), "a required integer NA")
  }

  if (has("logical")) {
    logical_false <- capture_result(function() do.call(.Call, list(symbols$logical, FALSE)))
    logical_true <- capture_result(function() do.call(.Call, list(symbols$logical, TRUE)))
    if (!logical_false$ok || !identical(logical_false$value, FALSE) ||
        !logical_true$ok || !identical(logical_true$value, TRUE)) stop(sprintf(
      "registration fixture logical scalar check failed for %s", runner_name
    ))
    expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$logical, 1L))), "a wrong logical scalar type")
    expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$logical, c(TRUE, FALSE)))), "an overlong logical scalar")
    expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$logical, logical()))), "an empty logical scalar")
    expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$logical, NA))), "a required logical NA")
  }

  if (has("optional")) {
    optional_null <- capture_result(function() do.call(.Call, list(symbols$optional, NULL)))
    optional_na <- capture_result(function() do.call(.Call, list(symbols$optional, NA_real_)))
    optional_nan <- capture_result(function() do.call(.Call, list(symbols$optional, NaN)))
    if (!optional_null$ok || !identical(optional_null$value, 0L) ||
        !optional_na$ok || !identical(optional_na$value, 0L) ||
        !optional_nan$ok || !identical(optional_nan$value, 1L)) stop(sprintf(
      "registration fixture optional scalar check failed for %s", runner_name
    ))
    expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$optional, c(NA_real_, 1.0)))), "an invalid optional scalar length")
    expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$optional, numeric()))), "an empty optional scalar")
  }

  if (has("optional_integer")) {
    optional_null <- capture_result(function() do.call(.Call, list(symbols$optional_integer, NULL)))
    optional_na <- capture_result(function() do.call(.Call, list(symbols$optional_integer, NA_integer_)))
    optional_value <- capture_result(function() do.call(.Call, list(symbols$optional_integer, 7L)))
    if (!optional_null$ok || !identical(optional_null$value, 0L) ||
        !optional_na$ok || !identical(optional_na$value, 0L) ||
        !optional_value$ok || !identical(optional_value$value, 1L)) stop(sprintf(
      "registration fixture optional integer check failed for %s", runner_name
    ))
    expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$optional_integer, c(NA_integer_, 1L)))), "an overlong optional integer scalar")
    expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$optional_integer, NA_real_))), "a wrong optional integer scalar type")
  }

  if (has("optional_logical")) {
    optional_null <- capture_result(function() do.call(.Call, list(symbols$optional_logical, NULL)))
    optional_na <- capture_result(function() do.call(.Call, list(symbols$optional_logical, NA)))
    optional_value <- capture_result(function() do.call(.Call, list(symbols$optional_logical, FALSE)))
    if (!optional_null$ok || !identical(optional_null$value, 0L) ||
        !optional_na$ok || !identical(optional_na$value, 0L) ||
        !optional_value$ok || !identical(optional_value$value, 1L)) stop(sprintf(
      "registration fixture optional logical check failed for %s", runner_name
    ))
    expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$optional_logical, c(NA, TRUE)))), "an overlong optional logical scalar")
    expect_fixture_error(capture_result(function() do.call(.Call, list(symbols$optional_logical, NA_integer_))), "a wrong optional logical scalar type")
  }

  invalid_result_contract <- validate_result_contract(1L, "real_scalar")
  if (invalid_result_contract$ok) stop(sprintf("result-contract preflight accepted an invalid shape for %s", runner_name))

  if (has("vector")) {
    vector <- capture_result(function() do.call(.Call, list(symbols$vector, c(1.0, 2.0, 3.0))))
    if (!vector$ok || !isTRUE(all.equal(vector$value, 6.0))) stop(sprintf(
      "registration fixture vector check failed for %s: %s", runner_name, vector$error %||% "wrong result"
    ))
  }

  if (has("new")) {
    receiver <- capture_result(function() do.call(.Call, list(symbols$new)))
    if (!receiver$ok || !identical(typeof(receiver$value), "externalptr")) stop(sprintf(
      "registration fixture constructor check failed for %s: %s", runner_name, receiver$error %||% "wrong result"
    ))
    method <- capture_result(function() do.call(.Call, list(symbols$method, receiver$value, 7L)))
    if (!method$ok || !isTRUE(all.equal(method$value, 7L))) stop(sprintf(
      "registration fixture method check failed for %s: %s", runner_name, method$error %||% "wrong result"
    ))
    expect_fixture_error(
      capture_result(function() do.call(.Call, list(symbols$method, 1L, 7L))),
      "an invalid method receiver"
    )
  }

  if (has("error")) {
    expected_error <- capture_result(function() do.call(.Call, list(symbols$error, 1.0)))
    if (expected_error$ok || !grepl("fixture error", expected_error$error, fixed = TRUE)) stop(sprintf(
      "registration fixture error check failed for %s", runner_name
    ))
  }

  if (has("external")) {
    external <- capture_result(function() do.call(.External, list(symbols$external, 4.0)))
    if (!external$ok || !isTRUE(all.equal(external$value, 5.0))) stop(sprintf(
      "registration fixture .External check failed for %s: %s", runner_name, external$error %||% "wrong result"
    ))
    expect_fixture_error(
      capture_result(function() do.call(.Call, list(symbols$external, 4.0))),
      "an external routine called through .Call"
    )
    expect_fixture_error(
      capture_result(function() do.call(.External, list(symbols$external))),
      "an invalid .External arity"
    )
  }

  if (has("scalar") && has("external")) {
    expect_fixture_error(
      capture_result(function() do.call(.External, list(symbols$scalar, 3.5))),
      "a .Call routine called through .External"
    )
  }

  pointer_case <- function(key, label, expected_message = NULL) {
    pointer <- capture_result(function() do.call(.Call, list(symbols[[key]])))
    if (!pointer$ok || !identical(typeof(pointer$value), "externalptr")) stop(sprintf(
      "registration fixture %s constructor check failed for %s", label, runner_name
    ))
    expect_fixture_error(
      capture_result(function() do.call(.Call, list(symbols$method, pointer$value, 7L))),
      sprintf("%s external pointer", label),
      expected_message
    )
  }
  if (has("wrong_tag")) pointer_case("wrong_tag", "wrong-tag")
  if (has("missing_metadata")) pointer_case(
    "missing_metadata",
    "missing-metadata",
    if (identical(runner_name, "zigr")) "external pointer is missing typed metadata" else NULL
  )
  if (has("cleared")) pointer_case("cleared", "cleared")
  if (has("misaligned")) pointer_case("misaligned", "misaligned")

  missing_symbol <- capture_result(function() getNativeSymbolInfo(
    paste0(package_name, "_fixture_missing"),
    PACKAGE = dll,
    withRegistrationInfo = TRUE
  ))
  if (missing_symbol$ok) stop(sprintf("registration fixture exposed an unregistered symbol for %s", runner_name))
  cat(sprintf("Registration preflight passed for %s\n", runner_name))
  invisible(NULL)
}

registered_export_names <- NULL
if (isTRUE(cfg$registered_symbols)) {
  registered_export_names <- cfg$exports
  cfg$exports <- resolve_registered_exports(registered_export_names, cfg)
  validate_registration_fixture(cfg)
}

method_receiver <- function() {
  if (call_type == "r") return(NULL)
  fixture <- cfg$registration_fixture
  package_name <- cfg$package_name %||% ""
  if (is.null(fixture) || is.null(fixture$new) || !nzchar(package_name)) return(NULL)
  dll <- loaded_dlls[[package_name]]
  if (is.null(dll)) stop(sprintf("runner %s has no loaded package DLL for the method receiver", runner_name))
  new_address <- getNativeSymbolInfo(fixture$new, PACKAGE = dll, withRegistrationInfo = TRUE)$address
  do.call(.Call, list(new_address))
}

if (check_only) {
  validate_task_arguments(manifest, all_tasks)
  cat(sprintf("Coverage preflight passed for %s (%d task specs)\n", runner_name, length(all_tasks)))
  quit(save = "no", status = 0, runLast = FALSE)
}

same_attributes <- function(expected, actual) {
  expected_names <- names(expected)
  actual_names <- names(actual)
  if (!identical(sort(expected_names), sort(actual_names))) return(FALSE)
  if (length(expected_names) == 0L) return(TRUE)
  all(vapply(expected_names, function(name) identical(expected[[name]], actual[[name]]), logical(1)))
}

compare_correctness <- function(expected, actual, path = "result") {
  result <- function(ok, message = "") list(ok = isTRUE(ok), message = message)
  if (is.null(expected) && is.null(actual)) return(result(TRUE))
  if (is.null(expected) || is.null(actual)) return(result(FALSE, sprintf("%s is NULL on one side", path)))
  if (!identical(typeof(expected), typeof(actual))) {
    return(result(FALSE, sprintf("%s type differs", path)))
  }
  if (!same_attributes(attributes(expected), attributes(actual))) {
    return(result(FALSE, sprintf("%s attributes differ", path)))
  }
  if (length(expected) != length(actual)) return(result(FALSE, sprintf("%s length differs", path)))

  if (is.numeric(expected) || is.complex(expected)) {
    if (!identical(is.na(expected), is.na(actual))) return(result(FALSE, sprintf("%s NA positions differ", path)))
    if (!identical(is.nan(expected), is.nan(actual))) return(result(FALSE, sprintf("%s NA and NaN kinds differ", path)))
    if (!isTRUE(all.equal(
      expected,
      actual,
      tolerance = sqrt(.Machine$double.eps),
      check.attributes = FALSE
    ))) {
      return(result(FALSE, sprintf("%s values differ", path)))
    }
    return(result(TRUE))
  }
  if (is.character(expected)) {
    if (!identical(is.na(expected), is.na(actual))) return(result(FALSE, sprintf("%s NA positions differ", path)))
    if (!identical(Encoding(expected), Encoding(actual))) return(result(FALSE, sprintf("%s encodings differ", path)))
    if (!identical(expected[!is.na(expected)], actual[!is.na(actual)])) {
      return(result(FALSE, sprintf("%s string values differ", path)))
    }
    return(result(TRUE))
  }
  if (is.list(expected)) {
    for (index in seq_along(expected)) {
      nested <- compare_correctness(expected[[index]], actual[[index]], sprintf("%s[[%d]]", path, index))
      if (!isTRUE(nested$ok)) return(nested)
    }
    return(result(TRUE))
  }
  if (identical(expected, actual)) return(result(TRUE))
  result(FALSE, sprintf("%s values differ", path))
}

result_preview <- function(value) {
  gsub(",", " ", substr(paste(deparse(value), collapse = ""), 1, 120))
}

results_list <- list()
exports <- cfg$exports
n_pass <- 0; n_fail <- 0; n_na <- 0

for (task in all_tasks) {
  tid <- task$id
  manifest_row <- match(tid, manifest$task)
  if (is.na(manifest_row)) stop(sprintf("task %s is absent from the manifest", tid))
  correctness_policy <- manifest$correctness_policy[[manifest_row]]
  expected_return <- manifest$expected_return[[manifest_row]]
  correctness_status <- if (call_type == "r") "REFERENCE" else "NOT_VALIDATED"
  correctness_message <- ""
  task_call_type <- if (call_type == "r") "r" else (task$call_type %||% call_type)
  task_expr <- if (is.function(task$expr)) task$expr(cfg, root_dir) else NULL
  cfun <- exports[[tid]]
  disposition <- run_manifest_disposition(run_metadata, runner_name, tid)
  input_record <- input_recipes$tasks[[tid]]
  mutation_policy <- as.character(input_record$mutation_policy)
  altrep_intent <- as.character(input_record$altrep_intent)
  task_seed <- input_scalar_integer(input_record$task_seed, sprintf("task seed for %s", tid))
  new_phase_arguments <- function() {
    arguments <- materialize_task_input(task, task_seed)$arguments
    if (identical(mutation_policy, "rng_reset_required")) {
      set.seed(task_seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
    }
    arguments
  }
  expression_for_arguments <- function(arguments) {
    if (!is.null(task_expr)) task_expr else make_call_expr(cfun, arguments, task_call_type)
  }
  if (call_type == ".Call" && is.null(registered_export_names) && !is.null(cfun)) {
    package_overrides <- cfg$package_overrides %||% list()
    package_name <- package_overrides[[tid]]
    if (!is.null(package_name)) {
      cfun <- getNativeSymbolInfo(cfun, PACKAGE = package_name)$address
    }
  }
  if (is.null(task_expr) && is.null(cfun)) {
    n_na <- n_na + 1
    na_allowed <- !isTRUE(disposition$executable)
    if (!na_allowed) {
      n_fail <- n_fail + 1
      correctness_message <- "normalized disposition requires an executable but the runner has none"
    }
    cat(sprintf("  %-14s [N/A]\n", tid))
    results_list[[length(results_list) + 1]] <- data.frame(
      runner = runner_name, task = tid, status = "N/A",
      call_type = task_call_type,
      mean_ms = NA, median_ms = NA, min_ms = NA, max_ms = NA,
      sd_ms = NA, cv_pct = NA, rss_kb = NA,
      cold_start_ms = NA, n_iterations = NA, error = NA_character_,
      correctness_status = "NOT_APPLICABLE",
      correctness_policy = correctness_policy,
      correctness_message = if (na_allowed) as.character(disposition$reason) else correctness_message,
      stringsAsFactors = FALSE,
      timing_summary_fields())
    next
  }
  if (!isTRUE(disposition$executable)) {
    stop(sprintf("runner %s exposes %s despite its non-executable disposition", runner_name, tid))
  }

  if (task_call_type == "r") {
    r_arguments <- new_phase_arguments()
    before <- task_arguments_fingerprint(tid, r_arguments, altrep_intent)
    r_eval <- capture_result(function() do.call(get(cfun, mode = "function"), r_arguments))
    if (!r_eval$ok) {
      correctness_status <- "FAIL"
      correctness_message <- sprintf("R implementation failed: %s", r_eval$error)
    } else {
      r_contract <- validate_result_contract(r_eval$value, expected_return)
      if (!r_contract$ok) {
        correctness_status <- "FAIL"
        correctness_message <- paste("R result:", r_contract$message)
      } else {
        if (identical(mutation_policy, "immutable")) {
          mutation <- capture_result(function() assert_immutable_input(tid, r_arguments, before, altrep_intent))
          if (!mutation$ok) {
            correctness_status <- "FAIL"
            correctness_message <- mutation$error
          }
        }
        if (!identical(correctness_status, "FAIL")) {
          provenance <- r_runner_provenance[[tid]]
          correctness_status <- if (identical(provenance$implementation_class, "pure_r")) "REFERENCE" else "PASS"
          correctness_message <- sprintf("validated %s result contract", provenance$implementation_class)
        }
      }
    }
  } else {
    native_arguments <- new_phase_arguments()
    native_before <- task_arguments_fingerprint(tid, native_arguments, altrep_intent)
    invoke_native <- function() {
      if (task_call_type == ".Call") return(do.call(.Call, c(list(cfun), native_arguments)))
      if (task_call_type == ".C") return(do.call(.C, c(list(cfun), native_arguments)))
      if (task_call_type == ".External") return(do.call(.External, c(list(cfun), native_arguments)))
      stop(sprintf("unsupported correctness call type: %s", task_call_type))
    }
    native_eval <- capture_result(invoke_native)
    if (!native_eval$ok) {
      correctness_status <- "FAIL"
      correctness_message <- sprintf("native call failed: %s", native_eval$error)
    } else {
      native_contract <- validate_result_contract(native_eval$value, expected_return)
      if (!native_contract$ok) {
        correctness_status <- "FAIL"
        correctness_message <- paste("native result:", native_contract$message)
      } else if (identical(mutation_policy, "immutable")) {
        mutation <- capture_result(function() assert_immutable_input(tid, native_arguments, native_before, altrep_intent))
        if (!mutation$ok) {
          correctness_status <- "FAIL"
          correctness_message <- mutation$error
        }
      }
      if (!identical(correctness_status, "FAIL") && identical(correctness_policy, "r_reference")) {
        ref_name <- r_ref[[tid]]
        if (is.null(ref_name) || !nzchar(ref_name) || !exists(ref_name, mode = "function")) {
          correctness_status <- "NOT_VALIDATED"
          correctness_message <- "R reference function is missing"
        } else {
          reference_arguments <- new_phase_arguments()
          reference_before <- task_arguments_fingerprint(tid, reference_arguments, altrep_intent)
          ref_eval <- capture_result(function() do.call(get(ref_name, mode = "function"), reference_arguments))
          if (!ref_eval$ok) {
            correctness_status <- "FAIL"
            correctness_message <- sprintf("R reference failed: %s", ref_eval$error)
          } else {
            ref_contract <- validate_result_contract(ref_eval$value, expected_return)
            if (!ref_contract$ok) {
              correctness_status <- "FAIL"
              correctness_message <- paste("R reference result:", ref_contract$message)
            } else if (identical(mutation_policy, "immutable")) {
              mutation <- capture_result(function() assert_immutable_input(tid, reference_arguments, reference_before, altrep_intent))
              if (!mutation$ok) {
                correctness_status <- "FAIL"
                correctness_message <- mutation$error
              }
            }
            if (!identical(correctness_status, "FAIL")) {
              comparison <- compare_correctness(ref_eval$value, native_eval$value)
              if (!isTRUE(comparison$ok)) {
                correctness_status <- "FAIL"
                correctness_message <- sprintf(
                  "structural validation mismatch: %s; expected '%s' got '%s'",
                  comparison$message,
                  result_preview(ref_eval$value),
                  result_preview(native_eval$value)
                )
              } else {
                correctness_status <- "PASS"
                correctness_message <- sprintf(
                  "validated against %s R reference",
                  r_reference_provenance[[tid]]$implementation_class
                )
              }
            }
          }
        }
      } else if (!identical(correctness_status, "FAIL")) {
        correctness_status <- "PASS"
      }
    }
  }

  if (correctness_status %in% c("FAIL", "NOT_VALIDATED")) {
    n_fail <- n_fail + 1
    cat(sprintf("  %-14s [%s] correctness: %s\n", tid, correctness_status, correctness_message))
    log_error(runner_name, tid, correctness_message, dir = staging_results_dir)
    results_list[[length(results_list) + 1]] <- data.frame(
      runner = runner_name, task = tid, status = "FAIL",
      call_type = task_call_type,
      mean_ms = NA, median_ms = NA, min_ms = NA, max_ms = NA,
      sd_ms = NA, cv_pct = NA, rss_kb = NA,
      cold_start_ms = NA, n_iterations = NA, error = correctness_message,
      correctness_status = correctness_status,
      correctness_policy = correctness_policy,
      correctness_message = correctness_message,
      stringsAsFactors = FALSE,
      timing_summary_fields())
    next
  }

  if (validation_only) {
    n_pass <- n_pass + 1L
    cat(sprintf("  %-14s [VALIDATED] %s\n", tid, correctness_message))
    next
  }

  # Do not retain large correctness results while timing the same input.
  native_eval <- NULL
  ref_eval <- NULL
  native_contract <- NULL
  ref_contract <- NULL
  comparison <- NULL

  cold_prepare <- function() expression_for_arguments(new_phase_arguments())
  cs <- timed_call(cold_prepare)
  log_cold_start(runner_name, tid, cs$wall_ms, run_id = run_id, dir = staging_results_dir)
  if (!is.na(cs$error)) {
    n_fail <- n_fail + 1
    cat(sprintf("  %-14s [FAIL] %s\n", tid, cs$error))
    log_error(runner_name, tid, cs$error, dir = staging_results_dir)
    results_list[[length(results_list) + 1]] <- data.frame(
      runner = runner_name, task = tid, status = "FAIL",
      call_type = task_call_type,
      mean_ms = NA, median_ms = NA, min_ms = NA, max_ms = NA,
      sd_ms = NA, cv_pct = NA, rss_kb = NA,
      cold_start_ms = round(cs$wall_ms, 3), n_iterations = NA,
      error = cs$error,
      correctness_status = correctness_status,
      correctness_policy = correctness_policy,
      correctness_message = correctness_message,
      stringsAsFactors = FALSE,
      timing_summary_fields())
    next
  }

  if (identical(mutation_policy, "immutable")) {
    warmup_arguments <- new_phase_arguments()
    timed_arguments <- new_phase_arguments()
    warmup_before <- task_arguments_fingerprint(tid, warmup_arguments, altrep_intent)
    timed_before <- task_arguments_fingerprint(tid, timed_arguments, altrep_intent)
    prepare_warmup <- function() expression_for_arguments(warmup_arguments)
    prepare_timed <- function() expression_for_arguments(timed_arguments)
  } else {
    prepare_warmup <- function() expression_for_arguments(new_phase_arguments())
    prepare_timed <- function() expression_for_arguments(new_phase_arguments())
  }

  bm <- benchmark_call(
    prepare_warmup,
    prepare_timed,
    warmup = as.integer(timing_policy$warmup_iterations),
    block_size = as.integer(timing_policy$block_size),
    max_iter = as.integer(timing_policy$max_iterations),
    cv_threshold = as.numeric(timing_policy$convergence_cv_threshold_pct),
    convergence_blocks = as.integer(timing_policy$convergence_window_blocks),
    timer_noise_floor_ms = as.numeric(timing_policy$timer_noise_floor_ms),
    rss_metric = as.character(timing_policy$rss_metric)
  )
  if (!is.na(bm$error)) {
    n_fail <- n_fail + 1
    cat(sprintf("  %-14s [FAIL] %s\n", tid, bm$error))
    log_error(runner_name, tid, bm$error, dir = staging_results_dir)
    results_list[[length(results_list) + 1]] <- data.frame(
      runner = runner_name, task = tid, status = "FAIL",
      call_type = task_call_type,
      mean_ms = NA, median_ms = NA, min_ms = NA, max_ms = NA,
      sd_ms = NA, cv_pct = NA, rss_kb = NA,
      cold_start_ms = round(cs$wall_ms, 3), n_iterations = NA,
      error = bm$error,
      correctness_status = correctness_status,
      correctness_policy = correctness_policy,
      correctness_message = correctness_message,
      stringsAsFactors = FALSE,
      timing_summary_fields())
    next
  }
  if (identical(mutation_policy, "immutable")) {
    assert_immutable_input(tid, warmup_arguments, warmup_before, altrep_intent)
    assert_immutable_input(tid, timed_arguments, timed_before, altrep_intent)
  }

  n_pass <- n_pass + 1
  cat(sprintf("  %-14s mean=%8.4fms median=%8.4fms sd=%7.4fms cv=%5.2f%% rss=%dKB runs=%d\n",
              tid, bm$mean_ms, bm$median_ms, bm$sd_ms, bm$cv_pct, bm$peak_rss, bm$n_runs))

  runs_df <- data.frame(
    runner      = runner_name,
    task        = tid,
    call_type   = task_call_type,
    iteration   = seq_len(length(bm$times)),
    wall_ms     = bm$times,
    peak_rss_kb = c(rep(NA_integer_, length(bm$times) - 1), bm$peak_rss),
    error       = NA_character_,
    run_id      = run_id,
    correctness_status = correctness_status,
    correctness_policy = correctness_policy,
    correctness_message = correctness_message,
    stringsAsFactors = FALSE
  )
  write_csv(runs_df, file.path(staging_results_dir, runner_name, sprintf("task_%s.csv", tid)))

  results_list[[length(results_list) + 1]] <- data.frame(
    runner        = runner_name,
    task          = tid,
    status        = "PASS",
    call_type     = task_call_type,
    mean_ms       = round(bm$mean_ms, 4),
    median_ms     = round(bm$median_ms, 4),
    min_ms        = round(bm$min_ms, 4),
    max_ms        = round(bm$max_ms, 4),
    sd_ms         = round(bm$sd_ms, 4),
    cv_pct        = round(bm$cv_pct, 2),
    rss_kb        = bm$peak_rss,
    cold_start_ms = round(cs$wall_ms, 3),
    n_iterations  = bm$n_runs,
    error         = NA_character_,
    correctness_status = correctness_status,
    correctness_policy = correctness_policy,
    correctness_message = correctness_message,
    stringsAsFactors = FALSE,
    timing_summary_fields(bm)
  )
}

if (validation_only) {
  if (n_fail > 0L) stop(sprintf("runner %s failed validation for %d task(s)", runner_name, n_fail))
  cat(sprintf("Validation preflight passed for %s: %d executable and %d N/A task(s).\n", runner_name, n_pass, n_na))
  quit(save = "no", status = 0, runLast = FALSE)
}

summary <- do.call(rbind, results_list)
summary$run_id <- run_id
summary_dispositions <- lapply(summary$task, function(task_id) {
  run_manifest_disposition(run_metadata, runner_name, as.character(task_id))
})
summary_inputs <- input_recipes$tasks[as.character(summary$task)]
summary$master_seed <- master_seed
summary$task_seed <- vapply(summary_inputs, function(record) input_scalar_integer(record$task_seed, "task seed"), integer(1))
summary$input_fingerprint <- vapply(summary_inputs, function(record) as.character(record$fingerprint), character(1))
summary$contract_version <- vapply(summary_dispositions, function(record) as.character(record$contract_version), character(1))
summary$path_kind <- vapply(summary_dispositions, function(record) as.character(record$path_kind), character(1))
summary$evidence_use <- vapply(summary_dispositions, function(record) as.character(record$evidence_use), character(1))
summary$kernel_id <- vapply(summary_dispositions, function(record) as.character(record$kernel_id), character(1))
summary$representation_strategy <- vapply(
  summary_dispositions,
  function(record) as.character(record$representation_strategy),
  character(1)
)
summary$mutation_policy <- vapply(summary_inputs, function(record) as.character(record$mutation_policy), character(1))
summary$tool_identity <- as.character(runner_environment$tool_identity)
summary$generated_glue_kind <- as.character(runner_environment$generated_glue_kind)
summary$generated_glue_digest <- as.character(runner_environment$generated_glue_digest)
summary$artifact_digest <- as.character(runner_environment$artifact_digest)
summary$source_digest <- as.character(runner_environment$source_digest)
summary$build_digest <- as.character(runner_environment$build_digest)
summary$dependency_digest <- as.character(runner_environment$dependency_digest)
summary$artifact_dependency_digest <- as.character(runner_environment$artifact_dependency_digest)
summary$source_ledger_identity_digest <- as.character(runner_environment$source_ledger_identity_digest)
summary_source_records <- lapply(as.character(summary$task), function(task_id) {
  source_ledger_verification_record(run_metadata$environment$tool_source_ledger, runner_name, task_id)
})
summary$source_path_class <- vapply(summary_source_records, function(record) as.character(record$source_class), character(1))
summary$source_verification_digest <- vapply(
  summary_source_records,
  function(record) as.character(record$verification_digest),
  character(1)
)
summary$disposition <- vapply(summary_dispositions, function(record) as.character(record$status), character(1))
summary$disposition_reason <- vapply(summary_dispositions, function(record) as.character(record$reason), character(1))
summary$r_implementation_provenance <- vapply(as.character(summary$task), function(task_id) {
  if (identical(runner_name, "r")) {
    if (!is.null(r_runner_provenance[[task_id]])) return(as.character(r_runner_provenance[[task_id]]$implementation_class))
    return("pure_r_unrepresentable")
  }
  if (!is.null(r_reference_provenance[[task_id]])) {
    return(paste0("reference:", as.character(r_reference_provenance[[task_id]]$implementation_class)))
  }
  "not_applicable"
}, character(1))
summary$r_source_digest <- vapply(as.character(summary$task), function(task_id) {
  if (identical(runner_name, "r") && !is.null(r_runner_provenance[[task_id]])) {
    return(as.character(r_runner_provenance[[task_id]]$source_digest))
  }
  if (!is.null(r_reference_provenance[[task_id]])) return(as.character(r_reference_provenance[[task_id]]$source_digest))
  "not_applicable"
}, character(1))
staged_summary <- file.path(staging_results_dir, sprintf("%s_summary.csv", runner_name))
write_csv(summary, staged_summary)
if (n_fail > 0L) {
  stop(sprintf(
    "runner %s failed correctness or timing validation for %d task(s); run cannot be completed",
    runner_name, n_fail
  ))
}
final_runner_dir <- file.path(results_dir, runner_name)
final_summary <- file.path(results_dir, sprintf("%s_summary.csv", runner_name))
if (dir.exists(final_runner_dir) || file.exists(final_summary)) {
  stop(sprintf("final result path already exists for runner %s", runner_name))
}
if (!file.rename(file.path(staging_results_dir, runner_name), final_runner_dir)) {
  stop(sprintf("cannot promote staged results for runner %s", runner_name))
}
if (!file.rename(staged_summary, final_summary)) {
  stop(sprintf("cannot promote staged summary for runner %s", runner_name))
}

cat(sprintf("  Results: %d PASS, %d FAIL, %d N/A\n", n_pass, n_fail, n_na))
for (i in seq_len(nrow(summary))) {
  s <- summary[i, ]
  cat(sprintf("  %-14s %8.4f %7.4f %8.4f %5.2f %8d %5d  %s\n",
              s$task, s$mean_ms, s$median_ms, s$sd_ms, s$cv_pct, s$rss_kb, s$n_iterations, s$status))
}
