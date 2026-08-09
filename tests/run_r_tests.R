#!/usr/bin/env Rscript

build_so <- function() {
  zig <- Sys.getenv("ZIG", "")
  if (!nzchar(zig)) zig <- Sys.which("zig")
  if (!nzchar(zig)) zig <- file.path("zig-0.16.0", "zig")
  if (!file.exists(zig)) stop("zig executable not found; set ZIG or install zig")
  zig <- normalizePath(zig)

  r_include <- Sys.getenv("R_INCLUDE", "")
  if (!nzchar(r_include)) {
    candidates <- c(file.path(R.home(), "include"), file.path(R.home(), "../share/R/include"), "/usr/share/R/include")
    candidates <- candidates[dir.exists(candidates)]
    if (length(candidates) == 0L) stop("R include directory not found; set R_INCLUDE")
    r_include <- normalizePath(candidates[[1L]])
  }
  r_lib <- Sys.getenv("R_LIB", file.path(R.home(), "lib"))
  if (!dir.exists(r_include) || !dir.exists(r_lib)) stop("invalid R include/lib directories")
  cache_dir <- Sys.getenv("ZIG_CACHE_DIR", ".zig-cache")
  global_cache <- Sys.getenv("ZIG_GLOBAL_CACHE_DIR", file.path("benchmarks", ".zig-global-cache"))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(global_cache, recursive = TRUE, showWarnings = FALSE)
  optimize <- Sys.getenv("ZIGR_OPTIMIZE", "ReleaseFast")
  build_args <- c(
    "build", "rtest",
    paste0("-Doptimize=", optimize),
    paste0("-Dr-include=", r_include),
    paste0("-Dr-lib=", r_lib),
    "--cache-dir", cache_dir,
    "--global-cache-dir", global_cache
  )
  target <- Sys.getenv("ZIGR_TARGET", "")
  if (nzchar(target) && target != "native") build_args <- c(build_args, paste0("-Dtarget=", target))
  cpu_features <- Sys.getenv("ZIGR_CPU_FEATURES", "")
  if (nzchar(cpu_features) && cpu_features != "default") build_args <- c(build_args, paste0("-Dcpu=", cpu_features))
  checked_sexp <- tolower(Sys.getenv("ZIGR_CHECKED_SEXP", "false"))
  if (checked_sexp %in% c("1", "true", "yes")) build_args <- c(build_args, "-Dchecked-sexp=true")
  direct_sexp <- tolower(Sys.getenv("ZIGR_DIRECT_SEXP", "false"))
  if (direct_sexp %in% c("1", "true", "yes")) build_args <- c(build_args, "-Ddirect-sexp=true")

  cat("Building test .so...\n")
  cat("  ", paste(c(zig, build_args), collapse = " "), "\n")
  exit_code <- system2(zig, args = build_args)
  if (exit_code != 0) {
    stop("zig build rtest failed with exit code ", exit_code)
  }

  so_path <- file.path("zig-out", "lib", "libzigr_r_test.so")
  if (!file.exists(so_path)) {
    so_path <- Sys.glob(file.path("zig-out", "*", "*.so"))[1]
  }
  if (!file.exists(so_path)) {
    stop("Test .so not found in zig-out/")
  }
  so_path
}

so_path <- build_so()

cat("Loading .so:", so_path, "\n")
test_dll <- dyn.load(so_path)

run_generated_registration_arity_test <- function() {
  load_registration_dll <- function(name) {
    path <- file.path(tempdir(), paste0(name, .Platform$dynlib.ext))
    if (!file.copy(so_path, path, overwrite = TRUE)) stop("failed to copy generated registration fixture")
    dyn.load(path)
  }
  call_dll <- load_registration_dll("zigr_arity_calls")
  method_dll <- load_registration_dll("zigr_arity_methods")
  call_routines <- getDLLRegisteredRoutines(call_dll)[[".Call"]]
  method_routines <- getDLLRegisteredRoutines(method_dll)[[".Call"]]
  if (is.null(call_routines) || is.null(method_routines)) stop("missing generated .Call routine table")
  rejects_call <- function(entry, arguments) {
    tryCatch(
      {
        do.call(.Call, c(list(entry), arguments))
        FALSE
      },
      error = function(e) TRUE
    )
  }

  for (arity in 0:8) {
    name <- paste0("zigr_arity_probe_", arity)
    entry <- call_routines[[name]]
    if (is.null(entry) || entry$numParameters != arity) stop("missing or wrong ordinary registration: ", name)
    actual <- do.call(.Call, c(list(entry), as.list(seq_len(arity))))
    expected <- as.integer(arity * (arity + 1L) / 2L)
    if (!identical(actual, expected)) stop("ordinary invocation failed: ", name)

    wrong_arguments <- if (arity == 0L) list(1L) else as.list(seq_len(arity - 1L))
    if (!rejects_call(entry, wrong_arguments)) stop("ordinary wrapper accepted wrong arity: ", name)
  }

  state_entry <- call_routines[["zigr_arity_probe_state"]]
  if (is.null(state_entry) || state_entry$numParameters != 0L) stop("missing state constructor registration")
  state <- .Call(state_entry)
  if (!rejects_call(state_entry, list(1L))) stop("state constructor accepted wrong arity")

  for (extra_arity in 0:4) {
    name <- paste0("r_runtime_ArityProbeState__arity_", extra_arity)
    entry <- method_routines[[name]]
    registered_arity <- extra_arity + 1L
    if (is.null(entry) || entry$numParameters != registered_arity) stop("missing or wrong method registration: ", name)
    actual <- do.call(.Call, c(list(entry, state), as.list(seq_len(extra_arity))))
    expected <- as.integer(extra_arity * (extra_arity + 1L) / 2L)
    if (!identical(actual, expected)) stop("method invocation failed: ", name)

    wrong_arguments <- if (extra_arity == 0L) {
      list(state, 1L)
    } else {
      c(list(state), as.list(seq_len(extra_arity - 1L)))
    }
    if (!rejects_call(entry, wrong_arguments)) stop("method wrapper accepted wrong arity: ", name)
  }

  TRUE
}

tests <- list(
  "zigr_test_protect",
  "zigr_test_return42",
  "zigr_test_abi_contract",
  list(name="zigr_test_longjmp", expect_error=TRUE),
  "zigr_longjmp_flag",
  "zigr_test_longjmp_normal",

  list(name="zigr_test_error_signal", expect_error=TRUE),
  "zigr_test_error_warn",
  "zigr_test_error_format_messages",
  "zigr_test_validation_failures",
  list(name="zigr_test_error_signalif", expect_error=TRUE),
  list(name="zigr_raise_error", expect_error=TRUE),

  "zigr_test_interrupt",
  "zigr_test_check_stack",
  "zigr_test_check_stack_longjmp",

  "zigr_test_rev_eval",
  "zigr_test_rev_define_find",
  "zigr_test_rev_lang3",
  "zigr_test_symbol_contract",
  "zigr_test_eval_contract",
  "zigr_test_language_call_contract",
  "zigr_test_language_call_longjmp",
  "zigr_test_runtime_semantics",

  "zigr_test_rng",

  "zigr_test_ralloc",

  list(name="zigr_test_preserve_longjmp", expect_error=TRUE),
  "zigr_preserve_flag",
  list(name="zigr_test_nested_outer", expect_error=TRUE),
  "zigr_nested_flags",

  "zigr_test_to_real_slice",
  "zigr_test_from_real_slice",
  "zigr_test_real_roundtrip",
  "zigr_test_int_create",
  "zigr_test_int_from_slice",
  "zigr_test_str_create",
  "zigr_test_str_from_slice",
  "zigr_test_lgl_create",
  "zigr_test_lgl_from_slice",
  "zigr_test_list_create",
  "zigr_test_to_logical_slice",
  "zigr_test_raw_create",
  "zigr_test_cplx_create",

  "zigr_test_df_build",
  "zigr_test_df_column",
  "zigr_test_df_contract",
  "zigr_test_df_cleanup_capacity",
  "zigr_test_factor_contract",
  "zigr_test_factor_longjmp",
  "zigr_test_s4_contract",
  "zigr_test_s4_longjmp",
  "zigr_test_advanced_altrep_retention",

  "zigr_test_attrib_class",
  "zigr_test_attrib_names",
  "zigr_test_attrib_contract",
  "zigr_test_attrib_allocation_longjmp",
  "zigr_test_attrib_cleanup_capacity",
  "zigr_test_native_cleanup_capacity",
  "zigr_test_native_allocator_failure",
  "zigr_test_df_names_longjmp",

  "zigr_test_altrep_create",
  "zigr_test_altrep_sum",
  "zigr_test_altrep_direct_ptr",
  "zigr_test_altint_direct_slice",
  "zigr_test_altint_sum_direct",
  "zigr_test_altint_min_direct",
  "zigr_test_altint_max_direct",
  "zigr_test_altint_argmin_direct",
  "zigr_test_altint_argmax_direct",
  "zigr_test_altlogical_direct_slice",
  "zigr_test_altlogical_count_true_direct",
  "zigr_test_altlogical_min_direct",
  "zigr_test_altlogical_max_direct",
  "zigr_test_altlogical_argmin_direct",
  "zigr_test_altlogical_argmax_direct",
  "zigr_test_altrep_mean",
  "zigr_test_altrep_norm2_simd",
  "zigr_test_altrep_min_simd",
  "zigr_test_altrep_max_simd",
  "zigr_test_altrep_argmin_simd",
  "zigr_test_altrep_argmax_simd",
  "zigr_test_argminmax_missing_contract",
  "zigr_test_altrep_sum_narm",
  "zigr_test_altrep_mean_narm",
  "zigr_test_altraw_create",
  "zigr_test_altcomplex_create",
  "zigr_test_altstring_create",
  "zigr_test_string_representations",
  "zigr_test_string_projections",
  "zigr_test_altstring_inputs",
  "zigr_test_altrep_registration_contract",
  "zigr_test_altrep_logical_input_contract",
  "zigr_test_altrep_owned_storage",
  "zigr_test_altrep_finalizer_lifecycle",
  "zigr_test_altrep_serialization_contract",
  "zigr_test_altrep_serialized_state_validation",
  "zigr_test_altrep_summary_contract",
  "zigr_test_altrep_empty_callback_contract",
  "zigr_test_altrep_real_callback_contract",
  "zigr_test_altrep_typed_callback_contract",
  "zigr_test_altrep_complex_string_callback_contract",
  "zigr_test_borrowed_views",
  "zigr_test_borrowed_lifetimes",
  "zigr_test_owned_view_cleanup_order",
  "zigr_test_compact_altrep_views",
  "zigr_test_short_region",

  "zigr_test_from_empty",
  "zigr_test_real_huge",
  "zigr_test_str_na",
  "zigr_test_str_na_nullable",
  "zigr_test_lgl_edge",
  "zigr_test_list_null",
  "zigr_test_df_col_missing",
  "zigr_test_real_wrong_type",
  "zigr_test_raw_logical",
  "zigr_test_raw_real",
  "zigr_test_raw_int",
  "zigr_test_raw_real_mut",
  "zigr_test_raw_int_mut",
  "zigr_test_raw_raw",
  "zigr_test_raw_complex",
  "zigr_test_raw_dims",
  "zigr_test_raw_checked_access",
  list(name="zigr_test_from_sexp_missing_required", expect_error=TRUE),
  list(name="zigr_test_from_sexp_invalid_names", expect_error=TRUE),
  list(name="zigr_test_from_sexp_missing_names", expect_error=TRUE),
  list(name="zigr_test_from_sexp_name_length_mismatch", expect_error=TRUE),
  "zigr_test_list_wrong_type",
  "zigr_test_real_scalar_na",
  "zigr_test_int_scalar_na",
  "zigr_test_bool_scalar_na",
  "zigr_test_scalar_contract",
  "zigr_test_optional_scalar_contract",
  "zigr_test_optional_real_na_to_null",
  "zigr_test_optional_int_na_to_null",
  "zigr_test_optional_bool_na_to_null",
  "zigr_test_pmin_recycling",
  "zigr_test_pmax_recycling",

  "zigr_test_embed_sum",
  "zigr_test_embed_paste",
  "zigr_test_serialize_roundtrip_contract",
  "zigr_test_serialize_checked_input",
  "zigr_test_serialize_malformed_unwind",
  "zigr_test_raw_eval",
  "zigr_test_struct_to_sexp",
  "zigr_test_struct_from_sexp",
  list(name="zigr_test_embed_empty", expect_error=FALSE),
  "zigr_test_embed_vector",
  "zigr_test_embed_braces",
  "zigr_test_embed_null",
  "zigr_test_struct_to_sexp_empty",
  "zigr_test_struct_to_sexp_nested",
  list(name="zigr_test_struct_from_sexp_missing_optional_field", expect_error=TRUE),
  "zigr_test_struct_from_sexp_optional_present",
  "zigr_test_fixed_schema_contract",
  "zigr_test_fixed_schema_nested_optional_long",
  "zigr_test_stress_protect",
  "zigr_test_stress_embed",
  list(name="zigr_test_embed_syntax_error", expect_error=FALSE),
  list(name="zigr_test_embed_stop_error", expect_error=FALSE),
  "zigr_test_embed_warning",
  "zigr_test_embed_unicode",
  "zigr_test_embed_long_code",
  "zigr_test_embed_cleanup_capacity",
  "zigr_test_struct_roundtrip",
  "zigr_test_struct_nan_inf",
  "zigr_test_struct_neg_zero",
  "zigr_test_struct_many_fields",
  list(name="zigr_test_trycatch_nested", expect_error=FALSE),
  "zigr_test_stress_protect_10k",

  "zigr_test_build_call",
  "zigr_test_build_named_call",

  "zigr_test_scoped_release",
  "zigr_test_scoped_get",

  "zigr_test_rvector_f64",
  "zigr_test_rvector_i32",
  "zigr_test_rvector_complex",
  "zigr_test_rvector_wrong_type",
  "zigr_test_rvector_add_scalar",
  "zigr_test_rvector_sub_scalar",
  "zigr_test_rvector_mul_scalar",
  "zigr_test_rvector_div_scalar",
  "zigr_test_rvector_add_vec",
  "zigr_test_rvector_f64_sum",
  "zigr_test_rvector_i32_sum",
  "zigr_test_rvector_recycle",
  "zigr_test_rvector_empty",

  "zigr_test_export_two_scalars",

  "zigr_test_export_complex_sum",

  "zigr_test_export_sum",
  "zigr_test_export_string_lengths",
  "zigr_test_export_string_na",
  "zigr_test_export_cached_string_lengths",
  "zigr_test_export_sum_empty",
  "zigr_test_export_raw_roundtrip",

  "zigr_test_export_optional_null",
  "zigr_test_export_scalar_na",
  "zigr_test_export_int_scalar_na",
  "zigr_test_export_bool_scalar_na",
  "zigr_test_export_wrong_type_real",

  "zigr_test_export_method_call",
  "zigr_test_export_method_tag",

  "zigr_test_export_external",

  "zigr_test_export_generatemethods",
  "zigr_test_export_generatemethods_external",
  "zigr_test_generated_logical_vector",
  "zigr_test_generated_logical_wrong_type",
  "zigr_test_generated_method_receiver_errors",
  "zigr_test_dispatch_semantics",
  "zigr_test_registered_method_dispatch",
  "zigr_test_generated_ownership_gc",
  "zigr_test_allocation_diagnostics",
  "zigr_test_cleanup_diagnostics",
  list(name="zigr_test_generated_spill_longjmp", expect_error=TRUE),
  "zigr_test_altrep_access_strategies",
  "zigr_test_generated_result_longjmp",
  "zigr_test_direct_result_builder",
  "zigr_test_result_builder_lifetimes",
  "zigr_test_scalar_reference",
  "zigr_test_kernel_reference",
  "zigr_test_raw_complex_reference",
  "zigr_test_result_reference",
  "zigr_test_strategy_equivalence",
  "zigr_test_string_projection_semantics",
  "zigr_test_object_semantics",
  "zigr_test_externalptr_finalizer",
  "zigr_test_externalptr_finalizer_idempotent",
  "zigr_test_externalptr_typed_protected",
  "zigr_test_externalptr_lazy_tag_gc",
  "zigr_test_weakref_reachable_contract",
  "zigr_test_weakref_checked_errors",
  "zigr_test_weakref_gc_finalizer",
  "zigr_test_raw_views",
  "zigr_test_generated_view_selection",
  "zigr_test_complex_boundary",
  "zigr_test_generated_string_shapes",
  "zigr_test_generated_string_projections",
  "zigr_test_string_allocation_longjmp",
  "zigr_test_view_allocation_longjmp",
  "zigr_test_conversion_allocation_longjmp",
  "zigr_test_pmin_pmax_longjmp",
  "zigr_test_materializer_region_errors",
  "zigr_test_chunk_region_errors",

  list(name="zigr_test_export_from_sexp_wrong_type", expect_error=TRUE),

  "zigr_alloc_real",
  "zigr_alloc_large",
  "zigr_protect_many",
  "zigr_protect_index",
  "zigr_check_na",
  "zigr_raise_warning",
  "zigr_typeof_nil",

  "zigr_test_cleanup_fires_on_longjmp",
  "zigr_test_recovered_cleanup",
  "zigr_test_unwind_state_restoration",
  "zigr_test_nested_unwind_state",
  "zigr_test_cleanup_capacity_recovers",
  "zigr_test_with_rng_longjmp",
  "zigr_test_with_rng_nested",

  "zigr_test_fib_recursive",

  "zigr_test_lang_builder",

  list(name="zigr_fuzz_sum_type", expect_error=TRUE),
  list(name="zigr_fuzz_norm2_type", expect_error=TRUE),
  list(name="zigr_fuzz_min_type", expect_error=TRUE),
  list(name="zigr_fuzz_max_type", expect_error=TRUE),
  list(name="zigr_fuzz_scaleAdd_type", expect_error=TRUE),
  list(name="zigr_fuzz_cumsum_type", expect_error=TRUE),
  list(name="zigr_fuzz_sum_narm_type", expect_error=TRUE),
  list(name="zigr_fuzz_mean_narm_type", expect_error=TRUE),
  list(name="zigr_fuzz_argmin_type", expect_error=TRUE),
  list(name="zigr_fuzz_argmax_type", expect_error=TRUE),
  list(name="zigr_fuzz_sumInt_type", expect_error=TRUE),
  list(name="zigr_fuzz_countTrue_type", expect_error=TRUE),
  list(name="zigr_fuzz_minInt_type", expect_error=TRUE),
  list(name="zigr_fuzz_maxInt_type", expect_error=TRUE),
  list(name="zigr_fuzz_argminInt_type", expect_error=TRUE),
  list(name="zigr_fuzz_argmaxInt_type", expect_error=TRUE),
  list(name="zigr_fuzz_minLogical_type", expect_error=TRUE),
  list(name="zigr_fuzz_maxLogical_type", expect_error=TRUE),
  list(name="zigr_fuzz_argminLogical_type", expect_error=TRUE),
  list(name="zigr_fuzz_argmaxLogical_type", expect_error=TRUE),
  list(name="zigr_fuzz_pmin_type", expect_error=TRUE),
  list(name="zigr_fuzz_pmax_type", expect_error=TRUE),
  list(name="zigr_fuzz_findVar_unbound", expect_error=TRUE),
  "zigr_fuzz_toRealScalar_type",
  "zigr_fuzz_toIntScalar_type",
  "zigr_fuzz_toBoolScalar_type",
  "zigr_fuzz_toRealSlice_type",
  "zigr_fuzz_toIntSlice_type",
  "zigr_fuzz_toStringSlice_type",
  "zigr_fuzz_toListSlice_type",
  "zigr_fuzz_toRawSlice_type",
  "zigr_fuzz_toRawSliceView_type",
  "zigr_fuzz_toRealSliceView_type",
  "zigr_fuzz_toIntSliceView_type",
  "zigr_fuzz_toComplexSlice_type",
  "zigr_fuzz_toComplexSliceView_type",
  "zigr_fuzz_toLogicalSlice_type",
  "zigr_fuzz_toLogicalSliceView_type",
  "zigr_fuzz_toStringSliceView_type",
  "zigr_fuzz_toCachedStringSliceView_type",
  "zigr_fuzz_scalar_na",
  "zigr_fuzz_scalar_empty",
  "zigr_test_final_cleanup_state"
)

cat("\n=== zigr R runtime tests ===\n")
passed <- 0
failed <- 0
skipped <- 0
gctorture_setting <- Sys.getenv("ZIGR_GCTORTURE", "")
gctorture_mode <- nzchar(gctorture_setting) && !tolower(gctorture_setting) %in% c("0", "false", "no")
gctorture_step <- suppressWarnings(as.integer(gctorture_setting))
if (is.na(gctorture_step) || gctorture_step < 1L) gctorture_step <- 10L
reduction_only <- tolower(Sys.getenv("ZIGR_REDUCTION_ONLY", "false")) %in% c("1", "true", "yes")

run_with_gctorture <- function(expr) {
  if (!gctorture_mode) return(force(expr))

  gctorture2(gctorture_step)
  on.exit(gctorture(FALSE), add = TRUE)
  force(expr)
}

reduction_symbols <- local({
  path <- file.path(tempdir(), paste0("zigr_reductions", .Platform$dynlib.ext))
  if (!file.copy(so_path, path, overwrite = TRUE)) stop("failed to copy reduction oracle fixture")
  reduction_dll <- dyn.load(path)
  names <- c(
    sum = "zigr_reduction_sum",
    mean = "zigr_reduction_mean",
    sum_narm = "zigr_reduction_sum_narm",
    mean_narm = "zigr_reduction_mean_narm",
    list = "zigr_reduction_list",
    direct = "zigr_reduction_direct",
    region = "zigr_reduction_region",
    failing_region = "zigr_reduction_failing_region",
    uses_direct = "zigr_reduction_uses_direct",
    uses_regions = "zigr_reduction_uses_regions",
    reset_diagnostics = "zigr_reduction_reset_diagnostics",
    resource_state = "zigr_reduction_resource_state"
  )
  lapply(names, function(name) getNativeSymbolInfo(name, reduction_dll)$address)
})

reduction_call <- function(name, ...) {
  .Call(reduction_symbols[[name]], ...)
}

reduction_direct <- function(values) {
  result <- reduction_call("direct", values)
  if (!identical(reduction_call("uses_direct", result), TRUE)) {
    stop("reduction direct fixture lacks direct storage")
  }
  result
}

reduction_region <- function(values) {
  result <- reduction_call("region", values)
  if (!identical(reduction_call("uses_regions", result), TRUE)) {
    stop("reduction region fixture exposes direct storage")
  }
  result
}

reduction_failing_region <- function(values, fail_after = 1L) {
  result <- reduction_call("failing_region", values, as.integer(fail_after))
  if (!identical(reduction_call("uses_regions", result), TRUE)) {
    stop("failing reduction fixture exposes direct storage")
  }
  result
}

reduction_ordinary <- function(values) {
  if (!identical(reduction_call("uses_direct", values), FALSE) ||
      !identical(reduction_call("uses_regions", values), FALSE)) {
    stop("reduction ordinary fixture is ALTREP")
  }
  values
}

exact_scalar_equal <- function(actual, expected) {
  if (!identical(actual, expected)) return(FALSE)
  if (isTRUE(expected == 0)) return(identical(1 / actual, 1 / expected))
  TRUE
}

exact_scalar_label <- function(value) {
  if (isTRUE(value == 0)) return(if (1 / value < 0) "-0" else "0")
  paste(format(value, digits = 17L, scientific = TRUE), collapse = ",")
}

check_reduction_error <- function(expression, expected) {
  condition <- tryCatch(
    {
      force(expression)
      NULL
    },
    error = function(error) error
  )
  if (is.null(condition)) stop(sprintf("expected error: %s", expected))
  actual <- conditionMessage(condition)
  if (!identical(actual, expected)) {
    stop(sprintf("expected error %s, got %s", expected, actual))
  }
  TRUE
}

check_vector_reduction_oracle <- function(symbol, oracle, cases) {
  failures <- character()
  for (case_name in names(cases)) {
    ordinary <- reduction_ordinary(cases[[case_name]])
    expected <- oracle(ordinary)
    inputs <- list(
      ordinary = ordinary,
      direct = reduction_direct(ordinary),
      region = reduction_region(ordinary)
    )
    for (representation in names(inputs)) {
      input <- inputs[[representation]]
      actual <- reduction_call(symbol, input)
      if (!exact_scalar_equal(actual, expected)) {
        failures <- c(
          failures,
          sprintf(
            "%s/%s expected %s, got %s",
            case_name, representation, exact_scalar_label(expected), exact_scalar_label(actual)
          )
        )
      }
    }
  }
  if (length(failures)) stop(paste(failures, collapse = "; "))
  check_reduction_error(reduction_call(symbol, 1L), "expected REALSXP")
  TRUE
}

check_list_reduction_oracle <- function(cases) {
  oracle <- function(value) sum(vapply(value, sum, numeric(1)))
  failures <- character()
  for (case_name in names(cases)) {
    ordinary <- lapply(cases[[case_name]], reduction_ordinary)
    expected <- oracle(ordinary)
    inputs <- list(
      ordinary = ordinary,
      direct = lapply(ordinary, reduction_direct),
      region = lapply(ordinary, reduction_region)
    )
    for (representation in names(inputs)) {
      input <- inputs[[representation]]
      actual <- reduction_call("list", input)
      if (!exact_scalar_equal(actual, expected)) {
        failures <- c(
          failures,
          sprintf(
            "%s/%s expected %s, got %s",
            case_name, representation, exact_scalar_label(expected), exact_scalar_label(actual)
          )
        )
      }
    }
  }
  if (length(failures)) stop(paste(failures, collapse = "; "))
  check_reduction_error(reduction_call("list", numeric()), "expected VECSXP")
  check_reduction_error(reduction_call("list", list(1L)), "expected REALSXP")
  TRUE
}

check_reduction_resource_state <- function(expected_region_calls) {
  actual <- reduction_call("resource_state")
  expected <- as.integer(expected_region_calls)
  if (!identical(actual, expected)) {
    stop(sprintf("expected %d region calls with restored state, got %d", expected, actual))
  }
  TRUE
}

reset_reduction_diagnostics <- function() {
  if (!identical(reduction_call("reset_diagnostics"), TRUE)) {
    stop("failed to reset reduction diagnostics")
  }
}

check_reduction_resource_contract <- function() {
  values <- rep(c(-4, 1, 2, 8), 2049L)
  region_chunks <- ceiling(length(values) / 4096L)
  vectors <- list(
    sum = list(oracle = sum, region_passes = 1L, failure_after = 1L),
    mean = list(oracle = mean, region_passes = 2L, failure_after = region_chunks + 1L),
    sum_narm = list(
      oracle = function(value) sum(value, na.rm = TRUE),
      region_passes = 1L,
      failure_after = 1L
    ),
    mean_narm = list(
      oracle = function(value) mean(value, na.rm = TRUE),
      region_passes = 2L,
      failure_after = region_chunks + 1L
    )
  )

  for (name in names(vectors)) {
    family <- vectors[[name]]
    expected <- family$oracle(values)
    for (input in list(values, reduction_direct(values))) {
      reset_reduction_diagnostics()
      if (!exact_scalar_equal(reduction_call(name, input), expected)) {
        stop("reduction resource success mismatch: ", name)
      }
      check_reduction_resource_state(0L)
    }

    reset_reduction_diagnostics()
    if (!exact_scalar_equal(reduction_call(name, reduction_region(values)), expected)) {
      stop("region reduction resource success mismatch: ", name)
    }
    check_reduction_resource_state(region_chunks * family$region_passes)

    reset_reduction_diagnostics()
    check_reduction_error(
      reduction_call(name, reduction_failing_region(values, family$failure_after)),
      "injected reduction region error"
    )
    check_reduction_resource_state(family$failure_after)

    reset_reduction_diagnostics()
    check_reduction_error(reduction_call(name, 1L), "expected REALSXP")
    check_reduction_resource_state(0L)
  }

  overflow_values <- rep(1e305, length(values))
  overflow_vectors <- list(
    mean = list(values = overflow_values, oracle = mean),
    mean_narm = list(
      values = replace(overflow_values, 4097L, NA_real_),
      oracle = function(value) mean(value, na.rm = TRUE)
    )
  )
  overflow_failure_after <- 2L * region_chunks + 1L
  for (name in names(overflow_vectors)) {
    family <- overflow_vectors[[name]]
    reset_reduction_diagnostics()
    if (!exact_scalar_equal(
      reduction_call(name, reduction_region(family$values)),
      family$oracle(family$values)
    )) stop("overflow mean resource success mismatch: ", name)
    check_reduction_resource_state(3L * region_chunks)

    reset_reduction_diagnostics()
    check_reduction_error(
      reduction_call(
        name,
        reduction_failing_region(family$values, overflow_failure_after)
      ),
      "injected reduction region error"
    )
    check_reduction_resource_state(overflow_failure_after)
  }

  list_values <- list(values, rev(values))
  expected_list <- sum(vapply(list_values, sum, numeric(1)))
  for (input in list(list_values, lapply(list_values, reduction_direct))) {
    reset_reduction_diagnostics()
    if (!exact_scalar_equal(reduction_call("list", input), expected_list)) {
      stop("list reduction resource success mismatch")
    }
    check_reduction_resource_state(0L)
  }

  reset_reduction_diagnostics()
  if (!exact_scalar_equal(
    reduction_call("list", lapply(list_values, reduction_region)),
    expected_list
  )) stop("region list reduction resource success mismatch")
  check_reduction_resource_state(2L * region_chunks)

  reset_reduction_diagnostics()
  check_reduction_error(
    reduction_call(
      "list",
      list(reduction_region(values), reduction_failing_region(values, region_chunks + 1L))
    ),
    "injected reduction region error"
  )
  check_reduction_resource_state(region_chunks + 1L)

  reset_reduction_diagnostics()
  check_reduction_error(reduction_call("list", numeric()), "expected VECSXP")
  check_reduction_resource_state(0L)

  reset_reduction_diagnostics()
  check_reduction_error(
    reduction_call("list", list(reduction_region(values), 1L)),
    "expected REALSXP"
  )
  check_reduction_resource_state(region_chunks)

  reset_reduction_diagnostics()
  check_reduction_error(
    reduction_failing_region(values, 0L),
    "reduction failure point must be positive"
  )
  check_reduction_resource_state(0L)
  TRUE
}

cancellation <- c(1e16, 1, -1e16)
chunk_cancellation <- numeric(4099L)
chunk_cancellation[4096:4098] <- cancellation
missing_cancellation <- c(1e16, NA_real_, 1, NaN, -1e16)
chunk_missing_cancellation <- numeric(4101L)
chunk_missing_cancellation[4096:4100] <- missing_cancellation

reduction_cases <- list(
  finite = c(-4, 1, 2, 8),
  cancellation = cancellation,
  chunk_boundary = chunk_cancellation,
  missing_precedence = c(NaN, NA_real_, 1),
  opposing_infinities = c(Inf, -Inf),
  double_overflow = rep(.Machine$double.xmax, 2L),
  scaled_rounding_overflow = rep(.Machine$double.xmax, 3L),
  signed_zero = -0,
  empty = numeric()
)

narm_reduction_cases <- list(
  finite_missing = c(1, NA_real_, 2, NaN, 3),
  cancellation = missing_cancellation,
  chunk_boundary = chunk_missing_cancellation,
  all_missing = c(NA_real_, NaN),
  opposing_infinities = c(Inf, NA_real_, -Inf),
  double_overflow = c(.Machine$double.xmax, NA_real_, .Machine$double.xmax),
  scaled_rounding_overflow = c(rep(.Machine$double.xmax, 3L), NA_real_),
  signed_zero = -0,
  empty = numeric()
)

list_reduction_cases <- list(
  finite = list(c(-4, 1), c(2, 8)),
  cancellation = list(cancellation, cancellation),
  chunk_boundary = list(chunk_cancellation, cancellation),
  missing_precedence = list(c(NaN, NA_real_, 1), c(2, 3)),
  opposing_infinities = list(c(Inf, 1), c(-Inf)),
  double_overflow = list(.Machine$double.xmax, .Machine$double.xmax),
  signed_zero = list(-0),
  empty = list()
)

reduction_oracle_tests <- list(
  list(
    name = "sum_exact_r_oracle",
    run = function() check_vector_reduction_oracle("sum", sum, reduction_cases)
  ),
  list(
    name = "mean_exact_r_oracle",
    run = function() check_vector_reduction_oracle("mean", mean, reduction_cases)
  ),
  list(
    name = "sum_narm_exact_r_oracle",
    run = function() check_vector_reduction_oracle(
      "sum_narm", function(value) sum(value, na.rm = TRUE),
      narm_reduction_cases
    )
  ),
  list(
    name = "mean_narm_exact_r_oracle",
    run = function() check_vector_reduction_oracle(
      "mean_narm", function(value) mean(value, na.rm = TRUE),
      narm_reduction_cases
    )
  ),
  list(
    name = "list_reduction_exact_r_oracle",
    run = function() check_list_reduction_oracle(list_reduction_cases)
  ),
  list(
    name = "reduction_resource_contract",
    run = check_reduction_resource_contract
  )
)

if (gctorture_mode) cat("  GC diagnostic mode: gctorture2 step", gctorture_step, "around each registered call\n")

if (!reduction_only) for (t in tests) {
  name <- if (is.list(t)) t$name else t
  expect_error <- if (is.list(t) && !is.null(t$expect_error)) t$expect_error else FALSE

  result <- tryCatch({
    run_with_gctorture({
      val <- .Call(name)
      if (expect_error) {
        "FAIL"
      } else {
        if (is.numeric(val) && length(val) == 1 && !is.na(val) && val == 1.0) {
          "PASS"
        } else {
          "FAIL"
        }
      }
    })
  }, error = function(e) {
    if (expect_error) {
      "PASS"
    } else {
      "FAIL"
    }
  })

  if (result == "PASS") {
    cat("  PASS:", name, "\n")
    passed <- passed + 1
  } else if (result == "FAIL") {
    cat("  FAIL:", name, "\n")
    failed <- failed + 1
  } else {
    cat("  SKIP:", name, "-", result, "\n")
    skipped <- skipped + 1
  }
}

for (test in reduction_oracle_tests) {
  result <- tryCatch(
    run_with_gctorture(test$run()),
    error = function(error) error
  )
  if (identical(result, TRUE)) {
    cat("  PASS:", test$name, "\n")
    passed <- passed + 1
  } else {
    cat("  FAIL:", test$name, "\n")
    cat("    ", conditionMessage(result), "\n", sep = "")
    failed <- failed + 1
  }
}

if (reduction_only) {
  cat(sprintf("\nResults: %d passed, %d failed, %d skipped\n", passed, failed, skipped))
  quit(status = if (failed > 0) 1L else 0L)
}

warning_message <- NULL
warning_result <- withCallingHandlers(
  run_with_gctorture(.Call("zigr_test_error_warn_format")),
  warning = function(w) {
    warning_message <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
if (identical(warning_result, 1) && identical(warning_message, "zigr warning format %s")) {
  cat("  PASS: zigr_test_error_warn_format\n")
  passed <- passed + 1
} else {
  cat("  FAIL: zigr_test_error_warn_format\n")
  failed <- failed + 1
}

registration_result <- tryCatch(
  run_with_gctorture(run_generated_registration_arity_test()),
  error = function(e) e
)
if (identical(registration_result, TRUE)) {
  cat("  PASS: generated_registration_every_supported_arity\n")
  passed <- passed + 1
} else {
  cat("  FAIL: generated_registration_every_supported_arity\n")
  cat("    ", conditionMessage(registration_result), "\n", sep = "")
  failed <- failed + 1
}

run_altrep_persistence_test <- function() {
  fixture_root <- tempfile("zigr-altrep-package-")
  package_source <- file.path(fixture_root, "zigr")
  package_library <- file.path(fixture_root, "library")
  dir.create(file.path(package_source, "R"), recursive = TRUE)
  dir.create(package_library, recursive = TRUE)
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  writeLines(c(
    "Package: zigr",
    "Type: Package",
    "Title: zigr ALTREP Persistence Fixture",
    "Version: 0.0.0.9000",
    "Authors@R: person('zigr', 'tests', email = 'tests@example.invalid', role = c('aut', 'cre'))",
    "Description: Loads the zigr runtime test library for persistent ALTREP restoration.",
    "License: MIT",
    "Encoding: UTF-8"
  ), file.path(package_source, "DESCRIPTION"))
  writeLines("", file.path(package_source, "NAMESPACE"))
  writeLines(c(
    ".onLoad <- function(libname, pkgname) {",
    "  dyn.load(Sys.getenv('ZIGR_TEST_SO'))",
    "}"
  ), file.path(package_source, "R", "zzz.R"))

  r_binary <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
  rscript_binary <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  install_output <- system2(
    r_binary,
    c("CMD", "INSTALL", "--no-multiarch", "--no-test-load", paste0("--library=", package_library), package_source),
    stdout = TRUE,
    stderr = TRUE
  )
  if (!is.null(attr(install_output, "status"))) return(list(ok = FALSE, output = install_output))

  persistence_file <- file.path(fixture_root, "owned-altrep.rds")
  child_script <- file.path(fixture_root, "restore.R")
  saveRDS(.Call("zigr_test_altrep_persistence_fixture"), persistence_file, version = 3)
  quote_r_string <- function(value) encodeString(normalizePath(value), quote = '"')
  writeLines(c(
    paste0(".libPaths(c(", quote_r_string(package_library), ", .libPaths()))"),
    paste0("Sys.setenv(ZIGR_TEST_SO = ", quote_r_string(so_path), ")"),
    "gctorture(TRUE)",
    paste0("value <- readRDS(", quote_r_string(persistence_file), ")"),
    "gctorture(FALSE)",
    "stopifnot(identical(.Call('zigr_test_altrep_persistence_check', value), 1))"
  ), child_script)
  child_output <- system2(rscript_binary, child_script, stdout = TRUE, stderr = TRUE)
  list(ok = is.null(attr(child_output, "status")), output = child_output)
}

persistence_result <- tryCatch(
  run_altrep_persistence_test(),
  error = function(e) list(ok = FALSE, output = conditionMessage(e))
)
if (persistence_result$ok) {
  cat("  PASS: zigr_test_altrep_cross_process_persistence\n")
  passed <- passed + 1
} else {
  cat("  FAIL: zigr_test_altrep_cross_process_persistence\n")
  cat(paste0("    ", persistence_result$output, collapse = "\n"), "\n")
  failed <- failed + 1
}

cat(sprintf("\nResults: %d passed, %d failed, %d skipped\n", passed, failed, skipped))
if (failed > 0) quit(status = 1)
