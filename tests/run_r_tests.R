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
  global_cache <- Sys.getenv("ZIG_GLOBAL_CACHE_DIR", file.path("benchmarks", ".zig-global-cache"))
  dir.create(global_cache, recursive = TRUE, showWarnings = FALSE)
  optimize <- Sys.getenv("ZIGR_OPTIMIZE", "ReleaseFast")
  build_args <- c(
    "build", "rtest",
    paste0("-Doptimize=", optimize),
    paste0("-Dr-include=", r_include),
    paste0("-Dr-lib=", r_lib),
    "--cache-dir", ".zig-cache",
    "--global-cache-dir", global_cache
  )
  target <- Sys.getenv("ZIGR_TARGET", "")
  if (nzchar(target) && target != "native") build_args <- c(build_args, paste0("-Dtarget=", target))
  cpu_features <- Sys.getenv("ZIGR_CPU_FEATURES", "")
  if (nzchar(cpu_features) && cpu_features != "default") build_args <- c(build_args, paste0("-Dcpu=", cpu_features))
  checked_sexp <- tolower(Sys.getenv("ZIGR_CHECKED_SEXP", "false"))
  if (checked_sexp %in% c("1", "true", "yes")) build_args <- c(build_args, "-Dchecked-sexp=true")

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
dyn.load(so_path)

tests <- list(
  "zigr_test_protect",
  "zigr_test_return42",
  "zigr_test_abi_contract",
  list(name="zigr_test_longjmp", expect_error=TRUE),
  "zigr_longjmp_flag",
  "zigr_test_longjmp_normal",

  list(name="zigr_test_error_signal", expect_error=TRUE),
  "zigr_test_error_warn",
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
  "zigr_test_df_names_longjmp",

  "zigr_test_altrep_create",
  "zigr_test_altrep_sum_simd",
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
  "zigr_test_altrep_mean_simd",
  "zigr_test_altrep_norm2_simd",
  "zigr_test_altrep_min_simd",
  "zigr_test_altrep_max_simd",
  "zigr_test_altrep_argmin_simd",
  "zigr_test_altrep_argmax_simd",
  "zigr_test_altrep_sum_narm_simd",
  "zigr_test_altrep_mean_narm_simd",
  "zigr_test_altraw_create",
  "zigr_test_altcomplex_create",
  "zigr_test_altstring_create",
  "zigr_test_string_representations",
  "zigr_test_altstring_inputs",
  "zigr_test_altrep_owned_storage",
  "zigr_test_altrep_summary_contract",
  "zigr_test_altrep_empty_callback_contract",
  "zigr_test_altrep_real_callback_contract",
  "zigr_test_altrep_typed_callback_contract",
  "zigr_test_altrep_complex_string_callback_contract",
  "zigr_test_borrowed_views",
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
  "zigr_test_generated_method_receiver_errors",
  "zigr_test_generated_ownership_gc",
  "zigr_test_allocation_diagnostics",
  "zigr_test_cleanup_diagnostics",
  list(name="zigr_test_generated_spill_longjmp", expect_error=TRUE),
  "zigr_test_generated_result_longjmp",
  "zigr_test_externalptr_finalizer",
  "zigr_test_externalptr_finalizer_idempotent",
  "zigr_test_externalptr_typed_protected",
  "zigr_test_externalptr_lazy_tag_gc",
  "zigr_test_weakref_reachable_contract",
  "zigr_test_weakref_checked_errors",
  "zigr_test_weakref_gc_finalizer",
  "zigr_test_raw_views",
  "zigr_test_complex_boundary",
  "zigr_test_generated_string_shapes",
  "zigr_test_string_allocation_longjmp",
  "zigr_test_conversion_allocation_longjmp",

  list(name="zigr_test_export_from_sexp_wrong_type", expect_error=TRUE),

  "zigr_alloc_real",
  "zigr_alloc_large",
  "zigr_protect_many",
  "zigr_protect_index",
  "zigr_check_na",
  "zigr_raise_warning",
  "zigr_typeof_nil",

  "zigr_test_cleanup_fires_on_longjmp",
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

for (t in tests) {
  name <- if (is.list(t)) t$name else t
  expect_error <- if (is.list(t) && !is.null(t$expect_error)) t$expect_error else FALSE

  result <- tryCatch({
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

cat(sprintf("\nResults: %d passed, %d failed, %d skipped\n", passed, failed, skipped))
if (failed > 0) quit(status = 1)
