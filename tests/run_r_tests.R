#!/usr/bin/env Rscript
# Run zigr's R-dependent tests.
# Builds a test .so via `zig build rtest`, loads it in R, and runs every
# exported test function. Test functions return R.Rf_ScalarReal(1.0) on pass
# or R.Rf_ScalarReal(0.0) on fail. Functions that deliberately signal R errors
# are wrapped in tryCatch and expected to fail.

build_so <- function() {
  zig <- Sys.getenv("ZIG", "./zig-0.16.0/zig")
  r_include <- Sys.getenv("R_INCLUDE", file.path(R.home(), "include"))
  r_lib <- Sys.getenv("R_LIB", file.path(R.home(), "lib"))

  # Build using the project's own build.zig which wires the full module graph
  cat("Building test .so...\n")
  build_cmd <- paste(zig, "build", "rtest",
    "-Doptimize=ReleaseFast",
    paste0("-Dr-include=", r_include),
    paste0("-Dr-lib=", r_lib))
  cat("  ", build_cmd, "\n")
  exit_code <- system(build_cmd, intern = FALSE)
  if (exit_code != 0) {
    stop("zig build rtest failed with exit code ", exit_code)
  }

  # Locate the built .so in zig-out/lib/
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

# ── Test registry ────────────────────────────────────
# Each entry: name (exported C symbol) or list(name, expect_error=FALSE)
# expect_error=TRUE means the test deliberately signals an R error.
# When expect_error=TRUE, the test passes if the error is caught.

tests <- list(
  # Basic creation and protect tests
  "zigr_test_protect",
  "zigr_test_return42",
  list(name="zigr_test_longjmp", expect_error=TRUE),
  "zigr_test_longjmp_normal",

  # Error signaling
  list(name="zigr_test_error_signal", expect_error=TRUE),
  "zigr_test_error_warn",
  list(name="zigr_test_error_signalif", expect_error=TRUE),
  list(name="zigr_raise_error", expect_error=TRUE),

  # Interrupt and stack
  "zigr_test_interrupt",
  "zigr_test_check_stack",

  # Reverse FFI
  "zigr_test_rev_eval",
  "zigr_test_rev_define_find",
  "zigr_test_rev_lang3",

  # RNG
  "zigr_test_rng",

  # Memory allocator
  "zigr_test_ralloc",

  # Preserve and longjmp
  list(name="zigr_test_preserve_longjmp", expect_error=TRUE),

  # Type conversion
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

  # Data frames
  "zigr_test_df_build",
  "zigr_test_df_column",

  # Attributes
  "zigr_test_attrib_class",
  "zigr_test_attrib_names",

  # ALTREP creation and SIMD helpers
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

  # Edge-case / adversarial
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
  "zigr_test_optional_real_na_to_null",
  "zigr_test_optional_int_na_to_null",
  "zigr_test_optional_bool_na_to_null",
  "zigr_test_pmin_recycling",
  "zigr_test_pmax_recycling",

  # Phase 4: embed, struct conversion, tryCatch
  "zigr_phase4_embed_sum",
  "zigr_phase4_embed_paste",
  "zigr_phase4_raw_eval",
  "zigr_phase4_as_sexp",
  "zigr_phase4_from_sexp",
  list(name="zigr_phase4_embed_empty", expect_error=FALSE),  # caught by tryCatch internally
  "zigr_phase4_embed_vector",
  "zigr_phase4_embed_braces",
  "zigr_phase4_embed_null",
  "zigr_phase4_as_sexp_empty",
  "zigr_phase4_as_sexp_nested",
  "zigr_phase4_from_sexp_optional",
  "zigr_phase4_from_sexp_optional_present",
  "zigr_phase4_stress_protect",
  "zigr_phase4_stress_embed",
  list(name="zigr_p4_embed_syntax_error", expect_error=FALSE),
  list(name="zigr_p4_embed_stop_error", expect_error=FALSE),
  "zigr_p4_embed_warning_noerror",
  "zigr_p4_embed_unicode",
  "zigr_p4_embed_long_code",
  "zigr_p4_struct_roundtrip",
  "zigr_p4_struct_nan_inf",
  "zigr_p4_struct_neg_zero",
  "zigr_p4_struct_many_fields",
  list(name="zigr_p4_trycatch_nested", expect_error=FALSE),
  "zigr_p4_stress_protect_10k",

  # lang builder tests
  "zigr_test_build_call",
  "zigr_test_build_named_call",

  # ScopedProtect lifecycle
  "zigr_test_scoped_release",
  "zigr_test_scoped_get",

  # RVector runtime tests
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

  # Export system conversion tests
  # Export system: multi-parameter unwrapping
  "zigr_test_export_two_scalars",

  # Export system: complex slice conversion
  "zigr_test_export_complex_sum",

  # Export system: numeric and string slice conversion
  "zigr_test_export_sum",
  "zigr_test_export_string_lengths",
  "zigr_test_export_string_na",
  "zigr_test_export_cached_string_lengths",
  "zigr_test_export_sum_empty",
  "zigr_test_export_raw_roundtrip",

  # Export system: optional and NA handling
  "zigr_test_export_optional_null",
  "zigr_test_export_scalar_na",
  "zigr_test_export_int_scalar_na",
  "zigr_test_export_bool_scalar_na",
  "zigr_test_export_wrong_type_real",

  # Export system: external pointer method dispatch
  "zigr_test_export_method_call",
  "zigr_test_export_method_tag",

  # Export system: external interface
  "zigr_test_export_external",

  # Export system: generateMethods comptime codegen
  "zigr_test_export_generatemethods",

  # Export system: error signaling
  list(name="zigr_test_export_from_sexp_wrong_type", expect_error=TRUE),

  # Infrastructure and creation tests
  "zigr_alloc_real",
  "zigr_alloc_large",
  "zigr_protect_many",
  "zigr_protect_index",
  "zigr_check_na",
  "zigr_raise_warning",
  "zigr_typeof_nil",

  # Query functions for longjmp/preserve callback tests
  # zigr_longjmp_flag, zigr_preserve_flag, zigr_nested_flags
  # are called internally by the test suite. Not run directly.

  # Longjmp safety (catch internally, no expect_error)
  "zigr_test_cleanup_fires_on_longjmp",
  "zigr_test_with_rng_longjmp",

  # Recursive fib (call overhead showcase)
  "zigr_test_fib_recursive",

  # Additional conversion and call builder tests
  "zigr_test_lang_builder"
)

# ── Test runner ──────────────────────────────────────

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
      # Expected an error but got a return value, so test failed
      "FAIL"
    } else {
      # Check that the return value indicates pass
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
