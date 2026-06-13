#include <stdint.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "api.h"

static uintptr_t TAGGED_POINTER_MASK = (uintptr_t)1;

static SEXP handle_result(SEXP res_) {
  uintptr_t res = (uintptr_t)res_;

  if ((res & TAGGED_POINTER_MASK) == 1) {
    SEXP res_aligned = (SEXP)(res & ~TAGGED_POINTER_MASK);

    if (TYPEOF(res_aligned) == CHARSXP) {
      Rf_errorcall(R_NilValue, "%s", CHAR(res_aligned));
    } else {
      R_ContinueUnwind(res_aligned);
    }
  }

  return (SEXP)res;
}

#define SAVVY_WRAP1(name, arg1)               \
  SEXP savvy_##name##__impl(SEXP arg1) {     \
    SEXP res = savvy_##name##__ffi(arg1);    \
    return handle_result(res);               \
  }

#define SAVVY_WRAP2(name, arg1, arg2)                 \
  SEXP savvy_##name##__impl(SEXP arg1, SEXP arg2) {  \
    SEXP res = savvy_##name##__ffi(arg1, arg2);      \
    return handle_result(res);                       \
  }

SEXP savvy_bench_fib_recursive__impl(SEXP c_arg__n) {
  SEXP res = savvy_bench_fib_recursive__ffi(c_arg__n);
  return handle_result(res);
}

SAVVY_WRAP1(bench_vectorsum, c_arg__vec)
SAVVY_WRAP1(bench_transpose, c_arg__mat)
SAVVY_WRAP1(bench_matrix_transpose, c_arg__mat)
SAVVY_WRAP2(bench_strings, c_arg__vec, c_arg__sep)
SAVVY_WRAP1(bench_na_prop, c_arg__vec)
SAVVY_WRAP1(bench_parallel, c_arg__vec)
SAVVY_WRAP1(bench_protect_stress, c_arg__n)

SEXP savvy_bench_protect_shallow__impl(SEXP c_arg__vec) {
    SEXP res = savvy_bench_protect_shallow__ffi(c_arg__vec);
    return res;
}

SEXP savvy_bench_protect_scaling__impl(SEXP c_arg__vec) {
    SEXP res = savvy_bench_protect_scaling__ffi(c_arg__vec);
    return res;
}

SAVVY_WRAP2(bench_blas_matmul, c_arg__a, c_arg__b)
SAVVY_WRAP1(bench_crossprod, c_arg__x)
SAVVY_WRAP1(bench_cholesky, c_arg__a)
SAVVY_WRAP2(bench_lm, c_arg__x, c_arg__y)
SAVVY_WRAP1(bench_rowsums, c_arg__mat)
SAVVY_WRAP1(bench_matrix_rowsums, c_arg__mat)
SAVVY_WRAP1(bench_elem_ops, c_arg__vec)
SAVVY_WRAP1(bench_rowcol_means, c_arg__mat)
SAVVY_WRAP1(bench_matrix_rowcol_means, c_arg__mat)
SAVVY_WRAP2(bench_broadcast, c_arg__vec, c_arg__scalar)
SAVVY_WRAP1(bench_sort, c_arg__vec)
SAVVY_WRAP1(bench_list_access, c_arg__vec)
SAVVY_WRAP1(bench_string_concat, c_arg__vec)
SAVVY_WRAP1(bench_type_dispatch, c_arg__vec)
SAVVY_WRAP1(bench_cumsum, c_arg__vec)
SAVVY_WRAP1(bench_rnorm, c_arg__n)
SAVVY_WRAP1(bench_string_nchar, c_arg__vec)
SAVVY_WRAP1(bench_which_na, c_arg__vec)
SAVVY_WRAP1(bench_altrep_sum, c_arg__vec)
SAVVY_WRAP1(bench_altrep_read, c_arg__vec)
SAVVY_WRAP1(bench_altrep_create, c_arg__n)
SAVVY_WRAP1(bench_sexp_create, c_arg__n)
SAVVY_WRAP1(bench_owned_altrep_real_sum, c_arg__n)
SAVVY_WRAP1(bench_owned_altrep_int_sum, c_arg__n)
SAVVY_WRAP1(bench_owned_altrep_logical_sum, c_arg__n)
SAVVY_WRAP1(bench_comptime_dispatch, c_arg__inputs)
SAVVY_WRAP1(bench_struct_convert, c_arg__input)
SAVVY_WRAP1(bench_na_prop_vary, c_arg__inputs)
SAVVY_WRAP1(bench_scale_law, c_arg__inputs)
SAVVY_WRAP1(bench_arena_vs_rmalloc, c_arg__vec)
SAVVY_WRAP1(bench_prot_overhead, c_arg__vec)
SAVVY_WRAP1(bench_longjmp_safety, c_arg__vec)
SAVVY_WRAP1(bench_translate_c_cost, c_arg__vec)
SAVVY_WRAP1(bench_parallel_scaling, c_arg__vec)
SAVVY_WRAP1(bench_memory_bandwidth, c_arg__vec)
SAVVY_WRAP1(bench_sexp_inspect, c_arg__vec)

extern SEXP savvy_bench_string_encoding__impl(SEXP);
// savvy_bench_string_encoding__impl is defined directly in Rust
extern SEXP savvy_bench_factor_ops__impl(SEXP);
extern SEXP savvy_bench_attrib_ops__impl(SEXP);
extern SEXP savvy_bench_s4_slot_access__impl(SEXP);
extern SEXP savvy_bench_na_propagation__impl(SEXP);
extern SEXP savvy_bench_long_vector_idx__impl(SEXP);
extern SEXP savvy_bench_l1_arithmetic__impl(SEXP);
extern SEXP savvy_bench_altrep_materialize__impl(SEXP);
extern SEXP savvy_bench_altrep_elt_walk__impl(SEXP);
extern SEXP savvy_bench_altrep_region_read__impl(SEXP);
extern SEXP savvy_bench_altrep_sum_via_R__impl(SEXP);
extern SEXP savvy_bench_altrep_sum_native__impl(SEXP);
extern SEXP savvy_bench_altrep_min_max__impl(SEXP);
extern SEXP savvy_bench_altrep_no_na_query__impl(SEXP);
extern SEXP savvy_bench_r_eval__impl(SEXP);
extern SEXP savvy_bench_r_tryeval__impl(SEXP);
extern SEXP savvy_bench_serialize_roundtrip__impl(SEXP);
extern SEXP savvy_bench_external_ptr__impl(SEXP);
extern SEXP savvy_bench_rng_stress__impl(SEXP);
// savvy_bench_factor_ops__impl is defined directly in Rust

SEXP savvy_bench_string_variants__impl(SEXP c_arg__vec) {
  SEXP res = savvy_bench_string_variants_manual__ffi(c_arg__vec);
  return handle_result(res);
}

SEXP savvy_bench_dataframe__impl(SEXP c_arg__df) {
  SEXP res = savvy_bench_dataframe_manual__ffi(c_arg__df);
  return handle_result(res);
}

SEXP savvy_bench_dataframe_filter__impl(SEXP c_arg__df) {
  SEXP res = savvy_bench_dataframe_filter__ffi(c_arg__df);
  return handle_result(res);
}

static const R_CallMethodDef CallEntries[] = {
  {"savvy_bench_fib_recursive__impl", (DL_FUNC)&savvy_bench_fib_recursive__impl, 1},
  {"savvy_bench_vectorsum__impl", (DL_FUNC)&savvy_bench_vectorsum__impl, 1},
  {"savvy_bench_transpose__impl", (DL_FUNC)&savvy_bench_transpose__impl, 1},
  {"savvy_bench_matrix_transpose__impl", (DL_FUNC)&savvy_bench_matrix_transpose__impl, 1},
  {"savvy_bench_strings__impl", (DL_FUNC)&savvy_bench_strings__impl, 2},
  {"savvy_bench_dataframe__impl", (DL_FUNC)&savvy_bench_dataframe__impl, 1},
  {"savvy_bench_dataframe_filter__impl", (DL_FUNC)&savvy_bench_dataframe_filter__impl, 1},
  {"savvy_bench_na_prop__impl", (DL_FUNC)&savvy_bench_na_prop__impl, 1},
  {"savvy_bench_parallel__impl", (DL_FUNC)&savvy_bench_parallel__impl, 1},
  {"savvy_bench_protect_stress__impl", (DL_FUNC)&savvy_bench_protect_stress__impl, 1},
  {"savvy_bench_protect_shallow__impl", (DL_FUNC)&savvy_bench_protect_shallow__impl, 1},
  {"savvy_bench_protect_scaling__impl", (DL_FUNC)&savvy_bench_protect_scaling__impl, 1},
  {"savvy_bench_blas_matmul__impl", (DL_FUNC)&savvy_bench_blas_matmul__impl, 2},
  {"savvy_bench_crossprod__impl", (DL_FUNC)&savvy_bench_crossprod__impl, 1},
  {"savvy_bench_cholesky__impl", (DL_FUNC)&savvy_bench_cholesky__impl, 1},
  {"savvy_bench_lm__impl", (DL_FUNC)&savvy_bench_lm__impl, 2},
  {"savvy_bench_rowsums__impl", (DL_FUNC)&savvy_bench_rowsums__impl, 1},
  {"savvy_bench_matrix_rowsums__impl", (DL_FUNC)&savvy_bench_matrix_rowsums__impl, 1},
  {"savvy_bench_elem_ops__impl", (DL_FUNC)&savvy_bench_elem_ops__impl, 1},
  {"savvy_bench_rowcol_means__impl", (DL_FUNC)&savvy_bench_rowcol_means__impl, 1},
  {"savvy_bench_matrix_rowcol_means__impl", (DL_FUNC)&savvy_bench_matrix_rowcol_means__impl, 1},
  {"savvy_bench_broadcast__impl", (DL_FUNC)&savvy_bench_broadcast__impl, 2},
  {"savvy_bench_sort__impl", (DL_FUNC)&savvy_bench_sort__impl, 1},
  {"savvy_bench_list_access__impl", (DL_FUNC)&savvy_bench_list_access__impl, 1},
  {"savvy_bench_string_concat__impl", (DL_FUNC)&savvy_bench_string_concat__impl, 1},
  {"savvy_bench_type_dispatch__impl", (DL_FUNC)&savvy_bench_type_dispatch__impl, 1},
  {"savvy_bench_cumsum__impl", (DL_FUNC)&savvy_bench_cumsum__impl, 1},
  {"savvy_bench_rnorm__impl", (DL_FUNC)&savvy_bench_rnorm__impl, 1},
  {"savvy_bench_string_nchar__impl", (DL_FUNC)&savvy_bench_string_nchar__impl, 1},
  {"savvy_bench_string_encoding__impl", (DL_FUNC)&savvy_bench_string_encoding__impl, 1},
  {"savvy_bench_which_na__impl", (DL_FUNC)&savvy_bench_which_na__impl, 1},
  {"savvy_bench_altrep_sum__impl", (DL_FUNC)&savvy_bench_altrep_sum__impl, 1},
  {"savvy_bench_altrep_read__impl", (DL_FUNC)&savvy_bench_altrep_read__impl, 1},
  {"savvy_bench_altrep_create__impl", (DL_FUNC)&savvy_bench_altrep_create__impl, 1},
  {"savvy_bench_sexp_create__impl", (DL_FUNC)&savvy_bench_sexp_create__impl, 1},
  {"savvy_bench_owned_altrep_real_sum__impl", (DL_FUNC)&savvy_bench_owned_altrep_real_sum__impl, 1},
  {"savvy_bench_owned_altrep_int_sum__impl", (DL_FUNC)&savvy_bench_owned_altrep_int_sum__impl, 1},
  {"savvy_bench_owned_altrep_logical_sum__impl", (DL_FUNC)&savvy_bench_owned_altrep_logical_sum__impl, 1},
  {"savvy_bench_comptime_dispatch__impl", (DL_FUNC)&savvy_bench_comptime_dispatch__impl, 1},
  {"savvy_bench_struct_convert__impl", (DL_FUNC)&savvy_bench_struct_convert__impl, 1},
  {"savvy_bench_na_prop_vary__impl", (DL_FUNC)&savvy_bench_na_prop_vary__impl, 1},
  {"savvy_bench_scale_law__impl", (DL_FUNC)&savvy_bench_scale_law__impl, 1},
  {"savvy_bench_arena_vs_rmalloc__impl", (DL_FUNC)&savvy_bench_arena_vs_rmalloc__impl, 1},
  {"savvy_bench_prot_overhead__impl", (DL_FUNC)&savvy_bench_prot_overhead__impl, 1},
  {"savvy_bench_longjmp_safety__impl", (DL_FUNC)&savvy_bench_longjmp_safety__impl, 1},
  {"savvy_bench_translate_c_cost__impl", (DL_FUNC)&savvy_bench_translate_c_cost__impl, 1},
  {"savvy_bench_parallel_scaling__impl", (DL_FUNC)&savvy_bench_parallel_scaling__impl, 1},
  {"savvy_bench_memory_bandwidth__impl", (DL_FUNC)&savvy_bench_memory_bandwidth__impl, 1},
  {"savvy_bench_sexp_inspect__impl", (DL_FUNC)&savvy_bench_sexp_inspect__impl, 1},
  {"savvy_bench_factor_ops__impl", (DL_FUNC)&savvy_bench_factor_ops__impl, 1},
  {"savvy_bench_attrib_ops__impl", (DL_FUNC)&savvy_bench_attrib_ops__impl, 1},
  {"savvy_bench_s4_slot_access__impl", (DL_FUNC)&savvy_bench_s4_slot_access__impl, 1},
  {"savvy_bench_na_propagation__impl", (DL_FUNC)&savvy_bench_na_propagation__impl, 1},
  {"savvy_bench_altrep_materialize__impl", (DL_FUNC)&savvy_bench_altrep_materialize__impl, 1},
  {"savvy_bench_altrep_elt_walk__impl", (DL_FUNC)&savvy_bench_altrep_elt_walk__impl, 1},
  {"savvy_bench_altrep_region_read__impl", (DL_FUNC)&savvy_bench_altrep_region_read__impl, 1},
  {"savvy_bench_altrep_sum_via_R__impl", (DL_FUNC)&savvy_bench_altrep_sum_via_R__impl, 1},
  {"savvy_bench_altrep_sum_native__impl", (DL_FUNC)&savvy_bench_altrep_sum_native__impl, 1},
  {"savvy_bench_altrep_min_max__impl", (DL_FUNC)&savvy_bench_altrep_min_max__impl, 1},
  {"savvy_bench_altrep_no_na_query__impl", (DL_FUNC)&savvy_bench_altrep_no_na_query__impl, 1},
  {"savvy_bench_r_eval__impl", (DL_FUNC)&savvy_bench_r_eval__impl, 1},
  {"savvy_bench_r_tryeval__impl", (DL_FUNC)&savvy_bench_r_tryeval__impl, 1},
  {"savvy_bench_serialize_roundtrip__impl", (DL_FUNC)&savvy_bench_serialize_roundtrip__impl, 1},
  {"savvy_bench_external_ptr__impl", (DL_FUNC)&savvy_bench_external_ptr__impl, 1},
  {"savvy_bench_rng_stress__impl", (DL_FUNC)&savvy_bench_rng_stress__impl, 1},
  {"savvy_bench_long_vector_idx__impl", (DL_FUNC)&savvy_bench_long_vector_idx__impl, 1},
  {"savvy_bench_l1_arithmetic__impl", (DL_FUNC)&savvy_bench_l1_arithmetic__impl, 1},
  {"savvy_bench_string_variants__impl", (DL_FUNC)&savvy_bench_string_variants__impl, 1},
  {NULL, NULL, 0}
};

void R_init_savvy_benchmarks(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}