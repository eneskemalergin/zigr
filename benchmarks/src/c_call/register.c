// Registration table for all C_Call benchmark tasks.
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Forward declarations with correct parameter lists
extern SEXP c_call_bench_fib(SEXP);
extern SEXP c_call_bench_vectorsum(SEXP);
extern SEXP c_call_bench_transpose(SEXP);
extern SEXP c_call_bench_strings(SEXP, SEXP);
extern SEXP c_call_bench_dataframe(SEXP);
extern SEXP c_call_bench_na_prop(SEXP);
extern SEXP c_call_bench_parallel(SEXP);
extern SEXP c_call_bench_protect_stress(SEXP);
extern SEXP c_call_bench_blas_matmul(SEXP, SEXP);
extern SEXP c_call_bench_crossprod(SEXP);
extern SEXP c_call_bench_cholesky(SEXP);
extern SEXP c_call_bench_lm(SEXP, SEXP);
extern SEXP c_call_bench_rowsums(SEXP);
extern SEXP c_call_bench_elem_ops(SEXP);
extern SEXP c_call_bench_rowcol_means(SEXP);
extern SEXP c_call_bench_broadcast(SEXP, SEXP);
extern SEXP c_call_bench_sort(SEXP);
extern SEXP c_call_bench_cumsum(SEXP);
extern SEXP c_call_bench_rnorm(SEXP);
extern SEXP c_call_bench_string_nchar(SEXP);
extern SEXP c_call_bench_which_na(SEXP);
extern SEXP c_call_bench_altrep_sum(SEXP);
extern SEXP c_call_bench_altrep_read(SEXP);
extern SEXP c_call_bench_altrep_create(SEXP);
extern SEXP c_call_bench_comptime_dispatch(SEXP);
extern SEXP c_call_bench_struct_convert(SEXP);
extern SEXP c_call_bench_na_prop_vary(SEXP);
extern SEXP c_call_bench_scale_law(SEXP);
extern SEXP c_call_bench_arena_vs_rmalloc(SEXP);
extern SEXP c_call_bench_prot_overhead(SEXP);
extern SEXP c_call_bench_longjmp_safety(SEXP);
extern SEXP c_call_bench_translate_c_cost(SEXP);
extern SEXP c_call_bench_string_variants(SEXP);
extern SEXP c_call_bench_parallel_scaling(SEXP);
extern SEXP c_call_bench_memory_bandwidth(SEXP);
extern SEXP c_call_bench_owned_altrep_real_sum(SEXP);
extern SEXP c_call_bench_owned_altrep_int_sum(SEXP);
extern SEXP c_call_bench_owned_altrep_logical_sum(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"c_call_bench_fib",            (DL_FUNC) &c_call_bench_fib,            1},
  {"c_call_bench_vectorsum",      (DL_FUNC) &c_call_bench_vectorsum,      1},
  {"c_call_bench_transpose",         (DL_FUNC) &c_call_bench_transpose,         1},
  {"c_call_bench_strings",        (DL_FUNC) &c_call_bench_strings,        2},
  {"c_call_bench_dataframe",      (DL_FUNC) &c_call_bench_dataframe,      1},
  {"c_call_bench_na_prop",        (DL_FUNC) &c_call_bench_na_prop,        1},
  {"c_call_bench_parallel",       (DL_FUNC) &c_call_bench_parallel,       1},
  {"c_call_bench_protect_stress", (DL_FUNC) &c_call_bench_protect_stress, 1},
  {"c_call_bench_blas_matmul",    (DL_FUNC) &c_call_bench_blas_matmul,    2},
  {"c_call_bench_crossprod",      (DL_FUNC) &c_call_bench_crossprod,      1},
  {"c_call_bench_cholesky",       (DL_FUNC) &c_call_bench_cholesky,       1},
  {"c_call_bench_lm",             (DL_FUNC) &c_call_bench_lm,             2},
  {"c_call_bench_rowsums",        (DL_FUNC) &c_call_bench_rowsums,        1},
  {"c_call_bench_elem_ops",       (DL_FUNC) &c_call_bench_elem_ops,       1},
  {"c_call_bench_rowcol_means",   (DL_FUNC) &c_call_bench_rowcol_means,   1},
  {"c_call_bench_broadcast",      (DL_FUNC) &c_call_bench_broadcast,      2},
  {"c_call_bench_sort",           (DL_FUNC) &c_call_bench_sort,           1},
  {"c_call_bench_cumsum",         (DL_FUNC) &c_call_bench_cumsum,         1},
  {"c_call_bench_rnorm",          (DL_FUNC) &c_call_bench_rnorm,          1},
  {"c_call_bench_string_nchar",   (DL_FUNC) &c_call_bench_string_nchar,   1},
  {"c_call_bench_which_na",       (DL_FUNC) &c_call_bench_which_na,       1},
  {"c_call_bench_altrep_sum",    (DL_FUNC) &c_call_bench_altrep_sum,    1},
  {"c_call_bench_altrep_read",   (DL_FUNC) &c_call_bench_altrep_read,   1},
  {"c_call_bench_altrep_create", (DL_FUNC) &c_call_bench_altrep_create, 1},
  {"c_call_bench_comptime_dispatch", (DL_FUNC) &c_call_bench_comptime_dispatch, 1},
  {"c_call_bench_struct_convert", (DL_FUNC) &c_call_bench_struct_convert, 1},
  {"c_call_bench_na_prop_vary", (DL_FUNC) &c_call_bench_na_prop_vary, 1},
  {"c_call_bench_scale_law", (DL_FUNC) &c_call_bench_scale_law, 1},
  {"c_call_bench_arena_vs_rmalloc", (DL_FUNC) &c_call_bench_arena_vs_rmalloc, 1},
  {"c_call_bench_prot_overhead", (DL_FUNC) &c_call_bench_prot_overhead, 1},
  {"c_call_bench_longjmp_safety", (DL_FUNC) &c_call_bench_longjmp_safety, 1},
  {"c_call_bench_translate_c_cost", (DL_FUNC) &c_call_bench_translate_c_cost, 1},
  {"c_call_bench_string_variants", (DL_FUNC) &c_call_bench_string_variants, 1},
  {"c_call_bench_parallel_scaling", (DL_FUNC) &c_call_bench_parallel_scaling, 1},
  {"c_call_bench_memory_bandwidth", (DL_FUNC) &c_call_bench_memory_bandwidth, 1},
  {"c_call_bench_owned_altrep_real_sum", (DL_FUNC) &c_call_bench_owned_altrep_real_sum, 1},
  {"c_call_bench_owned_altrep_int_sum", (DL_FUNC) &c_call_bench_owned_altrep_int_sum, 1},
  {"c_call_bench_owned_altrep_logical_sum", (DL_FUNC) &c_call_bench_owned_altrep_logical_sum, 1},
  {NULL, NULL, 0}
};

void R_init_c_call_benchmarks(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
