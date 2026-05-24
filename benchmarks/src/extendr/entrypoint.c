#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void R_init_extendr_benchmarks_extendr(void *dll);
void register_extendr_panic_hook(void);
SEXP bench_arena_vs_rmalloc_manual__ffi(SEXP c_arg__vec);
SEXP bench_prot_overhead_manual__ffi(SEXP c_arg__vec);
SEXP bench_longjmp_safety_manual__ffi(SEXP c_arg__vec);
SEXP bench_translate_c_cost_manual__ffi(SEXP c_arg__vec);
SEXP bench_string_variants_manual__ffi(SEXP c_arg__vec);
SEXP bench_parallel_scaling_manual__ffi(SEXP c_arg__vec);
SEXP bench_memory_bandwidth_manual__ffi(SEXP c_arg__vec);

SEXP manual_wrap__bench_arena_vs_rmalloc(SEXP c_arg__vec) {
  return bench_arena_vs_rmalloc_manual__ffi(c_arg__vec);
}

SEXP manual_wrap__bench_prot_overhead(SEXP c_arg__vec) {
  return bench_prot_overhead_manual__ffi(c_arg__vec);
}

SEXP manual_wrap__bench_longjmp_safety(SEXP c_arg__vec) {
  return bench_longjmp_safety_manual__ffi(c_arg__vec);
}

SEXP manual_wrap__bench_translate_c_cost(SEXP c_arg__vec) {
  return bench_translate_c_cost_manual__ffi(c_arg__vec);
}

SEXP manual_wrap__bench_string_variants(SEXP c_arg__vec) {
  return bench_string_variants_manual__ffi(c_arg__vec);
}

SEXP manual_wrap__bench_parallel_scaling(SEXP c_arg__vec) {
  return bench_parallel_scaling_manual__ffi(c_arg__vec);
}

SEXP manual_wrap__bench_memory_bandwidth(SEXP c_arg__vec) {
  return bench_memory_bandwidth_manual__ffi(c_arg__vec);
}

void R_init_extendr_benchmarks(void *dll) {
  register_extendr_panic_hook();
  R_init_extendr_benchmarks_extendr(dll);
  R_useDynamicSymbols((DllInfo *)dll, TRUE);
}