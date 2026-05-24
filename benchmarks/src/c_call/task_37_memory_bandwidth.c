// Task 37: Memory bandwidth sweep across copy and fill paths.
#include <Rinternals.h>
#include <string.h>
#include <stdlib.h>

#define MEMORY_BANDWIDTH_REPEATS 2

static double sum_buffer(const double *data, R_xlen_t n) {
  double total = 0.0;
  for (R_xlen_t i = 0; i < n; ++i) total += data[i];
  return total;
}

SEXP c_call_bench_memory_bandwidth(SEXP vec) {
  const double *src = REAL(vec);
  R_xlen_t n = XLENGTH(vec);
  SEXP result = PROTECT(allocVector(REALSXP, 3));
  SEXP names = PROTECT(allocVector(STRSXP, 3));
  double *out = REAL(result);

  double copy_temp_total = 0.0;
  double copy_out_total = 0.0;
  double fill_out_total = 0.0;

  for (int repeat = 0; repeat < MEMORY_BANDWIDTH_REPEATS; ++repeat) {
    double *tmp = (double *)malloc((size_t)n * sizeof(double));
    if (tmp == NULL) {
      UNPROTECT(2);
      error("malloc failed in c_call_bench_memory_bandwidth");
    }
    memcpy(tmp, src, (size_t)n * sizeof(double));
    copy_temp_total += sum_buffer(tmp, n);
    free(tmp);

    SEXP copy_out = PROTECT(allocVector(REALSXP, n));
    memcpy(REAL(copy_out), src, (size_t)n * sizeof(double));
    copy_out_total += sum_buffer(REAL(copy_out), n);
    UNPROTECT(1);

    SEXP fill_out = PROTECT(allocVector(REALSXP, n));
    double *fill_ptr = REAL(fill_out);
    for (R_xlen_t i = 0; i < n; ++i) fill_ptr[i] = src[i] + 0.5;
    fill_out_total += sum_buffer(fill_ptr, n);
    UNPROTECT(1);
  }

  out[0] = copy_temp_total;
  out[1] = copy_out_total;
  out[2] = fill_out_total;

  SET_STRING_ELT(names, 0, mkChar("copy_temp"));
  SET_STRING_ELT(names, 1, mkChar("copy_out"));
  SET_STRING_ELT(names, 2, mkChar("fill_out"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(2);
  return result;
}