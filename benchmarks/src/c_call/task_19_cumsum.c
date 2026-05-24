// Task 19: Cumulative sum.
#include <Rinternals.h>

SEXP c_call_bench_cumsum(SEXP vec_sexp) {
  R_xlen_t n = XLENGTH(vec_sexp);
  double *src = REAL(vec_sexp);

  SEXP result = PROTECT(allocVector(REALSXP, n));
  double *rp = REAL(result);

  double total = 0.0;
  for (R_xlen_t i = 0; i < n; i++) {
    total += src[i];
    rp[i] = total;
  }

  UNPROTECT(1);
  return result;
}
