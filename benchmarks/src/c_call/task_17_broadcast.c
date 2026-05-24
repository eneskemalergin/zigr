// Task 17: Vector + scalar broadcast.
#include <Rinternals.h>

SEXP c_call_bench_broadcast(SEXP vec_sexp, SEXP scalar_sexp) {
  R_xlen_t n = XLENGTH(vec_sexp);
  double *src = REAL(vec_sexp);
  double scalar = REAL(scalar_sexp)[0];

  SEXP result = PROTECT(allocVector(REALSXP, n));
  double *rp = REAL(result);

  for (R_xlen_t i = 0; i < n; i++) rp[i] = src[i] + scalar;

  UNPROTECT(1);
  return result;
}
