// Task 15: Element-wise operations (abs, log, exp, sqrt).
#include <Rinternals.h>
#include <math.h>

SEXP c_call_bench_elem_ops(SEXP vec_sexp) {
  R_xlen_t n = XLENGTH(vec_sexp);
  double *src = REAL(vec_sexp);

  SEXP result = PROTECT(allocMatrix(REALSXP, n, 4));
  double *rp = REAL(result);

  for (R_xlen_t i = 0; i < n; i++) {
    double v = src[i];
    rp[i] = fabs(v);
    rp[i + n] = v > 0 ? log(v) : 0.0;
    rp[i + 2 * n] = exp(v);
    rp[i + 3 * n] = v >= 0 ? sqrt(v) : 0.0;
  }

  UNPROTECT(1);
  return result;
}
