// Task 20: Generate n N(0,1) random variates.
#include <Rinternals.h>
#include <R_ext/Random.h>

SEXP c_call_bench_rnorm(SEXP n_sexp) {
  int n = INTEGER(n_sexp)[0];

  GetRNGstate();

  SEXP result = PROTECT(allocVector(REALSXP, n));
  double *rp = REAL(result);
  for (int i = 0; i < n; i++) rp[i] = norm_rand();

  PutRNGstate();
  UNPROTECT(1);
  return result;
}
