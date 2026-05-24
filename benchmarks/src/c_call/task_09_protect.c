// Task 9: PROTECT 1e5 SEXPs, then UNPROTECT all.
#include <Rinternals.h>

SEXP c_call_bench_protect_stress(SEXP n_sexp) {
  int n = INTEGER(n_sexp)[0];
  for (int i = 0; i < n; i++) {
    PROTECT(allocVector(REALSXP, 1));
  }
  UNPROTECT(n);
  return ScalarInteger(0);
}
