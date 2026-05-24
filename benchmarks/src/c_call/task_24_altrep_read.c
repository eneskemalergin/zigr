#include <Rinternals.h>

// Read first and last elements of an ALTREP compact integer sequence.
// Uses INTEGER_GET_REGION to read individual elements through the
// ALTREP method table without materializing the entire vector.
// (Same approach as zigR.)
SEXP c_call_bench_altrep_read(SEXP sexp) {
  R_xlen_t n = XLENGTH(sexp);
  int first, last;
  INTEGER_GET_REGION(sexp, 0, 1, &first);
  INTEGER_GET_REGION(sexp, n - 1, 1, &last);
  SEXP res = PROTECT(allocVector(INTSXP, 2));
  INTEGER(res)[0] = first;
  INTEGER(res)[1] = last;
  UNPROTECT(1);
  return res;
}
