#include <Rinternals.h>
#include <math.h>

SEXP c_call_bench_which_na(SEXP vec) {
  R_xlen_t n = XLENGTH(vec);
  double *src = REAL(vec);

  R_xlen_t na_count = 0;
  for (R_xlen_t i = 0; i < n; i++)
    if (isnan(src[i])) na_count++;

  SEXP result = PROTECT(allocVector(INTSXP, na_count));
  int *rp = INTEGER(result);
  R_xlen_t pos = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    if (isnan(src[i])) rp[pos++] = (int)(i + 1);
  }

  UNPROTECT(1);
  return result;
}
