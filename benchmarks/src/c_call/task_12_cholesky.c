// Task 12: Cholesky decomposition via dpotrf.
#include <Rinternals.h>
#include <R_ext/Lapack.h>
#include <string.h>

SEXP c_call_bench_cholesky(SEXP a_sexp) {
  int n = nrows(a_sexp);

  SEXP result = PROTECT(allocMatrix(REALSXP, n, n));
  double *rp = REAL(result);
  memcpy(rp, REAL(a_sexp), (size_t)n * n * sizeof(double));

  int info = 0;
  char uplo = 'U';
  F77_CALL(dpotrf)(&uplo, &n, rp, &n, &info FCONE);

  // Zero out lower triangle to match R convention
  for (int col = 0; col < n; col++)
    for (int row = col + 1; row < n; row++)
      rp[col * n + row] = 0.0;

  UNPROTECT(1);
  return result;
}
