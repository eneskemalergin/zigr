// Task 11: Cross-product X'X via dsyrk (symmetric, half the work).
#include <Rinternals.h>
#include <R_ext/BLAS.h>

SEXP c_call_bench_crossprod(SEXP x_sexp) {
  int nr = nrows(x_sexp), nc = ncols(x_sexp);

  SEXP result = PROTECT(allocMatrix(REALSXP, nc, nc));
  double *rp = REAL(result);

  double alpha = 1.0, beta = 0.0;
  char uplo = 'U', trans = 'T';

  F77_CALL(dsyrk)(&uplo, &trans, &nc, &nr,
                  &alpha, REAL(x_sexp), &nr,
                  &beta, rp, &nc FCONE FCONE);

  // Fill lower triangle from upper
  for (int i = 0; i < nc; i++)
    for (int j = 0; j < i; j++)
      rp[i * nc + j] = rp[j * nc + i];

  UNPROTECT(1);
  return result;
}
