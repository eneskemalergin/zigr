// Task 13: Linear model fit via dgemm + dpotrf + dtrsm (zero-copy).
#include <Rinternals.h>
#include <R_ext/BLAS.h>
#include <R_ext/Lapack.h>

SEXP c_call_bench_lm(SEXP x_sexp, SEXP y_sexp) {
  int n = nrows(x_sexp), p = ncols(x_sexp);

  // Use R's data directly — no copy (same as zigR raw.real())
  double *x_data = REAL(x_sexp);
  double *y_data = REAL(y_sexp);

  double *xtx = (double *)R_alloc((size_t)p * p, sizeof(double));
  double *xty = (double *)R_alloc(p, sizeof(double));

  double alpha = 1.0, beta = 0.0;
  char notrans = 'N', trans = 'T';

  // X'X = X^T * X (use x_data directly, zero copy)
  F77_CALL(dgemm)(&trans, &notrans, &p, &p, &n,
                  &alpha, x_data, &n, x_data, &n, &beta, xtx, &p FCONE FCONE);

  // X'y = X^T * y
  int one = 1;
  F77_CALL(dgemm)(&trans, &notrans, &p, &one, &n,
                  &alpha, x_data, &n, y_data, &n, &beta, xty, &p FCONE FCONE);

  // Cholesky: X'X = L * L^T
  int info = 0;
  char uplo = 'U';
  F77_CALL(dpotrf)(&uplo, &p, xtx, &p, &info FCONE);

  // Solve L * z = xty
  char side = 'L', diag = 'N';
  F77_CALL(dtrsm)(&side, &uplo, &trans, &diag, &p, &one, &alpha, xtx, &p, xty, &p FCONE FCONE FCONE FCONE);

  // Solve L^T * beta = z
  F77_CALL(dtrsm)(&side, &uplo, &notrans, &diag, &p, &one, &alpha, xtx, &p, xty, &p FCONE FCONE FCONE FCONE);

  SEXP result = PROTECT(allocVector(REALSXP, p));
  memcpy(REAL(result), xty, (size_t)p * sizeof(double));
  UNPROTECT(1);
  return result;
}
