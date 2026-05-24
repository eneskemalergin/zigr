// Task 10: BLAS-accelerated matrix multiply via dgemm.
#include <Rinternals.h>
#include <R_ext/BLAS.h>

SEXP c_call_bench_blas_matmul(SEXP a_sexp, SEXP b_sexp) {
  int n = nrows(a_sexp), m = ncols(b_sexp), k = ncols(a_sexp);

  SEXP result = PROTECT(allocMatrix(REALSXP, n, m));
  double *rp = REAL(result);

  double alpha = 1.0, beta = 0.0;
  char notrans = 'N';

  F77_CALL(dgemm)(&notrans, &notrans, &n, &m, &k,
                  &alpha, REAL(a_sexp), &n,
                  REAL(b_sexp), &k,
                  &beta, rp, &n FCONE FCONE);

  UNPROTECT(1);
  return result;
}
