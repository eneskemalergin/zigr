// Task 14: Row sums of a matrix (column-major access).
#include <Rinternals.h>
#include <string.h>

SEXP c_call_bench_rowsums(SEXP mat_sexp) {
  int nr = nrows(mat_sexp), nc = ncols(mat_sexp);
  double *data = REAL(mat_sexp);

  SEXP result = PROTECT(allocVector(REALSXP, nr));
  double *rp = REAL(result);
  memset(rp, 0, (size_t)nr * sizeof(double));

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      rp[i] += data[i + j * nr];

  UNPROTECT(1);
  return result;
}
