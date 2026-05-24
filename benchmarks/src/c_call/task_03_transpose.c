#include <Rinternals.h>

SEXP c_call_bench_transpose(SEXP mat_sexp) {
  int nr = nrows(mat_sexp), nc = ncols(mat_sexp);
  double *data = REAL(mat_sexp);
  SEXP result = PROTECT(allocMatrix(REALSXP, nc, nr));
  double *rp = REAL(result);
  for (int i = 0; i < nr; i++)
    for (int j = 0; j < nc; j++)
      rp[j * nr + i] = data[i * nc + j];
  UNPROTECT(1);
  return result;
}
