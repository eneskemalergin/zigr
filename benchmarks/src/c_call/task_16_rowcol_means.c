// Task 16: Row means and column sums (unnamed list, matching zigR).
#include <Rinternals.h>

SEXP c_call_bench_rowcol_means(SEXP mat_sexp) {
  int nr = nrows(mat_sexp), nc = ncols(mat_sexp);
  double *data = REAL(mat_sexp);

  SEXP row_means = PROTECT(allocVector(REALSXP, nr));
  SEXP col_sums = PROTECT(allocVector(REALSXP, nc));

  for (int i = 0; i < nr; i++) {
    double sum = 0.0;
    for (int j = 0; j < nc; j++) sum += data[i + j * nr];
    REAL(row_means)[i] = sum / nc;
  }

  for (int j = 0; j < nc; j++) {
    double sum = 0.0;
    for (int i = 0; i < nr; i++) sum += data[i + j * nr];
    REAL(col_sums)[j] = sum;
  }

  SEXP result = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, row_means);
  SET_VECTOR_ELT(result, 1, col_sums);

  UNPROTECT(3);
  return result;
}
