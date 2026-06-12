#include <R.h>
#include <Rinternals.h>
#include <string.h>

SEXP c_call_bench_matrix_rowcol_means(SEXP arg) {
    int nr = Rf_nrows(arg);
    int nc = Rf_ncols(arg);
    double *xp = REAL(arg);

    SEXP row_means = PROTECT(Rf_allocVector(REALSXP, nr));
    SEXP col_sums = PROTECT(Rf_allocVector(REALSXP, nc));
    double *rm = REAL(row_means);
    double *cs = REAL(col_sums);
    memset(rm, 0, nr * sizeof(double));

    for (int j = 0; j < nc; j++) {
        double *col = xp + j * nr;
        double sum = 0.0;
        for (int i = 0; i < nr; i++) {
            double v = col[i];
            rm[i] += v;
            sum += v;
        }
        cs[j] = sum;
    }

    double inv_nc = 1.0 / nc;
    for (int i = 0; i < nr; i++) rm[i] *= inv_nc;

    SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(result, 0, row_means);
    SET_VECTOR_ELT(result, 1, col_sums);
    UNPROTECT(3);
    return result;
}
