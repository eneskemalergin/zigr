#include <R.h>
#include <Rinternals.h>
#include <string.h>

SEXP c_call_bench_matrix_rowsums(SEXP arg) {
    int nr = Rf_nrows(arg);
    int nc = Rf_ncols(arg);
    double *xp = REAL(arg);

    SEXP result = PROTECT(Rf_allocVector(REALSXP, nr));
    double *rp = REAL(result);
    memset(rp, 0, nr * sizeof(double));

    for (int j = 0; j < nc; j++) {
        double *col = xp + j * nr;
        for (int i = 0; i < nr; i++) {
            rp[i] += col[i];
        }
    }

    UNPROTECT(1);
    return result;
}
