#include <R.h>
#include <Rinternals.h>
#include <string.h>

#define BLOCK 32

SEXP c_call_bench_matrix_transpose(SEXP arg) {
    int nr = Rf_nrows(arg);
    int nc = Rf_ncols(arg);
    double *xp = REAL(arg);

    SEXP result = PROTECT(Rf_allocMatrix(REALSXP, nc, nr));
    double *rp = REAL(result);

    for (int jj = 0; jj < nc; jj += BLOCK) {
        int j_end = jj + BLOCK < nc ? jj + BLOCK : nc;
        for (int ii = 0; ii < nr; ii += BLOCK) {
            int i_end = ii + BLOCK < nr ? ii + BLOCK : nr;
            for (int i = ii; i < i_end; i++) {
                double *out_row = rp + i * nc;
                for (int j = jj; j < j_end; j++) {
                    out_row[j] = xp[i + j * nr];
                }
            }
        }
    }

    UNPROTECT(1);
    return result;
}
