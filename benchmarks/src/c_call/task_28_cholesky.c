#include <R.h>
#include <Rinternals.h>

extern void dpotrf_(char *uplo, int *n, double *a, int *lda, int *info);

SEXP c_call_bench_cholesky(SEXP arg) {
    int n = Rf_nrows(arg);
    SEXP result = PROTECT(Rf_allocMatrix(REALSXP, n, n));
    double *rp = REAL(result);
    double *src = REAL(arg);
    R_xlen_t len = (R_xlen_t)n * n;
    for (R_xlen_t i = 0; i < len; i++) rp[i] = src[i];

    int info = 0;
    char uplo = 'U';
    dpotrf_(&uplo, &n, rp, &n, &info);

    for (int col = 0; col < n; col++)
        for (int row = col + 1; row < n; row++)
            rp[col * n + row] = 0.0;

    UNPROTECT(1);
    return result;
}
