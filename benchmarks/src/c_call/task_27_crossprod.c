#include <R.h>
#include <Rinternals.h>

extern void dsyrk_(char *uplo, char *trans, int *n, int *k,
                   double *alpha, double *a, int *lda,
                   double *beta, double *c, int *ldc);

SEXP c_call_bench_crossprod(SEXP arg) {
    int n = Rf_ncols(arg);
    int k = Rf_nrows(arg);

    SEXP result = PROTECT(Rf_allocMatrix(REALSXP, n, n));
    double *rp = REAL(result);

    double alpha = 1.0, beta = 0.0;
    char uplo = 'U', trans = 'T';
    dsyrk_(&uplo, &trans, &n, &k,
           &alpha, REAL(arg), &k,
           &beta, rp, &n);

    for (int i = 0; i < n; i++)
        for (int j = 0; j < i; j++)
            rp[i * n + j] = rp[j * n + i];

    UNPROTECT(1);
    return result;
}
