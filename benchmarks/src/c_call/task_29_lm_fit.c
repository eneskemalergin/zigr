#include <R.h>
#include <Rinternals.h>

extern void dgemm_(char *transa, char *transb, int *m, int *n, int *k,
                   double *alpha, double *a, int *lda, double *b, int *ldb,
                   double *beta, double *c, int *ldc);
extern void dpotrf_(char *uplo, int *n, double *a, int *lda, int *info);
extern void dtrsm_(char *side, char *uplo, char *transa, char *diag,
                   int *m, int *n, double *alpha, double *a, int *lda,
                   double *b, int *ldb);

SEXP c_call_bench_lm_fit(SEXP X, SEXP y) {
    int n = Rf_nrows(X);
    int p = Rf_ncols(X);

    SEXP xtx = PROTECT(Rf_allocMatrix(REALSXP, p, p));
    SEXP xty = PROTECT(Rf_allocVector(REALSXP, p));
    double *xtx_rp = REAL(xtx);
    double *xty_rp = REAL(xty);

    double alpha = 1.0, beta = 0.0;
    char notrans = 'N', trans = 'T';
    int one = 1;

    dgemm_(&trans, &notrans, &p, &p, &n, &alpha,
           REAL(X), &n, REAL(X), &n, &beta, xtx_rp, &p);
    dgemm_(&trans, &notrans, &p, &one, &n, &alpha,
           REAL(X), &n, REAL(y), &n, &beta, xty_rp, &p);

    int info = 0;
    char uplo = 'U';
    dpotrf_(&uplo, &p, xtx_rp, &p, &info);

    char side = 'L', diag = 'N';
    dtrsm_(&side, &uplo, &trans, &diag, &p, &one, &alpha,
           xtx_rp, &p, xty_rp, &p);
    dtrsm_(&side, &uplo, &notrans, &diag, &p, &one, &alpha,
           xtx_rp, &p, xty_rp, &p);

    UNPROTECT(2);
    return xty;
}
