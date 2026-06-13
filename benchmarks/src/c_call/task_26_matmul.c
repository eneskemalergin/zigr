#include <R.h>
#include <Rinternals.h>

extern void dgemm_(char *transa, char *transb, int *m, int *n, int *k,
                   double *alpha, double *a, int *lda, double *b, int *ldb,
                   double *beta, double *c, int *ldc);

SEXP c_call_bench_matmul(SEXP a, SEXP b) {
    int n = Rf_nrows(a);
    int m = Rf_ncols(b);
    int k = Rf_ncols(a);

    SEXP result = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)n * m));
    SEXP dims = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(dims)[0] = n;
    INTEGER(dims)[1] = m;
    Rf_setAttrib(result, R_DimSymbol, dims);
    UNPROTECT(1);

    double alpha = 1.0, beta = 0.0;
    char notrans = 'N';
    dgemm_(&notrans, &notrans, &n, &m, &k,
           &alpha, REAL(a), &n, REAL(b), &k,
           &beta, REAL(result), &n);

    UNPROTECT(1);
    return result;
}
