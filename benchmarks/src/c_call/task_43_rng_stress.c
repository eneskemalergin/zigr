#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_rng_stress(SEXP arg) {
    int n = Rf_asInteger(arg);
    SEXP result = PROTECT(Rf_allocVector(REALSXP, n));
    double *rp = REAL(result);
    GetRNGstate();
    for (int i = 0; i < n; i++) rp[i] = norm_rand();
    PutRNGstate();
    UNPROTECT(1);
    return result;
}
