#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_vectorsum(SEXP arg) {
    double *xp = REAL(arg);
    R_xlen_t n = XLENGTH(arg);
    double total = 0.0;
    for (R_xlen_t i = 0; i < n; i++) total += xp[i];
    return Rf_ScalarReal(total);
}
