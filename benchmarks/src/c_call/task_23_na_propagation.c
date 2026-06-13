#include <R.h>
#include <Rinternals.h>
#include <math.h>

SEXP c_call_bench_na_propagation(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    double *xp = REAL(arg);
    double total = 0.0;
    R_xlen_t count = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        if (!isnan(xp[i])) { total += xp[i]; count++; }
    }
    return Rf_ScalarReal(count > 0 ? total / count : NA_REAL);
}
