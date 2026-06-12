#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_broadcast(SEXP vec, SEXP scalar_sexp) {
    double *xp = REAL(vec);
    R_xlen_t n = XLENGTH(vec);
    double scalar = REAL(scalar_sexp)[0];
    double total = 0.0;
    for (R_xlen_t i = 0; i < n; i++) total += xp[i] + scalar;
    return Rf_ScalarReal(total);
}
