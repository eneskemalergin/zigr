#include <R.h>
#include <Rinternals.h>

#define N_REP 2500

SEXP c_call_bench_l1_arithmetic(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    double *xp = REAL(arg);
    double total = 0.0;
    for (int rep = 0; rep < N_REP; rep++) {
        for (R_xlen_t i = 0; i < n; i++) {
            total += xp[i] * 0.5 + 0.5;
        }
    }
    return Rf_ScalarReal(total);
}
