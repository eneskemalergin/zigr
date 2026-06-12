#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_sexp_inspect(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    SEXP elts[5];
    for (R_xlen_t i = 0; i < n; i++) elts[i] = VECTOR_ELT(arg, i);
    int total = 0;
    for (int iter = 0; iter < 10000; iter++) {
        for (R_xlen_t i = 0; i < n; i++) {
            total += TYPEOF(elts[i]);
            total += Rf_isVector(elts[i]);
            total += Rf_isReal(elts[i]);
        }
    }
    return Rf_ScalarInteger(total);
}
