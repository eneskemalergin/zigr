#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_list_access(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    double total = 0.0;
    for (R_xlen_t i = 0; i < n; i++) {
        total += REAL(VECTOR_ELT(arg, i))[0];
    }
    return Rf_ScalarReal(total);
}
