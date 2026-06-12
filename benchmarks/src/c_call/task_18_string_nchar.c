#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_string_nchar(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    long long total = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP elt = STRING_ELT(arg, i);
        if (elt == NA_STRING) continue;
        total += LENGTH(elt);
    }
    return Rf_ScalarInteger(total);
}
