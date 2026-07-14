#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_string_encoding(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    int total = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        total += getCharCE(STRING_ELT(arg, i)) == CE_UTF8;
    }
    return Rf_ScalarInteger(total);
}
