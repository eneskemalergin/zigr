#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_serialize_roundtrip(SEXP arg) {
    SEXP ser_call = PROTECT(Rf_lang3(Rf_install("serialize"), arg, R_NilValue));
    SEXP conn = PROTECT(Rf_eval(ser_call, R_GlobalEnv));

    SEXP unser_call = PROTECT(Rf_lang2(Rf_install("unserialize"), conn));
    SEXP result = PROTECT(Rf_eval(unser_call, R_GlobalEnv));
    double total = 0;
    double *xp = REAL(result);
    R_xlen_t n = XLENGTH(result);
    for (R_xlen_t i = 0; i < n; i++) total += xp[i];
    UNPROTECT(4);
    return Rf_ScalarReal(total);
}
