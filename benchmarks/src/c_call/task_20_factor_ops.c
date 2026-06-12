#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_factor_ops(SEXP arg) {
    SEXP factor_sym = Rf_install("factor");
    SEXP call = PROTECT(Rf_lang2(factor_sym, arg));
    int err = 0;
    SEXP factor = PROTECT(R_tryEvalSilent(call, R_GlobalEnv, &err));
    if (err != 0) {
        UNPROTECT(2);
        return Rf_ScalarInteger(NA_INTEGER);
    }

    R_xlen_t n = XLENGTH(factor);
    int *codes = INTEGER(factor);
    long long total = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        int c = codes[i];
        if (c != NA_INTEGER) total += c;
    }

    UNPROTECT(2);
    return Rf_ScalarInteger((int)total);
}
