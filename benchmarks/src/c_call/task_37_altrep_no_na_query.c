#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_altrep_no_na_query(SEXP arg) {
    SEXP call = PROTECT(Rf_lang2(Rf_install("seq_len"), arg));
    int err = 0;
    SEXP alt = R_tryEvalSilent(call, R_GlobalEnv, &err);
    UNPROTECT(1);
    if (err != 0) return R_NilValue;
    R_xlen_t n = XLENGTH(alt);
    int has_na = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        if (INTEGER_ELT(alt, i) == NA_INTEGER) { has_na = 1; break; }
    }
    return Rf_ScalarInteger(has_na);
}
