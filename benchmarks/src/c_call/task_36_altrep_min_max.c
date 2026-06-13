#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_altrep_min_max(SEXP arg) {
    SEXP call = PROTECT(Rf_lang2(Rf_install("seq_len"), arg));
    int err = 0;
    SEXP alt = R_tryEvalSilent(call, R_GlobalEnv, &err);
    UNPROTECT(1);
    if (err != 0) return R_NilValue;
    R_xlen_t n = XLENGTH(alt);
    int min_val = INTEGER_ELT(alt, 0);
    int max_val = min_val;
    for (R_xlen_t i = 1; i < n; i++) {
        int v = INTEGER_ELT(alt, i);
        if (v < min_val) min_val = v;
        if (v > max_val) max_val = v;
    }
    return Rf_ScalarInteger(max_val - min_val);
}
