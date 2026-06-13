#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_altrep_sum_via_R(SEXP arg) {
    SEXP call = PROTECT(Rf_lang2(Rf_install("seq_len"), arg));
    int err = 0;
    SEXP alt = R_tryEvalSilent(call, R_GlobalEnv, &err);
    if (err != 0) { UNPROTECT(1); return R_NilValue; }
    SEXP sum_call = PROTECT(Rf_lang2(Rf_install("sum"), alt));
    SEXP res = R_tryEvalSilent(sum_call, R_GlobalEnv, &err);
    UNPROTECT(2);
    if (err != 0) return R_NilValue;
    return res;
}
