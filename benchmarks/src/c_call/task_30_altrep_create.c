#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_altrep_create(SEXP arg) {
    SEXP call = PROTECT(Rf_lang2(Rf_install("seq_len"), arg));
    int err = 0;
    SEXP result = R_tryEvalSilent(call, R_GlobalEnv, &err);
    UNPROTECT(1);
    if (err != 0) return R_NilValue;
    return result;
}
