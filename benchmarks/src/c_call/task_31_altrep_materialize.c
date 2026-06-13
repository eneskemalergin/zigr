#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_altrep_materialize(SEXP arg) {
    SEXP call = PROTECT(Rf_lang2(Rf_install("seq_len"), arg));
    int err = 0;
    SEXP alt = R_tryEvalSilent(call, R_GlobalEnv, &err);
    if (err != 0) { UNPROTECT(1); return R_NilValue; }
    SEXP mat = Rf_duplicate(alt);
    UNPROTECT(1);
    int n = LENGTH(mat);
    return Rf_ScalarInteger(INTEGER(mat)[0] + INTEGER(mat)[n - 1]);
}
