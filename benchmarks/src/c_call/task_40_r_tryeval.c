#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_r_tryeval(SEXP arg) {
    (void)arg;
    int count = 0;
    for (int i = 0; i < 512; i++) {
        SEXP call = PROTECT(Rf_lang2(Rf_install("stop"), Rf_mkString("task40")));
        int err = 0;
        R_tryEvalSilent(call, R_GlobalEnv, &err);
        UNPROTECT(1);
        if (err != 0) count++;
    }
    return Rf_ScalarInteger(count);
}
