#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_altrep_elt_walk(SEXP arg) {
    SEXP call = PROTECT(Rf_lang2(Rf_install("seq_len"), arg));
    int err = 0;
    SEXP alt = R_tryEvalSilent(call, R_GlobalEnv, &err);
    UNPROTECT(1);
    if (err != 0) return R_NilValue;
    R_xlen_t n = XLENGTH(alt);
    long long total = 0;
    for (R_xlen_t i = 0; i < n; i++)
        total += INTEGER_ELT(alt, i);
    return Rf_ScalarReal((double)total);
}
