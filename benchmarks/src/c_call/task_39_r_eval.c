#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_r_eval(SEXP arg) {
    SEXP sum_sym = Rf_install("sum");
    SEXP mean_sym = Rf_install("mean");

    SEXP sum_call = PROTECT(Rf_lang2(sum_sym, arg));
    SEXP sum_res = PROTECT(Rf_eval(sum_call, R_GlobalEnv));
    double sum_val = REAL(sum_res)[0];

    SEXP mean_call = PROTECT(Rf_lang2(mean_sym, arg));
    SEXP mean_res = PROTECT(Rf_eval(mean_call, R_GlobalEnv));
    double mean_val = REAL(mean_res)[0];

    UNPROTECT(4);
    return Rf_ScalarReal(sum_val + mean_val);
}
