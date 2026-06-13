const R = @import("R");

export fn zigr_bench_r_eval(vec: R.SEXP) R.SEXP {
    const sum_call = R.Rf_protect(R.Rf_lang2(R.Rf_install("sum"), vec));
    defer R.Rf_unprotect(1);
    const sum_res = R.Rf_eval(sum_call, R.R_GlobalEnv);
    const sum_val = R.REAL(sum_res)[0];

    const mean_call = R.Rf_protect(R.Rf_lang2(R.Rf_install("mean"), vec));
    defer R.Rf_unprotect(1);
    const mean_res = R.Rf_eval(mean_call, R.R_GlobalEnv);
    const mean_val = R.REAL(mean_res)[0];

    return R.Rf_ScalarReal(sum_val + mean_val);
}
