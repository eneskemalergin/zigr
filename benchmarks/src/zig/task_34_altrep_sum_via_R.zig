const R = @import("R");

export fn zigr_bench_altrep_sum_via_R(vec: R.SEXP) R.SEXP {
    const call = R.Rf_protect(R.Rf_lang2(R.Rf_install("seq_len"), vec));
    var err: c_int = 0;
    const alt = R.R_tryEvalSilent(call, R.R_GlobalEnv, &err);
    if (err != 0) { R.Rf_unprotect(1); return R.R_NilValue; }
    const sum_call = R.Rf_protect(R.Rf_lang2(R.Rf_install("sum"), alt));
    const res = R.R_tryEvalSilent(sum_call, R.R_GlobalEnv, &err);
    R.Rf_unprotect(2);
    if (err != 0) return R.R_NilValue;
    return res;
}
