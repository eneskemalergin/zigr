const R = @import("R");

export fn zigr_bench_altrep_create(vec: R.SEXP) R.SEXP {
    const call = R.Rf_protect(R.Rf_lang2(R.Rf_install("seq_len"), vec));
    var err: c_int = 0;
    const result = R.R_tryEvalSilent(call, R.R_GlobalEnv, &err);
    R.Rf_unprotect(1);
    if (err != 0) return R.R_NilValue;
    return result;
}
