const R = @import("R");

export fn zigr_bench_altrep_min_max(vec: R.SEXP) R.SEXP {
    const call = R.Rf_protect(R.Rf_lang2(R.Rf_install("seq_len"), vec));
    var err: c_int = 0;
    const alt = R.R_tryEvalSilent(call, R.R_GlobalEnv, &err);
    R.Rf_unprotect(1);
    if (err != 0) return R.R_NilValue;
    const n = R.XLENGTH(alt);
    var min_val: c_int = R.INTEGER_ELT(alt, 0);
    var max_val: c_int = min_val;
    var i: i64 = 1;
    while (i < n) {
        const v = R.INTEGER_ELT(alt, i);
        if (v < min_val) min_val = v;
        if (v > max_val) max_val = v;
        i += 1;
    }
    return R.Rf_ScalarInteger(max_val - min_val);
}
