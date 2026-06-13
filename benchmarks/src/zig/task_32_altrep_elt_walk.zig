const R = @import("R");

export fn zigr_bench_altrep_elt_walk(vec: R.SEXP) R.SEXP {
    const call = R.Rf_protect(R.Rf_lang2(R.Rf_install("seq_len"), vec));
    var err: c_int = 0;
    const alt = R.R_tryEvalSilent(call, R.R_GlobalEnv, &err);
    R.Rf_unprotect(1);
    if (err != 0) return R.R_NilValue;
    const n = R.XLENGTH(alt);
    var total: i64 = 0;
    var i: i64 = 0;
    while (i < n) {
        total += @as(i64, @intCast(R.INTEGER_ELT(alt, i)));
        i += 1;
    }
    return R.Rf_ScalarReal(@as(f64, @floatFromInt(total)));
}
