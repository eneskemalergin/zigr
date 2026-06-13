const R = @import("R");

export fn zigr_bench_altrep_region_read(vec: R.SEXP) R.SEXP {
    const call = R.Rf_protect(R.Rf_lang2(R.Rf_install("seq_len"), vec));
    var err: c_int = 0;
    const alt = R.R_tryEvalSilent(call, R.R_GlobalEnv, &err);
    R.Rf_unprotect(1);
    if (err != 0) return R.R_NilValue;
    const n = R.XLENGTH(alt);
    var buf: [4096]c_int = undefined;
    var total: i64 = 0;
    var i: i64 = 0;
    while (i < n) {
        const want = if (n - i < 4096) @as(i64, n - i) else 4096;
        const got = R.INTEGER_GET_REGION(alt, i, want, &buf);
        for (0..@as(usize, @intCast(got))) |j| total += @as(i64, @intCast(buf[j]));
        i += got;
    }
    return R.Rf_ScalarReal(@as(f64, @floatFromInt(total)));
}
