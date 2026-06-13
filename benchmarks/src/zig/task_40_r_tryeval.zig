const R = @import("R");

export fn zigr_bench_r_tryeval(_: R.SEXP) R.SEXP {
    var count: c_int = 0;
    for (0..512) |_| {
        const call = R.Rf_protect(R.Rf_lang2(R.Rf_install("stop"), R.Rf_mkString("task40")));
        defer R.Rf_unprotect(1);
        var err: c_int = 0;
        _ = R.R_tryEvalSilent(call, R.R_GlobalEnv, &err);
        if (err != 0) count += 1;
    }
    return R.Rf_ScalarInteger(count);
}
