const R = @import("R");
const zigr = @import("zigr");

export fn zigr_bench_altrep_region_read(vec: R.SEXP) R.SEXP {
    const call = R.Rf_protect(R.Rf_lang2(R.Rf_install("seq_len"), vec));
    var err: c_int = 0;
    const alt = R.R_tryEvalSilent(call, R.R_GlobalEnv, &err);
    R.Rf_unprotect(1);
    if (err != 0) return R.R_NilValue;
    return R.Rf_ScalarReal(@as(f64, @floatFromInt(zigr.convert.sumInt(alt))));
}
