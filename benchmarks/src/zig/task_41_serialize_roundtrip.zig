const R = @import("R");

export fn zigr_bench_serialize_roundtrip(vec: R.SEXP) R.SEXP {
    const ser_call = R.Rf_protect(R.Rf_lang3(R.Rf_install("serialize"), vec, R.R_NilValue));
    defer R.Rf_unprotect(1);
    const conn = R.Rf_eval(ser_call, R.R_GlobalEnv);

    const unser_call = R.Rf_protect(R.Rf_lang2(R.Rf_install("unserialize"), conn));
    defer R.Rf_unprotect(1);
    const result = R.Rf_eval(unser_call, R.R_GlobalEnv);

    const n = R.XLENGTH(result);
    const xp: [*]const f64 = @ptrCast(R.REAL(result));
    var total: f64 = 0.0;
    for (0..@as(usize, @intCast(n))) |i| total += xp[i];
    return R.Rf_ScalarReal(total);
}
