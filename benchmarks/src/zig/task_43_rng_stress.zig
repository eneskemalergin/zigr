const R = @import("R");
const zigr = @import("zigr");

threadlocal var rng_result: R.SEXP = null;

fn fillNormalDraws() R.SEXP {
    const result = rng_result orelse zigr.@"error".signal("RNG benchmark result is not initialized");
    const values = R.REAL(result)[0..@as(usize, @intCast(R.XLENGTH(result)))];
    for (values) |*value| value.* = R.norm_rand();
    return result;
}

export fn zigr_bench_rng_stress(vec: R.SEXP) R.SEXP {
    const n = R.Rf_asInteger(vec);
    if (n < 0 or n == R.R_NaInt) zigr.@"error".signal("RNG benchmark expected a non-negative length");
    const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, n));
    defer R.Rf_unprotect(1);
    rng_result = result;
    defer rng_result = null;
    return zigr.rng.withRng(fillNormalDraws);
}
