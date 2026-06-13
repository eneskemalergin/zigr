const R = @import("R");

export fn zigr_bench_rng_stress(vec: R.SEXP) R.SEXP {
    const n = R.Rf_asInteger(vec);
    const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, n));
    defer R.Rf_unprotect(1);
    const rp: [*]f64 = @ptrCast(R.REAL(result));
    R.GetRNGstate();
    for (0..@as(usize, @intCast(n))) |i| rp[i] = R.norm_rand();
    R.PutRNGstate();
    return result;
}
