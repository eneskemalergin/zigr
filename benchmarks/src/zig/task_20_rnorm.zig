const R = @import("R");

const SEXP = R.SEXP;

export fn zigr_bench_rnorm(n_sexp: SEXP) SEXP {
    const n = R.Rf_asInteger(n_sexp);

    // Acquire RNG state
    R.GetRNGstate();

    const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, n));
    const rp = R.REAL(result);

    for (0..@as(usize, @intCast(n))) |i| rp[i] = R.norm_rand();

    R.PutRNGstate();
    R.Rf_unprotect(1);
    return result;
}
