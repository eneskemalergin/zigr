const R = @import("R");

const SEXP = R.SEXP;

export fn zigr_bench_protect_stress(n_sexp: SEXP) SEXP {
    const n = R.Rf_asInteger(n_sexp);
    const limit: usize = @intCast(n);

    for (0..limit) |_| {
        const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
        _ = vec;
    }

    R.Rf_unprotect(@intCast(n));
    return R.Rf_ScalarInteger(0);
}
