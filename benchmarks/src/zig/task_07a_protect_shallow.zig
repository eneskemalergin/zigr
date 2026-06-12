const R = @import("R");

const SEXP = R.SEXP;

export fn zigr_bench_protect_shallow(vec: SEXP) SEXP {
    for (0..100) |_| {
        for (0..10) |_| _ = R.Rf_protect(vec);
        R.Rf_unprotect(10);
    }
    return R.Rf_ScalarInteger(0);
}
