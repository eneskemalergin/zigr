const R = @import("R");
const SEXP = R.SEXP;

export fn zigr_bench_sexp_create(_: SEXP) SEXP {
    for (0..10) |_| {
        for (0..10000) |_| {
            _ = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
        }
        R.Rf_unprotect(10000);
    }
    return R.Rf_ScalarInteger(0);
}
