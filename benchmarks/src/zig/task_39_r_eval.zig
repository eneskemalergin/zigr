const R = @import("R");

export fn zigr_bench_r_eval(_: R.SEXP) R.SEXP {
    return R.Rf_ScalarReal(0.0);
}
