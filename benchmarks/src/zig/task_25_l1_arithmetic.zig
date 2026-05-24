const R = @import("R");

export fn zigr_bench_l1_arithmetic(v: R.SEXP) R.SEXP {
    _ = v;
    return R.Rf_ScalarReal(0.0);
}
