const R = @import("R");

export fn zigr_bench_sexp_inspect(_: R.SEXP) R.SEXP {
    return R.Rf_ScalarReal(0.0);
}
