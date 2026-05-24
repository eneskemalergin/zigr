const R = @import("R");
const convert = @import("convert");

const SEXP = R.SEXP;

export fn zigr_bench_na_prop(vec: SEXP) SEXP {
    return R.Rf_ScalarReal(convert.mean_narm(vec));
}
