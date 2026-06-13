const R = @import("R");
const convert = @import("zigr").convert;

export fn zigr_bench_l1_arithmetic(vec: R.SEXP) R.SEXP {
    var total: f64 = 0.0;
    for (0..2500) |_| {
        total += convert.scaleAdd(vec, 0.5, 0.5);
    }
    return R.Rf_ScalarReal(total);
}
