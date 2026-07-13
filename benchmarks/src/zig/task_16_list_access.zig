const R = @import("R");
const sexp = @import("zigr").sexp;
const SEXP = R.SEXP;

export fn zigr_bench_list_access(arg: SEXP) SEXP {
    const n = sexp.xlength(arg);
    var total: f64 = 0.0;
    for (0..n) |i| {
        const elt = sexp.fastVectorElt(arg, i);
        const data = sexp.fastDataPtr(elt) orelse return R.R_NilValue;
        total += @as([*]const f64, @ptrCast(@alignCast(data)))[0];
    }
    return R.Rf_ScalarReal(total);
}
