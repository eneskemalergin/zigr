const R = @import("R");
const sexp = @import("zigr").sexp;
const SEXP = R.SEXP;

export fn zigr_bench_list_access(arg: SEXP) SEXP {
    const n = sexp.xlength(arg);
    var total: f64 = 0.0;
    for (0..n) |i| {
        const elt = sexp.fastVectorElt(arg, i);
        total += @as([*]const f64, @ptrCast(@alignCast(@as([*]u8, @ptrCast(elt)) + 0x30)))[0];
    }
    return R.Rf_ScalarReal(total);
}
