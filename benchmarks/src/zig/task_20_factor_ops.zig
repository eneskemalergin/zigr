const R = @import("R");
const zigr = @import("zigr");
const sexp = zigr.sexp;
const factor = zigr.factor;

export fn zigr_bench_factor_ops(vec: R.SEXP) R.SEXP {
    const n = sexp.xlength(vec);
    const fac = factor.asFactor(vec);
    const codes: [*]c_int = @ptrCast(R.INTEGER(fac));
    var total: i64 = 0;
    for (0..n) |i| {
        const c = codes[i];
        if (c != R.R_NaInt) total += @as(i64, @intCast(c));
    }
    return R.Rf_ScalarInteger(@intCast(total));
}
