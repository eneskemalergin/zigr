const std = @import("std");
const R = @import("R");
const SEXP = R.SEXP;

export fn zigr_bench_string_nchar(vec: SEXP) SEXP {
    const n = @as(usize, @intCast(R.XLENGTH(vec)));
    var total: i64 = 0;
    for (0..n) |i| {
        const elt = R.STRING_ELT(vec, @intCast(i));
        if (elt != R.R_NaString) {
            total += @as(i64, @intCast(std.mem.len(R.R_CHAR(elt))));
        }
    }
    return R.Rf_ScalarInteger(@intCast(total));
}
