const std = @import("std");
const R = @import("R");
const convert = @import("convert");
const SEXP = R.SEXP;

export fn zigr_bench_string_nchar(vec: SEXP) SEXP {
    const strings = convert.toStringSliceView(vec) catch |err| convert.signalError(err);
    var total: i64 = 0;
    for (0..strings.len) |i| {
        const value = strings.at(i);
        if (value.is_na) return R.Rf_ScalarInteger(R.R_NaInt);
        total += @as(i64, @intCast(value.len));
    }
    return R.Rf_ScalarInteger(@intCast(total));
}
