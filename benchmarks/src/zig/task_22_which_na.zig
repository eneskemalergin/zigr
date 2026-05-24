const std = @import("std");
const R = @import("R");
const SEXP = R.SEXP;

export fn zigr_bench_which_na(vec: SEXP) SEXP {
    const n = @as(usize, @intCast(R.XLENGTH(vec)));
    const src = R.REAL(vec);

    // First pass: count NAs
    var na_count: usize = 0;
    for (0..n) |i| {
        if (std.math.isNan(src[i])) na_count += 1;
    }

    // Allocate result vector
    const result = R.Rf_protect(R.Rf_allocVector(R.INTSXP, @intCast(na_count)));
    defer R.Rf_unprotect(1);
    const rp = R.INTEGER(result);

    var pos: usize = 0;
    for (0..n) |i| {
        if (std.math.isNan(src[i])) {
            rp[pos] = @intCast(i + 1); // 1-based R index
            pos += 1;
        }
    }

    return result;
}
