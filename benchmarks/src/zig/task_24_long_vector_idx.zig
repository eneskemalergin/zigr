const R = @import("R");

export fn zigr_bench_long_vector_idx(vec: R.SEXP) R.SEXP {
    const n = R.XLENGTH(vec);
    var total: i64 = 0;
    var i: i64 = 0;
    while (i < n) {
        total += @as(i64, @intCast(R.INTEGER_ELT(vec, i)));
        i += 10000;
    }
    return R.Rf_ScalarReal(@as(f64, @floatFromInt(total)));
}
