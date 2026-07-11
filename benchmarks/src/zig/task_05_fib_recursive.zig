const R = @import("R");

const SEXP = R.SEXP;

fn fib(n: i64) i64 {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

export fn zigr_bench_fib_recursive(n_sexp: SEXP) SEXP {
    const n = @as(i64, @intCast(R.Rf_asInteger(n_sexp)));
    const result = fib(n);
    // Keep the reference result exact beyond the i32 range.
    return R.Rf_ScalarReal(@as(f64, @floatFromInt(result)));
}
