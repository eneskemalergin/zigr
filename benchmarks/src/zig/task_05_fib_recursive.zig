const R = @import("R");

const SEXP = R.SEXP;

/// Double-recursive fib to measure function call overhead.
/// Reads n at runtime so the compiler cannot precompute the result.
/// Uses n=30 as a standard recursion benchmark (fast enough for many runs).
fn fib(n: i64) i64 {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

export fn zigr_bench_fib_recursive(n_sexp: SEXP) SEXP {
    const n = @as(i64, @intCast(R.Rf_asInteger(n_sexp)));
    const result = fib(n);
    // Return as f64 to avoid i32 overflow (fib(40) = 102334155 fits i32, but
    // larger n may not. f64 is exact up to 2^53.)
    return R.Rf_ScalarReal(@as(f64, @floatFromInt(result)));
}
