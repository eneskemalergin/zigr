const R = @import("R");

const SEXP = R.SEXP;

fn fib(n: i64) i64 {
    if (n <= 1) return n;
    var a: i64 = 0;
    var b: i64 = 1;
    var i: i64 = 2;
    while (i <= n) : (i += 1) {
        const next = a + b;
        a = b;
        b = next;
    }
    return b;
}

export fn zigr_bench_fib(n_sexp: SEXP) SEXP {
    const n = R.Rf_asInteger(n_sexp);
    const result = fib(n);
    return R.Rf_ScalarInteger(@intCast(result));
}
