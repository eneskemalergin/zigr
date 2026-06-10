#include <R.h>
#include <Rinternals.h>

static long long fib(long long n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

SEXP c_call_bench_fib_recursive(SEXP arg) {
    long long n = (long long)Rf_asInteger(arg);
    long long result = fib(n);
    return Rf_ScalarReal((double)result);
}
