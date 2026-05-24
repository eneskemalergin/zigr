// Task 1: Fibonacci (n=35), iterative. Returns ScalarInteger.
// No SEXP allocations other than the return value.
#include <Rinternals.h>

static int fib(int n) {
  int a = 0, b = 1, next;
  for (int i = 2; i <= n; i++) {
    next = a + b;
    a = b;
    b = next;
  }
  return (n <= 1) ? n : b;
}

SEXP c_call_bench_fib(SEXP n_sexp) {
  int n = INTEGER(n_sexp)[0];
  return ScalarInteger(fib(n));
}
