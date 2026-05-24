// Task 2: Sum 1e7 double-precision values. Returns ScalarReal.
#include <Rinternals.h>

SEXP c_call_bench_vectorsum(SEXP vec) {
  double *data = REAL(vec);
  R_xlen_t n = XLENGTH(vec);
  double total = 0.0;
  for (R_xlen_t i = 0; i < n; i++) total += data[i];
  return ScalarReal(total);
}
