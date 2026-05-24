// Task 6: Safe mean skipping NA values.
#include <Rinternals.h>

SEXP c_call_bench_na_prop(SEXP vec) {
  double *data = REAL(vec);
  R_xlen_t n = XLENGTH(vec);
  double sum = 0.0;
  int count = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    if (ISNA(data[i])) continue;
    sum += data[i];
    count++;
  }
  if (count == 0) return ScalarReal(NA_REAL);
  return ScalarReal(sum / count);
}
