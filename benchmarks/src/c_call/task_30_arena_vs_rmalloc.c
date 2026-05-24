#include <Rinternals.h>
#include <stdlib.h>

static double c_call_sum_temp(const double *data, R_xlen_t n) {
  double total = 0.0;
  for (R_xlen_t i = 0; i < n; ++i) {
    total += data[i];
  }
  return total;
}

SEXP c_call_bench_arena_vs_rmalloc(SEXP vec) {
  double *src = REAL(vec);
  R_xlen_t n = XLENGTH(vec);
  double total = 0.0;

  for (int repeat = 0; repeat < 100; ++repeat) {
    double bias = ((double)repeat + 1.0) * 0.001;
    double *temp = (double *)malloc((size_t)n * sizeof(double));
    if (temp == NULL) {
      Rf_error("malloc failed in c_call_bench_arena_vs_rmalloc");
    }

    for (R_xlen_t i = 0; i < n; ++i) {
      temp[i] = src[i] + bias;
    }
    total += c_call_sum_temp(temp, n);
    free(temp);
  }

  return ScalarReal(total);
}