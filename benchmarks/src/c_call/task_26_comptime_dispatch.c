#include <Rinternals.h>

#define COMPTIME_DISPATCH_REPEATS 256

static double c_call_dispatch_sum(SEXP vec) {
  R_xlen_t n = XLENGTH(vec);
  double total = 0.0;

  switch (TYPEOF(vec)) {
    case REALSXP: {
      double *data = REAL(vec);
      for (R_xlen_t i = 0; i < n; ++i) total += data[i];
      return total;
    }
    case INTSXP: {
      int *data = INTEGER(vec);
      for (R_xlen_t i = 0; i < n; ++i) total += (double) data[i];
      return total;
    }
    case LGLSXP: {
      int *data = LOGICAL(vec);
      for (R_xlen_t i = 0; i < n; ++i) total += data[i] != 0 ? 1.0 : 0.0;
      return total;
    }
    default:
      return 0.0;
  }
}

SEXP c_call_bench_comptime_dispatch(SEXP inputs_sexp) {
  R_xlen_t n_inputs = XLENGTH(inputs_sexp);
  double total = 0.0;

  for (int repeat = 0; repeat < COMPTIME_DISPATCH_REPEATS; ++repeat) {
    for (R_xlen_t i = 0; i < n_inputs; ++i) {
      total += c_call_dispatch_sum(VECTOR_ELT(inputs_sexp, i));
    }
  }

  return ScalarReal(total);
}