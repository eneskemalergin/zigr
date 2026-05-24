#include <Rinternals.h>

extern SEXP c_call_bench_altrep_create(SEXP);

SEXP c_call_bench_owned_altrep_real_sum(SEXP n_sexp) {
  SEXP vec = c_call_bench_altrep_create(n_sexp);
  R_xlen_t n = XLENGTH(vec);
  double *data = REAL(vec);
  double total = 0.0;
  for (R_xlen_t i = 0; i < n; ++i) total += data[i];
  return ScalarReal(total);
}