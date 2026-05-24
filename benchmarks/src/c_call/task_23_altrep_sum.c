#include <Rinternals.h>

// Sum of an ALTREP compact integer sequence.
// C forces materialization by calling INTEGER() first,
// which copies the full ALTREP vector to RAM before summing.
SEXP c_call_bench_altrep_sum(SEXP sexp) {
  R_xlen_t n = XLENGTH(sexp);
  int *data = INTEGER(sexp);       // forces materialization
  double total = 0.0;              // use double to avoid overflow
  for (R_xlen_t i = 0; i < n; i++) total += data[i];
  return ScalarReal(total);
}

