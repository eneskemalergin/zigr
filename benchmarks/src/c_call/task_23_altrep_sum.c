#include <Rinternals.h>

// Sum of an ALTREP compact integer sequence via R's sum() generic so every
// backend measures the same ALTREP-aware dispatch path.
SEXP c_call_bench_altrep_sum(SEXP sexp) {
  SEXP sum_sym = PROTECT(Rf_install("sum"));
  SEXP call = PROTECT(Rf_lang2(sum_sym, sexp));
  SEXP result = Rf_eval(call, R_GlobalEnv);
  UNPROTECT(2);
  return result;
}

