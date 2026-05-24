#include <Rinternals.h>
#include <math.h>

#define TRANSLATE_C_COST_REPEATS 512

SEXP c_call_bench_translate_c_cost(SEXP vec) {
  const double *src = REAL(vec);
  R_xlen_t n = XLENGTH(vec);
  SEXP result = PROTECT(allocVector(REALSXP, 4));
  SEXP names = PROTECT(allocVector(STRSXP, 4));
  double *out = REAL(result);

  double abs_total = 0.0;
  double log_total = 0.0;
  double exp_total = 0.0;
  double sqrt_total = 0.0;

  for (int repeat = 0; repeat < TRANSLATE_C_COST_REPEATS; ++repeat) {
    double bias = ((double)repeat + 1.0) * 0.001;
    for (R_xlen_t i = 0; i < n; ++i) {
      double shifted = src[i] + bias;
      abs_total += fabs(shifted - 0.75);
      log_total += log(shifted);
      exp_total += exp(shifted);
      sqrt_total += sqrt(shifted);
    }
  }

  out[0] = abs_total;
  out[1] = log_total;
  out[2] = exp_total;
  out[3] = sqrt_total;

  SET_STRING_ELT(names, 0, mkChar("abs"));
  SET_STRING_ELT(names, 1, mkChar("log"));
  SET_STRING_ELT(names, 2, mkChar("exp"));
  SET_STRING_ELT(names, 3, mkChar("sqrt"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(2);
  return result;
}