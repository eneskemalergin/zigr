#include <Rinternals.h>

static double c_call_na_mean_one(SEXP vec) {
  double *data = REAL(vec);
  R_xlen_t n = XLENGTH(vec);
  double sum = 0.0;
  R_xlen_t count = 0;

  for (R_xlen_t i = 0; i < n; ++i) {
    if (ISNA(data[i])) continue;
    sum += data[i];
    count++;
  }

  return count == 0 ? NA_REAL : sum / (double)count;
}

SEXP c_call_bench_na_prop_vary(SEXP inputs_sexp) {
  R_xlen_t n_inputs = XLENGTH(inputs_sexp);
  SEXP result = PROTECT(allocVector(REALSXP, n_inputs));

  for (R_xlen_t i = 0; i < n_inputs; ++i) {
    REAL(result)[i] = c_call_na_mean_one(VECTOR_ELT(inputs_sexp, i));
  }

  setAttrib(result, R_NamesSymbol, getAttrib(inputs_sexp, R_NamesSymbol));
  UNPROTECT(1);
  return result;
}