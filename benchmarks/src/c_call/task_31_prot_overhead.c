#include <Rinternals.h>

#define PROT_OVERHEAD_REPEATS 4096
#define PROT_OVERHEAD_STRATEGIES 5

static double c_call_fill_sum(double *dst, const double *src, R_xlen_t n, double bias) {
  double total = 0.0;
  for (R_xlen_t i = 0; i < n; ++i) {
    double adjusted = src[i] + bias;
    dst[i] = adjusted;
    total += adjusted;
  }
  return total;
}

static void c_call_set_prot_names(SEXP result) {
  SEXP names = PROTECT(allocVector(STRSXP, PROT_OVERHEAD_STRATEGIES));
  SET_STRING_ELT(names, 0, mkChar("unsafe"));
  SET_STRING_ELT(names, 1, mkChar("manual"));
  SET_STRING_ELT(names, 2, mkChar("batch"));
  SET_STRING_ELT(names, 3, mkChar("preserve"));
  SET_STRING_ELT(names, 4, mkChar("reprotect"));
  setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(1);
}

SEXP c_call_bench_prot_overhead(SEXP vec) {
  const double *src = REAL(vec);
  R_xlen_t n = XLENGTH(vec);
  SEXP result = PROTECT(allocVector(REALSXP, PROT_OVERHEAD_STRATEGIES));
  double *out = REAL(result);

  double unsafe_total = 0.0;
  for (int repeat = 0; repeat < PROT_OVERHEAD_REPEATS; ++repeat) {
    double bias = ((double)repeat + 1.0) * 0.001;
    SEXP temp = allocVector(REALSXP, n);
    unsafe_total += c_call_fill_sum(REAL(temp), src, n, bias);
  }
  out[0] = unsafe_total;

  double manual_total = 0.0;
  for (int repeat = 0; repeat < PROT_OVERHEAD_REPEATS; ++repeat) {
    double bias = ((double)repeat + 1.0) * 0.001;
    SEXP temp = PROTECT(allocVector(REALSXP, n));
    manual_total += c_call_fill_sum(REAL(temp), src, n, bias);
    UNPROTECT(1);
  }
  out[1] = manual_total;

  double batch_total = 0.0;
  for (int repeat = 0; repeat < PROT_OVERHEAD_REPEATS; ++repeat) {
    double bias = ((double)repeat + 1.0) * 0.001;
    SEXP temp = PROTECT(allocVector(REALSXP, n));
    batch_total += c_call_fill_sum(REAL(temp), src, n, bias);
  }
  UNPROTECT(PROT_OVERHEAD_REPEATS);
  out[2] = batch_total;

  double preserve_total = 0.0;
  for (int repeat = 0; repeat < PROT_OVERHEAD_REPEATS; ++repeat) {
    double bias = ((double)repeat + 1.0) * 0.001;
    SEXP temp = allocVector(REALSXP, n);
    R_PreserveObject(temp);
    preserve_total += c_call_fill_sum(REAL(temp), src, n, bias);
    R_ReleaseObject(temp);
  }
  out[3] = preserve_total;

  double reprotect_total = 0.0;
  PROTECT_INDEX protect_index;
  PROTECT_WITH_INDEX(R_NilValue, &protect_index);
  for (int repeat = 0; repeat < PROT_OVERHEAD_REPEATS; ++repeat) {
    double bias = ((double)repeat + 1.0) * 0.001;
    SEXP temp = allocVector(REALSXP, n);
    REPROTECT(temp, protect_index);
    reprotect_total += c_call_fill_sum(REAL(temp), src, n, bias);
  }
  UNPROTECT(1);
  out[4] = reprotect_total;

  c_call_set_prot_names(result);
  UNPROTECT(1);
  return result;
}