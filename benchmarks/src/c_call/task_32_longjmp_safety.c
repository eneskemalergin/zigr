#include <Rinternals.h>

#define LONGJMP_SAFETY_REPEATS 512

typedef struct {
  const double *data;
  R_xlen_t len;
  double bias;
} c_call_unwind_state_t;

static double c_call_adjusted_sum(const double *data, R_xlen_t len, double bias) {
  double total = 0.0;
  for (R_xlen_t i = 0; i < len; ++i) total += data[i] + bias;
  return total;
}

static void c_call_fill_adjusted(SEXP vec, const double *data, R_xlen_t len, double bias) {
  double *dst = REAL(vec);
  for (R_xlen_t i = 0; i < len; ++i) dst[i] = data[i] + bias;
}

static void c_call_noop_clean(void *data, Rboolean jump) {
  (void)data;
  (void)jump;
}

static SEXP c_call_unwind_ok(void *data) {
  c_call_unwind_state_t *state = (c_call_unwind_state_t *)data;
  return ScalarReal(c_call_adjusted_sum(state->data, state->len, state->bias));
}

SEXP c_call_bench_longjmp_safety(SEXP vec) {
  const double *src = REAL(vec);
  R_xlen_t n = XLENGTH(vec);
  SEXP result = PROTECT(allocVector(REALSXP, 4));
  SEXP names = PROTECT(allocVector(STRSXP, 4));
  double *out = REAL(result);
  SEXP sum_sym = Rf_install("sum");
  SEXP stop_sym = Rf_install("stop");
  SEXP stop_msg = PROTECT(mkString("task32"));
  SEXP stop_call = PROTECT(lang2(stop_sym, stop_msg));

  double direct_total = 0.0;
  double try_ok_total = 0.0;
  double try_err_total = 0.0;
  double unwind_ok_total = 0.0;

  for (int repeat = 0; repeat < LONGJMP_SAFETY_REPEATS; ++repeat) {
    double bias = ((double)repeat + 1.0) * 0.001;
    direct_total += c_call_adjusted_sum(src, n, bias);

    SEXP temp = PROTECT(allocVector(REALSXP, n));
    c_call_fill_adjusted(temp, src, n, bias);
    SEXP expr = PROTECT(lang2(sum_sym, temp));
    int err = 0;
    SEXP eval_result = R_tryEvalSilent(expr, R_GlobalEnv, &err);
    try_ok_total += REAL(eval_result)[0];
    UNPROTECT(2);

    err = 0;
    R_tryEvalSilent(stop_call, R_GlobalEnv, &err);
    if (err != 0) try_err_total += 1.0;

    c_call_unwind_state_t state = { .data = src, .len = n, .bias = bias };
    SEXP cont = PROTECT(R_MakeUnwindCont());
    SEXP unwind_result = R_UnwindProtect(c_call_unwind_ok, &state, c_call_noop_clean, NULL, cont);
    unwind_ok_total += REAL(unwind_result)[0];
    UNPROTECT(1);
  }

  out[0] = direct_total;
  out[1] = try_ok_total;
  out[2] = try_err_total;
  out[3] = unwind_ok_total;

  SET_STRING_ELT(names, 0, mkChar("direct"));
  SET_STRING_ELT(names, 1, mkChar("try_ok"));
  SET_STRING_ELT(names, 2, mkChar("try_err"));
  SET_STRING_ELT(names, 3, mkChar("unwind_ok"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(4);
  return result;
}