#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <stdlib.h>

#define REPEATS 2
#define N_STRATS 3

static double sum_slice(double *data, R_xlen_t n) {
  double total = 0.0;
  for (R_xlen_t i = 0; i < n; i++) total += data[i];
  return total;
}

SEXP c_call_bench_memcpy_bandwidth(SEXP arg) {
  double *xp = REAL(arg);
  R_xlen_t n = XLENGTH(arg);

  const char *names[] = {"copy_temp", "copy_out", "fill_out"};
  SEXP result = PROTECT(Rf_allocVector(REALSXP, N_STRATS));
  SEXP rnames = PROTECT(Rf_allocVector(STRSXP, N_STRATS));
  for (int i = 0; i < N_STRATS; i++)
    SET_STRING_ELT(rnames, i, Rf_mkChar(names[i]));
  Rf_setAttrib(result, R_NamesSymbol, rnames);
  double *rp = REAL(result);

  double copy_temp_total = 0.0;
  double copy_out_total = 0.0;
  double fill_out_total = 0.0;

  for (int rep = 0; rep < REPEATS; rep++) {
    double *temp = malloc(n * sizeof(double));
    memcpy(temp, xp, n * sizeof(double));
    copy_temp_total += sum_slice(temp, n);
    free(temp);

    SEXP copy_out = PROTECT(Rf_allocVector(REALSXP, n));
    double *copy_rp = REAL(copy_out);
    memcpy(copy_rp, xp, n * sizeof(double));
    copy_out_total += sum_slice(copy_rp, n);
    UNPROTECT(1);

    SEXP fill_out = PROTECT(Rf_allocVector(REALSXP, n));
    double *fill_rp = REAL(fill_out);
    for (R_xlen_t i = 0; i < n; i++) fill_rp[i] = xp[i] + 0.5;
    fill_out_total += sum_slice(fill_rp, n);
    UNPROTECT(1);
  }

  rp[0] = copy_temp_total;
  rp[1] = copy_out_total;
  rp[2] = fill_out_total;
  UNPROTECT(2);
  return result;
}
