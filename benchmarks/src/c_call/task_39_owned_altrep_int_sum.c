#include <Rinternals.h>
#include <R_ext/Altrep.h>
#include <string.h>

#define ALTREP_OWNED_INT_MAX_LEN 1000000

static R_altrep_class_t c_call_owned_altint_class;
static int c_call_owned_altint_class_ready = 0;
static int c_call_owned_altint_backing[ALTREP_OWNED_INT_MAX_LEN];
static R_xlen_t c_call_owned_altint_backing_init = 0;

static R_xlen_t c_call_owned_altint_length(SEXP x) {
  return INTEGER(R_altrep_data2(x))[0];
}

static int c_call_owned_altint_elt(SEXP x, R_xlen_t i) {
  (void)x;
  return c_call_owned_altint_backing[i];
}

static void *c_call_owned_altint_dataptr(SEXP x, Rboolean writable) {
  (void)x;
  (void)writable;
  return (void *) c_call_owned_altint_backing;
}

static R_xlen_t c_call_owned_altint_get_region(SEXP x, R_xlen_t i, R_xlen_t n, int *buf) {
  R_xlen_t len = c_call_owned_altint_length(x);
  if (i >= len) return 0;
  R_xlen_t available = len - i;
  R_xlen_t count = n < available ? n : available;
  memcpy(buf, c_call_owned_altint_backing + i, (size_t) count * sizeof(int));
  return count;
}

static SEXP c_call_owned_altint_duplicate(SEXP x, Rboolean deep) {
  (void)deep;
  R_xlen_t len = c_call_owned_altint_length(x);
  SEXP out = PROTECT(allocVector(INTSXP, len));
  memcpy(INTEGER(out), c_call_owned_altint_backing, (size_t) len * sizeof(int));
  UNPROTECT(1);
  return out;
}

static void c_call_owned_altint_ensure_class(void) {
  if (c_call_owned_altint_class_ready) return;

  c_call_owned_altint_class = R_make_altinteger_class("bench_altinteger_owned_sum_c",
                                                      "c_call_benchmarks",
                                                      NULL);
  R_set_altrep_Length_method(c_call_owned_altint_class, c_call_owned_altint_length);
  R_set_altinteger_Elt_method(c_call_owned_altint_class, c_call_owned_altint_elt);
  R_set_altvec_Dataptr_method(c_call_owned_altint_class, c_call_owned_altint_dataptr);
  R_set_altrep_Duplicate_method(c_call_owned_altint_class, c_call_owned_altint_duplicate);
  R_set_altinteger_Get_region_method(c_call_owned_altint_class, c_call_owned_altint_get_region);
  c_call_owned_altint_class_ready = 1;
}

static void c_call_owned_altint_ensure_backing(R_xlen_t n) {
  if (c_call_owned_altint_backing_init >= n) return;

  for (R_xlen_t i = c_call_owned_altint_backing_init; i < n; ++i) {
    c_call_owned_altint_backing[i] = (int)((i % 1024) + 1);
  }
  c_call_owned_altint_backing_init = n;
}

SEXP c_call_bench_owned_altrep_int_sum(SEXP n_sexp) {
  int n_int = Rf_asInteger(n_sexp);
  if (n_int < 0 || n_int > ALTREP_OWNED_INT_MAX_LEN) {
    Rf_error("n out of range for c_call_bench_owned_altrep_int_sum");
  }

  R_xlen_t n = (R_xlen_t) n_int;
  c_call_owned_altint_ensure_class();
  c_call_owned_altint_ensure_backing(n);

  SEXP len = PROTECT(ScalarInteger(n_int));
  SEXP vec = R_new_altrep(c_call_owned_altint_class, R_NilValue, len);
  UNPROTECT(1);

  int *data = INTEGER(vec);
  double total = 0.0;
  for (R_xlen_t i = 0; i < n; ++i) total += (double)data[i];
  return ScalarReal(total);
}