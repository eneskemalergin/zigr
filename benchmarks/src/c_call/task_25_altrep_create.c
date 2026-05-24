#include <Rinternals.h>
#include <R_ext/Altrep.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h>
#include <string.h>

#define ALTREP_CREATE_MAX_LEN 1000000

typedef struct {
  const double *ptr;
  R_xlen_t len;
} altrep_create_wrap_t;

static R_altrep_class_t c_call_altrep_create_class;
static int c_call_altrep_create_class_ready = 0;
static double c_call_altrep_backing[ALTREP_CREATE_MAX_LEN];
static R_xlen_t c_call_altrep_backing_init = 0;

static void c_call_altrep_create_finalize(SEXP ext) {
  void *ptr = R_ExternalPtrAddr(ext);
  if (ptr != NULL) {
    free(ptr);
    R_ClearExternalPtr(ext);
  }
}

static altrep_create_wrap_t *c_call_altrep_create_wrap(SEXP x) {
  return (altrep_create_wrap_t *) R_ExternalPtrAddr(R_altrep_data1(x));
}

static R_xlen_t c_call_altrep_create_length(SEXP x) {
  return c_call_altrep_create_wrap(x)->len;
}

static double c_call_altrep_create_elt(SEXP x, R_xlen_t i) {
  altrep_create_wrap_t *wrap = c_call_altrep_create_wrap(x);
  return wrap->ptr[i];
}

static void *c_call_altrep_create_dataptr(SEXP x, Rboolean writable) {
  altrep_create_wrap_t *wrap = c_call_altrep_create_wrap(x);
  return (void *) wrap->ptr;
}

static R_xlen_t c_call_altrep_create_get_region(SEXP x, R_xlen_t i, R_xlen_t n, double *buf) {
  altrep_create_wrap_t *wrap = c_call_altrep_create_wrap(x);
  if (i >= wrap->len) return 0;

  R_xlen_t available = wrap->len - i;
  R_xlen_t count = n < available ? n : available;
  memcpy(buf, wrap->ptr + i, (size_t) count * sizeof(double));
  return count;
}

static SEXP c_call_altrep_create_duplicate(SEXP x, Rboolean deep) {
  altrep_create_wrap_t *wrap = c_call_altrep_create_wrap(x);
  SEXP out = PROTECT(allocVector(REALSXP, wrap->len));
  memcpy(REAL(out), wrap->ptr, (size_t) wrap->len * sizeof(double));
  UNPROTECT(1);
  return out;
}

static void c_call_altrep_create_ensure_class(void) {
  if (c_call_altrep_create_class_ready) return;

  c_call_altrep_create_class = R_make_altreal_class("bench_altreal_create_c",
                                                    "c_call_benchmarks",
                                                    NULL);
  R_set_altrep_Length_method(c_call_altrep_create_class, c_call_altrep_create_length);
  R_set_altreal_Elt_method(c_call_altrep_create_class, c_call_altrep_create_elt);
  R_set_altvec_Dataptr_method(c_call_altrep_create_class, c_call_altrep_create_dataptr);
  R_set_altrep_Duplicate_method(c_call_altrep_create_class, c_call_altrep_create_duplicate);
  R_set_altreal_Get_region_method(c_call_altrep_create_class, c_call_altrep_create_get_region);
  c_call_altrep_create_class_ready = 1;
}

static void c_call_altrep_create_ensure_backing(R_xlen_t n) {
  if (c_call_altrep_backing_init >= n) return;

  for (R_xlen_t i = c_call_altrep_backing_init; i < n; ++i) {
    c_call_altrep_backing[i] = (double) (i + 1);
  }
  c_call_altrep_backing_init = n;
}

SEXP c_call_bench_altrep_create(SEXP n_sexp) {
  int n_int = Rf_asInteger(n_sexp);
  if (n_int < 0 || n_int > ALTREP_CREATE_MAX_LEN) {
    Rf_error("n out of range for c_call_bench_altrep_create");
  }

  R_xlen_t n = (R_xlen_t) n_int;
  c_call_altrep_create_ensure_class();
  c_call_altrep_create_ensure_backing(n);

  altrep_create_wrap_t *wrap = (altrep_create_wrap_t *) malloc(sizeof(*wrap));
  if (wrap == NULL) {
    Rf_error("OOM in c_call_bench_altrep_create");
  }

  wrap->ptr = c_call_altrep_backing;
  wrap->len = n;

  SEXP data1 = PROTECT(R_MakeExternalPtr(wrap, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(data1, c_call_altrep_create_finalize, TRUE);
  SEXP out = R_new_altrep(c_call_altrep_create_class, data1, R_NilValue);
  UNPROTECT(1);
  return out;
}