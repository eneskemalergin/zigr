#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void R_init_extendr_benchmarks_extendr(void *dll);
void register_extendr_panic_hook(void);

SEXP extendr_ffi_matmul(SEXP a, SEXP b);
SEXP extendr_ffi_struct_convert(SEXP x);
SEXP extendr_ffi_external_ptr(SEXP x);
SEXP extendr_ffi_rng_stress(SEXP x);

static const R_CallMethodDef CallEntries[] = {
  {"extendr_ffi_matmul",         (DL_FUNC) &extendr_ffi_matmul,         2},
  {"extendr_ffi_struct_convert", (DL_FUNC) &extendr_ffi_struct_convert, 1},
  {"extendr_ffi_external_ptr",   (DL_FUNC) &extendr_ffi_external_ptr,   1},
  {"extendr_ffi_rng_stress",     (DL_FUNC) &extendr_ffi_rng_stress,     1},
  {NULL, NULL, 0}
};

void R_init_extendr_benchmarks(void *dll) {
  register_extendr_panic_hook();
  R_init_extendr_benchmarks_extendr(dll);
  R_registerRoutines((DllInfo *)dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols((DllInfo *)dll, TRUE);
}
