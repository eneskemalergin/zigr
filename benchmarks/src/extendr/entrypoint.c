#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void R_init_extendr_benchmarks_extendr(void *dll);
void register_extendr_panic_hook(void);

void R_init_extendr_benchmarks(void *dll) {
  register_extendr_panic_hook();
  R_init_extendr_benchmarks_extendr(dll);
  R_useDynamicSymbols((DllInfo *)dll, TRUE);
}
