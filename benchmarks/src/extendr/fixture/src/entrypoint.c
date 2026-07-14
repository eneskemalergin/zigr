#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

void R_init_zigr_extendr_extendr(void *dll);
void register_extendr_panic_hook(void);

void attribute_visible R_init_zigrExtendr(DllInfo *dll) {
  register_extendr_panic_hook();
  R_init_zigr_extendr_extendr(dll);
}

