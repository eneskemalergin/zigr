#include <Rinternals.h>
#include <string.h>

SEXP c_call_bench_string_nchar(SEXP vec) {
  R_xlen_t n = XLENGTH(vec);
  long total = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    SEXP elt = STRING_ELT(vec, i);
    if (elt != NA_STRING) total += strlen(CHAR(elt));
  }
  return ScalarInteger((int)total);
}
