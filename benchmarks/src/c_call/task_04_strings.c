// Task 4: Concatenate 1e4 strings with separator (O(n) memcpy).
#include <Rinternals.h>
#include <string.h>

SEXP c_call_bench_strings(SEXP vec, SEXP sep_sexp) {
  R_xlen_t n = XLENGTH(vec);
  const char *sep = CHAR(STRING_ELT(sep_sexp, 0));

  size_t total_len = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    if (STRING_ELT(vec, i) != NA_STRING) {
      total_len += strlen(CHAR(STRING_ELT(vec, i)));
    }
  }
  total_len += n * strlen(sep);
  if (total_len == 0) return PROTECT(ScalarString(mkChar("")));

  char *buf = (char *)R_alloc(total_len + 1, sizeof(char));
  size_t pos = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    if (STRING_ELT(vec, i) != NA_STRING) {
      const char *s = CHAR(STRING_ELT(vec, i));
      size_t slen = strlen(s);
      memcpy(buf + pos, s, slen);
      pos += slen;
    }
    size_t seplen = strlen(sep);
    memcpy(buf + pos, sep, seplen);
    pos += seplen;
  }
  buf[pos] = '\0';

  SEXP result = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(result, 0, mkChar(buf));
  UNPROTECT(1);
  return result;
}
