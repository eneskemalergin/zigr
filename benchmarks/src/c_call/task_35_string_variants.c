#include <Rinternals.h>
#include <ctype.h>
#include <string.h>

static int c_call_has_prefix_abc(const char *s) {
  return s[0] == 'a' && s[1] == 'b' && s[2] == 'c';
}

SEXP c_call_bench_string_variants(SEXP vec) {
  R_xlen_t n = XLENGTH(vec);
  size_t concat_len = 0;
  int valid_count = 0;
  int nchar_sum = 0;
  int prefix_match = 0;

  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP elt = STRING_ELT(vec, i);
    if (elt == NA_STRING) continue;
    const char *s = CHAR(elt);
    size_t slen = strlen(s);
    concat_len += slen;
    valid_count += 1;
    nchar_sum += (int)slen;
    if (slen >= 3 && c_call_has_prefix_abc(s)) prefix_match += 1;
  }
  if (valid_count > 0) concat_len += (size_t)(valid_count - 1);

  SEXP result = PROTECT(allocVector(VECSXP, 5));
  SEXP names = PROTECT(allocVector(STRSXP, 5));
  SEXP concat = PROTECT(allocVector(STRSXP, 1));
  SEXP nchar_value = PROTECT(ScalarInteger(nchar_sum));
  SEXP prefix_value = PROTECT(ScalarInteger(prefix_match));
  SEXP extract = PROTECT(allocVector(STRSXP, n));
  SEXP upper = PROTECT(allocVector(STRSXP, n));

  if (concat_len == 0) {
    SET_STRING_ELT(concat, 0, mkChar(""));
  } else {
    char *buf = (char *)R_alloc(concat_len + 1, sizeof(char));
    size_t pos = 0;
    int added = 0;
    for (R_xlen_t i = 0; i < n; ++i) {
      SEXP elt = STRING_ELT(vec, i);
      if (elt == NA_STRING) continue;
      const char *s = CHAR(elt);
      size_t slen = strlen(s);
      if (added > 0) buf[pos++] = ',';
      memcpy(buf + pos, s, slen);
      pos += slen;
      added += 1;
    }
    buf[concat_len] = '\0';
    SET_STRING_ELT(concat, 0, mkChar(buf));
  }

  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP elt = STRING_ELT(vec, i);
    if (elt == NA_STRING) {
      SET_STRING_ELT(extract, i, NA_STRING);
      SET_STRING_ELT(upper, i, NA_STRING);
      continue;
    }

    const char *s = CHAR(elt);
    size_t slen = strlen(s);
    size_t sub_len = slen < 3 ? slen : 3;
    SET_STRING_ELT(extract, i, mkCharLen(s, (int)sub_len));

    char *upper_buf = (char *)R_alloc(slen + 1, sizeof(char));
    for (size_t j = 0; j < slen; ++j) {
      upper_buf[j] = (char)toupper((unsigned char)s[j]);
    }
    upper_buf[slen] = '\0';
    SET_STRING_ELT(upper, i, mkChar(upper_buf));
  }

  SET_VECTOR_ELT(result, 0, concat);
  SET_VECTOR_ELT(result, 1, nchar_value);
  SET_VECTOR_ELT(result, 2, prefix_value);
  SET_VECTOR_ELT(result, 3, extract);
  SET_VECTOR_ELT(result, 4, upper);

  SET_STRING_ELT(names, 0, mkChar("concat"));
  SET_STRING_ELT(names, 1, mkChar("nchar_sum"));
  SET_STRING_ELT(names, 2, mkChar("prefix_match"));
  SET_STRING_ELT(names, 3, mkChar("extract_substr"));
  SET_STRING_ELT(names, 4, mkChar("to_upper"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(7);
  return result;
}