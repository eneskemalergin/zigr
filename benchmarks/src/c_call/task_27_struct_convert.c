#include <Rinternals.h>
#include <string.h>

typedef struct {
  int id;
  int count;
  int level;
  int flag;
  int enabled;
  double ratio;
  double offset;
  double scale;
  double *weights;
  R_xlen_t weights_len;
  int *indices;
  R_xlen_t indices_len;
} struct_convert_payload_t;

static SEXP c_call_find_named(SEXP list_sexp, const char *name) {
  SEXP names = getAttrib(list_sexp, R_NamesSymbol);
  R_xlen_t n = XLENGTH(list_sexp);
  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP elt = STRING_ELT(names, i);
    if (elt != NA_STRING && strcmp(CHAR(elt), name) == 0) {
      return VECTOR_ELT(list_sexp, i);
    }
  }
  Rf_error("missing field '%s' in c_call_bench_struct_convert", name);
  return R_NilValue;
}

SEXP c_call_bench_struct_convert(SEXP input_sexp) {
  struct_convert_payload_t payload;
  SEXP weights_sexp = c_call_find_named(input_sexp, "weights");
  SEXP indices_sexp = c_call_find_named(input_sexp, "indices");

  payload.id = INTEGER(c_call_find_named(input_sexp, "id"))[0];
  payload.count = INTEGER(c_call_find_named(input_sexp, "count"))[0];
  payload.level = INTEGER(c_call_find_named(input_sexp, "level"))[0];
  payload.flag = LOGICAL(c_call_find_named(input_sexp, "flag"))[0];
  payload.enabled = LOGICAL(c_call_find_named(input_sexp, "enabled"))[0];
  payload.ratio = REAL(c_call_find_named(input_sexp, "ratio"))[0];
  payload.offset = REAL(c_call_find_named(input_sexp, "offset"))[0];
  payload.scale = REAL(c_call_find_named(input_sexp, "scale"))[0];

  payload.weights_len = XLENGTH(weights_sexp);
  payload.weights = (double *)R_alloc((size_t)payload.weights_len, sizeof(double));
  memcpy(payload.weights, REAL(weights_sexp), (size_t)payload.weights_len * sizeof(double));

  payload.indices_len = XLENGTH(indices_sexp);
  payload.indices = (int *)R_alloc((size_t)payload.indices_len, sizeof(int));
  memcpy(payload.indices, INTEGER(indices_sexp), (size_t)payload.indices_len * sizeof(int));

  SEXP result = PROTECT(allocVector(VECSXP, 10));
  SEXP names = PROTECT(allocVector(STRSXP, 10));
  SEXP weights_out = PROTECT(allocVector(REALSXP, payload.weights_len));
  SEXP indices_out = PROTECT(allocVector(INTSXP, payload.indices_len));

  memcpy(REAL(weights_out), payload.weights, (size_t)payload.weights_len * sizeof(double));
  memcpy(INTEGER(indices_out), payload.indices, (size_t)payload.indices_len * sizeof(int));

  SET_VECTOR_ELT(result, 0, ScalarInteger(payload.id));
  SET_VECTOR_ELT(result, 1, ScalarInteger(payload.count));
  SET_VECTOR_ELT(result, 2, ScalarInteger(payload.level));
  SET_VECTOR_ELT(result, 3, ScalarLogical(payload.flag));
  SET_VECTOR_ELT(result, 4, ScalarLogical(payload.enabled));
  SET_VECTOR_ELT(result, 5, ScalarReal(payload.ratio));
  SET_VECTOR_ELT(result, 6, ScalarReal(payload.offset));
  SET_VECTOR_ELT(result, 7, ScalarReal(payload.scale));
  SET_VECTOR_ELT(result, 8, weights_out);
  SET_VECTOR_ELT(result, 9, indices_out);

  SET_STRING_ELT(names, 0, mkChar("id"));
  SET_STRING_ELT(names, 1, mkChar("count"));
  SET_STRING_ELT(names, 2, mkChar("level"));
  SET_STRING_ELT(names, 3, mkChar("flag"));
  SET_STRING_ELT(names, 4, mkChar("enabled"));
  SET_STRING_ELT(names, 5, mkChar("ratio"));
  SET_STRING_ELT(names, 6, mkChar("offset"));
  SET_STRING_ELT(names, 7, mkChar("scale"));
  SET_STRING_ELT(names, 8, mkChar("weights"));
  SET_STRING_ELT(names, 9, mkChar("indices"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(4);
  return result;
}