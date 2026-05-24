// Task 5: Filter x > 0, compute z = x/y, sum by group. Returns data.frame.
#include <Rinternals.h>
#include <string.h>

SEXP c_call_bench_dataframe(SEXP df_sexp) {
  SEXP names = getAttrib(df_sexp, R_NamesSymbol);
  R_xlen_t ncols = XLENGTH(df_sexp);

  SEXP x_sexp = R_NilValue, y_sexp = R_NilValue, grp_sexp = R_NilValue;
  for (R_xlen_t i = 0; i < ncols; i++) {
    const char *nm = CHAR(STRING_ELT(names, i));
    if (strcmp(nm, "x") == 0)        x_sexp = VECTOR_ELT(df_sexp, i);
    else if (strcmp(nm, "y") == 0)   y_sexp = VECTOR_ELT(df_sexp, i);
    else if (strcmp(nm, "grp") == 0) grp_sexp = VECTOR_ELT(df_sexp, i);
  }
  if (x_sexp == R_NilValue || y_sexp == R_NilValue || grp_sexp == R_NilValue)
    error("Missing required columns");

  double *x = REAL(x_sexp), *y = REAL(y_sexp);
  int *grp = INTEGER(grp_sexp);
  R_xlen_t nrows = XLENGTH(x_sexp);

  int max_grp = 0;
  for (R_xlen_t i = 0; i < nrows; i++) {
    if (x[i] > 0.0 && grp[i] > max_grp) max_grp = grp[i];
  }

  double *sums = (double *)R_alloc(max_grp, sizeof(double));
  memset(sums, 0, max_grp * sizeof(double));

  for (R_xlen_t i = 0; i < nrows; i++) {
    if (x[i] > 0.0) {
      sums[grp[i] - 1] += x[i] / y[i];
    }
  }

  SEXP grp_out = PROTECT(allocVector(INTSXP, max_grp));
  SEXP sum_out = PROTECT(allocVector(REALSXP, max_grp));
  int *gp = INTEGER(grp_out);
  double *sp = REAL(sum_out);
  for (int i = 0; i < max_grp; i++) {
    gp[i] = i + 1;
    sp[i] = sums[i];
  }

  SEXP col_names = PROTECT(allocVector(STRSXP, 2));
  SET_STRING_ELT(col_names, 0, mkChar("grp"));
  SET_STRING_ELT(col_names, 1, mkChar("z_sum"));

  SEXP result = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, grp_out);
  SET_VECTOR_ELT(result, 1, sum_out);
  setAttrib(result, R_NamesSymbol, col_names);

  SEXP cls = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(cls, 0, mkChar("data.frame"));
  setAttrib(result, R_ClassSymbol, cls);

  SEXP rn = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rn)[0] = NA_INTEGER;
  INTEGER(rn)[1] = -max_grp;
  setAttrib(result, R_RowNamesSymbol, rn);

  UNPROTECT(6);
  return result;
}
