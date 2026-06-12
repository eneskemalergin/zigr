#include <R.h>
#include <Rinternals.h>
#include <string.h>

SEXP c_call_bench_dataframe_filter(SEXP arg) {
    SEXP names = Rf_getAttrib(arg, R_NamesSymbol);
    R_xlen_t ncols = XLENGTH(arg);
    SEXP x_col = R_NilValue, y_col = R_NilValue, grp_col = R_NilValue;

    for (R_xlen_t i = 0; i < ncols; i++) {
        SEXP nm = STRING_ELT(names, i);
        if (nm == R_NaString) continue;
        const char *s = CHAR(nm);
        if (strcmp(s, "x") == 0) x_col = VECTOR_ELT(arg, i);
        else if (strcmp(s, "y") == 0) y_col = VECTOR_ELT(arg, i);
        else if (strcmp(s, "grp") == 0) grp_col = VECTOR_ELT(arg, i);
    }

    R_xlen_t nrows = XLENGTH(x_col);
    double *xp = REAL(x_col);
    double *yp = REAL(y_col);
    int *gp = INTEGER(grp_col);

    int max_grp = 0;
    for (R_xlen_t i = 0; i < nrows; i++) {
        if (xp[i] > 0.0 && gp[i] > max_grp) max_grp = gp[i];
    }

    int ngroups = max_grp;
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
    SEXP grp_out = PROTECT(Rf_allocVector(INTSXP, ngroups));
    SEXP sum_out = PROTECT(Rf_allocVector(REALSXP, ngroups));
    
    int *gop = INTEGER(grp_out);
    double *sop = REAL(sum_out);
    memset(sop, 0, ngroups * sizeof(double));
    for (int i = 0; i < ngroups; i++) gop[i] = i + 1;

    for (R_xlen_t i = 0; i < nrows; i++) {
        if (xp[i] > 0.0) {
            int g = gp[i] - 1;
            if (g >= 0 && g < ngroups) sop[g] += xp[i] / yp[i];
        }
    }

    SET_VECTOR_ELT(result, 0, grp_out);
    SET_VECTOR_ELT(result, 1, sum_out);
    
    SEXP rn = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(rn, 0, Rf_mkChar("grp"));
    SET_STRING_ELT(rn, 1, Rf_mkChar("z_sum"));
    Rf_setAttrib(result, R_NamesSymbol, rn);
    
    SEXP cls = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(cls, 0, Rf_mkChar("data.frame"));
    Rf_setAttrib(result, R_ClassSymbol, cls);
    
    SEXP rns = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(rns)[0] = NA_INTEGER;
    INTEGER(rns)[1] = -ngroups;
    Rf_setAttrib(result, R_RowNamesSymbol, rns);
    
    UNPROTECT(6);
    return result;
}
