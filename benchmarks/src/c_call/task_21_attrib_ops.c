#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_attrib_ops(SEXP arg) {
    SEXP cls_val = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(cls_val, 0, Rf_mkChar("bench_class"));
    Rf_classgets(arg, cls_val);
    UNPROTECT(1);

    SEXP cr_val = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(cr_val, 0, Rf_mkChar("zigr_bench"));
    Rf_setAttrib(arg, Rf_install("creator"), cr_val);
    UNPROTECT(1);

    SEXP got_cls = Rf_getAttrib(arg, R_ClassSymbol);
    SEXP got_cr = Rf_getAttrib(arg, Rf_install("creator"));

    int total = 0;
    R_xlen_t nc = XLENGTH(got_cls);
    for (R_xlen_t i = 0; i < nc; i++)
        total += LENGTH(STRING_ELT(got_cls, i));
    R_xlen_t ncr = XLENGTH(got_cr);
    for (R_xlen_t i = 0; i < ncr; i++)
        total += LENGTH(STRING_ELT(got_cr, i));

    return Rf_ScalarInteger(total);
}
