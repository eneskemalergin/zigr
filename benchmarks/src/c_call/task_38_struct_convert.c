#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_struct_convert(SEXP arg) {
    const char *field_names[] = {"id", "count", "level", "flag", "enabled",
                                 "ratio", "offset", "scale", "weights", "indices"};

    SEXP names = PROTECT(Rf_allocVector(STRSXP, 10));
    for (int i = 0; i < 10; i++)
        SET_STRING_ELT(names, i, Rf_mkChar(field_names[i]));

    SEXP result = PROTECT(Rf_allocVector(VECSXP, 10));
    Rf_setAttrib(result, R_NamesSymbol, names);

    SET_VECTOR_ELT(result, 0, Rf_ScalarInteger(asInteger(VECTOR_ELT(arg, 0))));
    SET_VECTOR_ELT(result, 1, Rf_ScalarInteger(asInteger(VECTOR_ELT(arg, 1))));
    SET_VECTOR_ELT(result, 2, Rf_ScalarInteger(asInteger(VECTOR_ELT(arg, 2))));
    SET_VECTOR_ELT(result, 3, Rf_ScalarLogical(asLogical(VECTOR_ELT(arg, 3))));
    SET_VECTOR_ELT(result, 4, Rf_ScalarLogical(asLogical(VECTOR_ELT(arg, 4))));
    SET_VECTOR_ELT(result, 5, Rf_ScalarReal(asReal(VECTOR_ELT(arg, 5))));
    SET_VECTOR_ELT(result, 6, Rf_ScalarReal(asReal(VECTOR_ELT(arg, 6))));
    SET_VECTOR_ELT(result, 7, Rf_ScalarReal(asReal(VECTOR_ELT(arg, 7))));
    SET_VECTOR_ELT(result, 8, VECTOR_ELT(arg, 8));
    SET_VECTOR_ELT(result, 9, VECTOR_ELT(arg, 9));

    UNPROTECT(2);
    return result;
}
