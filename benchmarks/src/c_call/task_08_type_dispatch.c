#include <R.h>
#include <Rinternals.h>

static int classify_type(SEXP x) {
    switch (TYPEOF(x)) {
        case REALSXP: return 1;
        case INTSXP: return 2;
        case STRSXP: return 3;
        default: return 0;
    }
}

SEXP c_call_bench_type_dispatch(SEXP arg) {
    SEXP elts[3] = {
        VECTOR_ELT(arg, 0),
        VECTOR_ELT(arg, 1),
        VECTOR_ELT(arg, 2)
    };
    int total = 0;
    for (int i = 0; i < 2048; i++) {
        total += classify_type(elts[0]);
        total += classify_type(elts[1]);
        total += classify_type(elts[2]);
    }
    return Rf_ScalarInteger(total);
}
