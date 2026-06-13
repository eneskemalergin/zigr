#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_struct_convert(SEXP arg) {
    int id = asInteger(VECTOR_ELT(arg, 0));
    int count = asInteger(VECTOR_ELT(arg, 1));
    int level = asInteger(VECTOR_ELT(arg, 2));
    int flag = asLogical(VECTOR_ELT(arg, 3));
    int enabled = asLogical(VECTOR_ELT(arg, 4));
    double ratio = asReal(VECTOR_ELT(arg, 5));
    double offset = asReal(VECTOR_ELT(arg, 6));
    double scale = asReal(VECTOR_ELT(arg, 7));
    int weights_len = LENGTH(VECTOR_ELT(arg, 8));
    double *weights = REAL(VECTOR_ELT(arg, 8));
    int indices_len = LENGTH(VECTOR_ELT(arg, 9));
    int *indices = INTEGER(VECTOR_ELT(arg, 9));

    double ws = 0;
    for (int i = 0; i < weights_len; i++) ws += weights[i];
    int is = 0;
    for (int i = 0; i < indices_len; i++) is += indices[i];

    return Rf_ScalarReal(id + count + level + flag + enabled + ratio + offset + scale + ws + is);
}
