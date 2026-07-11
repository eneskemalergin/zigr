// Registration table for all C benchmark tasks.
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP c_call_bench_vectorsum(SEXP);
extern SEXP c_call_bench_elem_ops(SEXP);
extern SEXP c_call_bench_memcpy_bandwidth(SEXP);
extern SEXP c_call_bench_sort(SEXP);
extern SEXP c_call_bench_fib_recursive(SEXP);
extern SEXP c_call_bench_broadcast(SEXP, SEXP);
extern SEXP c_call_bench_protect_shallow(SEXP);
extern SEXP c_call_bench_protect_scaling(SEXP);
extern SEXP c_call_bench_type_dispatch(SEXP);
extern SEXP c_call_bench_longjmp_safety(SEXP);
extern SEXP c_call_bench_sexp_create(SEXP);
extern SEXP c_call_bench_sexp_inspect(SEXP);
extern SEXP c_call_bench_matrix_transpose(SEXP);
extern SEXP c_call_bench_matrix_rowsums(SEXP);
extern SEXP c_call_bench_matrix_rowcol_means(SEXP);
extern SEXP c_call_bench_dataframe_filter(SEXP);
extern SEXP c_call_bench_list_access(SEXP);
extern SEXP c_call_bench_string_concat(SEXP);
extern SEXP c_call_bench_string_nchar(SEXP);
extern SEXP c_call_bench_string_encoding(SEXP);
extern SEXP c_call_bench_factor_ops(SEXP);
extern SEXP c_call_bench_attrib_ops(SEXP);
extern SEXP c_call_bench_s4_slot_access(SEXP);
extern SEXP c_call_bench_na_propagation(SEXP);
extern SEXP c_call_bench_long_vector_idx(SEXP);
extern SEXP c_call_bench_l1_arithmetic(SEXP);
extern SEXP c_call_bench_matmul(SEXP, SEXP);
extern SEXP c_call_bench_crossprod(SEXP);
extern SEXP c_call_bench_cholesky(SEXP);
extern SEXP c_call_bench_lm_fit(SEXP, SEXP);
extern SEXP c_call_bench_altrep_create(SEXP);
extern SEXP c_call_bench_altrep_materialize(SEXP);
extern SEXP c_call_bench_altrep_elt_walk(SEXP);
extern SEXP c_call_bench_altrep_region_read(SEXP);
extern SEXP c_call_bench_altrep_sum_via_R(SEXP);
extern SEXP c_call_bench_altrep_sum_native(SEXP);
extern SEXP c_call_bench_altrep_min_max(SEXP);
extern SEXP c_call_bench_altrep_no_na_query(SEXP);
extern SEXP c_call_bench_struct_convert(SEXP);
extern SEXP c_call_bench_r_eval(SEXP);
extern SEXP c_call_bench_r_tryeval(SEXP);
extern SEXP c_call_bench_serialize_roundtrip(SEXP);
extern SEXP c_call_bench_external_ptr(SEXP);
extern SEXP c_call_bench_rng_stress(SEXP);

static int fixture_state = 0;

static SEXP c_fixture_scalar(SEXP value) {
    if (TYPEOF(value) != REALSXP || XLENGTH(value) != 1) {
        Rf_error("c fixture scalar expected one REAL value");
    }
    return Rf_ScalarReal(REAL(value)[0]);
}

static SEXP c_fixture_vector(SEXP value) {
    if (TYPEOF(value) != REALSXP) {
        Rf_error("c fixture vector expected a REAL vector");
    }
    double total = 0.0;
    for (R_xlen_t i = 0; i < XLENGTH(value); ++i) total += REAL(value)[i];
    return Rf_ScalarReal(total);
}

static SEXP c_fixture_new(void) {
    fixture_state = 0;
    return R_MakeExternalPtr(&fixture_state, R_NilValue, R_NilValue);
}

static SEXP c_fixture_method(SEXP receiver, SEXP amount) {
    if (TYPEOF(receiver) != EXTPTRSXP || R_ExternalPtrAddr(receiver) == NULL) {
        Rf_error("c fixture method expected a live external pointer");
    }
    if (TYPEOF(amount) != INTSXP || XLENGTH(amount) != 1 || INTEGER(amount)[0] == NA_INTEGER) {
        Rf_error("c fixture method expected one non-missing integer");
    }
    int *state = (int *) R_ExternalPtrAddr(receiver);
    *state += INTEGER(amount)[0];
    return Rf_ScalarInteger(*state);
}

static SEXP c_fixture_error(SEXP value) {
    (void) value;
    Rf_error("c fixture error: expected failure");
    return R_NilValue;
}

static SEXP c_fixture_external(SEXP args) {
    if (args == R_NilValue || TYPEOF(CADR(args)) != REALSXP || XLENGTH(CADR(args)) != 1) {
        Rf_error("c fixture external expected one REAL value");
    }
    return Rf_ScalarReal(REAL(CADR(args))[0] + 1.0);
}

static const R_CallMethodDef CallEntries[] = {
  {"c_call_bench_vectorsum",        (DL_FUNC) &c_call_bench_vectorsum,        1},
  {"c_call_bench_elem_ops",         (DL_FUNC) &c_call_bench_elem_ops,         1},
  {"c_call_bench_memcpy_bandwidth", (DL_FUNC) &c_call_bench_memcpy_bandwidth, 1},
  {"c_call_bench_sort",             (DL_FUNC) &c_call_bench_sort,             1},
  {"c_call_bench_fib_recursive",    (DL_FUNC) &c_call_bench_fib_recursive,    1},
  {"c_call_bench_broadcast",        (DL_FUNC) &c_call_bench_broadcast,        2},
  {"c_call_bench_protect_shallow",  (DL_FUNC) &c_call_bench_protect_shallow,  1},
  {"c_call_bench_protect_scaling",  (DL_FUNC) &c_call_bench_protect_scaling,  1},
  {"c_call_bench_type_dispatch",    (DL_FUNC) &c_call_bench_type_dispatch,    1},
  {"c_call_bench_longjmp_safety",   (DL_FUNC) &c_call_bench_longjmp_safety,   1},
  {"c_call_bench_sexp_create",      (DL_FUNC) &c_call_bench_sexp_create,      1},
  {"c_call_bench_sexp_inspect",     (DL_FUNC) &c_call_bench_sexp_inspect,     1},
  {"c_call_bench_matrix_transpose", (DL_FUNC) &c_call_bench_matrix_transpose, 1},
  {"c_call_bench_matrix_rowsums",   (DL_FUNC) &c_call_bench_matrix_rowsums,   1},
  {"c_call_bench_matrix_rowcol_means",(DL_FUNC)&c_call_bench_matrix_rowcol_means,1},
  {"c_call_bench_dataframe_filter", (DL_FUNC) &c_call_bench_dataframe_filter, 1},
  {"c_call_bench_list_access",      (DL_FUNC) &c_call_bench_list_access,      1},
  {"c_call_bench_string_concat",    (DL_FUNC) &c_call_bench_string_concat,    1},
  {"c_call_bench_string_nchar",     (DL_FUNC) &c_call_bench_string_nchar,     1},
  {"c_call_bench_string_encoding",  (DL_FUNC) &c_call_bench_string_encoding,  1},
  {"c_call_bench_factor_ops",       (DL_FUNC) &c_call_bench_factor_ops,       1},
  {"c_call_bench_attrib_ops",       (DL_FUNC) &c_call_bench_attrib_ops,       1},
  {"c_call_bench_s4_slot_access",   (DL_FUNC) &c_call_bench_s4_slot_access,   1},
  {"c_call_bench_na_propagation",   (DL_FUNC) &c_call_bench_na_propagation,   1},
  {"c_call_bench_long_vector_idx",  (DL_FUNC) &c_call_bench_long_vector_idx,  1},
  {"c_call_bench_l1_arithmetic",    (DL_FUNC) &c_call_bench_l1_arithmetic,    1},
  {"c_call_bench_matmul",           (DL_FUNC) &c_call_bench_matmul,           2},
  {"c_call_bench_crossprod",        (DL_FUNC) &c_call_bench_crossprod,        1},
  {"c_call_bench_cholesky",         (DL_FUNC) &c_call_bench_cholesky,         1},
  {"c_call_bench_lm_fit",           (DL_FUNC) &c_call_bench_lm_fit,           2},
  {"c_call_bench_altrep_create",    (DL_FUNC) &c_call_bench_altrep_create,    1},
  {"c_call_bench_altrep_materialize",(DL_FUNC)&c_call_bench_altrep_materialize,1},
  {"c_call_bench_altrep_elt_walk",  (DL_FUNC) &c_call_bench_altrep_elt_walk,  1},
  {"c_call_bench_altrep_region_read",(DL_FUNC)&c_call_bench_altrep_region_read,1},
  {"c_call_bench_altrep_sum_via_R", (DL_FUNC) &c_call_bench_altrep_sum_via_R, 1},
  {"c_call_bench_altrep_sum_native",(DL_FUNC) &c_call_bench_altrep_sum_native,1},
  {"c_call_bench_altrep_min_max",   (DL_FUNC) &c_call_bench_altrep_min_max,   1},
  {"c_call_bench_altrep_no_na_query",(DL_FUNC)&c_call_bench_altrep_no_na_query,1},
  {"c_call_bench_struct_convert",   (DL_FUNC) &c_call_bench_struct_convert,   1},
  {"c_call_bench_r_eval",           (DL_FUNC) &c_call_bench_r_eval,           1},
  {"c_call_bench_r_tryeval",        (DL_FUNC) &c_call_bench_r_tryeval,        1},
  {"c_call_bench_serialize_roundtrip",(DL_FUNC)&c_call_bench_serialize_roundtrip,1},
  {"c_call_bench_external_ptr",     (DL_FUNC) &c_call_bench_external_ptr,     1},
  {"c_call_bench_rng_stress",       (DL_FUNC) &c_call_bench_rng_stress,       1},
  {"c_fixture_scalar",               (DL_FUNC) &c_fixture_scalar,               1},
  {"c_fixture_vector",               (DL_FUNC) &c_fixture_vector,               1},
  {"c_fixture_new",                  (DL_FUNC) &c_fixture_new,                  0},
  {"c_fixture_method",               (DL_FUNC) &c_fixture_method,               2},
  {"c_fixture_error",                (DL_FUNC) &c_fixture_error,                1},
  {NULL, NULL, 0}
};

static const R_ExternalMethodDef ExternalEntries[] = {
  {"c_fixture_external", (DL_FUNC) &c_fixture_external, 1},
  {NULL, NULL, 0}
};

void R_init_bench(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, ExternalEntries);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
