#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdint.h>
#include <string.h>

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
static SEXP fixture_tag_symbol = NULL;

static SEXP fixture_tag(void) {
    if (fixture_tag_symbol == NULL) fixture_tag_symbol = Rf_install("zigr_fixture_state");
    return fixture_tag_symbol;
}

static SEXP c_fixture_scalar(SEXP value) {
    if (TYPEOF(value) != REALSXP || XLENGTH(value) != 1) {
        Rf_error("c fixture scalar expected one REAL value");
    }
    if (ISNA(REAL(value)[0])) {
        Rf_error("c fixture scalar expected a non-missing REAL value");
    }
    return Rf_ScalarReal(REAL(value)[0]);
}

static SEXP c_fixture_int_scalar(SEXP value) {
    if (TYPEOF(value) != INTSXP || XLENGTH(value) != 1) {
        Rf_error("c fixture scalar expected one INTEGER value");
    }
    if (INTEGER(value)[0] == NA_INTEGER) {
        Rf_error("c fixture scalar expected a non-missing INTEGER value");
    }
    return Rf_ScalarInteger(INTEGER(value)[0]);
}

static SEXP c_fixture_bool_scalar(SEXP value) {
    if (TYPEOF(value) != LGLSXP || XLENGTH(value) != 1) {
        Rf_error("c fixture scalar expected one LOGICAL value");
    }
    if (LOGICAL(value)[0] == NA_LOGICAL) {
        Rf_error("c fixture scalar expected a non-missing LOGICAL value");
    }
    return Rf_ScalarLogical(LOGICAL(value)[0] != 0);
}

static SEXP c_fixture_scalar_after_allocation(SEXP value) {
    double scalar;
    if (TYPEOF(value) != REALSXP || XLENGTH(value) != 1) {
        Rf_error("c fixture scalar expected one REAL value");
    }
    scalar = REAL(value)[0];
    if (ISNA(scalar)) {
        Rf_error("c fixture scalar expected a non-missing REAL value");
    }
    (void) Rf_allocVector(INTSXP, 1);
    return Rf_ScalarReal(scalar);
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
    return R_MakeExternalPtr(&fixture_state, fixture_tag(), R_NilValue);
}

static SEXP c_fixture_method(SEXP receiver, SEXP amount) {
    if (TYPEOF(receiver) != EXTPTRSXP || R_ExternalPtrAddr(receiver) == NULL) {
        Rf_error("c fixture method expected a live external pointer");
    }
    if (R_ExternalPtrTag(receiver) != fixture_tag()) {
        Rf_error("c fixture method received an external pointer with the wrong tag");
    }
    if ((uintptr_t) R_ExternalPtrAddr(receiver) % _Alignof(int) != 0) {
        Rf_error("c fixture method received a misaligned external pointer");
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

static SEXP c_fixture_wrong_tag(void) {
    return R_MakeExternalPtr(&fixture_state, Rf_install("zigr_fixture_wrong_tag"), R_NilValue);
}

static SEXP c_fixture_cleared(void) {
    SEXP result = R_MakeExternalPtr(&fixture_state, fixture_tag(), R_NilValue);
    R_ClearExternalPtr(result);
    return result;
}

static SEXP c_fixture_misaligned(void) {
    void *address = (void *) ((unsigned char *) &fixture_state + 1);
    return R_MakeExternalPtr(address, fixture_tag(), R_NilValue);
}

static SEXP c_boundary_zero(void) {
    return Rf_ScalarReal(1.0);
}

static SEXP c_boundary_scalar(SEXP value) {
    return c_fixture_scalar(value);
}

static SEXP c_boundary_optional(SEXP value) {
    if (value == R_NilValue) return Rf_ScalarInteger(0);
    if (TYPEOF(value) != REALSXP || XLENGTH(value) != 1) {
        Rf_error("c fixture expected NULL or one REAL value");
    }
    if (ISNA(REAL(value)[0])) return Rf_ScalarInteger(0);
    return Rf_ScalarInteger(1);
}

static SEXP c_boundary_optional_int(SEXP value) {
    if (value == R_NilValue) return Rf_ScalarInteger(0);
    if (TYPEOF(value) != INTSXP || XLENGTH(value) != 1) {
        Rf_error("c fixture expected NULL or one INTEGER value");
    }
    if (INTEGER(value)[0] == NA_INTEGER) return Rf_ScalarInteger(0);
    return Rf_ScalarInteger(1);
}

static SEXP c_boundary_optional_bool(SEXP value) {
    if (value == R_NilValue) return Rf_ScalarInteger(0);
    if (TYPEOF(value) != LGLSXP || XLENGTH(value) != 1) {
        Rf_error("c fixture expected NULL or one LOGICAL value");
    }
    if (LOGICAL(value)[0] == NA_LOGICAL) return Rf_ScalarInteger(0);
    return Rf_ScalarInteger(1);
}

static SEXP c_boundary_numeric(SEXP value) {
    if (TYPEOF(value) != REALSXP) Rf_error("c fixture expected REALSXP");
    double total = 0.0;
    const double *values = REAL(value);
    for (R_xlen_t i = 0; i < XLENGTH(value); ++i) total += values[i];
    return Rf_ScalarReal(total);
}

static SEXP c_boundary_altrep_integer(SEXP value) {
    if (TYPEOF(value) != INTSXP) Rf_error("c fixture expected an integer ALTREP");
    double total = 0.0;
    const R_xlen_t length = XLENGTH(value);
    const int *values = (const int *) DATAPTR_OR_NULL(value);
    if (values != NULL) {
        for (R_xlen_t i = 0; i < length; ++i) total += values[i];
    } else {
        int buffer[4096];
        R_xlen_t offset = 0;
        while (offset < length) {
            const R_xlen_t want = (length - offset) < 4096 ? (length - offset) : 4096;
            const R_xlen_t got = INTEGER_GET_REGION(value, offset, want, buffer);
            if (got == 0) Rf_error("c fixture could not read an integer ALTREP region");
            for (R_xlen_t i = 0; i < got; ++i) total += buffer[i];
            offset += got;
        }
    }
    return Rf_ScalarReal(total);
}

static SEXP c_boundary_string_view(SEXP value) {
    if (TYPEOF(value) != STRSXP) Rf_error("c fixture expected STRSXP");
    int total = 0;
    for (R_xlen_t i = 0; i < XLENGTH(value); ++i) {
        if (STRING_ELT(value, i) != R_NaString) ++total;
    }
    return Rf_ScalarInteger(total);
}

static SEXP c_boundary_raw(SEXP value) {
    if (TYPEOF(value) != RAWSXP) Rf_error("c fixture expected RAWSXP");
    int total = 0;
    for (R_xlen_t i = 0; i < XLENGTH(value); ++i) total += RAW(value)[i];
    return Rf_ScalarInteger(total);
}

static SEXP c_boundary_complex(SEXP value) {
    if (TYPEOF(value) != CPLXSXP) Rf_error("c fixture expected CPLXSXP");
    double total = 0.0;
    for (R_xlen_t i = 0; i < XLENGTH(value); ++i) total += COMPLEX(value)[i].r;
    return Rf_ScalarReal(total);
}

static int c_boundary_schema_is_valid(SEXP value) {
    static const char *const fields[] = {"id", "count", "ratio", "enabled"};
    if (TYPEOF(value) != VECSXP || XLENGTH(value) != 4) return 0;
    if (R_getAttribCount(value) != 1 || !R_hasAttrib(value, R_NamesSymbol)) return 0;
    SEXP names = Rf_getAttrib(value, R_NamesSymbol);
    if (TYPEOF(names) != STRSXP || XLENGTH(names) != 4 || R_getAttribCount(names) != 0) return 0;
    for (R_xlen_t i = 0; i < 4; ++i) {
        SEXP name = STRING_ELT(names, i);
        if (name == R_NaString || strcmp(CHAR(name), fields[i]) != 0) return 0;
    }
    SEXP id = VECTOR_ELT(value, 0);
    SEXP count = VECTOR_ELT(value, 1);
    SEXP ratio = VECTOR_ELT(value, 2);
    SEXP enabled = VECTOR_ELT(value, 3);
    if (TYPEOF(id) != INTSXP || XLENGTH(id) != 1 || INTEGER(id)[0] == NA_INTEGER) return 0;
    if (TYPEOF(count) != INTSXP || XLENGTH(count) != 1 || INTEGER(count)[0] == NA_INTEGER) return 0;
    if (TYPEOF(ratio) != REALSXP || XLENGTH(ratio) != 1 || ISNA(REAL(ratio)[0])) return 0;
    if (TYPEOF(enabled) != LGLSXP || XLENGTH(enabled) != 1 || LOGICAL(enabled)[0] == NA_LOGICAL) return 0;
    return 1;
}

static SEXP c_boundary_schema(SEXP value) {
    if (!c_boundary_schema_is_valid(value)) Rf_error("c fixture expected the fixed named-list shape");
    return value;
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
  {"c_fixture_int_scalar",           (DL_FUNC) &c_fixture_int_scalar,           1},
  {"c_fixture_bool_scalar",          (DL_FUNC) &c_fixture_bool_scalar,          1},
  {"c_fixture_scalar_after_allocation", (DL_FUNC) &c_fixture_scalar_after_allocation, 1},
  {"c_fixture_vector",               (DL_FUNC) &c_fixture_vector,               1},
  {"c_fixture_new",                  (DL_FUNC) &c_fixture_new,                  0},
  {"c_fixture_method",               (DL_FUNC) &c_fixture_method,               2},
  {"c_fixture_error",                (DL_FUNC) &c_fixture_error,                1},
  {"c_fixture_wrong_tag",             (DL_FUNC) &c_fixture_wrong_tag,             0},
  {"c_fixture_cleared",               (DL_FUNC) &c_fixture_cleared,               0},
  {"c_fixture_misaligned",            (DL_FUNC) &c_fixture_misaligned,            0},
  {"c_boundary_zero",                 (DL_FUNC) &c_boundary_zero,                 0},
  {"c_boundary_scalar",               (DL_FUNC) &c_boundary_scalar,               1},
  {"c_boundary_optional",             (DL_FUNC) &c_boundary_optional,             1},
  {"c_boundary_optional_int",         (DL_FUNC) &c_boundary_optional_int,         1},
  {"c_boundary_optional_bool",        (DL_FUNC) &c_boundary_optional_bool,        1},
  {"c_boundary_numeric",              (DL_FUNC) &c_boundary_numeric,              1},
  {"c_boundary_altrep_integer",       (DL_FUNC) &c_boundary_altrep_integer,       1},
  {"c_boundary_string_view",          (DL_FUNC) &c_boundary_string_view,          1},
  {"c_boundary_raw",                  (DL_FUNC) &c_boundary_raw,                  1},
  {"c_boundary_complex",              (DL_FUNC) &c_boundary_complex,              1},
  {"c_boundary_schema",               (DL_FUNC) &c_boundary_schema,               1},
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
