#include <Rinternals.h>
#include <R_ext/Altrep.h>
#include <R_ext/Rdynload.h>
#include <stdint.h>
#include <stdlib.h>
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

typedef struct {
    int constructor;
    int method;
    int error;
    int finalizer;
} c_p4_lifecycle_counts;

typedef struct {
    int element;
    int region;
    int pointer;
    int materialization;
} c_p4_altrep_counts;

static c_p4_lifecycle_counts c_p4_lifecycle = {0, 0, 0, 0};
static c_p4_altrep_counts c_p4_altrep = {0, 0, 0, 0};
static R_altrep_class_t c_p4_altrep_integer_class;

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

static void c_fixture_state_finalizer(SEXP pointer) {
    int *state = (int *) R_ExternalPtrAddr(pointer);
    if (state == NULL) return;
    free(state);
    R_ClearExternalPtr(pointer);
}

static SEXP c_fixture_new(void) {
    SEXP result = PROTECT(R_MakeExternalPtr(NULL, fixture_tag(), R_NilValue));
    int *state = (int *) malloc(sizeof(int));
    if (state == NULL) {
        UNPROTECT(1);
        Rf_error("c fixture could not allocate state");
    }
    *state = 0;
    R_SetExternalPtrAddr(result, state);
    R_RegisterCFinalizerEx(result, c_fixture_state_finalizer, TRUE);
    UNPROTECT(1);
    return result;
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

typedef struct {
    int value;
} c_p4_fixture_state;

static SEXP c_p4_fixture_tag_symbol = NULL;

static SEXP c_p4_fixture_tag(void) {
    if (c_p4_fixture_tag_symbol == NULL) c_p4_fixture_tag_symbol = Rf_install("zigr.p4.c.fixture.state");
    return c_p4_fixture_tag_symbol;
}

static void c_p4_set_names(SEXP value, const char *const *names, R_xlen_t length) {
    SEXP name_vector = PROTECT(Rf_allocVector(STRSXP, length));
    for (R_xlen_t index = 0; index < length; ++index) {
        SET_STRING_ELT(name_vector, index, Rf_mkCharCE(names[index], CE_UTF8));
    }
    Rf_setAttrib(value, R_NamesSymbol, name_vector);
    UNPROTECT(1);
}

static SEXP c_p4_lifecycle_reset(void) {
    memset(&c_p4_lifecycle, 0, sizeof(c_p4_lifecycle));
    return R_NilValue;
}

static SEXP c_p4_lifecycle_snapshot(void) {
    static const char *const names[] = {"constructor", "method", "error", "finalizer"};
    SEXP result = PROTECT(Rf_allocVector(INTSXP, 4));
    INTEGER(result)[0] = c_p4_lifecycle.constructor;
    INTEGER(result)[1] = c_p4_lifecycle.method;
    INTEGER(result)[2] = c_p4_lifecycle.error;
    INTEGER(result)[3] = c_p4_lifecycle.finalizer;
    c_p4_set_names(result, names, 4);
    UNPROTECT(1);
    return result;
}

static SEXP c_p4_same_sexp(SEXP left, SEXP right) {
    return Rf_ScalarLogical(left == right);
}

static SEXP c_p4_same_data_pointer(SEXP left, SEXP right) {
    if (TYPEOF(left) != TYPEOF(right) || XLENGTH(left) != XLENGTH(right)) {
        return Rf_ScalarLogical(FALSE);
    }
    switch (TYPEOF(left)) {
        case REALSXP:
        case INTSXP:
        case LGLSXP:
        case RAWSXP:
        case CPLXSXP:
            return Rf_ScalarLogical(DATAPTR_RO(left) == DATAPTR_RO(right));
        default:
            Rf_error("data-pointer comparison requires matching atomic vectors");
    }
    return R_NilValue;
}

static SEXP c_p4_wrong_pointer(void) {
    return R_MakeExternalPtr(&fixture_state, Rf_install("p4_wrong_fixture_type"), R_NilValue);
}

static SEXP c_p4_cleared_pointer_like(SEXP value) {
    if (TYPEOF(value) != EXTPTRSXP) Rf_error("pointer template must be an external pointer");
    SEXP result = R_MakeExternalPtr(&fixture_state, R_ExternalPtrTag(value), R_NilValue);
    R_ClearExternalPtr(result);
    return result;
}

static R_xlen_t c_p4_altrep_length(SEXP value) {
    return (R_xlen_t) INTEGER(R_altrep_data1(value))[1];
}

static int c_p4_altrep_value(SEXP value, R_xlen_t index) {
    return INTEGER(R_altrep_data1(value))[0] + (int) index;
}

static int c_p4_altrep_elt(SEXP value, R_xlen_t index) {
    ++c_p4_altrep.element;
    SEXP materialized = R_altrep_data2(value);
    if (materialized != R_NilValue) return INTEGER(materialized)[index];
    return c_p4_altrep_value(value, index);
}

static R_xlen_t c_p4_altrep_get_region(SEXP value, R_xlen_t index, R_xlen_t length, int *buffer) {
    ++c_p4_altrep.region;
    const R_xlen_t total = c_p4_altrep_length(value);
    if (index >= total) return 0;
    const R_xlen_t available = total - index;
    const R_xlen_t count = available < length ? available : length;
    SEXP materialized = R_altrep_data2(value);
    for (R_xlen_t offset = 0; offset < count; ++offset) {
        buffer[offset] = materialized == R_NilValue
            ? c_p4_altrep_value(value, index + offset)
            : INTEGER(materialized)[index + offset];
    }
    return count;
}

static void *c_p4_altrep_dataptr(SEXP value, Rboolean writable) {
    (void) writable;
    ++c_p4_altrep.pointer;
    SEXP materialized = R_altrep_data2(value);
    if (materialized == R_NilValue) {
        ++c_p4_altrep.materialization;
        const R_xlen_t length = c_p4_altrep_length(value);
        materialized = PROTECT(Rf_allocVector(INTSXP, length));
        for (R_xlen_t index = 0; index < length; ++index) {
            INTEGER(materialized)[index] = c_p4_altrep_value(value, index);
        }
        R_set_altrep_data2(value, materialized);
        UNPROTECT(1);
    }
    return INTEGER(materialized);
}

static const void *c_p4_altrep_dataptr_or_null(SEXP value) {
    SEXP materialized = R_altrep_data2(value);
    return materialized == R_NilValue ? NULL : INTEGER(materialized);
}

static SEXP c_p4_altrep_new(SEXP start, SEXP length) {
    if (TYPEOF(start) != INTSXP || XLENGTH(start) != 1 || INTEGER(start)[0] == NA_INTEGER) {
        Rf_error("instrumented ALTREP start must be one non-missing integer");
    }
    if (TYPEOF(length) != INTSXP || XLENGTH(length) != 1 ||
        INTEGER(length)[0] == NA_INTEGER || INTEGER(length)[0] < 0) {
        Rf_error("instrumented ALTREP length must be one non-negative integer");
    }
    SEXP state = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(state)[0] = INTEGER(start)[0];
    INTEGER(state)[1] = INTEGER(length)[0];
    SEXP result = R_new_altrep(c_p4_altrep_integer_class, state, R_NilValue);
    UNPROTECT(1);
    return result;
}

static SEXP c_p4_altrep_reset(void) {
    memset(&c_p4_altrep, 0, sizeof(c_p4_altrep));
    return R_NilValue;
}

static SEXP c_p4_altrep_snapshot(SEXP value) {
    if (!ALTREP(value)) Rf_error("ALTREP snapshot requires an ALTREP object");
    SEXP result = PROTECT(Rf_allocVector(INTSXP, 5));
    INTEGER(result)[0] = c_p4_altrep.element;
    INTEGER(result)[1] = c_p4_altrep.region;
    INTEGER(result)[2] = c_p4_altrep.pointer;
    INTEGER(result)[3] = c_p4_altrep.materialization;
    INTEGER(result)[4] = R_altrep_data2(value) != R_NilValue;
    static const char *const snapshot_names[] = {
        "element", "region", "pointer", "materialization", "is_materialized"
    };
    c_p4_set_names(result, snapshot_names, 5);
    UNPROTECT(1);
    return result;
}

static SEXP c_p4_fixture_zero(void) {
    return Rf_ScalarInteger(1);
}

static SEXP c_p4_fixture_scalar(SEXP value) {
    return c_fixture_scalar(value);
}

static SEXP c_p4_fixture_numeric(SEXP value) {
    if (TYPEOF(value) != REALSXP) Rf_error("numeric fixture expected a double vector");
    const R_xlen_t length = XLENGTH(value);
    SEXP result = PROTECT(Rf_allocVector(REALSXP, length));
    const double *source = REAL(value);
    double *destination = REAL(result);
    for (R_xlen_t index = 0; index < length; ++index) destination[index] = source[index] * 2.0;
    UNPROTECT(1);
    return result;
}

static SEXP c_p4_fixture_altrep_integer(SEXP value) {
    if (TYPEOF(value) != INTSXP) Rf_error("ALTREP fixture expected an integer vector");
    const R_xlen_t length = XLENGTH(value);
    int buffer[4096];
    R_xlen_t offset = 0;
    double total = 0.0;
    while (offset < length) {
        const R_xlen_t requested = (length - offset) < 4096 ? length - offset : 4096;
        const R_xlen_t received = INTEGER_GET_REGION(value, offset, requested, buffer);
        if (received == 0) Rf_error("ALTREP fixture could not read an integer region");
        for (R_xlen_t index = 0; index < received; ++index) {
            if (buffer[index] == NA_INTEGER) return Rf_ScalarReal(NA_REAL);
            total += buffer[index];
        }
        offset += received;
    }
    return Rf_ScalarReal(total);
}

static SEXP c_p4_fixture_strings(SEXP value) {
    if (TYPEOF(value) != STRSXP) Rf_error("string fixture expected a character vector");
    int count = 0;
    for (R_xlen_t index = 0; index < XLENGTH(value); ++index) {
        if (STRING_ELT(value, index) != R_NaString) ++count;
    }
    return Rf_ScalarInteger(count);
}

static SEXP c_p4_fixture_raw(SEXP value) {
    if (TYPEOF(value) != RAWSXP) Rf_error("raw fixture expected a raw vector");
    const R_xlen_t length = XLENGTH(value);
    SEXP result = PROTECT(Rf_allocVector(RAWSXP, length));
    memcpy(RAW(result), RAW(value), (size_t) length);
    UNPROTECT(1);
    return result;
}

static SEXP c_p4_fixture_complex(SEXP value) {
    if (TYPEOF(value) != CPLXSXP) Rf_error("complex fixture expected a complex vector");
    const R_xlen_t length = XLENGTH(value);
    SEXP result = PROTECT(Rf_allocVector(CPLXSXP, length));
    memcpy(COMPLEX(result), COMPLEX(value), (size_t) length * sizeof(Rcomplex));
    UNPROTECT(1);
    return result;
}

static SEXP c_p4_fixture_logical_counts(SEXP value) {
    static const char *const names[] = {"false", "true", "missing"};
    if (TYPEOF(value) != LGLSXP) Rf_error("logical fixture expected a logical vector");
    int counts[3] = {0, 0, 0};
    for (R_xlen_t index = 0; index < XLENGTH(value); ++index) {
        const int element = LOGICAL_ELT(value, index);
        if (element == NA_LOGICAL) {
            ++counts[2];
        } else if (element) {
            ++counts[1];
        } else {
            ++counts[0];
        }
    }
    SEXP result = PROTECT(Rf_allocVector(INTSXP, 3));
    memcpy(INTEGER(result), counts, sizeof(counts));
    c_p4_set_names(result, names, 3);
    UNPROTECT(1);
    return result;
}

static SEXP c_p4_fixture_schema(SEXP value) {
    return c_boundary_schema(value);
}

static void c_p4_fixture_state_finalizer(SEXP pointer) {
    c_p4_fixture_state *state = (c_p4_fixture_state *) R_ExternalPtrAddr(pointer);
    if (state == NULL) return;
    R_ClearExternalPtr(pointer);
    free(state);
    ++c_p4_lifecycle.finalizer;
}

static SEXP c_p4_fixture_new(void) {
    SEXP result = PROTECT(R_MakeExternalPtr(NULL, c_p4_fixture_tag(), R_NilValue));
    c_p4_fixture_state *state = (c_p4_fixture_state *) malloc(sizeof(c_p4_fixture_state));
    if (state == NULL) Rf_error("native state allocation failed");
    state->value = 0;
    ++c_p4_lifecycle.constructor;
    R_SetExternalPtrAddr(result, state);
    R_RegisterCFinalizerEx(result, c_p4_fixture_state_finalizer, TRUE);
    UNPROTECT(1);
    return result;
}

static c_p4_fixture_state *c_p4_fixture_state_pointer(SEXP receiver) {
    if (TYPEOF(receiver) != EXTPTRSXP || R_ExternalPtrAddr(receiver) == NULL) {
        Rf_error("fixture state expected a live external pointer");
    }
    if (R_ExternalPtrTag(receiver) != c_p4_fixture_tag()) {
        Rf_error("fixture state external pointer has the wrong tag");
    }
    return (c_p4_fixture_state *) R_ExternalPtrAddr(receiver);
}

static SEXP c_p4_fixture_method(SEXP receiver, SEXP amount) {
    if (TYPEOF(amount) != INTSXP || XLENGTH(amount) != 1 || INTEGER(amount)[0] == NA_INTEGER) {
        Rf_error("fixture method expected one non-missing integer");
    }
    c_p4_fixture_state *state = c_p4_fixture_state_pointer(receiver);
    ++c_p4_lifecycle.method;
    state->value += INTEGER(amount)[0];
    return Rf_ScalarInteger(state->value);
}

static SEXP c_p4_fixture_read(SEXP receiver) {
    c_p4_fixture_state *state = c_p4_fixture_state_pointer(receiver);
    ++c_p4_lifecycle.method;
    return Rf_ScalarInteger(state->value);
}

static SEXP c_p4_fixture_error(SEXP trigger) {
    if (TYPEOF(trigger) != REALSXP || XLENGTH(trigger) != 1) {
        Rf_error("error trigger expected one REAL value");
    }
    ++c_p4_lifecycle.error;
    Rf_error("fixture error");
    return R_NilValue;
}

static SEXP c_p4_fixture_outputs(void) {
    static const char *const output_names[] = {"numeric", "string", "raw", "complex", "logical", "list"};
    static const char *const nested_names[] = {"value"};
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 6));
    SEXP numeric = PROTECT(Rf_allocVector(REALSXP, 2));
    SEXP string = PROTECT(Rf_allocVector(STRSXP, 1));
    SEXP raw = PROTECT(Rf_allocVector(RAWSXP, 3));
    SEXP complex = PROTECT(Rf_allocVector(CPLXSXP, 2));
    SEXP logical = PROTECT(Rf_allocVector(LGLSXP, 3));
    SEXP nested = PROTECT(Rf_allocVector(VECSXP, 1));

    REAL(numeric)[0] = 1.5;
    REAL(numeric)[1] = NA_REAL;
    SET_STRING_ELT(string, 0, Rf_mkCharCE("fixture", CE_UTF8));
    RAW(raw)[0] = 1;
    RAW(raw)[1] = 2;
    RAW(raw)[2] = 3;
    COMPLEX(complex)[0].r = 1.0;
    COMPLEX(complex)[0].i = 2.0;
    COMPLEX(complex)[1].r = NA_REAL;
    COMPLEX(complex)[1].i = NA_REAL;
    LOGICAL(logical)[0] = FALSE;
    LOGICAL(logical)[1] = TRUE;
    LOGICAL(logical)[2] = NA_LOGICAL;
    SET_VECTOR_ELT(nested, 0, Rf_ScalarInteger(7));
    c_p4_set_names(nested, nested_names, 1);

    SET_VECTOR_ELT(result, 0, numeric);
    SET_VECTOR_ELT(result, 1, string);
    SET_VECTOR_ELT(result, 2, raw);
    SET_VECTOR_ELT(result, 3, complex);
    SET_VECTOR_ELT(result, 4, logical);
    SET_VECTOR_ELT(result, 5, nested);
    c_p4_set_names(result, output_names, 6);
    UNPROTECT(7);
    return result;
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
  {"c_p4_fixture_zero",               (DL_FUNC) &c_p4_fixture_zero,               0},
  {"c_p4_fixture_scalar",             (DL_FUNC) &c_p4_fixture_scalar,             1},
  {"c_p4_fixture_numeric",            (DL_FUNC) &c_p4_fixture_numeric,            1},
  {"c_p4_fixture_altrep_integer",     (DL_FUNC) &c_p4_fixture_altrep_integer,     1},
  {"c_p4_fixture_strings",            (DL_FUNC) &c_p4_fixture_strings,            1},
  {"c_p4_fixture_raw",                (DL_FUNC) &c_p4_fixture_raw,                1},
  {"c_p4_fixture_complex",            (DL_FUNC) &c_p4_fixture_complex,            1},
  {"c_p4_fixture_logical_counts",     (DL_FUNC) &c_p4_fixture_logical_counts,     1},
  {"c_p4_fixture_schema",             (DL_FUNC) &c_p4_fixture_schema,             1},
  {"c_p4_fixture_new",                (DL_FUNC) &c_p4_fixture_new,                0},
  {"c_p4_fixture_method",             (DL_FUNC) &c_p4_fixture_method,             2},
  {"c_p4_fixture_read",               (DL_FUNC) &c_p4_fixture_read,               1},
  {"c_p4_fixture_error",              (DL_FUNC) &c_p4_fixture_error,              1},
  {"c_p4_fixture_outputs",            (DL_FUNC) &c_p4_fixture_outputs,            0},
  {"c_p4_lifecycle_reset",            (DL_FUNC) &c_p4_lifecycle_reset,            0},
  {"c_p4_lifecycle_snapshot",         (DL_FUNC) &c_p4_lifecycle_snapshot,         0},
  {"c_p4_same_sexp",                  (DL_FUNC) &c_p4_same_sexp,                  2},
  {"c_p4_same_data_pointer",          (DL_FUNC) &c_p4_same_data_pointer,          2},
  {"c_p4_wrong_pointer",              (DL_FUNC) &c_p4_wrong_pointer,              0},
  {"c_p4_cleared_pointer_like",       (DL_FUNC) &c_p4_cleared_pointer_like,       1},
  {"c_p4_altrep_new",                 (DL_FUNC) &c_p4_altrep_new,                 2},
  {"c_p4_altrep_reset",               (DL_FUNC) &c_p4_altrep_reset,               0},
  {"c_p4_altrep_snapshot",            (DL_FUNC) &c_p4_altrep_snapshot,            1},
  {NULL, NULL, 0}
};

static const R_ExternalMethodDef ExternalEntries[] = {
  {"c_fixture_external", (DL_FUNC) &c_fixture_external, 1},
  {NULL, NULL, 0}
};

void R_init_bench(DllInfo *dll) {
    c_p4_altrep_integer_class = R_make_altinteger_class(
        "p4_instrumented_integer", "zigrBenchmarks", dll
    );
    R_set_altrep_Length_method(c_p4_altrep_integer_class, c_p4_altrep_length);
    R_set_altvec_Dataptr_method(c_p4_altrep_integer_class, c_p4_altrep_dataptr);
    R_set_altvec_Dataptr_or_null_method(c_p4_altrep_integer_class, c_p4_altrep_dataptr_or_null);
    R_set_altinteger_Elt_method(c_p4_altrep_integer_class, c_p4_altrep_elt);
    R_set_altinteger_Get_region_method(c_p4_altrep_integer_class, c_p4_altrep_get_region);
    R_registerRoutines(dll, NULL, CallEntries, NULL, ExternalEntries);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
