#include <Rinternals.h>
#include <R_ext/Altrep.h>
#include <R_ext/Rdynload.h>
#include <float.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern SEXP c_call_bench_sort(SEXP);
extern SEXP c_call_bench_matrix_transpose(SEXP);
extern SEXP c_call_bench_s4_slot_access(SEXP);
extern SEXP c_call_bench_matmul(SEXP, SEXP);
extern SEXP c_call_bench_rng_stress(SEXP);

static int fixture_state = 0;
static SEXP fixture_tag_symbol = NULL;

typedef struct {
    int constructor;
    int method;
    int error;
    int finalizer;
} c_benchmark_lifecycle_counts;

typedef struct {
    int element;
    int region;
    int pointer;
    int materialization;
} c_benchmark_altrep_counts;

static c_benchmark_lifecycle_counts c_benchmark_lifecycle = {0, 0, 0, 0};
static c_benchmark_altrep_counts c_benchmark_altrep = {0, 0, 0, 0};
static R_altrep_class_t c_benchmark_altrep_integer_class;
static volatile double c_measurement_probe_sink = 0.0;

static double c_exact_real_sum(const double *values, R_xlen_t length) {
    long double total = 0.0;
    int na_seen = 0, nan_seen = 0;
    for (R_xlen_t index = 0; index < length; ++index) {
        double value = values[index];
        if (ISNAN(value)) {
            if (ISNA(value)) na_seen = 1;
            else nan_seen = 1;
        } else {
            total += (long double) value;
        }
    }
    if (na_seen) return NA_REAL;
    if (nan_seen) return R_NaN;
    if (total > DBL_MAX) return R_PosInf;
    if (total < -DBL_MAX) return R_NegInf;
    return (double) total;
}

static double c_exact_real_mean_narm(const double *values, R_xlen_t length) {
    long double total = 0.0;
    R_xlen_t count = 0;
    for (R_xlen_t index = 0; index < length; ++index) {
        double value = values[index];
        if (!ISNAN(value)) {
            total += (long double) value;
            ++count;
        }
    }
    if (count == 0) return R_NaN;

    long double divisor = (long double) count;
    int finite_total = R_FINITE((double) total);
    long double result;
    if (finite_total) {
        result = total / divisor;
    } else {
        long double scaled_total = 0.0;
        double scaled_divisor = (double) count;
        for (R_xlen_t index = 0; index < length; ++index) {
            double value = values[index];
            if (!ISNAN(value)) scaled_total += (long double) (value / scaled_divisor);
        }
        result = scaled_total;
    }
    if (R_FINITE((double) result)) {
        long double correction = 0.0;
        for (R_xlen_t index = 0; index < length; ++index) {
            double value = values[index];
            if (ISNAN(value)) continue;
            correction += finite_total
                ? (long double) value - result
                : ((long double) value - result) / divisor;
        }
        result += finite_total ? correction / divisor : correction;
    }
    return (double) result;
}

static SEXP c_measurement_probe_noop(void) {
    return R_NilValue;
}

static SEXP c_measurement_probe_cpu(SEXP value) {
    R_xlen_t length;
    const double *values;
    double total = 0.0;
    if (TYPEOF(value) != REALSXP) Rf_error("CPU probe expected a numeric vector");
    if (ALTREP(value)) Rf_error("CPU probe expected an ordinary numeric vector");
    length = XLENGTH(value);
    values = REAL(value);
    for (R_xlen_t index = 0; index < length; ++index) total += values[index];
    c_measurement_probe_sink = total;
    return value;
}

static SEXP c_measurement_probe_allocate(SEXP size) {
    int length;
    SEXP result;
    if (TYPEOF(size) != INTSXP || XLENGTH(size) != 1 || INTEGER(size)[0] < 1) {
        Rf_error("allocation probe expected one positive integer");
    }
    length = INTEGER(size)[0];
    result = PROTECT(Rf_allocVector(REALSXP, length));
    for (int index = 0; index < length; ++index) REAL(result)[index] = (double) index;
    UNPROTECT(1);
    return result;
}

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
} c_benchmark_fixture_state;

static SEXP c_benchmark_fixture_tag_symbol = NULL;

static SEXP c_benchmark_fixture_tag(void) {
    if (c_benchmark_fixture_tag_symbol == NULL) c_benchmark_fixture_tag_symbol = Rf_install("zigr.benchmark.c.fixture.state");
    return c_benchmark_fixture_tag_symbol;
}

static void c_benchmark_set_names(SEXP value, const char *const *names, R_xlen_t length) {
    SEXP name_vector = PROTECT(Rf_allocVector(STRSXP, length));
    for (R_xlen_t index = 0; index < length; ++index) {
        SET_STRING_ELT(name_vector, index, Rf_mkCharCE(names[index], CE_UTF8));
    }
    Rf_setAttrib(value, R_NamesSymbol, name_vector);
    UNPROTECT(1);
}

static SEXP c_benchmark_lifecycle_reset(void) {
    memset(&c_benchmark_lifecycle, 0, sizeof(c_benchmark_lifecycle));
    return R_NilValue;
}

static SEXP c_benchmark_lifecycle_snapshot(void) {
    static const char *const names[] = {"constructor", "method", "error", "finalizer"};
    SEXP result = PROTECT(Rf_allocVector(INTSXP, 4));
    INTEGER(result)[0] = c_benchmark_lifecycle.constructor;
    INTEGER(result)[1] = c_benchmark_lifecycle.method;
    INTEGER(result)[2] = c_benchmark_lifecycle.error;
    INTEGER(result)[3] = c_benchmark_lifecycle.finalizer;
    c_benchmark_set_names(result, names, 4);
    UNPROTECT(1);
    return result;
}

static SEXP c_benchmark_same_sexp(SEXP left, SEXP right) {
    return Rf_ScalarLogical(left == right);
}

static SEXP c_benchmark_same_data_pointer(SEXP left, SEXP right) {
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

static SEXP c_benchmark_wrong_pointer(void) {
    return R_MakeExternalPtr(&fixture_state, Rf_install("benchmark_wrong_fixture_type"), R_NilValue);
}

static SEXP c_benchmark_cleared_pointer_like(SEXP value) {
    if (TYPEOF(value) != EXTPTRSXP) Rf_error("pointer template must be an external pointer");
    SEXP result = R_MakeExternalPtr(&fixture_state, R_ExternalPtrTag(value), R_NilValue);
    R_ClearExternalPtr(result);
    return result;
}

static R_xlen_t c_benchmark_altrep_length(SEXP value) {
    return (R_xlen_t) INTEGER(R_altrep_data1(value))[1];
}

static int c_benchmark_altrep_value(SEXP value, R_xlen_t index) {
    return INTEGER(R_altrep_data1(value))[0] + (int) index;
}

static int c_benchmark_altrep_elt(SEXP value, R_xlen_t index) {
    ++c_benchmark_altrep.element;
    SEXP materialized = R_altrep_data2(value);
    if (materialized != R_NilValue) return INTEGER(materialized)[index];
    return c_benchmark_altrep_value(value, index);
}

static R_xlen_t c_benchmark_altrep_get_region(SEXP value, R_xlen_t index, R_xlen_t length, int *buffer) {
    ++c_benchmark_altrep.region;
    const R_xlen_t total = c_benchmark_altrep_length(value);
    if (index >= total) return 0;
    const R_xlen_t available = total - index;
    const R_xlen_t count = available < length ? available : length;
    SEXP materialized = R_altrep_data2(value);
    for (R_xlen_t offset = 0; offset < count; ++offset) {
        buffer[offset] = materialized == R_NilValue
            ? c_benchmark_altrep_value(value, index + offset)
            : INTEGER(materialized)[index + offset];
    }
    return count;
}

static void *c_benchmark_altrep_dataptr(SEXP value, Rboolean writable) {
    (void) writable;
    ++c_benchmark_altrep.pointer;
    SEXP materialized = R_altrep_data2(value);
    if (materialized == R_NilValue) {
        ++c_benchmark_altrep.materialization;
        const R_xlen_t length = c_benchmark_altrep_length(value);
        materialized = PROTECT(Rf_allocVector(INTSXP, length));
        for (R_xlen_t index = 0; index < length; ++index) {
            INTEGER(materialized)[index] = c_benchmark_altrep_value(value, index);
        }
        R_set_altrep_data2(value, materialized);
        UNPROTECT(1);
    }
    return INTEGER(materialized);
}

static const void *c_benchmark_altrep_dataptr_or_null(SEXP value) {
    SEXP materialized = R_altrep_data2(value);
    return materialized == R_NilValue ? NULL : INTEGER(materialized);
}

static SEXP c_benchmark_altrep_new(SEXP start, SEXP length) {
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
    SEXP result = R_new_altrep(c_benchmark_altrep_integer_class, state, R_NilValue);
    UNPROTECT(1);
    return result;
}

static SEXP c_benchmark_altrep_reset(void) {
    memset(&c_benchmark_altrep, 0, sizeof(c_benchmark_altrep));
    return R_NilValue;
}

static SEXP c_benchmark_altrep_snapshot(SEXP value) {
    if (!ALTREP(value)) Rf_error("ALTREP snapshot requires an ALTREP object");
    SEXP result = PROTECT(Rf_allocVector(INTSXP, 5));
    INTEGER(result)[0] = c_benchmark_altrep.element;
    INTEGER(result)[1] = c_benchmark_altrep.region;
    INTEGER(result)[2] = c_benchmark_altrep.pointer;
    INTEGER(result)[3] = c_benchmark_altrep.materialization;
    INTEGER(result)[4] = R_altrep_data2(value) != R_NilValue;
    static const char *const snapshot_names[] = {
        "element", "region", "pointer", "materialization", "is_materialized"
    };
    c_benchmark_set_names(result, snapshot_names, 5);
    UNPROTECT(1);
    return result;
}

static SEXP c_benchmark_fixture_zero(void) {
    return Rf_ScalarInteger(1);
}

static SEXP c_benchmark_fixture_scalar(SEXP value) {
    return c_fixture_scalar(value);
}

static SEXP c_benchmark_fixture_numeric(SEXP value) {
    if (TYPEOF(value) != REALSXP) Rf_error("numeric fixture expected a double vector");
    const R_xlen_t length = XLENGTH(value);
    SEXP result = PROTECT(Rf_allocVector(REALSXP, length));
    const double *source = REAL(value);
    double *destination = REAL(result);
    for (R_xlen_t index = 0; index < length; ++index) destination[index] = source[index] * 2.0;
    UNPROTECT(1);
    return result;
}

static SEXP c_benchmark_fixture_altrep_integer(SEXP value) {
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

static SEXP c_benchmark_fixture_strings(SEXP value) {
    if (TYPEOF(value) != STRSXP) Rf_error("string fixture expected a character vector");
    int count = 0;
    for (R_xlen_t index = 0; index < XLENGTH(value); ++index) {
        if (STRING_ELT(value, index) != R_NaString) ++count;
    }
    return Rf_ScalarInteger(count);
}

static SEXP c_benchmark_fixture_raw(SEXP value) {
    if (TYPEOF(value) != RAWSXP) Rf_error("raw fixture expected a raw vector");
    const R_xlen_t length = XLENGTH(value);
    SEXP result = PROTECT(Rf_allocVector(RAWSXP, length));
    memcpy(RAW(result), RAW(value), (size_t) length);
    UNPROTECT(1);
    return result;
}

static SEXP c_benchmark_fixture_complex(SEXP value) {
    if (TYPEOF(value) != CPLXSXP) Rf_error("complex fixture expected a complex vector");
    const R_xlen_t length = XLENGTH(value);
    SEXP result = PROTECT(Rf_allocVector(CPLXSXP, length));
    memcpy(COMPLEX(result), COMPLEX(value), (size_t) length * sizeof(Rcomplex));
    UNPROTECT(1);
    return result;
}

static SEXP c_benchmark_fixture_logical_counts(SEXP value) {
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
    c_benchmark_set_names(result, names, 3);
    UNPROTECT(1);
    return result;
}

static SEXP c_benchmark_fixture_schema(SEXP value) {
    return c_boundary_schema(value);
}

static void c_benchmark_fixture_state_finalizer(SEXP pointer) {
    c_benchmark_fixture_state *state = (c_benchmark_fixture_state *) R_ExternalPtrAddr(pointer);
    if (state == NULL) return;
    R_ClearExternalPtr(pointer);
    free(state);
    ++c_benchmark_lifecycle.finalizer;
}

static SEXP c_benchmark_fixture_new(void) {
    SEXP result = PROTECT(R_MakeExternalPtr(NULL, c_benchmark_fixture_tag(), R_NilValue));
    c_benchmark_fixture_state *state = (c_benchmark_fixture_state *) malloc(sizeof(c_benchmark_fixture_state));
    if (state == NULL) Rf_error("native state allocation failed");
    state->value = 0;
    ++c_benchmark_lifecycle.constructor;
    R_SetExternalPtrAddr(result, state);
    R_RegisterCFinalizerEx(result, c_benchmark_fixture_state_finalizer, TRUE);
    UNPROTECT(1);
    return result;
}

static c_benchmark_fixture_state *c_benchmark_fixture_state_pointer(SEXP receiver) {
    if (TYPEOF(receiver) != EXTPTRSXP || R_ExternalPtrAddr(receiver) == NULL) {
        Rf_error("fixture state expected a live external pointer");
    }
    if (R_ExternalPtrTag(receiver) != c_benchmark_fixture_tag()) {
        Rf_error("fixture state external pointer has the wrong tag");
    }
    return (c_benchmark_fixture_state *) R_ExternalPtrAddr(receiver);
}

static SEXP c_benchmark_fixture_method(SEXP receiver, SEXP amount) {
    if (TYPEOF(amount) != INTSXP || XLENGTH(amount) != 1 || INTEGER(amount)[0] == NA_INTEGER) {
        Rf_error("fixture method expected one non-missing integer");
    }
    c_benchmark_fixture_state *state = c_benchmark_fixture_state_pointer(receiver);
    ++c_benchmark_lifecycle.method;
    state->value += INTEGER(amount)[0];
    return Rf_ScalarInteger(state->value);
}

static SEXP c_benchmark_fixture_read(SEXP receiver) {
    c_benchmark_fixture_state *state = c_benchmark_fixture_state_pointer(receiver);
    ++c_benchmark_lifecycle.method;
    return Rf_ScalarInteger(state->value);
}

static SEXP c_benchmark_fixture_error(SEXP trigger) {
    if (TYPEOF(trigger) != REALSXP || XLENGTH(trigger) != 1) {
        Rf_error("error trigger expected one REAL value");
    }
    ++c_benchmark_lifecycle.error;
    Rf_error("fixture error");
    return R_NilValue;
}

static SEXP c_benchmark_fixture_outputs(void) {
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
    c_benchmark_set_names(nested, nested_names, 1);

    SET_VECTOR_ELT(result, 0, numeric);
    SET_VECTOR_ELT(result, 1, string);
    SET_VECTOR_ELT(result, 2, raw);
    SET_VECTOR_ELT(result, 3, complex);
    SET_VECTOR_ELT(result, 4, logical);
    SET_VECTOR_ELT(result, 5, nested);
    c_benchmark_set_names(result, output_names, 6);
    UNPROTECT(7);
    return result;
}

static SEXP c_revision_vector_sum(SEXP x) {
    return Rf_ScalarReal(c_exact_real_sum(REAL(x), XLENGTH(x)));
}

static SEXP c_revision_numeric_transform(SEXP x) { return c_benchmark_fixture_numeric(x); }

static SEXP c_revision_broadcast(SEXP x, SEXP scalar) {
    R_xlen_t n = XLENGTH(x);
    long double total = 0.0;
    double value = Rf_asReal(scalar);
    for (R_xlen_t i = 0; i < n; ++i) total += REAL(x)[i] + value;
    return Rf_ScalarReal((double) total);
}

static SEXP c_revision_sort(SEXP x) { return c_call_bench_sort(x); }

static SEXP c_revision_missing_mean(SEXP x) {
    return Rf_ScalarReal(c_exact_real_mean_narm(REAL(x), XLENGTH(x)));
}

static SEXP c_revision_transpose(SEXP x) { return c_call_bench_matrix_transpose(x); }

static SEXP c_revision_rowcol(SEXP x) {
    int rows = Rf_nrows(x), columns = Rf_ncols(x);
    SEXP row_means = PROTECT(Rf_allocVector(REALSXP, rows));
    SEXP column_sums = PROTECT(Rf_allocVector(REALSXP, columns));
    for (int row = 0; row < rows; ++row) REAL(row_means)[row] = 0.0;
    for (int column = 0; column < columns; ++column) {
        double total = 0.0;
        for (int row = 0; row < rows; ++row) {
            double value = REAL(x)[row + column * rows];
            REAL(row_means)[row] += value;
            total += value;
        }
        REAL(column_sums)[column] = total;
    }
    for (int row = 0; row < rows; ++row) REAL(row_means)[row] /= columns;
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(result, 0, row_means);
    SET_VECTOR_ELT(result, 1, column_sums);
    const char *names[] = {"row_means", "column_sums"};
    c_benchmark_set_names(result, names, 2);
    UNPROTECT(3);
    return result;
}

static SEXP c_revision_matmul(SEXP x, SEXP y) { return c_call_bench_matmul(x, y); }

static SEXP c_revision_dataframe(SEXP data) {
    SEXP x = VECTOR_ELT(data, 0), y = VECTOR_ELT(data, 1), group = VECTOR_ELT(data, 2);
    SEXP groups = PROTECT(Rf_allocVector(INTSXP, 10));
    SEXP sums = PROTECT(Rf_allocVector(REALSXP, 10));
    for (int i = 0; i < 10; ++i) { INTEGER(groups)[i] = i + 1; REAL(sums)[i] = 0.0; }
    for (R_xlen_t i = 0; i < XLENGTH(x); ++i) {
        double value = REAL(x)[i];
        if (!ISNAN(value) && value > 0.0) REAL(sums)[INTEGER(group)[i] - 1] += value / REAL(y)[i];
    }
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(result, 0, groups);
    SET_VECTOR_ELT(result, 1, sums);
    const char *names[] = {"grp", "z_sum"};
    c_benchmark_set_names(result, names, 2);
    SEXP row_names = PROTECT(Rf_allocVector(INTSXP, 2));
    SEXP class_name = PROTECT(Rf_mkString("data.frame"));
    INTEGER(row_names)[0] = NA_INTEGER;
    INTEGER(row_names)[1] = -10;
    Rf_setAttrib(result, R_ClassSymbol, class_name);
    Rf_setAttrib(result, R_RowNamesSymbol, row_names);
    UNPROTECT(5);
    return result;
}

static SEXP c_revision_list_sum(SEXP x) {
    long double total = 0.0;
    int na_seen = 0, nan_seen = 0;
    for (R_xlen_t i = 0; i < XLENGTH(x); ++i) {
        SEXP values = VECTOR_ELT(x, i);
        double item_total = c_exact_real_sum(REAL(values), XLENGTH(values));
        if (ISNA(item_total)) na_seen = 1;
        else if (ISNAN(item_total)) nan_seen = 1;
        else total += (long double) item_total;
    }
    if (na_seen) return Rf_ScalarReal(NA_REAL);
    if (nan_seen) return Rf_ScalarReal(R_NaN);
    if (total > DBL_MAX) return Rf_ScalarReal(R_PosInf);
    if (total < -DBL_MAX) return Rf_ScalarReal(R_NegInf);
    return Rf_ScalarReal((double) total);
}

static SEXP c_revision_string_concat(SEXP x) {
    SEXP separator = PROTECT(Rf_mkString(", "));
    SEXP call = PROTECT(Rf_lang3(Rf_install("paste"), x, separator));
    SET_TAG(CDDR(call), Rf_install("collapse"));
    SEXP result = Rf_eval(call, R_BaseEnv);
    UNPROTECT(2);
    return result;
}

static SEXP c_revision_string_metadata(SEXP x) {
    int counts[5] = {0, 0, 0, 0, 0};
    for (R_xlen_t i = 0; i < XLENGTH(x); ++i) {
        SEXP value = STRING_ELT(x, i);
        if (value == NA_STRING) { ++counts[4]; continue; }
        counts[0] += LENGTH(value);
        switch (Rf_getCharCE(value)) {
            case CE_UTF8: ++counts[1]; break;
            case CE_LATIN1: ++counts[2]; break;
            case CE_BYTES: ++counts[3]; break;
            default: break;
        }
    }
    SEXP result = PROTECT(Rf_allocVector(INTSXP, 5));
    memcpy(INTEGER(result), counts, sizeof(counts));
    const char *names[] = {"bytes", "utf8", "latin1", "bytes_marked", "missing"};
    c_benchmark_set_names(result, names, 5);
    UNPROTECT(1);
    return result;
}

static SEXP c_revision_factor(SEXP x) {
    SEXP levels = PROTECT(Rf_allocVector(STRSXP, 100));
    char level[16];
    for (int i = 0; i < 100; ++i) {
        snprintf(level, sizeof(level), "level_%03d", i + 1);
        SET_STRING_ELT(levels, i, Rf_mkChar(level));
    }
    SEXP call = PROTECT(Rf_lang3(Rf_install("factor"), x, levels));
    SET_TAG(CDDR(call), Rf_install("levels"));
    SEXP result = Rf_eval(call, R_BaseEnv);
    UNPROTECT(2);
    return result;
}

static SEXP c_revision_attributes(SEXP x) {
    SEXP result = PROTECT(Rf_duplicate(x));
    SEXP class_name = PROTECT(Rf_mkString("bench_class"));
    SEXP creator = PROTECT(Rf_mkString("zigr_bench"));
    Rf_setAttrib(result, R_ClassSymbol, class_name);
    Rf_setAttrib(result, Rf_install("creator"), creator);
    (void) Rf_getAttrib(result, R_ClassSymbol);
    (void) Rf_getAttrib(result, Rf_install("creator"));
    UNPROTECT(3);
    return result;
}

static SEXP c_revision_s4(SEXP x) { return c_call_bench_s4_slot_access(x); }
static SEXP c_revision_logical_counts(SEXP x) { return c_benchmark_fixture_logical_counts(x); }
static SEXP c_revision_raw_copy(SEXP x) { return c_benchmark_fixture_raw(x); }

static SEXP c_revision_complex_conjugate(SEXP x) {
    R_xlen_t n = XLENGTH(x);
    SEXP result = PROTECT(Rf_allocVector(CPLXSXP, n));
    for (R_xlen_t i = 0; i < n; ++i) {
        COMPLEX(result)[i].r = COMPLEX(x)[i].r;
        COMPLEX(result)[i].i = -COMPLEX(x)[i].i;
    }
    UNPROTECT(1);
    return result;
}

static SEXP c_revision_schema(SEXP x) { return c_benchmark_fixture_schema(x); }
static SEXP c_revision_altrep_sum(SEXP x) { return c_benchmark_fixture_altrep_integer(x); }

static SEXP c_revision_altrep_index(SEXP x) {
    R_xlen_t n = XLENGTH(x);
    double total = 0.0;
    for (R_xlen_t i = 0; i < n; i += 32) total += INTEGER_ELT(x, i);
    return Rf_ScalarReal(total);
}

static SEXP c_revision_altrep_materialize(SEXP x) {
    R_xlen_t n = XLENGTH(x);
    SEXP result = PROTECT(Rf_allocVector(INTSXP, n));
    for (R_xlen_t i = 0; i < n; ++i) INTEGER(result)[i] = INTEGER_ELT(x, i);
    UNPROTECT(1);
    return result;
}

static SEXP c_revision_external_state(void) {
    SEXP state = PROTECT(c_benchmark_fixture_new());
    SEXP amount = PROTECT(Rf_ScalarInteger(7));
    for (int i = 0; i < 100; ++i) c_benchmark_fixture_method(state, amount);
    SEXP result = c_benchmark_fixture_read(state);
    UNPROTECT(2);
    return result;
}

static SEXP c_revision_eval(SEXP x) {
    SEXP environment = PROTECT(R_NewEnv(R_BaseEnv, TRUE, 1));
    Rf_defineVar(Rf_install("x"), x, environment);
    SEXP sum = PROTECT(Rf_lang2(Rf_install("sum"), Rf_install("x")));
    SEXP mean = PROTECT(Rf_lang2(Rf_install("mean"), Rf_install("x")));
    SEXP expression = PROTECT(Rf_lang3(Rf_install("+"), sum, mean));
    SEXP result = Rf_eval(expression, environment);
    UNPROTECT(4);
    return result;
}

static SEXP c_revision_serialize(SEXP x) {
    SEXP version = PROTECT(Rf_ScalarInteger(3));
    SEXP call = PROTECT(Rf_lang4(Rf_install("serialize"), x, R_NilValue, version));
    SET_TAG(CDDDR(call), Rf_install("version"));
    SEXP bytes = PROTECT(Rf_eval(call, R_BaseEnv));
    SEXP restore = PROTECT(Rf_lang2(Rf_install("unserialize"), bytes));
    SEXP result = Rf_eval(restore, R_BaseEnv);
    UNPROTECT(4);
    return result;
}

static SEXP c_revision_rng(SEXP n) { return c_call_bench_rng_stress(n); }
static SEXP c_revision_outputs(void) { return c_benchmark_fixture_outputs(); }
static SEXP c_revision_is_altrep(SEXP x) { return Rf_ScalarLogical(ALTREP(x)); }
static SEXP c_revision_altrep_unmaterialized(SEXP x) {
    return Rf_ScalarLogical(ALTREP(x) && DATAPTR_OR_NULL(x) == NULL);
}

static const R_CallMethodDef CallEntries[] = {
  {"c_revision_vector_sum",         (DL_FUNC) &c_revision_vector_sum,         1},
  {"c_revision_numeric_transform",  (DL_FUNC) &c_revision_numeric_transform,  1},
  {"c_revision_broadcast",          (DL_FUNC) &c_revision_broadcast,          2},
  {"c_revision_sort",               (DL_FUNC) &c_revision_sort,               1},
  {"c_revision_missing_mean",       (DL_FUNC) &c_revision_missing_mean,       1},
  {"c_revision_transpose",          (DL_FUNC) &c_revision_transpose,          1},
  {"c_revision_rowcol",             (DL_FUNC) &c_revision_rowcol,             1},
  {"c_revision_matmul",             (DL_FUNC) &c_revision_matmul,             2},
  {"c_revision_dataframe",          (DL_FUNC) &c_revision_dataframe,          1},
  {"c_revision_list_sum",           (DL_FUNC) &c_revision_list_sum,           1},
  {"c_revision_string_concat",      (DL_FUNC) &c_revision_string_concat,      1},
  {"c_revision_string_metadata",    (DL_FUNC) &c_revision_string_metadata,    1},
  {"c_revision_factor",             (DL_FUNC) &c_revision_factor,             1},
  {"c_revision_attributes",         (DL_FUNC) &c_revision_attributes,         1},
  {"c_revision_s4",                 (DL_FUNC) &c_revision_s4,                 1},
  {"c_revision_logical_counts",     (DL_FUNC) &c_revision_logical_counts,     1},
  {"c_revision_raw_copy",           (DL_FUNC) &c_revision_raw_copy,           1},
  {"c_revision_complex_conjugate",  (DL_FUNC) &c_revision_complex_conjugate,  1},
  {"c_revision_schema",             (DL_FUNC) &c_revision_schema,             1},
  {"c_revision_altrep_sum",         (DL_FUNC) &c_revision_altrep_sum,         1},
  {"c_revision_altrep_index",       (DL_FUNC) &c_revision_altrep_index,       1},
  {"c_revision_altrep_materialize", (DL_FUNC) &c_revision_altrep_materialize, 1},
  {"c_revision_external_state",     (DL_FUNC) &c_revision_external_state,     0},
  {"c_revision_eval",               (DL_FUNC) &c_revision_eval,               1},
  {"c_revision_serialize",          (DL_FUNC) &c_revision_serialize,          1},
  {"c_revision_rng",                (DL_FUNC) &c_revision_rng,                1},
  {"c_revision_outputs",            (DL_FUNC) &c_revision_outputs,            0},
  {"c_revision_is_altrep",          (DL_FUNC) &c_revision_is_altrep,          1},
  {"c_revision_altrep_unmaterialized", (DL_FUNC) &c_revision_altrep_unmaterialized, 1},
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
  {"c_benchmark_fixture_zero",               (DL_FUNC) &c_benchmark_fixture_zero,               0},
  {"c_benchmark_fixture_scalar",             (DL_FUNC) &c_benchmark_fixture_scalar,             1},
  {"c_benchmark_fixture_numeric",            (DL_FUNC) &c_benchmark_fixture_numeric,            1},
  {"c_benchmark_fixture_altrep_integer",     (DL_FUNC) &c_benchmark_fixture_altrep_integer,     1},
  {"c_benchmark_fixture_strings",            (DL_FUNC) &c_benchmark_fixture_strings,            1},
  {"c_benchmark_fixture_raw",                (DL_FUNC) &c_benchmark_fixture_raw,                1},
  {"c_benchmark_fixture_complex",            (DL_FUNC) &c_benchmark_fixture_complex,            1},
  {"c_benchmark_fixture_logical_counts",     (DL_FUNC) &c_benchmark_fixture_logical_counts,     1},
  {"c_benchmark_fixture_schema",             (DL_FUNC) &c_benchmark_fixture_schema,             1},
  {"c_benchmark_fixture_new",                (DL_FUNC) &c_benchmark_fixture_new,                0},
  {"c_benchmark_fixture_method",             (DL_FUNC) &c_benchmark_fixture_method,             2},
  {"c_benchmark_fixture_read",               (DL_FUNC) &c_benchmark_fixture_read,               1},
  {"c_benchmark_fixture_error",              (DL_FUNC) &c_benchmark_fixture_error,              1},
  {"c_benchmark_fixture_outputs",            (DL_FUNC) &c_benchmark_fixture_outputs,            0},
  {"c_benchmark_lifecycle_reset",            (DL_FUNC) &c_benchmark_lifecycle_reset,            0},
  {"c_benchmark_lifecycle_snapshot",         (DL_FUNC) &c_benchmark_lifecycle_snapshot,         0},
  {"c_benchmark_same_sexp",                  (DL_FUNC) &c_benchmark_same_sexp,                  2},
  {"c_benchmark_same_data_pointer",          (DL_FUNC) &c_benchmark_same_data_pointer,          2},
  {"c_benchmark_wrong_pointer",              (DL_FUNC) &c_benchmark_wrong_pointer,              0},
  {"c_benchmark_cleared_pointer_like",       (DL_FUNC) &c_benchmark_cleared_pointer_like,       1},
  {"c_benchmark_altrep_new",                 (DL_FUNC) &c_benchmark_altrep_new,                 2},
  {"c_benchmark_altrep_reset",               (DL_FUNC) &c_benchmark_altrep_reset,               0},
  {"c_benchmark_altrep_snapshot",            (DL_FUNC) &c_benchmark_altrep_snapshot,            1},
  {"c_measurement_probe_noop",                (DL_FUNC) &c_measurement_probe_noop,                0},
  {"c_measurement_probe_cpu",                 (DL_FUNC) &c_measurement_probe_cpu,                 1},
  {"c_measurement_probe_allocate",            (DL_FUNC) &c_measurement_probe_allocate,            1},
  {NULL, NULL, 0}
};

static const R_ExternalMethodDef ExternalEntries[] = {
  {"c_fixture_external", (DL_FUNC) &c_fixture_external, 1},
  {NULL, NULL, 0}
};

void R_init_bench(DllInfo *dll) {
    c_benchmark_altrep_integer_class = R_make_altinteger_class(
        "benchmark_instrumented_integer", "zigrBenchmarks", dll
    );
    R_set_altrep_Length_method(c_benchmark_altrep_integer_class, c_benchmark_altrep_length);
    R_set_altvec_Dataptr_method(c_benchmark_altrep_integer_class, c_benchmark_altrep_dataptr);
    R_set_altvec_Dataptr_or_null_method(c_benchmark_altrep_integer_class, c_benchmark_altrep_dataptr_or_null);
    R_set_altinteger_Elt_method(c_benchmark_altrep_integer_class, c_benchmark_altrep_elt);
    R_set_altinteger_Get_region_method(c_benchmark_altrep_integer_class, c_benchmark_altrep_get_region);
    R_registerRoutines(dll, NULL, CallEntries, NULL, ExternalEntries);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
