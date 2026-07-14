
// clang-format sorts includes unless SortIncludes: Never. However, the ordering
// does matter here. So, we need to disable clang-format for safety.

// clang-format off
#include <stdint.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>
// clang-format on

#include "rust/api.h"

static uintptr_t TAGGED_POINTER_MASK = (uintptr_t)1;

SEXP handle_result(SEXP res_) {
    uintptr_t res = (uintptr_t)res_;

    // An error is indicated by tag.
    if ((res & TAGGED_POINTER_MASK) == 1) {
        // Remove tag
        SEXP res_aligned = (SEXP)(res & ~TAGGED_POINTER_MASK);

        // Currently, there are two types of error cases:
        //
        //   1. Error from Rust code
        //   2. Error from R's C API, which is caught by R_UnwindProtect()
        //
        if (TYPEOF(res_aligned) == CHARSXP) {
            // In case 1, the result is an error message that can be passed to
            // Rf_errorcall() directly.
            Rf_errorcall(R_NilValue, "%s", CHAR(res_aligned));
        } else {
            // In case 2, the result is the token to restart the
            // cleanup process on R's side.
            R_ContinueUnwind(res_aligned);
        }
    }

    return (SEXP)res;
}

SEXP savvy_fixture_altrep_integer__impl(SEXP c_arg__value) {
    SEXP res = savvy_fixture_altrep_integer__ffi(c_arg__value);
    return handle_result(res);
}

SEXP savvy_fixture_complex__impl(SEXP c_arg__value) {
    SEXP res = savvy_fixture_complex__ffi(c_arg__value);
    return handle_result(res);
}

SEXP savvy_fixture_error__impl(SEXP c_arg___trigger) {
    SEXP res = savvy_fixture_error__ffi(c_arg___trigger);
    return handle_result(res);
}

SEXP savvy_fixture_lifecycle_counts__impl(void) {
    SEXP res = savvy_fixture_lifecycle_counts__ffi();
    return handle_result(res);
}

SEXP savvy_fixture_lifecycle_reset__impl(void) {
    SEXP res = savvy_fixture_lifecycle_reset__ffi();
    return handle_result(res);
}

SEXP savvy_fixture_logical_counts__impl(SEXP c_arg__value) {
    SEXP res = savvy_fixture_logical_counts__ffi(c_arg__value);
    return handle_result(res);
}

SEXP savvy_fixture_numeric__impl(SEXP c_arg__value) {
    SEXP res = savvy_fixture_numeric__ffi(c_arg__value);
    return handle_result(res);
}

SEXP savvy_fixture_outputs__impl(void) {
    SEXP res = savvy_fixture_outputs__ffi();
    return handle_result(res);
}

SEXP savvy_fixture_raw__impl(SEXP c_arg__value) {
    SEXP res = savvy_fixture_raw__ffi(c_arg__value);
    return handle_result(res);
}

SEXP savvy_fixture_scalar__impl(SEXP c_arg__value) {
    SEXP res = savvy_fixture_scalar__ffi(c_arg__value);
    return handle_result(res);
}

SEXP savvy_fixture_schema__impl(SEXP c_arg__value) {
    SEXP res = savvy_fixture_schema__ffi(c_arg__value);
    return handle_result(res);
}

SEXP savvy_fixture_strings__impl(SEXP c_arg__value) {
    SEXP res = savvy_fixture_strings__ffi(c_arg__value);
    return handle_result(res);
}

SEXP savvy_fixture_zero__impl(void) {
    SEXP res = savvy_fixture_zero__ffi();
    return handle_result(res);
}

SEXP savvy_FixtureState_increment__impl(SEXP self__, SEXP c_arg__amount) {
    SEXP res = savvy_FixtureState_increment__ffi(self__, c_arg__amount);
    return handle_result(res);
}

SEXP savvy_FixtureState_new__impl(void) {
    SEXP res = savvy_FixtureState_new__ffi();
    return handle_result(res);
}

SEXP savvy_FixtureState_read__impl(SEXP self__) {
    SEXP res = savvy_FixtureState_read__ffi(self__);
    return handle_result(res);
}


static const R_CallMethodDef CallEntries[] = {
    {"savvy_fixture_altrep_integer__impl", (DL_FUNC) &savvy_fixture_altrep_integer__impl, 1},
    {"savvy_fixture_complex__impl", (DL_FUNC) &savvy_fixture_complex__impl, 1},
    {"savvy_fixture_error__impl", (DL_FUNC) &savvy_fixture_error__impl, 1},
    {"savvy_fixture_lifecycle_counts__impl", (DL_FUNC) &savvy_fixture_lifecycle_counts__impl, 0},
    {"savvy_fixture_lifecycle_reset__impl", (DL_FUNC) &savvy_fixture_lifecycle_reset__impl, 0},
    {"savvy_fixture_logical_counts__impl", (DL_FUNC) &savvy_fixture_logical_counts__impl, 1},
    {"savvy_fixture_numeric__impl", (DL_FUNC) &savvy_fixture_numeric__impl, 1},
    {"savvy_fixture_outputs__impl", (DL_FUNC) &savvy_fixture_outputs__impl, 0},
    {"savvy_fixture_raw__impl", (DL_FUNC) &savvy_fixture_raw__impl, 1},
    {"savvy_fixture_scalar__impl", (DL_FUNC) &savvy_fixture_scalar__impl, 1},
    {"savvy_fixture_schema__impl", (DL_FUNC) &savvy_fixture_schema__impl, 1},
    {"savvy_fixture_strings__impl", (DL_FUNC) &savvy_fixture_strings__impl, 1},
    {"savvy_fixture_zero__impl", (DL_FUNC) &savvy_fixture_zero__impl, 0},
    {"savvy_FixtureState_increment__impl", (DL_FUNC) &savvy_FixtureState_increment__impl, 2},
    {"savvy_FixtureState_new__impl", (DL_FUNC) &savvy_FixtureState_new__impl, 0},
    {"savvy_FixtureState_read__impl", (DL_FUNC) &savvy_FixtureState_read__impl, 1},
    {NULL, NULL, 0}
};

void R_init_zigrSavvy(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    // Functions for initialization, if any.

}
