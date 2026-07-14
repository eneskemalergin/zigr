#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>

typedef struct {
    int value;
} c_call_benchmark_state;

static void c_call_benchmark_state_finalizer(SEXP pointer) {
    c_call_benchmark_state *state = (c_call_benchmark_state *) R_ExternalPtrAddr(pointer);
    if (state == NULL) return;
    free(state);
    R_ClearExternalPtr(pointer);
}

SEXP c_call_bench_external_ptr(SEXP arg) {
    int value = Rf_asInteger(arg);
    SEXP ptr = PROTECT(R_MakeExternalPtr(
        NULL,
        Rf_install("zigr.p4.c_call.task42.state"),
        R_NilValue
    ));
    c_call_benchmark_state *state = (c_call_benchmark_state *) malloc(sizeof(c_call_benchmark_state));
    if (state == NULL) {
        UNPROTECT(1);
        Rf_error("C external-pointer benchmark could not allocate state");
    }
    state->value = value;
    R_SetExternalPtrAddr(ptr, state);
    R_RegisterCFinalizerEx(ptr, c_call_benchmark_state_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}
