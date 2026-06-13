#include <R.h>
#include <Rinternals.h>

static char dummy;

SEXP c_call_bench_external_ptr(SEXP arg) {
    (void)arg;
    SEXP ptr = PROTECT(R_MakeExternalPtr(&dummy, R_NilValue, R_NilValue));
    UNPROTECT(1);
    return ptr;
}
