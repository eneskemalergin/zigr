#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_protect_shallow(SEXP arg) {
    for (int r = 0; r < 100; r++) {
        for (int i = 0; i < 10; i++) PROTECT(arg);
        UNPROTECT(10);
    }
    return Rf_ScalarInteger(0);
}
