#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_protect_scaling(SEXP arg) {
    for (int r = 0; r < 100; r++) {
        for (int b = 0; b < 10; b++) {
            for (int i = 0; i < 10000; i++) PROTECT(arg);
            Rf_unprotect(10000);
        }
    }
    return Rf_ScalarInteger(0);
}
