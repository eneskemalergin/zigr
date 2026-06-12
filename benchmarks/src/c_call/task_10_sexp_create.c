#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_sexp_create(SEXP arg) {
    (void)arg;
    for (int b = 0; b < 10; b++) {
        for (int i = 0; i < 10000; i++) {
            PROTECT(Rf_allocVector(REALSXP, 1));
        }
        UNPROTECT(10000);
    }
    return Rf_ScalarReal(0.0);
}
