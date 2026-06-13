#include <R.h>
#include <Rinternals.h>

#define STEP 10000

SEXP c_call_bench_long_vector_idx(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    long long total = 0;
    for (R_xlen_t i = 0; i < n; i += STEP) {
        total += INTEGER_ELT(arg, i);
    }
    return Rf_ScalarReal((double)total);
}
