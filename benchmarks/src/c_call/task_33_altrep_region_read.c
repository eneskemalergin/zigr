#include <R.h>
#include <Rinternals.h>

#define CHUNK 4096

SEXP c_call_bench_altrep_region_read(SEXP arg) {
    SEXP call = PROTECT(Rf_lang2(Rf_install("seq_len"), arg));
    int err = 0;
    SEXP alt = R_tryEvalSilent(call, R_GlobalEnv, &err);
    UNPROTECT(1);
    if (err != 0) return R_NilValue;
    R_xlen_t n = XLENGTH(alt);
    int buf[CHUNK];
    long long total = 0;
    R_xlen_t i = 0;
    while (i < n) {
        R_xlen_t want = CHUNK;
        if (n - i < want) want = n - i;
        R_xlen_t got = INTEGER_GET_REGION(alt, i, want, buf);
        for (R_xlen_t j = 0; j < got; j++) total += buf[j];
        i += got;
    }
    return Rf_ScalarReal((double)total);
}
