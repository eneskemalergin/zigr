#include <R.h>
#include <Rinternals.h>
#include <stdint.h>
#include <string.h>

static void radix_sort_f64(double *arr, size_t n) {
    if (n < 2) return;

    uint64_t *buf = (uint64_t *)arr;
    const uint64_t sign_bit = (uint64_t)1 << 63;

    for (size_t i = 0; i < n; i++) {
        uint64_t v = buf[i];
        buf[i] = (v & sign_bit) ? ~v : v ^ sign_bit;
    }

    uint64_t stack_temp[256];
    uint64_t *temp = (n <= 256) ? stack_temp : (uint64_t *)R_chk_calloc(n, sizeof(uint64_t));
    int needs_free = (n > 256);

    for (int shift = 0; shift < 64; shift += 8) {
        unsigned int counts[256];
        memset(counts, 0, sizeof(counts));

        for (size_t i = 0; i < n; i++) {
            counts[(buf[i] >> shift) & 0xFF]++;
        }

        unsigned int total = 0;
        for (int j = 0; j < 256; j++) {
            unsigned int old = counts[j];
            counts[j] = total;
            total += old;
        }

        for (size_t i = 0; i < n; i++) {
            uint64_t v = buf[i];
            unsigned int digit = (v >> shift) & 0xFF;
            temp[counts[digit]++] = v;
        }

        memcpy(buf, temp, n * sizeof(uint64_t));
    }

    if (needs_free) R_chk_free(temp);

    for (size_t i = 0; i < n; i++) {
        uint64_t v = buf[i];
        buf[i] = (v & sign_bit) ? v ^ sign_bit : ~v;
    }
}

SEXP c_call_bench_sort(SEXP arg) {
    double *xp = REAL(arg);
    R_xlen_t n = XLENGTH(arg);

    SEXP result = PROTECT(Rf_allocVector(REALSXP, n));
    double *rp = REAL(result);
    memcpy(rp, xp, n * sizeof(double));

    radix_sort_f64(rp, (size_t)n);
    UNPROTECT(1);
    return result;
}
