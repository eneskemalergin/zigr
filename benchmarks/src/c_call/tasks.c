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

SEXP c_call_bench_matrix_transpose(SEXP arg) {
    int nr = Rf_nrows(arg);
    int nc = Rf_ncols(arg);
    double *xp = REAL(arg);

    SEXP result = PROTECT(Rf_allocMatrix(REALSXP, nc, nr));
    double *rp = REAL(result);

    for (int jj = 0; jj < nc; jj += 32) {
        int j_end = jj + 32 < nc ? jj + 32 : nc;
        for (int ii = 0; ii < nr; ii += 32) {
            int i_end = ii + 32 < nr ? ii + 32 : nr;
            for (int i = ii; i < i_end; i++) {
                double *out_row = rp + i * nc;
                for (int j = jj; j < j_end; j++) {
                    out_row[j] = xp[i + j * nr];
                }
            }
        }
    }
    UNPROTECT(1);
    return result;
}

extern void dgemm_(char *transa, char *transb, int *m, int *n, int *k,
                   double *alpha, double *a, int *lda, double *b, int *ldb,
                   double *beta, double *c, int *ldc);

SEXP c_call_bench_matmul(SEXP a, SEXP b) {
    int n = Rf_nrows(a);
    int m = Rf_ncols(b);
    int k = Rf_ncols(a);

    SEXP result = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)n * m));
    SEXP dims = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(dims)[0] = n;
    INTEGER(dims)[1] = m;
    Rf_setAttrib(result, R_DimSymbol, dims);
    UNPROTECT(1);

    double alpha = 1.0, beta = 0.0;
    char notrans = 'N';
    dgemm_(&notrans, &notrans, &n, &m, &k,
           &alpha, REAL(a), &n, REAL(b), &k,
           &beta, REAL(result), &n);
    UNPROTECT(1);
    return result;
}

SEXP c_call_bench_s4_slot_access(SEXP arg) {
    int err = 0;
    SEXP new_call = PROTECT(Rf_lang2(Rf_install("new"), Rf_mkString("BenchS4")));
    SEXP obj = PROTECT(R_tryEvalSilent(new_call, R_GlobalEnv, &err));
    UNPROTECT(1);
    if (err != 0) { UNPROTECT(1); return arg; }

    SEXP slot_sym = Rf_install("slot_x");
    R_do_slot_assign(obj, slot_sym, arg);
    SEXP result = R_do_slot(obj, slot_sym);
    UNPROTECT(1);
    return result;
}

SEXP c_call_bench_rng_stress(SEXP arg) {
    int n = Rf_asInteger(arg);
    SEXP result = PROTECT(Rf_allocVector(REALSXP, n));
    double *rp = REAL(result);
    GetRNGstate();
    for (int i = 0; i < n; i++) rp[i] = norm_rand();
    PutRNGstate();
    UNPROTECT(1);
    return result;
}
