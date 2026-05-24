// Task 18: LSD radix sort for doubles (1e6 elements).
#include <Rinternals.h>
#include <math.h>
#include <string.h>
#include <stdint.h>

static void radix_sort_f64(double *arr, size_t n) {
  if (n < 2) return;
  uint64_t *buf = (uint64_t *)arr;
  const uint64_t sign_bit = 1ULL << 63;

  // Transform: positive -> flip sign bit, negative -> flip all bits
  for (size_t i = 0; i < n; i++) {
    uint64_t v = buf[i];
    buf[i] = (v & sign_bit) ? ~v : v ^ sign_bit;
  }

  // LSD radix sort, 8 bits per pass
  size_t counts[256];
  uint64_t *temp = (uint64_t *)R_alloc(n * 8, 1);

  for (int shift = 0; shift < 64; shift += 8) {
    memset(counts, 0, sizeof(counts));
    for (size_t i = 0; i < n; i++) counts[(buf[i] >> shift) & 0xFF]++;

    size_t total = 0;
    for (int i = 0; i < 256; i++) {
      size_t old = counts[i];
      counts[i] = total;
      total += old;
    }

    for (size_t i = 0; i < n; i++) {
      uint64_t v = buf[i];
      temp[counts[(v >> shift) & 0xFF]++] = v;
    }

    memcpy(buf, temp, n * sizeof(uint64_t));
  }

  // Restore
  for (size_t i = 0; i < n; i++) {
    uint64_t v = buf[i];
    buf[i] = (v & sign_bit) ? v ^ sign_bit : ~v;
  }
}

SEXP c_call_bench_sort(SEXP vec_sexp) {
  R_xlen_t n = XLENGTH(vec_sexp);
  double *src = REAL(vec_sexp);

  SEXP result = PROTECT(allocVector(REALSXP, n));
  double *rp = REAL(result);
  memcpy(rp, src, (size_t)n * sizeof(double));

  radix_sort_f64(rp, n);

  UNPROTECT(1);
  return result;
}
