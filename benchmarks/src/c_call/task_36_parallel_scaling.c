// Task 36: Parallel thread-count sweep.
#include <Rinternals.h>
#include <pthread.h>

typedef struct {
  const double *data;
  R_xlen_t start;
  R_xlen_t end;
  double result;
} scaling_chunk_t;

static double serial_sum(const double *data, R_xlen_t start, R_xlen_t end) {
  double total = 0.0;
  for (R_xlen_t i = start; i < end; ++i) total += data[i];
  return total;
}

static void *parallel_scaling_worker(void *arg) {
  scaling_chunk_t *chunk = (scaling_chunk_t *)arg;
  chunk->result = serial_sum(chunk->data, chunk->start, chunk->end);
  return NULL;
}

static double parallel_scaling_sum(const double *data, R_xlen_t n, int requested_threads) {
  if (n == 0) return 0.0;
  int actual_threads = requested_threads;
  if ((R_xlen_t)actual_threads > n) actual_threads = (int)n;
  if (actual_threads <= 1) return serial_sum(data, 0, n);

  pthread_t threads[16];
  scaling_chunk_t chunks[16];
  int started = 0;
  R_xlen_t chunk_size = n / actual_threads;

  for (int i = 0; i < actual_threads; ++i) {
    chunks[i].data = data;
    chunks[i].start = (R_xlen_t)i * chunk_size;
    chunks[i].end = (i == actual_threads - 1) ? n : (R_xlen_t)(i + 1) * chunk_size;
    chunks[i].result = 0.0;
    if (pthread_create(&threads[started], NULL, parallel_scaling_worker, &chunks[i]) == 0) {
      started++;
    } else {
      chunks[i].result = serial_sum(data, chunks[i].start, chunks[i].end);
    }
  }

  for (int i = 0; i < started; ++i) pthread_join(threads[i], NULL);

  double total = 0.0;
  for (int i = 0; i < actual_threads; ++i) total += chunks[i].result;
  return total;
}

SEXP c_call_bench_parallel_scaling(SEXP vec) {
  static const int thread_counts[] = {1, 2, 4, 8, 16};
  const double *data = REAL(vec);
  R_xlen_t n = XLENGTH(vec);

  SEXP result = PROTECT(allocVector(REALSXP, 5));
  SEXP names = PROTECT(allocVector(STRSXP, 5));

  for (int i = 0; i < 5; ++i) {
    REAL(result)[i] = parallel_scaling_sum(data, n, thread_counts[i]);
  }

  SET_STRING_ELT(names, 0, mkChar("threads_1"));
  SET_STRING_ELT(names, 1, mkChar("threads_2"));
  SET_STRING_ELT(names, 2, mkChar("threads_4"));
  SET_STRING_ELT(names, 3, mkChar("threads_8"));
  SET_STRING_ELT(names, 4, mkChar("threads_16"));
  setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
  return result;
}