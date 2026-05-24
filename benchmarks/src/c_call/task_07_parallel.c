// Task 7: Parallel vector sum with 4 threads.
#include <Rinternals.h>
#include <pthread.h>

typedef struct {
  double *data;
  R_xlen_t start, end;
  double result;
} chunk_t;

static void *sum_chunk(void *arg) {
  chunk_t *ck = (chunk_t *)arg;
  double total = 0.0;
  for (R_xlen_t i = ck->start; i < ck->end; i++) total += ck->data[i];
  ck->result = total;
  return NULL;
}

SEXP c_call_bench_parallel(SEXP vec) {
  double *data = REAL(vec);
  R_xlen_t n = XLENGTH(vec);
  int nthreads = 4;
  pthread_t threads[4];
  chunk_t chunks[4];

  R_xlen_t chunk_size = n / nthreads;
  for (int i = 0; i < nthreads; i++) {
    chunks[i].data = data;
    chunks[i].start = i * chunk_size;
    chunks[i].end = (i == nthreads - 1) ? n : (i + 1) * chunk_size;
    chunks[i].result = 0.0;
    pthread_create(&threads[i], NULL, sum_chunk, &chunks[i]);
  }

  double total = 0.0;
  for (int i = 0; i < nthreads; i++) {
    pthread_join(threads[i], NULL);
    total += chunks[i].result;
  }

  return ScalarReal(total);
}
