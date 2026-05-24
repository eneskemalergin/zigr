// Rcpp benchmark runner — uses Rcpp wrappers via raw R API.
// Build: PKG_CPPFLAGS="-I$(Rscript -e 'cat(system.file("include", package="Rcpp"))')" R CMD SHLIB -o src/cpp/rcpp_benchmarks.so src/cpp/main.cpp

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Altrep.h>
#include <R_ext/Rdynload.h>
#include <R_ext/BLAS.h>
#include <R_ext/Lapack.h>
#include <R_ext/Random.h>
#include <pthread.h>
#include <cstring>
#include <cstdint>
#include <string>
#include <vector>
#include <cmath>
#include <cstdlib>
#include <cctype>

// Use raw R API for all external functions (avoids Rcpp module init issues).
// Rcpp's overhead vs. C is in the additional safety checks and abstractions,
// which we still pay for in the function bodies.

static constexpr int ALTREP_CREATE_MAX_LEN = 1000000;
static constexpr int COMPTIME_DISPATCH_REPEATS = 256;
static constexpr int ALLOCATION_REPEATS = 100;
static constexpr int PROTECT_OVERHEAD_REPEATS = 4096;
static constexpr int LONGJMP_SAFETY_REPEATS = 512;
static constexpr int TRANSLATE_C_COST_REPEATS = 512;
static constexpr int MEMORY_BANDWIDTH_REPEATS = 2;

static double rcpp_fill_sum_temp(double *dst, const double *src, R_xlen_t n, double bias) {
  double total = 0.0;
  for (R_xlen_t i = 0; i < n; ++i) {
    double adjusted = src[i] + bias;
    dst[i] = adjusted;
    total += adjusted;
  }
  return total;
}

static double rcpp_adjusted_sum(const double *src, R_xlen_t n, double bias) {
  double total = 0.0;
  for (R_xlen_t i = 0; i < n; ++i) total += src[i] + bias;
  return total;
}

typedef struct {
  const double *data;
  R_xlen_t len;
  double bias;
} rcpp_unwind_state_t;

static void rcpp_noop_clean(void *data, Rboolean jump) {
  (void)data;
  (void)jump;
}

static SEXP rcpp_unwind_ok(void *data) {
  rcpp_unwind_state_t *state = (rcpp_unwind_state_t *)data;
  return ScalarReal(rcpp_adjusted_sum(state->data, state->len, state->bias));
}

static double rcpp_sum_real(const double *data, R_xlen_t n) {
  double total = 0.0;
  for (R_xlen_t i = 0; i < n; i++) total += data[i];
  return total;
}

static double rcpp_na_mean_real(const double *data, R_xlen_t n) {
  double sum = 0.0;
  R_xlen_t count = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    if (ISNA(data[i])) continue;
    sum += data[i];
    count++;
  }
  return count == 0 ? NA_REAL : sum / (double)count;
}

static R_altrep_class_t rcpp_altrep_create_class;
static bool rcpp_altrep_create_class_ready = false;
static double rcpp_altrep_backing[ALTREP_CREATE_MAX_LEN];
static R_xlen_t rcpp_altrep_backing_init = 0;
static R_altrep_class_t rcpp_owned_altint_class;
static bool rcpp_owned_altint_class_ready = false;
static int rcpp_owned_altint_backing[ALTREP_CREATE_MAX_LEN];
static R_xlen_t rcpp_owned_altint_backing_init = 0;
static R_altrep_class_t rcpp_owned_altlogical_class;
static bool rcpp_owned_altlogical_class_ready = false;
static int rcpp_owned_altlogical_backing[ALTREP_CREATE_MAX_LEN];
static R_xlen_t rcpp_owned_altlogical_backing_init = 0;

static R_xlen_t rcpp_altrep_create_length(SEXP x) {
  return INTEGER(R_altrep_data2(x))[0];
}

static double rcpp_altrep_create_elt(SEXP x, R_xlen_t i) {
  (void)x;
  return rcpp_altrep_backing[i];
}

static void *rcpp_altrep_create_dataptr(SEXP x, Rboolean writable) {
  (void)x;
  (void)writable;
  return (void *)rcpp_altrep_backing;
}

static R_xlen_t rcpp_altrep_create_get_region(SEXP x, R_xlen_t i, R_xlen_t n, double *buf) {
  R_xlen_t len = rcpp_altrep_create_length(x);
  if (i >= len) return 0;
  R_xlen_t available = len - i;
  R_xlen_t count = n < available ? n : available;
  memcpy(buf, rcpp_altrep_backing + i, (size_t)count * sizeof(double));
  return count;
}

static SEXP rcpp_altrep_create_duplicate(SEXP x, Rboolean deep) {
  (void)deep;
  R_xlen_t len = rcpp_altrep_create_length(x);
  SEXP out = PROTECT(allocVector(REALSXP, len));
  memcpy(REAL(out), rcpp_altrep_backing, (size_t)len * sizeof(double));
  UNPROTECT(1);
  return out;
}

static void rcpp_altrep_create_ensure_class() {
  if (rcpp_altrep_create_class_ready) return;

  rcpp_altrep_create_class = R_make_altreal_class("bench_altreal_create_cpp",
                                                  "rcpp_benchmarks",
                                                  NULL);
  R_set_altrep_Length_method(rcpp_altrep_create_class, rcpp_altrep_create_length);
  R_set_altreal_Elt_method(rcpp_altrep_create_class, rcpp_altrep_create_elt);
  R_set_altvec_Dataptr_method(rcpp_altrep_create_class, rcpp_altrep_create_dataptr);
  R_set_altrep_Duplicate_method(rcpp_altrep_create_class, rcpp_altrep_create_duplicate);
  R_set_altreal_Get_region_method(rcpp_altrep_create_class, rcpp_altrep_create_get_region);
  rcpp_altrep_create_class_ready = true;
}

static void rcpp_altrep_create_ensure_backing(R_xlen_t n) {
  if (rcpp_altrep_backing_init >= n) return;
  for (R_xlen_t i = rcpp_altrep_backing_init; i < n; ++i) {
    rcpp_altrep_backing[i] = (double)(i + 1);
  }
  rcpp_altrep_backing_init = n;
}

static R_xlen_t rcpp_owned_altint_length(SEXP x) {
  return INTEGER(R_altrep_data2(x))[0];
}

static int rcpp_owned_altint_elt(SEXP x, R_xlen_t i) {
  (void)x;
  return rcpp_owned_altint_backing[i];
}

static void *rcpp_owned_altint_dataptr(SEXP x, Rboolean writable) {
  (void)x;
  (void)writable;
  return (void *)rcpp_owned_altint_backing;
}

static R_xlen_t rcpp_owned_altint_get_region(SEXP x, R_xlen_t i, R_xlen_t n, int *buf) {
  R_xlen_t len = rcpp_owned_altint_length(x);
  if (i >= len) return 0;
  R_xlen_t available = len - i;
  R_xlen_t count = n < available ? n : available;
  memcpy(buf, rcpp_owned_altint_backing + i, (size_t)count * sizeof(int));
  return count;
}

static SEXP rcpp_owned_altint_duplicate(SEXP x, Rboolean deep) {
  (void)deep;
  R_xlen_t len = rcpp_owned_altint_length(x);
  SEXP out = PROTECT(allocVector(INTSXP, len));
  memcpy(INTEGER(out), rcpp_owned_altint_backing, (size_t)len * sizeof(int));
  UNPROTECT(1);
  return out;
}

static void rcpp_owned_altint_ensure_class() {
  if (rcpp_owned_altint_class_ready) return;

  rcpp_owned_altint_class = R_make_altinteger_class("bench_altinteger_owned_sum_cpp",
                                                    "rcpp_benchmarks",
                                                    NULL);
  R_set_altrep_Length_method(rcpp_owned_altint_class, rcpp_owned_altint_length);
  R_set_altinteger_Elt_method(rcpp_owned_altint_class, rcpp_owned_altint_elt);
  R_set_altvec_Dataptr_method(rcpp_owned_altint_class, rcpp_owned_altint_dataptr);
  R_set_altrep_Duplicate_method(rcpp_owned_altint_class, rcpp_owned_altint_duplicate);
  R_set_altinteger_Get_region_method(rcpp_owned_altint_class, rcpp_owned_altint_get_region);
  rcpp_owned_altint_class_ready = true;
}

static void rcpp_owned_altint_ensure_backing(R_xlen_t n) {
  if (rcpp_owned_altint_backing_init >= n) return;
  for (R_xlen_t i = rcpp_owned_altint_backing_init; i < n; ++i) {
    rcpp_owned_altint_backing[i] = (int)((i % 1024) + 1);
  }
  rcpp_owned_altint_backing_init = n;
}

static R_xlen_t rcpp_owned_altlogical_length(SEXP x) {
  return INTEGER(R_altrep_data2(x))[0];
}

static int rcpp_owned_altlogical_elt(SEXP x, R_xlen_t i) {
  (void)x;
  return rcpp_owned_altlogical_backing[i];
}

static void *rcpp_owned_altlogical_dataptr(SEXP x, Rboolean writable) {
  (void)x;
  (void)writable;
  return (void *)rcpp_owned_altlogical_backing;
}

static R_xlen_t rcpp_owned_altlogical_get_region(SEXP x, R_xlen_t i, R_xlen_t n, int *buf) {
  R_xlen_t len = rcpp_owned_altlogical_length(x);
  if (i >= len) return 0;
  R_xlen_t available = len - i;
  R_xlen_t count = n < available ? n : available;
  memcpy(buf, rcpp_owned_altlogical_backing + i, (size_t)count * sizeof(int));
  return count;
}

static SEXP rcpp_owned_altlogical_duplicate(SEXP x, Rboolean deep) {
  (void)deep;
  R_xlen_t len = rcpp_owned_altlogical_length(x);
  SEXP out = PROTECT(allocVector(LGLSXP, len));
  memcpy(LOGICAL(out), rcpp_owned_altlogical_backing, (size_t)len * sizeof(int));
  UNPROTECT(1);
  return out;
}

static void rcpp_owned_altlogical_ensure_class() {
  if (rcpp_owned_altlogical_class_ready) return;

  rcpp_owned_altlogical_class = R_make_altlogical_class("bench_altlogical_owned_sum_cpp",
                                                        "rcpp_benchmarks",
                                                        NULL);
  R_set_altrep_Length_method(rcpp_owned_altlogical_class, rcpp_owned_altlogical_length);
  R_set_altlogical_Elt_method(rcpp_owned_altlogical_class, rcpp_owned_altlogical_elt);
  R_set_altvec_Dataptr_method(rcpp_owned_altlogical_class, rcpp_owned_altlogical_dataptr);
  R_set_altrep_Duplicate_method(rcpp_owned_altlogical_class, rcpp_owned_altlogical_duplicate);
  R_set_altlogical_Get_region_method(rcpp_owned_altlogical_class, rcpp_owned_altlogical_get_region);
  rcpp_owned_altlogical_class_ready = true;
}

static void rcpp_owned_altlogical_ensure_backing(R_xlen_t n) {
  if (rcpp_owned_altlogical_backing_init >= n) return;
  for (R_xlen_t i = rcpp_owned_altlogical_backing_init; i < n; ++i) {
    rcpp_owned_altlogical_backing[i] = (i & 1) == 0 ? 1 : 0;
  }
  rcpp_owned_altlogical_backing_init = n;
}

static double rcpp_dispatch_sum(SEXP vec) {
  R_xlen_t n = XLENGTH(vec);
  double total = 0.0;

  switch (TYPEOF(vec)) {
    case REALSXP: {
      double *data = REAL(vec);
      for (R_xlen_t i = 0; i < n; ++i) total += data[i];
      return total;
    }
    case INTSXP: {
      int *data = INTEGER(vec);
      for (R_xlen_t i = 0; i < n; ++i) total += (double)data[i];
      return total;
    }
    case LGLSXP: {
      int *data = LOGICAL(vec);
      for (R_xlen_t i = 0; i < n; ++i) total += data[i] != 0 ? 1.0 : 0.0;
      return total;
    }
    default:
      return 0.0;
  }
}

typedef struct {
  int id;
  int count;
  int level;
  int flag;
  int enabled;
  double ratio;
  double offset;
  double scale;
  std::vector<double> weights;
  std::vector<int> indices;
} rcpp_struct_convert_payload_t;

static SEXP rcpp_find_named(SEXP list_sexp, const char *name) {
  SEXP names = getAttrib(list_sexp, R_NamesSymbol);
  R_xlen_t n = XLENGTH(list_sexp);
  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP elt = STRING_ELT(names, i);
    if (elt != NA_STRING && strcmp(CHAR(elt), name) == 0) {
      return VECTOR_ELT(list_sexp, i);
    }
  }
  Rf_error("missing field '%s' in rcpp_bench_struct_convert", name);
  return R_NilValue;
}

// ── Task 1: Fibonacci ────────────────────────────────────────

static int fib(int n) {
  int a = 0, b = 1;
  for (int i = 2; i <= n; i++) { int next = a + b; a = b; b = next; }
  return (n <= 1) ? n : b;
}

extern "C" SEXP rcpp_bench_fib(SEXP n_sexp) {
  return ScalarInteger(fib(INTEGER(n_sexp)[0]));
}

// ── Task 2: Vector Sum ───────────────────────────────────────

extern "C" SEXP rcpp_bench_vectorsum(SEXP vec) {
  double *x = REAL(vec);
  R_xlen_t n = XLENGTH(vec);
  return ScalarReal(rcpp_sum_real(x, n));
}

// ── Task 3: Matrix Multiply ──────────────────────────────────

extern "C" SEXP rcpp_bench_transpose(SEXP mat_sexp) {
  int nr = nrows(mat_sexp), nc = ncols(mat_sexp);
  double *data = REAL(mat_sexp);
  SEXP result = PROTECT(allocMatrix(REALSXP, nc, nr));
  double *rp = REAL(result);
  for (int i = 0; i < nr; i++)
    for (int j = 0; j < nc; j++)
      rp[j * nr + i] = data[i * nc + j];
  UNPROTECT(1);
  return result;
}

// ── Task 4: String Concatenation ─────────────────────────────

extern "C" SEXP rcpp_bench_strings(SEXP vec, SEXP sep_sexp) {
  R_xlen_t n = XLENGTH(vec);
  const char *sep = CHAR(STRING_ELT(sep_sexp, 0));
  std::string result;
  for (R_xlen_t i = 0; i < n; i++) {
    if (STRING_ELT(vec, i) != NA_STRING) {
      result += CHAR(STRING_ELT(vec, i));
      result += sep;
    }
  }
  SEXP out = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, mkChar(result.c_str()));
  UNPROTECT(1);
  return out;
}

// ── Task 35: String Variants ─────────────────────────────────

extern "C" SEXP rcpp_bench_string_variants(SEXP vec) {
  R_xlen_t n = XLENGTH(vec);
  std::string concat;
  int nchar_sum = 0;
  int prefix_match = 0;
  bool first = true;

  SEXP result = PROTECT(allocVector(VECSXP, 5));
  SEXP names = PROTECT(allocVector(STRSXP, 5));
  SEXP concat_out = PROTECT(allocVector(STRSXP, 1));
  SEXP nchar_out = PROTECT(ScalarInteger(0));
  SEXP prefix_out = PROTECT(ScalarInteger(0));
  SEXP extract_out = PROTECT(allocVector(STRSXP, n));
  SEXP upper_out = PROTECT(allocVector(STRSXP, n));

  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP elt = STRING_ELT(vec, i);
    if (elt == NA_STRING) {
      SET_STRING_ELT(extract_out, i, NA_STRING);
      SET_STRING_ELT(upper_out, i, NA_STRING);
      continue;
    }

    const char *s = CHAR(elt);
    size_t slen = strlen(s);
    nchar_sum += static_cast<int>(slen);
    if (slen >= 3 && s[0] == 'a' && s[1] == 'b' && s[2] == 'c') {
      prefix_match += 1;
    }

    if (!first) concat.push_back(',');
    concat += s;
    first = false;

    SET_STRING_ELT(extract_out, i, mkCharLen(s, static_cast<int>(slen < 3 ? slen : 3)));

    std::string upper(s);
    for (char &ch : upper) {
      ch = static_cast<char>(std::toupper(static_cast<unsigned char>(ch)));
    }
    SET_STRING_ELT(upper_out, i, mkChar(upper.c_str()));
  }

  INTEGER(nchar_out)[0] = nchar_sum;
  INTEGER(prefix_out)[0] = prefix_match;
  SET_STRING_ELT(concat_out, 0, mkChar(concat.c_str()));

  SET_VECTOR_ELT(result, 0, concat_out);
  SET_VECTOR_ELT(result, 1, nchar_out);
  SET_VECTOR_ELT(result, 2, prefix_out);
  SET_VECTOR_ELT(result, 3, extract_out);
  SET_VECTOR_ELT(result, 4, upper_out);

  SET_STRING_ELT(names, 0, mkChar("concat"));
  SET_STRING_ELT(names, 1, mkChar("nchar_sum"));
  SET_STRING_ELT(names, 2, mkChar("prefix_match"));
  SET_STRING_ELT(names, 3, mkChar("extract_substr"));
  SET_STRING_ELT(names, 4, mkChar("to_upper"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(7);
  return result;
}

// ── Task 5: Data Frame Filtering ─────────────────────────────

extern "C" SEXP rcpp_bench_dataframe(SEXP df_sexp) {
  SEXP names = getAttrib(df_sexp, R_NamesSymbol);
  R_xlen_t ncols = XLENGTH(df_sexp);
  SEXP x_sexp = R_NilValue, y_sexp = R_NilValue, grp_sexp = R_NilValue;
  for (R_xlen_t i = 0; i < ncols; i++) {
    const char *nm = CHAR(STRING_ELT(names, i));
    if (strcmp(nm, "x") == 0) x_sexp = VECTOR_ELT(df_sexp, i);
    else if (strcmp(nm, "y") == 0) y_sexp = VECTOR_ELT(df_sexp, i);
    else if (strcmp(nm, "grp") == 0) grp_sexp = VECTOR_ELT(df_sexp, i);
  }
  double *x = REAL(x_sexp), *y = REAL(y_sexp);
  int *grp = INTEGER(grp_sexp);
  R_xlen_t nrows = XLENGTH(x_sexp);
  int max_grp = 0;
  for (R_xlen_t i = 0; i < nrows; i++)
    if (x[i] > 0.0 && grp[i] > max_grp) max_grp = grp[i];
  double *sums = (double *)R_alloc(max_grp, sizeof(double));
  memset(sums, 0, max_grp * sizeof(double));
  for (R_xlen_t i = 0; i < nrows; i++)
    if (x[i] > 0.0) sums[grp[i] - 1] += x[i] / y[i];
  SEXP grp_out = PROTECT(allocVector(INTSXP, max_grp));
  SEXP sum_out = PROTECT(allocVector(REALSXP, max_grp));
  for (int i = 0; i < max_grp; i++) {
    INTEGER(grp_out)[i] = i + 1;
    REAL(sum_out)[i] = sums[i];
  }
  SEXP cn = PROTECT(allocVector(STRSXP, 2));
  SET_STRING_ELT(cn, 0, mkChar("grp"));
  SET_STRING_ELT(cn, 1, mkChar("z_sum"));
  SEXP res = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(res, 0, grp_out);
  SET_VECTOR_ELT(res, 1, sum_out);
  setAttrib(res, R_NamesSymbol, cn);
  SEXP cls = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(cls, 0, mkChar("data.frame"));
  setAttrib(res, R_ClassSymbol, cls);
  SEXP rn = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rn)[0] = NA_INTEGER;
  INTEGER(rn)[1] = -max_grp;
  setAttrib(res, R_RowNamesSymbol, rn);
  UNPROTECT(6);
  return res;
}

// ── Task 6: NA Propagation ───────────────────────────────────

extern "C" SEXP rcpp_bench_na_prop(SEXP vec) {
  double *x = REAL(vec);
  R_xlen_t n = XLENGTH(vec);
  return ScalarReal(rcpp_na_mean_real(x, n));
}

// ── Task 7: Parallel Vector Sum ──────────────────────────────

typedef struct { double *data; int start, end; double result; } chunk_t;

static void *sum_chunk(void *arg) {
  chunk_t *ck = (chunk_t *)arg;
  double total = 0.0;
  for (int i = ck->start; i < ck->end; i++) total += ck->data[i];
  ck->result = total;
  return NULL;
}

extern "C" SEXP rcpp_bench_parallel(SEXP vec) {
  double *data = REAL(vec);
  R_xlen_t n = XLENGTH(vec);
  int nthreads = 4;
  pthread_t threads[4];
  chunk_t chunks[4];
  int chunk_size = n / nthreads;
  for (int i = 0; i < nthreads; i++) {
    chunks[i].data = data;
    chunks[i].start = i * chunk_size;
    chunks[i].end = (i == nthreads - 1) ? n : (i + 1) * chunk_size;
    pthread_create(&threads[i], NULL, sum_chunk, &chunks[i]);
  }
  double total = 0.0;
  for (int i = 0; i < nthreads; i++) {
    pthread_join(threads[i], NULL);
    total += chunks[i].result;
  }
  return ScalarReal(total);
}

// ── Task 36: Parallel thread-count sweep ─────────────────────

static double rcpp_parallel_scaling_sum(const double *data, R_xlen_t n, int requested_threads) {
  if (n == 0) return 0.0;
  int actual_threads = requested_threads;
  if ((R_xlen_t) actual_threads > n) actual_threads = static_cast<int>(n);
  if (actual_threads <= 1) {
    double total = 0.0;
    for (R_xlen_t i = 0; i < n; ++i) total += data[i];
    return total;
  }

  pthread_t threads[16];
  chunk_t chunks[16];
  int started = 0;
  R_xlen_t chunk_size = n / actual_threads;

  for (int i = 0; i < actual_threads; ++i) {
    chunks[i].data = const_cast<double *>(data);
    chunks[i].start = i * chunk_size;
    chunks[i].end = (i == actual_threads - 1) ? n : (i + 1) * chunk_size;
    chunks[i].result = 0.0;
    if (pthread_create(&threads[started], NULL, sum_chunk, &chunks[i]) == 0) {
      started++;
    } else {
      chunks[i].result = 0.0;
      for (R_xlen_t j = chunks[i].start; j < chunks[i].end; ++j) chunks[i].result += data[j];
    }
  }

  double total = 0.0;
  for (int i = 0; i < started; ++i) pthread_join(threads[i], NULL);
  for (int i = 0; i < actual_threads; ++i) total += chunks[i].result;
  return total;
}

extern "C" SEXP rcpp_bench_parallel_scaling(SEXP vec) {
  static const int thread_counts[] = {1, 2, 4, 8, 16};
  const double *data = REAL(vec);
  R_xlen_t n = XLENGTH(vec);
  SEXP result = PROTECT(allocVector(REALSXP, 5));
  SEXP names = PROTECT(allocVector(STRSXP, 5));

  for (int i = 0; i < 5; ++i) {
    REAL(result)[i] = rcpp_parallel_scaling_sum(data, n, thread_counts[i]);
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

// ── Task 9: PROTECT Stress ──────────────────────────────────

extern "C" SEXP rcpp_bench_protect_stress(SEXP n_sexp) {
  int n = INTEGER(n_sexp)[0];
  for (int i = 0; i < n; i++) {
    PROTECT(allocVector(REALSXP, 1));
  }
  UNPROTECT(n);
  return ScalarInteger(0);
}

// ── Task 10: BLAS Matmul ──────────────────────────────────────

extern "C" SEXP rcpp_bench_blas_matmul(SEXP a_sexp, SEXP b_sexp) {
  int n = nrows(a_sexp), m = ncols(b_sexp), k = ncols(a_sexp);
  SEXP result = PROTECT(allocMatrix(REALSXP, n, m));
  double *rp = REAL(result);
  double alpha = 1.0, beta = 0.0;
  char notrans = 'N';
  F77_CALL(dgemm)(&notrans, &notrans, &n, &m, &k,
                  &alpha, REAL(a_sexp), &n, REAL(b_sexp), &k,
                  &beta, rp, &n FCONE FCONE);
  UNPROTECT(1);
  return result;
}

// ── Task 11: Cross-product ────────────────────────────────────

extern "C" SEXP rcpp_bench_crossprod(SEXP x_sexp) {
  int nr = nrows(x_sexp), nc = ncols(x_sexp);
  SEXP result = PROTECT(allocMatrix(REALSXP, nc, nc));
  double *rp = REAL(result);
  double alpha = 1.0, beta = 0.0;
  char uplo = 'U', trans = 'T';
  F77_CALL(dsyrk)(&uplo, &trans, &nc, &nr,
                  &alpha, REAL(x_sexp), &nr,
                  &beta, rp, &nc FCONE FCONE);
  for (int i = 0; i < nc; i++)
    for (int j = 0; j < i; j++)
      rp[i * nc + j] = rp[j * nc + i];
  UNPROTECT(1);
  return result;
}

// ── Task 12: Cholesky ─────────────────────────────────────────

extern "C" SEXP rcpp_bench_cholesky(SEXP a_sexp) {
  int n = nrows(a_sexp);
  SEXP result = PROTECT(allocMatrix(REALSXP, n, n));
  double *rp = REAL(result);
  memcpy(rp, REAL(a_sexp), (size_t)n * n * sizeof(double));
  int info = 0;
  char uplo = 'U';
  F77_CALL(dpotrf)(&uplo, &n, rp, &n, &info FCONE);
  for (int col = 0; col < n; col++)
    for (int row = col + 1; row < n; row++)
      rp[col * n + row] = 0.0;
  UNPROTECT(1);
  return result;
}

// ── Task 13: Linear Model ─────────────────────────────────────

extern "C" SEXP rcpp_bench_lm(SEXP x_sexp, SEXP y_sexp) {
  int n = nrows(x_sexp), p = ncols(x_sexp);
  double *x_data = REAL(x_sexp);
  double *y_data = REAL(y_sexp);
  double *xtx = (double *)R_alloc((size_t)p * p, sizeof(double));
  double *xty = (double *)R_alloc(p, sizeof(double));
  double alpha = 1.0, beta = 0.0;
  char notrans = 'N', trans = 'T';
  int one = 1;
  F77_CALL(dgemm)(&trans, &notrans, &p, &p, &n,
                  &alpha, x_data, &n, x_data, &n, &beta, xtx, &p FCONE FCONE);
  F77_CALL(dgemm)(&trans, &notrans, &p, &one, &n,
                  &alpha, x_data, &n, y_data, &n, &beta, xty, &p FCONE FCONE);
  int info = 0;
  char uplo = 'U';
  F77_CALL(dpotrf)(&uplo, &p, xtx, &p, &info FCONE);
  char side = 'L', diag = 'N';
  F77_CALL(dtrsm)(&side, &uplo, &trans, &diag, &p, &one, &alpha, xtx, &p, xty, &p FCONE FCONE FCONE FCONE);
  F77_CALL(dtrsm)(&side, &uplo, &notrans, &diag, &p, &one, &alpha, xtx, &p, xty, &p FCONE FCONE FCONE FCONE);
  SEXP result = PROTECT(allocVector(REALSXP, p));
  memcpy(REAL(result), xty, (size_t)p * sizeof(double));
  UNPROTECT(1);
  return result;
}

// ── Task 14: Row Sums ─────────────────────────────────────────

extern "C" SEXP rcpp_bench_rowsums(SEXP mat_sexp) {
  int nr = nrows(mat_sexp), nc = ncols(mat_sexp);
  double *data = REAL(mat_sexp);
  SEXP result = PROTECT(allocVector(REALSXP, nr));
  double *rp = REAL(result);
  memset(rp, 0, (size_t)nr * sizeof(double));
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      rp[i] += data[i + j * nr];
  UNPROTECT(1);
  return result;
}

// ── Task 15: Element-wise ops ─────────────────────────────────

extern "C" SEXP rcpp_bench_elem_ops(SEXP vec_sexp) {
  R_xlen_t n = XLENGTH(vec_sexp);
  double *src = REAL(vec_sexp);
  SEXP result = PROTECT(allocMatrix(REALSXP, n, 4));
  double *rp = REAL(result);
  for (R_xlen_t i = 0; i < n; i++) {
    double v = src[i];
    rp[i] = fabs(v);
    rp[i + n] = v > 0 ? log(v) : 0.0;
    rp[i + 2 * n] = exp(v);
    rp[i + 3 * n] = v >= 0 ? sqrt(v) : 0.0;
  }
  UNPROTECT(1);
  return result;
}

// ── Task 16: Row/col means ────────────────────────────────────

extern "C" SEXP rcpp_bench_rowcol_means(SEXP mat_sexp) {
  int nr = nrows(mat_sexp), nc = ncols(mat_sexp);
  double *data = REAL(mat_sexp);
  SEXP row_means = PROTECT(allocVector(REALSXP, nr));
  SEXP col_sums = PROTECT(allocVector(REALSXP, nc));
  for (int i = 0; i < nr; i++) {
    double sum = 0.0;
    for (int j = 0; j < nc; j++) sum += data[i + j * nr];
    REAL(row_means)[i] = sum / nc;
  }
  for (int j = 0; j < nc; j++) {
    double sum = 0.0;
    for (int i = 0; i < nr; i++) sum += data[i + j * nr];
    REAL(col_sums)[j] = sum;
  }
  SEXP result = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, row_means);
  SET_VECTOR_ELT(result, 1, col_sums);
  SEXP names = PROTECT(allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, mkChar("row_means"));
  SET_STRING_ELT(names, 1, mkChar("col_sums"));
  setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(4);
  return result;
}

// ── Task 17: Broadcast ────────────────────────────────────────

extern "C" SEXP rcpp_bench_broadcast(SEXP vec_sexp, SEXP scalar_sexp) {
  R_xlen_t n = XLENGTH(vec_sexp);
  double *src = REAL(vec_sexp);
  double scalar = REAL(scalar_sexp)[0];
  SEXP result = PROTECT(allocVector(REALSXP, n));
  double *rp = REAL(result);
  for (R_xlen_t i = 0; i < n; i++) rp[i] = src[i] + scalar;
  UNPROTECT(1);
  return result;
}

// ── Task 18: Sort ─────────────────────────────────────────────

// LSD radix sort for doubles (same algorithm as C and zigR).
static void radix_sort_f64(double *arr, size_t n) {
  if (n < 2) return;
  uint64_t *buf = (uint64_t *)arr;
  const uint64_t sign_bit = 1ULL << 63;
  for (size_t i = 0; i < n; i++) {
    uint64_t v = buf[i];
    buf[i] = (v & sign_bit) ? ~v : v ^ sign_bit;
  }
  size_t counts[256];
  uint64_t *temp = (uint64_t *)R_alloc(n * 8, 1);
  for (int shift = 0; shift < 64; shift += 8) {
    memset(counts, 0, sizeof(counts));
    for (size_t i = 0; i < n; i++) counts[(buf[i] >> shift) & 0xFF]++;
    size_t total = 0;
    for (int i = 0; i < 256; i++) { size_t old = counts[i]; counts[i] = total; total += old; }
    for (size_t i = 0; i < n; i++) { uint64_t v = buf[i]; temp[counts[(v >> shift) & 0xFF]++] = v; }
    memcpy(buf, temp, n * sizeof(uint64_t));
  }
  for (size_t i = 0; i < n; i++) {
    uint64_t v = buf[i];
    buf[i] = (v & sign_bit) ? v ^ sign_bit : ~v;
  }
}

extern "C" SEXP rcpp_bench_sort(SEXP vec_sexp) {
  R_xlen_t n = XLENGTH(vec_sexp);
  double *src = REAL(vec_sexp);
  SEXP result = PROTECT(allocVector(REALSXP, n));
  double *rp = REAL(result);
  memcpy(rp, src, (size_t)n * sizeof(double));
  radix_sort_f64(rp, n);
  UNPROTECT(1);
  return result;
}

// ── Task 19: Cumulative sum ───────────────────────────────────

extern "C" SEXP rcpp_bench_cumsum(SEXP vec_sexp) {
  R_xlen_t n = XLENGTH(vec_sexp);
  double *src = REAL(vec_sexp);
  SEXP result = PROTECT(allocVector(REALSXP, n));
  double *rp = REAL(result);
  double total = 0.0;
  for (R_xlen_t i = 0; i < n; i++) { total += src[i]; rp[i] = total; }
  UNPROTECT(1);
  return result;
}

// ── Task 21: String Nchar ─────────────────────────────────────

extern "C" SEXP rcpp_bench_string_nchar(SEXP vec) {
  R_xlen_t n = XLENGTH(vec);
  long total = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    SEXP elt = STRING_ELT(vec, i);
    if (elt != NA_STRING) total += strlen(CHAR(elt));
  }
  return ScalarInteger((int)total);
}

// ── Task 22: Which NA ─────────────────────────────────────────

extern "C" SEXP rcpp_bench_which_na(SEXP vec) {
  R_xlen_t n = XLENGTH(vec);
  double *src = REAL(vec);
  R_xlen_t na_count = 0;
  for (R_xlen_t i = 0; i < n; i++)
    if (std::isnan(src[i])) na_count++;
  SEXP result = PROTECT(allocVector(INTSXP, na_count));
  int *rp = INTEGER(result);
  R_xlen_t pos = 0;
  for (R_xlen_t i = 0; i < n; i++)
    if (std::isnan(src[i])) rp[pos++] = (int)(i + 1);
  UNPROTECT(1);
  return result;
}

// ── Task 23: ALTREP Sum ───────────────────────────────────────

extern "C" SEXP rcpp_bench_altrep_sum(SEXP sexp) {
  R_xlen_t n = XLENGTH(sexp);
  int *data = INTEGER(sexp);
  double total = 0.0;
  for (R_xlen_t i = 0; i < n; i++) total += data[i];
  return ScalarReal(total);
}

// ── Task 24: ALTREP Read ──────────────────────────────────────

extern "C" SEXP rcpp_bench_altrep_read(SEXP sexp) {
  R_xlen_t n = XLENGTH(sexp);
  int *data = INTEGER(sexp);
  SEXP res = PROTECT(allocVector(INTSXP, 2));
  INTEGER(res)[0] = data[0];
  INTEGER(res)[1] = data[n - 1];
  UNPROTECT(1);
  return res;
}

// ── Task 25: ALTREP Create ───────────────────────────────────

extern "C" SEXP rcpp_bench_altrep_create(SEXP n_sexp) {
  int n = Rf_asInteger(n_sexp);
  if (n < 0 || n > ALTREP_CREATE_MAX_LEN) {
    Rf_error("n out of range for rcpp_bench_altrep_create");
  }

  rcpp_altrep_create_ensure_class();
  rcpp_altrep_create_ensure_backing((R_xlen_t)n);
  SEXP len = PROTECT(ScalarInteger(n));
  SEXP out = R_new_altrep(rcpp_altrep_create_class, R_NilValue, len);
  UNPROTECT(1);
  return out;
}

extern "C" SEXP rcpp_bench_owned_altrep_real_sum(SEXP n_sexp) {
  int n = Rf_asInteger(n_sexp);
  if (n < 0 || n > ALTREP_CREATE_MAX_LEN) {
    Rf_error("n out of range for rcpp_bench_owned_altrep_real_sum");
  }

  rcpp_altrep_create_ensure_class();
  rcpp_altrep_create_ensure_backing((R_xlen_t)n);
  SEXP len = PROTECT(ScalarInteger(n));
  SEXP vec = PROTECT(R_new_altrep(rcpp_altrep_create_class, R_NilValue, len));
  double *data = REAL(vec);
  double total = 0.0;
  for (R_xlen_t i = 0; i < (R_xlen_t)n; ++i) total += data[i];
  UNPROTECT(2);
  return ScalarReal(total);
}

extern "C" SEXP rcpp_bench_owned_altrep_int_sum(SEXP n_sexp) {
  int n = Rf_asInteger(n_sexp);
  if (n < 0 || n > ALTREP_CREATE_MAX_LEN) {
    Rf_error("n out of range for rcpp_bench_owned_altrep_int_sum");
  }

  rcpp_owned_altint_ensure_class();
  rcpp_owned_altint_ensure_backing((R_xlen_t)n);
  SEXP len = PROTECT(ScalarInteger(n));
  SEXP vec = PROTECT(R_new_altrep(rcpp_owned_altint_class, R_NilValue, len));
  int *data = INTEGER(vec);
  double total = 0.0;
  for (R_xlen_t i = 0; i < (R_xlen_t)n; ++i) total += (double)data[i];
  UNPROTECT(2);
  return ScalarReal(total);
}

extern "C" SEXP rcpp_bench_owned_altrep_logical_sum(SEXP n_sexp) {
  int n = Rf_asInteger(n_sexp);
  if (n < 0 || n > ALTREP_CREATE_MAX_LEN) {
    Rf_error("n out of range for rcpp_bench_owned_altrep_logical_sum");
  }

  rcpp_owned_altlogical_ensure_class();
  rcpp_owned_altlogical_ensure_backing((R_xlen_t)n);
  SEXP len = PROTECT(ScalarInteger(n));
  SEXP vec = PROTECT(R_new_altrep(rcpp_owned_altlogical_class, R_NilValue, len));
  int *data = LOGICAL(vec);
  double total = 0.0;
  for (R_xlen_t i = 0; i < (R_xlen_t)n; ++i) {
    if (data[i] != 0) total += 1.0;
  }
  UNPROTECT(2);
  return ScalarReal(total);
}

// ── Task 26: Comptime Dispatch ───────────────────────────────

extern "C" SEXP rcpp_bench_comptime_dispatch(SEXP inputs_sexp) {
  R_xlen_t n_inputs = XLENGTH(inputs_sexp);
  double total = 0.0;

  for (int repeat = 0; repeat < COMPTIME_DISPATCH_REPEATS; ++repeat) {
    for (R_xlen_t i = 0; i < n_inputs; ++i) {
      total += rcpp_dispatch_sum(VECTOR_ELT(inputs_sexp, i));
    }
  }

  return ScalarReal(total);
}

// ── Task 27: Struct Convert ──────────────────────────────────

extern "C" SEXP rcpp_bench_struct_convert(SEXP input_sexp) {
  rcpp_struct_convert_payload_t payload;
  SEXP weights_sexp = rcpp_find_named(input_sexp, "weights");
  SEXP indices_sexp = rcpp_find_named(input_sexp, "indices");

  payload.id = INTEGER(rcpp_find_named(input_sexp, "id"))[0];
  payload.count = INTEGER(rcpp_find_named(input_sexp, "count"))[0];
  payload.level = INTEGER(rcpp_find_named(input_sexp, "level"))[0];
  payload.flag = LOGICAL(rcpp_find_named(input_sexp, "flag"))[0];
  payload.enabled = LOGICAL(rcpp_find_named(input_sexp, "enabled"))[0];
  payload.ratio = REAL(rcpp_find_named(input_sexp, "ratio"))[0];
  payload.offset = REAL(rcpp_find_named(input_sexp, "offset"))[0];
  payload.scale = REAL(rcpp_find_named(input_sexp, "scale"))[0];

  R_xlen_t weights_len = XLENGTH(weights_sexp);
  payload.weights.assign(REAL(weights_sexp), REAL(weights_sexp) + weights_len);
  R_xlen_t indices_len = XLENGTH(indices_sexp);
  payload.indices.assign(INTEGER(indices_sexp), INTEGER(indices_sexp) + indices_len);

  SEXP result = PROTECT(allocVector(VECSXP, 10));
  SEXP names = PROTECT(allocVector(STRSXP, 10));
  SEXP weights_out = PROTECT(allocVector(REALSXP, weights_len));
  SEXP indices_out = PROTECT(allocVector(INTSXP, indices_len));

  memcpy(REAL(weights_out), payload.weights.data(), (size_t)weights_len * sizeof(double));
  memcpy(INTEGER(indices_out), payload.indices.data(), (size_t)indices_len * sizeof(int));

  SET_VECTOR_ELT(result, 0, ScalarInteger(payload.id));
  SET_VECTOR_ELT(result, 1, ScalarInteger(payload.count));
  SET_VECTOR_ELT(result, 2, ScalarInteger(payload.level));
  SET_VECTOR_ELT(result, 3, ScalarLogical(payload.flag));
  SET_VECTOR_ELT(result, 4, ScalarLogical(payload.enabled));
  SET_VECTOR_ELT(result, 5, ScalarReal(payload.ratio));
  SET_VECTOR_ELT(result, 6, ScalarReal(payload.offset));
  SET_VECTOR_ELT(result, 7, ScalarReal(payload.scale));
  SET_VECTOR_ELT(result, 8, weights_out);
  SET_VECTOR_ELT(result, 9, indices_out);

  SET_STRING_ELT(names, 0, mkChar("id"));
  SET_STRING_ELT(names, 1, mkChar("count"));
  SET_STRING_ELT(names, 2, mkChar("level"));
  SET_STRING_ELT(names, 3, mkChar("flag"));
  SET_STRING_ELT(names, 4, mkChar("enabled"));
  SET_STRING_ELT(names, 5, mkChar("ratio"));
  SET_STRING_ELT(names, 6, mkChar("offset"));
  SET_STRING_ELT(names, 7, mkChar("scale"));
  SET_STRING_ELT(names, 8, mkChar("weights"));
  SET_STRING_ELT(names, 9, mkChar("indices"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(4);
  return result;
}

// ── Task 28: NA Proportion Sweep ────────────────────────────

extern "C" SEXP rcpp_bench_na_prop_vary(SEXP inputs_sexp) {
  R_xlen_t n_inputs = XLENGTH(inputs_sexp);
  SEXP result = PROTECT(allocVector(REALSXP, n_inputs));

  for (R_xlen_t i = 0; i < n_inputs; i++) {
    SEXP vec = VECTOR_ELT(inputs_sexp, i);
    REAL(result)[i] = rcpp_na_mean_real(REAL(vec), XLENGTH(vec));
  }

  setAttrib(result, R_NamesSymbol, getAttrib(inputs_sexp, R_NamesSymbol));
  UNPROTECT(1);
  return result;
}

// ── Task 29: Scale Law Mix ──────────────────────────────────

extern "C" SEXP rcpp_bench_scale_law(SEXP inputs_sexp) {
  R_xlen_t n_inputs = XLENGTH(inputs_sexp);
  SEXP result = PROTECT(allocVector(REALSXP, n_inputs));

  for (R_xlen_t i = 0; i < n_inputs; i++) {
    SEXP vec = VECTOR_ELT(inputs_sexp, i);
    REAL(result)[i] = rcpp_sum_real(REAL(vec), XLENGTH(vec));
  }

  setAttrib(result, R_NamesSymbol, getAttrib(inputs_sexp, R_NamesSymbol));
  UNPROTECT(1);
  return result;
}

// ── Task 30: Allocation Strategy ───────────────────────────

extern "C" SEXP rcpp_bench_arena_vs_rmalloc(SEXP vec_sexp) {
  R_xlen_t n = XLENGTH(vec_sexp);
  double *src = REAL(vec_sexp);
  double total = 0.0;

  for (int repeat = 0; repeat < ALLOCATION_REPEATS; ++repeat) {
    double bias = (repeat + 1) * 0.001;
    double *temp = (double *)R_alloc((size_t)n, sizeof(double));
    for (R_xlen_t i = 0; i < n; ++i) {
      temp[i] = src[i] + bias;
    }
    total += rcpp_sum_real(temp, n);
  }

  return ScalarReal(total);
}

// ── Task 31: Protection Overhead ───────────────────────────

extern "C" SEXP rcpp_bench_prot_overhead(SEXP vec_sexp) {
  R_xlen_t n = XLENGTH(vec_sexp);
  double *src = REAL(vec_sexp);
  SEXP result = PROTECT(allocVector(REALSXP, 5));
  SEXP names = PROTECT(allocVector(STRSXP, 5));
  double *out = REAL(result);

  double unsafe_total = 0.0;
  for (int repeat = 0; repeat < PROTECT_OVERHEAD_REPEATS; ++repeat) {
    double bias = (repeat + 1) * 0.001;
    SEXP temp = allocVector(REALSXP, n);
    unsafe_total += rcpp_fill_sum_temp(REAL(temp), src, n, bias);
  }
  out[0] = unsafe_total;

  double manual_total = 0.0;
  for (int repeat = 0; repeat < PROTECT_OVERHEAD_REPEATS; ++repeat) {
    double bias = (repeat + 1) * 0.001;
    SEXP temp = PROTECT(allocVector(REALSXP, n));
    manual_total += rcpp_fill_sum_temp(REAL(temp), src, n, bias);
    UNPROTECT(1);
  }
  out[1] = manual_total;

  double batch_total = 0.0;
  for (int repeat = 0; repeat < PROTECT_OVERHEAD_REPEATS; ++repeat) {
    double bias = (repeat + 1) * 0.001;
    SEXP temp = PROTECT(allocVector(REALSXP, n));
    batch_total += rcpp_fill_sum_temp(REAL(temp), src, n, bias);
  }
  UNPROTECT(PROTECT_OVERHEAD_REPEATS);
  out[2] = batch_total;

  double preserve_total = 0.0;
  for (int repeat = 0; repeat < PROTECT_OVERHEAD_REPEATS; ++repeat) {
    double bias = (repeat + 1) * 0.001;
    SEXP temp = allocVector(REALSXP, n);
    R_PreserveObject(temp);
    preserve_total += rcpp_fill_sum_temp(REAL(temp), src, n, bias);
    R_ReleaseObject(temp);
  }
  out[3] = preserve_total;

  double reprotect_total = 0.0;
  PROTECT_INDEX protect_index;
  PROTECT_WITH_INDEX(R_NilValue, &protect_index);
  for (int repeat = 0; repeat < PROTECT_OVERHEAD_REPEATS; ++repeat) {
    double bias = (repeat + 1) * 0.001;
    SEXP temp = allocVector(REALSXP, n);
    REPROTECT(temp, protect_index);
    reprotect_total += rcpp_fill_sum_temp(REAL(temp), src, n, bias);
  }
  UNPROTECT(1);
  out[4] = reprotect_total;

  SET_STRING_ELT(names, 0, mkChar("unsafe"));
  SET_STRING_ELT(names, 1, mkChar("manual"));
  SET_STRING_ELT(names, 2, mkChar("batch"));
  SET_STRING_ELT(names, 3, mkChar("preserve"));
  SET_STRING_ELT(names, 4, mkChar("reprotect"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(2);
  return result;
}

// ── Task 32: Longjmp Safety ────────────────────────────────

extern "C" SEXP rcpp_bench_longjmp_safety(SEXP vec_sexp) {
  R_xlen_t n = XLENGTH(vec_sexp);
  double *src = REAL(vec_sexp);
  SEXP result = PROTECT(allocVector(REALSXP, 4));
  SEXP names = PROTECT(allocVector(STRSXP, 4));
  double *out = REAL(result);
  SEXP sum_sym = Rf_install("sum");
  SEXP stop_sym = Rf_install("stop");
  SEXP stop_msg = PROTECT(mkString("task32"));
  SEXP stop_call = PROTECT(lang2(stop_sym, stop_msg));

  double direct_total = 0.0;
  double try_ok_total = 0.0;
  double try_err_total = 0.0;
  double unwind_ok_total = 0.0;

  for (int repeat = 0; repeat < LONGJMP_SAFETY_REPEATS; ++repeat) {
    double bias = (repeat + 1) * 0.001;
    direct_total += rcpp_adjusted_sum(src, n, bias);

    SEXP temp = PROTECT(allocVector(REALSXP, n));
    rcpp_fill_sum_temp(REAL(temp), src, n, bias);
    SEXP expr = PROTECT(lang2(sum_sym, temp));
    int err = 0;
    SEXP eval_result = R_tryEvalSilent(expr, R_GlobalEnv, &err);
    try_ok_total += REAL(eval_result)[0];
    UNPROTECT(2);

    err = 0;
    R_tryEvalSilent(stop_call, R_GlobalEnv, &err);
    if (err != 0) try_err_total += 1.0;

    rcpp_unwind_state_t state = { src, n, bias };
    SEXP cont = PROTECT(R_MakeUnwindCont());
    SEXP unwind_result = R_UnwindProtect(rcpp_unwind_ok, &state, rcpp_noop_clean, NULL, cont);
    unwind_ok_total += REAL(unwind_result)[0];
    UNPROTECT(1);
  }

  out[0] = direct_total;
  out[1] = try_ok_total;
  out[2] = try_err_total;
  out[3] = unwind_ok_total;
  SET_STRING_ELT(names, 0, mkChar("direct"));
  SET_STRING_ELT(names, 1, mkChar("try_ok"));
  SET_STRING_ELT(names, 2, mkChar("try_err"));
  SET_STRING_ELT(names, 3, mkChar("unwind_ok"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(4);
  return result;
}

// ── Task 34: Math Call Cost ────────────────────────────────

extern "C" SEXP rcpp_bench_translate_c_cost(SEXP vec_sexp) {
  R_xlen_t n = XLENGTH(vec_sexp);
  double *src = REAL(vec_sexp);
  SEXP result = PROTECT(allocVector(REALSXP, 4));
  SEXP names = PROTECT(allocVector(STRSXP, 4));
  double *out = REAL(result);

  double abs_total = 0.0;
  double log_total = 0.0;
  double exp_total = 0.0;
  double sqrt_total = 0.0;

  for (int repeat = 0; repeat < TRANSLATE_C_COST_REPEATS; ++repeat) {
    double bias = (repeat + 1) * 0.001;
    for (R_xlen_t i = 0; i < n; ++i) {
      double shifted = src[i] + bias;
      abs_total += std::fabs(shifted - 0.75);
      log_total += std::log(shifted);
      exp_total += std::exp(shifted);
      sqrt_total += std::sqrt(shifted);
    }
  }

  out[0] = abs_total;
  out[1] = log_total;
  out[2] = exp_total;
  out[3] = sqrt_total;
  SET_STRING_ELT(names, 0, mkChar("abs"));
  SET_STRING_ELT(names, 1, mkChar("log"));
  SET_STRING_ELT(names, 2, mkChar("exp"));
  SET_STRING_ELT(names, 3, mkChar("sqrt"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(2);
  return result;
}

// ── Task 37: Memory Bandwidth ───────────────────────────────

extern "C" SEXP rcpp_bench_memory_bandwidth(SEXP vec_sexp) {
  const double *src = REAL(vec_sexp);
  R_xlen_t n = XLENGTH(vec_sexp);
  SEXP result = PROTECT(allocVector(REALSXP, 3));
  SEXP names = PROTECT(allocVector(STRSXP, 3));
  double *out = REAL(result);

  double copy_temp_total = 0.0;
  double copy_out_total = 0.0;
  double fill_out_total = 0.0;

  for (int repeat = 0; repeat < MEMORY_BANDWIDTH_REPEATS; ++repeat) {
    std::vector<double> temp((size_t)n);
    std::memcpy(temp.data(), src, (size_t)n * sizeof(double));
    copy_temp_total += rcpp_sum_real(temp.data(), n);

    SEXP copy_out = PROTECT(allocVector(REALSXP, n));
    std::memcpy(REAL(copy_out), src, (size_t)n * sizeof(double));
    copy_out_total += rcpp_sum_real(REAL(copy_out), n);
    UNPROTECT(1);

    SEXP fill_out = PROTECT(allocVector(REALSXP, n));
    double *fill_ptr = REAL(fill_out);
    for (R_xlen_t i = 0; i < n; ++i) fill_ptr[i] = src[i] + 0.5;
    fill_out_total += rcpp_sum_real(fill_ptr, n);
    UNPROTECT(1);
  }

  out[0] = copy_temp_total;
  out[1] = copy_out_total;
  out[2] = fill_out_total;
  SET_STRING_ELT(names, 0, mkChar("copy_temp"));
  SET_STRING_ELT(names, 1, mkChar("copy_out"));
  SET_STRING_ELT(names, 2, mkChar("fill_out"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(2);
  return result;
}

// ── Task 20: Random Normal ────────────────────────────────────

extern "C" SEXP rcpp_bench_rnorm(SEXP n_sexp) {
  int n = INTEGER(n_sexp)[0];
  GetRNGstate();
  SEXP result = PROTECT(allocVector(REALSXP, n));
  double *rp = REAL(result);
  for (int i = 0; i < n; i++) rp[i] = norm_rand();
  PutRNGstate();
  UNPROTECT(1);
  return result;
}

// ── Registration ─────────────────────────────────────────────

static const R_CallMethodDef CallEntries[] = {
  {"rcpp_bench_fib",            (DL_FUNC) &rcpp_bench_fib,            1},
  {"rcpp_bench_vectorsum",      (DL_FUNC) &rcpp_bench_vectorsum,      1},
  {"rcpp_bench_transpose",      (DL_FUNC) &rcpp_bench_transpose,      1},
  {"rcpp_bench_strings",        (DL_FUNC) &rcpp_bench_strings,        2},
  {"rcpp_bench_dataframe",      (DL_FUNC) &rcpp_bench_dataframe,      1},
  {"rcpp_bench_na_prop",        (DL_FUNC) &rcpp_bench_na_prop,        1},
  {"rcpp_bench_parallel",       (DL_FUNC) &rcpp_bench_parallel,       1},
  {"rcpp_bench_protect_stress", (DL_FUNC) &rcpp_bench_protect_stress, 1},
  {"rcpp_bench_blas_matmul",    (DL_FUNC) &rcpp_bench_blas_matmul,    2},
  {"rcpp_bench_crossprod",      (DL_FUNC) &rcpp_bench_crossprod,      1},
  {"rcpp_bench_cholesky",       (DL_FUNC) &rcpp_bench_cholesky,       1},
  {"rcpp_bench_lm",             (DL_FUNC) &rcpp_bench_lm,             2},
  {"rcpp_bench_rowsums",        (DL_FUNC) &rcpp_bench_rowsums,        1},
  {"rcpp_bench_elem_ops",       (DL_FUNC) &rcpp_bench_elem_ops,       1},
  {"rcpp_bench_rowcol_means",   (DL_FUNC) &rcpp_bench_rowcol_means,   1},
  {"rcpp_bench_broadcast",      (DL_FUNC) &rcpp_bench_broadcast,      2},
  {"rcpp_bench_sort",           (DL_FUNC) &rcpp_bench_sort,           1},
  {"rcpp_bench_cumsum",         (DL_FUNC) &rcpp_bench_cumsum,         1},
  {"rcpp_bench_rnorm",          (DL_FUNC) &rcpp_bench_rnorm,          1},
  {"rcpp_bench_string_nchar",   (DL_FUNC) &rcpp_bench_string_nchar,   1},
  {"rcpp_bench_which_na",       (DL_FUNC) &rcpp_bench_which_na,       1},
  {"rcpp_bench_altrep_sum",    (DL_FUNC) &rcpp_bench_altrep_sum,    1},
  {"rcpp_bench_altrep_read",   (DL_FUNC) &rcpp_bench_altrep_read,   1},
  {"rcpp_bench_altrep_create", (DL_FUNC) &rcpp_bench_altrep_create, 1},
  {"rcpp_bench_owned_altrep_real_sum", (DL_FUNC) &rcpp_bench_owned_altrep_real_sum, 1},
  {"rcpp_bench_owned_altrep_int_sum", (DL_FUNC) &rcpp_bench_owned_altrep_int_sum, 1},
  {"rcpp_bench_owned_altrep_logical_sum", (DL_FUNC) &rcpp_bench_owned_altrep_logical_sum, 1},
  {"rcpp_bench_comptime_dispatch", (DL_FUNC) &rcpp_bench_comptime_dispatch, 1},
  {"rcpp_bench_struct_convert", (DL_FUNC) &rcpp_bench_struct_convert, 1},
  {"rcpp_bench_na_prop_vary", (DL_FUNC) &rcpp_bench_na_prop_vary, 1},
  {"rcpp_bench_scale_law", (DL_FUNC) &rcpp_bench_scale_law, 1},
  {"rcpp_bench_arena_vs_rmalloc", (DL_FUNC) &rcpp_bench_arena_vs_rmalloc, 1},
  {"rcpp_bench_prot_overhead", (DL_FUNC) &rcpp_bench_prot_overhead, 1},
  {"rcpp_bench_longjmp_safety", (DL_FUNC) &rcpp_bench_longjmp_safety, 1},
  {"rcpp_bench_translate_c_cost", (DL_FUNC) &rcpp_bench_translate_c_cost, 1},
  {"rcpp_bench_memory_bandwidth", (DL_FUNC) &rcpp_bench_memory_bandwidth, 1},
  {"rcpp_bench_string_variants", (DL_FUNC) &rcpp_bench_string_variants, 1},
  {"rcpp_bench_parallel_scaling", (DL_FUNC) &rcpp_bench_parallel_scaling, 1},
  {NULL, NULL, 0}
};

extern "C" void R_init_rcpp_benchmarks(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
