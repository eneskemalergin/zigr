/* Historical task kernels, grouped in one translation unit to keep the control runner auditable. */

/* Task 01_vectorsum */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_vectorsum(SEXP arg) {
    double *xp = REAL(arg);
    R_xlen_t n = XLENGTH(arg);
    double total = 0.0;
    for (R_xlen_t i = 0; i < n; i++) total += xp[i];
    return Rf_ScalarReal(total);
}

/* Task 02_elem_ops */
#include <R.h>
#include <Rinternals.h>
#include <math.h>

SEXP c_call_bench_elem_ops(SEXP arg) {
    double *xp = REAL(arg);
    R_xlen_t n = XLENGTH(arg);

    SEXP result = PROTECT(Rf_allocMatrix(REALSXP, n, 4));
    double *rp = REAL(result);

    for (R_xlen_t i = 0; i < n; i++) {
        double v = xp[i];
        rp[i] = fabs(v);
        rp[i + n] = v > 0 ? log(v) : 0.0;
        rp[i + 2 * n] = exp(v);
        rp[i + 3 * n] = v >= 0 ? sqrt(v) : 0.0;
    }

    UNPROTECT(1);
    return result;
}

/* Task 03_memcpy_bandwidth */
#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <stdlib.h>

#define REPEATS 2
#define N_STRATS 3

static double sum_slice(double *data, R_xlen_t n) {
  double total = 0.0;
  for (R_xlen_t i = 0; i < n; i++) total += data[i];
  return total;
}

SEXP c_call_bench_memcpy_bandwidth(SEXP arg) {
  double *xp = REAL(arg);
  R_xlen_t n = XLENGTH(arg);

  const char *names[] = {"copy_temp", "copy_out", "fill_out"};
  SEXP result = PROTECT(Rf_allocVector(REALSXP, N_STRATS));
  SEXP rnames = PROTECT(Rf_allocVector(STRSXP, N_STRATS));
  for (int i = 0; i < N_STRATS; i++)
    SET_STRING_ELT(rnames, i, Rf_mkChar(names[i]));
  Rf_setAttrib(result, R_NamesSymbol, rnames);
  double *rp = REAL(result);

  double copy_temp_total = 0.0;
  double copy_out_total = 0.0;
  double fill_out_total = 0.0;

  for (int rep = 0; rep < REPEATS; rep++) {
    double *temp = malloc(n * sizeof(double));
    memcpy(temp, xp, n * sizeof(double));
    copy_temp_total += sum_slice(temp, n);
    free(temp);

    SEXP copy_out = PROTECT(Rf_allocVector(REALSXP, n));
    double *copy_rp = REAL(copy_out);
    memcpy(copy_rp, xp, n * sizeof(double));
    copy_out_total += sum_slice(copy_rp, n);
    UNPROTECT(1);

    SEXP fill_out = PROTECT(Rf_allocVector(REALSXP, n));
    double *fill_rp = REAL(fill_out);
    for (R_xlen_t i = 0; i < n; i++) fill_rp[i] = xp[i] + 0.5;
    fill_out_total += sum_slice(fill_rp, n);
    UNPROTECT(1);
  }

  rp[0] = copy_temp_total;
  rp[1] = copy_out_total;
  rp[2] = fill_out_total;
  UNPROTECT(2);
  return result;
}

#undef REPEATS

/* Task 04_sort */
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

/* Task 05_fib_recursive */
#include <R.h>
#include <Rinternals.h>

static long long fib(long long n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

SEXP c_call_bench_fib_recursive(SEXP arg) {
    long long n = (long long)Rf_asInteger(arg);
    long long result = fib(n);
    return Rf_ScalarReal((double)result);
}

/* Task 06_broadcast */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_broadcast(SEXP vec, SEXP scalar_sexp) {
    double *xp = REAL(vec);
    R_xlen_t n = XLENGTH(vec);
    double scalar = REAL(scalar_sexp)[0];
    double total = 0.0;
    for (R_xlen_t i = 0; i < n; i++) total += xp[i] + scalar;
    return Rf_ScalarReal(total);
}

/* Task 07a_protect_shallow */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_protect_shallow(SEXP arg) {
    for (int r = 0; r < 100; r++) {
        for (int i = 0; i < 10; i++) PROTECT(arg);
        UNPROTECT(10);
    }
    return Rf_ScalarInteger(0);
}

/* Task 07b_protect_scaling */
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

/* Task 08_type_dispatch */
#include <R.h>
#include <Rinternals.h>

static int classify_type(SEXP x) {
    switch (TYPEOF(x)) {
        case REALSXP: return 1;
        case INTSXP: return 2;
        case STRSXP: return 3;
        default: return 0;
    }
}

SEXP c_call_bench_type_dispatch(SEXP arg) {
    SEXP elts[3] = {
        VECTOR_ELT(arg, 0),
        VECTOR_ELT(arg, 1),
        VECTOR_ELT(arg, 2)
    };
    int total = 0;
    for (int i = 0; i < 2048; i++) {
        total += classify_type(elts[0]);
        total += classify_type(elts[1]);
        total += classify_type(elts[2]);
    }
    return Rf_ScalarInteger(total);
}

/* Task 09_longjmp_safety */
#include <R.h>
#include <Rinternals.h>

#define REPEATS 512

static double adjusted_sum(const double *x, int n, double bias) {
    double total = 0.0;
    for (int i = 0; i < n; i++) total += x[i] + bias;
    return total;
}

static void fill_adjusted(double *out, const double *x, int n, double bias) {
    for (int i = 0; i < n; i++) out[i] = x[i] + bias;
}

typedef struct {
    const double *x;
    int n;
    double bias;
} unwind_data_t;

static SEXP unwind_callback(void *data) {
    unwind_data_t *ud = (unwind_data_t *)data;
    return Rf_ScalarReal(adjusted_sum(ud->x, ud->n, ud->bias));
}

static void unwind_noop(void *data, Rboolean jump) {}

SEXP c_call_bench_longjmp_safety(SEXP arg) {
    int n = LENGTH(arg);
    const double *x = REAL(arg);

    double direct_total = 0.0, try_ok_total = 0.0;
    double try_err_total = 0.0, unwind_ok_total = 0.0;

    SEXP sum_sym = Rf_install("sum");
    SEXP stop_sym = Rf_install("stop");

    for (int rep = 0; rep < REPEATS; rep++) {
        double bias = (rep + 1.0) * 0.001;
        direct_total += adjusted_sum(x, n, bias);

        SEXP tmp = PROTECT(Rf_allocVector(REALSXP, n));
        fill_adjusted(REAL(tmp), x, n, bias);
        SEXP expr = PROTECT(Rf_lang2(sum_sym, tmp));
        int err = 0;
        SEXP res = R_tryEvalSilent(expr, R_GlobalEnv, &err);
        if (err == 0) try_ok_total += REAL(res)[0];
        UNPROTECT(2);

        err = 0;
        SEXP stop_call = PROTECT(Rf_lang2(stop_sym, Rf_mkString("task32")));
        R_tryEvalSilent(stop_call, R_GlobalEnv, &err);
        UNPROTECT(1);
        if (err != 0) try_err_total += 1.0;

        unwind_data_t ud = { .x = x, .n = n, .bias = bias };
        SEXP cont = PROTECT(R_MakeUnwindCont());
        SEXP ures = R_UnwindProtect(unwind_callback, &ud, unwind_noop, NULL, cont);
        unwind_ok_total += REAL(ures)[0];
        UNPROTECT(1);
    }

    const char *names[] = {"direct", "try_ok", "try_err", "unwind_ok"};
    int nstrat = 4;
    SEXP out = PROTECT(Rf_allocVector(REALSXP, nstrat));
    SEXP nms = PROTECT(Rf_allocVector(STRSXP, nstrat));
    double *outd = REAL(out);
    outd[0] = direct_total;
    outd[1] = try_ok_total;
    outd[2] = try_err_total;
    outd[3] = unwind_ok_total;
    for (int i = 0; i < nstrat; i++)
        SET_STRING_ELT(nms, i, Rf_mkChar(names[i]));
    Rf_setAttrib(out, R_NamesSymbol, nms);
    UNPROTECT(2);
    return out;
}

/* Task 10_sexp_create */
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
    return Rf_ScalarInteger(0);
}

/* Task 11_sexp_inspect */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_sexp_inspect(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    SEXP elts[5];
    for (R_xlen_t i = 0; i < n; i++) elts[i] = VECTOR_ELT(arg, i);
    int total = 0;
    for (int iter = 0; iter < 10000; iter++) {
        for (R_xlen_t i = 0; i < n; i++) {
            total += TYPEOF(elts[i]);
            total += Rf_isVector(elts[i]);
            total += Rf_isReal(elts[i]);
        }
    }
    return Rf_ScalarInteger(total);
}

/* Task 12_matrix_transpose */
#include <R.h>
#include <Rinternals.h>
#include <string.h>

#define BLOCK 32

SEXP c_call_bench_matrix_transpose(SEXP arg) {
    int nr = Rf_nrows(arg);
    int nc = Rf_ncols(arg);
    double *xp = REAL(arg);

    SEXP result = PROTECT(Rf_allocMatrix(REALSXP, nc, nr));
    double *rp = REAL(result);

    for (int jj = 0; jj < nc; jj += BLOCK) {
        int j_end = jj + BLOCK < nc ? jj + BLOCK : nc;
        for (int ii = 0; ii < nr; ii += BLOCK) {
            int i_end = ii + BLOCK < nr ? ii + BLOCK : nr;
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

/* Task 13_matrix_rowsums */
#include <R.h>
#include <Rinternals.h>
#include <string.h>

SEXP c_call_bench_matrix_rowsums(SEXP arg) {
    int nr = Rf_nrows(arg);
    int nc = Rf_ncols(arg);
    double *xp = REAL(arg);

    SEXP result = PROTECT(Rf_allocVector(REALSXP, nr));
    double *rp = REAL(result);
    memset(rp, 0, nr * sizeof(double));

    for (int j = 0; j < nc; j++) {
        double *col = xp + j * nr;
        for (int i = 0; i < nr; i++) {
            rp[i] += col[i];
        }
    }

    UNPROTECT(1);
    return result;
}

/* Task 14_matrix_rowcol_means */
#include <R.h>
#include <Rinternals.h>
#include <string.h>

SEXP c_call_bench_matrix_rowcol_means(SEXP arg) {
    int nr = Rf_nrows(arg);
    int nc = Rf_ncols(arg);
    double *xp = REAL(arg);

    SEXP row_means = PROTECT(Rf_allocVector(REALSXP, nr));
    SEXP col_sums = PROTECT(Rf_allocVector(REALSXP, nc));
    double *rm = REAL(row_means);
    double *cs = REAL(col_sums);
    memset(rm, 0, nr * sizeof(double));

    for (int j = 0; j < nc; j++) {
        double *col = xp + j * nr;
        double sum = 0.0;
        for (int i = 0; i < nr; i++) {
            double v = col[i];
            rm[i] += v;
            sum += v;
        }
        cs[j] = sum;
    }

    double inv_nc = 1.0 / nc;
    for (int i = 0; i < nr; i++) rm[i] *= inv_nc;

    SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(result, 0, row_means);
    SET_VECTOR_ELT(result, 1, col_sums);
    UNPROTECT(3);
    return result;
}

/* Task 15_dataframe_filter */
#include <R.h>
#include <Rinternals.h>
#include <string.h>

SEXP c_call_bench_dataframe_filter(SEXP arg) {
    SEXP names = Rf_getAttrib(arg, R_NamesSymbol);
    R_xlen_t ncols = XLENGTH(arg);
    SEXP x_col = R_NilValue, y_col = R_NilValue, grp_col = R_NilValue;

    for (R_xlen_t i = 0; i < ncols; i++) {
        SEXP nm = STRING_ELT(names, i);
        if (nm == R_NaString) continue;
        const char *s = CHAR(nm);
        if (strcmp(s, "x") == 0) x_col = VECTOR_ELT(arg, i);
        else if (strcmp(s, "y") == 0) y_col = VECTOR_ELT(arg, i);
        else if (strcmp(s, "grp") == 0) grp_col = VECTOR_ELT(arg, i);
    }

    R_xlen_t nrows = XLENGTH(x_col);
    double *xp = REAL(x_col);
    double *yp = REAL(y_col);
    int *gp = INTEGER(grp_col);

    int max_grp = 0;
    for (R_xlen_t i = 0; i < nrows; i++) {
        if (xp[i] > 0.0 && gp[i] > max_grp) max_grp = gp[i];
    }

    int ngroups = max_grp;
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
    SEXP grp_out = PROTECT(Rf_allocVector(INTSXP, ngroups));
    SEXP sum_out = PROTECT(Rf_allocVector(REALSXP, ngroups));
    
    int *gop = INTEGER(grp_out);
    double *sop = REAL(sum_out);
    memset(sop, 0, ngroups * sizeof(double));
    for (int i = 0; i < ngroups; i++) gop[i] = i + 1;

    for (R_xlen_t i = 0; i < nrows; i++) {
        if (xp[i] > 0.0) {
            int g = gp[i] - 1;
            if (g >= 0 && g < ngroups) sop[g] += xp[i] / yp[i];
        }
    }

    SET_VECTOR_ELT(result, 0, grp_out);
    SET_VECTOR_ELT(result, 1, sum_out);
    
    SEXP rn = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(rn, 0, Rf_mkChar("grp"));
    SET_STRING_ELT(rn, 1, Rf_mkChar("z_sum"));
    Rf_setAttrib(result, R_NamesSymbol, rn);
    
    SEXP cls = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(cls, 0, Rf_mkChar("data.frame"));
    Rf_setAttrib(result, R_ClassSymbol, cls);
    
    SEXP rns = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(rns)[0] = NA_INTEGER;
    INTEGER(rns)[1] = -ngroups;
    Rf_setAttrib(result, R_RowNamesSymbol, rns);
    
    UNPROTECT(6);
    return result;
}

/* Task 16_list_access */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_list_access(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    double total = 0.0;
    for (R_xlen_t i = 0; i < n; i++) {
        total += REAL(VECTOR_ELT(arg, i))[0];
    }
    return Rf_ScalarReal(total);
}

/* Task 17_string_concat */
#include <R.h>
#include <Rinternals.h>
#include <string.h>

static const char *string_bytes(SEXP value) {
    return getCharCE(value) == CE_BYTES ? CHAR(value) : Rf_translateCharUTF8(value);
}

SEXP c_call_bench_string_concat(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    R_xlen_t total = 0;
    int output_encoding = CE_UTF8;
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP elt = STRING_ELT(arg, i);
        if (elt == NA_STRING) {
            total += 2;
        } else {
            if (getCharCE(elt) == CE_BYTES) output_encoding = CE_BYTES;
            total += (R_xlen_t) strlen(string_bytes(elt));
        }
    }
    if (n > 1) total += (n - 1) * 2;

    char *buf = (char *)R_alloc(total, 1);
    char *pos = buf;
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP elt = STRING_ELT(arg, i);
        if (elt == NA_STRING) {
            memcpy(pos, "NA", 2); pos += 2;
        } else {
            const char *bytes = string_bytes(elt);
            R_xlen_t len = (R_xlen_t) strlen(bytes);
            memcpy(pos, bytes, len);
            pos += len;
        }
        if (i + 1 < n) { memcpy(pos, ", ", 2); pos += 2; }
    }

    SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
    SEXP cs = Rf_mkCharLenCE(buf, pos - buf, (cetype_t) output_encoding);
    SET_STRING_ELT(out, 0, cs);
    UNPROTECT(1);
    return out;
}

/* Task 18_string_nchar */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_string_nchar(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    long long total = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP elt = STRING_ELT(arg, i);
        if (elt == NA_STRING) continue;
        total += LENGTH(elt);
    }
    return Rf_ScalarInteger(total);
}

/* Task 19_string_encoding */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_string_encoding(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    int total = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        total += getCharCE(STRING_ELT(arg, i)) == CE_UTF8;
    }
    return Rf_ScalarInteger(total);
}

/* Task 20_factor_ops */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_factor_ops(SEXP arg) {
    SEXP factor_sym = Rf_install("factor");
    SEXP call = PROTECT(Rf_lang2(factor_sym, arg));
    int err = 0;
    SEXP factor = PROTECT(R_tryEvalSilent(call, R_GlobalEnv, &err));
    if (err != 0) {
        UNPROTECT(2);
        return Rf_ScalarInteger(NA_INTEGER);
    }

    R_xlen_t n = XLENGTH(factor);
    int *codes = INTEGER(factor);
    long long total = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        int c = codes[i];
        if (c != NA_INTEGER) total += c;
    }

    UNPROTECT(2);
    return Rf_ScalarInteger((int)total);
}

/* Task 21_attrib_ops */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_attrib_ops(SEXP arg) {
    SEXP cls_val = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(cls_val, 0, Rf_mkChar("bench_class"));
    Rf_classgets(arg, cls_val);
    UNPROTECT(1);

    SEXP cr_val = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(cr_val, 0, Rf_mkChar("zigr_bench"));
    Rf_setAttrib(arg, Rf_install("creator"), cr_val);
    UNPROTECT(1);

    SEXP got_cls = Rf_getAttrib(arg, R_ClassSymbol);
    SEXP got_cr = Rf_getAttrib(arg, Rf_install("creator"));

    int total = 0;
    R_xlen_t nc = XLENGTH(got_cls);
    for (R_xlen_t i = 0; i < nc; i++)
        total += LENGTH(STRING_ELT(got_cls, i));
    R_xlen_t ncr = XLENGTH(got_cr);
    for (R_xlen_t i = 0; i < ncr; i++)
        total += LENGTH(STRING_ELT(got_cr, i));

    return Rf_ScalarInteger(total);
}

/* Task 22_s4_slot_access */
#include <R.h>
#include <Rinternals.h>

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

/* Task 23_na_propagation */
#include <R.h>
#include <Rinternals.h>
#include <math.h>

SEXP c_call_bench_na_propagation(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    double *xp = REAL(arg);
    double total = 0.0;
    R_xlen_t count = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        if (!isnan(xp[i])) { total += xp[i]; count++; }
    }
    return Rf_ScalarReal(count > 0 ? total / count : NA_REAL);
}

/* Task 24_long_vector_idx */
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

/* Task 25_l1_arithmetic */
#include <R.h>
#include <Rinternals.h>

#define N_REP 2500

SEXP c_call_bench_l1_arithmetic(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    double *xp = REAL(arg);
    double total = 0.0;
    for (int rep = 0; rep < N_REP; rep++) {
        for (R_xlen_t i = 0; i < n; i++) {
            total += xp[i] * 0.5 + 0.5;
        }
    }
    return Rf_ScalarReal(total);
}

/* Task 26_matmul */
#include <R.h>
#include <Rinternals.h>

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

/* Task 27_crossprod */
#include <R.h>
#include <Rinternals.h>

extern void dsyrk_(char *uplo, char *trans, int *n, int *k,
                   double *alpha, double *a, int *lda,
                   double *beta, double *c, int *ldc);

SEXP c_call_bench_crossprod(SEXP arg) {
    int n = Rf_ncols(arg);
    int k = Rf_nrows(arg);

    SEXP result = PROTECT(Rf_allocMatrix(REALSXP, n, n));
    double *rp = REAL(result);

    double alpha = 1.0, beta = 0.0;
    char uplo = 'U', trans = 'T';
    dsyrk_(&uplo, &trans, &n, &k,
           &alpha, REAL(arg), &k,
           &beta, rp, &n);

    for (int i = 0; i < n; i++)
        for (int j = 0; j < i; j++)
            rp[j * n + i] = rp[i * n + j];

    UNPROTECT(1);
    return result;
}

/* Task 28_cholesky */
#include <R.h>
#include <Rinternals.h>

extern void dpotrf_(char *uplo, int *n, double *a, int *lda, int *info);

SEXP c_call_bench_cholesky(SEXP arg) {
    int n = Rf_nrows(arg);
    SEXP result = PROTECT(Rf_allocMatrix(REALSXP, n, n));
    double *rp = REAL(result);
    double *src = REAL(arg);
    R_xlen_t len = (R_xlen_t)n * n;
    for (R_xlen_t i = 0; i < len; i++) rp[i] = src[i];

    int info = 0;
    char uplo = 'U';
    dpotrf_(&uplo, &n, rp, &n, &info);

    for (int col = 0; col < n; col++)
        for (int row = col + 1; row < n; row++)
            rp[col * n + row] = 0.0;

    UNPROTECT(1);
    return result;
}

/* Task 29_lm_fit */
#include <R.h>
#include <Rinternals.h>

extern void dgemm_(char *transa, char *transb, int *m, int *n, int *k,
                   double *alpha, double *a, int *lda, double *b, int *ldb,
                   double *beta, double *c, int *ldc);
extern void dpotrf_(char *uplo, int *n, double *a, int *lda, int *info);
extern void dtrsm_(char *side, char *uplo, char *transa, char *diag,
                   int *m, int *n, double *alpha, double *a, int *lda,
                   double *b, int *ldb);

SEXP c_call_bench_lm_fit(SEXP X, SEXP y) {
    int n = Rf_nrows(X);
    int p = Rf_ncols(X);

    SEXP xtx = PROTECT(Rf_allocMatrix(REALSXP, p, p));
    SEXP xty = PROTECT(Rf_allocVector(REALSXP, p));
    double *xtx_rp = REAL(xtx);
    double *xty_rp = REAL(xty);

    double alpha = 1.0, beta = 0.0;
    char notrans = 'N', trans = 'T';
    int one = 1;

    dgemm_(&trans, &notrans, &p, &p, &n, &alpha,
           REAL(X), &n, REAL(X), &n, &beta, xtx_rp, &p);
    dgemm_(&trans, &notrans, &p, &one, &n, &alpha,
           REAL(X), &n, REAL(y), &n, &beta, xty_rp, &p);

    int info = 0;
    char uplo = 'U';
    dpotrf_(&uplo, &p, xtx_rp, &p, &info);

    char side = 'L', diag = 'N';
    dtrsm_(&side, &uplo, &trans, &diag, &p, &one, &alpha,
           xtx_rp, &p, xty_rp, &p);
    dtrsm_(&side, &uplo, &notrans, &diag, &p, &one, &alpha,
           xtx_rp, &p, xty_rp, &p);

    UNPROTECT(2);
    return xty;
}

/* Task 30_altrep_create */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_altrep_create(SEXP arg) {
    SEXP call = PROTECT(Rf_lang2(Rf_install("seq_len"), arg));
    int err = 0;
    SEXP result = R_tryEvalSilent(call, R_GlobalEnv, &err);
    UNPROTECT(1);
    if (err != 0) return R_NilValue;
    return result;
}

/* Task 31_altrep_materialize */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_altrep_materialize(SEXP arg) {
    SEXP call = PROTECT(Rf_lang2(Rf_install("seq_len"), arg));
    int err = 0;
    SEXP alt = R_tryEvalSilent(call, R_GlobalEnv, &err);
    if (err != 0) { UNPROTECT(1); return R_NilValue; }
    SEXP mat = Rf_duplicate(alt);
    UNPROTECT(1);
    int n = LENGTH(mat);
    return Rf_ScalarInteger(INTEGER(mat)[0] + INTEGER(mat)[n - 1]);
}

/* Task 32_altrep_elt_walk */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_altrep_elt_walk(SEXP arg) {
    SEXP call = PROTECT(Rf_lang2(Rf_install("seq_len"), arg));
    int err = 0;
    SEXP alt = R_tryEvalSilent(call, R_GlobalEnv, &err);
    UNPROTECT(1);
    if (err != 0) return R_NilValue;
    R_xlen_t n = XLENGTH(alt);
    long long total = 0;
    for (R_xlen_t i = 0; i < n; i++)
        total += INTEGER_ELT(alt, i);
    return Rf_ScalarReal((double)total);
}

/* Task 33_altrep_region_read */
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

/* Task 34_altrep_sum_via_R */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_altrep_sum_via_R(SEXP arg) {
    SEXP call = PROTECT(Rf_lang2(Rf_install("seq_len"), arg));
    int err = 0;
    SEXP alt = R_tryEvalSilent(call, R_GlobalEnv, &err);
    if (err != 0) { UNPROTECT(1); return R_NilValue; }
    SEXP sum_call = PROTECT(Rf_lang2(Rf_install("sum"), alt));
    SEXP res = R_tryEvalSilent(sum_call, R_GlobalEnv, &err);
    UNPROTECT(2);
    if (err != 0) return R_NilValue;
    return res;
}

/* Task 35_altrep_sum_native */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_altrep_sum_native(SEXP arg) {
    SEXP call = PROTECT(Rf_lang2(Rf_install("seq_len"), arg));
    int err = 0;
    SEXP alt = R_tryEvalSilent(call, R_GlobalEnv, &err);
    UNPROTECT(1);
    if (err != 0) return R_NilValue;
    R_xlen_t n = XLENGTH(alt);
    long long total = 0;
    for (R_xlen_t i = 0; i < n; i++)
        total += INTEGER_ELT(alt, i);
    return Rf_ScalarReal((double)total);
}

/* Task 36_altrep_min_max */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_altrep_min_max(SEXP arg) {
    SEXP call = PROTECT(Rf_lang2(Rf_install("seq_len"), arg));
    int err = 0;
    SEXP alt = R_tryEvalSilent(call, R_GlobalEnv, &err);
    UNPROTECT(1);
    if (err != 0) return R_NilValue;
    R_xlen_t n = XLENGTH(alt);
    int min_val = INTEGER_ELT(alt, 0);
    int max_val = min_val;
    for (R_xlen_t i = 1; i < n; i++) {
        int v = INTEGER_ELT(alt, i);
        if (v < min_val) min_val = v;
        if (v > max_val) max_val = v;
    }
    return Rf_ScalarInteger(max_val - min_val);
}

/* Task 37_altrep_no_na_query */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_altrep_no_na_query(SEXP arg) {
    SEXP call = PROTECT(Rf_lang2(Rf_install("seq_len"), arg));
    int err = 0;
    SEXP alt = R_tryEvalSilent(call, R_GlobalEnv, &err);
    UNPROTECT(1);
    if (err != 0) return R_NilValue;
    R_xlen_t n = XLENGTH(alt);
    int has_na = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        if (INTEGER_ELT(alt, i) == NA_INTEGER) { has_na = 1; break; }
    }
    return Rf_ScalarInteger(has_na);
}

/* Task 38_struct_convert */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_struct_convert(SEXP arg) {
    const char *field_names[] = {"id", "count", "level", "flag", "enabled",
                                 "ratio", "offset", "scale", "weights", "indices"};

    SEXP names = PROTECT(Rf_allocVector(STRSXP, 10));
    for (int i = 0; i < 10; i++)
        SET_STRING_ELT(names, i, Rf_mkChar(field_names[i]));

    SEXP result = PROTECT(Rf_allocVector(VECSXP, 10));
    Rf_setAttrib(result, R_NamesSymbol, names);

    SET_VECTOR_ELT(result, 0, Rf_ScalarInteger(asInteger(VECTOR_ELT(arg, 0))));
    SET_VECTOR_ELT(result, 1, Rf_ScalarInteger(asInteger(VECTOR_ELT(arg, 1))));
    SET_VECTOR_ELT(result, 2, Rf_ScalarInteger(asInteger(VECTOR_ELT(arg, 2))));
    SET_VECTOR_ELT(result, 3, Rf_ScalarLogical(asLogical(VECTOR_ELT(arg, 3))));
    SET_VECTOR_ELT(result, 4, Rf_ScalarLogical(asLogical(VECTOR_ELT(arg, 4))));
    SET_VECTOR_ELT(result, 5, Rf_ScalarReal(asReal(VECTOR_ELT(arg, 5))));
    SET_VECTOR_ELT(result, 6, Rf_ScalarReal(asReal(VECTOR_ELT(arg, 6))));
    SET_VECTOR_ELT(result, 7, Rf_ScalarReal(asReal(VECTOR_ELT(arg, 7))));
    SET_VECTOR_ELT(result, 8, VECTOR_ELT(arg, 8));
    SET_VECTOR_ELT(result, 9, VECTOR_ELT(arg, 9));

    UNPROTECT(2);
    return result;
}

/* Task 39_r_eval */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_r_eval(SEXP arg) {
    SEXP sum_sym = Rf_install("sum");
    SEXP mean_sym = Rf_install("mean");

    SEXP sum_call = PROTECT(Rf_lang2(sum_sym, arg));
    SEXP sum_res = PROTECT(Rf_eval(sum_call, R_GlobalEnv));
    double sum_val = REAL(sum_res)[0];

    SEXP mean_call = PROTECT(Rf_lang2(mean_sym, arg));
    SEXP mean_res = PROTECT(Rf_eval(mean_call, R_GlobalEnv));
    double mean_val = REAL(mean_res)[0];

    UNPROTECT(4);
    return Rf_ScalarReal(sum_val + mean_val);
}

/* Task 40_r_tryeval */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_r_tryeval(SEXP arg) {
    (void)arg;
    int count = 0;
    for (int i = 0; i < 512; i++) {
        SEXP call = PROTECT(Rf_lang2(Rf_install("stop"), Rf_mkString("task40")));
        int err = 0;
        R_tryEvalSilent(call, R_GlobalEnv, &err);
        UNPROTECT(1);
        if (err != 0) count++;
    }
    return Rf_ScalarInteger(count);
}

/* Task 41_serialize_roundtrip */
#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_serialize_roundtrip(SEXP arg) {
    SEXP ser_call = PROTECT(Rf_lang3(Rf_install("serialize"), arg, R_NilValue));
    SEXP conn = PROTECT(Rf_eval(ser_call, R_GlobalEnv));

    SEXP unser_call = PROTECT(Rf_lang2(Rf_install("unserialize"), conn));
    SEXP result = PROTECT(Rf_eval(unser_call, R_GlobalEnv));
    double total = 0;
    double *xp = REAL(result);
    R_xlen_t n = XLENGTH(result);
    for (R_xlen_t i = 0; i < n; i++) total += xp[i];
    UNPROTECT(4);
    return Rf_ScalarReal(total);
}

/* Task 42_external_ptr */
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>

typedef struct {
    int value;
} c_call_benchmark_state;

static void c_call_benchmark_state_finalizer(SEXP pointer) {
    c_call_benchmark_state *state = (c_call_benchmark_state *) R_ExternalPtrAddr(pointer);
    if (state == NULL) return;
    free(state);
    R_ClearExternalPtr(pointer);
}

SEXP c_call_bench_external_ptr(SEXP arg) {
    int value = Rf_asInteger(arg);
    SEXP ptr = PROTECT(R_MakeExternalPtr(
        NULL,
        Rf_install("zigr.benchmark.c_call.task42.state"),
        R_NilValue
    ));
    c_call_benchmark_state *state = (c_call_benchmark_state *) malloc(sizeof(c_call_benchmark_state));
    if (state == NULL) {
        UNPROTECT(1);
        Rf_error("C external-pointer benchmark could not allocate state");
    }
    state->value = value;
    R_SetExternalPtrAddr(ptr, state);
    R_RegisterCFinalizerEx(ptr, c_call_benchmark_state_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

/* Task 43_rng_stress */
#include <R.h>
#include <Rinternals.h>

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
