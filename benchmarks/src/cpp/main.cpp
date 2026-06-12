#include <Rcpp.h>
#include <cstdint>
#include <cstring>
#include <algorithm>
using namespace Rcpp;

static void radix_sort_f64(double *arr, size_t n) {
    if (n < 2) return;
    uint64_t *buf = reinterpret_cast<uint64_t *>(arr);
    const uint64_t sign_bit = uint64_t(1) << 63;
    for (size_t i = 0; i < n; i++) {
        uint64_t v = buf[i];
        buf[i] = (v & sign_bit) ? ~v : v ^ sign_bit;
    }
    uint64_t stack_temp[256];
    uint64_t *temp = (n <= 256) ? stack_temp : (uint64_t *)R_chk_calloc(n, sizeof(uint64_t));
    int needs_free = (n > 256);
    for (int shift = 0; shift < 64; shift += 8) {
        unsigned int counts[256];
        std::memset(counts, 0, sizeof(counts));
        for (size_t i = 0; i < n; i++) counts[(buf[i] >> shift) & 0xFF]++;
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
        std::memcpy(buf, temp, n * sizeof(uint64_t));
    }
    if (needs_free) R_chk_free(temp);
    for (size_t i = 0; i < n; i++) {
        uint64_t v = buf[i];
        buf[i] = (v & sign_bit) ? v ^ sign_bit : ~v;
    }
}

extern "C" {
// Layer 1: Primitives
SEXP rcpp_bench_vectorsum(SEXP x) {
    double *xp = REAL(x);
    R_xlen_t n = XLENGTH(x);
    double total = 0.0;
    for (R_xlen_t i = 0; i < n; i++) total += xp[i];
    return Rf_ScalarReal(total);
}
SEXP rcpp_bench_elem_ops(SEXP x) {
    double *xp = REAL(x);
    R_xlen_t n = XLENGTH(x);
    SEXP result = PROTECT(Rf_allocMatrix(REALSXP, n, 4));
    double *rp = REAL(result);
    for (R_xlen_t i = 0; i < n; i++) {
        double v = xp[i];
        rp[i] = fabs(v);
        rp[i + n] = v > 0.0 ? log(v) : 0.0;
        rp[i + 2 * n] = exp(v);
        rp[i + 3 * n] = v >= 0.0 ? sqrt(v) : 0.0;
    }
    UNPROTECT(1);
    return result;
}
SEXP rcpp_bench_memcpy_bandwidth(SEXP x) {
    double *xp = REAL(x);
    R_xlen_t n = XLENGTH(x);
    size_t bytes = n * sizeof(double);

    double totals[3] = {0.0, 0.0, 0.0};

    for (int rep = 0; rep < 2; rep++) {
        double *temp = (double *)malloc(bytes);
        memcpy(temp, xp, bytes);
        double s = 0;
        for (R_xlen_t i = 0; i < n; i++) s += temp[i];
        totals[0] += s;
        free(temp);

        SEXP copy_out = PROTECT(Rf_allocVector(REALSXP, n));
        memcpy(REAL(copy_out), xp, bytes);
        s = 0;
        for (R_xlen_t i = 0; i < n; i++) s += REAL(copy_out)[i];
        totals[1] += s;
        UNPROTECT(1);

        SEXP fill_out = PROTECT(Rf_allocVector(REALSXP, n));
        double *fp = REAL(fill_out);
        for (R_xlen_t i = 0; i < n; i++) fp[i] = xp[i] + 0.5;
        s = 0;
        for (R_xlen_t i = 0; i < n; i++) s += fp[i];
        totals[2] += s;
        UNPROTECT(1);
    }

    const char *nms[] = {"copy_temp", "copy_out", "fill_out"};
    SEXP result = PROTECT(Rf_allocVector(REALSXP, 3));
    SEXP rn = PROTECT(Rf_allocVector(STRSXP, 3));
    for (int i = 0; i < 3; i++) SET_STRING_ELT(rn, i, Rf_mkChar(nms[i]));
    Rf_setAttrib(result, R_NamesSymbol, rn);
    REAL(result)[0] = totals[0];
    REAL(result)[1] = totals[1];
    REAL(result)[2] = totals[2];
    UNPROTECT(2);
    return result;
}
SEXP rcpp_bench_sort(SEXP x) {
    R_xlen_t n = XLENGTH(x);
    double *xp = REAL(x);
    SEXP result = PROTECT(Rf_allocVector(REALSXP, n));
    double *rp = REAL(result);
    memcpy(rp, xp, n * sizeof(double));
    radix_sort_f64(rp, (size_t)n);
    UNPROTECT(1);
    return result;
}
static long long fib_cpp(long long n) {
    if (n <= 1) return n;
    return fib_cpp(n - 1) + fib_cpp(n - 2);
}
SEXP rcpp_bench_fib_recursive(SEXP x) {
    long long n = Rcpp::as<long long>(x);
    long long result = fib_cpp(n);
    return Rcpp::wrap((double)result);
}
SEXP rcpp_bench_broadcast(SEXP x, SEXP s_sexp) {
    double *xp = REAL(x);
    R_xlen_t n = XLENGTH(x);
    double s = REAL(s_sexp)[0];
    double total = 0.0;
    for (R_xlen_t i = 0; i < n; i++) total += xp[i] + s;
    return Rf_ScalarReal(total);
}

// Layer 2: R API overhead
SEXP rcpp_bench_protect_shallow(SEXP x) {
    for (int r = 0; r < 100; r++) {
        for (int i = 0; i < 10; i++) PROTECT(x);
        Rf_unprotect(10);
    }
    return Rf_ScalarInteger(0);
}
SEXP rcpp_bench_protect_scaling(SEXP x) {
    for (int r = 0; r < 100; r++) {
        for (int b = 0; b < 10; b++) {
            for (int i = 0; i < 10000; i++) PROTECT(x);
            Rf_unprotect(10000);
        }
    }
    return Rf_ScalarInteger(0);
}
SEXP rcpp_bench_type_dispatch(SEXP arg) {
    SEXP elts[3] = {
        VECTOR_ELT(arg, 0),
        VECTOR_ELT(arg, 1),
        VECTOR_ELT(arg, 2)
    };
    int total = 0;
    for (int i = 0; i < 2048; i++) {
        switch (TYPEOF(elts[0])) {
            case REALSXP: total += 1; break;
            case INTSXP: total += 2; break;
            case STRSXP: total += 3; break;
            default: break;
        }
        switch (TYPEOF(elts[1])) {
            case REALSXP: total += 1; break;
            case INTSXP: total += 2; break;
            case STRSXP: total += 3; break;
            default: break;
        }
        switch (TYPEOF(elts[2])) {
            case REALSXP: total += 1; break;
            case INTSXP: total += 2; break;
            case STRSXP: total += 3; break;
            default: break;
        }
    }
    return Rf_ScalarInteger(total);
}

#define RCPP_REPEATS 512

static double rcpp_adjusted_sum(const double *x, int n, double bias) {
    double total = 0.0;
    for (int i = 0; i < n; i++) total += x[i] + bias;
    return total;
}

static void rcpp_fill_adjusted(double *out, const double *x, int n, double bias) {
    for (int i = 0; i < n; i++) out[i] = x[i] + bias;
}

typedef struct {
    const double *x;
    int n;
    double bias;
} rcpp_unwind_data_t;

static SEXP rcpp_unwind_callback(void *data) {
    rcpp_unwind_data_t *ud = (rcpp_unwind_data_t *)data;
    return Rf_ScalarReal(rcpp_adjusted_sum(ud->x, ud->n, ud->bias));
}

static void rcpp_unwind_noop(void *data, Rboolean jump) {}

SEXP rcpp_bench_longjmp_safety(SEXP arg) {
    int n = LENGTH(arg);
    const double *x = REAL(arg);

    double direct_total = 0.0, try_ok_total = 0.0;
    double try_err_total = 0.0, unwind_ok_total = 0.0;

    SEXP sum_sym = Rf_install("sum");
    SEXP stop_sym = Rf_install("stop");

    for (int rep = 0; rep < RCPP_REPEATS; rep++) {
        double bias = (rep + 1.0) * 0.001;
        direct_total += rcpp_adjusted_sum(x, n, bias);

        SEXP tmp = PROTECT(Rf_allocVector(REALSXP, n));
        rcpp_fill_adjusted(REAL(tmp), x, n, bias);
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

        rcpp_unwind_data_t ud = { .x = x, .n = n, .bias = bias };
        SEXP cont = PROTECT(R_MakeUnwindCont());
        SEXP ures = R_UnwindProtect(rcpp_unwind_callback, &ud, rcpp_unwind_noop, NULL, cont);
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

SEXP rcpp_bench_sexp_create(SEXP arg) {
    (void)arg;
    for (int b = 0; b < 10; b++) {
        for (int i = 0; i < 10000; i++) {
            PROTECT(Rf_allocVector(REALSXP, 1));
        }
        UNPROTECT(10000);
    }
    return Rf_ScalarReal(0.0);
}
SEXP rcpp_bench_sexp_inspect(SEXP arg) {
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

// Layer 3: Data structures
#define BLOCK 32

SEXP rcpp_bench_matrix_transpose(SEXP arg) {
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
SEXP rcpp_bench_matrix_rowsums(SEXP arg) {
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
SEXP rcpp_bench_matrix_rowcol_means(SEXP arg) {
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
SEXP rcpp_bench_dataframe_filter(SEXP arg) {
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
        if (xp[i] > 0.0 && !ISNA(xp[i]) && gp[i] > max_grp) max_grp = gp[i];
    }
    int ngroups = max_grp;
    SEXP grp_out = PROTECT(Rf_allocVector(INTSXP, ngroups));
    SEXP sum_out = PROTECT(Rf_allocVector(REALSXP, ngroups));
    int *gop = INTEGER(grp_out);
    double *sop = REAL(sum_out);
    memset(sop, 0, ngroups * sizeof(double));
    for (int i = 0; i < ngroups; i++) gop[i] = i + 1;
    for (R_xlen_t i = 0; i < nrows; i++) {
        if (xp[i] > 0.0 && !ISNA(xp[i])) {
            int g = gp[i] - 1;
            if (g >= 0 && g < ngroups) sop[g] += xp[i] / yp[i];
        }
    }
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
    SEXP rn = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_VECTOR_ELT(result, 0, grp_out);
    SET_VECTOR_ELT(result, 1, sum_out);
    SET_STRING_ELT(rn, 0, Rf_mkChar("grp"));
    SET_STRING_ELT(rn, 1, Rf_mkChar("z_sum"));
    Rf_namesgets(result, rn);
    SEXP cls = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(cls, 0, Rf_mkChar("data.frame"));
    Rf_classgets(result, cls);
    SEXP rns = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(rns)[0] = NA_INTEGER;
    INTEGER(rns)[1] = -ngroups;
    Rf_setAttrib(result, R_RowNamesSymbol, rns);
    UNPROTECT(6);
    return result;
}
SEXP rcpp_bench_list_access(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    double total = 0.0;
    for (R_xlen_t i = 0; i < n; i++) {
        total += REAL(VECTOR_ELT(arg, i))[0];
    }
    return Rf_ScalarReal(total);
}
SEXP rcpp_bench_string_concat(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    R_xlen_t total = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP elt = STRING_ELT(arg, i);
        total += (elt == NA_STRING) ? 2 : LENGTH(elt);
    }
    if (n > 1) total += (n - 1) * 2;

    char *buf = (char *)R_alloc(total, 1);
    char *pos = buf;
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP elt = STRING_ELT(arg, i);
        if (elt == NA_STRING) {
            memcpy(pos, "NA", 2); pos += 2;
        } else {
            R_xlen_t len = LENGTH(elt);
            memcpy(pos, CHAR(elt), len);
            pos += len;
        }
        if (i + 1 < n) { memcpy(pos, ", ", 2); pos += 2; }
    }

    SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
    SEXP cs = Rf_mkCharLenCE(buf, pos - buf, CE_UTF8);
    SET_STRING_ELT(out, 0, cs);
    UNPROTECT(1);
    return out;
}
SEXP rcpp_bench_string_nchar(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    long long total = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP elt = STRING_ELT(arg, i);
        if (elt == NA_STRING) continue;
        total += LENGTH(elt);
    }
    return Rf_ScalarInteger(total);
}
SEXP rcpp_bench_string_encoding(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    int total = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        total += Rf_getCharCE(STRING_ELT(arg, i));
    }
    return Rf_ScalarInteger(total);
}
SEXP rcpp_bench_factor_ops(SEXP arg) {
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
SEXP rcpp_bench_attrib_ops(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_s4_slot_access(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_na_propagation(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_long_vector_idx(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_l1_arithmetic(SEXP x) { return R_NilValue; }

// Layer 4: Numerical
SEXP rcpp_bench_matmul(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_crossprod(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_cholesky(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_lm_fit(SEXP x) { return R_NilValue; }

// Layer 5: ALTREP
SEXP rcpp_bench_altrep_create(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_altrep_materialize(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_altrep_elt_walk(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_altrep_region_read(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_altrep_sum_via_R(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_altrep_sum_native(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_altrep_min_max(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_altrep_no_na_query(SEXP x) { return R_NilValue; }

// Layer 6: Integration
SEXP rcpp_bench_struct_convert(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_r_eval(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_r_tryeval(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_serialize_roundtrip(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_external_ptr(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_rng_stress(SEXP x) { return R_NilValue; }
} // extern "C"
