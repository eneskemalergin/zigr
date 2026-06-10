// Rcpp benchmark stubs. Each function is registered via R_init.
#include <Rcpp.h>
using namespace Rcpp;

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
SEXP rcpp_bench_sort(SEXP x) { return R_NilValue; }
static long long fib_cpp(long long n) {
    if (n <= 1) return n;
    return fib_cpp(n - 1) + fib_cpp(n - 2);
}
SEXP rcpp_bench_fib_recursive(SEXP x) {
    long long n = Rcpp::as<long long>(x);
    long long result = fib_cpp(n);
    return Rcpp::wrap((double)result);
}
SEXP rcpp_bench_broadcast(SEXP x) { return R_NilValue; }

// Layer 2: R API overhead
SEXP rcpp_bench_protect_shallow(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_protect_scaling(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_type_dispatch(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_longjmp_safety(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_sexp_create(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_sexp_inspect(SEXP x) { return R_NilValue; }

// Layer 3: Data structures
SEXP rcpp_bench_matrix_transpose(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_matrix_rowsums(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_matrix_rowcol_means(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_dataframe_filter(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_list_access(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_string_concat(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_string_nchar(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_string_encoding(SEXP x) { return R_NilValue; }
SEXP rcpp_bench_factor_ops(SEXP x) { return R_NilValue; }
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
