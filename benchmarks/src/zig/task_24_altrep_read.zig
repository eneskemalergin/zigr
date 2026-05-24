const R = @import("R");
const SEXP = R.SEXP;

/// Read first and last elements of an ALTREP compact integer sequence.
///
/// zigR uses INTEGER_GET_REGION to read individual elements through
/// the ALTREP method table without materializing the entire vector.
/// O(1) per element, O(1) overall for two reads.
///
/// C/Rcpp must call INTEGER() which forces full O(n) materialization
/// before any element can be accessed — even for a single element.
export fn zigr_bench_altrep_read(sexp: SEXP) SEXP {
    const n = R.XLENGTH(sexp);
    var first: c_int = 0;
    var last: c_int = 0;
    _ = R.INTEGER_GET_REGION(sexp, 0, 1, @ptrCast(&first));
    _ = R.INTEGER_GET_REGION(sexp, n - 1, 1, @ptrCast(&last));
    const result = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 2));
    defer R.Rf_unprotect(1);
    R.INTEGER(result)[0] = first;
    R.INTEGER(result)[1] = last;
    return result;
}
