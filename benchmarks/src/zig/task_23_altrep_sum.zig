const R = @import("R");
const SEXP = R.SEXP;

/// Sum of an ALTREP compact integer sequence (e.g. 1:1e7).
///
/// zigR calls R's sum() which delegates to the ALTREP method table.
/// The compact_intseq Sum method uses n*(n+1)/2 — O(1), zero copy.
///
/// C/Rcpp must call INTEGER() which forces full O(n) materialization
/// before summing. This task proves zigR's ALTREP advantage: the
/// same R function, called from Zig, avoids the copy entirely.
export fn zigr_bench_altrep_sum(sexp: SEXP) SEXP {
    const sum_sym = R.Rf_install("sum");
    const call = R.Rf_lang2(sum_sym, sexp);
    return R.Rf_eval(call, R.R_GlobalEnv);
}
