const R = @import("R");
const SEXP = R.SEXP;

/// Sum of an ALTREP compact integer sequence (e.g. 1:1e7).
///
/// Every backend routes this task through R's sum() generic so the
/// benchmark measures the same ALTREP-aware dispatch path.
export fn zigr_bench_altrep_sum(sexp: SEXP) SEXP {
    const sum_sym = R.Rf_install("sum");
    const call = R.Rf_lang2(sum_sym, sexp);
    return R.Rf_eval(call, R.R_GlobalEnv);
}
