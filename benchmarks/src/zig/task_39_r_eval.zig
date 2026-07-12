const R = @import("R");
const zigr_eval = @import("zigr").eval;

export fn zigr_bench_r_eval(vec: R.SEXP) R.SEXP {
    const args = [_]R.SEXP{vec};
    const sum_res = zigr_eval.callIn("sum", args[0..], R.R_GlobalEnv);
    const sum_val = R.REAL(sum_res)[0];

    const mean_res = zigr_eval.callIn("mean", args[0..], R.R_GlobalEnv);
    const mean_val = R.REAL(mean_res)[0];

    return R.Rf_ScalarReal(sum_val + mean_val);
}
