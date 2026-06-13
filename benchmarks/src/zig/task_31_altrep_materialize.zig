const R = @import("R");

export fn zigr_bench_altrep_materialize(vec: R.SEXP) R.SEXP {
    const call = R.Rf_protect(R.Rf_lang2(R.Rf_install("seq_len"), vec));
    var err: c_int = 0;
    const alt = R.R_tryEvalSilent(call, R.R_GlobalEnv, &err);
    R.Rf_unprotect(1);
    if (err != 0) return R.R_NilValue;
    // Rf_duplicate forces ALTREP to materialize its backing store.
    // Rf_coerceVector may return the same ALTREP when types match.
    const mat = R.Rf_duplicate(alt);
    const nu = @as(usize, @intCast(R.XLENGTH(mat)));
    const data: [*]c_int = @ptrCast(R.INTEGER(mat));
    const sum = data[0] + data[nu - 1];
    return R.Rf_ScalarInteger(sum);
}
