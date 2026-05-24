const R = @import("R");

const SEXP = R.SEXP;

export fn zigr_bench_elem_ops(vec_sexp: SEXP) SEXP {
    const n = @as(usize, @intCast(R.XLENGTH(vec_sexp)));
    const src = R.REAL(vec_sexp);

    const result = R.Rf_protect(R.Rf_allocMatrix(R.REALSXP, @intCast(n), 4));
    defer R.Rf_unprotect(1);
    const rp = @as([*]f64, @ptrCast(R.REAL(result)));

    var i: usize = 0;
    while (i < n) : (i += 1) {
        const v = src[i];
        rp[i] = @abs(v);
        rp[i + n] = if (v > 0) @log(v) else 0;
        rp[i + 2 * n] = @exp(v);
        rp[i + 3 * n] = if (v >= 0) @sqrt(v) else 0;
    }

    return result;
}
