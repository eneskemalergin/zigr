const R = @import("R");
const raw = @import("zigr").raw;

const SEXP = R.SEXP;

export fn zigr_bench_matrix_rowsums(mat_sexp: SEXP) SEXP {
    const nr = @as(usize, @intCast(R.Rf_nrows(mat_sexp)));
    const nc = @as(usize, @intCast(R.Rf_ncols(mat_sexp)));
    const data = raw.real(mat_sexp);

    const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(nr)));
    defer R.Rf_unprotect(1);
    const rp = raw.realMut(result);
    @memset(rp, 0.0);

    for (0..nc) |j| {
        const col = data[j * nr ..][0..nr];
        for (0..nr) |i| rp[i] += col[i];
    }

    return result;
}
