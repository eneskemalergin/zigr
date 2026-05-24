const R = @import("R");
const simd = @import("simd");
const raw = @import("zigr").raw;

const SEXP = R.SEXP;
const lanes = simd.f64_lanes;

export fn zigr_bench_rowsums(mat_sexp: SEXP) SEXP {
    const nr = @as(usize, @intCast(R.Rf_nrows(mat_sexp)));
    const nc = @as(usize, @intCast(R.Rf_ncols(mat_sexp)));
    const data = raw.real(mat_sexp);

    const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(nr)));
    defer R.Rf_unprotect(1);
    const rp = raw.realMut(result);
    @memset(rp, 0.0);

    for (0..nc) |j| {
        const col = data[j * nr ..][0..nr];
        var i: usize = 0;
        if (nr >= lanes) {
            const end = nr - (nr % lanes);
            while (i < end) : (i += lanes) {
                const v: @Vector(lanes, f64) = col[i..][0..lanes].*;
                inline for (0..lanes) |k| rp[i + k] += v[k];
            }
        }
        while (i < nr) : (i += 1) rp[i] += col[i];
    }

    return result;
}
