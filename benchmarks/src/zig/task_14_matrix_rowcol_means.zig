const R = @import("R");
const simd = @import("simd");
const raw = @import("zigr").raw;

const SEXP = R.SEXP;

export fn zigr_bench_rowcol_means(mat_sexp: SEXP) SEXP {
    const nr = @as(usize, @intCast(R.Rf_nrows(mat_sexp)));
    const nc = @as(usize, @intCast(R.Rf_ncols(mat_sexp)));
    const data = raw.real(mat_sexp);

    const row_means = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(nr)));
    const col_sums = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(nc)));
    defer R.Rf_unprotect(2);

    const rm = R.REAL(row_means);
    const cs = R.REAL(col_sums);

    // Row means: strided access (row i = data[i + j*nr]), not vectorizable
    for (0..nr) |i| {
        var sum: f64 = 0.0;
        var j: usize = 0;
        while (j < nc) : (j += 1) sum += data[i + j * nr];
        rm[i] = sum / @as(f64, @floatFromInt(nc));
    }

    // Col sums: unit-stride access (col j = data[j*nr..j*nr+nr]), SIMD-friendly
    const lanes = simd.f64_lanes;
    for (0..nc) |j| {
        const col = data[j * nr ..][0..nr];
        var vec: @Vector(lanes, f64) = @splat(0.0);
        var i: usize = 0;
        if (nr >= lanes) {
            const end = nr - (nr % lanes);
            while (i < end) : (i += lanes) {
                vec += col[i..][0..lanes].*;
            }
        }
        cs[j] = @reduce(.Add, vec);
        while (i < nr) : (i += 1) cs[j] += col[i];
    }

    const result = R.Rf_protect(R.Rf_allocVector(R.VECSXP, 2));
    defer R.Rf_unprotect(1);
    _ = R.SET_VECTOR_ELT(result, 0, row_means);
    _ = R.SET_VECTOR_ELT(result, 1, col_sums);

    return result;
}
