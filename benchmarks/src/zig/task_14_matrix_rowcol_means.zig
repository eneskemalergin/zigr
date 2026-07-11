const R = @import("R");
const raw = @import("zigr").raw;

const SEXP = R.SEXP;

export fn zigr_bench_matrix_rowcol_means(mat_sexp: SEXP) SEXP {
    const nr = @as(usize, @intCast(R.Rf_nrows(mat_sexp)));
    const nc = @as(usize, @intCast(R.Rf_ncols(mat_sexp)));
    const data = raw.real(mat_sexp);

    const row_means = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(nr)));
    const col_sums = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(nc)));
    defer R.Rf_unprotect(2);

    const rm = raw.realMut(row_means);
    const cs = raw.realMut(col_sums);

    @memset(rm, 0.0);

    // R does not promise SIMD alignment, so this loop must accept any address.
    for (0..nc) |j| {
        const col = data[j * nr ..][0..nr];
        var i: usize = 0;
        var col_acc: @Vector(4, f64) = @splat(0.0);
        while (i + 4 <= nr) : (i += 4) {
            const v: @Vector(4, f64) = col[i..][0..4].*;
            col_acc += v;
            const rp: *align(1) @Vector(4, f64) = @ptrCast(&rm[i]);
            rp.* += v;
        }
        cs[j] = @reduce(.Add, col_acc);
        while (i < nr) : (i += 1) {
            rm[i] += col[i];
            cs[j] += col[i];
        }
    }

    const inv_nc = 1.0 / @as(f64, @floatFromInt(nc));
    for (0..nr) |i| rm[i] *= inv_nc;

    const result = R.Rf_protect(R.Rf_allocVector(R.VECSXP, 2));
    defer R.Rf_unprotect(1);
    _ = R.SET_VECTOR_ELT(result, 0, row_means);
    _ = R.SET_VECTOR_ELT(result, 1, col_sums);

    return result;
}
