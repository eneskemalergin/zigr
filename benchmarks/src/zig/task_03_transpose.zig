const R = @import("R");
const raw = @import("raw");
const SEXP = R.SEXP;
const block_len: usize = 32;

/// Matrix transpose: output[j][i] = input[i][j].
/// Process tiles so each inner loop writes a contiguous output span.
export fn zigr_bench_transpose(mat_sexp: SEXP) SEXP {
    const nr = @as(usize, @intCast(R.Rf_nrows(mat_sexp)));
    const nc = @as(usize, @intCast(R.Rf_ncols(mat_sexp)));
    const data = raw.real(mat_sexp);

    const result = R.Rf_protect(R.Rf_allocMatrix(R.REALSXP, @intCast(nc), @intCast(nr)));
    defer R.Rf_unprotect(1);
    const rp = raw.realMut(result);

    var jj: usize = 0;
    while (jj < nc) : (jj += block_len) {
        const j_end = @min(jj + block_len, nc);
        var ii: usize = 0;
        while (ii < nr) : (ii += block_len) {
            const i_end = @min(ii + block_len, nr);
            var i: usize = ii;
            while (i < i_end) : (i += 1) {
                const out_row = rp[i * nc .. (i + 1) * nc];
                var j: usize = jj;
                while (j < j_end) : (j += 1) {
                    out_row[j] = data[i + (j * nr)];
                }
            }
        }
    }

    return result;
}
