const R = @import("R");

const SEXP = R.SEXP;

const dsyrk_ = @extern(*const fn (uplo: [*c]const u8, trans: [*c]const u8, n: [*c]const c_int, k: [*c]const c_int, alpha: [*c]const f64, a: [*c]const f64, lda: [*c]const c_int, beta: [*c]const f64, c: [*c]f64, ldc: [*c]const c_int, len_up: c_int, len_tr: c_int) callconv(.c) void, .{ .name = "dsyrk_" });

export fn zigr_bench_crossprod(x_sexp: SEXP) SEXP {
    const nr = R.Rf_nrows(x_sexp);
    const nc = R.Rf_ncols(x_sexp);
    const n = nc;

    const result = R.Rf_protect(R.Rf_allocMatrix(R.REALSXP, nc, nc));
    defer R.Rf_unprotect(1);
    const rp = @as([*]f64, @ptrCast(R.REAL(result)));

    const alpha: f64 = 1.0;
    const beta: f64 = 0.0;
    const uplo: u8 = 'U';
    const trans: u8 = 'T';

    dsyrk_(
        @ptrCast(&uplo), @ptrCast(&trans),
        @ptrCast(&nc), @ptrCast(&nr),
        @ptrCast(&alpha), @ptrCast(R.REAL(x_sexp)), @ptrCast(&nr),
        @ptrCast(&beta), @ptrCast(rp), @ptrCast(&nc),
        1, 1,
    );

    const nu = @as(usize, @intCast(n));
    for (0..nu) |i| {
        for (0..i) |j| {
            rp[i * nu + j] = rp[j * nu + i];
        }
    }

    return result;
}
