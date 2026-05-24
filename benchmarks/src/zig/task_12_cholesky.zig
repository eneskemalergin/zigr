const R = @import("R");

const SEXP = R.SEXP;

const dpotrf_ = @extern(*const fn (uplo: [*c]const u8, n: [*c]const c_int, a: [*c]f64, lda: [*c]const c_int, info: [*c]c_int, len_up: c_int) callconv(.c) void, .{ .name = "dpotrf_" });

export fn zigr_bench_cholesky(a_sexp: SEXP) SEXP {
    const n = R.Rf_nrows(a_sexp);
    const nu = @as(usize, @intCast(n));

    const result = R.Rf_protect(R.Rf_allocMatrix(R.REALSXP, n, n));
    defer R.Rf_unprotect(1);
    const rp = R.REAL(result);

    @memcpy(rp[0 .. nu * nu], R.REAL(a_sexp)[0 .. nu * nu]);

    var info: c_int = 0;
    const uplo: u8 = 'U';
    dpotrf_(ptr(&uplo), ptr(&n), rp, ptr(&n), @ptrCast(&info), 1);

    for (0..nu) |col| {
        for (col + 1..nu) |row| rp[col * nu + row] = 0.0;
    }

    return result;
}

fn ptr(x: anytype) [*c]const @TypeOf(x.*) {
    return @ptrCast(x);
}
