const R = @import("R");

const SEXP = R.SEXP;

const dgemm_ = @extern(*const fn (transa: [*c]const u8, transb: [*c]const u8, m: [*c]const c_int, n: [*c]const c_int, k: [*c]const c_int, alpha: [*c]const f64, a: [*c]const f64, lda: [*c]const c_int, b: [*c]const f64, ldb: [*c]const c_int, beta: [*c]const f64, c: [*c]f64, ldc: [*c]const c_int, len_a: c_int, len_b: c_int) callconv(.c) void, .{ .name = "dgemm_" });

export fn zigr_bench_blas_matmul(a_sexp: SEXP, b_sexp: SEXP) SEXP {
    const n = R.Rf_nrows(a_sexp);
    const m = R.Rf_ncols(b_sexp);
    const k = R.Rf_ncols(a_sexp);

    const result = R.Rf_protect(R.Rf_allocMatrix(R.REALSXP, n, m));
    defer R.Rf_unprotect(1);
    const rp = @as([*]f64, @ptrCast(R.REAL(result)));

    const alpha: f64 = 1.0;
    const beta: f64 = 0.0;
    const notrans: u8 = 'N';

    dgemm_(
        @ptrCast(&notrans),
        @ptrCast(&notrans),
        @ptrCast(&n),
        @ptrCast(&m),
        @ptrCast(&k),
        @ptrCast(&alpha),
        @ptrCast(R.REAL(a_sexp)),
        @ptrCast(&n), // lda = n (rows of A)
        @ptrCast(R.REAL(b_sexp)),
        @ptrCast(&k), // ldb = k (rows of B)
        @ptrCast(&beta),
        @ptrCast(rp),
        @ptrCast(&n), // ldc = n (rows of C)
        1,
        1,
    );

    return result;
}
