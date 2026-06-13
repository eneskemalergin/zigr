const R = @import("R");
const raw = @import("zigr").raw;

const SEXP = R.SEXP;

const dsyrk_ = @extern(*const fn (uplo: [*c]const u8, trans: [*c]const u8, n: [*c]const c_int, k: [*c]const c_int, alpha: [*c]const f64, a: [*c]const f64, lda: [*c]const c_int, beta: [*c]const f64, c: [*c]f64, ldc: [*c]const c_int, len_up: c_int, len_tr: c_int) callconv(.c) void, .{ .name = "dsyrk_" });
const dgemm_ = @extern(*const fn (transa: [*c]const u8, transb: [*c]const u8, m: [*c]const c_int, n: [*c]const c_int, k: [*c]const c_int, alpha: [*c]const f64, a: [*c]const f64, lda: [*c]const c_int, b: [*c]const f64, ldb: [*c]const c_int, beta: [*c]const f64, c: [*c]f64, ldc: [*c]const c_int, len_a: c_int, len_b: c_int) callconv(.c) void, .{ .name = "dgemm_" });
const dpotrf_ = @extern(*const fn (uplo: [*c]const u8, n: [*c]const c_int, a: [*c]f64, lda: [*c]const c_int, info: [*c]c_int, len_up: c_int) callconv(.c) void, .{ .name = "dpotrf_" });
const dtrsm_ = @extern(*const fn (side: [*c]const u8, uplo: [*c]const u8, transa: [*c]const u8, diag: [*c]const u8, m: [*c]const c_int, n: [*c]const c_int, alpha: [*c]const f64, a: [*c]const f64, lda: [*c]const c_int, b: [*c]f64, ldb: [*c]const c_int, len_s: c_int, len_u: c_int, len_t: c_int, len_d: c_int) callconv(.c) void, .{ .name = "dtrsm_" });

export fn zigr_bench_lm(x_sexp: SEXP, y_sexp: SEXP) SEXP {
    const n = R.Rf_nrows(x_sexp);
    const p = R.Rf_ncols(x_sexp);

    const x_data = raw.real(x_sexp);
    const y_data = raw.real(y_sexp);

    const xtx = R.Rf_protect(R.Rf_allocMatrix(R.REALSXP, p, p));
    const xty_prot = R.Rf_protect(R.Rf_allocVector(R.REALSXP, p));
    defer R.Rf_unprotect(2);
    const xtx_rp = R.REAL(xtx);
    const xty_rp = R.REAL(xty_prot);

    const alpha: f64 = 1.0;
    const beta: f64 = 0.0;
    const notrans: u8 = 'N';
    const trans: u8 = 'T';
    const uplo: u8 = 'U';
    const one: c_int = 1;

    dsyrk_(ptr(&uplo), ptr(&trans), ptr(&p), ptr(&n), ptr(&alpha), x_data.ptr, ptr(&n), ptr(&beta), xtx_rp, ptr(&p), 1, 1);

    dgemm_(ptr(&trans), ptr(&notrans), ptr(&p), ptr(&one), ptr(&n), ptr(&alpha), x_data.ptr, ptr(&n), y_data.ptr, ptr(&n), ptr(&beta), xty_rp, ptr(&p), 1, 1);

    var info: c_int = 0;
    dpotrf_(ptr(&uplo), ptr(&p), xtx_rp, ptr(&p), @ptrCast(&info), 1);

    const side: u8 = 'L';
    const diag: u8 = 'N';
    dtrsm_(ptr(&side), ptr(&uplo), ptr(&trans), ptr(&diag), ptr(&p), ptr(&one), ptr(&alpha), xtx_rp, ptr(&p), xty_rp, ptr(&p), 1, 1, 1, 1);

    dtrsm_(ptr(&side), ptr(&uplo), ptr(&notrans), ptr(&diag), ptr(&p), ptr(&one), ptr(&alpha), xtx_rp, ptr(&p), xty_rp, ptr(&p), 1, 1, 1, 1);

    return xty_prot;
}

fn ptr(x: anytype) [*c]const @TypeOf(x.*) {
    return @ptrCast(x);
}
