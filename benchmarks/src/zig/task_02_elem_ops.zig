const R = @import("R");
const simd = @import("simd");

const SEXP = R.SEXP;

export fn zigr_bench_elem_ops(vec_sexp: SEXP) SEXP {
    const n = @as(usize, @intCast(R.XLENGTH(vec_sexp)));
    const src = R.REAL(vec_sexp);

    const result = R.Rf_protect(R.Rf_allocMatrix(R.REALSXP, @intCast(n), 4));
    defer R.Rf_unprotect(1);
    const rp = @as([*]f64, @ptrCast(R.REAL(result)));

    const lanes = simd.f64_lanes;
    var i: usize = 0;
    const end = n - (n % lanes);
    while (i < end) : (i += lanes) {
        const v: @Vector(lanes, f64) = src[i..][0..lanes].*;
        const v_arr: [lanes]f64 = v;

        const abs_mask: @Vector(lanes, u64) = @splat(~(@as(u64, 1) << 63));
        const abs_bits = @as(@Vector(lanes, u64), @bitCast(v)) & abs_mask;
        const abs_vals: @Vector(lanes, f64) = @bitCast(abs_bits);
        @memcpy(rp[i..][0..lanes], @as(*const [lanes]f64, @ptrCast(&abs_vals)));

        inline for (0..lanes) |j| {
            const val = v_arr[j];
            rp[i + n + j] = if (val > 0) @log(val) else 0;
            rp[i + 2 * n + j] = @exp(val);
        }

        const zero: @Vector(lanes, f64) = @splat(0.0);
        const sqrt_vals = @select(f64, v >= zero, @sqrt(v), zero);
        @memcpy(rp[i + 3 * n ..][0..lanes], @as(*const [lanes]f64, @ptrCast(&sqrt_vals)));
    }

    while (i < n) : (i += 1) {
        const v = src[i];
        rp[i] = @abs(v);
        rp[i + n] = if (v > 0) @log(v) else 0;
        rp[i + 2 * n] = @exp(v);
        rp[i + 3 * n] = if (v >= 0) @sqrt(v) else 0;
    }

    return result;
}
