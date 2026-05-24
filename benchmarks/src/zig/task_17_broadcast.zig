const R = @import("R");
const simd = @import("simd");
const raw = @import("raw");

const SEXP = R.SEXP;

export fn zigr_bench_broadcast(vec_sexp: SEXP, scalar_sexp: SEXP) SEXP {
    const n = @as(usize, @intCast(R.XLENGTH(vec_sexp)));
    const src = raw.real(vec_sexp);
    const scalar = raw.real(scalar_sexp)[0];

    const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(n)));
    defer R.Rf_unprotect(1);
    const rp = raw.realMut(result);

    const s: @Vector(simd.f64_lanes, f64) = @splat(scalar);
    var i: usize = 0;
    if (n >= 8) {
        const end = n - (n % 8);
        while (i < end) : (i += 8) {
            @as(*@Vector(simd.f64_lanes, f64), @ptrCast(@alignCast(&rp[i]))).* = src[i..][0..simd.f64_lanes].* + s;
        }
    }

    while (i < n) : (i += 1) {
        rp[i] = src[i] + scalar;
    }

    return result;
}
