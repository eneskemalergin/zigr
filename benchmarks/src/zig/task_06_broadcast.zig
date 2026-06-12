const R = @import("R");
const simd = @import("simd");
const raw = @import("zigr").raw;

const SEXP = R.SEXP;

export fn zigr_bench_broadcast(vec_sexp: SEXP, scalar_sexp: SEXP) SEXP {
    const src = raw.real(vec_sexp);
    const scalar = raw.real(scalar_sexp)[0];
    const n = src.len;

    const lanes = simd.f64_lanes;
    const s: @Vector(lanes, f64) = @splat(scalar);
    var i: usize = 0;
    var vec_total: @Vector(lanes, f64) = @splat(0.0);
    if (n >= lanes) {
        const end = n - (n % lanes);
        while (i < end) : (i += lanes) {
            vec_total += src[i..][0..lanes].* + s;
        }
    }
    var total = @reduce(.Add, vec_total);
    while (i < n) : (i += 1) total += src[i] + scalar;
    return R.Rf_ScalarReal(total);
}
