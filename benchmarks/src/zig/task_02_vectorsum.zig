const R = @import("R");
const simd = @import("simd");
const raw = @import("raw");

const SEXP = R.SEXP;

export fn zigr_bench_vectorsum(vec: SEXP) SEXP {
    const slice = raw.real(vec);
    var total: f64 = 0.0;

    var i: usize = 0;
    const n = slice.len;

    const lanes = simd.f64_lanes;
    if (n >= lanes) {
        var vec_total: @Vector(lanes, f64) = @splat(0.0);
        const end = n - (n % lanes);
        while (i < end) : (i += lanes) {
            vec_total += slice[i..][0..lanes].*;
        }
        total += @reduce(.Add, vec_total);
    }

    while (i < n) : (i += 1) {
        total += slice[i];
    }

    return R.Rf_ScalarReal(total);
}
