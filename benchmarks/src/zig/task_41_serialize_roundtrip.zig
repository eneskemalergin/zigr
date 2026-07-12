const R = @import("R");
const zigr = @import("zigr");

export fn zigr_bench_serialize_roundtrip(vec: R.SEXP) R.SEXP {
    const serialized = R.Rf_protect(zigr.serialize.toVector(vec));
    defer R.Rf_unprotect(1);
    const result = R.Rf_protect(zigr.serialize.fromVector(serialized));
    defer R.Rf_unprotect(1);

    const n = R.XLENGTH(result);
    const xp: [*]const f64 = @ptrCast(R.REAL(result));
    var total: f64 = 0.0;
    for (0..@as(usize, @intCast(n))) |i| total += xp[i];
    return R.Rf_ScalarReal(total);
}
