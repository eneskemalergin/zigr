const R = @import("R");
const zigr = @import("zigr");

export fn zigr_bench_weakref_lifecycle(count_sxp: R.SEXP) R.SEXP {
    const count = R.Rf_asInteger(count_sxp);
    if (count < 0 or count == R.R_NaInt) zigr.@"error".signal("weak-reference benchmark expected a non-negative count");

    var key = zigr.protect.scoped(R.R_NewEnv(R.R_EmptyEnv, 0, 29));
    defer key.deinit();
    var value = zigr.protect.scoped(R.Rf_ScalarReal(42.0));
    defer value.deinit();

    for (0..@as(usize, @intCast(count))) |_| {
        var reference = zigr.protect.scoped(zigr.weakref.make(key.get(), value.get(), null, false));
        defer reference.deinit();
        const stored = zigr.weakref.value(reference.get());
        if (zigr.weakref.key(reference.get()) != key.get() or
            R.TYPEOF(stored) != R.REALSXP or R.XLENGTH(stored) != 1 or R.REAL(stored)[0] != 42.0)
        {
            return R.R_NilValue;
        }
    }
    return R.Rf_ScalarInteger(count);
}
