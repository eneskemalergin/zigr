const R = @import("R");
const altrep_create = @import("altrep_create");

const SEXP = R.SEXP;
const max_len = 1_000_000;
const BenchAltReal = altrep_create.AltReal("zigrbench", "bench_altreal_create");

var backing: [max_len]f64 = undefined;
var initialized_len: usize = 0;

fn ensureBacking(n: usize) void {
    if (initialized_len >= n) return;
    for (initialized_len..n) |i| {
        backing[i] = @as(f64, @floatFromInt(i + 1));
    }
    initialized_len = n;
}

/// Create an ALTREP vector backed by Zig memory.
///
/// This measures the overhead of creating a custom ALTREP object, not
/// materializing or traversing it.
export fn zigr_bench_altrep_create(n_sexp: SEXP) SEXP {
    const n_int = R.Rf_asInteger(n_sexp);
    if (n_int < 0) return R.R_NilValue;

    const n = @as(usize, @intCast(n_int));
    if (n > max_len) return R.R_NilValue;

    ensureBacking(n);
    return BenchAltReal.init(backing[0..n]);
}
