const R = @import("R");
const convert = @import("convert");
const altrep_create = @import("altrep_create");

const SEXP = R.SEXP;
const max_len = 1_000_000;
const BenchAltReal = altrep_create.AltReal("zigrbench", "bench_altreal_owned_sum");

var backing: [max_len]f64 = undefined;
var initialized_len: usize = 0;

fn ensureBacking(n: usize) void {
    if (initialized_len >= n) return;
    for (initialized_len..n) |i| {
        backing[i] = @as(f64, @floatFromInt(i + 1));
    }
    initialized_len = n;
}

export fn zigr_bench_owned_altrep_real_sum(n_sexp: SEXP) SEXP {
    const n_int = R.Rf_asInteger(n_sexp);
    if (n_int < 0) return R.R_NilValue;

    const n = @as(usize, @intCast(n_int));
    if (n > max_len) return R.R_NilValue;

    ensureBacking(n);
    const vec = BenchAltReal.init(backing[0..n]);
    return R.Rf_ScalarReal(convert.sum(vec));
}
