const R = @import("R");
const convert = @import("convert");
const altrep_create = @import("altrep_create");

const SEXP = R.SEXP;
const max_len = 1_000_000;
const BenchAltInteger = altrep_create.AltInteger("zigrbench", "bench_altinteger_owned_max");

var backing: [max_len]i32 = undefined;
var initialized_len: usize = 0;

fn ensureBacking(n: usize) void {
    if (initialized_len >= n) return;
    for (initialized_len..n) |i| {
        backing[i] = @intCast((i % 1024) + 1);
    }
    initialized_len = n;
}

export fn zigr_bench_owned_altrep_int_max(n_sexp: SEXP) SEXP {
    const n_int = R.Rf_asInteger(n_sexp);
    if (n_int < 0) return R.R_NilValue;

    const n = @as(usize, @intCast(n_int));
    if (n > max_len) return R.R_NilValue;

    ensureBacking(n);
    const vec = BenchAltInteger.init(backing[0..n]);
    return R.Rf_ScalarInteger(convert.maxInt(vec));
}
