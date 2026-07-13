const std = @import("std");
const R = @import("R");
const zigr = @import("zigr");

const BenchAltInteger = zigr.altrep_create.AltInteger("zigr_benchmarks", "benchmark_owned_integer");

pub fn register(info: *R.DllInfo) void {
    BenchAltInteger.register(info);
}

export fn zigr_bench_owned_altrep_create(length_sxp: R.SEXP) R.SEXP {
    const len = R.Rf_asInteger(length_sxp);
    if (len < 0 or len == R.R_NaInt) zigr.@"error".signal("owned ALTREP benchmark expected a non-negative length");

    var source = zigr.protect.scoped(R.Rf_allocVector(R.INTSXP, len));
    defer source.deinit();
    const values = R.INTEGER(source.get())[0..@as(usize, @intCast(len))];
    for (values, 0..) |*value, index| value.* = @intCast(index + 1);

    var result = zigr.protect.scoped(BenchAltInteger.init(values));
    defer result.deinit();
    if (R.TYPEOF(result.get()) != R.INTSXP or R.ALTREP(result.get()) == 0 or R.XLENGTH(result.get()) != len) {
        zigr.@"error".signal("owned ALTREP benchmark constructed an invalid result");
    }
    if (!std.mem.eql(u8, zigr.altrep.className(result.get()), "benchmark_owned_integer") or
        !std.mem.eql(u8, zigr.altrep.classPackage(result.get()), "zigr_benchmarks"))
    {
        zigr.@"error".signal("owned ALTREP benchmark used the wrong registered class");
    }
    if (len > 0 and (R.INTEGER_ELT(result.get(), 0) != 1 or R.INTEGER_ELT(result.get(), len - 1) != len)) {
        zigr.@"error".signal("owned ALTREP benchmark constructed invalid values");
    }
    return result.get();
}
