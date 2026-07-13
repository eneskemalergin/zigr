//! Representative callback sequence for a package-registered owned integer ALTREP.
//!
//! The exported construction name preserves task 49's stable manifest and routine identity.

const std = @import("std");
const R = @import("R");
const zigr = @import("zigr");

const BenchAltInteger = zigr.altrep_create.AltInteger("zigr_benchmarks", "benchmark_owned_integer");
const REGION_CAPACITY = 4096;

pub fn register(info: *R.DllInfo) void {
    BenchAltInteger.register(info);
}

fn isBenchmarkClass(value: R.SEXP) bool {
    return R.TYPEOF(value) == R.INTSXP and
        R.ALTREP(value) != 0 and
        std.mem.eql(u8, zigr.altrep.className(value), "benchmark_owned_integer") and
        std.mem.eql(u8, zigr.altrep.classPackage(value), "zigr_benchmarks");
}

fn numericScalar(value: R.SEXP) f64 {
    const value_type = R.TYPEOF(value);
    if (value_type != R.INTSXP and value_type != R.REALSXP) {
        zigr.@"error".signal("owned ALTREP callback benchmark received a non-numeric summary");
    }
    if (R.XLENGTH(value) != 1) {
        zigr.@"error".signal("owned ALTREP callback benchmark expected a scalar summary");
    }
    return switch (value_type) {
        R.INTSXP => blk: {
            const result = R.INTEGER_ELT(value, 0);
            if (result == R.R_NaInt) {
                zigr.@"error".signal("owned ALTREP callback benchmark received an NA summary");
            }
            break :blk @floatFromInt(result);
        },
        R.REALSXP => blk: {
            const result = R.REAL_ELT(value, 0);
            if (R.ISNA(result) != 0 or R.ISNAN(result)) {
                zigr.@"error".signal("owned ALTREP callback benchmark received a missing summary");
            }
            break :blk result;
        },
        else => unreachable,
    };
}

fn summary(name: []const u8, value: R.SEXP) R.SEXP {
    const args = [_]R.SEXP{value};
    return zigr.eval.callIn(name, args[0..], R.R_BaseEnv);
}

fn regionSum(value: R.SEXP) i64 {
    var buffer: [REGION_CAPACITY]i32 = undefined;
    const length = R.XLENGTH(value);
    var offset: R.R_xlen_t = 0;
    var total: i64 = 0;
    while (offset < length) {
        const requested = @min(length - offset, @as(R.R_xlen_t, @intCast(buffer.len)));
        const received = R.INTEGER_GET_REGION(value, offset, requested, buffer[0..].ptr);
        if (received <= 0 or received > requested) {
            zigr.@"error".signal("owned ALTREP callback benchmark received an invalid region");
        }
        for (buffer[0..@as(usize, @intCast(received))]) |item| total += item;
        offset += received;
    }
    return total;
}

export fn zigr_bench_owned_altrep_create(length_sxp: R.SEXP) R.SEXP {
    const len = zigr.convert.toIntScalar(length_sxp) catch |error_value|
        zigr.convert.signalError(error_value);
    if (len <= 0) {
        zigr.@"error".signal("owned ALTREP callback benchmark expected a positive length");
    }

    var source = zigr.protect.scoped(R.Rf_allocVector(R.INTSXP, len));
    defer source.deinit();
    const values = R.INTEGER(source.get())[0..@as(usize, @intCast(len))];
    for (values, 0..) |*value, index| value.* = @intCast(index + 1);

    var result = zigr.protect.scoped(BenchAltInteger.init(values));
    defer result.deinit();
    if (!isBenchmarkClass(result.get()) or R.XLENGTH(result.get()) != len) {
        zigr.@"error".signal("owned ALTREP callback benchmark constructed an invalid result");
    }

    var sum_result = zigr.protect.scoped(summary("sum", result.get()));
    defer sum_result.deinit();
    var min_result = zigr.protect.scoped(summary("min", result.get()));
    defer min_result.deinit();
    var max_result = zigr.protect.scoped(summary("max", result.get()));
    defer max_result.deinit();
    const expected_sum = @divExact(@as(i64, len) * (@as(i64, len) + 1), 2);
    const sum_value = numericScalar(sum_result.get());
    const min_value = numericScalar(min_result.get());
    const max_value = numericScalar(max_result.get());
    if (sum_value != @as(f64, @floatFromInt(expected_sum)) or min_value != 1.0 or max_value != @as(f64, @floatFromInt(len))) {
        zigr.@"error".signal("owned ALTREP callback benchmark received invalid summaries");
    }

    const region_sum = regionSum(result.get());
    if (region_sum != expected_sum) {
        zigr.@"error".signal("owned ALTREP callback benchmark received invalid region values");
    }

    var deep = zigr.protect.scoped(R.Rf_duplicate(result.get()));
    defer deep.deinit();
    var shallow = zigr.protect.scoped(R.Rf_shallow_duplicate(result.get()));
    defer shallow.deinit();
    if (R.TYPEOF(deep.get()) != R.INTSXP or R.ALTREP(deep.get()) != 0 or R.XLENGTH(deep.get()) != len or
        R.TYPEOF(shallow.get()) != R.INTSXP or R.ALTREP(shallow.get()) != 0 or R.XLENGTH(shallow.get()) != len)
    {
        zigr.@"error".signal("owned ALTREP callback benchmark received invalid duplicates");
    }
    R.INTEGER(deep.get())[0] = -1;
    R.INTEGER(shallow.get())[@as(usize, @intCast(len - 1))] = -2;
    if (R.INTEGER_ELT(result.get(), 0) != 1 or R.INTEGER_ELT(result.get(), len - 1) != len or
        R.INTEGER(deep.get())[0] != -1 or R.INTEGER(shallow.get())[@as(usize, @intCast(len - 1))] != -2 or
        (len > 1 and (R.INTEGER(deep.get())[@as(usize, @intCast(len - 1))] != len or R.INTEGER(shallow.get())[0] != 1)))
    {
        zigr.@"error".signal("owned ALTREP callback benchmark duplicates are not independent");
    }

    const sortedness = R.INTEGER_IS_SORTED(result.get());
    const no_na = R.INTEGER_NO_NA(result.get());
    if (sortedness != R.SORTED_INCR or no_na != 1) {
        zigr.@"error".signal("owned ALTREP callback benchmark received invalid metadata");
    }

    var serialized = zigr.protect.scoped(zigr.serialize.toVector(result.get()));
    defer serialized.deinit();
    var restored = zigr.protect.scoped(zigr.serialize.fromVector(serialized.get()));
    defer restored.deinit();
    if (!isBenchmarkClass(restored.get()) or R.XLENGTH(restored.get()) != len or
        R.INTEGER_ELT(restored.get(), 0) != 1 or R.INTEGER_ELT(restored.get(), len - 1) != len)
    {
        zigr.@"error".signal("owned ALTREP callback benchmark restored an invalid class");
    }

    const names = [_][]const u8{
        "sum",
        "min",
        "max",
        "region_sum",
        "sortedness",
        "no_na",
        "deep_first",
        "shallow_last",
        "restored_first",
        "restored_last",
    };
    var output = zigr.protect.scoped(R.Rf_allocVector(R.REALSXP, names.len));
    defer output.deinit();
    const output_values = R.REAL(output.get());
    output_values[0] = sum_value;
    output_values[1] = min_value;
    output_values[2] = max_value;
    output_values[3] = @floatFromInt(region_sum);
    output_values[4] = @floatFromInt(sortedness);
    output_values[5] = @floatFromInt(no_na);
    output_values[6] = @floatFromInt(R.INTEGER(deep.get())[0]);
    output_values[7] = @floatFromInt(R.INTEGER(shallow.get())[@as(usize, @intCast(len - 1))]);
    output_values[8] = @floatFromInt(R.INTEGER_ELT(restored.get(), 0));
    output_values[9] = @floatFromInt(R.INTEGER_ELT(restored.get(), len - 1));
    zigr.attrib.setNames(output.get(), names[0..]);
    return output.get();
}
