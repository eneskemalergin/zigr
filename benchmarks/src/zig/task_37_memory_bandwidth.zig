const std = @import("std");
const R = @import("R");
const raw = @import("raw");

const SEXP = R.SEXP;
const repeats: usize = 2;
const strategy_names = [_][:0]const u8{ "copy_temp", "copy_out", "fill_out" };

fn sumSlice(data: []const f64) f64 {
    var total: f64 = 0.0;
    for (data) |value| total += value;
    return total;
}

fn setNames(result: SEXP) void {
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, strategy_names.len));
    defer R.Rf_unprotect(1);

    for (strategy_names, 0..) |name, index| {
        R.SET_STRING_ELT(names, @intCast(index), R.Rf_mkChar(name));
    }
    _ = R.Rf_setAttrib(result, R.R_NamesSymbol, names);
}

export fn zigr_bench_memory_bandwidth(vec: SEXP) SEXP {
    const input = raw.real(vec);
    const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, strategy_names.len));
    defer R.Rf_unprotect(1);
    const out = R.REAL(result);

    var copy_temp_total: f64 = 0.0;
    var copy_out_total: f64 = 0.0;
    var fill_out_total: f64 = 0.0;

    for (0..repeats) |_| {
        const temp = std.heap.c_allocator.alloc(f64, input.len) catch unreachable;
        defer std.heap.c_allocator.free(temp);
        std.mem.copyForwards(f64, temp, input);
        copy_temp_total += sumSlice(temp);

        const copy_out = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(input.len)));
        const copy_slice = @as([*]f64, @ptrCast(R.REAL(copy_out)))[0..input.len];
        std.mem.copyForwards(f64, copy_slice, input);
        copy_out_total += sumSlice(copy_slice);
        R.Rf_unprotect(1);

        const fill_out = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(input.len)));
        const fill_slice = @as([*]f64, @ptrCast(R.REAL(fill_out)))[0..input.len];
        for (input, 0..) |value, index| {
            fill_slice[index] = value + 0.5;
        }
        fill_out_total += sumSlice(fill_slice);
        R.Rf_unprotect(1);
    }

    out[0] = copy_temp_total;
    out[1] = copy_out_total;
    out[2] = fill_out_total;
    setNames(result);
    return result;
}
