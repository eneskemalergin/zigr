const std = @import("std");
const R = @import("R");
const raw = @import("raw");

const SEXP = R.SEXP;
const allocation_repeats: usize = 100;

export fn zigr_bench_arena_vs_rmalloc(vec: SEXP) SEXP {
    const input = raw.real(vec);
    const temp = std.heap.page_allocator.alloc(f64, input.len) catch @panic("scratch allocation failed");
    defer std.heap.page_allocator.free(temp);
    var total: f64 = 0.0;

    for (0..allocation_repeats) |repeat_index| {
        const bias = (@as(f64, @floatFromInt(repeat_index)) + 1.0) * 0.001;

        for (input, 0..) |value, offset| {
            temp[offset] = value + bias;
        }
        for (temp) |value| total += value;
    }

    return R.Rf_ScalarReal(total);
}
