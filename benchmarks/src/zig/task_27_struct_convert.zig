const std = @import("std");
const R = @import("R");
const convert = @import("convert");

const SEXP = R.SEXP;

const StructPayload = struct {
    id: i32,
    count: i32,
    level: i32,
    flag: bool,
    enabled: bool,
    ratio: f64,
    offset: f64,
    scale: f64,
    weights: []const f64,
    indices: []const i32,
};

export fn zigr_bench_struct_convert(input_sexp: SEXP) SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const payload = convert.fromSEXP(StructPayload, input_sexp, arena.allocator());
    return convert.asSEXP(payload);
}
