const zigr = @import("zigr");
const R = @import("R");

fn vectorSum(slice: []const f64) f64 {
    var total: f64 = 0;
    for (slice) |v| total += v;
    return total;
}

const Exports = zigr.@"export".generateExports(&.{
    .{ .name = "C_vector_sum", .func = vectorSum },
}, &.{});

comptime {
    @export(@as(*const anyopaque, @ptrCast(&Exports.init)), .{ .name = "R_init_mypackage", .linkage = .strong });
    @export(@as(*const anyopaque, @ptrCast(&Exports.unload)), .{ .name = "R_unload_mypackage", .linkage = .strong });
}
