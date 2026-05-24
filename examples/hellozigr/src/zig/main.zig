const R = @import("R");
const zigr = @import("zigr");

fn vectorSum(slice: []const f64) f64 {
    var total: f64 = 0;
    for (slice) |v| total += v;
    return total;
}

fn stringTotalBytes(strings: zigr.convert.StringSliceView) i32 {
    var total: usize = 0;
    var it = strings.iterator();
    while (it.next()) |s| total += s.len;
    return @intCast(total);
}

const Exports = zigr.@"export".generateExports(&.{
    .{ .name = "C_vector_sum", .func = vectorSum },
    .{ .name = "C_string_total_bytes", .func = stringTotalBytes },
}, &.{});

comptime {
    @export(@as(*const anyopaque, @ptrCast(&Exports.init)), .{ .name = "R_init_hellozigr", .linkage = .strong });
    @export(@as(*const anyopaque, @ptrCast(&Exports.unload)), .{ .name = "R_unload_hellozigr", .linkage = .strong });
}
