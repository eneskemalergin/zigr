//! R error and warning helpers.
//!
//! R errors longjmp and do not return to Zig.

const std = @import("std");
const R = @import("R");

fn cstr(buf: *[4096:0]u8, s: []const u8) [*c]const u8 {
    const n = @min(s.len, buf.len - 1);
    @memcpy(buf[0..n], s[0..n]);
    buf[n] = 0;
    return @ptrCast(buf);
}

pub fn signal(msg: []const u8) noreturn {
    var buf: [4096:0]u8 = undefined;
    R.Rf_error("%s", cstr(&buf, msg));
}

pub fn warn(msg: []const u8) void {
    var buf: [4096:0]u8 = undefined;
    R.Rf_warning("%s", cstr(&buf, msg));
}

pub fn signalIf(condition: bool, msg: []const u8) void {
    if (condition) signal(msg);
}

pub fn toR(result: anytype) @TypeOf(result catch unreachable) {
    return result catch |err| {
        signal(@errorName(err));
    };
}

test "toR error union compile guard" {
    const r1: anyerror!i32 = 42;
    const v1 = toR(r1);
    try std.testing.expectEqual(v1, 42);
}
