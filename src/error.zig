//! Error and warning signaling helpers.
//!
//! Wraps Rf_error and Rf_warning so Zig code can signal conditions
//! to R without calling the C API directly. Also provides toR for
//! translating Zig error unions into R stop() calls.

const std = @import("std");
const R = @import("R");

fn cstr(buf: *[4096:0]u8, s: []const u8) [*c]const u8 {
    const n = @min(s.len, buf.len - 1);
    @memcpy(buf[0..n], s[0..n]);
    buf[n] = 0;
    return @ptrCast(buf);
}

/// Signal an error to R. Calls Rf_error which never returns (C longjmp).
pub fn signal(msg: []const u8) noreturn {
    var buf: [4096:0]u8 = undefined;
    R.Rf_error(cstr(&buf, msg));
}

/// Signal a warning to R. R prints the message and continues execution.
pub fn warn(msg: []const u8) void {
    var buf: [4096:0]u8 = undefined;
    R.Rf_warning(cstr(&buf, msg));
}

/// If condition is true, signal an error with the given message.
pub fn signalIf(condition: bool, msg: []const u8) void {
    if (condition) signal(msg);
}

/// Unwrap a Zig error union. On success, return the value. On error,
/// signal the error name to R (which longjmps and never returns).
pub fn toR(result: anytype) @TypeOf(result catch unreachable) {
    return result catch |err| {
        signal(@errorName(err));
    };
}

test "toR error union compile guard" {
    // Should compile: result is an error union
    const r1: anyerror!i32 = 42;
    const v1 = toR(r1);
    try std.testing.expectEqual(v1, 42);
}
