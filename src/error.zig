//! Error and warning signaling helpers.
//!
//! Wraps Rf_error and Rf_warning so Zig code can signal conditions
//! to R without calling the C API directly. Also provides toR for
//! translating Zig error unions into R stop() calls.

const std = @import("std");
const R = @import("R");

/// Signal an error to R. Calls Rf_error which never returns (C longjmp).
pub fn signal(msg: []const u8) noreturn {
    const cstr: [*c]const u8 = @ptrCast(msg.ptr);
    R.Rf_error(cstr);
}

/// Signal a warning to R. R prints the message and continues execution.
pub fn warn(msg: []const u8) void {
    const cstr: [*c]const u8 = @ptrCast(msg.ptr);
    R.Rf_warning(cstr);
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
