//! Error and warning signaling helpers.
//!
//! Wraps Rf_error and Rf_warning so Zig code can signal conditions
//! to R without calling the C API directly.

const R = @import("R");

/// Signal an error to R. Calls Rf_error which never returns (C longjmp).
pub fn signal(msg: []const u8) void {
    R.Rf_error(@ptrCast(msg.ptr));
}

/// Signal a warning to R. R prints the message and continues execution.
pub fn warn(msg: []const u8) void {
    R.Rf_warning(@ptrCast(msg.ptr));
}

/// If condition is true, signal an error with the given message.
pub fn signalIf(condition: bool, msg: []const u8) void {
    if (condition) signal(msg);
}
