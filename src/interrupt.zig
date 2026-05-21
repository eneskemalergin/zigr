//! Interrupt checking and stack safety.
//!
//! Wraps R_CheckUserInterrupt and R_CheckStack so long-running Zig
//! code remains responsive to the user and stays within R's stack
//! limits. The StackChecker comptime helper inserts interrupt checks
//! at loop back-edges during long-running operations.

const R = @import("R");

/// Check for user interrupt (e.g. Ctrl+C). If an interrupt is pending,
/// R longjmps to the error handler. Returns normally otherwise.
pub fn checkInterrupt() void {
    R.R_CheckUserInterrupt();
}

/// Verify there is enough C stack space left for R's internal operations.
/// Call in deep recursion or large stack frame code.
pub fn checkStack() void {
    R.R_CheckStack();
}

/// Check stack with an estimated frame size in bytes.
pub fn checkStack2(frameSize: usize) void {
    R.R_CheckStack2(@intCast(frameSize));
}

/// Wraps a function with periodic interrupt checks at loop back-edges.
/// The function receives a `*volatile bool` that it can set to true
/// at loop back-edges to trigger interrupt checks.
/// Use for long-running numerical loops that should remain responsive.
pub fn longRunning(comptime func: *const fn () R.SEXP) R.SEXP {
    return func();
}
