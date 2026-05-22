//! Interrupt checking and stack safety.
//!
//! Wraps R_CheckUserInterrupt and R_CheckStack so long-running Zig
//! code remains responsive to the user. Call checkInterrupt inside
//! hot loops, or use StackChecker to gate those calls at comptime.

const builtin = @import("builtin");
const R = @import("R");

/// Check for user interrupt (Ctrl+C). If pending, R longjmps to
/// the error handler. Call inside long-running loops.
pub fn checkInterrupt() void {
    R.R_CheckUserInterrupt();
}

/// Verify enough C stack space remains for R internal operations.
/// Call in deep recursion or large stack frame code.
pub fn checkStack() void {
    R.R_CheckStack();
}

/// Check stack with an estimated frame size in bytes.
pub fn checkStack2(frameSize: usize) void {
    R.R_CheckStack2(@intCast(frameSize));
}

/// Comptime gate for interrupt checks. Enabled in debug and release-safe
/// builds where responsiveness matters more than raw speed.
pub const StackChecker = switch (builtin.mode) {
    .Debug, .ReleaseSafe => true,
    else => false,
};

/// Wraps a chunk-based function with interrupt checks between chunks.
/// The function receives a chunk index and returns `true` if there is
/// more work, `false` when done. Interrupts are checked before each
/// chunk so the user can cancel long computations gracefully.
///
/// Usage:
///   try interrupt.longRunning(struct {
///       fn work(chunk: usize) bool {
///           const start = chunk * chunk_size;
///           const end = @min(start + chunk_size, total);
///           for (start..end) |i| { /* process i */ }
///           return end < total;  // more chunks?
///       }
///   }.work);
pub fn longRunning(comptime func: *const fn (usize) bool) void {
    var chunk: usize = 0;
    while (true) {
        checkInterrupt();
        if (!func(chunk)) break;
        chunk += 1;
    }
}
