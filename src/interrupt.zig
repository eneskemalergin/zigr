//! Interrupt checking and stack safety.
//!
//! Wraps R_CheckUserInterrupt and R_CheckStack so long-running Zig
//! code remains responsive to the user. Call checkInterrupt inside
//! hot loops, or use StackChecker to gate those calls at comptime.

const builtin = @import("builtin");
const R = @import("R");

/// R_CheckUserInterrupt longjmps if Ctrl+C was pressed. Call inside tight
/// loops so long computations are cancellable, not just between them.
pub fn checkInterrupt() void {
    R.R_CheckUserInterrupt();
}

/// R_CheckStack aborts if the C stack is near overflow. Call before deep
/// recursion or large alloca frames to get a clear error instead of a SEGV.
pub fn checkStack() void {
    R.R_CheckStack();
}

/// Like checkStack but accounts for a specific additional frame size.
pub fn checkStack2(frameSize: usize) void {
    R.R_CheckStack2(@intCast(frameSize));
}

/// Comptime gate for interrupt checks. Enabled in debug and release-safe
/// builds where responsiveness matters more than raw speed.
pub const StackChecker = switch (builtin.mode) {
    .Debug, .ReleaseSafe => true,
    else => false,
};

/// Wraps a chunk-based function with interrupt checks between chunks. The function receives a chunk index and returns `true` if there is more work. Interrupts are checked before each chunk so the user can cancel long computations.
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
