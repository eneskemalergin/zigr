//! R interrupt and stack checks.
//!
//! Interrupt checks can longjmp, so native cleanup must already be armed.

const builtin = @import("builtin");
const R = @import("R");

pub fn checkInterrupt() void {
    R.R_CheckUserInterrupt();
}

pub fn checkStack() void {
    R.R_CheckStack();
}

pub fn checkStack2(frameSize: usize) void {
    R.R_CheckStack2(@intCast(frameSize));
}

pub const StackChecker = switch (builtin.mode) {
    .Debug, .ReleaseSafe => true,
    else => false,
};

pub fn longRunning(comptime func: *const fn (usize) bool) void {
    var chunk: usize = 0;
    while (true) {
        checkInterrupt();
        if (!func(chunk)) break;
        chunk += 1;
    }
}
