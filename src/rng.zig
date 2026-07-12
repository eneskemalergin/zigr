//! R RNG state.
//!
//! `withRng` releases R's state even when R longjmps.

const std = @import("std");
const R = @import("R");
const cleanup = @import("cleanup");
const err = @import("error");

threadlocal var active = false;

const RngCleanup = struct {
    acquired: bool = false,

    fn fire(self: *@This()) void {
        if (self.acquired) R.PutRNGstate();
        active = false;
    }
};

pub fn acquire() void {
    R.GetRNGstate();
}

pub fn release() void {
    R.PutRNGstate();
}

/// The cleanup frame releases R's state after a non-local exit.
pub fn withRng(comptime func: *const fn () R.SEXP) R.SEXP {
    return cleanup.protectCall(struct {
        fn call() R.SEXP {
            if (active) err.signal("nested withRng calls are not supported");
            const state = cleanup.pushFrameInline(RngCleanup, .{}, RngCleanup.fire);
            active = true;
            acquire();
            state.acquired = true;
            const result = func();
            release();
            state.acquired = false;
            active = false;
            cleanup.popFrame();
            return result;
        }
    }.call);
}

test "withRng type" {
    try std.testing.expectEqual(@TypeOf(withRng), fn (comptime *const fn () R.SEXP) R.SEXP);
}

test "acquire and release types" {
    try std.testing.expectEqual(@TypeOf(acquire), fn () void);
    try std.testing.expectEqual(@TypeOf(release), fn () void);
}
