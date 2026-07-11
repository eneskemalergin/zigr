//! R RNG state.
//!
//! `withRng` releases R's state even when R longjmps.

const std = @import("std");
const R = @import("R");
const cleanup = @import("cleanup");

fn releaseRng(_: ?*anyopaque) void {
    R.PutRNGstate();
}

pub fn acquire() void {
    R.GetRNGstate();
}

pub fn release() void {
    R.PutRNGstate();
}

/// The cleanup frame releases R's state after a non-local exit.
pub fn withRng(comptime func: *const fn () R.SEXP) R.SEXP {
    acquire();
    cleanup.pushFrame(releaseRng, null);
    const result = func();
    cleanup.popFrame();
    release();
    return result;
}

test "withRng type" {
    try std.testing.expectEqual(@TypeOf(withRng), fn (comptime *const fn () R.SEXP) R.SEXP);
}

test "acquire and release types" {
    try std.testing.expectEqual(@TypeOf(acquire), fn () void);
    try std.testing.expectEqual(@TypeOf(release), fn () void);
}
