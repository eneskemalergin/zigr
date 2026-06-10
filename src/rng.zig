//! RNG state management.
//!
//! Wraps GetRNGstate / PutRNGstate so exported functions that use
//! R's random number generator properly acquire and release the RNG.
//! withRng uses a cleanup frame to ensure release fires even when
//! the wrapped function longjmps (Rf_error).

const std = @import("std");
const R = @import("R");
const cleanup = @import("cleanup");

fn releaseRng(_: ?*anyopaque) void {
    R.PutRNGstate();
}

/// Acquire R's RNG. Must be called before calling unif_rand, norm_rand,
/// or any R random number generation function.
pub fn acquire() void {
    R.GetRNGstate();
}

/// Release R's RNG. Must be called after finishing random number
/// generation to restore R's internal state.
pub fn release() void {
    R.PutRNGstate();
}

/// Call a zero-argument function with R's RNG acquired and a cleanup
/// frame armed. On normal return the frame pops silently. On longjmp
/// (Rf_error) the cleanup fires release() before the unwind propagates.
pub fn withRng(comptime func: *const fn () R.SEXP) R.SEXP {
    cleanup.pushFrame(releaseRng, null);
    acquire();
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
