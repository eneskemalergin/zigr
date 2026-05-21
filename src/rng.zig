//! RNG state management.
//!
//! Wraps GetRNGstate / PutRNGstate so exported functions that use
//! R's random number generator properly acquire and release the RNG.
//!
//! For functions that may error (call Rf_error), use acquire/release
//! manually with cleanup frames rather than withRng, because R longjmp
//! bypasses normal scope exit.

const R = @import("R");

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

/// Call a zero-argument function with R's RNG acquired on entry and
/// released on return. Does NOT handle R longjmp: if the function may
/// error, use acquire/release manually with cleanup frames.
pub fn withRng(comptime func: *const fn () R.SEXP) R.SEXP {
    acquire();
    const result = func();
    release();
    return result;
}
