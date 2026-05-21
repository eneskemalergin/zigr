//! PROTECT / UNPROTECT wrappers.
//!
//! R's GC frees any SEXP not on the protection stack. Every SEXP crossing
//! the R boundary must be protected before any allocating R API call.
//! Unprotected SEXPs cause use-after-free crashes that are hard to debug.
//!
//! HACK: This module does NOT call R's actual PROTECT yet. The extern
//! declarations below are for when zigr links against libR. Until then,
//! protect() is a no-op counter. Using it in real R interop will crash.

const std = @import("std");
const SEXP = @import("sexp.zig").SEXP;

// R's C PROTECT/UNPROTECT functions. Not linked until zigr is loaded
// into an R process. These are here so the types are right when I wire
// them up - right now they're dead code.
extern fn Rf_protect(value: SEXP) SEXP;
extern fn Rf_unprotect(n: c_int) void;
extern fn R_ProtectWithIndex(value: SEXP, index: *c_int) void;
extern fn R_UnprotectFromIndex(index: c_int) void;

// HACK: shadow depth counter. Does not register SEXPs with R's GC.
// Replace with Rf_protect/Rf_unprotect calls when libR is linked.
var protect_depth: i32 = 0;

/// Handle returned by `protect()`. Call `unprotect()` when the SEXP no
/// longer needs protection. Leaking handles bloats R's protection stack.
pub const Protection = struct {
    index: i32,

    /// Push a SEXP onto the protection stack.
    /// HACK: This does NOT call Rf_protect. The SEXP is not registered
    /// with R's GC. Real protection requires linking libR.
    pub fn protect(value: SEXP) Protection {
        _ = value;
        const p = Protection{ .index = protect_depth };
        protect_depth += 1;
        return p;
    }

    pub fn protectWithIndex(value: SEXP, index: *i32) void {
        _ = value;
        _ = index;
    }

    /// Pop the stack. After this, R may collect the SEXP at any time.
    /// Accessing it without re-protecting is a use-after-free bug.
    pub fn unprotect(self: Protection) void {
        _ = self;
        protect_depth -= 1;
    }
};

test "protect depth tracking" {
    // The counter moves but the SEXP is never actually protected.
    // Passes without R running, which is exactly the problem.
    const p1 = Protection.protect(null);
    const p2 = Protection.protect(null);
    p2.unprotect();
    p1.unprotect();
    try std.testing.expectEqual(protect_depth, 0);
}

test "protect underflow does not panic" {
    // Unprotected SEXPs are not caught here either. In real R an
    // underflow is a warning, not a crash.
    const p = Protection.protect(null);
    p.unprotect();
    // second unprotect: depth goes negative. R tolerates this.
    p.unprotect();
}
