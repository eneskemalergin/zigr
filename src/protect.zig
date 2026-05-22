//! PROTECT / UNPROTECT wrappers.
//!
//! R's protection stack is strictly LIFO. Protect pushes, unprotect pops
//! the top. There is no random-access unprotect in the public R API.
//! The depth counter tracks balance for leak detection, not stack position.

const std = @import("std");
const builtin = @import("builtin");
const SEXP = @import("sexp.zig").SEXP;
const R = @import("R");

threadlocal var depth: i32 = 0;
const leak_warn_threshold = 64;

/// Push a SEXP onto R's protection stack. Each push must be paired with
/// exactly one unprotect() call on the normal return path.
pub fn protect(value: SEXP) SEXP {
    if (builtin.mode == .Debug and depth > leak_warn_threshold) {
        std.log.warn("protect depth is {} (possible leak)", .{depth});
    }
    depth += 1;
    return R.Rf_protect(value);
}

/// Pop one entry from R's protection stack (LIFO). Must match a prior
/// protect() call. Calling unprotect more times than protect will
/// unbalance the stack and crash R.
pub fn unprotect() void {
    R.Rf_unprotect(1);
    depth -= 1;
    if (builtin.mode == .Debug and depth < 0) {
        std.log.warn("protect depth went negative ({})", .{depth});
    }
}

/// Indexed protect: records the stack position so the SEXP can be
/// replaced later via reprotect(). The index is not for random-access
/// unprotect (use unprotect() for that).
pub fn protectWithIndex(value: SEXP, index: *i32) void {
    var ri: R.PROTECT_INDEX = undefined;
    R.R_ProtectWithIndex(value, &ri);
    index.* = @intCast(ri);
    depth += 1;
    if (builtin.mode == .Debug and depth > leak_warn_threshold) {
        std.log.warn("protect depth is {} (possible leak)", .{depth});
    }
}

/// Replace the value at the given protection index without changing the
/// stack depth. Used with protectWithIndex to update a protected SEXP
/// while keeping its stack position.
pub fn reprotect(value: SEXP, index: i32) void {
    R.R_Reprotect(value, @intCast(index));
}

/// Read the current protection depth. Useful for leak detection in
/// debug-mode R sessions.
pub fn getDepth() i32 {
    return depth;
}
