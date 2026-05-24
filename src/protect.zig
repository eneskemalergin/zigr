//! PROTECT / UNPROTECT wrappers.
//!
//! R's protection stack is strictly LIFO. Protect pushes, unprotect pops
//! the top. There is no random-access unprotect in the public R API.
//!
//! Depth tracking (leak detection) is zero-cost in ReleaseFast/ReleaseSmall:
//! the depth variable and warning checks are eliminated at compile time.
//! In Debug/ReleaseSafe the depth counter warns at >64 and on negative depth.

const std = @import("std");
const builtin = @import("builtin");
const SEXP = @import("sexp.zig").SEXP;
const R = @import("R");

// Comptime gate: true if we should track protection depth.
// Eliminates depth code entirely in ReleaseFast/ReleaseSmall.
const track_depth = builtin.mode == .Debug or builtin.mode == .ReleaseSafe;

threadlocal var depth: i32 = 0;
const leak_warn_threshold = 64;

/// Push a SEXP onto R's protection stack. Each push must be paired with
/// exactly one unprotect() call on the normal return path.
/// Zero-cost in ReleaseFast/ReleaseSmall (depth tracking eliminated).
pub fn protect(value: SEXP) SEXP {
    if (track_depth) {
        if (depth > leak_warn_threshold) {
            std.log.warn("protect depth is {} (possible leak)", .{depth});
        }
        depth += 1;
    }
    return R.Rf_protect(value);
}

pub const ScopedProtect = struct {
    value: SEXP,
    active: bool = true,

    pub fn init(value: SEXP) ScopedProtect {
        return .{ .value = protect(value) };
    }

    pub fn get(self: ScopedProtect) SEXP {
        return self.value;
    }

    pub fn release(self: *ScopedProtect) SEXP {
        self.active = false;
        return self.value;
    }

    pub fn deinit(self: *ScopedProtect) void {
        if (!self.active) return;
        self.active = false;
        unprotect();
    }
};

pub fn scoped(value: SEXP) ScopedProtect {
    return ScopedProtect.init(value);
}

/// Pop one entry from R's protection stack (LIFO). Must match a prior
/// protect() call. Calling unprotect more times than protect will
/// unbalance the stack and crash R.
/// Zero-cost in ReleaseFast/ReleaseSmall (depth tracking eliminated).
pub fn unprotect() void {
    if (track_depth) {
        depth -= 1;
        if (depth < 0) {
            std.log.warn("protect depth went negative ({})", .{depth});
        }
    }
    R.Rf_unprotect(1);
}

/// Indexed protect: records the stack position so the SEXP can be
/// replaced later via reprotect(). The index is not for random-access
/// unprotect (use unprotect() for that).
pub fn protectWithIndex(value: SEXP, index: *R.PROTECT_INDEX) void {
    R.R_ProtectWithIndex(value, index);
    if (track_depth) {
        depth += 1;
        if (depth > leak_warn_threshold) {
            std.log.warn("protect depth is {} (possible leak)", .{depth});
        }
    }
}

/// Replace the value at the given protection index without changing the
/// stack depth. Used with protectWithIndex to update a protected SEXP
/// while keeping its stack position.
pub fn reprotect(value: SEXP, index: R.PROTECT_INDEX) void {
    R.R_Reprotect(value, index);
}

/// Pop N entries from R's protection stack. Equivalent to N sequential
/// unprotect() calls but only one FFI boundary crossing.
/// Zero-cost in ReleaseFast/ReleaseSmall (depth tracking eliminated).
pub fn unprotectN(count: usize) void {
    if (track_depth) {
        const new_depth = depth - @as(i32, @intCast(count));
        if (new_depth < 0) {
            std.log.warn("protect depth went negative ({}) from unprotectN({})", .{ new_depth, count });
        }
        depth = new_depth;
    }
    R.Rf_unprotect(@intCast(count));
}

/// Read the current protection depth. Useful for leak detection in
/// debug-mode R sessions. Returns 0 in ReleaseFast/ReleaseSmall
/// since depth is not tracked there.
pub fn getDepth() i32 {
    if (track_depth) return depth;
    return 0;
}
