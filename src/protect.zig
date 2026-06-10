//! PROTECT / UNPROTECT wrappers.
//!
//! R's protection stack is strictly LIFO. Protect pushes, unprotect pops
//! the top. There is no random-access unprotect in the public R API.
//!
//! Depth tracking is zero-cost in ReleaseFast/ReleaseSmall (the depth variable and warning checks are eliminated at compile time). In Debug/ReleaseSafe the depth counter warns at >64 and on negative depth.

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

/// Records the stack position so the SEXP can be replaced via reprotect(). Not for random-access (use unprotect for that).
pub fn protectWithIndex(value: SEXP, index: *R.PROTECT_INDEX) void {
    R.R_ProtectWithIndex(value, index);
    if (track_depth) {
        depth += 1;
        if (depth > leak_warn_threshold) {
            std.log.warn("protect depth is {} (possible leak)", .{depth});
        }
    }
}

/// Replace the value at a protection index without changing stack depth. Used with protectWithIndex.
pub fn reprotect(value: SEXP, index: R.PROTECT_INDEX) void {
    R.R_Reprotect(value, index);
}

/// Pop N entries from R's protection stack. One FFI call instead of N.
pub fn unprotectN(count: usize) void {
    if (count > std.math.maxInt(c_int)) @panic("unprotectN count exceeds c_int range");
    if (track_depth) {
        const new_depth = depth - @as(i32, @intCast(count));
        if (new_depth < 0) {
            std.log.warn("protect depth went negative ({}) from unprotectN({})", .{ new_depth, count });
        }
        depth = new_depth;
    }
    R.Rf_unprotect(@intCast(count));
}

/// Useful for leak detection. Returns 0 in ReleaseFast/ReleaseSmall (depth tracking is eliminated there).
pub fn getDepth() i32 {
    if (track_depth) return depth;
    return 0;
}

test "ScopedProtect type" {
    try std.testing.expectEqual(@TypeOf(ScopedProtect.init), fn (SEXP) ScopedProtect);
}

test "scoped helper type" {
    try std.testing.expectEqual(@TypeOf(scoped), fn (SEXP) ScopedProtect);
}

test "protect and unprotect types" {
    try std.testing.expectEqual(@TypeOf(protect), fn (SEXP) SEXP);
    try std.testing.expectEqual(@TypeOf(unprotect), fn () void);
}

test "protectWithIndex and reprotect types" {
    try std.testing.expectEqual(@TypeOf(protectWithIndex), fn (SEXP, *R.PROTECT_INDEX) void);
    try std.testing.expectEqual(@TypeOf(reprotect), fn (SEXP, R.PROTECT_INDEX) void);
}

test "getDepth returns i32" {
    try std.testing.expectEqual(@TypeOf(getDepth()), i32);
}
