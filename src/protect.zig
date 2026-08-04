//! R protection-stack helpers.
//!
//! The stack is LIFO; diagnostic depth tracking compiles out of fast builds.

const std = @import("std");
const builtin = @import("builtin");
const SEXP = @import("sexp.zig").SEXP;
const R = @import("R");
const cleanup = @import("cleanup");

const track_depth = builtin.mode == .Debug or builtin.mode == .ReleaseSafe;

const leak_warn_threshold = 64;

pub fn protect(value: SEXP) SEXP {
    if (track_depth) {
        if (cleanup.getProtectDepth() > leak_warn_threshold) {
            std.log.warn("protect depth is {} (possible leak)", .{cleanup.getProtectDepth()});
        }
        cleanup.adjustProtectDepth(1);
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

pub fn unprotect() void {
    if (track_depth) {
        cleanup.adjustProtectDepth(-1);
        if (cleanup.getProtectDepth() < 0) {
            std.log.warn("protect depth went negative ({})", .{cleanup.getProtectDepth()});
        }
    }
    if (!cleanup.isRecoveringCondition()) R.Rf_unprotect(1);
}

pub fn protectWithIndex(value: SEXP, index: *R.PROTECT_INDEX) void {
    R.R_ProtectWithIndex(value, index);
    if (track_depth) {
        cleanup.adjustProtectDepth(1);
        if (cleanup.getProtectDepth() > leak_warn_threshold) {
            std.log.warn("protect depth is {} (possible leak)", .{cleanup.getProtectDepth()});
        }
    }
}

pub fn reprotect(value: SEXP, index: R.PROTECT_INDEX) void {
    R.R_Reprotect(value, index);
}

pub fn unprotectN(count: usize) void {
    if (count > std.math.maxInt(c_int)) @panic("unprotectN count exceeds c_int range");
    if (track_depth) {
        const new_depth = cleanup.getProtectDepth() - @as(i32, @intCast(count));
        if (new_depth < 0) {
            std.log.warn("protect depth went negative ({}) from unprotectN({})", .{ new_depth, count });
        }
        cleanup.adjustProtectDepth(new_depth - cleanup.getProtectDepth());
    }
    if (!cleanup.isRecoveringCondition()) R.Rf_unprotect(@intCast(count));
}

pub fn getDepth() i32 {
    if (track_depth) return cleanup.getProtectDepth();
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
