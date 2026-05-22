//! PROTECT / UNPROTECT wrappers.
//!
//! Rf_protect / Rf_unprotect through the translated R headers.
//! R_UnprotectFromIndex is not in the public R API, so unprotect
//! pops one from the top of the stack (standard R pattern).

const std = @import("std");
const builtin = @import("builtin");
const SEXP = @import("sexp.zig").SEXP;
const R = @import("R");

/// Protection depth counter. Tracks active protects for leak detection.
pub var depth: i32 = 0;

const leak_warn_threshold = 64;

/// A protected SEXP handle. Drop it via unprotect() to release.
/// Leaking handles bloats R's protection stack.
pub const Protection = struct {
    index: i32,

    pub fn protect(value: SEXP) Protection {
        if (builtin.mode == .Debug and depth > leak_warn_threshold) {
            std.log.warn("protect depth is {} (possible leak)", .{depth});
        }
        _ = R.Rf_protect(@as(R.SEXP, @ptrCast(value)));
        depth += 1;
        return Protection{ .index = depth };
    }

    pub fn unprotect(self: Protection) void {
        _ = self;
        R.Rf_unprotect(1);
        depth -= 1;
        if (builtin.mode == .Debug and depth < 0) {
            std.log.warn("protect depth went negative ({})", .{depth});
        }
    }
};

pub fn protectWithIndex(value: SEXP, index: *i32) void {
    var ri: R.PROTECT_INDEX = @intCast(index.*);
    R.R_ProtectWithIndex(@as(R.SEXP, @ptrCast(value)), &ri);
    index.* = @intCast(ri);
    depth += 1;
    if (builtin.mode == .Debug and depth > leak_warn_threshold) {
        std.log.warn("protect depth is {} (possible leak)", .{depth});
    }
}

pub fn reprotect(value: SEXP, index: i32) void {
    R.R_Reprotect(@as(R.SEXP, @ptrCast(value)), @intCast(index));
}

pub fn unprotectIndex(_index: i32) void {
    _ = _index;
    R.Rf_unprotect(1);
    depth -= 1;
    if (builtin.mode == .Debug and depth < 0) {
        std.log.warn("protect depth went negative ({})", .{depth});
    }
}

pub fn getDepth() i32 {
    return depth;
}
