//! PROTECT / UNPROTECT wrappers.
//!
//! Calls Rf_protect / Rf_unprotect through the translated R headers.
//! Building this module requires libR to be linked (-lR).

const SEXP = @import("sexp.zig").SEXP;
const R = @import("R");

/// Debug-only protection depth counter. Tracks active protects for leak
/// detection. Exported so that debug utilities can query it.
pub var depth: i32 = 0;

pub const Protection = struct {
    pub fn protect(value: SEXP) Protection {
        _ = R.Rf_protect(@as(R.SEXP, @ptrCast(value)));
        depth += 1;
        return Protection{};
    }

    pub fn unprotect(self: Protection) void {
        _ = self;
        R.Rf_unprotect(1);
        depth -= 1;
    }
};

pub fn protectWithIndex(value: SEXP, index: *i32) void {
    var ri: R.PROTECT_INDEX = @intCast(index.*);
    R.R_ProtectWithIndex(@as(R.SEXP, @ptrCast(value)), &ri);
    index.* = @intCast(ri);
    depth += 1;
}

pub fn reprotect(value: SEXP, index: i32) void {
    R.R_Reprotect(@as(R.SEXP, @ptrCast(value)), @intCast(index));
}

pub fn unprotectIndex(_index: i32) void {
    _ = _index;
    R.Rf_unprotect(1);
    depth -= 1;
}

pub fn getDepth() i32 {
    return depth;
}
