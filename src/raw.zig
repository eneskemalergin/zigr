//! Zero-copy access to R vector data.
//! Returns slices into R's memory directly, no allocation, no copy.
//! The caller must not hold the slice across a GC-triggering call.
//!
//! Every function here is a no-copy view. Names mirror R types (real, int, logical, raw, complex).

const R = @import("R");
const sexp = @import("sexp.zig");
const Rcomplex = @import("convert.zig").Rcomplex;

pub fn real(sexp_: R.SEXP) []const f64 {
    return R.REAL(sexp_)[0..sexp.xlength(sexp_)];
}

pub fn int(sexp_: R.SEXP) []const i32 {
    return R.INTEGER(sexp_)[0..sexp.xlength(sexp_)];
}

pub fn logical(sexp_: R.SEXP) []const i32 {
    return R.LOGICAL(sexp_)[0..sexp.xlength(sexp_)];
}

pub fn realMut(sexp_: R.SEXP) []f64 {
    return R.REAL(sexp_)[0..sexp.xlength(sexp_)];
}

pub fn intMut(sexp_: R.SEXP) []i32 {
    return R.INTEGER(sexp_)[0..sexp.xlength(sexp_)];
}

pub fn raw(sexp_: R.SEXP) []const u8 {
    return R.RAW(sexp_)[0..sexp.xlength(sexp_)];
}

pub fn rawMut(sexp_: R.SEXP) []u8 {
    return R.RAW(sexp_)[0..sexp.xlength(sexp_)];
}

/// COMPLEX can return null for some ALTREP vectors; returns empty slice instead.
pub fn complex(sexp_: R.SEXP) []const Rcomplex {
    const ptr = R.COMPLEX(sexp_) orelse return &[0]Rcomplex{};
    return @as([*]const Rcomplex, @ptrCast(@alignCast(ptr)))[0..sexp.xlength(sexp_)];
}

/// COMPLEX can return null for some ALTREP vectors; returns empty slice instead.
pub fn complexMut(sexp_: R.SEXP) []Rcomplex {
    const ptr = R.COMPLEX(sexp_) orelse return &[0]Rcomplex{};
    return @as([*]Rcomplex, @ptrCast(@alignCast(ptr)))[0..sexp.xlength(sexp_)];
}

/// Uses R_nrow/R_ncol (R 4.6+) which return R_xlen_t, safe for >2^31 rows.
pub fn dims(sexp_: R.SEXP) struct { rows: R.R_xlen_t, cols: R.R_xlen_t } {
    return .{ .rows = R.R_nrow(sexp_), .cols = R.R_ncol(sexp_) };
}
