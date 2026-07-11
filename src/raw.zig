//! Borrowed R vector views.
//!
//! Slices die at the next GC-triggering R call. ALTREP uses R accessors
//! because its data can be virtual.

const R = @import("R");
const sexp = @import("sexp.zig");
const Rcomplex = @import("convert.zig").Rcomplex;

fn typedData(comptime T: type, sexp_: R.SEXP) ?[]const T {
    const n = sexp.xlength(sexp_);
    if (R.ALTREP(sexp_) != 0) return switch (T) {
        f64 => R.REAL(sexp_)[0..n],
        i32 => R.INTEGER(sexp_)[0..n],
        u8 => R.RAW(sexp_)[0..n],
        else => unreachable,
    };
    const ptr = sexp.fastDataPtr(sexp_) orelse return null;
    return @as([*]const T, @ptrCast(@alignCast(ptr)))[0..n];
}

fn typedDataMut(comptime T: type, sexp_: R.SEXP) ?[]T {
    const n = sexp.xlength(sexp_);
    if (R.ALTREP(sexp_) != 0) return switch (T) {
        f64 => R.REAL(sexp_)[0..n],
        i32 => R.INTEGER(sexp_)[0..n],
        u8 => R.RAW(sexp_)[0..n],
        else => unreachable,
    };
    const ptr = sexp.fastDataPtr(sexp_) orelse return null;
    return @as([*]T, @ptrCast(@alignCast(ptr)))[0..n];
}

pub fn real(sexp_: R.SEXP) []const f64 {
    return typedData(f64, sexp_) orelse &[0]f64{};
}

pub fn int(sexp_: R.SEXP) []const i32 {
    return typedData(i32, sexp_) orelse &[0]i32{};
}

pub fn logical(sexp_: R.SEXP) []const i32 {
    return typedData(i32, sexp_) orelse &[0]i32{};
}

pub fn realMut(sexp_: R.SEXP) []f64 {
    return typedDataMut(f64, sexp_) orelse &[0]f64{};
}

pub fn intMut(sexp_: R.SEXP) []i32 {
    return typedDataMut(i32, sexp_) orelse &[0]i32{};
}

pub fn raw(sexp_: R.SEXP) []const u8 {
    return typedData(u8, sexp_) orelse &[0]u8{};
}

pub fn rawMut(sexp_: R.SEXP) []u8 {
    return typedDataMut(u8, sexp_) orelse &[0]u8{};
}

/// Some ALTREP values have no direct complex pointer.
pub fn complex(sexp_: R.SEXP) []const Rcomplex {
    const ptr = R.COMPLEX(sexp_) orelse return &[0]Rcomplex{};
    return @as([*]const Rcomplex, @ptrCast(@alignCast(ptr)))[0..sexp.xlength(sexp_)];
}

/// Some ALTREP values have no direct complex pointer.
pub fn complexMut(sexp_: R.SEXP) []Rcomplex {
    const ptr = R.COMPLEX(sexp_) orelse return &[0]Rcomplex{};
    return @as([*]Rcomplex, @ptrCast(@alignCast(ptr)))[0..sexp.xlength(sexp_)];
}

pub fn dims(sexp_: R.SEXP) struct { rows: R.R_xlen_t, cols: R.R_xlen_t } {
    return .{ .rows = R.R_nrow(sexp_), .cols = R.R_ncol(sexp_) };
}
