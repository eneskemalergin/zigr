//! Attribute and name handling.
//!
//! Wraps R's attribute access functions so Zig code can get and set
//! SEXP attributes (names, class, dim, row names, etc.) without
//! calling the C API directly.

const std = @import("std");
const R = @import("R");


/// Set column/row names on a VECSXP or data frame.
pub fn setNames(sexp: R.SEXP, names: []const []const u8) void {
    const ns = R.Rf_protect(R.Rf_allocVector(R.STRSXP, @as(R.R_xlen_t, @intCast(names.len))));
    defer R.Rf_unprotect(1);
    for (0..names.len) |i| {
        const cs = R.Rf_mkCharLenCE(@ptrCast(names[i].ptr), @intCast(names[i].len), @as(R.cetype_t, @intCast(R.CE_UTF8)));
        R.SET_STRING_ELT(ns, @intCast(i), cs);
    }
    _ = R.Rf_namesgets(sexp, ns);
}

/// Return the class attribute as a slice of strings.
pub fn getClass(allocator: std.mem.Allocator, sexp: R.SEXP) ![][]const u8 {
    const cls = R.Rf_getAttrib(sexp, R.R_ClassSymbol);
    if (cls == R.R_NilValue) return &.{};
    const n = @as(usize, @intCast(R.XLENGTH(cls)));
    const result = try allocator.alloc([]const u8, n);
    for (0..n) |i| {
        const elt = R.STRING_ELT(cls, @intCast(i));
        result[i] = if (elt == R.R_NaString) "" else std.mem.sliceTo(R.R_CHAR(elt), 0);
    }
    return result;
}

/// Set the class attribute to a single string.
pub fn setClass(sexp: R.SEXP, class: []const u8) void {
    const cls = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    defer R.Rf_unprotect(1);
    R.SET_STRING_ELT(cls, 0, R.Rf_mkCharLenCE(@ptrCast(class.ptr), @intCast(class.len), @as(R.cetype_t, @intCast(R.CE_UTF8))));
    _ = R.Rf_classgets(sexp, cls);
}

/// Set dimensions (rows, cols for a matrix).
pub fn setDim(sexp: R.SEXP, dims: []const i32) void {
    const d = R.Rf_protect(R.Rf_allocVector(R.INTSXP, @as(R.R_xlen_t, @intCast(dims.len))));
    defer R.Rf_unprotect(1);
    const ptr = R.INTEGER(d);
    for (0..dims.len) |i| ptr[i] = dims[i];
    _ = R.Rf_dimgets(sexp, d);
}

/// Read an attribute by symbol.
pub fn getAttrib(sexp: R.SEXP, sym: R.SEXP) R.SEXP {
    return R.Rf_getAttrib(sexp, sym);
}

/// Write an attribute by symbol.
pub fn setAttrib(sexp: R.SEXP, sym: R.SEXP, value: R.SEXP) void {
    _ = R.Rf_setAttrib(sexp, sym, value);
}
