//! Checked names, class, and dimension helpers plus raw attribute access.
//!
//! Allocating setters may longjmp. Call them inside a generated entry point or
//! another unwind boundary. Raw attribute get/set retains values without
//! requesting ALTREP payload storage. String readers intentionally iterate an
//! ALTSTRING attribute through `STRING_ELT`.

const std = @import("std");
const R = @import("R");
const cleanup = @import("cleanup");
const protect = @import("protect.zig");
const sexp_mod = @import("sexp.zig");

pub const AttributeError = error{ExpectedStringAttribute};

const HeaderCleanup = struct {
    allocator: std.mem.Allocator,
    values: ?[][]const u8 = null,

    fn fire(self: *@This()) void {
        if (self.values) |values| self.allocator.free(values);
    }
};

const OptionalHeaderCleanup = struct {
    allocator: std.mem.Allocator,
    values: ?[]?[]const u8 = null,

    fn fire(self: *@This()) void {
        if (self.values) |values| self.allocator.free(values);
    }
};

/// The returned headers are caller-owned. R owns the bytes; `NA` becomes empty.
/// Translated bytes are valid only for the current R call. The allocator must
/// remain valid if R unwinds while an ALTSTRING attribute is read.
pub fn getString(allocator: std.mem.Allocator, sexp: R.SEXP, symbol: R.SEXP) (AttributeError || std.mem.Allocator.Error)![][]const u8 {
    const value = R.Rf_getAttrib(sexp, symbol);
    if (value == R.R_NilValue) return allocator.alloc([]const u8, 0);
    if (R.TYPEOF(value) != R.STRSXP) return error.ExpectedStringAttribute;
    const n: usize = @intCast(R.XLENGTH(value));
    const registration = cleanup.pushFrameInlineWithHandle(HeaderCleanup, .{ .allocator = allocator }, HeaderCleanup.fire);
    const state = registration.state;
    errdefer _ = cleanup.releaseFrame(registration.handle);
    const result = try allocator.alloc([]const u8, n);
    state.values = result;
    for (0..n) |i| {
        const elt = R.STRING_ELT(value, @intCast(i));
        result[i] = if (elt == R.R_NaString) "" else sexp_mod.charsxpBytes(elt);
    }
    _ = cleanup.releaseFrame(registration.handle);
    return result;
}

/// The returned headers are caller-owned. R owns present bytes; `NA` stays null.
/// Translated bytes are valid only for the current R call. The allocator must
/// remain valid if R unwinds while an ALTSTRING attribute is read.
pub fn getOptionalString(allocator: std.mem.Allocator, sexp: R.SEXP, symbol: R.SEXP) (AttributeError || std.mem.Allocator.Error)![]?[]const u8 {
    const value = R.Rf_getAttrib(sexp, symbol);
    if (value == R.R_NilValue) return allocator.alloc(?[]const u8, 0);
    if (R.TYPEOF(value) != R.STRSXP) return error.ExpectedStringAttribute;
    const n: usize = @intCast(R.XLENGTH(value));
    const registration = cleanup.pushFrameInlineWithHandle(OptionalHeaderCleanup, .{ .allocator = allocator }, OptionalHeaderCleanup.fire);
    const state = registration.state;
    errdefer _ = cleanup.releaseFrame(registration.handle);
    const result = try allocator.alloc(?[]const u8, n);
    state.values = result;
    for (0..n) |i| {
        const elt = R.STRING_ELT(value, @intCast(i));
        result[i] = if (elt == R.R_NaString) null else sexp_mod.charsxpBytes(elt);
    }
    _ = cleanup.releaseFrame(registration.handle);
    return result;
}

pub fn setNames(sexp: R.SEXP, names: []const []const u8) void {
    var ns = protect.scoped(R.Rf_allocVector(R.STRSXP, @as(R.R_xlen_t, @intCast(names.len))));
    defer ns.deinit();
    for (0..names.len) |i| {
        const cs = R.Rf_mkCharLenCE(@ptrCast(names[i].ptr), @intCast(names[i].len), @as(R.cetype_t, @intCast(R.CE_UTF8)));
        R.SET_STRING_ELT(ns.get(), @intCast(i), cs);
    }
    _ = R.Rf_namesgets(sexp, ns.get());
}

/// The returned headers are caller-owned and their bytes remain R-owned for the
/// current R call. The allocator must remain valid during an R unwind.
pub fn getClass(allocator: std.mem.Allocator, sexp: R.SEXP) (AttributeError || std.mem.Allocator.Error)![][]const u8 {
    return getString(allocator, sexp, R.R_ClassSymbol);
}

/// The returned headers are caller-owned and their bytes remain R-owned for the
/// current R call. The allocator must remain valid during an R unwind.
pub fn getNames(allocator: std.mem.Allocator, sexp: R.SEXP) (AttributeError || std.mem.Allocator.Error)![][]const u8 {
    return getString(allocator, sexp, R.R_NamesSymbol);
}

pub fn setClass(sexp: R.SEXP, class: []const u8) void {
    const cls = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    R.SET_STRING_ELT(cls, 0, R.Rf_mkCharLenCE(@ptrCast(class.ptr), @intCast(class.len), @as(R.cetype_t, @intCast(R.CE_UTF8))));
    _ = R.Rf_classgets(sexp, cls);
    R.Rf_unprotect(1);
}

pub fn setDim(sexp: R.SEXP, dims: []const i32) void {
    var d = protect.scoped(R.Rf_allocVector(R.INTSXP, @as(R.R_xlen_t, @intCast(dims.len))));
    defer d.deinit();
    const ptr = R.INTEGER(d.get());
    for (0..dims.len) |i| ptr[i] = dims[i];
    _ = R.Rf_dimgets(sexp, d.get());
}

pub fn getAttrib(sexp: R.SEXP, sym: R.SEXP) R.SEXP {
    return R.Rf_getAttrib(sexp, sym);
}

pub fn setAttrib(sexp: R.SEXP, sym: R.SEXP, value: R.SEXP) void {
    _ = R.Rf_setAttrib(sexp, sym, value);
}
