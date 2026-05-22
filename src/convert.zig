//! Convert between Zig native types and R SEXPs.
//!
//! Each vector type has toSlice (allocate on caller's allocator, copy) and
//! fromSlice (allocate on R's heap, copy). to* functions return owned
//! slices — caller must free. from* functions unprotect their result before
//! returning (standard R pattern). A cleanup frame protects against longjmp
//! during allocation.

const std = @import("std");
const SEXP = @import("sexp.zig").SEXP;
const R = @import("R");
const cleanup = @import("cleanup");

const Rcomplex = extern struct { r: f64, i: f64 };

const Unprot = struct {
    fn fire(_: ?*anyopaque) void {
        R.Rf_unprotect(1);
    }
};

/// REALSXP: allocate and copy. Uses REAL_ELT which handles ALTREP
/// transparently without materialization.
pub fn toRealSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]f64 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc(f64, n);
    for (0..n) |i| result[i] = R.REAL_ELT(sexp, @intCast(i));
    return result;
}

/// REALSXP: allocate and copy.
pub fn fromRealSlice(slice: []const f64) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, len));
    cleanup.pushFrame(Unprot.fire, null);
    @memcpy(R.REAL(vec)[0..slice.len], slice);
    cleanup.popFrame();
    R.Rf_unprotect(1);
    return vec;
}

/// INTSXP: allocate and copy. Uses INTEGER_ELT which handles ALTREP
/// transparently without materialization.
pub fn toIntSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]i32 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc(i32, n);
    for (0..n) |i| result[i] = R.INTEGER_ELT(sexp, @intCast(i));
    return result;
}

/// INTSXP: allocate and copy.
pub fn fromIntSlice(slice: []const i32) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, len));
    cleanup.pushFrame(Unprot.fire, null);
    @memcpy(R.INTEGER(vec)[0..slice.len], slice);
    cleanup.popFrame();
    R.Rf_unprotect(1);
    return vec;
}

/// STRSXP: borrow CHARSXP data into a Zig slice array.
pub fn toStringSlice(allocator: std.mem.Allocator, sexp: SEXP) ![][]const u8 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc([]const u8, n);
    for (0..n) |i| {
        const elt = R.STRING_ELT(sexp, @intCast(i));
        result[i] = if (elt == R.R_NaString) "" else std.mem.sliceTo(R.R_CHAR(elt), 0);
    }
    return result;
}

/// STRSXP: intern strings and build a vector.
pub fn fromStringSlice(slice: []const []const u8) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    const vec = R.Rf_protect(R.Rf_allocVector(R.STRSXP, len));
    cleanup.pushFrame(Unprot.fire, null);
    for (0..@as(usize, @intCast(len))) |i| {
        const s = slice[i];
        const cs = R.Rf_mkCharLenCE(@ptrCast(s.ptr), @intCast(s.len), @as(R.cetype_t, @intCast(R.CE_UTF8)));
        R.SET_STRING_ELT(vec, @intCast(i), cs);
    }
    cleanup.popFrame();
    R.Rf_unprotect(1);
    return vec;
}

/// LGLSXP: allocate and copy. Uses LOGICAL_ELT which handles ALTREP
/// transparently without materialization.
pub fn toLogicalSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]i32 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc(i32, n);
    for (0..n) |i| result[i] = R.LOGICAL_ELT(sexp, @intCast(i));
    return result;
}

/// LGLSXP: build from i32 slice.
pub fn fromLogicalSlice(slice: []const i32) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    const vec = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, len));
    cleanup.pushFrame(Unprot.fire, null);
    const ptr = R.LOGICAL(vec);
    for (0..@as(usize, @intCast(len))) |i| ptr[i] = slice[i];
    cleanup.popFrame();
    R.Rf_unprotect(1);
    return vec;
}

/// VECSXP: borrow list elements into a SEXP slice.
pub fn toListSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]SEXP {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc(SEXP, n);
    for (0..n) |i| result[i] = R.VECTOR_ELT(sexp, @intCast(i));
    return result;
}

/// VECSXP: build from a SEXP slice.
pub fn fromListSlice(slice: []const SEXP) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    const vec = R.Rf_protect(R.Rf_allocVector(R.VECSXP, len));
    cleanup.pushFrame(Unprot.fire, null);
    for (0..@as(usize, @intCast(len))) |i| {
        R.SET_VECTOR_ELT(vec, @intCast(i), slice[i]);
    }
    cleanup.popFrame();
    R.Rf_unprotect(1);
    return vec;
}

/// RAWSXP: allocate and copy.
pub fn toRawSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]const u8 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc(u8, n);
    @memcpy(result, R.RAW(sexp)[0..n]);
    return result;
}

/// RAWSXP: allocate and copy.
pub fn fromRawSlice(slice: []const u8) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    const vec = R.Rf_protect(R.Rf_allocVector(R.RAWSXP, len));
    cleanup.pushFrame(Unprot.fire, null);
    @memcpy(R.RAW(vec)[0..slice.len], slice);
    cleanup.popFrame();
    R.Rf_unprotect(1);
    return vec;
}

/// CPLXSXP: allocate and copy.
pub fn toComplexSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]const Rcomplex {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc(Rcomplex, n);
    const src: [*]const Rcomplex = @ptrCast(@alignCast(R.COMPLEX(sexp).?));
    @memcpy(result, src[0..n]);
    return result;
}

/// CPLXSXP: allocate and copy.
pub fn fromComplexSlice(slice: []const Rcomplex) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    const vec = R.Rf_protect(R.Rf_allocVector(R.CPLXSXP, len));
    cleanup.pushFrame(Unprot.fire, null);
    const dst: [*]Rcomplex = @ptrCast(@alignCast(R.COMPLEX(vec).?));
    @memcpy(dst[0..slice.len], slice);
    cleanup.popFrame();
    R.Rf_unprotect(1);
    return vec;
}
