//! Convert between Zig native types and R SEXPs.
//!
//! Each vector type has toSlice (borrow, zero-copy for non-ALTREP) and
//! fromSlice (allocate on R's heap, copy). from* functions unprotect
//! their result before returning (standard R pattern). A cleanup frame
//! protects against longjmp during allocation.

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

/// REALSXP: zero-copy borrow from REAL() pointer. If ALTREP, reads via
/// REAL_ELT to avoid materialization.
pub fn toRealSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]f64 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    if (R.ALTREP(sexp) != 0) {
        const result = try allocator.alloc(f64, n);
        for (0..n) |i| result[i] = R.REAL_ELT(sexp, @intCast(i));
        return result;
    }
    return R.REAL(sexp)[0..n];
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

/// INTSXP: zero-copy from INTEGER() pointer. ALTREP-safe via INTEGER_ELT.
pub fn toIntSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]i32 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    if (R.ALTREP(sexp) != 0) {
        const result = try allocator.alloc(i32, n);
        for (0..n) |i| result[i] = R.INTEGER_ELT(sexp, @intCast(i));
        return result;
    }
    return R.INTEGER(sexp)[0..n];
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

/// LGLSXP: zero-copy from LOGICAL() pointer. ALTREP-safe via LOGICAL_ELT.
pub fn toLogicalSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]i32 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    if (R.ALTREP(sexp) != 0) {
        const result = try allocator.alloc(i32, n);
        for (0..n) |i| result[i] = R.LOGICAL_ELT(sexp, @intCast(i));
        return result;
    }
    return R.LOGICAL(sexp)[0..n];
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

/// RAWSXP: zero-copy borrow from RAW() pointer.
pub fn toRawSlice(sexp: SEXP) []u8 {
    return R.RAW(sexp)[0..@as(usize, @intCast(R.XLENGTH(sexp)))];
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

/// CPLXSXP: borrow from COMPLEX() pointer.
pub fn toComplexSlice(sexp: SEXP) []Rcomplex {
    const ptr: [*]Rcomplex = @ptrCast(@alignCast(R.COMPLEX(sexp).?));
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    return ptr[0..n];
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
