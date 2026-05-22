//! Convert between Zig native types and R SEXPs.
//!
//! Each vector type has toSlice (allocate on caller's allocator, copy) and
//! fromSlice (allocate on R's heap, copy). to* functions return owned
//! slices, caller must free. from* functions unprotect their result before
//! returning (standard R pattern).
//!
//! asSEXP/fromSEXP convert Zig structs to/from R named lists using
//! @typeInfo reflection. Field names become list names.
//!
//! to* functions use a fast path: @memcpy from the data pointer for
//! non-ALTREP, and *_GET_REGION (single C call) for ALTREP. This matches
//! C baseline performance for the common non-ALTREP case.
//! from* functions with R API calls in the copy path (fromStringSlice)
//! use a cleanup frame for longjmp safety. Pure @memcpy paths skip it.

const std = @import("std");
const SEXP = @import("sexp.zig").SEXP;
const R = @import("R");
const cleanup = @import("cleanup");

pub const Rcomplex = extern struct { r: f64, i: f64 };

const Unprot = struct {
    fn fire(_: ?*anyopaque) void {
        R.Rf_unprotect(1);
    }
};

/// REALSXP: allocate and copy. Uses REAL_GET_REGION for ALTREP (one C call),
/// @memcpy from REAL() for non-ALTREP (zero C FFI, matches C baseline).
pub fn toRealSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]f64 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc(f64, n);
    if (R.ALTREP(sexp) != 0) {
        _ = R.REAL_GET_REGION(sexp, 0, @intCast(n), result.ptr);
    } else {
        @memcpy(result, R.REAL(sexp)[0..n]);
    }
    return result;
}

/// REALSXP: allocate and copy.
pub fn fromRealSlice(slice: []const f64) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, len));
    @memcpy(R.REAL(vec)[0..slice.len], slice);
    R.Rf_unprotect(1);
    return vec;
}

/// INTSXP: allocate and copy. Uses INTEGER_GET_REGION for ALTREP,
/// @memcpy from INTEGER() for non-ALTREP.
pub fn toIntSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]i32 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc(i32, n);
    if (R.ALTREP(sexp) != 0) {
        _ = R.INTEGER_GET_REGION(sexp, 0, @intCast(n), result.ptr);
    } else {
        @memcpy(result, R.INTEGER(sexp)[0..n]);
    }
    return result;
}

/// INTSXP: allocate and copy.
pub fn fromIntSlice(slice: []const i32) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, len));
    @memcpy(R.INTEGER(vec)[0..slice.len], slice);
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

/// LGLSXP: allocate and copy. Uses LOGICAL_GET_REGION for ALTREP,
/// @memcpy from LOGICAL() for non-ALTREP.
pub fn toLogicalSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]i32 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc(i32, n);
    if (R.ALTREP(sexp) != 0) {
        _ = R.LOGICAL_GET_REGION(sexp, 0, @intCast(n), result.ptr);
    } else {
        @memcpy(result, R.LOGICAL(sexp)[0..n]);
    }
    return result;
}

/// LGLSXP: build from i32 slice.
pub fn fromLogicalSlice(slice: []const i32) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    const vec = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, len));
    @memcpy(R.LOGICAL(vec)[0..slice.len], slice);
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
    for (0..@as(usize, @intCast(len))) |i| {
        R.SET_VECTOR_ELT(vec, @intCast(i), slice[i]);
    }
    R.Rf_unprotect(1);
    return vec;
}

/// RAWSXP: allocate and copy. Uses RAW_GET_REGION for ALTREP,
/// @memcpy from RAW() for non-ALTREP.
pub fn toRawSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]const u8 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc(u8, n);
    if (R.ALTREP(sexp) != 0) {
        _ = R.RAW_GET_REGION(sexp, 0, @intCast(n), result.ptr);
    } else {
        @memcpy(result, R.RAW(sexp)[0..n]);
    }
    return result;
}

/// RAWSXP: allocate and copy.
pub fn fromRawSlice(slice: []const u8) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    const vec = R.Rf_protect(R.Rf_allocVector(R.RAWSXP, len));
    @memcpy(R.RAW(vec)[0..slice.len], slice);
    R.Rf_unprotect(1);
    return vec;
}

/// CPLXSXP: allocate and copy. Uses COMPLEX_GET_REGION for ALTREP,
/// @memcpy from COMPLEX() for non-ALTREP.
pub fn toComplexSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]const Rcomplex {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc(Rcomplex, n);
    if (R.ALTREP(sexp) != 0) {
        _ = R.COMPLEX_GET_REGION(sexp, 0, @intCast(n), result.ptr);
    } else {
        const src: [*]const Rcomplex = @ptrCast(@alignCast(R.COMPLEX(sexp).?));
        @memcpy(result, src[0..n]);
    }
    return result;
}

/// CPLXSXP: allocate and copy.
pub fn fromComplexSlice(slice: []const Rcomplex) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    const vec = R.Rf_protect(R.Rf_allocVector(R.CPLXSXP, len));
    const dst: [*]Rcomplex = @ptrCast(@alignCast(R.COMPLEX(vec).?));
    @memcpy(dst[0..slice.len], slice);
    R.Rf_unprotect(1);
    return vec;
}

fn zigToSexp(value: anytype, comptime T: type, arena: std.mem.Allocator) SEXP {
    if (comptime @typeInfo(T) == .optional) {
        if (value) |v| return zigToSexp(v, @TypeOf(v), arena);
        return R.R_NilValue;
    }
    if (comptime T == f64) return R.Rf_ScalarReal(value);
    if (comptime T == i32) return R.Rf_ScalarInteger(value);
    if (comptime T == bool) return R.Rf_ScalarLogical(if (value) 1 else 0);
    if (comptime T == []const f64) return fromRealSlice(value);
    if (comptime T == []const i32) return fromIntSlice(value);
    if (comptime T == []const []const u8) return fromStringSlice(value);
    if (comptime T == []const u8) return fromRawSlice(value);
    if (comptime T == []const Rcomplex) return fromComplexSlice(value);
    if (comptime T == SEXP) return value;
    if (comptime @typeInfo(T) == .@"struct") {
        return structToSexp(value, T, arena);
    }
    @compileError("unsupported type in struct conversion: " ++ @typeName(T));
}

fn sexpToZig(comptime T: type, sexp: SEXP, arena: std.mem.Allocator) T {
    if (comptime @typeInfo(T) == .optional) {
        if (sexp == R.R_NilValue) return null;
        return sexpToZig(@typeInfo(T).optional.child, sexp, arena);
    }
    if (comptime T == f64) return R.REAL(sexp)[0];
    if (comptime T == i32) return R.INTEGER(sexp)[0];
    if (comptime T == bool) return R.LOGICAL(sexp)[0] != 0;
    if (comptime T == []const f64) return toRealSlice(arena, sexp) catch |err| @panic(@errorName(err));
    if (comptime T == []const i32) return toIntSlice(arena, sexp) catch |err| @panic(@errorName(err));
    if (comptime T == []const []const u8) return toStringSlice(arena, sexp) catch |err| @panic(@errorName(err));
    if (comptime T == []const u8) return toRawSlice(arena, sexp) catch |err| @panic(@errorName(err));
    if (comptime T == []const Rcomplex) return toComplexSlice(arena, sexp) catch |err| @panic(@errorName(err));
    if (comptime T == SEXP) return sexp;
    if (comptime @typeInfo(T) == .@"struct") {
        return structFromSexp(T, sexp, arena);
    }
    @compileError("unsupported type in struct conversion: " ++ @typeName(T));
}

fn structToSexp(st: anytype, comptime T: type, arena: std.mem.Allocator) SEXP {
    const fields = @typeInfo(T).@"struct".fields;
    const n: R.R_xlen_t = @intCast(fields.len);
    const vec = R.Rf_protect(R.Rf_allocVector(R.VECSXP, n));
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, n));

    inline for (fields, 0..) |field, i| {
        const val = @field(st, field.name);
        const elt = zigToSexp(val, field.type, arena);
        _ = R.SET_VECTOR_ELT(vec, @intCast(i), elt);
        const cs = R.Rf_mkCharLenCE(@ptrCast(field.name.ptr), @intCast(field.name.len), @as(R.cetype_t, @intCast(R.CE_UTF8)));
        R.SET_STRING_ELT(names, @intCast(i), cs);
    }

    _ = R.Rf_namesgets(vec, names);
    R.Rf_unprotect(2);
    return vec;
}

fn structFromSexp(comptime T: type, sexp: SEXP, arena: std.mem.Allocator) T {
    const fields = @typeInfo(T).@"struct".fields;
    const ns = R.Rf_getAttrib(sexp, R.R_NamesSymbol);
    var result: T = undefined;

    inline for (fields) |field| {
        var found = false;
        for (0..@as(usize, @intCast(R.XLENGTH(ns)))) |i| {
            const elt = R.STRING_ELT(ns, @intCast(i));
            if (elt == R.R_NaString) continue;
            const cn = std.mem.sliceTo(R.R_CHAR(elt), 0);
            if (std.mem.eql(u8, cn, field.name)) {
                const elem = R.VECTOR_ELT(sexp, @intCast(i));
                @field(result, field.name) = sexpToZig(field.type, elem, arena);
                found = true;
                break;
            }
        }
        if (!found) {
            if (comptime @typeInfo(field.type) != .optional)
                @panic("missing field in R list: " ++ field.name);
            @field(result, field.name) = @as(field.type, null);
        }
    }

    return result;
}

/// Convert a Zig struct to an R named list. Field names become list
/// names. Supports nested structs, slices, scalars, optionals, SEXP.
pub fn asSEXP(st: anytype) SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    return structToSexp(st, @TypeOf(st), arena.allocator());
}

/// Convert an R named list to a Zig struct. Field names are matched
/// against list names. `arena` is used for any slice allocations in the
/// struct fields. Missing optional fields default to null; missing
/// non-optional fields panic.
pub fn fromSEXP(comptime T: type, sexp: SEXP, arena: std.mem.Allocator) T {
    return structFromSexp(T, sexp, arena);
}
