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
const simd = @import("simd");
const cleanup = @import("cleanup");
const protect = @import("protect.zig");

pub const Rcomplex = extern struct { r: f64, i: f64 };
const zigr_altreal_slice_tag_name = "zigr_altreal_slice_wrap";
const zigr_altinteger_slice_tag_name = "zigr_altinteger_slice_wrap";
const zigr_altlogical_slice_tag_name = "zigr_altlogical_slice_wrap";

const ZigrAltRealSliceWrap = struct {
    ptr: [*]const f64,
    len: usize,
};

const ZigrAltIntegerSliceWrap = struct {
    ptr: [*]const i32,
    len: usize,
};

const ZigrAltLogicalSliceWrap = struct {
    ptr: [*]const i32,
    len: usize,
};

const Unprot = struct {
    fn fire(_: ?*anyopaque) void {
        R.Rf_unprotect(1);
    }
};

const AllocSliceCleanup = struct {
    allocator: std.mem.Allocator,
    memory: []u8,
    alignment: std.mem.Alignment,

    fn init(comptime T: type, allocator: std.mem.Allocator, slice: []T) AllocSliceCleanup {
        return .{
            .allocator = allocator,
            .memory = std.mem.sliceAsBytes(slice),
            .alignment = .fromByteUnits(@alignOf(T)),
        };
    }

    fn fire(ptr: ?*anyopaque) void {
        const self: *const AllocSliceCleanup = @ptrCast(@alignCast(ptr.?));
        self.allocator.rawFree(self.memory, self.alignment, @returnAddress());
    }
};

pub const ConvertError = error{
    ExpectedReal,
    ExpectedInteger,
    ExpectedLogical,
    ExpectedString,
    ExpectedList,
    ExpectedNamedList,
    ExpectedRaw,
    ExpectedComplex,
    ZeroLength,
    ScalarNA,
    AltRepRegionRead,
    MissingField,
};

fn expectType(sexp: SEXP, expected: c_uint, comptime err: ConvertError) ConvertError!void {
    if (@as(c_uint, @intCast(R.TYPEOF(sexp))) != expected) return err;
}

fn expectNonEmpty(sexp: SEXP) ConvertError!void {
    if (R.XLENGTH(sexp) == 0) return error.ZeroLength;
}

pub fn errorMessage(err: anyerror) []const u8 {
    return switch (err) {
        error.ExpectedReal => "expected REALSXP",
        error.ExpectedInteger => "expected INTSXP",
        error.ExpectedLogical => "expected LGLSXP",
        error.ExpectedString => "expected STRSXP",
        error.ExpectedList => "expected VECSXP",
        error.ExpectedNamedList => "expected named VECSXP",
        error.ExpectedRaw => "expected RAWSXP",
        error.ExpectedComplex => "expected CPLXSXP",
        error.ZeroLength => "expected non-empty vector",
        error.ScalarNA => "scalar inputs must not be NA",
        error.AltRepRegionRead => "ALTREP region read failed",
        error.MissingField => "missing required field in R list",
        error.OutOfMemory => "out of memory during SEXP conversion",
        else => @errorName(err),
    };
}

fn expectNamedList(sexp: SEXP) ConvertError!SEXP {
    try expectType(sexp, R.VECSXP, error.ExpectedNamedList);
    const ns = R.Rf_getAttrib(sexp, R.R_NamesSymbol);
    if (ns == R.R_NilValue or R.TYPEOF(ns) != R.STRSXP) return error.ExpectedNamedList;
    if (R.XLENGTH(ns) != R.XLENGTH(sexp)) return error.ExpectedNamedList;
    for (0..@as(usize, @intCast(R.XLENGTH(ns)))) |i| {
        if (R.STRING_ELT(ns, @intCast(i)) == R.R_NaString) return error.ExpectedNamedList;
    }
    return ns;
}

pub fn optionalInputIsNullish(comptime T: type, sexp: SEXP) bool {
    if (sexp == R.R_NilValue) return true;
    if (comptime T == f64) {
        return R.TYPEOF(sexp) == R.REALSXP and R.XLENGTH(sexp) > 0 and R.ISNA(R.REAL(sexp)[0]) != 0;
    }
    if (comptime T == i32) {
        return R.TYPEOF(sexp) == R.INTSXP and R.XLENGTH(sexp) > 0 and R.INTEGER(sexp)[0] == R.R_NaInt;
    }
    if (comptime T == bool) {
        return R.TYPEOF(sexp) == R.LGLSXP and R.XLENGTH(sexp) > 0 and R.LOGICAL(sexp)[0] == R.R_NaInt;
    }
    return false;
}

pub fn signalError(err: anyerror) noreturn {
    const msg = errorMessage(err);
    var buf: [256:0]u8 = undefined;
    const n = @min(msg.len, buf.len - 1);
    if (n > 0) @memcpy(buf[0..n], msg[0..n]);
    buf[n] = 0;
    R.Rf_error(&buf);
}

pub fn toRealScalar(sexp: SEXP) ConvertError!f64 {
    try expectType(sexp, R.REALSXP, error.ExpectedReal);
    try expectNonEmpty(sexp);
    const value = R.REAL(sexp)[0];
    if (R.ISNA(value) != 0) return error.ScalarNA;
    return value;
}

pub fn toIntScalar(sexp: SEXP) ConvertError!i32 {
    try expectType(sexp, R.INTSXP, error.ExpectedInteger);
    try expectNonEmpty(sexp);
    const value = R.INTEGER(sexp)[0];
    if (value == R.R_NaInt) return error.ScalarNA;
    return value;
}

pub fn toBoolScalar(sexp: SEXP) ConvertError!bool {
    try expectType(sexp, R.LGLSXP, error.ExpectedLogical);
    try expectNonEmpty(sexp);
    const value = R.LOGICAL(sexp)[0];
    if (value == R.R_NaInt) return error.ScalarNA;
    return value != 0;
}

fn zigrAltRealSliceOrNull(sexp: SEXP) ?[]const f64 {
    if (R.ALTREP(sexp) == 0) return null;

    const data1 = R.R_altrep_data1(sexp);
    if (data1 == R.R_NilValue or R.TYPEOF(data1) != R.EXTPTRSXP) return null;
    if (R.R_ExternalPtrTag(data1) != R.Rf_install(zigr_altreal_slice_tag_name)) return null;

    const addr = R.R_ExternalPtrAddr(data1) orelse return null;
    const wrap: *const ZigrAltRealSliceWrap = @ptrCast(@alignCast(addr));
    return wrap.ptr[0..wrap.len];
}

fn zigrAltIntegerSliceOrNull(sexp: SEXP) ?[]const i32 {
    if (R.ALTREP(sexp) == 0) return null;

    const data1 = R.R_altrep_data1(sexp);
    if (data1 == R.R_NilValue or R.TYPEOF(data1) != R.EXTPTRSXP) return null;
    if (R.R_ExternalPtrTag(data1) != R.Rf_install(zigr_altinteger_slice_tag_name)) return null;

    const addr = R.R_ExternalPtrAddr(data1) orelse return null;
    const wrap: *const ZigrAltIntegerSliceWrap = @ptrCast(@alignCast(addr));
    return wrap.ptr[0..wrap.len];
}

fn zigrAltLogicalSliceOrNull(sexp: SEXP) ?[]const i32 {
    if (R.ALTREP(sexp) == 0) return null;

    const data1 = R.R_altrep_data1(sexp);
    if (data1 == R.R_NilValue or R.TYPEOF(data1) != R.EXTPTRSXP) return null;
    if (R.R_ExternalPtrTag(data1) != R.Rf_install(zigr_altlogical_slice_tag_name)) return null;

    const addr = R.R_ExternalPtrAddr(data1) orelse return null;
    const wrap: *const ZigrAltLogicalSliceWrap = @ptrCast(@alignCast(addr));
    return wrap.ptr[0..wrap.len];
}

fn directRealSliceOrNull(sexp: SEXP) ?[]const f64 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    if (R.ALTREP(sexp) == 0) return R.REAL(sexp)[0..n];

    if (zigrAltRealSliceOrNull(sexp)) |data| return data;

    const ptr = R.DATAPTR_OR_NULL(sexp) orelse return null;
    const real_ptr: [*]const f64 = @ptrCast(@alignCast(ptr));
    return real_ptr[0..n];
}

fn directIntSliceOrNull(sexp: SEXP) ?[]const i32 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    if (R.ALTREP(sexp) == 0) return R.INTEGER(sexp)[0..n];

    if (zigrAltIntegerSliceOrNull(sexp)) |data| return data;

    const ptr = R.DATAPTR_OR_NULL(sexp) orelse return null;
    const int_ptr: [*]const i32 = @ptrCast(@alignCast(ptr));
    return int_ptr[0..n];
}

fn directLogicalSliceOrNull(sexp: SEXP) ?[]const i32 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    if (R.ALTREP(sexp) == 0) return R.LOGICAL(sexp)[0..n];

    if (zigrAltLogicalSliceOrNull(sexp)) |data| return data;

    const ptr = R.DATAPTR_OR_NULL(sexp) orelse return null;
    const logical_ptr: [*]const i32 = @ptrCast(@alignCast(ptr));
    return logical_ptr[0..n];
}

fn directComplexSliceOrNull(sexp: SEXP) ?[]const Rcomplex {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    if (R.ALTREP(sexp) == 0) {
        const ptr = R.COMPLEX(sexp) orelse return null;
        const complex_ptr: [*]const Rcomplex = @ptrCast(@alignCast(ptr));
        return complex_ptr[0..n];
    }

    const ptr = R.DATAPTR_OR_NULL(sexp) orelse return null;
    const complex_ptr: [*]const Rcomplex = @ptrCast(@alignCast(ptr));
    return complex_ptr[0..n];
}

pub fn toRealSliceView(allocator: std.mem.Allocator, sexp: SEXP) ![]const f64 {
    try expectType(sexp, R.REALSXP, error.ExpectedReal);
    return directRealSliceOrNull(sexp) orelse try toRealSlice(allocator, sexp);
}

pub fn toIntSliceView(allocator: std.mem.Allocator, sexp: SEXP) ![]const i32 {
    try expectType(sexp, R.INTSXP, error.ExpectedInteger);
    return directIntSliceOrNull(sexp) orelse try toIntSlice(allocator, sexp);
}

/// REALSXP: allocate and copy. Uses REAL_GET_REGION for ALTREP (one C call),
/// @memcpy from REAL() for non-ALTREP (zero C FFI, matches C baseline).
pub fn toRealSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]f64 {
    try expectType(sexp, R.REALSXP, error.ExpectedReal);
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc(f64, n);
    if (directRealSliceOrNull(sexp)) |data| {
        @memcpy(result, data);
    } else if (R.ALTREP(sexp) != 0) {
        var free_result = AllocSliceCleanup.init(f64, allocator, result);
        cleanup.pushFrame(AllocSliceCleanup.fire, @as(?*anyopaque, @ptrCast(&free_result)));
        defer cleanup.popFrame();
        _ = R.REAL_GET_REGION(sexp, 0, @intCast(n), result.ptr);
    } else {
        @memcpy(result, R.REAL(sexp)[0..n]);
    }
    return result;
}

/// REALSXP: allocate and copy.
pub fn fromRealSlice(slice: []const f64) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    var vec = protect.scoped(R.Rf_allocVector(R.REALSXP, len));
    defer vec.deinit();
    @memcpy(R.REAL(vec.get())[0..slice.len], slice);
    return vec.get();
}

/// INTSXP: allocate and copy. Uses INTEGER_GET_REGION for ALTREP,
/// @memcpy from INTEGER() for non-ALTREP.
pub fn toIntSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]i32 {
    try expectType(sexp, R.INTSXP, error.ExpectedInteger);
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc(i32, n);
    if (directIntSliceOrNull(sexp)) |data| {
        @memcpy(result, data);
    } else if (R.ALTREP(sexp) != 0) {
        var free_result = AllocSliceCleanup.init(i32, allocator, result);
        cleanup.pushFrame(AllocSliceCleanup.fire, @as(?*anyopaque, @ptrCast(&free_result)));
        defer cleanup.popFrame();
        _ = R.INTEGER_GET_REGION(sexp, 0, @intCast(n), result.ptr);
    } else {
        @memcpy(result, R.INTEGER(sexp)[0..n]);
    }
    return result;
}

/// INTSXP: allocate and copy.
pub fn fromIntSlice(slice: []const i32) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    var vec = protect.scoped(R.Rf_allocVector(R.INTSXP, len));
    defer vec.deinit();
    @memcpy(R.INTEGER(vec.get())[0..slice.len], slice);
    return vec.get();
}

/// STRSXP: borrow CHARSXP data into a Zig slice array.
pub fn toStringSlice(allocator: std.mem.Allocator, sexp: SEXP) ![][]const u8 {
    try expectType(sexp, R.STRSXP, error.ExpectedString);
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc([]const u8, n);
    for (0..n) |i| {
        const elt = R.STRING_ELT(sexp, @intCast(i));
        result[i] = if (elt == R.R_NaString) "" else std.mem.sliceTo(R.R_CHAR(elt), 0);
    }
    return result;
}

pub const StringSliceView = struct {
    sexp: SEXP,
    len: usize,

    pub fn at(self: StringSliceView, index: usize) []const u8 {
        const elt = R.STRING_ELT(self.sexp, @intCast(index));
        return if (elt == R.R_NaString) "" else std.mem.sliceTo(R.R_CHAR(elt), 0);
    }

    pub const Iterator = struct {
        view: StringSliceView,
        index: usize = 0,

        pub fn next(self: *Iterator) ?[]const u8 {
            if (self.index >= self.view.len) return null;
            const value = self.view.at(self.index);
            self.index += 1;
            return value;
        }
    };

    pub fn iterator(self: StringSliceView) Iterator {
        return .{ .view = self };
    }
};

/// STRSXP: borrow CHARSXP data without allocating slice headers.
pub fn toStringSliceView(sexp: SEXP) !StringSliceView {
    try expectType(sexp, R.STRSXP, error.ExpectedString);
    return .{
        .sexp = sexp,
        .len = @as(usize, @intCast(R.XLENGTH(sexp))),
    };
}

/// STRSXP: intern strings and build a vector.
pub fn fromStringSlice(slice: []const []const u8) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    var vec = protect.scoped(R.Rf_allocVector(R.STRSXP, len));
    cleanup.pushFrame(Unprot.fire, null);
    defer vec.deinit();
    defer cleanup.popFrame();
    for (0..@as(usize, @intCast(len))) |i| {
        const s = slice[i];
        const cs = R.Rf_mkCharLenCE(@ptrCast(s.ptr), @intCast(s.len), @as(R.cetype_t, @intCast(R.CE_UTF8)));
        R.SET_STRING_ELT(vec.get(), @intCast(i), cs);
    }
    return vec.get();
}

/// LGLSXP: allocate and copy. Uses LOGICAL_GET_REGION for ALTREP,
/// @memcpy from LOGICAL() for non-ALTREP.
pub fn toLogicalSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]i32 {
    try expectType(sexp, R.LGLSXP, error.ExpectedLogical);
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc(i32, n);
    if (directLogicalSliceOrNull(sexp)) |data| {
        @memcpy(result, data);
    } else if (R.ALTREP(sexp) != 0) {
        var free_result = AllocSliceCleanup.init(i32, allocator, result);
        cleanup.pushFrame(AllocSliceCleanup.fire, @as(?*anyopaque, @ptrCast(&free_result)));
        defer cleanup.popFrame();
        _ = R.LOGICAL_GET_REGION(sexp, 0, @intCast(n), result.ptr);
    } else {
        @memcpy(result, R.LOGICAL(sexp)[0..n]);
    }
    return result;
}

pub fn toLogicalSliceView(allocator: std.mem.Allocator, sexp: SEXP) ![]const i32 {
    try expectType(sexp, R.LGLSXP, error.ExpectedLogical);
    return directLogicalSliceOrNull(sexp) orelse try toLogicalSlice(allocator, sexp);
}

/// LGLSXP: build from i32 slice.
pub fn fromLogicalSlice(slice: []const i32) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    var vec = protect.scoped(R.Rf_allocVector(R.LGLSXP, len));
    defer vec.deinit();
    @memcpy(R.LOGICAL(vec.get())[0..slice.len], slice);
    return vec.get();
}

/// VECSXP: borrow list elements into a SEXP slice.
pub fn toListSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]SEXP {
    try expectType(sexp, R.VECSXP, error.ExpectedList);
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc(SEXP, n);
    for (0..n) |i| result[i] = R.VECTOR_ELT(sexp, @intCast(i));
    return result;
}

/// VECSXP: build from a SEXP slice.
pub fn fromListSlice(slice: []const SEXP) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    var vec = protect.scoped(R.Rf_allocVector(R.VECSXP, len));
    defer vec.deinit();
    for (0..@as(usize, @intCast(len))) |i| {
        R.SET_VECTOR_ELT(vec.get(), @intCast(i), slice[i]);
    }
    return vec.get();
}

/// RAWSXP: allocate and copy. Uses RAW_GET_REGION for ALTREP,
/// @memcpy from RAW() for non-ALTREP.
pub fn toRawSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]const u8 {
    try expectType(sexp, R.RAWSXP, error.ExpectedRaw);
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc(u8, n);
    if (R.ALTREP(sexp) != 0) {
        var free_result = AllocSliceCleanup.init(u8, allocator, result);
        cleanup.pushFrame(AllocSliceCleanup.fire, @as(?*anyopaque, @ptrCast(&free_result)));
        defer cleanup.popFrame();
        _ = R.RAW_GET_REGION(sexp, 0, @intCast(n), result.ptr);
    } else {
        @memcpy(result, R.RAW(sexp)[0..n]);
    }
    return result;
}

/// RAWSXP: allocate and copy.
pub fn fromRawSlice(slice: []const u8) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    var vec = protect.scoped(R.Rf_allocVector(R.RAWSXP, len));
    defer vec.deinit();
    @memcpy(R.RAW(vec.get())[0..slice.len], slice);
    return vec.get();
}

/// CPLXSXP: allocate and copy. Uses COMPLEX_GET_REGION for ALTREP,
/// @memcpy from COMPLEX() for non-ALTREP.
pub fn toComplexSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]const Rcomplex {
    try expectType(sexp, R.CPLXSXP, error.ExpectedComplex);
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const result = try allocator.alloc(Rcomplex, n);
    if (R.ALTREP(sexp) != 0) {
        var free_result = AllocSliceCleanup.init(Rcomplex, allocator, result);
        cleanup.pushFrame(AllocSliceCleanup.fire, @as(?*anyopaque, @ptrCast(&free_result)));
        defer cleanup.popFrame();
        _ = R.COMPLEX_GET_REGION(sexp, 0, @intCast(n), @ptrCast(result.ptr));
    } else {
        const src: [*]const Rcomplex = @ptrCast(@alignCast(R.COMPLEX(sexp).?));
        @memcpy(result, src[0..n]);
    }
    return result;
}

pub fn toComplexSliceView(allocator: std.mem.Allocator, sexp: SEXP) ![]const Rcomplex {
    try expectType(sexp, R.CPLXSXP, error.ExpectedComplex);
    return directComplexSliceOrNull(sexp) orelse try toComplexSlice(allocator, sexp);
}

/// CPLXSXP: allocate and copy.
pub fn fromComplexSlice(slice: []const Rcomplex) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    var vec = protect.scoped(R.Rf_allocVector(R.CPLXSXP, len));
    defer vec.deinit();
    const dst: [*]Rcomplex = @ptrCast(@alignCast(R.COMPLEX(vec.get()).?));
    @memcpy(dst[0..slice.len], slice);
    return vec.get();
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

fn sexpToZig(comptime T: type, sexp: SEXP, arena: std.mem.Allocator) !T {
    if (comptime @typeInfo(T) == .optional) {
        const child = @typeInfo(T).optional.child;
        if (optionalInputIsNullish(child, sexp)) return null;
        return try sexpToZig(child, sexp, arena);
    }
    if (comptime T == f64) return try toRealScalar(sexp);
    if (comptime T == i32) return try toIntScalar(sexp);
    if (comptime T == bool) return try toBoolScalar(sexp);
    if (comptime T == []const f64) return try toRealSlice(arena, sexp);
    if (comptime T == []const i32) return try toIntSlice(arena, sexp);
    if (comptime T == []const []const u8) return try toStringSlice(arena, sexp);
    if (comptime T == []const u8) return try toRawSlice(arena, sexp);
    if (comptime T == []const Rcomplex) return try toComplexSlice(arena, sexp);
    if (comptime T == SEXP) return sexp;
    if (comptime @typeInfo(T) == .@"struct") {
        return try structFromSexp(T, sexp, arena);
    }
    @compileError("unsupported type in struct conversion: " ++ @typeName(T));
}

fn structToSexp(st: anytype, comptime T: type, arena: std.mem.Allocator) SEXP {
    const fields = @typeInfo(T).@"struct".fields;
    const n: R.R_xlen_t = @intCast(fields.len);
    var vec = protect.scoped(R.Rf_allocVector(R.VECSXP, n));
    var names = protect.scoped(R.Rf_allocVector(R.STRSXP, n));
    defer vec.deinit();
    defer names.deinit();

    inline for (fields, 0..) |field, i| {
        const val = @field(st, field.name);
        const elt = zigToSexp(val, field.type, arena);
        _ = R.SET_VECTOR_ELT(vec.get(), @intCast(i), elt);
        const cs = R.Rf_mkCharLenCE(@ptrCast(field.name.ptr), @intCast(field.name.len), @as(R.cetype_t, @intCast(R.CE_UTF8)));
        R.SET_STRING_ELT(names.get(), @intCast(i), cs);
    }

    _ = R.Rf_namesgets(vec.get(), names.get());
    return vec.get();
}

fn structFromSexp(comptime T: type, sexp: SEXP, arena: std.mem.Allocator) !T {
    const fields = @typeInfo(T).@"struct".fields;
    const ns = try expectNamedList(sexp);
    var result: T = undefined;

    inline for (fields) |field| {
        var found = false;
        for (0..@as(usize, @intCast(R.XLENGTH(ns)))) |i| {
            const elt = R.STRING_ELT(ns, @intCast(i));
            if (elt == R.R_NaString) continue;
            const cn = std.mem.sliceTo(R.R_CHAR(elt), 0);
            if (std.mem.eql(u8, cn, field.name)) {
                const elem = R.VECTOR_ELT(sexp, @intCast(i));
                @field(result, field.name) = try sexpToZig(field.type, elem, arena);
                found = true;
                break;
            }
        }
        if (!found) {
            if (comptime @typeInfo(field.type) != .optional) return error.MissingField;
            @field(result, field.name) = @as(field.type, null);
        }
    }

    return result;
}

const real_chunk_len = simd.f64_lanes * 8;
const int_chunk_len = simd.i32_lanes * 8;

const RealChunk = struct {
    offset: usize,
    data: []const f64,
};

const IntChunk = struct {
    offset: usize,
    data: []const i32,
};

const RealChunkIter = struct {
    sexp: SEXP,
    n: usize,
    offset: usize = 0,
    direct: ?[]const f64,
    buf: [real_chunk_len]f64 = undefined,

    fn init(sexp: SEXP) RealChunkIter {
        const n = @as(usize, @intCast(R.XLENGTH(sexp)));
        return .{
            .sexp = sexp,
            .n = n,
            .direct = directRealSliceOrNull(sexp),
        };
    }

    fn next(self: *RealChunkIter) ?RealChunk {
        if (self.offset >= self.n) return null;

        if (self.direct) |data| {
            self.offset = self.n;
            return .{ .offset = 0, .data = data };
        }

        const chunk_offset = self.offset;
        const want = @min(self.buf.len, self.n - self.offset);
        const got = @as(usize, @intCast(R.REAL_GET_REGION(self.sexp, @intCast(self.offset), @intCast(want), self.buf[0..].ptr)));
        if (got == 0) signalError(error.AltRepRegionRead);
        self.offset += got;
        return .{ .offset = chunk_offset, .data = self.buf[0..got] };
    }
};

const IntChunkIter = struct {
    sexp: SEXP,
    n: usize,
    offset: usize = 0,
    direct: ?[]const i32,
    buf: [int_chunk_len]i32 = undefined,

    fn init(sexp: SEXP) IntChunkIter {
        const n = @as(usize, @intCast(R.XLENGTH(sexp)));
        return .{
            .sexp = sexp,
            .n = n,
            .direct = directIntSliceOrNull(sexp),
        };
    }

    fn next(self: *IntChunkIter) ?IntChunk {
        if (self.offset >= self.n) return null;

        if (self.direct) |data| {
            self.offset = self.n;
            return .{ .offset = 0, .data = data };
        }

        const chunk_offset = self.offset;
        const want = @min(self.buf.len, self.n - self.offset);
        const got = @as(usize, @intCast(R.INTEGER_GET_REGION(self.sexp, @intCast(self.offset), @intCast(want), self.buf[0..].ptr)));
        if (got == 0) signalError(error.AltRepRegionRead);
        self.offset += got;
        return .{ .offset = chunk_offset, .data = self.buf[0..got] };
    }
};

// LogicalChunkIter reuses IntChunk because both store i32 data under the hood.
// R's LOGICAL() and INTEGER() both return int* -- the difference is semantic only.
const LogicalChunkIter = struct {
    sexp: SEXP,
    n: usize,
    offset: usize = 0,
    direct: ?[]const i32,
    buf: [int_chunk_len]i32 = undefined,

    fn init(sexp: SEXP) LogicalChunkIter {
        const n = @as(usize, @intCast(R.XLENGTH(sexp)));
        return .{
            .sexp = sexp,
            .n = n,
            .direct = directLogicalSliceOrNull(sexp),
        };
    }

    fn next(self: *LogicalChunkIter) ?IntChunk {
        if (self.offset >= self.n) return null;

        if (self.direct) |data| {
            self.offset = self.n;
            return .{ .offset = 0, .data = data };
        }

        const chunk_offset = self.offset;
        const want = @min(self.buf.len, self.n - self.offset);
        const got = @as(usize, @intCast(R.LOGICAL_GET_REGION(self.sexp, @intCast(self.offset), @intCast(want), self.buf[0..].ptr)));
        if (got == 0) signalError(error.AltRepRegionRead);
        self.offset += got;
        return .{ .offset = chunk_offset, .data = self.buf[0..got] };
    }
};

/// Sum of a REALSXP using SIMD @Vector reduction.
/// Up to 2.5x faster than a scalar loop for large vectors.
pub fn sum(sexp: SEXP) f64 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    if (n == 0) return 0.0;

    const lanes = simd.f64_lanes;
    var total: f64 = 0.0;
    var iter = RealChunkIter.init(sexp);
    while (iter.next()) |chunk| {
        var i: usize = 0;
        if (chunk.data.len >= lanes) {
            var vec_total: @Vector(lanes, f64) = @splat(0.0);
            const end = chunk.data.len - (chunk.data.len % lanes);
            while (i < end) : (i += lanes) {
                vec_total += chunk.data[i..][0..lanes].*;
            }
            total += @reduce(.Add, vec_total);
        }
        while (i < chunk.data.len) : (i += 1) total += chunk.data[i];
    }

    return total;
}

/// Sum of an INTSXP using direct owned-backing or INTEGER_GET_REGION.
pub fn sumInt(sexp: SEXP) i64 {
    expectType(sexp, R.INTSXP, error.ExpectedInteger) catch |err| signalError(err);

    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    if (n == 0) return 0;

    var total: i64 = 0;
    var iter = IntChunkIter.init(sexp);
    while (iter.next()) |chunk| {
        for (chunk.data) |value| total += value;
    }

    return total;
}

/// Count TRUE values in a LGLSXP using direct owned-backing or LOGICAL_GET_REGION.
pub fn countTrue(sexp: SEXP) i64 {
    expectType(sexp, R.LGLSXP, error.ExpectedLogical) catch |err| signalError(err);

    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    if (n == 0) return 0;

    var total: i64 = 0;
    var iter = LogicalChunkIter.init(sexp);
    while (iter.next()) |chunk| {
        for (chunk.data) |value| {
            if (value == 1) total += 1;
        }
    }

    return total;
}

fn minmaxIntChunks(comptime find_min: bool, iter: anytype, empty_value: i32) i32 {
    const lanes = simd.i32_lanes;
    var initialized = false;
    var best = empty_value;

    while (iter.next()) |chunk| {
        var local_best = chunk.data[0];
        var i: usize = 1;

        if (chunk.data.len >= lanes) {
            var vec: @Vector(lanes, i32) = @splat(chunk.data[0]);
            i = 0;
            const end = chunk.data.len - (chunk.data.len % lanes);
            while (i < end) : (i += lanes) {
                const values: @Vector(lanes, i32) = chunk.data[i..][0..lanes].*;
                vec = @select(i32, if (find_min) values < vec else values > vec, values, vec);
            }
            local_best = if (find_min) @reduce(.Min, vec) else @reduce(.Max, vec);
        }

        while (i < chunk.data.len) : (i += 1) {
            const better = if (find_min) chunk.data[i] < local_best else chunk.data[i] > local_best;
            if (better) local_best = chunk.data[i];
        }

        if (!initialized) {
            initialized = true;
            best = local_best;
            continue;
        }

        const better = if (find_min) local_best < best else local_best > best;
        if (better) best = local_best;
    }

    return best;
}

fn argminmaxIntChunks(comptime find_min: bool, iter: anytype) i64 {
    const lanes = simd.i32_lanes;
    const lane_offsets: @Vector(lanes, usize) = comptime blk: {
        var seq: [lanes]usize = undefined;
        for (&seq, 0..) |*s, k| s.* = k;
        break :blk @as(@Vector(lanes, usize), seq);
    };

    var initialized = false;
    var best: i32 = 0;
    var best_idx: usize = 0;

    while (iter.next()) |chunk| {
        var local_best = chunk.data[0];
        var local_idx = chunk.offset;
        var base: usize = 1;

        if (chunk.data.len >= lanes) {
            var vec_val: @Vector(lanes, i32) = @splat(chunk.data[0]);
            var vec_idx: @Vector(lanes, usize) = @splat(chunk.offset);
            base = 0;
            const end = chunk.data.len - (chunk.data.len % lanes);
            while (base < end) : (base += lanes) {
                const values: @Vector(lanes, i32) = chunk.data[base..][0..lanes].*;
                const cmp = if (find_min) values < vec_val else values > vec_val;
                const base_offset: @Vector(lanes, usize) = @splat(chunk.offset + base);
                const idx = base_offset + lane_offsets;
                vec_val = @select(i32, cmp, values, vec_val);
                vec_idx = @select(usize, cmp, idx, vec_idx);
            }

            const vals: [lanes]i32 = vec_val;
            const idxs: [lanes]usize = vec_idx;
            local_best = vals[0];
            local_idx = idxs[0];
            for (1..lanes) |j| {
                const better = if (find_min) vals[j] < local_best else vals[j] > local_best;
                if (better) {
                    local_best = vals[j];
                    local_idx = idxs[j];
                }
            }
        }

        while (base < chunk.data.len) : (base += 1) {
            const better = if (find_min) chunk.data[base] < local_best else chunk.data[base] > local_best;
            if (better) {
                local_best = chunk.data[base];
                local_idx = chunk.offset + base;
            }
        }

        if (!initialized) {
            initialized = true;
            best = local_best;
            best_idx = local_idx;
            continue;
        }

        const better = if (find_min) local_best < best else local_best > best;
        if (better) {
            best = local_best;
            best_idx = local_idx;
        }
    }

    return if (initialized) @intCast(best_idx) else -1;
}

/// Minimum of an INTSXP using direct owned-backing or INTEGER_GET_REGION.
pub fn minInt(sexp: SEXP) i32 {
    expectType(sexp, R.INTSXP, error.ExpectedInteger) catch |err| signalError(err);

    var iter = IntChunkIter.init(sexp);
    return minmaxIntChunks(true, &iter, std.math.maxInt(i32));
}

/// Maximum of an INTSXP using direct owned-backing or INTEGER_GET_REGION.
pub fn maxInt(sexp: SEXP) i32 {
    expectType(sexp, R.INTSXP, error.ExpectedInteger) catch |err| signalError(err);

    var iter = IntChunkIter.init(sexp);
    return minmaxIntChunks(false, &iter, std.math.minInt(i32));
}

/// Index of the minimum integer value (0-based), preserving the first hit.
pub fn argminInt(sexp: SEXP) i64 {
    expectType(sexp, R.INTSXP, error.ExpectedInteger) catch |err| signalError(err);

    var iter = IntChunkIter.init(sexp);
    return argminmaxIntChunks(true, &iter);
}

/// Index of the maximum integer value (0-based), preserving the first hit.
pub fn argmaxInt(sexp: SEXP) i64 {
    expectType(sexp, R.INTSXP, error.ExpectedInteger) catch |err| signalError(err);

    var iter = IntChunkIter.init(sexp);
    return argminmaxIntChunks(false, &iter);
}

/// Minimum of a LGLSXP over raw logical codes using direct owned-backing or LOGICAL_GET_REGION.
pub fn minLogical(sexp: SEXP) i32 {
    expectType(sexp, R.LGLSXP, error.ExpectedLogical) catch |err| signalError(err);

    var iter = LogicalChunkIter.init(sexp);
    return minmaxIntChunks(true, &iter, std.math.maxInt(i32));
}

/// Maximum of a LGLSXP over raw logical codes using direct owned-backing or LOGICAL_GET_REGION.
pub fn maxLogical(sexp: SEXP) i32 {
    expectType(sexp, R.LGLSXP, error.ExpectedLogical) catch |err| signalError(err);

    var iter = LogicalChunkIter.init(sexp);
    return minmaxIntChunks(false, &iter, std.math.minInt(i32));
}

/// Index of the minimum raw logical code (0-based), preserving the first hit.
pub fn argminLogical(sexp: SEXP) i64 {
    expectType(sexp, R.LGLSXP, error.ExpectedLogical) catch |err| signalError(err);

    var iter = LogicalChunkIter.init(sexp);
    return argminmaxIntChunks(true, &iter);
}

/// Index of the maximum raw logical code (0-based), preserving the first hit.
pub fn argmaxLogical(sexp: SEXP) i64 {
    expectType(sexp, R.LGLSXP, error.ExpectedLogical) catch |err| signalError(err);

    var iter = LogicalChunkIter.init(sexp);
    return argminmaxIntChunks(false, &iter);
}

/// Mean of a REALSXP using SIMD.
pub fn mean(sexp: SEXP) f64 {
    return sum(sexp) / @as(f64, @floatFromInt(R.XLENGTH(sexp)));
}

/// Sum of squares (L2 norm squared) of a REALSXP using SIMD.
pub fn norm2(sexp: SEXP) f64 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    if (n == 0) return 0.0;

    const lanes = simd.f64_lanes;
    var total: f64 = 0.0;
    var iter = RealChunkIter.init(sexp);
    while (iter.next()) |chunk| {
        var i: usize = 0;
        if (chunk.data.len >= lanes) {
            var vec_total: @Vector(lanes, f64) = @splat(0.0);
            const end = chunk.data.len - (chunk.data.len % lanes);
            while (i < end) : (i += lanes) {
                const v: @Vector(lanes, f64) = chunk.data[i..][0..lanes].*;
                vec_total += v * v;
            }
            total += @reduce(.Add, vec_total);
        }
        while (i < chunk.data.len) : (i += 1) {
            const v = chunk.data[i];
            total += v * v;
        }
    }

    return total;
}

/// Minimum of a REALSXP using SIMD @Vector reduction.
pub fn min(sexp: SEXP) f64 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    if (n == 0) return std.math.inf(f64);

    const lanes = simd.f64_lanes;
    var value = std.math.inf(f64);
    var iter = RealChunkIter.init(sexp);
    while (iter.next()) |chunk| {
        var i: usize = 0;
        if (chunk.data.len >= lanes) {
            var vec: @Vector(lanes, f64) = @splat(std.math.inf(f64));
            const end = chunk.data.len - (chunk.data.len % lanes);
            while (i < end) : (i += lanes) {
                vec = @select(f64, chunk.data[i..][0..lanes].* < vec, chunk.data[i..][0..lanes].*, vec);
            }
            const vec_min = @reduce(.Min, vec);
            if (vec_min < value) value = vec_min;
        }
        while (i < chunk.data.len) : (i += 1) {
            if (chunk.data[i] < value) value = chunk.data[i];
        }
    }

    return value;
}

/// Maximum of a REALSXP using SIMD @Vector reduction.
pub fn max(sexp: SEXP) f64 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    if (n == 0) return -std.math.inf(f64);

    const lanes = simd.f64_lanes;
    var value = -std.math.inf(f64);
    var iter = RealChunkIter.init(sexp);
    while (iter.next()) |chunk| {
        var i: usize = 0;
        if (chunk.data.len >= lanes) {
            var vec: @Vector(lanes, f64) = @splat(-std.math.inf(f64));
            const end = chunk.data.len - (chunk.data.len % lanes);
            while (i < end) : (i += lanes) {
                vec = @select(f64, chunk.data[i..][0..lanes].* > vec, chunk.data[i..][0..lanes].*, vec);
            }
            const vec_max = @reduce(.Max, vec);
            if (vec_max > value) value = vec_max;
        }
        while (i < chunk.data.len) : (i += 1) {
            if (chunk.data[i] > value) value = chunk.data[i];
        }
    }

    return value;
}

fn argminmax(comptime find_min: bool, sexp: SEXP) i64 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    if (n == 0) return -1;

    const lanes = simd.f64_lanes;
    const lane_offsets: @Vector(lanes, usize) = comptime blk: {
        var seq: [lanes]usize = undefined;
        for (&seq, 0..) |*s, k| s.* = k;
        break :blk @as(@Vector(lanes, usize), seq);
    };
    var initialized = false;
    var best: f64 = 0.0;
    var best_idx: usize = 0;
    var iter = RealChunkIter.init(sexp);

    while (iter.next()) |chunk| {
        var local_best = chunk.data[0];
        var local_idx = chunk.offset;
        var base: usize = 1;

        if (chunk.data.len >= lanes) {
            var vec_val: @Vector(lanes, f64) = @splat(chunk.data[0]);
            var vec_idx: @Vector(lanes, usize) = @splat(chunk.offset);
            base = 0;
            const end = chunk.data.len - (chunk.data.len % lanes);
            while (base < end) : (base += lanes) {
                const v: @Vector(lanes, f64) = chunk.data[base..][0..lanes].*;
                const cmp = if (find_min) v < vec_val else v > vec_val;
                const base_offset: @Vector(lanes, usize) = @splat(chunk.offset + base);
                const idx = base_offset + lane_offsets;
                vec_val = @select(f64, cmp, v, vec_val);
                vec_idx = @select(usize, cmp, idx, vec_idx);
            }
            const vals: [lanes]f64 = vec_val;
            const idxs: [lanes]usize = vec_idx;
            local_best = vals[0];
            local_idx = idxs[0];
            for (1..lanes) |j| {
                const better = if (find_min) vals[j] < local_best else vals[j] > local_best;
                if (better) {
                    local_best = vals[j];
                    local_idx = idxs[j];
                }
            }
        }

        while (base < chunk.data.len) : (base += 1) {
            const better = if (find_min) chunk.data[base] < local_best else chunk.data[base] > local_best;
            if (better) {
                local_best = chunk.data[base];
                local_idx = chunk.offset + base;
            }
        }

        if (!initialized) {
            initialized = true;
            best = local_best;
            best_idx = local_idx;
            continue;
        }

        const better = if (find_min) local_best < best else local_best > best;
        if (better) {
            best = local_best;
            best_idx = local_idx;
        }
    }

    return @intCast(best_idx);
}

/// Index of the minimum value in a REALSXP (0-based).
pub fn argmin(sexp: SEXP) i64 {
    return argminmax(true, sexp);
}

/// Index of the maximum value in a REALSXP (0-based).
pub fn argmax(sexp: SEXP) i64 {
    return argminmax(false, sexp);
}

/// Sum of a REALSXP excluding NA values. Uses @select for branchless NA masking.
pub fn sum_narm(sexp: SEXP) f64 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    if (n == 0) return 0.0;

    const lanes = simd.f64_lanes;
    const na_bits: @Vector(lanes, u64) = @splat(@as(u64, @bitCast(R.R_NaReal)));
    const zero: @Vector(lanes, f64) = @splat(0.0);
    var total: f64 = 0.0;
    var iter = RealChunkIter.init(sexp);
    while (iter.next()) |chunk| {
        var i: usize = 0;
        if (chunk.data.len >= lanes) {
            var vec_total: @Vector(lanes, f64) = @splat(0.0);
            const end = chunk.data.len - (chunk.data.len % lanes);
            while (i < end) : (i += lanes) {
                const v: @Vector(lanes, f64) = chunk.data[i..][0..lanes].*;
                const ok = @as(@Vector(lanes, u64), @bitCast(v)) != na_bits;
                vec_total += @select(f64, ok, v, zero);
            }
            total += @reduce(.Add, vec_total);
        }
        while (i < chunk.data.len) : (i += 1) {
            if (R.ISNA(chunk.data[i]) != 0) continue;
            total += chunk.data[i];
        }
    }

    return total;
}

/// Mean of a REALSXP excluding NA values.
pub fn mean_narm(sexp: SEXP) f64 {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    if (n == 0) return 0.0;

    const lanes = simd.f64_lanes;
    const na_bits: @Vector(lanes, u64) = @splat(@as(u64, @bitCast(R.R_NaReal)));
    const zero: @Vector(lanes, f64) = @splat(0.0);
    const one: @Vector(lanes, f64) = @splat(1.0);
    var total: f64 = 0.0;
    var count: i64 = 0;
    var iter = RealChunkIter.init(sexp);
    while (iter.next()) |chunk| {
        var i: usize = 0;
        if (chunk.data.len >= lanes) {
            var vec_total: @Vector(lanes, f64) = @splat(0.0);
            var vec_cnt: @Vector(lanes, f64) = @splat(0.0);
            const end = chunk.data.len - (chunk.data.len % lanes);
            while (i < end) : (i += lanes) {
                const v: @Vector(lanes, f64) = chunk.data[i..][0..lanes].*;
                const ok = @as(@Vector(lanes, u64), @bitCast(v)) != na_bits;
                vec_total += @select(f64, ok, v, zero);
                vec_cnt += @select(f64, ok, one, zero);
            }
            total += @reduce(.Add, vec_total);
            count += @as(i64, @intFromFloat(@reduce(.Add, vec_cnt)));
        }
        while (i < chunk.data.len) : (i += 1) {
            if (R.ISNA(chunk.data[i]) != 0) continue;
            total += chunk.data[i];
            count += 1;
        }
    }

    return if (count == 0) R.R_NaReal else total / @as(f64, @floatFromInt(count));
}

/// Element-wise minimum of two REALSXPs. Returns a new REALSXP.
pub fn pmin(a: SEXP, b: SEXP) SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const da = toRealSliceView(arena.allocator(), a) catch |err| signalError(err);
    const db = toRealSliceView(arena.allocator(), b) catch |err| signalError(err);
    const n = if (da.len == 0 or db.len == 0) @as(usize, 0) else @max(da.len, db.len);

    var result = protect.scoped(R.Rf_allocVector(R.REALSXP, @intCast(n)));
    defer result.deinit();
    const rp = R.REAL(result.get());

    for (0..n) |i| {
        rp[i] = @min(da[i % da.len], db[i % db.len]);
    }

    return result.get();
}

/// Element-wise maximum of two REALSXPs. Returns a new REALSXP.
pub fn pmax(a: SEXP, b: SEXP) SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const da = toRealSliceView(arena.allocator(), a) catch |err| signalError(err);
    const db = toRealSliceView(arena.allocator(), b) catch |err| signalError(err);
    const n = if (da.len == 0 or db.len == 0) @as(usize, 0) else @max(da.len, db.len);

    var result = protect.scoped(R.Rf_allocVector(R.REALSXP, @intCast(n)));
    defer result.deinit();
    const rp = R.REAL(result.get());

    for (0..n) |i| {
        rp[i] = @max(da[i % da.len], db[i % db.len]);
    }

    return result.get();
}

/// Cumulative sum of a REALSXP. Returns a new REALSXP.
pub fn cumsum(sexp: SEXP) SEXP {
    const n = @as(usize, @intCast(R.XLENGTH(sexp)));
    const data = R.REAL(sexp);

    var result = protect.scoped(R.Rf_allocVector(R.REALSXP, @intCast(n)));
    defer result.deinit();
    const rp = R.REAL(result.get());

    var total: f64 = 0.0;
    var i: usize = 0;
    while (i + 3 < n) {
        total += data[i];
        rp[i] = total;
        total += data[i + 1];
        rp[i + 1] = total;
        total += data[i + 2];
        rp[i + 2] = total;
        total += data[i + 3];
        rp[i + 3] = total;
        i += 4;
    }
    while (i < n) : (i += 1) {
        total += data[i];
        rp[i] = total;
    }

    return result.get();
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
/// non-optional fields signal an R error.
pub fn fromSEXP(comptime T: type, sexp: SEXP, arena: std.mem.Allocator) T {
    return structFromSexp(T, sexp, arena) catch |err| signalError(err);
}

/// Map a Zig numeric type to the corresponding R SEXPTYPE constant.
pub fn typeToSEXPTYPE(comptime T: type) R.SEXPTYPE {
    return switch (T) {
        f64 => R.REALSXP,
        i32 => R.INTSXP,
        u8 => R.RAWSXP,
        Rcomplex => R.CPLXSXP,
        else => @compileError("unsupported type: " ++ @typeName(T)),
    };
}

/// Get a mutable pointer to the underlying data array of a SEXP.
/// The caller is responsible for ensuring the SEXP is of the expected type.
pub fn dataPtr(comptime T: type, sexp: SEXP) [*]T {
    return switch (T) {
        f64 => @ptrCast(R.REAL(sexp)),
        i32 => @ptrCast(R.INTEGER(sexp)),
        u8 => @ptrCast(R.RAW(sexp)),
        Rcomplex => @ptrCast(@alignCast(R.COMPLEX(sexp).?)),
        else => @compileError("unsupported type: " ++ @typeName(T)),
    };
}
