//! Convert between Zig values and R SEXPs.
//!
//! Borrowed views stay within the source R call. ALTREP fallback copies one
//! contiguous native buffer because R may expose only regions.

const std = @import("std");
const SEXP = @import("sexp.zig").SEXP;
const xlength = @import("sexp.zig").xlength;
const tryXlength = @import("sexp.zig").tryXlength;
const sexp_mod = @import("sexp.zig");
const R = @import("R");
const simd = @import("simd");
const cleanup = @import("cleanup");
const protect = @import("protect.zig");

pub const Rcomplex = extern struct { r: f64, i: f64 };

const Unprot = struct {
    fn fire(_: ?*anyopaque) void {
        // Keep ReleaseSafe/Debug depth accounting in step with the actual
        // protection stack when R interrupts fromStringSlice mid-loop.
        protect.unprotect();
    }
};

const AllocSliceCleanup = struct {
    allocator: std.mem.Allocator,
    memory: []u8,
    alignment: std.mem.Alignment,
    return_address: usize,

    fn init(comptime T: type, allocator: std.mem.Allocator, slice: []T, ra: usize) AllocSliceCleanup {
        return .{
            .allocator = allocator,
            .memory = std.mem.sliceAsBytes(slice),
            .alignment = .fromByteUnits(@alignOf(T)),
            .return_address = ra,
        };
    }

    /// Its inline state survives an R longjmp without retaining a stack pointer.
    fn fire(self: *AllocSliceCleanup) void {
        self.allocator.rawFree(self.memory, self.alignment, self.return_address);
    }
};

pub const ConvertError = error{
    ExpectedReal,
    ExpectedInteger,
    ExpectedLogical,
    ExpectedString,
    ExpectedList,
    ExpectedSchema,
    ExpectedRaw,
    ExpectedComplex,
    ZeroLength,
    ScalarLength,
    ScalarNA,
    AltRepRegionRead,
    SchemaLength,
    SchemaNames,
    SchemaAttributes,
    OutOfMemory,
    NegativeLength,
    LengthOverflow,
};

fn expectType(sexp: SEXP, expected: c_uint, comptime err: ConvertError) ConvertError!void {
    if (@as(c_uint, sexp_mod.typeTag(sexp)) != expected) return err;
}

fn expectScalarLength(sexp: SEXP) ConvertError!void {
    const len = R.XLENGTH(sexp);
    if (len == 0) return error.ZeroLength;
    if (len != 1) return error.ScalarLength;
}

pub fn errorMessage(err: anyerror) []const u8 {
    return switch (err) {
        error.ExpectedReal => "expected REALSXP",
        error.ExpectedInteger => "expected INTSXP",
        error.ExpectedLogical => "expected LGLSXP",
        error.ExpectedString => "expected STRSXP",
        error.ExpectedList => "expected VECSXP",
        error.ExpectedSchema => "expected fixed-schema named VECSXP",
        error.ExpectedRaw => "expected RAWSXP",
        error.ExpectedComplex => "expected CPLXSXP",
        error.ZeroLength => "expected non-empty vector",
        error.ScalarLength => "scalar inputs must have length one",
        error.ScalarNA => "scalar inputs must not be NA",
        error.AltRepRegionRead => "ALTREP region read failed",
        error.SchemaLength => "fixed schema field count does not match",
        error.SchemaNames => "fixed schema names do not match",
        error.SchemaAttributes => "fixed schema has unsupported attributes",
        error.OutOfMemory => "out of memory during SEXP conversion",
        else => @errorName(err),
    };
}

pub fn optionalInputIsNullish(comptime T: type, sexp: SEXP) bool {
    if (sexp == R.R_NilValue) return true;
    if (comptime T == f64) {
        return @as(c_uint, sexp_mod.typeTag(sexp)) == R.REALSXP and
            R.XLENGTH(sexp) == 1 and
            R.ISNA(R.REAL(sexp)[0]) != 0;
    }
    if (comptime T == i32) {
        return @as(c_uint, sexp_mod.typeTag(sexp)) == R.INTSXP and
            R.XLENGTH(sexp) == 1 and
            R.INTEGER(sexp)[0] == R.R_NaInt;
    }
    if (comptime T == bool) {
        return @as(c_uint, sexp_mod.typeTag(sexp)) == R.LGLSXP and
            R.XLENGTH(sexp) == 1 and
            R.LOGICAL(sexp)[0] == R.R_NaInt;
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

/// R NA is distinct from IEEE NaN.
pub fn toRealScalar(sexp: SEXP) ConvertError!f64 {
    try expectType(sexp, R.REALSXP, error.ExpectedReal);
    try expectScalarLength(sexp);
    const value = R.REAL(sexp)[0];
    if (R.ISNA(value) != 0) return error.ScalarNA;
    return value;
}

pub fn toIntScalar(sexp: SEXP) ConvertError!i32 {
    try expectType(sexp, R.INTSXP, error.ExpectedInteger);
    try expectScalarLength(sexp);
    const value = R.INTEGER(sexp)[0];
    if (value == R.R_NaInt) return error.ScalarNA;
    return value;
}

pub fn toBoolScalar(sexp: SEXP) ConvertError!bool {
    try expectType(sexp, R.LGLSXP, error.ExpectedLogical);
    try expectScalarLength(sexp);
    const value = R.LOGICAL(sexp)[0];
    if (value == R.R_NaInt) return error.ScalarNA;
    return value != 0;
}

pub fn toOptionalRealScalar(sexp: SEXP) ConvertError!?f64 {
    if (sexp == R.R_NilValue) return null;
    try expectType(sexp, R.REALSXP, error.ExpectedReal);
    try expectScalarLength(sexp);
    const value = R.REAL(sexp)[0];
    if (R.ISNA(value) != 0) return null;
    return value;
}

pub fn toOptionalIntScalar(sexp: SEXP) ConvertError!?i32 {
    if (sexp == R.R_NilValue) return null;
    try expectType(sexp, R.INTSXP, error.ExpectedInteger);
    try expectScalarLength(sexp);
    const value = R.INTEGER(sexp)[0];
    if (value == R.R_NaInt) return null;
    return value;
}

pub fn toOptionalBoolScalar(sexp: SEXP) ConvertError!?bool {
    if (sexp == R.R_NilValue) return null;
    try expectType(sexp, R.LGLSXP, error.ExpectedLogical);
    try expectScalarLength(sexp);
    const value = R.LOGICAL(sexp)[0];
    if (value == R.R_NaInt) return null;
    return value != 0;
}

const VectorRepresentation = struct {
    len: usize,
    altrep: bool,
};

fn vectorRepresentation(sexp: SEXP) ConvertError!VectorRepresentation {
    const altrep = R.ALTREP(sexp) != 0;
    const raw_len = if (altrep) R.XLENGTH(sexp) else sexp_mod.fastLength(sexp);
    if (raw_len < 0) return error.NegativeLength;
    return .{ .len = @intCast(raw_len), .altrep = altrep };
}

fn directRealSliceOrNull(sexp: SEXP, representation: VectorRepresentation) ?[]const f64 {
    const n = representation.len;
    // Zero-length vectors need not expose a data pointer.
    if (n == 0) return &.{};
    if (!representation.altrep) return R.REAL(sexp)[0..n];

    // This asks ALTREP for an existing buffer without materializing it.
    const ptr = R.REAL_OR_NULL(sexp);
    if (ptr == null) return null;
    return ptr[0..n];
}

fn directIntSliceOrNull(sexp: SEXP, representation: VectorRepresentation) ?[]const i32 {
    const n = representation.len;
    if (n == 0) return &.{};
    if (!representation.altrep) return R.INTEGER(sexp)[0..n];

    const ptr = R.INTEGER_OR_NULL(sexp);
    if (ptr == null) return null;
    return ptr[0..n];
}

fn directLogicalSliceOrNull(sexp: SEXP, representation: VectorRepresentation) ?[]const i32 {
    const n = representation.len;
    if (n == 0) return &.{};
    if (!representation.altrep) return R.LOGICAL(sexp)[0..n];

    const ptr = R.LOGICAL_OR_NULL(sexp);
    if (ptr == null) return null;
    return ptr[0..n];
}

fn directRawSliceOrNull(sexp: SEXP, representation: VectorRepresentation) ?[]const u8 {
    const n = representation.len;
    if (n == 0) return &.{};
    if (!representation.altrep) return R.RAW(sexp)[0..n];

    const ptr = R.RAW_OR_NULL(sexp);
    if (ptr == null) return null;
    return ptr[0..n];
}

fn directComplexSliceOrNull(sexp: SEXP, representation: VectorRepresentation) ?[]const Rcomplex {
    const n = representation.len;
    if (n == 0) return &.{};
    if (!representation.altrep) {
        const ptr = R.COMPLEX(sexp) orelse return null;
        const complex_ptr: [*]const Rcomplex = @ptrCast(@alignCast(ptr));
        return complex_ptr[0..n];
    }

    const ptr = R.COMPLEX_OR_NULL(sexp) orelse return null;
    const complex_ptr: [*]const Rcomplex = @ptrCast(@alignCast(ptr));
    return complex_ptr[0..n];
}

/// Borrowed data is not a GC root; owned data must use its recorded allocator.
pub fn SliceView(comptime T: type) type {
    return union(enum) {
        borrowed: []const T,
        owned: struct {
            data: []const T,
            allocator: std.mem.Allocator,
        },

        pub fn constSlice(self: @This()) []const T {
            return switch (self) {
                .borrowed => |s| s,
                .owned => |s| s.data,
            };
        }

        pub fn deinit(self: *@This()) void {
            switch (self.*) {
                .owned => |s| s.allocator.free(s.data),
                .borrowed => {},
            }
            self.* = undefined;
        }
    };
}

pub const RawSliceView = SliceView(u8);

pub fn toRealSliceView(allocator: std.mem.Allocator, sexp: SEXP) !SliceView(f64) {
    try expectType(sexp, R.REALSXP, error.ExpectedReal);
    const representation = try vectorRepresentation(sexp);
    if (directRealSliceOrNull(sexp, representation)) |data| return .{ .borrowed = data };
    return .{ .owned = .{ .data = try toRealSliceWithRepresentation(allocator, sexp, representation), .allocator = allocator } };
}

pub fn toIntSliceView(allocator: std.mem.Allocator, sexp: SEXP) !SliceView(i32) {
    try expectType(sexp, R.INTSXP, error.ExpectedInteger);
    const representation = try vectorRepresentation(sexp);
    if (directIntSliceOrNull(sexp, representation)) |data| return .{ .borrowed = data };
    return .{ .owned = .{ .data = try toIntSliceWithRepresentation(allocator, sexp, representation), .allocator = allocator } };
}

pub fn toRealSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]f64 {
    try expectType(sexp, R.REALSXP, error.ExpectedReal);
    return toRealSliceWithRepresentation(allocator, sexp, try vectorRepresentation(sexp));
}

fn toRealSliceWithRepresentation(allocator: std.mem.Allocator, sexp: SEXP, representation: VectorRepresentation) ![]f64 {
    const n = representation.len;
    const result = try allocator.alloc(f64, n);
    errdefer allocator.free(result);
    if (directRealSliceOrNull(sexp, representation)) |data| {
        @memcpy(result, data);
    } else if (representation.altrep) {
        _ = cleanup.pushFrameInline(AllocSliceCleanup, AllocSliceCleanup.init(f64, allocator, result, @returnAddress()), AllocSliceCleanup.fire);
        defer cleanup.popFrame();
        var offset: R.R_xlen_t = 0;
        const ncast = @as(R.R_xlen_t, @intCast(n));
        while (offset < ncast) {
            const got = R.REAL_GET_REGION(sexp, offset, ncast - offset, result.ptr + @as(usize, @intCast(offset)));
            if (got == 0) return error.AltRepRegionRead;
            offset += got;
        }
    } else {
        unreachable;
    }
    return result;
}

pub fn fromRealSlice(slice: []const f64) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    var vec = protect.scoped(R.Rf_allocVector(R.REALSXP, len));
    defer vec.deinit();
    @memcpy(R.REAL(vec.get())[0..slice.len], slice);
    return vec.get();
}

pub fn toIntSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]i32 {
    try expectType(sexp, R.INTSXP, error.ExpectedInteger);
    return toIntSliceWithRepresentation(allocator, sexp, try vectorRepresentation(sexp));
}

fn toIntSliceWithRepresentation(allocator: std.mem.Allocator, sexp: SEXP, representation: VectorRepresentation) ![]i32 {
    const n = representation.len;
    const result = try allocator.alloc(i32, n);
    errdefer allocator.free(result);
    if (directIntSliceOrNull(sexp, representation)) |data| {
        @memcpy(result, data);
    } else if (representation.altrep) {
        _ = cleanup.pushFrameInline(AllocSliceCleanup, AllocSliceCleanup.init(i32, allocator, result, @returnAddress()), AllocSliceCleanup.fire);
        defer cleanup.popFrame();
        var offset: R.R_xlen_t = 0;
        const ncast = @as(R.R_xlen_t, @intCast(n));
        while (offset < ncast) {
            const got = R.INTEGER_GET_REGION(sexp, offset, ncast - offset, result.ptr + @as(usize, @intCast(offset)));
            if (got == 0) return error.AltRepRegionRead;
            offset += got;
        }
    } else {
        unreachable;
    }
    return result;
}

pub fn fromIntSlice(slice: []const i32) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    var vec = protect.scoped(R.Rf_allocVector(R.INTSXP, len));
    defer vec.deinit();
    @memcpy(R.INTEGER(vec.get())[0..slice.len], slice);
    return vec.get();
}

/// The headers borrow R-owned bytes and must stay inside the source R call.
pub fn toStringSlice(allocator: std.mem.Allocator, sexp: SEXP) ![][]const u8 {
    try expectType(sexp, R.STRSXP, error.ExpectedString);
    const n = try tryXlength(sexp);
    const result = try allocator.alloc([]const u8, n);
    _ = cleanup.pushFrameInline(AllocSliceCleanup, AllocSliceCleanup.init([]const u8, allocator, result, @returnAddress()), AllocSliceCleanup.fire);
    defer cleanup.popFrame();
    for (0..n) |i| {
        const elt = R.STRING_ELT(sexp, @intCast(i));
        result[i] = if (elt == R.R_NaString) "" else sexp_mod.charsxpBytes(elt);
    }
    return result;
}

pub fn toStringSliceNullable(allocator: std.mem.Allocator, sexp: SEXP) ![]?[]const u8 {
    try expectType(sexp, R.STRSXP, error.ExpectedString);
    const n = try tryXlength(sexp);
    const result = try allocator.alloc(?[]const u8, n);
    _ = cleanup.pushFrameInline(AllocSliceCleanup, AllocSliceCleanup.init(?[]const u8, allocator, result, @returnAddress()), AllocSliceCleanup.fire);
    defer cleanup.popFrame();
    for (0..n) |i| {
        const elt = R.STRING_ELT(sexp, @intCast(i));
        result[i] = if (elt == R.R_NaString) null else sexp_mod.charsxpBytes(elt);
    }
    return result;
}

pub const StringView = struct {
    charsxp: SEXP,
    bytes: []const u8,
    len: usize,
    is_na: bool,
    encoding_mark: R.cetype_t,
};

fn makeStringView(elt: SEXP) StringView {
    const is_na = elt == R.R_NaString;
    const bytes = if (is_na) "" else sexp_mod.charsxpBytes(elt);
    return .{
        .charsxp = elt,
        .bytes = bytes,
        .len = bytes.len,
        .is_na = is_na,
        .encoding_mark = if (is_na) @as(R.cetype_t, @intCast(R.CE_NATIVE)) else R.Rf_getCharCE(elt),
    };
}

/// Translation storage remains R-owned for the current R call.
pub const StringSliceView = struct {
    sexp: SEXP,
    len: usize,

    pub fn at(self: StringSliceView, index: usize) StringView {
        if (index >= self.len) @panic("StringSliceView.at index out of bounds");
        if (R.ALTREP(self.sexp) == 0) {
            return makeStringView(sexp_mod.fastVectorElt(self.sexp, index));
        }
        const elt = R.STRING_ELT(self.sexp, @intCast(index));
        return makeStringView(elt);
    }

    pub const Iterator = struct {
        view: StringSliceView,
        index: usize = 0,

        pub fn next(self: *Iterator) ?StringView {
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

pub const CachedStringSliceView = struct {
    items: []const StringView,
    len: usize,
    allocator: std.mem.Allocator,

    pub fn at(self: CachedStringSliceView, index: usize) StringView {
        if (index >= self.len) @panic("CachedStringSliceView.at index out of bounds");
        return self.items[index];
    }

    pub const Iterator = struct {
        view: CachedStringSliceView,
        index: usize = 0,

        pub fn next(self: *Iterator) ?StringView {
            if (self.index >= self.view.len) return null;
            const value = self.view.at(self.index);
            self.index += 1;
            return value;
        }
    };

    pub fn iterator(self: CachedStringSliceView) Iterator {
        return .{ .view = self };
    }

    pub fn deinit(self: *CachedStringSliceView) void {
        self.allocator.free(self.items);
        self.* = undefined;
    }
};

pub fn toStringSliceView(sexp: SEXP) !StringSliceView {
    try expectType(sexp, R.STRSXP, error.ExpectedString);
    return .{
        .sexp = sexp,
        .len = try tryXlength(sexp),
    };
}

pub fn toCachedStringSliceView(allocator: std.mem.Allocator, sexp: SEXP) !CachedStringSliceView {
    try expectType(sexp, R.STRSXP, error.ExpectedString);
    const n = try tryXlength(sexp);
    const items = try allocator.alloc(StringView, n);
    _ = cleanup.pushFrameInline(AllocSliceCleanup, AllocSliceCleanup.init(StringView, allocator, items, @returnAddress()), AllocSliceCleanup.fire);
    defer cleanup.popFrame();
    for (0..n) |i| {
        items[i] = makeStringView(R.STRING_ELT(sexp, @intCast(i)));
    }
    return .{
        .items = items,
        .len = n,
        .allocator = allocator,
    };
}

/// R character creation can longjmp, so direct callers need an unwind boundary.
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

pub fn toLogicalSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]i32 {
    try expectType(sexp, R.LGLSXP, error.ExpectedLogical);
    return toLogicalSliceWithRepresentation(allocator, sexp, try vectorRepresentation(sexp));
}

fn toLogicalSliceWithRepresentation(allocator: std.mem.Allocator, sexp: SEXP, representation: VectorRepresentation) ![]i32 {
    const n = representation.len;
    const result = try allocator.alloc(i32, n);
    errdefer allocator.free(result);
    if (directLogicalSliceOrNull(sexp, representation)) |data| {
        @memcpy(result, data);
    } else if (representation.altrep) {
        _ = cleanup.pushFrameInline(AllocSliceCleanup, AllocSliceCleanup.init(i32, allocator, result, @returnAddress()), AllocSliceCleanup.fire);
        defer cleanup.popFrame();
        var offset: R.R_xlen_t = 0;
        const ncast = @as(R.R_xlen_t, @intCast(n));
        while (offset < ncast) {
            const got = R.LOGICAL_GET_REGION(sexp, offset, ncast - offset, result.ptr + @as(usize, @intCast(offset)));
            if (got == 0) return error.AltRepRegionRead;
            offset += got;
        }
    } else {
        unreachable;
    }
    return result;
}

pub fn toLogicalSliceView(allocator: std.mem.Allocator, sexp: SEXP) !SliceView(i32) {
    try expectType(sexp, R.LGLSXP, error.ExpectedLogical);
    const representation = try vectorRepresentation(sexp);
    if (directLogicalSliceOrNull(sexp, representation)) |data| return .{ .borrowed = data };
    return .{ .owned = .{ .data = try toLogicalSliceWithRepresentation(allocator, sexp, representation), .allocator = allocator } };
}

pub fn fromLogicalSlice(slice: []const i32) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    var vec = protect.scoped(R.Rf_allocVector(R.LGLSXP, len));
    defer vec.deinit();
    @memcpy(R.LOGICAL(vec.get())[0..slice.len], slice);
    return vec.get();
}

pub fn toListSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]SEXP {
    try expectType(sexp, R.VECSXP, error.ExpectedList);
    const n = try tryXlength(sexp);
    const result = try allocator.alloc(SEXP, n);
    for (0..n) |i| result[i] = R.VECTOR_ELT(sexp, @intCast(i));
    return result;
}

pub fn fromListSlice(slice: []const SEXP) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    var vec = protect.scoped(R.Rf_allocVector(R.VECSXP, len));
    defer vec.deinit();
    for (0..@as(usize, @intCast(len))) |i| {
        R.SET_VECTOR_ELT(vec.get(), @intCast(i), slice[i]);
    }
    return vec.get();
}

pub fn toRawSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]const u8 {
    try expectType(sexp, R.RAWSXP, error.ExpectedRaw);
    return toRawSliceWithRepresentation(allocator, sexp, try vectorRepresentation(sexp));
}

fn toRawSliceWithRepresentation(allocator: std.mem.Allocator, sexp: SEXP, representation: VectorRepresentation) ![]const u8 {
    const n = representation.len;
    const result = try allocator.alloc(u8, n);
    errdefer allocator.free(result);
    if (directRawSliceOrNull(sexp, representation)) |data| {
        @memcpy(result, data);
    } else if (representation.altrep) {
        _ = cleanup.pushFrameInline(AllocSliceCleanup, AllocSliceCleanup.init(u8, allocator, result, @returnAddress()), AllocSliceCleanup.fire);
        defer cleanup.popFrame();
        var offset: R.R_xlen_t = 0;
        const ncast = @as(R.R_xlen_t, @intCast(n));
        while (offset < ncast) {
            const got = R.RAW_GET_REGION(sexp, offset, ncast - offset, result.ptr + @as(usize, @intCast(offset)));
            if (got == 0) return error.AltRepRegionRead;
            offset += got;
        }
    } else {
        unreachable;
    }
    return result;
}

pub fn toRawSliceView(allocator: std.mem.Allocator, sexp: SEXP) !RawSliceView {
    try expectType(sexp, R.RAWSXP, error.ExpectedRaw);
    const representation = try vectorRepresentation(sexp);
    if (directRawSliceOrNull(sexp, representation)) |data| return .{ .borrowed = data };
    return .{ .owned = .{ .data = try toRawSliceWithRepresentation(allocator, sexp, representation), .allocator = allocator } };
}

pub fn fromRawSlice(slice: []const u8) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    var vec = protect.scoped(R.Rf_allocVector(R.RAWSXP, len));
    defer vec.deinit();
    @memcpy(R.RAW(vec.get())[0..slice.len], slice);
    return vec.get();
}

pub fn toComplexSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]const Rcomplex {
    try expectType(sexp, R.CPLXSXP, error.ExpectedComplex);
    return toComplexSliceWithRepresentation(allocator, sexp, try vectorRepresentation(sexp));
}

fn toComplexSliceWithRepresentation(allocator: std.mem.Allocator, sexp: SEXP, representation: VectorRepresentation) ![]Rcomplex {
    const n = representation.len;
    const result = try allocator.alloc(Rcomplex, n);
    errdefer allocator.free(result);
    if (directComplexSliceOrNull(sexp, representation)) |data| {
        @memcpy(result, data);
    } else if (representation.altrep) {
        _ = cleanup.pushFrameInline(AllocSliceCleanup, AllocSliceCleanup.init(Rcomplex, allocator, result, @returnAddress()), AllocSliceCleanup.fire);
        defer cleanup.popFrame();
        var offset: R.R_xlen_t = 0;
        const ncast = @as(R.R_xlen_t, @intCast(n));
        while (offset < ncast) {
            const got = R.COMPLEX_GET_REGION(sexp, offset, ncast - offset, @ptrCast(result.ptr + @as(usize, @intCast(offset))));
            if (got == 0) return error.AltRepRegionRead;
            offset += got;
        }
    } else {
        return error.AltRepRegionRead;
    }
    return result;
}

pub fn toComplexSliceView(allocator: std.mem.Allocator, sexp: SEXP) !SliceView(Rcomplex) {
    try expectType(sexp, R.CPLXSXP, error.ExpectedComplex);
    const representation = try vectorRepresentation(sexp);
    if (directComplexSliceOrNull(sexp, representation)) |data| return .{ .borrowed = data };
    return .{ .owned = .{ .data = try toComplexSliceWithRepresentation(allocator, sexp, representation), .allocator = allocator } };
}

pub fn fromComplexSlice(slice: []const Rcomplex) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    var vec = protect.scoped(R.Rf_allocVector(R.CPLXSXP, len));
    defer vec.deinit();
    const dst: [*]Rcomplex = @ptrCast(@alignCast(R.COMPLEX(vec.get()) orelse @panic("COMPLEX returned null on freshly allocated CPLXSXP")));
    @memcpy(dst[0..slice.len], slice);
    return vec.get();
}

fn zigToSexp(value: anytype, comptime T: type, arena: std.mem.Allocator) SEXP {
    if (comptime T == SEXP) return value;
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
    if (comptime T == []const bool) {
        var result = protect.scoped(R.Rf_allocVector(R.LGLSXP, @intCast(value.len)));
        defer result.deinit();
        for (value, 0..) |b, i| R.LOGICAL(result.get())[i] = if (b) 1 else 0;
        return result.get();
    }
    if (comptime T == []const Rcomplex) return fromComplexSlice(value);
    if (comptime @typeInfo(T) == .@"struct") {
        return fixedSchemaToSexp(value, T, arena);
    }
    @compileError("unsupported type in struct conversion: " ++ @typeName(T));
}

fn sexpToZig(comptime T: type, sexp: SEXP, arena: std.mem.Allocator) !T {
    if (comptime T == SEXP) return sexp;
    if (comptime T == ?f64) return try toOptionalRealScalar(sexp);
    if (comptime T == ?i32) return try toOptionalIntScalar(sexp);
    if (comptime T == ?bool) return try toOptionalBoolScalar(sexp);
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
    if (comptime T == []const bool) {
        const int_slice = try toLogicalSlice(arena, sexp);
        const result = try arena.alloc(bool, int_slice.len);
        for (int_slice, 0..) |v, i| result[i] = v == 1;
        return result;
    }
    if (comptime T == []const u8) return try toRawSlice(arena, sexp);
    if (comptime T == []const Rcomplex) return try toComplexSlice(arena, sexp);
    if (comptime @typeInfo(T) == .@"struct") {
        return try fixedSchemaFromSexp(T, sexp, arena);
    }
    @compileError("unsupported type in struct conversion: " ++ @typeName(T));
}

fn fixedSchemaToSexp(st: anytype, comptime T: type, arena: std.mem.Allocator) SEXP {
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

fn fixedSchemaFromSexp(comptime T: type, sexp: SEXP, arena: std.mem.Allocator) !T {
    const fields = @typeInfo(T).@"struct".fields;
    try expectType(sexp, R.VECSXP, error.ExpectedSchema);
    if (R.XLENGTH(sexp) != @as(R.R_xlen_t, @intCast(fields.len))) return error.SchemaLength;
    if (R.R_getAttribCount(sexp) != 1 or !R.R_hasAttrib(sexp, R.R_NamesSymbol)) return error.SchemaAttributes;

    const names = R.Rf_getAttrib(sexp, R.R_NamesSymbol);
    if (sexp_mod.typeTag(names) != @as(u5, @intCast(R.STRSXP)) or R.XLENGTH(names) != R.XLENGTH(sexp)) {
        return error.SchemaNames;
    }
    if (R.R_getAttribCount(names) != 0) return error.SchemaAttributes;

    var result: T = undefined;

    inline for (fields, 0..) |field, i| {
        const name = R.STRING_ELT(names, @intCast(i));
        if (name == R.R_NaString or !std.mem.eql(u8, sexp_mod.charsxpBytes(name), field.name)) return error.SchemaNames;
        @field(result, field.name) = try sexpToZig(field.type, R.VECTOR_ELT(sexp, @intCast(i)), arena);
    }

    return result;
}

const region_chunk_len = 4096;

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
    buf: [region_chunk_len]f64 = undefined,

    fn init(sexp: SEXP) RealChunkIter {
        const representation = vectorRepresentation(sexp) catch |err| signalError(err);
        return .{
            .sexp = sexp,
            .n = representation.len,
            .direct = directRealSliceOrNull(sexp, representation),
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
    buf: [region_chunk_len]i32 = undefined,

    fn init(sexp: SEXP) IntChunkIter {
        const representation = vectorRepresentation(sexp) catch |err| signalError(err);
        return .{
            .sexp = sexp,
            .n = representation.len,
            .direct = directIntSliceOrNull(sexp, representation),
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

const LogicalChunkIter = struct {
    sexp: SEXP,
    n: usize,
    offset: usize = 0,
    direct: ?[]const i32,
    buf: [region_chunk_len]i32 = undefined,

    fn init(sexp: SEXP) LogicalChunkIter {
        const representation = vectorRepresentation(sexp) catch |err| signalError(err);
        return .{
            .sexp = sexp,
            .n = representation.len,
            .direct = directLogicalSliceOrNull(sexp, representation),
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

pub fn sum(sexp: SEXP) f64 {
    expectType(sexp, R.REALSXP, error.ExpectedReal) catch |err| signalError(err);
    const n = xlength(sexp);
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

pub fn sumInt(sexp: SEXP) i64 {
    expectType(sexp, R.INTSXP, error.ExpectedInteger) catch |err| signalError(err);

    const n = xlength(sexp);
    if (n == 0) return 0;

    const lanes = simd.i32_lanes;
    var total: i64 = 0;
    var iter = IntChunkIter.init(sexp);
    while (iter.next()) |chunk| {
        var i: usize = 0;
        if (chunk.data.len >= lanes) {
            var vec_total: @Vector(lanes, i64) = @splat(0);
            const end = chunk.data.len - (chunk.data.len % lanes);
            while (i < end) : (i += lanes) {
                const v: @Vector(lanes, i32) = chunk.data[i..][0..lanes].*;
                vec_total += @as(@Vector(lanes, i64), @intCast(v));
            }
            total += @reduce(.Add, vec_total);
        }
        while (i < chunk.data.len) : (i += 1) total += chunk.data[i];
    }

    return total;
}

pub fn countTrue(sexp: SEXP) i64 {
    expectType(sexp, R.LGLSXP, error.ExpectedLogical) catch |err| signalError(err);

    const n = xlength(sexp);
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
    var initialized = false;
    var best = empty_value;

    while (iter.next()) |chunk| {
        for (chunk.data) |value| {
            if (value == R.R_NaInt) continue;
            if (!initialized or (if (find_min) value < best else value > best)) {
                best = value;
                initialized = true;
            }
        }
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

pub fn minInt(sexp: SEXP) i32 {
    expectType(sexp, R.INTSXP, error.ExpectedInteger) catch |err| signalError(err);

    var iter = IntChunkIter.init(sexp);
    return minmaxIntChunks(true, &iter, std.math.maxInt(i32));
}

pub fn maxInt(sexp: SEXP) i32 {
    expectType(sexp, R.INTSXP, error.ExpectedInteger) catch |err| signalError(err);

    var iter = IntChunkIter.init(sexp);
    return minmaxIntChunks(false, &iter, std.math.minInt(i32));
}

pub fn argminInt(sexp: SEXP) i64 {
    expectType(sexp, R.INTSXP, error.ExpectedInteger) catch |err| signalError(err);

    var iter = IntChunkIter.init(sexp);
    return argminmaxIntChunks(true, &iter);
}

pub fn argmaxInt(sexp: SEXP) i64 {
    expectType(sexp, R.INTSXP, error.ExpectedInteger) catch |err| signalError(err);

    var iter = IntChunkIter.init(sexp);
    return argminmaxIntChunks(false, &iter);
}

pub fn minLogical(sexp: SEXP) i32 {
    expectType(sexp, R.LGLSXP, error.ExpectedLogical) catch |err| signalError(err);

    var iter = LogicalChunkIter.init(sexp);
    return minmaxIntChunks(true, &iter, std.math.maxInt(i32));
}

pub fn maxLogical(sexp: SEXP) i32 {
    expectType(sexp, R.LGLSXP, error.ExpectedLogical) catch |err| signalError(err);

    var iter = LogicalChunkIter.init(sexp);
    return minmaxIntChunks(false, &iter, std.math.minInt(i32));
}

pub fn argminLogical(sexp: SEXP) i64 {
    expectType(sexp, R.LGLSXP, error.ExpectedLogical) catch |err| signalError(err);

    var iter = LogicalChunkIter.init(sexp);
    return argminmaxIntChunks(true, &iter);
}

pub fn argmaxLogical(sexp: SEXP) i64 {
    expectType(sexp, R.LGLSXP, error.ExpectedLogical) catch |err| signalError(err);

    var iter = LogicalChunkIter.init(sexp);
    return argminmaxIntChunks(false, &iter);
}

pub fn mean(sexp: SEXP) f64 {
    return sum(sexp) / @as(f64, @floatFromInt(R.XLENGTH(sexp)));
}

pub fn norm2(sexp: SEXP) f64 {
    expectType(sexp, R.REALSXP, error.ExpectedReal) catch |err| signalError(err);
    const n = xlength(sexp);
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

fn chunkHasNA(data: []const f64) bool {
    const lanes = simd.f64_lanes;
    const na_bits: @Vector(lanes, u64) = @splat(@bitCast(R.R_NaReal));
    var i: usize = 0;
    while (i + lanes <= data.len) : (i += lanes) {
        const bits: @Vector(lanes, u64) = @bitCast(data[i..][0..lanes].*);
        if (@reduce(.Or, bits == na_bits)) return true;
    }
    while (i < data.len) : (i += 1) {
        if (R.ISNA(data[i]) != 0) return true;
    }
    return false;
}

pub fn min(sexp: SEXP) f64 {
    expectType(sexp, R.REALSXP, error.ExpectedReal) catch |err| signalError(err);
    const n = xlength(sexp);
    if (n == 0) return std.math.inf(f64);

    const lanes = simd.f64_lanes;
    const inf_vec: @Vector(lanes, f64) = @splat(std.math.inf(f64));
    var value = std.math.inf(f64);
    var iter = RealChunkIter.init(sexp);
    while (iter.next()) |chunk| {
        var i: usize = 0;
        if (chunk.data.len >= lanes) {
            const has_na = chunkHasNA(chunk.data);
            var vec: @Vector(lanes, f64) = inf_vec;
            const end = chunk.data.len - (chunk.data.len % lanes);
            if (has_na) {
                const na_bits: @Vector(lanes, u64) = @splat(@bitCast(R.R_NaReal));
                while (i < end) : (i += lanes) {
                    const vals = chunk.data[i..][0..lanes].*;
                    const not_na = @as(@Vector(lanes, u64), @bitCast(vals)) != na_bits;
                    const clean = @select(f64, not_na, vals, inf_vec);
                    vec = @select(f64, clean < vec, clean, vec);
                }
            } else {
                while (i < end) : (i += lanes) {
                    const vals = chunk.data[i..][0..lanes].*;
                    vec = @select(f64, vals < vec, vals, vec);
                }
            }
            const vec_min = @reduce(.Min, vec);
            if (vec_min < value) value = vec_min;
        }
        while (i < chunk.data.len) : (i += 1) {
            if (R.ISNA(chunk.data[i]) != 0) continue;
            if (chunk.data[i] < value) value = chunk.data[i];
        }
    }

    return value;
}

pub fn max(sexp: SEXP) f64 {
    expectType(sexp, R.REALSXP, error.ExpectedReal) catch |err| signalError(err);
    const n = xlength(sexp);
    if (n == 0) return -std.math.inf(f64);

    const lanes = simd.f64_lanes;
    const neg_inf_vec: @Vector(lanes, f64) = @splat(-std.math.inf(f64));
    var value = -std.math.inf(f64);
    var iter = RealChunkIter.init(sexp);
    while (iter.next()) |chunk| {
        var i: usize = 0;
        if (chunk.data.len >= lanes) {
            const has_na = chunkHasNA(chunk.data);
            var vec: @Vector(lanes, f64) = neg_inf_vec;
            const end = chunk.data.len - (chunk.data.len % lanes);
            if (has_na) {
                const na_bits: @Vector(lanes, u64) = @splat(@bitCast(R.R_NaReal));
                while (i < end) : (i += lanes) {
                    const vals = chunk.data[i..][0..lanes].*;
                    const not_na = @as(@Vector(lanes, u64), @bitCast(vals)) != na_bits;
                    const clean = @select(f64, not_na, vals, neg_inf_vec);
                    vec = @select(f64, clean > vec, clean, vec);
                }
            } else {
                while (i < end) : (i += lanes) {
                    const vals = chunk.data[i..][0..lanes].*;
                    vec = @select(f64, vals > vec, vals, vec);
                }
            }
            const vec_max = @reduce(.Max, vec);
            if (vec_max > value) value = vec_max;
        }
        while (i < chunk.data.len) : (i += 1) {
            if (R.ISNA(chunk.data[i]) != 0) continue;
            if (chunk.data[i] > value) value = chunk.data[i];
        }
    }

    return value;
}

fn argminmax(comptime find_min: bool, sexp: SEXP) i64 {
    expectType(sexp, R.REALSXP, error.ExpectedReal) catch |err| signalError(err);
    const n = xlength(sexp);
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

pub fn argmin(sexp: SEXP) i64 {
    return argminmax(true, sexp);
}

pub fn argmax(sexp: SEXP) i64 {
    return argminmax(false, sexp);
}

pub fn sum_narm(sexp: SEXP) f64 {
    expectType(sexp, R.REALSXP, error.ExpectedReal) catch |err| signalError(err);
    const n = xlength(sexp);
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

pub fn mean_narm(sexp: SEXP) f64 {
    expectType(sexp, R.REALSXP, error.ExpectedReal) catch |err| signalError(err);
    const n = xlength(sexp);
    if (n == 0) return R.R_NaReal;

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
            if (R.ISNA(chunk.data[i]) != 0) {
                @branchHint(.unlikely);
                continue;
            }
            total += chunk.data[i];
            count += 1;
        }
    }

    return if (count == 0) R.R_NaReal else total / @as(f64, @floatFromInt(count));
}

pub fn scaleAdd(sexp: SEXP, alpha: f64, beta: f64) f64 {
    expectType(sexp, R.REALSXP, error.ExpectedReal) catch |err| signalError(err);
    const n = xlength(sexp);
    if (n == 0) return 0.0;

    const lanes = simd.f64_lanes;
    const valpha: @Vector(lanes, f64) = @splat(alpha);
    const vbeta: @Vector(lanes, f64) = @splat(beta);
    var total: f64 = 0.0;

    var iter = RealChunkIter.init(sexp);
    while (iter.next()) |chunk| {
        var i: usize = 0;
        if (chunk.data.len >= lanes) {
            var vtotal: @Vector(lanes, f64) = @splat(0.0);
            const end = chunk.data.len - (chunk.data.len % lanes);
            while (i < end) : (i += lanes) {
                const v: @Vector(lanes, f64) = chunk.data[i..][0..lanes].*;
                vtotal += v * valpha + vbeta;
            }
            total += @reduce(.Add, vtotal);
        }
        while (i < chunk.data.len) : (i += 1) {
            total += chunk.data[i] * alpha + beta;
        }
    }
    return total;
}

pub fn pminAlloc(a: SEXP, b: SEXP, arena: std.mem.Allocator) SEXP {
    var da_view = toRealSliceView(arena, a) catch |err| signalError(err);
    defer da_view.deinit();
    var db_view = toRealSliceView(arena, b) catch |err| signalError(err);
    defer db_view.deinit();
    const da = da_view.constSlice();
    const db = db_view.constSlice();
    const n = if (da.len == 0 or db.len == 0) @as(usize, 0) else @max(da.len, db.len);

    var result = protect.scoped(R.Rf_allocVector(R.REALSXP, @intCast(n)));
    defer result.deinit();
    const rp = R.REAL(result.get());

    for (0..n) |i| {
        rp[i] = @min(da[i % da.len], db[i % db.len]);
    }

    return result.get();
}

pub fn pmin(a: SEXP, b: SEXP) SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    return pminAlloc(a, b, arena.allocator());
}

pub fn pmaxAlloc(a: SEXP, b: SEXP, arena: std.mem.Allocator) SEXP {
    var da_view = toRealSliceView(arena, a) catch |err| signalError(err);
    defer da_view.deinit();
    var db_view = toRealSliceView(arena, b) catch |err| signalError(err);
    defer db_view.deinit();
    const da = da_view.constSlice();
    const db = db_view.constSlice();
    const n = if (da.len == 0 or db.len == 0) @as(usize, 0) else @max(da.len, db.len);

    var result = protect.scoped(R.Rf_allocVector(R.REALSXP, @intCast(n)));
    defer result.deinit();
    const rp = R.REAL(result.get());

    for (0..n) |i| {
        rp[i] = @max(da[i % da.len], db[i % db.len]);
    }

    return result.get();
}

pub fn pmax(a: SEXP, b: SEXP) SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    return pmaxAlloc(a, b, arena.allocator());
}

pub fn cumsum(sexp: SEXP) SEXP {
    expectType(sexp, R.REALSXP, error.ExpectedReal) catch |err| signalError(err);
    const n = xlength(sexp);
    var result = protect.scoped(R.Rf_allocVector(R.REALSXP, @intCast(n)));
    defer result.deinit();
    const rp = R.REAL(result.get());

    var total: f64 = 0.0;
    var na_seen = false;
    var idx: usize = 0;
    var iter = RealChunkIter.init(sexp);
    while (iter.next()) |chunk| {
        for (chunk.data) |value| {
            if (na_seen or R.ISNAN(value)) {
                na_seen = true;
                rp[idx] = R.R_NaReal;
            } else {
                total += value;
                rp[idx] = total;
            }
            idx += 1;
        }
    }

    return result.get();
}

/// The result is unprotected so the caller controls protection across later R allocations.
pub fn asSEXPAlloc(st: anytype, arena: std.mem.Allocator) SEXP {
    return fixedSchemaToSexp(st, @TypeOf(st), arena);
}

pub fn asSEXP(st: anytype) SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    return asSEXPAlloc(st, arena.allocator());
}

/// Fixed positions avoid a runtime name map; slices and raw SEXP fields borrow the source.
pub fn tryFromSEXP(comptime T: type, sexp: SEXP, arena: std.mem.Allocator) ConvertError!T {
    return fixedSchemaFromSexp(T, sexp, arena);
}

/// R-facing adapters use this to keep Zig errors inside the native boundary.
pub fn fromSEXP(comptime T: type, sexp: SEXP, arena: std.mem.Allocator) T {
    return tryFromSEXP(T, sexp, arena) catch |err| signalError(err);
}

pub fn typeToSEXPTYPE(comptime T: type) R.SEXPTYPE {
    return switch (T) {
        f64 => R.REALSXP,
        i32 => R.INTSXP,
        bool => R.LGLSXP,
        u8 => R.RAWSXP,
        Rcomplex => R.CPLXSXP,
        else => @compileError("unsupported type: " ++ @typeName(T)),
    };
}

/// Some ALTREP values have no direct complex pointer.
pub fn dataPtr(comptime T: type, sexp: SEXP) ?[*]T {
    return switch (T) {
        f64 => @ptrCast(R.REAL(sexp)),
        i32 => @ptrCast(R.INTEGER(sexp)),
        u8 => @ptrCast(R.RAW(sexp)),
        Rcomplex => if (R.COMPLEX(sexp)) |ptr| @ptrCast(@alignCast(ptr)) else null,
        else => @compileError("unsupported type: " ++ @typeName(T)),
    };
}

test "Rcomplex is the expected layout" {
    try std.testing.expectEqual(@sizeOf(Rcomplex), @sizeOf(extern struct { r: f64, i: f64 }));
    try std.testing.expectEqual(@alignOf(Rcomplex), @alignOf(f64));
}

test "SliceView borrowed constSlice returns borrowed data" {
    const data: [3]f64 = .{ 1.0, 2.0, 3.0 };
    var view: SliceView(f64) = .{ .borrowed = &data };
    const slice = view.constSlice();
    try std.testing.expectEqual(slice.len, 3);
    try std.testing.expectEqual(slice[0], 1.0);
    try std.testing.expectEqual(slice[2], 3.0);
}

test "SliceView owned constSlice and deinit" {
    const allocator = std.testing.allocator;
    var data = try allocator.alloc(f64, 3);
    data[0] = 1.0;
    data[1] = 2.0;
    data[2] = 3.0;
    var view: SliceView(f64) = .{ .owned = .{ .data = data, .allocator = allocator } };
    defer view.deinit();
    const slice = view.constSlice();
    try std.testing.expectEqual(slice.len, 3);
    try std.testing.expectEqual(slice[0], 1.0);
}

test "SliceView deinit owned frees memory" {
    const allocator = std.testing.allocator;
    var data = try allocator.alloc(i32, 2);
    data[0] = 42;
    data[1] = 43;
    var view: SliceView(i32) = .{ .owned = .{ .data = data, .allocator = allocator } };
    view.deinit();
}

test "SliceView deinit borrowed is no-op" {
    const data: [2]i32 = .{ 1, 2 };
    var view: SliceView(i32) = .{ .borrowed = &data };
    view.deinit();
}

test "errorMessage covers all ConvertError variants" {
    try std.testing.expectEqualSlices(u8, errorMessage(error.ExpectedReal), "expected REALSXP");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ExpectedInteger), "expected INTSXP");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ExpectedLogical), "expected LGLSXP");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ExpectedString), "expected STRSXP");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ExpectedList), "expected VECSXP");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ExpectedSchema), "expected fixed-schema named VECSXP");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ExpectedRaw), "expected RAWSXP");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ExpectedComplex), "expected CPLXSXP");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ZeroLength), "expected non-empty vector");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ScalarLength), "scalar inputs must have length one");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ScalarNA), "scalar inputs must not be NA");
    try std.testing.expectEqualSlices(u8, errorMessage(error.AltRepRegionRead), "ALTREP region read failed");
    try std.testing.expectEqualSlices(u8, errorMessage(error.SchemaLength), "fixed schema field count does not match");
    try std.testing.expectEqualSlices(u8, errorMessage(error.SchemaNames), "fixed schema names do not match");
    try std.testing.expectEqualSlices(u8, errorMessage(error.SchemaAttributes), "fixed schema has unsupported attributes");
    try std.testing.expectEqualSlices(u8, errorMessage(error.OutOfMemory), "out of memory during SEXP conversion");
}

test "errorMessage handles unknown error via @errorName" {
    const msg = errorMessage(error.ExpectedReal);
    try std.testing.expect(msg.len > 0);
}

test "typeToSEXPTYPE returns correct R constants" {
    try std.testing.expectEqual(typeToSEXPTYPE(f64), R.REALSXP);
    try std.testing.expectEqual(typeToSEXPTYPE(i32), R.INTSXP);
    try std.testing.expectEqual(typeToSEXPTYPE(bool), R.LGLSXP);
    try std.testing.expectEqual(typeToSEXPTYPE(u8), R.RAWSXP);
    try std.testing.expectEqual(typeToSEXPTYPE(Rcomplex), R.CPLXSXP);
}

test "Rcomplex has the right fields" {
    const c = Rcomplex{ .r = 1.5, .i = -2.5 };
    try std.testing.expectEqual(c.r, 1.5);
    try std.testing.expectEqual(c.i, -2.5);
}

test "StringView type compiles" {
    const v = StringView{
        .charsxp = undefined,
        .bytes = "",
        .len = 0,
        .is_na = false,
        .encoding_mark = @as(R.cetype_t, @intCast(R.CE_NATIVE)),
    };
    try std.testing.expectEqual(@TypeOf(v.is_na), bool);
}

test "StringSliceView type compiles" {
    const v = StringSliceView{ .sexp = undefined, .len = 0 };
    _ = v;
}

test "CachedStringSliceView type compiles" {
    const items: []const StringView = &.{};
    const v = CachedStringSliceView{ .items = items, .len = 0, .allocator = std.testing.allocator };
    try std.testing.expectEqual(@TypeOf(v.len), usize);
}
