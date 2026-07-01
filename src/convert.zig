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
const xlength = @import("sexp.zig").xlength;
const tryXlength = @import("sexp.zig").tryXlength;
const sexp_mod = @import("sexp.zig");
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
    return_address: usize,

    fn init(comptime T: type, allocator: std.mem.Allocator, slice: []T, ra: usize) AllocSliceCleanup {
        return .{
            .allocator = allocator,
            .memory = std.mem.sliceAsBytes(slice),
            .alignment = .fromByteUnits(@alignOf(T)),
            .return_address = ra,
        };
    }

    /// Frees the buffer this guard owns. Used as the `fireFn` for
    /// `pushFrameInline`. The state itself lives in the cleanup frame's
    /// inline buffer (thread-local), so there is no `c_allocator.destroy`
    /// here. Compare to the previous heap-allocated pattern, which had
    /// a stack-resident pointer to a heap-allocated state.
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
    ExpectedNamedList,
    ExpectedRaw,
    ExpectedComplex,
    ZeroLength,
    ScalarNA,
    AltRepRegionRead,
    MissingField,
    OutOfMemory,
    NegativeLength,
    LengthOverflow,
};

fn expectType(sexp: SEXP, expected: c_uint, comptime err: ConvertError) ConvertError!void {
    if (@as(c_uint, sexp_mod.typeTag(sexp)) != expected) return err;
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
    if (ns == R.R_NilValue or sexp_mod.typeTag(ns) != 16) return error.ExpectedNamedList;
    if (R.XLENGTH(ns) != R.XLENGTH(sexp)) return error.ExpectedNamedList;
    for (0..xlength(ns)) |i| {
        if (R.STRING_ELT(ns, @intCast(i)) == R.R_NaString) return error.ExpectedNamedList;
    }
    return ns;
}

pub fn optionalInputIsNullish(comptime T: type, sexp: SEXP) bool {
    if (sexp == R.R_NilValue) return true;
    if (comptime T == f64) {
        return sexp_mod.typeTag(sexp) == 14 and R.XLENGTH(sexp) > 0 and R.ISNA(R.REAL(sexp)[0]) != 0;
    }
    if (comptime T == i32) {
        return sexp_mod.typeTag(sexp) == 13 and R.XLENGTH(sexp) > 0 and R.INTEGER(sexp)[0] == R.R_NaInt;
    }
    if (comptime T == bool) {
        return sexp_mod.typeTag(sexp) == 10 and R.XLENGTH(sexp) > 0 and R.LOGICAL(sexp)[0] == R.R_NaInt;
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
    if (data1 == R.R_NilValue or sexp_mod.typeTag(data1) != 22) return null;
    if (R.R_ExternalPtrTag(data1) != R.Rf_install(zigr_altreal_slice_tag_name)) return null;

    const addr = R.R_ExternalPtrAddr(data1) orelse return null;
    const wrap: *const ZigrAltRealSliceWrap = @ptrCast(@alignCast(addr));
    return wrap.ptr[0..wrap.len];
}

fn zigrAltIntegerSliceOrNull(sexp: SEXP) ?[]const i32 {
    if (R.ALTREP(sexp) == 0) return null;

    const data1 = R.R_altrep_data1(sexp);
    if (data1 == R.R_NilValue or sexp_mod.typeTag(data1) != 22) return null;
    if (R.R_ExternalPtrTag(data1) != R.Rf_install(zigr_altinteger_slice_tag_name)) return null;

    const addr = R.R_ExternalPtrAddr(data1) orelse return null;
    const wrap: *const ZigrAltIntegerSliceWrap = @ptrCast(@alignCast(addr));
    return wrap.ptr[0..wrap.len];
}

fn zigrAltLogicalSliceOrNull(sexp: SEXP) ?[]const i32 {
    if (R.ALTREP(sexp) == 0) return null;

    const data1 = R.R_altrep_data1(sexp);
    if (data1 == R.R_NilValue or sexp_mod.typeTag(data1) != 22) return null;
    if (R.R_ExternalPtrTag(data1) != R.Rf_install(zigr_altlogical_slice_tag_name)) return null;

    const addr = R.R_ExternalPtrAddr(data1) orelse return null;
    const wrap: *const ZigrAltLogicalSliceWrap = @ptrCast(@alignCast(addr));
    return wrap.ptr[0..wrap.len];
}

fn directRealSliceOrNull(sexp: SEXP) ?[]const f64 {
    const n = xlength(sexp);
    if (R.ALTREP(sexp) == 0) return R.REAL(sexp)[0..n];

    if (zigrAltRealSliceOrNull(sexp)) |data| return data;

    const ptr = R.DATAPTR_OR_NULL(sexp) orelse return null;
    const real_ptr: [*]const f64 = @ptrCast(@alignCast(ptr));
    return real_ptr[0..n];
}

fn directIntSliceOrNull(sexp: SEXP) ?[]const i32 {
    const n = xlength(sexp);
    if (R.ALTREP(sexp) == 0) return R.INTEGER(sexp)[0..n];

    if (zigrAltIntegerSliceOrNull(sexp)) |data| return data;

    const ptr = R.DATAPTR_OR_NULL(sexp) orelse return null;
    const int_ptr: [*]const i32 = @ptrCast(@alignCast(ptr));
    return int_ptr[0..n];
}

fn directLogicalSliceOrNull(sexp: SEXP) ?[]const i32 {
    const n = xlength(sexp);
    if (R.ALTREP(sexp) == 0) return R.LOGICAL(sexp)[0..n];

    if (zigrAltLogicalSliceOrNull(sexp)) |data| return data;

    const ptr = R.DATAPTR_OR_NULL(sexp) orelse return null;
    const logical_ptr: [*]const i32 = @ptrCast(@alignCast(ptr));
    return logical_ptr[0..n];
}

fn directComplexSliceOrNull(sexp: SEXP) ?[]const Rcomplex {
    const n = xlength(sexp);
    if (R.ALTREP(sexp) == 0) {
        const ptr = R.COMPLEX(sexp) orelse return null;
        const complex_ptr: [*]const Rcomplex = @ptrCast(@alignCast(ptr));
        return complex_ptr[0..n];
    }

    const ptr = R.DATAPTR_OR_NULL(sexp) orelse return null;
    const complex_ptr: [*]const Rcomplex = @ptrCast(@alignCast(ptr));
    return complex_ptr[0..n];
}

/// Tagged union: borrowed (R-owned) or owned (arena copy). `.constSlice()` returns `[]const T` in either case. `.deinit(allocator)` frees an owned copy, no-op for borrowed.
pub fn SliceView(comptime T: type) type {
    return union(enum) {
        borrowed: []const T,
        owned: []T,

        pub fn constSlice(self: @This()) []const T {
            return switch (self) {
                .borrowed => |s| s,
                .owned => |s| s,
            };
        }

        pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
            switch (self.*) {
                .owned => |s| allocator.free(s),
                .borrowed => {},
            }
            self.* = undefined;
        }
    };
}

/// Recommended boundary helper for REALSXP -> Zig (W4.1).
///
/// Returns a `SliceView(f64)` that is either borrowed from R memory (for
/// non-ALTREP SEXPs that have a direct data pointer) or owned by the
/// caller (for ALTREP SEXPs that need a one-time materialization). Use
/// `.constSlice()` to get a uniform `[]const f64` regardless of which
/// branch was taken, and `.deinit(allocator)` to free the owned copy.
///
/// Cost (x86_64-linux, 1M elements, see plan/PLAN.md W4.1 Findings):
///   - non-ALTREP REALSXP: ~600us median, same as toRealSlice (delegates)
///   - ALTREP-backed REALSXP: zero-copy borrowed path; the 600us copy
///     is avoided because the ALTREP is materialized in place by R's
///     own data pointer
///
/// When to use:
///   - The input may be ALTREP-backed (e.g. `seq_len`, `rep`, or any
///     `convert.altrep_create`-built vector). This is the win.
///   - The caller can tolerate a borrowed slice (no caller ownership of
///     the underlying bytes; the SEXP must outlive the slice).
///
/// When to prefer `toRealSlice` instead:
///   - The input is known to be non-ALTREP. The view is overhead.
///   - The caller needs a stable owned slice (e.g. to pass across an
///     arena boundary).
pub fn toRealSliceView(allocator: std.mem.Allocator, sexp: SEXP) !SliceView(f64) {
    try expectType(sexp, R.REALSXP, error.ExpectedReal);
    if (directRealSliceOrNull(sexp)) |data| return .{ .borrowed = data };
    return .{ .owned = try toRealSlice(allocator, sexp) };
}

/// Recommended boundary helper for INTSXP -> Zig (W4.1, parallels toRealSliceView).
///
/// Returns a `SliceView(i32)` that is borrowed from R memory (for non-ALTREP
/// INTSXP with a direct data pointer) or owned by the caller (for ALTREP).
/// Use `.constSlice()` for a uniform `[]const i32`, `.deinit(allocator)`
/// to free the owned copy.
///
/// Cost (x86_64-linux, 1M elements, see plan/PLAN.md W4.1 Findings):
///   - non-ALTREP INTSXP: ~275us median, same as toIntSlice (delegates)
///   - ALTREP-backed INTSXP: zero-copy borrowed path
///
/// See `toRealSliceView` for the full rationale. The same use/don't-use
/// rules apply, swapped for i32 and 4 bytes per element instead of 8.
pub fn toIntSliceView(allocator: std.mem.Allocator, sexp: SEXP) !SliceView(i32) {
    try expectType(sexp, R.INTSXP, error.ExpectedInteger);
    if (directIntSliceOrNull(sexp)) |data| return .{ .borrowed = data };
    return .{ .owned = try toIntSlice(allocator, sexp) };
}

/// Loops on REAL_GET_REGION for partial reads (ALTREP). @memcpy from REAL() for non-ALTREP (zero C FFI).
pub fn toRealSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]f64 {
    try expectType(sexp, R.REALSXP, error.ExpectedReal);
    const n = try tryXlength(sexp);
    const result = try allocator.alloc(f64, n);
    if (directRealSliceOrNull(sexp)) |data| {
        @memcpy(result, data);
    } else if (R.ALTREP(sexp) != 0) {
        cleanup.pushFrameInline(AllocSliceCleanup, AllocSliceCleanup.init(f64, allocator, result, @returnAddress()), AllocSliceCleanup.fire);
        defer cleanup.popFrame();
        var offset: R.R_xlen_t = 0;
        const ncast = @as(R.R_xlen_t, @intCast(n));
        while (offset < ncast) {
            const got = R.REAL_GET_REGION(sexp, offset, ncast - offset, result.ptr + @as(usize, @intCast(offset)));
            if (got == 0) return error.AltRepRegionRead;
            offset += got;
        }
    } else {
        @memcpy(result, R.REAL(sexp)[0..n]);
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

/// Loops on INTEGER_GET_REGION for partial reads (ALTREP). @memcpy from INTEGER() for non-ALTREP.
pub fn toIntSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]i32 {
    try expectType(sexp, R.INTSXP, error.ExpectedInteger);
    const n = try tryXlength(sexp);
    const result = try allocator.alloc(i32, n);
    if (directIntSliceOrNull(sexp)) |data| {
        @memcpy(result, data);
    } else if (R.ALTREP(sexp) != 0) {
        cleanup.pushFrameInline(AllocSliceCleanup, AllocSliceCleanup.init(i32, allocator, result, @returnAddress()), AllocSliceCleanup.fire);
        defer cleanup.popFrame();
        var offset: R.R_xlen_t = 0;
        const ncast = @as(R.R_xlen_t, @intCast(n));
        while (offset < ncast) {
            const got = R.INTEGER_GET_REGION(sexp, offset, ncast - offset, result.ptr + @as(usize, @intCast(offset)));
            if (got == 0) return error.AltRepRegionRead;
            offset += got;
        }
    } else {
        @memcpy(result, R.INTEGER(sexp)[0..n]);
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

/// Default string boundary helper (W4.2).
///
/// Allocates a `[][]const u8` of slice headers. Each slice points into
/// the CHARSXP bytes after a call to `Rf_translateCharUTF8` (Latin-1 and
/// native-encoded strings are normalized to UTF-8 on the way in).
/// NA strings map to empty slices; the NA/empty distinction is lost.
/// Use `toStringSliceNullable` to preserve it, or `toStringSliceView`
/// or `toCachedStringSliceView` if you need `is_na` on each element.
///
/// Cost (x86_64-linux, 1M STRSXP, see plan/PLAN.md W4.2 Findings):
///   - create + iterate: ~14ms median, 1 alloc/call, 16MB (1M slice
///     headers, 16 bytes each on 64-bit)
///   - iterate only (reuse the slice): ~0.6ms per pass, memory-bandwidth
///     bound
///
/// When to use:
///   - Single-pass or multi-pass over a STRSXP. Pays the per-element
///     R API cost upfront, then iterate is a plain Zig loop.
///   - Familiar Zig slice-of-slices API; you can pass `[]const u8` to
///     downstream code without per-element dereference.
///
/// When to prefer the view variants:
///   - Memory-constrained single-pass: `toStringSliceView`. No alloc,
///     but each pass costs ~15ms (R API call per access).
///   - NA preservation with multi-pass: `toCachedStringSliceView`.
///     40MB peak, 2ms per pass, but NA preserved as `StringView.is_na`.
pub fn toStringSlice(allocator: std.mem.Allocator, sexp: SEXP) ![][]const u8 {
    try expectType(sexp, R.STRSXP, error.ExpectedString);
    const n = try tryXlength(sexp);
    const result = try allocator.alloc([]const u8, n);
    for (0..n) |i| {
        const elt = R.STRING_ELT(sexp, @intCast(i));
        result[i] = if (elt == R.R_NaString) "" else sexp_mod.charsxpBytes(elt);
    }
    return result;
}

/// STRSXP: borrow CHARSXP data into a Zig slice of nullable strings.
/// NA_STRING elements become `null`, preserving the NA/empty distinction.
pub fn toStringSliceNullable(allocator: std.mem.Allocator, sexp: SEXP) ![]?[]const u8 {
    try expectType(sexp, R.STRSXP, error.ExpectedString);
    const n = try tryXlength(sexp);
    const result = try allocator.alloc(?[]const u8, n);
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
};

fn makeStringView(elt: SEXP) StringView {
    const is_na = elt == R.R_NaString;
    const bytes = if (is_na) "" else sexp_mod.charsxpBytes(elt);
    return .{
        .charsxp = elt,
        .bytes = bytes,
        .len = bytes.len,
        .is_na = is_na,
    };
}

pub const StringSliceView = struct {
    sexp: SEXP,
    len: usize,

    pub fn at(self: StringSliceView, index: usize) StringView {
        if (R.ALTREP(self.sexp) == 0) {
            const elt = sexp_mod.fastVectorElt(self.sexp, index);
            const is_na = elt == R.R_NaString;
            const bytes = if (is_na) "" else sexp_mod.charsxpBytes(elt);
            return .{ .charsxp = elt, .bytes = bytes, .len = bytes.len, .is_na = is_na };
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

    pub fn at(self: CachedStringSliceView, index: usize) StringView {
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
};

/// STRSXP: borrow CHARSXP data without allocating slice headers.
pub fn toStringSliceView(sexp: SEXP) !StringSliceView {
    try expectType(sexp, R.STRSXP, error.ExpectedString);
    return .{
        .sexp = sexp,
        .len = try tryXlength(sexp),
    };
}

/// STRSXP: cache per-element string metadata once for repeated multi-pass use.
pub fn toCachedStringSliceView(allocator: std.mem.Allocator, sexp: SEXP) !CachedStringSliceView {
    try expectType(sexp, R.STRSXP, error.ExpectedString);
    const n = try tryXlength(sexp);
    const items = try allocator.alloc(StringView, n);
    for (0..n) |i| {
        items[i] = makeStringView(R.STRING_ELT(sexp, @intCast(i)));
    }
    return .{
        .items = items,
        .len = n,
    };
}

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
    const n = try tryXlength(sexp);
    const result = try allocator.alloc(i32, n);
    if (directLogicalSliceOrNull(sexp)) |data| {
        @memcpy(result, data);
    } else if (R.ALTREP(sexp) != 0) {
        cleanup.pushFrameInline(AllocSliceCleanup, AllocSliceCleanup.init(i32, allocator, result, @returnAddress()), AllocSliceCleanup.fire);
        defer cleanup.popFrame();
        var offset: R.R_xlen_t = 0;
        const ncast = @as(R.R_xlen_t, @intCast(n));
        while (offset < ncast) {
            const got = R.LOGICAL_GET_REGION(sexp, offset, ncast - offset, result.ptr + @as(usize, @intCast(offset)));
            if (got == 0) return error.AltRepRegionRead;
            offset += got;
        }
    } else {
        @memcpy(result, R.LOGICAL(sexp)[0..n]);
    }
    return result;
}

pub fn toLogicalSliceView(allocator: std.mem.Allocator, sexp: SEXP) !SliceView(i32) {
    try expectType(sexp, R.LGLSXP, error.ExpectedLogical);
    if (directLogicalSliceOrNull(sexp)) |data| return .{ .borrowed = data };
    return .{ .owned = try toLogicalSlice(allocator, sexp) };
}

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

/// Loops on RAW_GET_REGION for partial reads (ALTREP). @memcpy from RAW() for non-ALTREP.
pub fn toRawSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]const u8 {
    try expectType(sexp, R.RAWSXP, error.ExpectedRaw);
    const n = try tryXlength(sexp);
    const result = try allocator.alloc(u8, n);
    if (R.ALTREP(sexp) != 0) {
        cleanup.pushFrameInline(AllocSliceCleanup, AllocSliceCleanup.init(u8, allocator, result, @returnAddress()), AllocSliceCleanup.fire);
        defer cleanup.popFrame();
        var offset: R.R_xlen_t = 0;
        const ncast = @as(R.R_xlen_t, @intCast(n));
        while (offset < ncast) {
            const got = R.RAW_GET_REGION(sexp, offset, ncast - offset, result.ptr + @as(usize, @intCast(offset)));
            if (got == 0) return error.AltRepRegionRead;
            offset += got;
        }
    } else {
        @memcpy(result, R.RAW(sexp)[0..n]);
    }
    return result;
}

pub fn fromRawSlice(slice: []const u8) SEXP {
    const len: R.R_xlen_t = @intCast(slice.len);
    var vec = protect.scoped(R.Rf_allocVector(R.RAWSXP, len));
    defer vec.deinit();
    @memcpy(R.RAW(vec.get())[0..slice.len], slice);
    return vec.get();
}

/// Loops on COMPLEX_GET_REGION for partial reads (ALTREP). @memcpy from COMPLEX() for non-ALTREP.
pub fn toComplexSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]Rcomplex {
    try expectType(sexp, R.CPLXSXP, error.ExpectedComplex);
    const n = try tryXlength(sexp);
    const result = try allocator.alloc(Rcomplex, n);
    if (R.ALTREP(sexp) != 0) {
        cleanup.pushFrameInline(AllocSliceCleanup, AllocSliceCleanup.init(Rcomplex, allocator, result, @returnAddress()), AllocSliceCleanup.fire);
        defer cleanup.popFrame();
        var offset: R.R_xlen_t = 0;
        const ncast = @as(R.R_xlen_t, @intCast(n));
        while (offset < ncast) {
            const got = R.COMPLEX_GET_REGION(sexp, offset, ncast - offset, @ptrCast(result.ptr + @as(usize, @intCast(offset))));
            if (got == 0) return error.AltRepRegionRead;
            offset += got;
        }
    } else {
        const src = R.COMPLEX(sexp) orelse return error.AltRepRegionRead;
        const typed: [*]const Rcomplex = @ptrCast(@alignCast(src));
        @memcpy(result, typed[0..n]);
    }
    return result;
}

pub fn toComplexSliceView(allocator: std.mem.Allocator, sexp: SEXP) !SliceView(Rcomplex) {
    try expectType(sexp, R.CPLXSXP, error.ExpectedComplex);
    if (directComplexSliceOrNull(sexp)) |data| return .{ .borrowed = data };
    return .{ .owned = try toComplexSlice(allocator, sexp) };
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
        const int_slice = arena.alloc(i32, value.len) catch {
            R.Rf_error("out of memory in struct-to-SEXP bool conversion");
            return R.R_NilValue;
        };
        for (value, 0..) |b, i| int_slice[i] = if (b) 1 else 0;
        const result = fromLogicalSlice(int_slice);
        arena.free(int_slice);
        return result;
    }
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
    if (comptime T == []const bool) {
        const int_slice = try toLogicalSlice(arena, sexp);
        const result = try arena.alloc(bool, int_slice.len);
        for (int_slice, 0..) |v, i| result[i] = v == 1;
        return result;
    }
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

fn charsxpBytes(elt: SEXP) []const u8 {
    if (elt == R.R_NaString) return "";
    return std.mem.sliceTo(R.Rf_translateCharUTF8(elt), 0);
}

fn buildNameIndex(ns: SEXP, allocator: std.mem.Allocator) !std.StringHashMapUnmanaged(usize) {
    var index: std.StringHashMapUnmanaged(usize) = .empty;
    errdefer index.deinit(allocator);

    for (0..xlength(ns)) |i| {
        const elt = R.STRING_ELT(ns, @intCast(i));
        if (elt == R.R_NaString) continue;

        const name = charsxpBytes(elt);
        const gop = try index.getOrPut(allocator, name);
        if (!gop.found_existing) gop.value_ptr.* = i;
    }

    return index;
}

fn structFromSexp(comptime T: type, sexp: SEXP, arena: std.mem.Allocator) !T {
    const fields = @typeInfo(T).@"struct".fields;
    const ns = try expectNamedList(sexp);
    var name_index = try buildNameIndex(ns, arena);
    defer name_index.deinit(arena);
    var result: T = undefined;

    inline for (fields) |field| {
        if (name_index.get(field.name)) |i| {
            const elem = R.VECTOR_ELT(sexp, @intCast(i));
            @field(result, field.name) = try sexpToZig(field.type, elem, arena);
        } else {
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
        const n = xlength(sexp);
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
        const n = xlength(sexp);
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
// R's LOGICAL() and INTEGER() both return int* the difference is semantic only.
const LogicalChunkIter = struct {
    sexp: SEXP,
    n: usize,
    offset: usize = 0,
    direct: ?[]const i32,
    buf: [int_chunk_len]i32 = undefined,

    fn init(sexp: SEXP) LogicalChunkIter {
        const n = xlength(sexp);
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

/// Sum of an INTSXP using SIMD @Vector reduction on i32 widened to i64.
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

/// Count TRUE values in a LGLSXP using direct owned-backing or LOGICAL_GET_REGION.
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

/// Scan a chunk for any NA_REAL values. Used before the SIMD loop to pick
/// the fast path (no masking) for NA-free data.
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

/// Minimum of a REALSXP using SIMD @Vector reduction.
/// NA-free chunks avoid the 4-op NA masking penalty entirely.
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

/// Maximum of a REALSXP using SIMD @Vector reduction.
/// NA-free chunks avoid the 4-op NA masking penalty entirely.
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

/// Mean of a REALSXP excluding NA values.
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

/// Computes `sum(x * alpha + beta)` using SIMD.
/// Each element is scaled by `alpha` then shifted by `beta`.
/// Returns the sum of all `x[i] * alpha + beta`.
/// ALTREP-aware via RealChunkIter.
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

/// Element-wise minimum of two REALSXPs using a caller-provided
/// arena for internal allocations.  Reuse one arena across many
/// calls for hot loops.
pub fn pminAlloc(a: SEXP, b: SEXP, arena: std.mem.Allocator) SEXP {
    const da = (toRealSliceView(arena, a) catch |err| signalError(err)).constSlice();
    const db = (toRealSliceView(arena, b) catch |err| signalError(err)).constSlice();
    const n = if (da.len == 0 or db.len == 0) @as(usize, 0) else @max(da.len, db.len);

    var result = protect.scoped(R.Rf_allocVector(R.REALSXP, @intCast(n)));
    defer result.deinit();
    const rp = R.REAL(result.get());

    for (0..n) |i| {
        rp[i] = @min(da[i % da.len], db[i % db.len]);
    }

    return result.get();
}

/// Element-wise minimum of two REALSXPs.  Creates an internal arena
/// for each call.  For repeated calls use pminAlloc with a shared arena.
pub fn pmin(a: SEXP, b: SEXP) SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    return pminAlloc(a, b, arena.allocator());
}

/// Element-wise maximum of two REALSXPs using a caller-provided
/// arena for internal allocations.
pub fn pmaxAlloc(a: SEXP, b: SEXP, arena: std.mem.Allocator) SEXP {
    const da = (toRealSliceView(arena, a) catch |err| signalError(err)).constSlice();
    const db = (toRealSliceView(arena, b) catch |err| signalError(err)).constSlice();
    const n = if (da.len == 0 or db.len == 0) @as(usize, 0) else @max(da.len, db.len);

    var result = protect.scoped(R.Rf_allocVector(R.REALSXP, @intCast(n)));
    defer result.deinit();
    const rp = R.REAL(result.get());

    for (0..n) |i| {
        rp[i] = @max(da[i % da.len], db[i % db.len]);
    }

    return result.get();
}

/// Element-wise maximum of two REALSXPs.  Creates an internal arena
/// for each call.  For repeated calls use pmaxAlloc with a shared arena.
pub fn pmax(a: SEXP, b: SEXP) SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    return pmaxAlloc(a, b, arena.allocator());
}

/// Cumulative sum of a REALSXP using chunked iteration for ALTREP
/// compatibility. Returns a new REALSXP.
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

/// Convert a Zig struct to an R named list using a caller-owned arena.
/// Reuse the arena across hot-loop calls. Field names become list names.
/// Supports nested structs, slices, scalars, optionals, and SEXP.
///
/// Recommended boundary helper for Zig struct -> R list (W4.3).
///
/// Cost (x86_64-linux, see plan/PLAN.md W4.3 Findings):
///   - 5 fields:  0.25us median, 0 Zig allocs, ~8 R-heap calls
///   - 10 fields: 0.48us median, 0 Zig allocs, ~14 R-heap calls
///   - 20 fields: 0.93us median, 0 Zig allocs, ~25 R-heap calls
///   - ~50ns per field, dominated by R's allocator (`Rf_allocVector`,
///     `Rf_mkChar`). The Zig overhead is real but small.
///
/// Use `asSEXP` if you do not have a hot loop (it creates an internal
/// arena). Use `asSEXPAlloc` with a shared arena in any loop that
/// produces a struct per iteration; the per-call alloc count is 0
/// because the arena amortizes its chunk growth.
///
/// Allocations on the R heap (not counted in Zig-heap metrics):
///   1 VECSXP for the list + 1 STRSXP for field names + N
///   `Rf_allocVector` calls for slice fields + N `Rf_mkChar` calls
///   for field names. The result SEXP is not protected; the caller
///   must `Rf_protect` it if it should survive past the next R
///   allocation.
pub fn asSEXPAlloc(st: anytype, arena: std.mem.Allocator) SEXP {
    return structToSexp(st, @TypeOf(st), arena);
}

/// Convert a Zig struct to an R named list, creating an internal arena.
/// For repeated conversions in a loop, use `asSEXPAlloc` with a shared arena.
pub fn asSEXP(st: anytype) SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    return asSEXPAlloc(st, arena.allocator());
}

/// Convert an R named list to a Zig struct. Matches field names against
/// list names. Missing optional fields default to null. Missing
/// non-optional fields signal an R error. R_NilValue in a VECSXP
/// element is treated as missing, so `?T` fields cannot distinguish
/// "missing" from "present with null."
///
/// Recommended boundary helper for R list -> Zig struct (W4.3).
///
/// Cost (x86_64-linux, see plan/PLAN.md W4.3 Findings):
///   - 5 fields:  0.27us median, 1 Zig alloc/call (408 bytes, arena
///     chunk 1)
///   - 10 fields: 0.67us median, 2 Zig allocs/call (1692 bytes, arena
///     chunks 1+2)
///   - 20 fields: 1.30us median, 3 Zig allocs/call (3750 bytes, arena
///     chunks 1+2+3)
///   - ~65ns per field. Alloc count is logarithmic in field count
///     because the arena grows geometrically; 100+ field structs are
///     fine.
///
/// Use a fresh arena per call. The arena is freed on `deinit`. The
/// returned struct borrows any slice fields from the SEXP - the SEXP
/// must outlive the struct. Slice elements are validated against the
/// expected type; mismatches signal an R error.
///
/// There is no R-heap allocation on this path. All work is in Zig.
pub fn fromSEXP(comptime T: type, sexp: SEXP, arena: std.mem.Allocator) T {
    return structFromSexp(T, sexp, arena) catch |err| signalError(err);
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

/// Returns null if COMPLEX returns null (some exotic ALTREP). Caller must ensure the SEXP is the expected type.
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
    defer allocator.free(data);
    data[0] = 1.0;
    data[1] = 2.0;
    data[2] = 3.0;
    var view: SliceView(f64) = .{ .owned = data };
    const slice = view.constSlice();
    try std.testing.expectEqual(slice.len, 3);
    try std.testing.expectEqual(slice[0], 1.0);
}

test "SliceView deinit owned frees memory" {
    const allocator = std.testing.allocator;
    var data = try allocator.alloc(i32, 2);
    data[0] = 42;
    data[1] = 43;
    var view: SliceView(i32) = .{ .owned = data };
    view.deinit(allocator);
}

test "SliceView deinit borrowed is no-op" {
    const data: [2]i32 = .{ 1, 2 };
    var view: SliceView(i32) = .{ .borrowed = &data };
    view.deinit(std.testing.allocator);
}

test "errorMessage covers all ConvertError variants" {
    try std.testing.expectEqualSlices(u8, errorMessage(error.ExpectedReal), "expected REALSXP");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ExpectedInteger), "expected INTSXP");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ExpectedLogical), "expected LGLSXP");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ExpectedString), "expected STRSXP");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ExpectedList), "expected VECSXP");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ExpectedNamedList), "expected named VECSXP");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ExpectedRaw), "expected RAWSXP");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ExpectedComplex), "expected CPLXSXP");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ZeroLength), "expected non-empty vector");
    try std.testing.expectEqualSlices(u8, errorMessage(error.ScalarNA), "scalar inputs must not be NA");
    try std.testing.expectEqualSlices(u8, errorMessage(error.AltRepRegionRead), "ALTREP region read failed");
    try std.testing.expectEqualSlices(u8, errorMessage(error.MissingField), "missing required field in R list");
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
    const v = StringView{ .charsxp = undefined, .bytes = "", .len = 0, .is_na = false };
    try std.testing.expectEqual(@TypeOf(v.is_na), bool);
}

test "StringSliceView type compiles" {
    const v = StringSliceView{ .sexp = undefined, .len = 0 };
    _ = v;
}

test "CachedStringSliceView type compiles" {
    const items: []const StringView = &.{};
    const v = CachedStringSliceView{ .items = items, .len = 0 };
    try std.testing.expectEqual(@TypeOf(v.len), usize);
}
