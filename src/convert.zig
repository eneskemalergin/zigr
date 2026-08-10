//! Convert between Zig values and R SEXPs.
//!
//! Borrowed views stay within the source R call. Legacy slice converters use a
//! contiguous native fallback when R exposes only regions; VectorAccess adds
//! explicit direct, bounded-region, and deliberate-materialization policies.

const std = @import("std");
const SEXP = @import("sexp.zig").SEXP;
const xlength = @import("sexp.zig").xlength;
const tryXlength = @import("sexp.zig").tryXlength;
const sexp_mod = @import("sexp.zig");
const R = @import("R");
const simd = @import("simd");
const cleanup = @import("cleanup");
const memory = @import("memory.zig");
const protect = @import("protect.zig");

pub const Rcomplex = extern struct { r: f64, i: f64 };

const ResultCleanup = struct {
    protected: bool = false,

    fn fire(state: *ResultCleanup) void {
        if (state.protected) {
            state.protected = false;
            protect.unprotect();
        }
    }
};

const ResultGuard = struct {
    protected: protect.ScopedProtect,
    cleanup_state: *ResultCleanup,
    unwind_frame: cleanup.FrameHandle,
    active: bool = true,

    fn init(sexptype: R.SEXPTYPE, len: usize) ResultGuard {
        const registration = cleanup.pushFrameInlineWithHandle(ResultCleanup, .{}, ResultCleanup.fire);
        const result = R.Rf_allocVector(sexptype, @intCast(len));
        const protected = protect.scoped(result);
        registration.state.protected = true;
        return .{
            .protected = protected,
            .cleanup_state = registration.state,
            .unwind_frame = registration.handle,
        };
    }

    fn get(self: ResultGuard) SEXP {
        if (!self.active or !cleanup.frameIsActive(self.unwind_frame)) @panic("result builder is inactive");
        return self.protected.get();
    }

    fn deinit(self: *ResultGuard) void {
        if (!self.active) return;
        self.active = false;
        if (!cleanup.frameIsActive(self.unwind_frame)) return;
        self.protected.deinit();
        self.cleanup_state.protected = false;
        _ = cleanup.releaseFrame(self.unwind_frame);
    }

    fn finish(self: *ResultGuard) SEXP {
        const result = self.get();
        self.deinit();
        return result;
    }
};

const AllocSliceCleanup = struct {
    allocator: std.mem.Allocator,
    memory: []u8,
    alignment: std.mem.Alignment,
    return_address: usize,

    fn init(comptime T: type, allocator: std.mem.Allocator, slice: []const T, ra: usize) AllocSliceCleanup {
        return .{
            .allocator = allocator,
            .memory = @constCast(std.mem.sliceAsBytes(slice)),
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
    DirectPointerUnavailable,
    SchemaLength,
    SchemaNames,
    SchemaAttributes,
    OutOfMemory,
    NegativeLength,
    LengthOverflow,
    NullPointer,
};

fn hasType(sexp: SEXP, expected: c_uint) bool {
    return sexp_mod.typeTag(sexp) == @as(c_int, @intCast(expected));
}

fn expectType(sexp: SEXP, expected: c_uint, comptime err: ConvertError) ConvertError!void {
    if (sexp == null) return error.NullPointer;
    if (!hasType(sexp, expected)) return err;
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
        error.DirectPointerUnavailable => "ALTREP direct pointer is unavailable",
        error.SchemaLength => "fixed schema field count does not match",
        error.SchemaNames => "fixed schema names do not match",
        error.SchemaAttributes => "fixed schema has unsupported attributes",
        error.OutOfMemory => "out of memory during SEXP conversion",
        error.NullPointer => "SEXP pointer is null",
        else => @errorName(err),
    };
}

pub fn optionalInputIsNullish(comptime T: type, sexp: SEXP) bool {
    if (sexp == R.R_NilValue) return true;
    if (comptime T == f64) {
        return hasType(sexp, R.REALSXP) and
            R.XLENGTH(sexp) == 1 and
            R.ISNA(R.REAL(sexp)[0]) != 0;
    }
    if (comptime T == i32) {
        return hasType(sexp, R.INTSXP) and
            R.XLENGTH(sexp) == 1 and
            R.INTEGER(sexp)[0] == R.R_NaInt;
    }
    if (comptime T == bool) {
        return hasType(sexp, R.LGLSXP) and
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
    R.Rf_error("%s", &buf);
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

/// Borrowed data is not a GC root: the caller keeps the source SEXP rooted and
/// does not let the view escape its source R call. Owned data uses its recorded
/// allocator and releases its cleanup frame, when present, during `deinit`.
/// That allocator must remain valid during an R unwind; `UnwindArena` provides
/// call-scoped ownership for storage that may cross an R call.
pub fn SliceView(comptime T: type) type {
    return union(enum) {
        borrowed: []const T,
        owned: struct {
            data: []const T,
            allocator: std.mem.Allocator,
            unwind_frame: ?cleanup.FrameHandle = null,
        },

        pub fn constSlice(self: @This()) []const T {
            return switch (self) {
                .borrowed => |s| s,
                .owned => |s| s.data,
            };
        }

        pub fn deinit(self: *@This()) void {
            switch (self.*) {
                .owned => |s| {
                    if (s.unwind_frame) |frame| {
                        if (!cleanup.releaseFrame(frame)) {
                            self.* = undefined;
                            return;
                        }
                    }
                    s.allocator.free(s.data);
                },
                .borrowed => {},
            }
            self.* = undefined;
        }
    };
}

pub const RawSliceView = SliceView(u8);

/// Declares how a generated kernel consumes a vector input.
pub const AccessNeed = enum {
    one_pass,
    repeated_pass,
    random_access,
};

/// Identifies the representation selected for a vector input.
pub const AccessStrategy = enum {
    direct,
    region,
    materialized,
};

const region_chunk_elements = 256;

fn checkedRegionCount(got: R.R_xlen_t, requested: R.R_xlen_t) ConvertError!usize {
    if (got <= 0 or got > requested) return error.AltRepRegionRead;
    return @intCast(got);
}

/// A call-scoped vector input selected for one-pass, repeated, or random access.
/// Direct storage borrows the R vector. Region storage owns one bounded buffer,
/// and materialized storage owns one contiguous native representation. The
/// caller keeps the source SEXP rooted and deinitializes the value before leaving
/// its allocation scope. The allocator must remain valid during an R unwind.
/// When multiple access values share a cleanup stack, each deinitialization
/// releases only its own cleanup frame.
pub fn VectorAccess(comptime T: type, comptime need: AccessNeed) type {
    return union(enum) {
        direct: struct {
            data: []const T,
            emitted: bool = false,
        },
        region: struct {
            sexp: SEXP,
            len: usize,
            offset: usize,
            buffer: []T,
            allocator: std.mem.Allocator,
            unwind_frame: ?cleanup.FrameHandle,
        },
        materialized: struct {
            data: []const T,
            allocator: std.mem.Allocator,
            emitted: bool = false,
            unwind_frame: ?cleanup.FrameHandle,
        },

        pub const zigr_vector_access = true;
        pub const element_type = T;
        pub const access_need = need;

        pub fn strategy(self: @This()) AccessStrategy {
            return switch (self) {
                .direct => .direct,
                .region => .region,
                .materialized => .materialized,
            };
        }

        pub fn contiguousSlice(self: @This()) ?[]const T {
            return switch (self) {
                .direct => |state| state.data,
                .materialized => |state| state.data,
                .region => null,
            };
        }

        pub fn next(self: *@This()) ConvertError!?[]const T {
            switch (self.*) {
                .direct => |*state| {
                    if (state.emitted) return null;
                    state.emitted = true;
                    return state.data;
                },
                .materialized => |*state| {
                    if (state.emitted) return null;
                    state.emitted = true;
                    return state.data;
                },
                .region => |*state| {
                    if (state.offset >= state.len) return null;
                    const remaining = state.len - state.offset;
                    const requested = @min(remaining, state.buffer.len);
                    const got = try checkedRegionCount(
                        readRegion(T, state.sexp, state.offset, requested, state.buffer),
                        @intCast(requested),
                    );
                    state.offset += got;
                    return state.buffer[0..got];
                },
            }
        }

        pub fn deinit(self: *@This()) void {
            switch (self.*) {
                .direct => {},
                .region => |state| {
                    if (state.unwind_frame) |frame| {
                        if (!cleanup.releaseFrame(frame)) {
                            self.* = undefined;
                            return;
                        }
                    }
                    state.allocator.free(state.buffer);
                },
                .materialized => |state| {
                    if (state.unwind_frame) |frame| {
                        if (!cleanup.releaseFrame(frame)) {
                            self.* = undefined;
                            return;
                        }
                    }
                    state.allocator.free(state.data);
                },
            }
            self.* = undefined;
        }
    };
}

/// A read-only numeric input view. Ordinary R storage is borrowed; an ALTREP
/// fallback owns a call-scoped contiguous representation.
pub const RealSliceView = SliceView(f64);

/// A read-only integer input view. Ordinary R storage is borrowed; an ALTREP
/// fallback owns a call-scoped contiguous representation.
pub const IntegerSliceView = SliceView(i32);

/// A read-only complex input view. Ordinary R storage is borrowed; an ALTREP
/// fallback owns a call-scoped contiguous representation.
pub const ComplexSliceView = SliceView(Rcomplex);

/// A generated-boundary logical input view. Values remain R's three-state
/// representation: `0`, `1`, or `R_NaInt`; callers must not coerce them to
/// `bool` when missingness is possible. The view is call-scoped; any fallback
/// storage belongs to the generated wrapper's arena.
pub const LogicalSliceView = struct {
    data: []const i32,

    pub fn constSlice(self: @This()) []const i32 {
        return self.data;
    }
};

/// A generated logical result whose values retain R's three-state encoding.
/// The caller owns the native storage until result conversion completes.
pub const LogicalSlice = struct {
    data: []const i32,
};

fn expectVectorType(comptime T: type, sexp: SEXP) ConvertError!void {
    if (comptime T == f64) return expectType(sexp, R.REALSXP, error.ExpectedReal);
    if (comptime T == i32) return expectType(sexp, R.INTSXP, error.ExpectedInteger);
    if (comptime T == u8) return expectType(sexp, R.RAWSXP, error.ExpectedRaw);
    if (comptime T == Rcomplex) return expectType(sexp, R.CPLXSXP, error.ExpectedComplex);
    @compileError("unsupported vector access type: " ++ @typeName(T));
}

fn directVectorSlice(comptime T: type, sexp: SEXP, representation: VectorRepresentation) ?[]const T {
    if (comptime T == f64) return directRealSliceOrNull(sexp, representation);
    if (comptime T == i32) return directIntSliceOrNull(sexp, representation);
    if (comptime T == u8) return directRawSliceOrNull(sexp, representation);
    if (comptime T == Rcomplex) return directComplexSliceOrNull(sexp, representation);
    @compileError("unsupported vector access type: " ++ @typeName(T));
}

fn materializedVectorSlice(comptime T: type, allocator: std.mem.Allocator, sexp: SEXP, representation: VectorRepresentation) ![]const T {
    if (comptime T == f64) return toRealSliceWithRepresentation(allocator, sexp, representation, false, null);
    if (comptime T == i32) return toIntSliceWithRepresentation(allocator, sexp, representation, false, null);
    if (comptime T == u8) return toRawSliceWithRepresentation(allocator, sexp, representation, false, null);
    if (comptime T == Rcomplex) return toComplexSliceWithRepresentation(allocator, sexp, representation, false, null);
    @compileError("unsupported vector access type: " ++ @typeName(T));
}

fn readRegion(comptime T: type, sexp: SEXP, start: usize, requested: usize, buffer: []T) R.R_xlen_t {
    const offset: R.R_xlen_t = @intCast(start);
    const count: R.R_xlen_t = @intCast(requested);
    if (comptime T == f64) return R.REAL_GET_REGION(sexp, offset, count, buffer.ptr);
    if (comptime T == i32) return R.INTEGER_GET_REGION(sexp, offset, count, buffer.ptr);
    if (comptime T == u8) return R.RAW_GET_REGION(sexp, offset, count, buffer.ptr);
    if (comptime T == Rcomplex) return R.COMPLEX_GET_REGION(sexp, offset, count, @ptrCast(buffer.ptr));
    @compileError("unsupported vector access type: " ++ @typeName(T));
}

pub fn toVectorAccess(comptime T: type, comptime need: AccessNeed, allocator: std.mem.Allocator, sexp: SEXP) !VectorAccess(T, need) {
    try expectVectorType(T, sexp);
    const representation = try vectorRepresentation(sexp);
    const direct = directVectorSlice(T, sexp, representation);
    const strategy: AccessStrategy = if (direct != null) .direct else switch (need) {
        .one_pass => .region,
        .repeated_pass, .random_access => .materialized,
    };
    return toVectorAccessWithRepresentation(T, need, allocator, sexp, representation, strategy);
}

pub fn toVectorAccessWithStrategy(
    comptime T: type,
    comptime need: AccessNeed,
    allocator: std.mem.Allocator,
    sexp: SEXP,
    strategy: AccessStrategy,
) !VectorAccess(T, need) {
    try expectVectorType(T, sexp);
    const representation = try vectorRepresentation(sexp);
    return toVectorAccessWithRepresentation(T, need, allocator, sexp, representation, strategy);
}

fn toVectorAccessWithRepresentation(
    comptime T: type,
    comptime need: AccessNeed,
    allocator: std.mem.Allocator,
    sexp: SEXP,
    representation: VectorRepresentation,
    strategy: AccessStrategy,
) !VectorAccess(T, need) {
    switch (strategy) {
        .direct => {
            const data = directVectorSlice(T, sexp, representation) orelse return error.DirectPointerUnavailable;
            return .{ .direct = .{ .data = data } };
        },
        .region => {
            if (representation.len == 0) {
                return .{ .region = .{
                    .sexp = sexp,
                    .len = 0,
                    .offset = 0,
                    .buffer = &.{},
                    .allocator = allocator,
                    .unwind_frame = null,
                } };
            }
            const buffer_len = @min(representation.len, region_chunk_elements);
            cleanup.requireCapacity(2);
            const buffer = try allocator.alloc(T, buffer_len);
            const registration = cleanup.pushFrameInlineWithHandle(
                AllocSliceCleanup,
                AllocSliceCleanup.init(T, allocator, buffer, @returnAddress()),
                AllocSliceCleanup.fire,
            );
            return .{ .region = .{
                .sexp = sexp,
                .len = representation.len,
                .offset = 0,
                .buffer = buffer,
                .allocator = allocator,
                .unwind_frame = registration.handle,
            } };
        },
        .materialized => {
            cleanup.requireCapacity(3);
            const data = try materializedVectorSlice(T, allocator, sexp, representation);
            if (data.len == 0) {
                return .{ .materialized = .{
                    .data = data,
                    .allocator = allocator,
                    .unwind_frame = null,
                } };
            }
            const registration = cleanup.pushFrameInlineWithHandle(
                AllocSliceCleanup,
                AllocSliceCleanup.init(T, allocator, data, @returnAddress()),
                AllocSliceCleanup.fire,
            );
            return .{ .materialized = .{
                .data = data,
                .allocator = allocator,
                .unwind_frame = registration.handle,
            } };
        },
    }
}

pub fn toRealSliceView(allocator: std.mem.Allocator, sexp: SEXP) !RealSliceView {
    return toRealSliceViewWithCleanup(allocator, sexp, true);
}

/// Uses the supplied arena as the owner of fallback storage. The returned
/// view remains deinitializable and does not register a separate cleanup frame.
pub fn toRealSliceViewWithArenaOwner(allocator: std.mem.Allocator, sexp: SEXP) !RealSliceView {
    return toRealSliceViewWithCleanup(allocator, sexp, false);
}

fn toRealSliceViewWithCleanup(allocator: std.mem.Allocator, sexp: SEXP, comptime retain_cleanup: bool) !RealSliceView {
    try expectType(sexp, R.REALSXP, error.ExpectedReal);
    const representation = try vectorRepresentation(sexp);
    if (directRealSliceOrNull(sexp, representation)) |data| return .{ .borrowed = data };
    var unwind_frame: ?cleanup.FrameHandle = null;
    return .{ .owned = .{
        .data = try toRealSliceWithRepresentation(allocator, sexp, representation, retain_cleanup, &unwind_frame),
        .allocator = allocator,
        .unwind_frame = unwind_frame,
    } };
}

pub fn toIntSliceView(allocator: std.mem.Allocator, sexp: SEXP) !IntegerSliceView {
    return toIntSliceViewWithCleanup(allocator, sexp, true);
}

/// Uses the supplied arena as the owner of fallback storage. The returned
/// view remains deinitializable and does not register a separate cleanup frame.
pub fn toIntSliceViewWithArenaOwner(allocator: std.mem.Allocator, sexp: SEXP) !IntegerSliceView {
    return toIntSliceViewWithCleanup(allocator, sexp, false);
}

fn toIntSliceViewWithCleanup(allocator: std.mem.Allocator, sexp: SEXP, comptime retain_cleanup: bool) !IntegerSliceView {
    try expectType(sexp, R.INTSXP, error.ExpectedInteger);
    const representation = try vectorRepresentation(sexp);
    if (directIntSliceOrNull(sexp, representation)) |data| return .{ .borrowed = data };
    var unwind_frame: ?cleanup.FrameHandle = null;
    return .{ .owned = .{
        .data = try toIntSliceWithRepresentation(allocator, sexp, representation, retain_cleanup, &unwind_frame),
        .allocator = allocator,
        .unwind_frame = unwind_frame,
    } };
}

pub fn toRealSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]f64 {
    try expectType(sexp, R.REALSXP, error.ExpectedReal);
    return toRealSliceWithRepresentation(allocator, sexp, try vectorRepresentation(sexp), false, null);
}

fn toRealSliceWithRepresentation(
    allocator: std.mem.Allocator,
    sexp: SEXP,
    representation: VectorRepresentation,
    comptime retain_cleanup: bool,
    unwind_frame: ?*?cleanup.FrameHandle,
) ![]f64 {
    const n = representation.len;
    cleanup.requireCapacity(2);
    const result = try allocator.alloc(f64, n);
    errdefer allocator.free(result);
    const registration = cleanup.pushFrameInlineWithHandle(
        AllocSliceCleanup,
        AllocSliceCleanup.init(f64, allocator, result, @returnAddress()),
        AllocSliceCleanup.fire,
    );
    errdefer {
        _ = cleanup.releaseFrame(registration.handle);
    }
    if (directRealSliceOrNull(sexp, representation)) |data| {
        @memcpy(result, data);
    } else if (representation.altrep) {
        var offset: R.R_xlen_t = 0;
        const ncast = @as(R.R_xlen_t, @intCast(n));
        while (offset < ncast) {
            const remaining = ncast - offset;
            const got = try checkedRegionCount(
                R.REAL_GET_REGION(sexp, offset, remaining, result.ptr + @as(usize, @intCast(offset))),
                remaining,
            );
            offset += @intCast(got);
        }
    } else {
        unreachable;
    }
    if (comptime retain_cleanup) unwind_frame.?.* = registration.handle else _ = cleanup.releaseFrame(registration.handle);
    return result;
}

/// Owns one protected, final R vector. `mutableSlice` exposes its typed R
/// storage while it remains protected; `finish` transfers the completed
/// vector to the caller and `deinit` abandons it during error cleanup. Access
/// after `finish` or `deinit` is rejected before an unrooted SEXP is exposed.
pub fn ResultBuilder(comptime T: type) type {
    comptime _ = typeToSEXPTYPE(T);
    return struct {
        guard: ResultGuard,
        len: usize,

        const Self = @This();
        pub const storage_type = if (T == bool) i32 else T;

        pub fn init(len: usize) Self {
            return .{ .guard = ResultGuard.init(typeToSEXPTYPE(T), len), .len = len };
        }

        pub fn initFromInput(input: SEXP) ConvertError!Self {
            try expectType(input, typeToSEXPTYPE(T), expectedInputError(T));
            return init(try tryXlength(input));
        }

        pub fn get(self: Self) SEXP {
            return self.guard.get();
        }

        pub fn mutableSlice(self: *Self) []storage_type {
            if (self.len == 0) return &[_]storage_type{};
            const ptr: [*]storage_type = switch (T) {
                f64 => @ptrCast(R.REAL(self.get())),
                i32 => @ptrCast(R.INTEGER(self.get())),
                bool => @ptrCast(R.LOGICAL(self.get())),
                u8 => @ptrCast(R.RAW(self.get())),
                Rcomplex => @ptrCast(@alignCast(R.COMPLEX(self.get()) orelse @panic("COMPLEX returned null on freshly allocated CPLXSXP"))),
                else => unreachable,
            };
            return ptr[0..self.len];
        }

        pub fn finish(self: *Self) SEXP {
            return self.guard.finish();
        }

        pub fn deinit(self: *Self) void {
            self.guard.deinit();
        }
    };
}

fn expectedInputError(comptime T: type) ConvertError {
    return switch (T) {
        f64 => error.ExpectedReal,
        i32 => error.ExpectedInteger,
        bool => error.ExpectedLogical,
        u8 => error.ExpectedRaw,
        Rcomplex => error.ExpectedComplex,
        else => @compileError("unsupported result builder type: " ++ @typeName(T)),
    };
}

/// Builds a protected STRSXP while each character constructor remains within
/// the builder's cleanup frame.
pub const StringResultBuilder = struct {
    guard: ResultGuard,
    len: usize,

    pub fn init(len: usize) StringResultBuilder {
        return .{ .guard = ResultGuard.init(R.STRSXP, len), .len = len };
    }

    pub fn get(self: StringResultBuilder) SEXP {
        return self.guard.get();
    }

    pub fn set(self: StringResultBuilder, index: usize, bytes: []const u8) void {
        if (index >= self.len) @panic("StringResultBuilder.set index out of bounds");
        const ptr: [*]const u8 = if (bytes.len == 0) "" else bytes.ptr;
        const value = R.Rf_mkCharLenCE(@ptrCast(ptr), @intCast(bytes.len), @as(R.cetype_t, @intCast(R.CE_UTF8)));
        R.SET_STRING_ELT(self.get(), @intCast(index), value);
    }

    pub fn finish(self: *StringResultBuilder) SEXP {
        return self.guard.finish();
    }

    pub fn deinit(self: *StringResultBuilder) void {
        self.guard.deinit();
    }
};

/// Builds a protected VECSXP while nested values and names are allocated.
pub const ListResultBuilder = struct {
    guard: ResultGuard,
    len: usize,

    pub fn init(len: usize) ListResultBuilder {
        return .{ .guard = ResultGuard.init(R.VECSXP, len), .len = len };
    }

    pub fn get(self: ListResultBuilder) SEXP {
        return self.guard.get();
    }

    pub fn set(self: ListResultBuilder, index: usize, value: SEXP) void {
        if (index >= self.len) @panic("ListResultBuilder.set index out of bounds");
        _ = R.SET_VECTOR_ELT(self.get(), @intCast(index), value);
    }

    pub fn finish(self: *ListResultBuilder) SEXP {
        return self.guard.finish();
    }

    pub fn deinit(self: *ListResultBuilder) void {
        self.guard.deinit();
    }
};

pub fn fromRealSlice(slice: []const f64) SEXP {
    var result = ResultBuilder(f64).init(slice.len);
    defer result.deinit();
    @memcpy(result.mutableSlice(), slice);
    return result.finish();
}

pub fn toIntSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]i32 {
    try expectType(sexp, R.INTSXP, error.ExpectedInteger);
    return toIntSliceWithRepresentation(allocator, sexp, try vectorRepresentation(sexp), false, null);
}

fn toIntSliceWithRepresentation(
    allocator: std.mem.Allocator,
    sexp: SEXP,
    representation: VectorRepresentation,
    comptime retain_cleanup: bool,
    unwind_frame: ?*?cleanup.FrameHandle,
) ![]i32 {
    const n = representation.len;
    cleanup.requireCapacity(2);
    const result = try allocator.alloc(i32, n);
    errdefer allocator.free(result);
    const registration = cleanup.pushFrameInlineWithHandle(
        AllocSliceCleanup,
        AllocSliceCleanup.init(i32, allocator, result, @returnAddress()),
        AllocSliceCleanup.fire,
    );
    errdefer {
        _ = cleanup.releaseFrame(registration.handle);
    }
    if (directIntSliceOrNull(sexp, representation)) |data| {
        @memcpy(result, data);
    } else if (representation.altrep) {
        var offset: R.R_xlen_t = 0;
        const ncast = @as(R.R_xlen_t, @intCast(n));
        while (offset < ncast) {
            const remaining = ncast - offset;
            const got = try checkedRegionCount(
                R.INTEGER_GET_REGION(sexp, offset, remaining, result.ptr + @as(usize, @intCast(offset))),
                remaining,
            );
            offset += @intCast(got);
        }
    } else {
        unreachable;
    }
    if (comptime retain_cleanup) unwind_frame.?.* = registration.handle else _ = cleanup.releaseFrame(registration.handle);
    return result;
}

pub fn fromIntSlice(slice: []const i32) SEXP {
    var result = ResultBuilder(i32).init(slice.len);
    defer result.deinit();
    @memcpy(result.mutableSlice(), slice);
    return result.finish();
}

/// The headers borrow R-owned bytes and must stay inside the source R call.
/// The allocator must remain valid if R unwinds while elements are read.
pub fn toStringSlice(allocator: std.mem.Allocator, sexp: SEXP) ![][]const u8 {
    try expectType(sexp, R.STRSXP, error.ExpectedString);
    const n = try tryXlength(sexp);
    cleanup.requireCapacity(2);
    const result = try allocator.alloc([]const u8, n);
    const registration = cleanup.pushFrameInlineWithHandle(
        AllocSliceCleanup,
        AllocSliceCleanup.init([]const u8, allocator, result, @returnAddress()),
        AllocSliceCleanup.fire,
    );
    errdefer _ = cleanup.releaseFrame(registration.handle);
    for (0..n) |i| {
        const elt = R.STRING_ELT(sexp, @intCast(i));
        result[i] = if (elt == R.R_NaString) "" else sexp_mod.charsxpBytes(elt);
    }
    _ = cleanup.releaseFrame(registration.handle);
    return result;
}

/// The headers borrow R-owned bytes and must stay inside the source R call.
/// The allocator must remain valid if R unwinds while elements are read.
pub fn toStringSliceNullable(allocator: std.mem.Allocator, sexp: SEXP) ![]?[]const u8 {
    try expectType(sexp, R.STRSXP, error.ExpectedString);
    const n = try tryXlength(sexp);
    cleanup.requireCapacity(2);
    const result = try allocator.alloc(?[]const u8, n);
    const registration = cleanup.pushFrameInlineWithHandle(
        AllocSliceCleanup,
        AllocSliceCleanup.init(?[]const u8, allocator, result, @returnAddress()),
        AllocSliceCleanup.fire,
    );
    errdefer _ = cleanup.releaseFrame(registration.handle);
    for (0..n) |i| {
        const elt = R.STRING_ELT(sexp, @intCast(i));
        result[i] = if (elt == R.R_NaString) null else sexp_mod.charsxpBytes(elt);
    }
    _ = cleanup.releaseFrame(registration.handle);
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

/// The caller keeps the source SEXP rooted. Returned character data remains
/// R-owned and may not escape the source R call.
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

/// The fields a string kernel may request from each R character element.
/// Projection values borrow R-owned state and are valid only for the current R
/// call while the caller roots the source SEXP. `bytes` is the stored CHARSXP
/// byte sequence; `translated_text` is R-managed UTF-8 text produced by
/// `Rf_translateCharUTF8`.
pub const StringProjection = enum {
    identity,
    missingness,
    bytes,
    encoding_mark,
    translated_text,
    metadata,
};

pub const StringProjectionProbe = struct {
    requested: ?StringProjection = null,
    elements: usize = 0,
    identity_reads: usize = 0,
    missingness_reads: usize = 0,
    byte_reads: usize = 0,
    encoding_reads: usize = 0,
    translation_reads: usize = 0,
    broader_work: usize = 0,

    fn recordRequest(self: *StringProjectionProbe, comptime projection: StringProjection) void {
        if (self.requested == null) {
            self.requested = projection;
        } else if (self.requested.? != projection) {
            self.broader_work += 1;
        }
        self.elements += 1;
    }

    fn recordOperation(self: *StringProjectionProbe, comptime operation: StringProjection) void {
        const requested = self.requested orelse return;
        const allowed = switch (requested) {
            .identity => operation == .identity,
            .missingness => operation == .missingness,
            .bytes => operation == .bytes,
            .encoding_mark => operation == .encoding_mark,
            // Classification is required to preserve CE_BYTES without
            // asking R to translate stored bytes.
            .translated_text => operation == .encoding_mark or operation == .bytes or operation == .translated_text,
            .metadata => operation == .identity or operation == .missingness or operation == .encoding_mark,
        };
        if (!allowed) self.broader_work += 1;
        switch (operation) {
            .identity => self.identity_reads += 1,
            .missingness => self.missingness_reads += 1,
            .bytes => self.byte_reads += 1,
            .encoding_mark => self.encoding_reads += 1,
            .translated_text => self.translation_reads += 1,
            .metadata => {},
        }
    }
};

pub const StringIdentity = struct { charsxp: SEXP };
pub const StringMissingness = struct { is_na: bool };
pub const StringBytes = struct { charsxp: SEXP, bytes: []const u8 };
pub const StringEncoding = struct { charsxp: SEXP, encoding_mark: R.cetype_t };
pub const StringTranslatedText = struct { charsxp: SEXP, bytes: []const u8 };
pub const StringMetadata = struct {
    charsxp: SEXP,
    is_na: bool,
    encoding_mark: R.cetype_t,
};

fn StringProjectionValue(comptime projection: StringProjection) type {
    return switch (projection) {
        .identity => StringIdentity,
        .missingness => StringMissingness,
        .bytes => StringBytes,
        .encoding_mark => StringEncoding,
        .translated_text => StringTranslatedText,
        .metadata => StringMetadata,
    };
}

fn stringProjectionValue(comptime projection: StringProjection, elt: SEXP, probe: ?*StringProjectionProbe) StringProjectionValue(projection) {
    const is_na = elt == R.R_NaString;
    if (comptime projection == .identity) {
        if (probe) |p| p.recordOperation(.identity);
        return .{ .charsxp = elt };
    }
    if (comptime projection == .missingness) {
        if (probe) |p| p.recordOperation(.missingness);
        return .{ .is_na = is_na };
    }
    if (comptime projection == .bytes) {
        if (!is_na) {
            if (probe) |p| p.recordOperation(.bytes);
        }
        return .{ .charsxp = elt, .bytes = if (is_na) "" else sexp_mod.charsxpRawBytes(elt) };
    }
    if (comptime projection == .encoding_mark) {
        if (!is_na) {
            if (probe) |p| p.recordOperation(.encoding_mark);
        }
        return .{
            .charsxp = elt,
            .encoding_mark = if (is_na) @as(R.cetype_t, @intCast(R.CE_NATIVE)) else R.Rf_getCharCE(elt),
        };
    }
    if (comptime projection == .translated_text) {
        if (!is_na) {
            const encoding_mark = R.Rf_getCharCE(elt);
            if (probe) |p| p.recordOperation(.encoding_mark);
            if (encoding_mark == @as(R.cetype_t, @intCast(R.CE_BYTES))) {
                if (probe) |p| p.recordOperation(.bytes);
            } else {
                if (probe) |p| p.recordOperation(.translated_text);
            }
            return .{
                .charsxp = elt,
                .bytes = sexp_mod.charsxpTranslatedBytesWithEncoding(elt, encoding_mark),
            };
        }
        return .{ .charsxp = elt, .bytes = "" };
    }
    if (probe) |p| {
        p.recordOperation(.identity);
        p.recordOperation(.missingness);
        if (!is_na) p.recordOperation(.encoding_mark);
    }
    return .{
        .charsxp = elt,
        .is_na = is_na,
        .encoding_mark = if (is_na) @as(R.cetype_t, @intCast(R.CE_NATIVE)) else R.Rf_getCharCE(elt),
    };
}

/// A zero-allocation, call-scoped projection of an R character vector.
pub fn StringProjectionView(comptime projection: StringProjection, comptime probe_enabled: bool) type {
    return struct {
        sexp: SEXP,
        len: usize,
        is_altrep: bool,
        probe: if (probe_enabled) ?*StringProjectionProbe else void = if (probe_enabled) null else {},

        pub const selected_projection = projection;

        fn element(self: @This(), index: usize) SEXP {
            if (!self.is_altrep) return sexp_mod.fastVectorElt(self.sexp, index);
            return R.STRING_ELT(self.sexp, @intCast(index));
        }

        pub fn at(self: @This(), index: usize) StringProjectionValue(projection) {
            if (index >= self.len) @panic("StringProjectionView.at index out of bounds");
            const elt = self.element(index);
            if (comptime probe_enabled) {
                if (self.probe) |probe| probe.recordRequest(projection);
            }
            return stringProjectionValue(projection, elt, if (comptime probe_enabled) self.probe else null);
        }

        pub const Iterator = struct {
            view: StringProjectionView(projection, probe_enabled),
            index: usize = 0,

            pub fn next(self: *Iterator) ?StringProjectionValue(projection) {
                if (self.index >= self.view.len) return null;
                const value = self.view.at(self.index);
                self.index += 1;
                return value;
            }
        };

        pub fn iterator(self: @This()) Iterator {
            return .{ .view = self };
        }
    };
}

pub const StringIdentityView = StringProjectionView(.identity, false);
pub const StringMissingnessView = StringProjectionView(.missingness, false);
pub const StringBytesView = StringProjectionView(.bytes, false);
pub const StringEncodingView = StringProjectionView(.encoding_mark, false);
pub const StringTranslatedTextView = StringProjectionView(.translated_text, false);
pub const StringMetadataView = StringProjectionView(.metadata, false);

fn initStringProjectionView(comptime projection: StringProjection, comptime probe_enabled: bool, sexp: SEXP, probe: ?*StringProjectionProbe) !StringProjectionView(projection, probe_enabled) {
    try expectType(sexp, R.STRSXP, error.ExpectedString);
    return .{
        .sexp = sexp,
        .len = try tryXlength(sexp),
        .is_altrep = R.ALTREP(sexp) != 0,
        .probe = if (comptime probe_enabled) probe else {},
    };
}

pub fn toStringProjectionView(comptime projection: StringProjection, sexp: SEXP) !StringProjectionView(projection, false) {
    return initStringProjectionView(projection, false, sexp, null);
}

/// Diagnostic entry point. Generated wrappers use the probe-free constructor,
/// so observing a projection has no production hot-path state.
pub fn toStringProjectionViewWithProbe(comptime projection: StringProjection, sexp: SEXP, probe: *StringProjectionProbe) !StringProjectionView(projection, true) {
    return initStringProjectionView(projection, true, sexp, probe);
}

/// Caches native headers while borrowing each R character value and its bytes.
/// The caller keeps the source string vector rooted and limits use to its R
/// call. The allocator must remain valid during an R unwind while the cache is
/// live.
pub const CachedStringSliceView = struct {
    items: []const StringView,
    len: usize,
    allocator: std.mem.Allocator,
    unwind_frame: ?cleanup.FrameHandle = null,

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
        if (self.unwind_frame) |frame| {
            if (!cleanup.releaseFrame(frame)) {
                self.* = undefined;
                return;
            }
        }
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
    return toCachedStringSliceViewWithCleanup(allocator, sexp, true);
}

/// Uses the supplied arena as the owner of cached storage. The returned cache
/// remains deinitializable and does not register a separate cleanup frame.
pub fn toCachedStringSliceViewWithArenaOwner(allocator: std.mem.Allocator, sexp: SEXP) !CachedStringSliceView {
    return toCachedStringSliceViewWithCleanup(allocator, sexp, false);
}

fn toCachedStringSliceViewWithCleanup(allocator: std.mem.Allocator, sexp: SEXP, comptime retain_cleanup: bool) !CachedStringSliceView {
    try expectType(sexp, R.STRSXP, error.ExpectedString);
    const n = try tryXlength(sexp);
    cleanup.requireCapacity(2);
    const items = try allocator.alloc(StringView, n);
    const registration = cleanup.pushFrameInlineWithHandle(
        AllocSliceCleanup,
        AllocSliceCleanup.init(StringView, allocator, items, @returnAddress()),
        AllocSliceCleanup.fire,
    );
    errdefer {
        _ = cleanup.releaseFrame(registration.handle);
    }
    for (0..n) |i| {
        items[i] = makeStringView(R.STRING_ELT(sexp, @intCast(i)));
    }
    const unwind_frame: ?cleanup.FrameHandle = if (comptime retain_cleanup)
        registration.handle
    else blk: {
        _ = cleanup.releaseFrame(registration.handle);
        break :blk null;
    };
    return .{
        .items = items,
        .len = n,
        .allocator = allocator,
        .unwind_frame = unwind_frame,
    };
}

/// R character creation is protected by the builder's unwind cleanup frame.
pub fn fromStringSlice(slice: []const []const u8) SEXP {
    var result = StringResultBuilder.init(slice.len);
    defer result.deinit();
    for (slice, 0..) |value, i| result.set(i, value);
    return result.finish();
}

pub fn toLogicalSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]i32 {
    try expectType(sexp, R.LGLSXP, error.ExpectedLogical);
    return toLogicalSliceWithRepresentation(allocator, sexp, try vectorRepresentation(sexp), false, null);
}

fn toLogicalSliceWithRepresentation(
    allocator: std.mem.Allocator,
    sexp: SEXP,
    representation: VectorRepresentation,
    comptime retain_cleanup: bool,
    unwind_frame: ?*?cleanup.FrameHandle,
) ![]i32 {
    const n = representation.len;
    cleanup.requireCapacity(2);
    const result = try allocator.alloc(i32, n);
    errdefer allocator.free(result);
    const registration = cleanup.pushFrameInlineWithHandle(
        AllocSliceCleanup,
        AllocSliceCleanup.init(i32, allocator, result, @returnAddress()),
        AllocSliceCleanup.fire,
    );
    errdefer {
        _ = cleanup.releaseFrame(registration.handle);
    }
    if (directLogicalSliceOrNull(sexp, representation)) |data| {
        @memcpy(result, data);
    } else if (representation.altrep) {
        var offset: R.R_xlen_t = 0;
        const ncast = @as(R.R_xlen_t, @intCast(n));
        while (offset < ncast) {
            const remaining = ncast - offset;
            const got = try checkedRegionCount(
                R.LOGICAL_GET_REGION(sexp, offset, remaining, result.ptr + @as(usize, @intCast(offset))),
                remaining,
            );
            offset += @intCast(got);
        }
    } else {
        unreachable;
    }
    if (comptime retain_cleanup) unwind_frame.?.* = registration.handle else _ = cleanup.releaseFrame(registration.handle);
    return result;
}

pub fn toLogicalSliceView(allocator: std.mem.Allocator, sexp: SEXP) !SliceView(i32) {
    return toLogicalSliceViewWithCleanup(allocator, sexp, true);
}

/// Uses the supplied arena as the owner of fallback storage. The returned
/// view remains deinitializable and does not register a separate cleanup frame.
pub fn toLogicalSliceViewWithArenaOwner(allocator: std.mem.Allocator, sexp: SEXP) !SliceView(i32) {
    return toLogicalSliceViewWithCleanup(allocator, sexp, false);
}

fn toLogicalSliceViewWithCleanup(allocator: std.mem.Allocator, sexp: SEXP, comptime retain_cleanup: bool) !SliceView(i32) {
    try expectType(sexp, R.LGLSXP, error.ExpectedLogical);
    const representation = try vectorRepresentation(sexp);
    if (directLogicalSliceOrNull(sexp, representation)) |data| return .{ .borrowed = data };
    var unwind_frame: ?cleanup.FrameHandle = null;
    return .{ .owned = .{
        .data = try toLogicalSliceWithRepresentation(allocator, sexp, representation, retain_cleanup, &unwind_frame),
        .allocator = allocator,
        .unwind_frame = unwind_frame,
    } };
}

pub fn fromLogicalSlice(slice: []const i32) SEXP {
    var result = ResultBuilder(bool).init(slice.len);
    defer result.deinit();
    @memcpy(result.mutableSlice(), slice);
    return result.finish();
}

/// The returned element handles borrow R-owned storage and stay valid only
/// while the source list remains rooted in the current R call.
pub fn toListSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]SEXP {
    try expectType(sexp, R.VECSXP, error.ExpectedList);
    const n = try tryXlength(sexp);
    cleanup.requireCapacity(2);
    const result = try allocator.alloc(SEXP, n);
    const registration = cleanup.pushFrameInlineWithHandle(
        AllocSliceCleanup,
        AllocSliceCleanup.init(SEXP, allocator, result, @returnAddress()),
        AllocSliceCleanup.fire,
    );
    errdefer _ = cleanup.releaseFrame(registration.handle);
    for (0..n) |i| result[i] = R.VECTOR_ELT(sexp, @intCast(i));
    _ = cleanup.releaseFrame(registration.handle);
    return result;
}

pub fn fromListSlice(slice: []const SEXP) SEXP {
    var result = ListResultBuilder.init(slice.len);
    defer result.deinit();
    for (slice, 0..) |value, i| result.set(i, value);
    return result.finish();
}

pub fn toRawSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]const u8 {
    try expectType(sexp, R.RAWSXP, error.ExpectedRaw);
    return toRawSliceWithRepresentation(allocator, sexp, try vectorRepresentation(sexp), false, null);
}

fn toRawSliceWithRepresentation(
    allocator: std.mem.Allocator,
    sexp: SEXP,
    representation: VectorRepresentation,
    comptime retain_cleanup: bool,
    unwind_frame: ?*?cleanup.FrameHandle,
) ![]const u8 {
    const n = representation.len;
    cleanup.requireCapacity(2);
    const result = try allocator.alloc(u8, n);
    errdefer allocator.free(result);
    const registration = cleanup.pushFrameInlineWithHandle(
        AllocSliceCleanup,
        AllocSliceCleanup.init(u8, allocator, result, @returnAddress()),
        AllocSliceCleanup.fire,
    );
    errdefer {
        _ = cleanup.releaseFrame(registration.handle);
    }
    if (directRawSliceOrNull(sexp, representation)) |data| {
        @memcpy(result, data);
    } else if (representation.altrep) {
        var offset: R.R_xlen_t = 0;
        const ncast = @as(R.R_xlen_t, @intCast(n));
        while (offset < ncast) {
            const remaining = ncast - offset;
            const got = try checkedRegionCount(
                R.RAW_GET_REGION(sexp, offset, remaining, result.ptr + @as(usize, @intCast(offset))),
                remaining,
            );
            offset += @intCast(got);
        }
    } else {
        unreachable;
    }
    if (comptime retain_cleanup) unwind_frame.?.* = registration.handle else _ = cleanup.releaseFrame(registration.handle);
    return result;
}

pub fn toRawSliceView(allocator: std.mem.Allocator, sexp: SEXP) !RawSliceView {
    return toRawSliceViewWithCleanup(allocator, sexp, true);
}

/// Uses the supplied arena as the owner of fallback storage. The returned
/// view remains deinitializable and does not register a separate cleanup frame.
pub fn toRawSliceViewWithArenaOwner(allocator: std.mem.Allocator, sexp: SEXP) !RawSliceView {
    return toRawSliceViewWithCleanup(allocator, sexp, false);
}

fn toRawSliceViewWithCleanup(allocator: std.mem.Allocator, sexp: SEXP, comptime retain_cleanup: bool) !RawSliceView {
    try expectType(sexp, R.RAWSXP, error.ExpectedRaw);
    const representation = try vectorRepresentation(sexp);
    if (directRawSliceOrNull(sexp, representation)) |data| return .{ .borrowed = data };
    var unwind_frame: ?cleanup.FrameHandle = null;
    return .{ .owned = .{
        .data = try toRawSliceWithRepresentation(allocator, sexp, representation, retain_cleanup, &unwind_frame),
        .allocator = allocator,
        .unwind_frame = unwind_frame,
    } };
}

pub fn fromRawSlice(slice: []const u8) SEXP {
    var result = ResultBuilder(u8).init(slice.len);
    defer result.deinit();
    @memcpy(result.mutableSlice(), slice);
    return result.finish();
}

pub fn toComplexSlice(allocator: std.mem.Allocator, sexp: SEXP) ![]const Rcomplex {
    try expectType(sexp, R.CPLXSXP, error.ExpectedComplex);
    return toComplexSliceWithRepresentation(allocator, sexp, try vectorRepresentation(sexp), false, null);
}

fn toComplexSliceWithRepresentation(
    allocator: std.mem.Allocator,
    sexp: SEXP,
    representation: VectorRepresentation,
    comptime retain_cleanup: bool,
    unwind_frame: ?*?cleanup.FrameHandle,
) ![]Rcomplex {
    const n = representation.len;
    cleanup.requireCapacity(2);
    const result = try allocator.alloc(Rcomplex, n);
    errdefer allocator.free(result);
    const registration = cleanup.pushFrameInlineWithHandle(
        AllocSliceCleanup,
        AllocSliceCleanup.init(Rcomplex, allocator, result, @returnAddress()),
        AllocSliceCleanup.fire,
    );
    errdefer {
        _ = cleanup.releaseFrame(registration.handle);
    }
    if (directComplexSliceOrNull(sexp, representation)) |data| {
        @memcpy(result, data);
    } else if (representation.altrep) {
        var offset: R.R_xlen_t = 0;
        const ncast = @as(R.R_xlen_t, @intCast(n));
        while (offset < ncast) {
            const remaining = ncast - offset;
            const got = try checkedRegionCount(
                R.COMPLEX_GET_REGION(sexp, offset, remaining, @ptrCast(result.ptr + @as(usize, @intCast(offset)))),
                remaining,
            );
            offset += @intCast(got);
        }
    } else {
        return error.AltRepRegionRead;
    }
    if (comptime retain_cleanup) unwind_frame.?.* = registration.handle else _ = cleanup.releaseFrame(registration.handle);
    return result;
}

pub fn toComplexSliceView(allocator: std.mem.Allocator, sexp: SEXP) !ComplexSliceView {
    return toComplexSliceViewWithCleanup(allocator, sexp, true);
}

/// Uses the supplied arena as the owner of fallback storage. The returned
/// view remains deinitializable and does not register a separate cleanup frame.
pub fn toComplexSliceViewWithArenaOwner(allocator: std.mem.Allocator, sexp: SEXP) !ComplexSliceView {
    return toComplexSliceViewWithCleanup(allocator, sexp, false);
}

fn toComplexSliceViewWithCleanup(allocator: std.mem.Allocator, sexp: SEXP, comptime retain_cleanup: bool) !ComplexSliceView {
    try expectType(sexp, R.CPLXSXP, error.ExpectedComplex);
    const representation = try vectorRepresentation(sexp);
    if (directComplexSliceOrNull(sexp, representation)) |data| return .{ .borrowed = data };
    var unwind_frame: ?cleanup.FrameHandle = null;
    return .{ .owned = .{
        .data = try toComplexSliceWithRepresentation(allocator, sexp, representation, retain_cleanup, &unwind_frame),
        .allocator = allocator,
        .unwind_frame = unwind_frame,
    } };
}

pub fn fromComplexSlice(slice: []const Rcomplex) SEXP {
    var result = ResultBuilder(Rcomplex).init(slice.len);
    defer result.deinit();
    @memcpy(result.mutableSlice(), slice);
    return result.finish();
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
    if (comptime T == []const bool) @compileError("logical vectors require LogicalSlice to preserve NA");
    if (comptime T == LogicalSlice) return fromLogicalSlice(value.data);
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
    if (comptime T == []const bool) @compileError("logical vectors require LogicalSlice to preserve NA");
    if (comptime T == LogicalSlice) return .{ .data = try toLogicalSlice(arena, sexp) };
    if (comptime T == []const u8) return try toRawSlice(arena, sexp);
    if (comptime T == []const Rcomplex) return try toComplexSlice(arena, sexp);
    if (comptime @typeInfo(T) == .@"struct") {
        return try fixedSchemaFromSexp(T, sexp, arena);
    }
    @compileError("unsupported type in struct conversion: " ++ @typeName(T));
}

fn fixedSchemaToSexp(st: anytype, comptime T: type, arena: std.mem.Allocator) SEXP {
    const fields = @typeInfo(T).@"struct".fields;
    var vec = ListResultBuilder.init(fields.len);
    var names = StringResultBuilder.init(fields.len);
    defer vec.deinit();
    defer names.deinit();

    inline for (fields, 0..) |field, i| {
        const val = @field(st, field.name);
        const elt = zigToSexp(val, field.type, arena);
        vec.set(i, elt);
        names.set(i, field.name);
    }

    _ = R.Rf_namesgets(vec.get(), names.get());
    names.deinit();
    return vec.finish();
}

fn fixedSchemaFromSexp(comptime T: type, sexp: SEXP, arena: std.mem.Allocator) !T {
    const fields = @typeInfo(T).@"struct".fields;
    try expectType(sexp, R.VECSXP, error.ExpectedSchema);
    if (R.XLENGTH(sexp) != @as(R.R_xlen_t, @intCast(fields.len))) return error.SchemaLength;
    if (R.R_getAttribCount(sexp) != 1 or !R.R_hasAttrib(sexp, R.R_NamesSymbol)) return error.SchemaAttributes;

    const names = R.Rf_getAttrib(sexp, R.R_NamesSymbol);
    if (!hasType(names, R.STRSXP) or R.XLENGTH(names) != R.XLENGTH(sexp)) {
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

const DirectRealChunkIter = struct {
    data: []const f64,
    n: usize,
    consumed: bool = false,

    fn init(data: []const f64) DirectRealChunkIter {
        return .{ .data = data, .n = data.len };
    }

    fn next(self: *DirectRealChunkIter) ?RealChunk {
        if (self.consumed) return null;
        self.consumed = true;
        return .{ .offset = 0, .data = self.data };
    }

    fn rewind(self: *DirectRealChunkIter) void {
        self.consumed = false;
    }
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
        return initWithRepresentation(sexp, representation);
    }

    fn initWithRepresentation(sexp: SEXP, representation: VectorRepresentation) RealChunkIter {
        return .{
            .sexp = sexp,
            .n = representation.len,
            .direct = directRealSliceOrNull(sexp, representation),
        };
    }

    fn initRegions(sexp: SEXP, representation: VectorRepresentation) RealChunkIter {
        return .{ .sexp = sexp, .n = representation.len, .direct = null };
    }

    fn next(self: *RealChunkIter) ?RealChunk {
        if (self.offset >= self.n) return null;

        if (self.direct) |data| {
            self.offset = self.n;
            return .{ .offset = 0, .data = data };
        }

        const chunk_offset = self.offset;
        const want = @min(self.buf.len, self.n - self.offset);
        const got = checkedRegionCount(
            R.REAL_GET_REGION(self.sexp, @intCast(self.offset), @intCast(want), self.buf[0..].ptr),
            @intCast(want),
        ) catch |err| signalError(err);
        self.offset += got;
        return .{ .offset = chunk_offset, .data = self.buf[0..got] };
    }

    fn rewind(self: *RealChunkIter) void {
        self.offset = 0;
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
        const got = checkedRegionCount(
            R.INTEGER_GET_REGION(self.sexp, @intCast(self.offset), @intCast(want), self.buf[0..].ptr),
            @intCast(want),
        ) catch |err| signalError(err);
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
        const got = checkedRegionCount(
            R.LOGICAL_GET_REGION(self.sexp, @intCast(self.offset), @intCast(want), self.buf[0..].ptr),
            @intCast(want),
        ) catch |err| signalError(err);
        self.offset += got;
        return .{ .offset = chunk_offset, .data = self.buf[0..got] };
    }
};

fn narrowRealSum(total: c_longdouble) f64 {
    const largest_f64: c_longdouble = std.math.floatMax(f64);
    if (total > largest_f64) return std.math.inf(f64);
    if (total < -largest_f64) return -std.math.inf(f64);
    return @floatCast(total);
}

const RealSumAccumulator = struct {
    total: c_longdouble = 0.0,
    na_seen: bool = false,
    nan_seen: bool = false,

    fn add(self: *RealSumAccumulator, values: []const f64, comptime na_rm: bool) void {
        for (values) |value| {
            if (value != value) {
                @branchHint(.unlikely);
                if (!na_rm) {
                    if (R.ISNA(value) != 0) self.na_seen = true else self.nan_seen = true;
                }
                continue;
            }
            self.total += @as(c_longdouble, @floatCast(value));
        }
    }

    fn finish(self: RealSumAccumulator) f64 {
        if (self.na_seen) return R.R_NaReal;
        if (self.nan_seen) return R.R_NaN;
        return narrowRealSum(self.total);
    }
};

noinline fn realSumRegions(sexp: SEXP, representation: VectorRepresentation, comptime na_rm: bool) f64 {
    var accumulator: RealSumAccumulator = .{};
    var iter = RealChunkIter.initRegions(sexp, representation);
    while (iter.next()) |chunk| accumulator.add(chunk.data, na_rm);
    return accumulator.finish();
}

fn realSum(sexp: SEXP, comptime na_rm: bool) f64 {
    const representation = vectorRepresentation(sexp) catch |err| signalError(err);
    if (directRealSliceOrNull(sexp, representation)) |data| {
        var accumulator: RealSumAccumulator = .{};
        accumulator.add(data, na_rm);
        return accumulator.finish();
    }
    return realSumRegions(sexp, representation, na_rm);
}

inline fn addRealMeanCorrection(
    correction: *c_longdouble,
    value: f64,
    result: c_longdouble,
    divisor: c_longdouble,
    finite_total: bool,
) void {
    const extended: c_longdouble = @floatCast(value);
    correction.* += if (finite_total)
        extended - result
    else
        (extended - result) / divisor;
}

inline fn realMeanCorrection(
    correction: *c_longdouble,
    values: []const f64,
    result: c_longdouble,
    divisor: c_longdouble,
    finite_total: bool,
    comptime na_rm: bool,
) void {
    var index: usize = 0;

    if (comptime na_rm) {
        // Classifying pairs keeps extended arithmetic behind the NaN branch without changing order.
        while (index + 2 <= values.len) : (index += 2) {
            const block = values[index..][0..2];
            const all_non_nan = block[0] == block[0] and block[1] == block[1];
            if (all_non_nan) {
                for (block) |value| {
                    addRealMeanCorrection(correction, value, result, divisor, finite_total);
                }
            } else {
                for (block) |value| {
                    if (value == value) {
                        addRealMeanCorrection(correction, value, result, divisor, finite_total);
                    }
                }
            }
        }
    }

    for (values[index..]) |value| {
        if (na_rm and value != value) continue;
        addRealMeanCorrection(correction, value, result, divisor, finite_total);
    }
}

fn realMeanChunks(iter: anytype, comptime na_rm: bool) f64 {
    var total: c_longdouble = 0.0;
    var na_seen = false;
    var nan_seen = false;
    var count = iter.n;
    while (iter.next()) |chunk| {
        for (chunk.data) |value| {
            if (value != value) {
                @branchHint(.unlikely);
                count -= 1;
                if (!na_rm) {
                    if (R.ISNA(value) != 0) na_seen = true else nan_seen = true;
                }
                continue;
            }
            total += @as(c_longdouble, @floatCast(value));
        }
    }

    if (na_seen) return R.R_NaReal;
    if (nan_seen) return R.R_NaN;
    if (count == 0) return R.R_NaN;

    const divisor: c_longdouble = @floatFromInt(count);
    const finite_total = std.math.isFinite(@as(f64, @floatCast(total)));
    var result = if (finite_total) total / divisor else scaled: {
        const scaled_divisor: f64 = @floatFromInt(count);
        var scaled_total: c_longdouble = 0.0;
        iter.rewind();
        while (iter.next()) |chunk| {
            for (chunk.data) |value| {
                if (na_rm and value != value) {
                    @branchHint(.unlikely);
                    continue;
                }
                scaled_total += @as(c_longdouble, @floatCast(value / scaled_divisor));
            }
        }
        break :scaled scaled_total;
    };

    if (std.math.isFinite(@as(f64, @floatCast(result)))) {
        var correction: c_longdouble = 0.0;
        iter.rewind();
        while (iter.next()) |chunk| {
            realMeanCorrection(&correction, chunk.data, result, divisor, finite_total, na_rm);
        }
        result += if (finite_total) correction / divisor else correction;
    }

    return @floatCast(result);
}

noinline fn realMeanRegions(sexp: SEXP, representation: VectorRepresentation, comptime na_rm: bool) f64 {
    var iter = RealChunkIter.initRegions(sexp, representation);
    return realMeanChunks(&iter, na_rm);
}

fn realMean(sexp: SEXP, comptime na_rm: bool) f64 {
    const representation = vectorRepresentation(sexp) catch |err| signalError(err);
    if (directRealSliceOrNull(sexp, representation)) |data| {
        var iter = DirectRealChunkIter.init(data);
        return realMeanChunks(&iter, na_rm);
    }
    return realMeanRegions(sexp, representation, na_rm);
}

pub fn sum(sexp: SEXP) f64 {
    expectType(sexp, R.REALSXP, error.ExpectedReal) catch |err| signalError(err);
    return realSum(sexp, false);
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
    const na_vec: @Vector(lanes, i32) = @splat(R.R_NaInt);
    const lane_offsets: @Vector(lanes, usize) = comptime blk: {
        var seq: [lanes]usize = undefined;
        for (&seq, 0..) |*s, k| s.* = k;
        break :blk @as(@Vector(lanes, usize), seq);
    };

    var initialized = false;
    var best: i32 = 0;
    var best_idx: usize = 0;

    while (iter.next()) |chunk| {
        var seed: usize = 0;
        while (seed < chunk.data.len and chunk.data[seed] == R.R_NaInt) : (seed += 1) {}
        if (seed == chunk.data.len) continue;

        var local_best = chunk.data[seed];
        var local_idx = chunk.offset + seed;
        var base: usize = 0;

        if (chunk.data.len >= lanes) {
            var vec_val: @Vector(lanes, i32) = @splat(local_best);
            var vec_idx: @Vector(lanes, usize) = @splat(local_idx);
            const end = chunk.data.len - (chunk.data.len % lanes);
            while (base < end) : (base += lanes) {
                const values: @Vector(lanes, i32) = chunk.data[base..][0..lanes].*;
                const candidates = @select(i32, values == na_vec, vec_val, values);
                const cmp = if (find_min) candidates < vec_val else candidates > vec_val;
                const base_offset: @Vector(lanes, usize) = @splat(chunk.offset + base);
                const idx = base_offset + lane_offsets;
                vec_val = @select(i32, cmp, candidates, vec_val);
                vec_idx = @select(usize, cmp, idx, vec_idx);
            }

            const vals: [lanes]i32 = vec_val;
            const idxs: [lanes]usize = vec_idx;
            local_best = vals[0];
            local_idx = idxs[0];
            for (1..lanes) |j| {
                const better_value = if (find_min) vals[j] < local_best else vals[j] > local_best;
                const better = better_value or (vals[j] == local_best and idxs[j] < local_idx);
                if (better) {
                    local_best = vals[j];
                    local_idx = idxs[j];
                }
            }
        }

        while (base < chunk.data.len) : (base += 1) {
            const value = chunk.data[base];
            if (value == R.R_NaInt) continue;
            const better = if (find_min) value < local_best else value > local_best;
            if (better) {
                local_best = value;
                local_idx = chunk.offset + base;
            }
        }

        if (!initialized) {
            initialized = true;
            best = local_best;
            best_idx = local_idx;
            continue;
        }

        const better_value = if (find_min) local_best < best else local_best > best;
        const better = better_value or (local_best == best and local_idx < best_idx);
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
    expectType(sexp, R.REALSXP, error.ExpectedReal) catch |err| signalError(err);
    return realMean(sexp, false);
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

const RealMissing = enum {
    none,
    nan,
    na,
};

fn realMissing(value: f64) RealMissing {
    if (R.ISNA(value) != 0) return .na;
    if (R.ISNAN(value)) return .nan;
    return .none;
}

fn combineRealMissing(current: RealMissing, next: RealMissing) RealMissing {
    if (current == .na or next == .na) return .na;
    if (current == .nan or next == .nan) return .nan;
    return .none;
}

fn chunkRealMissing(data: []const f64) RealMissing {
    var missing: RealMissing = .none;
    for (data) |value| missing = combineRealMissing(missing, realMissing(value));
    return missing;
}

pub fn min(sexp: SEXP) f64 {
    expectType(sexp, R.REALSXP, error.ExpectedReal) catch |err| signalError(err);
    const n = xlength(sexp);
    if (n == 0) return std.math.inf(f64);

    const lanes = simd.f64_lanes;
    const inf_vec: @Vector(lanes, f64) = @splat(std.math.inf(f64));
    var value = std.math.inf(f64);
    var missing: RealMissing = .none;
    var iter = RealChunkIter.init(sexp);
    while (iter.next()) |chunk| {
        var i: usize = 0;
        if (chunk.data.len >= lanes) {
            const chunk_missing = chunkRealMissing(chunk.data);
            missing = combineRealMissing(missing, chunk_missing);
            var vec: @Vector(lanes, f64) = inf_vec;
            const end = chunk.data.len - (chunk.data.len % lanes);
            if (chunk_missing != .none) {
                while (i < end) : (i += lanes) {
                    const vals: @Vector(lanes, f64) = chunk.data[i..][0..lanes].*;
                    const clean = @select(f64, vals == vals, vals, inf_vec);
                    vec = @select(f64, clean < vec, clean, vec);
                }
            } else {
                while (i < end) : (i += lanes) {
                    const vals: @Vector(lanes, f64) = chunk.data[i..][0..lanes].*;
                    vec = @select(f64, vals < vec, vals, vec);
                }
            }
            const vec_min = @reduce(.Min, vec);
            if (vec_min < value) value = vec_min;
        }
        while (i < chunk.data.len) : (i += 1) {
            const value_missing = realMissing(chunk.data[i]);
            missing = combineRealMissing(missing, value_missing);
            if (value_missing != .none) continue;
            if (chunk.data[i] < value) value = chunk.data[i];
        }
    }

    return switch (missing) {
        .none => value,
        .nan => R.R_NaN,
        .na => R.R_NaReal,
    };
}

pub fn max(sexp: SEXP) f64 {
    expectType(sexp, R.REALSXP, error.ExpectedReal) catch |err| signalError(err);
    const n = xlength(sexp);
    if (n == 0) return -std.math.inf(f64);

    const lanes = simd.f64_lanes;
    const neg_inf_vec: @Vector(lanes, f64) = @splat(-std.math.inf(f64));
    var value = -std.math.inf(f64);
    var missing: RealMissing = .none;
    var iter = RealChunkIter.init(sexp);
    while (iter.next()) |chunk| {
        var i: usize = 0;
        if (chunk.data.len >= lanes) {
            const chunk_missing = chunkRealMissing(chunk.data);
            missing = combineRealMissing(missing, chunk_missing);
            var vec: @Vector(lanes, f64) = neg_inf_vec;
            const end = chunk.data.len - (chunk.data.len % lanes);
            if (chunk_missing != .none) {
                while (i < end) : (i += lanes) {
                    const vals: @Vector(lanes, f64) = chunk.data[i..][0..lanes].*;
                    const clean = @select(f64, vals == vals, vals, neg_inf_vec);
                    vec = @select(f64, clean > vec, clean, vec);
                }
            } else {
                while (i < end) : (i += lanes) {
                    const vals: @Vector(lanes, f64) = chunk.data[i..][0..lanes].*;
                    vec = @select(f64, vals > vec, vals, vec);
                }
            }
            const vec_max = @reduce(.Max, vec);
            if (vec_max > value) value = vec_max;
        }
        while (i < chunk.data.len) : (i += 1) {
            const value_missing = realMissing(chunk.data[i]);
            missing = combineRealMissing(missing, value_missing);
            if (value_missing != .none) continue;
            if (chunk.data[i] > value) value = chunk.data[i];
        }
    }

    return switch (missing) {
        .none => value,
        .nan => R.R_NaN,
        .na => R.R_NaReal,
    };
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
        var seed: usize = 0;
        while (seed < chunk.data.len and std.math.isNan(chunk.data[seed])) : (seed += 1) {}
        if (seed == chunk.data.len) continue;

        var local_best = chunk.data[seed];
        var local_idx = chunk.offset + seed;
        var base: usize = 0;

        if (chunk.data.len >= lanes) {
            var vec_val: @Vector(lanes, f64) = @splat(local_best);
            var vec_idx: @Vector(lanes, usize) = @splat(local_idx);
            const end = chunk.data.len - (chunk.data.len % lanes);
            while (base < end) : (base += lanes) {
                const values: @Vector(lanes, f64) = chunk.data[base..][0..lanes].*;
                const candidates = @select(f64, values != values, vec_val, values);
                const cmp = if (find_min) candidates < vec_val else candidates > vec_val;
                const base_offset: @Vector(lanes, usize) = @splat(chunk.offset + base);
                const idx = base_offset + lane_offsets;
                vec_val = @select(f64, cmp, candidates, vec_val);
                vec_idx = @select(usize, cmp, idx, vec_idx);
            }
            const vals: [lanes]f64 = vec_val;
            const idxs: [lanes]usize = vec_idx;
            local_best = vals[0];
            local_idx = idxs[0];
            for (1..lanes) |j| {
                const better_value = if (find_min) vals[j] < local_best else vals[j] > local_best;
                const better = better_value or (vals[j] == local_best and idxs[j] < local_idx);
                if (better) {
                    local_best = vals[j];
                    local_idx = idxs[j];
                }
            }
        }

        while (base < chunk.data.len) : (base += 1) {
            const value = chunk.data[base];
            if (std.math.isNan(value)) continue;
            const better = if (find_min) value < local_best else value > local_best;
            if (better) {
                local_best = value;
                local_idx = chunk.offset + base;
            }
        }

        if (!initialized) {
            initialized = true;
            best = local_best;
            best_idx = local_idx;
            continue;
        }

        const better_value = if (find_min) local_best < best else local_best > best;
        const better = better_value or (local_best == best and local_idx < best_idx);
        if (better) {
            best = local_best;
            best_idx = local_idx;
        }
    }

    return if (initialized) @intCast(best_idx) else -1;
}

pub fn argmin(sexp: SEXP) i64 {
    return argminmax(true, sexp);
}

pub fn argmax(sexp: SEXP) i64 {
    return argminmax(false, sexp);
}

pub fn sum_narm(sexp: SEXP) f64 {
    expectType(sexp, R.REALSXP, error.ExpectedReal) catch |err| signalError(err);
    return realSum(sexp, true);
}

pub fn mean_narm(sexp: SEXP) f64 {
    expectType(sexp, R.REALSXP, error.ExpectedReal) catch |err| signalError(err);
    return realMean(sexp, true);
}

fn realPairMin(a: f64, b: f64) f64 {
    if (R.ISNA(a) != 0 or R.ISNA(b) != 0) return R.R_NaReal;
    if (R.ISNAN(a) or R.ISNAN(b)) return R.R_NaN;
    return if (a <= b) a else b;
}

fn realPairMax(a: f64, b: f64) f64 {
    if (R.ISNA(a) != 0 or R.ISNA(b) != 0) return R.R_NaReal;
    if (R.ISNAN(a) or R.ISNAN(b)) return R.R_NaN;
    return if (a >= b) a else b;
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

fn pairwiseRealView(allocator: std.mem.Allocator, sexp: SEXP, comptime retain_cleanup: bool) !RealSliceView {
    if (retain_cleanup) return toRealSliceView(allocator, sexp);
    return toRealSliceViewWithArenaOwner(allocator, sexp);
}

fn pminWithAllocator(a: SEXP, b: SEXP, allocator: std.mem.Allocator, comptime retain_cleanup: bool) SEXP {
    var da_view = pairwiseRealView(allocator, a, retain_cleanup) catch |err| signalError(err);
    defer da_view.deinit();
    var db_view = pairwiseRealView(allocator, b, retain_cleanup) catch |err| signalError(err);
    defer db_view.deinit();
    const da = da_view.constSlice();
    const db = db_view.constSlice();
    const n = if (da.len == 0 or db.len == 0) @as(usize, 0) else @max(da.len, db.len);

    var result = protect.scoped(R.Rf_allocVector(R.REALSXP, @intCast(n)));
    defer result.deinit();
    const rp = R.REAL(result.get());

    for (0..n) |i| {
        rp[i] = realPairMin(da[i % da.len], db[i % db.len]);
    }

    return result.get();
}

/// The allocator must remain valid if an ALTREP input causes R to unwind.
pub fn pminAlloc(a: SEXP, b: SEXP, allocator: std.mem.Allocator) SEXP {
    return pminWithAllocator(a, b, allocator, true);
}

pub fn pmin(a: SEXP, b: SEXP) SEXP {
    var arena = memory.UnwindArena.init();
    defer arena.deinit();
    return pminWithAllocator(a, b, arena.allocator(), false);
}

fn pmaxWithAllocator(a: SEXP, b: SEXP, allocator: std.mem.Allocator, comptime retain_cleanup: bool) SEXP {
    var da_view = pairwiseRealView(allocator, a, retain_cleanup) catch |err| signalError(err);
    defer da_view.deinit();
    var db_view = pairwiseRealView(allocator, b, retain_cleanup) catch |err| signalError(err);
    defer db_view.deinit();
    const da = da_view.constSlice();
    const db = db_view.constSlice();
    const n = if (da.len == 0 or db.len == 0) @as(usize, 0) else @max(da.len, db.len);

    var result = protect.scoped(R.Rf_allocVector(R.REALSXP, @intCast(n)));
    defer result.deinit();
    const rp = R.REAL(result.get());

    for (0..n) |i| {
        rp[i] = realPairMax(da[i % da.len], db[i % db.len]);
    }

    return result.get();
}

/// The allocator must remain valid if an ALTREP input causes R to unwind.
pub fn pmaxAlloc(a: SEXP, b: SEXP, allocator: std.mem.Allocator) SEXP {
    return pmaxWithAllocator(a, b, allocator, true);
}

pub fn pmax(a: SEXP, b: SEXP) SEXP {
    var arena = memory.UnwindArena.init();
    defer arena.deinit();
    return pmaxWithAllocator(a, b, arena.allocator(), false);
}

pub fn cumsum(sexp: SEXP) SEXP {
    expectType(sexp, R.REALSXP, error.ExpectedReal) catch |err| signalError(err);
    const n = xlength(sexp);
    var result = protect.scoped(R.Rf_allocVector(R.REALSXP, @intCast(n)));
    defer result.deinit();
    const rp = R.REAL(result.get());

    var total: f64 = 0.0;
    var missing: RealMissing = .none;
    var idx: usize = 0;
    var iter = RealChunkIter.init(sexp);
    while (iter.next()) |chunk| {
        for (chunk.data) |value| {
            missing = combineRealMissing(missing, realMissing(value));
            if (missing == .none) {
                total += value;
                rp[idx] = total;
            } else {
                rp[idx] = switch (missing) {
                    .none => unreachable,
                    .nan => R.R_NaN,
                    .na => R.R_NaReal,
                };
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

/// Borrows typed contiguous R storage. The caller supplies the matching vector
/// type, keeps `sexp` rooted, and does not retain the pointer past the source R
/// call. This may materialize ALTREP storage.
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

test "checkedRegionCount rejects invalid callback counts" {
    try std.testing.expectEqual(@as(usize, 4), try checkedRegionCount(4, 4));
    try std.testing.expectError(error.AltRepRegionRead, checkedRegionCount(0, 4));
    try std.testing.expectError(error.AltRepRegionRead, checkedRegionCount(-1, 4));
    try std.testing.expectError(error.AltRepRegionRead, checkedRegionCount(5, 4));
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
    try std.testing.expectEqualSlices(u8, errorMessage(error.NullPointer), "SEXP pointer is null");
}

test "errorMessage handles unknown error via @errorName" {
    const msg = errorMessage(error.ExpectedReal);
    try std.testing.expect(msg.len > 0);
}

test "expectType rejects null SEXP" {
    try std.testing.expectError(error.NullPointer, expectType(null, R.REALSXP, error.ExpectedReal));
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
