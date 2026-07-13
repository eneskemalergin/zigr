//! Comptime ALTREP classes backed by Zig-owned data.
//!
//! R declares `R_ext/Altrep.h` experimental. Keep class registration and
//! callback signatures pinned to the installed headers used for each build.
//!
//! Callback contract for owned vectors:
//! - `Elt` accepts every valid `R_xlen_t` index. Empty vectors have no valid
//!   element index, matching R's accessor contract.
//! - `Get_region` returns zero for non-positive requests and exhausted or
//!   invalid starts, never narrows a long request before bounding it, and
//!   otherwise copies the available prefix.
//! - `Dataptr_or_null` exposes the stable non-empty owned buffer without
//!   materializing and returns null for empty vectors, which have no data.
//! - duplicate returns an independent ordinary vector; R's `DuplicateEX`
//!   wrapper copies attributes according to the requested depth.
//! - summaries use R's empty identities, NA takes precedence over NaN when
//!   `na.rm` is false, and both missing forms are removed when it is true.
//! - sortedness is exact for non-missing real, integer, and logical data and
//!   conservative (`UNKNOWN_SORTEDNESS`) in the presence of NA or NaN.
//! - no-NA methods treat both real NA and NaN as missing. Raw and complex have
//!   no public sorted/no-NA hooks; logical has no public min/max hooks.
//! - R serialization version 3 stores a versioned state record containing an
//!   ordinary vector snapshot. Restoration validates the complete record before
//!   copying native payloads into new ownership. R serialization version 2 uses
//!   R's ordinary-vector fallback and does not preserve the ALTREP class. R's
//!   default `UnserializeEX` path restores attributes and object metadata.
//!   Version 1 is a four-field `VECSXP`: magic string, integer format version,
//!   integer family code, and ordinary typed payload. Family codes are real 1,
//!   integer 2, logical 3, raw 4, complex 5, and string 6.
//!   State validation is a format and ownership check, not a security boundary
//!   for untrusted R serialization streams.

const std = @import("std");
const R = @import("R");
const simd = @import("simd");
const cleanup = @import("cleanup");
const err = @import("error");
const protect = @import("protect.zig");
const sexp_mod = @import("sexp.zig");

pub const ComplexElem = extern struct {
    r: f64,
    i: f64,
};

const AltKind = enum {
    real,
    integer,
    logical,
    raw,
    complex,
};

/// Version of the zigr-owned ALTREP state record embedded in R serialization version 3.
pub const OWNED_ALTREP_STATE_VERSION: c_int = 1;

/// Checked state-generation and restoration failures reported before ownership moves.
pub const SerializedStateError = error{
    NullState,
    ExpectedRecord,
    WrongFieldCount,
    InvalidMagic,
    InvalidVersion,
    WrongKind,
    WrongPayloadType,
    NestedAltrepPayload,
    InvalidLogicalValue,
    WrongClass,
};

const OWNED_ALTREP_STATE_MAGIC = "zigr-owned-altrep";
const STATE_FIELD_COUNT = 4;
const STATE_MAGIC_INDEX = 0;
const STATE_VERSION_INDEX = 1;
const STATE_KIND_INDEX = 2;
const STATE_PAYLOAD_INDEX = 3;

fn kindCode(comptime kind: AltKind) c_int {
    return switch (kind) {
        .real => 1,
        .integer => 2,
        .logical => 3,
        .raw => 4,
        .complex => 5,
    };
}

const STRING_KIND_CODE: c_int = 6;

fn sexpType(comptime kind: AltKind) R.SEXPTYPE {
    return switch (kind) {
        .real => R.REALSXP,
        .integer => R.INTSXP,
        .logical => R.LGLSXP,
        .raw => R.RAWSXP,
        .complex => R.CPLXSXP,
    };
}

fn serializedStateError(error_value: SerializedStateError) noreturn {
    switch (error_value) {
        error.NullState => err.signal("owned ALTREP serialized state is null"),
        error.ExpectedRecord => err.signal("owned ALTREP serialized state must be a VECSXP record"),
        error.WrongFieldCount => err.signal("owned ALTREP serialized state has the wrong field count"),
        error.InvalidMagic => err.signal("owned ALTREP serialized state has an invalid magic value"),
        error.InvalidVersion => err.signal("owned ALTREP serialized state version is unsupported"),
        error.WrongKind => err.signal("owned ALTREP serialized state belongs to a different family"),
        error.WrongPayloadType => err.signal("owned ALTREP serialized payload has the wrong type"),
        error.NestedAltrepPayload => err.signal("owned ALTREP serialized payload must be an ordinary vector"),
        error.InvalidLogicalValue => err.signal("owned ALTREP serialized logical payload contains an invalid value"),
        error.WrongClass => err.signal("serialization input is not an instance of this owned ALTREP class"),
    }
}

fn matchesStateMagic(charsxp: R.SEXP) bool {
    if (charsxp == R.R_NaString or R.TYPEOF(charsxp) != R.CHARSXP) return false;
    const len = R.XLENGTH(charsxp);
    if (len != OWNED_ALTREP_STATE_MAGIC.len) return false;
    return std.mem.eql(
        u8,
        R.R_CHAR(charsxp)[0..@as(usize, @intCast(len))],
        OWNED_ALTREP_STATE_MAGIC,
    );
}

fn validateSerializedState(
    state: R.SEXP,
    expected_kind: c_int,
    expected_type: R.SEXPTYPE,
) SerializedStateError!R.SEXP {
    if (state == null) return error.NullState;
    if (R.TYPEOF(state) != R.VECSXP) return error.ExpectedRecord;
    if (R.ALTREP(state) != 0) return error.ExpectedRecord;
    if (R.XLENGTH(state) != STATE_FIELD_COUNT) return error.WrongFieldCount;

    const magic = R.VECTOR_ELT(state, STATE_MAGIC_INDEX);
    if (R.TYPEOF(magic) != R.STRSXP or R.ALTREP(magic) != 0 or R.XLENGTH(magic) != 1) {
        return error.InvalidMagic;
    }
    const magic_charsxp = R.STRING_ELT(magic, 0);
    if (!matchesStateMagic(magic_charsxp)) return error.InvalidMagic;

    const version = R.VECTOR_ELT(state, STATE_VERSION_INDEX);
    if (R.TYPEOF(version) != R.INTSXP or R.ALTREP(version) != 0 or R.XLENGTH(version) != 1 or
        R.INTEGER_ELT(version, 0) != OWNED_ALTREP_STATE_VERSION)
    {
        return error.InvalidVersion;
    }

    const kind = R.VECTOR_ELT(state, STATE_KIND_INDEX);
    if (R.TYPEOF(kind) != R.INTSXP or R.ALTREP(kind) != 0 or R.XLENGTH(kind) != 1 or
        R.INTEGER_ELT(kind, 0) != expected_kind)
    {
        return error.WrongKind;
    }

    const payload = R.VECTOR_ELT(state, STATE_PAYLOAD_INDEX);
    if (R.TYPEOF(payload) != expected_type) return error.WrongPayloadType;
    if (R.ALTREP(payload) != 0) return error.NestedAltrepPayload;
    if (expected_kind == kindCode(.logical)) {
        for (0..@as(usize, @intCast(R.XLENGTH(payload)))) |index| {
            const value = R.LOGICAL(payload)[index];
            if (value != 0 and value != 1 and value != R.R_NaInt) return error.InvalidLogicalValue;
        }
    }
    return payload;
}

fn makeSerializedState(kind_code: c_int, payload: R.SEXP) R.SEXP {
    var state = protect.scoped(R.Rf_allocVector(R.VECSXP, STATE_FIELD_COUNT));
    defer state.deinit();
    _ = R.SET_VECTOR_ELT(state.get(), STATE_MAGIC_INDEX, R.Rf_mkString(OWNED_ALTREP_STATE_MAGIC));
    _ = R.SET_VECTOR_ELT(state.get(), STATE_VERSION_INDEX, R.Rf_ScalarInteger(OWNED_ALTREP_STATE_VERSION));
    _ = R.SET_VECTOR_ELT(state.get(), STATE_KIND_INDEX, R.Rf_ScalarInteger(kind_code));
    _ = R.SET_VECTOR_ELT(state.get(), STATE_PAYLOAD_INDEX, payload);
    return state.get();
}

fn cString(comptime value: []const u8, comptime label: []const u8) []const u8 {
    if (value.len == 0) @compileError(label ++ " must not be empty");
    if (std.mem.indexOfScalar(u8, value, 0) != null) {
        @compileError(label ++ " must not contain a NUL byte");
    }
    return value ++ "\x00";
}

fn ElemType(comptime kind: AltKind) type {
    return switch (kind) {
        .real => f64,
        .integer, .logical => i32,
        .raw => u8,
        .complex => ComplexElem,
    };
}

fn TagName(comptime kind: AltKind) [:0]const u8 {
    return switch (kind) {
        .real => "zigr_altreal_slice_wrap",
        .integer => "zigr_altinteger_slice_wrap",
        .logical => "zigr_altlogical_slice_wrap",
        .raw => "zigr_altraw_slice_wrap",
        .complex => "zigr_altcomplex_slice_wrap",
    };
}

fn tagSymbol(comptime kind: AltKind) R.SEXP {
    return R.Rf_install(@as([*c]const u8, @ptrCast(TagName(kind).ptr)));
}

fn Wrap(comptime kind: AltKind) type {
    const T = ElemType(kind);
    return struct {
        ptr: [*]const T,
        len: usize,
    };
}

fn PendingWrap(comptime kind: AltKind) type {
    return struct {
        values: ?[]ElemType(kind) = null,
        wrap: ?*Wrap(kind) = null,
        data1: ?R.SEXP = null,

        fn fire(self: *@This()) void {
            if (self.data1) |data1| R.R_ClearExternalPtr(data1);
            if (self.wrap) |wrap| {
                destroyWrap(kind, wrap);
            } else if (self.values) |values| {
                std.heap.c_allocator.free(values);
            }
        }
    };
}

fn makeWrap(comptime kind: AltKind, slice: []const ElemType(kind), pending: *PendingWrap(kind)) *Wrap(kind) {
    const W = Wrap(kind);
    const values = std.heap.c_allocator.dupe(ElemType(kind), slice) catch
        err.signal("out of memory during ALTREP creation");
    pending.values = values;
    const w = std.heap.c_allocator.create(W) catch err.signal("out of memory during ALTREP creation");
    w.* = .{ .ptr = values.ptr, .len = values.len };
    pending.wrap = w;
    pending.values = null;
    return w;
}

fn destroyWrap(comptime kind: AltKind, wrap: *Wrap(kind)) void {
    std.heap.c_allocator.free(wrap.ptr[0..wrap.len]);
    std.heap.c_allocator.destroy(wrap);
}

fn wrapFromData1(comptime kind: AltKind, sexp: R.SEXP) *Wrap(kind) {
    const raw = R.R_ExternalPtrAddr(sexp) orelse @panic("null ALTREP data pointer");
    return @ptrCast(@alignCast(raw));
}

fn wrapFromAltrep(comptime kind: AltKind, x: R.SEXP) *Wrap(kind) {
    return wrapFromData1(kind, R.R_altrep_data1(x));
}

fn freeWrapImpl(comptime kind: AltKind, sexp: R.SEXP) void {
    const raw = R.R_ExternalPtrAddr(sexp) orelse return;
    R.R_ClearExternalPtr(sexp);
    const wrap: *Wrap(kind) = @ptrCast(@alignCast(raw));
    destroyWrap(kind, wrap);
}

fn lengthImpl(comptime kind: AltKind, x: R.SEXP) R.R_xlen_t {
    return @intCast(wrapFromAltrep(kind, x).len);
}

fn dataptrImpl(comptime kind: AltKind, x: R.SEXP, _: R.Rboolean) ?*anyopaque {
    const w = wrapFromAltrep(kind, x);
    return @as(?*anyopaque, @ptrCast(@constCast(w.ptr)));
}

fn dataptrOrNullImpl(comptime kind: AltKind, x: R.SEXP) ?*const anyopaque {
    const w = wrapFromAltrep(kind, x);
    if (w.len == 0) return null;
    return @as(?*const anyopaque, @ptrCast(w.ptr));
}

fn duplicateImpl(comptime kind: AltKind, x: R.SEXP, _: R.Rboolean) R.SEXP {
    const T = ElemType(kind);
    const w = wrapFromAltrep(kind, x);
    const dup = R.Rf_protect(R.Rf_allocVector(sexpType(kind), @intCast(w.len)));
    defer R.Rf_unprotect(1);

    switch (kind) {
        .real => @memcpy(R.REAL(dup)[0..w.len], @as([*]const T, @ptrCast(w.ptr))[0..w.len]),
        .integer => @memcpy(R.INTEGER(dup)[0..w.len], @as([*]const T, @ptrCast(w.ptr))[0..w.len]),
        .logical => @memcpy(R.LOGICAL(dup)[0..w.len], @as([*]const T, @ptrCast(w.ptr))[0..w.len]),
        .raw => @memcpy(R.RAW(dup)[0..w.len], @as([*]const T, @ptrCast(w.ptr))[0..w.len]),
        .complex => {
            if (w.len == 0) return dup;
            const dst: [*]T = @ptrCast(@alignCast(R.COMPLEX(dup) orelse @panic("COMPLEX returned null on freshly allocated CPLXSXP")));
            @memcpy(dst[0..w.len], @as([*]const T, @ptrCast(w.ptr))[0..w.len]);
        },
    }
    return dup;
}

fn serializedStateImpl(comptime kind: AltKind, x: R.SEXP) R.SEXP {
    var payload = protect.scoped(duplicateImpl(kind, x, 0));
    defer payload.deinit();
    return makeSerializedState(kindCode(kind), payload.get());
}

fn serializedStateWithBoundary(comptime kind: AltKind, x: R.SEXP) R.SEXP {
    const Request = struct { value: R.SEXP };
    var request = Request{ .value = x };
    return cleanup.protectCallData(struct {
        fn call(data: ?*anyopaque) R.SEXP {
            const req: *Request = @ptrCast(@alignCast(data.?));
            var input = protect.scoped(req.value);
            defer input.deinit();
            return serializedStateImpl(kind, input.get());
        }
    }.call, @ptrCast(&request));
}

fn payloadSlice(comptime kind: AltKind, payload: R.SEXP) []const ElemType(kind) {
    const len: usize = @intCast(R.XLENGTH(payload));
    if (len == 0) return &.{};
    return switch (kind) {
        .real => R.REAL(payload)[0..len],
        .integer => R.INTEGER(payload)[0..len],
        .logical => R.LOGICAL(payload)[0..len],
        .raw => R.RAW(payload)[0..len],
        .complex => @as([*]const ComplexElem, @ptrCast(@alignCast(
            R.COMPLEX(payload) orelse @panic("COMPLEX returned null for non-empty serialized payload"),
        )))[0..len],
    };
}

const Region = struct {
    start: usize,
    count: usize,
};

fn regionBounds(len: usize, i: R.R_xlen_t, n: R.R_xlen_t) ?Region {
    if (i < 0 or n <= 0) return null;
    const start = std.math.cast(usize, i) orelse return null;
    if (start >= len) return null;

    const available: R.R_xlen_t = @intCast(len - start);
    const count: usize = @intCast(@min(n, available));
    return .{ .start = start, .count = count };
}

fn getRegionImpl(comptime kind: AltKind, x: R.SEXP, i: R.R_xlen_t, n: R.R_xlen_t, buf: [*c]ElemType(kind)) R.R_xlen_t {
    const w = wrapFromAltrep(kind, x);
    const region = regionBounds(w.len, i, n) orelse return 0;
    if (buf == null) return 0;
    @memcpy(buf[0..region.count], w.ptr[region.start..][0..region.count]);
    return @intCast(region.count);
}

fn isSortedNumeric(comptime T: type, slice: []const T) c_int {
    if (slice.len <= 1) return R.SORTED_INCR;
    var increasing = true;
    var decreasing = true;
    for (0..slice.len - 1) |i| {
        if (slice[i] > slice[i + 1]) increasing = false;
        if (slice[i] < slice[i + 1]) decreasing = false;
        if (!increasing and !decreasing) return R.KNOWN_UNSORTED;
    }
    return if (increasing) R.SORTED_INCR else R.SORTED_DECR;
}

fn noNAMethodImpl(comptime kind: AltKind, x: R.SEXP) c_int {
    const w = wrapFromAltrep(kind, x);
    switch (kind) {
        .real => {
            for (w.ptr[0..w.len]) |value| {
                if (R.ISNA(value) != 0 or R.ISNAN(value)) return 0;
            }
            return 1;
        },
        .integer, .logical => {
            for (w.ptr[0..w.len]) |value| {
                if (value == R.R_NaInt) return 0;
            }
            return 1;
        },
        .raw => return 1,
        .complex => {
            for (w.ptr[0..w.len]) |value| {
                if (R.ISNA(value.r) != 0 or R.ISNAN(value.r) or R.ISNA(value.i) != 0 or R.ISNAN(value.i)) return 0;
            }
            return 1;
        },
    }
}

fn isSortedMethodImpl(comptime kind: AltKind, x: R.SEXP) c_int {
    const w = wrapFromAltrep(kind, x);
    if (noNAMethodImpl(kind, x) == 0) return R.UNKNOWN_SORTEDNESS;
    return switch (kind) {
        .real => isSortedNumeric(f64, w.ptr[0..w.len]),
        .integer, .logical => isSortedNumeric(i32, w.ptr[0..w.len]),
        .raw => isSortedNumeric(u8, w.ptr[0..w.len]),
        .complex => R.UNKNOWN_SORTEDNESS,
    };
}

fn realElt(x: R.SEXP, i: R.R_xlen_t) callconv(.c) f64 {
    return wrapFromAltrep(.real, x).ptr[@as(usize, @intCast(i))];
}

fn integerElt(x: R.SEXP, i: R.R_xlen_t) callconv(.c) c_int {
    return wrapFromAltrep(.integer, x).ptr[@as(usize, @intCast(i))];
}

fn logicalElt(x: R.SEXP, i: R.R_xlen_t) callconv(.c) c_int {
    return wrapFromAltrep(.logical, x).ptr[@as(usize, @intCast(i))];
}

fn rawElt(x: R.SEXP, i: R.R_xlen_t) callconv(.c) R.Rbyte {
    return wrapFromAltrep(.raw, x).ptr[@as(usize, @intCast(i))];
}

export fn zigr_altcomplex_elt_parts_impl(x: R.SEXP, i: R.R_xlen_t, real: [*c]f64, imaginary: [*c]f64) callconv(.c) void {
    const value = wrapFromAltrep(.complex, x).ptr[@as(usize, @intCast(i))];
    real.* = value.r;
    imaginary.* = value.i;
}

fn realSum(x: R.SEXP, na_rm: R.Rboolean) callconv(.c) R.SEXP {
    const w = wrapFromAltrep(.real, x);
    var total: c_longdouble = 0.0;
    var nan_seen = false;
    for (w.ptr[0..w.len]) |value| {
        if (R.ISNA(value) != 0) {
            if (na_rm == 0) return R.Rf_ScalarReal(R.NA_REAL());
            continue;
        }
        if (R.ISNAN(value)) {
            if (na_rm == 0) nan_seen = true;
            continue;
        }
        total += @as(c_longdouble, @floatCast(value));
    }
    if (nan_seen) return R.Rf_ScalarReal(std.math.nan(f64));
    return R.Rf_ScalarReal(@floatCast(total));
}

fn integerSum(x: R.SEXP, na_rm: R.Rboolean) callconv(.c) R.SEXP {
    const w = wrapFromAltrep(.integer, x);
    var total: i128 = 0;
    for (w.ptr[0..w.len]) |value| {
        if (value == R.R_NaInt) {
            if (na_rm == 0) return R.Rf_ScalarInteger(R.R_NaInt);
            continue;
        }
        total += value;
    }
    if (total >= std.math.minInt(i32) and total <= std.math.maxInt(i32)) return R.Rf_ScalarInteger(@intCast(total));
    return R.Rf_ScalarReal(@floatFromInt(total));
}

fn logicalSum(x: R.SEXP, na_rm: R.Rboolean) callconv(.c) R.SEXP {
    const w = wrapFromAltrep(.logical, x);
    var total: i64 = 0;
    for (w.ptr[0..w.len]) |value| {
        if (value == R.R_NaInt) {
            if (na_rm == 0) return R.Rf_ScalarLogical(R.R_NaInt);
            continue;
        }
        if (value != 0) total += 1;
    }
    if (total <= std.math.maxInt(i32)) return R.Rf_ScalarInteger(@intCast(total));
    return R.Rf_ScalarReal(@floatFromInt(total));
}

fn warnEmptyMin() void {
    R.Rf_warning("no non-missing arguments to min; returning Inf");
}

fn warnEmptyMax() void {
    R.Rf_warning("no non-missing arguments to max; returning -Inf");
}

fn realMin(x: R.SEXP, na_rm: R.Rboolean) callconv(.c) R.SEXP {
    const w = wrapFromAltrep(.real, x);
    var value = std.math.inf(f64);
    var nan_seen = false;
    var found = false;
    for (w.ptr[0..w.len]) |item| {
        if (R.ISNA(item) != 0) {
            if (na_rm == 0) return R.Rf_ScalarReal(R.NA_REAL());
            continue;
        }
        if (R.ISNAN(item)) {
            if (na_rm == 0) nan_seen = true;
            continue;
        }
        found = true;
        if (item < value) value = item;
    }
    if (nan_seen) return R.Rf_ScalarReal(std.math.nan(f64));
    if (!found) warnEmptyMin();
    return R.Rf_ScalarReal(value);
}

fn realMax(x: R.SEXP, na_rm: R.Rboolean) callconv(.c) R.SEXP {
    const w = wrapFromAltrep(.real, x);
    var value = -std.math.inf(f64);
    var nan_seen = false;
    var found = false;
    for (w.ptr[0..w.len]) |item| {
        if (R.ISNA(item) != 0) {
            if (na_rm == 0) return R.Rf_ScalarReal(R.NA_REAL());
            continue;
        }
        if (R.ISNAN(item)) {
            if (na_rm == 0) nan_seen = true;
            continue;
        }
        found = true;
        if (item > value) value = item;
    }
    if (nan_seen) return R.Rf_ScalarReal(std.math.nan(f64));
    if (!found) warnEmptyMax();
    return R.Rf_ScalarReal(value);
}

fn integerMin(x: R.SEXP, na_rm: R.Rboolean) callconv(.c) R.SEXP {
    const w = wrapFromAltrep(.integer, x);
    const V = simd.i32_lanes;
    var lanes: @Vector(V, i32) = @splat(std.math.maxInt(i32));
    var found = false;
    var i: usize = 0;
    while (w.len - i >= V) : (i += V) {
        const chunk = @as(@Vector(V, i32), @as(*const [V]i32, @ptrCast(w.ptr + i)).*);
        const valid = chunk != @as(@Vector(V, i32), @splat(R.R_NaInt));
        if (!@reduce(.And, valid) and na_rm == 0) return R.Rf_ScalarInteger(R.R_NaInt);
        found = found or @reduce(.Or, valid);
        const clean = @select(i32, valid, chunk, @as(@Vector(V, i32), @splat(std.math.maxInt(i32))));
        lanes = @select(i32, clean < lanes, clean, lanes);
    }
    var value = @reduce(.Min, lanes);
    for (w.ptr[i..w.len]) |item| {
        if (item == R.R_NaInt) {
            if (na_rm == 0) return R.Rf_ScalarInteger(R.R_NaInt);
            continue;
        }
        found = true;
        if (item < value) value = item;
    }
    if (!found) {
        warnEmptyMin();
        return R.Rf_ScalarReal(std.math.inf(f64));
    }
    return R.Rf_ScalarInteger(value);
}

fn integerMax(x: R.SEXP, na_rm: R.Rboolean) callconv(.c) R.SEXP {
    const w = wrapFromAltrep(.integer, x);
    const V = simd.i32_lanes;
    var lanes: @Vector(V, i32) = @splat(std.math.minInt(i32) + 1);
    var found = false;
    var i: usize = 0;
    while (w.len - i >= V) : (i += V) {
        const chunk = @as(@Vector(V, i32), @as(*const [V]i32, @ptrCast(w.ptr + i)).*);
        const valid = chunk != @as(@Vector(V, i32), @splat(R.R_NaInt));
        if (!@reduce(.And, valid) and na_rm == 0) return R.Rf_ScalarInteger(R.R_NaInt);
        found = found or @reduce(.Or, valid);
        const clean = @select(i32, valid, chunk, @as(@Vector(V, i32), @splat(std.math.minInt(i32) + 1)));
        lanes = @select(i32, clean > lanes, clean, lanes);
    }
    var value = @reduce(.Max, lanes);
    for (w.ptr[i..w.len]) |item| {
        if (item == R.R_NaInt) {
            if (na_rm == 0) return R.Rf_ScalarInteger(R.R_NaInt);
            continue;
        }
        found = true;
        if (item > value) value = item;
    }
    if (!found) {
        warnEmptyMax();
        return R.Rf_ScalarReal(-std.math.inf(f64));
    }
    return R.Rf_ScalarInteger(value);
}

fn buildClass(comptime kind: AltKind, comptime pkg: []const u8, comptime name: []const u8, info: anytype) R.R_altrep_class_t {
    const package_name = comptime cString(pkg, "ALTREP package name");
    const class_name = comptime cString(name, "ALTREP class name");
    const cls = switch (kind) {
        .real => R.R_make_altreal_class(@ptrCast(class_name.ptr), @ptrCast(package_name.ptr), info),
        .integer => R.R_make_altinteger_class(@ptrCast(class_name.ptr), @ptrCast(package_name.ptr), info),
        .logical => R.R_make_altlogical_class(@ptrCast(class_name.ptr), @ptrCast(package_name.ptr), info),
        .raw => R.R_make_altraw_class(@ptrCast(class_name.ptr), @ptrCast(package_name.ptr), info),
        .complex => R.R_make_altcomplex_class(@ptrCast(class_name.ptr), @ptrCast(package_name.ptr), info),
    };
    R.R_set_altrep_Length_method(cls, struct {
        fn f(x: R.SEXP) callconv(.c) R.R_xlen_t {
            return lengthImpl(kind, x);
        }
    }.f);
    R.R_set_altvec_Dataptr_method(cls, struct {
        fn f(x: R.SEXP, writable: R.Rboolean) callconv(.c) ?*anyopaque {
            return dataptrImpl(kind, x, writable);
        }
    }.f);
    R.R_set_altvec_Dataptr_or_null_method(cls, struct {
        fn f(x: R.SEXP) callconv(.c) ?*const anyopaque {
            return dataptrOrNullImpl(kind, x);
        }
    }.f);
    R.R_set_altrep_Duplicate_method(cls, struct {
        fn f(x: R.SEXP, deep: R.Rboolean) callconv(.c) R.SEXP {
            return duplicateImpl(kind, x, deep);
        }
    }.f);

    switch (kind) {
        .real => {
            R.R_set_altreal_Elt_method(cls, realElt);
            R.R_set_altreal_Get_region_method(cls, struct {
                fn f(x: R.SEXP, i: R.R_xlen_t, n: R.R_xlen_t, buf: [*c]f64) callconv(.c) R.R_xlen_t {
                    return getRegionImpl(.real, x, i, n, buf);
                }
            }.f);
            R.R_set_altreal_Is_sorted_method(cls, struct {
                fn f(x: R.SEXP) callconv(.c) c_int {
                    return isSortedMethodImpl(.real, x);
                }
            }.f);
            R.R_set_altreal_No_NA_method(cls, struct {
                fn f(x: R.SEXP) callconv(.c) c_int {
                    return noNAMethodImpl(.real, x);
                }
            }.f);
            R.R_set_altreal_Sum_method(cls, realSum);
            R.R_set_altreal_Min_method(cls, realMin);
            R.R_set_altreal_Max_method(cls, realMax);
        },
        .integer => {
            R.R_set_altinteger_Elt_method(cls, integerElt);
            R.R_set_altinteger_Get_region_method(cls, struct {
                fn f(x: R.SEXP, i: R.R_xlen_t, n: R.R_xlen_t, buf: [*c]c_int) callconv(.c) R.R_xlen_t {
                    return getRegionImpl(.integer, x, i, n, @ptrCast(buf));
                }
            }.f);
            R.R_set_altinteger_Is_sorted_method(cls, struct {
                fn f(x: R.SEXP) callconv(.c) c_int {
                    return isSortedMethodImpl(.integer, x);
                }
            }.f);
            R.R_set_altinteger_No_NA_method(cls, struct {
                fn f(x: R.SEXP) callconv(.c) c_int {
                    return noNAMethodImpl(.integer, x);
                }
            }.f);
            R.R_set_altinteger_Sum_method(cls, integerSum);
            R.R_set_altinteger_Min_method(cls, integerMin);
            R.R_set_altinteger_Max_method(cls, integerMax);
        },
        .logical => {
            R.R_set_altlogical_Elt_method(cls, logicalElt);
            R.R_set_altlogical_Get_region_method(cls, struct {
                fn f(x: R.SEXP, i: R.R_xlen_t, n: R.R_xlen_t, buf: [*c]c_int) callconv(.c) R.R_xlen_t {
                    return getRegionImpl(.logical, x, i, n, @ptrCast(buf));
                }
            }.f);
            R.R_set_altlogical_Is_sorted_method(cls, struct {
                fn f(x: R.SEXP) callconv(.c) c_int {
                    return isSortedMethodImpl(.logical, x);
                }
            }.f);
            R.R_set_altlogical_No_NA_method(cls, struct {
                fn f(x: R.SEXP) callconv(.c) c_int {
                    return noNAMethodImpl(.logical, x);
                }
            }.f);
            R.R_set_altlogical_Sum_method(cls, logicalSum);
        },
        .raw => {
            R.R_set_altraw_Elt_method(cls, rawElt);
            R.R_set_altraw_Get_region_method(cls, struct {
                fn f(x: R.SEXP, i: R.R_xlen_t, n: R.R_xlen_t, buf: [*c]R.Rbyte) callconv(.c) R.R_xlen_t {
                    return getRegionImpl(.raw, x, i, n, @ptrCast(buf));
                }
            }.f);
        },
        .complex => {
            R.zigr_set_altcomplex_elt_method(cls);
            R.R_set_altcomplex_Get_region_method(cls, struct {
                fn f(x: R.SEXP, i: R.R_xlen_t, n: R.R_xlen_t, buf: ?*R.Rcomplex) callconv(.c) R.R_xlen_t {
                    if (buf == null) return 0;
                    return getRegionImpl(.complex, x, i, n, @ptrCast(@alignCast(buf.?)));
                }
            }.f);
        },
    }

    return cls;
}

fn OwnedAltVector(comptime kind: AltKind, comptime pkg: []const u8, comptime name: []const u8) type {
    return struct {
        const Self = @This();

        var class: R.R_altrep_class_t = undefined;
        var registered: bool = false;

        fn makeClass(info: anytype) R.R_altrep_class_t {
            const cls = buildClass(kind, pkg, name, info);
            R.R_set_altrep_Serialized_state_method(cls, struct {
                fn f(x: R.SEXP) callconv(.c) R.SEXP {
                    return Self.serializedState(x);
                }
            }.f);
            R.R_set_altrep_Unserialize_method(cls, struct {
                fn f(_: R.SEXP, state: R.SEXP) callconv(.c) R.SEXP {
                    return Self.restoreSerializedState(state);
                }
            }.f);
            return cls;
        }

        /// Registers the class from its installed package initializer.
        ///
        /// Cross-session restoration requires R to load that package by `pkg` and run this call.
        pub fn register(info: anytype) void {
            class = makeClass(info);
            registered = true;
        }

        /// Returns an unprotected version-1 state record with an ordinary vector snapshot.
        /// Wrong-class input is a Zig error; R allocation failures still signal through R.
        pub fn serializedStateChecked(x: R.SEXP) SerializedStateError!R.SEXP {
            if (!registered or x == null or R.ALTREP(x) == 0 or R.R_altrep_inherits(x, class) == 0) {
                return error.WrongClass;
            }
            return serializedStateWithBoundary(kind, x);
        }

        /// Callback-compatible state generation. A foreign object signals an R error.
        pub fn serializedState(x: R.SEXP) R.SEXP {
            return serializedStateChecked(x) catch |error_value|
                serializedStateError(error_value);
        }

        /// Validates `state`, then returns an unprotected ALTREP with an independent native copy.
        ///
        /// Structural failures are Zig errors. Allocation failures still signal through R because
        /// the ALTREP callback ABI cannot propagate a Zig error union.
        pub fn restoreSerializedStateChecked(state: R.SEXP) SerializedStateError!R.SEXP {
            const payload = try validateSerializedState(state, kindCode(kind), sexpType(kind));
            return init(payloadSlice(kind, payload));
        }

        /// Callback-compatible restoration. Invalid state signals an R error before ownership moves.
        pub fn restoreSerializedState(state: R.SEXP) R.SEXP {
            return restoreSerializedStateChecked(state) catch |error_value|
                serializedStateError(error_value);
        }

        /// Copies `slice`; the returned SEXP is unprotected and owns the copy through its finalizer.
        pub fn init(slice: []const ElemType(kind)) R.SEXP {
            if (!sexp_mod.fitsVectorLength(slice.len)) err.signal("ALTREP input exceeds R_XLEN_T_MAX");
            const Request = struct { values: []const ElemType(kind) };
            var request = Request{ .values = slice };
            return cleanup.protectCallData(struct {
                fn call(data: ?*anyopaque) R.SEXP {
                    const req: *Request = @ptrCast(@alignCast(data.?));
                    if (!registered) {
                        class = makeClass(null);
                        registered = true;
                    }

                    const Pending = PendingWrap(kind);
                    const pending = cleanup.pushFrameInline(Pending, .{}, Pending.fire);
                    const w = makeWrap(kind, req.values, pending);
                    const tag = tagSymbol(kind);
                    var d1 = protect.scoped(R.R_MakeExternalPtr(@as(?*anyopaque, @ptrCast(w)), tag, R.R_NilValue));
                    defer d1.deinit();
                    pending.data1 = d1.get();
                    R.R_RegisterCFinalizerEx(d1.get(), struct {
                        fn f(sexp: R.SEXP) callconv(.c) void {
                            freeWrapImpl(kind, sexp);
                        }
                    }.f, 1);
                    const result = R.R_new_altrep(class, d1.get(), R.R_NilValue);
                    cleanup.popFrame();
                    return result;
                }
            }.call, @ptrCast(&request));
        }
    };
}

pub fn AltReal(comptime pkg: []const u8, comptime name: []const u8) type {
    return OwnedAltVector(.real, pkg, name);
}

pub fn AltInteger(comptime pkg: []const u8, comptime name: []const u8) type {
    return OwnedAltVector(.integer, pkg, name);
}

pub fn AltLogical(comptime pkg: []const u8, comptime name: []const u8) type {
    return OwnedAltVector(.logical, pkg, name);
}

pub fn AltRaw(comptime pkg: []const u8, comptime name: []const u8) type {
    return OwnedAltVector(.raw, pkg, name);
}

pub fn AltComplex(comptime pkg: []const u8, comptime name: []const u8) type {
    return OwnedAltVector(.complex, pkg, name);
}

fn stringValues(x: R.SEXP) R.SEXP {
    return R.R_altrep_data1(x);
}

fn stringElt(x: R.SEXP, i: R.R_xlen_t) callconv(.c) R.SEXP {
    return R.STRING_ELT(stringValues(x), i);
}

fn stringIsSorted(x: R.SEXP) callconv(.c) c_int {
    const values = stringValues(x);
    const len = @as(usize, @intCast(R.XLENGTH(values)));
    if (len <= 1 and stringNoNA(x) != 0) return R.SORTED_INCR;
    return R.UNKNOWN_SORTEDNESS;
}

fn stringNoNA(x: R.SEXP) callconv(.c) c_int {
    const values = stringValues(x);
    const len = @as(usize, @intCast(R.XLENGTH(values)));
    for (0..len) |i| {
        if (R.STRING_ELT(values, @intCast(i)) == R.R_NaString) return 0;
    }
    return 1;
}

fn duplicateString(x: R.SEXP, _: R.Rboolean) callconv(.c) R.SEXP {
    return R.Rf_duplicate(stringValues(x));
}

fn copyStringPayload(values: R.SEXP) R.SEXP {
    const len = R.XLENGTH(values);
    var payload = protect.scoped(R.Rf_allocVector(R.STRSXP, len));
    defer payload.deinit();
    for (0..@as(usize, @intCast(len))) |index| {
        R.SET_STRING_ELT(payload.get(), @intCast(index), R.STRING_ELT(values, @intCast(index)));
    }
    return payload.get();
}

fn serializedStringState(x: R.SEXP) R.SEXP {
    var payload = protect.scoped(copyStringPayload(stringValues(x)));
    defer payload.deinit();
    return makeSerializedState(STRING_KIND_CODE, payload.get());
}

fn serializedStringStateWithBoundary(x: R.SEXP) R.SEXP {
    const Request = struct { value: R.SEXP };
    var request = Request{ .value = x };
    return cleanup.protectCallData(struct {
        fn call(data: ?*anyopaque) R.SEXP {
            const req: *Request = @ptrCast(@alignCast(data.?));
            var input = protect.scoped(req.value);
            defer input.deinit();
            return serializedStringState(input.get());
        }
    }.call, @ptrCast(&request));
}

const StringRestoreRequest = struct {
    source: R.SEXP,
    class: R.R_altrep_class_t,
};

fn restoreStringWithBoundary(source: R.SEXP, class: R.R_altrep_class_t) R.SEXP {
    var request = StringRestoreRequest{ .source = source, .class = class };
    return cleanup.protectCallData(struct {
        fn call(data: ?*anyopaque) R.SEXP {
            const req: *StringRestoreRequest = @ptrCast(@alignCast(data.?));
            var input = protect.scoped(req.source);
            defer input.deinit();
            var payload = protect.scoped(copyStringPayload(input.get()));
            defer payload.deinit();
            return R.R_new_altrep(req.class, payload.get(), R.R_NilValue);
        }
    }.call, @ptrCast(&request));
}

pub fn AltString(comptime pkg: []const u8, comptime name: []const u8) type {
    return struct {
        const Self = @This();

        var class: R.R_altrep_class_t = undefined;
        var registered: bool = false;

        fn buildStringClass(info: anytype) R.R_altrep_class_t {
            const package_name = comptime cString(pkg, "ALTREP package name");
            const class_name = comptime cString(name, "ALTREP class name");
            const cls = R.R_make_altstring_class(@ptrCast(class_name.ptr), @ptrCast(package_name.ptr), info);
            R.R_set_altrep_Length_method(cls, struct {
                fn f(x: R.SEXP) callconv(.c) R.R_xlen_t {
                    return R.XLENGTH(stringValues(x));
                }
            }.f);
            R.R_set_altrep_Duplicate_method(cls, duplicateString);
            R.R_set_altstring_Elt_method(cls, stringElt);
            R.R_set_altstring_Is_sorted_method(cls, stringIsSorted);
            R.R_set_altstring_No_NA_method(cls, stringNoNA);
            R.R_set_altrep_Serialized_state_method(cls, struct {
                fn f(x: R.SEXP) callconv(.c) R.SEXP {
                    return Self.serializedState(x);
                }
            }.f);
            R.R_set_altrep_Unserialize_method(cls, struct {
                fn f(_: R.SEXP, state: R.SEXP) callconv(.c) R.SEXP {
                    return Self.restoreSerializedState(state);
                }
            }.f);
            return cls;
        }

        /// Registers the class from its installed package initializer.
        ///
        /// Cross-session restoration requires R to load that package by `pkg` and run this call.
        pub fn register(info: anytype) void {
            class = buildStringClass(info);
            registered = true;
        }

        /// Returns an unprotected version-1 state record with an ordinary STRSXP snapshot.
        /// Wrong-class input is a Zig error; R allocation failures still signal through R.
        pub fn serializedStateChecked(x: R.SEXP) SerializedStateError!R.SEXP {
            if (!registered or x == null or R.ALTREP(x) == 0 or R.R_altrep_inherits(x, class) == 0) {
                return error.WrongClass;
            }
            return serializedStringStateWithBoundary(x);
        }

        /// Callback-compatible state generation. A foreign object signals an R error.
        pub fn serializedState(x: R.SEXP) R.SEXP {
            return serializedStateChecked(x) catch |error_value|
                serializedStateError(error_value);
        }

        /// Validates `state`, then returns an unprotected ALTSTRING with an independent R copy.
        /// Structural failures are Zig errors; R allocation failures still signal through R.
        pub fn restoreSerializedStateChecked(state: R.SEXP) SerializedStateError!R.SEXP {
            const source = try validateSerializedState(state, STRING_KIND_CODE, R.STRSXP);
            if (!registered) {
                class = buildStringClass(null);
                registered = true;
            }
            return restoreStringWithBoundary(source, class);
        }

        /// Callback-compatible restoration. Invalid state signals an R error before ownership moves.
        pub fn restoreSerializedState(state: R.SEXP) R.SEXP {
            return restoreSerializedStateChecked(state) catch |error_value|
                serializedStateError(error_value);
        }

        /// Copies UTF-8 bytes into R strings; the returned SEXP is unprotected.
        pub fn init(slice: []const []const u8) R.SEXP {
            if (!sexp_mod.fitsVectorLength(slice.len)) err.signal("ALTSTRING input exceeds R_XLEN_T_MAX");
            for (slice) |item| {
                if (item.len > std.math.maxInt(c_int)) err.signal("ALTSTRING element exceeds C int length");
            }
            const Request = struct { values: []const []const u8 };
            var request = Request{ .values = slice };
            return cleanup.protectCallData(struct {
                fn call(data: ?*anyopaque) R.SEXP {
                    const req: *Request = @ptrCast(@alignCast(data.?));
                    if (!registered) {
                        class = buildStringClass(null);
                        registered = true;
                    }

                    var values = protect.scoped(R.Rf_allocVector(R.STRSXP, @intCast(req.values.len)));
                    defer values.deinit();
                    for (req.values, 0..) |item, index| {
                        const charsxp = R.Rf_mkCharLenCE(@ptrCast(item.ptr), @intCast(item.len), @as(R.cetype_t, @intCast(R.CE_UTF8)));
                        R.SET_STRING_ELT(values.get(), @intCast(index), charsxp);
                    }
                    return R.R_new_altrep(class, values.get(), R.R_NilValue);
                }
            }.call, @ptrCast(&request));
        }
    };
}

test "ALTREP class identities are explicit C strings" {
    const package_name = comptime cString("zigr"[0..2], "ALTREP package name");
    const class_name = comptime cString("owned", "ALTREP class name");

    try std.testing.expectEqual(@as(u8, 0), package_name[2]);
    try std.testing.expectEqual(@as(u8, 0), class_name[5]);
}

test "ALTREP region bounds preserve long requests and reject invalid ranges" {
    try std.testing.expectEqual(@as(?Region, null), regionBounds(5, -1, 1));
    try std.testing.expectEqual(@as(?Region, null), regionBounds(5, 0, 0));
    try std.testing.expectEqual(@as(?Region, null), regionBounds(5, 5, 1));
    try std.testing.expectEqual(Region{ .start = 2, .count = 3 }, regionBounds(5, 2, 20).?);

    if (comptime @bitSizeOf(usize) >= 64) {
        const long_len: usize = @as(usize, std.math.maxInt(i32)) + 17;
        const start: R.R_xlen_t = std.math.maxInt(i32);

        try std.testing.expectEqual(
            Region{ .start = @intCast(start), .count = 17 },
            regionBounds(long_len, start, R.R_XLEN_T_MAX).?,
        );
    }
}
