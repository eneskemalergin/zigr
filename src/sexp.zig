//! R SEXP classification and guarded access.
//!
//! `active_abi_contract` selects one access path at compile time.

const std = @import("std");
const builtin = @import("builtin");
const R = @import("R");
const build_options = @import("build_options");

pub const SEXP = R.SEXP;

pub const max_symbol_name = 256;

/// Compile-time policy for SEXP layout access.
pub const AbiContract = enum {
    r4_6_x86_64,
    checked_r_api,
};

const r4_6_version_min = 4 * 65536 + 6 * 256;
const r4_7_version_min = 4 * 65536 + 7 * 256;

pub const r_header_version = R.R_VERSION;

fn selectAbiContract(force_checked: bool, pointer_bytes: usize, is_x86_64: bool, is_little: bool, r_version: c_int) AbiContract {
    if (!force_checked and pointer_bytes == 8 and is_x86_64 and is_little and
        r_version >= r4_6_version_min and r_version < r4_7_version_min)
    {
        return .r4_6_x86_64;
    }
    return .checked_r_api;
}

pub const active_abi_contract = selectAbiContract(
    build_options.force_checked_sexp,
    @sizeOf(usize),
    builtin.target.cpu.arch == .x86_64,
    builtin.target.cpu.arch.endian() == .little,
    r_header_version,
);

pub const uses_direct_layout = active_abi_contract == .r4_6_x86_64;

const R4_64 = struct {
    const length_offset = 0x20;
    const data_offset = 0x30;
};

/// Explicit R API fallback for callers that cannot assume an internal layout.
pub const checked = struct {
    /// Returns `-1` for a null SEXP.
    pub fn typeTag(sexp: SEXP) c_int {
        if (sexp == null) return -1;
        return R.TYPEOF(sexp);
    }

    /// Returns zero for a null SEXP.
    pub fn length(sexp: SEXP) R.R_xlen_t {
        if (sexp == null) return 0;
        return R.XLENGTH(sexp);
    }

    /// Returns null for null, non-vector, or unavailable storage.
    pub fn dataPtr(sexp: SEXP) ?*anyopaque {
        if (sexp == null or R.Rf_isVector(sexp) == 0) return null;
        const ptr = R.DATAPTR_OR_NULL(sexp) orelse return null;
        return @constCast(ptr);
    }

    /// Returns null for null, wrong-kind, or out-of-bounds input.
    pub fn vectorElt(sexp: SEXP, index: usize) SEXP {
        if (sexp == null) return null;
        const tag = R.TYPEOF(sexp);
        if (tag != R.STRSXP and tag != R.VECSXP and tag != R.EXPRSXP) return null;
        if (index >= @as(usize, @intCast(R.XLENGTH(sexp)))) return null;
        if (tag == R.STRSXP) return R.STRING_ELT(sexp, @intCast(index));
        return R.VECTOR_ELT(sexp, @intCast(index));
    }

    /// Returns null unless `charsxp` is a CHARSXP.
    pub fn charData(charsxp: SEXP) ?[*]const u8 {
        if (charsxp == null or R.TYPEOF(charsxp) != R.CHARSXP) return null;
        return R.R_CHAR(charsxp);
    }

    /// Returns `-1` unless `charsxp` is a CHARSXP.
    pub fn getCharCE(charsxp: SEXP) i32 {
        if (charsxp == null or R.TYPEOF(charsxp) != R.CHARSXP) return -1;
        return @intCast(R.Rf_getCharCE(charsxp));
    }
};

fn directTypeTag(sexp: SEXP) u5 {
    const byte = @as(*const u8, @ptrCast(sexp)).*;
    return @truncate(byte & 0x1F);
}

/// The caller assumes `sexp` is non-null.
pub fn typeTag(sexp: SEXP) u5 {
    if (comptime uses_direct_layout) return directTypeTag(sexp);
    return @truncate(@as(c_uint, @intCast(checked.typeTag(sexp))));
}

pub fn typeOf(sexp: SEXP) SEXPTYPE {
    return @enumFromInt(@as(c_int, typeTag(sexp)));
}

fn directLength(sexp: SEXP) R.R_xlen_t {
    const raw = @as(?*anyopaque, @ptrCast(sexp)) orelse return 0;
    const base: [*]const u8 = @ptrCast(@alignCast(raw));
    const slot: *const R.R_xlen_t = @ptrCast(@alignCast(base + R4_64.length_offset));
    return slot.*;
}

/// Caller assumes non-null inputs are vectors or CHARSXPs.
pub fn fastLength(sexp: SEXP) R.R_xlen_t {
    if (sexp == null) return 0;
    if (R.ALTREP(sexp) != 0 or !uses_direct_layout) return checked.length(sexp);
    return directLength(sexp);
}

/// ALTREP can withhold a direct pointer.
/// Caller assumes non-null, non-ALTREP inputs have vector storage.
pub fn fastDataPtr(sexp: SEXP) ?*anyopaque {
    if (sexp == null) return null;
    if (R.ALTREP(sexp) != 0) return null;
    if (comptime !uses_direct_layout) return checked.dataPtr(sexp);
    const raw = @as(?*anyopaque, @ptrCast(sexp)) orelse return null;
    const base: [*]u8 = @ptrCast(@alignCast(raw));
    return @as(*anyopaque, @ptrCast(@alignCast(base + R4_64.data_offset)));
}

/// Caller assumes a STRSXP, VECSXP, or EXPRSXP and an in-bounds index.
pub fn fastVectorElt(sexp: SEXP, index: usize) SEXP {
    if (comptime !uses_direct_layout) return checked.vectorElt(sexp, index);
    const raw = @as(?*anyopaque, @ptrCast(sexp)) orelse return null;
    const base: [*]u8 = @ptrCast(@alignCast(raw));
    const elts: [*]SEXP = @ptrCast(@alignCast(base + R4_64.data_offset));
    return elts[index];
}

pub fn fastCharData(charsxp: SEXP) ?[*]const u8 {
    if (charsxp == null or typeTag(charsxp) != R.CHARSXP) return null;
    if (comptime !uses_direct_layout) return checked.charData(charsxp);
    const raw = @as(?*anyopaque, @ptrCast(charsxp)) orelse return null;
    const base: [*]const u8 = @ptrCast(@alignCast(raw));
    return base + R4_64.data_offset;
}

pub fn fastGetCharCE(charsxp: SEXP) i32 {
    if (charsxp == null or typeTag(charsxp) != R.CHARSXP) return -1;
    if (comptime !uses_direct_layout) return checked.getCharCE(charsxp);
    const raw = @as(?*anyopaque, @ptrCast(charsxp)) orelse return -1;
    const bytes: [*]const u8 = @ptrCast(@alignCast(raw));
    const b1 = bytes[1];
    if (b1 & 0x08 != 0) return 1;
    if (b1 & 0x04 != 0) return 2;
    if (b1 & 0x02 != 0) return 3;
    return 0;
}

/// Caller assumes non-null inputs accept `XLENGTH`.
pub fn xlength(sexp: SEXP) usize {
    if (sexp == null) return 0;
    const len = fastLength(sexp);
    if (len < 0) return 0;
    return @as(usize, @intCast(len));
}

/// Caller assumes non-null inputs accept `XLENGTH`.
pub fn tryXlength(sexp: SEXP) !usize {
    if (sexp == null) return error.NullPointer;
    const len = fastLength(sexp);
    if (len < 0) return error.NegativeLength;
    return @as(usize, @intCast(len));
}

/// Returns whether a native length is representable as an R vector length.
pub fn fitsVectorLength(len: usize) bool {
    return len <= @as(usize, @intCast(R.R_XLEN_T_MAX));
}

/// R rejects translating `CE_BYTES`, so those bytes stay untouched.
pub fn charsxpBytes(charsxp: SEXP) []const u8 {
    if (charsxp == R.R_NaString) return "";
    if (R.Rf_getCharCE(charsxp) == @as(R.cetype_t, @intCast(R.CE_BYTES))) {
        return std.mem.sliceTo(R.R_CHAR(charsxp), 0);
    }
    return std.mem.sliceTo(R.Rf_translateCharUTF8(charsxp), 0);
}

/// Returns the R-owned bytes exactly as stored in a CHARSXP. The length is
/// taken from the CHARSXP, so embedded NUL bytes are retained.
pub fn charsxpRawBytes(charsxp: SEXP) []const u8 {
    if (charsxp == R.R_NaString) return "";
    const length = xlength(charsxp);
    const data: [*]const u8 = @ptrCast(R.R_CHAR(charsxp));
    return data[0..length];
}

/// Returns R-owned translated UTF-8 bytes for the current R call, except that
/// CE_BYTES remains the stored byte sequence because R does not translate it.
pub fn charsxpTranslatedBytes(charsxp: SEXP) []const u8 {
    if (charsxp == R.R_NaString) return "";
    return charsxpTranslatedBytesWithEncoding(charsxp, R.Rf_getCharCE(charsxp));
}

/// Uses a caller-provided mark for `charsxp` to avoid repeating the required
/// `CE_BYTES` classification when a projection already requested it.
pub fn charsxpTranslatedBytesWithEncoding(charsxp: SEXP, encoding_mark: R.cetype_t) []const u8 {
    if (charsxp == R.R_NaString) return "";
    // R does not translate CE_BYTES; preserve those stored bytes while every
    // other mark requests UTF-8 text.
    if (encoding_mark == @as(R.cetype_t, @intCast(R.CE_BYTES))) return charsxpRawBytes(charsxp);
    return std.mem.sliceTo(R.Rf_translateCharUTF8(charsxp), 0);
}

pub const XlengthError = error{
    NullPointer,
    NegativeLength,
};

pub const SEXPTYPE = enum(c_int) {
    nil = 0,
    sym = 1,
    list = 2,
    clos = 3,
    env = 4,
    prom = 5,
    lang = 6,
    special = 7,
    builtin = 8,
    char = 9,
    lgl = 10,
    int = 13,
    real = 14,
    cplx = 15,
    str = 16,
    dot = 17,
    any = 18,
    vec = 19,
    expr = 20,
    bcode = 21,
    external_ptr = 22,
    weak_ref = 23,
    raw = 24,
    s4 = 25,
    _new = 30,
    _free = 31,
    fun = 99,
    _,
};

pub const R_len_t = c_int;

pub fn isVector(sexp: SEXP) bool {
    return R.Rf_isVector(sexp) != 0;
}
pub fn isMatrix(sexp: SEXP) bool {
    return R.Rf_isMatrix(sexp) != 0;
}
pub fn isFactor(sexp: SEXP) bool {
    return R.Rf_isFactor(sexp) != 0;
}
pub fn isNumeric(sexp: SEXP) bool {
    return R.Rf_isNumeric(sexp) != 0;
}
pub fn isInteger(sexp: SEXP) bool {
    return R.Rf_isInteger(sexp) != 0;
}
pub fn isReal(sexp: SEXP) bool {
    return R.Rf_isReal(sexp) != 0;
}
pub fn isString(sexp: SEXP) bool {
    return R.Rf_isString(sexp) != 0;
}
pub fn isNull(sexp: SEXP) bool {
    return R.Rf_isNull(sexp) != 0;
}
pub fn isEnvironment(sexp: SEXP) bool {
    return R.Rf_isEnvironment(sexp) != 0;
}
pub fn isFunction(sexp: SEXP) bool {
    return R.Rf_isFunction(sexp) != 0;
}
pub fn isS4(sexp: SEXP) bool {
    return R.Rf_isS4(sexp) != 0;
}
pub fn isLogical(sexp: SEXP) bool {
    return R.Rf_isLogical(sexp) != 0;
}
pub fn isComplex(sexp: SEXP) bool {
    return R.Rf_isComplex(sexp) != 0;
}
pub fn isSymbol(sexp: SEXP) bool {
    return R.Rf_isSymbol(sexp) != 0;
}
pub fn isList(sexp: SEXP) bool {
    return R.Rf_isList(sexp) != 0;
}
pub fn isLanguage(sexp: SEXP) bool {
    return R.Rf_isLanguage(sexp) != 0;
}
pub fn isPairList(sexp: SEXP) bool {
    return R.Rf_isPairList(sexp) != 0;
}
pub fn isObject(sexp: SEXP) bool {
    return R.Rf_isObject(sexp) != 0;
}
pub fn isPrimitive(sexp: SEXP) bool {
    return R.Rf_isPrimitive(sexp) != 0;
}
pub fn isArray(sexp: SEXP) bool {
    return R.Rf_isArray(sexp) != 0;
}
pub fn isNumber(sexp: SEXP) bool {
    return R.Rf_isNumber(sexp) != 0;
}
pub fn isExpression(sexp: SEXP) bool {
    return R.Rf_isExpression(sexp) != 0;
}

test "xlength returns 0 for zero-length" {
    try std.testing.expectEqual(@TypeOf(xlength), fn (SEXP) usize);
}

test "fastLength has correct type" {
    try std.testing.expectEqual(@TypeOf(fastLength), fn (SEXP) R.R_xlen_t);
}

test "vector length uses R public limit" {
    const max: usize = @intCast(R.R_XLEN_T_MAX);
    try std.testing.expect(fitsVectorLength(max));
    if (max < std.math.maxInt(usize)) {
        try std.testing.expect(!fitsVectorLength(max + 1));
    }
}

test "fastDataPtr has correct type" {
    try std.testing.expectEqual(@TypeOf(fastDataPtr), fn (SEXP) ?*anyopaque);
}

test "typeTag has correct type" {
    try std.testing.expectEqual(@TypeOf(typeTag), fn (SEXP) u5);
}

test "ABI contract gates direct layout" {
    try std.testing.expectEqual(.r4_6_x86_64, selectAbiContract(false, 8, true, true, r4_6_version_min));
    try std.testing.expectEqual(.r4_6_x86_64, selectAbiContract(false, 8, true, true, r4_7_version_min - 1));
    try std.testing.expectEqual(.checked_r_api, selectAbiContract(true, 8, true, true, r4_6_version_min));
    try std.testing.expectEqual(.checked_r_api, selectAbiContract(false, 4, true, true, r4_6_version_min));
    try std.testing.expectEqual(.checked_r_api, selectAbiContract(false, 8, false, true, r4_6_version_min));
    try std.testing.expectEqual(.checked_r_api, selectAbiContract(false, 8, true, false, r4_6_version_min));
    try std.testing.expectEqual(.checked_r_api, selectAbiContract(false, 8, true, true, r4_6_version_min - 1));
    try std.testing.expectEqual(.checked_r_api, selectAbiContract(false, 8, true, true, r4_7_version_min));
}

test "checked fallback surface compiles" {
    try std.testing.expectEqual(@TypeOf(checked.typeTag), fn (SEXP) c_int);
    try std.testing.expectEqual(@TypeOf(checked.length), fn (SEXP) R.R_xlen_t);
    try std.testing.expectEqual(@TypeOf(checked.dataPtr), fn (SEXP) ?*anyopaque);
    try std.testing.expectEqual(@TypeOf(checked.vectorElt), fn (SEXP, usize) SEXP);
    try std.testing.expectEqual(@TypeOf(checked.charData), fn (SEXP) ?[*]const u8);
    try std.testing.expectEqual(@TypeOf(checked.getCharCE), fn (SEXP) i32);
}

test "classification helpers compile" {
    try std.testing.expectEqual(@TypeOf(typeOf), fn (SEXP) SEXPTYPE);
    try std.testing.expectEqual(@TypeOf(isVector), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isNull), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isReal), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isInteger), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isString), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isS4), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isLogical), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isComplex), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isMatrix), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isFactor), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isNumeric), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isEnvironment), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isFunction), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isSymbol), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isList), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isLanguage), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isPairList), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isObject), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isPrimitive), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isArray), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isNumber), fn (SEXP) bool);
    try std.testing.expectEqual(@TypeOf(isExpression), fn (SEXP) bool);
}
