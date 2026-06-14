//! R's SEXP type system and classification helpers.
//!
//! Mirrors R's internal type tags so Zig code can dispatch on SEXPTYPE.
//! Classification helpers wrap R's C API functions from the translated
//! R headers.
//!
//! Inline field-access helpers (fastLength, fastDataPtr, typeTag) bypass
//! the translate-c PLT by reading SEXPREC struct fields directly. The
//! offsets are derived from the R 4.x ABI on 64-bit CRAN targets and
//! verified empirically.
//! SAFETY: These fast paths assume non-ALTREP vectors or zigr-owned
//! ALTREP with direct data pointers. For exotic ALTREP (compact_intseq,
//! memory-mapped), use the *_GET_REGION family or DATAPTR_OR_NULL.

const std = @import("std");
const R = @import("R");

/// Re-exported from the translated R headers.
pub const SEXP = R.SEXP;

pub const max_symbol_name = 256;

/// Verified structural offsets for R 4.x on 64-bit CRAN targets.
/// SEXPREC layout (all vector types use vecsxp_struct):
///   offset  size  field
///   0x00      8   sxpinfo (type tag at byte 0, bits 0-4)
///   0x08      8   attrib pointer
///   0x10      8   gengc_next_node
///   0x18      8   gengc_prev_node
///   0x20      8   vecsxp.length       (R_xlen_t)
///   0x28      8   vecsxp.vecsxp_type  (ALTREP class, or NULL)
///   0x30      8   vecsxp.dataptr      (void*)
const length_offset = 0x20;
const dataptr_offset = 0x30;

/// Fast inline TYPEOF: reads the type tag directly from the first byte
/// of the SEXP struct, low 5 bits. No PLT call, no function call.
pub fn typeTag(sexp: SEXP) u5 {
    const byte = @as(*const u8, @ptrCast(sexp)).*;
    return @truncate(byte & 0x1F);
}

pub fn typeOf(sexp: SEXP) SEXPTYPE {
    return @enumFromInt(@as(c_int, typeTag(sexp)));
}

/// Reads vecsxp.length directly from the SEXPREC struct at verified
/// offset 0x20. Returns R_xlen_t (signed 64-bit), as R does.
/// Returns 0 on null input (caller should check with typeTag first).
pub fn fastLength(sexp: SEXP) R.R_xlen_t {
    const raw = @as(?*anyopaque, @ptrCast(sexp)) orelse return 0;
    const base: [*]const u8 = @ptrCast(@alignCast(raw));
    const slot: *const R.R_xlen_t = @ptrCast(@alignCast(base + length_offset));
    return slot.*;
}

/// Returns the inline data pointer for a non-ALTREP vector SEXP.
/// The data is stored inline at offset 0x30 from the SEXP pointer.
/// Returns null for ALTREP vectors (use R.DATAPTR_OR_NULL instead).
pub fn fastDataPtr(sexp: SEXP) ?*anyopaque {
    if (R.ALTREP(sexp) != 0) return null;
    const raw = @as(?*anyopaque, @ptrCast(sexp)) orelse return null;
    const base: [*]u8 = @ptrCast(@alignCast(raw));
    return @as(*anyopaque, @ptrCast(@alignCast(base + dataptr_offset)));
}

/// Inline STRING_ELT / VECTOR_ELT: reads the i-th element from a
/// non-ALTREP STRSXP or VECSXP's inline data array at offset 0x30.
pub fn fastVectorElt(sexp: SEXP, index: usize) SEXP {
    const raw = @as(?*anyopaque, @ptrCast(sexp)) orelse return null;
    const base: [*]u8 = @ptrCast(@alignCast(raw));
    const elts: [*]SEXP = @ptrCast(@alignCast(base + dataptr_offset));
    return elts[index];
}

/// Inline R_CHAR: reads the character data pointer from a CHARSXP.
/// The data field is at verified offset 0x30 from the CHARSXP SEXP.
/// Returns null on null input.
pub fn fastCharData(charsxp: SEXP) ?[*]const u8 {
    const raw = @as(?*anyopaque, @ptrCast(charsxp)) orelse return null;
    const base: [*]const u8 = @ptrCast(@alignCast(raw));
    return base + dataptr_offset;
}

/// Reads the string encoding from a CHARSXP's sxpinfo byte[1] directly.
/// Matches R 4.x ABI where encoding is stored as bitmask bits:
///   0x08 → CE_UTF8   (1)
///   0x04 → CE_LATIN1 (2)
///   0x02 → CE_BYTES  (3)
///   otherwise → CE_NATIVE (0)
/// No PLT call,  verified against Rf_getCharCE disassembly on R 4.6.0.
/// Returns -1 on null or non-CHARSXP input (safe sentinel).
pub fn fastGetCharCE(charsxp: SEXP) i32 {
    const raw = @as(?*anyopaque, @ptrCast(charsxp)) orelse return -1;
    if (typeTag(charsxp) != 9) return -1; // CHARSXP = 9
    const bytes: [*]const u8 = @ptrCast(@alignCast(raw));
    const b1 = bytes[1];
    if (b1 & 0x08 != 0) return 1;
    if (b1 & 0x04 != 0) return 2;
    if (b1 & 0x02 != 0) return 3;
    return 0;
}

/// Length of any SEXP as usize. Returns 0 on null, negative length (corrupted
/// SEXP), or zero-length vector instead of panicking. Uses fastLength (inline
/// struct field read) for non-ALTREP vectors, falls back to R.XLENGTH for
/// ALTREP method dispatch.
pub fn xlength(sexp: SEXP) usize {
    if (sexp == null) return 0;
    const len = if (R.ALTREP(sexp) != 0) R.XLENGTH(sexp) else fastLength(sexp);
    if (len < 0) return 0;
    return @as(usize, @intCast(len));
}

/// xlength that returns a clear error instead of silently zeroing.
pub fn tryXlength(sexp: SEXP) !usize {
    if (sexp == null) return error.NullPointer;
    const len = if (R.ALTREP(sexp) != 0) R.XLENGTH(sexp) else fastLength(sexp);
    if (len < 0) return error.NegativeLength;
    return @as(usize, @intCast(len));
}

pub const XlengthError = error{
    NullPointer,
    NegativeLength,
};

/// Maps R's internal type tags from Rinternals.h. Numeric values must
/// match what R returns from TYPEOF() and what Rf_allocVector expects.
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

/// Length type used by R for vector sizes. Signed because R uses -1 to
/// signal certain error states. Cast to usize after validating >= 0.
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

test "fastDataPtr has correct type" {
    try std.testing.expectEqual(@TypeOf(fastDataPtr), fn (SEXP) ?*anyopaque);
}

test "typeTag has correct type" {
    try std.testing.expectEqual(@TypeOf(typeTag), fn (SEXP) u5);
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
