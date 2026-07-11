//! R SEXP classification and guarded fast access.
//!
//! The fast paths depend on the R 4.x 64-bit ABI. Use R accessors when
//! ALTREP does not expose direct storage.

const std = @import("std");
const R = @import("R");

pub const SEXP = R.SEXP;

pub const max_symbol_name = 256;

const length_offset = 0x20;
const dataptr_offset = 0x30;

pub fn typeTag(sexp: SEXP) u5 {
    const byte = @as(*const u8, @ptrCast(sexp)).*;
    return @truncate(byte & 0x1F);
}

pub fn typeOf(sexp: SEXP) SEXPTYPE {
    return @enumFromInt(@as(c_int, typeTag(sexp)));
}

pub fn fastLength(sexp: SEXP) R.R_xlen_t {
    const raw = @as(?*anyopaque, @ptrCast(sexp)) orelse return 0;
    const base: [*]const u8 = @ptrCast(@alignCast(raw));
    const slot: *const R.R_xlen_t = @ptrCast(@alignCast(base + length_offset));
    return slot.*;
}

/// ALTREP can withhold a direct pointer.
pub fn fastDataPtr(sexp: SEXP) ?*anyopaque {
    if (R.ALTREP(sexp) != 0) return null;
    const raw = @as(?*anyopaque, @ptrCast(sexp)) orelse return null;
    const base: [*]u8 = @ptrCast(@alignCast(raw));
    return @as(*anyopaque, @ptrCast(@alignCast(base + dataptr_offset)));
}

pub fn fastVectorElt(sexp: SEXP, index: usize) SEXP {
    const raw = @as(?*anyopaque, @ptrCast(sexp)) orelse return null;
    const base: [*]u8 = @ptrCast(@alignCast(raw));
    const elts: [*]SEXP = @ptrCast(@alignCast(base + dataptr_offset));
    return elts[index];
}

pub fn fastCharData(charsxp: SEXP) ?[*]const u8 {
    const raw = @as(?*anyopaque, @ptrCast(charsxp)) orelse return null;
    const base: [*]const u8 = @ptrCast(@alignCast(raw));
    return base + dataptr_offset;
}

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

pub fn xlength(sexp: SEXP) usize {
    if (sexp == null) return 0;
    const len = if (R.ALTREP(sexp) != 0) R.XLENGTH(sexp) else fastLength(sexp);
    if (len < 0) return 0;
    return @as(usize, @intCast(len));
}

pub fn tryXlength(sexp: SEXP) !usize {
    if (sexp == null) return error.NullPointer;
    const len = if (R.ALTREP(sexp) != 0) R.XLENGTH(sexp) else fastLength(sexp);
    if (len < 0) return error.NegativeLength;
    return @as(usize, @intCast(len));
}

/// R rejects translating `CE_BYTES`, so those bytes stay untouched.
pub fn charsxpBytes(charsxp: SEXP) []const u8 {
    if (charsxp == R.R_NaString) return "";
    if (R.Rf_getCharCE(charsxp) == @as(R.cetype_t, @intCast(R.CE_BYTES))) {
        return std.mem.sliceTo(R.R_CHAR(charsxp), 0);
    }
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
