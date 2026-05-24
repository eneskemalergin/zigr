//! R's SEXP type system and classification helpers.
//!
//! Mirrors R's internal type tags so Zig code can dispatch on SEXPTYPE.
//! Classification helpers wrap R's C API functions from the translated
//! R headers.

const std = @import("std");
const R = @import("R");

/// Re-exported from the translated R headers.
pub const SEXP = R.SEXP;

/// Safe length of any SEXP. Returns the XLENGTH cast to usize, panicking
/// on negative lengths (corrupted SEXP). Use instead of raw @intCast.
pub fn xlength(sexp: SEXP) usize {
    const len = R.XLENGTH(sexp);
    if (len < 0) @panic("negative vector length from corrupted SEXP");
    return @as(usize, @intCast(len));
}

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

pub fn typeOf(sexp: SEXP) SEXPTYPE {
    return @enumFromInt(R.TYPEOF(sexp));
}

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

test "classification helpers compile" {
    try std.testing.expectEqual(@TypeOf(typeOf), fn (SEXP) SEXPTYPE);
}
