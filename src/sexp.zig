//! R's SEXP type system and classification helpers.
//!
//! Mirrors R's internal type tags so Zig code can dispatch on SEXPTYPE.
//! Classification helpers wrap R's C API functions from the translated
//! R headers. Callers outside R must not call functions that reach libR.

const std = @import("std");
const R = @import("R");

/// Opaque pointer to an R SEXP. R's GC moves things around, so holding
/// a raw SEXP across an R API call is a bug. Use protect.zig for that.
pub const SEXP = ?*anyopaque;

/// Maps R's internal type tags (from Rinternals.h). Numeric values match
/// what R returns from TYPEOF().
pub const SEXPTYPE = enum(c_int) {
    nil = 0,
    sym = 1,
    list = 2,
    clos = 3,
    env = 4,
    prompt = 5,
    lang = 6,
    special = 7,
    builtin = 8,
    closure = 9,
    code = 10,
    global_env = 11,
    empty_env = 12,
    base_env = 13,
    null = 14,
    pairlist = 15,
    blank = 16,
    int = 17,
    real = 18,
    cplx = 19,
    str = 20,
    dot = 21,
    any = 22,
    vec = 23,
    expr = 24,
    glenv = 25,
    external_ptr = 26,
    weak_ref = 27,
    raw = 28,
    s4 = 29,
    _new = 30,
    _fresh = 31,
    fun = 32,
    _,
};

/// Length type used by R for vector sizes. Signed because R uses -1 for
/// some error states. Cast to usize after validating >= 0.
pub const R_len_t = c_int;

/// Query the SEXPTYPE of a SEXP via R's TYPEOF() macro.
pub fn typeOf(sexp: SEXP) SEXPTYPE {
    return @enumFromInt(R.TYPEOF(@as(R.SEXP, @ptrCast(sexp))));
}

/// True if the SEXP is any vector type (atomic, list, or expression).
pub fn isVector(sexp: SEXP) bool {
    return R.Rf_isVector(@as(R.SEXP, @ptrCast(sexp))) != 0;
}

/// True if the SEXP is a matrix (has dim attribute with 2 elements).
pub fn isMatrix(sexp: SEXP) bool {
    return R.Rf_isMatrix(@as(R.SEXP, @ptrCast(sexp))) != 0;
}

/// True if the SEXP is a factor (INTSXP with class = "factor").
pub fn isFactor(sexp: SEXP) bool {
    return R.Rf_isFactor(@as(R.SEXP, @ptrCast(sexp))) != 0;
}

/// True if the SEXP is a numeric vector (INTSXP, REALSXP, CPLXSXP).
pub fn isNumeric(sexp: SEXP) bool {
    return R.Rf_isNumeric(@as(R.SEXP, @ptrCast(sexp))) != 0;
}

/// True if the SEXP is an integer vector (not factor, not raw).
pub fn isInteger(sexp: SEXP) bool {
    return R.Rf_isInteger(@as(R.SEXP, @ptrCast(sexp))) != 0;
}

/// True if the SEXP is a real (double) vector.
pub fn isReal(sexp: SEXP) bool {
    return R.Rf_isReal(@as(R.SEXP, @ptrCast(sexp))) != 0;
}

/// True if the SEXP is a string vector (STRSXP).
pub fn isString(sexp: SEXP) bool {
    return R.Rf_isString(@as(R.SEXP, @ptrCast(sexp))) != 0;
}

/// True if the SEXP is NULL (R_NilValue).
pub fn isNull(sexp: SEXP) bool {
    return R.Rf_isNull(@as(R.SEXP, @ptrCast(sexp))) != 0;
}

/// True if the SEXP is an environment.
pub fn isEnvironment(sexp: SEXP) bool {
    return R.Rf_isEnvironment(@as(R.SEXP, @ptrCast(sexp))) != 0;
}

/// True if the SEXP is a function (closure, builtin, or special).
pub fn isFunction(sexp: SEXP) bool {
    return R.Rf_isFunction(@as(R.SEXP, @ptrCast(sexp))) != 0;
}

/// True if the SEXP is an S4 object.
pub fn isS4(sexp: SEXP) bool {
    return R.Rf_isS4(@as(R.SEXP, @ptrCast(sexp))) != 0;
}

/// True if the SEXP is a data frame (VECSXP with class = "data.frame").
pub fn isDataFrame(sexp: SEXP) bool {
    return R.Rf_isFrame(@as(R.SEXP, @ptrCast(sexp))) != 0;
}

/// Wraps a CHARSXP (R string scalar). R interned strings are read-only.
pub const StringSexp = extern struct {};

/// Wraps an INTSXP (R integer vector). Missing values are INT_MIN.
pub const IntSexp = extern struct {};

/// Wraps a REALSXP (R numeric vector). Missing values are NA_REAL.
pub const RealSexp = extern struct {};

test "classification helpers compile" {
    // Verify the function signatures are valid. Calls require R runtime.
    try std.testing.expectEqual(@TypeOf(typeOf), fn (SEXP) SEXPTYPE);
}
