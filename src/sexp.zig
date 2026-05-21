//! R's SEXP type system.
//!
//! I mirror R's internal type tags and pointer layout so Zig code can
//! dispatch on SEXPTYPE and manipulate R vectors without the C API getting
//! in the way. Every operation here is unsafe at the boundary - callers
//! must pair with the protection stack in protect.zig.

const std = @import("std");

/// Opaque pointer to an R SEXP. R's GC moves things around, so holding
/// a raw SEXP across an R API call is a bug. Use protect.zig for that.
pub const SEXP = ?*anyopaque;

/// Maps R's internal type tags (from Rinternals.h). The C API uses these
/// to switch on what kind of R object I'm looking at. I keep the same
/// numeric values because R returns them directly.
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

/// Length type used by R for vector sizes. Signed integer because R uses
/// -1 for certain error states. Cast to usize after you validate.
pub const R_len_t = c_int;

/// Storage header for R vector objects. Every R vector starts with this
/// layout in memory. I read the type tag here instead of calling R
/// introspection functions - saves a function call per dispatch.
pub const VECTOR_SEXPREC = extern struct {
    type: SEXPTYPE,
};

/// Wraps a CHARSXP (R string scalar). R interned strings are read-only
/// and reference-counted. Writing through this pointer will segfault.
pub const StringSexp = extern struct {};

/// Wraps an INTSXP (R integer vector). Elements are 32-bit signed ints.
/// Missing values are INT_MIN. Check via `== R_NaInt` (from R headers).
pub const IntSexp = extern struct {};

/// Wraps a REALSXP (R numeric vector). Elements are 64-bit doubles.
/// Missing values are IEEE NaN with a specific payload. R provides
/// ISNA() / ISNAN() for checking.
pub const RealSexp = extern struct {};
