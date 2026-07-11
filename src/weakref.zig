//! R weak references.
//!
//! They associate data without extending the key's lifetime.

const R = @import("R");

pub fn make(key_sxp: R.SEXP, val: R.SEXP, comptime finalizer: *const fn (R.SEXP) callconv(.c) void, onexit: bool) R.SEXP {
    return R.R_MakeWeakRefC(key_sxp, val, finalizer, if (onexit) @as(R.Rboolean, 1) else 0);
}

pub fn key(weakref: R.SEXP) R.SEXP {
    return R.R_WeakRefKey(weakref);
}

pub fn value(weakref: R.SEXP) R.SEXP {
    return R.R_WeakRefValue(weakref);
}
