//! Weak reference wrappers.

const R = @import("R");

/// Create a weak reference. `finalizer` runs when GC collects the key.
/// `onexit` runs the finalizer on R shutdown too.
pub fn make(key_sxp: R.SEXP, val: R.SEXP, comptime finalizer: *const fn (R.SEXP) callconv(.c) void, onexit: bool) R.SEXP {
    return R.R_MakeWeakRefC(key_sxp, val, finalizer, if (onexit) @as(R.Rboolean, 1) else 0);
}

/// Read the key from a weak reference.
pub fn key(weakref: R.SEXP) R.SEXP {
    return R.R_WeakRefKey(weakref);
}

/// Read the value from a weak reference.
pub fn value(weakref: R.SEXP) R.SEXP {
    return R.R_WeakRefValue(weakref);
}
