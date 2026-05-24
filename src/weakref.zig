//! Weak reference wrappers.
//!
//! R weak references associate a value with a key without preventing
//! the key from being garbage-collected. When GC collects the key,
//! the finalizer runs. Use weak references for caches, observers, or
//! any association that should not extend the key's lifetime. For
//! owned native resources (allocations, handles), use externalptr.zig
//! instead: finalizers there are more predictable.

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
