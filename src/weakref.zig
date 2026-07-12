//! Checked R weak-reference construction, access, and finalization.
//!
//! Keys may be `R_NilValue`, environments, external pointers, or bytecode. A
//! live key keeps the value reachable through the weak reference. Before a
//! finalizer runs, R clears both fields and passes the original key to it.
//! Returned SEXPs are unprotected.

const std = @import("std");
const R = @import("R");
const err = @import("error");

pub const WeakRefError = error{
    NullKey,
    NullValue,
    ExpectedReferenceKey,
    ExpectedWeakReference,
};

pub fn errorMessage(error_value: WeakRefError) []const u8 {
    return switch (error_value) {
        error.NullKey => "weak-reference key is null",
        error.NullValue => "weak-reference value is null",
        error.ExpectedReferenceKey => "weak-reference key must be nil, an environment, an external pointer, or bytecode",
        error.ExpectedWeakReference => "expected WEAKREFSXP",
    };
}

fn validateKey(key_sxp: R.SEXP) WeakRefError!void {
    if (key_sxp == null) return error.NullKey;
    const key_type = R.TYPEOF(key_sxp);
    if (key_type != R.NILSXP and key_type != R.ENVSXP and key_type != R.EXTPTRSXP and key_type != R.BCODESXP) {
        return error.ExpectedReferenceKey;
    }
}

fn validateWeakRef(weak_ref: R.SEXP) WeakRefError!void {
    if (weak_ref == null or R.TYPEOF(weak_ref) != R.WEAKREFSXP) {
        return error.ExpectedWeakReference;
    }
}

/// Creates an unprotected weak reference after validating its key and value.
/// The caller must keep both inputs reachable across this allocating call. R
/// may duplicate a referenced value before storing it.
pub fn makeChecked(
    key_sxp: R.SEXP,
    value_sxp: R.SEXP,
    finalizer: R.R_CFinalizer_t,
    on_exit: bool,
) WeakRefError!R.SEXP {
    try validateKey(key_sxp);
    if (value_sxp == null) return error.NullValue;
    const run_on_exit: R.Rboolean = if (on_exit) 1 else 0;
    if (finalizer) |c_finalizer| {
        return R.R_MakeWeakRefC(key_sxp, value_sxp, c_finalizer, run_on_exit);
    }
    return R.R_MakeWeakRef(key_sxp, value_sxp, R.R_NilValue, run_on_exit);
}

/// Creates a weak reference or raises an R error for an invalid key or value.
pub fn make(
    key_sxp: R.SEXP,
    value_sxp: R.SEXP,
    finalizer: R.R_CFinalizer_t,
    on_exit: bool,
) R.SEXP {
    return makeChecked(key_sxp, value_sxp, finalizer, on_exit) catch |error_value|
        err.signal(errorMessage(error_value));
}

/// Returns the borrowed key, or `R_NilValue` after R clears the weak reference.
pub fn keyChecked(weak_ref: R.SEXP) WeakRefError!R.SEXP {
    try validateWeakRef(weak_ref);
    return R.R_WeakRefKey(weak_ref);
}

pub fn key(weak_ref: R.SEXP) R.SEXP {
    return keyChecked(weak_ref) catch |error_value|
        err.signal(errorMessage(error_value));
}

/// Returns the borrowed value, or `R_NilValue` after R clears the weak reference.
pub fn valueChecked(weak_ref: R.SEXP) WeakRefError!R.SEXP {
    try validateWeakRef(weak_ref);
    return R.R_WeakRefValue(weak_ref);
}

pub fn value(weak_ref: R.SEXP) R.SEXP {
    return valueChecked(weak_ref) catch |error_value|
        err.signal(errorMessage(error_value));
}

/// Runs the registered finalizer at most once. R clears the weak-reference
/// fields first and passes the original key. The finalizer must not longjmp,
/// signal an R error, or retain the borrowed key after it returns.
pub fn runFinalizerChecked(weak_ref: R.SEXP) WeakRefError!void {
    try validateWeakRef(weak_ref);
    R.R_RunWeakRefFinalizer(weak_ref);
}

pub fn runFinalizer(weak_ref: R.SEXP) void {
    runFinalizerChecked(weak_ref) catch |error_value|
        err.signal(errorMessage(error_value));
}

test "weak-reference contract types compile" {
    try std.testing.expectEqual(@TypeOf(makeChecked), fn (R.SEXP, R.SEXP, R.R_CFinalizer_t, bool) WeakRefError!R.SEXP);
    try std.testing.expectEqual(@TypeOf(keyChecked), fn (R.SEXP) WeakRefError!R.SEXP);
    try std.testing.expectEqual(@TypeOf(valueChecked), fn (R.SEXP) WeakRefError!R.SEXP);
    try std.testing.expectEqual(@TypeOf(runFinalizerChecked), fn (R.SEXP) WeakRefError!void);
}
