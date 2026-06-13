//! S4 object support.
//!
//! Wraps R's S4 detection and slot access functions.
//! Names are self-explanatory (isS4, getSlot, setSlot, hasSlot).
//!
//! newS4Object provides a fast path that bypasses R's new() eval by
//! directly allocating a VECSXP, setting the S4 bit, and attaching
//! the class attribute.  This is safe for simple representation-based
//! classes that have no initialize() or validity() methods.  For
//! complex S4 classes (inheritance, custom initialize, validity),
//! use R's standard new() via eval.

const std = @import("std");
const R = @import("R");
const symbols = @import("symbols.zig");

fn toSym(name: []const u8) R.SEXP {
    return symbols.install(name);
}

pub fn isS4(sexp: R.SEXP) bool {
    return R.Rf_isS4(sexp) != 0;
}

/// Sets both the S4 bit and the object bit so R recognizes it as S4.
pub fn setS4Object(sexp: R.SEXP, value: bool) void {
    _ = R.Rf_asS4(sexp, if (value) @as(R.Rboolean, 1) else 0, 1);
}

pub fn hasSlot(sexp: R.SEXP, name: []const u8) bool {
    return R.R_has_slot(sexp, toSym(name)) != 0;
}

/// Returns R_NilValue if the slot does not exist (no error signaled).
pub fn getSlot(sexp: R.SEXP, name: []const u8) R.SEXP {
    return R.R_do_slot(sexp, toSym(name));
}

pub fn setSlot(sexp: R.SEXP, name: []const u8, value: R.SEXP) void {
    _ = R.R_do_slot_assign(sexp, toSym(name), value);
}

/// Creates a minimal S4 instance without calling R's `new()` eval.
///
/// Allocates a VECSXP of `slot_count` elements, sets the S4 bit,
/// sets the class to `class_name`.  Slots must be set individually
/// via `setSlot` after creation.
///
/// SAFETY:
/// - The class `class_name` must already be defined via `setClass()`
///   before any slot access (getSlot/setSlot) on the returned object.
/// - If the class has a custom `initialize()` method that sets up
///   derived state, that method is NOT called.  Use R's eval-based
///   `new()` for such classes.
/// - Each VECSXP element is initialized to R_NilValue (safe default).
///
/// Memory: the returned SEXP is PROTECTed once at creation; the
/// caller must UNPROTECT it or pass it to a PROTECT-aware context.
pub fn newS4Object(class_name: []const u8, slot_count: i32) R.SEXP {
    const obj = R.Rf_protect(R.Rf_allocVector(R.VECSXP, slot_count));
    _ = R.Rf_asS4(obj, 1, 1);
    const cls = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    R.SET_STRING_ELT(cls, 0, R.Rf_mkCharLenCE(
        @ptrCast(class_name.ptr),
        @intCast(class_name.len),
        @as(R.cetype_t, @intCast(R.CE_UTF8)),
    ));
    _ = R.Rf_classgets(obj, cls);
    R.Rf_unprotect(2);
    return obj;
}

// TODO: Complex S4 class support
//
// Current newS4Object is optimized for representation-based S4 classes
// with no initialize/validity methods or inheritance chains.  For
// production readiness we should add:
//
// 1. newS4ObjectEx(class_name, slots: []const SlotDef)
//    - Accepts slot definitions (name + type + default value) and
//      sets them at construction time, matching R's new() behavior
//      for simple classes.
//
// 2. Class definition cache
//    - Internal registry that maps class name to def once, eliminating
//      repeated setClass() eval calls (see benchmark task 22 pattern).
//      Thread-safe via std.atomic.Value.
//      Key challenge: the setClass() call signature varies by class
//      (representation vs contains vs slots=), so the cache must
//      store the full call expression, not just a boolean.
//
// 3. initialize() method support
//    - If the class has an initialize() method (detected via
//      R_getClassDef -> R_do_slot(class_def, "initialize")), fall
//      back to R's eval-based new() automatically.
//
// 4. Validity check call
//    - After setting slots, optionally call validObject() via eval
//      to catch invalid slot values early.
//
// 5. Inheritance walk
//    - For extends() classes, walk the parent chain to determine
//      total slot count and accumulate slot definitions from each
//      ancestor.
//
// These additions will close the gap with R's new() while keeping
// the fast path for the common simple-class case.
