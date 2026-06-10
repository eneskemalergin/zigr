//! External pointer wrappers.
//!
//! R EXTPTRSXP wraps a Zig pointer in an R object with automatic
//! finalization. Use externalptr.create to attach Zig resources
//! to R objects, and externalptr.addr to retrieve them.

const std = @import("std");
const R = @import("R");
const err = @import("error");

/// R_MakeExternalPtr with tag + protected value. The tag identifies the
/// pointer type for safe downcasting; the protected value survives GC.
pub fn make(ptr: ?*anyopaque, tag_sxp: R.SEXP, prot: R.SEXP) R.SEXP {
    return R.R_MakeExternalPtr(ptr, tag_sxp, prot);
}

/// R_ExternalPtrAddr returns null if the pointer was cleared.
pub fn addr(sexp: R.SEXP) ?*anyopaque {
    return R.R_ExternalPtrAddr(sexp);
}

/// R_ExternalPtrTag returns the tag set at creation time.
pub fn tag(sexp: R.SEXP) R.SEXP {
    return R.R_ExternalPtrTag(sexp);
}

/// Registers a finalizer that R calls when the EXTPTRSXP is GC'd.
/// The finalizer receives the EXTPTRSXP (not the raw pointer).
pub fn registerFinalizer(sexp: R.SEXP, comptime cleanupFn: *const fn (R.SEXP) callconv(.c) void) void {
    R.R_RegisterCFinalizerEx(sexp, cleanupFn, 1);
}

/// Wraps a Zig value in an EXTPTRSXP and registers a finalizer that
/// calls `deinitFn` then frees the heap allocation.
pub fn create(comptime T: type, init_val: T, comptime deinitFn: *const fn (*T) void) R.SEXP {
    // c_allocator (not R_chk_*) because GC finalizers run asynchronously
    // and cannot use R_chk_*.
    const heap_val = std.heap.c_allocator.create(T) catch err.signal("out of memory during external pointer creation");
    heap_val.* = init_val;
    const sexp = make(@as(?*anyopaque, @ptrCast(heap_val)), R.R_NilValue, R.R_NilValue);
    const F = struct {
        fn finalizer(s: R.SEXP) callconv(.c) void {
            const raw = R.R_ExternalPtrAddr(s) orelse return;
            const p: *T = @ptrCast(@alignCast(raw));
            deinitFn(p);
            std.heap.c_allocator.destroy(p);
        }
    };
    registerFinalizer(sexp, F.finalizer);
    return sexp;
}
