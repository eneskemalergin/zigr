//! External pointer wrappers.
//!
//! R EXTPTRSXP wraps a Zig pointer in an R object with automatic
//! finalization. Use externalptr.create to attach Zig resources
//! to R objects, and externalptr.addr to retrieve them.

const std = @import("std");
const R = @import("R");
const err = @import("error");

/// Wrap a Zig pointer as an R external pointer with optional tag and
/// protected value. The tag identifies the pointer type. The protected
/// value is preserved from GC.
pub fn make(ptr: ?*anyopaque, tag_sxp: R.SEXP, prot: R.SEXP) R.SEXP {
    return R.R_MakeExternalPtr(ptr, tag_sxp, prot);
}

/// Get the pointer from an external pointer SEXP.
pub fn addr(sexp: R.SEXP) ?*anyopaque {
    return R.R_ExternalPtrAddr(sexp);
}

/// Get the tag from an external pointer SEXP.
pub fn tag(sexp: R.SEXP) R.SEXP {
    return R.R_ExternalPtrTag(sexp);
}

/// Register a finalizer that R calls when the external pointer is
/// garbage-collected. The finalizer receives the EXTPTRSXP and can
/// extract the pointer via addr().
pub fn registerFinalizer(sexp: R.SEXP, comptime cleanupFn: *const fn (R.SEXP) callconv(.c) void) void {
    R.R_RegisterCFinalizerEx(sexp, cleanupFn, 1);
}

/// Create an external pointer from a Zig type, wrapping the value
/// and registering a finalizer that calls `deinit` on the value.
pub fn create(comptime T: type, init_val: T, comptime deinitFn: *const fn (*T) void) R.SEXP {
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
