//! R external-pointer ownership.

const std = @import("std");
const R = @import("R");
const err = @import("error");
const cleanup = @import("cleanup");
const protect = @import("protect.zig");

/// The result is unprotected, so protect it before another R allocation.
pub fn make(ptr: ?*anyopaque, tag_sxp: R.SEXP, prot: R.SEXP) R.SEXP {
    return R.R_MakeExternalPtr(ptr, tag_sxp, prot);
}

pub fn addr(sexp: R.SEXP) ?*anyopaque {
    return R.R_ExternalPtrAddr(sexp);
}

pub fn tag(sexp: R.SEXP) R.SEXP {
    return R.R_ExternalPtrTag(sexp);
}

/// Keep `sexp` protected until registration and ownership transfer finish.
pub fn registerFinalizer(sexp: R.SEXP, comptime cleanupFn: *const fn (R.SEXP) callconv(.c) void) void {
    R.R_RegisterCFinalizerEx(sexp, cleanupFn, 1);
}

/// Native ownership transfers to R only after its finalizer is registered.
pub fn create(comptime T: type, init_val: T, comptime deinitFn: *const fn (*T) void) R.SEXP {
    // Finalizers run after the originating R call, outside R's checked heap.
    const heap_val = std.heap.c_allocator.create(T) catch err.signal("out of memory during external pointer creation");
    heap_val.* = init_val;

    const Owner = struct {
        fn destroy(p: *T) void {
            deinitFn(p);
            std.heap.c_allocator.destroy(p);
        }

        fn fire(raw: ?*anyopaque) void {
            const p: *T = @ptrCast(@alignCast(raw.?));
            destroy(p);
        }

        fn finalizer(s: R.SEXP) callconv(.c) void {
            const raw = R.R_ExternalPtrAddr(s) orelse return;
            // Clear first so re-entry cannot free the allocation twice.
            R.R_ClearExternalPtr(s);
            destroy(@ptrCast(@alignCast(raw)));
        }
    };

    cleanup.pushFrame(Owner.fire, @as(?*anyopaque, @ptrCast(heap_val)));
    const sexp = protect.protect(make(@as(?*anyopaque, @ptrCast(heap_val)), R.R_NilValue, R.R_NilValue));
    defer protect.unprotect();
    registerFinalizer(sexp, Owner.finalizer);
    // R owns the allocation after finalizer registration.
    cleanup.popFrame();
    return sexp;
}
