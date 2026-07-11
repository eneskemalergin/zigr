//! R external-pointer ownership and generated-method receiver checks.
//!
//! Typed pointers keep a per-type tag and protected metadata. Generated
//! methods verify both before they cast the receiver.

const std = @import("std");
const R = @import("R");
const err = @import("error");
const cleanup = @import("cleanup");
const protect = @import("protect.zig");

/// Separates rejected receivers from native pointer casts.
pub const PointerError = error{
    ExpectedExternalPointer,
    WrongExternalPointerTag,
    MissingExternalPointerMetadata,
    ClearedExternalPointer,
    MisalignedExternalPointer,
};

/// Keeps receiver diagnostics identical for `.Call` and `.External` methods.
pub fn errorMessage(err_value: PointerError) []const u8 {
    return switch (err_value) {
        error.ExpectedExternalPointer => "expected EXTPTRSXP receiver",
        error.WrongExternalPointerTag => "external pointer tag does not match method type",
        error.MissingExternalPointerMetadata => "external pointer is missing typed metadata",
        error.ClearedExternalPointer => "external pointer has been cleared",
        error.MisalignedExternalPointer => "external pointer is misaligned for method type",
    };
}

/// Turns receiver validation failures into R errors at an R entry boundary.
pub fn signalPointerError(err_value: PointerError) noreturn {
    err.signal(errorMessage(err_value));
}

/// A permanent R symbol gives generated methods one stable tag per Zig type.
pub fn typeTag(comptime T: type) R.SEXP {
    const Cache = struct {
        const type_key = T;
        var value: R.SEXP = null;
    };
    _ = Cache.type_key;
    if (Cache.value == null) {
        const name = comptime "zigr.externalptr." ++ @typeName(T);
        Cache.value = R.Rf_install(name);
    }
    return Cache.value;
}

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

/// Returns the raw R protected field for low-level external-pointer work.
pub fn protected(sexp: R.SEXP) R.SEXP {
    return R.R_ExternalPtrProtected(sexp);
}

fn makeTypedMetadata(comptime T: type, backing: R.SEXP) R.SEXP {
    const metadata = R.Rf_allocVector(R.VECSXP, 2);
    _ = R.SET_VECTOR_ELT(metadata, 0, typeTag(T));
    _ = R.SET_VECTOR_ELT(metadata, 1, backing);
    return metadata;
}

fn typedMetadata(comptime T: type, sexp: R.SEXP) ?R.SEXP {
    const metadata = R.R_ExternalPtrProtected(sexp);
    if (metadata == null or R.TYPEOF(metadata) != R.VECSXP or R.XLENGTH(metadata) != 2) return null;
    if (R.VECTOR_ELT(metadata, 0) != typeTag(T)) return null;
    return metadata;
}

/// Caller assumes every non-null address refers to a live `T`.
pub fn makeTypedRaw(comptime T: type, ptr: ?*anyopaque, backing: R.SEXP) R.SEXP {
    const held_backing = protect.protect(backing);
    defer protect.unprotect();
    const metadata = protect.protect(makeTypedMetadata(T, held_backing));
    defer protect.unprotect();
    return make(ptr, typeTag(T), metadata);
}

/// The external pointer retains `backing`; callers own the native lifetime.
pub fn makeTyped(comptime T: type, ptr: *T, backing: R.SEXP) R.SEXP {
    return makeTypedRaw(T, @ptrCast(ptr), backing);
}

/// Rejects a receiver before its native address is cast to the method type.
pub fn checkedPointer(comptime T: type, sexp: R.SEXP) PointerError!*T {
    if (sexp == null or R.TYPEOF(sexp) != R.EXTPTRSXP) return error.ExpectedExternalPointer;
    if (R.R_ExternalPtrTag(sexp) != typeTag(T)) return error.WrongExternalPointerTag;
    if (typedMetadata(T, sexp) == null) return error.MissingExternalPointerMetadata;
    const raw = R.R_ExternalPtrAddr(sexp) orelse return error.ClearedExternalPointer;
    if (@intFromPtr(raw) % @alignOf(T) != 0) return error.MisalignedExternalPointer;
    return @ptrCast(@alignCast(raw));
}

/// The returned SEXP borrows from `sexp`, which must remain live across R allocation.
pub fn typedBacking(comptime T: type, sexp: R.SEXP) PointerError!R.SEXP {
    _ = try checkedPointer(T, sexp);
    const metadata = typedMetadata(T, sexp) orelse return error.MissingExternalPointerMetadata;
    return R.VECTOR_ELT(metadata, 1);
}

/// Keep `sexp` protected until registration and ownership transfer finish.
pub fn registerFinalizer(sexp: R.SEXP, comptime cleanupFn: *const fn (R.SEXP) callconv(.c) void) void {
    R.R_RegisterCFinalizerEx(sexp, cleanupFn, 1);
}

/// Caller assumes `sexp` owns a `c_allocator` value created for this finalizer.
pub fn finalizeOwned(comptime T: type, comptime deinitFn: *const fn (*T) void, sexp: R.SEXP) void {
    const raw = R.R_ExternalPtrAddr(sexp) orelse return;
    R.R_ClearExternalPtr(sexp);
    const value: *T = @ptrCast(@alignCast(raw));
    deinitFn(value);
    std.heap.c_allocator.destroy(value);
}

fn createWithMetadata(comptime T: type, init_val: T, comptime deinitFn: *const fn (*T) void, tag_sxp: R.SEXP, prot: R.SEXP) R.SEXP {
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
            // Clear first so re-entry cannot free the allocation twice.
            finalizeOwned(T, deinitFn, s);
        }
    };

    cleanup.pushFrame(Owner.fire, @as(?*anyopaque, @ptrCast(heap_val)));
    const sexp = protect.protect(make(@as(?*anyopaque, @ptrCast(heap_val)), tag_sxp, prot));
    defer protect.unprotect();
    registerFinalizer(sexp, Owner.finalizer);
    // R owns the allocation after finalizer registration.
    cleanup.popFrame();
    return sexp;
}

/// Keeps the raw, untagged constructor for callers that do not expose methods.
pub fn create(comptime T: type, init_val: T, comptime deinitFn: *const fn (*T) void) R.SEXP {
    return createWithMetadata(T, init_val, deinitFn, R.R_NilValue, R.R_NilValue);
}

/// R owns the allocation after registration; `deinitFn` must not call R.
pub fn createTyped(comptime T: type, init_val: T, comptime deinitFn: *const fn (*T) void) R.SEXP {
    const metadata = protect.protect(makeTypedMetadata(T, R.R_NilValue));
    defer protect.unprotect();
    return createWithMetadata(T, init_val, deinitFn, typeTag(T), metadata);
}
