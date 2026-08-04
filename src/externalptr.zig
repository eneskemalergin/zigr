//! R external-pointer ownership and generated-method receiver checks.
//!
//! Typed pointers keep a per-type tag and protected metadata. Generated
//! methods verify both before they cast the receiver. Constructors return
//! unprotected SEXPs; callers keep R inputs reachable across allocating calls.
//! Typed backing is retained as an R object without inspecting ALTREP payload data.
//! Finalizers and pointer access are main-thread-only; finalizer state is
//! cleared before native destruction and is not reentrant.

const std = @import("std");
const R = @import("R");
const err = @import("error");
const cleanup = @import("cleanup");
const protect = @import("protect.zig");

/// Separates rejected receivers from native pointer casts.
pub const PointerError = error{
    NullBacking,
    ExpectedExternalPointer,
    WrongExternalPointerTag,
    MissingExternalPointerMetadata,
    ClearedExternalPointer,
    MisalignedExternalPointer,
};

/// Keeps receiver diagnostics identical for `.Call` and `.External` methods.
pub fn errorMessage(err_value: PointerError) []const u8 {
    return switch (err_value) {
        error.NullBacking => "external pointer backing is null",
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

fn makeTypedMetadata(type_tag: R.SEXP, backing: R.SEXP) R.SEXP {
    const metadata = R.Rf_allocVector(R.VECSXP, 2);
    _ = R.SET_VECTOR_ELT(metadata, 0, type_tag);
    _ = R.SET_VECTOR_ELT(metadata, 1, backing);
    return metadata;
}

fn typedMetadata(comptime T: type, sexp: R.SEXP) ?R.SEXP {
    const metadata = R.R_ExternalPtrProtected(sexp);
    if (metadata == null or R.TYPEOF(metadata) != R.VECSXP or R.XLENGTH(metadata) != 2) return null;
    if (R.VECTOR_ELT(metadata, 0) != typeTag(T)) return null;
    if (R.VECTOR_ELT(metadata, 1) == null) return null;
    return metadata;
}

/// Caller assumes every non-null address refers to a live `T` and keeps
/// `backing` reachable until this allocating call returns. The first use of a
/// type tag may install an R symbol and longjmp, so callers with live native
/// cleanup state must provide an enclosing unwind boundary.
pub fn makeTypedRawChecked(comptime T: type, ptr: ?*anyopaque, backing: R.SEXP) PointerError!R.SEXP {
    if (backing == null) return error.NullBacking;
    const Request = struct {
        ptr: ?*anyopaque,
        backing: R.SEXP,
    };
    var request = Request{ .ptr = ptr, .backing = backing };
    return cleanup.protectCallData(struct {
        fn call(data: ?*anyopaque) R.SEXP {
            const req: *Request = @ptrCast(@alignCast(data.?));
            var held_backing = protect.scoped(req.backing);
            defer held_backing.deinit();
            const type_tag = typeTag(T);
            var metadata = protect.scoped(makeTypedMetadata(type_tag, held_backing.get()));
            defer metadata.deinit();
            return make(req.ptr, type_tag, metadata.get());
        }
    }.call, @ptrCast(&request));
}

/// Raises an R error for invalid backing. The result is unprotected; callers
/// keep `backing` reachable through construction and assume every non-null
/// address refers to a live `T`. Callers with live native cleanup state must
/// provide an enclosing unwind boundary.
pub fn makeTypedRaw(comptime T: type, ptr: ?*anyopaque, backing: R.SEXP) R.SEXP {
    return makeTypedRawChecked(T, ptr, backing) catch |pointer_error|
        signalPointerError(pointer_error);
}

/// The external pointer retains `backing`; callers keep it reachable through
/// construction and own the native lifetime. Callers with live native cleanup
/// state must provide an enclosing unwind boundary.
pub fn makeTyped(comptime T: type, ptr: *T, backing: R.SEXP) R.SEXP {
    return makeTypedRaw(T, @ptrCast(ptr), backing);
}

/// Rejects a receiver before its native address is cast to the method type.
/// The first use of a type tag may install an R symbol and longjmp, so callers
/// with live native cleanup state must provide an enclosing unwind boundary and
/// keep `sexp` protected across that allocation.
pub fn checkedPointer(comptime T: type, sexp: R.SEXP) PointerError!*T {
    if (sexp == null or R.TYPEOF(sexp) != R.EXTPTRSXP) return error.ExpectedExternalPointer;
    if (R.R_ExternalPtrTag(sexp) != typeTag(T)) return error.WrongExternalPointerTag;
    if (typedMetadata(T, sexp) == null) return error.MissingExternalPointerMetadata;
    const raw = R.R_ExternalPtrAddr(sexp) orelse return error.ClearedExternalPointer;
    if (@intFromPtr(raw) % @alignOf(T) != 0) return error.MisalignedExternalPointer;
    return @ptrCast(@alignCast(raw));
}

/// The returned SEXP borrows from `sexp`, which must remain live across R allocation.
/// The caller must keep the receiver protected while using the returned backing.
pub fn typedBacking(comptime T: type, sexp: R.SEXP) PointerError!R.SEXP {
    _ = try checkedPointer(T, sexp);
    const metadata = typedMetadata(T, sexp) orelse return error.MissingExternalPointerMetadata;
    return R.VECTOR_ELT(metadata, 1);
}

/// Keep `sexp` protected until registration and ownership transfer finish.
pub fn registerFinalizer(sexp: R.SEXP, comptime cleanupFn: *const fn (R.SEXP) callconv(.c) void) void {
    R.R_RegisterCFinalizerEx(sexp, cleanupFn, 1);
}

/// Clears the address before deinitializing and freeing the owned value. A
/// repeated call after clearing is a no-op. Caller assumes `sexp` owns a
/// `c_allocator` value created for this finalizer, and `deinitFn` must not R-longjmp.
pub fn finalizeOwned(comptime T: type, comptime deinitFn: *const fn (*T) void, sexp: R.SEXP) void {
    const raw = R.R_ExternalPtrAddr(sexp) orelse return;
    R.R_ClearExternalPtr(sexp);
    const value: *T = @ptrCast(@alignCast(raw));
    deinitFn(value);
    std.heap.c_allocator.destroy(value);
}

fn PendingOwned(comptime T: type, comptime deinitFn: *const fn (*T) void) type {
    return struct {
        value: ?*T = null,

        fn fire(self: *@This()) void {
            if (self.value) |value| {
                deinitFn(value);
                std.heap.c_allocator.destroy(value);
            }
        }
    };
}

fn createOwnedInner(
    comptime T: type,
    init_val: T,
    comptime deinitFn: *const fn (*T) void,
    comptime typed: bool,
) R.SEXP {
    const Owner = struct {
        fn finalizer(s: R.SEXP) callconv(.c) void {
            // Clear first so re-entry cannot free the allocation twice.
            finalizeOwned(T, deinitFn, s);
        }
    };

    var source = init_val;
    const Pending = PendingOwned(T, deinitFn);
    // Keep ownership armed through lazy typed-tag and metadata setup as well
    // as external-pointer creation and finalizer registration.
    const pending = cleanup.pushFrameInline(Pending, .{}, Pending.fire);
    // Finalizers run after the originating R call, outside R's checked heap.
    const heap_val = std.heap.c_allocator.create(T) catch {
        deinitFn(&source);
        err.signal("out of memory during external pointer creation");
    };
    heap_val.* = source;
    pending.value = heap_val;

    const actual_tag = if (comptime typed) typeTag(T) else R.R_NilValue;
    var metadata: protect.ScopedProtect = undefined;
    if (comptime typed) metadata = protect.scoped(makeTypedMetadata(actual_tag, R.R_NilValue));
    defer if (comptime typed) metadata.deinit();

    const actual_prot = if (comptime typed) metadata.get() else R.R_NilValue;
    const sexp = protect.protect(make(@as(?*anyopaque, @ptrCast(heap_val)), actual_tag, actual_prot));
    defer protect.unprotect();
    registerFinalizer(sexp, Owner.finalizer);
    // R owns the allocation after finalizer registration.
    cleanup.popFrame();
    return sexp;
}

/// Keeps the raw, untagged constructor for callers that do not expose methods.
/// Ownership of `init_val` transfers on entry, including when construction raises an R error.
/// Callers with live native cleanup state must provide an enclosing unwind boundary.
pub fn create(comptime T: type, init_val: T, comptime deinitFn: *const fn (*T) void) R.SEXP {
    const Request = struct { value: T };
    var request = Request{ .value = init_val };
    return cleanup.protectCallData(struct {
        fn call(data: ?*anyopaque) R.SEXP {
            const req: *Request = @ptrCast(@alignCast(data.?));
            return createOwnedInner(T, req.value, deinitFn, false);
        }
    }.call, @ptrCast(&request));
}

/// Ownership transfers on entry. R owns the allocation after registration;
/// `deinitFn` must not call R. Callers with live native cleanup state must
/// provide an enclosing unwind boundary.
pub fn createTyped(comptime T: type, init_val: T, comptime deinitFn: *const fn (*T) void) R.SEXP {
    const Request = struct { value: T };
    var request = Request{ .value = init_val };
    return cleanup.protectCallData(struct {
        fn call(data: ?*anyopaque) R.SEXP {
            const req: *Request = @ptrCast(@alignCast(data.?));
            return createOwnedInner(T, req.value, deinitFn, true);
        }
    }.call, @ptrCast(&request));
}
