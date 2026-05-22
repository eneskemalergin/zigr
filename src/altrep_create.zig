//! Comptime ALTREP class generation.
//!
//! Creates R ALTREP vectors backed by Zig-managed memory. The generated
//! class registers Length, Elt, Dataptr, and Duplicate methods so R
//! reads elements from the Zig slice without copying.

const std = @import("std");
const R = @import("R");

const SliceWrap = struct {
    ptr: [*]const f64,
    len: usize,
};

fn makeWrap(slice: []const f64) *SliceWrap {
    const w = std.heap.c_allocator.create(SliceWrap) catch @panic("OOM");
    w.* = .{ .ptr = slice.ptr, .len = slice.len };
    return w;
}

fn freeWrap(sexp: R.SEXP) callconv(.c) void {
    const w: *SliceWrap = @ptrCast(@alignCast(R.R_ExternalPtrAddr(sexp).?));
    std.heap.c_allocator.destroy(w);
}

fn altLength(x: R.SEXP) callconv(.c) R.R_xlen_t {
    const w: *SliceWrap = @ptrCast(@alignCast(R.R_ExternalPtrAddr(R.R_altrep_data1(x)).?));
    return @intCast(w.len);
}

fn altElt(x: R.SEXP, i: R.R_xlen_t) callconv(.c) f64 {
    const w: *SliceWrap = @ptrCast(@alignCast(R.R_ExternalPtrAddr(R.R_altrep_data1(x)).?));
    return w.ptr[@as(usize, @intCast(i))];
}

fn altDataptr(x: R.SEXP, _: R.Rboolean) callconv(.c) ?*anyopaque {
    const w: *SliceWrap = @ptrCast(@alignCast(R.R_ExternalPtrAddr(R.R_altrep_data1(x)).?));
    return @as(?*anyopaque, @ptrCast(@constCast(w.ptr)));
}

fn altDup(x: R.SEXP, _: R.Rboolean) callconv(.c) R.SEXP {
    const w: *SliceWrap = @ptrCast(@alignCast(R.R_ExternalPtrAddr(R.R_altrep_data1(x)).?));
    const dup = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(w.len)));
    defer R.Rf_unprotect(1);
    @memcpy(R.REAL(dup)[0..w.len], w.ptr[0..w.len]);
    return dup;
}

fn buildClass(comptime pkg: []const u8, comptime name: []const u8) R.R_altrep_class_t {
    const cls = R.R_make_altreal_class(@ptrCast(name.ptr), @ptrCast(pkg.ptr), null);
    R.R_set_altrep_Length_method(cls, altLength);
    R.R_set_altreal_Elt_method(cls, altElt);
    R.R_set_altvec_Dataptr_method(cls, altDataptr);
    R.R_set_altrep_Duplicate_method(cls, altDup);
    return cls;
}

pub fn AltReal(comptime pkg: []const u8, comptime name: []const u8) type {
    return struct {
        var class: R.R_altrep_class_t = undefined;
        var initialized: bool = false;

        pub fn init(slice: []const f64) R.SEXP {
            if (!initialized) {
                class = buildClass(pkg, name);
                initialized = true;
            }
            const w = makeWrap(slice);
            const d1 = R.R_MakeExternalPtr(@as(?*anyopaque, @ptrCast(w)), R.R_NilValue, R.R_NilValue);
            R.R_RegisterCFinalizerEx(d1, freeWrap, 1);
            return R.R_new_altrep(class, d1, R.R_NilValue);
        }
    };
}
