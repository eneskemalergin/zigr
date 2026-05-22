//! Comptime ALTREP class generation.
//!
//! Creates R ALTREP vectors backed by Zig-managed memory. The generated
//! class registers Length, Elt, Dataptr, and Duplicate methods so R
//! reads elements from the Zig slice without copying.

const std = @import("std");
const R = @import("R");
const cleanup = @import("cleanup");

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

fn altGetRegion(x: R.SEXP, i: R.R_xlen_t, n: R.R_xlen_t, buf: [*c]f64) callconv(.c) R.R_xlen_t {
    const w: *SliceWrap = @ptrCast(@alignCast(R.R_ExternalPtrAddr(R.R_altrep_data1(x)).?));
    const start = @as(usize, @intCast(i));
    if (start >= w.len) return 0;
    const count = @min(@as(usize, @intCast(n)), w.len - start);
    @memcpy(buf[0..count], w.ptr[start..][0..count]);
    return @intCast(count);
}

fn altSum(x: R.SEXP, _: R.Rboolean) callconv(.c) R.SEXP {
    const w: *SliceWrap = @ptrCast(@alignCast(R.R_ExternalPtrAddr(R.R_altrep_data1(x)).?));
    const n = w.len;
    if (n == 0) return R.Rf_ScalarReal(0.0);
    const ptr = w.ptr;
    const V = 8;
    var i: usize = 0;
    var vec: @Vector(V, f64) = @splat(0.0);
    while (i + V <= n) : (i += V) {
        const chunk = @as(@Vector(V, f64), @as(*const [V]f64, @ptrCast(ptr + i)).*);
        vec += chunk;
    }
    var total = @reduce(.Add, vec);
    for (i..n) |j| total += ptr[j];
    return R.Rf_ScalarReal(total);
}

fn altMin(x: R.SEXP, _: R.Rboolean) callconv(.c) R.SEXP {
    const w: *SliceWrap = @ptrCast(@alignCast(R.R_ExternalPtrAddr(R.R_altrep_data1(x)).?));
    const n = w.len;
    if (n == 0) return R.Rf_ScalarReal(std.math.inf(f64));
    const ptr = w.ptr;
    const V = 8;
    var i: usize = 0;
    var vec: @Vector(V, f64) = @splat(std.math.inf(f64));
    while (i + V <= n) : (i += V) {
        const chunk = @as(@Vector(V, f64), @as(*const [V]f64, @ptrCast(ptr + i)).*);
        vec = @select(f64, chunk < vec, chunk, vec);
    }
    var val = @reduce(.Min, vec);
    for (i..n) |j| {
        if (ptr[j] < val) val = ptr[j];
    }
    return R.Rf_ScalarReal(val);
}

fn altMax(x: R.SEXP, _: R.Rboolean) callconv(.c) R.SEXP {
    const w: *SliceWrap = @ptrCast(@alignCast(R.R_ExternalPtrAddr(R.R_altrep_data1(x)).?));
    const n = w.len;
    if (n == 0) return R.Rf_ScalarReal(-std.math.inf(f64));
    const ptr = w.ptr;
    const V = 8;
    var i: usize = 0;
    var vec: @Vector(V, f64) = @splat(-std.math.inf(f64));
    while (i + V <= n) : (i += V) {
        const chunk = @as(@Vector(V, f64), @as(*const [V]f64, @ptrCast(ptr + i)).*);
        vec = @select(f64, chunk > vec, chunk, vec);
    }
    var val = @reduce(.Max, vec);
    for (i..n) |j| {
        if (ptr[j] > val) val = ptr[j];
    }
    return R.Rf_ScalarReal(val);
}

fn altIsSorted(x: R.SEXP) callconv(.c) c_int {
    const w: *SliceWrap = @ptrCast(@alignCast(R.R_ExternalPtrAddr(R.R_altrep_data1(x)).?));
    const n = w.len;
    if (n <= 1) return 1;
    const ptr = w.ptr;
    const V = 8;
    var i: usize = 0;
    while (i + V < n) : (i += V) {
        const a = @as(@Vector(V, f64), @as(*const [V]f64, @ptrCast(ptr + i)).*);
        const b = @as(@Vector(V, f64), @as(*const [V]f64, @ptrCast(ptr + i + 1)).*);
        if (@reduce(.Min, b - a) < 0.0) return 0;
    }
    for (i..n - 1) |j| {
        if (ptr[j] > ptr[j + 1]) return 0;
    }
    return 1;
}

fn altNoNA(_: R.SEXP) callconv(.c) c_int {
    return 1;
}

fn buildClass(comptime pkg: []const u8, comptime name: []const u8, info: anytype) R.R_altrep_class_t {
    const cls = R.R_make_altreal_class(@ptrCast(name.ptr), @ptrCast(pkg.ptr), info);
    R.R_set_altrep_Length_method(cls, altLength);
    R.R_set_altreal_Elt_method(cls, altElt);
    R.R_set_altvec_Dataptr_method(cls, altDataptr);
    R.R_set_altrep_Duplicate_method(cls, altDup);
    R.R_set_altreal_Get_region_method(cls, altGetRegion);
    R.R_set_altreal_Sum_method(cls, altSum);
    R.R_set_altreal_Min_method(cls, altMin);
    R.R_set_altreal_Max_method(cls, altMax);
    R.R_set_altreal_Is_sorted_method(cls, altIsSorted);
    R.R_set_altreal_No_NA_method(cls, altNoNA);
    return cls;
}

pub fn AltReal(comptime pkg: []const u8, comptime name: []const u8) type {
    return struct {
        var class: R.R_altrep_class_t = undefined;
        var registered: bool = false;

        pub fn register(info: anytype) void {
            class = buildClass(pkg, name, info);
            registered = true;
        }

        pub fn init(slice: []const f64) R.SEXP {
            if (!registered) {
                class = buildClass(pkg, name, null);
                registered = true;
            }
            const w = makeWrap(slice);
            // Cleanup: free SliceWrap if any R call after this longjmps.
            // On normal return we pop the frame (SliceWrap lives in R's
            // external pointer finalizer instead).
            const Free = struct {
                fn fire(ptr: ?*anyopaque) void {
                    std.heap.c_allocator.destroy(@as(*SliceWrap, @ptrCast(@alignCast(ptr))));
                }
            };
            cleanup.pushFrame(Free.fire, @as(?*anyopaque, @ptrCast(w)));
            const d1 = R.R_MakeExternalPtr(@as(?*anyopaque, @ptrCast(w)), R.R_NilValue, R.R_NilValue);
            R.R_RegisterCFinalizerEx(d1, freeWrap, 1);
            const result = R.R_new_altrep(class, d1, R.R_NilValue);
            cleanup.popFrame();
            return result;
        }
    };
}
