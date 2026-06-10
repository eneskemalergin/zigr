//! Comptime ALTREP class generation.
//!
//! Creates R ALTREP vectors backed by Zig-managed memory. The generated
//! classes register Length, Elt, Dataptr, and Duplicate methods so R
//! reads elements from the Zig slice without copying.

const std = @import("std");
const R = @import("R");
const simd = @import("simd");
const cleanup = @import("cleanup");
const err = @import("error");

pub const ComplexElem = extern struct {
    r: f64,
    i: f64,
};

const AltKind = enum {
    real,
    integer,
    logical,
    raw,
    complex,
};

fn ElemType(comptime kind: AltKind) type {
    return switch (kind) {
        .real => f64,
        .integer, .logical => i32,
        .raw => u8,
        .complex => ComplexElem,
    };
}

fn TagName(comptime kind: AltKind) [:0]const u8 {
    return switch (kind) {
        .real => "zigr_altreal_slice_wrap",
        .integer => "zigr_altinteger_slice_wrap",
        .logical => "zigr_altlogical_slice_wrap",
        .raw => "zigr_altraw_slice_wrap",
        .complex => "zigr_altcomplex_slice_wrap",
    };
}

fn tagSymbol(comptime kind: AltKind) R.SEXP {
    return R.Rf_install(@as([*c]const u8, @ptrCast(TagName(kind).ptr)));
}

fn Wrap(comptime kind: AltKind) type {
    const T = ElemType(kind);
    return struct {
        ptr: [*]const T,
        len: usize,
    };
}

fn makeWrap(comptime kind: AltKind, slice: []const ElemType(kind)) *Wrap(kind) {
    const W = Wrap(kind);
    // c_allocator (not R_chk_*) because the Wrap must outlive the
    // .Call invocation, R's finalizer frees it during a later GC.
    const w = std.heap.c_allocator.create(W) catch err.signal("out of memory during ALTREP creation");
    w.* = .{ .ptr = slice.ptr, .len = slice.len };
    return w;
}

fn wrapFromData1(comptime kind: AltKind, sexp: R.SEXP) *Wrap(kind) {
    const raw = R.R_ExternalPtrAddr(sexp) orelse @panic("null ALTREP data pointer");
    return @ptrCast(@alignCast(raw));
}

fn wrapFromAltrep(comptime kind: AltKind, x: R.SEXP) *Wrap(kind) {
    return wrapFromData1(kind, R.R_altrep_data1(x));
}

fn freeWrapImpl(comptime kind: AltKind, sexp: R.SEXP) void {
    std.heap.c_allocator.destroy(wrapFromData1(kind, sexp));
}

fn lengthImpl(comptime kind: AltKind, x: R.SEXP) R.R_xlen_t {
    return @intCast(wrapFromAltrep(kind, x).len);
}

fn dataptrImpl(comptime kind: AltKind, x: R.SEXP, _: R.Rboolean) ?*anyopaque {
    const w = wrapFromAltrep(kind, x);
    // Returns a non-const pointer to the Zig backing slice.  R 4.6's
    // DATAPTR_RW() would add COW checks, but this ALTREP class has
    // exclusive ownership of its backing memory, so no COW needed.
    return @as(?*anyopaque, @ptrCast(@constCast(w.ptr)));
}

fn dataptrOrNullImpl(comptime kind: AltKind, x: R.SEXP) ?*const anyopaque {
    const w = wrapFromAltrep(kind, x);
    return @as(?*const anyopaque, @ptrCast(w.ptr));
}

fn duplicateImpl(comptime kind: AltKind, x: R.SEXP, _: R.Rboolean) R.SEXP {
    const T = ElemType(kind);
    const w = wrapFromAltrep(kind, x);
    const sexp_type = switch (kind) {
        .real => R.REALSXP,
        .integer => R.INTSXP,
        .logical => R.LGLSXP,
        .raw => R.RAWSXP,
        .complex => R.CPLXSXP,
    };
    const dup = R.Rf_protect(R.Rf_allocVector(sexp_type, @intCast(w.len)));
    defer R.Rf_unprotect(1);

    switch (kind) {
        .real => @memcpy(R.REAL(dup)[0..w.len], @as([*]const T, @ptrCast(w.ptr))[0..w.len]),
        .integer => @memcpy(R.INTEGER(dup)[0..w.len], @as([*]const T, @ptrCast(w.ptr))[0..w.len]),
        .logical => @memcpy(R.LOGICAL(dup)[0..w.len], @as([*]const T, @ptrCast(w.ptr))[0..w.len]),
        .raw => @memcpy(R.RAW(dup)[0..w.len], @as([*]const T, @ptrCast(w.ptr))[0..w.len]),
        .complex => {
            const dst: [*]T = @ptrCast(@alignCast(R.COMPLEX(dup) orelse @panic("COMPLEX returned null on freshly allocated CPLXSXP")));
            @memcpy(dst[0..w.len], @as([*]const T, @ptrCast(w.ptr))[0..w.len]);
        },
    }
    return dup;
}

fn getRegionImpl(comptime kind: AltKind, x: R.SEXP, i: R.R_xlen_t, n: R.R_xlen_t, buf: [*c]ElemType(kind)) R.R_xlen_t {
    const w = wrapFromAltrep(kind, x);
    const start = @as(usize, @intCast(i));
    if (start >= w.len) return 0;
    const count = @min(@as(usize, @intCast(n)), w.len - start);
    @memcpy(buf[0..count], w.ptr[start..][0..count]);
    return @intCast(count);
}

fn isSortedNumeric(comptime T: type, slice: []const T) c_int {
    if (slice.len <= 1) return 1;
    for (0..slice.len - 1) |i| {
        if (slice[i] > slice[i + 1]) return 0;
    }
    return 1;
}

fn noNAMethodImpl(comptime kind: AltKind, x: R.SEXP) c_int {
    const w = wrapFromAltrep(kind, x);
    switch (kind) {
        .real => {
            for (w.ptr[0..w.len]) |value| {
                if (R.ISNA(value) != 0 or R.ISNAN(value)) return 0;
            }
            return 1;
        },
        .integer, .logical => {
            for (w.ptr[0..w.len]) |value| {
                if (value == R.R_NaInt) return 0;
            }
            return 1;
        },
        .raw => return 1,
        .complex => {
            for (w.ptr[0..w.len]) |value| {
                if (R.ISNA(value.r) != 0 or R.ISNAN(value.r) or R.ISNA(value.i) != 0 or R.ISNAN(value.i)) return 0;
            }
            return 1;
        },
    }
}

fn isSortedMethodImpl(comptime kind: AltKind, x: R.SEXP) c_int {
    const w = wrapFromAltrep(kind, x);
    return switch (kind) {
        .real => isSortedNumeric(f64, w.ptr[0..w.len]),
        .integer, .logical => isSortedNumeric(i32, w.ptr[0..w.len]),
        .raw => isSortedNumeric(u8, w.ptr[0..w.len]),
        .complex => 0,
    };
}

fn realElt(x: R.SEXP, i: R.R_xlen_t) callconv(.c) f64 {
    return wrapFromAltrep(.real, x).ptr[@as(usize, @intCast(i))];
}

fn integerElt(x: R.SEXP, i: R.R_xlen_t) callconv(.c) c_int {
    return wrapFromAltrep(.integer, x).ptr[@as(usize, @intCast(i))];
}

fn logicalElt(x: R.SEXP, i: R.R_xlen_t) callconv(.c) c_int {
    return wrapFromAltrep(.logical, x).ptr[@as(usize, @intCast(i))];
}

fn rawElt(x: R.SEXP, i: R.R_xlen_t) callconv(.c) R.Rbyte {
    return wrapFromAltrep(.raw, x).ptr[@as(usize, @intCast(i))];
}

fn realSum(x: R.SEXP, _: R.Rboolean) callconv(.c) R.SEXP {
    const w = wrapFromAltrep(.real, x);
    const n = w.len;
    if (n == 0) return R.Rf_ScalarReal(0.0);
    const ptr = w.ptr;
    const V = simd.f64_lanes;
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

fn integerSum(x: R.SEXP, _: R.Rboolean) callconv(.c) R.SEXP {
    const w = wrapFromAltrep(.integer, x);
    var total: i64 = 0;
    for (w.ptr[0..w.len]) |value| total += value;
    return R.Rf_ScalarReal(@floatFromInt(total));
}

fn logicalSum(x: R.SEXP, na_rm: R.Rboolean) callconv(.c) R.SEXP {
    const w = wrapFromAltrep(.logical, x);
    var total: i64 = 0;
    for (w.ptr[0..w.len]) |value| {
        if (value == R.R_NaInt) {
            if (na_rm == 0) return R.Rf_ScalarLogical(R.R_NaInt);
            continue;
        }
        if (value != 0) total += 1;
    }
    return R.Rf_ScalarReal(@floatFromInt(total));
}

fn realMin(x: R.SEXP, _: R.Rboolean) callconv(.c) R.SEXP {
    const w = wrapFromAltrep(.real, x);
    const n = w.len;
    if (n == 0) return R.Rf_ScalarReal(std.math.inf(f64));
    const ptr = w.ptr;
    const V = simd.f64_lanes;
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

fn realMax(x: R.SEXP, _: R.Rboolean) callconv(.c) R.SEXP {
    const w = wrapFromAltrep(.real, x);
    const n = w.len;
    if (n == 0) return R.Rf_ScalarReal(-std.math.inf(f64));
    const ptr = w.ptr;
    const V = simd.f64_lanes;
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

fn integerMin(x: R.SEXP, _: R.Rboolean) callconv(.c) R.SEXP {
    const w = wrapFromAltrep(.integer, x);
    const n = w.len;
    if (n == 0) return R.Rf_ScalarInteger(std.math.maxInt(i32));
    const ptr = w.ptr;
    const V = simd.i32_lanes;
    var i: usize = 0;
    var vec: @Vector(V, i32) = @splat(std.math.maxInt(i32));
    while (i + V <= n) : (i += V) {
        const chunk = @as(@Vector(V, i32), @as(*const [V]i32, @ptrCast(ptr + i)).*);
        vec = @select(i32, chunk < vec, chunk, vec);
    }
    var val = @reduce(.Min, vec);
    for (i..n) |j| {
        if (ptr[j] < val) val = ptr[j];
    }
    return R.Rf_ScalarInteger(val);
}

fn integerMax(x: R.SEXP, _: R.Rboolean) callconv(.c) R.SEXP {
    const w = wrapFromAltrep(.integer, x);
    const n = w.len;
    if (n == 0) return R.Rf_ScalarInteger(std.math.minInt(i32));
    const ptr = w.ptr;
    const V = simd.i32_lanes;
    var i: usize = 0;
    var vec: @Vector(V, i32) = @splat(std.math.minInt(i32));
    while (i + V <= n) : (i += V) {
        const chunk = @as(@Vector(V, i32), @as(*const [V]i32, @ptrCast(ptr + i)).*);
        vec = @select(i32, chunk > vec, chunk, vec);
    }
    var val = @reduce(.Max, vec);
    for (i..n) |j| {
        if (ptr[j] > val) val = ptr[j];
    }
    return R.Rf_ScalarInteger(val);
}

fn buildClass(comptime kind: AltKind, comptime pkg: []const u8, comptime name: []const u8, info: anytype) R.R_altrep_class_t {
    const cls = switch (kind) {
        .real => R.R_make_altreal_class(@ptrCast(name.ptr), @ptrCast(pkg.ptr), info),
        .integer => R.R_make_altinteger_class(@ptrCast(name.ptr), @ptrCast(pkg.ptr), info),
        .logical => R.R_make_altlogical_class(@ptrCast(name.ptr), @ptrCast(pkg.ptr), info),
        .raw => R.R_make_altraw_class(@ptrCast(name.ptr), @ptrCast(pkg.ptr), info),
        .complex => R.R_make_altcomplex_class(@ptrCast(name.ptr), @ptrCast(pkg.ptr), info),
    };
    R.R_set_altrep_Length_method(cls, struct {
        fn f(x: R.SEXP) callconv(.c) R.R_xlen_t {
            return lengthImpl(kind, x);
        }
    }.f);
    R.R_set_altvec_Dataptr_method(cls, struct {
        fn f(x: R.SEXP, writable: R.Rboolean) callconv(.c) ?*anyopaque {
            return dataptrImpl(kind, x, writable);
        }
    }.f);
    R.R_set_altvec_Dataptr_or_null_method(cls, struct {
        fn f(x: R.SEXP) callconv(.c) ?*const anyopaque {
            return dataptrOrNullImpl(kind, x);
        }
    }.f);
    R.R_set_altrep_Duplicate_method(cls, struct {
        fn f(x: R.SEXP, deep: R.Rboolean) callconv(.c) R.SEXP {
            return duplicateImpl(kind, x, deep);
        }
    }.f);

    switch (kind) {
        .real => {
            R.R_set_altreal_Elt_method(cls, realElt);
            R.R_set_altreal_Get_region_method(cls, struct {
                fn f(x: R.SEXP, i: R.R_xlen_t, n: R.R_xlen_t, buf: [*c]f64) callconv(.c) R.R_xlen_t {
                    return getRegionImpl(.real, x, i, n, buf);
                }
            }.f);
            R.R_set_altreal_Is_sorted_method(cls, struct {
                fn f(x: R.SEXP) callconv(.c) c_int {
                    return isSortedMethodImpl(.real, x);
                }
            }.f);
            R.R_set_altreal_No_NA_method(cls, struct {
                fn f(x: R.SEXP) callconv(.c) c_int {
                    return noNAMethodImpl(.real, x);
                }
            }.f);
            R.R_set_altreal_Sum_method(cls, realSum);
            R.R_set_altreal_Min_method(cls, realMin);
            R.R_set_altreal_Max_method(cls, realMax);
        },
        .integer => {
            R.R_set_altinteger_Elt_method(cls, integerElt);
            R.R_set_altinteger_Get_region_method(cls, struct {
                fn f(x: R.SEXP, i: R.R_xlen_t, n: R.R_xlen_t, buf: [*c]c_int) callconv(.c) R.R_xlen_t {
                    return getRegionImpl(.integer, x, i, n, @ptrCast(buf));
                }
            }.f);
            R.R_set_altinteger_Is_sorted_method(cls, struct {
                fn f(x: R.SEXP) callconv(.c) c_int {
                    return isSortedMethodImpl(.integer, x);
                }
            }.f);
            R.R_set_altinteger_No_NA_method(cls, struct {
                fn f(x: R.SEXP) callconv(.c) c_int {
                    return noNAMethodImpl(.integer, x);
                }
            }.f);
            R.R_set_altinteger_Sum_method(cls, integerSum);
            R.R_set_altinteger_Min_method(cls, integerMin);
            R.R_set_altinteger_Max_method(cls, integerMax);
        },
        .logical => {
            R.R_set_altlogical_Elt_method(cls, logicalElt);
            R.R_set_altlogical_Get_region_method(cls, struct {
                fn f(x: R.SEXP, i: R.R_xlen_t, n: R.R_xlen_t, buf: [*c]c_int) callconv(.c) R.R_xlen_t {
                    return getRegionImpl(.logical, x, i, n, @ptrCast(buf));
                }
            }.f);
            R.R_set_altlogical_Is_sorted_method(cls, struct {
                fn f(x: R.SEXP) callconv(.c) c_int {
                    return isSortedMethodImpl(.logical, x);
                }
            }.f);
            R.R_set_altlogical_No_NA_method(cls, struct {
                fn f(x: R.SEXP) callconv(.c) c_int {
                    return noNAMethodImpl(.logical, x);
                }
            }.f);
            R.R_set_altlogical_Sum_method(cls, logicalSum);
        },
        .raw => {
            R.R_set_altraw_Elt_method(cls, rawElt);
            R.R_set_altraw_Get_region_method(cls, struct {
                fn f(x: R.SEXP, i: R.R_xlen_t, n: R.R_xlen_t, buf: [*c]R.Rbyte) callconv(.c) R.R_xlen_t {
                    return getRegionImpl(.raw, x, i, n, @ptrCast(buf));
                }
            }.f);
        },
        .complex => {
            R.R_set_altcomplex_Get_region_method(cls, struct {
                fn f(x: R.SEXP, i: R.R_xlen_t, n: R.R_xlen_t, buf: ?*R.Rcomplex) callconv(.c) R.R_xlen_t {
                    if (buf == null) return 0;
                    return getRegionImpl(.complex, x, i, n, @ptrCast(@alignCast(buf.?)));
                }
            }.f);
        },
    }

    return cls;
}

fn OwnedAltVector(comptime kind: AltKind, comptime pkg: []const u8, comptime name: []const u8) type {
    return struct {
        var class: R.R_altrep_class_t = undefined;
        var registered: bool = false;

        pub fn register(info: anytype) void {
            class = buildClass(kind, pkg, name, info);
            registered = true;
        }

        pub fn init(slice: []const ElemType(kind)) R.SEXP {
            if (!registered) {
                class = buildClass(kind, pkg, name, null);
                registered = true;
            }

            const w = makeWrap(kind, slice);
            const Free = struct {
                fn fire(ptr: ?*anyopaque) void {
                    std.heap.c_allocator.destroy(@as(*Wrap(kind), @ptrCast(@alignCast(ptr))));
                }
            };
            cleanup.pushFrame(Free.fire, @as(?*anyopaque, @ptrCast(w)));
            const tag = tagSymbol(kind);
            const d1 = R.R_MakeExternalPtr(@as(?*anyopaque, @ptrCast(w)), tag, R.R_NilValue);
            R.R_RegisterCFinalizerEx(d1, struct {
                fn f(sexp: R.SEXP) callconv(.c) void {
                    freeWrapImpl(kind, sexp);
                }
            }.f, 1);
            const result = R.R_new_altrep(class, d1, R.R_NilValue);
            cleanup.popFrame();
            return result;
        }
    };
}

pub fn AltReal(comptime pkg: []const u8, comptime name: []const u8) type {
    return OwnedAltVector(.real, pkg, name);
}

pub fn AltInteger(comptime pkg: []const u8, comptime name: []const u8) type {
    return OwnedAltVector(.integer, pkg, name);
}

pub fn AltLogical(comptime pkg: []const u8, comptime name: []const u8) type {
    return OwnedAltVector(.logical, pkg, name);
}

pub fn AltRaw(comptime pkg: []const u8, comptime name: []const u8) type {
    return OwnedAltVector(.raw, pkg, name);
}

pub fn AltComplex(comptime pkg: []const u8, comptime name: []const u8) type {
    return OwnedAltVector(.complex, pkg, name);
}

const StringWrap = struct {
    ptr: [*]const R.SEXP,
    len: usize,
};

fn makeStringWrap(slice: []const []const u8) *StringWrap {
    const values = std.heap.c_allocator.alloc(R.SEXP, slice.len) catch err.signal("out of memory during ALTREP string creation");
    errdefer std.heap.c_allocator.free(values);
    for (slice, 0..) |item, index| {
        values[index] = R.Rf_mkCharLenCE(@ptrCast(item.ptr), @intCast(item.len), @as(R.cetype_t, @intCast(R.CE_UTF8)));
    }

    const wrap = std.heap.c_allocator.create(StringWrap) catch err.signal("out of memory during ALTREP string creation");
    errdefer std.heap.c_allocator.destroy(wrap);
    wrap.* = .{ .ptr = values.ptr, .len = values.len };
    return wrap;
}

fn stringWrapFromData1(sexp: R.SEXP) *StringWrap {
    const raw = R.R_ExternalPtrAddr(sexp) orelse @panic("null ALTREP string data pointer");
    return @ptrCast(@alignCast(raw));
}

fn stringWrapFromAltrep(x: R.SEXP) *StringWrap {
    return stringWrapFromData1(R.R_altrep_data1(x));
}

fn freeStringWrap(sexp: R.SEXP) void {
    const wrap = stringWrapFromData1(sexp);
    std.heap.c_allocator.free(wrap.ptr[0..wrap.len]);
    std.heap.c_allocator.destroy(wrap);
}

fn stringElt(x: R.SEXP, i: R.R_xlen_t) callconv(.c) R.SEXP {
    return stringWrapFromAltrep(x).ptr[@as(usize, @intCast(i))];
}

fn stringIsSorted(x: R.SEXP) callconv(.c) c_int {
    const wrap = stringWrapFromAltrep(x);
    if (wrap.len <= 1) return 1;
    for (0..wrap.len - 1) |i| {
        const lhs = std.mem.sliceTo(R.R_CHAR(wrap.ptr[i]), 0);
        const rhs = std.mem.sliceTo(R.R_CHAR(wrap.ptr[i + 1]), 0);
        if (std.mem.order(u8, lhs, rhs) == .gt) return 0;
    }
    return 1;
}

fn stringNoNA(x: R.SEXP) callconv(.c) c_int {
    const wrap = stringWrapFromAltrep(x);
    for (wrap.ptr[0..wrap.len]) |value| {
        if (value == R.R_NaString) return 0;
    }
    return 1;
}

fn duplicateString(x: R.SEXP, _: R.Rboolean) callconv(.c) R.SEXP {
    const wrap = stringWrapFromAltrep(x);
    const dup = R.Rf_protect(R.Rf_allocVector(R.STRSXP, @intCast(wrap.len)));
    defer R.Rf_unprotect(1);
    for (0..wrap.len) |i| {
        R.SET_STRING_ELT(dup, @intCast(i), wrap.ptr[i]);
    }
    return dup;
}

pub fn AltString(comptime pkg: []const u8, comptime name: []const u8) type {
    return struct {
        var class: R.R_altrep_class_t = undefined;
        var registered: bool = false;

        fn buildStringClass(info: anytype) R.R_altrep_class_t {
            const cls = R.R_make_altstring_class(@ptrCast(name.ptr), @ptrCast(pkg.ptr), info);
            R.R_set_altrep_Length_method(cls, struct {
                fn f(x: R.SEXP) callconv(.c) R.R_xlen_t {
                    return @intCast(stringWrapFromAltrep(x).len);
                }
            }.f);
            R.R_set_altrep_Duplicate_method(cls, duplicateString);
            R.R_set_altstring_Elt_method(cls, stringElt);
            // R 4.6 has no R_set_altstring_Dataptr_method or
            // R_set_altstring_Dataptr_or_null_method (strings are
            // an array of SEXP, not a flat data buffer).
            R.R_set_altstring_Is_sorted_method(cls, stringIsSorted);
            R.R_set_altstring_No_NA_method(cls, stringNoNA);
            return cls;
        }

        pub fn register(info: anytype) void {
            class = buildStringClass(info);
            registered = true;
        }

        pub fn init(slice: []const []const u8) R.SEXP {
            if (!registered) {
                class = buildStringClass(null);
                registered = true;
            }

            const wrap = makeStringWrap(slice);
            const Free = struct {
                fn fire(ptr: ?*anyopaque) void {
                    const w: *StringWrap = @ptrCast(@alignCast(ptr));
                    std.heap.c_allocator.free(w.ptr[0..w.len]);
                    std.heap.c_allocator.destroy(w);
                }
            };
            cleanup.pushFrame(Free.fire, @as(?*anyopaque, @ptrCast(wrap)));
            const d1 = R.R_MakeExternalPtr(@as(?*anyopaque, @ptrCast(wrap)), R.Rf_install("zigr_altstring_slice_wrap"), R.R_NilValue);
            R.R_RegisterCFinalizerEx(d1, struct {
                fn f(sexp: R.SEXP) callconv(.c) void {
                    freeStringWrap(sexp);
                }
            }.f, 1);
            const result = R.R_new_altrep(class, d1, R.R_NilValue);
            cleanup.popFrame();
            return result;
        }
    };
}
