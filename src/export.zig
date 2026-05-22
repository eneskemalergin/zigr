//! Comptime export generator.
//!
//! Usage:
//!   const e = zigr.export.generateExports("mypkg", &.{.{ .name = "my_sum", .func = my_sum }}, &.{});
//!
//! Generates C wrappers inside R_UnwindProtect (arena freed on longjmp).
//! Errors signal Rf_error instead of panicking.
//!
//! Supported types: []const f64, []const i32, []const []const u8,
//! []const u8 (RAWSXP), []const Rcomplex, f64, i32, bool, ?f64, ?i32, ?bool,
//! void, R.SEXP.
//! Scalar f64/i32/bool receive raw values including NA sentinels (NaN/INT_MIN).
//! The function body should call R.ISNA(v) or R.INTEGER_ELT checks if NA is
//! possible. []const u8 maps to RAWSXP (raw bytes), not STRSXP. For scalar
//! strings, extract via R.STRING_ELT inside the function body.

const std = @import("std");
const R = @import("R");
const convert = @import("convert.zig");
const cleanup = @import("cleanup");

fn signalErrorMsg(prefix: []const u8, detail: []const u8) noreturn {
    var buf: [4096:0]u8 = undefined;
    const pn = @min(prefix.len, buf.len - 2);
    const dn = @min(detail.len, buf.len - pn - 2);
    if (pn > 0) @memcpy(buf[0..pn], prefix[0..pn]);
    if (pn > 0 and pn < buf.len) buf[pn] = ':';
    if (pn + 1 < buf.len) buf[pn + 1] = ' ';
    if (dn > 0) @memcpy(buf[pn + 2 .. pn + 2 + dn], detail[0..dn]);
    buf[pn + 2 + dn] = 0;
    R.Rf_error(&buf);
}

fn signalError(msg: []const u8) noreturn {
    signalErrorMsg(msg, "");
}

fn fromSexp(comptime T: type, sexp: R.SEXP, arena: std.mem.Allocator) T {
    if (comptime @typeInfo(T) == .optional) {
        if (sexp == R.R_NilValue) return null;
        const child = @typeInfo(T).Optional.child;
        return @as(T, fromSexp(child, sexp, arena));
    }
    if (comptime T == []const f64) {
        return convert.toRealSlice(arena, sexp) catch |err| signalErrorMsg("toRealSlice", @errorName(err));
    }
    if (comptime T == []const i32) {
        return convert.toIntSlice(arena, sexp) catch |err| signalErrorMsg("toIntSlice", @errorName(err));
    }
    if (comptime T == []const []const u8) {
        return convert.toStringSlice(arena, sexp) catch |err| signalErrorMsg("toStringSlice", @errorName(err));
    }
    if (comptime T == f64) {
        if (R.XLENGTH(sexp) == 0) signalError("zero-length vector passed to f64");
        return R.REAL(sexp)[0];
    }
    if (comptime T == i32) {
        if (R.XLENGTH(sexp) == 0) signalError("zero-length vector passed to i32");
        return R.INTEGER(sexp)[0];
    }
    if (comptime T == bool) {
        if (R.XLENGTH(sexp) == 0) signalError("zero-length vector passed to bool");
        return R.LOGICAL(sexp)[0] != 0;
    }
    if (comptime T == []const u8) {
        return convert.toRawSlice(arena, sexp) catch |err| signalErrorMsg("toRawSlice", @errorName(err));
    }
    if (comptime T == []const convert.Rcomplex) {
        return convert.toComplexSlice(arena, sexp) catch |err| signalErrorMsg("toComplexSlice", @errorName(err));
    }
    if (comptime T == R.SEXP) {
        return sexp;
    }
    @compileError("unsupported parameter type: " ++ @typeName(T));
}

fn toSexp(value: anytype, comptime T: type) R.SEXP {
    if (comptime @typeInfo(T) == .optional) {
        if (value) |v| {
            return toSexp(v, @typeInfo(T).Optional.child);
        } else {
            return R.R_NilValue;
        }
    }
    if (comptime T == void) return R.R_NilValue;
    if (comptime T == f64) return R.Rf_ScalarReal(value);
    if (comptime T == i32) return R.Rf_ScalarInteger(value);
    if (comptime T == bool) return R.Rf_ScalarLogical(if (value) 1 else 0);
    if (comptime T == []const f64) return convert.fromRealSlice(value);
    if (comptime T == []const i32) return convert.fromIntSlice(value);
    if (comptime T == []const []const u8) return convert.fromStringSlice(value);
    if (comptime T == []const u8) return convert.fromRawSlice(value);
    if (comptime T == []const convert.Rcomplex) return convert.fromComplexSlice(value);
    if (comptime T == R.SEXP) return value;
    @compileError("unsupported return type: " ++ @typeName(T));
}

const FreeArena = struct {
    fn fire(ptr: ?*anyopaque) void {
        @as(*std.heap.ArenaAllocator, @ptrCast(@alignCast(ptr))).deinit();
    }
};

/// Returns true if a type requires arena allocation for conversion.
fn needsArena(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .optional => |info| needsArena(info.child),
        .pointer => |info| info.size == .Slice,
        .@"struct" => true,
        else => false,
    };
}

fn makeWrapper(comptime func: anytype) *const fn (R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP) callconv(.c) R.SEXP {
    const func_info = @typeInfo(@TypeOf(func)).@"fn";
    const n = func_info.params.len;
    const ret_type = func_info.return_type orelse void;

    comptime var arena_needed = needsArena(ret_type);
    inline for (func_info.params) |p| {
        if (needsArena(p.type.?)) arena_needed = true;
    }

    const W = struct {
        fn wrap(a0: R.SEXP, a1: R.SEXP, a2: R.SEXP, a3: R.SEXP, a4: R.SEXP, a5: R.SEXP, a6: R.SEXP, a7: R.SEXP) callconv(.c) R.SEXP {
            _ = .{ a0, a1, a2, a3, a4, a5, a6, a7 };
            var arena: std.heap.ArenaAllocator = undefined;
            var have_arena = false;
            if (arena_needed) {
                arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
                cleanup.pushFrame(FreeArena.fire, @as(?*anyopaque, @ptrCast(&arena)));
                have_arena = true;
            }
            defer if (have_arena) arena.deinit();
            defer if (have_arena) cleanup.popFrame();
            const alloc = if (have_arena) arena.allocator() else std.heap.page_allocator;

            if (comptime n == 0) return toSexp(func(), ret_type);
            if (comptime n == 1) {
                const p0 = fromSexp(func_info.params[0].type.?, a0, alloc);
                return toSexp(func(p0), ret_type);
            }
            if (comptime n == 2) {
                const p0 = fromSexp(func_info.params[0].type.?, a0, alloc);
                const p1 = fromSexp(func_info.params[1].type.?, a1, alloc);
                return toSexp(func(p0, p1), ret_type);
            }
            if (comptime n == 3) {
                const p0 = fromSexp(func_info.params[0].type.?, a0, alloc);
                const p1 = fromSexp(func_info.params[1].type.?, a1, alloc);
                const p2 = fromSexp(func_info.params[2].type.?, a2, alloc);
                return toSexp(func(p0, p1, p2), ret_type);
            }
            if (comptime n == 4) {
                const p0 = fromSexp(func_info.params[0].type.?, a0, alloc);
                const p1 = fromSexp(func_info.params[1].type.?, a1, alloc);
                const p2 = fromSexp(func_info.params[2].type.?, a2, alloc);
                const p3 = fromSexp(func_info.params[3].type.?, a3, alloc);
                return toSexp(func(p0, p1, p2, p3), ret_type);
            }
            if (comptime n == 5) {
                const p0 = fromSexp(func_info.params[0].type.?, a0, alloc);
                const p1 = fromSexp(func_info.params[1].type.?, a1, alloc);
                const p2 = fromSexp(func_info.params[2].type.?, a2, alloc);
                const p3 = fromSexp(func_info.params[3].type.?, a3, alloc);
                const p4 = fromSexp(func_info.params[4].type.?, a4, alloc);
                return toSexp(func(p0, p1, p2, p3, p4), ret_type);
            }
            if (comptime n == 6) {
                const p0 = fromSexp(func_info.params[0].type.?, a0, alloc);
                const p1 = fromSexp(func_info.params[1].type.?, a1, alloc);
                const p2 = fromSexp(func_info.params[2].type.?, a2, alloc);
                const p3 = fromSexp(func_info.params[3].type.?, a3, alloc);
                const p4 = fromSexp(func_info.params[4].type.?, a4, alloc);
                const p5 = fromSexp(func_info.params[5].type.?, a5, alloc);
                return toSexp(func(p0, p1, p2, p3, p4, p5), ret_type);
            }
            if (comptime n == 7) {
                const p0 = fromSexp(func_info.params[0].type.?, a0, alloc);
                const p1 = fromSexp(func_info.params[1].type.?, a1, alloc);
                const p2 = fromSexp(func_info.params[2].type.?, a2, alloc);
                const p3 = fromSexp(func_info.params[3].type.?, a3, alloc);
                const p4 = fromSexp(func_info.params[4].type.?, a4, alloc);
                const p5 = fromSexp(func_info.params[5].type.?, a5, alloc);
                const p6 = fromSexp(func_info.params[6].type.?, a6, alloc);
                return toSexp(func(p0, p1, p2, p3, p4, p5, p6), ret_type);
            }
            if (comptime n == 8) {
                const p0 = fromSexp(func_info.params[0].type.?, a0, alloc);
                const p1 = fromSexp(func_info.params[1].type.?, a1, alloc);
                const p2 = fromSexp(func_info.params[2].type.?, a2, alloc);
                const p3 = fromSexp(func_info.params[3].type.?, a3, alloc);
                const p4 = fromSexp(func_info.params[4].type.?, a4, alloc);
                const p5 = fromSexp(func_info.params[5].type.?, a5, alloc);
                const p6 = fromSexp(func_info.params[6].type.?, a6, alloc);
                const p7 = fromSexp(func_info.params[7].type.?, a7, alloc);
                return toSexp(func(p0, p1, p2, p3, p4, p5, p6, p7), ret_type);
            }
            @compileError("unsupported param count, max 8");
        }
    };
    return W.wrap;
}

fn makeExternalWrapper(comptime func: anytype) *const fn (R.SEXP) callconv(.c) R.SEXP {
    const func_info = @typeInfo(@TypeOf(func)).@"fn";
    const n = func_info.params.len;
    const ret_type = func_info.return_type orelse void;

    comptime var arena_needed = needsArena(ret_type);
    inline for (func_info.params) |p| {
        if (needsArena(p.type.?)) arena_needed = true;
    }

    const W = struct {
        fn wrap(args: R.SEXP) callconv(.c) R.SEXP {
            return cleanup.protectCall(struct {
                fn call() R.SEXP {
                    var arena: std.heap.ArenaAllocator = undefined;
                    var have_arena = false;
                    if (arena_needed) {
                        arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
                        cleanup.pushFrame(FreeArena.fire, @as(?*anyopaque, @ptrCast(&arena)));
                        have_arena = true;
                    }
                    defer if (have_arena) arena.deinit();
                    defer if (have_arena) cleanup.popFrame();
                    const alloc = if (have_arena) arena.allocator() else std.heap.page_allocator;

                    if (comptime n == 1) {
                        const p0 = fromSexp(func_info.params[0].type.?, R.CAR(args), alloc);
                        return toSexp(func(p0), ret_type);
                    }
                    if (comptime n == 2) {
                        const p0 = fromSexp(func_info.params[0].type.?, R.CAR(args), alloc);
                        const p1 = fromSexp(func_info.params[1].type.?, R.CADR(args), alloc);
                        return toSexp(func(p0, p1), ret_type);
                    }
                    if (comptime n == 3) {
                        const p0 = fromSexp(func_info.params[0].type.?, R.CAR(args), alloc);
                        const p1 = fromSexp(func_info.params[1].type.?, R.CADR(args), alloc);
                        const p2 = fromSexp(func_info.params[2].type.?, R.CADDR(args), alloc);
                        return toSexp(func(p0, p1, p2), ret_type);
                    }
                    if (comptime n == 4) {
                        const p0 = fromSexp(func_info.params[0].type.?, R.CAR(args), alloc);
                        const p1 = fromSexp(func_info.params[1].type.?, R.CADR(args), alloc);
                        const p2 = fromSexp(func_info.params[2].type.?, R.CADDR(args), alloc);
                        const p3 = fromSexp(func_info.params[3].type.?, R.CADDDR(args), alloc);
                        return toSexp(func(p0, p1, p2, p3), ret_type);
                    }
                    if (comptime n == 5) {
                        const p0 = fromSexp(func_info.params[0].type.?, R.CAR(args), alloc);
                        const p1 = fromSexp(func_info.params[1].type.?, R.CADR(args), alloc);
                        const p2 = fromSexp(func_info.params[2].type.?, R.CADDR(args), alloc);
                        const p3 = fromSexp(func_info.params[3].type.?, R.CADDDR(args), alloc);
                        const p4 = fromSexp(func_info.params[4].type.?, R.CAD4R(args), alloc);
                        return toSexp(func(p0, p1, p2, p3, p4), ret_type);
                    }
                    if (comptime n == 6) {
                        const p0 = fromSexp(func_info.params[0].type.?, R.CAR(args), alloc);
                        const p1 = fromSexp(func_info.params[1].type.?, R.CADR(args), alloc);
                        const p2 = fromSexp(func_info.params[2].type.?, R.CADDR(args), alloc);
                        const p3 = fromSexp(func_info.params[3].type.?, R.CADDDR(args), alloc);
                        const p4 = fromSexp(func_info.params[4].type.?, R.CAD4R(args), alloc);
                        const p5 = fromSexp(func_info.params[5].type.?, R.CAD5R(args), alloc);
                        return toSexp(func(p0, p1, p2, p3, p4, p5), ret_type);
                    }
                    if (comptime n == 7) {
                        const p0 = fromSexp(func_info.params[0].type.?, R.CAR(args), alloc);
                        const p1 = fromSexp(func_info.params[1].type.?, R.CADR(args), alloc);
                        const p2 = fromSexp(func_info.params[2].type.?, R.CADDR(args), alloc);
                        const p3 = fromSexp(func_info.params[3].type.?, R.CADDDR(args), alloc);
                        const p4 = fromSexp(func_info.params[4].type.?, R.CAD4R(args), alloc);
                        const p5 = fromSexp(func_info.params[5].type.?, R.CAD5R(args), alloc);
                        const p6 = fromSexp(func_info.params[6].type.?, R.CAR(R.CDDR(R.CDDR(R.CDDR(args)))), alloc);
                        return toSexp(func(p0, p1, p2, p3, p4, p5, p6), ret_type);
                    }
                    if (comptime n == 8) {
                        const p0 = fromSexp(func_info.params[0].type.?, R.CAR(args), alloc);
                        const p1 = fromSexp(func_info.params[1].type.?, R.CADR(args), alloc);
                        const p2 = fromSexp(func_info.params[2].type.?, R.CADDR(args), alloc);
                        const p3 = fromSexp(func_info.params[3].type.?, R.CADDDR(args), alloc);
                        const p4 = fromSexp(func_info.params[4].type.?, R.CAD4R(args), alloc);
                        const p5 = fromSexp(func_info.params[5].type.?, R.CAD5R(args), alloc);
                        const p6 = fromSexp(func_info.params[6].type.?, R.CAR(R.CDDR(R.CDDR(R.CDDR(args)))), alloc);
                        const p7 = fromSexp(func_info.params[7].type.?, R.CAR(R.CDR(R.CDDR(R.CDDR(R.CDDR(args))))), alloc);
                        return toSexp(func(p0, p1, p2, p3, p4, p5, p6, p7), ret_type);
                    }
                    @compileError("unsupported param count, max 8 for .External");
                }
            }.call);
        }
    };
    return W.wrap;
}

fn makeMethodDef(name: []const u8, wrapper: *const fn (R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP) callconv(.c) R.SEXP, arity: c_int) R.R_CallMethodDef {
    return R.R_CallMethodDef{
        .name = @ptrCast(name.ptr),
        .fun = @ptrCast(wrapper),
        .numArgs = arity,
    };
}

fn makeExternalMethodDef(name: []const u8, wrapper: *const fn (R.SEXP) callconv(.c) R.SEXP, arity: c_int) R.R_ExternalMethodDef {
    return R.R_ExternalMethodDef{
        .name = @ptrCast(name.ptr),
        .fun = @ptrCast(wrapper),
        .numArgs = arity,
    };
}

// Generate a method wrapper where the first arg is an external pointer *T.
fn makeMethodWrapper(comptime T: type, comptime func: anytype) *const fn (R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP) callconv(.c) R.SEXP {
    const func_info = @typeInfo(@TypeOf(func)).@"fn";
    const n = func_info.params.len;
    const ret_type = func_info.return_type orelse void;

    comptime var arena_needed = needsArena(ret_type);
    inline for (func_info.params) |p| {
        if (needsArena(p.type.?)) arena_needed = true;
    }

    const W = struct {
        fn wrap(a0: R.SEXP, a1: R.SEXP, a2: R.SEXP, a3: R.SEXP, a4: R.SEXP, a5: R.SEXP, a6: R.SEXP, a7: R.SEXP) callconv(.c) R.SEXP {
            _ = .{ a0, a1, a2, a3, a4, a5, a6, a7 };
            var arena: std.heap.ArenaAllocator = undefined;
            var have_arena = false;
            if (arena_needed) {
                arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
                cleanup.pushFrame(FreeArena.fire, @as(?*anyopaque, @ptrCast(&arena)));
                have_arena = true;
            }
            defer if (have_arena) arena.deinit();
            defer if (have_arena) cleanup.popFrame();
            const alloc = if (have_arena) arena.allocator() else std.heap.page_allocator;

            const ptr: *T = @ptrCast(@alignCast(R.R_ExternalPtrAddr(a0).?));

            if (comptime n == 1) return toSexp(func(ptr), ret_type);
            if (comptime n == 2) {
                const p0 = fromSexp(func_info.params[1].type.?, a1, alloc);
                return toSexp(func(ptr, p0), ret_type);
            }
            if (comptime n == 3) {
                const p0 = fromSexp(func_info.params[1].type.?, a1, alloc);
                const p1 = fromSexp(func_info.params[2].type.?, a2, alloc);
                return toSexp(func(ptr, p0, p1), ret_type);
            }
            if (comptime n == 4) {
                const p0 = fromSexp(func_info.params[1].type.?, a1, alloc);
                const p1 = fromSexp(func_info.params[2].type.?, a2, alloc);
                const p2 = fromSexp(func_info.params[3].type.?, a3, alloc);
                return toSexp(func(ptr, p0, p1, p2), ret_type);
            }
            if (comptime n == 5) {
                const p0 = fromSexp(func_info.params[1].type.?, a1, alloc);
                const p1 = fromSexp(func_info.params[2].type.?, a2, alloc);
                const p2 = fromSexp(func_info.params[3].type.?, a3, alloc);
                const p3 = fromSexp(func_info.params[4].type.?, a4, alloc);
                return toSexp(func(ptr, p0, p1, p2, p3), ret_type);
            }
            @compileError("method with >4 extra params not supported");
        }
    };
    return W.wrap;
}

/// Generate exports for an R package. `call_exports` register under .Call,
/// `external_exports` register under .External. Both are comptime slices
/// of {name, func} pairs. Pass &.{} for empty tables.
/// Generates R_init_<pkg> and R_unload_<pkg> (no-op) via @export.
pub fn generateExports(comptime call_exports: anytype, comptime external_exports: anytype) type {
    const call_count = call_exports.len;
    const ext_count = external_exports.len;

    return struct {
        var call_defs: [call_count + 1]R.R_CallMethodDef = undefined;
        var ext_defs: [ext_count + 1]R.R_ExternalMethodDef = undefined;
        var initialized: bool = false;

        /// Call this from your R_init_<pkg> entry point.
        pub fn init(info: *R.DllInfo) void {
            if (initialized) return;
            initialized = true;

            inline for (0..call_count) |i| {
                const exp = call_exports[i];
                const fi = @typeInfo(@TypeOf(exp.func)).@"fn";
                call_defs[i] = makeMethodDef(exp.name, makeWrapper(exp.func), @intCast(fi.params.len));
            }
            call_defs[call_count] = .{ .name = null, .fun = null, .numArgs = 0 };

            inline for (0..ext_count) |i| {
                const exp = external_exports[i];
                const fi = @typeInfo(@TypeOf(exp.func)).@"fn";
                ext_defs[i] = makeExternalMethodDef(exp.name, makeExternalWrapper(exp.func), @intCast(fi.params.len));
            }
            ext_defs[ext_count] = .{ .name = null, .fun = null, .numArgs = 0 };

            _ = R.R_registerRoutines(
                info,
                null,
                if (call_count > 0) @as([*c]const R.R_CallMethodDef, @ptrCast(&call_defs[0])) else null,
                null,
                if (ext_count > 0) @as([*c]const R.R_ExternalMethodDef, @ptrCast(&ext_defs[0])) else null,
            );
        }

        /// Call this from your R_unload_<pkg> entry point.
        pub fn unload(_: *R.DllInfo) void {}
    };
}

/// Generate method exports for a struct type `T`. Functions receive
/// `self: *T` as the first parameter, extracted from an EXTPTRSXP.
/// The generated wrapper verifies the pointer is non-null, calls the
/// method, and returns the result. Method names are prefixed with
/// `T__` to avoid collisions (e.g. `Person__greet`).
pub fn generateMethods(comptime T: type, comptime call_exports: anytype, comptime external_exports: anytype) type {
    const call_count = call_exports.len;
    const ext_count = external_exports.len;

    return struct {
        var call_defs: [call_count + 1]R.R_CallMethodDef = undefined;
        var ext_defs: [ext_count + 1]R.R_ExternalMethodDef = undefined;
        var initialized: bool = false;

        fn init(info: *R.DllInfo) void {
            if (initialized) return;
            initialized = true;

            inline for (0..call_count) |i| {
                const exp = call_exports[i];
                const fi = @typeInfo(@TypeOf(exp.func)).@"fn";
                const full_name = @typeName(T) ++ "__" ++ exp.name;
                call_defs[i] = makeMethodDef(full_name, makeMethodWrapper(T, exp.func), @intCast(fi.params.len));
            }
            call_defs[call_count] = .{ .name = null, .fun = null, .numArgs = 0 };

            inline for (0..ext_count) |i| {
                const exp = external_exports[i];
                const fi = @typeInfo(@TypeOf(exp.func)).@"fn";
                const full_name = @typeName(T) ++ "__" ++ exp.name;
                ext_defs[i] = makeExternalMethodDef(full_name, makeExternalWrapper(exp.func), @intCast(fi.params.len));
            }
            ext_defs[ext_count] = .{ .name = null, .fun = null, .numArgs = 0 };

            _ = R.R_registerRoutines(
                info,
                null,
                if (call_count > 0) @as([*c]const R.R_CallMethodDef, @ptrCast(&call_defs[0])) else null,
                null,
                if (ext_count > 0) @as([*c]const R.R_ExternalMethodDef, @ptrCast(&ext_defs[0])) else null,
            );
        }

        pub fn unload(_: *R.DllInfo) void {}
    };
}
