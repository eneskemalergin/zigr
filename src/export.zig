//! Comptime export generator.
//!
//! Usage:
//!   const Exports = zigr.@"export".generateExports(
//!       &.{.{ .name = "my_sum", .func = my_sum }}, &.{});
//!
//! The package root owns the actual C entry points:
//!   export fn R_init_mypkg(info: *R.DllInfo) callconv(.c) void {
//!       Exports.init(info);
//!   }
//!   export fn R_unload_mypkg(info: *R.DllInfo) callconv(.c) void {
//!       Exports.unload(info);
//!   }
//!
//! # Longjmp safety
//!
//! - **.Call wrappers** (`makeWrapper`, `makeMethodWrapper`) enter
//!   `protectCallData`, which wraps the call in `R_UnwindProtect`.
//!   The current arena cleanup frame still carries a pointer to a
//!   stack-local arena, so arena-backed longjmp safety is a P1 contract
//!   item and must not be described as complete yet.
//!
//! - **.External wrappers** (`makeExternalWrapper`) also enter
//!   `protectCallData`; the same arena-frame limitation applies.
//!
//! Errors signal Rf_error instead of panicking.
//!
//! Supported types: []const f64, []const i32, []const []const u8,
//! convert.StringSliceView, []const u8 (RAWSXP), []const Rcomplex,
//! f64, i32, bool, ?f64, ?i32, ?bool,
//! void, R.SEXP.
//! The P1 contract requires scalar f64/i32/bool to receive a non-NA
//! length-1 vector. The conversion rejects empty, length-greater-than-one,
//! and NA values.
//! Optional scalar ?f64/?i32/?bool accept NULL and typed NA as nullish;
//! non-null values use the same exact-length rule.
//! IEEE NaN is a non-null f64 value and passes through unchanged.
//! Use R.SEXP or a vector parameter when NA values need custom handling.
//! []const u8 maps to RAWSXP (raw bytes), not STRSXP. For scalar strings,
//! extract via R.STRING_ELT inside the function body.
//! `[]const []const u8` still allocates slice headers because Zig needs a
//! concrete slice-of-slices container. `convert.StringSliceView` is the
//! zero-copy input-only alternative for read-only string access.

const std = @import("std");
const R = @import("R");
const convert = @import("convert.zig");
const cleanup = @import("cleanup");
const sexp_mod = @import("sexp.zig");

/// Panics on any allocation. Used when arena_needed is false (all param/return types are scalars or SEXP). If this fires, a code path reached fromSexp with a type needing allocation despite arena_needed=false, which is a bug.
fn panicAlloc(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ra: usize) ?[*]u8 {
    _ = ctx;
    _ = len;
    _ = alignment;
    _ = ra;
    @panic("allocation triggered without arena. Bug in arena_needed check or fromSexp type routing");
}
fn panicFree(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, ra: usize) void {
    _ = ctx;
    _ = buf;
    _ = alignment;
    _ = ra;
}
fn panicResize(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, ra: usize) bool {
    _ = ctx;
    _ = buf;
    _ = alignment;
    _ = new_len;
    _ = ra;
    return false;
}
fn panicRemap(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, ra: usize) ?[*]u8 {
    _ = ctx;
    _ = buf;
    _ = alignment;
    _ = new_len;
    _ = ra;
    return null;
}
const panic_allocator = std.mem.Allocator{
    .ptr = undefined,
    .vtable = &.{
        .alloc = panicAlloc,
        .free = panicFree,
        .resize = panicResize,
        .remap = panicRemap,
    },
};

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
    if (comptime T == R.SEXP) {
        return sexp;
    }
    // These must precede the generic optional path. A valid non-null optional
    // scalar otherwise pays for one nullish probe and a second full conversion.
    if (comptime T == ?f64) {
        return convert.toOptionalRealScalar(sexp) catch |err| convert.signalError(err);
    }
    if (comptime T == ?i32) {
        return convert.toOptionalIntScalar(sexp) catch |err| convert.signalError(err);
    }
    if (comptime T == ?bool) {
        return convert.toOptionalBoolScalar(sexp) catch |err| convert.signalError(err);
    }
    if (comptime @typeInfo(T) == .optional) {
        const child = @typeInfo(T).optional.child;
        if (convert.optionalInputIsNullish(child, sexp)) return null;
        return @as(T, fromSexp(child, sexp, arena));
    }
    // Generated wrappers pass their call-scoped TwoTierArena here. A numeric
    // fallback copy therefore remains valid for the user function and is
    // reclaimed with that arena; a borrowed branch remains tied to this R
    // boundary and is never retained by the wrapper.
    if (comptime T == []const f64) {
        const view = convert.toRealSliceView(arena, sexp) catch |err| signalErrorMsg("toRealSliceView", @errorName(err));
        return view.constSlice();
    }
    if (comptime T == []const i32) {
        const view = convert.toIntSliceView(arena, sexp) catch |err| signalErrorMsg("toIntSliceView", @errorName(err));
        return view.constSlice();
    }
    if (comptime T == []const []const u8) {
        return convert.toStringSlice(arena, sexp) catch |err| signalErrorMsg("toStringSlice", @errorName(err));
    }
    if (comptime T == convert.StringSliceView) {
        return convert.toStringSliceView(sexp) catch |err| signalErrorMsg("toStringSliceView", @errorName(err));
    }
    if (comptime T == f64) {
        return convert.toRealScalar(sexp) catch |err| convert.signalError(err);
    }
    if (comptime T == i32) {
        return convert.toIntScalar(sexp) catch |err| convert.signalError(err);
    }
    if (comptime T == bool) {
        return convert.toBoolScalar(sexp) catch |err| convert.signalError(err);
    }
    if (comptime T == []const u8) {
        return convert.toRawSlice(arena, sexp) catch |err| signalErrorMsg("toRawSlice", @errorName(err));
    }
    if (comptime T == []const convert.Rcomplex) {
        const view = convert.toComplexSliceView(arena, sexp) catch |err| signalErrorMsg("toComplexSliceView", @errorName(err));
        return view.constSlice();
    }
    @compileError("unsupported parameter type: " ++ @typeName(T));
}

fn toSexp(value: anytype, comptime T: type) R.SEXP {
    if (comptime T == R.SEXP) return value;
    if (comptime @typeInfo(T) == .optional) {
        if (value) |v| {
            return toSexp(v, @typeInfo(T).optional.child);
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
    @compileError("unsupported return type: " ++ @typeName(T));
}

/// Two-tier allocator: stack buffer first, heap on spill. The fixed buffer covers ~90% of export calls; the heap arena exists only for large returns.
const TwoTierArena = struct {
    fixed_buf: [8192]u8,
    fba: std.heap.FixedBufferAllocator,
    heap_arena: std.heap.ArenaAllocator,

    fn init() TwoTierArena {
        var self = TwoTierArena{
            .fixed_buf = undefined,
            .fba = undefined,
            .heap_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
        };
        self.fba = std.heap.FixedBufferAllocator.init(&self.fixed_buf);
        return self;
    }

    fn allocator(self: *TwoTierArena) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = allocFn,
                .free = freeFn,
                .resize = resizeFn,
                .remap = remapFn,
            },
        };
    }

    fn deinit(self: *TwoTierArena) void {
        self.heap_arena.deinit();
    }

    fn allocFn(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ra: usize) ?[*]u8 {
        const self: *TwoTierArena = @ptrCast(@alignCast(ctx));
        const fixed_result = self.fba.allocator().alloc(u8, len) catch null;
        if (fixed_result) |mem| return mem.ptr;
        return self.heap_arena.allocator().rawAlloc(len, alignment, ra);
    }

    fn freeFn(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, ra: usize) void {
        const self: *TwoTierArena = @ptrCast(@alignCast(ctx));
        // Fixed buffer: free is a no-op (reset on next alloc).
        // Heap arena: individual frees are not supported; arena deinit frees all.
        _ = self;
        _ = buf;
        _ = alignment;
        _ = ra;
    }

    fn resizeFn(_: *anyopaque, buf: []u8, _: std.mem.Alignment, new_len: usize, _: usize) bool {
        _ = buf;
        _ = new_len;
        return false;
    }

    fn remapFn(_: *anyopaque, memory: []u8, _: std.mem.Alignment, new_len: usize, _: usize) ?[*]u8 {
        _ = memory;
        _ = new_len;
        return null;
    }
};

const FreeArena = struct {
    fn fire(ptr: ?*anyopaque) void {
        @as(*TwoTierArena, @ptrCast(@alignCast(ptr))).deinit();
    }
};

fn needsArena(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .optional => |info| needsArena(info.child),
        .pointer => |info| info.size == .slice,
        .@"struct" => true,
        else => false,
    };
}

fn makeWrapper(comptime func: anytype) *const fn (R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP) callconv(.c) R.SEXP {
    const func_info = @typeInfo(@TypeOf(func)).@"fn";
    const n = func_info.params.len;
    const ret_type = func_info.return_type orelse void;

    const arena_needed = comptime blk: {
        var needed = needsArena(ret_type);
        for (func_info.params) |p| needed = needed or needsArena(p.type.?);
        break :blk needed;
    };

    const W = struct {
        const CallArgs = struct {
            a0: R.SEXP,
            a1: R.SEXP,
            a2: R.SEXP,
            a3: R.SEXP,
            a4: R.SEXP,
            a5: R.SEXP,
            a6: R.SEXP,
            a7: R.SEXP,
        };

        fn doCall(data: ?*anyopaque) R.SEXP {
            const args: *CallArgs = @ptrCast(@alignCast(data.?));
            var arena: TwoTierArena = undefined;
            var have_arena = false;
            if (arena_needed) {
                arena = TwoTierArena.init();
                cleanup.pushFrame(FreeArena.fire, @as(?*anyopaque, @ptrCast(&arena)));
                have_arena = true;
            }
            defer if (have_arena) arena.deinit();
            defer if (have_arena) cleanup.popFrame();
            const alloc = if (have_arena) arena.allocator() else panic_allocator;

            if (comptime n == 0) return toSexp(func(), ret_type);
            if (comptime n == 1) {
                const p0 = fromSexp(func_info.params[0].type.?, args.a0, alloc);
                return toSexp(func(p0), ret_type);
            }
            if (comptime n == 2) {
                const p0 = fromSexp(func_info.params[0].type.?, args.a0, alloc);
                const p1 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                return toSexp(func(p0, p1), ret_type);
            }
            if (comptime n == 3) {
                const p0 = fromSexp(func_info.params[0].type.?, args.a0, alloc);
                const p1 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const p2 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                return toSexp(func(p0, p1, p2), ret_type);
            }
            if (comptime n == 4) {
                const p0 = fromSexp(func_info.params[0].type.?, args.a0, alloc);
                const p1 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const p2 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                const p3 = fromSexp(func_info.params[3].type.?, args.a3, alloc);
                return toSexp(func(p0, p1, p2, p3), ret_type);
            }
            if (comptime n == 5) {
                const p0 = fromSexp(func_info.params[0].type.?, args.a0, alloc);
                const p1 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const p2 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                const p3 = fromSexp(func_info.params[3].type.?, args.a3, alloc);
                const p4 = fromSexp(func_info.params[4].type.?, args.a4, alloc);
                return toSexp(func(p0, p1, p2, p3, p4), ret_type);
            }
            if (comptime n == 6) {
                const p0 = fromSexp(func_info.params[0].type.?, args.a0, alloc);
                const p1 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const p2 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                const p3 = fromSexp(func_info.params[3].type.?, args.a3, alloc);
                const p4 = fromSexp(func_info.params[4].type.?, args.a4, alloc);
                const p5 = fromSexp(func_info.params[5].type.?, args.a5, alloc);
                return toSexp(func(p0, p1, p2, p3, p4, p5), ret_type);
            }
            if (comptime n == 7) {
                const p0 = fromSexp(func_info.params[0].type.?, args.a0, alloc);
                const p1 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const p2 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                const p3 = fromSexp(func_info.params[3].type.?, args.a3, alloc);
                const p4 = fromSexp(func_info.params[4].type.?, args.a4, alloc);
                const p5 = fromSexp(func_info.params[5].type.?, args.a5, alloc);
                const p6 = fromSexp(func_info.params[6].type.?, args.a6, alloc);
                return toSexp(func(p0, p1, p2, p3, p4, p5, p6), ret_type);
            }
            if (comptime n == 8) {
                const p0 = fromSexp(func_info.params[0].type.?, args.a0, alloc);
                const p1 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const p2 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                const p3 = fromSexp(func_info.params[3].type.?, args.a3, alloc);
                const p4 = fromSexp(func_info.params[4].type.?, args.a4, alloc);
                const p5 = fromSexp(func_info.params[5].type.?, args.a5, alloc);
                const p6 = fromSexp(func_info.params[6].type.?, args.a6, alloc);
                const p7 = fromSexp(func_info.params[7].type.?, args.a7, alloc);
                return toSexp(func(p0, p1, p2, p3, p4, p5, p6, p7), ret_type);
            }
            @compileError("unsupported param count, max 8");
        }

        fn wrap(a0: R.SEXP, a1: R.SEXP, a2: R.SEXP, a3: R.SEXP, a4: R.SEXP, a5: R.SEXP, a6: R.SEXP, a7: R.SEXP) callconv(.c) R.SEXP {
            var call_args = CallArgs{ .a0 = a0, .a1 = a1, .a2 = a2, .a3 = a3, .a4 = a4, .a5 = a5, .a6 = a6, .a7 = a7 };
            return cleanup.protectCallData(doCall, @as(?*anyopaque, @ptrCast(&call_args)));
        }
    };
    return W.wrap;
}

fn externalArg(args: R.SEXP, comptime index: usize) R.SEXP {
    var current = args;
    inline for (0..index + 1) |_| current = R.CDR(current);
    return R.CAR(current);
}

fn makeExternalWrapper(comptime func: anytype) *const fn (R.SEXP) callconv(.c) R.SEXP {
    const func_info = @typeInfo(@TypeOf(func)).@"fn";
    const n = func_info.params.len;
    const ret_type = func_info.return_type orelse void;

    const arena_needed = comptime blk: {
        var needed = needsArena(ret_type);
        for (func_info.params) |p| needed = needed or needsArena(p.type.?);
        break :blk needed;
    };

    const W = struct {
        fn doCall(args_ptr: ?*anyopaque) R.SEXP {
            const args: R.SEXP = @ptrCast(@alignCast(args_ptr.?));
            var arena: TwoTierArena = undefined;
            var have_arena = false;
            if (arena_needed) {
                arena = TwoTierArena.init();
                cleanup.pushFrame(FreeArena.fire, @as(?*anyopaque, @ptrCast(&arena)));
                have_arena = true;
            }
            defer if (have_arena) arena.deinit();
            defer if (have_arena) cleanup.popFrame();
            const alloc = if (have_arena) arena.allocator() else panic_allocator;

            if (comptime n == 0) {
                return toSexp(func(), ret_type);
            }
            if (comptime n == 1) {
                const p0 = fromSexp(func_info.params[0].type.?, externalArg(args, 0), alloc);
                return toSexp(func(p0), ret_type);
            }
            if (comptime n == 2) {
                const p0 = fromSexp(func_info.params[0].type.?, externalArg(args, 0), alloc);
                const p1 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                return toSexp(func(p0, p1), ret_type);
            }
            if (comptime n == 3) {
                const p0 = fromSexp(func_info.params[0].type.?, externalArg(args, 0), alloc);
                const p1 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                const p2 = fromSexp(func_info.params[2].type.?, externalArg(args, 2), alloc);
                return toSexp(func(p0, p1, p2), ret_type);
            }
            if (comptime n == 4) {
                const p0 = fromSexp(func_info.params[0].type.?, externalArg(args, 0), alloc);
                const p1 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                const p2 = fromSexp(func_info.params[2].type.?, externalArg(args, 2), alloc);
                const p3 = fromSexp(func_info.params[3].type.?, externalArg(args, 3), alloc);
                return toSexp(func(p0, p1, p2, p3), ret_type);
            }
            if (comptime n == 5) {
                const p0 = fromSexp(func_info.params[0].type.?, externalArg(args, 0), alloc);
                const p1 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                const p2 = fromSexp(func_info.params[2].type.?, externalArg(args, 2), alloc);
                const p3 = fromSexp(func_info.params[3].type.?, externalArg(args, 3), alloc);
                const p4 = fromSexp(func_info.params[4].type.?, externalArg(args, 4), alloc);
                return toSexp(func(p0, p1, p2, p3, p4), ret_type);
            }
            if (comptime n == 6) {
                const p0 = fromSexp(func_info.params[0].type.?, externalArg(args, 0), alloc);
                const p1 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                const p2 = fromSexp(func_info.params[2].type.?, externalArg(args, 2), alloc);
                const p3 = fromSexp(func_info.params[3].type.?, externalArg(args, 3), alloc);
                const p4 = fromSexp(func_info.params[4].type.?, externalArg(args, 4), alloc);
                const p5 = fromSexp(func_info.params[5].type.?, externalArg(args, 5), alloc);
                return toSexp(func(p0, p1, p2, p3, p4, p5), ret_type);
            }
            if (comptime n == 7) {
                const p0 = fromSexp(func_info.params[0].type.?, externalArg(args, 0), alloc);
                const p1 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                const p2 = fromSexp(func_info.params[2].type.?, externalArg(args, 2), alloc);
                const p3 = fromSexp(func_info.params[3].type.?, externalArg(args, 3), alloc);
                const p4 = fromSexp(func_info.params[4].type.?, externalArg(args, 4), alloc);
                const p5 = fromSexp(func_info.params[5].type.?, externalArg(args, 5), alloc);
                const p6 = fromSexp(func_info.params[6].type.?, externalArg(args, 6), alloc);
                return toSexp(func(p0, p1, p2, p3, p4, p5, p6), ret_type);
            }
            if (comptime n == 8) {
                const p0 = fromSexp(func_info.params[0].type.?, externalArg(args, 0), alloc);
                const p1 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                const p2 = fromSexp(func_info.params[2].type.?, externalArg(args, 2), alloc);
                const p3 = fromSexp(func_info.params[3].type.?, externalArg(args, 3), alloc);
                const p4 = fromSexp(func_info.params[4].type.?, externalArg(args, 4), alloc);
                const p5 = fromSexp(func_info.params[5].type.?, externalArg(args, 5), alloc);
                const p6 = fromSexp(func_info.params[6].type.?, externalArg(args, 6), alloc);
                const p7 = fromSexp(func_info.params[7].type.?, externalArg(args, 7), alloc);
                return toSexp(func(p0, p1, p2, p3, p4, p5, p6, p7), ret_type);
            }
            @compileError("unsupported param count, max 8 for .External");
        }

        fn wrap(args: R.SEXP) callconv(.c) R.SEXP {
            return cleanup.protectCallData(doCall, @as(?*anyopaque, @ptrCast(args)));
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

    const arena_needed = comptime blk: {
        var needed = needsArena(ret_type);
        for (func_info.params) |p| needed = needed or needsArena(p.type.?);
        break :blk needed;
    };

    const W = struct {
        const MethodCallArgs = struct {
            a0: R.SEXP,
            a1: R.SEXP,
            a2: R.SEXP,
            a3: R.SEXP,
            a4: R.SEXP,
            a5: R.SEXP,
            a6: R.SEXP,
            a7: R.SEXP,
        };

        fn doCall(data: ?*anyopaque) R.SEXP {
            const args: *MethodCallArgs = @ptrCast(@alignCast(data.?));
            var arena: TwoTierArena = undefined;
            var have_arena = false;
            if (arena_needed) {
                arena = TwoTierArena.init();
                cleanup.pushFrame(FreeArena.fire, @as(?*anyopaque, @ptrCast(&arena)));
                have_arena = true;
            }
            defer if (have_arena) arena.deinit();
            defer if (have_arena) cleanup.popFrame();
            const alloc = if (have_arena) arena.allocator() else panic_allocator;

            if (sexp_mod.typeTag(args.a0) != 22) signalError("expected external pointer");
            const raw_ptr = R.R_ExternalPtrAddr(args.a0) orelse signalError("null external pointer");
            const ptr: *T = @ptrCast(@alignCast(raw_ptr));

            if (comptime n == 1) return toSexp(func(ptr), ret_type);
            if (comptime n == 2) {
                const p0 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                return toSexp(func(ptr, p0), ret_type);
            }
            if (comptime n == 3) {
                const p0 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const p1 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                return toSexp(func(ptr, p0, p1), ret_type);
            }
            if (comptime n == 4) {
                const p0 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const p1 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                const p2 = fromSexp(func_info.params[3].type.?, args.a3, alloc);
                return toSexp(func(ptr, p0, p1, p2), ret_type);
            }
            if (comptime n == 5) {
                const p0 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const p1 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                const p2 = fromSexp(func_info.params[3].type.?, args.a3, alloc);
                const p3 = fromSexp(func_info.params[4].type.?, args.a4, alloc);
                return toSexp(func(ptr, p0, p1, p2, p3), ret_type);
            }
            @compileError("method with >4 extra params not supported");
        }

        fn wrap(a0: R.SEXP, a1: R.SEXP, a2: R.SEXP, a3: R.SEXP, a4: R.SEXP, a5: R.SEXP, a6: R.SEXP, a7: R.SEXP) callconv(.c) R.SEXP {
            var call_args = MethodCallArgs{ .a0 = a0, .a1 = a1, .a2 = a2, .a3 = a3, .a4 = a4, .a5 = a5, .a6 = a6, .a7 = a7 };
            return cleanup.protectCallData(doCall, @as(?*anyopaque, @ptrCast(&call_args)));
        }
    };
    return W.wrap;
}

/// Generate exports for an R package. `call_exports` register under .Call, `external_exports` under .External. Both are comptime slices of `{ .name, .func }` pairs. Pass &.{} for empty tables.
pub fn generateExports(comptime call_exports: anytype, comptime external_exports: anytype) type {
    const call_count = call_exports.len;
    const ext_count = external_exports.len;

    return struct {
        pub var call_defs: [call_count + 1]R.R_CallMethodDef = undefined;
        pub var ext_defs: [ext_count + 1]R.R_ExternalMethodDef = undefined;
        var initialized: bool = false;

        /// Call this from your R_init_<pkg> entry point.
        pub fn init(info: *R.DllInfo) callconv(.c) void {
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
            _ = R.R_useDynamicSymbols(info, 0);
        }

        /// Call this from your R_unload_<pkg> entry point.
        pub fn unload(_: *R.DllInfo) callconv(.c) void {}
    };
}

fn safeName(comptime T: type) []const u8 {
    return comptime blk: {
        const name = @typeName(T);
        var buf: [name.len]u8 = undefined;
        for (name, 0..) |c, i| {
            buf[i] = if (c == '.') '_' else c;
        }
        break :blk &buf;
    };
}

/// Generate method exports for a struct type `T`. Functions receive
/// `self: *T` as the first parameter, extracted from an EXTPTRSXP.
/// The generated wrapper verifies the pointer is non-null, calls the
/// method, and returns the result. Method names are prefixed with
/// `T__` to avoid collisions (e.g. `Person__greet`). Dots in the
/// type name are replaced with underscores for valid C identifiers.
pub fn generateMethods(comptime T: type, comptime call_exports: anytype, comptime external_exports: anytype) type {
    const call_count = call_exports.len;
    const ext_count = external_exports.len;

    return struct {
        pub var call_defs: [call_count + 1]R.R_CallMethodDef = undefined;
        pub var ext_defs: [ext_count + 1]R.R_ExternalMethodDef = undefined;
        var initialized: bool = false;

        pub fn init(info: *R.DllInfo) callconv(.c) void {
            if (initialized) return;
            initialized = true;

            inline for (0..call_count) |i| {
                const exp = call_exports[i];
                const fi = @typeInfo(@TypeOf(exp.func)).@"fn";
                const full_name = comptime safeName(T) ++ "__" ++ exp.name;
                call_defs[i] = makeMethodDef(full_name, makeMethodWrapper(T, exp.func), @intCast(fi.params.len));
            }
            call_defs[call_count] = .{ .name = null, .fun = null, .numArgs = 0 };

            inline for (0..ext_count) |i| {
                const exp = external_exports[i];
                const fi = @typeInfo(@TypeOf(exp.func)).@"fn";
                const full_name = safeName(T) ++ "__" ++ exp.name;
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
            _ = R.R_useDynamicSymbols(info, 0);
        }

        pub fn unload(_: *R.DllInfo) callconv(.c) void {}
    };
}

fn compileExampleSum(value: f64) f64 {
    return value;
}

const CompileExampleExports = generateExports(&.{.{ .name = "my_sum", .func = compileExampleSum }}, &.{});

test "generateExports .Call usage example compiles" {
    const init_fn = CompileExampleExports.init;
    _ = init_fn;
}

test "scalar and optional scalar wrappers do not need an arena" {
    try std.testing.expect(!needsArena(f64));
    try std.testing.expect(!needsArena(i32));
    try std.testing.expect(!needsArena(bool));
    try std.testing.expect(!needsArena(?f64));
    try std.testing.expect(!needsArena(?i32));
    try std.testing.expect(!needsArena(?bool));
}
