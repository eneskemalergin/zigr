//! Comptime export generator.
//!
//! Keep the package's C entry points at the package root. Generated calls run
//! inside R's unwind boundary so temporary native storage is released on both
//! return and error. Conversion failures become R errors.

const std = @import("std");
const R = @import("R");
const convert = @import("convert.zig");
const cleanup = @import("cleanup");
const externalptr = @import("externalptr.zig");
const memory = @import("memory.zig");

fn rejectAlloc(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ra: usize) ?[*]u8 {
    _ = ctx;
    _ = len;
    _ = alignment;
    _ = ra;
    return null;
}
fn rejectFree(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, ra: usize) void {
    _ = ctx;
    _ = buf;
    _ = alignment;
    _ = ra;
}
fn rejectResize(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, ra: usize) bool {
    _ = ctx;
    _ = buf;
    _ = alignment;
    _ = new_len;
    _ = ra;
    return false;
}
fn rejectRemap(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, ra: usize) ?[*]u8 {
    _ = ctx;
    _ = buf;
    _ = alignment;
    _ = new_len;
    _ = ra;
    return null;
}
const no_alloc_allocator = std.mem.Allocator{
    .ptr = undefined,
    .vtable = &.{
        .alloc = rejectAlloc,
        .free = rejectFree,
        .resize = rejectResize,
        .remap = rejectRemap,
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

fn isVectorAccess(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .@"struct", .@"union" => @hasDecl(T, "zigr_vector_access"),
        else => false,
    };
}

fn fromSexp(comptime T: type, sexp: R.SEXP, arena: std.mem.Allocator) T {
    if (comptime T == R.SEXP) {
        return sexp;
    }
    if (comptime isVectorAccess(T)) {
        return convert.toVectorAccess(T.element_type, T.access_need, arena, sexp) catch |err| signalErrorMsg("toVectorAccess", @errorName(err));
    }
    // Avoid a second conversion for common non-null optional scalars.
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
    // The arena owns fallback copies until this R call returns or unwinds.
    if (comptime T == []const f64) {
        const view = convert.toRealSliceView(arena, sexp) catch |err| signalErrorMsg("toRealSliceView", @errorName(err));
        return view.constSlice();
    }
    if (comptime T == convert.RealSliceView) {
        return convert.toRealSliceView(arena, sexp) catch |err| signalErrorMsg("toRealSliceView", @errorName(err));
    }
    if (comptime T == []const i32) {
        const view = convert.toIntSliceView(arena, sexp) catch |err| signalErrorMsg("toIntSliceView", @errorName(err));
        return view.constSlice();
    }
    if (comptime T == convert.IntegerSliceView) {
        return convert.toIntSliceView(arena, sexp) catch |err| signalErrorMsg("toIntSliceView", @errorName(err));
    }
    if (comptime T == convert.LogicalSliceView) {
        const view = convert.toLogicalSliceView(arena, sexp) catch |err| signalErrorMsg("toLogicalSliceView", @errorName(err));
        return .{ .data = view.constSlice() };
    }
    if (comptime T == []const bool) @compileError("logical vectors require LogicalSliceView to preserve NA");
    if (comptime T == []const []const u8) {
        return convert.toStringSlice(arena, sexp) catch |err| signalErrorMsg("toStringSlice", @errorName(err));
    }
    if (comptime T == convert.StringSliceView) {
        return convert.toStringSliceView(sexp) catch |err| signalErrorMsg("toStringSliceView", @errorName(err));
    }
    if (comptime T == convert.CachedStringSliceView) {
        return convert.toCachedStringSliceView(arena, sexp) catch |err| signalErrorMsg("toCachedStringSliceView", @errorName(err));
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
    if (comptime T == convert.RawSliceView) {
        return convert.toRawSliceView(arena, sexp) catch |err| signalErrorMsg("toRawSliceView", @errorName(err));
    }
    if (comptime T == []const convert.Rcomplex) {
        const view = convert.toComplexSliceView(arena, sexp) catch |err| signalErrorMsg("toComplexSliceView", @errorName(err));
        return view.constSlice();
    }
    if (comptime T == convert.ComplexSliceView) {
        return convert.toComplexSliceView(arena, sexp) catch |err| signalErrorMsg("toComplexSliceView", @errorName(err));
    }
    @compileError("unsupported generated parameter type: " ++ @typeName(T) ++ "; accept R.SEXP and call convert.fromSEXP for a fixed schema");
}

fn toSexp(value: anytype, comptime T: type) R.SEXP {
    if (comptime @typeInfo(T) == .error_union) {
        @compileError("generated functions must handle Zig errors before returning to R");
    }
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
    if (comptime T == convert.LogicalSlice) return convert.fromLogicalSlice(value.data);
    if (comptime T == []const bool) @compileError("logical vectors require LogicalSlice to preserve NA");
    if (comptime T == []const []const u8) return convert.fromStringSlice(value);
    if (comptime T == []const u8) return convert.fromRawSlice(value);
    if (comptime T == []const convert.Rcomplex) return convert.fromComplexSlice(value);
    @compileError("unsupported generated return type: " ++ @typeName(T) ++ "; return R.SEXP from an explicit adapter or call convert.asSEXP");
}

/// Its spill state survives an R longjmp.
const TwoTierArena = struct {
    const fixed_capacity = 8192;

    fixed_buf: [fixed_capacity]u8 align(64),
    fba: std.heap.FixedBufferAllocator,
    spill: memory.UnwindArena,

    /// Initializing by value would leave the allocator pointing at a dead frame.
    fn init(self: *TwoTierArena) void {
        self.fixed_buf = undefined;
        self.spill = memory.UnwindArena.init();
        self.fba = std.heap.FixedBufferAllocator.init(&self.fixed_buf);
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
        self.spill.deinit();
    }

    fn allocFn(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ra: usize) ?[*]u8 {
        const self: *TwoTierArena = @ptrCast(@alignCast(ctx));
        if (self.fba.allocator().rawAlloc(len, alignment, ra)) |fixed| return fixed;
        return self.spill.allocator().rawAlloc(len, alignment, ra);
    }

    fn freeFn(_: *anyopaque, buf: []u8, alignment: std.mem.Alignment, ra: usize) void {
        // Longjmp can invalidate the wrapper frame before cleanup fires.
        _ = buf;
        _ = alignment;
        _ = ra;
    }

    fn resizeFn(_: *anyopaque, buf: []u8, _: std.mem.Alignment, new_len: usize, _: usize) bool {
        _ = buf;
        _ = new_len;
        return false;
    }

    fn remapFn(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) ?[*]u8 {
        return null;
    }
};

fn needsInputArena(comptime T: type) bool {
    if (comptime T == convert.StringSliceView) return false;
    if (comptime isVectorAccess(T)) return true;
    if (comptime T == convert.RealSliceView or
        T == convert.IntegerSliceView or
        T == convert.RawSliceView or
        T == convert.ComplexSliceView or
        T == convert.CachedStringSliceView) return true;
    return switch (@typeInfo(T)) {
        .optional => |info| needsInputArena(info.child),
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
        var needed = false;
        for (func_info.params) |p| needed = needed or needsInputArena(p.type.?);
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
            // Scalar wrappers should not pay for unused scratch storage.
            const Arena = if (arena_needed) TwoTierArena else struct {};
            var arena: Arena = undefined;
            if (comptime arena_needed) arena.init();
            defer if (comptime arena_needed) arena.deinit();
            const alloc = if (comptime arena_needed) arena.allocator() else no_alloc_allocator;

            if (comptime n == 0) return toSexp(func(), ret_type);
            if (comptime n == 1) {
                const arg0 = fromSexp(func_info.params[0].type.?, args.a0, alloc);
                return toSexp(func(arg0), ret_type);
            }
            if (comptime n == 2) {
                const arg0 = fromSexp(func_info.params[0].type.?, args.a0, alloc);
                const arg1 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                return toSexp(func(arg0, arg1), ret_type);
            }
            if (comptime n == 3) {
                const arg0 = fromSexp(func_info.params[0].type.?, args.a0, alloc);
                const arg1 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const arg2 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                return toSexp(func(arg0, arg1, arg2), ret_type);
            }
            if (comptime n == 4) {
                const arg0 = fromSexp(func_info.params[0].type.?, args.a0, alloc);
                const arg1 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const arg2 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                const arg3 = fromSexp(func_info.params[3].type.?, args.a3, alloc);
                return toSexp(func(arg0, arg1, arg2, arg3), ret_type);
            }
            if (comptime n == 5) {
                const arg0 = fromSexp(func_info.params[0].type.?, args.a0, alloc);
                const arg1 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const arg2 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                const arg3 = fromSexp(func_info.params[3].type.?, args.a3, alloc);
                const arg4 = fromSexp(func_info.params[4].type.?, args.a4, alloc);
                return toSexp(func(arg0, arg1, arg2, arg3, arg4), ret_type);
            }
            if (comptime n == 6) {
                const arg0 = fromSexp(func_info.params[0].type.?, args.a0, alloc);
                const arg1 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const arg2 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                const arg3 = fromSexp(func_info.params[3].type.?, args.a3, alloc);
                const arg4 = fromSexp(func_info.params[4].type.?, args.a4, alloc);
                const arg5 = fromSexp(func_info.params[5].type.?, args.a5, alloc);
                return toSexp(func(arg0, arg1, arg2, arg3, arg4, arg5), ret_type);
            }
            if (comptime n == 7) {
                const arg0 = fromSexp(func_info.params[0].type.?, args.a0, alloc);
                const arg1 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const arg2 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                const arg3 = fromSexp(func_info.params[3].type.?, args.a3, alloc);
                const arg4 = fromSexp(func_info.params[4].type.?, args.a4, alloc);
                const arg5 = fromSexp(func_info.params[5].type.?, args.a5, alloc);
                const arg6 = fromSexp(func_info.params[6].type.?, args.a6, alloc);
                return toSexp(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6), ret_type);
            }
            if (comptime n == 8) {
                const arg0 = fromSexp(func_info.params[0].type.?, args.a0, alloc);
                const arg1 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const arg2 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                const arg3 = fromSexp(func_info.params[3].type.?, args.a3, alloc);
                const arg4 = fromSexp(func_info.params[4].type.?, args.a4, alloc);
                const arg5 = fromSexp(func_info.params[5].type.?, args.a5, alloc);
                const arg6 = fromSexp(func_info.params[6].type.?, args.a6, alloc);
                const arg7 = fromSexp(func_info.params[7].type.?, args.a7, alloc);
                return toSexp(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7), ret_type);
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
        var needed = false;
        for (func_info.params) |p| needed = needed or needsInputArena(p.type.?);
        break :blk needed;
    };

    const W = struct {
        fn doCall(args_ptr: ?*anyopaque) R.SEXP {
            const args: R.SEXP = @ptrCast(@alignCast(args_ptr.?));
            const Arena = if (arena_needed) TwoTierArena else struct {};
            var arena: Arena = undefined;
            if (comptime arena_needed) arena.init();
            defer if (comptime arena_needed) arena.deinit();
            const alloc = if (comptime arena_needed) arena.allocator() else no_alloc_allocator;

            if (comptime n == 0) {
                return toSexp(func(), ret_type);
            }
            if (comptime n == 1) {
                const arg0 = fromSexp(func_info.params[0].type.?, externalArg(args, 0), alloc);
                return toSexp(func(arg0), ret_type);
            }
            if (comptime n == 2) {
                const arg0 = fromSexp(func_info.params[0].type.?, externalArg(args, 0), alloc);
                const arg1 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                return toSexp(func(arg0, arg1), ret_type);
            }
            if (comptime n == 3) {
                const arg0 = fromSexp(func_info.params[0].type.?, externalArg(args, 0), alloc);
                const arg1 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                const arg2 = fromSexp(func_info.params[2].type.?, externalArg(args, 2), alloc);
                return toSexp(func(arg0, arg1, arg2), ret_type);
            }
            if (comptime n == 4) {
                const arg0 = fromSexp(func_info.params[0].type.?, externalArg(args, 0), alloc);
                const arg1 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                const arg2 = fromSexp(func_info.params[2].type.?, externalArg(args, 2), alloc);
                const arg3 = fromSexp(func_info.params[3].type.?, externalArg(args, 3), alloc);
                return toSexp(func(arg0, arg1, arg2, arg3), ret_type);
            }
            if (comptime n == 5) {
                const arg0 = fromSexp(func_info.params[0].type.?, externalArg(args, 0), alloc);
                const arg1 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                const arg2 = fromSexp(func_info.params[2].type.?, externalArg(args, 2), alloc);
                const arg3 = fromSexp(func_info.params[3].type.?, externalArg(args, 3), alloc);
                const arg4 = fromSexp(func_info.params[4].type.?, externalArg(args, 4), alloc);
                return toSexp(func(arg0, arg1, arg2, arg3, arg4), ret_type);
            }
            if (comptime n == 6) {
                const arg0 = fromSexp(func_info.params[0].type.?, externalArg(args, 0), alloc);
                const arg1 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                const arg2 = fromSexp(func_info.params[2].type.?, externalArg(args, 2), alloc);
                const arg3 = fromSexp(func_info.params[3].type.?, externalArg(args, 3), alloc);
                const arg4 = fromSexp(func_info.params[4].type.?, externalArg(args, 4), alloc);
                const arg5 = fromSexp(func_info.params[5].type.?, externalArg(args, 5), alloc);
                return toSexp(func(arg0, arg1, arg2, arg3, arg4, arg5), ret_type);
            }
            if (comptime n == 7) {
                const arg0 = fromSexp(func_info.params[0].type.?, externalArg(args, 0), alloc);
                const arg1 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                const arg2 = fromSexp(func_info.params[2].type.?, externalArg(args, 2), alloc);
                const arg3 = fromSexp(func_info.params[3].type.?, externalArg(args, 3), alloc);
                const arg4 = fromSexp(func_info.params[4].type.?, externalArg(args, 4), alloc);
                const arg5 = fromSexp(func_info.params[5].type.?, externalArg(args, 5), alloc);
                const arg6 = fromSexp(func_info.params[6].type.?, externalArg(args, 6), alloc);
                return toSexp(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6), ret_type);
            }
            if (comptime n == 8) {
                const arg0 = fromSexp(func_info.params[0].type.?, externalArg(args, 0), alloc);
                const arg1 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                const arg2 = fromSexp(func_info.params[2].type.?, externalArg(args, 2), alloc);
                const arg3 = fromSexp(func_info.params[3].type.?, externalArg(args, 3), alloc);
                const arg4 = fromSexp(func_info.params[4].type.?, externalArg(args, 4), alloc);
                const arg5 = fromSexp(func_info.params[5].type.?, externalArg(args, 5), alloc);
                const arg6 = fromSexp(func_info.params[6].type.?, externalArg(args, 6), alloc);
                const arg7 = fromSexp(func_info.params[7].type.?, externalArg(args, 7), alloc);
                return toSexp(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7), ret_type);
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

fn validateMethodSignature(comptime T: type, comptime func: anytype) void {
    const func_info = @typeInfo(@TypeOf(func)).@"fn";
    if (func_info.params.len == 0) {
        @compileError("generated method must declare *" ++ @typeName(T) ++ " as its first parameter");
    }
    const receiver_type = func_info.params[0].type orelse @compileError("generated method receiver must have a concrete type");
    if (receiver_type != *T) {
        @compileError("generated method receiver must be *" ++ @typeName(T) ++ ", found " ++ @typeName(receiver_type));
    }
    if (func_info.params.len > 5) {
        @compileError("generated method supports *" ++ @typeName(T) ++ " plus at most four parameters");
    }
}

fn methodNeedsInputArena(comptime func: anytype) bool {
    const func_info = @typeInfo(@TypeOf(func)).@"fn";
    var needed = false;
    for (func_info.params[1..]) |p| needed = needed or needsInputArena(p.type.?);
    return needed;
}

fn makeMethodWrapper(comptime T: type, comptime func: anytype) *const fn (R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP) callconv(.c) R.SEXP {
    comptime validateMethodSignature(T, func);
    const func_info = @typeInfo(@TypeOf(func)).@"fn";
    const n = func_info.params.len;
    const ret_type = func_info.return_type orelse void;

    const arena_needed = comptime methodNeedsInputArena(func);

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
            const Arena = if (arena_needed) TwoTierArena else struct {};
            var arena: Arena = undefined;
            if (comptime arena_needed) arena.init();
            defer if (comptime arena_needed) arena.deinit();
            const alloc = if (comptime arena_needed) arena.allocator() else no_alloc_allocator;

            const ptr = externalptr.checkedPointer(T, args.a0) catch |pointer_err| externalptr.signalPointerError(pointer_err);

            if (comptime n == 1) return toSexp(func(ptr), ret_type);
            if (comptime n == 2) {
                const arg0 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                return toSexp(func(ptr, arg0), ret_type);
            }
            if (comptime n == 3) {
                const arg0 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const arg1 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                return toSexp(func(ptr, arg0, arg1), ret_type);
            }
            if (comptime n == 4) {
                const arg0 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const arg1 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                const arg2 = fromSexp(func_info.params[3].type.?, args.a3, alloc);
                return toSexp(func(ptr, arg0, arg1, arg2), ret_type);
            }
            if (comptime n == 5) {
                const arg0 = fromSexp(func_info.params[1].type.?, args.a1, alloc);
                const arg1 = fromSexp(func_info.params[2].type.?, args.a2, alloc);
                const arg2 = fromSexp(func_info.params[3].type.?, args.a3, alloc);
                const arg3 = fromSexp(func_info.params[4].type.?, args.a4, alloc);
                return toSexp(func(ptr, arg0, arg1, arg2, arg3), ret_type);
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

fn makeExternalMethodWrapper(comptime T: type, comptime func: anytype) *const fn (R.SEXP) callconv(.c) R.SEXP {
    comptime validateMethodSignature(T, func);
    const func_info = @typeInfo(@TypeOf(func)).@"fn";
    const n = func_info.params.len;
    const ret_type = func_info.return_type orelse void;
    const arena_needed = comptime methodNeedsInputArena(func);

    const W = struct {
        fn doCall(args_ptr: ?*anyopaque) R.SEXP {
            const args: R.SEXP = @ptrCast(@alignCast(args_ptr.?));
            const Arena = if (arena_needed) TwoTierArena else struct {};
            var arena: Arena = undefined;
            if (comptime arena_needed) arena.init();
            defer if (comptime arena_needed) arena.deinit();
            const alloc = if (comptime arena_needed) arena.allocator() else no_alloc_allocator;
            const ptr = externalptr.checkedPointer(T, externalArg(args, 0)) catch |pointer_err| externalptr.signalPointerError(pointer_err);

            if (comptime n == 1) return toSexp(func(ptr), ret_type);
            if (comptime n == 2) {
                const arg0 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                return toSexp(func(ptr, arg0), ret_type);
            }
            if (comptime n == 3) {
                const arg0 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                const arg1 = fromSexp(func_info.params[2].type.?, externalArg(args, 2), alloc);
                return toSexp(func(ptr, arg0, arg1), ret_type);
            }
            if (comptime n == 4) {
                const arg0 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                const arg1 = fromSexp(func_info.params[2].type.?, externalArg(args, 2), alloc);
                const arg2 = fromSexp(func_info.params[3].type.?, externalArg(args, 3), alloc);
                return toSexp(func(ptr, arg0, arg1, arg2), ret_type);
            }
            if (comptime n == 5) {
                const arg0 = fromSexp(func_info.params[1].type.?, externalArg(args, 1), alloc);
                const arg1 = fromSexp(func_info.params[2].type.?, externalArg(args, 2), alloc);
                const arg2 = fromSexp(func_info.params[3].type.?, externalArg(args, 3), alloc);
                const arg3 = fromSexp(func_info.params[4].type.?, externalArg(args, 4), alloc);
                return toSexp(func(ptr, arg0, arg1, arg2, arg3), ret_type);
            }
            unreachable;
        }

        fn wrap(args: R.SEXP) callconv(.c) R.SEXP {
            return cleanup.protectCallData(doCall, @as(?*anyopaque, @ptrCast(args)));
        }
    };
    return W.wrap;
}

pub fn generateExports(comptime call_exports: anytype, comptime external_exports: anytype) type {
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

/// Generated methods accept only typed pointers created for their receiver type.
pub fn generateMethods(comptime T: type, comptime call_exports: anytype, comptime external_exports: anytype) type {
    comptime {
        for (call_exports) |exp| validateMethodSignature(T, exp.func);
        for (external_exports) |exp| validateMethodSignature(T, exp.func);
    }
    const call_count = call_exports.len;
    const ext_count = external_exports.len;

    return struct {
        pub var call_defs: [call_count + 1]R.R_CallMethodDef = undefined;
        pub var ext_defs: [ext_count + 1]R.R_ExternalMethodDef = undefined;
        var initialized: bool = false;

        pub fn init(info: *R.DllInfo) callconv(.c) void {
            if (initialized) return;
            _ = externalptr.typeTag(T);
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
                const full_name = comptime safeName(T) ++ "__" ++ exp.name;
                ext_defs[i] = makeExternalMethodDef(full_name, makeExternalMethodWrapper(T, exp.func), @intCast(fi.params.len));
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

const GeneratedCoverageState = struct { value: i32 };

fn generatedCoverageInputs(
    real: f64,
    integer: i32,
    logical: bool,
    optional_real: ?f64,
    optional_integer: ?i32,
    optional_logical: ?bool,
    real_vector: []const f64,
    integer_vector: []const i32,
    logical_vector: convert.LogicalSliceView,
    raw_view: convert.RawSliceView,
) convert.LogicalSlice {
    _ = real;
    _ = integer;
    _ = logical;
    _ = optional_real;
    _ = optional_integer;
    _ = optional_logical;
    _ = real_vector;
    _ = integer_vector;
    _ = logical_vector;
    _ = raw_view;
    return .{ .data = &.{} };
}

fn generatedCoverageViews(
    real_view: convert.RealSliceView,
    integer_view: convert.IntegerSliceView,
    complex_view: convert.ComplexSliceView,
) i32 {
    _ = real_view;
    _ = integer_view;
    _ = complex_view;
    return 1;
}

fn generatedCoverageAccess(
    one_pass: convert.VectorAccess(i32, .one_pass),
    repeated_pass: convert.VectorAccess(i32, .repeated_pass),
    random_access: convert.VectorAccess(i32, .random_access),
) i32 {
    _ = one_pass;
    _ = repeated_pass;
    _ = random_access;
    return 1;
}

fn generatedCoverageObject(value: R.SEXP) R.SEXP {
    return value;
}

fn generatedCoverageStrings(
    strings: []const []const u8,
    string_view: convert.StringSliceView,
    cached_strings: convert.CachedStringSliceView,
    complex: []const convert.Rcomplex,
) []const []const u8 {
    _ = strings;
    _ = string_view;
    _ = cached_strings;
    _ = complex;
    return &.{};
}

fn generatedCoverageOptional(logical: ?convert.LogicalSliceView) ?convert.LogicalSlice {
    _ = logical;
    return null;
}

fn generatedCoverageExternal(logical: convert.LogicalSliceView) convert.LogicalSlice {
    _ = logical;
    return .{ .data = &.{} };
}

fn generatedCoverageMethod(_: *GeneratedCoverageState, logical: convert.LogicalSliceView) convert.LogicalSlice {
    return generatedCoverageExternal(logical);
}

const GeneratedCoverageExports = generateExports(&.{
    .{ .name = "coverage_inputs", .func = generatedCoverageInputs },
    .{ .name = "coverage_views", .func = generatedCoverageViews },
    .{ .name = "coverage_access", .func = generatedCoverageAccess },
    .{ .name = "coverage_object", .func = generatedCoverageObject },
    .{ .name = "coverage_strings", .func = generatedCoverageStrings },
    .{ .name = "coverage_optional", .func = generatedCoverageOptional },
}, &.{
    .{ .name = "coverage_external", .func = generatedCoverageExternal },
});

const GeneratedCoverageMethods = generateMethods(GeneratedCoverageState, &.{
    .{ .name = "coverage_method", .func = generatedCoverageMethod },
}, &.{
    .{ .name = "coverage_method_external", .func = generatedCoverageMethod },
});

test "generateExports .Call usage example compiles" {
    const init_fn = CompileExampleExports.init;
    _ = init_fn;
}

test "generated coverage compiles logical vectors across entry forms" {
    _ = GeneratedCoverageExports.init;
    _ = GeneratedCoverageMethods.init;
    try std.testing.expect(needsInputArena(convert.LogicalSliceView));
    try std.testing.expect(needsInputArena(?convert.LogicalSliceView));
    try std.testing.expect(needsInputArena(convert.LogicalSlice));
    try std.testing.expect(needsInputArena(convert.RealSliceView));
    try std.testing.expect(needsInputArena(convert.IntegerSliceView));
    try std.testing.expect(needsInputArena(convert.RawSliceView));
    try std.testing.expect(needsInputArena(convert.ComplexSliceView));
    try std.testing.expect(needsInputArena(convert.VectorAccess(i32, .one_pass)));
    try std.testing.expect(needsInputArena(convert.VectorAccess(i32, .repeated_pass)));
    try std.testing.expect(needsInputArena(convert.VectorAccess(i32, .random_access)));
}

test "scalar and optional scalar wrappers do not need an arena" {
    try std.testing.expect(!needsInputArena(f64));
    try std.testing.expect(!needsInputArena(i32));
    try std.testing.expect(!needsInputArena(bool));
    try std.testing.expect(!needsInputArena(?f64));
    try std.testing.expect(!needsInputArena(?i32));
    try std.testing.expect(!needsInputArena(?bool));
    try std.testing.expect(!needsInputArena(convert.StringSliceView));
    try std.testing.expect(needsInputArena(convert.RawSliceView));
    try std.testing.expect(needsInputArena(convert.CachedStringSliceView));
    try std.testing.expect(needsInputArena([]const f64));
    try std.testing.expect(needsInputArena([]const u8));
}

test "TwoTierArena honors alignment and spills beyond fixed capacity" {
    var fixed: TwoTierArena = undefined;
    fixed.init();
    defer fixed.deinit();
    const aligned = fixed.allocator().rawAlloc(32, .@"64", @returnAddress()) orelse return error.OutOfMemory;
    try std.testing.expect(std.mem.Alignment.@"64".check(@intFromPtr(aligned)));
    const fixed_start = @intFromPtr(fixed.fixed_buf[0..].ptr);
    const fixed_end = fixed_start + fixed.fixed_buf.len;
    try std.testing.expect(@intFromPtr(aligned) >= fixed_start and @intFromPtr(aligned) < fixed_end);

    var spilled: TwoTierArena = undefined;
    spilled.init();
    defer spilled.deinit();
    const overflow = spilled.allocator().rawAlloc(TwoTierArena.fixed_capacity + 1, .of(u8), @returnAddress()) orelse return error.OutOfMemory;
    const spill_start = @intFromPtr(spilled.fixed_buf[0..].ptr);
    const spill_end = spill_start + spilled.fixed_buf.len;
    try std.testing.expect(@intFromPtr(overflow) < spill_start or @intFromPtr(overflow) >= spill_end);
}
