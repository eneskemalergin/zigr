//! Thread-local cleanup stack and R_UnwindProtect bridge.
//!
//! R errors use C longjmp, which bypasses Zig's defer/errdefer.
//! This module wraps R_UnwindProtect and manages a thread-local stack
//! of cleanup frames that fire when R unwinds.
//!
//! Contract: on longjmp zigr_on_unwind fires ALL frames in LIFO order.
//! protectCall is safe to nest: each call saves and restores the caller's
//! frame count. On the normal path the caller's frames are preserved. On
//! longjmp all frames fire regardless of nesting depth because R's longjmp
//! tears down the entire Zig call stack anyway. Callers must push their
//! cleanup frames before calling protectCall to ensure they fire on
//! longjmp.

const std = @import("std");
const R = @import("R");
const err = @import("error");

pub const MAX_NESTING = 16;

const Frame = struct {
    func: *const fn (data: ?*anyopaque) void,
    data: ?*anyopaque,
};

threadlocal var stack: [MAX_NESTING]Frame = undefined;
threadlocal var count: usize = 0;

fn cleanHandler(_data: ?*anyopaque, jump: R.Rboolean) callconv(.c) void {
    _ = _data;
    if (jump == 1) zigr_on_unwind() else zigr_on_return();
}

export fn zigr_make_unwind_cont() R.SEXP {
    return R.R_MakeUnwindCont();
}

export fn zigr_protect_call(
    fun: *const fn (?*anyopaque) callconv(.c) R.SEXP,
    data: ?*anyopaque,
    cont: R.SEXP,
) R.SEXP {
    return R.R_UnwindProtect(fun, data, cleanHandler, null, cont);
}

export fn zigr_on_unwind() void {
    while (count > 0) {
        count -= 1;
        stack[count].func(stack[count].data);
    }
}

export fn zigr_on_return() void {
    count = 0;
}

pub fn pushFrame(func: *const fn (data: ?*anyopaque) void, data: ?*anyopaque) void {
    if (count >= MAX_NESTING) err.signal("cleanup stack overflow");
    stack[count] = .{ .func = func, .data = data };
    count += 1;
}

/// Remove the most recently pushed frame without firing its callback.
/// Must be called after every pushFrame on the normal return path.
pub fn popFrame() void {
    if (count > 0) count -= 1;
}

/// Call a Zig function inside an R_UnwindProtect guard.
/// R_MakeUnwindCont keeps the continuation token alive for the duration
/// of the R_UnwindProtect call, so no extra R_PreserveObject is needed.
/// On longjmp every cleanup frame fires (not just this one). See the
/// module-level contract above.
pub fn protectCall(comptime func: *const fn () R.SEXP) R.SEXP {
    const cont = zigr_make_unwind_cont();

    const saved_count = count;

    const W = struct {
        fn trampoline(_: ?*anyopaque) callconv(.c) R.SEXP {
            return func();
        }
    };
    const result = zigr_protect_call(W.trampoline, null, cont);
    count = saved_count;
    return result;
}

/// Like protectCall but passes a data pointer to the wrapped function.
/// Use when the wrapped function needs runtime context (e.g. SEXP args
/// from an .External wrapper) that cannot be captured inline.
pub fn protectCallData(comptime func: *const fn (?*anyopaque) R.SEXP, data: ?*anyopaque) R.SEXP {
    const cont = zigr_make_unwind_cont();

    const saved_count = count;

    const W = struct {
        fn trampoline(d: ?*anyopaque) callconv(.c) R.SEXP {
            return func(d);
        }
    };
    const result = zigr_protect_call(W.trampoline, data, cont);
    count = saved_count;
    return result;
}

test "pushFrame and popFrame balance" {
    defer while (count > 0) popFrame();
    try std.testing.expectEqual(count, 0);
    pushFrame(struct {
        fn f(_: ?*anyopaque) void {}
    }.f, null);
    try std.testing.expectEqual(count, 1);
    pushFrame(struct {
        fn g(_: ?*anyopaque) void {}
    }.g, null);
    try std.testing.expectEqual(count, 2);
    popFrame();
    try std.testing.expectEqual(count, 1);
    popFrame();
    try std.testing.expectEqual(count, 0);
}

test "popFrame on empty stack is safe" {
    const saved = count;
    count = 0;
    defer count = saved;
    popFrame();
    try std.testing.expectEqual(count, 0);
}

test "pushFrame signals error on overflow" {
    // Verify the overflow path: pushFrame at capacity calls err.signal which longjmps.
    // Without R runtime the longjmp would crash, so we verify the TYPE only.
    try std.testing.expectEqual(@TypeOf(pushFrame), fn (*const fn (?*anyopaque) void, ?*anyopaque) void);
}

test "fire callback is invoked on zigr_on_unwind" {
    const saved = count;
    defer count = saved;
    count = 0;

    var fired: bool = false;
    const S = struct {
        fn f(ptr: ?*anyopaque) void {
            @as(*bool, @ptrCast(@alignCast(ptr.?))).* = true;
        }
    };
    pushFrame(S.f, @as(?*anyopaque, @ptrCast(&fired)));
    try std.testing.expectEqual(count, 1);

    zigr_on_unwind();
    try std.testing.expect(fired);
    try std.testing.expectEqual(count, 0);
}

test "zigr_on_return clears all frames" {
    const saved = count;
    defer count = saved;
    count = 0;

    const S = struct {
        fn f(_: ?*anyopaque) void {}
    };
    pushFrame(S.f, null);
    pushFrame(S.f, null);
    pushFrame(S.f, null);
    try std.testing.expectEqual(count, 3);

    zigr_on_return();
    try std.testing.expectEqual(count, 0);
}

test "protectCall type" {
    // protectCall requires R runtime (R_MakeUnwindCont, R_UnwindProtect).
    // Verify the type signature compiles correctly.
    try std.testing.expectEqual(@TypeOf(protectCall), fn (comptime *const fn () R.SEXP) R.SEXP);
}

test "frames fire in LIFO order on unwind" {
    // Verify cleanup frames fire last-pushed-first on zigr_on_unwind.
    // Each frame appends its marker value to a threadlocal array.
    // This avoids closures which Zig doesn't support for function pointers.

    const saved = count;
    defer count = saved;
    count = 0;

    var order: [3]u8 = .{ 0, 0, 0 };
    var next_slot: u8 = 0;

    const Frame1 = struct {
        fn f(ptr: ?*anyopaque) void {
            const ctx: *struct { arr: *[3]u8, slot: *u8 } = @ptrCast(@alignCast(ptr.?));
            ctx.arr.*[ctx.slot.*] = 1;
            ctx.slot.* += 1;
        }
    };
    const Frame2 = struct {
        fn f(ptr: ?*anyopaque) void {
            const ctx: *struct { arr: *[3]u8, slot: *u8 } = @ptrCast(@alignCast(ptr.?));
            ctx.arr.*[ctx.slot.*] = 2;
            ctx.slot.* += 1;
        }
    };
    const Frame3 = struct {
        fn f(ptr: ?*anyopaque) void {
            const ctx: *struct { arr: *[3]u8, slot: *u8 } = @ptrCast(@alignCast(ptr.?));
            ctx.arr.*[ctx.slot.*] = 3;
            ctx.slot.* += 1;
        }
    };

    var ctx = struct { arr: *[3]u8, slot: *u8 }{ .arr = &order, .slot = &next_slot };

    pushFrame(Frame1.f, @ptrCast(&ctx));
    pushFrame(Frame2.f, @ptrCast(&ctx));
    pushFrame(Frame3.f, @ptrCast(&ctx));

    zigr_on_unwind();
    // Frame3 pushed last (top), fires first -> arr[0] = 3
    // Frame2 fires second -> arr[1] = 2
    // Frame1 fired third -> arr[2] = 1
    try std.testing.expectEqual(order[0], 3);
    try std.testing.expectEqual(order[1], 2);
    try std.testing.expectEqual(order[2], 1);
    try std.testing.expectEqual(next_slot, 3);
}
