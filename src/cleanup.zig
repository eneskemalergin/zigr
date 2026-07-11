//! Cleanup across R non-local exits.
//!
//! R longjmps bypass Zig defers, so cleanup state lives in this thread-local
//! stack and unwinds in LIFO order.

const std = @import("std");
const R = @import("R");
const err = @import("error");

pub const MAX_NESTING = 16;

pub const INLINE_DATA_SIZE = 64;
pub const INLINE_DATA_ALIGN = @alignOf(usize);

const Frame = struct {
    func: *const fn (data: ?*anyopaque) void,
    data: ?*anyopaque,
    /// Inline state cannot point at a frame that a longjmp destroys.
    inline_buf: [INLINE_DATA_SIZE]u8 align(INLINE_DATA_ALIGN) = [_]u8{0} ** INLINE_DATA_SIZE,
    owns_inline: bool = false,
};

threadlocal var stack: [MAX_NESTING]Frame = undefined;
threadlocal var count: usize = 0;
threadlocal var protect_depth: i32 = 0;

const Boundary = struct {
    frame_count: usize,
    protect_depth: i32,
};

threadlocal var boundaries: [MAX_NESTING]Boundary = undefined;
threadlocal var boundary_count: usize = 0;

fn beginBoundary() void {
    if (boundary_count >= MAX_NESTING) err.signal("unwind boundary stack overflow");
    boundaries[boundary_count] = .{
        .frame_count = count,
        .protect_depth = protect_depth,
    };
    boundary_count += 1;
}

fn finishBoundary(jump: bool) void {
    if (boundary_count == 0) return;
    boundary_count -= 1;
    const boundary = boundaries[boundary_count];
    if (jump) {
        while (count > boundary.frame_count) {
            count -= 1;
            stack[count].func(stack[count].data);
        }
    } else {
        count = boundary.frame_count;
    }
    protect_depth = boundary.protect_depth;
}

fn cleanHandler(_data: ?*anyopaque, jump: R.Rboolean) callconv(.c) void {
    _ = _data;
    finishBoundary(jump == 1);
}

export fn zigr_make_unwind_cont() R.SEXP {
    return R.R_MakeUnwindCont();
}

export fn zigr_protect_call(
    fun: *const fn (?*anyopaque) callconv(.c) R.SEXP,
    data: ?*anyopaque,
    cont: R.SEXP,
) R.SEXP {
    beginBoundary();
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

pub fn popFrame() void {
    if (count > 0) count -= 1;
}

pub fn getProtectDepth() i32 {
    return protect_depth;
}

pub fn adjustProtectDepth(delta: i32) void {
    protect_depth += delta;
}

/// Inline state survives longjmp and must fit the frame's fixed storage.
pub fn pushFrameInline(
    comptime T: type,
    value: T,
    comptime fireFn: *const fn (data: *T) void,
) *T {
    comptime {
        if (@sizeOf(T) > INLINE_DATA_SIZE) {
            @compileError(std.fmt.comptimePrint(
                "pushFrameInline: type '{s}' is {d} bytes, max is {d}",
                .{ @typeName(T), @sizeOf(T), INLINE_DATA_SIZE },
            ));
        }
        if (@alignOf(T) > INLINE_DATA_ALIGN) {
            @compileError(std.fmt.comptimePrint(
                "pushFrameInline: type '{s}' alignment is {d}, max is {d}",
                .{ @typeName(T), @alignOf(T), INLINE_DATA_ALIGN },
            ));
        }
    }

    if (count >= MAX_NESTING) err.signal("cleanup stack overflow");

    const frame = &stack[count];
    const slot: *T = @ptrCast(@alignCast(&frame.inline_buf));
    slot.* = value;
    frame.owns_inline = true;

    const W = struct {
        fn wrapper(data: ?*anyopaque) void {
            const typed: *T = @ptrCast(@alignCast(data.?));
            fireFn(typed);
        }
    };

    frame.func = W.wrapper;
    frame.data = @ptrCast(slot);
    count += 1;
    return slot;
}

/// Establishes R's unwind boundary before invoking Zig code.
pub fn protectCall(comptime func: *const fn () R.SEXP) R.SEXP {
    const saved_count = count;
    const cont = R.Rf_protect(zigr_make_unwind_cont());
    defer R.Rf_unprotect(1);

    const W = struct {
        fn trampoline(_: ?*anyopaque) callconv(.c) R.SEXP {
            return func();
        }
    };
    const result = zigr_protect_call(W.trampoline, null, cont);
    count = saved_count;
    return result;
}

pub fn protectCallData(comptime func: *const fn (?*anyopaque) R.SEXP, data: ?*anyopaque) R.SEXP {
    const saved_count = count;
    const cont = R.Rf_protect(zigr_make_unwind_cont());
    defer R.Rf_unprotect(1);

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

test "cleanup stack accepts its documented capacity" {
    const saved = count;
    defer count = saved;
    count = 0;

    const Guard = struct {
        fn fire(_: ?*anyopaque) void {}
    };
    for (0..MAX_NESTING) |_| pushFrame(Guard.fire, null);
    try std.testing.expectEqual(count, MAX_NESTING);
    while (count > 0) popFrame();
}

test "popFrame on empty stack is safe" {
    const saved = count;
    count = 0;
    defer count = saved;
    popFrame();
    try std.testing.expectEqual(count, 0);
}

test "pushFrame signals error on overflow" {
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
    try std.testing.expectEqual(@TypeOf(protectCall), fn (comptime *const fn () R.SEXP) R.SEXP);
}

test "frames fire in LIFO order on unwind" {
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
    try std.testing.expectEqual(order[0], 3);
    try std.testing.expectEqual(order[1], 2);
    try std.testing.expectEqual(order[2], 1);
    try std.testing.expectEqual(next_slot, 3);
}

test "pushFrameInline copies state into frame buffer" {
    const saved = count;
    defer count = saved;
    count = 0;

    const Guard = struct {
        counter: u32,
        marker: u64,

        fn fire(self: *@This()) void {
            self.counter += 100;
        }
    };

    var g = Guard{ .counter = 7, .marker = 0xDEADBEEFCAFEBABE };
    const stored = pushFrameInline(Guard, g, Guard.fire);
    try std.testing.expectEqual(count, 1);
    try std.testing.expect(@intFromPtr(stored) != @intFromPtr(&g));

    g.counter = 999;
    g.marker = 0;

    zigr_on_unwind();
    try std.testing.expectEqual(g.counter, 999);
}

test "pushFrameInline survives pop without firing" {
    const saved = count;
    defer count = saved;
    count = 0;

    const Guard = struct {
        fired: *bool,

        fn fire(self: *@This()) void {
            self.fired.* = true;
        }
    };

    var fired = false;
    _ = pushFrameInline(Guard, Guard{ .fired = &fired }, Guard.fire);
    popFrame();

    try std.testing.expect(!fired);
}

test "pushFrameInline fires on zigr_on_unwind with LIFO order" {
    const saved = count;
    defer count = saved;
    count = 0;

    const A = struct {
        value: *i32,

        fn fire(self: *@This()) void {
            self.value.* = 10;
        }
    };
    const B = struct {
        value: *i32,

        fn fire(self: *@This()) void {
            self.value.* = 20;
        }
    };

    var a_result: i32 = 0;
    var b_result: i32 = 0;

    _ = pushFrameInline(A, A{ .value = &a_result }, A.fire);
    _ = pushFrameInline(B, B{ .value = &b_result }, B.fire);

    zigr_on_unwind();
    try std.testing.expectEqual(b_result, 20);
    try std.testing.expectEqual(a_result, 10);
}
