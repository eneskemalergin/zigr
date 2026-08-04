//! Cleanup across R non-local exits.
//!
//! R longjmps bypass Zig defers, so cleanup state lives in this thread-local
//! stack and unwinds in LIFO order. The stack belongs to the R calling thread;
//! it is not a synchronization mechanism for worker-thread R calls.

const std = @import("std");
const builtin = @import("builtin");
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
    armed: bool = false,
    generation: usize = 0,
};

threadlocal var stack: [MAX_NESTING]Frame = undefined;
threadlocal var count: usize = 0;
threadlocal var next_generation: usize = 0;
threadlocal var protect_depth: i32 = 0;
threadlocal var recovering_condition = false;
const diagnostics_enabled = builtin.mode == .Debug or builtin.mode == .ReleaseSafe;

/// Identifies one armed cleanup frame until it is released or unwound.
pub const FrameHandle = struct {
    slot: usize,
    generation: usize,
};

const FrameReservation = struct {
    frame: *Frame,
    handle: FrameHandle,
};

/// High-water marks are disabled in ReleaseFast builds.
pub const DiagnosticSnapshot = struct {
    enabled: bool,
    cleanup_frames: usize,
    unwind_boundaries: usize,
    protect_depth: i32,
    max_cleanup_frames: usize,
    max_unwind_boundaries: usize,
    max_protect_depth: i32,
};

/// Captures the state that an R recovery API must restore after a condition.
pub const RecoveryCheckpoint = struct {
    cleanup_frames: usize,
    protect_depth: i32,
};

threadlocal var max_cleanup_frames: usize = 0;
threadlocal var max_unwind_boundaries: usize = 0;
threadlocal var max_protect_depth: i32 = 0;

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
    if (diagnostics_enabled) max_unwind_boundaries = @max(max_unwind_boundaries, boundary_count);
}

fn reserveFrame() FrameReservation {
    if (count >= MAX_NESTING) err.signal("cleanup stack overflow");

    const slot = count;
    count += 1;
    const generation = next_generation;
    next_generation +%= 1;
    const frame = &stack[slot];
    frame.armed = true;
    frame.generation = generation;
    if (diagnostics_enabled) max_cleanup_frames = @max(max_cleanup_frames, count);
    return .{
        .frame = frame,
        .handle = .{ .slot = slot, .generation = generation },
    };
}

fn discardInactiveFrames() void {
    while (count > 0 and !stack[count - 1].armed) count -= 1;
}

fn finishBoundary(jump: bool) void {
    if (boundary_count == 0) return;
    boundary_count -= 1;
    const boundary = boundaries[boundary_count];
    if (jump) {
        while (count > boundary.frame_count) {
            count -= 1;
            if (stack[count].armed) stack[count].func(stack[count].data);
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

/// Direct ABI and test hook; live R calls use cleanHandler through R_UnwindProtect.
export fn zigr_on_unwind() void {
    while (count > 0) {
        count -= 1;
        if (stack[count].armed) stack[count].func(stack[count].data);
    }
}

/// Direct ABI and test hook; live R calls restore their boundary snapshot in cleanHandler.
export fn zigr_on_return() void {
    count = 0;
}

pub fn pushFrame(func: *const fn (data: ?*anyopaque) void, data: ?*anyopaque) void {
    const reservation = reserveFrame();
    reservation.frame.* = .{
        .func = func,
        .data = data,
        .armed = true,
        .generation = reservation.handle.generation,
    };
}

pub fn popFrame() void {
    if (count == 0) return;
    count -= 1;
    discardInactiveFrames();
}

/// Disarms one exact cleanup frame without disturbing newer live frames.
pub fn releaseFrame(handle: FrameHandle) bool {
    if (handle.slot >= count) return false;
    const frame = &stack[handle.slot];
    if (!frame.armed or frame.generation != handle.generation) return false;
    frame.armed = false;
    discardInactiveFrames();
    return true;
}

pub fn frameIsActive(handle: FrameHandle) bool {
    if (handle.slot >= count) return false;
    const frame = &stack[handle.slot];
    return frame.armed and frame.generation == handle.generation;
}

pub fn recoveryCheckpoint() RecoveryCheckpoint {
    return .{
        .cleanup_frames = count,
        .protect_depth = protect_depth,
    };
}

/// Fires cleanup registered after `checkpoint` and restores tracked protection depth.
pub fn rollbackRecovery(checkpoint: RecoveryCheckpoint) void {
    recovering_condition = true;
    defer recovering_condition = false;
    while (count > checkpoint.cleanup_frames) {
        count -= 1;
        if (stack[count].armed) stack[count].func(stack[count].data);
    }
    protect_depth = checkpoint.protect_depth;
}

/// R has already restored its protection stack while recovery cleanup is running.
pub fn isRecoveringCondition() bool {
    return recovering_condition;
}

/// Reserve room before an operation that may register an internal frame before
/// the caller registers ownership for the operation's native allocation.
pub fn requireCapacity(frames: usize) void {
    if (frames > MAX_NESTING) err.signal("cleanup stack overflow");
    if (count > MAX_NESTING - frames) err.signal("cleanup stack overflow");
}

pub fn getProtectDepth() i32 {
    return protect_depth;
}

pub fn adjustProtectDepth(delta: i32) void {
    protect_depth += delta;
    if (diagnostics_enabled) max_protect_depth = @max(max_protect_depth, protect_depth);
}

/// Starts a new high-water interval at the current cleanup state.
pub fn resetDiagnostics() void {
    if (!diagnostics_enabled) return;
    max_cleanup_frames = count;
    max_unwind_boundaries = boundary_count;
    max_protect_depth = protect_depth;
}

/// Returns zero-valued, disabled diagnostics in ReleaseFast builds.
pub fn diagnosticSnapshot() DiagnosticSnapshot {
    if (!diagnostics_enabled) return .{
        .enabled = false,
        .cleanup_frames = 0,
        .unwind_boundaries = 0,
        .protect_depth = 0,
        .max_cleanup_frames = 0,
        .max_unwind_boundaries = 0,
        .max_protect_depth = 0,
    };
    return .{
        .enabled = true,
        .cleanup_frames = count,
        .unwind_boundaries = boundary_count,
        .protect_depth = protect_depth,
        .max_cleanup_frames = max_cleanup_frames,
        .max_unwind_boundaries = max_unwind_boundaries,
        .max_protect_depth = max_protect_depth,
    };
}

/// Inline state survives longjmp and must fit the frame's fixed storage.
pub fn pushFrameInline(
    comptime T: type,
    value: T,
    comptime fireFn: *const fn (data: *T) void,
) *T {
    return pushFrameInlineWithHandle(T, value, fireFn).state;
}

/// Registers inline cleanup state and returns the handle that releases it.
pub fn pushFrameInlineWithHandle(
    comptime T: type,
    value: T,
    comptime fireFn: *const fn (data: *T) void,
) struct { state: *T, handle: FrameHandle } {
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

    const reservation = reserveFrame();
    const frame = reservation.frame;
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
    return .{ .state = slot, .handle = reservation.handle };
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

test "cleanup diagnostics record high-water marks in safe builds" {
    if (!diagnostics_enabled) return;
    const saved = count;
    defer count = saved;
    count = 0;
    resetDiagnostics();
    const Guard = struct {
        fn fire(_: ?*anyopaque) void {}
    };

    pushFrame(Guard.fire, null);
    pushFrame(Guard.fire, null);
    adjustProtectDepth(3);
    const snapshot = diagnosticSnapshot();
    adjustProtectDepth(-3);
    popFrame();
    popFrame();

    try std.testing.expect(snapshot.enabled);
    try std.testing.expectEqual(2, snapshot.max_cleanup_frames);
    try std.testing.expectEqual(3, snapshot.max_protect_depth);
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

test "releaseFrame disarms an exact frame" {
    const saved = count;
    defer count = saved;
    count = 0;

    const Guard = struct {
        fired: *usize,

        fn fire(self: *@This()) void {
            self.fired.* += 1;
        }
    };

    var first_fired: usize = 0;
    var second_fired: usize = 0;
    const first = pushFrameInlineWithHandle(Guard, .{ .fired = &first_fired }, Guard.fire);
    _ = pushFrameInlineWithHandle(Guard, .{ .fired = &second_fired }, Guard.fire);

    try std.testing.expect(frameIsActive(first.handle));
    try std.testing.expect(releaseFrame(first.handle));
    try std.testing.expect(!frameIsActive(first.handle));
    zigr_on_unwind();

    try std.testing.expectEqual(0, first_fired);
    try std.testing.expectEqual(1, second_fired);
    try std.testing.expectEqual(0, count);

    const replacement = pushFrameInlineWithHandle(Guard, .{ .fired = &first_fired }, Guard.fire);
    try std.testing.expect(!releaseFrame(first.handle));
    try std.testing.expect(frameIsActive(replacement.handle));
    try std.testing.expect(releaseFrame(replacement.handle));
    try std.testing.expect(!frameIsActive(replacement.handle));
}
