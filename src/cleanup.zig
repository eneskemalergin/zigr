//! Thread-local cleanup stack and R_UnwindProtect bridge.
//!
//! R errors use C longjmp, which bypasses Zig's defer/errdefer.
//! This module wraps R_UnwindProtect and manages a thread-local stack
//! of cleanup frames that fire when R unwinds.

const std = @import("std");
const R = @import("R");

const MAX_NESTING = 16;

const Frame = struct {
    func: *const fn (data: ?*anyopaque) void,
    data: ?*anyopaque,
};

threadlocal var stack: [MAX_NESTING]Frame = undefined;
threadlocal var count: usize = 0;

// Thread-local to match the frame stack they manage.
threadlocal var on_unwind: *const fn () callconv(.c) void = zigr_on_unwind;
threadlocal var on_return: *const fn () callconv(.c) void = zigr_on_return;

const noop = struct {
    fn nop() callconv(.c) void {}
}.nop;

fn signalError(msg: []const u8) noreturn {
    var buf: [256:0]u8 = undefined;
    const n = @min(msg.len, buf.len - 1);
    if (n > 0) @memcpy(buf[0..n], msg[0..n]);
    buf[n] = 0;
    R.Rf_error(&buf);
}

fn clean_handler(_data: ?*anyopaque, jump: R.Rboolean) callconv(.c) void {
    _ = _data;
    if (jump == 1) on_unwind() else on_return();
}

/// Register the Zig-side callbacks that the C clean handler calls
/// on unwind (R longjmp) and normal return. Called once by init().
export fn zigr_set_unwind_handlers(
    cb_on_unwind: *const fn () callconv(.c) void,
    cb_on_return: *const fn () callconv(.c) void,
) void {
    on_unwind = cb_on_unwind;
    on_return = cb_on_return;
}

export fn zigr_make_unwind_cont() R.SEXP {
    return R.R_MakeUnwindCont();
}

export fn zigr_protect_call(
    fun: *const fn (?*anyopaque) callconv(.c) R.SEXP,
    data: ?*anyopaque,
    cont: R.SEXP,
) R.SEXP {
    return R.R_UnwindProtect(fun, data, clean_handler, null, cont);
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

pub fn init() void {
    zigr_set_unwind_handlers(zigr_on_unwind, zigr_on_return);
}

pub fn pushFrame(func: *const fn (data: ?*anyopaque) void, data: ?*anyopaque) void {
    if (count >= MAX_NESTING) signalError("cleanup stack overflow");
    stack[count] = .{ .func = func, .data = data };
    count += 1;
}

/// Remove the most recently pushed frame without firing its callback.
/// Must be called after every pushFrame on the normal return path.
pub fn popFrame() void {
    if (count > 0) count -= 1;
}

/// Call a Zig function inside an R_UnwindProtect guard.
/// The continuation token is preserved via R_PreserveObject. On unwind
/// a cleanup frame releases it. On normal return defer + popFrame
/// handle the release.
pub fn protectCall(comptime func: *const fn () R.SEXP) R.SEXP {
    const cont = zigr_make_unwind_cont();
    R.R_PreserveObject(cont);

    // Save count before pushing our frame. On normal return,
    // zigr_on_return clears every frame. We restore caller
    // frames so their popFrame calls still work.
    const saved_count = count;
    const Release = struct {
        fn release(ptr: ?*anyopaque) void {
            R.R_ReleaseObject(@as(R.SEXP, @ptrCast(ptr)));
        }
    };
    pushFrame(Release.release, @as(?*anyopaque, @ptrCast(cont)));
    defer R.R_ReleaseObject(cont);
    defer count = saved_count;

    const W = struct {
        fn trampoline(_: ?*anyopaque) callconv(.c) R.SEXP {
            return func();
        }
    };
    return zigr_protect_call(W.trampoline, null, cont);
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
    count = 0;
    popFrame();
    try std.testing.expectEqual(count, 0);
}
