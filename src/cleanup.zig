//! Thread-local cleanup stack and R_UnwindProtect bridge.
//!
//! R errors use C longjmp, which bypasses Zig's defer/errdefer.
//! This module wraps R_UnwindProtect and manages a thread-local stack
//! of cleanup frames that fire when R unwinds.

const R = @import("R");

const MAX_NESTING = 64;

const Frame = struct {
    func: *const fn (data: ?*anyopaque) void,
    data: ?*anyopaque,
};

threadlocal var stack: [MAX_NESTING]Frame = undefined;
threadlocal var count: usize = 0;

var on_unwind: ?*const fn () callconv(.c) void = null;
var on_return: ?*const fn () callconv(.c) void = null;

fn clean_handler(data: ?*anyopaque, jump: R.Rboolean) callconv(.c) void {
    _ = data;
    if (jump == 1) {
        if (on_unwind) |cb| cb();
    } else {
        if (on_return) |cb| cb();
    }
}

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
    if (count >= MAX_NESTING) @panic("cleanup stack overflow");
    stack[count] = .{ .func = func, .data = data };
    count += 1;
}

pub fn popFrame() void {
    if (count > 0) count -= 1;
}

pub fn protectCall(comptime func: *const fn () R.SEXP) R.SEXP {
    const cont = zigr_make_unwind_cont();
    const W = struct {
        fn trampoline(_: ?*anyopaque) callconv(.c) R.SEXP {
            return func();
        }
    };
    return zigr_protect_call(W.trampoline, null, cont);
}
