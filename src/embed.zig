//! Evaluate R code from Zig strings.
//!
//! This is a main-thread-only R API entry. The parser can longjmp, so its
//! buffer uses the cleanup stack. The environment is borrowed for the call and
//! returned results are unprotected; callers must root them before allocating.

const std = @import("std");
const R = @import("R");
const cleanup = @import("cleanup");
const err = @import("error");

const FreeBuf = struct {
    ptr: ?*anyopaque = null,

    fn fire(self: *@This()) void {
        if (self.ptr) |ptr| R.R_chk_free(ptr);
    }
};

pub fn rCodeEval(code: []const u8, envir: ?R.SEXP) R.SEXP {
    const Context = struct {
        code: []const u8,
        env: R.SEXP,
    };
    var context = Context{ .code = code, .env = envir orelse R.R_GlobalEnv };

    return cleanup.protectCallData(struct {
        fn call(raw: ?*anyopaque) R.SEXP {
            const ctx: *Context = @ptrCast(@alignCast(raw.?));
            const state = cleanup.pushFrameInline(FreeBuf, .{}, FreeBuf.fire);
            const buf = R.R_chk_calloc(ctx.code.len + 1, 1) orelse err.signal("out of memory during embedded R evaluation");
            state.ptr = buf;
            const c_buf: [*]u8 = @ptrCast(@as(*anyopaque, @ptrCast(buf)));
            @memcpy(c_buf[0..ctx.code.len], ctx.code);
            c_buf[ctx.code.len] = 0;
            const result = R.R_ParseEvalString(@ptrCast(c_buf), ctx.env);
            cleanup.popFrame();
            R.R_chk_free(buf);
            return result;
        }
    }.call, @ptrCast(&context));
}

pub const rRawEval = rCodeEval;
