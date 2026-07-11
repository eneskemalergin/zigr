//! Evaluate R code from Zig strings.
//!
//! The parser can longjmp, so its buffer uses the cleanup stack.

const std = @import("std");
const R = @import("R");
const cleanup = @import("cleanup");
const err = @import("error");

const FreeBuf = struct {
    fn fire(ptr: ?*anyopaque) void {
        R.R_chk_free(ptr);
    }
};

pub fn rCodeEval(code: []const u8, envir: ?R.SEXP) R.SEXP {
    const env = envir orelse R.R_GlobalEnv;
    // R_chk_free only needs the pointer stored by the cleanup frame.
    const buf = R.R_chk_calloc(code.len + 1, 1) orelse err.signal("out of memory during embedded R evaluation");
    cleanup.pushFrame(FreeBuf.fire, buf);
    const c_buf: [*]u8 = @ptrCast(@as(*anyopaque, @ptrCast(buf)));
    @memcpy(c_buf[0..code.len], code);
    c_buf[code.len] = 0;
    const result = R.R_ParseEvalString(@ptrCast(c_buf), env);
    cleanup.popFrame();
    R.R_chk_free(buf);
    return result;
}

pub const rRawEval = rCodeEval;
