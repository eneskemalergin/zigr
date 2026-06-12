//! R code evaluation from Zig strings.
//!
//! Wraps R_ParseEvalString so Zig code can run arbitrary R expressions
//! without constructing call nodes manually. Uses c_allocator for the
//! buffer; frees via R_chk_free (pointer-only, no length needed) on the
//! cleanup path since the cleanup frame only carries a ?*anyopaque.

const std = @import("std");
const R = @import("R");
const cleanup = @import("cleanup");
const err = @import("error");

const FreeBuf = struct {
    fn fire(ptr: ?*anyopaque) void {
        R.R_chk_free(ptr);
    }
};

/// Parse and evaluate an R expression from a Zig string.
/// Returns the result SEXP. Wraps R_ParseEvalString.
pub fn rCodeEval(code: []const u8, envir: ?R.SEXP) R.SEXP {
    const env = envir orelse R.R_GlobalEnv;
    // Use R_chk_calloc for the longer-lived buffer because the cleanup
    // frame only carries a pointer (no length). R_chk_free handles it.
    // This matches the CRAN memory tracking when used in packages.
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

/// Evaluate R code via R_ParseEvalString. Semantically identical to
/// rCodeEval; kept as a distinct function for API compatibility.
pub const rRawEval = rCodeEval;
