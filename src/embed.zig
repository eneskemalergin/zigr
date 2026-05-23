//! R code evaluation from Zig strings.
//!
//! Wraps R_ParseEvalString so Zig code can run arbitrary R expressions
//! without constructing call nodes manually. No arena needed: embed
//! functions don't allocate Zig memory.

const R = @import("R");

/// Parse and evaluate an R expression from a Zig string.
/// Returns the result SEXP. Wraps R_ParseEvalString.
pub fn rCodeEval(code: []const u8, envir: ?R.SEXP) R.SEXP {
    const env = envir orelse R.R_GlobalEnv;
    const buf = R.R_chk_calloc(code.len + 1, 1) orelse @panic("OOM");
    defer R.R_chk_free(buf);
    const c_buf: [*]u8 = @ptrCast(@as(*anyopaque, @ptrCast(buf.?)));
    @memcpy(c_buf[0..code.len], code);
    c_buf[code.len] = 0;
    return R.R_ParseEvalString(@ptrCast(c_buf), env);
}

/// Evaluate R code via R_ParseEvalString. Semantically identical to
/// rCodeEval; kept as a distinct function for API compatibility.
pub fn rRawEval(code: []const u8, envir: ?R.SEXP) R.SEXP {
    return rCodeEval(code, envir);
}
