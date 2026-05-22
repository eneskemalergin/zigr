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
    var buf: [4096:0]u8 = undefined;
    const n = @min(code.len, buf.len - 1);
    @memcpy(buf[0..n], code[0..n]);
    buf[n] = 0;
    return R.R_ParseEvalString(&buf, env);
}

/// Evaluate R code via R_ParseEvalString. Semantically identical to
/// rCodeEval; kept as a distinct function for API compatibility.
pub fn rRawEval(code: []const u8, envir: ?R.SEXP) R.SEXP {
    return rCodeEval(code, envir);
}
