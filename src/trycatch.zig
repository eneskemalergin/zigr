//! R-level condition handling.
//!
//! Wraps R_tryCatch so Zig code can call R without crashing on errors.
//! Without this, Rf_error longjmps past Zig's defer/errdefer.

const std = @import("std");
const R = @import("R");

pub const RCondition = error{RCondition};

const HandlerState = struct {
    happened: bool = false,
    condition: R.SEXP,
};

// C-callable handler that R_tryCatch calls when a condition is signaled.
fn catchHandler(cond: R.SEXP, data: ?*anyopaque) callconv(.c) R.SEXP {
    const state = @as(*HandlerState, @ptrCast(@alignCast(data.?)));
    state.happened = true;
    state.condition = cond;
    return R.R_NilValue;
}

/// Evaluate a Zig function under R_tryCatch, catching all conditions.
/// Returns error.RCondition if any condition was signaled.
pub fn tryCatch(comptime func: *const fn () R.SEXP) RCondition!R.SEXP {
    var state = HandlerState{ .condition = undefined };
    const classes = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    R.SET_STRING_ELT(classes, 0, R.Rf_mkChar("condition"));

    const W = struct {
        fn trampoline(_: ?*anyopaque) callconv(.c) R.SEXP {
            return func();
        }
    };

    const result = R.R_tryCatch(
        W.trampoline,
        null,
        classes,
        catchHandler,
        @as(?*anyopaque, @ptrCast(&state)),
        null,
        null,
    );

    R.Rf_unprotect(1);
    if (state.happened) return error.RCondition;
    return result;
}

/// Evaluate a Zig function under R_tryCatch, catching only error
/// conditions. Returns the caught condition SEXP on error (the caller
/// can extract "message" from it via getAttrib).
pub fn tryCatchError(comptime func: *const fn () R.SEXP) RCondition!?R.SEXP {
    var state = HandlerState{ .condition = undefined };
    const classes = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    R.SET_STRING_ELT(classes, 0, R.Rf_mkChar("error"));

    const W = struct {
        fn trampoline(_: ?*anyopaque) callconv(.c) R.SEXP {
            return func();
        }
    };

    const result = R.R_tryCatch(
        W.trampoline,
        null,
        classes,
        catchHandler,
        @as(?*anyopaque, @ptrCast(&state)),
        null,
        null,
    );

    R.Rf_unprotect(1);
    if (state.happened) return state.condition;
    return result;
}

/// Extract the "message" from a condition SEXP. Returns "" if no
/// message attribute is present.
pub fn extractMessage(cond: R.SEXP) []const u8 {
    const msg_sym = R.Rf_install("message");
    const msg_sexp = R.Rf_getAttrib(cond, msg_sym);
    if (msg_sexp == R.R_NilValue) return "";
    const elt = R.STRING_ELT(msg_sexp, 0);
    if (elt == R.R_NaString) return "";
    return std.mem.sliceTo(R.R_CHAR(elt), 0);
}
