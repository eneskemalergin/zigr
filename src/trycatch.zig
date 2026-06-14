//! R-level condition handling.
//!
//! Wraps R_tryCatch so Zig code can call R without crashing on errors.
//! Without this, Rf_error longjmps past Zig's defer/errdefer.

const std = @import("std");
const R = @import("R");
const protect = @import("protect.zig");
const symbols = @import("symbols.zig");
const sexp_mod = @import("sexp.zig");

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
    var classes = protect.scoped(R.Rf_allocVector(R.STRSXP, 1));
    defer classes.deinit();
    R.SET_STRING_ELT(classes.get(), 0, R.Rf_mkChar("condition"));

    const W = struct {
        fn trampoline(_: ?*anyopaque) callconv(.c) R.SEXP {
            return func();
        }
    };

    const result = R.R_tryCatch(
        W.trampoline,
        null,
        classes.get(),
        catchHandler,
        @as(?*anyopaque, @ptrCast(&state)),
        null,
        null,
    );

    if (state.happened) return error.RCondition;
    return result;
}

/// Evaluate a Zig function under R_tryCatch, catching only error
/// conditions. Returns the caught condition SEXP on error (the caller
/// can extract "message" from it via getAttrib).
pub fn tryCatchError(comptime func: *const fn () R.SEXP) RCondition!?R.SEXP {
    var state = HandlerState{ .condition = undefined };
    var classes = protect.scoped(R.Rf_allocVector(R.STRSXP, 1));
    defer classes.deinit();
    R.SET_STRING_ELT(classes.get(), 0, R.Rf_mkChar("error"));

    const W = struct {
        fn trampoline(_: ?*anyopaque) callconv(.c) R.SEXP {
            return func();
        }
    };

    const result = R.R_tryCatch(
        W.trampoline,
        null,
        classes.get(),
        catchHandler,
        @as(?*anyopaque, @ptrCast(&state)),
        null,
        null,
    );

    if (state.happened) return state.condition;
    return result;
}

/// Extract the "message" from a condition SEXP. Returns "" if no
/// message attribute is present.
pub fn extractMessage(cond: R.SEXP) []const u8 {
    const msg_sym = symbols.install("message");
    const msg_sexp = R.Rf_getAttrib(cond, msg_sym);
    if (msg_sexp == R.R_NilValue) return "";
    if (sexp_mod.typeTag(msg_sexp) != 16) return "";
    if (R.XLENGTH(msg_sexp) < 1) return "";
    const elt = R.STRING_ELT(msg_sexp, 0);
    if (elt == R.R_NaString) return "";
    return sexp_mod.charsxpBytes(elt);
}
