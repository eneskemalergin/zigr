//! R condition handling.
//!
//! Conditions can longjmp past Zig defers.

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

fn catchHandler(cond: R.SEXP, data: ?*anyopaque) callconv(.c) R.SEXP {
    const state = @as(*HandlerState, @ptrCast(@alignCast(data.?)));
    state.happened = true;
    state.condition = cond;
    return R.R_NilValue;
}

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

fn messageText(msg_sexp: R.SEXP) []const u8 {
    if (msg_sexp == R.R_NilValue) return "";
    if (sexp_mod.typeTag(msg_sexp) != 16) return "";
    if (R.XLENGTH(msg_sexp) < 1) return "";
    const elt = R.STRING_ELT(msg_sexp, 0);
    if (elt == R.R_NaString) return "";
    return sexp_mod.charsxpBytes(elt);
}

/// R error conditions usually keep `message` as a named list element.
pub fn extractMessage(cond: R.SEXP) []const u8 {
    const msg_sym = symbols.install("message");
    const attr_message = messageText(R.Rf_getAttrib(cond, msg_sym));
    if (attr_message.len != 0) return attr_message;
    if (sexp_mod.typeTag(cond) != 19) return "";

    const names = R.Rf_getAttrib(cond, symbols.install("names"));
    if (sexp_mod.typeTag(names) != 16) return "";
    const len = @min(R.XLENGTH(cond), R.XLENGTH(names));
    for (0..@intCast(len)) |i| {
        const name = R.STRING_ELT(names, @intCast(i));
        if (name != R.R_NaString and std.mem.eql(u8, sexp_mod.charsxpBytes(name), "message")) {
            return messageText(R.VECTOR_ELT(cond, @intCast(i)));
        }
    }
    return "";
}
