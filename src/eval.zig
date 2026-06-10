//! R evaluation and variable lookup from Zig.
//!
//! Rf_eval, Rf_findFun can longjmp (R errors). These wrappers don't
//! allocate Zig memory so longjmp through them is safe.
//! Uses R 4.6 API functions (R_getVar) instead of non-API Rf_findVar.
//!
//! Names are self-explanatory (rEval, findVar, setVar, defineVar, call)
//! so thin wrappers omit doc comments.

const std = @import("std");
const R = @import("R");
const lang = @import("lang.zig");
const symbols = @import("symbols.zig");

fn resolveEnv(envir: ?R.SEXP) R.SEXP {
    return envir orelse R.R_GlobalEnv;
}

/// envir defaults to R_GlobalEnv when null.
pub fn rEval(expr: R.SEXP, envir: ?R.SEXP) R.SEXP {
    return R.Rf_eval(expr, resolveEnv(envir));
}

/// envir defaults to R_GlobalEnv when null.
pub fn findVar(sym: R.SEXP, envir: ?R.SEXP) R.SEXP {
    return R.R_getVar(sym, resolveEnv(envir), 1);
}

fn installSym(name: []const u8) R.SEXP {
    return symbols.install(name);
}

pub fn findVarName(name: []const u8) R.SEXP {
    return findVar(installSym(name), null);
}

/// Searches only the given frame, not the parent chain (inherits=FALSE).
pub fn findVarInFrame(frame: R.SEXP, name: []const u8) R.SEXP {
    return R.R_getVar(installSym(name), frame, 0);
}

pub fn findFunction(name: []const u8) R.SEXP {
    return R.Rf_findFun(installSym(name), R.R_GlobalEnv);
}

/// setVar assigns to an existing binding; defineVar creates a new one. envir defaults to R_GlobalEnv when null.
pub fn setVar(sym: R.SEXP, val: R.SEXP, envir: ?R.SEXP) void {
    R.Rf_setVar(sym, val, resolveEnv(envir));
}

pub fn defineVar(name: []const u8, value: R.SEXP) void {
    R.Rf_defineVar(installSym(name), value, R.R_GlobalEnv);
}

pub fn defineVarIn(name: []const u8, value: R.SEXP, envir: R.SEXP) void {
    R.Rf_defineVar(installSym(name), value, envir);
}

/// Wraps R_ToplevelExec. Returns false if the wrapped function longjmps.
pub fn topLevelExec(func: *const fn (?*anyopaque) callconv(.c) void, data: ?*anyopaque) bool {
    return R.R_ToplevelExec(func, data) != 0;
}

pub const baseEnv: R.SEXP = R.R_BaseEnv;
pub const emptyEnv: R.SEXP = R.R_EmptyEnv;

/// Looks up function by name, builds call expression, evaluates it.
pub fn call(name: []const u8, args: []const R.SEXP) R.SEXP {
    const fun = R.Rf_findFun(installSym(name), R.R_GlobalEnv);
    const call_expr = lang.buildCall(fun, args);
    return R.Rf_eval(call_expr, R.R_GlobalEnv);
}

/// Returns null if an error is signaled, instead of longjmp-ing.
pub fn tryEval(expr: R.SEXP, envir: R.SEXP) ?R.SEXP {
    var err: c_int = 0;
    const result = R.R_tryEval(expr, envir, &err);
    return if (err != 0) null else result;
}

/// Uses R_tryEval internally; returns null if the variable is missing. envir defaults to R_GlobalEnv when null.
pub fn tryFindVar(sym: R.SEXP, envir: ?R.SEXP) ?R.SEXP {
    return tryEval(sym, resolveEnv(envir));
}

pub fn tryFindVarName(name: []const u8) ?R.SEXP {
    return tryFindVar(installSym(name), null);
}

/// Like tryEval but suppresses the error message printed to stderr.
pub fn tryEvalSilent(expr: R.SEXP, envir: R.SEXP) ?R.SEXP {
    var err: c_int = 0;
    const result = R.R_tryEvalSilent(expr, envir, &err);
    return if (err != 0) null else result;
}
