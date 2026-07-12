//! R evaluation and lookup.
//!
//! R can longjmp through these calls. Callers holding native state must
//! establish their own cleanup boundary first. Call construction retains
//! ALTREP arguments, but evaluated R code may inspect or materialize them.

const R = @import("R");
const lang = @import("lang.zig");
const protect = @import("protect.zig");
const symbols = @import("symbols.zig");

fn resolveEnv(envir: ?R.SEXP) R.SEXP {
    return envir orelse R.R_GlobalEnv;
}

pub fn rEval(expr: R.SEXP, envir: ?R.SEXP) R.SEXP {
    return R.Rf_eval(expr, resolveEnv(envir));
}

/// An unbound lookup signals an R error instead of returning a sentinel.
pub fn findVar(sym: R.SEXP, envir: ?R.SEXP) R.SEXP {
    const result = R.R_getVar(sym, resolveEnv(envir), 1);
    if (result == R.R_UnboundValue) {
        R.Rf_error("variable not found");
    }
    return result;
}

fn installSym(name: []const u8) R.SEXP {
    return symbols.install(name);
}

pub fn findVarName(name: []const u8) R.SEXP {
    return findVar(installSym(name), null);
}

/// This does not search parent frames.
pub fn findVarInFrame(frame: R.SEXP, name: []const u8) R.SEXP {
    const result = R.R_getVar(installSym(name), frame, 0);
    if (result == R.R_UnboundValue) {
        R.Rf_error("variable not found in frame");
    }
    return result;
}

pub fn findFunction(name: []const u8) R.SEXP {
    return findFunctionIn(name, R.R_GlobalEnv);
}

pub fn findFunctionIn(name: []const u8, envir: R.SEXP) R.SEXP {
    return R.Rf_findFun(installSym(name), envir);
}

pub fn setVar(sym: R.SEXP, val: R.SEXP, envir: ?R.SEXP) void {
    R.Rf_setVar(sym, val, resolveEnv(envir));
}

pub fn defineVar(name: []const u8, value: R.SEXP) void {
    R.Rf_defineVar(installSym(name), value, R.R_GlobalEnv);
}

pub fn defineVarIn(name: []const u8, value: R.SEXP, envir: R.SEXP) void {
    R.Rf_defineVar(installSym(name), value, envir);
}

pub fn topLevelExec(func: *const fn (?*anyopaque) callconv(.c) void, data: ?*anyopaque) bool {
    return R.R_ToplevelExec(func, data) != 0;
}

pub const baseEnv: R.SEXP = R.R_BaseEnv;
pub const emptyEnv: R.SEXP = R.R_EmptyEnv;

pub fn call(name: []const u8, args: []const R.SEXP) R.SEXP {
    return callIn(name, args, R.R_GlobalEnv);
}

/// Function lookup and evaluation both start in `envir`; the result is unprotected.
pub fn callIn(name: []const u8, args: []const R.SEXP, envir: R.SEXP) R.SEXP {
    const fun = findFunctionIn(name, envir);
    var call_expr = protect.scoped(lang.buildCall(fun, args));
    defer call_expr.deinit();
    return R.Rf_eval(call_expr.get(), envir);
}

/// Evaluates an already resolved function with positional arguments.
pub fn callFunctionIn(fun: R.SEXP, args: []const R.SEXP, envir: R.SEXP) lang.CallError!R.SEXP {
    var call_expr = protect.scoped(try lang.buildCallChecked(fun, args));
    defer call_expr.deinit();
    return R.Rf_eval(call_expr.get(), envir);
}

/// Evaluates an already resolved function with positional or tagged arguments.
pub fn callTaggedIn(fun: R.SEXP, args: []const lang.Argument, envir: R.SEXP) lang.CallError!R.SEXP {
    var call_expr = protect.scoped(try lang.buildTaggedCall(fun, args));
    defer call_expr.deinit();
    return R.Rf_eval(call_expr.get(), envir);
}

pub fn tryEval(expr: R.SEXP, envir: R.SEXP) ?R.SEXP {
    var err: c_int = 0;
    const result = R.R_tryEval(expr, envir, &err);
    return if (err != 0) null else result;
}

pub fn tryFindVar(sym: R.SEXP, envir: ?R.SEXP) ?R.SEXP {
    return tryEvalSilent(sym, resolveEnv(envir));
}

pub fn tryFindVarName(name: []const u8) ?R.SEXP {
    return tryFindVar(installSym(name), null);
}

/// Keeps expected lookup failures out of R's error output.
pub fn tryEvalSilent(expr: R.SEXP, envir: R.SEXP) ?R.SEXP {
    var err: c_int = 0;
    const result = R.R_tryEvalSilent(expr, envir, &err);
    return if (err != 0) null else result;
}
