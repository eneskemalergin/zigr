//! R evaluation and variable lookup from Zig.
//!
//! Rf_eval, Rf_findFun, Rf_applyClosure can longjmp (R errors). These
//! wrappers don't allocate Zig memory so longjmp through them is safe.

const R = @import("R");
const lang = @import("lang");

const Rf_findFun = @extern(*const fn (R.SEXP, R.SEXP) callconv(.c) R.SEXP, .{ .name = "Rf_findFun" });
const Rf_applyClosure = @extern(*const fn (R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP) callconv(.c) R.SEXP, .{ .name = "Rf_applyClosure" });
const Rf_findVar = @extern(*const fn (R.SEXP, R.SEXP) callconv(.c) R.SEXP, .{ .name = "Rf_findVar" });

fn resolveEnv(envir: ?R.SEXP) R.SEXP {
    return envir orelse R.R_GlobalEnv;
}

/// Evaluate an R expression. Null envir defaults to the global env.
pub fn rEval(expr: R.SEXP, envir: ?R.SEXP) R.SEXP {
    return R.Rf_eval(expr, resolveEnv(envir));
}

/// Look up a variable by symbol. Null envir defaults to the global env.
pub fn findVar(sym: R.SEXP, envir: ?R.SEXP) R.SEXP {
    return Rf_findVar(sym, resolveEnv(envir));
}

/// Look up a variable by name in the global environment.
pub fn findVarName(name: []const u8) R.SEXP {
    return findVar(lang.symbol(name), null);
}

/// Look up a function by name in the search path. Errors if not found.
pub fn findFunction(name: []const u8) R.SEXP {
    return Rf_findFun(lang.symbol(name), R.R_GlobalEnv);
}

/// Set a variable in an environment (wraps Rf_setVar).
pub fn setVar(sym: R.SEXP, val: R.SEXP, envir: ?R.SEXP) void {
    R.Rf_setVar(sym, val, resolveEnv(envir));
}

/// Apply a closure with supplied arguments. Low-level interface.
pub fn applyClosure(call_sxp: R.SEXP, env: R.SEXP, suppliedargs: R.SEXP, parent: R.SEXP, rho: R.SEXP) R.SEXP {
    return Rf_applyClosure(call_sxp, env, suppliedargs, parent, rho);
}

/// Execute a function in a top-level context. Returns false on longjmp.
pub fn topLevelExec(func: *const fn (?*anyopaque) callconv(.c) void, data: ?*anyopaque) bool {
    return R.R_ToplevelExec(func, data) != 0;
}

pub const baseEnv: R.SEXP = R.R_BaseEnv;
pub const emptyEnv: R.SEXP = R.R_EmptyEnv;

/// Call an R function by name with positional arguments.
pub fn call(name: []const u8, args: []const R.SEXP) R.SEXP {
    const fun_sym = lang.symbol(name);
    const fun = Rf_findFun(fun_sym, R.R_GlobalEnv);

    var arg_list = R.R_NilValue;
    var i = args.len;
    while (i > 0) {
        i -= 1;
        arg_list = R.Rf_lcons(args[i], arg_list);
    }

    const call_expr = R.Rf_lcons(fun, arg_list);
    return R.Rf_eval(call_expr, R.R_GlobalEnv);
}
