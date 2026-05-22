//! R evaluation and variable lookup from Zig.
//!
//! Rf_eval, Rf_findFun, Rf_applyClosure can longjmp (R errors). All
//! functions here are wrapped in R_UnwindProtect so defer/cleanup runs.

const R = @import("R");
const cleanup = @import("cleanup");
const lang = @import("lang");

const Rf_findFun = @extern(*const fn (R.SEXP, R.SEXP) callconv(.c) R.SEXP, .{ .name = "Rf_findFun" });
const Rf_applyClosure = @extern(*const fn (R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP) callconv(.c) R.SEXP, .{ .name = "Rf_applyClosure" });

// Return R_GlobalEnv when envir is null, otherwise pass through.
fn resolveEnv(envir: ?R.SEXP) R.SEXP {
    return envir orelse R.R_GlobalEnv;
}

/// Evaluate an R expression. Null envir defaults to the global env.
pub fn rEval(expr: R.SEXP, envir: ?R.SEXP) R.SEXP {
    const env = resolveEnv(envir);
    return cleanup.protectCall(struct {
        fn call() R.SEXP {
            return R.Rf_eval(expr, env);
        }
    }.call);
}

/// Look up a variable by symbol. Null envir defaults to the global env.
pub fn findVar(sym: R.SEXP, envir: ?R.SEXP) R.SEXP {
    const env = resolveEnv(envir);
    return cleanup.protectCall(struct {
        fn call() R.SEXP {
            return R.Rf_findVar(sym, env);
        }
    }.call);
}

/// Look up a variable by name in the global environment.
pub fn findVarName(name: []const u8) R.SEXP {
    const sym = lang.symbol(name);
    return findVar(sym, null);
}

/// Look up a function by name in the search path. Errors if not found.
pub fn findFunction(name: []const u8) R.SEXP {
    const sym = lang.symbol(name);
    return cleanup.protectCall(struct {
        fn call() R.SEXP {
            return Rf_findFun(sym, R.R_GlobalEnv);
        }
    }.call);
}

/// Set a variable in an environment (wraps Rf_setVar).
pub fn setVar(sym: R.SEXP, val: R.SEXP, envir: ?R.SEXP) void {
    const env = resolveEnv(envir);
    R.Rf_setVar(sym, val, env);
}

/// Apply a closure with supplied arguments. Low-level interface.
/// `call` is a LANGSXP (function + args), `env` for evaluation,
/// `suppliedargs` pairlist, `parent` env, `rho` calling env.
pub fn applyClosure(call_sxp: R.SEXP, env: R.SEXP, suppliedargs: R.SEXP, parent: R.SEXP, rho: R.SEXP) R.SEXP {
    return cleanup.protectCall(struct {
        fn call() R.SEXP {
            return Rf_applyClosure(call_sxp, env, suppliedargs, parent, rho);
        }
    }.call);
}

/// Execute a function in a top-level context. Returns false if the
/// function longjmps (R error), true on normal return. Useful for
/// calling R code that might error without crashing Zig cleanup.
pub fn topLevelExec(func: *const fn (?*anyopaque) callconv(.c) void, data: ?*anyopaque) bool {
    return R.R_ToplevelExec(func, data) != 0;
}

/// R_BaseEnv constant: the base environment.
pub const baseEnv: R.SEXP = R.R_BaseEnv;

/// R_EmptyEnv constant: the empty environment at the root of the tree.
pub const emptyEnv: R.SEXP = R.R_EmptyEnv;

/// Call an R function by name with positional arguments. Finds the
/// function, builds the call, evaluates, returns the result.
/// Intermediates protected on the normal path; on longjmp the
/// R_UnwindProtect handler handles cleanup.
pub fn call(name: []const u8, args: []const R.SEXP) R.SEXP {
    return cleanup.protectCall(struct {
        fn callFn() R.SEXP {
            const fun_sym = lang.symbol(name);
            _ = R.Rf_protect(fun_sym);

            const fun = Rf_findFun(fun_sym, R.R_GlobalEnv);
            _ = R.Rf_protect(fun);

            var arg_list = R.R_NilValue;
            var i = args.len;
            while (i > 0) {
                i -= 1;
                arg_list = R.Rf_lcons(args[i], arg_list);
                _ = R.Rf_protect(arg_list);
            }

            const call_expr = R.Rf_lcons(fun, arg_list);
            _ = R.Rf_protect(call_expr);

            const result = R.Rf_eval(call_expr, R.R_GlobalEnv);
            R.Rf_unprotect(@intCast(args.len + 2));
            return result;
        }
    }.callFn);
}
