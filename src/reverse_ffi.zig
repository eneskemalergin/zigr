//! Calling R functions from Zig.
//!
//! Wraps Rf_eval, Rf_lang2-4, Rf_install, Rf_findVar, Rf_defineVar
//! so Zig code can construct and evaluate R expressions, look up
//! variables, and define new bindings.

const R = @import("R");

// Rf_findVar is declared in Rinternals.h but the translator could not
// resolve it through the macro layer. Call it directly here.
extern fn Rf_findVar(R.SEXP, R.SEXP) R.SEXP;

/// Install a symbol from a Zig string slice.
/// Returns the symbol SEXP for use in lang2/lang3/lang4.
pub fn symbol(name: []const u8) R.SEXP {
    return R.Rf_install(@ptrCast(name.ptr));
}

/// Construct a call with 1 argument: fun(arg1).
pub fn lang2(fun: R.SEXP, arg1: R.SEXP) R.SEXP {
    return R.Rf_lang2(fun, arg1);
}

/// Construct a call with 2 arguments: fun(arg1, arg2).
pub fn lang3(fun: R.SEXP, arg1: R.SEXP, arg2: R.SEXP) R.SEXP {
    return R.Rf_lang3(fun, arg1, arg2);
}

/// Construct a call with 3 arguments: fun(arg1, arg2, arg3).
pub fn lang4(fun: R.SEXP, arg1: R.SEXP, arg2: R.SEXP, arg3: R.SEXP) R.SEXP {
    return R.Rf_lang4(fun, arg1, arg2, arg3);
}

/// Evaluate an R expression in the global environment.
pub fn eval(expr: R.SEXP) R.SEXP {
    return R.Rf_eval(expr, R.R_GlobalEnv);
}

/// Evaluate an R expression in a given environment.
pub fn evalIn(expr: R.SEXP, envir: R.SEXP) R.SEXP {
    return R.Rf_eval(expr, envir);
}

/// Define a variable in the global environment.
pub fn defineVar(name: []const u8, value: R.SEXP) void {
    R.Rf_defineVar(symbol(name), value, R.R_GlobalEnv);
}

/// Define a variable in a given environment.
pub fn defineVarIn(name: []const u8, value: R.SEXP, envir: R.SEXP) void {
    R.Rf_defineVar(symbol(name), value, envir);
}

/// Look up a variable by name in the global environment.
/// Returns R_NilValue if not found (R's missing symbol handling).
pub fn findVar(name: []const u8) R.SEXP {
    return Rf_findVar(symbol(name), R.R_GlobalEnv);
}
