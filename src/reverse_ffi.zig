//! Calling R functions from Zig.
//!
//! Wraps Rf_eval, Rf_lang2-6, Rf_install, Rf_findVar, Rf_findVarInFrame,
//! Rf_defineVar, R_tryEval, R_tryEvalSilent so Zig code can construct and
//! evaluate R expressions, look up variables, and define new bindings.

const std = @import("std");
const R = @import("R");

const Rf_findVar = @extern(*const fn (R.SEXP, R.SEXP) callconv(.c) R.SEXP, .{ .name = "Rf_findVar" });

/// Install a symbol from a Zig string slice.
/// Returns the symbol SEXP for use in lang2/lang3/lang4.
/// Results are cached (R symbols live forever, safe to reuse the SEXP).
pub fn symbol(name: []const u8) R.SEXP {
    const S = struct {
        var cache: [64]struct {
            hash: u64,
            name: [256:0]u8,
            len: usize,
            sexp: R.SEXP,
        } = undefined;
        var count: usize = 0;
    };

    const hash = std.hash.Wyhash.hash(0, name);

    for (0..S.count) |i| {
        if (S.cache[i].hash == hash and S.cache[i].len == name.len and
            std.mem.eql(u8, S.cache[i].name[0..name.len], name))
        {
            return S.cache[i].sexp;
        }
    }

    var buf: [256:0]u8 = undefined;
    const n = @min(name.len, buf.len - 1);
    @memcpy(buf[0..n], name[0..n]);
    buf[n] = 0;
    const sexp = R.Rf_install(@ptrCast(&buf));

    if (S.count < S.cache.len) {
        const cn = @min(name.len, S.cache[0].name.len - 1);
        @memcpy(S.cache[S.count].name[0..cn], name[0..cn]);
        S.cache[S.count].name[cn] = 0;
        S.cache[S.count].hash = hash;
        S.cache[S.count].len = cn;
        S.cache[S.count].sexp = sexp;
        S.count += 1;
    }

    return sexp;
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

const Rf_findVarInFrame = @extern(*const fn (R.SEXP, R.SEXP) callconv(.c) R.SEXP, .{ .name = "Rf_findVarInFrame" });

/// Look up a variable in a specific environment frame without searching
/// the parent chain.
pub fn findVarInFrame(frame: R.SEXP, name: []const u8) R.SEXP {
    return Rf_findVarInFrame(frame, symbol(name));
}

/// Construct a call with 4 arguments: fun(arg1, arg2, arg3, arg4).
pub fn lang5(fun: R.SEXP, arg1: R.SEXP, arg2: R.SEXP, arg3: R.SEXP, arg4: R.SEXP) R.SEXP {
    return R.Rf_lang5(fun, arg1, arg2, arg3, arg4);
}

/// Construct a call with 5 arguments: fun(arg1, arg2, arg3, arg4, arg5).
pub fn lang6(fun: R.SEXP, arg1: R.SEXP, arg2: R.SEXP, arg3: R.SEXP, arg4: R.SEXP, arg5: R.SEXP) R.SEXP {
    return R.Rf_lang6(fun, arg1, arg2, arg3, arg4, arg5);
}

/// Evaluate an R expression and catch errors. Returns null if an error
/// was signaled, otherwise the result SEXP.
pub fn tryEval(expr: R.SEXP, envir: R.SEXP) ?R.SEXP {
    var err: c_int = 0;
    const result = R.R_tryEval(expr, envir, &err);
    return if (err != 0) null else result;
}

/// Evaluate an R expression and catch errors silently (no error printed).
pub fn tryEvalSilent(expr: R.SEXP, envir: R.SEXP) ?R.SEXP {
    var err: c_int = 0;
    const result = R.R_tryEvalSilent(expr, envir, &err);
    return if (err != 0) null else result;
}
