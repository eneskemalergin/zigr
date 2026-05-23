//! R evaluation and variable lookup from Zig.
//!
//! Rf_eval, Rf_findFun can longjmp (R errors). These wrappers don't
//! allocate Zig memory so longjmp through them is safe.

const std = @import("std");
const R = @import("R");

const Rf_findFun = @extern(*const fn (R.SEXP, R.SEXP) callconv(.c) R.SEXP, .{ .name = "Rf_findFun" });
const Rf_findVar = @extern(*const fn (R.SEXP, R.SEXP) callconv(.c) R.SEXP, .{ .name = "Rf_findVar" });
const Rf_findVarInFrame = @extern(*const fn (R.SEXP, R.SEXP) callconv(.c) R.SEXP, .{ .name = "Rf_findVarInFrame" });

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

fn installSym(name: []const u8) R.SEXP {
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
    const sxp = R.Rf_install(@ptrCast(&buf));
    if (S.count < S.cache.len) {
        const cn = @min(name.len, S.cache[0].name.len - 1);
        @memcpy(S.cache[S.count].name[0..cn], name[0..cn]);
        S.cache[S.count].name[cn] = 0;
        S.cache[S.count].hash = hash;
        S.cache[S.count].len = cn;
        S.cache[S.count].sexp = sxp;
        S.count += 1;
    }
    return sxp;
}

/// Look up a variable by name in the global environment.
pub fn findVarName(name: []const u8) R.SEXP {
    return findVar(installSym(name), null);
}

/// Look up a variable in a specific environment frame without searching
/// the parent chain.
pub fn findVarInFrame(frame: R.SEXP, name: []const u8) R.SEXP {
    return Rf_findVarInFrame(frame, installSym(name));
}

/// Look up a function by name in the search path. Errors if not found.
pub fn findFunction(name: []const u8) R.SEXP {
    return Rf_findFun(installSym(name), R.R_GlobalEnv);
}

/// Set a variable in an environment (wraps Rf_setVar).
pub fn setVar(sym: R.SEXP, val: R.SEXP, envir: ?R.SEXP) void {
    R.Rf_setVar(sym, val, resolveEnv(envir));
}

/// Define a variable in the global environment.
pub fn defineVar(name: []const u8, value: R.SEXP) void {
    R.Rf_defineVar(installSym(name), value, R.R_GlobalEnv);
}

/// Define a variable in a given environment.
pub fn defineVarIn(name: []const u8, value: R.SEXP, envir: R.SEXP) void {
    R.Rf_defineVar(installSym(name), value, envir);
}

/// Execute a function in a top-level context. Returns false on longjmp.
pub fn topLevelExec(func: *const fn (?*anyopaque) callconv(.c) void, data: ?*anyopaque) bool {
    return R.R_ToplevelExec(func, data) != 0;
}

pub const baseEnv: R.SEXP = R.R_BaseEnv;
pub const emptyEnv: R.SEXP = R.R_EmptyEnv;

/// Call an R function by name with positional arguments.
pub fn call(name: []const u8, args: []const R.SEXP) R.SEXP {
    const fun_sym = installSym(name);
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
