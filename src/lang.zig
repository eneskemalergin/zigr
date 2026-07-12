//! R language nodes and call construction.
//!
//! `car` and related helpers are unchecked C API access. Constructors may
//! allocate and longjmp. Inputs remain caller-rooted during construction and
//! built calls are returned unprotected.

const std = @import("std");
const R = @import("R");
const protect = @import("protect.zig");
const symbols = @import("symbols.zig");

const max_name_len = @import("sexp.zig").max_symbol_name;

pub const CallError = error{
    InvalidName,
    NullArgument,
    NullFunction,
};

pub const Argument = struct {
    value: R.SEXP,
    name: ?[]const u8 = null,
};

pub fn car(sexp: R.SEXP) R.SEXP {
    return R.CAR(sexp);
}

pub fn cdr(sexp: R.SEXP) R.SEXP {
    return R.CDR(sexp);
}

pub fn setCar(sexp: R.SEXP, value: R.SEXP) void {
    R.SETCAR(sexp, value);
}

pub fn setCdr(sexp: R.SEXP, value: R.SEXP) void {
    R.SETCDR(sexp, value);
}

pub fn tag(sexp: R.SEXP) R.SEXP {
    return R.TAG(sexp);
}

pub fn setTag(sexp: R.SEXP, value: R.SEXP) void {
    R.SET_TAG(sexp, value);
}

pub fn dataCons(car_val: R.SEXP, cdr_val: R.SEXP) R.SEXP {
    return R.Rf_cons(car_val, cdr_val);
}

pub fn list1(s: R.SEXP) R.SEXP {
    return R.Rf_list1(s);
}

pub fn list2(s1: R.SEXP, s2: R.SEXP) R.SEXP {
    return R.Rf_list2(s1, s2);
}

pub fn list3(s1: R.SEXP, s2: R.SEXP, s3: R.SEXP) R.SEXP {
    return R.Rf_list3(s1, s2, s3);
}

pub fn list4(s1: R.SEXP, s2: R.SEXP, s3: R.SEXP, s4: R.SEXP) R.SEXP {
    return R.Rf_list4(s1, s2, s3, s4);
}

pub fn list5(s1: R.SEXP, s2: R.SEXP, s3: R.SEXP, s4: R.SEXP, s5: R.SEXP) R.SEXP {
    return R.Rf_list5(s1, s2, s3, s4, s5);
}

pub fn list6(s1: R.SEXP, s2: R.SEXP, s3: R.SEXP, s4: R.SEXP, s5: R.SEXP, s6: R.SEXP) R.SEXP {
    return R.Rf_list6(s1, s2, s3, s4, s5, s6);
}

pub fn symbol(name: []const u8) R.SEXP {
    return symbols.install(name);
}

pub fn call0(fun: R.SEXP) R.SEXP {
    return R.Rf_lang1(fun);
}

pub fn call1(fun: R.SEXP, arg1: R.SEXP) R.SEXP {
    return R.Rf_lang2(fun, arg1);
}

pub fn call2(fun: R.SEXP, arg1: R.SEXP, arg2: R.SEXP) R.SEXP {
    return R.Rf_lang3(fun, arg1, arg2);
}

pub fn call3(fun: R.SEXP, arg1: R.SEXP, arg2: R.SEXP, arg3: R.SEXP) R.SEXP {
    return R.Rf_lang4(fun, arg1, arg2, arg3);
}

pub fn call4(fun: R.SEXP, arg1: R.SEXP, arg2: R.SEXP, arg3: R.SEXP, arg4: R.SEXP) R.SEXP {
    return R.Rf_lang5(fun, arg1, arg2, arg3, arg4);
}

pub fn call5(fun: R.SEXP, arg1: R.SEXP, arg2: R.SEXP, arg3: R.SEXP, arg4: R.SEXP, arg5: R.SEXP) R.SEXP {
    return R.Rf_lang6(fun, arg1, arg2, arg3, arg4, arg5);
}

pub fn lcons(car_val: R.SEXP, cdr_val: R.SEXP) R.SEXP {
    return R.Rf_lcons(car_val, cdr_val);
}

pub fn consList(items: []const R.SEXP) R.SEXP {
    var list = R.R_NilValue;
    var index: R.PROTECT_INDEX = 0;
    protect.protectWithIndex(list, &index);
    defer protect.unprotect();
    var i = items.len;
    while (i > 0) {
        i -= 1;
        list = R.Rf_cons(items[i], list);
        protect.reprotect(list, index);
    }
    return list;
}

/// The returned call is unprotected after construction.
pub fn buildCall(fun: R.SEXP, args: []const R.SEXP) R.SEXP {
    var arg_list = protect.scoped(consList(args));
    defer arg_list.deinit();
    var call_expr = protect.scoped(R.Rf_lcons(fun, arg_list.get()));
    defer call_expr.deinit();
    return call_expr.get();
}

pub fn buildCallChecked(fun: R.SEXP, args: []const R.SEXP) CallError!R.SEXP {
    if (fun == null) return error.NullFunction;
    for (args) |arg| {
        if (arg == null) return error.NullArgument;
    }
    return buildCall(fun, args);
}

/// Argument names become pairlist tags; unnamed arguments keep a nil tag.
pub fn buildTaggedCall(fun: R.SEXP, args: []const Argument) CallError!R.SEXP {
    if (fun == null) return error.NullFunction;
    for (args) |arg| {
        if (arg.value == null) return error.NullArgument;
        if (arg.name) |name| {
            if (!validName(name)) return error.InvalidName;
        }
    }

    var list = R.R_NilValue;
    var index: R.PROTECT_INDEX = 0;
    protect.protectWithIndex(list, &index);
    defer protect.unprotect();
    var i = args.len;
    while (i > 0) {
        i -= 1;
        const name = if (args[i].name) |name| symbols.install(name) else R.R_NilValue;
        list = R.Rf_cons(args[i].value, list);
        protect.reprotect(list, index);
        if (name != R.R_NilValue) R.SET_TAG(list, name);
    }

    return R.Rf_lcons(fun, list);
}

pub fn buildNamedCall(comptime name: []const u8, args: anytype) R.SEXP {
    const Args = @TypeOf(args);
    const info = @typeInfo(Args);
    if (comptime info != .@"struct" or !info.@"struct".is_tuple) {
        @compileError("buildNamedCall expects a tuple literal like .{ arg1, arg2 }");
    }

    const fields = info.@"struct".fields;
    // R can longjmp before a heap allocation would be released.
    var values: [fields.len]R.SEXP = undefined;
    inline for (fields, 0..) |field, index| {
        values[index] = @field(args, field.name);
    }
    return buildCall(symbol(name), values[0..]);
}

fn validName(name: []const u8) bool {
    return name.len != 0 and name.len < max_name_len and std.mem.indexOfScalar(u8, name, 0) == null;
}
