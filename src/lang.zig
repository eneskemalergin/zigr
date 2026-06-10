//! R language node and call construction.
//!
//! Thin wrappers around R's pairlist and call-node APIs.  The function
//! names mirror R's C conventions (car/cdr/tag) and are self-explanatory.
//! Doc comments are omitted on one-liners for brevity.

const std = @import("std");
const R = @import("R");
const protect = @import("protect.zig");
const symbols = @import("symbols.zig");

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

/// Install a symbol. Delegates to the shared symbol cache in symbols.zig.
/// O(1) average lookup. R symbols live forever so no deletion logic is needed.
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

/// lcons creates a LANGSXP node. Use for building call nodes;
/// use dataCons for data pairlists (LISTSXP).
pub fn lcons(car_val: R.SEXP, cdr_val: R.SEXP) R.SEXP {
    return R.Rf_lcons(car_val, cdr_val);
}

/// Build a pairlist from a slice. Intermediate nodes are protected while the
/// list is under construction, then released before returning the head.
pub fn consList(items: []const R.SEXP) R.SEXP {
    var list = R.R_NilValue;
    var protect_count: usize = 0;
    var i = items.len;
    while (i > 0) {
        i -= 1;
        list = protect.protect(R.Rf_lcons(items[i], list));
        protect_count += 1;
    }
    if (protect_count > 0) protect.unprotectN(protect_count);
    return list;
}

/// Build a call node from a function SEXP and positional arguments.
/// Intermediate pairlist nodes are protected only while the call is assembled.
pub fn buildCall(fun: R.SEXP, args: []const R.SEXP) R.SEXP {
    const arg_list = consList(args);
    var call_expr = protect.scoped(R.Rf_lcons(fun, arg_list));
    defer call_expr.deinit();
    return call_expr.get();
}

/// Narrow comptime expression builder for common call construction.
/// Accepts a tuple of positional SEXP arguments.
pub fn buildNamedCall(comptime name: []const u8, args: anytype) R.SEXP {
    const Args = @TypeOf(args);
    const info = @typeInfo(Args);
    if (comptime info != .@"struct" or !info.@"struct".is_tuple) {
        @compileError("buildNamedCall expects a tuple literal like .{ arg1, arg2 }");
    }

    const fields = info.@"struct".fields;
    const values = std.heap.page_allocator.alloc(R.SEXP, fields.len) catch {
        R.Rf_error("out of memory in buildNamedCall");
        return R.R_NilValue;
    };
    defer std.heap.page_allocator.free(values);
    inline for (fields, 0..) |field, index| {
        values[index] = @field(args, field.name);
    }
    return buildCall(symbol(name), values);
}
