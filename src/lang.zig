//! R language node and call construction.

const std = @import("std");
const R = @import("R");
const protect = @import("protect");
const sexp_types = @import("sexp");

/// Read the first element of a pairlist or call node.
pub fn car(sexp: R.SEXP) R.SEXP {
    return R.CAR(sexp);
}

/// Read the rest of a pairlist or call node (all arguments after the first).
pub fn cdr(sexp: R.SEXP) R.SEXP {
    return R.CDR(sexp);
}

/// Replace the first element of a pairlist or call node.
pub fn setCar(sexp: R.SEXP, value: R.SEXP) void {
    _ = R.SETCAR(sexp, value);
}

/// Replace the rest of a pairlist or call node.
pub fn setCdr(sexp: R.SEXP, value: R.SEXP) void {
    _ = R.SETCDR(sexp, value);
}

/// Read the TAG (named argument name) of a pairlist element.
pub fn tag(sexp: R.SEXP) R.SEXP {
    return R.TAG(sexp);
}

/// Set the TAG of a pairlist element. Tags are SYMSXP symbols.
pub fn setTag(sexp: R.SEXP, value: R.SEXP) void {
    R.SET_TAG(sexp, value);
}

/// Allocate a SEXP of the given type. Uses @extern since Rf_allocSExp
/// is behind ENABLE_LEGACY_NONAPI_FUNS and may not be in the R module.
pub fn allocSExp(sxp_type: sexp_types.SEXPTYPE) R.SEXP {
    const Rf_allocSExp = @extern(*const fn (R.SEXPTYPE) callconv(.c) R.SEXP, .{ .name = "Rf_allocSExp" });
    return Rf_allocSExp(@intCast(@intFromEnum(sxp_type)));
}

/// Create a data pairlist node (LISTSXP, not a call node).
pub fn dataCons(car_val: R.SEXP, cdr_val: R.SEXP) R.SEXP {
    return R.Rf_cons(car_val, cdr_val);
}

/// Build a data pairlist with 1 element.
pub fn list1(s: R.SEXP) R.SEXP {
    return R.Rf_list1(s);
}

/// Build a data pairlist with 2 elements.
pub fn list2(s1: R.SEXP, s2: R.SEXP) R.SEXP {
    return R.Rf_list2(s1, s2);
}

/// Build a data pairlist with 3 elements.
pub fn list3(s1: R.SEXP, s2: R.SEXP, s3: R.SEXP) R.SEXP {
    return R.Rf_list3(s1, s2, s3);
}

/// Build a data pairlist with 4 elements.
pub fn list4(s1: R.SEXP, s2: R.SEXP, s3: R.SEXP, s4: R.SEXP) R.SEXP {
    return R.Rf_list4(s1, s2, s3, s4);
}

/// Build a data pairlist with 5 elements.
pub fn list5(s1: R.SEXP, s2: R.SEXP, s3: R.SEXP, s4: R.SEXP, s5: R.SEXP) R.SEXP {
    return R.Rf_list5(s1, s2, s3, s4, s5);
}

/// Build a data pairlist with 6 elements.
pub fn list6(s1: R.SEXP, s2: R.SEXP, s3: R.SEXP, s4: R.SEXP, s5: R.SEXP, s6: R.SEXP) R.SEXP {
    return R.Rf_list6(s1, s2, s3, s4, s5, s6);
}

/// Install a symbol. Result is cached: R symbols live forever, so we
/// reuse the SEXP across calls instead of hitting Rf_install each time.
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

/// Build a call node with 0 arguments.
pub fn call1(fun: R.SEXP) R.SEXP {
    return R.Rf_lang1(fun);
}

/// Build a call node with 1 argument: fun(arg1).
pub fn call2(fun: R.SEXP, arg1: R.SEXP) R.SEXP {
    return R.Rf_lang2(fun, arg1);
}

/// Build a call node with 2 arguments: fun(arg1, arg2).
pub fn call3(fun: R.SEXP, arg1: R.SEXP, arg2: R.SEXP) R.SEXP {
    return R.Rf_lang3(fun, arg1, arg2);
}

/// Build a call node with 3 arguments: fun(arg1, arg2, arg3).
pub fn call4(fun: R.SEXP, arg1: R.SEXP, arg2: R.SEXP, arg3: R.SEXP) R.SEXP {
    return R.Rf_lang4(fun, arg1, arg2, arg3);
}

/// Build a call node with 4 arguments: fun(arg1, arg2, arg3, arg4).
pub fn call5(fun: R.SEXP, arg1: R.SEXP, arg2: R.SEXP, arg3: R.SEXP, arg4: R.SEXP) R.SEXP {
    return R.Rf_lang5(fun, arg1, arg2, arg3, arg4);
}

/// Build a call node with 5 arguments: fun(arg1, arg2, arg3, arg4, arg5).
pub fn call6(fun: R.SEXP, arg1: R.SEXP, arg2: R.SEXP, arg3: R.SEXP, arg4: R.SEXP, arg5: R.SEXP) R.SEXP {
    return R.Rf_lang6(fun, arg1, arg2, arg3, arg4, arg5);
}

/// Create a pairlist node. The result is protected: pairlists are built
/// from last to first, and an intermediate CONS cell could be collected
/// before the full list is assembled.
pub fn cons(car_val: R.SEXP, cdr_val: R.SEXP) R.SEXP {
    const result = R.Rf_lcons(car_val, cdr_val);
    protect.protect(result);
    return result;
}

/// Build a pairlist from a slice. Each node is protected as it is created.
pub fn consList(items: []const R.SEXP) R.SEXP {
    var list = R.R_NilValue;
    var i = items.len;
    while (i > 0) {
        i -= 1;
        list = R.Rf_lcons(items[i], list);
        protect.protect(list);
    }
    return list;
}
