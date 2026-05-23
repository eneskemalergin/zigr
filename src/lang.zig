//! R language node and call construction.

const std = @import("std");
const R = @import("R");
const protect = @import("protect.zig");

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

/// Install a symbol. Result is cached in an open-addressing hash table
/// with linear probing. O(1) average lookup vs the previous O(n) linear scan.
/// R symbols live forever so no deletion logic is needed.
pub fn symbol(name: []const u8) R.SEXP {
    const S = struct {
        // Power-of-2 size for cheap modulo via mask.
        // 0 hash = empty slot. Wyhash never produces 0.
        const cap = 64;
        const mask = cap - 1;
        var hashes: [cap]u64 = undefined;
        var names: [cap][256:0]u8 = undefined;
        var lengths: [cap]usize = undefined;
        var sexps: [cap]R.SEXP = undefined;
        var count: usize = 0;
    };

    const hash = std.hash.Wyhash.hash(0, name);
    var idx = @as(usize, @truncate(hash)) & S.mask;

    // Probe until empty slot or match.
    while (S.hashes[idx] != 0) {
        if (S.hashes[idx] == hash and S.lengths[idx] == name.len and
            std.mem.eql(u8, S.names[idx][0..name.len], name))
        {
            return S.sexps[idx];
        }
        idx = (idx + 1) & S.mask;
    }

    // Not cached: install into R's symbol table.
    var buf: [256:0]u8 = undefined;
    const n = @min(name.len, buf.len - 1);
    @memcpy(buf[0..n], name[0..n]);
    buf[n] = 0;
    const sxp = R.Rf_install(@ptrCast(&buf));

    // Store in cache if space remains.
    if (S.count < S.cap) {
        S.hashes[idx] = hash;
        S.lengths[idx] = n;
        @memcpy(S.names[idx][0..n], name[0..n]);
        S.names[idx][n] = 0;
        S.sexps[idx] = sxp;
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
