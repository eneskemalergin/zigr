//! Phase 4 comprehensive example.
//! Build: zig test tests/phase4_example.zig -I/usr/share/R/include -lc
//!   --dep R -Mr_imports=.zig-cache/o/*/r_imports.zig
//!   -Mlang=src/lang.zig -Meval=src/eval.zig
//!   -Mtrycatch=src/trycatch.zig -Mserialize=src/serialize.zig
//!   -Mweakref=src/weakref.zig -Mexport=src/export.zig
//!   -Msexp=src/sexp.zig -Mprotect=src/protect.zig
//!   -Mconvert=src/convert.zig -Mcleanup=src/cleanup.zig
//! (Adjust the -Mr_imports path to match your .zig-cache)

const std = @import("std");
const R = @import("R");
const lang = @import("lang");
const eval = @import("eval");
const trycatch = @import("trycatch");
const serialize = @import("serialize");
const weakref = @import("weakref");
const sexp = @import("sexp");

fn mySum(slice: []const f64) f64 {
    var total: f64 = 0;
    for (slice) |v| total += v;
    return total;
}

fn myOptional(opt: ?f64) f64 {
    return opt orelse -1.0;
}

test "lang" {
    const sym = lang.symbol("my_test");

    const cell = lang.cons(R.R_NilValue, R.R_NilValue);
    _ = lang.car(cell);
    _ = lang.cdr(cell);
    lang.setCar(cell, R.R_NilValue);
    lang.setCdr(cell, R.R_NilValue);
    _ = lang.tag(cell);
    lang.setTag(cell, R.R_NilValue);

    _ = lang.allocSExp(sexp.SEXPTYPE.lang);
    _ = lang.dataCons(R.R_NilValue, R.R_NilValue);

    _ = lang.list1(R.R_NilValue);
    _ = lang.list2(R.R_NilValue, R.R_NilValue);
    _ = lang.list3(R.R_NilValue, R.R_NilValue, R.R_NilValue);
    _ = lang.list4(R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue);
    _ = lang.list5(R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue);
    _ = lang.list6(R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue);

    _ = lang.call1(sym);
    _ = lang.call2(sym, R.R_NilValue);
    _ = lang.call3(sym, R.R_NilValue, R.R_NilValue);
    _ = lang.call4(sym, R.R_NilValue, R.R_NilValue, R.R_NilValue);
    _ = lang.call5(sym, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue);
    _ = lang.call6(sym, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue);
}

test "eval" {
    const plus = lang.symbol("+");
    const one = R.Rf_ScalarReal(1.0);
    const call = lang.call3(plus, one, one);
    const res = eval.rEval(call, null);
    _ = res;
    _ = eval.baseEnv;
    _ = eval.emptyEnv;
}

test "trycatch" {
    const err = trycatch.tryCatch(struct {
        fn call() R.SEXP {
            R.Rf_error("expected test error");
            return R.R_NilValue;
        }
    }.call);
    try std.testing.expectError(error.RCondition, err);
}

test "serialize" {
    const vec = serialize.toVector(R.R_NilValue);
    const restored = serialize.fromVector(vec);
    _ = restored;
}

test "weakref" {
    const finalizer = struct {
        fn fin(_: R.SEXP) callconv(.c) void {}
    }.fin;
    const wr = weakref.make(R.R_NilValue, R.R_NilValue, finalizer, false);
    _ = weakref.key(wr);
    _ = weakref.value(wr);
}

test "conversion functions" {
    const data = try std.testing.allocator.alloc(f64, 5);
    defer std.testing.allocator.free(data);
    for (0..5) |i| data[i] = @floatFromInt(i);
    _ = mySum(data);
    _ = myOptional(null);
    _ = myOptional(42.0);
}
