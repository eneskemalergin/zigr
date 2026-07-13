const std = @import("std");
const R = @import("R");
const zigr = @import("zigr");
const raw_mod = zigr.raw;
const cleanup = @import("cleanup");
const err = zigr.@"error";
const ict = zigr.interrupt;
const test_eval = zigr.eval;
const rng = zigr.rng;
const mem = zigr.memory;
const zigr_convert = zigr.convert;
const df = zigr.dataframe;
const factor = zigr.factor;
const s4 = zigr.s4;
const attrib = zigr.attrib;
const altrep_mod = zigr.altrep;
const altrep_create = zigr.altrep_create;
const test_lang = zigr.lang;
const embed = zigr.embed;
const serialize_mod = zigr.serialize;
const weakref_mod = zigr.weakref;
const trycatch_mod = zigr.trycatch;
const protect = zigr.protect;

const SEXP = R.SEXP;

var test_dll: ?*R.DllInfo = null;

fn initTestDll(info: *R.DllInfo) void {
    test_dll = info;
    MyAlt.register(info);
    MyAltInt.register(info);
    MyAltLogical.register(info);
    MyAltRaw.register(info);
    MyAltComplex.register(info);
    MyAltString.register(info);
    ExternalExports.init(info);
    ArenaExports.init(info);
    BoundaryExports.init(info);
    CounterMethods.init(info);
    _ = R.R_useDynamicSymbols(info, 1);
}

export fn R_init_zigr_r_test(info: *R.DllInfo) callconv(.c) void {
    initTestDll(info);
}

export fn R_init_libzigr_r_test(info: *R.DllInfo) callconv(.c) void {
    initTestDll(info);
}

export fn zigr_alloc_real() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @as(R.R_xlen_t, 100)));
    const ptr: [*]f64 = @ptrCast(R.REAL(vec));
    var i: usize = 0;
    while (i < 100) : (i += 1) ptr[i] = @floatFromInt(i);
    R.Rf_unprotect(1);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_alloc_large() SEXP {
    _ = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @as(R.R_xlen_t, 10000000)));
    R.Rf_unprotect(1);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_protect_many() SEXP {
    const vec = R.Rf_allocVector(R.INTSXP, 1);
    var i: usize = 0;
    while (i < 100) : (i += 1) _ = R.Rf_protect(vec);
    R.Rf_unprotect(100);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_protect_index() SEXP {
    var idx: R.PROTECT_INDEX = 0;
    const vec1 = R.Rf_allocVector(R.REALSXP, 10);
    R.R_ProtectWithIndex(vec1, &idx);

    const vec2 = R.Rf_allocVector(R.INTSXP, 5);
    R.R_Reprotect(vec2, idx);

    R.Rf_unprotect(1);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_check_na() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 3));
    const ptr = R.REAL(vec);
    ptr[0] = 1.0;
    ptr[1] = R.NA_REAL();
    ptr[2] = 3.0;
    R.Rf_unprotect(1);

    if (ptr[0] != 1.0 or R.ISNA(ptr[1]) == 0 or ptr[2] != 3.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_raise_error() SEXP {
    R.Rf_error("zigr test error: this is expected");
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_raise_warning() SEXP {
    R.Rf_warning("zigr test warning: this is expected");
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_typeof_nil() SEXP {
    if (R.TYPEOF(R.R_NilValue) != 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_protect() SEXP {
    _ = R.Rf_protect(R.R_NilValue);
    R.Rf_unprotect(1);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_return42() SEXP {
    const val = R.REAL(R.Rf_ScalarReal(42.0))[0];
    if (val != 42.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_abi_contract() SEXP {
    if (zigr.sexp.fastLength(null) != 0) return R.Rf_ScalarReal(0.0);
    if (zigr.sexp.fastDataPtr(null) != null) return R.Rf_ScalarReal(0.0);
    if (zigr.sexp.fastVectorElt(null, 0) != null) return R.Rf_ScalarReal(0.0);
    if (zigr.sexp.fastCharData(null) != null or zigr.sexp.fastGetCharCE(null) != -1) return R.Rf_ScalarReal(0.0);
    if (zigr.sexp.checked.typeTag(null) != -1 or zigr.sexp.checked.length(null) != 0 or zigr.sexp.checked.dataPtr(null) != null) return R.Rf_ScalarReal(0.0);

    const real = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 3));
    defer R.Rf_unprotect(1);
    R.REAL(real)[0] = 1.0;
    R.REAL(real)[1] = 2.0;
    R.REAL(real)[2] = 3.0;

    if (zigr.sexp.typeTag(real) != zigr.sexp.checked.typeTag(real)) return R.Rf_ScalarReal(0.0);
    if (zigr.sexp.fastLength(real) != zigr.sexp.checked.length(real)) return R.Rf_ScalarReal(0.0);
    const active_data = zigr.sexp.fastDataPtr(real) orelse return R.Rf_ScalarReal(0.0);
    const checked_data = zigr.sexp.checked.dataPtr(real) orelse return R.Rf_ScalarReal(0.0);
    if (active_data != checked_data) return R.Rf_ScalarReal(0.0);

    const strings = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 4));
    defer R.Rf_unprotect(1);
    const encodings = [_]c_int{ R.CE_NATIVE, R.CE_UTF8, R.CE_LATIN1, R.CE_BYTES };
    for (encodings, 0..) |encoding, index| {
        const charsxp = R.Rf_mkCharLenCE("layout", 6, @as(R.cetype_t, @intCast(encoding)));
        R.SET_STRING_ELT(strings, @intCast(index), charsxp);

        if (zigr.sexp.fastVectorElt(strings, index) != zigr.sexp.checked.vectorElt(strings, index)) return R.Rf_ScalarReal(0.0);
        const active_chars = zigr.sexp.fastCharData(charsxp) orelse return R.Rf_ScalarReal(0.0);
        const checked_chars = zigr.sexp.checked.charData(charsxp) orelse return R.Rf_ScalarReal(0.0);
        if (active_chars != checked_chars or !std.mem.eql(u8, std.mem.sliceTo(active_chars, 0), "layout")) return R.Rf_ScalarReal(0.0);
        if (zigr.sexp.fastGetCharCE(charsxp) != zigr.sexp.checked.getCharCE(charsxp)) return R.Rf_ScalarReal(0.0);
    }

    if (zigr.sexp.checked.vectorElt(strings, 4) != null or zigr.sexp.checked.vectorElt(real, 0) != null) return R.Rf_ScalarReal(0.0);
    if (zigr.sexp.checked.charData(real) != null or zigr.sexp.checked.getCharCE(real) != -1) return R.Rf_ScalarReal(0.0);

    return R.Rf_ScalarReal(1.0);
}

var longjmp_cleanup_fired: bool = false;

fn markCleanupFired(_: ?*anyopaque) void {
    longjmp_cleanup_fired = true;
}

export fn zigr_test_longjmp() SEXP {
    longjmp_cleanup_fired = false;

    _ = cleanup.protectCall(struct {
        fn doBoom() R.SEXP {
            cleanup.pushFrame(markCleanupFired, null);
            R.Rf_error("zigr longjmp test: expected error");
            return R.R_NilValue;
        }
    }.doBoom);

    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_longjmp_normal() SEXP {
    longjmp_cleanup_fired = false;
    cleanup.pushFrame(markCleanupFired, null);

    const result = cleanup.protectCall(struct {
        fn doNormal() SEXP {
            return R.Rf_ScalarReal(99.0);
        }
    }.doNormal);

    cleanup.popFrame();

    const val = R.REAL(result)[0];
    if (val != 99.0) return R.Rf_ScalarReal(0.0);
    if (longjmp_cleanup_fired) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_longjmp_flag() SEXP {
    return R.Rf_ScalarInteger(if (longjmp_cleanup_fired) 1 else 0);
}

export fn zigr_test_error_signal() SEXP {
    err.signal("zigr error signal test");
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_error_warn() SEXP {
    err.warn("zigr warning signal test");
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_error_signalif() SEXP {
    err.signalIf(true, "zigr error signalIf test");
    return R.R_NilValue;
}

export fn zigr_test_interrupt() SEXP {
    ict.checkInterrupt();
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_check_stack() SEXP {
    ict.checkStack();
    return R.Rf_ScalarReal(1.0);
}

var stack_check_cleanup_fired = false;

fn markStackCheckCleanup(_: ?*anyopaque) void {
    stack_check_cleanup_fired = true;
}

export fn zigr_test_check_stack_longjmp() SEXP {
    stack_check_cleanup_fired = false;
    if (trycatch_mod.tryCatch(struct {
        fn call() SEXP {
            return cleanup.protectCall(struct {
                fn check() SEXP {
                    cleanup.pushFrame(markStackCheckCleanup, null);
                    ict.checkStack2(std.math.maxInt(usize));
                    cleanup.popFrame();
                    return R.R_NilValue;
                }
            }.check);
        }
    }.call)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    return R.Rf_ScalarReal(if (stack_check_cleanup_fired) 1.0 else 0.0);
}

export fn zigr_test_rev_eval() SEXP {
    const plus = test_lang.symbol("+");
    const one = R.Rf_ScalarReal(1.0);
    const call = test_lang.call2(plus, one, one);
    const result = test_eval.rEval(call, null);
    const val = R.REAL(result)[0];
    if (val != 2.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_rev_define_find() SEXP {
    test_eval.defineVar("zigr_test_var", R.Rf_ScalarReal(42.0));
    const result = test_eval.findVarName("zigr_test_var");
    const val = R.REAL(result)[0];
    if (val != 42.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_rev_lang3() SEXP {
    const fsum = test_lang.symbol("sum");
    const a = R.Rf_ScalarReal(10.0);
    const b = R.Rf_ScalarReal(20.0);
    const call = test_lang.call2(fsum, a, b);
    const result = test_eval.rEval(call, null);
    const val = R.REAL(result)[0];
    if (val != 30.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

fn installLongSymbol() SEXP {
    var name = [_]u8{'x'} ** zigr.sexp.max_symbol_name;
    return zigr.symbols.install(&name);
}

fn installNulSymbol() SEXP {
    const name = [_]u8{ 'x', 0, 'y' };
    return zigr.symbols.install(&name);
}

export fn zigr_test_symbol_contract() SEXP {
    const first = zigr.symbols.install("zigr_service_symbol");
    if (first != zigr.symbols.install("zigr_service_symbol")) return R.Rf_ScalarReal(0.0);

    var buf: [64]u8 = undefined;
    var installed: [80]SEXP = undefined;
    for (0..80) |i| {
        const name = std.fmt.bufPrint(&buf, "zigr_service_symbol_{d}", .{i}) catch return R.Rf_ScalarReal(0.0);
        installed[i] = zigr.symbols.install(name);
    }
    for (0..80) |i| {
        const name = std.fmt.bufPrint(&buf, "zigr_service_symbol_{d}", .{i}) catch return R.Rf_ScalarReal(0.0);
        if (installed[i] != zigr.symbols.install(name)) return R.Rf_ScalarReal(0.0);
    }
    if (first != zigr.symbols.install("zigr_service_symbol")) return R.Rf_ScalarReal(0.0);

    if (trycatch_mod.tryCatch(installLongSymbol)) |_| return R.Rf_ScalarReal(0.0) else |_| {}
    if (trycatch_mod.tryCatch(installNulSymbol)) |_| return R.Rf_ScalarReal(0.0) else |_| {}
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_eval_contract() SEXP {
    if (test_eval.tryFindVarName("__zigr_missing_service_value__") != null) return R.Rf_ScalarReal(0.0);

    const one = R.Rf_protect(R.Rf_ScalarReal(1.0));
    defer R.Rf_unprotect(1);
    const call = R.Rf_protect(test_lang.buildNamedCall("sum", .{ one, one }));
    defer R.Rf_unprotect(1);
    for (0..8) |_| {
        const noise = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 131_072));
        R.REAL(noise)[0] = 1.0;
        R.Rf_unprotect(1);
    }
    const result = test_eval.tryEval(call, R.R_GlobalEnv) orelse return R.Rf_ScalarReal(0.0);
    if (R.REAL(result)[0] != 2.0) return R.Rf_ScalarReal(0.0);
    if (test_eval.tryEvalSilent(call, one) != null) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_language_call_contract() SEXP {
    var envir = protect.scoped(R.R_NewEnv(R.R_BaseEnv, 1, 29));
    defer envir.deinit();
    var function = protect.scoped(embed.rCodeEval("function(x, scale=1) x * scale", R.R_BaseEnv));
    defer function.deinit();
    test_eval.defineVarIn("zigr_local_scale", function.get(), envir.get());
    const resolved = test_eval.findFunctionIn("zigr_local_scale", envir.get());
    if (resolved != function.get()) return R.Rf_ScalarReal(0.0);

    var value = protect.scoped(R.Rf_ScalarReal(6.0));
    defer value.deinit();
    var scale = protect.scoped(R.Rf_ScalarReal(7.0));
    defer scale.deinit();
    const args = [_]test_lang.Argument{
        .{ .value = value.get() },
        .{ .name = "scale", .value = scale.get() },
    };
    var call = protect.scoped(test_lang.buildTaggedCall(resolved, args[0..]) catch return R.Rf_ScalarReal(0.0));
    defer call.deinit();
    if (R.TYPEOF(call.get()) != R.LANGSXP or R.TYPEOF(R.CDR(call.get())) != R.LISTSXP) return R.Rf_ScalarReal(0.0);
    if (R.TAG(R.CDR(call.get())) != R.R_NilValue) return R.Rf_ScalarReal(0.0);
    if (R.TAG(R.CDR(R.CDR(call.get()))) != test_lang.symbol("scale")) return R.Rf_ScalarReal(0.0);

    const result = test_eval.rEval(call.get(), envir.get());
    if (R.TYPEOF(result) != R.REALSXP or R.REAL(result)[0] != 42.0) return R.Rf_ScalarReal(0.0);
    const direct = test_eval.callTaggedIn(resolved, args[0..], envir.get()) catch return R.Rf_ScalarReal(0.0);
    if (R.REAL(direct)[0] != 42.0) return R.Rf_ScalarReal(0.0);

    const positional = [_]SEXP{value.get()};
    const defaulted = test_eval.callIn("zigr_local_scale", positional[0..], envir.get());
    if (R.REAL(defaulted)[0] != 6.0) return R.Rf_ScalarReal(0.0);
    if (test_lang.buildTaggedCall(resolved, &.{.{ .name = "bad\x00name", .value = value.get() }})) |_| return R.Rf_ScalarReal(0.0) else |call_error| {
        if (call_error != error.InvalidName) return R.Rf_ScalarReal(0.0);
    }
    if (test_lang.buildCallChecked(null, positional[0..])) |_| return R.Rf_ScalarReal(0.0) else |call_error| {
        if (call_error != error.NullFunction) return R.Rf_ScalarReal(0.0);
    }
    if (test_lang.buildCallChecked(resolved, &.{null})) |_| return R.Rf_ScalarReal(0.0) else |call_error| {
        if (call_error != error.NullArgument) return R.Rf_ScalarReal(0.0);
    }

    var empty_call = protect.scoped(test_lang.buildCallChecked(resolved, &.{}) catch return R.Rf_ScalarReal(0.0));
    defer empty_call.deinit();
    if (R.CDR(empty_call.get()) != R.R_NilValue) return R.Rf_ScalarReal(0.0);

    const initial_depth = protect.getDepth();
    const many_args = [_]SEXP{value.get()} ** 2048;
    var long_call = protect.scoped(test_lang.buildCallChecked(resolved, many_args[0..]) catch return R.Rf_ScalarReal(0.0));
    if (R.TYPEOF(R.CDR(long_call.get())) != R.LISTSXP or R.Rf_length(R.CDR(long_call.get())) != many_args.len) return R.Rf_ScalarReal(0.0);
    long_call.deinit();
    if (protect.getDepth() != initial_depth) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

fn languageCallError() SEXP {
    const message = protect.protect(R.Rf_mkString("expected language call error"));
    defer protect.unprotect();
    return test_eval.callIn("stop", &.{message}, R.R_GlobalEnv);
}

export fn zigr_test_language_call_longjmp() SEXP {
    const initial_depth = protect.getDepth();
    if (trycatch_mod.tryCatch(struct {
        fn call() SEXP {
            return cleanup.protectCall(languageCallError);
        }
    }.call)) |_| return R.Rf_ScalarReal(0.0) else |_| {}
    if (protect.getDepth() != initial_depth) return R.Rf_ScalarReal(0.0);

    var value = protect.scoped(R.Rf_ScalarInteger(9));
    defer value.deinit();
    const result = test_eval.callIn("identity", &.{value.get()}, R.R_BaseEnv);
    if (R.TYPEOF(result) != R.INTSXP or R.INTEGER(result)[0] != 9) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_rng() SEXP {
    rng.acquire();
    rng.release();
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_ralloc() SEXP {
    const alloc = mem.RAllocator;
    const buf = alloc.alloc(u8, 100) catch return R.Rf_ScalarReal(0.0);
    defer alloc.free(buf);
    buf[0] = 42;
    if (buf[0] != 42) return R.Rf_ScalarReal(0.0);

    var grown = alloc.alloc(u8, 8) catch return R.Rf_ScalarReal(0.0);
    grown[0] = 7;
    grown = alloc.realloc(grown, 64) catch return R.Rf_ScalarReal(0.0);
    defer alloc.free(grown);
    if (grown[0] != 7) return R.Rf_ScalarReal(0.0);

    const aligned = alloc.alignedAlloc(f64, null, 2) catch return R.Rf_ScalarReal(0.0);
    defer alloc.free(aligned);
    if (!std.mem.Alignment.of(f64).check(@intFromPtr(aligned.ptr))) return R.Rf_ScalarReal(0.0);
    if (alloc.alignedAlloc(u8, .@"64", 1)) |over_aligned| {
        alloc.free(over_aligned);
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    return R.Rf_ScalarReal(1.0);
}

var preserve_released: bool = false;

const PreservedCleanup = struct {
    value: SEXP,
    armed: bool = false,

    fn fire(self: *@This()) void {
        if (!self.armed) return;
        R.R_ReleaseObject(self.value);
        preserve_released = true;
    }
};

fn preserveThenError() SEXP {
    const state = cleanup.pushFrameInline(PreservedCleanup, .{ .value = R.Rf_ScalarReal(99.0) }, PreservedCleanup.fire);
    R.R_PreserveObject(state.value);
    state.armed = true;
    R.Rf_error("preserve: expected");
}

export fn zigr_test_preserve_longjmp() SEXP {
    preserve_released = false;
    _ = cleanup.protectCall(preserveThenError);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_preserve_flag() SEXP {
    return R.Rf_ScalarInteger(if (preserve_released) 1 else 0);
}

var nested_outer_fired: bool = false;
var nested_inner_fired: bool = false;

fn markOuter(_: ?*anyopaque) void {
    nested_outer_fired = true;
}
fn markInner(_: ?*anyopaque) void {
    nested_inner_fired = true;
}

export fn zigr_test_nested_inner() SEXP {
    return cleanup.protectCall(struct {
        fn call() SEXP {
            cleanup.pushFrame(markInner, null);
            R.Rf_error("nested inner: expected");
        }
    }.call);
}

export fn zigr_test_nested_outer() SEXP {
    nested_outer_fired = false;
    nested_inner_fired = false;
    _ = cleanup.protectCall(struct {
        fn doNested() R.SEXP {
            cleanup.pushFrame(markOuter, null);
            const fn_name = R.Rf_mkChar("zigr_test_nested_inner");
            const fn_string = R.Rf_ScalarString(fn_name);
            const call_sexp = test_lang.call1(
                test_lang.symbol(".Call"),
                fn_string,
            );
            return test_eval.rEval(call_sexp, null);
        }
    }.doNested);

    return R.R_NilValue;
}

export fn zigr_nested_flags() SEXP {
    var v: i32 = 0;
    if (nested_outer_fired) v += 1;
    if (nested_inner_fired) v += 2;
    return R.Rf_ScalarInteger(if (v == 3) 1 else 0);
}

export fn zigr_test_to_real_slice() SEXP {
    const n: R.R_xlen_t = 5;
    const rvec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, n));
    defer R.Rf_unprotect(1);
    const ptr = R.REAL(rvec);
    const expected = [_]f64{ 1.0, 2.0, 3.0, 4.0, 5.0 };
    for (0..expected.len) |i| ptr[i] = expected[i];
    for (0..expected.len) |i| {
        if (ptr[i] != expected[i]) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_from_real_slice() SEXP {
    const values = [_]f64{ 10.0, 20.0, 30.0 };
    const sexp: R.SEXP = @ptrCast(zigr_convert.fromRealSlice(values[0..]));
    const ptr = R.REAL(sexp);
    for (0..values.len) |i| {
        if (ptr[i] != values[i]) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_real_roundtrip() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const input = [_]f64{ 1.5, 2.5, 3.5, 4.5 };
    const sexp: R.SEXP = @ptrCast(zigr_convert.fromRealSlice(input[0..]));
    const ptr = R.REAL(sexp);
    for (input, 0..) |v, i| {
        if (ptr[i] != v) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_int_create() SEXP {
    const n: R.R_xlen_t = 4;
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, n));
    defer R.Rf_unprotect(1);
    const ptr = R.INTEGER(vec);
    const expected = [_]i32{ 10, 20, 30, 40 };
    for (0..expected.len) |i| ptr[i] = expected[i];
    for (0..expected.len) |i| {
        if (ptr[i] != expected[i]) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_int_from_slice() SEXP {
    const values = [_]i32{ 100, 200, 300 };
    const sexp: R.SEXP = @ptrCast(zigr_convert.fromIntSlice(values[0..]));
    const ptr = R.INTEGER(sexp);
    for (0..values.len) |i| {
        if (ptr[i] != values[i]) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_str_create() SEXP {
    const n: R.R_xlen_t = 3;
    const vec = R.Rf_protect(R.Rf_allocVector(R.STRSXP, n));
    defer R.Rf_unprotect(1);
    const expected = [_][]const u8{ "hello", "world", "zigr" };
    for (0..@as(usize, @intCast(n))) |i| {
        const cs = R.Rf_mkCharLenCE(@ptrCast(expected[i].ptr), @intCast(expected[i].len), @as(R.cetype_t, @intCast(R.CE_UTF8)));
        R.SET_STRING_ELT(vec, @intCast(i), cs);
    }
    for (0..@as(usize, @intCast(n))) |i| {
        const elt = R.STRING_ELT(vec, @intCast(i));
        const s = std.mem.sliceTo(R.R_CHAR(elt), 0);
        if (!std.mem.eql(u8, s, expected[i])) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_str_from_slice() SEXP {
    const values = [_][]const u8{ "alpha", "beta", "gamma" };
    const sexp: R.SEXP = @ptrCast(zigr_convert.fromStringSlice(values[0..]));
    for (0..values.len) |i| {
        const elt = R.STRING_ELT(sexp, @intCast(i));
        const s = std.mem.sliceTo(R.R_CHAR(elt), 0);
        if (!std.mem.eql(u8, s, values[i])) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_lgl_create() SEXP {
    const n: R.R_xlen_t = 4;
    const vec = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, n));
    defer R.Rf_unprotect(1);
    const ptr = R.LOGICAL(vec);
    ptr[0] = 1;
    ptr[1] = 0;
    ptr[2] = R.R_NaInt;
    ptr[3] = 1;
    if (ptr[0] != 1 or ptr[1] != 0 or ptr[2] != R.R_NaInt or ptr[3] != 1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_lgl_from_slice() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const values = [_]i32{ 1, 0, 1 };
    const result = zigr_convert.fromLogicalSlice(values[0..]);
    const sexp: R.SEXP = @ptrCast(result);
    if (R.XLENGTH(sexp) != values.len) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_list_create() SEXP {
    const n: R.R_xlen_t = 3;
    const vec = R.Rf_protect(R.Rf_allocVector(R.VECSXP, n));
    defer R.Rf_unprotect(1);
    _ = R.SET_VECTOR_ELT(vec, 0, R.Rf_ScalarReal(1.5));
    _ = R.SET_VECTOR_ELT(vec, 1, R.Rf_ScalarInteger(42));
    _ = R.SET_VECTOR_ELT(vec, 2, R.R_NilValue);
    if (R.TYPEOF(vec) != R.VECSXP) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_to_logical_slice() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const n: R.R_xlen_t = 4;
    const vec = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, n));
    defer R.Rf_unprotect(1);
    const ptr = R.LOGICAL(vec);
    ptr[0] = 1;
    ptr[1] = 0;
    ptr[2] = R.R_NaInt;
    ptr[3] = 1;

    const slice = zigr_convert.toLogicalSlice(arena.allocator(), @as(SEXP, @ptrCast(vec))) catch return R.Rf_ScalarReal(0.0);
    var ok = true;
    if (slice.len != 4) ok = false;
    if (slice[0] != 1) ok = false;
    if (slice[1] != 0) ok = false;
    if (slice[2] != R.R_NaInt) ok = false;
    if (slice[3] != 1) ok = false;
    return if (ok) R.Rf_ScalarReal(1.0) else R.Rf_ScalarReal(0.0);
}

export fn zigr_test_df_build() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const vals = [_]f64{ 1.0, 2.0, 3.0 };
    var col1 = protect.scoped(@as(R.SEXP, @ptrCast(zigr_convert.fromRealSlice(vals[0..]))));
    defer col1.deinit();
    const strs = [_][]const u8{ "a", "b", "c" };
    var col2 = protect.scoped(@as(R.SEXP, @ptrCast(zigr_convert.fromStringSlice(strs[0..]))));
    defer col2.deinit();
    const names = [_][]const u8{ "x", "y" };
    const cols = [_]R.SEXP{ col1.get(), col2.get() };

    const df_sexp = df.build(names[0..], cols[0..]);
    const wrap = df.DataFrame.wrap(df_sexp) orelse return R.Rf_ScalarReal(0.0);
    if (wrap.columnCount() != 2) return R.Rf_ScalarReal(0.0);
    if (wrap.rowCount() != 3) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_df_column() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const vals = [_]f64{ 10.0, 20.0 };
    var col1 = protect.scoped(@as(R.SEXP, @ptrCast(zigr_convert.fromRealSlice(vals[0..]))));
    defer col1.deinit();
    const names = [_][]const u8{"val"};
    const cols = [_]R.SEXP{col1.get()};
    const df_sexp = df.build(names[0..], cols[0..]);

    const df_wrap = df.DataFrame.wrap(df_sexp) orelse return R.R_NilValue;
    const col = df_wrap.column("val") orelse return R.Rf_ScalarReal(0.0);
    const rcol: R.SEXP = @as(R.SEXP, @ptrCast(col));
    const ptr: [*]f64 = @ptrCast(R.REAL(rcol));
    if (ptr[0] == 10.0 and ptr[1] == 20.0) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_df_contract() SEXP {
    const left_values = [_]f64{ 1.0, 2.0 };
    var left = protect.scoped(@as(R.SEXP, @ptrCast(zigr_convert.fromRealSlice(left_values[0..]))));
    defer left.deinit();
    const right_values = [_]i32{ 3, 4 };
    var right = protect.scoped(@as(R.SEXP, @ptrCast(zigr_convert.fromIntSlice(right_values[0..]))));
    defer right.deinit();

    const names = [_][]const u8{ "left", "right" };
    const columns = [_]R.SEXP{ left.get(), right.get() };
    var frame = protect.scoped(df.buildChecked(names[0..], columns[0..]) catch return R.Rf_ScalarReal(0.0));
    defer frame.deinit();
    const wrapped = df.DataFrame.wrap(frame.get()) orelse return R.Rf_ScalarReal(0.0);
    if (wrapped.rowCount() != 2 or wrapped.columnCount() != 2) return R.Rf_ScalarReal(0.0);
    if (wrapped.columnAt(0) != left.get() or wrapped.columnAt(2) != null) return R.Rf_ScalarReal(0.0);
    const column_names = wrapped.columnNames(std.heap.page_allocator) catch return R.Rf_ScalarReal(0.0);
    defer std.heap.page_allocator.free(column_names);
    if (column_names.len != 2 or !std.mem.eql(u8, column_names[0], "left") or !std.mem.eql(u8, column_names[1], "right")) return R.Rf_ScalarReal(0.0);
    var column_map = wrapped.columnMap(std.heap.page_allocator) catch return R.Rf_ScalarReal(0.0);
    defer column_map.deinit();
    if (column_map.get("left") != 0 or column_map.get("right") != 1) return R.Rf_ScalarReal(0.0);

    const row_names = R.Rf_getAttrib(frame.get(), R.R_RowNamesSymbol);
    if (R.TYPEOF(row_names) != R.INTSXP or R.XLENGTH(row_names) != 2) return R.Rf_ScalarReal(0.0);
    if (R.INTEGER(row_names)[0] != 1 or R.INTEGER(row_names)[1] != 2) return R.Rf_ScalarReal(0.0);

    const short_values = [_]f64{1.0};
    var short = protect.scoped(@as(R.SEXP, @ptrCast(zigr_convert.fromRealSlice(short_values[0..]))));
    defer short.deinit();
    const short_columns = [_]R.SEXP{ left.get(), short.get() };
    if (df.buildChecked(names[0..], short_columns[0..])) |_| return R.Rf_ScalarReal(0.0) else |build_error| {
        if (build_error != error.ColumnLength) return R.Rf_ScalarReal(0.0);
    }
    if (df.buildChecked(names[0..1], columns[0..])) |_| return R.Rf_ScalarReal(0.0) else |build_error| {
        if (build_error != error.NameColumnCount) return R.Rf_ScalarReal(0.0);
    }
    const empty_names = [_][]const u8{""};
    const one_column = [_]R.SEXP{left.get()};
    if (df.buildChecked(empty_names[0..], one_column[0..])) |_| return R.Rf_ScalarReal(0.0) else |build_error| {
        if (build_error != error.InvalidName) return R.Rf_ScalarReal(0.0);
    }
    const oversized_name = @as([*]const u8, @ptrFromInt(1))[0 .. @as(usize, std.math.maxInt(c_int)) + 1];
    const oversized_names = [_][]const u8{oversized_name};
    if (df.buildChecked(oversized_names[0..], one_column[0..])) |_| return R.Rf_ScalarReal(0.0) else |build_error| {
        if (build_error != error.InvalidName) return R.Rf_ScalarReal(0.0);
    }

    var matrix = protect.scoped(R.Rf_allocMatrix(R.REALSXP, 2, 2));
    defer matrix.deinit();
    const matrix_columns = [_]R.SEXP{ matrix.get(), left.get() };
    var matrix_frame = protect.scoped(df.buildChecked(names[0..], matrix_columns[0..]) catch return R.Rf_ScalarReal(0.0));
    defer matrix_frame.deinit();
    const wrapped_matrix = df.DataFrame.wrap(matrix_frame.get()) orelse return R.Rf_ScalarReal(0.0);
    if (wrapped_matrix.rowCount() != 2 or wrapped_matrix.columnAt(0) != matrix.get()) return R.Rf_ScalarReal(0.0);

    var empty = protect.scoped(df.buildChecked(&.{}, &.{}) catch return R.Rf_ScalarReal(0.0));
    defer empty.deinit();
    const empty_frame = df.DataFrame.wrap(empty.get()) orelse return R.Rf_ScalarReal(0.0);
    if (empty_frame.columnCount() != 0 or empty_frame.rowCount() != 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_factor_contract() SEXP {
    var input = protect.scoped(R.Rf_allocVector(R.STRSXP, 4));
    defer input.deinit();
    R.SET_STRING_ELT(input.get(), 0, R.Rf_mkChar("b"));
    R.SET_STRING_ELT(input.get(), 1, R.Rf_mkChar("a"));
    R.SET_STRING_ELT(input.get(), 2, R.Rf_mkChar("b"));
    R.SET_STRING_ELT(input.get(), 3, R.R_NaString);

    var result = protect.scoped(factor.asFactorChecked(input.get()) catch return R.Rf_ScalarReal(0.0));
    defer result.deinit();
    const codes = R.INTEGER(result.get());
    if (codes[0] != 2 or codes[1] != 1 or codes[2] != 2 or codes[3] != R.R_NaInt) return R.Rf_ScalarReal(0.0);
    if (R.Rf_inherits(result.get(), "factor") == 0) return R.Rf_ScalarReal(0.0);
    const levels = R.Rf_getAttrib(result.get(), R.R_LevelsSymbol);
    if (R.XLENGTH(levels) != 2 or R.STRING_ELT(levels, 0) != R.Rf_mkChar("a") or R.STRING_ELT(levels, 1) != R.Rf_mkChar("b")) return R.Rf_ScalarReal(0.0);
    codes[0] = 1;
    if (R.STRING_ELT(input.get(), 0) != R.Rf_mkChar("b")) return R.Rf_ScalarReal(0.0);

    var empty_input = protect.scoped(R.Rf_allocVector(R.STRSXP, 0));
    defer empty_input.deinit();
    var empty = protect.scoped(factor.asFactorChecked(empty_input.get()) catch return R.Rf_ScalarReal(0.0));
    defer empty.deinit();
    if (R.XLENGTH(empty.get()) != 0 or R.XLENGTH(R.Rf_getAttrib(empty.get(), R.R_LevelsSymbol)) != 0) return R.Rf_ScalarReal(0.0);

    const utf8 = "caf\xc3\xa9";
    const latin1 = [_]u8{ 'c', 'a', 'f', 0xe9 };
    var encoded = protect.scoped(R.Rf_allocVector(R.STRSXP, 2));
    defer encoded.deinit();
    R.SET_STRING_ELT(encoded.get(), 0, R.Rf_mkCharLenCE(@ptrCast(utf8.ptr), @intCast(utf8.len), @as(R.cetype_t, @intCast(R.CE_UTF8))));
    R.SET_STRING_ELT(encoded.get(), 1, R.Rf_mkCharLenCE(@ptrCast(&latin1), @intCast(latin1.len), @as(R.cetype_t, @intCast(R.CE_LATIN1))));
    var encoded_factor = protect.scoped(factor.asFactorChecked(encoded.get()) catch return R.Rf_ScalarReal(0.0));
    defer encoded_factor.deinit();
    if (R.XLENGTH(R.Rf_getAttrib(encoded_factor.get(), R.R_LevelsSymbol)) != 1) return R.Rf_ScalarReal(0.0);
    if (R.INTEGER(encoded_factor.get())[0] != 1 or R.INTEGER(encoded_factor.get())[1] != 1) return R.Rf_ScalarReal(0.0);

    const alt_data = [_][]const u8{ "b", "a", "b" };
    var alt = protect.scoped(MyAltString.init(alt_data[0..]));
    defer alt.deinit();
    var alt_factor = protect.scoped(factor.asFactorChecked(alt.get()) catch return R.Rf_ScalarReal(0.0));
    defer alt_factor.deinit();
    if (R.ALTREP(alt.get()) == 0 or R.INTEGER(alt_factor.get())[0] != 2 or R.XLENGTH(R.Rf_getAttrib(alt_factor.get(), R.R_LevelsSymbol)) != 2) return R.Rf_ScalarReal(0.0);

    const many_count = 1025;
    var many = protect.scoped(R.Rf_allocVector(R.STRSXP, many_count));
    defer many.deinit();
    for (0..many_count) |i| {
        var buffer: [16]u8 = undefined;
        const value = std.fmt.bufPrint(&buffer, "level-{d:0>4}", .{i}) catch return R.Rf_ScalarReal(0.0);
        R.SET_STRING_ELT(many.get(), @intCast(i), R.Rf_mkCharLen(@ptrCast(value.ptr), @intCast(value.len)));
    }
    var many_factor = protect.scoped(factor.asFactorChecked(many.get()) catch return R.Rf_ScalarReal(0.0));
    defer many_factor.deinit();
    if (R.XLENGTH(R.Rf_getAttrib(many_factor.get(), R.R_LevelsSymbol)) != many_count) return R.Rf_ScalarReal(0.0);

    var wrong = protect.scoped(R.Rf_allocVector(R.INTSXP, 1));
    defer wrong.deinit();
    if (factor.asFactorChecked(wrong.get())) |_| return R.Rf_ScalarReal(0.0) else |factor_error| {
        if (factor_error != error.WrongType) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

fn defineS4ContractClass() bool {
    const result = embed.rCodeEval(
        "if (!methods::isClass('ZigrS4Contract')) methods::setClass('ZigrS4Contract', slots=c(value='numeric', label='character'), prototype=list(value=7, label='base')) else TRUE",
        R.R_GlobalEnv,
    );
    return result != R.R_NilValue;
}

export fn zigr_test_s4_contract() SEXP {
    if (!defineS4ContractClass()) return R.Rf_ScalarReal(0.0);

    var object = protect.scoped(s4.newObjectChecked("ZigrS4Contract") catch return R.Rf_ScalarReal(0.0));
    defer object.deinit();
    if (!s4.isS4(object.get()) or !s4.hasSlot(object.get(), "value") or !s4.hasSlot(object.get(), "label")) return R.Rf_ScalarReal(0.0);
    const initial = s4.getSlotChecked(object.get(), "value") catch return R.Rf_ScalarReal(0.0);
    if (R.TYPEOF(initial) != R.REALSXP or R.XLENGTH(initial) != 1 or R.REAL(initial)[0] != 7.0) return R.Rf_ScalarReal(0.0);

    var duplicate = protect.scoped(R.Rf_duplicate(object.get()));
    defer duplicate.deinit();
    var replacement = protect.scoped(R.Rf_ScalarReal(19.0));
    defer replacement.deinit();
    var assigned = protect.scoped(s4.setSlotChecked(duplicate.get(), "value", replacement.get()) catch return R.Rf_ScalarReal(0.0));
    defer assigned.deinit();
    R.R_gc();
    const changed = s4.getSlotChecked(assigned.get(), "value") catch return R.Rf_ScalarReal(0.0);
    const unchanged = s4.getSlotChecked(object.get(), "value") catch return R.Rf_ScalarReal(0.0);
    if (R.REAL(changed)[0] != 19.0 or R.REAL(unchanged)[0] != 7.0) return R.Rf_ScalarReal(0.0);

    var ordinary = protect.scoped(R.Rf_allocVector(R.VECSXP, 0));
    defer ordinary.deinit();
    if (s4.getSlotChecked(ordinary.get(), "value")) |_| return R.Rf_ScalarReal(0.0) else |slot_error| {
        if (slot_error != error.NotS4) return R.Rf_ScalarReal(0.0);
    }
    if (s4.getSlotChecked(object.get(), "missing")) |_| return R.Rf_ScalarReal(0.0) else |slot_error| {
        if (slot_error != error.MissingSlot) return R.Rf_ScalarReal(0.0);
    }
    if (s4.newObjectChecked("ZigrMissingS4Class")) |_| return R.Rf_ScalarReal(0.0) else |class_error| {
        if (class_error != error.MissingClass) return R.Rf_ScalarReal(0.0);
    }
    if (s4.newObjectChecked("bad\x00class")) |_| return R.Rf_ScalarReal(0.0) else |class_error| {
        if (class_error != error.InvalidClassName) return R.Rf_ScalarReal(0.0);
    }
    if (s4.setSlotChecked(object.get(), "value", null)) |_| return R.Rf_ScalarReal(0.0) else |slot_error| {
        if (slot_error != error.NullValue) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

const AltRepRetentionState = struct { marker: u8 = 1 };

fn defineAltRepHolderClass() bool {
    const result = embed.rCodeEval(
        "if (!methods::isClass('ZigrAltRepHolder')) methods::setClass('ZigrAltRepHolder', slots=c(value='ANY')) else TRUE",
        R.R_GlobalEnv,
    );
    return result != R.R_NilValue;
}

fn compactIntegerIsLazy(value: SEXP) bool {
    return R.ALTREP(value) != 0 and R.INTEGER_OR_NULL(value) == null;
}

export fn zigr_test_advanced_altrep_retention() SEXP {
    if (!defineAltRepHolderClass()) return R.Rf_ScalarReal(0.0);
    const compact = compactIntSequence(128) orelse return R.Rf_ScalarReal(0.0);
    defer R.Rf_unprotect(1);
    if (!compactIntegerIsLazy(compact)) return R.Rf_ScalarReal(0.0);

    const names = [_][]const u8{"compact"};
    const columns = [_]SEXP{compact};
    var frame = protect.scoped(df.buildChecked(names[0..], columns[0..]) catch return R.Rf_ScalarReal(0.0));
    defer frame.deinit();
    const wrapped = df.DataFrame.wrap(frame.get()) orelse return R.Rf_ScalarReal(0.0);
    if (wrapped.columnAt(0) != compact or !compactIntegerIsLazy(compact)) return R.Rf_ScalarReal(0.0);

    var attributed = protect.scoped(R.Rf_allocVector(R.VECSXP, 0));
    defer attributed.deinit();
    const payload_symbol = test_lang.symbol("zigr_altrep_payload");
    attrib.setAttrib(attributed.get(), payload_symbol, compact);
    if (attrib.getAttrib(attributed.get(), payload_symbol) != compact or !compactIntegerIsLazy(compact)) return R.Rf_ScalarReal(0.0);

    const args = [_]SEXP{compact};
    var call = protect.scoped(test_lang.buildCall(test_lang.symbol("identity"), args[0..]));
    defer call.deinit();
    if (R.CAR(R.CDR(call.get())) != compact or !compactIntegerIsLazy(compact)) return R.Rf_ScalarReal(0.0);
    const evaluated = test_eval.rEval(call.get(), R.R_BaseEnv);
    if (evaluated != compact or !compactIntegerIsLazy(compact)) return R.Rf_ScalarReal(0.0);

    var holder = protect.scoped(s4.newObjectChecked("ZigrAltRepHolder") catch return R.Rf_ScalarReal(0.0));
    defer holder.deinit();
    var assigned = protect.scoped(s4.setSlotChecked(holder.get(), "value", compact) catch return R.Rf_ScalarReal(0.0));
    defer assigned.deinit();
    if ((s4.getSlotChecked(assigned.get(), "value") catch return R.Rf_ScalarReal(0.0)) != compact or !compactIntegerIsLazy(compact)) return R.Rf_ScalarReal(0.0);

    var state = AltRepRetentionState{};
    var external = protect.scoped(zigr.externalptr.makeTyped(AltRepRetentionState, &state, compact));
    defer external.deinit();
    if ((zigr.externalptr.typedBacking(AltRepRetentionState, external.get()) catch return R.Rf_ScalarReal(0.0)) != compact or !compactIntegerIsLazy(compact)) return R.Rf_ScalarReal(0.0);

    R.R_gc();
    if (!compactIntegerIsLazy(compact)) return R.Rf_ScalarReal(0.0);
    const wrapped_after_gc = df.DataFrame.wrap(frame.get()) orelse return R.Rf_ScalarReal(0.0);
    if (wrapped_after_gc.columnAt(0) != compact) return R.Rf_ScalarReal(0.0);
    if ((s4.getSlotChecked(assigned.get(), "value") catch return R.Rf_ScalarReal(0.0)) != compact) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

threadlocal var s4_error_object: SEXP = null;

fn readMissingS4Slot() SEXP {
    return s4.getSlot(s4_error_object, "missing");
}

export fn zigr_test_s4_longjmp() SEXP {
    if (!defineS4ContractClass()) return R.Rf_ScalarReal(0.0);
    var object = protect.scoped(s4.newObjectChecked("ZigrS4Contract") catch return R.Rf_ScalarReal(0.0));
    defer object.deinit();
    s4_error_object = object.get();

    if (trycatch_mod.tryCatch(struct {
        fn call() SEXP {
            return cleanup.protectCall(readMissingS4Slot);
        }
    }.call)) |_| return R.Rf_ScalarReal(0.0) else |_| {}

    s4_error_object = null;
    var fresh = protect.scoped(s4.newObjectChecked("ZigrS4Contract") catch return R.Rf_ScalarReal(0.0));
    defer fresh.deinit();
    const value = s4.getSlotChecked(fresh.get(), "value") catch return R.Rf_ScalarReal(0.0);
    if (R.REAL(value)[0] != 7.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_raw_create() SEXP {
    const n: R.R_xlen_t = 4;
    const vec = R.Rf_protect(R.Rf_allocVector(R.RAWSXP, n));
    defer R.Rf_unprotect(1);
    const ptr = R.RAW(vec);
    const expected = [_]u8{ 0xde, 0xad, 0xbe, 0xef };
    for (0..expected.len) |i| ptr[i] = expected[i];
    for (0..expected.len) |i| {
        if (ptr[i] != expected[i]) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_cplx_create() SEXP {
    const n: R.R_xlen_t = 3;
    const vec = R.Rf_protect(R.Rf_allocVector(R.CPLXSXP, n));
    defer R.Rf_unprotect(1);
    const raw_ptr = R.COMPLEX(vec) orelse return R.Rf_ScalarReal(0.0);
    const ptr: [*]zigr_convert.Rcomplex = @ptrCast(@alignCast(raw_ptr));
    const expected = [_]zigr_convert.Rcomplex{
        .{ .r = 1.0, .i = 2.0 },
        .{ .r = 3.0, .i = 4.0 },
        .{ .r = 5.0, .i = 6.0 },
    };
    for (0..expected.len) |i| ptr[i] = expected[i];
    for (0..expected.len) |i| {
        if (ptr[i].r != expected[i].r or ptr[i].i != expected[i].i) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_attrib_class() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    defer R.Rf_unprotect(1);
    attrib.setClass(vec, "myclass");
    const cls = attrib.getClass(std.heap.page_allocator, vec) catch return R.Rf_ScalarReal(0.0);
    defer std.heap.page_allocator.free(cls);
    if (cls.len > 0 and std.mem.eql(u8, cls[0], "myclass")) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_attrib_names() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 3));
    defer R.Rf_unprotect(1);
    const names = [_][]const u8{ "a", "b", "c" };
    attrib.setNames(vec, names[0..]);
    const ns = attrib.getNames(std.heap.page_allocator, vec) catch return R.Rf_ScalarReal(0.0);
    defer std.heap.page_allocator.free(ns);
    const ok = ns.len == 3 and std.mem.eql(u8, ns[0], "a") and std.mem.eql(u8, ns[2], "c");
    var no_space: [0]u8 = .{};
    var no_alloc = std.heap.FixedBufferAllocator.init(&no_space);
    if (attrib.getNames(no_alloc.allocator(), vec)) |unexpected| {
        no_alloc.allocator().free(unexpected);
        return R.Rf_ScalarReal(0.0);
    } else |get_err| {
        if (get_err != error.OutOfMemory) return R.Rf_ScalarReal(0.0);
    }
    return if (ok) R.Rf_ScalarReal(1.0) else R.Rf_ScalarReal(0.0);
}

export fn zigr_test_attrib_contract() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    defer R.Rf_unprotect(1);

    const missing = attrib.getNames(std.heap.page_allocator, vec) catch return R.Rf_ScalarReal(0.0);
    defer std.heap.page_allocator.free(missing);
    if (missing.len != 0) return R.Rf_ScalarReal(0.0);

    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    defer R.Rf_unprotect(1);
    R.SET_STRING_ELT(names, 0, R.R_NaString);
    attrib.setAttrib(vec, R.R_NamesSymbol, names);
    const optional = attrib.getOptionalString(std.heap.page_allocator, vec, R.R_NamesSymbol) catch return R.Rf_ScalarReal(0.0);
    defer std.heap.page_allocator.free(optional);
    if (optional.len != 1 or optional[0] != null) return R.Rf_ScalarReal(0.0);

    const bad = R.Rf_protect(R.Rf_ScalarInteger(1));
    defer R.Rf_unprotect(1);
    const marker = zigr.symbols.install("zigr_service_bad_attribute");
    attrib.setAttrib(vec, marker, bad);

    if (attrib.getString(std.heap.page_allocator, vec, marker)) |value| {
        std.heap.page_allocator.free(value);
        return R.Rf_ScalarReal(0.0);
    } else |get_err| {
        if (get_err != error.ExpectedStringAttribute) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

const MyAlt = altrep_create.AltReal("zigr", "test_real");
const MyAltInt = altrep_create.AltInteger("zigr", "test_integer");
const MyAltLogical = altrep_create.AltLogical("zigr", "test_logical");
const MyAltRaw = altrep_create.AltRaw("zigr", "test_raw");
const MyAltComplex = altrep_create.AltComplex("zigr", "test_complex");
const MyAltString = altrep_create.AltString("zigr", "test_string");

const short_region_len: usize = 4097;
const short_region_cap: usize = 257;
var short_region_class: R.R_altrep_class_t = undefined;
var short_region_registered = false;
var short_region_get_calls: usize = 0;
var short_region_elt_calls: usize = 0;

fn shortRegionLength(_: SEXP) callconv(.c) R.R_xlen_t {
    return @intCast(short_region_len);
}

fn shortRegionDataptrOrNull(_: SEXP) callconv(.c) ?*const anyopaque {
    return null;
}

fn shortRegionElt(_: SEXP, index: R.R_xlen_t) callconv(.c) c_int {
    short_region_elt_calls += 1;
    return @intCast(index + 1);
}

fn shortRegionGetRegion(_: SEXP, start: R.R_xlen_t, requested: R.R_xlen_t, buffer: [*c]c_int) callconv(.c) R.R_xlen_t {
    if (buffer == null) return 0;
    const offset: usize = @intCast(start);
    if (offset >= short_region_len) return 0;

    short_region_get_calls += 1;
    const count = @min(@min(@as(usize, @intCast(requested)), short_region_cap), short_region_len - offset);
    const out: [*]i32 = @ptrCast(buffer);
    for (0..count) |i| out[i] = @intCast(offset + i + 1);
    return @intCast(count);
}

fn shortRegionAltInteger() SEXP {
    if (!short_region_registered) {
        short_region_class = R.R_make_altinteger_class("short_region_integer", "zigr", null);
        R.R_set_altrep_Length_method(short_region_class, shortRegionLength);
        R.R_set_altvec_Dataptr_or_null_method(short_region_class, shortRegionDataptrOrNull);
        R.R_set_altinteger_Elt_method(short_region_class, shortRegionElt);
        R.R_set_altinteger_Get_region_method(short_region_class, shortRegionGetRegion);
        short_region_registered = true;
    }
    return R.R_new_altrep(short_region_class, R.R_NilValue, R.R_NilValue);
}

var short_raw_region_class: R.R_altrep_class_t = undefined;
var short_raw_region_registered = false;
var short_raw_region_get_calls: usize = 0;
var short_raw_region_elt_calls: usize = 0;

fn shortRawRegionElt(_: SEXP, index: R.R_xlen_t) callconv(.c) R.Rbyte {
    short_raw_region_elt_calls += 1;
    return @intCast(@as(usize, @intCast(index)) % 251);
}

fn shortRawRegionGetRegion(_: SEXP, start: R.R_xlen_t, requested: R.R_xlen_t, buffer: [*c]R.Rbyte) callconv(.c) R.R_xlen_t {
    if (buffer == null) return 0;
    const offset: usize = @intCast(start);
    if (offset >= short_region_len) return 0;

    short_raw_region_get_calls += 1;
    const count = @min(@min(@as(usize, @intCast(requested)), short_region_cap), short_region_len - offset);
    const out: [*]u8 = @ptrCast(buffer);
    for (0..count) |i| out[i] = @intCast((offset + i) % 251);
    return @intCast(count);
}

fn shortRegionAltRaw() SEXP {
    if (!short_raw_region_registered) {
        short_raw_region_class = R.R_make_altraw_class("short_region_raw", "zigr", null);
        R.R_set_altrep_Length_method(short_raw_region_class, shortRegionLength);
        R.R_set_altvec_Dataptr_or_null_method(short_raw_region_class, shortRegionDataptrOrNull);
        R.R_set_altraw_Elt_method(short_raw_region_class, shortRawRegionElt);
        R.R_set_altraw_Get_region_method(short_raw_region_class, shortRawRegionGetRegion);
        short_raw_region_registered = true;
    }
    return R.R_new_altrep(short_raw_region_class, R.R_NilValue, R.R_NilValue);
}

var short_complex_region_class: R.R_altrep_class_t = undefined;
var short_complex_region_registered = false;
var short_complex_region_get_calls: usize = 0;

fn shortComplexRegionGetRegion(_: SEXP, start: R.R_xlen_t, requested: R.R_xlen_t, buffer: ?*R.Rcomplex) callconv(.c) R.R_xlen_t {
    const output: [*]zigr_convert.Rcomplex = @ptrCast(@alignCast(buffer orelse return 0));
    const offset: usize = @intCast(start);
    if (offset >= short_region_len) return 0;

    short_complex_region_get_calls += 1;
    const count = @min(@min(@as(usize, @intCast(requested)), short_region_cap), short_region_len - offset);
    for (0..count) |i| {
        const value: f64 = @floatFromInt(offset + i + 1);
        output[i] = .{ .r = value, .i = -value };
    }
    return @intCast(count);
}

fn shortRegionAltComplex() SEXP {
    if (!short_complex_region_registered) {
        short_complex_region_class = R.R_make_altcomplex_class("short_region_complex", "zigr", null);
        R.R_set_altrep_Length_method(short_complex_region_class, shortRegionLength);
        R.R_set_altvec_Dataptr_or_null_method(short_complex_region_class, shortRegionDataptrOrNull);
        R.R_set_altcomplex_Get_region_method(short_complex_region_class, shortComplexRegionGetRegion);
        short_complex_region_registered = true;
    }
    return R.R_new_altrep(short_complex_region_class, R.R_NilValue, R.R_NilValue);
}

fn compactIntSequence(n: i32) ?SEXP {
    const length = R.Rf_protect(R.Rf_ScalarInteger(n));
    defer R.Rf_unprotect(1);
    const call = R.Rf_protect(R.Rf_lang2(R.Rf_install("seq_len"), length));
    defer R.Rf_unprotect(1);
    var failed: c_int = 0;
    const result = R.R_tryEvalSilent(call, R.R_GlobalEnv, &failed);
    if (failed != 0) return null;
    return R.Rf_protect(result);
}

fn compactRealSequence(n: f64) ?SEXP {
    const length = R.Rf_protect(R.Rf_ScalarReal(n));
    defer R.Rf_unprotect(1);
    const integer_call = R.Rf_protect(R.Rf_lang2(R.Rf_install("seq_len"), length));
    defer R.Rf_unprotect(1);
    const real_call = R.Rf_protect(R.Rf_lang2(R.Rf_install("as.double"), integer_call));
    defer R.Rf_unprotect(1);
    var failed: c_int = 0;
    const result = R.R_tryEvalSilent(real_call, R.R_GlobalEnv, &failed);
    if (failed != 0) return null;
    return R.Rf_protect(result);
}

const AltRealSliceWrap = struct {
    ptr: [*]const f64,
    len: usize,
};
const AltIntSliceWrap = struct {
    ptr: [*]const i32,
    len: usize,
};
const zigr_altreal_slice_tag_name = "zigr_altreal_slice_wrap";
const zigr_altinteger_slice_tag_name = "zigr_altinteger_slice_wrap";
const zigr_altlogical_slice_tag_name = "zigr_altlogical_slice_wrap";

export fn zigr_test_altrep_create() SEXP {
    const data = [_]f64{ 1.0, 2.0, 3.0, 4.0, 5.0 };
    const vec = MyAlt.init(data[0..]);
    if (altrep_mod.isAltRep(null) or altrep_mod.className(null).len != 0 or altrep_mod.classPackage(null).len != 0) {
        return R.Rf_ScalarReal(0.0);
    }
    if (R.XLENGTH(vec) != 5) return R.Rf_ScalarReal(0.0);
    if (!std.mem.eql(u8, altrep_mod.className(vec), "test_real")) return R.Rf_ScalarReal(0.0);
    if (!std.mem.eql(u8, altrep_mod.classPackage(vec), "zigr")) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altrep_sum_simd() SEXP {
    const data = [_]f64{ 1.0, 2.0, 3.0, 4.0, 5.0 };
    const vec = MyAlt.init(data[0..]);
    const total = zigr_convert.sum(vec);
    if (total != 15.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altrep_direct_ptr() SEXP {
    const data = [_]f64{ 1.0, 2.0, 3.0, 4.0, 5.0 };
    const vec = MyAlt.init(data[0..]);
    const data1 = R.R_altrep_data1(vec);
    if (R.TYPEOF(data1) != R.EXTPTRSXP) return R.Rf_ScalarReal(0.0);
    if (R.R_ExternalPtrTag(data1) != R.Rf_install(zigr_altreal_slice_tag_name)) return R.Rf_ScalarReal(0.0);
    const ptr = R.R_ExternalPtrAddr(data1) orelse return R.Rf_ScalarReal(0.0);
    const wrap: *const AltRealSliceWrap = @ptrCast(@alignCast(ptr));
    const ok = wrap.len == 5 and wrap.ptr[0] == 1.0 and wrap.ptr[4] == 5.0;
    return R.Rf_ScalarReal(if (ok) 1.0 else 0.0);
}

export fn zigr_test_altint_direct_slice() SEXP {
    const data = [_]i32{ 4, 2, 9, -1, 3 };
    const vec = MyAltInt.init(data[0..]);
    const data1 = R.R_altrep_data1(vec);
    if (R.TYPEOF(data1) != R.EXTPTRSXP) return R.Rf_ScalarReal(0.0);
    if (R.R_ExternalPtrTag(data1) != R.Rf_install(zigr_altinteger_slice_tag_name)) return R.Rf_ScalarReal(0.0);
    const ptr = R.R_ExternalPtrAddr(data1) orelse return R.Rf_ScalarReal(0.0);
    const wrap: *const AltIntSliceWrap = @ptrCast(@alignCast(ptr));
    if (wrap.len != 5 or wrap.ptr[0] != 4 or wrap.ptr[3] != -1) return R.Rf_ScalarReal(0.0);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const slice = zigr_convert.toIntSlice(arena.allocator(), vec) catch return R.Rf_ScalarReal(0.0);
    const ok = slice.len == 5 and slice[0] == 4 and slice[1] == 2 and slice[2] == 9 and slice[3] == -1 and slice[4] == 3;
    return R.Rf_ScalarReal(if (ok) 1.0 else 0.0);
}

export fn zigr_test_altint_sum_direct() SEXP {
    const data = [_]i32{ 4, 2, 9, -1, 3 };
    const vec = MyAltInt.init(data[0..]);
    if (zigr_convert.sumInt(vec) != 17) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altint_min_direct() SEXP {
    const data = [_]i32{ 4, -1, 9, -1, 3 };
    const vec = MyAltInt.init(data[0..]);
    if (zigr_convert.minInt(vec) != -1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altint_max_direct() SEXP {
    const data = [_]i32{ 4, -1, 9, 9, 3 };
    const vec = MyAltInt.init(data[0..]);
    if (zigr_convert.maxInt(vec) != 9) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altint_argmin_direct() SEXP {
    const data = [_]i32{ 4, -1, 9, -1, 3 };
    const vec = MyAltInt.init(data[0..]);
    if (zigr_convert.argminInt(vec) != 1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altint_argmax_direct() SEXP {
    const data = [_]i32{ 4, 9, 9, -1, 3 };
    const vec = MyAltInt.init(data[0..]);
    if (zigr_convert.argmaxInt(vec) != 1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altlogical_direct_slice() SEXP {
    const data = [_]i32{ 1, 0, R.R_NaInt, 1 };
    const vec = MyAltLogical.init(data[0..]);
    const data1 = R.R_altrep_data1(vec);
    if (R.TYPEOF(data1) != R.EXTPTRSXP) return R.Rf_ScalarReal(0.0);
    if (R.R_ExternalPtrTag(data1) != R.Rf_install(zigr_altlogical_slice_tag_name)) return R.Rf_ScalarReal(0.0);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const slice = zigr_convert.toLogicalSlice(arena.allocator(), vec) catch return R.Rf_ScalarReal(0.0);
    const ok = slice.len == 4 and slice[0] == 1 and slice[1] == 0 and slice[2] == R.R_NaInt and slice[3] == 1;
    return R.Rf_ScalarReal(if (ok) 1.0 else 0.0);
}

export fn zigr_test_altlogical_count_true_direct() SEXP {
    const data = [_]i32{ 1, 0, R.R_NaInt, 1 };
    const vec = MyAltLogical.init(data[0..]);
    if (zigr_convert.countTrue(vec) != 2) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altlogical_min_direct() SEXP {
    const data = [_]i32{ 1, 0, 1, 0 };
    const vec = MyAltLogical.init(data[0..]);
    if (zigr_convert.minLogical(vec) != 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altlogical_max_direct() SEXP {
    const data = [_]i32{ 0, 1, 0, 1 };
    const vec = MyAltLogical.init(data[0..]);
    if (zigr_convert.maxLogical(vec) != 1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altlogical_argmin_direct() SEXP {
    const data = [_]i32{ 1, 0, 1, 0 };
    const vec = MyAltLogical.init(data[0..]);
    if (zigr_convert.argminLogical(vec) != 1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altlogical_argmax_direct() SEXP {
    const data = [_]i32{ 0, 1, 1, 0 };
    const vec = MyAltLogical.init(data[0..]);
    if (zigr_convert.argmaxLogical(vec) != 1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altrep_mean_simd() SEXP {
    const data = [_]f64{ 1.0, 2.0, 3.0, 4.0, 5.0 };
    const vec = MyAlt.init(data[0..]);
    if (zigr_convert.mean(vec) != 3.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altrep_norm2_simd() SEXP {
    const data = [_]f64{ 1.0, 2.0, 3.0, 4.0, 5.0 };
    const vec = MyAlt.init(data[0..]);
    if (@abs(zigr_convert.norm2(vec) - 55.0) > 1e-12) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altrep_min_simd() SEXP {
    const data = [_]f64{ 4.0, 2.0, 9.0, -1.0, 3.0 };
    const vec = MyAlt.init(data[0..]);
    if (zigr_convert.min(vec) != -1.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altrep_max_simd() SEXP {
    const data = [_]f64{ 4.0, 2.0, 9.0, -1.0, 3.0 };
    const vec = MyAlt.init(data[0..]);
    if (zigr_convert.max(vec) != 9.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altrep_argmin_simd() SEXP {
    const data = [_]f64{ 4.0, -1.0, 9.0, -1.0, 3.0 };
    const vec = MyAlt.init(data[0..]);
    if (zigr_convert.argmin(vec) != 1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altrep_argmax_simd() SEXP {
    const data = [_]f64{ 4.0, 9.0, 9.0, -1.0, 3.0 };
    const vec = MyAlt.init(data[0..]);
    if (zigr_convert.argmax(vec) != 1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altrep_sum_narm_simd() SEXP {
    var data = [_]f64{ 1.0, 0.0, 4.0, 5.0 };
    data[1] = R.NA_REAL();
    const vec = MyAlt.init(data[0..]);
    if (zigr_convert.sum_narm(vec) != 10.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altrep_mean_narm_simd() SEXP {
    var data = [_]f64{ 1.0, 0.0, 5.0 };
    data[1] = R.NA_REAL();
    const vec = MyAlt.init(data[0..]);
    if (zigr_convert.mean_narm(vec) != 3.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altraw_create() SEXP {
    const data = [_]u8{ 0xde, 0xad, 0xbe, 0xef };
    const vec = MyAltRaw.init(data[0..]);
    if (R.XLENGTH(vec) != 4) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altcomplex_create() SEXP {
    const data = [_]altrep_create.ComplexElem{
        .{ .r = 1.0, .i = 2.0 },
        .{ .r = 3.0, .i = 4.0 },
    };
    const vec = MyAltComplex.init(data[0..]);
    if (R.XLENGTH(vec) != 2) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_borrowed_views() SEXP {
    var no_alloc_storage: [0]u8 align(16) = .{};
    var no_alloc = std.heap.FixedBufferAllocator.init(&no_alloc_storage);

    const empty = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 0));
    defer R.Rf_unprotect(1);
    var empty_view = zigr_convert.toRealSliceView(no_alloc.allocator(), empty) catch return R.Rf_ScalarReal(0.0);
    defer empty_view.deinit();
    switch (empty_view) {
        .borrowed => {},
        .owned => return R.Rf_ScalarReal(0.0),
    }
    if (empty_view.constSlice().len != 0 or no_alloc.end_index != 0) return R.Rf_ScalarReal(0.0);

    const real = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 4));
    defer R.Rf_unprotect(1);
    const real_ptr = R.REAL(real);
    real_ptr[0] = 1.0;
    real_ptr[1] = R.NA_REAL();
    real_ptr[2] = std.math.nan(f64);
    real_ptr[3] = -4.0;
    var real_view = zigr_convert.toRealSliceView(no_alloc.allocator(), real) catch return R.Rf_ScalarReal(0.0);
    defer real_view.deinit();
    switch (real_view) {
        .borrowed => {},
        .owned => return R.Rf_ScalarReal(0.0),
    }
    const real_slice: []const f64 = real_view.constSlice();
    if (@intFromPtr(real_slice.ptr) % @alignOf(f64) != 0) return R.Rf_ScalarReal(0.0);
    if (real_slice.len != 4 or real_slice[0] != 1.0 or real_slice[3] != -4.0) return R.Rf_ScalarReal(0.0);
    if (R.ISNA(real_slice[1]) == 0 or R.ISNA(real_slice[2]) != 0 or !R.ISNAN(real_slice[2])) return R.Rf_ScalarReal(0.0);

    const integer = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 3));
    defer R.Rf_unprotect(1);
    const int_ptr = R.INTEGER(integer);
    int_ptr[0] = 7;
    int_ptr[1] = R.R_NaInt;
    int_ptr[2] = -9;
    var int_view = zigr_convert.toIntSliceView(no_alloc.allocator(), integer) catch return R.Rf_ScalarReal(0.0);
    defer int_view.deinit();
    switch (int_view) {
        .borrowed => {},
        .owned => return R.Rf_ScalarReal(0.0),
    }
    const int_slice: []const i32 = int_view.constSlice();
    if (@intFromPtr(int_slice.ptr) % @alignOf(i32) != 0) return R.Rf_ScalarReal(0.0);
    if (int_slice.len != 3 or int_slice[0] != 7 or int_slice[1] != R.R_NaInt or int_slice[2] != -9) return R.Rf_ScalarReal(0.0);

    const logical = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, 3));
    defer R.Rf_unprotect(1);
    const logical_ptr = R.LOGICAL(logical);
    logical_ptr[0] = 1;
    logical_ptr[1] = 0;
    logical_ptr[2] = R.R_NaInt;
    var logical_view = zigr_convert.toLogicalSliceView(no_alloc.allocator(), logical) catch return R.Rf_ScalarReal(0.0);
    defer logical_view.deinit();
    switch (logical_view) {
        .borrowed => {},
        .owned => return R.Rf_ScalarReal(0.0),
    }
    const logical_slice: []const i32 = logical_view.constSlice();
    if (@intFromPtr(logical_slice.ptr) % @alignOf(i32) != 0) return R.Rf_ScalarReal(0.0);
    if (logical_slice.len != 3 or logical_slice[0] != 1 or logical_slice[1] != 0 or logical_slice[2] != R.R_NaInt) return R.Rf_ScalarReal(0.0);

    const complex = R.Rf_protect(R.Rf_allocVector(R.CPLXSXP, 2));
    defer R.Rf_unprotect(1);
    const complex_raw = R.COMPLEX(complex) orelse return R.Rf_ScalarReal(0.0);
    const complex_ptr: [*]zigr_convert.Rcomplex = @ptrCast(@alignCast(complex_raw));
    complex_ptr[0] = .{ .r = 1.0, .i = -2.0 };
    complex_ptr[1] = .{ .r = R.NA_REAL(), .i = 3.0 };
    var complex_view = zigr_convert.toComplexSliceView(no_alloc.allocator(), complex) catch return R.Rf_ScalarReal(0.0);
    defer complex_view.deinit();
    switch (complex_view) {
        .borrowed => {},
        .owned => return R.Rf_ScalarReal(0.0),
    }
    const complex_slice: []const zigr_convert.Rcomplex = complex_view.constSlice();
    if (@intFromPtr(complex_slice.ptr) % @alignOf(zigr_convert.Rcomplex) != 0) return R.Rf_ScalarReal(0.0);
    if (complex_slice.len != 2 or complex_slice[0].r != 1.0 or complex_slice[0].i != -2.0 or R.ISNA(complex_slice[1].r) == 0) return R.Rf_ScalarReal(0.0);

    if (zigr_convert.toLogicalSliceView(no_alloc.allocator(), integer)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |conversion_err| {
        if (conversion_err != error.ExpectedLogical) return R.Rf_ScalarReal(0.0);
    }
    if (no_alloc.end_index != 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_compact_altrep_views() SEXP {
    const integer = compactIntSequence(@intCast(short_region_len)) orelse return R.Rf_ScalarReal(0.0);
    defer R.Rf_unprotect(1);
    if (R.ALTREP(integer) == 0 or R.INTEGER_OR_NULL(integer) != null) return R.Rf_ScalarReal(0.0);

    var int_storage: [short_region_len * @sizeOf(i32)]u8 align(16) = undefined;
    var int_fba = std.heap.FixedBufferAllocator.init(&int_storage);
    var int_view = zigr_convert.toIntSliceView(int_fba.allocator(), integer) catch return R.Rf_ScalarReal(0.0);
    switch (int_view) {
        .borrowed => return R.Rf_ScalarReal(0.0),
        .owned => {},
    }
    const int_slice = int_view.constSlice();
    if (int_slice.len != short_region_len or int_slice[0] != 1 or int_slice[64] != 65 or int_slice[512] != 513 or int_slice[4096] != 4097) return R.Rf_ScalarReal(0.0);
    if (int_fba.end_index != int_storage.len) return R.Rf_ScalarReal(0.0);
    int_view.deinit();
    if (int_fba.end_index != 0) return R.Rf_ScalarReal(0.0);

    const int_rvector = zigr.rvector.RVector(i32).init(integer) catch return R.Rf_ScalarReal(0.0);
    var rvector_view = int_rvector.view(int_fba.allocator()) catch return R.Rf_ScalarReal(0.0);
    switch (rvector_view) {
        .borrowed => return R.Rf_ScalarReal(0.0),
        .owned => {},
    }
    if (rvector_view.constSlice()[4096] != 4097 or int_fba.end_index != int_storage.len) return R.Rf_ScalarReal(0.0);
    rvector_view.deinit();
    if (int_fba.end_index != 0) return R.Rf_ScalarReal(0.0);

    const real = compactRealSequence(@floatFromInt(short_region_len)) orelse return R.Rf_ScalarReal(0.0);
    defer R.Rf_unprotect(1);
    if (R.ALTREP(real) == 0 or R.REAL_OR_NULL(real) != null) return R.Rf_ScalarReal(0.0);

    var real_storage: [short_region_len * @sizeOf(f64)]u8 align(16) = undefined;
    var real_fba = std.heap.FixedBufferAllocator.init(&real_storage);
    var real_view = zigr_convert.toRealSliceView(real_fba.allocator(), real) catch return R.Rf_ScalarReal(0.0);
    switch (real_view) {
        .borrowed => return R.Rf_ScalarReal(0.0),
        .owned => {},
    }
    const real_slice = real_view.constSlice();
    if (real_slice.len != short_region_len or real_slice[0] != 1.0 or real_slice[64] != 65.0 or real_slice[512] != 513.0 or real_slice[4096] != 4097.0) return R.Rf_ScalarReal(0.0);
    if (real_fba.end_index != real_storage.len) return R.Rf_ScalarReal(0.0);
    real_view.deinit();
    if (real_fba.end_index != 0) return R.Rf_ScalarReal(0.0);

    const long_real = compactRealSequence(2_147_483_648.0) orelse return R.Rf_ScalarReal(0.0);
    defer R.Rf_unprotect(1);
    const long_view = zigr.rvector.RVector(f64).init(long_real) catch return R.Rf_ScalarReal(0.0);
    if (R.ALTREP(long_real) == 0 or @as(u64, @intCast(long_view.len())) != 2_147_483_648) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_short_region() SEXP {
    const integer = R.Rf_protect(shortRegionAltInteger());
    defer R.Rf_unprotect(1);
    if (R.ALTREP(integer) == 0 or R.INTEGER_OR_NULL(integer) != null) return R.Rf_ScalarReal(0.0);

    short_region_get_calls = 0;
    short_region_elt_calls = 0;
    const expected_sum = @as(i64, @intCast(short_region_len)) * @as(i64, @intCast(short_region_len + 1)) / 2;
    if (zigr_convert.sumInt(integer) != expected_sum or short_region_get_calls <= 1 or short_region_elt_calls != 0) return R.Rf_ScalarReal(0.0);

    short_region_get_calls = 0;
    short_region_elt_calls = 0;
    var storage: [short_region_len * @sizeOf(i32)]u8 align(16) = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&storage);
    var view = zigr_convert.toIntSliceView(fba.allocator(), integer) catch return R.Rf_ScalarReal(0.0);
    switch (view) {
        .borrowed => return R.Rf_ScalarReal(0.0),
        .owned => {},
    }
    const slice = view.constSlice();
    const expected_regions = (short_region_len + short_region_cap - 1) / short_region_cap;
    if (slice.len != short_region_len or slice[0] != 1 or slice[512] != 513 or slice[4096] != 4097) return R.Rf_ScalarReal(0.0);
    if (short_region_get_calls != expected_regions or short_region_elt_calls != 0 or fba.end_index != storage.len) return R.Rf_ScalarReal(0.0);
    view.deinit();
    if (fba.end_index != 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

fn zigr_fib(n: i64) i64 {
    if (n <= 1) return n;
    return zigr_fib(n - 1) + zigr_fib(n - 2);
}

export fn zigr_test_fib_recursive() SEXP {
    const result = zigr_fib(20);
    if (result != 6765) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altstring_create() SEXP {
    const data = [_][]const u8{ "alpha", "beta", "gamma" };
    const vec = MyAltString.init(data[0..]);
    if (R.XLENGTH(vec) != 3) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_string_representations() SEXP {
    const utf8 = "na\xc3\xafve";
    const byte_marked = [_]u8{ 'x', 0xff, 'y' };
    const latin1 = [_]u8{ 'c', 'a', 'f', 0xe9 };
    const latin1_utf8 = "caf\xc3\xa9";
    const vec = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 5));
    defer R.Rf_unprotect(1);
    R.SET_STRING_ELT(vec, 0, R.Rf_mkCharLenCE(@ptrCast(utf8.ptr), @intCast(utf8.len), @as(R.cetype_t, @intCast(R.CE_UTF8))));
    R.SET_STRING_ELT(vec, 1, R.Rf_mkCharLenCE("", 0, @as(R.cetype_t, @intCast(R.CE_UTF8))));
    R.SET_STRING_ELT(vec, 2, R.R_NaString);
    R.SET_STRING_ELT(vec, 3, R.Rf_mkCharLenCE(@ptrCast(&byte_marked), @intCast(byte_marked.len), @as(R.cetype_t, @intCast(R.CE_BYTES))));
    R.SET_STRING_ELT(vec, 4, R.Rf_mkCharLenCE(@ptrCast(&latin1), @intCast(latin1.len), @as(R.cetype_t, @intCast(R.CE_LATIN1))));

    const view = zigr_convert.toStringSliceView(vec) catch return R.Rf_ScalarReal(0.0);
    const first = view.at(0);
    const empty = view.at(1);
    const na = view.at(2);
    const bytes = view.at(3);
    const translated_latin1 = view.at(4);
    if (first.is_na or first.encoding_mark != @as(R.cetype_t, @intCast(R.CE_UTF8)) or !std.mem.eql(u8, first.bytes, utf8)) return R.Rf_ScalarReal(0.0);
    if (empty.is_na or empty.len != 0) return R.Rf_ScalarReal(0.0);
    if (!na.is_na or na.len != 0 or na.encoding_mark != @as(R.cetype_t, @intCast(R.CE_NATIVE))) return R.Rf_ScalarReal(0.0);
    if (bytes.is_na or bytes.encoding_mark != @as(R.cetype_t, @intCast(R.CE_BYTES)) or !std.mem.eql(u8, bytes.bytes, &byte_marked)) return R.Rf_ScalarReal(0.0);
    if (translated_latin1.is_na or translated_latin1.encoding_mark != @as(R.cetype_t, @intCast(R.CE_LATIN1)) or !std.mem.eql(u8, translated_latin1.bytes, latin1_utf8)) return R.Rf_ScalarReal(0.0);

    var cache_storage: [5 * @sizeOf(zigr_convert.StringView)]u8 align(@alignOf(zigr_convert.StringView)) = undefined;
    var cache_fba = std.heap.FixedBufferAllocator.init(&cache_storage);
    var cached = zigr_convert.toCachedStringSliceView(cache_fba.allocator(), vec) catch return R.Rf_ScalarReal(0.0);
    if (cached.at(2).is_na != na.is_na or cached.at(3).encoding_mark != bytes.encoding_mark or !std.mem.eql(u8, cached.at(4).bytes, latin1_utf8)) return R.Rf_ScalarReal(0.0);
    if (cache_fba.end_index != cache_storage.len) return R.Rf_ScalarReal(0.0);

    var headers_storage: [5 * @sizeOf([]const u8)]u8 align(@alignOf([]const u8)) = undefined;
    var headers_fba = std.heap.FixedBufferAllocator.init(&headers_storage);
    const headers = zigr_convert.toStringSlice(headers_fba.allocator(), vec) catch return R.Rf_ScalarReal(0.0);
    defer headers_fba.allocator().free(headers);
    if (!std.mem.eql(u8, headers[0], utf8) or headers[1].len != 0 or headers[2].len != 0 or !std.mem.eql(u8, headers[3], &byte_marked) or !std.mem.eql(u8, headers[4], latin1_utf8)) return R.Rf_ScalarReal(0.0);

    var view_total: usize = 0;
    var cached_total: usize = 0;
    var headers_total: usize = 0;
    for (0..3) |_| {
        var iterator = view.iterator();
        while (iterator.next()) |item| {
            if (!item.is_na) view_total += item.len;
        }
        var cached_iterator = cached.iterator();
        while (cached_iterator.next()) |item| {
            if (!item.is_na) cached_total += item.len;
        }
        for (headers) |item| headers_total += item.len;
    }
    if (view_total != cached_total or view_total != headers_total) return R.Rf_ScalarReal(0.0);
    cached.deinit();
    if (cache_fba.end_index != 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altstring_inputs() SEXP {
    const data = [_][]const u8{ "alpha", "", "gamma" };
    const vec = MyAltString.init(data[0..]);
    if (R.ALTREP(vec) == 0) return R.Rf_ScalarReal(0.0);

    const view = zigr_convert.toStringSliceView(vec) catch return R.Rf_ScalarReal(0.0);
    if (view.len != data.len or !std.mem.eql(u8, view.at(0).bytes, "alpha") or view.at(1).len != 0 or !std.mem.eql(u8, view.at(2).bytes, "gamma")) return R.Rf_ScalarReal(0.0);

    var storage: [data.len * @sizeOf(zigr_convert.StringView)]u8 align(@alignOf(zigr_convert.StringView)) = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&storage);
    var cached = zigr_convert.toCachedStringSliceView(fba.allocator(), vec) catch return R.Rf_ScalarReal(0.0);
    defer cached.deinit();
    if (cached.len != data.len or !std.mem.eql(u8, cached.at(0).bytes, "alpha") or cached.at(1).len != 0 or !std.mem.eql(u8, cached.at(2).bytes, "gamma")) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altrep_owned_storage() SEXP {
    var numbers = [_]f64{ 1.0, 2.0, 3.0 };
    const numeric = R.Rf_protect(MyAlt.init(numbers[0..]));
    defer R.Rf_unprotect(2);
    numbers = .{ 9.0, 9.0, 9.0 };

    const words = [_][]const u8{ "alpha", "beta" };
    const strings = R.Rf_protect(MyAltString.init(words[0..]));
    if (R.TYPEOF(R.R_altrep_data1(strings)) != R.STRSXP) return R.Rf_ScalarReal(0.0);
    if (R.STRING_IS_SORTED(strings) != R.UNKNOWN_SORTEDNESS) return R.Rf_ScalarReal(0.0);

    R.R_gc();
    if (R.REAL_ELT(numeric, 0) != 1.0 or R.REAL_ELT(numeric, 2) != 3.0) return R.Rf_ScalarReal(0.0);
    if (!std.mem.eql(u8, std.mem.sliceTo(R.R_CHAR(R.STRING_ELT(strings, 0)), 0), "alpha")) return R.Rf_ScalarReal(0.0);
    if (!std.mem.eql(u8, std.mem.sliceTo(R.R_CHAR(R.STRING_ELT(strings, 1)), 0), "beta")) return R.Rf_ScalarReal(0.0);

    var duplicate = protect.scoped(R.Rf_duplicate(numeric));
    defer duplicate.deinit();
    if (R.ALTREP(duplicate.get()) != 0 or R.TYPEOF(duplicate.get()) != R.REALSXP) return R.Rf_ScalarReal(0.0);
    R.REAL(duplicate.get())[0] = 20.0;
    if (R.REAL_ELT(numeric, 0) != 1.0 or R.REAL(duplicate.get())[0] != 20.0) return R.Rf_ScalarReal(0.0);

    var string_duplicate = protect.scoped(R.Rf_duplicate(strings));
    defer string_duplicate.deinit();
    if (R.ALTREP(string_duplicate.get()) != 0 or R.TYPEOF(string_duplicate.get()) != R.STRSXP) return R.Rf_ScalarReal(0.0);
    if (!std.mem.eql(u8, std.mem.sliceTo(R.R_CHAR(R.STRING_ELT(string_duplicate.get(), 1)), 0), "beta")) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

fn forceGcAndDrainFinalizers() void {
    R.R_gc();
    R.R_RunPendingFinalizers();
}

fn finalizerDiagnosticsEqual(invocations: usize, destructions: usize) bool {
    const diagnostics = MyAlt.finalizerDiagnostics();
    return diagnostics.invocations == invocations and diagnostics.destructions == destructions;
}

export fn zigr_test_altrep_finalizer_lifecycle() SEXP {
    // Clear native wrappers left unreachable by earlier tests before establishing
    // this test's baseline.
    forceGcAndDrainFinalizers();
    forceGcAndDrainFinalizers();
    MyAlt.resetFinalizerDiagnostics();

    var duplicates = protect.scoped(R.Rf_allocVector(R.VECSXP, 2));
    defer duplicates.deinit();
    const values = [_]f64{ 1.0, 2.0, 3.0 };
    const owned = protect.protect(MyAlt.init(values[0..]));
    const deep = protect.protect(R.Rf_duplicate(owned));
    const shallow = protect.protect(R.Rf_shallow_duplicate(owned));
    _ = R.SET_VECTOR_ELT(duplicates.get(), 0, deep);
    _ = R.SET_VECTOR_ELT(duplicates.get(), 1, shallow);
    if (!finalizerDiagnosticsEqual(0, 0)) return R.Rf_ScalarReal(0.0);
    protect.unprotectN(3);

    forceGcAndDrainFinalizers();
    if (!finalizerDiagnosticsEqual(1, 1)) return R.Rf_ScalarReal(0.0);
    R.R_RunPendingFinalizers();
    if (!finalizerDiagnosticsEqual(1, 1)) return R.Rf_ScalarReal(0.0);
    forceGcAndDrainFinalizers();
    if (!finalizerDiagnosticsEqual(1, 1)) return R.Rf_ScalarReal(0.0);
    if (R.ALTREP(deep) != 0 or R.ALTREP(shallow) != 0) return R.Rf_ScalarReal(0.0);
    R.REAL(deep)[0] = 20.0;
    R.REAL(shallow)[1] = 30.0;
    if (R.REAL(deep)[0] != 20.0 or R.REAL(deep)[1] != 2.0) return R.Rf_ScalarReal(0.0);
    if (R.REAL(shallow)[0] != 1.0 or R.REAL(shallow)[1] != 30.0) return R.Rf_ScalarReal(0.0);

    _ = R.SET_VECTOR_ELT(duplicates.get(), 0, R.R_NilValue);
    _ = R.SET_VECTOR_ELT(duplicates.get(), 1, R.R_NilValue);
    forceGcAndDrainFinalizers();
    forceGcAndDrainFinalizers();
    return R.Rf_ScalarReal(if (finalizerDiagnosticsEqual(1, 1)) 1.0 else 0.0);
}

fn ownedAltrepRoundTrip(value: SEXP, version: serialize_mod.Version) SEXP {
    var serialized = protect.scoped(serialize_mod.toVectorVersion(value, version));
    defer serialized.deinit();
    return serialize_mod.fromVector(serialized.get());
}

export fn zigr_test_altrep_persistence_fixture() SEXP {
    const values = [_]f64{ 1.5, R.NA_REAL(), -0.0 };
    var result = protect.scoped(MyAlt.init(values[0..]));
    defer result.deinit();
    var names = protect.scoped(R.Rf_allocVector(R.STRSXP, values.len));
    defer names.deinit();
    for ([_][]const u8{ "one", "missing", "negative_zero" }, 0..) |name, index| {
        R.SET_STRING_ELT(
            names.get(),
            @intCast(index),
            R.Rf_mkCharLenCE(@ptrCast(name.ptr), @intCast(name.len), @as(R.cetype_t, @intCast(R.CE_UTF8))),
        );
    }
    _ = R.Rf_setAttrib(result.get(), R.R_NamesSymbol, names.get());
    return result.get();
}

export fn zigr_test_altrep_persistence_check(value: SEXP) SEXP {
    if (R.ALTREP(value) == 0 or !std.mem.eql(u8, altrep_mod.className(value), "test_real") or
        R.XLENGTH(value) != 3 or R.REAL_ELT(value, 0) != 1.5 or R.ISNA(R.REAL_ELT(value, 1)) == 0 or
        !std.math.isNegativeInf(1.0 / R.REAL_ELT(value, 2)))
    {
        return R.Rf_ScalarReal(0.0);
    }
    const names = R.Rf_getAttrib(value, R.R_NamesSymbol);
    if (R.TYPEOF(names) != R.STRSXP or R.XLENGTH(names) != 3 or
        !std.mem.eql(u8, std.mem.sliceTo(R.R_CHAR(R.STRING_ELT(names, 2)), 0), "negative_zero"))
    {
        return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altrep_serialization_contract() SEXP {
    const empty_values = [_]f64{};
    var empty = protect.scoped(MyAlt.init(empty_values[0..]));
    defer empty.deinit();
    var restored_empty = protect.scoped(ownedAltrepRoundTrip(empty.get(), .v3));
    defer restored_empty.deinit();
    if (R.ALTREP(restored_empty.get()) == 0 or R.XLENGTH(restored_empty.get()) != 0 or
        R.REAL_OR_NULL(restored_empty.get()) != null)
    {
        return R.Rf_ScalarReal(0.0);
    }

    const real_values = [_]f64{ 1.5, -0.0, R.NA_REAL(), std.math.nan(f64) };
    var real = protect.scoped(MyAlt.init(real_values[0..]));
    defer real.deinit();
    var names = protect.scoped(R.Rf_allocVector(R.STRSXP, real_values.len));
    defer names.deinit();
    for ([_][]const u8{ "one", "negative_zero", "missing", "nan" }, 0..) |name, index| {
        R.SET_STRING_ELT(
            names.get(),
            @intCast(index),
            R.Rf_mkCharLenCE(@ptrCast(name.ptr), @intCast(name.len), @as(R.cetype_t, @intCast(R.CE_UTF8))),
        );
    }
    _ = R.Rf_setAttrib(real.get(), R.R_NamesSymbol, names.get());

    var restored_real = protect.scoped(ownedAltrepRoundTrip(real.get(), .v3));
    defer restored_real.deinit();
    if (R.ALTREP(restored_real.get()) == 0 or
        !std.mem.eql(u8, altrep_mod.className(restored_real.get()), "test_real") or
        R.REAL_ELT(restored_real.get(), 0) != 1.5 or
        !std.math.isNegativeInf(1.0 / R.REAL_ELT(restored_real.get(), 1)) or
        R.ISNA(R.REAL_ELT(restored_real.get(), 2)) == 0 or
        !R.ISNAN(R.REAL_ELT(restored_real.get(), 3)) or
        R.ISNA(R.REAL_ELT(restored_real.get(), 3)) != 0)
    {
        return R.Rf_ScalarReal(0.0);
    }
    const restored_names = R.Rf_getAttrib(restored_real.get(), R.R_NamesSymbol);
    if (R.TYPEOF(restored_names) != R.STRSXP or R.XLENGTH(restored_names) != real_values.len or
        !std.mem.eql(u8, std.mem.sliceTo(R.R_CHAR(R.STRING_ELT(restored_names, 1)), 0), "negative_zero"))
    {
        return R.Rf_ScalarReal(0.0);
    }

    var restored_v2 = protect.scoped(ownedAltrepRoundTrip(real.get(), .v2));
    defer restored_v2.deinit();
    if (R.ALTREP(restored_v2.get()) != 0 or R.TYPEOF(restored_v2.get()) != R.REALSXP or
        R.REAL(restored_v2.get())[0] != 1.5 or
        R.Rf_getAttrib(restored_v2.get(), R.R_NamesSymbol) == R.R_NilValue)
    {
        return R.Rf_ScalarReal(0.0);
    }

    const int_values = [_]i32{ 7, R.R_NaInt, -9 };
    var integer = protect.scoped(MyAltInt.init(int_values[0..]));
    defer integer.deinit();
    var restored_integer = protect.scoped(ownedAltrepRoundTrip(integer.get(), .v3));
    defer restored_integer.deinit();
    if (R.ALTREP(restored_integer.get()) == 0 or R.INTEGER_ELT(restored_integer.get(), 0) != 7 or
        R.INTEGER_ELT(restored_integer.get(), 1) != R.R_NaInt or R.INTEGER_ELT(restored_integer.get(), 2) != -9)
    {
        return R.Rf_ScalarReal(0.0);
    }

    const logical_values = [_]i32{ 1, 0, R.R_NaInt };
    var logical = protect.scoped(MyAltLogical.init(logical_values[0..]));
    defer logical.deinit();
    var restored_logical = protect.scoped(ownedAltrepRoundTrip(logical.get(), .v3));
    defer restored_logical.deinit();
    if (R.ALTREP(restored_logical.get()) == 0 or R.LOGICAL_ELT(restored_logical.get(), 0) != 1 or
        R.LOGICAL_ELT(restored_logical.get(), 1) != 0 or R.LOGICAL_ELT(restored_logical.get(), 2) != R.R_NaInt)
    {
        return R.Rf_ScalarReal(0.0);
    }

    const raw_values = [_]u8{ 0, 127, 255 };
    var raw = protect.scoped(MyAltRaw.init(raw_values[0..]));
    defer raw.deinit();
    var restored_raw = protect.scoped(ownedAltrepRoundTrip(raw.get(), .v3));
    defer restored_raw.deinit();
    if (R.ALTREP(restored_raw.get()) == 0 or R.RAW_ELT(restored_raw.get(), 0) != 0 or
        R.RAW_ELT(restored_raw.get(), 1) != 127 or R.RAW_ELT(restored_raw.get(), 2) != 255)
    {
        return R.Rf_ScalarReal(0.0);
    }

    const complex_values = [_]altrep_create.ComplexElem{
        .{ .r = 1.0, .i = -2.0 },
        .{ .r = R.NA_REAL(), .i = std.math.nan(f64) },
    };
    var complex = protect.scoped(MyAltComplex.init(complex_values[0..]));
    defer complex.deinit();
    var restored_complex = protect.scoped(ownedAltrepRoundTrip(complex.get(), .v3));
    defer restored_complex.deinit();
    var real_part: f64 = 0;
    var imaginary_part: f64 = 0;
    R.zigr_complex_elt_parts(restored_complex.get(), 1, &real_part, &imaginary_part);
    if (R.ALTREP(restored_complex.get()) == 0 or R.ISNA(real_part) == 0 or
        !R.ISNAN(imaginary_part) or R.ISNA(imaginary_part) != 0)
    {
        return R.Rf_ScalarReal(0.0);
    }

    const string_values = [_][]const u8{ "alpha", "placeholder", "bytes" };
    var string = protect.scoped(MyAltString.init(string_values[0..]));
    defer string.deinit();
    const latin1 = [_]u8{ 0x63, 0x61, 0x66, 0xe9 };
    R.SET_STRING_ELT(R.R_altrep_data1(string.get()), 1, R.R_NaString);
    R.SET_STRING_ELT(
        R.R_altrep_data1(string.get()),
        2,
        R.Rf_mkCharLenCE(@ptrCast(&latin1), latin1.len, @as(R.cetype_t, @intCast(R.CE_LATIN1))),
    );
    var restored_string = protect.scoped(ownedAltrepRoundTrip(string.get(), .v3));
    defer restored_string.deinit();
    if (R.ALTREP(restored_string.get()) == 0 or
        !std.mem.eql(u8, std.mem.sliceTo(R.R_CHAR(R.STRING_ELT(restored_string.get(), 0)), 0), "alpha") or
        R.STRING_ELT(restored_string.get(), 1) != R.R_NaString or
        R.Rf_getCharCE(R.STRING_ELT(restored_string.get(), 2)) != @as(R.cetype_t, @intCast(R.CE_LATIN1)))
    {
        return R.Rf_ScalarReal(0.0);
    }

    R.R_gc();
    if (R.REAL_ELT(restored_real.get(), 0) != 1.5 or R.INTEGER_ELT(restored_integer.get(), 2) != -9 or
        R.LOGICAL_ELT(restored_logical.get(), 2) != R.R_NaInt or R.RAW_ELT(restored_raw.get(), 2) != 255 or
        R.STRING_ELT(restored_string.get(), 1) != R.R_NaString)
    {
        return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

threadlocal var invalid_owned_altrep_state: SEXP = null;
threadlocal var invalid_owned_altrep_input: SEXP = null;

fn restoreInvalidOwnedAltrepState() SEXP {
    return MyAlt.restoreSerializedState(invalid_owned_altrep_state);
}

fn serializeInvalidOwnedAltrepInput() SEXP {
    return MyAlt.serializedState(invalid_owned_altrep_input);
}

export fn zigr_test_altrep_serialized_state_validation() SEXP {
    const values = [_]f64{ 1.0, 2.0, 3.0 };
    var original = protect.scoped(MyAlt.init(values[0..]));
    defer original.deinit();
    var state = protect.scoped(MyAlt.serializedStateChecked(original.get()) catch return R.Rf_ScalarReal(0.0));
    defer state.deinit();

    if (R.TYPEOF(state.get()) != R.VECSXP or R.XLENGTH(state.get()) != 4 or
        !std.mem.eql(
            u8,
            std.mem.sliceTo(R.R_CHAR(R.STRING_ELT(R.VECTOR_ELT(state.get(), 0), 0)), 0),
            "zigr-owned-altrep",
        ) or
        R.INTEGER_ELT(R.VECTOR_ELT(state.get(), 1), 0) != altrep_create.OWNED_ALTREP_STATE_VERSION or
        R.INTEGER_ELT(R.VECTOR_ELT(state.get(), 2), 0) != 1)
    {
        return R.Rf_ScalarReal(0.0);
    }
    const payload = R.VECTOR_ELT(state.get(), 3);
    if (R.TYPEOF(payload) != R.REALSXP or R.ALTREP(payload) != 0 or R.REAL(payload)[0] != 1.0) {
        return R.Rf_ScalarReal(0.0);
    }

    R.REAL(original.get())[0] = 90.0;
    if (R.REAL(payload)[0] != 1.0) return R.Rf_ScalarReal(0.0);
    var restored = protect.scoped(MyAlt.restoreSerializedStateChecked(state.get()) catch return R.Rf_ScalarReal(0.0));
    defer restored.deinit();
    R.REAL(payload)[0] = 70.0;
    if (R.REAL_ELT(restored.get(), 0) != 1.0) return R.Rf_ScalarReal(0.0);

    if (MyAlt.restoreSerializedStateChecked(null)) |_| return R.Rf_ScalarReal(0.0) else |e| {
        if (e != error.NullState) return R.Rf_ScalarReal(0.0);
    }
    if (MyAlt.serializedStateChecked(null)) |_| return R.Rf_ScalarReal(0.0) else |e| {
        if (e != error.WrongClass) return R.Rf_ScalarReal(0.0);
    }
    const not_record = R.Rf_ScalarInteger(1);
    if (MyAlt.serializedStateChecked(not_record)) |_| return R.Rf_ScalarReal(0.0) else |e| {
        if (e != error.WrongClass) return R.Rf_ScalarReal(0.0);
    }
    if (MyAlt.restoreSerializedStateChecked(not_record)) |_| return R.Rf_ScalarReal(0.0) else |e| {
        if (e != error.ExpectedRecord) return R.Rf_ScalarReal(0.0);
    }

    const foreign_values = [_]i32{1};
    var foreign = protect.scoped(MyAltInt.init(foreign_values[0..]));
    defer foreign.deinit();
    if (MyAlt.serializedStateChecked(foreign.get())) |_| return R.Rf_ScalarReal(0.0) else |e| {
        if (e != error.WrongClass) return R.Rf_ScalarReal(0.0);
    }
    if (MyAltString.serializedStateChecked(original.get())) |_| return R.Rf_ScalarReal(0.0) else |e| {
        if (e != error.WrongClass) return R.Rf_ScalarReal(0.0);
    }

    var short_state = protect.scoped(R.Rf_allocVector(R.VECSXP, 3));
    defer short_state.deinit();
    if (MyAlt.restoreSerializedStateChecked(short_state.get())) |_| return R.Rf_ScalarReal(0.0) else |e| {
        if (e != error.WrongFieldCount) return R.Rf_ScalarReal(0.0);
    }

    var bad_magic = protect.scoped(R.Rf_duplicate(state.get()));
    defer bad_magic.deinit();
    _ = R.SET_VECTOR_ELT(bad_magic.get(), 0, R.Rf_mkString("not-zigr"));
    if (MyAlt.restoreSerializedStateChecked(bad_magic.get())) |_| return R.Rf_ScalarReal(0.0) else |e| {
        if (e != error.InvalidMagic) return R.Rf_ScalarReal(0.0);
    }

    const magic_values = [_][]const u8{"zigr-owned-altrep"};
    var altrep_magic_value = protect.scoped(MyAltString.init(magic_values[0..]));
    defer altrep_magic_value.deinit();
    var altrep_magic = protect.scoped(R.Rf_duplicate(state.get()));
    defer altrep_magic.deinit();
    _ = R.SET_VECTOR_ELT(altrep_magic.get(), 0, altrep_magic_value.get());
    if (MyAlt.restoreSerializedStateChecked(altrep_magic.get())) |_| return R.Rf_ScalarReal(0.0) else |e| {
        if (e != error.InvalidMagic) return R.Rf_ScalarReal(0.0);
    }

    var bad_version = protect.scoped(R.Rf_duplicate(state.get()));
    defer bad_version.deinit();
    _ = R.SET_VECTOR_ELT(bad_version.get(), 1, R.Rf_ScalarInteger(2));
    if (MyAlt.restoreSerializedStateChecked(bad_version.get())) |_| return R.Rf_ScalarReal(0.0) else |e| {
        if (e != error.InvalidVersion) return R.Rf_ScalarReal(0.0);
    }

    var altrep_version = protect.scoped(R.Rf_duplicate(state.get()));
    defer altrep_version.deinit();
    _ = R.SET_VECTOR_ELT(altrep_version.get(), 1, foreign.get());
    if (MyAlt.restoreSerializedStateChecked(altrep_version.get())) |_| return R.Rf_ScalarReal(0.0) else |e| {
        if (e != error.InvalidVersion) return R.Rf_ScalarReal(0.0);
    }

    var altrep_kind = protect.scoped(R.Rf_duplicate(state.get()));
    defer altrep_kind.deinit();
    _ = R.SET_VECTOR_ELT(altrep_kind.get(), 2, foreign.get());
    if (MyAlt.restoreSerializedStateChecked(altrep_kind.get())) |_| return R.Rf_ScalarReal(0.0) else |e| {
        if (e != error.WrongKind) return R.Rf_ScalarReal(0.0);
    }

    if (MyAltInt.restoreSerializedStateChecked(state.get())) |_| return R.Rf_ScalarReal(0.0) else |e| {
        if (e != error.WrongKind) return R.Rf_ScalarReal(0.0);
    }

    var wrong_payload = protect.scoped(R.Rf_duplicate(state.get()));
    defer wrong_payload.deinit();
    _ = R.SET_VECTOR_ELT(wrong_payload.get(), 3, R.Rf_ScalarInteger(1));
    if (MyAlt.restoreSerializedStateChecked(wrong_payload.get())) |_| return R.Rf_ScalarReal(0.0) else |e| {
        if (e != error.WrongPayloadType) return R.Rf_ScalarReal(0.0);
    }

    var nested_payload = protect.scoped(R.Rf_duplicate(state.get()));
    defer nested_payload.deinit();
    _ = R.SET_VECTOR_ELT(nested_payload.get(), 3, original.get());
    if (MyAlt.restoreSerializedStateChecked(nested_payload.get())) |_| return R.Rf_ScalarReal(0.0) else |e| {
        if (e != error.NestedAltrepPayload) return R.Rf_ScalarReal(0.0);
    }

    const logical_values = [_]i32{1};
    var logical = protect.scoped(MyAltLogical.init(logical_values[0..]));
    defer logical.deinit();
    var invalid_logical = protect.scoped(MyAltLogical.serializedState(logical.get()));
    defer invalid_logical.deinit();
    R.LOGICAL(R.VECTOR_ELT(invalid_logical.get(), 3))[0] = 2;
    if (MyAltLogical.restoreSerializedStateChecked(invalid_logical.get())) |_| return R.Rf_ScalarReal(0.0) else |e| {
        if (e != error.InvalidLogicalValue) return R.Rf_ScalarReal(0.0);
    }

    const strings = [_][]const u8{ "alpha", "beta" };
    var string = protect.scoped(MyAltString.init(strings[0..]));
    defer string.deinit();
    var string_state = protect.scoped(MyAltString.serializedState(string.get()));
    defer string_state.deinit();
    var restored_string = protect.scoped(MyAltString.restoreSerializedStateChecked(string_state.get()) catch return R.Rf_ScalarReal(0.0));
    defer restored_string.deinit();
    R.SET_STRING_ELT(R.VECTOR_ELT(string_state.get(), 3), 0, R.Rf_mkCharCE("changed", @as(R.cetype_t, @intCast(R.CE_UTF8))));
    if (!std.mem.eql(u8, std.mem.sliceTo(R.R_CHAR(R.STRING_ELT(restored_string.get(), 0)), 0), "alpha")) {
        return R.Rf_ScalarReal(0.0);
    }

    const depth_before = protect.getDepth();
    invalid_owned_altrep_state = bad_version.get();
    defer invalid_owned_altrep_state = null;
    if (trycatch_mod.tryCatch(restoreInvalidOwnedAltrepState)) |_| return R.Rf_ScalarReal(0.0) else |_| {}
    if (protect.getDepth() != depth_before) return R.Rf_ScalarReal(0.0);
    invalid_owned_altrep_input = foreign.get();
    defer invalid_owned_altrep_input = null;
    if (trycatch_mod.tryCatch(serializeInvalidOwnedAltrepInput)) |_| return R.Rf_ScalarReal(0.0) else |_| {}
    if (protect.getDepth() != depth_before) return R.Rf_ScalarReal(0.0);
    var recovered = protect.scoped(MyAlt.restoreSerializedState(state.get()));
    defer recovered.deinit();
    if (R.REAL_ELT(recovered.get(), 1) != 2.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

fn evalSummary(name: []const u8, value: SEXP, na_rm: bool) SEXP {
    var flag = protect.scoped(R.Rf_ScalarLogical(if (na_rm) 1 else 0));
    defer flag.deinit();
    var call = protect.scoped(R.Rf_lang3(test_eval.findFunction(name), value, flag.get()));
    defer call.deinit();
    R.SET_TAG(R.CDR(R.CDR(call.get())), zigr.symbols.install("na.rm"));
    return test_eval.rEval(call.get(), R.R_GlobalEnv);
}

export fn zigr_test_altrep_summary_contract() SEXP {
    var real_data = [_]f64{ 4.0, 1.0, 3.0 };
    var real = protect.scoped(MyAlt.init(real_data[0..]));
    defer real.deinit();
    if (R.REAL(evalSummary("sum", real.get(), false))[0] != 8.0) return R.Rf_ScalarReal(0.0);
    if (R.REAL(evalSummary("min", real.get(), false))[0] != 1.0) return R.Rf_ScalarReal(0.0);
    if (R.REAL(evalSummary("max", real.get(), false))[0] != 4.0) return R.Rf_ScalarReal(0.0);
    if (R.REAL_IS_SORTED(real.get()) != R.KNOWN_UNSORTED) return R.Rf_ScalarReal(0.0);

    const descending_data = [_]f64{ 3.0, 2.0, 1.0 };
    var descending = protect.scoped(MyAlt.init(descending_data[0..]));
    defer descending.deinit();
    if (R.REAL_IS_SORTED(descending.get()) != R.SORTED_DECR) return R.Rf_ScalarReal(0.0);

    real_data = .{ R.NA_REAL(), std.math.nan(f64), 3.0 };
    var missing_real = protect.scoped(MyAlt.init(real_data[0..]));
    defer missing_real.deinit();
    if (R.ISNA(R.REAL(evalSummary("sum", missing_real.get(), false))[0]) == 0) return R.Rf_ScalarReal(0.0);
    if (R.REAL(evalSummary("sum", missing_real.get(), true))[0] != 3.0) return R.Rf_ScalarReal(0.0);
    if (R.ISNA(R.REAL(evalSummary("min", missing_real.get(), false))[0]) == 0) return R.Rf_ScalarReal(0.0);
    if (R.REAL(evalSummary("min", missing_real.get(), true))[0] != 3.0) return R.Rf_ScalarReal(0.0);
    if (R.REAL(evalSummary("max", missing_real.get(), true))[0] != 3.0) return R.Rf_ScalarReal(0.0);
    if (R.REAL_IS_SORTED(missing_real.get()) != R.UNKNOWN_SORTEDNESS) return R.Rf_ScalarReal(0.0);

    var int_data = [_]i32{ 4, R.R_NaInt, 3 };
    var missing_int = protect.scoped(MyAltInt.init(int_data[0..]));
    defer missing_int.deinit();
    if (R.INTEGER(evalSummary("sum", missing_int.get(), false))[0] != R.R_NaInt) return R.Rf_ScalarReal(0.0);
    if (R.INTEGER(evalSummary("sum", missing_int.get(), true))[0] != 7) return R.Rf_ScalarReal(0.0);
    if (R.INTEGER(evalSummary("max", missing_int.get(), true))[0] != 4) return R.Rf_ScalarReal(0.0);
    if (R.INTEGER_IS_SORTED(missing_int.get()) != R.UNKNOWN_SORTEDNESS) return R.Rf_ScalarReal(0.0);

    const no_reals = [_]f64{};
    var empty_real = protect.scoped(MyAlt.init(no_reals[0..]));
    defer empty_real.deinit();
    if (R.REAL(evalSummary("sum", empty_real.get(), false))[0] != 0.0) return R.Rf_ScalarReal(0.0);
    if (R.REAL(evalSummary("min", empty_real.get(), false))[0] != std.math.inf(f64)) return R.Rf_ScalarReal(0.0);
    if (R.REAL(evalSummary("max", empty_real.get(), false))[0] != -std.math.inf(f64)) return R.Rf_ScalarReal(0.0);

    const no_ints = [_]i32{};
    var empty_int = protect.scoped(MyAltInt.init(no_ints[0..]));
    defer empty_int.deinit();
    if (R.INTEGER(evalSummary("sum", empty_int.get(), false))[0] != 0) return R.Rf_ScalarReal(0.0);
    if (R.REAL(evalSummary("min", empty_int.get(), false))[0] != std.math.inf(f64)) return R.Rf_ScalarReal(0.0);
    if (R.REAL(evalSummary("max", empty_int.get(), false))[0] != -std.math.inf(f64)) return R.Rf_ScalarReal(0.0);

    const large_ints = [_]i32{ std.math.maxInt(i32), 1 };
    var large_int = protect.scoped(MyAltInt.init(large_ints[0..]));
    defer large_int.deinit();
    const large_sum = evalSummary("sum", large_int.get(), false);
    if (R.TYPEOF(large_sum) != R.REALSXP or R.REAL(large_sum)[0] != 2147483648.0) return R.Rf_ScalarReal(0.0);

    const only_na = [_]i32{R.R_NaInt};
    var empty_after_rm = protect.scoped(MyAltInt.init(only_na[0..]));
    defer empty_after_rm.deinit();
    const empty_min = evalSummary("min", empty_after_rm.get(), true);
    if (R.TYPEOF(empty_min) != R.REALSXP or R.REAL(empty_min)[0] != std.math.inf(f64)) return R.Rf_ScalarReal(0.0);

    var real_lanes = [_]f64{ 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0 };
    real_lanes[3] = R.NA_REAL();
    var real_simd = protect.scoped(MyAlt.init(real_lanes[0..]));
    defer real_simd.deinit();
    if (R.ISNA(R.REAL(evalSummary("max", real_simd.get(), false))[0]) == 0) return R.Rf_ScalarReal(0.0);
    if (R.REAL(evalSummary("min", real_simd.get(), true))[0] != 1.0) return R.Rf_ScalarReal(0.0);

    const int_lanes = [_]i32{ 8, 7, 6, R.R_NaInt, 4, 3, 2, 1 };
    var int_simd = protect.scoped(MyAltInt.init(int_lanes[0..]));
    defer int_simd.deinit();
    if (R.INTEGER(evalSummary("min", int_simd.get(), false))[0] != R.R_NaInt) return R.Rf_ScalarReal(0.0);
    if (R.INTEGER(evalSummary("max", int_simd.get(), true))[0] != 8) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altrep_empty_callback_contract() SEXP {
    const empty_values = [_]f64{};
    var empty = protect.scoped(MyAlt.init(empty_values[0..]));
    defer empty.deinit();
    var region_buffer: [4]f64 = undefined;
    if (R.REAL_OR_NULL(empty.get()) != null or R.REAL_GET_REGION(empty.get(), 0, R.R_XLEN_T_MAX, &region_buffer) != 0) return R.Rf_ScalarReal(0.0);
    if (R.REAL_IS_SORTED(empty.get()) != R.SORTED_INCR or R.REAL_NO_NA(empty.get()) != 1) return R.Rf_ScalarReal(0.0);
    var empty_duplicate = protect.scoped(R.Rf_duplicate(empty.get()));
    defer empty_duplicate.deinit();
    if (R.ALTREP(empty_duplicate.get()) != 0 or R.XLENGTH(empty_duplicate.get()) != 0) return R.Rf_ScalarReal(0.0);

    const empty_complex_values = [_]altrep_create.ComplexElem{};
    var empty_complex = protect.scoped(MyAltComplex.init(empty_complex_values[0..]));
    defer empty_complex.deinit();
    var complex_buffer: [1]zigr_convert.Rcomplex = undefined;
    if (R.COMPLEX_OR_NULL(empty_complex.get()) != null or R.COMPLEX_GET_REGION(empty_complex.get(), 0, R.R_XLEN_T_MAX, @ptrCast(&complex_buffer)) != 0) return R.Rf_ScalarReal(0.0);
    var empty_complex_duplicate = protect.scoped(R.Rf_duplicate(empty_complex.get()));
    defer empty_complex_duplicate.deinit();
    if (R.ALTREP(empty_complex_duplicate.get()) != 0 or R.XLENGTH(empty_complex_duplicate.get()) != 0) return R.Rf_ScalarReal(0.0);

    const empty_string_values = [_][]const u8{};
    var empty_string = protect.scoped(MyAltString.init(empty_string_values[0..]));
    defer empty_string.deinit();
    if (R.DATAPTR_OR_NULL(empty_string.get()) != null or R.STRING_IS_SORTED(empty_string.get()) != R.SORTED_INCR or R.STRING_NO_NA(empty_string.get()) != 1) return R.Rf_ScalarReal(0.0);
    var empty_string_duplicate = protect.scoped(R.Rf_duplicate(empty_string.get()));
    defer empty_string_duplicate.deinit();
    if (R.ALTREP(empty_string_duplicate.get()) != 0 or R.XLENGTH(empty_string_duplicate.get()) != 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altrep_real_callback_contract() SEXP {
    var real_values = [_]f64{ 1.0, 2.0, 3.0, 4.0 };
    var real = protect.scoped(MyAlt.init(real_values[0..]));
    defer real.deinit();
    var region_buffer: [4]f64 = undefined;
    if (R.REAL_ELT(real.get(), 3) != 4.0) return R.Rf_ScalarReal(0.0);
    if (R.REAL_GET_REGION(real.get(), 2, R.R_XLEN_T_MAX, &region_buffer) != 2 or region_buffer[0] != 3.0 or region_buffer[1] != 4.0) return R.Rf_ScalarReal(0.0);
    real_values = .{ std.math.nan(f64), R.NA_REAL(), 3.0, 4.0 };
    var missing_real = protect.scoped(MyAlt.init(real_values[0..]));
    defer missing_real.deinit();
    if (R.REAL_NO_NA(missing_real.get()) != 0 or R.REAL_IS_SORTED(missing_real.get()) != R.UNKNOWN_SORTEDNESS) return R.Rf_ScalarReal(0.0);
    for ([_][]const u8{ "sum", "min", "max" }) |name| {
        const result = evalSummary(name, missing_real.get(), false);
        if (R.ISNA(R.REAL(result)[0]) == 0) return R.Rf_ScalarReal(0.0);
    }

    real_values = .{ std.math.nan(f64), 2.0, 3.0, 4.0 };
    var nan_real = protect.scoped(MyAlt.init(real_values[0..]));
    defer nan_real.deinit();
    if (R.REAL_NO_NA(nan_real.get()) != 0 or R.REAL_IS_SORTED(nan_real.get()) != R.UNKNOWN_SORTEDNESS) return R.Rf_ScalarReal(0.0);
    for ([_][]const u8{ "sum", "min", "max" }) |name| {
        const result = evalSummary(name, nan_real.get(), false);
        if (!R.ISNAN(R.REAL(result)[0]) or R.ISNA(R.REAL(result)[0]) != 0) return R.Rf_ScalarReal(0.0);
    }
    if (R.REAL(evalSummary("sum", nan_real.get(), true))[0] != 9.0) return R.Rf_ScalarReal(0.0);
    if (R.REAL(evalSummary("min", nan_real.get(), true))[0] != 2.0) return R.Rf_ScalarReal(0.0);
    if (R.REAL(evalSummary("max", nan_real.get(), true))[0] != 4.0) return R.Rf_ScalarReal(0.0);

    const cancellation_values = [_]f64{ 1e16, 1.0, -1e16 };
    var cancellation = protect.scoped(MyAlt.init(cancellation_values[0..]));
    defer cancellation.deinit();
    if (R.REAL(evalSummary("sum", cancellation.get(), false))[0] != 1.0) return R.Rf_ScalarReal(0.0);

    const signed_zero_values = [_]f64{ -0.0, 0.0 };
    var signed_zero = protect.scoped(MyAlt.init(signed_zero_values[0..]));
    defer signed_zero.deinit();
    if (!std.math.isNegativeInf(1.0 / R.REAL(evalSummary("min", signed_zero.get(), false))[0])) return R.Rf_ScalarReal(0.0);
    if (!std.math.isNegativeInf(1.0 / R.REAL(evalSummary("max", signed_zero.get(), false))[0])) return R.Rf_ScalarReal(0.0);

    const positive_zero_values = [_]f64{ 0.0, -0.0 };
    var positive_zero = protect.scoped(MyAlt.init(positive_zero_values[0..]));
    defer positive_zero.deinit();
    if (!std.math.isPositiveInf(1.0 / R.REAL(evalSummary("min", positive_zero.get(), false))[0])) return R.Rf_ScalarReal(0.0);
    if (!std.math.isPositiveInf(1.0 / R.REAL(evalSummary("max", positive_zero.get(), false))[0])) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altrep_typed_callback_contract() SEXP {
    const int_values = [_]i32{ 7, R.R_NaInt, 9 };
    var integer = protect.scoped(MyAltInt.init(int_values[0..]));
    defer integer.deinit();
    var int_buffer: [3]c_int = undefined;
    if (R.INTEGER_ELT(integer.get(), 2) != 9 or R.INTEGER_GET_REGION(integer.get(), 0, 3, &int_buffer) != 3) return R.Rf_ScalarReal(0.0);
    if (int_buffer[1] != R.R_NaInt or R.INTEGER_NO_NA(integer.get()) != 0 or R.INTEGER_IS_SORTED(integer.get()) != R.UNKNOWN_SORTEDNESS) return R.Rf_ScalarReal(0.0);

    const logical_values = [_]i32{ 0, 1, R.R_NaInt };
    var logical = protect.scoped(MyAltLogical.init(logical_values[0..]));
    defer logical.deinit();
    if (R.LOGICAL_ELT(logical.get(), 1) != 1 or R.LOGICAL_NO_NA(logical.get()) != 0) return R.Rf_ScalarReal(0.0);

    const raw_values = [_]u8{ 0, 127, 255 };
    var raw = protect.scoped(MyAltRaw.init(raw_values[0..]));
    defer raw.deinit();
    var raw_buffer: [3]R.Rbyte = undefined;
    if (R.RAW_ELT(raw.get(), 2) != 255 or R.RAW_GET_REGION(raw.get(), 1, 2, &raw_buffer) != 2 or raw_buffer[0] != 127 or raw_buffer[1] != 255) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altrep_complex_string_callback_contract() SEXP {
    const complex_values = [_]altrep_create.ComplexElem{
        .{ .r = 1.0, .i = -2.0 },
        .{ .r = R.NA_REAL(), .i = std.math.nan(f64) },
    };
    var complex = protect.scoped(MyAltComplex.init(complex_values[0..]));
    defer complex.deinit();
    var real_part: f64 = 0;
    var imaginary_part: f64 = 0;
    R.zigr_complex_elt_parts(complex.get(), 1, &real_part, &imaginary_part);
    if (R.ISNA(real_part) == 0 or !R.ISNAN(imaginary_part) or R.ISNA(imaginary_part) != 0) return R.Rf_ScalarReal(0.0);
    var complex_duplicate = protect.scoped(R.Rf_duplicate(complex.get()));
    defer complex_duplicate.deinit();
    const duplicate_ptr: [*]const zigr_convert.Rcomplex = @ptrCast(@alignCast(R.COMPLEX(complex_duplicate.get()) orelse return R.Rf_ScalarReal(0.0)));
    if (R.ALTREP(complex_duplicate.get()) != 0 or R.ISNA(duplicate_ptr[1].r) == 0 or !R.ISNAN(duplicate_ptr[1].i)) return R.Rf_ScalarReal(0.0);

    const string_values = [_][]const u8{ "alpha", "beta" };
    var string = protect.scoped(MyAltString.init(string_values[0..]));
    defer string.deinit();
    R.SET_STRING_ELT(R.R_altrep_data1(string.get()), 1, R.R_NaString);
    if (R.STRING_ELT(string.get(), 1) != R.R_NaString or R.STRING_NO_NA(string.get()) != 0 or R.STRING_IS_SORTED(string.get()) != R.UNKNOWN_SORTEDNESS) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

const StringCleanupState = struct {
    allocations: usize = 0,
    frees: usize = 0,
};

threadlocal var string_cleanup_state: StringCleanupState = .{};
threadlocal var string_cleanup_input: SEXP = null;
threadlocal var string_cleanup_mode: u2 = 0;
threadlocal var string_cleanup_elt_calls: usize = 0;

var string_error_class: R.R_altrep_class_t = undefined;
var string_error_registered = false;

fn stringErrorLength(_: SEXP) callconv(.c) R.R_xlen_t {
    return 1;
}

fn stringErrorElt(_: SEXP, _: R.R_xlen_t) callconv(.c) SEXP {
    string_cleanup_elt_calls += 1;
    R.Rf_error("zigr string cleanup: expected ALTSTRING error");
}

fn stringErrorAltString() SEXP {
    if (!string_error_registered) {
        string_error_class = R.R_make_altstring_class("string_cleanup_error", "zigr", null);
        R.R_set_altrep_Length_method(string_error_class, stringErrorLength);
        R.R_set_altstring_Elt_method(string_error_class, stringErrorElt);
        string_error_registered = true;
    }
    return R.R_new_altrep(string_error_class, R.R_NilValue, R.R_NilValue);
}

threadlocal var factor_error_input: SEXP = null;

fn factorErrorCall() SEXP {
    return factor.asFactorChecked(factor_error_input) catch R.R_NilValue;
}

export fn zigr_test_factor_longjmp() SEXP {
    const initial_depth = protect.getDepth();
    var input = protect.scoped(stringErrorAltString());
    defer input.deinit();
    factor_error_input = input.get();
    defer factor_error_input = null;

    if (trycatch_mod.tryCatch(factorErrorCall)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    if (protect.getDepth() != initial_depth + 1) return R.Rf_ScalarReal(0.0);

    var fresh_input = protect.scoped(R.Rf_allocVector(R.STRSXP, 1));
    defer fresh_input.deinit();
    R.SET_STRING_ELT(fresh_input.get(), 0, R.Rf_mkChar("fresh"));
    var fresh = protect.scoped(factor.asFactorChecked(fresh_input.get()) catch return R.Rf_ScalarReal(0.0));
    defer fresh.deinit();
    if (R.INTEGER(fresh.get())[0] != 1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

const StringCleanupAllocator = struct {
    fn alloc(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, return_address: usize) ?[*]u8 {
        const state: *StringCleanupState = @ptrCast(@alignCast(ctx));
        const memory = std.heap.c_allocator.rawAlloc(len, alignment, return_address) orelse return null;
        state.allocations += 1;
        return memory;
    }

    fn free(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, return_address: usize) void {
        const state: *StringCleanupState = @ptrCast(@alignCast(ctx));
        std.heap.c_allocator.rawFree(memory, alignment, return_address);
        state.frees += 1;
    }

    fn resize(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) bool {
        return false;
    }

    fn remap(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) ?[*]u8 {
        return null;
    }

    fn allocator() std.mem.Allocator {
        return .{
            .ptr = @ptrCast(&string_cleanup_state),
            .vtable = &.{
                .alloc = alloc,
                .free = free,
                .resize = resize,
                .remap = remap,
            },
        };
    }
};

fn stringAllocationThenError() SEXP {
    switch (string_cleanup_mode) {
        0 => {
            const headers = zigr_convert.toStringSlice(StringCleanupAllocator.allocator(), string_cleanup_input) catch return R.R_NilValue;
            _ = headers;
        },
        1 => {
            const headers = zigr_convert.toStringSliceNullable(StringCleanupAllocator.allocator(), string_cleanup_input) catch return R.R_NilValue;
            _ = headers;
        },
        2 => {
            const cache = zigr_convert.toCachedStringSliceView(StringCleanupAllocator.allocator(), string_cleanup_input) catch return R.R_NilValue;
            _ = cache;
        },
        else => unreachable,
    }
    R.Rf_error("zigr string cleanup: ALTSTRING Elt unexpectedly returned");
}

export fn zigr_test_string_allocation_longjmp() SEXP {
    const input = R.Rf_protect(stringErrorAltString());
    defer R.Rf_unprotect(1);
    string_cleanup_input = input;
    defer string_cleanup_input = null;

    for (0..3) |mode| {
        string_cleanup_state = .{};
        string_cleanup_mode = @intCast(mode);
        string_cleanup_elt_calls = 0;
        if (trycatch_mod.tryCatch(struct {
            fn call() SEXP {
                return cleanup.protectCall(stringAllocationThenError);
            }
        }.call)) |_| {
            return R.Rf_ScalarReal(0.0);
        } else |_| {}
        if (string_cleanup_elt_calls != 1 or string_cleanup_state.allocations != 1 or string_cleanup_state.frees != 1) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

threadlocal var conversion_cleanup_input: SEXP = null;
threadlocal var conversion_cleanup_mode: u3 = 0;
var conversion_error_classes: [6]R.R_altrep_class_t = undefined;
var conversion_error_registered = false;

fn conversionErrorReal(_: SEXP, _: R.R_xlen_t) callconv(.c) f64 {
    R.Rf_error("expected real conversion error");
}

fn conversionErrorInteger(_: SEXP, _: R.R_xlen_t) callconv(.c) c_int {
    R.Rf_error("expected integer conversion error");
}

fn conversionErrorLogical(_: SEXP, _: R.R_xlen_t) callconv(.c) c_int {
    R.Rf_error("expected logical conversion error");
}

fn conversionErrorRaw(_: SEXP, _: R.R_xlen_t) callconv(.c) R.Rbyte {
    R.Rf_error("expected raw conversion error");
}

fn conversionErrorComplex(_: SEXP, _: R.R_xlen_t, _: R.R_xlen_t, _: ?*R.Rcomplex) callconv(.c) R.R_xlen_t {
    R.Rf_error("expected complex conversion error");
}

fn conversionErrorList(_: SEXP, _: R.R_xlen_t) callconv(.c) SEXP {
    R.Rf_error("expected list conversion error");
}

fn conversionErrorInput(mode: usize) SEXP {
    if (!conversion_error_registered) {
        conversion_error_classes[0] = R.R_make_altreal_class("conversion_error_real", "zigr", null);
        conversion_error_classes[1] = R.R_make_altinteger_class("conversion_error_integer", "zigr", null);
        conversion_error_classes[2] = R.R_make_altlogical_class("conversion_error_logical", "zigr", null);
        conversion_error_classes[3] = R.R_make_altraw_class("conversion_error_raw", "zigr", null);
        conversion_error_classes[4] = R.R_make_altcomplex_class("conversion_error_complex", "zigr", null);
        conversion_error_classes[5] = R.R_make_altlist_class("conversion_error_list", "zigr", null);
        for (conversion_error_classes) |class| R.R_set_altrep_Length_method(class, stringErrorLength);
        R.R_set_altreal_Elt_method(conversion_error_classes[0], conversionErrorReal);
        R.R_set_altinteger_Elt_method(conversion_error_classes[1], conversionErrorInteger);
        R.R_set_altlogical_Elt_method(conversion_error_classes[2], conversionErrorLogical);
        R.R_set_altraw_Elt_method(conversion_error_classes[3], conversionErrorRaw);
        R.R_set_altcomplex_Get_region_method(conversion_error_classes[4], conversionErrorComplex);
        R.R_set_altlist_Elt_method(conversion_error_classes[5], conversionErrorList);
        conversion_error_registered = true;
    }
    return R.R_new_altrep(conversion_error_classes[mode], R.R_NilValue, R.R_NilValue);
}

fn conversionAllocationThenError() SEXP {
    const allocator = StringCleanupAllocator.allocator();
    switch (conversion_cleanup_mode) {
        0 => _ = zigr_convert.toRealSlice(allocator, conversion_cleanup_input) catch return R.R_NilValue,
        1 => _ = zigr_convert.toIntSlice(allocator, conversion_cleanup_input) catch return R.R_NilValue,
        2 => _ = zigr_convert.toLogicalSlice(allocator, conversion_cleanup_input) catch return R.R_NilValue,
        3 => _ = zigr_convert.toRawSlice(allocator, conversion_cleanup_input) catch return R.R_NilValue,
        4 => _ = zigr_convert.toComplexSlice(allocator, conversion_cleanup_input) catch return R.R_NilValue,
        5 => _ = zigr_convert.toListSlice(allocator, conversion_cleanup_input) catch return R.R_NilValue,
        else => unreachable,
    }
    R.Rf_error("conversion unexpectedly returned");
}

export fn zigr_test_conversion_allocation_longjmp() SEXP {
    for (0..conversion_error_classes.len) |mode| {
        const input = R.Rf_protect(conversionErrorInput(mode));
        conversion_cleanup_input = input;
        conversion_cleanup_mode = @intCast(mode);
        string_cleanup_state = .{};

        if (trycatch_mod.tryCatch(struct {
            fn call() SEXP {
                return cleanup.protectCall(conversionAllocationThenError);
            }
        }.call)) |_| {
            R.Rf_unprotect(1);
            return R.Rf_ScalarReal(0.0);
        } else |_| {}

        R.Rf_unprotect(1);
        conversion_cleanup_input = null;
        if (string_cleanup_state.allocations != 1 or string_cleanup_state.frees != 1) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

threadlocal var attribute_cleanup_object: SEXP = null;
threadlocal var attribute_cleanup_symbol: SEXP = null;

fn attributeReadThenError() SEXP {
    _ = attrib.getString(StringCleanupAllocator.allocator(), attribute_cleanup_object, attribute_cleanup_symbol) catch return R.R_NilValue;
    R.Rf_error("attribute read unexpectedly returned");
}

export fn zigr_test_attrib_allocation_longjmp() SEXP {
    const object = R.Rf_protect(R.Rf_ScalarReal(1.0));
    defer R.Rf_unprotect(1);
    const value = R.Rf_protect(stringErrorAltString());
    defer R.Rf_unprotect(1);
    const marker = zigr.symbols.install("zigr_service_altstring_attribute");
    attrib.setAttrib(object, marker, value);
    attribute_cleanup_object = object;
    attribute_cleanup_symbol = marker;
    string_cleanup_state = .{};
    string_cleanup_elt_calls = 0;

    if (trycatch_mod.tryCatch(struct {
        fn call() SEXP {
            return cleanup.protectCall(attributeReadThenError);
        }
    }.call)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}

    attribute_cleanup_object = null;
    attribute_cleanup_symbol = null;
    if (string_cleanup_elt_calls != 1 or string_cleanup_state.allocations != 1 or string_cleanup_state.frees != 1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

fn attributeCleanupCapacityCall() SEXP {
    for (0..cleanup.MAX_NESTING) |_| cleanup.pushFrame(ignoreCleanup, null);
    _ = attrib.getString(StringCleanupAllocator.allocator(), attribute_cleanup_object, attribute_cleanup_symbol) catch return R.R_NilValue;
    R.Rf_error("attribute cleanup capacity unexpectedly returned");
}

export fn zigr_test_attrib_cleanup_capacity() SEXP {
    var object = protect.scoped(R.Rf_allocVector(R.VECSXP, 0));
    defer object.deinit();
    var value = protect.scoped(R.Rf_allocVector(R.STRSXP, 1));
    defer value.deinit();
    R.SET_STRING_ELT(value.get(), 0, R.Rf_mkChar("value"));
    const marker = test_lang.symbol("zigr_attribute_capacity");
    attrib.setAttrib(object.get(), marker, value.get());
    attribute_cleanup_object = object.get();
    attribute_cleanup_symbol = marker;
    defer {
        attribute_cleanup_object = null;
        attribute_cleanup_symbol = null;
    }
    string_cleanup_state = .{};

    if (trycatch_mod.tryCatch(struct {
        fn call() SEXP {
            return cleanup.protectCall(attributeCleanupCapacityCall);
        }
    }.call)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    if (string_cleanup_state.allocations != 0 or string_cleanup_state.frees != 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

threadlocal var dataframe_cleanup_object: SEXP = null;

fn dataframeNamesThenError() SEXP {
    const wrapped = df.DataFrame.wrap(dataframe_cleanup_object) orelse return R.R_NilValue;
    _ = wrapped.columnNames(StringCleanupAllocator.allocator()) catch return R.R_NilValue;
    R.Rf_error("data-frame names unexpectedly returned");
}

export fn zigr_test_df_names_longjmp() SEXP {
    const values = [_]f64{1.0};
    var column = protect.scoped(@as(SEXP, @ptrCast(zigr_convert.fromRealSlice(values[0..]))));
    defer column.deinit();
    const names = [_][]const u8{"value"};
    const columns = [_]SEXP{column.get()};
    var frame = protect.scoped(df.buildChecked(names[0..], columns[0..]) catch return R.Rf_ScalarReal(0.0));
    defer frame.deinit();
    var error_names = protect.scoped(stringErrorAltString());
    defer error_names.deinit();
    _ = R.Rf_setAttrib(frame.get(), R.R_NamesSymbol, error_names.get());

    dataframe_cleanup_object = frame.get();
    string_cleanup_state = .{};
    string_cleanup_elt_calls = 0;
    if (trycatch_mod.tryCatch(struct {
        fn call() SEXP {
            return cleanup.protectCall(dataframeNamesThenError);
        }
    }.call)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}

    dataframe_cleanup_object = null;
    if (string_cleanup_elt_calls != 1 or string_cleanup_state.allocations != 1 or string_cleanup_state.frees != 1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

threadlocal var dataframe_capacity_mode: u1 = 0;

fn ignoreCleanup(_: ?*anyopaque) void {}

fn dataframeCleanupCapacityCall() SEXP {
    for (0..cleanup.MAX_NESTING) |_| cleanup.pushFrame(ignoreCleanup, null);
    const wrapped = df.DataFrame.wrap(dataframe_cleanup_object) orelse return R.R_NilValue;
    if (dataframe_capacity_mode == 0) {
        _ = wrapped.columnNames(StringCleanupAllocator.allocator()) catch return R.R_NilValue;
    } else {
        var map = wrapped.columnMap(StringCleanupAllocator.allocator()) catch return R.R_NilValue;
        map.deinit();
    }
    R.Rf_error("data-frame cleanup capacity unexpectedly returned");
}

export fn zigr_test_df_cleanup_capacity() SEXP {
    const values = [_]f64{1.0};
    var column = protect.scoped(@as(SEXP, @ptrCast(zigr_convert.fromRealSlice(values[0..]))));
    defer column.deinit();
    const names = [_][]const u8{"value"};
    const columns = [_]SEXP{column.get()};
    var frame = protect.scoped(df.buildChecked(names[0..], columns[0..]) catch return R.Rf_ScalarReal(0.0));
    defer frame.deinit();
    dataframe_cleanup_object = frame.get();
    defer dataframe_cleanup_object = null;

    for (0..2) |mode| {
        dataframe_capacity_mode = @intCast(mode);
        string_cleanup_state = .{};
        if (trycatch_mod.tryCatch(struct {
            fn call() SEXP {
                return cleanup.protectCall(dataframeCleanupCapacityCall);
            }
        }.call)) |_| {
            return R.Rf_ScalarReal(0.0);
        } else |_| {}
        if (string_cleanup_state.allocations != 0 or string_cleanup_state.frees != 0) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_lang_builder() SEXP {
    const args = [_]SEXP{
        R.Rf_ScalarInteger(1),
        R.Rf_ScalarInteger(2),
        R.Rf_ScalarInteger(3),
    };
    const result = test_eval.call("sum", args[0..]);
    const val = R.INTEGER(result)[0];
    if (val != 6) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_from_empty() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const empty: []const f64 = &.{};
    const result = zigr_convert.fromRealSlice(empty);
    const len = R.XLENGTH(@as(R.SEXP, @ptrCast(result)));
    if (len != 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_real_huge() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const n: R.R_xlen_t = 1000000;
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, n));
    defer R.Rf_unprotect(1);
    const ptr: [*]f64 = @ptrCast(R.REAL(vec));
    ptr[0] = std.math.nan(f64);
    ptr[n - 1] = std.math.inf(f64);
    ptr[n / 2] = -std.math.inf(f64);
    const slice = zigr_convert.toRealSlice(arena.allocator(), @as(SEXP, @ptrCast(vec))) catch return R.Rf_ScalarReal(0.0);
    if (slice.len != n) return R.Rf_ScalarReal(0.0);
    if (!std.math.isNan(slice[0])) return R.Rf_ScalarReal(0.0);
    if (slice[n - 1] != std.math.inf(f64)) return R.Rf_ScalarReal(0.0);
    if (slice[n / 2] != -std.math.inf(f64)) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_str_na() SEXP {
    const n: R.R_xlen_t = 3;
    const vec = R.Rf_protect(R.Rf_allocVector(R.STRSXP, n));
    defer R.Rf_unprotect(1);
    R.SET_STRING_ELT(vec, 0, R.Rf_mkChar("hello"));
    R.SET_STRING_ELT(vec, 1, R.R_NaString);
    R.SET_STRING_ELT(vec, 2, R.Rf_mkChar("world"));

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const slice = zigr_convert.toStringSlice(arena.allocator(), @as(SEXP, @ptrCast(vec))) catch return R.Rf_ScalarReal(0.0);
    if (slice.len != 3) return R.Rf_ScalarReal(0.0);
    if (!std.mem.eql(u8, slice[0], "hello")) return R.Rf_ScalarReal(0.0);
    if (slice[1].len != 0) return R.Rf_ScalarReal(0.0);
    if (!std.mem.eql(u8, slice[2], "world")) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_lgl_edge() SEXP {
    const n: R.R_xlen_t = 4;
    const vec = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, n));
    defer R.Rf_unprotect(1);
    const ptr = R.LOGICAL(vec);
    ptr[0] = 0;
    ptr[1] = 1;
    ptr[2] = 42;
    ptr[3] = -7;
    if (R.TYPEOF(vec) != R.LGLSXP) return R.Rf_ScalarReal(0.0);
    if (R.XLENGTH(vec) != n) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_list_null() SEXP {
    const n: R.R_xlen_t = 2;
    const vec = R.Rf_protect(R.Rf_allocVector(R.VECSXP, n));
    defer R.Rf_unprotect(1);
    _ = R.SET_VECTOR_ELT(vec, 0, R.Rf_ScalarReal(1.0));
    _ = R.SET_VECTOR_ELT(vec, 1, R.R_NilValue);
    if (R.TYPEOF(vec) != R.VECSXP) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_df_col_missing() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const vals = [_]f64{ 1.0, 2.0 };
    const col1 = zigr_convert.fromRealSlice(vals[0..]);
    const names = [_][]const u8{"x"};
    const cols = [_]R.SEXP{@as(R.SEXP, @ptrCast(col1))};
    const df_sexp = df.build(names[0..], cols[0..]);
    const df_wrap = df.DataFrame.wrap(df_sexp) orelse return R.Rf_ScalarReal(0.0);
    const missing = df_wrap.column("nonexistent");
    if (missing != null) return R.Rf_ScalarReal(0.0);
    const exists = df_wrap.column("x");
    if (exists == null) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_real_wrong_type() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 3));
    defer R.Rf_unprotect(1);
    _ = zigr_convert.toRealSlice(std.heap.page_allocator, @as(SEXP, @ptrCast(vec))) catch |convert_err| {
        return R.Rf_ScalarReal(if (convert_err == error.ExpectedReal) 1.0 else 0.0);
    };
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_raw_logical() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, 4));
    defer R.Rf_unprotect(1);
    const ptr = R.LOGICAL(vec);
    ptr[0] = 1;
    ptr[1] = 0;
    ptr[2] = R.R_NaInt;
    ptr[3] = 1;

    const slice = raw_mod.logical(vec);
    const ok = slice.len == 4 and slice[0] == 1 and slice[1] == 0 and slice[2] == R.R_NaInt and slice[3] == 1;
    return R.Rf_ScalarReal(if (ok) 1.0 else 0.0);
}

export fn zigr_test_raw_real() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 3));
    defer R.Rf_unprotect(1);
    const ptr = R.REAL(vec);
    ptr[0] = 1.5;
    ptr[1] = 2.5;
    ptr[2] = 3.5;

    const slice = raw_mod.real(vec);
    const ok = slice.len == 3 and slice[0] == 1.5 and slice[1] == 2.5 and slice[2] == 3.5;
    return R.Rf_ScalarReal(if (ok) 1.0 else 0.0);
}

export fn zigr_test_raw_int() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 3));
    defer R.Rf_unprotect(1);
    const ptr = R.INTEGER(vec);
    ptr[0] = 10;
    ptr[1] = 20;
    ptr[2] = -5;

    const slice = raw_mod.int(vec);
    const ok = slice.len == 3 and slice[0] == 10 and slice[1] == 20 and slice[2] == -5;
    return R.Rf_ScalarReal(if (ok) 1.0 else 0.0);
}

export fn zigr_test_raw_real_mut() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 2));
    defer R.Rf_unprotect(1);

    const slice = raw_mod.realMut(vec);
    slice[0] = 42.0;
    slice[1] = 99.0;

    const readback = R.REAL(vec);
    const ok = readback[0] == 42.0 and readback[1] == 99.0;
    return R.Rf_ScalarReal(if (ok) 1.0 else 0.0);
}

export fn zigr_test_raw_int_mut() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 2));
    defer R.Rf_unprotect(1);

    const slice = raw_mod.intMut(vec);
    slice[0] = 100;
    slice[1] = 200;

    const readback = R.INTEGER(vec);
    const ok = readback[0] == 100 and readback[1] == 200;
    return R.Rf_ScalarReal(if (ok) 1.0 else 0.0);
}

export fn zigr_test_raw_raw() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.RAWSXP, 3));
    defer R.Rf_unprotect(1);
    const ptr = R.RAW(vec);
    ptr[0] = 0xAB;
    ptr[1] = 0xCD;
    ptr[2] = 0xEF;

    const slice = raw_mod.raw(vec);
    const ok = slice.len == 3 and slice[0] == 0xAB and slice[1] == 0xCD and slice[2] == 0xEF;
    return R.Rf_ScalarReal(if (ok) 1.0 else 0.0);
}

export fn zigr_test_raw_complex() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.CPLXSXP, 2));
    defer R.Rf_unprotect(1);
    const wslice = raw_mod.complexMut(vec);
    wslice[0] = .{ .r = 1.0, .i = 2.0 };
    wslice[1] = .{ .r = 3.0, .i = 4.0 };

    const slice = raw_mod.complex(vec);
    const ok = slice.len == 2 and slice[0].r == 1.0 and slice[0].i == 2.0 and slice[1].r == 3.0 and slice[1].i == 4.0;
    return R.Rf_ScalarReal(if (ok) 1.0 else 0.0);
}

export fn zigr_test_raw_dims() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 12));
    defer R.Rf_unprotect(1);
    _ = R.Rf_setAttrib(vec, R.R_DimSymbol, R.R_NilValue);

    const d = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 2));
    R.INTEGER(d)[0] = 3;
    R.INTEGER(d)[1] = 4;
    _ = R.Rf_setAttrib(vec, R.R_DimSymbol, d);
    R.Rf_unprotect(1);

    const result = raw_mod.dims(vec);
    const ok = result.rows == 3 and result.cols == 4;
    return R.Rf_ScalarReal(if (ok) 1.0 else 0.0);
}

export fn zigr_test_from_sexp_missing_required() SEXP {
    const Test = struct { x: f64, y: f64 };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const vec = R.Rf_protect(R.Rf_allocVector(R.VECSXP, 1));
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    const xv = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    R.REAL(xv)[0] = 99.0;
    _ = R.SET_VECTOR_ELT(vec, 0, xv);
    R.SET_STRING_ELT(names, 0, R.Rf_mkChar("x"));
    _ = R.Rf_namesgets(vec, names);
    R.Rf_unprotect(3);

    _ = zigr_convert.fromSEXP(Test, vec, arena.allocator());
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_from_sexp_invalid_names() SEXP {
    const Test = struct { x: f64 };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const vec = R.Rf_protect(R.Rf_allocVector(R.VECSXP, 1));
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    const xv = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    R.REAL(xv)[0] = 1.0;
    _ = R.SET_VECTOR_ELT(vec, 0, xv);
    R.SET_STRING_ELT(names, 0, R.R_NaString);
    _ = R.Rf_namesgets(vec, names);
    R.Rf_unprotect(3);

    _ = zigr_convert.fromSEXP(Test, vec, arena.allocator());
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_from_sexp_missing_names() SEXP {
    const Test = struct { x: f64 };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const vec = R.Rf_protect(R.Rf_allocVector(R.VECSXP, 1));
    const xv = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    R.REAL(xv)[0] = 1.0;
    _ = R.SET_VECTOR_ELT(vec, 0, xv);
    R.Rf_unprotect(2);

    _ = zigr_convert.fromSEXP(Test, vec, arena.allocator());
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_from_sexp_name_length_mismatch() SEXP {
    const Test = struct { x: f64 };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const vec = R.Rf_protect(R.Rf_allocVector(R.VECSXP, 1));
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 2));
    const xv = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    R.REAL(xv)[0] = 1.0;
    _ = R.SET_VECTOR_ELT(vec, 0, xv);
    R.SET_STRING_ELT(names, 0, R.Rf_mkChar("x"));
    R.SET_STRING_ELT(names, 1, R.Rf_mkChar("extra"));
    _ = R.Rf_namesgets(vec, names);
    R.Rf_unprotect(3);

    _ = zigr_convert.fromSEXP(Test, vec, arena.allocator());
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_list_wrong_type() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    defer R.Rf_unprotect(1);
    _ = zigr_convert.toListSlice(std.heap.page_allocator, vec) catch |convert_err| {
        return R.Rf_ScalarReal(if (convert_err == error.ExpectedList) 1.0 else 0.0);
    };
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_real_scalar_na() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    defer R.Rf_unprotect(1);
    R.REAL(vec)[0] = R.NA_REAL();
    _ = zigr_convert.toRealScalar(vec) catch |convert_err| {
        return R.Rf_ScalarReal(if (convert_err == error.ScalarNA) 1.0 else 0.0);
    };
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_int_scalar_na() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 1));
    defer R.Rf_unprotect(1);
    R.INTEGER(vec)[0] = R.R_NaInt;
    _ = zigr_convert.toIntScalar(vec) catch |convert_err| {
        return R.Rf_ScalarReal(if (convert_err == error.ScalarNA) 1.0 else 0.0);
    };
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_bool_scalar_na() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, 1));
    defer R.Rf_unprotect(1);
    R.LOGICAL(vec)[0] = R.R_NaInt;
    _ = zigr_convert.toBoolScalar(vec) catch |convert_err| {
        return R.Rf_ScalarReal(if (convert_err == error.ScalarNA) 1.0 else 0.0);
    };
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_scalar_contract() SEXP {
    const real = R.Rf_protect(R.Rf_ScalarReal(3.5));
    const integer = R.Rf_protect(R.Rf_ScalarInteger(-7));
    const logical = R.Rf_protect(R.Rf_ScalarLogical(0));
    const nan = R.Rf_protect(R.Rf_ScalarReal(R.R_NaN));
    const empty = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 0));
    const multi = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 2));
    const missing = R.Rf_protect(R.Rf_ScalarReal(R.NA_REAL()));
    defer R.Rf_unprotect(7);
    R.REAL(multi)[0] = 1.0;
    R.REAL(multi)[1] = 2.0;

    const real_value = zigr_convert.toRealScalar(real) catch return R.Rf_ScalarReal(0.0);
    const int_value = zigr_convert.toIntScalar(integer) catch return R.Rf_ScalarReal(0.0);
    const bool_value = zigr_convert.toBoolScalar(logical) catch return R.Rf_ScalarReal(0.0);
    const nan_value = zigr_convert.toRealScalar(nan) catch return R.Rf_ScalarReal(0.0);

    if (real_value != 3.5 or int_value != -7 or bool_value) return R.Rf_ScalarReal(0.0);
    if (!std.math.isNan(nan_value) or R.ISNA(nan_value) != 0) return R.Rf_ScalarReal(0.0);
    if (zigr_convert.toRealScalar(integer) != error.ExpectedReal) return R.Rf_ScalarReal(0.0);
    if (zigr_convert.toRealScalar(empty) != error.ZeroLength) return R.Rf_ScalarReal(0.0);
    if (zigr_convert.toRealScalar(multi) != error.ScalarLength) return R.Rf_ScalarReal(0.0);
    if (zigr_convert.toRealScalar(missing) != error.ScalarNA) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_optional_scalar_contract() SEXP {
    const real_na = R.Rf_protect(R.Rf_ScalarReal(R.NA_REAL()));
    const real_nan = R.Rf_protect(R.Rf_ScalarReal(R.R_NaN));
    const int_na = R.Rf_protect(R.Rf_ScalarInteger(R.R_NaInt));
    const integer = R.Rf_protect(R.Rf_ScalarInteger(7));
    const logical_na = R.Rf_protect(R.Rf_ScalarLogical(R.R_NaInt));
    const logical = R.Rf_protect(R.Rf_ScalarLogical(0));
    const empty = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 0));
    const multi_real = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 2));
    const multi_int = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 2));
    const multi_logical = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, 2));
    defer R.Rf_unprotect(10);
    R.REAL(multi_real)[0] = R.NA_REAL();
    R.REAL(multi_real)[1] = 1.0;
    R.INTEGER(multi_int)[0] = R.R_NaInt;
    R.INTEGER(multi_int)[1] = 1;
    R.LOGICAL(multi_logical)[0] = R.R_NaInt;
    R.LOGICAL(multi_logical)[1] = 1;

    if ((zigr_convert.toOptionalRealScalar(R.R_NilValue) catch return R.Rf_ScalarReal(0.0)) != null) return R.Rf_ScalarReal(0.0);
    if ((zigr_convert.toOptionalIntScalar(R.R_NilValue) catch return R.Rf_ScalarReal(0.0)) != null) return R.Rf_ScalarReal(0.0);
    if ((zigr_convert.toOptionalBoolScalar(R.R_NilValue) catch return R.Rf_ScalarReal(0.0)) != null) return R.Rf_ScalarReal(0.0);
    if ((zigr_convert.toOptionalRealScalar(real_na) catch return R.Rf_ScalarReal(0.0)) != null) return R.Rf_ScalarReal(0.0);
    if ((zigr_convert.toOptionalIntScalar(int_na) catch return R.Rf_ScalarReal(0.0)) != null) return R.Rf_ScalarReal(0.0);
    if ((zigr_convert.toOptionalBoolScalar(logical_na) catch return R.Rf_ScalarReal(0.0)) != null) return R.Rf_ScalarReal(0.0);

    const nan_value = zigr_convert.toOptionalRealScalar(real_nan) catch return R.Rf_ScalarReal(0.0);
    const int_value = zigr_convert.toOptionalIntScalar(integer) catch return R.Rf_ScalarReal(0.0);
    const bool_value = zigr_convert.toOptionalBoolScalar(logical) catch return R.Rf_ScalarReal(0.0);

    if (nan_value == null or !std.math.isNan(nan_value.?) or R.ISNA(nan_value.?) != 0) return R.Rf_ScalarReal(0.0);
    if (int_value == null or int_value.? != 7) return R.Rf_ScalarReal(0.0);
    if (bool_value == null or bool_value.?) return R.Rf_ScalarReal(0.0);
    if (zigr_convert.toOptionalRealScalar(empty) != error.ZeroLength) return R.Rf_ScalarReal(0.0);
    if (zigr_convert.toOptionalRealScalar(multi_real) != error.ScalarLength) return R.Rf_ScalarReal(0.0);
    if (zigr_convert.toOptionalIntScalar(multi_int) != error.ScalarLength) return R.Rf_ScalarReal(0.0);
    if (zigr_convert.toOptionalBoolScalar(multi_logical) != error.ScalarLength) return R.Rf_ScalarReal(0.0);
    if (zigr_convert.toOptionalBoolScalar(integer) != error.ExpectedLogical) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_optional_real_na_to_null() SEXP {
    const Test = struct { x: ?f64 };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const vec = R.Rf_protect(R.Rf_allocVector(R.VECSXP, 1));
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    const xv = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    R.REAL(xv)[0] = R.NA_REAL();
    _ = R.SET_VECTOR_ELT(vec, 0, xv);
    R.SET_STRING_ELT(names, 0, R.Rf_mkChar("x"));
    _ = R.Rf_namesgets(vec, names);
    R.Rf_unprotect(3);

    const result = zigr_convert.fromSEXP(Test, vec, arena.allocator());
    return R.Rf_ScalarReal(if (result.x == null) 1.0 else 0.0);
}

export fn zigr_test_optional_int_na_to_null() SEXP {
    const Test = struct { x: ?i32 };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const vec = R.Rf_protect(R.Rf_allocVector(R.VECSXP, 1));
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    const xv = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 1));
    R.INTEGER(xv)[0] = R.R_NaInt;
    _ = R.SET_VECTOR_ELT(vec, 0, xv);
    R.SET_STRING_ELT(names, 0, R.Rf_mkChar("x"));
    _ = R.Rf_namesgets(vec, names);
    R.Rf_unprotect(3);

    const result = zigr_convert.fromSEXP(Test, vec, arena.allocator());
    return R.Rf_ScalarReal(if (result.x == null) 1.0 else 0.0);
}

export fn zigr_test_optional_bool_na_to_null() SEXP {
    const Test = struct { x: ?bool };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const vec = R.Rf_protect(R.Rf_allocVector(R.VECSXP, 1));
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    const xv = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, 1));
    R.LOGICAL(xv)[0] = R.R_NaInt;
    _ = R.SET_VECTOR_ELT(vec, 0, xv);
    R.SET_STRING_ELT(names, 0, R.Rf_mkChar("x"));
    _ = R.Rf_namesgets(vec, names);
    R.Rf_unprotect(3);

    const result = zigr_convert.fromSEXP(Test, vec, arena.allocator());
    return R.Rf_ScalarReal(if (result.x == null) 1.0 else 0.0);
}

export fn zigr_test_pmin_recycling() SEXP {
    const a = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 4));
    const b = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 2));
    defer R.Rf_unprotect(2);

    const ap = R.REAL(a);
    ap[0] = 5.0;
    ap[1] = 1.0;
    ap[2] = 7.0;
    ap[3] = -1.0;

    const bp = R.REAL(b);
    bp[0] = 3.0;
    bp[1] = 2.0;

    const result = zigr_convert.pmin(a, b);
    if (R.XLENGTH(result) != 4) return R.Rf_ScalarReal(0.0);
    const rp = R.REAL(result);
    const ok = rp[0] == 3.0 and rp[1] == 1.0 and rp[2] == 3.0 and rp[3] == -1.0;
    return R.Rf_ScalarReal(if (ok) 1.0 else 0.0);
}

export fn zigr_test_pmax_recycling() SEXP {
    const a = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 3));
    const b = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    defer R.Rf_unprotect(2);

    const ap = R.REAL(a);
    ap[0] = -4.0;
    ap[1] = 2.0;
    ap[2] = 9.0;

    R.REAL(b)[0] = 3.0;

    const result = zigr_convert.pmax(a, b);
    if (R.XLENGTH(result) != 3) return R.Rf_ScalarReal(0.0);
    const rp = R.REAL(result);
    const ok = rp[0] == 3.0 and rp[1] == 3.0 and rp[2] == 9.0;
    return R.Rf_ScalarReal(if (ok) 1.0 else 0.0);
}

export fn zigr_test_embed_sum() SEXP {
    const result = embed.rCodeEval("1 + 1", null);
    const val = R.REAL(result)[0];
    if (val == 2.0) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_embed_paste() SEXP {
    const result = embed.rCodeEval("paste('hello', 'world')", null);
    const elt = R.STRING_ELT(result, 0);
    if (elt == R.R_NaString) return R.Rf_ScalarReal(0.0);
    const s = std.mem.sliceTo(R.R_CHAR(elt), 0);
    if (std.mem.eql(u8, s, "hello world")) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

var serialized_raw_proxy_class: R.R_altrep_class_t = undefined;
var serialized_raw_proxy_registered = false;
var serialized_raw_proxy_region_calls: usize = 0;
var serialized_raw_proxy_elt_calls: usize = 0;

fn serializedRawProxyLength(x: SEXP) callconv(.c) R.R_xlen_t {
    return R.XLENGTH(R.R_altrep_data1(x));
}

fn serializedRawProxyElt(x: SEXP, index: R.R_xlen_t) callconv(.c) R.Rbyte {
    serialized_raw_proxy_elt_calls += 1;
    return R.RAW(R.R_altrep_data1(x))[@intCast(index)];
}

fn serializedRawProxyRegion(x: SEXP, start: R.R_xlen_t, requested: R.R_xlen_t, buffer: [*c]R.Rbyte) callconv(.c) R.R_xlen_t {
    if (buffer == null or start < 0 or requested <= 0) return 0;
    const source = R.R_altrep_data1(x);
    const len = R.XLENGTH(source);
    if (start >= len) return 0;
    serialized_raw_proxy_region_calls += 1;
    const count = @min(@min(requested, 7), len - start);
    @memcpy(
        @as([*]u8, @ptrCast(buffer))[0..@intCast(count)],
        R.RAW(source)[@intCast(start)..][0..@intCast(count)],
    );
    return count;
}

fn serializedRawProxy(source: SEXP) SEXP {
    if (!serialized_raw_proxy_registered) {
        serialized_raw_proxy_class = R.R_make_altraw_class("serialized_raw_proxy", "zigr", null);
        R.R_set_altrep_Length_method(serialized_raw_proxy_class, serializedRawProxyLength);
        R.R_set_altvec_Dataptr_or_null_method(serialized_raw_proxy_class, shortRegionDataptrOrNull);
        R.R_set_altraw_Elt_method(serialized_raw_proxy_class, serializedRawProxyElt);
        R.R_set_altraw_Get_region_method(serialized_raw_proxy_class, serializedRawProxyRegion);
        serialized_raw_proxy_registered = true;
    }
    return R.R_new_altrep(serialized_raw_proxy_class, source, R.R_NilValue);
}

export fn zigr_test_serialize_roundtrip_contract() SEXP {
    const original = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 3));
    defer R.Rf_unprotect(1);
    R.REAL(original)[0] = 1.25;
    R.REAL(original)[1] = R.NA_REAL();
    R.REAL(original)[2] = -0.0;

    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 3));
    defer R.Rf_unprotect(1);
    R.SET_STRING_ELT(names, 0, R.Rf_mkChar("first"));
    R.SET_STRING_ELT(names, 1, R.Rf_mkChar("missing"));
    R.SET_STRING_ELT(names, 2, R.Rf_mkChar("negative_zero"));
    _ = R.Rf_namesgets(original, names);

    const serialized = R.Rf_protect(serialize_mod.toVectorVersion(original, .v3));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(serialized) != R.RAWSXP or R.XLENGTH(serialized) < 6) return R.Rf_ScalarReal(0.0);
    const bytes = R.RAW(serialized);
    if (bytes[0] != 'X' or bytes[1] != '\n') return R.Rf_ScalarReal(0.0);
    if (bytes[2] != 0 or bytes[3] != 0 or bytes[4] != 0 or bytes[5] != 3) return R.Rf_ScalarReal(0.0);

    const serialized_v2 = R.Rf_protect(serialize_mod.toVectorVersion(original, .v2));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(serialized_v2) != R.RAWSXP or R.XLENGTH(serialized_v2) < 6) return R.Rf_ScalarReal(0.0);
    const bytes_v2 = R.RAW(serialized_v2);
    if (bytes_v2[0] != 'X' or bytes_v2[1] != '\n') return R.Rf_ScalarReal(0.0);
    if (bytes_v2[2] != 0 or bytes_v2[3] != 0 or bytes_v2[4] != 0 or bytes_v2[5] != 2) return R.Rf_ScalarReal(0.0);

    R.R_gc();
    const restored = R.Rf_protect(serialize_mod.fromVectorChecked(serialized) catch return R.Rf_ScalarReal(0.0));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(restored) != R.REALSXP or R.XLENGTH(restored) != 3) return R.Rf_ScalarReal(0.0);
    if (R.REAL(restored)[0] != 1.25 or R.ISNA(R.REAL(restored)[1]) == 0) return R.Rf_ScalarReal(0.0);
    if (R.REAL(restored)[2] != 0.0 or !std.math.signbit(R.REAL(restored)[2])) return R.Rf_ScalarReal(0.0);

    const restored_names = R.Rf_getAttrib(restored, R.R_NamesSymbol);
    if (R.TYPEOF(restored_names) != R.STRSXP or R.XLENGTH(restored_names) != 3) return R.Rf_ScalarReal(0.0);
    if (!std.mem.eql(u8, std.mem.sliceTo(R.R_CHAR(R.STRING_ELT(restored_names, 2)), 0), "negative_zero")) return R.Rf_ScalarReal(0.0);

    const restored_v2 = R.Rf_protect(serialize_mod.fromVector(serialized_v2));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(restored_v2) != R.REALSXP or R.XLENGTH(restored_v2) != 3) return R.Rf_ScalarReal(0.0);
    if (R.REAL(restored_v2)[0] != 1.25 or R.ISNA(R.REAL(restored_v2)[1]) == 0) return R.Rf_ScalarReal(0.0);
    if (R.REAL(restored_v2)[2] != 0.0 or !std.math.signbit(R.REAL(restored_v2)[2])) return R.Rf_ScalarReal(0.0);

    const r_unserialize_call = R.Rf_protect(R.Rf_lang2(R.Rf_install("unserialize"), serialized));
    defer R.Rf_unprotect(1);
    const r_restored = R.Rf_protect(R.Rf_eval(r_unserialize_call, R.R_BaseEnv));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(r_restored) != R.REALSXP or R.XLENGTH(r_restored) != 3) return R.Rf_ScalarReal(0.0);
    if (R.REAL(r_restored)[0] != 1.25 or R.ISNA(R.REAL(r_restored)[1]) == 0) return R.Rf_ScalarReal(0.0);

    const r_serialize_call = R.Rf_protect(R.Rf_lang3(R.Rf_install("serialize"), original, R.R_NilValue));
    defer R.Rf_unprotect(1);
    const r_serialized = R.Rf_protect(R.Rf_eval(r_serialize_call, R.R_BaseEnv));
    defer R.Rf_unprotect(1);
    const zigr_restored = R.Rf_protect(serialize_mod.fromVector(r_serialized));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(zigr_restored) != R.REALSXP or R.XLENGTH(zigr_restored) != 3) return R.Rf_ScalarReal(0.0);
    if (R.REAL(zigr_restored)[0] != 1.25 or R.ISNA(R.REAL(zigr_restored)[1]) == 0) return R.Rf_ScalarReal(0.0);

    var raw_proxy = protect.scoped(serializedRawProxy(serialized));
    defer raw_proxy.deinit();
    if (R.RAW_OR_NULL(raw_proxy.get()) != null) return R.Rf_ScalarReal(0.0);
    serialized_raw_proxy_region_calls = 0;
    serialized_raw_proxy_elt_calls = 0;
    var proxy_restored = protect.scoped(serialize_mod.fromVector(raw_proxy.get()));
    defer proxy_restored.deinit();
    if (serialized_raw_proxy_region_calls <= 1 or serialized_raw_proxy_elt_calls != 0) return R.Rf_ScalarReal(0.0);
    if (R.RAW_OR_NULL(raw_proxy.get()) != null) return R.Rf_ScalarReal(0.0);
    if (R.TYPEOF(proxy_restored.get()) != R.REALSXP or R.XLENGTH(proxy_restored.get()) != 3) return R.Rf_ScalarReal(0.0);
    if (R.REAL(proxy_restored.get())[0] != 1.25 or R.ISNA(R.REAL(proxy_restored.get())[1]) == 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_serialize_checked_input() SEXP {
    const integer = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 1));
    defer R.Rf_unprotect(1);
    if (serialize_mod.fromVectorChecked(integer)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |serialize_error| {
        if (serialize_error != error.ExpectedRaw) return R.Rf_ScalarReal(0.0);
    }
    if (serialize_mod.fromVectorChecked(null)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |serialize_error| {
        if (serialize_error != error.NullInput) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

threadlocal var malformed_serialized_input: SEXP = null;

fn unserializeMalformedInput() SEXP {
    return serialize_mod.fromVector(malformed_serialized_input);
}

fn serializeNullInput() SEXP {
    return serialize_mod.toVector(null);
}

export fn zigr_test_serialize_malformed_unwind() SEXP {
    const initial_depth = protect.getDepth();
    const malformed = R.Rf_protect(R.Rf_allocVector(R.RAWSXP, 6));
    defer R.Rf_unprotect(1);
    const bytes = R.RAW(malformed);
    bytes[0] = 'X';
    bytes[1] = '\n';
    bytes[2] = 0;
    bytes[3] = 0;
    bytes[4] = 0;
    bytes[5] = 3;
    malformed_serialized_input = malformed;
    defer malformed_serialized_input = null;

    if (trycatch_mod.tryCatch(unserializeMalformedInput)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    if (protect.getDepth() != initial_depth) return R.Rf_ScalarReal(0.0);
    if (trycatch_mod.tryCatch(serializeNullInput)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    if (protect.getDepth() != initial_depth) return R.Rf_ScalarReal(0.0);

    const original = R.Rf_protect(R.Rf_ScalarInteger(42));
    defer R.Rf_unprotect(1);
    const serialized = R.Rf_protect(serialize_mod.toVector(original));
    defer R.Rf_unprotect(1);
    const restored = R.Rf_protect(serialize_mod.fromVector(serialized));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(restored) != R.INTSXP or R.INTEGER(restored)[0] != 42) return R.Rf_ScalarReal(0.0);
    if (protect.getDepth() != initial_depth) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_raw_eval() SEXP {
    const result = embed.rRawEval("1 + 1", null);
    const val = R.REAL(result)[0];
    if (val == 2.0) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_struct_to_sexp() SEXP {
    const TestStruct = struct { x: f64, y: f64 };
    const s = TestStruct{ .x = 1.5, .y = 2.5 };
    const result = zigr_convert.asSEXP(s);
    if (R.TYPEOF(result) != R.VECSXP) return R.Rf_ScalarReal(0.0);
    if (R.XLENGTH(result) != 2) return R.Rf_ScalarReal(0.0);
    const ns = R.Rf_getAttrib(result, R.R_NamesSymbol);
    if (ns == R.R_NilValue) return R.Rf_ScalarReal(0.0);
    if (R.XLENGTH(ns) != 2) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_struct_from_sexp() SEXP {
    const TestStruct = struct { a: f64, b: []const f64 };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const vec = R.Rf_protect(R.Rf_allocVector(R.VECSXP, 2));
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 2));
    const av = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    R.REAL(av)[0] = 42.0;
    _ = R.SET_VECTOR_ELT(vec, 0, av);
    R.SET_STRING_ELT(names, 0, R.Rf_mkChar("a"));
    const bv = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 3));
    R.REAL(bv)[0] = 1.0;
    R.REAL(bv)[1] = 2.0;
    R.REAL(bv)[2] = 3.0;
    _ = R.SET_VECTOR_ELT(vec, 1, bv);
    R.SET_STRING_ELT(names, 1, R.Rf_mkChar("b"));
    _ = R.Rf_namesgets(vec, names);
    R.Rf_unprotect(4);

    const result = zigr_convert.fromSEXP(TestStruct, vec, arena.allocator());
    if (result.a != 42.0) return R.Rf_ScalarReal(0.0);
    if (result.b.len != 3) return R.Rf_ScalarReal(0.0);
    if (result.b[0] != 1.0 or result.b[2] != 3.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_embed_empty() SEXP {
    if (trycatch_mod.tryCatch(struct {
        fn call() R.SEXP {
            return embed.rCodeEval("", null);
        }
    }.call)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {
        return R.Rf_ScalarReal(1.0);
    }
}

export fn zigr_test_embed_vector() SEXP {
    const result = embed.rCodeEval("1:5", null);
    const len = R.XLENGTH(result);
    if (len == 5) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_embed_braces() SEXP {
    const result = embed.rCodeEval("{ x <- 1; x + 1 }", null);
    if (R.REAL(result)[0] == 2.0) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_embed_null() SEXP {
    const result = embed.rCodeEval("NULL", null);
    if (result == R.R_NilValue) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_struct_to_sexp_empty() SEXP {
    const Empty = struct {};
    const result = zigr_convert.asSEXP(Empty{});
    if (R.XLENGTH(result) != 0 or R.R_getAttribCount(result) != 1) return R.Rf_ScalarReal(0.0);
    const names = R.Rf_getAttrib(result, R.R_NamesSymbol);
    if (R.TYPEOF(names) != R.STRSXP or R.XLENGTH(names) != 0) return R.Rf_ScalarReal(0.0);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    _ = zigr_convert.tryFromSEXP(Empty, result, arena.allocator()) catch return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_struct_to_sexp_nested() SEXP {
    const Inner = struct { val: f64 };
    const Outer = struct { inner: Inner, name: []const u8 };
    const s = Outer{ .inner = Inner{ .val = 3.14 }, .name = "hello" };
    const result = zigr_convert.asSEXP(s);
    if (R.TYPEOF(result) != R.VECSXP) return R.Rf_ScalarReal(0.0);
    if (R.XLENGTH(result) != 2) return R.Rf_ScalarReal(0.0);
    const names = R.Rf_getAttrib(result, R.R_NamesSymbol);
    if (R.XLENGTH(names) != 2) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_struct_from_sexp_missing_optional_field() SEXP {
    const Test = struct { x: f64, y: ?f64 };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const vec = R.Rf_protect(R.Rf_allocVector(R.VECSXP, 1));
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    const xv = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    R.REAL(xv)[0] = 99.0;
    _ = R.SET_VECTOR_ELT(vec, 0, xv);
    R.SET_STRING_ELT(names, 0, R.Rf_mkChar("x"));
    _ = R.Rf_namesgets(vec, names);
    R.Rf_unprotect(3);

    const result = zigr_convert.fromSEXP(Test, vec, arena.allocator());
    if (result.x != 99.0) return R.Rf_ScalarReal(0.0);
    if (result.y != null) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_struct_from_sexp_optional_present() SEXP {
    const Test = struct { x: f64, y: ?f64 };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const vec = R.Rf_protect(R.Rf_allocVector(R.VECSXP, 2));
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 2));
    const xv = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    const yv = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    R.REAL(xv)[0] = 1.0;
    R.REAL(yv)[0] = 2.0;
    _ = R.SET_VECTOR_ELT(vec, 0, xv);
    _ = R.SET_VECTOR_ELT(vec, 1, yv);
    R.SET_STRING_ELT(names, 0, R.Rf_mkChar("x"));
    R.SET_STRING_ELT(names, 1, R.Rf_mkChar("y"));
    _ = R.Rf_namesgets(vec, names);
    R.Rf_unprotect(4);

    const result = zigr_convert.fromSEXP(Test, vec, arena.allocator());
    if (result.x != 1.0 or result.y != 2.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_fixed_schema_contract() SEXP {
    const Schema = struct { id: i32, ratio: f64, enabled: bool };
    const Short = struct { id: i32, ratio: f64 };
    const Extra = struct { id: i32, ratio: f64, enabled: bool, extra: i32 };
    const WrongField = struct { id: f64, ratio: f64, enabled: bool };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const valid = R.Rf_protect(zigr_convert.asSEXP(Schema{ .id = 7, .ratio = 2.5, .enabled = true }));
    defer R.Rf_unprotect(1);
    const parsed = zigr_convert.tryFromSEXP(Schema, valid, arena.allocator()) catch return R.Rf_ScalarReal(0.0);
    if (parsed.id != 7 or parsed.ratio != 2.5 or !parsed.enabled) return R.Rf_ScalarReal(0.0);

    var no_alloc_storage: [0]u8 align(16) = .{};
    var no_alloc = std.heap.FixedBufferAllocator.init(&no_alloc_storage);
    const no_alloc_parsed = zigr_convert.tryFromSEXP(Schema, valid, no_alloc.allocator()) catch return R.Rf_ScalarReal(0.0);
    if (no_alloc_parsed.id != 7 or no_alloc.end_index != 0) return R.Rf_ScalarReal(0.0);

    const short = R.Rf_protect(zigr_convert.asSEXP(Short{ .id = 7, .ratio = 2.5 }));
    defer R.Rf_unprotect(1);
    if (zigr_convert.tryFromSEXP(Schema, short, arena.allocator())) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |convert_err| {
        if (convert_err != error.SchemaLength) return R.Rf_ScalarReal(0.0);
    }

    const extra = R.Rf_protect(zigr_convert.asSEXP(Extra{ .id = 7, .ratio = 2.5, .enabled = true, .extra = 1 }));
    defer R.Rf_unprotect(1);
    if (zigr_convert.tryFromSEXP(Schema, extra, arena.allocator())) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |convert_err| {
        if (convert_err != error.SchemaLength) return R.Rf_ScalarReal(0.0);
    }

    const reordered = R.Rf_protect(zigr_convert.asSEXP(Schema{ .id = 7, .ratio = 2.5, .enabled = true }));
    defer R.Rf_unprotect(1);
    const reordered_names = R.Rf_getAttrib(reordered, R.R_NamesSymbol);
    const first_name = R.STRING_ELT(reordered_names, 0);
    R.SET_STRING_ELT(reordered_names, 0, R.STRING_ELT(reordered_names, 1));
    R.SET_STRING_ELT(reordered_names, 1, first_name);
    if (zigr_convert.tryFromSEXP(Schema, reordered, arena.allocator())) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |convert_err| {
        if (convert_err != error.SchemaNames) return R.Rf_ScalarReal(0.0);
    }

    const duplicate = R.Rf_protect(zigr_convert.asSEXP(Schema{ .id = 7, .ratio = 2.5, .enabled = true }));
    defer R.Rf_unprotect(1);
    const duplicate_names = R.Rf_getAttrib(duplicate, R.R_NamesSymbol);
    R.SET_STRING_ELT(duplicate_names, 1, R.STRING_ELT(duplicate_names, 0));
    if (zigr_convert.tryFromSEXP(Schema, duplicate, arena.allocator())) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |convert_err| {
        if (convert_err != error.SchemaNames) return R.Rf_ScalarReal(0.0);
    }

    const unnamed = R.Rf_protect(zigr_convert.asSEXP(Schema{ .id = 7, .ratio = 2.5, .enabled = true }));
    defer R.Rf_unprotect(1);
    _ = R.Rf_setAttrib(unnamed, R.R_NamesSymbol, R.R_NilValue);
    if (zigr_convert.tryFromSEXP(Schema, unnamed, arena.allocator())) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |convert_err| {
        if (convert_err != error.SchemaAttributes) return R.Rf_ScalarReal(0.0);
    }

    const attributed = R.Rf_protect(zigr_convert.asSEXP(Schema{ .id = 7, .ratio = 2.5, .enabled = true }));
    defer R.Rf_unprotect(1);
    const class = R.Rf_protect(R.Rf_ScalarString(R.Rf_mkChar("schema")));
    defer R.Rf_unprotect(1);
    _ = R.Rf_setAttrib(attributed, R.R_ClassSymbol, class);
    if (zigr_convert.tryFromSEXP(Schema, attributed, arena.allocator())) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |convert_err| {
        if (convert_err != error.SchemaAttributes) return R.Rf_ScalarReal(0.0);
    }

    const decorated_names = R.Rf_protect(zigr_convert.asSEXP(Schema{ .id = 7, .ratio = 2.5, .enabled = true }));
    defer R.Rf_unprotect(1);
    const names_class = R.Rf_protect(R.Rf_ScalarString(R.Rf_mkChar("schema-names")));
    defer R.Rf_unprotect(1);
    _ = R.Rf_setAttrib(R.Rf_getAttrib(decorated_names, R.R_NamesSymbol), R.R_ClassSymbol, names_class);
    if (zigr_convert.tryFromSEXP(Schema, decorated_names, arena.allocator())) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |convert_err| {
        if (convert_err != error.SchemaAttributes) return R.Rf_ScalarReal(0.0);
    }

    const wrong_field = R.Rf_protect(zigr_convert.asSEXP(WrongField{ .id = 7.0, .ratio = 2.5, .enabled = true }));
    defer R.Rf_unprotect(1);
    if (zigr_convert.tryFromSEXP(Schema, wrong_field, arena.allocator())) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |convert_err| {
        if (convert_err != error.ExpectedInteger) return R.Rf_ScalarReal(0.0);
    }

    if (zigr_convert.tryFromSEXP(Schema, R.R_NilValue, arena.allocator())) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |convert_err| {
        if (convert_err != error.ExpectedSchema) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_fixed_schema_nested_optional_long() SEXP {
    const Inner = struct { count: i32 };
    const Outer = struct {
        inner: Inner,
        value: ?f64,
        sequence: zigr.sexp.SEXP,
        optional_inner: ?Inner,
        raw_null: zigr.sexp.SEXP,
    };
    const root_sequence = compactRealSequence(2_147_483_648.0) orelse return R.Rf_ScalarReal(0.0);
    const sequence: zigr.sexp.SEXP = @ptrCast(root_sequence);
    defer R.Rf_unprotect(1);

    const encoded = R.Rf_protect(zigr_convert.asSEXP(Outer{
        .inner = .{ .count = 3 },
        .value = null,
        .sequence = sequence,
        .optional_inner = null,
        .raw_null = null,
    }));
    defer R.Rf_unprotect(1);
    if (R.R_getAttribCount(encoded) != 1) return R.Rf_ScalarReal(0.0);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const decoded = zigr_convert.tryFromSEXP(Outer, encoded, arena.allocator()) catch return R.Rf_ScalarReal(0.0);
    if (decoded.inner.count != 3 or decoded.value != null or decoded.sequence != sequence or decoded.optional_inner != null or decoded.raw_null != null) return R.Rf_ScalarReal(0.0);
    if (zigr.sexp.xlength(decoded.sequence) != 2_147_483_648) return R.Rf_ScalarReal(0.0);

    const present_inner = R.Rf_protect(zigr_convert.asSEXP(Inner{ .count = 9 }));
    defer R.Rf_unprotect(1);
    _ = R.SET_VECTOR_ELT(encoded, 3, present_inner);
    const present_decoded = zigr_convert.tryFromSEXP(Outer, encoded, arena.allocator()) catch return R.Rf_ScalarReal(0.0);
    if (present_decoded.optional_inner == null or present_decoded.optional_inner.?.count != 9) return R.Rf_ScalarReal(0.0);

    const typed_na = R.Rf_protect(R.Rf_ScalarReal(R.NA_REAL()));
    defer R.Rf_unprotect(1);
    _ = R.SET_VECTOR_ELT(encoded, 1, typed_na);
    const typed_na_decoded = zigr_convert.tryFromSEXP(Outer, encoded, arena.allocator()) catch return R.Rf_ScalarReal(0.0);
    if (typed_na_decoded.value != null or typed_na_decoded.optional_inner == null or typed_na_decoded.optional_inner.?.count != 9) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_stress_protect() SEXP {
    const Test = struct { a: f64, b: f64 };
    var i: u32 = 0;
    while (i < 1000) : (i += 1) {
        const s = Test{ .a = @as(f64, @floatFromInt(i)), .b = @as(f64, @floatFromInt(i + 1)) };
        const sexp = zigr_convert.asSEXP(s);
        _ = sexp;
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_stress_embed() SEXP {
    var i: u32 = 0;
    while (i < 500) : (i += 1) {
        const result = embed.rCodeEval("42", null);
        if (R.REAL(result)[0] != 42.0) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_embed_syntax_error() SEXP {
    if (trycatch_mod.tryCatch(struct {
        fn call() R.SEXP {
            return embed.rCodeEval("~~~", null);
        }
    }.call)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {
        return R.Rf_ScalarReal(1.0);
    }
}

export fn zigr_test_embed_stop_error() SEXP {
    if (trycatch_mod.tryCatch(struct {
        fn call() R.SEXP {
            return embed.rCodeEval("stop('test')", null);
        }
    }.call)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {
        return R.Rf_ScalarReal(1.0);
    }
}

export fn zigr_test_embed_warning() SEXP {
    if (trycatch_mod.tryCatchError(struct {
        fn call() R.SEXP {
            return embed.rCodeEval("{ warning('warn'); 42 }", null);
        }
    }.call)) |val| {
        if (val) |sxp| {
            if (R.TYPEOF(sxp) == R.REALSXP and R.REAL(sxp)[0] == 42.0) {
                return R.Rf_ScalarReal(1.0);
            }
        }
        return R.Rf_ScalarReal(0.0);
    } else |_| {
        return R.Rf_ScalarReal(0.0);
    }
}

export fn zigr_test_embed_unicode() SEXP {
    const result = embed.rCodeEval("nchar('abc')", null);
    if (R.INTEGER(result)[0] == 3) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_embed_long_code() SEXP {
    const result = embed.rCodeEval("paste(rep('x', 500), collapse='')", null);
    const elt = R.STRING_ELT(result, 0);
    if (elt == R.R_NaString) return R.Rf_ScalarReal(0.0);
    const s = std.mem.sliceTo(R.R_CHAR(elt), 0);
    if (s.len == 500) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_struct_roundtrip() SEXP {
    const Point = struct { x: f64, y: f64, label: []const u8 };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const original = Point{ .x = 1.5, .y = -3.2, .label = "pt1" };

    const sexp = zigr_convert.asSEXP(original);
    const restored = zigr_convert.fromSEXP(Point, sexp, arena.allocator());

    if (restored.x != 1.5) return R.Rf_ScalarReal(0.0);
    if (restored.y != -3.2) return R.Rf_ScalarReal(0.0);
    if (!std.mem.eql(u8, restored.label, "pt1")) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_struct_nan_inf() SEXP {
    const Data = struct { a: f64, b: f64 };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const original = Data{ .a = std.math.nan(f64), .b = std.math.inf(f64) };
    const sexp = zigr_convert.asSEXP(original);
    const restored = zigr_convert.fromSEXP(Data, sexp, arena.allocator());

    if (!std.math.isNan(restored.a)) return R.Rf_ScalarReal(0.0);
    if (restored.b != std.math.inf(f64)) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_struct_neg_zero() SEXP {
    const S = struct { v: f64 };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const sexp = zigr_convert.asSEXP(S{ .v = -0.0 });
    const restored = zigr_convert.fromSEXP(S, sexp, arena.allocator());

    const neg_zero: f64 = -0.0;
    if (@as(u64, @bitCast(restored.v)) != @as(u64, @bitCast(neg_zero))) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_struct_many_fields() SEXP {
    const Wide = struct {
        a: f64,
        b: f64,
        c: f64,
        d: f64,
        e: f64,
        f: f64,
        g: f64,
        h: f64,
        i: f64,
        j: f64,
    };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const original = Wide{
        .a = 1,
        .b = 2,
        .c = 3,
        .d = 4,
        .e = 5,
        .f = 6,
        .g = 7,
        .h = 8,
        .i = 9,
        .j = 10,
    };
    const sexp = zigr_convert.asSEXP(original);
    const restored = zigr_convert.fromSEXP(Wide, sexp, arena.allocator());

    if (restored.a != 1 or restored.j != 10) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_trycatch_nested() SEXP {
    const outer = trycatch_mod.tryCatch(struct {
        fn call() R.SEXP {
            const inner = trycatch_mod.tryCatch(struct {
                fn call() R.SEXP {
                    R.Rf_error("inner error");
                    return R.R_NilValue;
                }
            }.call);
            if (inner) |_| {
                return R.R_NilValue;
            } else |_| {
                return R.Rf_ScalarReal(42.0);
            }
        }
    }.call);
    if (outer) |val| {
        if (R.REAL(val)[0] == 42.0) return R.Rf_ScalarReal(1.0);
    } else |_| {}
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_stress_protect_10k() SEXP {
    const S = struct { v: f64 };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var i: u32 = 0;
    while (i < 10000) : (i += 1) {
        const sexp = zigr_convert.asSEXPAlloc(S{ .v = @floatFromInt(i) }, arena.allocator());
        _ = sexp;
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_build_call() SEXP {
    const args = [_]SEXP{
        R.Rf_ScalarReal(10.0),
        R.Rf_ScalarReal(20.0),
        R.Rf_ScalarReal(30.0),
    };
    const call = test_lang.buildCall(test_lang.symbol("sum"), args[0..]);
    const result = test_eval.rEval(call, null);
    if (R.TYPEOF(result) != R.REALSXP or R.XLENGTH(result) != 1) return R.Rf_ScalarReal(0.0);
    if (R.REAL(result)[0] != 60.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_build_named_call() SEXP {
    const call = test_lang.buildNamedCall("sum", .{ R.Rf_ScalarReal(1.0), R.Rf_ScalarReal(2.0) });
    const result = test_eval.rEval(call, null);
    if (R.TYPEOF(result) != R.REALSXP or R.XLENGTH(result) != 1) return R.Rf_ScalarReal(0.0);
    if (R.REAL(result)[0] != 3.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_scoped_release() SEXP {
    const vec = R.Rf_allocVector(R.REALSXP, 1);
    var s = protect.scoped(vec);
    const released = s.release();
    if (released != vec) return R.Rf_ScalarReal(0.0);
    protect.unprotect();
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_scoped_get() SEXP {
    const vec = R.Rf_allocVector(R.REALSXP, 5);
    var s = protect.scoped(vec);
    defer s.deinit();
    if (s.get() != vec) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_rvector_f64() SEXP {
    const n: R.R_xlen_t = 4;
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, n));
    defer R.Rf_unprotect(1);
    const ptr = R.REAL(vec);
    ptr[0] = 1.0;
    ptr[1] = 2.0;
    ptr[2] = 3.0;
    ptr[3] = 4.0;
    const rv = zigr.rvector.RVector(f64).init(vec) catch return R.Rf_ScalarReal(0.0);
    if (rv.len() != 4) return R.Rf_ScalarReal(0.0);
    if (rv.asSEXP() != vec) return R.Rf_ScalarReal(0.0);
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var view = rv.view(arena.allocator()) catch return R.Rf_ScalarReal(0.0);
    defer view.deinit();
    const v = view.constSlice();
    if (v.len != 4 or v[0] != 1.0 or v[3] != 4.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_rvector_i32() SEXP {
    const n: R.R_xlen_t = 3;
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, n));
    defer R.Rf_unprotect(1);
    const ptr = R.INTEGER(vec);
    ptr[0] = 10;
    ptr[1] = -5;
    ptr[2] = 99;
    const rv = zigr.rvector.RVector(i32).init(vec) catch return R.Rf_ScalarReal(0.0);
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var view = rv.view(arena.allocator()) catch return R.Rf_ScalarReal(0.0);
    defer view.deinit();
    const v = view.constSlice();
    if (v.len != 3 or v[0] != 10 or v[1] != -5 or v[2] != 99) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_rvector_wrong_type() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 3));
    R.Rf_unprotect(1);
    const rv = zigr.rvector.RVector(f64).init(vec);
    if (rv) |_| return R.Rf_ScalarReal(0.0) else |_| return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_rvector_add_scalar() SEXP {
    const n: R.R_xlen_t = 3;
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, n));
    defer R.Rf_unprotect(1);
    const ptr = R.REAL(vec);
    ptr[0] = 1.0;
    ptr[1] = 2.0;
    ptr[2] = 3.0;
    const rv = zigr.rvector.RVector(f64).init(vec) catch return R.Rf_ScalarReal(0.0);
    const result = rv.addScalar(10.0);
    const rp = R.REAL(result);
    if (rp[0] != 11.0 or rp[1] != 12.0 or rp[2] != 13.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_rvector_sub_scalar() SEXP {
    const n: R.R_xlen_t = 2;
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, n));
    defer R.Rf_unprotect(1);
    R.REAL(vec)[0] = 5.0;
    R.REAL(vec)[1] = 10.0;
    const rv = zigr.rvector.RVector(f64).init(vec) catch return R.Rf_ScalarReal(0.0);
    const result = rv.subScalar(3.0);
    const rp = R.REAL(result);
    if (rp[0] != 2.0 or rp[1] != 7.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_rvector_mul_scalar() SEXP {
    const n: R.R_xlen_t = 3;
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, n));
    defer R.Rf_unprotect(1);
    R.REAL(vec)[0] = 2.0;
    R.REAL(vec)[1] = 3.0;
    R.REAL(vec)[2] = 4.0;
    const rv = zigr.rvector.RVector(f64).init(vec) catch return R.Rf_ScalarReal(0.0);
    const result = rv.mulScalar(2.0);
    const rp = R.REAL(result);
    if (rp[0] != 4.0 or rp[1] != 6.0 or rp[2] != 8.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_rvector_div_scalar() SEXP {
    const n: R.R_xlen_t = 2;
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, n));
    defer R.Rf_unprotect(1);
    R.REAL(vec)[0] = 10.0;
    R.REAL(vec)[1] = 20.0;
    const rv = zigr.rvector.RVector(f64).init(vec) catch return R.Rf_ScalarReal(0.0);
    const result = rv.divScalar(2.0);
    const rp = R.REAL(result);
    if (rp[0] != 5.0 or rp[1] != 10.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_rvector_add_vec() SEXP {
    const n: R.R_xlen_t = 3;
    const va = R.Rf_protect(R.Rf_allocVector(R.REALSXP, n));
    const vb = R.Rf_protect(R.Rf_allocVector(R.REALSXP, n));
    defer R.Rf_unprotect(2);
    R.REAL(va)[0] = 1.0;
    R.REAL(va)[1] = 2.0;
    R.REAL(va)[2] = 3.0;
    R.REAL(vb)[0] = 10.0;
    R.REAL(vb)[1] = 20.0;
    R.REAL(vb)[2] = 30.0;
    const ra = (zigr.rvector.RVector(f64).init(va) catch return R.Rf_ScalarReal(0.0));
    const rb = (zigr.rvector.RVector(f64).init(vb) catch return R.Rf_ScalarReal(0.0));
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const result = ra.add(rb, arena.allocator());
    const rp = R.REAL(result);
    if (rp[0] != 11.0 or rp[1] != 22.0 or rp[2] != 33.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_rvector_f64_sum() SEXP {
    const n: R.R_xlen_t = 4;
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, n));
    defer R.Rf_unprotect(1);
    R.REAL(vec)[0] = 1.0;
    R.REAL(vec)[1] = 2.0;
    R.REAL(vec)[2] = 3.0;
    R.REAL(vec)[3] = 4.0;
    const rv = zigr.rvector.RVector(f64).init(vec) catch return R.Rf_ScalarReal(0.0);
    const total = rv.sum();
    if (total != 10.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_rvector_i32_sum() SEXP {
    const n: R.R_xlen_t = 3;
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, n));
    defer R.Rf_unprotect(1);
    R.INTEGER(vec)[0] = 100;
    R.INTEGER(vec)[1] = 200;
    R.INTEGER(vec)[2] = 300;
    const rv = zigr.rvector.RVector(i32).init(vec) catch return R.Rf_ScalarReal(0.0);
    const total = rv.sum();
    if (total != 600) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_rvector_recycle() SEXP {
    const va = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    const vb = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 3));
    defer R.Rf_unprotect(2);
    R.REAL(va)[0] = 10.0;
    R.REAL(vb)[0] = 1.0;
    R.REAL(vb)[1] = 2.0;
    R.REAL(vb)[2] = 3.0;
    const ra = (zigr.rvector.RVector(f64).init(va) catch return R.Rf_ScalarReal(0.0));
    const rb = (zigr.rvector.RVector(f64).init(vb) catch return R.Rf_ScalarReal(0.0));
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const result = ra.add(rb, arena.allocator());
    const rp = R.REAL(result);
    if (R.XLENGTH(result) != 3) return R.Rf_ScalarReal(0.0);
    if (rp[0] != 11.0 or rp[1] != 12.0 or rp[2] != 13.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_rvector_empty() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 0));
    R.Rf_unprotect(1);
    const rv = zigr.rvector.RVector(f64).init(vec) catch return R.Rf_ScalarReal(0.0);
    const result = rv.addScalar(5.0);
    if (R.XLENGTH(result) != 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_export_two_scalars() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const a = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    R.REAL(a)[0] = 3.0;
    const b = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 1));
    R.INTEGER(b)[0] = 4;
    defer R.Rf_unprotect(2);

    const a_val = zigr_convert.toRealScalar(a) catch return R.Rf_ScalarReal(0.0);
    const b_val = zigr_convert.toIntScalar(b) catch return R.Rf_ScalarReal(0.0);

    if (a_val != 3.0 or b_val != 4) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_export_sum() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const n = 5;
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, n));
    const ptr = R.REAL(vec);
    for (0..n) |i| ptr[i] = @as(f64, @floatFromInt(i + 1));
    R.Rf_unprotect(1);

    const view = zigr_convert.toRealSliceView(arena.allocator(), vec) catch return R.Rf_ScalarReal(0.0);
    const data = view.constSlice();

    var total: f64 = 0;
    for (data) |v| total += v;
    if (total != 15.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_export_string_lengths() SEXP {
    const strs = [_][]const u8{ "hello", "world", "zigr" };
    const n: R.R_xlen_t = @intCast(strs.len);
    const vec = R.Rf_protect(R.Rf_allocVector(R.STRSXP, n));
    for (0..@as(usize, @intCast(n))) |i| {
        const cs = R.Rf_mkCharLenCE(@ptrCast(strs[i].ptr), @intCast(strs[i].len), @as(R.cetype_t, @intCast(R.CE_UTF8)));
        R.SET_STRING_ELT(vec, @intCast(i), cs);
    }
    R.Rf_unprotect(1);

    const view = zigr_convert.toStringSliceView(vec) catch return R.Rf_ScalarReal(0.0);
    var total: usize = 0;
    var it = view.iterator();
    while (it.next()) |s| total += s.bytes.len;

    if (total != 14) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_export_string_na() SEXP {
    const n: R.R_xlen_t = 2;
    const vec = R.Rf_protect(R.Rf_allocVector(R.STRSXP, n));
    defer R.Rf_unprotect(1);
    R.SET_STRING_ELT(vec, 0, R.Rf_mkChar("ok"));
    R.SET_STRING_ELT(vec, 1, R.R_NaString);

    const view = zigr_convert.toStringSliceView(vec) catch return R.Rf_ScalarReal(0.0);
    const first = view.at(0);
    const second = view.at(1);

    if (first.is_na) return R.Rf_ScalarReal(0.0);
    if (!second.is_na) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_export_cached_string_lengths() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const strs = [_][]const u8{ "hello", "world", "zigr" };
    const n: R.R_xlen_t = @intCast(strs.len);
    const vec = R.Rf_protect(R.Rf_allocVector(R.STRSXP, n));
    defer R.Rf_unprotect(1);
    for (0..@as(usize, @intCast(n))) |i| {
        const cs = R.Rf_mkCharLenCE(@ptrCast(strs[i].ptr), @intCast(strs[i].len), @as(R.cetype_t, @intCast(R.CE_UTF8)));
        R.SET_STRING_ELT(vec, @intCast(i), cs);
    }

    const cached = zigr_convert.toCachedStringSliceView(arena.allocator(), vec) catch return R.Rf_ScalarReal(0.0);
    var total: usize = 0;
    for (0..cached.len) |i| total += cached.at(i).bytes.len;

    if (total != 14) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_export_sum_empty() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 0));
    R.Rf_unprotect(1);

    const view = zigr_convert.toRealSliceView(arena.allocator(), vec) catch return R.Rf_ScalarReal(0.0);
    const data = view.constSlice();
    if (data.len != 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_export_raw_roundtrip() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const expected = [_]u8{ 10, 20, 30 };
    const n: R.R_xlen_t = @intCast(expected.len);
    const vec = R.Rf_protect(R.Rf_allocVector(R.RAWSXP, n));
    @memcpy(R.RAW(vec)[0..expected.len], &expected);
    R.Rf_unprotect(1);

    const slice = zigr_convert.toRawSlice(arena.allocator(), vec) catch return R.Rf_ScalarReal(0.0);
    if (!std.mem.eql(u8, slice, &expected)) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_export_complex_sum() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const n: R.R_xlen_t = 2;
    const vec = R.Rf_protect(R.Rf_allocVector(R.CPLXSXP, n));
    const raw_ptr = R.COMPLEX(vec) orelse return R.Rf_ScalarReal(0.0);
    const ptr: [*]zigr_convert.Rcomplex = @ptrCast(@alignCast(raw_ptr));
    ptr[0] = .{ .r = 1.0, .i = 2.0 };
    ptr[1] = .{ .r = 3.0, .i = 4.0 };
    R.Rf_unprotect(1);

    const view = zigr_convert.toComplexSliceView(arena.allocator(), vec) catch return R.Rf_ScalarReal(0.0);
    const data = view.constSlice();
    if (data.len != 2) return R.Rf_ScalarReal(0.0);
    if (data[0].r != 1.0 or data[0].i != 2.0) return R.Rf_ScalarReal(0.0);
    if (data[1].r != 3.0 or data[1].i != 4.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_export_optional_null() SEXP {
    const nullish = zigr_convert.optionalInputIsNullish(f64, R.R_NilValue);
    if (!nullish) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_export_scalar_na() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    R.REAL(vec)[0] = R.NA_REAL();
    R.Rf_unprotect(1);

    const result = zigr_convert.toRealScalar(vec);
    if (result == error.ScalarNA) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_export_int_scalar_na() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 1));
    R.INTEGER(vec)[0] = R.R_NaInt;
    R.Rf_unprotect(1);

    const result = zigr_convert.toIntScalar(vec);
    if (result == error.ScalarNA) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_export_bool_scalar_na() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, 1));
    R.LOGICAL(vec)[0] = R.R_NaInt;
    R.Rf_unprotect(1);

    const result = zigr_convert.toBoolScalar(vec);
    if (result == error.ScalarNA) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_test_export_wrong_type_real() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 1));
    R.INTEGER(vec)[0] = 42;
    R.Rf_unprotect(1);

    const result = zigr_convert.toRealScalar(vec);
    if (result == error.ExpectedReal) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

const ExportCounter = struct {
    val: i32,

    fn increment(self: *ExportCounter, amount: i32) i32 {
        self.val += amount;
        return self.val;
    }
};

export fn zigr_test_export_method_call() SEXP {
    var counter = ExportCounter{ .val = 0 };
    const prot = R.Rf_protect(R.R_NilValue);
    const ext = zigr.externalptr.make(&counter, R.R_NilValue, prot);
    R.Rf_unprotect(1);

    const addr = zigr.externalptr.addr(ext) orelse return R.Rf_ScalarReal(0.0);
    const ptr: *ExportCounter = @ptrCast(@alignCast(addr));
    const result = ptr.increment(5);

    if (result != 5) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_export_method_tag() SEXP {
    var counter = ExportCounter{ .val = 0 };
    const tag_sexp = R.Rf_protect(R.Rf_ScalarReal(42.0));
    const prot = R.Rf_protect(R.R_NilValue);
    const ext = zigr.externalptr.make(&counter, tag_sexp, prot);
    R.Rf_unprotect(2);

    const tag_back = zigr.externalptr.tag(ext);
    if (tag_back != tag_sexp) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_export_from_sexp_wrong_type() SEXP {
    const Test = struct { x: f64 };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 1));
    R.INTEGER(vec)[0] = 42;
    R.Rf_unprotect(1);

    _ = zigr_convert.fromSEXP(Test, vec, arena.allocator());
    return R.Rf_ScalarReal(0.0);
}

fn externalSum(a: f64, b: f64) f64 {
    return a + b;
}

const ExternalExports = zigr.@"export".generateExports(&.{}, &.{
    .{ .name = "zigr_test_external_sum", .func = externalSum },
});

const arena_vector_output = [_]f64{ 2.0, 4.0, 8.0 };

fn arenaScalar(value: f64) f64 {
    return value;
}

fn fixedScratchRaw(values: []const u8) i32 {
    var total: i32 = 0;
    for (values) |value| total += @intCast(value);
    return total;
}

fn spillThenAllocate(values: []const i32) R.SEXP {
    const result = R.Rf_allocVector(R.REALSXP, 2);
    R.REAL(result)[0] = @floatFromInt(values[0]);
    R.REAL(result)[1] = @floatFromInt(values[values.len - 1]);
    return result;
}

fn arenaVectorOutput(_: f64) []const f64 {
    return arena_vector_output[0..];
}

fn spillThenError(values: []const i32) void {
    _ = values;
    R.Rf_error("zigr generated spill: expected error");
}

fn invalidStringResult() []const []const u8 {
    const values = struct {
        const items = [_][]const u8{"invalid\x00string"};
    };
    return values.items[0..];
}

fn arenaBorrowedSum(values: []const f64) f64 {
    var total: f64 = 0;
    for (values) |value| total += value;
    return total;
}

const ArenaExports = zigr.@"export".generateExports(&.{
    .{ .name = "zigr_arena_scalar", .func = arenaScalar },
    .{ .name = "zigr_fixed_scratch_raw", .func = fixedScratchRaw },
    .{ .name = "zigr_spill_allocate", .func = spillThenAllocate },
    .{ .name = "zigr_arena_vector_output", .func = arenaVectorOutput },
    .{ .name = "zigr_spill_error", .func = spillThenError },
    .{ .name = "zigr_invalid_string_result", .func = invalidStringResult },
    .{ .name = "zigr_arena_borrowed_sum", .func = arenaBorrowedSum },
}, &.{});

const ArenaCall = *const fn (R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP) callconv(.c) R.SEXP;

fn arenaCall(index: usize, arg: SEXP) SEXP {
    const fun: ArenaCall = @ptrCast(@alignCast(ArenaExports.call_defs[index].fun));
    return fun(arg, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue);
}

fn initArenaExports() bool {
    const dll = test_dll orelse (R.R_getEmbeddingDllInfo() orelse return false);
    ArenaExports.init(dll);
    return true;
}

fn stringViewLengths(values: zigr_convert.StringSliceView) i32 {
    var total: i32 = 0;
    var iterator = values.iterator();
    while (iterator.next()) |value| {
        if (!value.is_na) total += @intCast(value.len);
    }
    return total;
}

fn cachedStringLengths(values: zigr_convert.CachedStringSliceView) i32 {
    var cached = values;
    defer cached.deinit();
    var total: i32 = 0;
    for (0..4) |_| {
        var iterator = cached.iterator();
        while (iterator.next()) |value| {
            if (!value.is_na) total += @intCast(value.len);
        }
    }
    return total;
}

fn stringHeaderLengths(values: []const []const u8) i32 {
    var total: i32 = 0;
    for (values) |value| total += @intCast(value.len);
    return total;
}

fn rawViewSum(values: zigr_convert.RawSliceView) i32 {
    var view = values;
    defer view.deinit();
    var total: i32 = 0;
    for (view.constSlice()) |value| total += @intCast(value);
    return total;
}

fn complexEcho(values: []const zigr_convert.Rcomplex) []const zigr_convert.Rcomplex {
    return values;
}

const BoundaryExports = zigr.@"export".generateExports(&.{
    .{ .name = "zigr_string_view_lengths", .func = stringViewLengths },
    .{ .name = "zigr_cached_string_lengths", .func = cachedStringLengths },
    .{ .name = "zigr_string_header_lengths", .func = stringHeaderLengths },
    .{ .name = "zigr_raw_view_sum", .func = rawViewSum },
    .{ .name = "zigr_complex_echo", .func = complexEcho },
}, &.{});

const BoundaryCall = *const fn (R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP) callconv(.c) R.SEXP;

fn boundaryCall(index: usize, arg: SEXP) SEXP {
    const fun: BoundaryCall = @ptrCast(@alignCast(BoundaryExports.call_defs[index].fun));
    return fun(arg, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue);
}

fn initBoundaryExports() bool {
    const dll = test_dll orelse (R.R_getEmbeddingDllInfo() orelse return false);
    BoundaryExports.init(dll);
    return true;
}

export fn zigr_test_export_external() SEXP {
    const dll = test_dll orelse (R.R_getEmbeddingDllInfo() orelse return R.Rf_ScalarReal(0.0));
    ExternalExports.init(dll);

    const ext_fun: *const fn (R.SEXP) callconv(.c) R.SEXP = @ptrCast(@alignCast(ExternalExports.ext_defs[0].fun));
    const arg1 = R.Rf_protect(R.Rf_ScalarReal(3.0));
    const arg2 = R.Rf_protect(R.Rf_ScalarReal(4.0));
    const pairlist = R.Rf_protect(R.Rf_cons(
        R.Rf_install("zigr_test_external_sum"),
        R.Rf_cons(arg1, R.Rf_cons(arg2, R.R_NilValue)),
    ));
    R.Rf_unprotect(3);

    const result = ext_fun(pairlist);
    const val = R.REAL(result)[0];
    if (val != 7.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_generated_ownership_gc() SEXP {
    if (!initArenaExports()) return R.Rf_ScalarReal(0.0);
    if (!initBoundaryExports()) return R.Rf_ScalarReal(0.0);

    const scalar_arg = R.Rf_protect(R.Rf_ScalarReal(3.25));
    defer R.Rf_unprotect(1);
    const scalar_result = R.Rf_protect(arenaCall(0, scalar_arg));
    defer R.Rf_unprotect(1);

    const raw = R.Rf_protect(R.Rf_allocVector(R.RAWSXP, 64));
    defer R.Rf_unprotect(1);
    for (0..64) |i| R.RAW(raw)[i] = @intCast(i + 1);
    const raw_result = R.Rf_protect(arenaCall(1, raw));
    defer R.Rf_unprotect(1);

    const compact = compactIntSequence(100_000) orelse return R.Rf_ScalarReal(0.0);
    defer R.Rf_unprotect(1);
    if (R.ALTREP(compact) == 0 or R.INTEGER_OR_NULL(compact) != null) return R.Rf_ScalarReal(0.0);
    const spill_result = R.Rf_protect(arenaCall(2, compact));
    defer R.Rf_unprotect(1);

    const vector_result = R.Rf_protect(arenaCall(3, scalar_arg));
    defer R.Rf_unprotect(1);

    const call_args = [_]SEXP{ scalar_arg, scalar_arg };
    const call_result = R.Rf_protect(test_eval.call("sum", call_args[0..]));
    defer R.Rf_unprotect(1);

    const strings = [_][]const u8{ "arena", "ownership" };
    const string_result = R.Rf_protect(zigr_convert.fromStringSlice(strings[0..]));
    defer R.Rf_unprotect(1);

    const named_values = [_]f64{ 5.0, 7.0 };
    const Named = struct { id: i32, values: []const f64 };
    const named_result = R.Rf_protect(zigr_convert.asSEXP(Named{ .id = 9, .values = named_values[0..] }));
    defer R.Rf_unprotect(1);

    const borrowed_input = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 3));
    defer R.Rf_unprotect(1);
    R.REAL(borrowed_input)[0] = 2.0;
    R.REAL(borrowed_input)[1] = 3.0;
    R.REAL(borrowed_input)[2] = 5.0;
    const borrowed_result = R.Rf_protect(arenaCall(6, borrowed_input));
    defer R.Rf_unprotect(1);

    const cached_input = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 3));
    defer R.Rf_unprotect(1);
    R.SET_STRING_ELT(cached_input, 0, R.Rf_mkChar("one"));
    R.SET_STRING_ELT(cached_input, 1, R.R_NaString);
    R.SET_STRING_ELT(cached_input, 2, R.Rf_mkChar("three"));
    const cached_result = R.Rf_protect(boundaryCall(1, cached_input));
    defer R.Rf_unprotect(1);

    for (0..16) |_| {
        const noise = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 262_144));
        R.REAL(noise)[0] = 1.0;
        R.Rf_unprotect(1);
    }
    R.R_gc();

    if (R.REAL(scalar_result)[0] != 3.25) return R.Rf_ScalarReal(0.0);
    if (R.INTEGER(raw_result)[0] != 2080) return R.Rf_ScalarReal(0.0);
    if (R.XLENGTH(spill_result) != 2 or R.REAL(spill_result)[0] != 1.0 or R.REAL(spill_result)[1] != 100000.0) return R.Rf_ScalarReal(0.0);
    if (R.XLENGTH(vector_result) != 3 or R.REAL(vector_result)[2] != 8.0) return R.Rf_ScalarReal(0.0);
    if (R.REAL(call_result)[0] != 6.5) return R.Rf_ScalarReal(0.0);
    if (R.STRING_ELT(string_result, 0) == R.R_NaString or R.STRING_ELT(string_result, 1) == R.R_NaString) return R.Rf_ScalarReal(0.0);
    if (R.VECTOR_ELT(named_result, 0) == R.R_NilValue or R.INTEGER(R.VECTOR_ELT(named_result, 0))[0] != 9) return R.Rf_ScalarReal(0.0);
    if (R.REAL(R.VECTOR_ELT(named_result, 1))[1] != 7.0) return R.Rf_ScalarReal(0.0);
    if (R.REAL(borrowed_result)[0] != 10.0) return R.Rf_ScalarReal(0.0);
    if (R.INTEGER(cached_result)[0] != 32) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_allocation_diagnostics() SEXP {
    const real = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 4));
    defer R.Rf_unprotect(1);
    var borrowed_counting = mem.CountingAllocator.init(std.heap.page_allocator);
    var borrowed = zigr_convert.toRealSliceView(borrowed_counting.allocator(), real) catch return R.Rf_ScalarReal(0.0);
    defer borrowed.deinit();
    switch (borrowed) {
        .borrowed => {},
        .owned => return R.Rf_ScalarReal(0.0),
    }
    if (borrowed_counting.stats.allocations != 0) return R.Rf_ScalarReal(0.0);

    const compact = compactIntSequence(1024) orelse return R.Rf_ScalarReal(0.0);
    defer R.Rf_unprotect(1);
    if (R.ALTREP(compact) == 0 or R.INTEGER_OR_NULL(compact) != null) return R.Rf_ScalarReal(0.0);
    var altrep_counting = mem.CountingAllocator.init(std.heap.page_allocator);
    {
        var materialized = zigr_convert.toIntSliceView(altrep_counting.allocator(), compact) catch return R.Rf_ScalarReal(0.0);
        defer materialized.deinit();
        switch (materialized) {
            .borrowed => return R.Rf_ScalarReal(0.0),
            .owned => {},
        }
        if (altrep_counting.stats.allocations != 1 or altrep_counting.stats.live_bytes != 4096) return R.Rf_ScalarReal(0.0);
    }
    if (altrep_counting.stats.frees != 1 or altrep_counting.stats.live_bytes != 0) return R.Rf_ScalarReal(0.0);

    const strings = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 3));
    defer R.Rf_unprotect(1);
    R.SET_STRING_ELT(strings, 0, R.Rf_mkChar("one"));
    R.SET_STRING_ELT(strings, 1, R.R_NaString);
    R.SET_STRING_ELT(strings, 2, R.Rf_mkChar("three"));
    var cache_counting = mem.CountingAllocator.init(std.heap.page_allocator);
    {
        var cached = zigr_convert.toCachedStringSliceView(cache_counting.allocator(), strings) catch return R.Rf_ScalarReal(0.0);
        defer cached.deinit();
        if (cache_counting.stats.allocations != 1 or cache_counting.stats.live_bytes != 3 * @sizeOf(zigr_convert.StringView)) return R.Rf_ScalarReal(0.0);
    }
    if (cache_counting.stats.frees != 1 or cache_counting.stats.live_bytes != 0) return R.Rf_ScalarReal(0.0);

    const Schema = struct { id: i32, ratio: f64, enabled: bool };
    const schema = R.Rf_protect(zigr_convert.asSEXP(Schema{ .id = 7, .ratio = 2.5, .enabled = true }));
    defer R.Rf_unprotect(1);
    var schema_counting = mem.CountingAllocator.init(std.heap.page_allocator);
    const parsed = zigr_convert.tryFromSEXP(Schema, schema, schema_counting.allocator()) catch return R.Rf_ScalarReal(0.0);
    if (parsed.id != 7 or parsed.ratio != 2.5 or !parsed.enabled or schema_counting.stats.allocations != 0) return R.Rf_ScalarReal(0.0);

    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_cleanup_diagnostics() SEXP {
    if (!initBoundaryExports()) return R.Rf_ScalarReal(0.0);
    cleanup.resetDiagnostics();

    const strings = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 3));
    defer R.Rf_unprotect(1);
    R.SET_STRING_ELT(strings, 0, R.Rf_mkChar("one"));
    R.SET_STRING_ELT(strings, 1, R.R_NaString);
    R.SET_STRING_ELT(strings, 2, R.Rf_mkChar("three"));
    const result = boundaryCall(1, strings);
    if (R.INTEGER(result)[0] != 32) return R.Rf_ScalarReal(0.0);

    var tracked = protect.scoped(R.Rf_allocVector(R.INTSXP, 1));
    tracked.deinit();
    const diagnostics = cleanup.diagnosticSnapshot();
    if (!diagnostics.enabled) {
        if (diagnostics.max_cleanup_frames != 0 or diagnostics.max_unwind_boundaries != 0 or diagnostics.max_protect_depth != 0) return R.Rf_ScalarReal(0.0);
        return R.Rf_ScalarReal(1.0);
    }
    if (diagnostics.max_cleanup_frames != 1) return R.Rf_ScalarReal(0.0);
    if (diagnostics.max_unwind_boundaries != 1) return R.Rf_ScalarReal(0.0);
    if (diagnostics.max_protect_depth != 1) return R.Rf_ScalarReal(0.0);

    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_generated_spill_longjmp() SEXP {
    if (!initArenaExports()) return R.Rf_ScalarReal(0.0);
    const compact = compactIntSequence(100_000) orelse return R.Rf_ScalarReal(0.0);
    if (R.ALTREP(compact) == 0 or R.INTEGER_OR_NULL(compact) != null) return R.Rf_ScalarReal(0.0);
    return arenaCall(4, compact);
}

export fn zigr_test_generated_result_longjmp() SEXP {
    if (!initArenaExports()) return R.Rf_ScalarReal(0.0);
    if (trycatch_mod.tryCatch(struct {
        fn call() SEXP {
            return arenaCall(5, R.R_NilValue);
        }
    }.call)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}

    return arenaCall(0, R.Rf_ScalarReal(1.0));
}

var externalptr_finalizer_count: u8 = 0;

const ExternalValue = struct {
    value: i32,
};

fn externalValueDeinit(value: *ExternalValue) void {
    if (value.value == 17) externalptr_finalizer_count += 1;
}

export fn zigr_test_externalptr_finalizer() SEXP {
    externalptr_finalizer_count = 0;
    var ext = R.Rf_protect(zigr.externalptr.createTyped(ExternalValue, .{ .value = 17 }, externalValueDeinit));
    const raw = zigr.externalptr.addr(ext) orelse {
        R.Rf_unprotect(1);
        return R.Rf_ScalarReal(0.0);
    };
    const value: *ExternalValue = @ptrCast(@alignCast(raw));
    const backing = zigr.externalptr.typedBacking(ExternalValue, ext) catch {
        R.Rf_unprotect(1);
        return R.Rf_ScalarReal(0.0);
    };
    if (value.value != 17 or zigr.externalptr.tag(ext) != zigr.externalptr.typeTag(ExternalValue) or backing != R.R_NilValue) {
        R.Rf_unprotect(1);
        return R.Rf_ScalarReal(0.0);
    }

    R.Rf_unprotect(1);
    ext = R.R_NilValue;
    R.R_gc();
    R.R_gc();
    return R.Rf_ScalarReal(if (externalptr_finalizer_count == 1) 1.0 else 0.0);
}

export fn zigr_test_externalptr_finalizer_idempotent() SEXP {
    externalptr_finalizer_count = 0;
    var ext = R.Rf_protect(zigr.externalptr.createTyped(ExternalValue, .{ .value = 17 }, externalValueDeinit));

    zigr.externalptr.finalizeOwned(ExternalValue, externalValueDeinit, ext);
    zigr.externalptr.finalizeOwned(ExternalValue, externalValueDeinit, ext);
    if (zigr.externalptr.addr(ext) != null or externalptr_finalizer_count != 1) {
        R.Rf_unprotect(1);
        return R.Rf_ScalarReal(0.0);
    }
    R.Rf_unprotect(1);
    ext = R.R_NilValue;
    R.R_gc();
    R.R_gc();
    return R.Rf_ScalarReal(if (externalptr_finalizer_count == 1) 1.0 else 0.0);
}

export fn zigr_test_externalptr_typed_protected() SEXP {
    const dll = test_dll orelse (R.R_getEmbeddingDllInfo() orelse return R.Rf_ScalarReal(0.0));
    CounterMethods.init(dll);

    var counter = MethodCounter{ .val = 0 };
    const backing = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 1));
    R.INTEGER(backing)[0] = 42;
    const ext = R.Rf_protect(zigr.externalptr.makeTyped(MethodCounter, &counter, backing));
    R.Rf_unprotect(1);
    defer R.Rf_unprotect(1);
    R.R_gc();
    R.R_gc();

    if (zigr.externalptr.tag(ext) != zigr.externalptr.typeTag(MethodCounter)) return R.Rf_ScalarReal(0.0);
    const retained_backing = zigr.externalptr.typedBacking(MethodCounter, ext) catch return R.Rf_ScalarReal(0.0);
    if (retained_backing != backing or R.INTEGER(backing)[0] != 42) return R.Rf_ScalarReal(0.0);
    const amount = R.Rf_protect(R.Rf_ScalarInteger(1));
    defer R.Rf_unprotect(1);
    const method_fun: MethodCall = @ptrCast(@alignCast(CounterMethods.call_defs[0].fun));
    if (R.INTEGER(method_fun(ext, amount, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue))[0] != 1 or counter.val != 1) return R.Rf_ScalarReal(0.0);
    if (zigr.externalptr.makeTypedRawChecked(MethodCounter, @ptrCast(&counter), null)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |pointer_error| {
        if (pointer_error != error.NullBacking) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

const LazyTagValue = struct { value: i32 };

export fn zigr_test_externalptr_lazy_tag_gc() SEXP {
    var value = LazyTagValue{ .value = 23 };
    _ = embed.rCodeEval("gctorture(TRUE)", R.R_BaseEnv);
    const ext = R.Rf_protect(zigr.externalptr.makeTyped(LazyTagValue, &value, R.R_NilValue));
    _ = embed.rCodeEval("gctorture(FALSE)", R.R_BaseEnv);
    defer R.Rf_unprotect(1);

    const restored = zigr.externalptr.checkedPointer(LazyTagValue, ext) catch return R.Rf_ScalarReal(0.0);
    if (restored != &value or restored.value != 23) return R.Rf_ScalarReal(0.0);
    const backing = zigr.externalptr.typedBacking(LazyTagValue, ext) catch return R.Rf_ScalarReal(0.0);
    if (backing != R.R_NilValue) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_weakref_reachable_contract() SEXP {
    const key_sxp = R.Rf_protect(R.R_NewEnv(R.R_EmptyEnv, 0, 29));
    const value_sxp = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 1));
    R.INTEGER(value_sxp)[0] = 42;
    const weak_ref = R.Rf_protect(weakref_mod.makeChecked(key_sxp, value_sxp, null, false) catch {
        R.Rf_unprotect(2);
        return R.Rf_ScalarReal(0.0);
    });

    // Only the live key and weak reference may retain the value across GC.
    R.Rf_unprotect(3);
    _ = R.Rf_protect(key_sxp);
    _ = R.Rf_protect(weak_ref);
    defer R.Rf_unprotect(2);
    R.R_gc();
    R.R_gc();

    const retained_key = weakref_mod.keyChecked(weak_ref) catch return R.Rf_ScalarReal(0.0);
    if (retained_key != key_sxp) return R.Rf_ScalarReal(0.0);
    const retained_value = weakref_mod.valueChecked(weak_ref) catch return R.Rf_ScalarReal(0.0);
    if (R.TYPEOF(retained_value) != R.INTSXP or R.INTEGER(retained_value)[0] != 42) {
        return R.Rf_ScalarReal(0.0);
    }

    const external_key = R.Rf_protect(R.R_MakeExternalPtr(null, R.R_NilValue, R.R_NilValue));
    defer R.Rf_unprotect(1);
    const external_ref = R.Rf_protect(weakref_mod.makeChecked(external_key, R.R_NilValue, null, false) catch
        return R.Rf_ScalarReal(0.0));
    defer R.Rf_unprotect(1);
    if (weakref_mod.key(external_ref) != external_key or weakref_mod.value(external_ref) != R.R_NilValue) {
        return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_weakref_checked_errors() SEXP {
    const key_sxp = R.Rf_protect(R.R_NewEnv(R.R_EmptyEnv, 0, 29));
    defer R.Rf_unprotect(1);
    const integer = R.Rf_protect(R.Rf_ScalarInteger(1));
    defer R.Rf_unprotect(1);

    if (weakref_mod.makeChecked(null, R.R_NilValue, null, false)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |weakref_error| {
        if (weakref_error != error.NullKey) return R.Rf_ScalarReal(0.0);
    }
    for ([_]SEXP{integer}) |invalid_key| {
        if (weakref_mod.makeChecked(invalid_key, R.R_NilValue, null, false)) |_| {
            return R.Rf_ScalarReal(0.0);
        } else |weakref_error| {
            if (weakref_error != error.ExpectedReferenceKey) return R.Rf_ScalarReal(0.0);
        }
    }
    if (weakref_mod.makeChecked(key_sxp, null, null, false)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |weakref_error| {
        if (weakref_error != error.NullValue) return R.Rf_ScalarReal(0.0);
    }

    const inert = R.Rf_protect(weakref_mod.makeChecked(R.R_NilValue, integer, null, false) catch
        return R.Rf_ScalarReal(0.0));
    defer R.Rf_unprotect(1);
    if (weakref_mod.key(inert) != R.R_NilValue or weakref_mod.value(inert) != R.R_NilValue) {
        return R.Rf_ScalarReal(0.0);
    }
    weakref_mod.runFinalizer(inert);
    for ([_]SEXP{ null, R.R_NilValue, integer }) |invalid_ref| {
        if (weakref_mod.keyChecked(invalid_ref)) |_| {
            return R.Rf_ScalarReal(0.0);
        } else |weakref_error| {
            if (weakref_error != error.ExpectedWeakReference) return R.Rf_ScalarReal(0.0);
        }
        if (weakref_mod.valueChecked(invalid_ref)) |_| {
            return R.Rf_ScalarReal(0.0);
        } else |weakref_error| {
            if (weakref_error != error.ExpectedWeakReference) return R.Rf_ScalarReal(0.0);
        }
        if (weakref_mod.runFinalizerChecked(invalid_ref)) |_| {
            return R.Rf_ScalarReal(0.0);
        } else |weakref_error| {
            if (weakref_error != error.ExpectedWeakReference) return R.Rf_ScalarReal(0.0);
        }
    }
    return R.Rf_ScalarReal(1.0);
}

var weakref_finalizer_count: u8 = 0;
var weakref_finalizer_saw_key = false;

fn weakRefFinalizer(key_sxp: SEXP) callconv(.c) void {
    weakref_finalizer_count += 1;
    if (key_sxp != null and R.TYPEOF(key_sxp) == R.ENVSXP) weakref_finalizer_saw_key = true;
}

export fn zigr_test_weakref_gc_finalizer() SEXP {
    weakref_finalizer_count = 0;
    weakref_finalizer_saw_key = false;
    const key_sxp = R.Rf_protect(R.R_NewEnv(R.R_EmptyEnv, 0, 29));
    const value_sxp = R.Rf_protect(R.Rf_ScalarInteger(17));
    const weak_ref = R.Rf_protect(weakref_mod.make(key_sxp, value_sxp, weakRefFinalizer, false));

    R.Rf_unprotect(3);
    _ = R.Rf_protect(weak_ref);
    defer R.Rf_unprotect(1);
    R.R_gc();
    R.R_gc();
    if (weakref_finalizer_count != 1 or !weakref_finalizer_saw_key) return R.Rf_ScalarReal(0.0);
    if (weakref_mod.key(weak_ref) != R.R_NilValue or weakref_mod.value(weak_ref) != R.R_NilValue) {
        return R.Rf_ScalarReal(0.0);
    }

    const explicit_key = R.Rf_protect(R.R_NewEnv(R.R_EmptyEnv, 0, 29));
    defer R.Rf_unprotect(1);
    const explicit_ref = R.Rf_protect(weakref_mod.make(explicit_key, R.R_NilValue, weakRefFinalizer, false));
    defer R.Rf_unprotect(1);
    weakref_mod.runFinalizer(explicit_ref);
    weakref_mod.runFinalizer(explicit_ref);
    if (weakref_finalizer_count != 2) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_raw_views() SEXP {
    const expected = [_]u8{ 0x00, 0xff, 0x7f };
    const raw = R.Rf_protect(R.Rf_allocVector(R.RAWSXP, @intCast(expected.len)));
    defer R.Rf_unprotect(1);
    @memcpy(R.RAW(raw)[0..expected.len], &expected);

    var no_alloc_storage: [0]u8 align(16) = .{};
    var no_alloc = std.heap.FixedBufferAllocator.init(&no_alloc_storage);
    var view = zigr_convert.toRawSliceView(no_alloc.allocator(), raw) catch return R.Rf_ScalarReal(0.0);
    defer view.deinit();
    switch (view) {
        .borrowed => {},
        .owned => return R.Rf_ScalarReal(0.0),
    }
    if (!std.mem.eql(u8, view.constSlice(), &expected) or no_alloc.end_index != 0) return R.Rf_ScalarReal(0.0);

    const rvector = zigr.rvector.RVector(u8).init(raw) catch return R.Rf_ScalarReal(0.0);
    var rvector_view = rvector.view(no_alloc.allocator()) catch return R.Rf_ScalarReal(0.0);
    defer rvector_view.deinit();
    switch (rvector_view) {
        .borrowed => {},
        .owned => return R.Rf_ScalarReal(0.0),
    }
    if (!std.mem.eql(u8, rvector_view.constSlice(), &expected) or no_alloc.end_index != 0) return R.Rf_ScalarReal(0.0);

    var copy_storage: [expected.len]u8 align(16) = undefined;
    var copy_fba = std.heap.FixedBufferAllocator.init(&copy_storage);
    const copied = zigr_convert.toRawSlice(copy_fba.allocator(), raw) catch return R.Rf_ScalarReal(0.0);
    defer copy_fba.allocator().free(copied);
    R.RAW(raw)[0] = 0x42;
    if (!std.mem.eql(u8, copied, &expected)) return R.Rf_ScalarReal(0.0);

    const raw_altrep = R.Rf_protect(shortRegionAltRaw());
    defer R.Rf_unprotect(1);
    if (R.ALTREP(raw_altrep) == 0 or R.RAW_OR_NULL(raw_altrep) != null) return R.Rf_ScalarReal(0.0);
    var fallback_storage: [short_region_len]u8 align(16) = undefined;
    var fallback_fba = std.heap.FixedBufferAllocator.init(&fallback_storage);
    short_raw_region_get_calls = 0;
    short_raw_region_elt_calls = 0;
    {
        var fallback_view = zigr_convert.toRawSliceView(fallback_fba.allocator(), raw_altrep) catch return R.Rf_ScalarReal(0.0);
        defer fallback_view.deinit();
        switch (fallback_view) {
            .borrowed => return R.Rf_ScalarReal(0.0),
            .owned => {},
        }
        const fallback = fallback_view.constSlice();
        if (fallback.len != short_region_len or fallback[0] != 0 or fallback[250] != 250 or fallback[251] != 0) return R.Rf_ScalarReal(0.0);
        const expected_region_calls = (short_region_len + short_region_cap - 1) / short_region_cap;
        if (short_raw_region_get_calls != expected_region_calls or short_raw_region_elt_calls != 0) return R.Rf_ScalarReal(0.0);
    }
    if (fallback_fba.end_index != 0) return R.Rf_ScalarReal(0.0);

    if (!initBoundaryExports()) return R.Rf_ScalarReal(0.0);
    const generated = R.Rf_protect(boundaryCall(3, raw));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(generated) != R.INTSXP or R.INTEGER(generated)[0] != 448) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_complex_boundary() SEXP {
    const complex = R.Rf_protect(R.Rf_allocVector(R.CPLXSXP, 3));
    defer R.Rf_unprotect(1);
    const raw_ptr = R.COMPLEX(complex) orelse return R.Rf_ScalarReal(0.0);
    const ptr: [*]zigr_convert.Rcomplex = @ptrCast(@alignCast(raw_ptr));
    ptr[0] = .{ .r = 1.0, .i = -2.0 };
    ptr[1] = .{ .r = R.NA_REAL(), .i = std.math.nan(f64) };
    ptr[2] = .{ .r = std.math.nan(f64), .i = R.NA_REAL() };

    var no_alloc_storage: [0]u8 align(16) = .{};
    var no_alloc = std.heap.FixedBufferAllocator.init(&no_alloc_storage);
    var view = zigr_convert.toComplexSliceView(no_alloc.allocator(), complex) catch return R.Rf_ScalarReal(0.0);
    defer view.deinit();
    switch (view) {
        .borrowed => {},
        .owned => return R.Rf_ScalarReal(0.0),
    }
    const values = view.constSlice();
    if (@intFromPtr(values.ptr) % @alignOf(zigr_convert.Rcomplex) != 0 or values.len != 3) return R.Rf_ScalarReal(0.0);
    if (values[0].r != 1.0 or values[0].i != -2.0) return R.Rf_ScalarReal(0.0);
    if (R.ISNA(values[1].r) == 0 or R.ISNA(values[1].i) != 0 or !R.ISNAN(values[1].i)) return R.Rf_ScalarReal(0.0);
    if (R.ISNA(values[2].r) != 0 or !R.ISNAN(values[2].r) or R.ISNA(values[2].i) == 0) return R.Rf_ScalarReal(0.0);

    const altrep = R.Rf_protect(shortRegionAltComplex());
    defer R.Rf_unprotect(1);
    if (R.ALTREP(altrep) == 0 or R.COMPLEX_OR_NULL(altrep) != null) return R.Rf_ScalarReal(0.0);
    var fallback_storage: [short_region_len * @sizeOf(zigr_convert.Rcomplex)]u8 align(@alignOf(zigr_convert.Rcomplex)) = undefined;
    var fallback_fba = std.heap.FixedBufferAllocator.init(&fallback_storage);
    short_complex_region_get_calls = 0;
    {
        var fallback_view = zigr_convert.toComplexSliceView(fallback_fba.allocator(), altrep) catch return R.Rf_ScalarReal(0.0);
        defer fallback_view.deinit();
        switch (fallback_view) {
            .borrowed => return R.Rf_ScalarReal(0.0),
            .owned => {},
        }
        const fallback = fallback_view.constSlice();
        if (fallback.len != short_region_len or fallback[0].r != 1.0 or fallback[0].i != -1.0 or fallback[250].r != 251.0 or fallback[250].i != -251.0) return R.Rf_ScalarReal(0.0);
        const expected_region_calls = (short_region_len + short_region_cap - 1) / short_region_cap;
        if (short_complex_region_get_calls != expected_region_calls) return R.Rf_ScalarReal(0.0);
    }
    if (fallback_fba.end_index != 0) return R.Rf_ScalarReal(0.0);

    var copy_storage: [3 * @sizeOf(zigr_convert.Rcomplex)]u8 align(@alignOf(zigr_convert.Rcomplex)) = undefined;
    var copy_fba = std.heap.FixedBufferAllocator.init(&copy_storage);
    const copied = zigr_convert.toComplexSlice(copy_fba.allocator(), complex) catch return R.Rf_ScalarReal(0.0);
    defer copy_fba.allocator().free(copied);
    if (copied.len != values.len or R.ISNA(copied[1].r) == 0 or !R.ISNAN(copied[2].r)) return R.Rf_ScalarReal(0.0);

    const returned = R.Rf_protect(zigr_convert.fromComplexSlice(copied));
    defer R.Rf_unprotect(1);
    const returned_ptr = R.COMPLEX(returned) orelse return R.Rf_ScalarReal(0.0);
    const returned_values: [*]const zigr_convert.Rcomplex = @ptrCast(@alignCast(returned_ptr));
    if (R.TYPEOF(returned) != R.CPLXSXP or R.XLENGTH(returned) != 3 or R.ISNA(returned_values[1].r) == 0 or !R.ISNAN(returned_values[2].r)) return R.Rf_ScalarReal(0.0);

    if (!initBoundaryExports()) return R.Rf_ScalarReal(0.0);
    const generated = R.Rf_protect(boundaryCall(4, complex));
    defer R.Rf_unprotect(1);
    const generated_ptr = R.COMPLEX(generated) orelse return R.Rf_ScalarReal(0.0);
    const generated_values: [*]const zigr_convert.Rcomplex = @ptrCast(@alignCast(generated_ptr));
    if (R.TYPEOF(generated) != R.CPLXSXP or R.XLENGTH(generated) != 3 or R.ISNA(generated_values[1].r) == 0 or !R.ISNAN(generated_values[2].r)) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_generated_string_shapes() SEXP {
    if (!initBoundaryExports()) return R.Rf_ScalarReal(0.0);

    const strings = [_][]const u8{ "alpha", "", "beta" };
    const vec = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 4));
    defer R.Rf_unprotect(1);
    R.SET_STRING_ELT(vec, 0, R.Rf_mkCharLenCE(@ptrCast(strings[0].ptr), @intCast(strings[0].len), @as(R.cetype_t, @intCast(R.CE_UTF8))));
    R.SET_STRING_ELT(vec, 1, R.Rf_mkCharLenCE(@ptrCast(strings[1].ptr), @intCast(strings[1].len), @as(R.cetype_t, @intCast(R.CE_UTF8))));
    R.SET_STRING_ELT(vec, 2, R.R_NaString);
    R.SET_STRING_ELT(vec, 3, R.Rf_mkCharLenCE(@ptrCast(strings[2].ptr), @intCast(strings[2].len), @as(R.cetype_t, @intCast(R.CE_UTF8))));

    const view_result = R.Rf_protect(boundaryCall(0, vec));
    defer R.Rf_unprotect(1);
    const cached_result = R.Rf_protect(boundaryCall(1, vec));
    defer R.Rf_unprotect(1);
    const headers_result = R.Rf_protect(boundaryCall(2, vec));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(view_result) != R.INTSXP or R.INTEGER(view_result)[0] != 9) return R.Rf_ScalarReal(0.0);
    if (R.TYPEOF(cached_result) != R.INTSXP or R.INTEGER(cached_result)[0] != 36) return R.Rf_ScalarReal(0.0);
    if (R.TYPEOF(headers_result) != R.INTSXP or R.INTEGER(headers_result)[0] != 9) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

const MethodCounter = struct {
    val: i32,
};

fn counterAdd(self: *MethodCounter, amount: i32) i32 {
    self.val += amount;
    return self.val;
}

const CounterMethods = zigr.@"export".generateMethods(MethodCounter, &.{
    .{ .name = "add", .func = counterAdd },
}, &.{
    .{ .name = "add_external", .func = counterAdd },
});

const MethodCall = *const fn (R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP) callconv(.c) R.SEXP;
const ExternalMethodCall = *const fn (R.SEXP) callconv(.c) R.SEXP;

// R_tryCatch needs no-argument trampolines for these generated entry points.
var method_error_fun: ?MethodCall = null;
var method_error_receiver: R.SEXP = null;
var method_error_amount: R.SEXP = null;
var external_method_error_fun: ?ExternalMethodCall = null;
var external_method_error_args: R.SEXP = null;

fn callMethodForError() R.SEXP {
    return method_error_fun.?(method_error_receiver, method_error_amount, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue);
}

fn expectMethodError(receiver: R.SEXP, amount: R.SEXP, expected: []const u8) bool {
    method_error_receiver = receiver;
    method_error_amount = amount;
    const condition = trycatch_mod.tryCatchError(callMethodForError) catch return false;
    return if (condition) |value| std.mem.eql(u8, trycatch_mod.extractMessage(value), expected) else false;
}

fn callExternalMethodForError() R.SEXP {
    return external_method_error_fun.?(external_method_error_args);
}

fn expectExternalMethodError(args: R.SEXP, expected: []const u8) bool {
    external_method_error_args = args;
    const condition = trycatch_mod.tryCatchError(callExternalMethodForError) catch return false;
    return if (condition) |value| std.mem.eql(u8, trycatch_mod.extractMessage(value), expected) else false;
}

var inner_cleanup_fired: bool = false;
var outer_cleanup_fired: bool = false;

fn markInnerCleanup(_: ?*anyopaque) void {
    inner_cleanup_fired = true;
}

fn markOuterCleanup(_: ?*anyopaque) void {
    outer_cleanup_fired = true;
}

export fn zigr_test_cleanup_fires_on_longjmp() SEXP {
    inner_cleanup_fired = false;

    if (trycatch_mod.tryCatch(struct {
        fn call() R.SEXP {
            return cleanup.protectCall(struct {
                fn inner() R.SEXP {
                    cleanup.pushFrame(markInnerCleanup, null);
                    R.Rf_error("expected cleanup test error");
                    return R.R_NilValue;
                }
            }.inner);
        }
    }.call)) |_| {} else |_| {}

    return R.Rf_ScalarReal(if (inner_cleanup_fired) 1.0 else 0.0);
}

export fn zigr_test_nested_unwind_state() SEXP {
    const initial_depth = protect.getDepth();
    inner_cleanup_fired = false;
    outer_cleanup_fired = false;
    cleanup.pushFrame(markOuterCleanup, null);

    var iteration: usize = 0;
    while (iteration < 32) : (iteration += 1) {
        inner_cleanup_fired = false;
        if (trycatch_mod.tryCatch(struct {
            fn call() R.SEXP {
                return cleanup.protectCall(struct {
                    fn inner() R.SEXP {
                        cleanup.pushFrame(markInnerCleanup, null);
                        _ = protect.protect(R.Rf_ScalarReal(1.0));
                        R.Rf_error("expected nested unwind error");
                        return R.R_NilValue;
                    }
                }.inner);
            }
        }.call)) |_| {} else |_| {}

        if (!inner_cleanup_fired or outer_cleanup_fired) {
            cleanup.popFrame();
            return R.Rf_ScalarReal(0.0);
        }
        if (protect.getDepth() != initial_depth) {
            cleanup.popFrame();
            return R.Rf_ScalarReal(0.0);
        }
    }

    cleanup.popFrame();
    const fresh = cleanup.protectCall(struct {
        fn call() R.SEXP {
            return R.Rf_ScalarReal(1.0);
        }
    }.call);
    if (outer_cleanup_fired or protect.getDepth() != initial_depth) return R.Rf_ScalarReal(0.0);
    return fresh;
}

var capacity_cleanup_count: usize = 0;

fn countCapacityCleanup(_: ?*anyopaque) void {
    capacity_cleanup_count += 1;
}

export fn zigr_test_cleanup_capacity_recovers() SEXP {
    capacity_cleanup_count = 0;
    if (trycatch_mod.tryCatch(struct {
        fn call() R.SEXP {
            return cleanup.protectCall(struct {
                fn fill() R.SEXP {
                    for (0..cleanup.MAX_NESTING + 1) |_| cleanup.pushFrame(countCapacityCleanup, null);
                    return R.R_NilValue;
                }
            }.fill);
        }
    }.call)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    if (capacity_cleanup_count != cleanup.MAX_NESTING) return R.Rf_ScalarReal(0.0);

    return cleanup.protectCall(struct {
        fn call() R.SEXP {
            return R.Rf_ScalarReal(1.0);
        }
    }.call);
}

export fn zigr_test_final_cleanup_state() SEXP {
    return zigr_test_cleanup_capacity_recovers();
}

export fn zigr_test_str_na_nullable() SEXP {
    const n: R.R_xlen_t = 3;
    const vec = R.Rf_protect(R.Rf_allocVector(R.STRSXP, n));
    defer R.Rf_unprotect(1);
    R.SET_STRING_ELT(vec, 0, R.Rf_mkChar("hello"));
    R.SET_STRING_ELT(vec, 1, R.R_NaString);
    R.SET_STRING_ELT(vec, 2, R.Rf_mkChar("world"));

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const slice = zigr_convert.toStringSliceNullable(arena.allocator(), @as(SEXP, @ptrCast(vec))) catch return R.Rf_ScalarReal(0.0);
    if (slice.len != 3) return R.Rf_ScalarReal(0.0);
    if (slice[0] == null) return R.Rf_ScalarReal(0.0);
    if (!std.mem.eql(u8, slice[0].?, "hello")) return R.Rf_ScalarReal(0.0);
    if (slice[1] != null) return R.Rf_ScalarReal(0.0); // NA should map to null
    if (slice[2] == null) return R.Rf_ScalarReal(0.0);
    if (!std.mem.eql(u8, slice[2].?, "world")) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_with_rng_longjmp() SEXP {
    const Draw = struct {
        fn one() R.SEXP {
            return R.Rf_ScalarReal(R.unif_rand());
        }

        fn thenError() R.SEXP {
            _ = R.unif_rand();
            R.Rf_error("expected withRng test error");
        }
    };

    _ = embed.rCodeEval("set.seed(1729)", R.R_BaseEnv);
    const first = R.Rf_protect(rng.withRng(Draw.one));
    R.Rf_unprotect(1);
    _ = first;
    const expected_second = R.Rf_protect(rng.withRng(Draw.one));
    defer R.Rf_unprotect(1);

    _ = embed.rCodeEval("set.seed(1729)", R.R_BaseEnv);
    if (trycatch_mod.tryCatch(struct {
        fn call() R.SEXP {
            return rng.withRng(Draw.thenError);
        }
    }.call)) |_| {} else |_| {}

    const actual_second = R.Rf_protect(rng.withRng(Draw.one));
    defer R.Rf_unprotect(1);
    if (R.REAL(actual_second)[0] != R.REAL(expected_second)[0]) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_with_rng_nested() SEXP {
    if (trycatch_mod.tryCatch(struct {
        fn call() R.SEXP {
            return rng.withRng(struct {
                fn outer() R.SEXP {
                    return rng.withRng(struct {
                        fn inner() R.SEXP {
                            return R.Rf_ScalarReal(R.unif_rand());
                        }
                    }.inner);
                }
            }.outer);
        }
    }.call)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}

    const fresh = R.Rf_protect(rng.withRng(struct {
        fn draw() R.SEXP {
            return R.Rf_ScalarReal(R.unif_rand());
        }
    }.draw));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(fresh) != R.REALSXP or std.math.isNan(R.REAL(fresh)[0])) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_export_generatemethods() SEXP {
    const dll = test_dll orelse (R.R_getEmbeddingDllInfo() orelse return R.Rf_ScalarReal(0.0));
    CounterMethods.init(dll);

    const method_fun: MethodCall = @ptrCast(@alignCast(CounterMethods.call_defs[0].fun));

    var counter = MethodCounter{ .val = 10 };
    const extptr = R.Rf_protect(zigr.externalptr.makeTyped(MethodCounter, &counter, R.R_NilValue));
    defer R.Rf_unprotect(1);
    const amount = R.Rf_protect(R.Rf_ScalarInteger(5));
    defer R.Rf_unprotect(1);

    const result = method_fun(extptr, amount, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue);
    const val = R.INTEGER(result)[0];
    if (val != 15) return R.Rf_ScalarReal(0.0);
    if (counter.val != 15) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_export_generatemethods_external() SEXP {
    const dll = test_dll orelse (R.R_getEmbeddingDllInfo() orelse return R.Rf_ScalarReal(0.0));
    CounterMethods.init(dll);

    const method_fun: ExternalMethodCall = @ptrCast(@alignCast(CounterMethods.ext_defs[0].fun));
    if (CounterMethods.call_defs[0].numArgs != 2 or CounterMethods.ext_defs[0].numArgs != 2) return R.Rf_ScalarReal(0.0);
    var counter = MethodCounter{ .val = 10 };
    const receiver = R.Rf_protect(zigr.externalptr.makeTyped(MethodCounter, &counter, R.R_NilValue));
    defer R.Rf_unprotect(1);
    const amount = R.Rf_protect(R.Rf_ScalarInteger(5));
    defer R.Rf_unprotect(1);
    const args = R.Rf_protect(R.Rf_cons(
        R.Rf_install("zigr_test_counter_add_external"),
        R.Rf_cons(receiver, R.Rf_cons(amount, R.R_NilValue)),
    ));
    defer R.Rf_unprotect(1);

    const result = method_fun(args);
    if (R.INTEGER(result)[0] != 15 or counter.val != 15) return R.Rf_ScalarReal(0.0);

    external_method_error_fun = method_fun;
    const invalid_receiver = R.Rf_protect(R.Rf_ScalarInteger(0));
    defer R.Rf_unprotect(1);
    const invalid_args = R.Rf_protect(R.Rf_cons(
        R.Rf_install("zigr_test_counter_add_external"),
        R.Rf_cons(invalid_receiver, R.Rf_cons(amount, R.R_NilValue)),
    ));
    defer R.Rf_unprotect(1);
    if (!expectExternalMethodError(invalid_args, "expected EXTPTRSXP receiver")) return R.Rf_ScalarReal(0.0);

    const raw_tagged = R.Rf_protect(zigr.externalptr.make(@ptrCast(&counter), zigr.externalptr.typeTag(MethodCounter), R.R_NilValue));
    defer R.Rf_unprotect(1);
    const raw_args = R.Rf_protect(R.Rf_cons(
        R.Rf_install("zigr_test_counter_add_external"),
        R.Rf_cons(raw_tagged, R.Rf_cons(amount, R.R_NilValue)),
    ));
    defer R.Rf_unprotect(1);
    if (!expectExternalMethodError(raw_args, "external pointer is missing typed metadata")) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_generated_method_receiver_errors() SEXP {
    const dll = test_dll orelse (R.R_getEmbeddingDllInfo() orelse return R.Rf_ScalarReal(0.0));
    CounterMethods.init(dll);
    method_error_fun = @ptrCast(@alignCast(CounterMethods.call_defs[0].fun));

    var counter = MethodCounter{ .val = 0 };
    var foreign = struct { value: i32 }{ .value = 0 };
    const amount = R.Rf_protect(R.Rf_ScalarInteger(1));
    defer R.Rf_unprotect(1);
    const not_pointer = R.Rf_protect(R.Rf_ScalarInteger(0));
    defer R.Rf_unprotect(1);
    if (!expectMethodError(not_pointer, amount, "expected EXTPTRSXP receiver")) return R.Rf_ScalarReal(0.0);

    const wrong_tag = R.Rf_protect(zigr.externalptr.make(@ptrCast(&counter), R.Rf_install("zigr_test_wrong_method_tag"), R.R_NilValue));
    defer R.Rf_unprotect(1);
    if (!expectMethodError(wrong_tag, amount, "external pointer tag does not match method type")) return R.Rf_ScalarReal(0.0);

    const raw_tagged = R.Rf_protect(zigr.externalptr.make(@ptrCast(&counter), zigr.externalptr.typeTag(MethodCounter), R.R_NilValue));
    defer R.Rf_unprotect(1);
    if (!expectMethodError(raw_tagged, amount, "external pointer is missing typed metadata")) return R.Rf_ScalarReal(0.0);

    const tampered = R.Rf_protect(zigr.externalptr.makeTyped(MethodCounter, &counter, R.R_NilValue));
    defer R.Rf_unprotect(1);
    _ = R.SET_VECTOR_ELT(zigr.externalptr.protected(tampered), 0, R.R_NilValue);
    if (!expectMethodError(tampered, amount, "external pointer is missing typed metadata")) return R.Rf_ScalarReal(0.0);

    const ForeignCounter = @TypeOf(foreign);
    const foreign_pointer = R.Rf_protect(zigr.externalptr.makeTyped(ForeignCounter, &foreign, R.R_NilValue));
    defer R.Rf_unprotect(1);
    if (!expectMethodError(foreign_pointer, amount, "external pointer tag does not match method type")) return R.Rf_ScalarReal(0.0);

    const null_pointer = R.Rf_protect(zigr.externalptr.makeTypedRaw(MethodCounter, null, R.R_NilValue));
    defer R.Rf_unprotect(1);
    if (!expectMethodError(null_pointer, amount, "external pointer has been cleared")) return R.Rf_ScalarReal(0.0);

    const cleared_pointer = R.Rf_protect(zigr.externalptr.makeTyped(MethodCounter, &counter, R.R_NilValue));
    defer R.Rf_unprotect(1);
    R.R_ClearExternalPtr(cleared_pointer);
    if (!expectMethodError(cleared_pointer, amount, "external pointer has been cleared")) return R.Rf_ScalarReal(0.0);

    const address = @intFromPtr(&counter) + 1;
    const misaligned: *anyopaque = @ptrFromInt(address);
    const misaligned_pointer = R.Rf_protect(zigr.externalptr.makeTypedRaw(MethodCounter, misaligned, R.R_NilValue));
    defer R.Rf_unprotect(1);
    if (!expectMethodError(misaligned_pointer, amount, "external pointer is misaligned for method type")) return R.Rf_ScalarReal(0.0);
    if (counter.val != 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

fn fuzzExpectTypeRejects(comptime guardFn: *const fn (SEXP) void, sexp: SEXP) SEXP {
    guardFn(sexp);
    return R.Rf_ScalarReal(0.0);
}

fn fuzzExpectTypeAccepts(comptime guardFn: *const fn (SEXP) void, sexp: SEXP) SEXP {
    guardFn(sexp);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_fuzz_sum_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.sum(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_norm2_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, 3));
    R.Rf_unprotect(1);
    _ = zigr_convert.norm2(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_min_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.min(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_max_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.max(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_scaleAdd_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.scaleAdd(v, 2.0, 1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_cumsum_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.cumsum(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_sum_narm_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.sum_narm(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_mean_narm_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.mean_narm(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_argmin_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.argmin(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_argmax_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.argmax(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_toRealScalar_type() SEXP {
    const v = R.Rf_protect(R.Rf_ScalarInteger(42));
    R.Rf_unprotect(1);
    _ = zigr_convert.toRealScalar(v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_toIntScalar_type() SEXP {
    const v = R.Rf_protect(R.Rf_ScalarReal(3.14));
    R.Rf_unprotect(1);
    _ = zigr_convert.toIntScalar(v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_toBoolScalar_type() SEXP {
    const v = R.Rf_protect(R.Rf_ScalarInteger(1));
    R.Rf_unprotect(1);
    _ = zigr_convert.toBoolScalar(v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_toRealSlice_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    const alloc = std.heap.page_allocator;
    _ = zigr_convert.toRealSlice(alloc, v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_toIntSlice_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 5));
    R.Rf_unprotect(1);
    const alloc = std.heap.page_allocator;
    _ = zigr_convert.toIntSlice(alloc, v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_toStringSlice_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 5));
    R.Rf_unprotect(1);
    const alloc = std.heap.page_allocator;
    _ = zigr_convert.toStringSlice(alloc, v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_toListSlice_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 5));
    R.Rf_unprotect(1);
    const alloc = std.heap.page_allocator;
    _ = zigr_convert.toListSlice(alloc, v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_toRawSlice_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 5));
    R.Rf_unprotect(1);
    const alloc = std.heap.page_allocator;
    _ = zigr_convert.toRawSlice(alloc, v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_toRawSliceView_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 5));
    R.Rf_unprotect(1);
    const alloc = std.heap.page_allocator;
    _ = zigr_convert.toRawSliceView(alloc, v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_sumInt_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.sumInt(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_countTrue_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.countTrue(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_minInt_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.minInt(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_maxInt_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.maxInt(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_argminInt_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.argminInt(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_argmaxInt_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.argmaxInt(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_minLogical_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.minLogical(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_maxLogical_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.maxLogical(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_argminLogical_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.argminLogical(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_argmaxLogical_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.argmaxLogical(v);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_toRealSliceView_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    const alloc = std.heap.page_allocator;
    _ = zigr_convert.toRealSliceView(alloc, v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_toIntSliceView_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 5));
    R.Rf_unprotect(1);
    const alloc = std.heap.page_allocator;
    _ = zigr_convert.toIntSliceView(alloc, v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_toComplexSlice_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    const alloc = std.heap.page_allocator;
    _ = zigr_convert.toComplexSlice(alloc, v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_toLogicalSlice_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    const alloc = std.heap.page_allocator;
    _ = zigr_convert.toLogicalSlice(alloc, v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_toComplexSliceView_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    const alloc = std.heap.page_allocator;
    _ = zigr_convert.toComplexSliceView(alloc, v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_toLogicalSliceView_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(1);
    const alloc = std.heap.page_allocator;
    _ = zigr_convert.toLogicalSliceView(alloc, v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_pmin_type() SEXP {
    const a = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    const b = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(2);
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    _ = zigr_convert.pminAlloc(a, b, arena.allocator());
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_pmax_type() SEXP {
    const a = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    const b = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    R.Rf_unprotect(2);
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    _ = zigr_convert.pmaxAlloc(a, b, arena.allocator());
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_toStringSliceView_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 5));
    R.Rf_unprotect(1);
    _ = zigr_convert.toStringSliceView(v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_toCachedStringSliceView_type() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 5));
    R.Rf_unprotect(1);
    const alloc = std.heap.page_allocator;
    _ = zigr_convert.toCachedStringSliceView(alloc, v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_scalar_na() SEXP {
    const v = R.Rf_protect(R.Rf_ScalarReal(R.R_NaReal));
    R.Rf_unprotect(1);
    _ = zigr_convert.toRealScalar(v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_scalar_empty() SEXP {
    const v = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 0));
    R.Rf_unprotect(1);
    _ = zigr_convert.toRealScalar(v) catch return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

export fn zigr_fuzz_findVar_unbound() SEXP {
    const sym = R.Rf_install("__zigr_fuzz_nonexistent__");
    _ = test_eval.findVar(sym, R.R_EmptyEnv);
    return R.Rf_ScalarReal(0.0);
}
