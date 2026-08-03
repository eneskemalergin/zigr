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

extern var R_interrupts_pending: c_int;

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
    ObjectExports.init(info);
    BoundaryExports.init(info);
    RuntimeExports.init(info);
    ReferenceExports.init(info);
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

const long_string_len: R.R_xlen_t = @as(R.R_xlen_t, @intCast(std.math.maxInt(i32))) + 17;
var long_string_class: R.R_altrep_class_t = undefined;
var long_string_registered = false;

fn longStringLength(_: SEXP) callconv(.c) R.R_xlen_t {
    return long_string_len;
}

fn longStringElt(_: SEXP, index: R.R_xlen_t) callconv(.c) SEXP {
    return if (index == long_string_len - 1) R.Rf_mkChar("tail") else R.Rf_mkChar("head");
}

fn longStringAlt() SEXP {
    if (!long_string_registered) {
        long_string_class = R.R_make_altstring_class("long_string", "zigr", null);
        R.R_set_altrep_Length_method(long_string_class, longStringLength);
        R.R_set_altstring_Elt_method(long_string_class, longStringElt);
        long_string_registered = true;
    }
    return R.R_new_altrep(long_string_class, R.R_NilValue, R.R_NilValue);
}

fn embeddedNulConstructor() SEXP {
    const embedded = [_]u8{ 'a', 0, 'b' };
    return R.Rf_mkCharLenCE(@ptrCast(&embedded), @intCast(embedded.len), @as(R.cetype_t, @intCast(R.CE_BYTES)));
}

const short_region_len: usize = 4097;
const short_region_cap: usize = 257;
var short_region_class: R.R_altrep_class_t = undefined;
var short_region_registered = false;
var short_region_get_calls: usize = 0;
var short_region_elt_calls: usize = 0;
var short_region_max_requested: usize = 0;

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
    short_region_max_requested = @max(short_region_max_requested, @as(usize, @intCast(requested)));
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
var short_raw_region_max_requested: usize = 0;

fn shortRawRegionElt(_: SEXP, index: R.R_xlen_t) callconv(.c) R.Rbyte {
    short_raw_region_elt_calls += 1;
    return @intCast(@as(usize, @intCast(index)) % 251);
}

fn shortRawRegionGetRegion(_: SEXP, start: R.R_xlen_t, requested: R.R_xlen_t, buffer: [*c]R.Rbyte) callconv(.c) R.R_xlen_t {
    if (buffer == null) return 0;
    const offset: usize = @intCast(start);
    if (offset >= short_region_len) return 0;

    short_raw_region_get_calls += 1;
    short_raw_region_max_requested = @max(short_raw_region_max_requested, @as(usize, @intCast(requested)));
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

var failing_raw_region_class: R.R_altrep_class_t = undefined;
var failing_raw_region_registered = false;

fn failingRawRegionGetRegion(_: SEXP, _: R.R_xlen_t, _: R.R_xlen_t, _: [*c]R.Rbyte) callconv(.c) R.R_xlen_t {
    return 0;
}

fn failingRawRegion() SEXP {
    if (!failing_raw_region_registered) {
        failing_raw_region_class = R.R_make_altraw_class("failing_region_raw", "zigr", null);
        R.R_set_altrep_Length_method(failing_raw_region_class, shortRegionLength);
        R.R_set_altvec_Dataptr_or_null_method(failing_raw_region_class, shortRegionDataptrOrNull);
        R.R_set_altraw_Elt_method(failing_raw_region_class, shortRawRegionElt);
        R.R_set_altraw_Get_region_method(failing_raw_region_class, failingRawRegionGetRegion);
        failing_raw_region_registered = true;
    }
    return R.R_new_altrep(failing_raw_region_class, R.R_NilValue, R.R_NilValue);
}

var lifetime_region_class: R.R_altrep_class_t = undefined;
var lifetime_region_registered = false;
var lifetime_region_mode: u2 = 0;
threadlocal var lifetime_region_input: SEXP = null;

fn lifetimeRegionGetRegion(_: SEXP, start: R.R_xlen_t, requested: R.R_xlen_t, buffer: [*c]R.Rbyte) callconv(.c) R.R_xlen_t {
    if (lifetime_region_mode == 1) R.Rf_error("expected region callback error");
    if (lifetime_region_mode == 2) ict.checkInterrupt();
    return shortRawRegionGetRegion(R.R_NilValue, start, requested, buffer);
}

fn lifetimeRegionRaw() SEXP {
    if (!lifetime_region_registered) {
        lifetime_region_class = R.R_make_altraw_class("lifetime_region_raw", "zigr", null);
        R.R_set_altrep_Length_method(lifetime_region_class, shortRegionLength);
        R.R_set_altvec_Dataptr_or_null_method(lifetime_region_class, shortRegionDataptrOrNull);
        R.R_set_altraw_Elt_method(lifetime_region_class, shortRawRegionElt);
        R.R_set_altraw_Get_region_method(lifetime_region_class, lifetimeRegionGetRegion);
        lifetime_region_registered = true;
    }
    return R.R_new_altrep(lifetime_region_class, R.R_NilValue, R.R_NilValue);
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
    const data = [_]i32{
        R.R_NaInt, 4, 3, R.R_NaInt, 2, 1, 5, -1, -1, 8, 7, 8, 6, 5, 4, 3, 2, 1,
    };
    const vec = MyAltInt.init(data[0..]);
    if (zigr_convert.argminInt(vec) != 7) return R.Rf_ScalarReal(0.0);
    const missing = [_]i32{ R.R_NaInt, R.R_NaInt };
    if (zigr_convert.argminInt(MyAltInt.init(missing[0..])) != -1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altint_argmax_direct() SEXP {
    const data = [_]i32{
        R.R_NaInt, 4, 3, R.R_NaInt, 2, 1, 5, 9, 9, 8, 7, 8, 6, 5, 4, 3, 2, 1,
    };
    const vec = MyAltInt.init(data[0..]);
    if (zigr_convert.argmaxInt(vec) != 7) return R.Rf_ScalarReal(0.0);
    const missing = [_]i32{ R.R_NaInt, R.R_NaInt };
    if (zigr_convert.argmaxInt(MyAltInt.init(missing[0..])) != -1) return R.Rf_ScalarReal(0.0);
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
    const data = [_]i32{
        R.R_NaInt, 1, 1, R.R_NaInt, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    };
    const vec = MyAltLogical.init(data[0..]);
    if (zigr_convert.argminLogical(vec) != 7) return R.Rf_ScalarReal(0.0);
    const missing = [_]i32{ R.R_NaInt, R.R_NaInt };
    if (zigr_convert.argminLogical(MyAltLogical.init(missing[0..])) != -1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altlogical_argmax_direct() SEXP {
    const data = [_]i32{
        R.R_NaInt, 0, 0, R.R_NaInt, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    };
    const vec = MyAltLogical.init(data[0..]);
    if (zigr_convert.argmaxLogical(vec) != 7) return R.Rf_ScalarReal(0.0);
    const missing = [_]i32{ R.R_NaInt, R.R_NaInt };
    if (zigr_convert.argmaxLogical(MyAltLogical.init(missing[0..])) != -1) return R.Rf_ScalarReal(0.0);
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
    var data = [_]f64{
        0.0, 4.0, 3.0, std.math.nan(f64), 2.0, 1.0, 5.0, -1.0, -1.0,
        8.0, 7.0, 8.0, 6.0,               5.0, 4.0, 3.0, 2.0,  1.0,
    };
    data[0] = R.NA_REAL();
    const vec = MyAlt.init(data[0..]);
    if (zigr_convert.argmin(vec) != 7) return R.Rf_ScalarReal(0.0);
    var missing = [_]f64{ 0.0, std.math.nan(f64) };
    missing[0] = R.NA_REAL();
    if (zigr_convert.argmin(MyAlt.init(missing[0..])) != -1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_altrep_argmax_simd() SEXP {
    var data = [_]f64{
        0.0, 4.0, 3.0, std.math.nan(f64), 2.0, 1.0, 5.0, 9.0, 9.0,
        8.0, 7.0, 8.0, 6.0,               5.0, 4.0, 3.0, 2.0, 1.0,
    };
    data[0] = R.NA_REAL();
    const vec = MyAlt.init(data[0..]);
    if (zigr_convert.argmax(vec) != 7) return R.Rf_ScalarReal(0.0);
    var missing = [_]f64{ 0.0, std.math.nan(f64) };
    missing[0] = R.NA_REAL();
    if (zigr_convert.argmax(MyAlt.init(missing[0..])) != -1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_argminmax_missing_contract() SEXP {
    const real = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 5));
    const integer = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 5));
    const logical = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, 5));
    defer R.Rf_unprotect(3);

    const real_values = [_]f64{ R.NA_REAL(), 4.0, std.math.nan(f64), -1.0, 9.0 };
    const int_values = [_]i32{ R.R_NaInt, 4, -1, R.R_NaInt, 9 };
    const logical_values = [_]i32{ R.R_NaInt, 1, 0, R.R_NaInt, 1 };
    for (real_values, 0..) |value, i| R.REAL(real)[i] = value;
    for (int_values, 0..) |value, i| R.INTEGER(integer)[i] = value;
    for (logical_values, 0..) |value, i| R.LOGICAL(logical)[i] = value;

    if (zigr_convert.argmin(real) != 3 or zigr_convert.argmax(real) != 4) return R.Rf_ScalarReal(0.0);
    if (zigr_convert.argminInt(integer) != 2 or zigr_convert.argmaxInt(integer) != 4) return R.Rf_ScalarReal(0.0);
    if (zigr_convert.argminLogical(logical) != 2 or zigr_convert.argmaxLogical(logical) != 1) return R.Rf_ScalarReal(0.0);

    for (0..5) |i| {
        R.REAL(real)[i] = if (i % 2 == 0) R.NA_REAL() else std.math.nan(f64);
        R.INTEGER(integer)[i] = R.R_NaInt;
        R.LOGICAL(logical)[i] = R.R_NaInt;
    }
    if (zigr_convert.argmin(real) != -1 or zigr_convert.argmax(real) != -1) return R.Rf_ScalarReal(0.0);
    if (zigr_convert.argminInt(integer) != -1 or zigr_convert.argmaxInt(integer) != -1) return R.Rf_ScalarReal(0.0);
    if (zigr_convert.argminLogical(logical) != -1 or zigr_convert.argmaxLogical(logical) != -1) return R.Rf_ScalarReal(0.0);

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

fn lifetimeRegionBody() SEXP {
    var access = zigr_convert.toVectorAccessWithStrategy(
        u8,
        .one_pass,
        StringCleanupAllocator.allocator(),
        lifetime_region_input,
        .region,
    ) catch return R.R_NilValue;
    _ = access.next() catch return R.R_NilValue;
    access.deinit();
    return R.Rf_ScalarReal(0.0);
}

fn lifetimeRegionCall() SEXP {
    return cleanup.protectCall(lifetimeRegionBody);
}

export fn zigr_test_borrowed_lifetimes() SEXP {
    const real = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 3));
    defer R.Rf_unprotect(1);
    R.REAL(real)[0] = 1.0;
    R.REAL(real)[1] = 2.0;
    R.REAL(real)[2] = 3.0;
    var empty_storage: [0]u8 align(16) = .{};
    var empty_allocator = std.heap.FixedBufferAllocator.init(&empty_storage);
    var real_view = zigr_convert.toRealSliceView(empty_allocator.allocator(), real) catch return R.Rf_ScalarReal(0.0);
    defer real_view.deinit();
    switch (real_view) {
        .borrowed => {},
        .owned => return R.Rf_ScalarReal(0.0),
    }

    const raw = R.Rf_protect(R.Rf_allocVector(R.RAWSXP, 3));
    defer R.Rf_unprotect(1);
    R.RAW(raw)[0] = 1;
    R.RAW(raw)[1] = 2;
    R.RAW(raw)[2] = 3;
    var raw_view = zigr_convert.toRawSliceView(empty_allocator.allocator(), raw) catch return R.Rf_ScalarReal(0.0);
    defer raw_view.deinit();
    switch (raw_view) {
        .borrowed => {},
        .owned => return R.Rf_ScalarReal(0.0),
    }

    const strings = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 3));
    defer R.Rf_unprotect(1);
    R.SET_STRING_ELT(strings, 0, R.Rf_mkChar("borrowed"));
    R.SET_STRING_ELT(strings, 1, R.R_NaString);
    R.SET_STRING_ELT(strings, 2, R.Rf_mkChar("projection"));
    const string_view = zigr_convert.toStringSliceView(strings) catch return R.Rf_ScalarReal(0.0);
    const identity = zigr_convert.toStringProjectionView(.identity, strings) catch return R.Rf_ScalarReal(0.0);
    const missingness = zigr_convert.toStringProjectionView(.missingness, strings) catch return R.Rf_ScalarReal(0.0);
    const bytes = zigr_convert.toStringProjectionView(.bytes, strings) catch return R.Rf_ScalarReal(0.0);
    const translated = zigr_convert.toStringProjectionView(.translated_text, strings) catch return R.Rf_ScalarReal(0.0);
    const metadata = zigr_convert.toStringProjectionView(.metadata, strings) catch return R.Rf_ScalarReal(0.0);

    var cache_counting = mem.CountingAllocator.init(std.heap.page_allocator);
    var cached = zigr_convert.toCachedStringSliceView(cache_counting.allocator(), strings) catch return R.Rf_ScalarReal(0.0);
    var cached_active = true;
    defer if (cached_active) cached.deinit();
    if (cache_counting.stats.allocations != 1 or cache_counting.stats.live_bytes != 3 * @sizeOf(zigr_convert.StringView)) {
        return R.Rf_ScalarReal(0.0);
    }

    const before_gc = cleanup.diagnosticSnapshot();
    for (0..4) |_| {
        const noise = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 131_072));
        R.REAL(noise)[0] = 11.0;
        R.Rf_unprotect(1);
    }
    R.R_gc();

    if (!sameRestorationState(before_gc, cleanup.diagnosticSnapshot()) or
        !std.mem.eql(f64, real_view.constSlice(), &[_]f64{ 1.0, 2.0, 3.0 }) or
        !std.mem.eql(u8, raw_view.constSlice(), &[_]u8{ 1, 2, 3 })) return R.Rf_ScalarReal(0.0);
    if (!std.mem.eql(u8, string_view.at(0).bytes, "borrowed") or !string_view.at(1).is_na or
        identity.at(0).charsxp != R.STRING_ELT(strings, 0) or !missingness.at(1).is_na or
        !std.mem.eql(u8, bytes.at(0).bytes, "borrowed") or
        !std.mem.eql(u8, translated.at(2).bytes, "projection") or
        metadata.at(1).charsxp != R.R_NaString or !std.mem.eql(u8, cached.at(0).bytes, "borrowed"))
    {
        return R.Rf_ScalarReal(0.0);
    }

    const region = R.Rf_protect(lifetimeRegionRaw());
    defer R.Rf_unprotect(1);
    lifetime_region_input = region;
    defer lifetime_region_input = null;

    for ([_]u2{ 1, 2 }) |mode| {
        lifetime_region_mode = mode;
        string_cleanup_state = .{};
        const before = cleanup.diagnosticSnapshot();
        if (mode == 2) R_interrupts_pending = 1;
        const caught = trycatch_mod.tryCatch(lifetimeRegionCall);
        R_interrupts_pending = 0;
        if (caught) |_| return R.Rf_ScalarReal(0.0) else |_| {}
        if (string_cleanup_state.allocations != 1 or string_cleanup_state.frees != 1 or
            !sameRestorationState(before, cleanup.diagnosticSnapshot())) return R.Rf_ScalarReal(0.0);
    }
    lifetime_region_mode = 0;
    if (cache_counting.stats.frees != 0) return R.Rf_ScalarReal(0.0);
    cached.deinit();
    cached_active = false;
    if (cache_counting.stats.frees != 1 or cache_counting.stats.live_bytes != 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_compact_altrep_views() SEXP {
    const integer = compactIntSequence(@intCast(short_region_len)) orelse return R.Rf_ScalarReal(0.0);
    defer R.Rf_unprotect(1);
    if (R.ALTREP(integer) == 0 or R.INTEGER_OR_NULL(integer) != null) return R.Rf_ScalarReal(0.0);

    var int_storage: [short_region_len * @sizeOf(i32)]u8 align(16) = undefined;
    var int_fba = std.heap.FixedBufferAllocator.init(&int_storage);
    var int_view = zigr_convert.toIntSliceView(int_fba.allocator(), integer) catch return R.Rf_ScalarReal(0.0);
    var int_view_active = true;
    defer if (int_view_active) int_view.deinit();
    switch (int_view) {
        .borrowed => return R.Rf_ScalarReal(0.0),
        .owned => {},
    }
    const int_slice = int_view.constSlice();
    if (int_slice.len != short_region_len or int_slice[0] != 1 or int_slice[64] != 65 or int_slice[512] != 513 or int_slice[4096] != 4097) return R.Rf_ScalarReal(0.0);
    if (int_fba.end_index != int_storage.len) return R.Rf_ScalarReal(0.0);
    int_view.deinit();
    int_view_active = false;
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
    var real_view_active = true;
    defer if (real_view_active) real_view.deinit();
    switch (real_view) {
        .borrowed => return R.Rf_ScalarReal(0.0),
        .owned => {},
    }
    const real_slice = real_view.constSlice();
    if (real_slice.len != short_region_len or real_slice[0] != 1.0 or real_slice[64] != 65.0 or real_slice[512] != 513.0 or real_slice[4096] != 4097.0) return R.Rf_ScalarReal(0.0);
    if (real_fba.end_index != real_storage.len) return R.Rf_ScalarReal(0.0);
    real_view.deinit();
    real_view_active = false;
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
    var view_active = true;
    defer if (view_active) view.deinit();
    switch (view) {
        .borrowed => return R.Rf_ScalarReal(0.0),
        .owned => {},
    }
    const slice = view.constSlice();
    const expected_regions = (short_region_len + short_region_cap - 1) / short_region_cap;
    if (slice.len != short_region_len or slice[0] != 1 or slice[512] != 513 or slice[4096] != 4097) return R.Rf_ScalarReal(0.0);
    if (short_region_get_calls != expected_regions or short_region_elt_calls != 0 or fba.end_index != storage.len) return R.Rf_ScalarReal(0.0);
    view.deinit();
    view_active = false;
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
    var cached_active = true;
    defer if (cached_active) cached.deinit();
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
    cached_active = false;
    if (cache_fba.end_index != 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_string_projections() SEXP {
    const byte_marked = [_]u8{ 'x', 0xff, 'y' };
    const latin1 = [_]u8{ 'c', 'a', 'f', 0xe9 };
    const vector = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 5));
    defer R.Rf_unprotect(1);
    R.SET_STRING_ELT(vector, 0, R.Rf_mkChar("alpha"));
    R.SET_STRING_ELT(vector, 1, R.Rf_mkCharLenCE("", 0, @as(R.cetype_t, @intCast(R.CE_UTF8))));
    R.SET_STRING_ELT(vector, 2, R.R_NaString);
    R.SET_STRING_ELT(vector, 3, R.Rf_mkCharLenCE(@ptrCast(&byte_marked), @intCast(byte_marked.len), @as(R.cetype_t, @intCast(R.CE_BYTES))));
    R.SET_STRING_ELT(vector, 4, R.Rf_mkCharLenCE(@ptrCast(&latin1), @intCast(latin1.len), @as(R.cetype_t, @intCast(R.CE_LATIN1))));

    var missing_probe = zigr_convert.StringProjectionProbe{};
    const missing = zigr_convert.toStringProjectionViewWithProbe(.missingness, vector, &missing_probe) catch return R.Rf_ScalarReal(0.0);
    var missing_count: usize = 0;
    var missing_iterator = missing.iterator();
    while (missing_iterator.next()) |element| {
        if (element.is_na) missing_count += 1;
    }
    if (missing_count != 1 or missing_probe.requested != @as(?zigr_convert.StringProjection, .missingness) or
        missing_probe.elements != 5 or missing_probe.missingness_reads != 5 or missing_probe.identity_reads != 0 or
        missing_probe.byte_reads != 0 or missing_probe.encoding_reads != 0 or missing_probe.translation_reads != 0 or
        missing_probe.broader_work != 0)
    {
        return R.Rf_ScalarReal(0.0);
    }

    var identity_probe = zigr_convert.StringProjectionProbe{};
    const identity = zigr_convert.toStringProjectionViewWithProbe(.identity, vector, &identity_probe) catch return R.Rf_ScalarReal(0.0);
    if (identity.at(3).charsxp != R.STRING_ELT(vector, 3) or identity_probe.requested != @as(?zigr_convert.StringProjection, .identity) or
        identity_probe.identity_reads != 1 or identity_probe.byte_reads != 0 or identity_probe.encoding_reads != 0)
    {
        return R.Rf_ScalarReal(0.0);
    }

    var bytes_probe = zigr_convert.StringProjectionProbe{};
    const bytes = zigr_convert.toStringProjectionViewWithProbe(.bytes, vector, &bytes_probe) catch return R.Rf_ScalarReal(0.0);
    const raw = bytes.at(3);
    if (!std.mem.eql(u8, raw.bytes, &byte_marked) or raw.bytes.len != byte_marked.len or bytes_probe.byte_reads != 1 or
        bytes_probe.encoding_reads != 0 or bytes_probe.translation_reads != 0)
    {
        return R.Rf_ScalarReal(0.0);
    }

    var encoding_probe = zigr_convert.StringProjectionProbe{};
    const encoding = zigr_convert.toStringProjectionViewWithProbe(.encoding_mark, vector, &encoding_probe) catch return R.Rf_ScalarReal(0.0);
    if (encoding.at(4).encoding_mark != @as(R.cetype_t, @intCast(R.CE_LATIN1)) or encoding_probe.encoding_reads != 1 or
        encoding_probe.byte_reads != 0 or encoding_probe.translation_reads != 0)
    {
        return R.Rf_ScalarReal(0.0);
    }

    var translated_probe = zigr_convert.StringProjectionProbe{};
    const translated = zigr_convert.toStringProjectionViewWithProbe(.translated_text, vector, &translated_probe) catch return R.Rf_ScalarReal(0.0);
    if (!std.mem.eql(u8, translated.at(3).bytes, &byte_marked) or !std.mem.eql(u8, translated.at(4).bytes, "caf\xc3\xa9") or translated_probe.translation_reads != 1 or
        translated_probe.byte_reads != 1 or translated_probe.encoding_reads != 2 or translated_probe.broader_work != 0)
    {
        return R.Rf_ScalarReal(0.0);
    }

    var metadata_probe = zigr_convert.StringProjectionProbe{};
    const metadata = zigr_convert.toStringProjectionViewWithProbe(.metadata, vector, &metadata_probe) catch return R.Rf_ScalarReal(0.0);
    var raw_length: usize = 0;
    var metadata_iterator = metadata.iterator();
    while (metadata_iterator.next()) |element| {
        if (!element.is_na) raw_length += @intCast(R.XLENGTH(element.charsxp));
    }
    if (raw_length != 5 + byte_marked.len + latin1.len or metadata_probe.elements != 5 or metadata_probe.identity_reads != 5 or
        metadata_probe.missingness_reads != 5 or metadata_probe.encoding_reads != 4 or metadata_probe.byte_reads != 0 or
        metadata_probe.translation_reads != 0 or metadata_probe.broader_work != 0)
    {
        return R.Rf_ScalarReal(0.0);
    }

    const alt_data = [_][]const u8{ "alpha", "", "omega" };
    const altrep = R.Rf_protect(MyAltString.init(alt_data[0..]));
    defer R.Rf_unprotect(1);
    var altrep_probe = zigr_convert.StringProjectionProbe{};
    const altrep_missing = zigr_convert.toStringProjectionViewWithProbe(.missingness, altrep, &altrep_probe) catch return R.Rf_ScalarReal(0.0);
    var altrep_count: usize = 0;
    var altrep_iterator = altrep_missing.iterator();
    while (altrep_iterator.next()) |element| {
        if (!element.is_na) altrep_count += 1;
    }
    if (altrep_count != alt_data.len or altrep_probe.requested != @as(?zigr_convert.StringProjection, .missingness) or
        altrep_probe.byte_reads != 0 or altrep_probe.encoding_reads != 0 or altrep_probe.translation_reads != 0 or
        altrep_probe.broader_work != 0)
    {
        return R.Rf_ScalarReal(0.0);
    }
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
    string_cleanup_elt_calls = 0;
    var input = protect.scoped(stringErrorAltString());
    defer input.deinit();
    factor_error_input = input.get();
    defer factor_error_input = null;

    // Exceed the unwind-boundary capacity across separate calls. If a caught
    // longjmp leaks a boundary, a later call fails before reaching ALTSTRING
    // Elt and the exact per-attempt callback count catches it. The final
    // attempt follows forced GC to prove that the protected input remains live.
    for (0..cleanup.MAX_NESTING + 2) |attempt| {
        if (attempt == cleanup.MAX_NESTING + 1) R.R_gc();
        if (trycatch_mod.tryCatch(factorErrorCall)) |_| {
            return R.Rf_ScalarReal(0.0);
        } else |_| {}
        if (string_cleanup_elt_calls != attempt + 1) return R.Rf_ScalarReal(0.0);
        if (cleanup.diagnosticSnapshot().enabled and protect.getDepth() != initial_depth + 1) {
            return R.Rf_ScalarReal(0.0);
        }
    }
    var fresh_input = protect.scoped(R.Rf_allocVector(R.STRSXP, 3));
    defer fresh_input.deinit();
    R.SET_STRING_ELT(fresh_input.get(), 0, R.Rf_mkChar("b"));
    R.SET_STRING_ELT(fresh_input.get(), 1, R.Rf_mkChar("a"));
    R.SET_STRING_ELT(fresh_input.get(), 2, R.Rf_mkChar("b"));
    var fresh = protect.scoped(factor.asFactorChecked(fresh_input.get()) catch return R.Rf_ScalarReal(0.0));
    defer fresh.deinit();
    if (R.TYPEOF(fresh.get()) != R.INTSXP or R.XLENGTH(fresh.get()) != 3) {
        return R.Rf_ScalarReal(0.0);
    }
    const fresh_levels = R.Rf_getAttrib(fresh.get(), R.R_LevelsSymbol);
    const fresh_codes = R.INTEGER(fresh.get());
    if (fresh_codes[0] != 2 or fresh_codes[1] != 1 or fresh_codes[2] != 2 or
        R.Rf_inherits(fresh.get(), "factor") == 0 or R.TYPEOF(fresh_levels) != R.STRSXP or
        R.XLENGTH(fresh_levels) != 2 or R.STRING_ELT(fresh_levels, 0) != R.Rf_mkChar("a") or
        R.STRING_ELT(fresh_levels, 1) != R.Rf_mkChar("b"))
    {
        return R.Rf_ScalarReal(0.0);
    }
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

fn conversionErrorRealRegion(_: SEXP, _: R.R_xlen_t, _: R.R_xlen_t, _: [*c]f64) callconv(.c) R.R_xlen_t {
    R.Rf_error("expected real conversion error");
}

fn conversionErrorInteger(_: SEXP, _: R.R_xlen_t) callconv(.c) c_int {
    R.Rf_error("expected integer conversion error");
}

fn conversionErrorIntegerRegion(_: SEXP, _: R.R_xlen_t, _: R.R_xlen_t, _: [*c]c_int) callconv(.c) R.R_xlen_t {
    R.Rf_error("expected integer conversion error");
}

fn conversionErrorLogical(_: SEXP, _: R.R_xlen_t) callconv(.c) c_int {
    R.Rf_error("expected logical conversion error");
}

fn conversionErrorLogicalRegion(_: SEXP, _: R.R_xlen_t, _: R.R_xlen_t, _: [*c]c_int) callconv(.c) R.R_xlen_t {
    R.Rf_error("expected logical conversion error");
}

fn conversionErrorRaw(_: SEXP, _: R.R_xlen_t) callconv(.c) R.Rbyte {
    R.Rf_error("expected raw conversion error");
}

fn conversionErrorRawRegion(_: SEXP, _: R.R_xlen_t, _: R.R_xlen_t, _: [*c]R.Rbyte) callconv(.c) R.R_xlen_t {
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
        for (conversion_error_classes) |class| R.R_set_altvec_Dataptr_or_null_method(class, shortRegionDataptrOrNull);
        R.R_set_altreal_Get_region_method(conversion_error_classes[0], conversionErrorRealRegion);
        R.R_set_altinteger_Get_region_method(conversion_error_classes[1], conversionErrorIntegerRegion);
        R.R_set_altlogical_Get_region_method(conversion_error_classes[2], conversionErrorLogicalRegion);
        R.R_set_altraw_Get_region_method(conversion_error_classes[3], conversionErrorRawRegion);
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
        0 => _ = zigr_convert.toRealSliceView(allocator, conversion_cleanup_input) catch return R.R_NilValue,
        1 => _ = zigr_convert.toIntSliceView(allocator, conversion_cleanup_input) catch return R.R_NilValue,
        2 => _ = zigr_convert.toLogicalSliceView(allocator, conversion_cleanup_input) catch return R.R_NilValue,
        3 => _ = zigr_convert.toRawSliceView(allocator, conversion_cleanup_input) catch return R.R_NilValue,
        4 => _ = zigr_convert.toComplexSliceView(allocator, conversion_cleanup_input) catch return R.R_NilValue,
        5 => _ = zigr_convert.toListSlice(allocator, conversion_cleanup_input) catch return R.R_NilValue,
        else => unreachable,
    }
    R.Rf_error("conversion unexpectedly returned");
}

threadlocal var conversion_view_cleanup_input: SEXP = null;
threadlocal var conversion_view_cleanup_mode: u3 = 0;

fn conversionViewAllocationThenError() SEXP {
    const allocator = StringCleanupAllocator.allocator();
    switch (conversion_view_cleanup_mode) {
        0 => _ = zigr_convert.toRealSliceView(allocator, conversion_view_cleanup_input) catch return R.R_NilValue,
        1 => _ = zigr_convert.toIntSliceView(allocator, conversion_view_cleanup_input) catch return R.R_NilValue,
        2 => _ = zigr_convert.toLogicalSliceView(allocator, conversion_view_cleanup_input) catch return R.R_NilValue,
        3 => _ = zigr_convert.toRawSliceView(allocator, conversion_view_cleanup_input) catch return R.R_NilValue,
        4 => _ = zigr_convert.toComplexSliceView(allocator, conversion_view_cleanup_input) catch return R.R_NilValue,
        5 => _ = zigr_convert.toCachedStringSliceView(allocator, conversion_view_cleanup_input) catch return R.R_NilValue,
        else => unreachable,
    }
    R.Rf_error("view conversion unexpectedly returned");
}

export fn zigr_test_view_allocation_longjmp() SEXP {
    for (0..6) |mode| {
        const input = R.Rf_protect(if (mode == 5) stringErrorAltString() else conversionErrorInput(mode));
        conversion_view_cleanup_input = input;
        conversion_view_cleanup_mode = @intCast(mode);
        string_cleanup_state = .{};

        const caught = trycatch_mod.tryCatch(struct {
            fn call() SEXP {
                return cleanup.protectCall(conversionViewAllocationThenError);
            }
        }.call);
        R.Rf_unprotect(1);
        conversion_view_cleanup_input = null;
        if (caught) |_| return R.Rf_ScalarReal(@floatFromInt(100 + mode)) else |_| {}
        if (string_cleanup_state.allocations != 1 or string_cleanup_state.frees != 1) return R.Rf_ScalarReal(@floatFromInt(200 + mode * 10 + string_cleanup_state.allocations * 2 + string_cleanup_state.frees));
    }
    return R.Rf_ScalarReal(1.0);
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

threadlocal var native_capacity_input: SEXP = null;
threadlocal var native_capacity_mode: u4 = 0;

fn nativeCleanupCapacityCall() SEXP {
    for (0..cleanup.MAX_NESTING) |_| cleanup.pushFrame(ignoreCleanup, null);

    const allocator = StringCleanupAllocator.allocator();
    switch (native_capacity_mode) {
        0 => _ = zigr_convert.toRealSlice(allocator, native_capacity_input) catch return R.R_NilValue,
        1 => _ = zigr_convert.toIntSlice(allocator, native_capacity_input) catch return R.R_NilValue,
        2 => _ = zigr_convert.toLogicalSlice(allocator, native_capacity_input) catch return R.R_NilValue,
        3 => _ = zigr_convert.toRawSlice(allocator, native_capacity_input) catch return R.R_NilValue,
        4 => _ = zigr_convert.toComplexSlice(allocator, native_capacity_input) catch return R.R_NilValue,
        5 => _ = zigr_convert.toListSlice(allocator, native_capacity_input) catch return R.R_NilValue,
        6 => _ = zigr_convert.toStringSlice(allocator, native_capacity_input) catch return R.R_NilValue,
        7 => _ = zigr_convert.toStringSliceNullable(allocator, native_capacity_input) catch return R.R_NilValue,
        8 => _ = zigr_convert.toCachedStringSliceView(allocator, native_capacity_input) catch return R.R_NilValue,
        9 => {
            var access = zigr_convert.toVectorAccessWithStrategy(
                i32,
                .one_pass,
                allocator,
                native_capacity_input,
                .region,
            ) catch return R.R_NilValue;
            access.deinit();
        },
        10 => {
            var access = zigr_convert.toVectorAccessWithStrategy(
                i32,
                .random_access,
                allocator,
                native_capacity_input,
                .materialized,
            ) catch return R.R_NilValue;
            access.deinit();
        },
        11 => _ = serialize_mod.toVector(native_capacity_input),
        else => unreachable,
    }
    R.Rf_error("native cleanup capacity unexpectedly returned");
}

fn nativeCleanupNormalRetry(allocator: std.mem.Allocator) bool {
    switch (native_capacity_mode) {
        0 => {
            const slice = zigr_convert.toRealSlice(allocator, native_capacity_input) catch return false;
            allocator.free(slice);
        },
        1 => {
            const slice = zigr_convert.toIntSlice(allocator, native_capacity_input) catch return false;
            allocator.free(slice);
        },
        2 => {
            const slice = zigr_convert.toLogicalSlice(allocator, native_capacity_input) catch return false;
            allocator.free(slice);
        },
        3 => {
            const slice = zigr_convert.toRawSlice(allocator, native_capacity_input) catch return false;
            allocator.free(slice);
        },
        4 => {
            const slice = zigr_convert.toComplexSlice(allocator, native_capacity_input) catch return false;
            allocator.free(slice);
        },
        5 => {
            const slice = zigr_convert.toListSlice(allocator, native_capacity_input) catch return false;
            allocator.free(slice);
        },
        6 => {
            const slice = zigr_convert.toStringSlice(allocator, native_capacity_input) catch return false;
            allocator.free(slice);
        },
        7 => {
            const slice = zigr_convert.toStringSliceNullable(allocator, native_capacity_input) catch return false;
            allocator.free(slice);
        },
        8 => {
            var view = zigr_convert.toCachedStringSliceView(allocator, native_capacity_input) catch return false;
            view.deinit();
        },
        9 => {
            var access = zigr_convert.toVectorAccessWithStrategy(
                i32,
                .one_pass,
                allocator,
                native_capacity_input,
                .region,
            ) catch return false;
            access.deinit();
        },
        10 => {
            var access = zigr_convert.toVectorAccessWithStrategy(
                i32,
                .random_access,
                allocator,
                native_capacity_input,
                .materialized,
            ) catch return false;
            access.deinit();
        },
        11 => unreachable,
        else => unreachable,
    }
    return true;
}

export fn zigr_test_native_cleanup_capacity() SEXP {
    defer native_capacity_input = null;

    var real = protect.scoped(R.Rf_allocVector(R.REALSXP, 3));
    defer real.deinit();
    R.REAL(real.get())[0] = 1.0;
    R.REAL(real.get())[1] = 2.0;
    R.REAL(real.get())[2] = 3.0;

    var integer = protect.scoped(R.Rf_allocVector(R.INTSXP, 3));
    defer integer.deinit();
    R.INTEGER(integer.get())[0] = 1;
    R.INTEGER(integer.get())[1] = 2;
    R.INTEGER(integer.get())[2] = 3;

    var logical = protect.scoped(R.Rf_allocVector(R.LGLSXP, 3));
    defer logical.deinit();
    R.LOGICAL(logical.get())[0] = 0;
    R.LOGICAL(logical.get())[1] = 1;
    R.LOGICAL(logical.get())[2] = R.R_NaInt;

    var raw = protect.scoped(R.Rf_allocVector(R.RAWSXP, 3));
    defer raw.deinit();
    R.RAW(raw.get())[0] = 1;
    R.RAW(raw.get())[1] = 2;
    R.RAW(raw.get())[2] = 3;

    var complex = protect.scoped(R.Rf_allocVector(R.CPLXSXP, 1));
    defer complex.deinit();
    const complex_ptr = R.COMPLEX(complex.get()) orelse return R.Rf_ScalarReal(0.0);
    const complex_values: [*]zigr_convert.Rcomplex = @ptrCast(@alignCast(complex_ptr));
    complex_values[0] = .{ .r = 1.0, .i = -1.0 };

    var list = protect.scoped(R.Rf_allocVector(R.VECSXP, 1));
    defer list.deinit();

    var strings = protect.scoped(R.Rf_allocVector(R.STRSXP, 2));
    defer strings.deinit();
    R.SET_STRING_ELT(strings.get(), 0, R.Rf_mkChar("one"));
    R.SET_STRING_ELT(strings.get(), 1, R.Rf_mkChar("two"));

    var region = protect.scoped(shortRegionAltInteger());
    defer region.deinit();
    if (R.INTEGER_OR_NULL(region.get()) != null) return R.Rf_ScalarReal(0.0);

    const entry = cleanup.diagnosticSnapshot();
    const inputs = [_]SEXP{
        real.get(),
        integer.get(),
        logical.get(),
        raw.get(),
        complex.get(),
        list.get(),
        strings.get(),
        strings.get(),
        strings.get(),
        region.get(),
        region.get(),
        real.get(),
    };

    for (inputs, 0..) |input, mode| {
        native_capacity_input = input;
        native_capacity_mode = @intCast(mode);
        string_cleanup_state = .{};
        const caught = trycatch_mod.tryCatch(struct {
            fn call() SEXP {
                return cleanup.protectCall(nativeCleanupCapacityCall);
            }
        }.call);
        if (caught) |_| return R.Rf_ScalarReal(0.0) else |_| {}
        if (string_cleanup_state.allocations != 0 or string_cleanup_state.frees != 0 or
            !sameRestorationState(entry, cleanup.diagnosticSnapshot())) return R.Rf_ScalarReal(0.0);

        if (mode < 11) {
            string_cleanup_state = .{};
            if (!nativeCleanupNormalRetry(StringCleanupAllocator.allocator()) or
                string_cleanup_state.allocations != 1 or string_cleanup_state.frees != 1 or
                !sameRestorationState(entry, cleanup.diagnosticSnapshot()))
            {
                return R.Rf_ScalarReal(0.0);
            }
        } else {
            var serialized = protect.scoped(serialize_mod.toVector(native_capacity_input));
            const serialized_ok = R.TYPEOF(serialized.get()) == R.RAWSXP;
            serialized.deinit();
            if (!serialized_ok or !sameRestorationState(entry, cleanup.diagnosticSnapshot())) {
                return R.Rf_ScalarReal(0.0);
            }
        }
    }
    var retry_counting = mem.CountingAllocator.init(std.heap.page_allocator);
    const retry = zigr_convert.toRealSlice(retry_counting.allocator(), real.get()) catch return R.Rf_ScalarReal(0.0);
    if (retry_counting.stats.allocations != 1 or retry_counting.stats.live_bytes != 3 * @sizeOf(f64)) {
        return R.Rf_ScalarReal(0.0);
    }
    retry_counting.allocator().free(retry);
    if (retry_counting.stats.frees != 1 or retry_counting.stats.live_bytes != 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_native_allocator_failure() SEXP {
    var real = protect.scoped(R.Rf_allocVector(R.REALSXP, 3));
    defer real.deinit();
    R.REAL(real.get())[0] = 1.0;
    R.REAL(real.get())[1] = 2.0;
    R.REAL(real.get())[2] = 3.0;

    const values = [_]i32{ 4, 5, 6 };
    var integer = protect.scoped(@as(SEXP, @ptrCast(zigr_convert.fromIntSlice(values[0..]))));
    defer integer.deinit();
    const names = [_][]const u8{ "left", "right" };
    const columns = [_]SEXP{ real.get(), integer.get() };
    var frame = protect.scoped(df.buildChecked(names[0..], columns[0..]) catch return R.Rf_ScalarReal(0.0));
    defer frame.deinit();
    const wrapped = df.DataFrame.wrap(frame.get()) orelse return R.Rf_ScalarReal(0.0);
    const entry = cleanup.diagnosticSnapshot();

    var conversion_storage: [1]u8 = undefined;
    var conversion_fixed = std.heap.FixedBufferAllocator.init(&conversion_storage);
    if (zigr_convert.toRealSlice(conversion_fixed.allocator(), real.get())) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |conversion_error| {
        if (conversion_error != error.OutOfMemory) return R.Rf_ScalarReal(0.0);
    }
    if (!sameRestorationState(entry, cleanup.diagnosticSnapshot())) return R.Rf_ScalarReal(0.0);

    var header_storage: [1]u8 = undefined;
    var header_fixed = std.heap.FixedBufferAllocator.init(&header_storage);
    if (wrapped.columnNames(header_fixed.allocator())) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |header_error| {
        if (header_error != error.OutOfMemory) return R.Rf_ScalarReal(0.0);
    }
    if (!sameRestorationState(entry, cleanup.diagnosticSnapshot())) return R.Rf_ScalarReal(0.0);

    var map_storage: [1]u8 = undefined;
    var map_fixed = std.heap.FixedBufferAllocator.init(&map_storage);
    if (wrapped.columnMap(map_fixed.allocator())) |map| {
        var owned_map = map;
        owned_map.deinit();
        return R.Rf_ScalarReal(0.0);
    } else |map_error| {
        if (map_error != error.OutOfMemory) return R.Rf_ScalarReal(0.0);
    }
    if (!sameRestorationState(entry, cleanup.diagnosticSnapshot())) return R.Rf_ScalarReal(0.0);
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

fn embedCleanupCapacityCall() SEXP {
    for (0..cleanup.MAX_NESTING) |_| cleanup.pushFrame(ignoreCleanup, null);
    return embed.rCodeEval("1 + 1", null);
}

fn embedCleanupCapacityBoundary() SEXP {
    return cleanup.protectCall(embedCleanupCapacityCall);
}

export fn zigr_test_embed_cleanup_capacity() SEXP {
    const entry = cleanup.diagnosticSnapshot();
    if (trycatch_mod.tryCatch(embedCleanupCapacityBoundary)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    if (!sameRestorationState(entry, cleanup.diagnosticSnapshot())) return R.Rf_ScalarReal(0.0);

    const result = embed.rCodeEval("1 + 1", null);
    if (R.TYPEOF(result) != R.REALSXP or R.REAL(result)[0] != 2.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
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

    var cached = zigr_convert.toCachedStringSliceView(arena.allocator(), vec) catch return R.Rf_ScalarReal(0.0);
    defer cached.deinit();
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

fn logicalCountCode(values: zigr_convert.LogicalSliceView) i32 {
    var counts = [_]i32{ 0, 0, 0 };
    for (values.constSlice()) |value| {
        if (value == R.R_NaInt) counts[2] += 1 else if (value == 1) counts[1] += 1 else counts[0] += 1;
    }
    return counts[0] * 100 + counts[1] * 10 + counts[2];
}

const generated_logical_result_values = [_]i32{ 0, 1, std.math.minInt(i32) };

fn generatedLogicalResult() zigr_convert.LogicalSlice {
    return .{ .data = generated_logical_result_values[0..] };
}

const ExternalExports = zigr.@"export".generateExports(&.{}, &.{
    .{ .name = "zigr_test_external_sum", .func = externalSum },
    .{ .name = "zigr_test_external_logical", .func = logicalCountCode },
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

fn spillThenAllocate(values: zigr_convert.IntegerSliceView) R.SEXP {
    var view = values;
    defer view.deinit();
    const data = view.constSlice();
    const result = R.Rf_allocVector(R.REALSXP, 2);
    R.REAL(result)[0] = @floatFromInt(data[0]);
    R.REAL(result)[1] = @floatFromInt(data[data.len - 1]);
    return result;
}

fn arenaVectorOutput(_: f64) []const f64 {
    return arena_vector_output[0..];
}

fn directRealResult(value: R.SEXP) R.SEXP {
    var result = zigr_convert.ResultBuilder(f64).initFromInput(value) catch |build_err| zigr_convert.signalError(build_err);
    defer result.deinit();
    const input = zigr_convert.dataPtr(f64, value) orelse zigr.@"error".signal("numeric input data unavailable");
    const output = result.mutableSlice();
    for (input[0..output.len], output) |source, *destination| destination.* = source * 2.0;
    return result.finish();
}

fn directRawResult(value: R.SEXP) R.SEXP {
    var result = zigr_convert.ResultBuilder(u8).initFromInput(value) catch |build_err| zigr_convert.signalError(build_err);
    defer result.deinit();
    const input = zigr_convert.dataPtr(u8, value) orelse zigr.@"error".signal("raw input data unavailable");
    const output = result.mutableSlice();
    @memcpy(output, input[0..output.len]);
    return result.finish();
}

fn directResultThenError(value: R.SEXP) R.SEXP {
    var result = zigr_convert.ResultBuilder(f64).initFromInput(value) catch |build_err| zigr_convert.signalError(build_err);
    defer result.deinit();
    const output = result.mutableSlice();
    if (output.len != 0) output[0] = 99.0;
    R.Rf_error("zigr direct result: expected error after partial fill");
}

fn directResultOversized() R.SEXP {
    var result = zigr_convert.ResultBuilder(f64).init(@intCast(R.R_XLEN_T_MAX));
    return result.finish();
}

fn directResultThenInterrupt(value: R.SEXP) R.SEXP {
    var result = zigr_convert.ResultBuilder(f64).initFromInput(value) catch |build_err| zigr_convert.signalError(build_err);
    defer result.deinit();
    const output = result.mutableSlice();
    if (output.len != 0) output[0] = 101.0;
    ict.checkInterrupt();
    return result.finish();
}

fn spillThenError(values: zigr_convert.IntegerSliceView) void {
    var view = values;
    defer view.deinit();
    R.Rf_error("zigr generated spill: expected error");
}

fn generatedAltrepOnePassSum(values: zigr_convert.VectorAccess(i32, .one_pass)) f64 {
    var access = values;
    defer access.deinit();
    var total: f64 = 0.0;
    while (access.next() catch |access_err| zigr_convert.signalError(access_err)) |chunk| {
        for (chunk) |value| total += @floatFromInt(value);
    }
    return total;
}

fn generatedAltrepMaterialized(values: zigr_convert.VectorAccess(i32, .random_access)) []const i32 {
    return values.contiguousSlice() orelse unreachable;
}

fn generatedAltrepOnePassRaw(values: zigr_convert.VectorAccess(u8, .one_pass)) void {
    var access = values;
    defer access.deinit();
    while (access.next() catch |access_err| zigr_convert.signalError(access_err)) |chunk| {
        _ = chunk;
    }
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
    .{ .name = "zigr_altrep_one_pass_sum", .func = generatedAltrepOnePassSum },
    .{ .name = "zigr_altrep_materialized", .func = generatedAltrepMaterialized },
    .{ .name = "zigr_altrep_one_pass_raw", .func = generatedAltrepOnePassRaw },
    .{ .name = "zigr_direct_real_result", .func = directRealResult },
    .{ .name = "zigr_direct_raw_result", .func = directRawResult },
    .{ .name = "zigr_direct_result_error", .func = directResultThenError },
    .{ .name = "zigr_direct_result_oversized", .func = directResultOversized },
    .{ .name = "zigr_direct_result_interrupt", .func = directResultThenInterrupt },
}, &.{});

const ArenaCall = *const fn (R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP) callconv(.c) R.SEXP;
var generated_failure_input: SEXP = undefined;

fn arenaCall(index: usize, arg: SEXP) SEXP {
    const fun: ArenaCall = @ptrCast(@alignCast(ArenaExports.call_defs[index].fun));
    return fun(arg, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue);
}

fn generatedFailureCall() SEXP {
    return arenaCall(9, generated_failure_input);
}

fn initArenaExports() bool {
    const dll = test_dll orelse (R.R_getEmbeddingDllInfo() orelse return false);
    ArenaExports.init(dll);
    return true;
}

const SemanticSchema = struct {
    id: i32,
    count: i32,
    ratio: f64,
    enabled: bool,
};

fn generatedObjectListSummary(value: R.SEXP) i32 {
    var arena = zigr.memory.UnwindArena.init();
    defer arena.deinit();
    const items = zigr_convert.toListSlice(arena.allocator(), value) catch |conversion_error|
        zigr_convert.signalError(conversion_error);
    if (items.len != 3) return -1;

    const names = R.Rf_getAttrib(value, R.R_NamesSymbol);
    if (names == R.R_NilValue or R.TYPEOF(names) != R.STRSXP or R.XLENGTH(names) != 3 or
        !charsxpEqualsBytes(R.STRING_ELT(names, 0), "first") or
        !charsxpEqualsBytes(R.STRING_ELT(names, 1), "nested") or
        !charsxpEqualsBytes(R.STRING_ELT(names, 2), "empty")) return -2;
    const first_value = zigr_convert.toRealScalar(items[0]) catch |conversion_error|
        zigr_convert.signalError(conversion_error);
    if (first_value != 1.5) return -3;

    const nested = zigr_convert.toListSlice(arena.allocator(), items[1]) catch |conversion_error|
        zigr_convert.signalError(conversion_error);
    const nested_value = zigr_convert.toIntScalar(nested[1]) catch |conversion_error|
        zigr_convert.signalError(conversion_error);
    if (nested.len != 2 or nested[0] != R.R_NilValue or nested_value != 2 or items[2] != R.R_NilValue) return -4;
    return 1;
}

fn generatedObjectSchema(value: R.SEXP) R.SEXP {
    var arena = zigr.memory.UnwindArena.init();
    defer arena.deinit();
    const parsed = zigr_convert.fromSEXP(SemanticSchema, value, arena.allocator());
    return zigr_convert.asSEXP(parsed);
}

fn generatedObjectAttributes(value: R.SEXP) R.SEXP {
    var result = protect.scoped(R.Rf_duplicate(value));
    defer result.deinit();
    attrib.setClass(result.get(), "zigr_semantic");
    var creator = protect.scoped(R.Rf_mkString("runtime"));
    defer creator.deinit();
    attrib.setAttrib(result.get(), test_lang.symbol("creator"), creator.get());
    attrib.setDim(result.get(), &.{ 3, 1 });
    return result.get();
}

fn generatedObjectFactor(value: R.SEXP) R.SEXP {
    return factor.asFactorChecked(value) catch |factor_error| zigr_convert.signalError(factor_error);
}

fn generatedObjectDataFrame(value: R.SEXP) R.SEXP {
    if (R.TYPEOF(value) != R.VECSXP or R.XLENGTH(value) != 2) zigr.@"error".signal("list of two columns expected");
    const columns = [_]R.SEXP{ R.VECTOR_ELT(value, 0), R.VECTOR_ELT(value, 1) };
    return df.buildChecked(&.{ "left", "right" }, columns[0..]) catch |data_frame_error|
        zigr.@"error".signal(@errorName(data_frame_error));
}

fn generatedObjectS4(value: R.SEXP) R.SEXP {
    var object = protect.scoped(s4.newObjectChecked("ZigrS4Contract") catch |s4_error|
        zigr.@"error".signal(@errorName(s4_error)));
    defer object.deinit();
    return s4.setSlotChecked(object.get(), "value", value) catch |s4_error|
        zigr.@"error".signal(@errorName(s4_error));
}

const ObjectExports = zigr.@"export".generateExports(&.{
    .{ .name = "zigr_object_list_summary", .func = generatedObjectListSummary },
    .{ .name = "zigr_object_schema", .func = generatedObjectSchema },
    .{ .name = "zigr_object_attributes", .func = generatedObjectAttributes },
    .{ .name = "zigr_object_factor", .func = generatedObjectFactor },
    .{ .name = "zigr_object_data_frame", .func = generatedObjectDataFrame },
    .{ .name = "zigr_object_s4", .func = generatedObjectS4 },
}, &.{});

const ObjectCall = *const fn (R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP) callconv(.c) R.SEXP;
threadlocal var object_error_input: SEXP = null;

fn objectCall(index: usize, arg: SEXP) SEXP {
    const fun: ObjectCall = @ptrCast(@alignCast(ObjectExports.call_defs[index].fun));
    return fun(arg, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue);
}

fn objectListWrongTypeCall() SEXP {
    return objectCall(0, object_error_input);
}

fn initObjectExports() bool {
    const dll = test_dll orelse (R.R_getEmbeddingDllInfo() orelse return false);
    ObjectExports.init(dll);
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

fn generatedStringIdentity(values: zigr_convert.StringIdentityView) i32 {
    var missing: i32 = 0;
    var iterator = values.iterator();
    while (iterator.next()) |value| {
        if (value.charsxp == R.R_NaString) missing += 1;
    }
    return missing;
}

fn generatedStringMissingness(values: zigr_convert.StringMissingnessView) i32 {
    var present: i32 = 0;
    var iterator = values.iterator();
    while (iterator.next()) |value| {
        if (!value.is_na) present += 1;
    }
    return present;
}

fn generatedStringBytes(values: zigr_convert.StringBytesView) i32 {
    var total: i32 = 0;
    var iterator = values.iterator();
    while (iterator.next()) |value| {
        if (value.charsxp != R.R_NaString) total += @intCast(value.bytes.len);
    }
    return total;
}

fn generatedStringEncoding(values: zigr_convert.StringEncodingView) i32 {
    var utf8: i32 = 0;
    var latin1: i32 = 0;
    var bytes: i32 = 0;
    var iterator = values.iterator();
    while (iterator.next()) |value| {
        if (value.charsxp == R.R_NaString) continue;
        if (value.encoding_mark == @as(R.cetype_t, @intCast(R.CE_UTF8))) {
            utf8 += 1;
        } else if (value.encoding_mark == @as(R.cetype_t, @intCast(R.CE_LATIN1))) {
            latin1 += 1;
        } else if (value.encoding_mark == @as(R.cetype_t, @intCast(R.CE_BYTES))) {
            bytes += 1;
        }
    }
    return utf8 * 100 + latin1 * 10 + bytes;
}

fn generatedStringTranslated(values: zigr_convert.StringTranslatedTextView) i32 {
    var total: i32 = 0;
    var iterator = values.iterator();
    while (iterator.next()) |value| {
        if (value.charsxp != R.R_NaString) total += @intCast(value.bytes.len);
    }
    return total;
}

fn generatedStringMetadata(values: zigr_convert.StringMetadataView) i32 {
    var raw_length: i32 = 0;
    var missing: i32 = 0;
    var utf8: i32 = 0;
    var latin1: i32 = 0;
    var bytes: i32 = 0;
    var iterator = values.iterator();
    while (iterator.next()) |value| {
        if (value.is_na) {
            missing += 1;
            continue;
        }
        raw_length += @intCast(R.XLENGTH(value.charsxp));
        if (value.encoding_mark == @as(R.cetype_t, @intCast(R.CE_UTF8))) {
            utf8 += 1;
        } else if (value.encoding_mark == @as(R.cetype_t, @intCast(R.CE_LATIN1))) {
            latin1 += 1;
        } else if (value.encoding_mark == @as(R.cetype_t, @intCast(R.CE_BYTES))) {
            bytes += 1;
        }
    }
    return raw_length + missing * 1000 + utf8 * 100 + latin1 * 10 + bytes;
}

fn rawViewSum(values: zigr_convert.RawSliceView) i32 {
    var view = values;
    defer view.deinit();
    var total: i32 = 0;
    for (view.constSlice()) |value| total += @intCast(value);
    return total;
}

fn realViewRepresentation(values: zigr_convert.RealSliceView) i32 {
    var view = values;
    defer view.deinit();
    return switch (view) {
        .borrowed => 1,
        .owned => 2,
    };
}

fn integerViewRepresentation(values: zigr_convert.IntegerSliceView) i32 {
    var view = values;
    defer view.deinit();
    return switch (view) {
        .borrowed => 1,
        .owned => 2,
    };
}

fn complexViewRepresentation(values: zigr_convert.ComplexSliceView) i32 {
    var view = values;
    defer view.deinit();
    return switch (view) {
        .borrowed => 1,
        .owned => 2,
    };
}

fn rawViewEcho(values: zigr_convert.RawSliceView) []const u8 {
    return values.constSlice();
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
    .{ .name = "zigr_logical_count_code", .func = logicalCountCode },
    .{ .name = "zigr_logical_result", .func = generatedLogicalResult },
    .{ .name = "zigr_real_view_representation", .func = realViewRepresentation },
    .{ .name = "zigr_integer_view_representation", .func = integerViewRepresentation },
    .{ .name = "zigr_complex_view_representation", .func = complexViewRepresentation },
    .{ .name = "zigr_raw_view_echo", .func = rawViewEcho },
    .{ .name = "zigr_string_projection_identity", .func = generatedStringIdentity },
    .{ .name = "zigr_string_projection_missingness", .func = generatedStringMissingness },
    .{ .name = "zigr_string_projection_bytes", .func = generatedStringBytes },
    .{ .name = "zigr_string_projection_encoding", .func = generatedStringEncoding },
    .{ .name = "zigr_string_projection_translated", .func = generatedStringTranslated },
    .{ .name = "zigr_string_projection_metadata", .func = generatedStringMetadata },
}, &.{});

const RuntimeCall = *const fn (R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP) callconv(.c) R.SEXP;

threadlocal var runtime_rng_mode: i32 = 0;
threadlocal var runtime_rng_count: i32 = 0;

fn runtimeLanguage(value: SEXP) SEXP {
    var envir = protect.scoped(R.R_NewEnv(R.R_BaseEnv, 1, 29));
    defer envir.deinit();
    var function = protect.scoped(embed.rCodeEval("function(x, scale=3) x * scale", R.R_BaseEnv));
    defer function.deinit();

    const bound_name = test_lang.symbol("runtime_bound");
    test_eval.defineVarIn("runtime_bound", value, envir.get());
    test_eval.setVar(bound_name, value, envir.get());
    if (test_eval.findVarInFrame(envir.get(), "runtime_bound") != value) {
        err.signal("runtime binding lookup changed the bound value");
    }
    test_eval.defineVarIn("runtime_worker", function.get(), envir.get());
    const resolved = test_eval.findFunctionIn("runtime_worker", envir.get());
    if (resolved != function.get()) err.signal("runtime function lookup returned the wrong closure");

    var scale = protect.scoped(R.Rf_ScalarReal(3.0));
    defer scale.deinit();
    const args = [_]test_lang.Argument{
        .{ .value = value },
        .{ .name = "scale", .value = scale.get() },
    };
    var call = protect.scoped(test_lang.buildTaggedCall(resolved, args[0..]) catch |call_error|
        err.signal(@errorName(call_error)));
    defer call.deinit();
    if (R.TYPEOF(call.get()) != R.LANGSXP or R.TYPEOF(R.CDR(call.get())) != R.LISTSXP or
        R.TAG(R.CDR(call.get())) != R.R_NilValue or
        R.TAG(R.CDR(R.CDR(call.get()))) != test_lang.symbol("scale"))
    {
        err.signal("runtime named call structure changed");
    }
    return test_eval.rEval(call.get(), envir.get());
}

fn runtimeSilentEvaluation() SEXP {
    var call = protect.scoped(test_lang.call0(test_lang.symbol("__zigr_runtime_missing__")));
    defer call.deinit();
    if (test_eval.tryEvalSilent(call.get(), R.R_GlobalEnv) != null) {
        err.signal("silent evaluation returned a value for an unbound call");
    }
    return R.Rf_ScalarInteger(1);
}

fn runtimeNestedRecovery() SEXP {
    const condition = trycatch_mod.tryCatchError(struct {
        fn call() SEXP {
            R.Rf_error("runtime nested failure");
        }
    }.call) catch err.signal("nested condition capture failed");
    if (condition == null or !std.mem.eql(u8, trycatch_mod.extractMessage(condition.?), "runtime nested failure")) {
        err.signal("nested condition message changed");
    }
    return R.Rf_ScalarInteger(42);
}

fn runtimeWarning() SEXP {
    err.warn("runtime generated warning");
    return R.Rf_ScalarInteger(7);
}

fn runtimeError() SEXP {
    err.signal("runtime generated error");
}

fn runtimeRngDrawOne() SEXP {
    return R.Rf_ScalarReal(R.unif_rand());
}

fn runtimeRngBody() SEXP {
    return switch (runtime_rng_mode) {
        0 => blk: {
            const result = R.Rf_allocVector(R.REALSXP, @intCast(runtime_rng_count));
            for (0..@intCast(runtime_rng_count)) |index| R.REAL(result)[index] = R.unif_rand();
            break :blk result;
        },
        1 => rng.withRng(runtimeRngDrawOne),
        2 => {
            _ = R.unif_rand();
            R.Rf_error("runtime RNG failure");
        },
        3 => blk: {
            ict.checkInterrupt();
            break :blk R.Rf_ScalarReal(0.0);
        },
        else => err.signal("invalid runtime RNG mode"),
    };
}

fn runtimeRng(count: i32) SEXP {
    if (count < 0 or count > 32) err.signal("invalid runtime RNG count");
    runtime_rng_count = count;
    runtime_rng_mode = 0;
    return rng.withRng(runtimeRngBody);
}

fn runtimeRngFailure(mode: i32) SEXP {
    runtime_rng_mode = mode;
    runtime_rng_count = 0;
    return rng.withRng(runtimeRngBody);
}

fn runtimeInterrupt() SEXP {
    ict.checkInterrupt();
    return R.Rf_ScalarInteger(1);
}

fn runtimeStack() SEXP {
    ict.checkStack();
    ict.checkStack2(64);
    return R.Rf_ScalarInteger(1);
}

fn runtimeEmbed(mode: i32) SEXP {
    return switch (mode) {
        0 => embed.rCodeEval("", R.R_GlobalEnv),
        1 => embed.rCodeEval("local({ x <- 1; x + 1 })", R.R_GlobalEnv),
        2 => embed.rCodeEval("nchar('é')", R.R_GlobalEnv),
        3 => embed.rCodeEval("~~~", R.R_GlobalEnv),
        4 => embed.rCodeEval("stop('embedded stop')", R.R_GlobalEnv),
        else => err.signal("invalid runtime embedded-code mode"),
    };
}

const RuntimeExports = zigr.@"export".generateExports(&.{
    .{ .name = "zigr_runtime_language", .func = runtimeLanguage },
    .{ .name = "zigr_runtime_silent", .func = runtimeSilentEvaluation },
    .{ .name = "zigr_runtime_nested", .func = runtimeNestedRecovery },
    .{ .name = "zigr_runtime_warning", .func = runtimeWarning },
    .{ .name = "zigr_runtime_error", .func = runtimeError },
    .{ .name = "zigr_runtime_rng", .func = runtimeRng },
    .{ .name = "zigr_runtime_interrupt", .func = runtimeInterrupt },
    .{ .name = "zigr_runtime_stack", .func = runtimeStack },
    .{ .name = "zigr_runtime_embed", .func = runtimeEmbed },
}, &.{});

fn runtimeCall(index: usize, a0: SEXP, a1: SEXP) SEXP {
    const fun: RuntimeCall = @ptrCast(@alignCast(RuntimeExports.call_defs[index].fun));
    return fun(a0, a1, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue);
}

fn initRuntimeExports() bool {
    const dll = test_dll orelse (R.R_getEmbeddingDllInfo() orelse return false);
    RuntimeExports.init(dll);
    return true;
}

const RuntimeConditionCapture = struct {
    happened: bool = false,
    condition: SEXP = undefined,
};

fn captureRuntimeCondition(condition: SEXP, data: ?*anyopaque) callconv(.c) SEXP {
    const state: *RuntimeConditionCapture = @ptrCast(@alignCast(data.?));
    state.happened = true;
    state.condition = condition;
    return R.R_NilValue;
}

fn captureCondition(comptime function: *const fn () SEXP, classes: SEXP, state: *RuntimeConditionCapture) SEXP {
    const W = struct {
        fn call(_: ?*anyopaque) callconv(.c) SEXP {
            return function();
        }
    };
    return R.R_tryCatch(
        W.call,
        null,
        classes,
        captureRuntimeCondition,
        @as(?*anyopaque, @ptrCast(state)),
        null,
        null,
    );
}

fn runtimeLanguageCall() SEXP {
    return runtimeCall(0, runtime_language_input, R.R_NilValue);
}

fn runtimeWarningCall() SEXP {
    return runtimeCall(3, R.R_NilValue, R.R_NilValue);
}

fn runtimeErrorCall() SEXP {
    return runtimeCall(4, R.R_NilValue, R.R_NilValue);
}

fn runtimeRngFailureDirectCall() SEXP {
    return runtimeRngFailure(runtime_rng_mode);
}

fn runtimeEmbedCall() SEXP {
    return runtimeCall(8, R.Rf_ScalarInteger(runtime_embed_mode), R.R_NilValue);
}

fn runtimeEmbedDirectCall() SEXP {
    return runtimeEmbed(runtime_embed_mode);
}

threadlocal var runtime_language_input: SEXP = null;
threadlocal var runtime_embed_mode: i32 = 0;

const BoundaryCall = *const fn (R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP) callconv(.c) R.SEXP;
threadlocal var generated_logical_error_arg: SEXP = null;
threadlocal var generated_string_projection_error_arg: SEXP = null;

fn boundaryCall(index: usize, arg: SEXP) SEXP {
    const fun: BoundaryCall = @ptrCast(@alignCast(BoundaryExports.call_defs[index].fun));
    return fun(arg, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue);
}

fn generatedLogicalErrorCall() SEXP {
    return boundaryCall(5, generated_logical_error_arg);
}

fn generatedStringProjectionErrorCall() SEXP {
    return boundaryCall(12, generated_string_projection_error_arg);
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

    const logical = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, 3));
    R.LOGICAL(logical)[0] = 0;
    R.LOGICAL(logical)[1] = 1;
    R.LOGICAL(logical)[2] = R.R_NaInt;
    const logical_pairlist = R.Rf_protect(R.Rf_cons(
        R.Rf_install("zigr_test_external_logical"),
        R.Rf_cons(logical, R.R_NilValue),
    ));
    const logical_fun: *const fn (R.SEXP) callconv(.c) R.SEXP = @ptrCast(@alignCast(ExternalExports.ext_defs[1].fun));
    const logical_result = R.Rf_protect(logical_fun(logical_pairlist));
    defer R.Rf_unprotect(3);
    if (R.INTEGER(logical_result)[0] != 111) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_generated_logical_vector() SEXP {
    if (!initBoundaryExports()) return R.Rf_ScalarReal(0.0);

    const logical = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, 3));
    defer R.Rf_unprotect(1);
    R.LOGICAL(logical)[0] = 0;
    R.LOGICAL(logical)[1] = 1;
    R.LOGICAL(logical)[2] = R.R_NaInt;

    const input_result = R.Rf_protect(boundaryCall(5, logical));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(input_result) != R.INTSXP or R.INTEGER(input_result)[0] != 111) return R.Rf_ScalarReal(0.0);

    const output_result = R.Rf_protect(boundaryCall(6, R.R_NilValue));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(output_result) != R.LGLSXP or R.XLENGTH(output_result) != 3) return R.Rf_ScalarReal(0.0);
    if (R.LOGICAL(output_result)[0] != 0 or R.LOGICAL(output_result)[1] != 1 or R.LOGICAL(output_result)[2] != R.R_NaInt) {
        return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_generated_logical_wrong_type() SEXP {
    if (!initBoundaryExports()) return R.Rf_ScalarReal(0.0);

    const wrong_type = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 1));
    defer R.Rf_unprotect(1);
    generated_logical_error_arg = wrong_type;
    defer generated_logical_error_arg = null;

    const condition = trycatch_mod.tryCatchError(generatedLogicalErrorCall) catch return R.Rf_ScalarReal(0.0);
    if (condition) |value| {
        if (std.mem.eql(u8, trycatch_mod.extractMessage(value), "toLogicalSliceView: ExpectedLogical")) {
            return R.Rf_ScalarReal(1.0);
        }
    }
    return R.Rf_ScalarReal(0.0);
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

    const logical = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, 3));
    defer R.Rf_unprotect(1);
    R.LOGICAL(logical)[0] = 0;
    R.LOGICAL(logical)[1] = 1;
    R.LOGICAL(logical)[2] = R.R_NaInt;
    var logical_counting = mem.CountingAllocator.init(std.heap.page_allocator);
    var logical_view = zigr_convert.toLogicalSliceView(logical_counting.allocator(), logical) catch return R.Rf_ScalarReal(0.0);
    defer logical_view.deinit();
    switch (logical_view) {
        .borrowed => {},
        .owned => return R.Rf_ScalarReal(0.0),
    }
    const logical_values = logical_view.constSlice();
    if (logical_counting.stats.allocations != 0 or logical_counting.stats.bytes_allocated != 0 or
        logical_values[0] != 0 or logical_values[1] != 1 or logical_values[2] != R.R_NaInt) return R.Rf_ScalarReal(0.0);

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
    const entry = cleanup.diagnosticSnapshot();

    const strings = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 3));
    defer R.Rf_unprotect(1);
    R.SET_STRING_ELT(strings, 0, R.Rf_mkChar("one"));
    R.SET_STRING_ELT(strings, 1, R.R_NaString);
    R.SET_STRING_ELT(strings, 2, R.Rf_mkChar("three"));
    const result = boundaryCall(1, strings);
    if (R.INTEGER(result)[0] != 32) return R.Rf_ScalarReal(0.0);
    if (!sameRestorationState(entry, cleanup.diagnosticSnapshot())) return R.Rf_ScalarReal(0.0);

    var tracked = protect.scoped(R.Rf_allocVector(R.INTSXP, 1));
    tracked.deinit();
    if (!sameRestorationState(entry, cleanup.diagnosticSnapshot())) return R.Rf_ScalarReal(0.0);
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

export fn zigr_test_altrep_access_strategies() SEXP {
    const direct_values = [_]i32{ 4, 2, 9, -1, 3 };
    const direct = R.Rf_protect(MyAltInt.init(direct_values[0..]));
    defer R.Rf_unprotect(1);
    if (R.ALTREP(direct) == 0 or R.INTEGER_OR_NULL(direct) == null) return R.Rf_ScalarReal(0.0);

    var direct_access = zigr_convert.toVectorAccessWithStrategy(i32, .one_pass, std.heap.page_allocator, direct, .direct) catch
        return R.Rf_ScalarReal(0.0);
    if (direct_access.strategy() != .direct) return R.Rf_ScalarReal(0.0);
    const direct_slice = direct_access.contiguousSlice() orelse return R.Rf_ScalarReal(0.0);
    if (direct_slice.len != direct_values.len or direct_slice[0] != 4 or direct_slice[4] != 3 or
        R.INTEGER_OR_NULL(direct) == null) return R.Rf_ScalarReal(0.0);
    const direct_chunk = direct_access.next() catch return R.Rf_ScalarReal(0.0);
    if (direct_chunk == null) return R.Rf_ScalarReal(0.0);
    const direct_end = direct_access.next() catch return R.Rf_ScalarReal(0.0);
    if (direct_end != null) return R.Rf_ScalarReal(0.0);
    direct_access.deinit();

    const empty_values = [_]i32{};
    const empty = R.Rf_protect(MyAltInt.init(empty_values[0..]));
    defer R.Rf_unprotect(1);
    var empty_access = zigr_convert.toVectorAccessWithStrategy(i32, .one_pass, std.heap.page_allocator, empty, .region) catch
        return R.Rf_ScalarReal(0.0);
    if (empty_access.strategy() != .region) return R.Rf_ScalarReal(0.0);
    const empty_chunk = empty_access.next() catch return R.Rf_ScalarReal(0.0);
    if (empty_chunk != null) return R.Rf_ScalarReal(0.0);
    empty_access.deinit();

    const missing_values = [_]i32{ 1, R.R_NaInt, 3 };
    const missing = R.Rf_protect(MyAltInt.init(missing_values[0..]));
    defer R.Rf_unprotect(1);
    var missing_access = zigr_convert.toVectorAccessWithStrategy(i32, .random_access, std.heap.page_allocator, missing, .materialized) catch
        return R.Rf_ScalarReal(0.0);
    const missing_slice = missing_access.contiguousSlice() orelse return R.Rf_ScalarReal(0.0);
    if (missing_slice.len != missing_values.len or missing_slice[0] != 1 or
        missing_slice[1] != R.R_NaInt or missing_slice[2] != 3) return R.Rf_ScalarReal(0.0);
    missing_access.deinit();

    const region_input = R.Rf_protect(shortRegionAltInteger());
    defer R.Rf_unprotect(1);
    if (R.INTEGER_OR_NULL(region_input) != null) return R.Rf_ScalarReal(0.0);

    const direct_failure = zigr_convert.toVectorAccessWithStrategy(i32, .one_pass, std.heap.page_allocator, region_input, .direct);
    if (direct_failure) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |access_err| {
        if (access_err != error.DirectPointerUnavailable) return R.Rf_ScalarReal(0.0);
    }

    short_region_get_calls = 0;
    short_region_elt_calls = 0;
    var region_counting = mem.CountingAllocator.init(std.heap.page_allocator);
    var region_access = zigr_convert.toVectorAccessWithStrategy(i32, .one_pass, region_counting.allocator(), region_input, .region) catch
        return R.Rf_ScalarReal(0.0);
    if (region_access.strategy() != .region or region_access.contiguousSlice() != null) return R.Rf_ScalarReal(0.0);
    var region_sum: i64 = 0;
    var region_chunks: usize = 0;
    while (region_access.next() catch return R.Rf_ScalarReal(0.0)) |chunk| {
        region_chunks += 1;
        for (chunk) |value| region_sum += value;
    }
    if (region_sum != @as(i64, @intCast(short_region_len)) * @as(i64, @intCast(short_region_len + 1)) / 2 or
        region_chunks != (short_region_len + 255) / 256 or short_region_get_calls != region_chunks or
        short_region_elt_calls != 0 or R.INTEGER_OR_NULL(region_input) != null or
        region_counting.stats.allocations != 1 or
        region_counting.stats.bytes_allocated > 256 * @sizeOf(i32) or
        region_counting.stats.peak_live_bytes > 256 * @sizeOf(i32)) return R.Rf_ScalarReal(0.0);
    region_access.deinit();
    if (region_counting.stats.live_bytes != 0) return R.Rf_ScalarReal(0.0);

    var materialized_counting = mem.CountingAllocator.init(std.heap.page_allocator);
    var materialized = zigr_convert.toVectorAccessWithStrategy(i32, .random_access, materialized_counting.allocator(), region_input, .materialized) catch
        return R.Rf_ScalarReal(0.0);
    if (materialized.strategy() != .materialized) return R.Rf_ScalarReal(0.0);
    const materialized_slice = materialized.contiguousSlice() orelse return R.Rf_ScalarReal(0.0);
    if (materialized_slice.len != short_region_len or materialized_slice[0] != 1 or
        materialized_slice[4096] != 4097 or materialized_counting.stats.allocations != 1 or
        materialized_counting.stats.bytes_allocated != short_region_len * @sizeOf(i32) or
        materialized_counting.stats.peak_live_bytes != short_region_len * @sizeOf(i32) or
        R.INTEGER_OR_NULL(region_input) != null) return R.Rf_ScalarReal(0.0);
    materialized.deinit();
    if (materialized_counting.stats.live_bytes != 0) return R.Rf_ScalarReal(0.0);

    var automatic = zigr_convert.toVectorAccess(i32, .one_pass, std.heap.page_allocator, region_input) catch
        return R.Rf_ScalarReal(0.0);
    if (automatic.strategy() != .region) return R.Rf_ScalarReal(0.0);
    automatic.deinit();

    short_region_get_calls = 0;
    short_region_elt_calls = 0;
    short_region_max_requested = 0;
    const streamed_rvector = zigr.rvector.RVector(i32).init(region_input) catch return R.Rf_ScalarReal(0.0);
    const streamed_result = R.Rf_protect(streamed_rvector.mulScalar(2));
    defer R.Rf_unprotect(1);
    const expected_stream_regions = (short_region_len + 255) / 256;
    if (R.TYPEOF(streamed_result) != R.INTSXP or R.XLENGTH(streamed_result) != short_region_len or
        R.INTEGER(streamed_result)[0] != 2 or R.INTEGER(streamed_result)[short_region_len - 1] != 2 * @as(i32, @intCast(short_region_len)) or
        short_region_get_calls != expected_stream_regions or short_region_elt_calls != 0 or
        short_region_max_requested > 256 or R.INTEGER_OR_NULL(region_input) != null) return R.Rf_ScalarReal(0.0);

    const raw_input = R.Rf_protect(shortRegionAltRaw());
    defer R.Rf_unprotect(1);
    var raw_counting = mem.CountingAllocator.init(std.heap.page_allocator);
    var raw_access = zigr_convert.toVectorAccessWithStrategy(u8, .one_pass, raw_counting.allocator(), raw_input, .region) catch
        return R.Rf_ScalarReal(0.0);
    const raw_chunk = raw_access.next() catch return R.Rf_ScalarReal(0.0);
    if (raw_chunk == null or raw_chunk.?.len != 256 or raw_chunk.?[0] != 0 or raw_chunk.?[250] != 250 or
        raw_counting.stats.peak_live_bytes > 256 * @sizeOf(u8)) return R.Rf_ScalarReal(0.0);
    raw_access.deinit();
    if (raw_counting.stats.live_bytes != 0) return R.Rf_ScalarReal(0.0);

    const complex_input = R.Rf_protect(shortRegionAltComplex());
    defer R.Rf_unprotect(1);
    var complex_access = zigr_convert.toVectorAccessWithStrategy(zigr_convert.Rcomplex, .one_pass, std.heap.page_allocator, complex_input, .region) catch
        return R.Rf_ScalarReal(0.0);
    const complex_chunk = complex_access.next() catch return R.Rf_ScalarReal(0.0);
    if (complex_chunk == null or complex_chunk.?.len != 256 or complex_chunk.?[0].r != 1.0 or
        complex_chunk.?[0].i != -1.0 or complex_chunk.?[250].r != 251.0) return R.Rf_ScalarReal(0.0);
    complex_access.deinit();

    const failing = R.Rf_protect(failingRawRegion());
    defer R.Rf_unprotect(1);
    var failing_counting = mem.CountingAllocator.init(std.heap.page_allocator);
    var failing_access = zigr_convert.toVectorAccessWithStrategy(u8, .one_pass, failing_counting.allocator(), failing, .region) catch
        return R.Rf_ScalarReal(0.0);
    if (failing_access.next()) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |access_err| {
        if (access_err != error.AltRepRegionRead) return R.Rf_ScalarReal(0.0);
    }
    failing_access.deinit();
    if (failing_counting.stats.allocations != 1 or failing_counting.stats.frees != 1 or failing_counting.stats.live_bytes != 0) {
        return R.Rf_ScalarReal(0.0);
    }

    if (!initArenaExports()) return R.Rf_ScalarReal(0.0);
    const before_failure_depth = protect.getDepth();
    generated_failure_input = failing;
    if (trycatch_mod.tryCatch(generatedFailureCall)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    if (protect.getDepth() != before_failure_depth) return R.Rf_ScalarReal(0.0);
    generated_failure_input = R.R_NilValue;

    const generated_sum = R.Rf_protect(arenaCall(7, region_input));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(generated_sum) != R.REALSXP or R.REAL(generated_sum)[0] != @as(f64, @floatFromInt(region_sum)) or
        R.INTEGER_OR_NULL(region_input) != null) return R.Rf_ScalarReal(0.0);
    const generated_materialized = R.Rf_protect(arenaCall(8, region_input));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(generated_materialized) != R.INTSXP or R.XLENGTH(generated_materialized) != short_region_len or
        R.INTEGER(generated_materialized)[0] != 1 or R.INTEGER(generated_materialized)[4096] != 4097) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_generated_result_longjmp() SEXP {
    if (!initArenaExports()) return R.Rf_ScalarReal(0.0);
    const entry = cleanup.diagnosticSnapshot();
    if (trycatch_mod.tryCatch(struct {
        fn call() SEXP {
            return arenaCall(5, R.R_NilValue);
        }
    }.call)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    if (!sameRestorationState(entry, cleanup.diagnosticSnapshot())) return R.Rf_ScalarReal(0.0);

    const result = arenaCall(0, R.Rf_ScalarReal(1.0));
    if (!sameRestorationState(entry, cleanup.diagnosticSnapshot())) return R.Rf_ScalarReal(0.0);
    return result;
}

var direct_result_failure_input: SEXP = undefined;

fn directResultFailureCall() SEXP {
    return arenaCall(12, direct_result_failure_input);
}

fn directResultWrongTypeCall() SEXP {
    return arenaCall(10, direct_result_failure_input);
}

fn directResultOversizedCall() SEXP {
    return arenaCall(13, R.R_NilValue);
}

fn directResultInterruptCall() SEXP {
    return arenaCall(14, direct_result_failure_input);
}

export fn zigr_test_direct_result_builder() SEXP {
    if (!initArenaExports()) return R.Rf_ScalarReal(0.0);

    const real = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 3));
    R.REAL(real)[0] = 1.5;
    R.REAL(real)[1] = R.R_NaReal;
    R.REAL(real)[2] = -4.0;
    const raw = R.Rf_protect(R.Rf_allocVector(R.RAWSXP, 3));
    R.RAW(raw)[0] = 0;
    R.RAW(raw)[1] = 127;
    R.RAW(raw)[2] = 255;
    const empty = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 0));
    var protected_count: c_int = 3;
    defer R.Rf_unprotect(protected_count);

    const real_result = R.Rf_protect(arenaCall(10, real));
    protected_count += 1;
    const raw_result = R.Rf_protect(arenaCall(11, raw));
    protected_count += 1;
    if (real_result == real or raw_result == raw or R.TYPEOF(real_result) != R.REALSXP or
        R.XLENGTH(real_result) != 3 or R.REAL(real_result)[0] != 3.0 or
        R.ISNA(R.REAL(real_result)[1]) == 0 or R.REAL(real_result)[2] != -8.0 or
        R.TYPEOF(raw_result) != R.RAWSXP or R.XLENGTH(raw_result) != 3 or
        R.RAW(raw_result)[0] != 0 or R.RAW(raw_result)[1] != 127 or R.RAW(raw_result)[2] != 255 or
        R.Rf_getAttrib(real_result, R.R_NamesSymbol) != R.R_NilValue) return R.Rf_ScalarReal(0.0);

    const empty_result = R.Rf_protect(arenaCall(10, empty));
    protected_count += 1;
    if (empty_result == empty or R.TYPEOF(empty_result) != R.REALSXP or R.XLENGTH(empty_result) != 0) {
        return R.Rf_ScalarReal(0.0);
    }

    direct_result_failure_input = raw;
    if (trycatch_mod.tryCatch(directResultWrongTypeCall)) |_| {
        direct_result_failure_input = R.R_NilValue;
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    direct_result_failure_input = R.R_NilValue;

    const before_failure_depth = zigr.protect.getDepth();
    direct_result_failure_input = real;
    if (trycatch_mod.tryCatch(directResultFailureCall)) |_| {
        direct_result_failure_input = R.R_NilValue;
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    direct_result_failure_input = R.R_NilValue;
    if (zigr.protect.getDepth() != before_failure_depth) return R.Rf_ScalarReal(0.0);

    const before_oversized_depth = zigr.protect.getDepth();
    if (trycatch_mod.tryCatch(directResultOversizedCall)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    if (zigr.protect.getDepth() != before_oversized_depth) return R.Rf_ScalarReal(0.0);

    const before_interrupt_depth = zigr.protect.getDepth();
    direct_result_failure_input = real;
    R_interrupts_pending = 1;
    if (trycatch_mod.tryCatch(directResultInterruptCall)) |_| {
        R_interrupts_pending = 0;
        direct_result_failure_input = R.R_NilValue;
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    R_interrupts_pending = 0;
    direct_result_failure_input = R.R_NilValue;
    if (zigr.protect.getDepth() != before_interrupt_depth) return R.Rf_ScalarReal(0.0);

    const recovered = R.Rf_protect(arenaCall(10, real));
    protected_count += 1;
    if (R.TYPEOF(recovered) != R.REALSXP or R.XLENGTH(recovered) != 3 or R.REAL(recovered)[0] != 3.0 or
        R.ISNA(R.REAL(recovered)[1]) == 0 or R.REAL(recovered)[2] != -8.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

fn reference(code: []const u8) protect.ScopedProtect {
    return protect.scoped(embed.rCodeEval(code, R.R_BaseEnv));
}

fn sameReal(a: f64, b: f64) bool {
    if (R.ISNA(a) != 0 or R.ISNA(b) != 0) return R.ISNA(a) != 0 and R.ISNA(b) != 0;
    if (R.ISNAN(a) or R.ISNAN(b)) return R.ISNAN(a) and R.ISNA(a) == 0 and R.ISNAN(b) and R.ISNA(b) == 0;
    if (a == 0.0 and b == 0.0) return std.math.signbit(a) == std.math.signbit(b);
    return a == b;
}

fn sameRealVector(a: SEXP, b: SEXP) bool {
    if (R.TYPEOF(a) != R.REALSXP or R.TYPEOF(b) != R.REALSXP or R.XLENGTH(a) != R.XLENGTH(b)) return false;
    for (0..@as(usize, @intCast(R.XLENGTH(a)))) |i| {
        if (!sameReal(R.REAL(a)[i], R.REAL(b)[i])) return false;
    }
    return true;
}

fn sameRawVector(a: SEXP, b: SEXP) bool {
    if (R.TYPEOF(a) != R.RAWSXP or R.TYPEOF(b) != R.RAWSXP or R.XLENGTH(a) != R.XLENGTH(b)) return false;
    const n = @as(usize, @intCast(R.XLENGTH(a)));
    return std.mem.eql(u8, R.RAW(a)[0..n], R.RAW(b)[0..n]);
}

fn sameComplexVector(a: SEXP, b: SEXP) bool {
    if (R.TYPEOF(a) != R.CPLXSXP or R.TYPEOF(b) != R.CPLXSXP or R.XLENGTH(a) != R.XLENGTH(b)) return false;
    if (R.XLENGTH(a) == 0) return true;
    const ap = R.COMPLEX(a) orelse return false;
    const bp = R.COMPLEX(b) orelse return false;
    const ap_ptr: [*]const zigr_convert.Rcomplex = @ptrCast(@alignCast(ap));
    const bp_ptr: [*]const zigr_convert.Rcomplex = @ptrCast(@alignCast(bp));
    for (0..@as(usize, @intCast(R.XLENGTH(a)))) |i| {
        const av = ap_ptr[i];
        const bv = bp_ptr[i];
        if (!sameReal(av.r, bv.r) or !sameReal(av.i, bv.i)) return false;
    }
    return true;
}

fn sameIntegerVector(a: SEXP, b: SEXP) bool {
    if (R.TYPEOF(a) != R.INTSXP or R.TYPEOF(b) != R.INTSXP or R.XLENGTH(a) != R.XLENGTH(b)) return false;
    const n = @as(usize, @intCast(R.XLENGTH(a)));
    return std.mem.eql(i32, R.INTEGER(a)[0..n], R.INTEGER(b)[0..n]);
}

fn conditionMatchesReference(expected: SEXP, actual: SEXP, class_name: [*:0]const u8) bool {
    return R.Rf_inherits(expected, class_name) != 0 and
        R.Rf_inherits(actual, class_name) != 0 and
        std.mem.eql(u8, trycatch_mod.extractMessage(expected), trycatch_mod.extractMessage(actual));
}

export fn zigr_test_runtime_semantics() SEXP {
    if (!initRuntimeExports()) return R.Rf_ScalarReal(0.0);

    const input = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 3));
    R.REAL(input)[0] = 1.0;
    R.REAL(input)[1] = 2.0;
    R.REAL(input)[2] = 3.0;
    defer R.Rf_unprotect(1);

    var expected_language = reference("c(3, 6, 9)");
    defer expected_language.deinit();
    runtime_language_input = input;
    defer runtime_language_input = null;
    const language_attempt = trycatch_mod.tryCatchError(runtimeLanguageCall) catch return R.Rf_ScalarReal(0.0);
    if (language_attempt == null) return R.Rf_ScalarReal(0.0);
    if (R.Rf_inherits(language_attempt.?, "error") != 0) return R.Rf_ScalarReal(0.0);
    var actual_language = protect.scoped(language_attempt.?);
    defer actual_language.deinit();
    if (!sameRealVector(actual_language.get(), expected_language.get())) return R.Rf_ScalarReal(0.0);

    var silent = protect.scoped(runtimeCall(1, R.R_NilValue, R.R_NilValue));
    defer silent.deinit();
    if (R.TYPEOF(silent.get()) != R.INTSXP or R.INTEGER(silent.get())[0] != 1) return R.Rf_ScalarReal(0.0);

    var nested = protect.scoped(runtimeCall(2, R.R_NilValue, R.R_NilValue));
    defer nested.deinit();
    if (R.TYPEOF(nested.get()) != R.INTSXP or R.INTEGER(nested.get())[0] != 42) return R.Rf_ScalarReal(0.0);

    var expected_error = reference("tryCatch(stop('runtime generated error'), error=identity)");
    defer expected_error.deinit();
    const actual_error = trycatch_mod.tryCatchError(runtimeErrorCall) catch return R.Rf_ScalarReal(0.0);
    if (actual_error == null or !conditionMatchesReference(expected_error.get(), actual_error.?, "error")) {
        return R.Rf_ScalarReal(0.0);
    }

    var warning_classes = protect.scoped(R.Rf_allocVector(R.STRSXP, 1));
    defer warning_classes.deinit();
    R.SET_STRING_ELT(warning_classes.get(), 0, R.Rf_mkChar("warning"));
    var warning_capture = RuntimeConditionCapture{};
    _ = captureCondition(runtimeWarningCall, warning_classes.get(), &warning_capture);
    if (!warning_capture.happened) return R.Rf_ScalarReal(0.0);
    var warning_condition = protect.scoped(warning_capture.condition);
    defer warning_condition.deinit();
    var expected_warning = reference("tryCatch({ warning('runtime generated warning'); 7L }, warning=identity)");
    defer expected_warning.deinit();
    if (!conditionMatchesReference(expected_warning.get(), warning_condition.get(), "warning")) return R.Rf_ScalarReal(0.0);

    var saved_rng = reference("if (exists('.Random.seed', envir=.GlobalEnv, inherits=FALSE)) .Random.seed else NULL");
    defer saved_rng.deinit();
    const had_rng_state = saved_rng.get() != R.R_NilValue;
    defer if (had_rng_state) {
        test_eval.defineVarIn(".Random.seed", saved_rng.get(), R.R_GlobalEnv);
    } else {
        _ = embed.rCodeEval("rm('.Random.seed', envir=.GlobalEnv)", R.R_GlobalEnv);
    };

    _ = embed.rCodeEval("set.seed(1729)", R.R_GlobalEnv);
    var expected_rng_values = reference("stats::runif(4)");
    defer expected_rng_values.deinit();
    var expected_rng_state = reference("get('.Random.seed', .GlobalEnv)");
    defer expected_rng_state.deinit();
    _ = embed.rCodeEval("set.seed(1729)", R.R_GlobalEnv);
    var actual_rng = protect.scoped(runtimeCall(5, R.Rf_ScalarInteger(4), R.R_NilValue));
    defer actual_rng.deinit();
    var actual_rng_state = reference("get('.Random.seed', .GlobalEnv)");
    defer actual_rng_state.deinit();
    if (!sameRealVector(actual_rng.get(), expected_rng_values.get()) or
        !sameIntegerVector(actual_rng_state.get(), expected_rng_state.get())) return R.Rf_ScalarReal(0.0);

    _ = embed.rCodeEval("set.seed(1729)", R.R_GlobalEnv);
    var expected_after_failure = reference("stats::runif(2)");
    defer expected_after_failure.deinit();
    var expected_after_failure_state = reference("get('.Random.seed', .GlobalEnv)");
    defer expected_after_failure_state.deinit();
    _ = embed.rCodeEval("set.seed(1729)", R.R_GlobalEnv);
    runtime_rng_mode = 2;
    if (trycatch_mod.tryCatch(runtimeRngFailureDirectCall)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    if (runtime_rng_mode != 2) return R.Rf_ScalarReal(0.0);
    var recovered_rng = protect.scoped(runtimeCall(5, R.Rf_ScalarInteger(1), R.R_NilValue));
    defer recovered_rng.deinit();
    var recovered_rng_state = reference("get('.Random.seed', .GlobalEnv)");
    defer recovered_rng_state.deinit();
    if (R.TYPEOF(recovered_rng.get()) != R.REALSXP or R.XLENGTH(recovered_rng.get()) != 1 or
        !sameReal(R.REAL(recovered_rng.get())[0], R.REAL(expected_after_failure.get())[1]) or
        !sameIntegerVector(recovered_rng_state.get(), expected_after_failure_state.get()))
    {
        return R.Rf_ScalarReal(0.0);
    }

    _ = embed.rCodeEval("set.seed(1729)", R.R_GlobalEnv);
    var expected_one = reference("stats::runif(1)");
    defer expected_one.deinit();
    var expected_one_state = reference("get('.Random.seed', .GlobalEnv)");
    defer expected_one_state.deinit();
    _ = embed.rCodeEval("set.seed(1729)", R.R_GlobalEnv);
    runtime_rng_mode = 1;
    if (trycatch_mod.tryCatch(runtimeRngFailureDirectCall)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    if (runtime_rng_mode != 1) {
        return R.Rf_ScalarReal(0.0);
    }
    var nested_recovered_rng = protect.scoped(runtimeCall(5, R.Rf_ScalarInteger(1), R.R_NilValue));
    defer nested_recovered_rng.deinit();
    var nested_recovered_rng_state = reference("get('.Random.seed', .GlobalEnv)");
    defer nested_recovered_rng_state.deinit();
    if (!sameRealVector(nested_recovered_rng.get(), expected_one.get()) or
        !sameIntegerVector(nested_recovered_rng_state.get(), expected_one_state.get())) return R.Rf_ScalarReal(0.0);

    _ = embed.rCodeEval("set.seed(1729)", R.R_GlobalEnv);
    runtime_rng_mode = 3;
    R_interrupts_pending = 1;
    if (trycatch_mod.tryCatch(runtimeRngFailureDirectCall)) |_| {
        R_interrupts_pending = 0;
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    R_interrupts_pending = 0;
    if (runtime_rng_mode != 3) {
        return R.Rf_ScalarReal(0.0);
    }
    var interrupted_rng = protect.scoped(runtimeCall(5, R.Rf_ScalarInteger(1), R.R_NilValue));
    defer interrupted_rng.deinit();
    var interrupted_rng_state = reference("get('.Random.seed', .GlobalEnv)");
    defer interrupted_rng_state.deinit();
    if (!sameRealVector(interrupted_rng.get(), expected_one.get()) or
        !sameIntegerVector(interrupted_rng_state.get(), expected_one_state.get()))
    {
        return R.Rf_ScalarReal(0.0);
    }

    var condition_classes = protect.scoped(R.Rf_allocVector(R.STRSXP, 1));
    defer condition_classes.deinit();
    R.SET_STRING_ELT(condition_classes.get(), 0, R.Rf_mkChar("condition"));
    var generated_interrupt = protect.scoped(runtimeCall(6, R.R_NilValue, R.R_NilValue));
    defer generated_interrupt.deinit();
    if (R.TYPEOF(generated_interrupt.get()) != R.INTSXP or R.INTEGER(generated_interrupt.get())[0] != 1) {
        return R.Rf_ScalarReal(0.0);
    }
    var interrupt_capture = RuntimeConditionCapture{};
    R_interrupts_pending = 1;
    _ = captureCondition(runtimeInterrupt, condition_classes.get(), &interrupt_capture);
    R_interrupts_pending = 0;
    if (!interrupt_capture.happened) {
        return R.Rf_ScalarReal(0.0);
    }
    var interrupt_condition = protect.scoped(interrupt_capture.condition);
    defer interrupt_condition.deinit();
    if (R.Rf_inherits(interrupt_condition.get(), "interrupt") == 0 or
        R.Rf_inherits(interrupt_condition.get(), "error") != 0)
    {
        return R.Rf_ScalarReal(0.0);
    }

    var stack = protect.scoped(runtimeCall(7, R.R_NilValue, R.R_NilValue));
    defer stack.deinit();
    if (R.TYPEOF(stack.get()) != R.INTSXP or R.INTEGER(stack.get())[0] != 1) return R.Rf_ScalarReal(0.0);

    runtime_embed_mode = 1;
    const braced_attempt = trycatch_mod.tryCatchError(runtimeEmbedCall) catch return R.Rf_ScalarReal(0.0);
    if (braced_attempt == null) return R.Rf_ScalarReal(0.0);
    if (R.Rf_inherits(braced_attempt.?, "error") != 0) return R.Rf_ScalarReal(0.0);
    var braced = protect.scoped(braced_attempt.?);
    defer braced.deinit();
    var expected_braced = reference("local({ x <- 1; x + 1 })");
    defer expected_braced.deinit();
    if (!sameRealVector(braced.get(), expected_braced.get())) return R.Rf_ScalarReal(0.0);

    runtime_embed_mode = 2;
    var unicode = protect.scoped(runtimeEmbedCall());
    defer unicode.deinit();
    var expected_unicode = reference("nchar('é')");
    defer expected_unicode.deinit();
    if (R.TYPEOF(unicode.get()) != R.TYPEOF(expected_unicode.get()) or
        R.INTEGER(unicode.get())[0] != R.INTEGER(expected_unicode.get())[0]) return R.Rf_ScalarReal(0.0);

    var parser_reference = reference("tryCatch(parse(text='~~~'), error=identity)");
    defer parser_reference.deinit();
    if (R.Rf_inherits(parser_reference.get(), "error") == 0) return R.Rf_ScalarReal(0.0);
    // R_ParseEvalString reports parser failures with its API-level "parse error" message.
    var expected_syntax = reference("tryCatch(stop('parse error'), error=identity)");
    defer expected_syntax.deinit();
    runtime_embed_mode = 3;
    const actual_syntax = trycatch_mod.tryCatchError(runtimeEmbedDirectCall) catch return R.Rf_ScalarReal(0.0);
    if (actual_syntax == null or !conditionMatchesReference(expected_syntax.get(), actual_syntax.?, "error")) {
        return R.Rf_ScalarReal(0.0);
    }

    var expected_stop = reference("tryCatch(stop('embedded stop'), error=identity)");
    defer expected_stop.deinit();
    runtime_embed_mode = 4;
    const actual_stop = trycatch_mod.tryCatchError(runtimeEmbedDirectCall) catch return R.Rf_ScalarReal(0.0);
    if (actual_stop == null or !conditionMatchesReference(expected_stop.get(), actual_stop.?, "error")) {
        return R.Rf_ScalarReal(0.0);
    }

    runtime_embed_mode = 0;
    var expected_empty = reference("tryCatch(stop('parse error'), error=identity)");
    defer expected_empty.deinit();
    const empty = trycatch_mod.tryCatchError(runtimeEmbedDirectCall) catch return R.Rf_ScalarReal(0.0);
    if (empty == null or !conditionMatchesReference(expected_empty.get(), empty.?, "error")) {
        return R.Rf_ScalarReal(0.0);
    }

    var recovered_language = protect.scoped(runtimeLanguageCall());
    defer recovered_language.deinit();
    if (!sameRealVector(recovered_language.get(), expected_language.get())) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_scalar_reference() SEXP {
    var zero = reference("-0.0");
    defer zero.deinit();
    const zero_value = zigr_convert.toRealScalar(zero.get()) catch return R.Rf_ScalarReal(0.0);
    if (zero_value != 0.0 or !std.math.signbit(zero_value)) return R.Rf_ScalarReal(0.0);

    var nan = reference("NaN");
    defer nan.deinit();
    const nan_value = zigr_convert.toRealScalar(nan.get()) catch return R.Rf_ScalarReal(0.0);
    if (R.ISNA(nan_value) != 0 or !R.ISNAN(nan_value)) return R.Rf_ScalarReal(0.0);

    var real_values = reference("c(-0.0, NaN, Inf, -Inf)");
    defer real_values.deinit();
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const real_slice = zigr_convert.toRealSlice(arena.allocator(), real_values.get()) catch return R.Rf_ScalarReal(0.0);
    if (real_slice.len != 4 or !std.math.signbit(real_slice[0]) or R.ISNA(real_slice[1]) != 0 or
        !R.ISNAN(real_slice[1]) or !std.math.isInf(real_slice[2]) or !std.math.isInf(real_slice[3]))
    {
        return R.Rf_ScalarReal(0.0);
    }

    var int_ref = reference("2147483647L");
    defer int_ref.deinit();
    if ((zigr_convert.toIntScalar(int_ref.get()) catch return R.Rf_ScalarReal(0.0)) != std.math.maxInt(i32)) {
        return R.Rf_ScalarReal(0.0);
    }
    var logical_ref = reference("c(FALSE, TRUE, NA)");
    defer logical_ref.deinit();
    const logical_slice = zigr_convert.toLogicalSlice(arena.allocator(), logical_ref.get()) catch return R.Rf_ScalarReal(0.0);
    if (!std.mem.eql(i32, logical_slice, &[_]i32{ 0, 1, R.R_NaInt })) return R.Rf_ScalarReal(0.0);

    var optional_null = reference("NULL");
    defer optional_null.deinit();
    var optional_na = reference("NA_real_");
    defer optional_na.deinit();
    const null_value = zigr_convert.toOptionalRealScalar(optional_null.get()) catch return R.Rf_ScalarReal(0.0);
    const na_value = zigr_convert.toOptionalRealScalar(optional_na.get()) catch return R.Rf_ScalarReal(0.0);
    if (null_value != null or na_value != null) {
        return R.Rf_ScalarReal(0.0);
    }
    if (!initArenaExports()) return R.Rf_ScalarReal(0.0);
    var generated_zero = protect.scoped(arenaCall(0, zero.get()));
    defer generated_zero.deinit();
    if (R.TYPEOF(generated_zero.get()) != R.REALSXP or R.XLENGTH(generated_zero.get()) != 1 or
        !sameReal(R.REAL(generated_zero.get())[0], zero_value)) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_kernel_reference() SEXP {
    var clean = reference("c(-2.5, 0, 3.5, 7)");
    defer clean.deinit();
    var sum_ref = reference("sum(c(-2.5, 0, 3.5, 7))");
    defer sum_ref.deinit();
    var mean_ref = reference("mean(c(-2.5, 0, 3.5, 7))");
    defer mean_ref.deinit();
    var norm_ref = reference("sum(c(-2.5, 0, 3.5, 7)^2)");
    defer norm_ref.deinit();
    var min_ref = reference("min(c(-2.5, 0, 3.5, 7))");
    defer min_ref.deinit();
    var max_ref = reference("max(c(-2.5, 0, 3.5, 7))");
    defer max_ref.deinit();
    var argmin_ref = reference("as.numeric(which.min(c(-2.5, 0, 3.5, 7)) - 1)");
    defer argmin_ref.deinit();
    var argmax_ref = reference("as.numeric(which.max(c(-2.5, 0, 3.5, 7)) - 1)");
    defer argmax_ref.deinit();
    var scale_ref = reference("sum(c(-2.5, 0, 3.5, 7) * 2 + 3)");
    defer scale_ref.deinit();
    var cumsum_ref = reference("cumsum(c(-2.5, 0, 3.5, 7))");
    defer cumsum_ref.deinit();
    var empty_real = reference("numeric(0)");
    defer empty_real.deinit();
    var empty_sum_ref = reference("sum(numeric(0))");
    defer empty_sum_ref.deinit();
    var empty_mean_ref = reference("mean(numeric(0))");
    defer empty_mean_ref.deinit();
    var empty_norm_ref = reference("sum(numeric(0)^2)");
    defer empty_norm_ref.deinit();
    var empty_min_ref = reference("min(numeric(0))");
    defer empty_min_ref.deinit();
    var empty_max_ref = reference("max(numeric(0))");
    defer empty_max_ref.deinit();
    var empty_sum_narm_ref = reference("sum(numeric(0), na.rm=TRUE)");
    defer empty_sum_narm_ref.deinit();
    var empty_pmin_ref = reference("pmin(numeric(0), 1)");
    defer empty_pmin_ref.deinit();
    var empty_pmax_ref = reference("pmax(numeric(0), 1)");
    defer empty_pmax_ref.deinit();
    var empty_cumsum_ref = reference("cumsum(numeric(0))");
    defer empty_cumsum_ref.deinit();
    var empty_mean_narm_ref = reference("mean(numeric(0), na.rm=TRUE)");
    defer empty_mean_narm_ref.deinit();
    var all_missing_mean_narm_ref = reference("mean(c(NA_real_, NaN), na.rm=TRUE)");
    defer all_missing_mean_narm_ref.deinit();

    if (!sameReal(zigr_convert.sum(clean.get()), R.REAL(sum_ref.get())[0]) or
        !sameReal(zigr_convert.mean(clean.get()), R.REAL(mean_ref.get())[0]) or
        !sameReal(zigr_convert.norm2(clean.get()), R.REAL(norm_ref.get())[0]) or
        !sameReal(zigr_convert.min(clean.get()), R.REAL(min_ref.get())[0]) or
        !sameReal(zigr_convert.max(clean.get()), R.REAL(max_ref.get())[0]) or
        zigr_convert.argmin(clean.get()) != @as(i64, @intFromFloat(R.REAL(argmin_ref.get())[0])) or
        zigr_convert.argmax(clean.get()) != @as(i64, @intFromFloat(R.REAL(argmax_ref.get())[0])) or
        !sameReal(zigr_convert.scaleAdd(clean.get(), 2.0, 3.0), R.REAL(scale_ref.get())[0]))
    {
        return R.Rf_ScalarReal(0.0);
    }
    var cumsum_result = protect.scoped(zigr_convert.cumsum(clean.get()));
    defer cumsum_result.deinit();
    if (!sameRealVector(cumsum_result.get(), cumsum_ref.get())) return R.Rf_ScalarReal(0.0);
    var empty_pmin_result = protect.scoped(zigr_convert.pmin(empty_real.get(), clean.get()));
    defer empty_pmin_result.deinit();
    var empty_pmax_result = protect.scoped(zigr_convert.pmax(empty_real.get(), clean.get()));
    defer empty_pmax_result.deinit();
    var empty_cumsum_result = protect.scoped(zigr_convert.cumsum(empty_real.get()));
    defer empty_cumsum_result.deinit();
    if (!sameRealVector(empty_pmin_result.get(), empty_pmin_ref.get()) or
        !sameRealVector(empty_pmax_result.get(), empty_pmax_ref.get()) or
        !sameRealVector(empty_cumsum_result.get(), empty_cumsum_ref.get()) or
        !sameReal(zigr_convert.sum(empty_real.get()), R.REAL(empty_sum_ref.get())[0]) or
        !sameReal(zigr_convert.mean(empty_real.get()), R.REAL(empty_mean_ref.get())[0]) or
        !sameReal(zigr_convert.norm2(empty_real.get()), R.REAL(empty_norm_ref.get())[0]) or
        !sameReal(zigr_convert.min(empty_real.get()), R.REAL(empty_min_ref.get())[0]) or
        !sameReal(zigr_convert.max(empty_real.get()), R.REAL(empty_max_ref.get())[0]) or
        !sameReal(zigr_convert.sum_narm(empty_real.get()), R.REAL(empty_sum_narm_ref.get())[0]) or
        !sameReal(zigr_convert.mean_narm(empty_real.get()), R.REAL(empty_mean_narm_ref.get())[0])) return R.Rf_ScalarReal(0.0);

    var missing = reference("c(NA_real_, NaN, 2, 4, 5, 6, 7, 8)");
    defer missing.deinit();
    var sum_narm_ref = reference("sum(c(NA_real_, NaN, 2, 4, 5, 6, 7, 8), na.rm=TRUE)");
    defer sum_narm_ref.deinit();
    var mean_narm_ref = reference("mean(c(NA_real_, NaN, 2, 4, 5, 6, 7, 8), na.rm=TRUE)");
    defer mean_narm_ref.deinit();
    var sum_missing_ref = reference("sum(c(NA_real_, NaN, 2, 4, 5, 6, 7, 8))");
    defer sum_missing_ref.deinit();
    var mean_missing_ref = reference("mean(c(NA_real_, NaN, 2, 4, 5, 6, 7, 8))");
    defer mean_missing_ref.deinit();
    var norm_missing_ref = reference("sum(c(NA_real_, NaN, 2, 4, 5, 6, 7, 8)^2)");
    defer norm_missing_ref.deinit();
    var scale_missing_ref = reference("sum(c(NA_real_, NaN, 2, 4, 5, 6, 7, 8) * 2 + 3)");
    defer scale_missing_ref.deinit();
    var min_missing_ref = reference("min(c(NA_real_, NaN, 2, 4, 5, 6, 7, 8))");
    defer min_missing_ref.deinit();
    var max_missing_ref = reference("max(c(NA_real_, NaN, 2, 4, 5, 6, 7, 8))");
    defer max_missing_ref.deinit();
    var argmin_missing_ref = reference("as.numeric(which.min(c(NA_real_, NaN, 2, 4, 5, 6, 7, 8)) - 1)");
    defer argmin_missing_ref.deinit();
    var argmax_missing_ref = reference("as.numeric(which.max(c(NA_real_, NaN, 2, 4, 5, 6, 7, 8)) - 1)");
    defer argmax_missing_ref.deinit();
    var pairwise = reference("c(1, NA_real_, 3, NaN)");
    defer pairwise.deinit();
    var pairwise_other = reference("c(2, 3, 2, 4)");
    defer pairwise_other.deinit();
    var pmin_ref = reference("pmin(c(1, NA_real_, 3, NaN), c(2, 3, 2, 4))");
    defer pmin_ref.deinit();
    var pmax_ref = reference("pmax(c(1, NA_real_, 3, NaN), c(2, 3, 2, 4))");
    defer pmax_ref.deinit();
    var signed_pair = reference("c(-0.0, 0.0)");
    defer signed_pair.deinit();
    var signed_pair_other = reference("c(0.0, -0.0)");
    defer signed_pair_other.deinit();
    var signed_pmin_ref = reference("pmin(c(-0.0, 0.0), c(0.0, -0.0))");
    defer signed_pmin_ref.deinit();
    var signed_pmax_ref = reference("pmax(c(-0.0, 0.0), c(0.0, -0.0))");
    defer signed_pmax_ref.deinit();
    var missing_cumsum_ref = reference("cumsum(c(1, NaN, 3, NA_real_, 5))");
    defer missing_cumsum_ref.deinit();
    var cumsum_missing_input = reference("c(1, NaN, 3, NA_real_, 5)");
    defer cumsum_missing_input.deinit();

    if (!sameReal(zigr_convert.sum(missing.get()), R.REAL(sum_missing_ref.get())[0]) or
        !sameReal(zigr_convert.mean(missing.get()), R.REAL(mean_missing_ref.get())[0]) or
        !sameReal(zigr_convert.norm2(missing.get()), R.REAL(norm_missing_ref.get())[0]) or
        !sameReal(zigr_convert.scaleAdd(missing.get(), 2.0, 3.0), R.REAL(scale_missing_ref.get())[0]) or
        zigr_convert.argmin(missing.get()) != @as(i64, @intFromFloat(R.REAL(argmin_missing_ref.get())[0])) or
        zigr_convert.argmax(missing.get()) != @as(i64, @intFromFloat(R.REAL(argmax_missing_ref.get())[0])) or
        !sameReal(zigr_convert.sum_narm(missing.get()), R.REAL(sum_narm_ref.get())[0]) or
        !sameReal(zigr_convert.mean_narm(missing.get()), R.REAL(mean_narm_ref.get())[0]) or
        !sameReal(zigr_convert.min(missing.get()), R.REAL(min_missing_ref.get())[0]) or
        !sameReal(zigr_convert.max(missing.get()), R.REAL(max_missing_ref.get())[0]))
    {
        return R.Rf_ScalarReal(0.0);
    }
    var all_missing = reference("c(NA_real_, NaN)");
    defer all_missing.deinit();
    if (!sameReal(zigr_convert.mean_narm(all_missing.get()), R.REAL(all_missing_mean_narm_ref.get())[0])) {
        return R.Rf_ScalarReal(0.0);
    }
    var pmin_result = protect.scoped(zigr_convert.pmin(pairwise.get(), pairwise_other.get()));
    defer pmin_result.deinit();
    var pmax_result = protect.scoped(zigr_convert.pmax(pairwise.get(), pairwise_other.get()));
    defer pmax_result.deinit();
    var signed_pmin_result = protect.scoped(zigr_convert.pmin(signed_pair.get(), signed_pair_other.get()));
    defer signed_pmin_result.deinit();
    var signed_pmax_result = protect.scoped(zigr_convert.pmax(signed_pair.get(), signed_pair_other.get()));
    defer signed_pmax_result.deinit();
    var cumsum_missing = protect.scoped(zigr_convert.cumsum(cumsum_missing_input.get()));
    defer cumsum_missing.deinit();
    if (!sameRealVector(pmin_result.get(), pmin_ref.get()) or !sameRealVector(pmax_result.get(), pmax_ref.get()) or
        !sameRealVector(signed_pmin_result.get(), signed_pmin_ref.get()) or !sameRealVector(signed_pmax_result.get(), signed_pmax_ref.get()) or
        !sameRealVector(cumsum_missing.get(), missing_cumsum_ref.get())) return R.Rf_ScalarReal(0.0);

    var integer = reference("c(-2147483647L, 0L, 2147483647L)");
    defer integer.deinit();
    var integer_sum_ref = reference("sum(as.numeric(c(-2147483647L, 0L, 2147483647L)))");
    defer integer_sum_ref.deinit();
    var integer_min_ref = reference("min(c(-2147483647L, 0L, 2147483647L))");
    defer integer_min_ref.deinit();
    var integer_max_ref = reference("max(c(-2147483647L, 0L, 2147483647L))");
    defer integer_max_ref.deinit();
    var integer_argmin_ref = reference("as.numeric(which.min(c(-2147483647L, 0L, 2147483647L)) - 1)");
    defer integer_argmin_ref.deinit();
    var integer_argmax_ref = reference("as.numeric(which.max(c(-2147483647L, 0L, 2147483647L)) - 1)");
    defer integer_argmax_ref.deinit();
    if (zigr_convert.sumInt(integer.get()) != @as(i64, @intFromFloat(R.REAL(integer_sum_ref.get())[0])) or
        zigr_convert.minInt(integer.get()) != R.INTEGER(integer_min_ref.get())[0] or
        zigr_convert.maxInt(integer.get()) != R.INTEGER(integer_max_ref.get())[0] or
        zigr_convert.argminInt(integer.get()) != @as(i64, @intFromFloat(R.REAL(integer_argmin_ref.get())[0])) or
        zigr_convert.argmaxInt(integer.get()) != @as(i64, @intFromFloat(R.REAL(integer_argmax_ref.get())[0]))) return R.Rf_ScalarReal(0.0);

    var logical = reference("c(FALSE, TRUE, NA, TRUE)");
    defer logical.deinit();
    var logical_count_ref = reference("as.numeric(sum(c(FALSE, TRUE, NA, TRUE) == TRUE, na.rm=TRUE))");
    defer logical_count_ref.deinit();
    var logical_min_ref = reference("as.numeric(min(as.integer(c(FALSE, TRUE, NA, TRUE)), na.rm=TRUE))");
    defer logical_min_ref.deinit();
    var logical_max_ref = reference("as.numeric(max(as.integer(c(FALSE, TRUE, NA, TRUE)), na.rm=TRUE))");
    defer logical_max_ref.deinit();
    var logical_argmin_ref = reference("as.numeric(which.min(c(FALSE, TRUE, NA, TRUE)) - 1)");
    defer logical_argmin_ref.deinit();
    var logical_argmax_ref = reference("as.numeric(which.max(c(FALSE, TRUE, NA, TRUE)) - 1)");
    defer logical_argmax_ref.deinit();
    if (zigr_convert.countTrue(logical.get()) != @as(i64, @intFromFloat(R.REAL(logical_count_ref.get())[0])) or
        zigr_convert.minLogical(logical.get()) != @as(i32, @intFromFloat(R.REAL(logical_min_ref.get())[0])) or
        zigr_convert.maxLogical(logical.get()) != @as(i32, @intFromFloat(R.REAL(logical_max_ref.get())[0])) or
        zigr_convert.argminLogical(logical.get()) != @as(i64, @intFromFloat(R.REAL(logical_argmin_ref.get())[0])) or
        zigr_convert.argmaxLogical(logical.get()) != @as(i64, @intFromFloat(R.REAL(logical_argmax_ref.get())[0]))) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_raw_complex_reference() SEXP {
    if (!initBoundaryExports()) return R.Rf_ScalarReal(0.0);
    var raw = reference("as.raw(c(0, 255, 127, 1))");
    defer raw.deinit();
    var raw_sum_ref = reference("as.numeric(sum(as.integer(as.raw(c(0, 255, 127, 1)))))");
    defer raw_sum_ref.deinit();
    var raw_sum = protect.scoped(boundaryCall(3, raw.get()));
    defer raw_sum.deinit();
    var raw_echo = protect.scoped(boundaryCall(10, raw.get()));
    defer raw_echo.deinit();
    if (R.TYPEOF(raw_sum.get()) != R.INTSXP or R.INTEGER(raw_sum.get())[0] != @as(i32, @intFromFloat(R.REAL(raw_sum_ref.get())[0])) or
        !sameRawVector(raw_echo.get(), raw.get())) return R.Rf_ScalarReal(0.0);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const raw_slice = zigr_convert.toRawSlice(arena.allocator(), raw.get()) catch return R.Rf_ScalarReal(0.0);
    var raw_roundtrip = protect.scoped(zigr_convert.fromRawSlice(raw_slice));
    defer raw_roundtrip.deinit();
    if (!sameRawVector(raw_roundtrip.get(), raw.get())) return R.Rf_ScalarReal(0.0);
    var empty_raw = reference("raw(0)");
    defer empty_raw.deinit();
    const empty_raw_slice = zigr_convert.toRawSlice(arena.allocator(), empty_raw.get()) catch return R.Rf_ScalarReal(0.0);
    var empty_raw_result = protect.scoped(zigr_convert.fromRawSlice(empty_raw_slice));
    defer empty_raw_result.deinit();
    if (!sameRawVector(empty_raw_result.get(), empty_raw.get())) return R.Rf_ScalarReal(0.0);

    var complex = reference("complex(real=c(1, NaN, NA_real_), imaginary=c(-2, NA_real_, NaN))");
    defer complex.deinit();
    var complex_echo = protect.scoped(boundaryCall(4, complex.get()));
    defer complex_echo.deinit();
    if (!sameComplexVector(complex_echo.get(), complex.get())) return R.Rf_ScalarReal(0.0);
    const complex_slice = zigr_convert.toComplexSlice(arena.allocator(), complex.get()) catch return R.Rf_ScalarReal(0.0);
    var complex_roundtrip = protect.scoped(zigr_convert.fromComplexSlice(complex_slice));
    defer complex_roundtrip.deinit();
    if (!sameComplexVector(complex_roundtrip.get(), complex.get())) return R.Rf_ScalarReal(0.0);
    var empty_complex = reference("complex(length=0)");
    defer empty_complex.deinit();
    const empty_complex_slice = zigr_convert.toComplexSlice(arena.allocator(), empty_complex.get()) catch return R.Rf_ScalarReal(0.0);
    var empty_complex_result = protect.scoped(zigr_convert.fromComplexSlice(empty_complex_slice));
    defer empty_complex_result.deinit();
    if (!sameComplexVector(empty_complex_result.get(), empty_complex.get())) return R.Rf_ScalarReal(0.0);

    var logical = reference("c(FALSE, TRUE, NA, TRUE)");
    defer logical.deinit();
    var logical_ref = reference("as.numeric(sum(c(FALSE, TRUE, NA, TRUE) == FALSE, na.rm=TRUE) * 100 + sum(c(FALSE, TRUE, NA, TRUE) == TRUE, na.rm=TRUE) * 10 + sum(is.na(c(FALSE, TRUE, NA, TRUE))))");
    defer logical_ref.deinit();
    var logical_count = protect.scoped(boundaryCall(5, logical.get()));
    defer logical_count.deinit();
    const logical_value = R.INTEGER(logical_count.get());
    if (R.TYPEOF(logical_count.get()) != R.INTSXP or logical_value[0] != @as(i32, @intFromFloat(R.REAL(logical_ref.get())[0]))) {
        return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_result_reference() SEXP {
    if (!initArenaExports()) return R.Rf_ScalarReal(0.0);
    var input = reference("c(-2.5, NA_real_, 3.25, -0.0)");
    defer input.deinit();
    var expected = reference("c(-2.5, NA_real_, 3.25, -0.0) * 2");
    defer expected.deinit();
    var result = protect.scoped(arenaCall(10, input.get()));
    defer result.deinit();
    var second = protect.scoped(arenaCall(10, input.get()));
    defer second.deinit();
    if (result.get() == input.get() or result.get() == second.get() or !sameRealVector(result.get(), expected.get()) or
        R.Rf_getAttrib(result.get(), R.R_NamesSymbol) != R.R_NilValue) return R.Rf_ScalarReal(0.0);

    var raw = reference("as.raw(c(0, 255, 127))");
    defer raw.deinit();
    var raw_result = protect.scoped(arenaCall(11, raw.get()));
    defer raw_result.deinit();
    if (raw_result.get() == raw.get() or !sameRawVector(raw_result.get(), raw.get())) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

fn strategyResult(comptime T: type, input: SEXP, strategy: zigr_convert.AccessStrategy) !SEXP {
    var result = try zigr_convert.ResultBuilder(T).initFromInput(input);
    defer result.deinit();
    var access = try zigr_convert.toVectorAccessWithStrategy(T, .random_access, std.heap.page_allocator, input, strategy);
    defer access.deinit();
    if (access.strategy() != strategy) return error.UnexpectedStrategy;

    const output = result.mutableSlice();
    var offset: usize = 0;
    while (try access.next()) |chunk| {
        if (chunk.len > output.len -| offset) return error.VectorLengthMismatch;
        @memcpy(output[offset .. offset + chunk.len], chunk);
        offset += chunk.len;
    }
    if (offset != output.len) return error.VectorLengthMismatch;
    return result.finish();
}

fn automaticResult(comptime T: type, comptime need: zigr_convert.AccessNeed, input: SEXP) !SEXP {
    var result = try zigr_convert.ResultBuilder(T).initFromInput(input);
    defer result.deinit();
    var access = try zigr_convert.toVectorAccess(T, need, std.heap.page_allocator, input);
    defer access.deinit();

    const output = result.mutableSlice();
    var offset: usize = 0;
    while (try access.next()) |chunk| {
        if (chunk.len > output.len -| offset) return error.VectorLengthMismatch;
        @memcpy(output[offset .. offset + chunk.len], chunk);
        offset += chunk.len;
    }
    if (offset != output.len) return error.VectorLengthMismatch;
    return result.finish();
}

fn sameAtomicVector(comptime T: type, actual: SEXP, expected: SEXP) bool {
    return switch (T) {
        f64 => sameRealVector(actual, expected),
        i32 => blk: {
            if (R.TYPEOF(actual) != R.INTSXP or R.TYPEOF(expected) != R.INTSXP or R.XLENGTH(actual) != R.XLENGTH(expected)) break :blk false;
            const n = @as(usize, @intCast(R.XLENGTH(actual)));
            break :blk std.mem.eql(i32, R.INTEGER(actual)[0..n], R.INTEGER(expected)[0..n]);
        },
        u8 => sameRawVector(actual, expected),
        zigr_convert.Rcomplex => sameComplexVector(actual, expected),
        else => @compileError("unsupported strategy vector type: " ++ @typeName(T)),
    };
}

fn resultMatches(comptime T: type, actual: SEXP, expected: SEXP) bool {
    return sameAtomicVector(T, actual, expected) and
        R.ALTREP(actual) == 0 and
        R.Rf_getAttrib(actual, R.R_NamesSymbol) == R.R_NilValue;
}

fn checkForced(comptime T: type, input: SEXP, expected: SEXP, strategy: zigr_convert.AccessStrategy) bool {
    var result = protect.scoped(strategyResult(T, input, strategy) catch return false);
    defer result.deinit();
    return result.get() != input and resultMatches(T, result.get(), expected);
}

fn checkAutomatic(
    comptime T: type,
    comptime need: zigr_convert.AccessNeed,
    input: SEXP,
    expected: SEXP,
    expected_strategy: zigr_convert.AccessStrategy,
) bool {
    var access = zigr_convert.toVectorAccess(T, need, std.heap.page_allocator, input) catch return false;
    if (access.strategy() != expected_strategy) {
        access.deinit();
        return false;
    }
    access.deinit();

    var result = protect.scoped(automaticResult(T, need, input) catch return false);
    defer result.deinit();
    return result.get() != input and resultMatches(T, result.get(), expected);
}

fn checkInputState(comptime T: type, input: SEXP, expected_direct: bool) bool {
    const direct = switch (T) {
        f64 => R.REAL_OR_NULL(input) != null,
        i32 => R.INTEGER_OR_NULL(input) != null,
        u8 => R.RAW_OR_NULL(input) != null,
        zigr_convert.Rcomplex => R.COMPLEX_OR_NULL(input) != null,
        else => @compileError("unsupported strategy input type: " ++ @typeName(T)),
    };
    return direct == expected_direct;
}

fn checkInputStrategies(comptime T: type, input: SEXP, expected: SEXP, expected_direct: bool) bool {
    if (!checkInputState(T, input, expected_direct)) return false;
    if (!checkForced(T, input, expected, .direct) or
        !checkForced(T, input, expected, .region) or
        !checkForced(T, input, expected, .materialized)) return false;
    if (!checkAutomatic(T, .one_pass, input, expected, .direct) or
        !checkAutomatic(T, .random_access, input, expected, .direct)) return false;
    return checkInputState(T, input, expected_direct);
}

fn checkDirectUnavailable(comptime T: type, input: SEXP) bool {
    const result = zigr_convert.toVectorAccessWithStrategy(T, .one_pass, std.heap.page_allocator, input, .direct);
    if (result) |access| {
        var unexpected = access;
        unexpected.deinit();
        return false;
    } else |access_error| {
        return access_error == error.DirectPointerUnavailable;
    }
}

fn checkFailingRawStrategies(input: SEXP) bool {
    const direct = zigr_convert.toVectorAccessWithStrategy(u8, .one_pass, std.heap.page_allocator, input, .direct);
    if (direct) |access| {
        var unexpected = access;
        unexpected.deinit();
        return false;
    } else |access_error| {
        if (access_error != error.DirectPointerUnavailable) return false;
    }

    var region = zigr_convert.toVectorAccessWithStrategy(u8, .one_pass, std.heap.page_allocator, input, .region) catch return false;
    defer region.deinit();
    if (region.strategy() != .region or region.contiguousSlice() != null) return false;
    if (region.next()) |_| {
        return false;
    } else |access_error| {
        if (access_error != error.AltRepRegionRead) return false;
    }

    const materialized = zigr_convert.toVectorAccessWithStrategy(u8, .random_access, std.heap.page_allocator, input, .materialized);
    if (materialized) |access| {
        var unexpected = access;
        unexpected.deinit();
        return false;
    } else |access_error| {
        return access_error == error.AltRepRegionRead;
    }
}

fn charsxpEqualsBytes(charsxp: SEXP, expected: []const u8) bool {
    if (charsxp == R.R_NaString or R.TYPEOF(charsxp) != R.CHARSXP or R.XLENGTH(charsxp) != expected.len) return false;
    return std.mem.eql(u8, R.R_CHAR(charsxp)[0..expected.len], expected);
}

fn generatedStringSummariesMatch(input: SEXP) bool {
    const length = @as(usize, @intCast(R.XLENGTH(input)));
    const identity_result = R.Rf_protect(boundaryCall(11, input));
    const missingness_result = R.Rf_protect(boundaryCall(12, input));
    const bytes_result = R.Rf_protect(boundaryCall(13, input));
    const encoding_result = R.Rf_protect(boundaryCall(14, input));
    const translated_result = R.Rf_protect(boundaryCall(15, input));
    const metadata_result = R.Rf_protect(boundaryCall(16, input));
    defer R.Rf_unprotect(6);

    var missing_ref = protect.scoped(test_eval.callIn("is.na", &.{input}, R.R_BaseEnv));
    defer missing_ref.deinit();
    var encoding_ref = protect.scoped(test_eval.callIn("Encoding", &.{input}, R.R_BaseEnv));
    defer encoding_ref.deinit();
    var translated_ref = protect.scoped(test_eval.callIn("enc2utf8", &.{input}, R.R_BaseEnv));
    defer translated_ref.deinit();

    var missing: i32 = 0;
    var stored_bytes: i32 = 0;
    var translated_bytes: i32 = 0;
    var utf8: i32 = 0;
    var latin1: i32 = 0;
    var bytes: i32 = 0;
    for (0..length) |index| {
        const offset: R.R_xlen_t = @intCast(index);
        const element = R.STRING_ELT(input, offset);
        if (R.LOGICAL(missing_ref.get())[index] != @as(i32, if (element == R.R_NaString) 1 else 0)) return false;
        if (element == R.R_NaString) {
            missing += 1;
            continue;
        }
        stored_bytes += @intCast(R.XLENGTH(element));
        translated_bytes += @intCast(R.XLENGTH(R.STRING_ELT(translated_ref.get(), offset)));
        const encoding = R.STRING_ELT(encoding_ref.get(), offset);
        if (charsxpEqualsBytes(encoding, "UTF-8")) {
            utf8 += 1;
        } else if (charsxpEqualsBytes(encoding, "latin1")) {
            latin1 += 1;
        } else if (charsxpEqualsBytes(encoding, "bytes")) {
            bytes += 1;
        }
    }

    if (R.TYPEOF(identity_result) != R.INTSXP or R.TYPEOF(missingness_result) != R.INTSXP or
        R.TYPEOF(bytes_result) != R.INTSXP or R.TYPEOF(encoding_result) != R.INTSXP or
        R.TYPEOF(translated_result) != R.INTSXP or R.TYPEOF(metadata_result) != R.INTSXP)
    {
        return false;
    }
    const metadata = stored_bytes + missing * 1000 + utf8 * 100 + latin1 * 10 + bytes;
    return R.INTEGER(identity_result)[0] == missing and
        R.INTEGER(missingness_result)[0] == @as(i32, @intCast(length)) - missing and
        R.INTEGER(bytes_result)[0] == stored_bytes and
        R.INTEGER(encoding_result)[0] == utf8 * 100 + latin1 * 10 + bytes and
        R.INTEGER(translated_result)[0] == translated_bytes and
        R.INTEGER(metadata_result)[0] == metadata;
}

fn stringProjectionViewsMatch(input: SEXP) bool {
    const length = @as(usize, @intCast(R.XLENGTH(input)));
    const identity = zigr_convert.toStringProjectionView(.identity, input) catch return false;
    const missingness = zigr_convert.toStringProjectionView(.missingness, input) catch return false;
    const bytes = zigr_convert.toStringProjectionView(.bytes, input) catch return false;
    const encoding = zigr_convert.toStringProjectionView(.encoding_mark, input) catch return false;
    const translated = zigr_convert.toStringProjectionView(.translated_text, input) catch return false;
    const metadata = zigr_convert.toStringProjectionView(.metadata, input) catch return false;
    var missing_ref = protect.scoped(test_eval.callIn("is.na", &.{input}, R.R_BaseEnv));
    defer missing_ref.deinit();
    var encoding_ref = protect.scoped(test_eval.callIn("Encoding", &.{input}, R.R_BaseEnv));
    defer encoding_ref.deinit();
    var translated_ref = protect.scoped(test_eval.callIn("enc2utf8", &.{input}, R.R_BaseEnv));
    defer translated_ref.deinit();

    for (0..length) |index| {
        const offset: R.R_xlen_t = @intCast(index);
        const element = R.STRING_ELT(input, offset);
        const identity_value = identity.at(index);
        if (identity_value.charsxp != element) return false;

        const missing_value = missingness.at(index);
        const expected_missing = R.LOGICAL(missing_ref.get())[index] != 0;
        if (missing_value.is_na != expected_missing) return false;

        const bytes_value = bytes.at(index);
        if (bytes_value.charsxp != element) return false;
        if (element == R.R_NaString) {
            if (bytes_value.bytes.len != 0) return false;
        } else {
            const stored_length = @as(usize, @intCast(R.XLENGTH(element)));
            if (bytes_value.bytes.len != stored_length or !std.mem.eql(u8, bytes_value.bytes, R.R_CHAR(element)[0..stored_length])) return false;
        }

        const encoding_value = encoding.at(index);
        if (encoding_value.charsxp != element or
            encoding_value.encoding_mark != R.Rf_getCharCE(element)) return false;
        const expected_encoding = switch (encoding_value.encoding_mark) {
            R.CE_UTF8 => "UTF-8",
            R.CE_LATIN1 => "latin1",
            R.CE_BYTES => "bytes",
            else => "unknown",
        };
        if (!charsxpEqualsBytes(R.STRING_ELT(encoding_ref.get(), offset), expected_encoding)) return false;

        const translated_value = translated.at(index);
        const expected_translated = R.STRING_ELT(translated_ref.get(), offset);
        if (translated_value.charsxp != element) return false;
        if (element == R.R_NaString) {
            if (expected_translated != R.R_NaString or translated_value.bytes.len != 0) return false;
        } else {
            const translated_length = @as(usize, @intCast(R.XLENGTH(expected_translated)));
            if (translated_value.bytes.len != translated_length or
                !std.mem.eql(u8, translated_value.bytes, R.R_CHAR(expected_translated)[0..translated_length])) return false;
        }

        const metadata_value = metadata.at(index);
        if (metadata_value.charsxp != element or metadata_value.is_na != expected_missing or
            metadata_value.encoding_mark != R.Rf_getCharCE(element)) return false;
    }
    return true;
}

export fn zigr_test_strategy_equivalence() SEXP {
    var real_ref = reference("c(-0.0, NaN, NA_real_, 3.25, -4.5)");
    defer real_ref.deinit();
    const real_values = [_]f64{ -0.0, std.math.nan(f64), R.NA_REAL(), 3.25, -4.5 };
    const real_alt = R.Rf_protect(MyAlt.init(real_values[0..]));
    defer R.Rf_unprotect(1);
    if (!checkInputStrategies(f64, real_ref.get(), real_ref.get(), true) or
        !checkInputStrategies(f64, real_alt, real_ref.get(), true)) return R.Rf_ScalarReal(0.0);

    var integer_ref = reference("c(-2147483647L, 0L, NA_integer_, 2147483647L)");
    defer integer_ref.deinit();
    const integer_values = [_]i32{ -2147483647, 0, R.R_NaInt, 2147483647 };
    const integer_alt = R.Rf_protect(MyAltInt.init(integer_values[0..]));
    defer R.Rf_unprotect(1);
    if (!checkInputStrategies(i32, integer_ref.get(), integer_ref.get(), true) or
        !checkInputStrategies(i32, integer_alt, integer_ref.get(), true)) return R.Rf_ScalarReal(0.0);

    var raw_ref = reference("as.raw(c(0, 255, 127, 1))");
    defer raw_ref.deinit();
    const raw_values = [_]u8{ 0, 255, 127, 1 };
    const raw_alt = R.Rf_protect(MyAltRaw.init(raw_values[0..]));
    defer R.Rf_unprotect(1);
    if (!checkInputStrategies(u8, raw_ref.get(), raw_ref.get(), true) or
        !checkInputStrategies(u8, raw_alt, raw_ref.get(), true)) return R.Rf_ScalarReal(0.0);

    var complex_ref = reference("complex(real=c(1, NA_real_, NaN), imaginary=c(-1, NaN, NA_real_))");
    defer complex_ref.deinit();
    const complex_values = [_]altrep_create.ComplexElem{
        .{ .r = 1.0, .i = -1.0 },
        .{ .r = R.NA_REAL(), .i = std.math.nan(f64) },
        .{ .r = std.math.nan(f64), .i = R.NA_REAL() },
    };
    const complex_alt = R.Rf_protect(MyAltComplex.init(complex_values[0..]));
    defer R.Rf_unprotect(1);
    if (!checkInputStrategies(zigr_convert.Rcomplex, complex_ref.get(), complex_ref.get(), true) or
        !checkInputStrategies(zigr_convert.Rcomplex, complex_alt, complex_ref.get(), true)) return R.Rf_ScalarReal(0.0);

    var empty_real_ref = reference("numeric(0)");
    defer empty_real_ref.deinit();
    const empty_real_values = [_]f64{};
    const empty_real_alt = R.Rf_protect(MyAlt.init(empty_real_values[0..]));
    defer R.Rf_unprotect(1);
    if (!checkInputStrategies(f64, empty_real_ref.get(), empty_real_ref.get(), true) or
        !checkInputStrategies(f64, empty_real_alt, empty_real_ref.get(), false)) return R.Rf_ScalarReal(0.0);

    var one_ref = reference("42.5");
    defer one_ref.deinit();
    const one_values = [_]f64{42.5};
    const one_alt = R.Rf_protect(MyAlt.init(one_values[0..]));
    defer R.Rf_unprotect(1);
    if (!checkInputStrategies(f64, one_ref.get(), one_ref.get(), true) or
        !checkInputStrategies(f64, one_alt, one_ref.get(), true)) return R.Rf_ScalarReal(0.0);

    var boundary_values: [257]i32 = undefined;
    for (boundary_values[0..], 0..) |*value, index| value.* = @intCast(index + 1);
    const boundary = R.Rf_protect(R.Rf_allocVector(R.INTSXP, @intCast(boundary_values.len)));
    defer R.Rf_unprotect(1);
    @memcpy(R.INTEGER(boundary)[0..boundary_values.len], &boundary_values);
    const boundary_alt = R.Rf_protect(MyAltInt.init(boundary_values[0..]));
    defer R.Rf_unprotect(1);
    if (!checkInputStrategies(i32, boundary, boundary, true) or
        !checkInputStrategies(i32, boundary_alt, boundary, true)) return R.Rf_ScalarReal(0.0);

    const region_input = R.Rf_protect(shortRegionAltInteger());
    defer R.Rf_unprotect(1);
    var region_ref = reference("seq_len(4097L)");
    defer region_ref.deinit();
    const ordinary_region = R.Rf_protect(R.Rf_allocVector(R.INTSXP, @intCast(short_region_len)));
    defer R.Rf_unprotect(1);
    for (0..short_region_len) |index| R.INTEGER(ordinary_region)[index] = @intCast(index + 1);
    if (!checkInputStrategies(i32, ordinary_region, region_ref.get(), true) or
        !checkInputState(i32, region_input, false) or !checkDirectUnavailable(i32, region_input) or
        !checkForced(i32, region_input, region_ref.get(), .region) or
        !checkForced(i32, region_input, region_ref.get(), .materialized) or
        !checkAutomatic(i32, .one_pass, region_input, region_ref.get(), .region) or
        !checkAutomatic(i32, .random_access, region_input, region_ref.get(), .materialized) or
        !checkInputState(i32, region_input, false)) return R.Rf_ScalarReal(0.0);

    const failing = R.Rf_protect(failingRawRegion());
    defer R.Rf_unprotect(1);
    if (!checkFailingRawStrategies(failing)) return R.Rf_ScalarReal(0.0);

    const five_million = compactIntSequence(5_000_000) orelse return R.Rf_ScalarReal(0.0);
    defer R.Rf_unprotect(1);
    if (R.ALTREP(five_million) == 0 or R.INTEGER_OR_NULL(five_million) != null) return R.Rf_ScalarReal(0.0);
    var materialized = protect.scoped(automaticResult(i32, .random_access, five_million) catch return R.Rf_ScalarReal(0.0));
    defer materialized.deinit();
    if (R.TYPEOF(materialized.get()) != R.INTSXP or R.ALTREP(materialized.get()) != 0 or
        R.XLENGTH(materialized.get()) != 5_000_000 or R.INTEGER(materialized.get())[0] != 1 or
        R.INTEGER(materialized.get())[4_999_999] != 5_000_000 or
        !checkInputState(i32, five_million, false)) return R.Rf_ScalarReal(0.0);
    for (0..5_000_000) |index| {
        if (R.INTEGER(materialized.get())[index] != @as(i32, @intCast(index + 1))) return R.Rf_ScalarReal(0.0);
    }

    if (!initArenaExports()) return R.Rf_ScalarReal(0.0);
    var direct_expected = reference("c(-0.0, NaN, NA_real_, 3.25, -4.5) * 2");
    defer direct_expected.deinit();
    const generated_real = R.Rf_protect(arenaCall(10, real_alt));
    defer R.Rf_unprotect(1);
    const generated_raw = R.Rf_protect(arenaCall(11, raw_alt));
    defer R.Rf_unprotect(1);
    if (generated_real == real_alt or generated_raw == raw_alt or
        !sameRealVector(generated_real, direct_expected.get()) or R.ALTREP(generated_real) != 0 or
        R.Rf_getAttrib(generated_real, R.R_NamesSymbol) != R.R_NilValue or
        !sameRawVector(generated_raw, raw_ref.get()) or R.ALTREP(generated_raw) != 0 or
        R.Rf_getAttrib(generated_raw, R.R_NamesSymbol) != R.R_NilValue or
        !checkInputState(f64, real_alt, true) or !checkInputState(u8, raw_alt, true)) return R.Rf_ScalarReal(0.0);

    const public_materialized = R.Rf_protect(arenaCall(8, five_million));
    defer R.Rf_unprotect(1);
    if (public_materialized == five_million or R.TYPEOF(public_materialized) != R.INTSXP or
        R.ALTREP(public_materialized) != 0 or R.XLENGTH(public_materialized) != 5_000_000 or
        R.INTEGER(public_materialized)[0] != 1 or R.INTEGER(public_materialized)[4_999_999] != 5_000_000 or
        !checkInputState(i32, five_million, false)) return R.Rf_ScalarReal(0.0);

    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_string_projection_semantics() SEXP {
    if (!initBoundaryExports()) return R.Rf_ScalarReal(0.0);

    const native = [_]u8{ 'n', 'a', 't', 'i', 'v', 'e' };
    const utf8 = [_]u8{ 'c', 'a', 'f', 0xc3, 0xa9 };
    const latin1 = [_]u8{ 'c', 'a', 'f', 0xe9 };
    const byte_marked = [_]u8{ 'x', 0xff, 'y' };
    const vector = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 6));
    defer R.Rf_unprotect(1);
    R.SET_STRING_ELT(vector, 0, R.Rf_mkCharLenCE(@ptrCast(&native), @intCast(native.len), @as(R.cetype_t, @intCast(R.CE_NATIVE))));
    R.SET_STRING_ELT(vector, 1, R.Rf_mkCharLenCE(@ptrCast(&utf8), @intCast(utf8.len), @as(R.cetype_t, @intCast(R.CE_UTF8))));
    R.SET_STRING_ELT(vector, 2, R.Rf_mkCharLenCE(@ptrCast(&latin1), @intCast(latin1.len), @as(R.cetype_t, @intCast(R.CE_LATIN1))));
    R.SET_STRING_ELT(vector, 3, R.Rf_mkCharLenCE(@ptrCast(&byte_marked), @intCast(byte_marked.len), @as(R.cetype_t, @intCast(R.CE_BYTES))));
    R.SET_STRING_ELT(vector, 4, R.Rf_mkCharLenCE("", 0, @as(R.cetype_t, @intCast(R.CE_UTF8))));
    R.SET_STRING_ELT(vector, 5, R.R_NaString);

    if (!generatedStringSummariesMatch(vector)) return R.Rf_ScalarReal(0.0);
    if (!stringProjectionViewsMatch(vector)) return R.Rf_ScalarReal(0.0);

    const altrep_values = [_][]const u8{ "alpha", "", "omega" };
    const altrep = R.Rf_protect(MyAltString.init(altrep_values[0..]));
    defer R.Rf_unprotect(1);
    if (!generatedStringSummariesMatch(altrep)) return R.Rf_ScalarReal(0.0);
    if (!stringProjectionViewsMatch(altrep)) return R.Rf_ScalarReal(0.0);

    var nul_rejected = reference("isTRUE(inherits(tryCatch(rawToChar(as.raw(c(97, 0, 98))), error=function(e) e), 'error'))");
    defer nul_rejected.deinit();
    if (R.TYPEOF(nul_rejected.get()) != R.LGLSXP or R.LOGICAL(nul_rejected.get())[0] == 0) return R.Rf_ScalarReal(0.0);
    const nul_condition = trycatch_mod.tryCatchError(embeddedNulConstructor) catch return R.Rf_ScalarReal(0.0);
    if (nul_condition == null or std.mem.indexOf(u8, trycatch_mod.extractMessage(nul_condition.?), "embedded nul") == null) {
        return R.Rf_ScalarReal(0.0);
    }

    const long_input = R.Rf_protect(longStringAlt());
    defer R.Rf_unprotect(1);
    const long_index: usize = @intCast(long_string_len - 1);
    const long_identity = zigr_convert.toStringProjectionView(.identity, long_input) catch return R.Rf_ScalarReal(0.0);
    if (long_identity.len != @as(usize, @intCast(long_string_len)) or !charsxpEqualsBytes(long_identity.at(long_index).charsxp, "tail")) {
        return R.Rf_ScalarReal(0.0);
    }
    const long_bytes = zigr_convert.toStringProjectionView(.bytes, long_input) catch return R.Rf_ScalarReal(0.0);
    if (!std.mem.eql(u8, long_bytes.at(long_index).bytes, "tail")) return R.Rf_ScalarReal(0.0);

    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_object_semantics() SEXP {
    if (!initObjectExports() or !defineS4ContractClass()) return R.Rf_ScalarReal(0.0);

    var list_input = reference("list(first=1.5, nested=list(NULL, 2L), empty=NULL)");
    defer list_input.deinit();
    const list_result = R.Rf_protect(objectCall(0, list_input.get()));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(list_result) != R.INTSXP or R.XLENGTH(list_result) != 1 or R.INTEGER(list_result)[0] != 1) {
        return R.Rf_ScalarReal(0.0);
    }

    var wrong_list = reference("1:2");
    defer wrong_list.deinit();
    object_error_input = wrong_list.get();
    const wrong_container_condition = trycatch_mod.tryCatchError(objectListWrongTypeCall) catch {
        object_error_input = null;
        return R.Rf_ScalarReal(0.0);
    };
    object_error_input = null;
    if (wrong_container_condition == null or
        !std.mem.eql(u8, trycatch_mod.extractMessage(wrong_container_condition.?), "expected VECSXP")) return R.Rf_ScalarReal(0.0);

    var wrong_element = reference("list(first='wrong', nested=list(NULL, 2L), empty=NULL)");
    defer wrong_element.deinit();
    object_error_input = wrong_element.get();
    const wrong_element_condition = trycatch_mod.tryCatchError(objectListWrongTypeCall) catch {
        object_error_input = null;
        return R.Rf_ScalarReal(0.0);
    };
    object_error_input = null;
    if (wrong_element_condition == null or
        !std.mem.eql(u8, trycatch_mod.extractMessage(wrong_element_condition.?), "expected REALSXP")) return R.Rf_ScalarReal(0.0);

    var schema_input = reference("list(id=7L, count=2L, ratio=0.5, enabled=TRUE)");
    defer schema_input.deinit();
    const schema_result = R.Rf_protect(objectCall(1, schema_input.get()));
    defer R.Rf_unprotect(1);
    const schema_names = R.Rf_getAttrib(schema_result, R.R_NamesSymbol);
    if (schema_result == schema_input.get() or R.TYPEOF(schema_result) != R.VECSXP or R.XLENGTH(schema_result) != 4 or
        R.TYPEOF(schema_names) != R.STRSXP or R.XLENGTH(schema_names) != 4 or
        !charsxpEqualsBytes(R.STRING_ELT(schema_names, 0), "id") or
        !charsxpEqualsBytes(R.STRING_ELT(schema_names, 1), "count") or
        !charsxpEqualsBytes(R.STRING_ELT(schema_names, 2), "ratio") or
        !charsxpEqualsBytes(R.STRING_ELT(schema_names, 3), "enabled") or
        R.TYPEOF(R.VECTOR_ELT(schema_result, 0)) != R.INTSXP or R.INTEGER(R.VECTOR_ELT(schema_result, 0))[0] != 7 or
        R.TYPEOF(R.VECTOR_ELT(schema_result, 1)) != R.INTSXP or R.INTEGER(R.VECTOR_ELT(schema_result, 1))[0] != 2 or
        R.TYPEOF(R.VECTOR_ELT(schema_result, 2)) != R.REALSXP or R.REAL(R.VECTOR_ELT(schema_result, 2))[0] != 0.5 or
        R.TYPEOF(R.VECTOR_ELT(schema_result, 3)) != R.LGLSXP or R.LOGICAL(R.VECTOR_ELT(schema_result, 3))[0] != 1)
    {
        return R.Rf_ScalarReal(0.0);
    }
    var schema_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer schema_arena.deinit();
    const direct_schema = zigr_convert.tryFromSEXP(SemanticSchema, schema_input.get(), schema_arena.allocator()) catch
        return R.Rf_ScalarReal(0.0);
    if (direct_schema.id != 7 or direct_schema.count != 2 or direct_schema.ratio != 0.5 or !direct_schema.enabled) {
        return R.Rf_ScalarReal(0.0);
    }

    var attribute_input = reference("structure(c(1, 2, 3), names=c('a', 'b', 'c'))");
    defer attribute_input.deinit();
    const attribute_result = R.Rf_protect(objectCall(2, attribute_input.get()));
    defer R.Rf_unprotect(1);
    const attribute_class = R.Rf_getAttrib(attribute_result, R.R_ClassSymbol);
    const attribute_creator = R.Rf_getAttrib(attribute_result, test_lang.symbol("creator"));
    const attribute_names = R.Rf_getAttrib(attribute_result, R.R_NamesSymbol);
    if (attribute_result == attribute_input.get() or !sameRealVector(attribute_result, attribute_input.get()) or
        R.TYPEOF(attribute_class) != R.STRSXP or R.XLENGTH(attribute_class) != 1 or
        !charsxpEqualsBytes(R.STRING_ELT(attribute_class, 0), "zigr_semantic") or
        R.TYPEOF(attribute_creator) != R.STRSXP or R.XLENGTH(attribute_creator) != 1 or
        !charsxpEqualsBytes(R.STRING_ELT(attribute_creator, 0), "runtime") or
        R.TYPEOF(attribute_names) != R.STRSXP or R.XLENGTH(attribute_names) != 3 or
        !charsxpEqualsBytes(R.STRING_ELT(attribute_names, 0), "a") or
        !charsxpEqualsBytes(R.STRING_ELT(attribute_names, 1), "b") or
        !charsxpEqualsBytes(R.STRING_ELT(attribute_names, 2), "c") or
        R.Rf_getAttrib(attribute_input.get(), R.R_ClassSymbol) != R.R_NilValue or
        R.Rf_getAttrib(attribute_input.get(), test_lang.symbol("creator")) != R.R_NilValue)
    {
        return R.Rf_ScalarReal(0.0);
    }
    var attribute_expected = reference("local({ x <- structure(c(1, 2, 3), names=c('a', 'b', 'c')); y <- x[]; class(y) <- 'zigr_semantic'; attr(y, 'creator') <- 'runtime'; dim(y) <- c(3L, 1L); y })");
    defer attribute_expected.deinit();
    if (R.TYPEOF(attribute_expected.get()) != R.REALSXP or R.XLENGTH(attribute_expected.get()) != 3 or
        R.TYPEOF(R.Rf_getAttrib(attribute_expected.get(), R.R_ClassSymbol)) != R.STRSXP)
    {
        return R.Rf_ScalarReal(0.0);
    }
    if (!sameRealVector(attribute_result, attribute_expected.get())) return R.Rf_ScalarReal(0.0);
    const attribute_dims = R.Rf_getAttrib(attribute_result, R.R_DimSymbol);
    if (R.TYPEOF(attribute_dims) != R.INTSXP or R.XLENGTH(attribute_dims) != 2 or
        R.INTEGER(attribute_dims)[0] != 3 or R.INTEGER(attribute_dims)[1] != 1) return R.Rf_ScalarReal(0.0);
    R.REAL(attribute_result)[0] = 9.0;
    if (R.REAL(attribute_input.get())[0] != 1.0) return R.Rf_ScalarReal(0.0);

    var factor_input = reference("c('b', 'a', 'b', NA_character_)");
    defer factor_input.deinit();
    const factor_result = R.Rf_protect(objectCall(3, factor_input.get()));
    defer R.Rf_unprotect(1);
    var factor_expected = reference("factor(c('b', 'a', 'b', NA_character_))");
    defer factor_expected.deinit();
    const factor_levels = R.Rf_getAttrib(factor_result, R.R_LevelsSymbol);
    const factor_expected_levels = R.Rf_getAttrib(factor_expected.get(), R.R_LevelsSymbol);
    const factor_class = R.Rf_getAttrib(factor_result, R.R_ClassSymbol);
    if (factor_result == factor_input.get() or R.TYPEOF(factor_result) != R.INTSXP or
        R.XLENGTH(factor_result) != 4 or R.INTEGER(factor_result)[0] != R.INTEGER(factor_expected.get())[0] or
        R.INTEGER(factor_result)[1] != R.INTEGER(factor_expected.get())[1] or
        R.INTEGER(factor_result)[2] != R.INTEGER(factor_expected.get())[2] or
        R.INTEGER(factor_result)[3] != R.INTEGER(factor_expected.get())[3] or
        R.TYPEOF(factor_levels) != R.STRSXP or R.XLENGTH(factor_levels) != R.XLENGTH(factor_expected_levels) or
        !charsxpEqualsBytes(R.STRING_ELT(factor_levels, 0), "a") or
        !charsxpEqualsBytes(R.STRING_ELT(factor_levels, 1), "b") or
        R.TYPEOF(factor_class) != R.STRSXP or R.XLENGTH(factor_class) != 1 or
        !charsxpEqualsBytes(R.STRING_ELT(factor_class, 0), "factor"))
    {
        return R.Rf_ScalarReal(0.0);
    }
    var direct_factor = protect.scoped(factor.asFactorChecked(factor_input.get()) catch return R.Rf_ScalarReal(0.0));
    defer direct_factor.deinit();
    if (R.INTEGER(direct_factor.get())[0] != R.INTEGER(factor_result)[0] or
        R.INTEGER(direct_factor.get())[1] != R.INTEGER(factor_result)[1] or
        R.INTEGER(direct_factor.get())[2] != R.INTEGER(factor_result)[2] or
        R.INTEGER(direct_factor.get())[3] != R.INTEGER(factor_result)[3])
    {
        return R.Rf_ScalarReal(0.0);
    }

    var columns = reference("list(left=c(1, 2), right=c(3, 4))");
    defer columns.deinit();
    var frame_expected = reference("structure(list(left=c(1, 2), right=c(3, 4)), row.names=c(NA_integer_, -2L), class='data.frame')");
    defer frame_expected.deinit();
    const frame = R.Rf_protect(objectCall(4, columns.get()));
    defer R.Rf_unprotect(1);
    const frame_names = R.Rf_getAttrib(frame, R.R_NamesSymbol);
    const frame_class = R.Rf_getAttrib(frame, R.R_ClassSymbol);
    const frame_rows = R.Rf_getAttrib(frame, R.R_RowNamesSymbol);
    if (R.TYPEOF(frame) != R.VECSXP or R.XLENGTH(frame) != 2 or
        R.VECTOR_ELT(frame, 0) != R.VECTOR_ELT(columns.get(), 0) or
        R.VECTOR_ELT(frame, 1) != R.VECTOR_ELT(columns.get(), 1) or
        R.TYPEOF(frame_names) != R.STRSXP or R.XLENGTH(frame_names) != 2 or
        !charsxpEqualsBytes(R.STRING_ELT(frame_names, 0), "left") or
        !charsxpEqualsBytes(R.STRING_ELT(frame_names, 1), "right") or
        R.TYPEOF(frame_class) != R.STRSXP or R.XLENGTH(frame_class) != 1 or
        !charsxpEqualsBytes(R.STRING_ELT(frame_class, 0), "data.frame") or
        R.TYPEOF(frame_rows) != R.INTSXP or R.XLENGTH(frame_rows) != 2 or
        R.INTEGER(frame_rows)[0] != 1 or R.INTEGER(frame_rows)[1] != 2)
    {
        return R.Rf_ScalarReal(0.0);
    }
    if (!sameRealVector(R.VECTOR_ELT(frame, 0), R.VECTOR_ELT(frame_expected.get(), 0)) or
        !sameRealVector(R.VECTOR_ELT(frame, 1), R.VECTOR_ELT(frame_expected.get(), 1))) return R.Rf_ScalarReal(0.0);
    var direct_frame_columns = [_]R.SEXP{ R.VECTOR_ELT(columns.get(), 0), R.VECTOR_ELT(columns.get(), 1) };
    var direct_frame = protect.scoped(df.buildChecked(&.{ "left", "right" }, direct_frame_columns[0..]) catch
        return R.Rf_ScalarReal(0.0));
    defer direct_frame.deinit();
    if (R.VECTOR_ELT(direct_frame.get(), 0) != R.VECTOR_ELT(frame, 0) or
        R.VECTOR_ELT(direct_frame.get(), 1) != R.VECTOR_ELT(frame, 1)) return R.Rf_ScalarReal(0.0);

    var s4_input = reference("c(11, 12)");
    defer s4_input.deinit();
    var s4_expected = reference("methods::new('ZigrS4Contract', value=c(11, 12), label='base')");
    defer s4_expected.deinit();
    if (embed.rCodeEval(
        "{ methods::setMethod('initialize', 'ZigrS4Contract', function(.Object, ...) stop('initialize-called')); methods::setValidity('ZigrS4Contract', function(object) stop('validity-called')); TRUE }",
        R.R_GlobalEnv,
    ) == R.R_NilValue) return R.Rf_ScalarReal(0.0);
    const s4_result = R.Rf_protect(objectCall(5, s4_input.get()));
    defer R.Rf_unprotect(1);
    const s4_value = s4.getSlotChecked(s4_result, "value") catch return R.Rf_ScalarReal(0.0);
    const s4_label = s4.getSlotChecked(s4_result, "label") catch return R.Rf_ScalarReal(0.0);
    const s4_class = R.Rf_getAttrib(s4_result, R.R_ClassSymbol);
    if (!s4.isS4(s4_result) or !s4.hasSlot(s4_result, "value") or !s4.hasSlot(s4_result, "label") or
        !sameRealVector(s4_value, s4_input.get()) or
        !sameRealVector(s4_value, s4.getSlotChecked(s4_expected.get(), "value") catch return R.Rf_ScalarReal(0.0)) or
        !charsxpEqualsBytes(R.STRING_ELT(s4_label, 0), "base") or
        R.TYPEOF(s4_class) != R.STRSXP or R.XLENGTH(s4_class) != 1 or
        !charsxpEqualsBytes(R.STRING_ELT(s4_class, 0), "ZigrS4Contract")) return R.Rf_ScalarReal(0.0);
    var direct_s4 = protect.scoped(s4.newObjectChecked("ZigrS4Contract") catch return R.Rf_ScalarReal(0.0));
    defer direct_s4.deinit();
    var direct_s4_assigned = protect.scoped(s4.setSlotChecked(direct_s4.get(), "value", s4_input.get()) catch
        return R.Rf_ScalarReal(0.0));
    defer direct_s4_assigned.deinit();
    if (!sameRealVector(s4.getSlotChecked(direct_s4_assigned.get(), "value") catch return R.Rf_ScalarReal(0.0), s4_value)) {
        return R.Rf_ScalarReal(0.0);
    }
    var s4_duplicate = protect.scoped(R.Rf_duplicate(s4_result));
    defer s4_duplicate.deinit();
    var s4_replacement = protect.scoped(R.Rf_ScalarReal(99.0));
    defer s4_replacement.deinit();
    var s4_changed = protect.scoped(s4.setSlotChecked(s4_duplicate.get(), "value", s4_replacement.get()) catch
        return R.Rf_ScalarReal(0.0));
    defer s4_changed.deinit();
    if (R.REAL(s4.getSlotChecked(s4_changed.get(), "value") catch return R.Rf_ScalarReal(0.0))[0] != 99.0 or
        R.REAL(s4_value)[0] != 11.0) return R.Rf_ScalarReal(0.0);

    return R.Rf_ScalarReal(1.0);
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

fn generatedWeakReferenceState() i32 {
    weakref_finalizer_count = 0;
    weakref_finalizer_saw_key = false;
    const key = R.Rf_protect(R.R_NewEnv(R.R_EmptyEnv, 0, 29));
    defer R.Rf_unprotect(1);
    const value = R.Rf_protect(R.Rf_ScalarInteger(17));
    defer R.Rf_unprotect(1);
    const weak_ref = R.Rf_protect(weakref_mod.make(key, value, weakRefFinalizer, false));
    defer R.Rf_unprotect(1);
    if ((weakref_mod.keyChecked(weak_ref) catch return 0) != key or
        (weakref_mod.valueChecked(weak_ref) catch return 0) != value)
    {
        return 0;
    }
    weakref_mod.runFinalizer(weak_ref);
    if (weakref_mod.key(weak_ref) != R.R_NilValue or weakref_mod.value(weak_ref) != R.R_NilValue or
        weakref_finalizer_count != 1 or !weakref_finalizer_saw_key)
    {
        return 0;
    }

    const invalid_key = R.Rf_protect(R.Rf_ScalarInteger(1));
    defer R.Rf_unprotect(1);
    if (weakref_mod.makeChecked(invalid_key, value, null, false)) |_| {
        return 0;
    } else |weakref_error| {
        if (weakref_error != error.ExpectedReferenceKey) return 0;
    }
    if (weakref_mod.makeChecked(key, null, null, false)) |_| {
        return 0;
    } else |weakref_error| {
        if (weakref_error != error.NullValue) return 0;
    }
    return 1;
}

const ReferenceExports = zigr.@"export".generateExports(&.{
    .{ .name = "zigr_generated_weak_reference_state", .func = generatedWeakReferenceState },
}, &.{});

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

    var borrowed_counting = mem.CountingAllocator.init(std.heap.page_allocator);
    var counted_view = zigr_convert.toRawSliceView(borrowed_counting.allocator(), raw) catch return R.Rf_ScalarReal(0.0);
    defer counted_view.deinit();
    switch (counted_view) {
        .borrowed => {},
        .owned => return R.Rf_ScalarReal(0.0),
    }
    if (borrowed_counting.stats.allocations != 0 or borrowed_counting.stats.bytes_allocated != 0 or
        borrowed_counting.stats.peak_live_bytes != 0) return R.Rf_ScalarReal(0.0);

    var copied_counting = mem.CountingAllocator.init(std.heap.page_allocator);
    const counted_copy = zigr_convert.toRawSlice(copied_counting.allocator(), raw) catch return R.Rf_ScalarReal(0.0);
    defer copied_counting.allocator().free(counted_copy);
    if (copied_counting.stats.allocations != 1 or copied_counting.stats.bytes_allocated != expected.len or
        copied_counting.stats.peak_live_bytes != expected.len) return R.Rf_ScalarReal(0.0);
    if (cleanup.diagnosticSnapshot().cleanup_frames != 0) return R.Rf_ScalarReal(101.0);

    const rvector = zigr.rvector.RVector(u8).init(raw) catch return R.Rf_ScalarReal(0.0);
    var rvector_view = rvector.view(no_alloc.allocator()) catch return R.Rf_ScalarReal(0.0);
    defer rvector_view.deinit();
    switch (rvector_view) {
        .borrowed => {},
        .owned => return R.Rf_ScalarReal(0.0),
    }
    if (!std.mem.eql(u8, rvector_view.constSlice(), &expected) or no_alloc.end_index != 0) return R.Rf_ScalarReal(0.0);
    if (cleanup.diagnosticSnapshot().cleanup_frames != 0) return R.Rf_ScalarReal(102.0);

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
    if (cleanup.diagnosticSnapshot().cleanup_frames != 0) return R.Rf_ScalarReal(103.0);

    short_raw_region_get_calls = 0;
    short_raw_region_elt_calls = 0;
    short_raw_region_max_requested = 0;
    const copied_rvector = zigr.rvector.RVector(u8).init(raw_altrep) catch return R.Rf_ScalarReal(0.0);
    const copied_result = R.Rf_protect(copied_rvector.copy());
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(copied_result) != R.RAWSXP or R.XLENGTH(copied_result) != short_region_len or
        R.RAW(copied_result)[0] != 0 or R.RAW(copied_result)[250] != 250 or
        R.RAW(copied_result)[short_region_len - 1] != (short_region_len - 1) % 251 or
        short_raw_region_get_calls != (short_region_len + 255) / 256 or short_raw_region_elt_calls != 0 or
        short_raw_region_max_requested > 256 or R.RAW_OR_NULL(raw_altrep) != null) return R.Rf_ScalarReal(0.0);

    var too_small_storage: [short_region_len - 1]u8 = undefined;
    var too_small_fba = std.heap.FixedBufferAllocator.init(&too_small_storage);
    if (zigr_convert.toRawSliceView(too_small_fba.allocator(), raw_altrep)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |view_error| {
        if (view_error != error.OutOfMemory) return R.Rf_ScalarReal(0.0);
    }
    if (cleanup.diagnosticSnapshot().cleanup_frames != 0) return R.Rf_ScalarReal(108.0);
    const failing = R.Rf_protect(failingRawRegion());
    defer R.Rf_unprotect(1);
    if (zigr_convert.toRawSliceView(std.heap.page_allocator, failing)) |unexpected_view| {
        var unexpected = unexpected_view;
        unexpected.deinit();
        return R.Rf_ScalarReal(0.0);
    } else |view_error| {
        if (view_error != error.AltRepRegionRead) return R.Rf_ScalarReal(0.0);
    }
    if (cleanup.diagnosticSnapshot().cleanup_frames != 0) return R.Rf_ScalarReal(104.0);

    if (!initBoundaryExports()) return R.Rf_ScalarReal(0.0);
    const generated = R.Rf_protect(boundaryCall(3, raw));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(generated) != R.INTSXP or R.INTEGER(generated)[0] != 448) return R.Rf_ScalarReal(0.0);
    if (cleanup.diagnosticSnapshot().cleanup_frames != 0) return R.Rf_ScalarReal(105.0);
    const echoed = R.Rf_protect(boundaryCall(10, raw));
    defer R.Rf_unprotect(1);
    if (echoed == raw or R.TYPEOF(echoed) != R.RAWSXP or R.XLENGTH(echoed) != expected.len or
        R.RAW(echoed)[0] != 0x42 or R.RAW(echoed)[1] != 0xff or R.RAW(echoed)[2] != 0x7f) return R.Rf_ScalarReal(0.0);
    if (cleanup.diagnosticSnapshot().cleanup_frames != 0) return R.Rf_ScalarReal(106.0);
    const altrep_echo = R.Rf_protect(boundaryCall(10, raw_altrep));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(altrep_echo) != R.RAWSXP or R.XLENGTH(altrep_echo) != short_region_len or
        R.RAW(altrep_echo)[0] != 0 or R.RAW(altrep_echo)[250] != 250 or
        R.RAW(altrep_echo)[short_region_len - 1] != (short_region_len - 1) % 251) return R.Rf_ScalarReal(0.0);
    if (cleanup.diagnosticSnapshot().cleanup_frames != 0) return R.Rf_ScalarReal(107.0);
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

export fn zigr_test_generated_view_selection() SEXP {
    if (!initBoundaryExports()) return R.Rf_ScalarReal(0.0);

    const real = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 2));
    const integer = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 2));
    const complex = R.Rf_protect(R.Rf_allocVector(R.CPLXSXP, 2));
    defer R.Rf_unprotect(3);
    R.REAL(real)[0] = 1.0;
    R.REAL(real)[1] = R.NA_REAL();
    R.INTEGER(integer)[0] = 7;
    R.INTEGER(integer)[1] = R.R_NaInt;
    const complex_ptr = R.COMPLEX(complex) orelse return R.Rf_ScalarReal(0.0);
    const complex_values: [*]zigr_convert.Rcomplex = @ptrCast(@alignCast(complex_ptr));
    complex_values[0] = .{ .r = 1.0, .i = -1.0 };
    complex_values[1] = .{ .r = R.NA_REAL(), .i = R.NA_REAL() };

    const real_result = R.Rf_protect(boundaryCall(7, real));
    defer R.Rf_unprotect(1);
    const integer_result = R.Rf_protect(boundaryCall(8, integer));
    defer R.Rf_unprotect(1);
    const complex_result = R.Rf_protect(boundaryCall(9, complex));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(real_result) != R.INTSXP or R.INTEGER(real_result)[0] != 1 or
        R.TYPEOF(integer_result) != R.INTSXP or R.INTEGER(integer_result)[0] != 1 or
        R.TYPEOF(complex_result) != R.INTSXP or R.INTEGER(complex_result)[0] != 1) return R.Rf_ScalarReal(0.0);
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

export fn zigr_test_generated_string_projections() SEXP {
    if (!initBoundaryExports()) return R.Rf_ScalarReal(0.0);

    const utf8 = [_]u8{ 'c', 'a', 'f', 0xc3, 0xa9 };
    const byte_marked = [_]u8{ 'x', 0xff, 'y' };
    const latin1 = [_]u8{ 'c', 'a', 'f', 0xe9 };
    const vector = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 5));
    defer R.Rf_unprotect(1);
    R.SET_STRING_ELT(vector, 0, R.Rf_mkCharLenCE(@ptrCast(&utf8), @intCast(utf8.len), @as(R.cetype_t, @intCast(R.CE_UTF8))));
    R.SET_STRING_ELT(vector, 1, R.Rf_mkCharLenCE("", 0, @as(R.cetype_t, @intCast(R.CE_UTF8))));
    R.SET_STRING_ELT(vector, 2, R.R_NaString);
    R.SET_STRING_ELT(vector, 3, R.Rf_mkCharLenCE(@ptrCast(&byte_marked), @intCast(byte_marked.len), @as(R.cetype_t, @intCast(R.CE_BYTES))));
    R.SET_STRING_ELT(vector, 4, R.Rf_mkCharLenCE(@ptrCast(&latin1), @intCast(latin1.len), @as(R.cetype_t, @intCast(R.CE_LATIN1))));

    const identity = R.Rf_protect(boundaryCall(11, vector));
    const missingness = R.Rf_protect(boundaryCall(12, vector));
    const bytes = R.Rf_protect(boundaryCall(13, vector));
    const encoding = R.Rf_protect(boundaryCall(14, vector));
    const translated = R.Rf_protect(boundaryCall(15, vector));
    const metadata = R.Rf_protect(boundaryCall(16, vector));
    defer R.Rf_unprotect(6);

    const altrep_values = [_][]const u8{ "alpha", "", "omega" };
    const altrep = R.Rf_protect(MyAltString.init(altrep_values[0..]));
    const altrep_missingness = R.Rf_protect(boundaryCall(12, altrep));
    defer R.Rf_unprotect(2);

    const wrong_type = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 1));
    defer R.Rf_unprotect(1);
    generated_string_projection_error_arg = wrong_type;
    defer generated_string_projection_error_arg = null;
    const condition = trycatch_mod.tryCatchError(generatedStringProjectionErrorCall) catch return R.Rf_ScalarReal(0.0);
    if (condition == null or !std.mem.eql(u8, trycatch_mod.extractMessage(condition.?), "toStringMissingnessView: ExpectedString")) {
        return R.Rf_ScalarReal(0.0);
    }

    if (R.INTEGER(identity)[0] != 1 or R.INTEGER(missingness)[0] != 4 or
        R.INTEGER(bytes)[0] != 12 or R.INTEGER(encoding)[0] != 111 or
        R.INTEGER(translated)[0] != 13 or R.INTEGER(metadata)[0] != 1123 or
        R.INTEGER(altrep_missingness)[0] != 3)
    {
        return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

const MethodCounter = struct {
    val: i32,
};

fn methodCounterDeinit(_: *MethodCounter) void {}

fn counterAdd(self: *MethodCounter, amount: i32) i32 {
    self.val += amount;
    return self.val;
}

fn counterLogical(_: *MethodCounter, values: zigr_convert.LogicalSliceView) i32 {
    return logicalCountCode(values);
}

const CounterMethods = zigr.@"export".generateMethods(MethodCounter, &.{
    .{ .name = "add", .func = counterAdd },
    .{ .name = "logical", .func = counterLogical },
}, &.{
    .{ .name = "add_external", .func = counterAdd },
    .{ .name = "logical_external", .func = counterLogical },
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

fn expectExternalMethodErrorFor(fun: ExternalMethodCall, receiver: R.SEXP, amount: R.SEXP, expected: []const u8) bool {
    const args = R.Rf_protect(R.Rf_cons(
        R.Rf_install("zigr_dispatch_test"),
        R.Rf_cons(receiver, R.Rf_cons(amount, R.R_NilValue)),
    ));
    defer R.Rf_unprotect(1);
    external_method_error_fun = fun;
    return expectExternalMethodError(args, expected);
}

var inner_cleanup_fired: bool = false;
var outer_cleanup_fired: bool = false;

var restoration_cleanup_calls: usize = 0;

fn countRestorationCleanup(_: ?*anyopaque) void {
    restoration_cleanup_calls += 1;
}

fn sameRestorationState(entry: cleanup.DiagnosticSnapshot, current: cleanup.DiagnosticSnapshot) bool {
    if (entry.enabled != current.enabled) return false;
    if (!entry.enabled) return true;
    return entry.cleanup_frames == current.cleanup_frames and
        entry.unwind_boundaries == current.unwind_boundaries and
        entry.protect_depth == current.protect_depth;
}

fn restorationNormalBody() SEXP {
    var held = protect.scoped(R.Rf_allocVector(R.REALSXP, 1));
    defer held.deinit();
    var extra = protect.scoped(R.Rf_allocVector(R.INTSXP, 1));
    defer extra.deinit();
    R.REAL(held.get())[0] = 17.0;
    R.INTEGER(extra.get())[0] = 23;

    cleanup.pushFrame(countRestorationCleanup, null);
    defer cleanup.popFrame();
    R.R_gc();
    if (R.REAL(held.get())[0] != 17.0 or R.INTEGER(extra.get())[0] != 23) {
        return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

const RestorationZigError = error{Injected};

fn restorationZigErrorBody() RestorationZigError!SEXP {
    var held = protect.scoped(R.Rf_allocVector(R.REALSXP, 1));
    defer held.deinit();
    cleanup.pushFrame(countRestorationCleanup, null);
    defer cleanup.popFrame();
    R.R_gc();
    return error.Injected;
}

fn restorationZigErrorCall() SEXP {
    return cleanup.protectCall(struct {
        fn call() SEXP {
            _ = restorationZigErrorBody() catch return R.Rf_ScalarReal(1.0);
            return R.Rf_ScalarReal(0.0);
        }
    }.call);
}

fn restorationWarningBody() SEXP {
    var held = protect.scoped(R.Rf_allocVector(R.REALSXP, 1));
    defer held.deinit();
    cleanup.pushFrame(countRestorationCleanup, null);
    defer cleanup.popFrame();
    R.R_gc();
    err.warn("zigr state restoration warning");
    return R.Rf_ScalarReal(1.0);
}

fn restorationWarningCall() SEXP {
    return cleanup.protectCall(restorationWarningBody);
}

fn restorationInterruptBody() SEXP {
    var held = protect.scoped(R.Rf_allocVector(R.REALSXP, 1));
    defer held.deinit();
    cleanup.pushFrame(countRestorationCleanup, null);
    R.R_gc();
    ict.checkInterrupt();
    return R.Rf_ScalarReal(1.0);
}

fn restorationInterruptCall() SEXP {
    return cleanup.protectCall(restorationInterruptBody);
}

fn restorationBeforeFrameErrorBody() SEXP {
    var held = protect.scoped(R.Rf_allocVector(R.REALSXP, 1));
    defer held.deinit();
    R.R_gc();
    R.Rf_error("zigr pre-frame state restoration error");
}

fn restorationBeforeFrameErrorCall() SEXP {
    return cleanup.protectCall(restorationBeforeFrameErrorBody);
}

fn restorationRerrorBody() SEXP {
    var held = protect.scoped(R.Rf_allocVector(R.REALSXP, 1));
    defer held.deinit();
    cleanup.pushFrame(countRestorationCleanup, null);
    R.R_gc();
    R.Rf_error("zigr state restoration error");
}

fn restorationRerrorCall() SEXP {
    return cleanup.protectCall(restorationRerrorBody);
}

fn restorationNestedInner() SEXP {
    cleanup.pushFrame(countRestorationCleanup, null);
    R.Rf_error("zigr nested state restoration error");
}

fn restorationNestedOuter() SEXP {
    cleanup.pushFrame(countRestorationCleanup, null);
    const result = cleanup.protectCall(restorationNestedInner);
    cleanup.popFrame();
    return result;
}

fn restorationNestedCall() SEXP {
    return cleanup.protectCall(restorationNestedOuter);
}

var boundary_overflow_body_reached: bool = false;

fn restorationBoundaryFill(data: ?*anyopaque) SEXP {
    const remaining: *usize = @ptrCast(@alignCast(data.?));
    if (remaining.* == 0) {
        boundary_overflow_body_reached = true;
        R.Rf_error("zigr boundary overflow did not stop before the body");
    }
    cleanup.pushFrame(countRestorationCleanup, null);
    remaining.* -= 1;
    return cleanup.protectCallData(restorationBoundaryFill, data);
}

fn restorationBoundaryOverflowCall() SEXP {
    var remaining: usize = cleanup.MAX_NESTING;
    return cleanup.protectCallData(restorationBoundaryFill, @ptrCast(&remaining));
}

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

export fn zigr_test_unwind_state_restoration() SEXP {
    const entry = cleanup.diagnosticSnapshot();
    if (!entry.enabled) return R.Rf_ScalarReal(1.0);
    cleanup.resetDiagnostics();

    restoration_cleanup_calls = 0;
    var normal = protect.scoped(cleanup.protectCall(restorationNormalBody));
    defer normal.deinit();
    if (R.REAL(normal.get())[0] != 1.0 or restoration_cleanup_calls != 0) return R.Rf_ScalarReal(0.0);
    normal.deinit();
    if (!sameRestorationState(entry, cleanup.diagnosticSnapshot())) return R.Rf_ScalarReal(0.0);

    restoration_cleanup_calls = 0;
    var zig_error = protect.scoped(restorationZigErrorCall());
    defer zig_error.deinit();
    if (R.REAL(zig_error.get())[0] != 1.0 or restoration_cleanup_calls != 0) return R.Rf_ScalarReal(0.0);
    zig_error.deinit();
    if (!sameRestorationState(entry, cleanup.diagnosticSnapshot())) return R.Rf_ScalarReal(0.0);

    restoration_cleanup_calls = 0;
    if (trycatch_mod.tryCatch(restorationWarningCall)) |_| {} else |_| {}
    if (restoration_cleanup_calls > 1 or !sameRestorationState(entry, cleanup.diagnosticSnapshot())) {
        return R.Rf_ScalarReal(0.0);
    }

    restoration_cleanup_calls = 0;
    R_interrupts_pending = 1;
    if (trycatch_mod.tryCatch(restorationInterruptCall)) |_| {
        R_interrupts_pending = 0;
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    R_interrupts_pending = 0;
    if (restoration_cleanup_calls != 1 or !sameRestorationState(entry, cleanup.diagnosticSnapshot())) {
        return R.Rf_ScalarReal(0.0);
    }

    restoration_cleanup_calls = 0;
    if (trycatch_mod.tryCatch(restorationBeforeFrameErrorCall)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    if (restoration_cleanup_calls != 0 or !sameRestorationState(entry, cleanup.diagnosticSnapshot())) {
        return R.Rf_ScalarReal(0.0);
    }

    restoration_cleanup_calls = 0;
    if (trycatch_mod.tryCatch(restorationRerrorCall)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    if (restoration_cleanup_calls != 1 or !sameRestorationState(entry, cleanup.diagnosticSnapshot())) {
        return R.Rf_ScalarReal(0.0);
    }

    restoration_cleanup_calls = 0;
    if (trycatch_mod.tryCatch(restorationNestedCall)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    if (restoration_cleanup_calls != 2 or !sameRestorationState(entry, cleanup.diagnosticSnapshot())) {
        return R.Rf_ScalarReal(0.0);
    }

    restoration_cleanup_calls = 0;
    boundary_overflow_body_reached = false;
    if (trycatch_mod.tryCatch(restorationBoundaryOverflowCall)) |_| {
        return R.Rf_ScalarReal(0.0);
    } else |_| {}
    if (boundary_overflow_body_reached or restoration_cleanup_calls != cleanup.MAX_NESTING or
        !sameRestorationState(entry, cleanup.diagnosticSnapshot()))
    {
        return R.Rf_ScalarReal(0.0);
    }

    restoration_cleanup_calls = 0;
    var retry = protect.scoped(cleanup.protectCall(restorationNormalBody));
    defer retry.deinit();
    if (R.REAL(retry.get())[0] != 1.0 or restoration_cleanup_calls != 0) return R.Rf_ScalarReal(0.0);
    retry.deinit();
    if (!sameRestorationState(entry, cleanup.diagnosticSnapshot())) return R.Rf_ScalarReal(0.0);

    const final = cleanup.diagnosticSnapshot();
    if (final.max_cleanup_frames < 2 or final.max_unwind_boundaries < 2 or final.max_protect_depth < 2) {
        return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
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

export fn zigr_test_current_cleanup_state() SEXP {
    const snapshot = cleanup.diagnosticSnapshot();
    const encoded = snapshot.cleanup_frames * 1_000_000 + snapshot.unwind_boundaries * 10_000 +
        @as(usize, @intCast(@max(snapshot.protect_depth, 0))) * 100 + snapshot.max_cleanup_frames;
    return R.Rf_ScalarReal(@floatFromInt(encoded));
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

    const logical = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, 3));
    defer R.Rf_unprotect(1);
    R.LOGICAL(logical)[0] = 0;
    R.LOGICAL(logical)[1] = 1;
    R.LOGICAL(logical)[2] = R.R_NaInt;
    const logical_method: MethodCall = @ptrCast(@alignCast(CounterMethods.call_defs[1].fun));
    const logical_result = R.Rf_protect(logical_method(extptr, logical, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue));
    defer R.Rf_unprotect(1);
    if (R.INTEGER(logical_result)[0] != 111) return R.Rf_ScalarReal(0.0);
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

    const logical = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, 3));
    defer R.Rf_unprotect(1);
    R.LOGICAL(logical)[0] = 0;
    R.LOGICAL(logical)[1] = 1;
    R.LOGICAL(logical)[2] = R.R_NaInt;
    const logical_method: ExternalMethodCall = @ptrCast(@alignCast(CounterMethods.ext_defs[1].fun));
    const logical_args = R.Rf_protect(R.Rf_cons(
        R.Rf_install("zigr_test_counter_logical_external"),
        R.Rf_cons(receiver, R.Rf_cons(logical, R.R_NilValue)),
    ));
    defer R.Rf_unprotect(1);
    const logical_result = R.Rf_protect(logical_method(logical_args));
    defer R.Rf_unprotect(1);
    if (R.INTEGER(logical_result)[0] != 111) return R.Rf_ScalarReal(0.0);

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

export fn zigr_test_dispatch_semantics() SEXP {
    const dll = test_dll orelse (R.R_getEmbeddingDllInfo() orelse return R.Rf_ScalarReal(0.0));
    CounterMethods.init(dll);
    ReferenceExports.init(dll);

    const call_def = CounterMethods.call_defs[0];
    const external_def = CounterMethods.ext_defs[0];
    if (call_def.name == null or external_def.name == null or call_def.fun == null or external_def.fun == null or
        !std.mem.endsWith(u8, std.mem.span(call_def.name), "__add") or
        !std.mem.endsWith(u8, std.mem.span(external_def.name), "__add_external") or
        call_def.numArgs != 2 or external_def.numArgs != 2 or
        CounterMethods.call_defs[2].name != null or CounterMethods.ext_defs[2].name != null)
    {
        return R.Rf_ScalarReal(0.0);
    }

    const call_fun: MethodCall = @ptrCast(@alignCast(call_def.fun));
    const external_fun: ExternalMethodCall = @ptrCast(@alignCast(external_def.fun));
    method_error_fun = call_fun;
    external_method_error_fun = external_fun;

    var counter = MethodCounter{ .val = 10 };
    const backing = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 1));
    defer R.Rf_unprotect(1);
    R.INTEGER(backing)[0] = 42;
    const receiver = R.Rf_protect(zigr.externalptr.makeTyped(MethodCounter, &counter, backing));
    defer R.Rf_unprotect(1);
    const amount = R.Rf_protect(R.Rf_ScalarInteger(5));
    defer R.Rf_unprotect(1);
    const receiver_tag = zigr.externalptr.tag(receiver);
    const receiver_metadata = zigr.externalptr.protected(receiver);

    const call_result = R.Rf_protect(call_fun(
        receiver,
        amount,
        R.R_NilValue,
        R.R_NilValue,
        R.R_NilValue,
        R.R_NilValue,
        R.R_NilValue,
        R.R_NilValue,
    ));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(call_result) != R.INTSXP or R.INTEGER(call_result)[0] != 15 or counter.val != 15) {
        return R.Rf_ScalarReal(0.0);
    }

    const external_args = R.Rf_protect(R.Rf_cons(
        R.Rf_install("zigr_dispatch_test"),
        R.Rf_cons(receiver, R.Rf_cons(amount, R.R_NilValue)),
    ));
    defer R.Rf_unprotect(1);
    const external_result = R.Rf_protect(external_fun(external_args));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(external_result) != R.INTSXP or R.INTEGER(external_result)[0] != 20 or counter.val != 20) {
        return R.Rf_ScalarReal(0.0);
    }

    if ((zigr.externalptr.typedBacking(MethodCounter, receiver) catch return R.Rf_ScalarReal(0.0)) != backing or
        zigr.externalptr.addr(receiver) != @as(?*anyopaque, @ptrCast(&counter)) or
        zigr.externalptr.tag(receiver) != receiver_tag or
        zigr.externalptr.protected(receiver) != receiver_metadata)
    {
        return R.Rf_ScalarReal(0.0);
    }

    const invalid_receiver = R.Rf_protect(R.Rf_ScalarInteger(0));
    defer R.Rf_unprotect(1);
    if (!expectMethodError(invalid_receiver, amount, "expected EXTPTRSXP receiver") or
        !expectExternalMethodErrorFor(external_fun, invalid_receiver, amount, "expected EXTPTRSXP receiver"))
    {
        return R.Rf_ScalarReal(0.0);
    }

    const wrong_tag = R.Rf_protect(zigr.externalptr.make(@ptrCast(&counter), R.Rf_install("zigr_dispatch_wrong_tag"), R.R_NilValue));
    defer R.Rf_unprotect(1);
    if (!expectMethodError(wrong_tag, amount, "external pointer tag does not match method type") or
        !expectExternalMethodErrorFor(external_fun, wrong_tag, amount, "external pointer tag does not match method type"))
    {
        return R.Rf_ScalarReal(0.0);
    }

    const missing_metadata = R.Rf_protect(zigr.externalptr.make(
        @ptrCast(&counter),
        zigr.externalptr.typeTag(MethodCounter),
        R.R_NilValue,
    ));
    defer R.Rf_unprotect(1);
    if (!expectMethodError(missing_metadata, amount, "external pointer is missing typed metadata") or
        !expectExternalMethodErrorFor(external_fun, missing_metadata, amount, "external pointer is missing typed metadata"))
    {
        return R.Rf_ScalarReal(0.0);
    }

    const null_address = R.Rf_protect(zigr.externalptr.makeTypedRaw(MethodCounter, null, R.R_NilValue));
    defer R.Rf_unprotect(1);
    if (!expectMethodError(null_address, amount, "external pointer has been cleared") or
        !expectExternalMethodErrorFor(external_fun, null_address, amount, "external pointer has been cleared"))
    {
        return R.Rf_ScalarReal(0.0);
    }

    const cleared = R.Rf_protect(zigr.externalptr.makeTyped(MethodCounter, &counter, R.R_NilValue));
    defer R.Rf_unprotect(1);
    R.R_ClearExternalPtr(cleared);
    if (!expectMethodError(cleared, amount, "external pointer has been cleared") or
        !expectExternalMethodErrorFor(external_fun, cleared, amount, "external pointer has been cleared"))
    {
        return R.Rf_ScalarReal(0.0);
    }

    const misaligned_address = @intFromPtr(&counter) + 1;
    const misaligned = R.Rf_protect(zigr.externalptr.makeTypedRaw(
        MethodCounter,
        @ptrFromInt(misaligned_address),
        R.R_NilValue,
    ));
    defer R.Rf_unprotect(1);
    if (!expectMethodError(misaligned, amount, "external pointer is misaligned for method type") or
        !expectExternalMethodErrorFor(external_fun, misaligned, amount, "external pointer is misaligned for method type"))
    {
        return R.Rf_ScalarReal(0.0);
    }
    if (counter.val != 20) return R.Rf_ScalarReal(0.0);

    const weak_reference_fun: *const fn (R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP) callconv(.c) R.SEXP =
        @ptrCast(@alignCast(ReferenceExports.call_defs[0].fun));
    const weak_reference_result = R.Rf_protect(weak_reference_fun(
        R.R_NilValue,
        R.R_NilValue,
        R.R_NilValue,
        R.R_NilValue,
        R.R_NilValue,
        R.R_NilValue,
        R.R_NilValue,
        R.R_NilValue,
    ));
    defer R.Rf_unprotect(1);
    if (R.TYPEOF(weak_reference_result) != R.INTSXP or R.XLENGTH(weak_reference_result) != 1 or
        R.INTEGER(weak_reference_result)[0] != 1)
    {
        return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

export fn zigr_test_registered_method_dispatch() SEXP {
    const dll = test_dll orelse (R.R_getEmbeddingDllInfo() orelse return R.Rf_ScalarReal(0.0));
    CounterMethods.init(dll);

    var receiver = protect.scoped(zigr.externalptr.createTyped(MethodCounter, .{ .val = 0 }, methodCounterDeinit));
    defer receiver.deinit();
    var amount = protect.scoped(R.Rf_ScalarInteger(5));
    defer amount.deinit();

    var call_name = protect.scoped(R.Rf_mkString("r_runtime_MethodCounter__add"));
    defer call_name.deinit();
    var call_expr = protect.scoped(test_lang.call3(test_lang.symbol(".Call"), call_name.get(), receiver.get(), amount.get()));
    defer call_expr.deinit();
    const call_result = test_eval.tryEval(call_expr.get(), R.R_GlobalEnv) orelse return R.Rf_ScalarReal(0.0);
    if (R.TYPEOF(call_result) != R.INTSXP or R.XLENGTH(call_result) != 1 or R.INTEGER(call_result)[0] != 5) {
        return R.Rf_ScalarReal(0.0);
    }

    var external_name = protect.scoped(R.Rf_mkString("r_runtime_MethodCounter__add_external"));
    defer external_name.deinit();
    var external_expr = protect.scoped(test_lang.call3(test_lang.symbol(".External"), external_name.get(), receiver.get(), amount.get()));
    defer external_expr.deinit();
    const external_result = test_eval.tryEval(external_expr.get(), R.R_GlobalEnv) orelse return R.Rf_ScalarReal(0.0);
    if (R.TYPEOF(external_result) != R.INTSXP or R.XLENGTH(external_result) != 1 or R.INTEGER(external_result)[0] != 10) {
        return R.Rf_ScalarReal(0.0);
    }

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
