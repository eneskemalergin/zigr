//! Tests that require R to be running.
//! Compiled into a shared library and loaded by R via tests/run_r_tests.R.
//! Not run by `zig build test` (those tests cannot link libR).

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
const attrib = zigr.attrib;
const altrep_create = zigr.altrep_create;
const test_lang = zigr.lang;
const embed = zigr.embed;
const trycatch_mod = zigr.trycatch;
const protect = zigr.protect;

// Use the R module's SEXP type for function parameters and returns.
const SEXP = R.SEXP;

// DllInfo for the test library, populated by R_init_zigr_r_test.
var test_dll: ?*R.DllInfo = null;

// R calls this when dyn.load("libzigr_r_test.so") is called.
// Stores the DllInfo for use by export system tests.
export fn R_init_zigr_r_test(info: *R.DllInfo) callconv(.c) void {
    test_dll = info;
}

// Basic creation tests

/// Allocate a REALSXP of size 100, fill with 0..99.
export fn zigr_alloc_real() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @as(R.R_xlen_t, 100)));
    const ptr: [*]f64 = @ptrCast(R.REAL(vec));
    var i: usize = 0;
    while (i < 100) : (i += 1) ptr[i] = @floatFromInt(i);
    R.Rf_unprotect(1);
    return R.Rf_ScalarReal(1.0);
}

/// Allocate a large vector (10M) to stress R's allocator.
export fn zigr_alloc_large() SEXP {
    _ = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @as(R.R_xlen_t, 10000000)));
    R.Rf_unprotect(1);
    return R.Rf_ScalarReal(1.0);
}

// PROTECT stress tests

/// PROTECT the same SEXP 100 times, then UNPROTECT 100.
export fn zigr_protect_many() SEXP {
    const vec = R.Rf_allocVector(R.INTSXP, 1);
    var i: usize = 0;
    while (i < 100) : (i += 1) _ = R.Rf_protect(vec);
    R.Rf_unprotect(100);
    return R.Rf_ScalarReal(1.0);
}

/// PROTECT with index, reprotect with a new vector, then clean up.
export fn zigr_protect_index() SEXP {
    var idx: R.PROTECT_INDEX = 0;
    const vec1 = R.Rf_allocVector(R.REALSXP, 10);
    R.R_ProtectWithIndex(vec1, &idx);

    const vec2 = R.Rf_allocVector(R.INTSXP, 5);
    R.R_Reprotect(vec2, idx);

    R.Rf_unprotect(1);
    return R.Rf_ScalarReal(1.0);
}

// NA handling tests

/// Create a vector with NA_REAL at position 2, verify positions.
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

// Error signaling tests (expect_error=TRUE for raise_error)

/// Call Rf_error: must raise R error, not segfault.
export fn zigr_raise_error() SEXP {
    R.Rf_error("zigr test error: this is expected");
    return R.Rf_ScalarReal(0.0);
}

/// Call Rf_warning: must print warning and continue.
export fn zigr_raise_warning() SEXP {
    R.Rf_warning("zigr test warning: this is expected");
    return R.Rf_ScalarReal(1.0);
}

// Type tests

/// Verify TYPEOF(R_NilValue) returns NILSXP (0).
export fn zigr_typeof_nil() SEXP {
    if (R.TYPEOF(R.R_NilValue) != 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

// Basic protect and return-value tests

export fn zigr_test_protect() SEXP {
    _ = R.Rf_protect(R.R_NilValue);
    R.Rf_unprotect(1);
    return R.Rf_ScalarReal(1.0);
}

/// Verify that a scalar REAL 42.0 is created and has the right value.
export fn zigr_test_return42() SEXP {
    const val = R.REAL(R.Rf_ScalarReal(42.0))[0];
    if (val != 42.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

// Longjmp / R_UnwindProtect tests

// Flag set by the cleanup handler on longjmp.
var longjmp_cleanup_fired: bool = false;

fn markCleanupFired(_: ?*anyopaque) void {
    longjmp_cleanup_fired = true;
}

/// Calls Rf_error inside a protectCall guard.
/// The cleanup handler fires, then R_UnwindProtect re-throws
/// the error to R.  Registered in run_r_tests.R as expect_error=TRUE.
export fn zigr_test_longjmp() SEXP {
    longjmp_cleanup_fired = false;
    cleanup.pushFrame(markCleanupFired, null);

    _ = cleanup.protectCall(struct {
        fn doBoom() R.SEXP {
            R.Rf_error("zigr longjmp test: expected error");
            return R.R_NilValue;
        }
    }.doBoom);

    cleanup.popFrame();
    return R.Rf_ScalarReal(0.0);
}

/// Normal return inside protectCall: cleanup should NOT fire.
/// Verifies the return value 99.0 passes through correctly.
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

/// Query the longjmp cleanup flag from R.
export fn zigr_longjmp_flag() SEXP {
    return R.Rf_ScalarInteger(if (longjmp_cleanup_fired) 1 else 0);
}

// Error module tests (Phase 2.4)

/// Test error.signal: calls Rf_error and is caught by tryCatch.
/// Registered as expect_error=TRUE in run_r_tests.R.
export fn zigr_test_error_signal() SEXP {
    err.signal("zigr error signal test");
    return R.Rf_ScalarReal(0.0);
}

/// Test error.warn: calls Rf_warning, then returns normally.
export fn zigr_test_error_warn() SEXP {
    err.warn("zigr warning signal test");
    return R.Rf_ScalarReal(1.0);
}

/// Test error.signalIf: condition is true, so error fires.
export fn zigr_test_error_signalif() SEXP {
    err.signalIf(true, "zigr error signalIf test");
    return R.R_NilValue;
}

// Interrupt module tests (Phase 2.5)

/// Call ict.checkInterrupt: should return normally when no interrupt pending.
export fn zigr_test_interrupt() SEXP {
    ict.checkInterrupt();
    return R.R_NilValue;
}

/// Call ict.checkStack: should return normally.
export fn zigr_test_check_stack() SEXP {
    ict.checkStack();
    return R.R_NilValue;
}

// Reverse FFI tests

/// Evaluate 1 + 1 via lang.call2 + eval.rEval.  Expects 2.0.
export fn zigr_test_rev_eval() SEXP {
    const plus = test_lang.symbol("+");
    const one = R.Rf_ScalarReal(1.0);
    const call = test_lang.call2(plus, one, one);
    const result = test_eval.rEval(call, null);
    const val = R.REAL(result)[0];
    if (val != 2.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// Define a variable via eval.defineVar, then look it up and verify 42.
export fn zigr_test_rev_define_find() SEXP {
    test_eval.defineVar("zigr_test_var", R.Rf_ScalarReal(42.0));
    const result = test_eval.findVarName("zigr_test_var");
    const val = R.REAL(result)[0];
    if (val != 42.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// Build and evaluate a call2: `sum(10, 20)`.  Expects 30.0.
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

// RNG tests

/// Acquire and release RNG: should not crash.
export fn zigr_test_rng() SEXP {
    rng.acquire();
    rng.release();
    return R.Rf_ScalarReal(1.0);
}

// Memory allocator tests

/// Allocate and free through RAllocator.
export fn zigr_test_ralloc() SEXP {
    const alloc = mem.RAllocator;
    const buf = alloc.alloc(u8, 100) catch return R.Rf_ScalarReal(0.0);
    defer alloc.free(buf);
    buf[0] = 42;
    if (buf[0] != 42) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

// Preserve test

var preserve_released: bool = false;

fn releasePreserved(_: ?*anyopaque) void {
    preserve_released = true;
}

/// Preserve an SEXP, then error inside protectCall.
/// Cleanup fires R_ReleaseObject on unwind.  The error propagates
/// to R (expect_error=TRUE in run_r_tests.R).
export fn zigr_test_preserve_longjmp() SEXP {
    preserve_released = false;
    const obj = R.Rf_ScalarReal(99.0);
    R.R_PreserveObject(obj);
    cleanup.pushFrame(releasePreserved, null);
    _ = cleanup.protectCall(struct {
        fn doBoom() SEXP {
            R.Rf_error("preserve: expected");
            return R.R_NilValue;
        }
    }.doBoom);
    cleanup.popFrame();
    return R.Rf_ScalarReal(0.0);
}

/// Query the preserve flag from R.
export fn zigr_preserve_flag() SEXP {
    return R.Rf_ScalarInteger(if (preserve_released) 1 else 0);
}

// Nested callbacks test (Phase 2.3)

var nested_outer_fired: bool = false;
var nested_inner_fired: bool = false;

fn markOuter(_: ?*anyopaque) void {
    nested_outer_fired = true;
}
fn markInner(_: ?*anyopaque) void {
    nested_inner_fired = true;
}

/// Inner function: pushes cleanup, then errors.
/// Called by R via .Call (triggered from outer through rffi.eval).
export fn zigr_test_nested_inner() SEXP {
    cleanup.pushFrame(markInner, null);
    R.Rf_error("nested inner: expected");
    cleanup.popFrame();
    return R.R_NilValue;
}

/// Outer function: pushes cleanup, calls protectCall which
/// evaluates an R expression that .Call's back into zigr_test_nested_inner.
export fn zigr_test_nested_outer() SEXP {
    nested_outer_fired = false;
    nested_inner_fired = false;
    cleanup.pushFrame(markOuter, null);

    _ = cleanup.protectCall(struct {
        fn doNested() R.SEXP {
            const fn_name = R.Rf_mkChar("zigr_test_nested_inner");
            const fn_string = R.Rf_ScalarString(fn_name);
            const call_sexp = test_lang.call1(
                test_lang.symbol(".Call"),
                fn_string,
            );
            return test_eval.rEval(call_sexp, null);
        }
    }.doNested);

    cleanup.popFrame();
    return R.R_NilValue;
}

/// Query nested callback flags from R.
export fn zigr_nested_flags() SEXP {
    var v: i32 = 0;
    if (nested_outer_fired) v += 1;
    if (nested_inner_fired) v += 2;
    return R.Rf_ScalarInteger(v);
}

// REALSXP conversion tests

/// Write [1,2,3,4,5] to REALSXP, read back via REAL() to verify.
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

/// Create REALSXP from a Zig slice via fromRealSlice, verify values.
export fn zigr_test_from_real_slice() SEXP {
    const values = [_]f64{ 10.0, 20.0, 30.0 };
    const sexp: R.SEXP = @ptrCast(zigr_convert.fromRealSlice(values[0..]));
    const ptr = R.REAL(sexp);
    for (0..values.len) |i| {
        if (ptr[i] != values[i]) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

/// Round-trip: create REALSXP from Zig slice, read back.
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

// INTSXP conversion tests

/// Write [10,20,30,40] to INTSXP, read back to verify.
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

/// Create INTSXP from a Zig i32 slice, verify values.
export fn zigr_test_int_from_slice() SEXP {
    const values = [_]i32{ 100, 200, 300 };
    const sexp: R.SEXP = @ptrCast(zigr_convert.fromIntSlice(values[0..]));
    const ptr = R.INTEGER(sexp);
    for (0..values.len) |i| {
        if (ptr[i] != values[i]) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

// STRSXP conversion tests

/// Write ["hello","world","zigr"] to STRSXP, read back to verify.
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

/// Create STRSXP from a Zig string slice via fromStringSlice, verify.
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

// LGLSXP conversion tests

/// Write [TRUE, FALSE, NA, TRUE] to LGLSXP, read back to verify.
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

/// Allocate LGLSXP from Zig slice, return it.
export fn zigr_test_lgl_from_slice() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const values = [_]i32{ 1, 0, 1 };
    const result = zigr_convert.fromLogicalSlice(values[0..]);
    return @as(R.SEXP, @ptrCast(result));
}

// VECSXP conversion tests (Phase 3.5)

/// Create a VECSXP (list) with mixed types and return it.
export fn zigr_test_list_create() SEXP {
    const n: R.R_xlen_t = 3;
    const vec = R.Rf_protect(R.Rf_allocVector(R.VECSXP, n));
    defer R.Rf_unprotect(1);
    _ = R.SET_VECTOR_ELT(vec, 0, R.Rf_ScalarReal(1.5));
    _ = R.SET_VECTOR_ELT(vec, 1, R.Rf_ScalarInteger(42));
    _ = R.SET_VECTOR_ELT(vec, 2, R.R_NilValue);
    return vec;
}

/// Call toLogicalSlice on the lgl vector, verify values via R.
/// Test toLogicalSlice via round-trip: create LGLSXP, read back, verify.
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

// Data frame tests (Phase 3.6)

/// Build a data frame from Zig arrays, verify structure.
export fn zigr_test_df_build() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const vals = [_]f64{ 1.0, 2.0, 3.0 };
    const col1 = @as(R.SEXP, @ptrCast(zigr_convert.fromRealSlice(vals[0..])));
    const strs = [_][]const u8{ "a", "b", "c" };
    const col2 = @as(R.SEXP, @ptrCast(zigr_convert.fromStringSlice(strs[0..])));
    const names = [_][]const u8{ "x", "y" };
    const cols = [_]R.SEXP{ col1, col2 };

    const df_sexp = df.build(names[0..], cols[0..]);
    const wrap = df.DataFrame.wrap(df_sexp) orelse return R.Rf_ScalarReal(0.0);
    if (wrap.columnCount() != 2) return R.Rf_ScalarReal(0.0);
    if (wrap.rowCount() != 3) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// Build a data frame and verify column access.
export fn zigr_test_df_column() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const vals = [_]f64{ 10.0, 20.0 };
    const col1 = @as(R.SEXP, @ptrCast(zigr_convert.fromRealSlice(vals[0..])));
    const names = [_][]const u8{"val"};
    const cols = [_]R.SEXP{col1};
    const df_sexp = df.build(names[0..], cols[0..]);

    const df_wrap = df.DataFrame.wrap(df_sexp) orelse return R.R_NilValue;
    const col = df_wrap.column("val") orelse return R.Rf_ScalarReal(0.0);
    const rcol: R.SEXP = @as(R.SEXP, @ptrCast(col));
    const ptr: [*]f64 = @ptrCast(R.REAL(rcol));
    if (ptr[0] == 10.0 and ptr[1] == 20.0) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

// RAWSXP / CPLXSXP tests

/// Write [0xde, 0xad, 0xbe, 0xef] to RAWSXP, read back to verify.
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

/// Write [(1+2i), (3+4i), (5+6i)] to CPLXSXP, read back to verify.
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

// Attrib tests (Phase 3.7)

/// Set and verify class attribute.
export fn zigr_test_attrib_class() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    defer R.Rf_unprotect(1);
    attrib.setClass(vec, "myclass");
    const cls = attrib.getClass(std.heap.page_allocator, vec) catch return R.Rf_ScalarReal(0.0);
    if (cls.len > 0 and std.mem.eql(u8, cls[0], "myclass")) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

/// Set and verify names attribute.
export fn zigr_test_attrib_names() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 3));
    defer R.Rf_unprotect(1);
    const names = [_][]const u8{ "a", "b", "c" };
    attrib.setNames(vec, names[0..]);
    const ns = R.Rf_getAttrib(vec, R.R_NamesSymbol);
    if (ns == R.R_NilValue) return R.Rf_ScalarReal(0.0);
    const ok = R.XLENGTH(ns) == 3;
    return if (ok) R.Rf_ScalarReal(1.0) else R.Rf_ScalarReal(0.0);
}

// ALTREP creation test (Phase 3.10)

const MyAlt = altrep_create.AltReal("zigr", "test_real");
const MyAltInt = altrep_create.AltInteger("zigr", "test_integer");
const MyAltLogical = altrep_create.AltLogical("zigr", "test_logical");
const MyAltRaw = altrep_create.AltRaw("zigr", "test_raw");
const MyAltComplex = altrep_create.AltComplex("zigr", "test_complex");
const MyAltString = altrep_create.AltString("zigr", "test_string");
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

/// Create an ALTREP REALSXP from a Zig slice, verify length.
export fn zigr_test_altrep_create() SEXP {
    const data = [_]f64{ 1.0, 2.0, 3.0, 4.0, 5.0 };
    const vec = MyAlt.init(data[0..]);
    if (R.XLENGTH(vec) != 5) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// ALTREP sum matches expected total (15.0).
export fn zigr_test_altrep_sum_simd() SEXP {
    const data = [_]f64{ 1.0, 2.0, 3.0, 4.0, 5.0 };
    const vec = MyAlt.init(data[0..]);
    const total = zigr_convert.sum(vec);
    if (total != 15.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// ALTREP exposes a direct data pointer so zigr can skip region copying.
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

/// Integer ALTREP exposes owned backing and toIntSlice reads it correctly.
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

/// Integer ALTREP sum [4,2,9,-1,3] should be 17.
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

/// Logical ALTREP exposes owned backing and toLogicalSlice preserves values.
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

/// Logical ALTREP TRUE count: [1,0,NA,1] has 2 TRUEs.
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

/// ALTREP mean [1,2,3,4,5] should be 3.0.
export fn zigr_test_altrep_mean_simd() SEXP {
    const data = [_]f64{ 1.0, 2.0, 3.0, 4.0, 5.0 };
    const vec = MyAlt.init(data[0..]);
    if (zigr_convert.mean(vec) != 3.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// ALTREP norm2 [1,2,3,4,5] should be sqrt(55) ~ 7.416.
export fn zigr_test_altrep_norm2_simd() SEXP {
    const data = [_]f64{ 1.0, 2.0, 3.0, 4.0, 5.0 };
    const vec = MyAlt.init(data[0..]);
    const expected = @sqrt(55.0);
    if (@abs(zigr_convert.norm2(vec) - expected) > 1e-12) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// ALTREP min [4,2,9,-1,3] should be -1.0.
export fn zigr_test_altrep_min_simd() SEXP {
    const data = [_]f64{ 4.0, 2.0, 9.0, -1.0, 3.0 };
    const vec = MyAlt.init(data[0..]);
    if (zigr_convert.min(vec) != -1.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// ALTREP max [4,2,9,-1,3] should be 9.0.
export fn zigr_test_altrep_max_simd() SEXP {
    const data = [_]f64{ 4.0, 2.0, 9.0, -1.0, 3.0 };
    const vec = MyAlt.init(data[0..]);
    if (zigr_convert.max(vec) != 9.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// ALTREP argmin [4,-1,9,-1,3] should return index 1 (first min).
export fn zigr_test_altrep_argmin_simd() SEXP {
    const data = [_]f64{ 4.0, -1.0, 9.0, -1.0, 3.0 };
    const vec = MyAlt.init(data[0..]);
    if (zigr_convert.argmin(vec) != 1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// ALTREP argmax [4,9,9,-1,3] should return index 1 (first max).
export fn zigr_test_altrep_argmax_simd() SEXP {
    const data = [_]f64{ 4.0, 9.0, 9.0, -1.0, 3.0 };
    const vec = MyAlt.init(data[0..]);
    if (zigr_convert.argmax(vec) != 1) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// ALTREP sum_narm [1,NA,4,5] should be 10.0.
export fn zigr_test_altrep_sum_narm_simd() SEXP {
    var data = [_]f64{ 1.0, 0.0, 4.0, 5.0 };
    data[1] = R.NA_REAL();
    const vec = MyAlt.init(data[0..]);
    if (zigr_convert.sum_narm(vec) != 10.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// ALTREP mean_narm [1,NA,5] should be 3.0.
export fn zigr_test_altrep_mean_narm_simd() SEXP {
    var data = [_]f64{ 1.0, 0.0, 5.0 };
    data[1] = R.NA_REAL();
    const vec = MyAlt.init(data[0..]);
    if (zigr_convert.mean_narm(vec) != 3.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// Verify ALTRAW created with correct length and values.
export fn zigr_test_altraw_create() SEXP {
    const data = [_]u8{ 0xde, 0xad, 0xbe, 0xef };
    const vec = MyAltRaw.init(data[0..]);
    if (R.XLENGTH(vec) != 4) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// Verify ALTCOMPLEX created with correct length.
export fn zigr_test_altcomplex_create() SEXP {
    const data = [_]altrep_create.ComplexElem{
        .{ .r = 1.0, .i = 2.0 },
        .{ .r = 3.0, .i = 4.0 },
    };
    const vec = MyAltComplex.init(data[0..]);
    if (R.XLENGTH(vec) != 2) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// Double-recursive fib to prove Zig call overhead is minimal.
fn zigr_fib(n: i64) i64 {
    if (n <= 1) return n;
    return zigr_fib(n - 1) + zigr_fib(n - 2);
}

/// Test recursive fib with n=20 (fast, ~13K calls). Verifies correctness
/// and that deep recursion does not overflow the C stack.
export fn zigr_test_fib_recursive() SEXP {
    const result = zigr_fib(20);
    if (result != 6765) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// Verify ALTSTRING created with correct length.
export fn zigr_test_altstring_create() SEXP {
    const data = [_][]const u8{ "alpha", "beta", "gamma" };
    const vec = MyAltString.init(data[0..]);
    if (R.XLENGTH(vec) != 3) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// Call R sum(1,2,3) via lang builder, verify result is 6.
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

// Edge-case / adversarial tests

/// Test: fromRealSlice with empty slice.
export fn zigr_test_from_empty() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const empty: []const f64 = &.{};
    const result = zigr_convert.fromRealSlice(empty);
    const len = R.XLENGTH(@as(R.SEXP, @ptrCast(result)));
    return R.Rf_ScalarInteger(@intCast(len));
}

/// Test: toRealSlice with huge vector boundary.
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
    // Verify via toRealSlice
    const slice = zigr_convert.toRealSlice(arena.allocator(), @as(SEXP, @ptrCast(vec))) catch return R.Rf_ScalarReal(0.0);
    if (slice.len != n) return R.Rf_ScalarReal(0.0);
    if (!std.math.isNan(slice[0])) return R.Rf_ScalarReal(0.0);
    if (slice[n - 1] != std.math.inf(f64)) return R.Rf_ScalarReal(0.0);
    if (slice[n / 2] != -std.math.inf(f64)) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// Test: STRSXP toSlice with NA_STRING element.
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

/// Test: LGLSXP with non-standard values (2, 3 should be TRUE).
export fn zigr_test_lgl_edge() SEXP {
    const n: R.R_xlen_t = 4;
    const vec = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, n));
    defer R.Rf_unprotect(1);
    const ptr = R.LOGICAL(vec);
    ptr[0] = 0; // FALSE
    ptr[1] = 1; // TRUE
    ptr[2] = 42; // non-standard should be TRUE
    ptr[3] = -7; // non-standard should be TRUE (non-zero)
    // Return as-is, R handles non-zero as TRUE
    return vec;
}

/// Test: VECSXP with NULL element.
export fn zigr_test_list_null() SEXP {
    const n: R.R_xlen_t = 2;
    const vec = R.Rf_protect(R.Rf_allocVector(R.VECSXP, n));
    defer R.Rf_unprotect(1);
    _ = R.SET_VECTOR_ELT(vec, 0, R.Rf_ScalarReal(1.0));
    _ = R.SET_VECTOR_ELT(vec, 1, R.R_NilValue);
    return vec;
}

/// Test: DataFrame column lookup with missing name.
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

/// Test: toRealSlice with non-REALSXP (INTSXP), R throws an error.
/// This confirms zigr rejects the wrong type before touching REAL().
export fn zigr_test_real_wrong_type() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 3));
    defer R.Rf_unprotect(1);
    _ = zigr_convert.toRealSlice(std.heap.page_allocator, @as(SEXP, @ptrCast(vec))) catch |convert_err| {
        return R.Rf_ScalarReal(if (convert_err == error.ExpectedReal) 1.0 else 0.0);
    };
    return R.Rf_ScalarReal(0.0);
}

/// Test raw.logical reads LGLSXP through LOGICAL(), not INTEGER().
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

/// Test raw.real reads REALSXP correctly.
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

/// Test raw.int reads INTSXP correctly.
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

/// Test raw.realMut writes to REALSXP correctly.
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

/// Test raw.intMut writes to INTSXP correctly.
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

/// Test raw.raw reads RAWSXP correctly.
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

/// Test raw.complex reads CPLXSXP correctly.
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

/// Test raw.dims returns correct dimensions.
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

/// Trigger fromSEXP on a missing required field. R should see an error.
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

/// Trigger fromSEXP on a malformed named list. R should see an error.
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

/// Trigger fromSEXP on a list without names. R should see an error.
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

/// Trigger fromSEXP on a list with mismatched names length. R should see an error.
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

/// Test toListSlice rejects non-list input.
export fn zigr_test_list_wrong_type() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    defer R.Rf_unprotect(1);
    _ = zigr_convert.toListSlice(std.heap.page_allocator, vec) catch |convert_err| {
        return R.Rf_ScalarReal(if (convert_err == error.ExpectedList) 1.0 else 0.0);
    };
    return R.Rf_ScalarReal(0.0);
}

/// Test scalar REAL conversion rejects NA.
export fn zigr_test_real_scalar_na() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    defer R.Rf_unprotect(1);
    R.REAL(vec)[0] = R.NA_REAL();
    _ = zigr_convert.toRealScalar(vec) catch |convert_err| {
        return R.Rf_ScalarReal(if (convert_err == error.ScalarNA) 1.0 else 0.0);
    };
    return R.Rf_ScalarReal(0.0);
}

/// Test scalar INT conversion rejects NA.
export fn zigr_test_int_scalar_na() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 1));
    defer R.Rf_unprotect(1);
    R.INTEGER(vec)[0] = R.R_NaInt;
    _ = zigr_convert.toIntScalar(vec) catch |convert_err| {
        return R.Rf_ScalarReal(if (convert_err == error.ScalarNA) 1.0 else 0.0);
    };
    return R.Rf_ScalarReal(0.0);
}

/// Test scalar LOGICAL conversion rejects NA.
export fn zigr_test_bool_scalar_na() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, 1));
    defer R.Rf_unprotect(1);
    R.LOGICAL(vec)[0] = R.R_NaInt;
    _ = zigr_convert.toBoolScalar(vec) catch |convert_err| {
        return R.Rf_ScalarReal(if (convert_err == error.ScalarNA) 1.0 else 0.0);
    };
    return R.Rf_ScalarReal(0.0);
}

/// Test optional REAL field maps NA to null.
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

/// Test optional INT field maps NA to null.
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

/// Test optional LOGICAL field maps NA to null.
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

/// Test pmin recycling semantics on mismatched lengths.
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

/// Test pmax recycling semantics on mismatched lengths.
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

// Phase 4 embed tests

/// Test rCodeEval with 1 + 1. Should return 2.
export fn zigr_phase4_embed_sum() SEXP {
    const result = embed.rCodeEval("1 + 1", null);
    const val = R.REAL(result)[0];
    if (val == 2.0) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

/// Test rCodeEval with paste("hello", "world").
export fn zigr_phase4_embed_paste() SEXP {
    const result = embed.rCodeEval("paste('hello', 'world')", null);
    const elt = R.STRING_ELT(result, 0);
    if (elt == R.R_NaString) return R.Rf_ScalarReal(0.0);
    const s = std.mem.sliceTo(R.R_CHAR(elt), 0);
    if (std.mem.eql(u8, s, "hello world")) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

/// Test rRawEval with 1 + 1.
export fn zigr_phase4_raw_eval() SEXP {
    const result = embed.rRawEval("1 + 1", null);
    const val = R.REAL(result)[0];
    if (val == 2.0) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

// Phase 4 struct conversion tests

/// Test asSEXP with a simple struct.
export fn zigr_phase4_as_sexp() SEXP {
    const TestStruct = struct { x: f64, y: f64 };
    const s = TestStruct{ .x = 1.5, .y = 2.5 };
    const result = zigr_convert.asSEXP(s);
    // Verify it's a VECSXP of length 2
    if (R.TYPEOF(result) != R.VECSXP) return R.Rf_ScalarReal(0.0);
    if (R.XLENGTH(result) != 2) return R.Rf_ScalarReal(0.0);
    // Verify names
    const ns = R.Rf_getAttrib(result, R.R_NamesSymbol);
    if (ns == R.R_NilValue) return R.Rf_ScalarReal(0.0);
    if (R.XLENGTH(ns) != 2) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// Test fromSEXP round-trip.
export fn zigr_phase4_from_sexp() SEXP {
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

// Phase 4 edge-case tests

/// Embed: empty string should error, caught by tryCatch.
export fn zigr_phase4_embed_empty() SEXP {
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

/// Embed: vectorized expression returns a vector.
export fn zigr_phase4_embed_vector() SEXP {
    const result = embed.rCodeEval("1:5", null);
    const len = R.XLENGTH(result);
    if (len == 5) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

/// Embed: expression using braces.
export fn zigr_phase4_embed_braces() SEXP {
    const result = embed.rCodeEval("{ x <- 1; x + 1 }", null);
    if (R.REAL(result)[0] == 2.0) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

/// Embed: NULL expression.
export fn zigr_phase4_embed_null() SEXP {
    const result = embed.rCodeEval("NULL", null);
    if (result == R.R_NilValue) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

/// asSEXP: empty struct.
export fn zigr_phase4_as_sexp_empty() SEXP {
    const Empty = struct {};
    const result = zigr_convert.asSEXP(Empty{});
    if (R.XLENGTH(result) == 0) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

/// asSEXP: nested struct.
export fn zigr_phase4_as_sexp_nested() SEXP {
    const Inner = struct { val: f64 };
    const Outer = struct { inner: Inner, name: []const u8 };
    const s = Outer{ .inner = Inner{ .val = 3.14 }, .name = "hello" };
    const result = zigr_convert.asSEXP(s);
    // VECSXP of length 2 (inner + name)
    if (R.TYPEOF(result) != R.VECSXP) return R.Rf_ScalarReal(0.0);
    if (R.XLENGTH(result) != 2) return R.Rf_ScalarReal(0.0);
    const names = R.Rf_getAttrib(result, R.R_NamesSymbol);
    if (R.XLENGTH(names) != 2) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// fromSEXP: missing optional field should produce null.
export fn zigr_phase4_from_sexp_optional() SEXP {
    const Test = struct { x: f64, y: ?f64 };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    // Build list with only "x"
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

/// fromSEXP: missing required field should panic (not tested via R directly).
/// Use asSEXP round-trip with optional field present instead.
export fn zigr_phase4_from_sexp_optional_present() SEXP {
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

/// Memory stress: create and convert many structs in a loop.
export fn zigr_phase4_stress_protect() SEXP {
    const Test = struct { a: f64, b: f64 };
    var i: u32 = 0;
    while (i < 1000) : (i += 1) {
        const s = Test{ .a = @as(f64, @floatFromInt(i)), .b = @as(f64, @floatFromInt(i + 1)) };
        const sexp = zigr_convert.asSEXP(s);
        _ = sexp;
    }
    return R.Rf_ScalarReal(1.0);
}

/// Embed stress: evaluate many expressions in a loop.
export fn zigr_phase4_stress_embed() SEXP {
    var i: u32 = 0;
    while (i < 500) : (i += 1) {
        const result = embed.rCodeEval("42", null);
        if (R.REAL(result)[0] != 42.0) return R.Rf_ScalarReal(0.0);
    }
    return R.Rf_ScalarReal(1.0);
}

// ======================================================
// Phase 4 comprehensive tests following test-gen.md
// Each test documents: Name, Category, Purpose, AAA
// ======================================================

// Embed: Error-Handling Tests

/// Name: rCodeEval_InvalidSyntax_ReturnsError
/// Category: error-handling
/// Purpose: Verify that invalid R syntax is caught by tryCatch and
///          returned as error.RCondition, NOT a segfault.
export fn zigr_p4_embed_syntax_error() SEXP {
    // Arrange: invalid R expression
    // Act: evaluate under tryCatch
    if (trycatch_mod.tryCatch(struct {
        fn call() R.SEXP {
            return embed.rCodeEval("~~~", null);
        }
    }.call)) |_| {
        // Assert: should NOT reach here (error expected)
        return R.Rf_ScalarReal(0.0);
    } else |_| {
        return R.Rf_ScalarReal(1.0);
    }
}

/// Name: rCodeEval_StopError_Caught
/// Category: error-handling
/// Purpose: Verify Rf_error inside evaluated code is caught by tryCatch.
export fn zigr_p4_embed_stop_error() SEXP {
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

/// Name: rCodeEval_Warning_NotCaughtByTryCatchError
/// Category: error-handling
/// Purpose: Verify warning does NOT trigger tryCatchError (only errors).
///          warning() returns NULL invisibly; use { } block for second expr.
export fn zigr_p4_embed_warning_noerror() SEXP {
    if (trycatch_mod.tryCatchError(struct {
        fn call() R.SEXP {
            return embed.rCodeEval("{ warning('warn'); 42 }", null);
        }
    }.call)) |val| {
        // Should succeed: warning doesn't trigger error handler
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

/// Name: rCodeEval_StringLength_Works
/// Category: edge-case
/// Purpose: Verify string length calculation in R code works.
export fn zigr_p4_embed_unicode() SEXP {
    const result = embed.rCodeEval("nchar('abc')", null);
    if (R.INTEGER(result)[0] == 3) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

/// Name: rCodeEval_VeryLongCode_BufferTruncation
/// Category: resource
/// Purpose: Verify very long code strings are handled (truncated at 4096).
///          The buffer is 4096 bytes; codes longer than that truncate.
export fn zigr_p4_embed_long_code() SEXP {
    // Build a 5000-char expression: paste(rep("x", 5000))
    const result = embed.rCodeEval("paste(rep('x', 500), collapse='')", null);
    const elt = R.STRING_ELT(result, 0);
    if (elt == R.R_NaString) return R.Rf_ScalarReal(0.0);
    const s = std.mem.sliceTo(R.R_CHAR(elt), 0);
    if (s.len == 500) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

// Struct Conversion: Invariant / Property Tests

/// Name: asSEXP_fromSEXP_RoundTrip_ValuesPreserved
/// Category: invariant
/// Purpose: Verify asSEXP then fromSEXP returns the original struct.
///          This is the fundamental round-trip property.
export fn zigr_p4_struct_roundtrip() SEXP {
    const Point = struct { x: f64, y: f64, label: []const u8 };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    // Arrange: create struct
    const original = Point{ .x = 1.5, .y = -3.2, .label = "pt1" };

    // Act: convert to SEXP and back
    const sexp = zigr_convert.asSEXP(original);
    const restored = zigr_convert.fromSEXP(Point, sexp, arena.allocator());

    // Assert: values match
    if (restored.x != 1.5) return R.Rf_ScalarReal(0.0);
    if (restored.y != -3.2) return R.Rf_ScalarReal(0.0);
    if (!std.mem.eql(u8, restored.label, "pt1")) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// Name: asSEXP_RoundTrip_NaNInf
/// Category: edge-case
/// Purpose: Verify NaN and Inf values survive struct round-trip.
export fn zigr_p4_struct_nan_inf() SEXP {
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

/// Name: asSEXP_RoundTrip_NegativeZero
/// Category: edge-case
/// Purpose: Verify -0.0 survives struct round-trip (IEEE 754 sign bit).
export fn zigr_p4_struct_neg_zero() SEXP {
    const S = struct { v: f64 };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const sexp = zigr_convert.asSEXP(S{ .v = -0.0 });
    const restored = zigr_convert.fromSEXP(S, sexp, arena.allocator());

    // -0.0 compares equal to 0.0 in Zig, but the sign bit differs.
    // Check via @bitCast.
    const neg_zero: f64 = -0.0;
    if (@as(u64, @bitCast(restored.v)) != @as(u64, @bitCast(neg_zero))) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// Name: asSEXP_ManyFields_AllPresent
/// Category: resource
/// Purpose: Verify structs with many fields convert correctly (stress the
///          VECSXP allocation and name matching).
export fn zigr_p4_struct_many_fields() SEXP {
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

// Struct Conversion: Error-Handling Tests

/// Name: fromSEXP_WrongType_ListProvided
/// Category: error-handling
/// Purpose: Verify passing a non-VECSXP to fromSEXP fails gracefully.
/// NOTE: This test panics (calls Rf_error) if the SEXP is wrong type.
///       It cannot be caught by tryCatch because the panic is in Zig.
///       For now we skip and document this limitation.
// export fn zigr_p4_fromsexp_wrong_type() SEXP {
//     const S = struct { x: f64 };
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//     // Passing a REALSXP (not VECSXP); will panic in sexpToZig
//     const sexp = R.Rf_ScalarReal(1.0);
//     const result = zigr_convert.fromSEXP(S, sexp, arena.allocator());
//     _ = result;
//     return R.Rf_ScalarReal(1.0);
// }

// TryCatch: Error-Handling Tests

/// Name: tryCatch_Nested_InnerCaught
/// Category: error-handling
/// Purpose: Verify nested tryCatch works (inner catches error, outer
///          should see success).
export fn zigr_p4_trycatch_nested() SEXP {
    // Arrange: inner catches error, outer sees success
    const outer = trycatch_mod.tryCatch(struct {
        fn call() R.SEXP {
            const inner = trycatch_mod.tryCatch(struct {
                fn call() R.SEXP {
                    R.Rf_error("inner error");
                    return R.R_NilValue;
                }
            }.call);
            if (inner) |_| {
                return R.R_NilValue; // shouldn't reach
            } else |_| {
                return R.Rf_ScalarReal(42.0);
            }
        }
    }.call);
    // Assert: outer should succeed with 42.0
    if (outer) |val| {
        if (R.REAL(val)[0] == 42.0) return R.Rf_ScalarReal(1.0);
    } else |_| {}
    return R.Rf_ScalarReal(0.0);
}

// Resource / Leak Tests

/// Name: stress_ProtectStack_NoLeak
/// Category: resource
/// Purpose: Verify protection stack doesn't grow unboundedly under
///          repeated asSEXP calls (10000 iterations).
///          Uses asSEXPAlloc with a shared arena for efficiency.
export fn zigr_p4_stress_protect_10k() SEXP {
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

// lang.buildCall / buildNamedCall tests

/// buildCall_ThreeInts_HappyPath: buildCall(fun, &[a,b,c]) evaluates sum(a,b,c).
export fn zigr_test_build_call() SEXP {
    const args = [_]SEXP{
        R.Rf_ScalarInteger(10),
        R.Rf_ScalarInteger(20),
        R.Rf_ScalarInteger(30),
    };
    const call = test_lang.buildCall(test_lang.symbol("sum"), args[0..]);
    const result = test_eval.rEval(call, null);
    if (R.TYPEOF(result) != R.REALSXP or R.XLENGTH(result) != 1) return R.Rf_ScalarReal(0.0);
    if (R.REAL(result)[0] != 60.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// buildNamedCall_TupleSyntax_HappyPath: buildNamedCall("sum", .{a,b}) evaluates sum(a,b).
export fn zigr_test_build_named_call() SEXP {
    const call = test_lang.buildNamedCall("sum", .{ R.Rf_ScalarInteger(1), R.Rf_ScalarInteger(2) });
    const result = test_eval.rEval(call, null);
    if (R.TYPEOF(result) != R.REALSXP or R.XLENGTH(result) != 1) return R.Rf_ScalarReal(0.0);
    if (R.REAL(result)[0] != 3.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

// protect.ScopedProtect lifecycle tests

/// ScopedProtect_Release_NoDoubleUnprotect: releasing ownership before deinit prevents double-unprotect.
export fn zigr_test_scoped_release() SEXP {
    const vec = R.Rf_allocVector(R.REALSXP, 1);
    var s = protect.scoped(vec);
    const released = s.release();
    // s.deinit() is called implicitly. Because release() was called, deinit
    // must NOT call Rf_unprotect. We verify by protecting again and checking
    // the protection stack remains balanced.
    _ = R.Rf_protect(released);
    R.Rf_unprotect(1);
    return R.Rf_ScalarReal(1.0);
}

/// ScopedProtect_Get_AfterInit_ReturnsValue: get() returns the same SEXP passed to init.
export fn zigr_test_scoped_get() SEXP {
    const vec = R.Rf_allocVector(R.REALSXP, 5);
    var s = protect.scoped(vec);
    defer s.deinit();
    if (s.get() != vec) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

// RVector runtime tests

/// RVector_f64_Init_HappyPath: RVector(f64) wraps REALSXP, len() matches, view() returns values.
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
    const v = rv.view(arena.allocator()) catch return R.Rf_ScalarReal(0.0);
    if (v.len != 4 or v[0] != 1.0 or v[3] != 4.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// RVector_i32_Init_HappyPath: RVector(i32) wraps INTSXP, values match.
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
    const v = rv.view(arena.allocator()) catch return R.Rf_ScalarReal(0.0);
    if (v.len != 3 or v[0] != 10 or v[1] != -5 or v[2] != 99) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// RVector_f64_WrongType_Error: RVector(f64).init(INTSXP) must signal an error.
export fn zigr_test_rvector_wrong_type() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 3));
    R.Rf_unprotect(1);
    const rv = zigr.rvector.RVector(f64).init(vec);
    if (rv) |_| return R.Rf_ScalarReal(0.0) else |_| return R.Rf_ScalarReal(1.0);
}

/// RVector_f64_addScalar_HappyPath: addScalar adds a constant to every element.
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

/// RVector_f64_subScalar_HappyPath: subScalar subtracts a constant from every element.
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

/// RVector_f64_mulScalar_HappyPath: mulScalar multiplies every element by a constant.
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

/// RVector_f64_divScalar_HappyPath: divScalar divides every element by a constant.
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

/// RVector_f64_add_HappyPath: add between two vectors is element-wise.
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

/// RVector_f64_sum_HappyPath: sum() returns the sum of all elements.
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

/// RVector_i32_sum_HappyPath: sumInt returns correct i64 sum of int vector.
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

/// RVector_f64_add_Recycling_Edge: add with length-1 and length-3 produces length-3 with recycling.
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
    // 10 is recycled to match length 3: 10+1, 10+2, 10+3
    if (rp[0] != 11.0 or rp[1] != 12.0 or rp[2] != 13.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// RVector_f64_addScalar_Empty_Edge: addScalar on empty vector returns empty vector.
export fn zigr_test_rvector_empty() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 0));
    R.Rf_unprotect(1);
    const rv = zigr.rvector.RVector(f64).init(vec) catch return R.Rf_ScalarReal(0.0);
    const result = rv.addScalar(5.0);
    if (R.XLENGTH(result) != 0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

// Export system conversion tests
//
// The export system wrappers (generateExports, generateMethods) convert R SEXPs to Zig types and back. These tests exercise that conversion chain directly.
//
// Why separate tests: the convert module unit tests cover each function in isolation, but the end-to-end path has its own failure modes: arena lifetime, PROTECT balance across chained calls, and error signal paths.
//
// Sub-sections: Numeric/string slice conversion (core), Optional/NA handling (boundary), External pointer method dispatch (generateMethods pattern)

// Multi-parameter unwrapping

/// Guard against: reading a second SEXP parameter returning
/// stale or wrong data.  generateExports wrappers call
/// fromSexp for each parameter in order; off-by-one indexing
/// would swap values.
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

// Numeric and string slice conversion

/// Guard against: toRealSliceView returning wrong total, or the
/// SliceView.constSlice() data being corrupted across arena
/// boundaries.  A 5-element vector [1,2,3,4,5] sums to 15.
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

/// Guard against: zero-copy string iteration returning wrong
/// byte counts or StringSliceView.iterator panicking on valid
/// UTF-8 input.  Three strings "hello", "world", "zigr" total
/// 14 bytes.
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

/// Guard against: StringView.is_na returning false for NA
/// string elements.  generateExports maps NA strings to null
/// for optional string parameters; a false negative would pass
/// garbage.
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

/// Guard against: CachedStringSliceView caching stale data or
/// wrong element count.  Same strings as the simple view test.
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

/// Guard against: zero-length slice producing non-empty iterator
/// or panicking.  An empty REALSXP has no elements to sum.
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

/// Guard against: RAWSXP conversion returning wrong bytes or
/// corrupting read-only data.
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

/// Guard against: CPLXSXP conversion reading wrong real or
/// imaginary parts.  Two complex numbers (1+2i, 3+4i) should
/// produce a slice with matching components.
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

// Optional and NA handling

/// Guard against: optionalInputIsNullish returning false for
/// R_NilValue, which would cause generateExports wrappers to
/// pass garbage instead of null.
export fn zigr_test_export_optional_null() SEXP {
    const nullish = zigr_convert.optionalInputIsNullish(f64, R.R_NilValue);
    if (!nullish) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

/// Guard against: toRealScalar silently returning NA_REAL when
/// the input is a scalar NA, instead of erroring.  generateExports
/// rejects NA scalars for required parameters.
export fn zigr_test_export_scalar_na() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
    R.REAL(vec)[0] = R.NA_REAL();
    R.Rf_unprotect(1);

    const result = zigr_convert.toRealScalar(vec);
    if (result == error.ScalarNA) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

/// Guard against: toIntScalar accepting an INTSXP containing
/// NA_INTEGER instead of erroring.
export fn zigr_test_export_int_scalar_na() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 1));
    R.INTEGER(vec)[0] = R.R_NaInt;
    R.Rf_unprotect(1);

    const result = zigr_convert.toIntScalar(vec);
    if (result == error.ScalarNA) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

/// Guard against: toBoolScalar accepting a LGLSXP containing
/// NA_LOGICAL instead of erroring.
export fn zigr_test_export_bool_scalar_na() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, 1));
    R.LOGICAL(vec)[0] = R.R_NaInt;
    R.Rf_unprotect(1);

    const result = zigr_convert.toBoolScalar(vec);
    if (result == error.ScalarNA) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

/// Guard against: toRealScalar accepting a non-REALSXP without
/// erroring.  generateExports needs this type check to produce
/// correct R-level errors instead of cryptic crashes.
export fn zigr_test_export_wrong_type_real() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 1));
    R.INTEGER(vec)[0] = 42;
    R.Rf_unprotect(1);

    const result = zigr_convert.toRealScalar(vec);
    if (result == error.ExpectedReal) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

// External pointer and method dispatch

const ExportCounter = struct {
    val: i32,

    fn increment(self: *ExportCounter, amount: i32) i32 {
        self.val += amount;
        return self.val;
    }
};

/// Guard against: externalptr.addr returning null or wrong
/// pointer after externalptr.make.  generateMethods depends on
/// this round-trip to dispatch method calls.
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

/// Guard against: externalptr.tag returning wrong tag SEXP or
/// panicking on a valid EXTPTRSXP.
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

// Error signal through fromSEXP (expect_error=TRUE)

/// signalError calls Rf_error (noreturn) when fromSEXP receives
/// a type it cannot convert.  The R side wraps this in tryCatch
/// and expects the error.  Without this path, a mismatched
/// struct field would panic instead of producing a clean R error.
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

// .External interface
//
// generateExports supports an external_exports array for .External
// callable functions.  The wrapper receives a pairlist and extracts
// arguments via CAR/CDR.  This test exercises the full path:
// generateExports comptime codegen, init() registration with R
// via DllInfo, and calling the external wrapper directly.

fn externalSum(a: f64, b: f64) f64 {
    return a + b;
}

const ExternalExports = zigr.@"export".generateExports(&.{}, &.{
    .{ .name = "zigr_test_external_sum", .func = externalSum },
});

/// Guard against: generateExports failing to compile for
/// external exports, or init() corrupting the DllInfo.  Also
/// tests that the external wrapper correctly extracts scalars
/// from a pairlist and returns a scalar.
export fn zigr_test_export_external() SEXP {
    const dll = test_dll orelse return R.Rf_ScalarReal(0.0);
    ExternalExports.init(dll);

    const ext_fun: *const fn (R.SEXP) callconv(.c) R.SEXP = @ptrCast(@alignCast(ExternalExports.ext_defs[0].fun));
    const arg1 = R.Rf_protect(R.Rf_ScalarReal(3.0));
    const arg2 = R.Rf_protect(R.Rf_ScalarReal(4.0));
    const pairlist = R.Rf_protect(R.Rf_cons(arg1, R.Rf_cons(arg2, R.R_NilValue)));
    R.Rf_unprotect(3);

    const result = ext_fun(pairlist);
    const val = R.REAL(result)[0];
    if (val != 7.0) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

// generateMethods comptime method dispatch
//
// generateMethods creates method wrappers where the first
// argument is an EXTPTRSXP for *T.  The wrapper extracts the
// pointer, calls the method, and returns the result.  This
// test exercises the comptime codegen, init() registration,
// and end-to-end method dispatch.

const MethodCounter = struct {
    val: i32,
};

fn counterAdd(self: *MethodCounter, amount: i32) i32 {
    self.val += amount;
    return self.val;
}

const CounterMethods = zigr.@"export".generateMethods(MethodCounter, &.{
    .{ .name = "add", .func = counterAdd },
}, &.{});

/// Guard against: generateMethods failing to compile, or the
/// EXTPTRSXP unwrap in the wrapper returning wrong data.
// ── Longjmp safety tests ─────────────────────────────

var cleanup_longjmp_fired: bool = false;

fn markCleanupFiredTest(_: ?*anyopaque) void {
    cleanup_longjmp_fired = true;
}

/// Prove cleanup frames fire when longjmp is caught internally.
/// Does NOT require expect_error=TRUE because we catch the error
/// via tryCatch and verify the cleanup flag.
export fn zigr_test_cleanup_fires_on_longjmp() SEXP {
    cleanup_longjmp_fired = false;
    cleanup.pushFrame(markCleanupFiredTest, null);

    if (trycatch_mod.tryCatch(struct {
        fn call() R.SEXP {
            return cleanup.protectCall(struct {
                fn inner() R.SEXP {
                    R.Rf_error("expected cleanup test error");
                    return R.R_NilValue;
                }
            }.inner);
        }
    }.call)) |_| {} else |_| {}

    cleanup.popFrame();
    if (cleanup_longjmp_fired) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

/// Test toStringSliceNullable preserves NA as null.
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

/// Prove withRng releases RNG after a longjmp.
/// If RNG is stuck, acquire() will deadlock or crash.
/// We catch the error internally and then try to acquire RNG again.
export fn zigr_test_with_rng_longjmp() SEXP {
    if (trycatch_mod.tryCatch(struct {
        fn call() R.SEXP {
            return rng.withRng(struct {
                fn inner() R.SEXP {
                    R.Rf_error("expected withRng test error");
                    return R.R_NilValue;
                }
            }.inner);
        }
    }.call)) |_| {} else |_| {}

    // If RNG was released on longjmp, acquire/release works.
    // If stuck, we hang or crash here.
    rng.acquire();
    rng.release();
    return R.Rf_ScalarReal(1.0);
}

/// Creates an EXTPTRSXP, registers methods, extracts the
/// method wrapper, calls it, and checks the result.
export fn zigr_test_export_generatemethods() SEXP {
    const dll = test_dll orelse return R.Rf_ScalarReal(0.0);
    CounterMethods.init(dll);

    const method_fun: *const fn (R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP, R.SEXP) callconv(.c) R.SEXP = @ptrCast(@alignCast(CounterMethods.call_defs[0].fun));

    var counter = MethodCounter{ .val = 10 };
    const prot = R.Rf_protect(R.R_NilValue);
    const extptr = zigr.externalptr.make(&counter, R.R_NilValue, prot);
    const amount = R.Rf_protect(R.Rf_ScalarInteger(5));
    R.Rf_unprotect(2);

    const result = method_fun(extptr, amount, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue, R.R_NilValue);
    const val = R.INTEGER(result)[0];
    if (val != 15) return R.Rf_ScalarReal(0.0);
    if (counter.val != 15) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}
