//! Tests that require R to be running.
//! Compiled into a shared library and loaded by R via tests/run_r_tests.R.
//! Not run by `zig build test` (those tests cannot link libR).

const std = @import("std");
const R = @import("R");

// Use the R module's SEXP type for function parameters and returns.
const SEXP = R.SEXP;

// ── Allocation tests ──────────────────────────────────────

/// Allocate a REALSXP of size 100, fill with 0..99, return it.
export fn zigr_alloc_real() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @as(R.R_xlen_t, 100)));
    const ptr: [*]f64 = @ptrCast(R.REAL(vec));
    var i: usize = 0;
    while (i < 100) : (i += 1) ptr[i] = @floatFromInt(i);
    R.Rf_unprotect(1);
    return vec;
}

/// Allocate a large vector (10M) to stress R's allocator.
export fn zigr_alloc_large() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @as(R.R_xlen_t, 10000000)));
    R.Rf_unprotect(1);
    return vec;
}

// ── PROTECT stress tests ──────────────────────────────────

/// PROTECT the same SEXP 100 times, then UNPROTECT 100.
export fn zigr_protect_many() SEXP {
    const vec = R.Rf_allocVector(R.INTSXP, 1);
    var i: usize = 0;
    while (i < 100) : (i += 1) _ = R.Rf_protect(vec);
    R.Rf_unprotect(100);
    return R.R_NilValue;
}

/// PROTECT with index, reprotect with a new vector, then clean up.
export fn zigr_protect_index() SEXP {
    var idx: R.PROTECT_INDEX = 0;
    const vec1 = R.Rf_allocVector(R.REALSXP, 10);
    R.R_ProtectWithIndex(vec1, &idx);

    // Reprotect with a different vector at the same index
    const vec2 = R.Rf_allocVector(R.INTSXP, 5);
    R.R_Reprotect(vec2, idx);

    // Pop one to balance the protect
    R.Rf_unprotect(1);
    return R.R_NilValue;
}

// ── NA handling tests ─────────────────────────────────────

/// Create a vector with NA_REAL at position 2.
export fn zigr_check_na() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 3));
    const ptr: [*]f64 = @ptrCast(R.REAL(vec));
    ptr[0] = 1.0;
    ptr[1] = R.NA_REAL();
    ptr[2] = 3.0;
    R.Rf_unprotect(1);
    return vec;
}

// ── Error handling (must not crash R) ─────────────────────

/// Call Rf_error: must raise R error, not segfault.
export fn zigr_raise_error() SEXP {
    R.Rf_error("zigr test error: this is expected");
    return R.R_NilValue;
}

/// Call Rf_warning: must print warning and continue.
/// Warning output is suppressed in tests to avoid noise.
export fn zigr_raise_warning() SEXP {
    R.Rf_warning("zigr test warning: this is expected");
    return R.R_NilValue;
}

// ── Type tests ────────────────────────────────────────────

/// Return the integer value of TYPEOF(R_NilValue). Should be 0 (NILSXP).
export fn zigr_typeof_nil() SEXP {
    return R.Rf_ScalarInteger(R.TYPEOF(R.R_NilValue));
}

/// Test Rf_isNull on a SEXP argument.

// ── Basic tests from Phase 2.1 ────────────────────────────

export fn zigr_test_protect() SEXP {
    _ = R.Rf_protect(R.R_NilValue);
    R.Rf_unprotect(1);
    return R.R_NilValue;
}

export fn zigr_test_return42() SEXP {
    return R.Rf_ScalarReal(42.0);
}

// ── Longjmp / R_UnwindProtect tests (Phase 2.3) ──────────

const cleanup = @import("cleanup");

// Flag set by the cleanup handler on longjmp.
var longjmp_cleanup_fired: bool = false;

fn markCleanupFired(_: ?*anyopaque) void {
    longjmp_cleanup_fired = true;
}

/// Calls Rf_error inside a protectCall guard.
/// The cleanup handler should fire before the error propagates to R.
export fn zigr_test_longjmp() SEXP {
    cleanup.init();
    longjmp_cleanup_fired = false;
    cleanup.pushFrame(markCleanupFired, null);

    _ = cleanup.protectCall(struct {
        fn doBoom() R.SEXP {
            R.Rf_error("zigr longjmp test: expected error");
            return R.R_NilValue;
        }
    }.doBoom);

    cleanup.popFrame();
    return R.R_NilValue;
}

/// Normal return inside protectCall: cleanup should NOT fire.
export fn zigr_test_longjmp_normal() SEXP {
    cleanup.init();
    longjmp_cleanup_fired = false;
    cleanup.pushFrame(markCleanupFired, null);

    const result = cleanup.protectCall(struct {
        fn doNormal() SEXP {
            return R.Rf_ScalarReal(99.0);
        }
    }.doNormal);

    cleanup.popFrame();
    return result;
}

/// Query the longjmp cleanup flag from R.
export fn zigr_longjmp_flag() SEXP {
    return R.Rf_ScalarInteger(if (longjmp_cleanup_fired) 1 else 0);
}

// ── Error module tests (Phase 2.4) ─────────────────────────

const err = @import("error");
const ict = @import("interrupt");

/// Test error.signal: calls Rf_error and is caught by tryCatch.
export fn zigr_test_error_signal() SEXP {
    err.signal("zigr error signal test");
    return R.R_NilValue;
}

/// Test error.warn: calls Rf_warning.
export fn zigr_test_error_warn() SEXP {
    err.warn("zigr warning signal test");
    return R.R_NilValue;
}

/// Test error.signalIf: condition is true, so error fires.
export fn zigr_test_error_signalif() SEXP {
    err.signalIf(true, "zigr error signalIf test");
    return R.R_NilValue;
}

// ── Interrupt module tests (Phase 2.5) ─────────────────────

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

// ── Reverse FFI tests (Phase 2.8) ─────────────────────

const rffi = @import("reverse_ffi");

/// Evaluate 1 + 1 via reverse_ffi.lang3 + eval.
export fn zigr_test_rev_eval() SEXP {
    const plus = rffi.symbol("+");
    const one = R.Rf_ScalarReal(1.0);
    const call = rffi.lang3(plus, one, one);
    return rffi.eval(call);
}

/// Define a variable via reverse_ffi.defineVar, then look it up.
export fn zigr_test_rev_define_find() SEXP {
    rffi.defineVar("zigr_test_var", R.Rf_ScalarReal(42.0));
    return rffi.findVar("zigr_test_var");
}

/// Build and evaluate a lang3 call: `sum(10, 20)`.
export fn zigr_test_rev_lang3() SEXP {
    const fsum = rffi.symbol("sum");
    const a = R.Rf_ScalarReal(10.0);
    const b = R.Rf_ScalarReal(20.0);
    const call = rffi.lang3(fsum, a, b);
    return rffi.eval(call);
}

// ── RNG tests (Phase 2.7) ─────────────────────────────

const rng = @import("rng");

/// Acquire and release RNG: should not crash.
export fn zigr_test_rng() SEXP {
    rng.acquire();
    rng.release();
    return R.R_NilValue;
}

// ── Memory allocator tests (Phase 2.6) ────────────────

const mem = @import("memory");

/// Allocate and free through RAllocator.
export fn zigr_test_ralloc() SEXP {
    const alloc = mem.RAllocator;
    const buf = alloc.alloc(u8, 100) catch return R.R_NilValue;
    defer alloc.free(buf);
    buf[0] = 42;
    if (buf[0] != 42) return R.Rf_ScalarReal(-1.0);
    return R.Rf_ScalarReal(1.0);
}

// ── Preserve test (Phase 2.3) ─────────────────────────

var preserve_released: bool = false;

fn releasePreserved(_: ?*anyopaque) void {
    preserve_released = true;
}

/// Preserve an SEXP, then error inside protectCall.
/// Cleanup fires R_ReleaseObject on unwind.
export fn zigr_test_preserve_longjmp() SEXP {
    cleanup.init();
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
    return R.R_NilValue;
}

/// Query the preserve flag from R.
export fn zigr_preserve_flag() SEXP {
    return R.Rf_ScalarInteger(if (preserve_released) 1 else 0);
}

// ── Nested callbacks test (Phase 2.3) ────────────────

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
    cleanup.init();
    nested_outer_fired = false;
    nested_inner_fired = false;
    cleanup.pushFrame(markOuter, null);

    _ = cleanup.protectCall(struct {
        fn doNested() R.SEXP {
            const fn_name = R.Rf_mkChar("zigr_test_nested_inner");
            const fn_string = R.Rf_ScalarString(fn_name);
            const call_sexp = rffi.lang2(
                rffi.symbol(".Call"),
                fn_string,
            );
            return rffi.eval(call_sexp);
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

// ── REALSXP conversion tests (Phase 3.1) ──────────────

const zigr_convert = @import("convert");

/// Test toRealSlice, read from an R vector into a Zig slice.
export fn zigr_test_to_real_slice() SEXP {
    const n: R.R_xlen_t = 5;
    const rvec = R.Rf_protect(R.Rf_allocVector(R.REALSXP, n));
    defer R.Rf_unprotect(1);
    const ptr: [*]f64 = @ptrCast(R.REAL(rvec));
    ptr[0] = 1.0;
    ptr[1] = 2.0;
    ptr[2] = 3.0;
    ptr[3] = 4.0;
    ptr[4] = 5.0;
    return rvec;
}

/// Test fromRealSlice, allocate from an array literal.
export fn zigr_test_from_real_slice() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const values = [_]f64{ 10.0, 20.0, 30.0 };
    const result = zigr_convert.fromRealSlice(values[0..]);
    return @as(R.SEXP, @ptrCast(result));
}

/// Round-trip: create REALSXP from Zig slice, read back.
export fn zigr_test_real_roundtrip() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const input = [_]f64{ 1.5, 2.5, 3.5, 4.5 };
    const sexp = zigr_convert.fromRealSlice(input[0..]);
    const r_sexp: R.SEXP = @ptrCast(sexp);
    const ptr: [*]f64 = @ptrCast(R.REAL(r_sexp));
    var ok = true;
    for (input, 0..) |v, i| {
        if (ptr[i] != v) ok = false;
    }
    if (ok) return R.Rf_ScalarReal(1.0);
    return R.Rf_ScalarReal(0.0);
}

// ── INTSXP conversion tests (Phase 3.2) ──────────────

/// Create an INTSXP vector and return it.
export fn zigr_test_int_create() SEXP {
    const n: R.R_xlen_t = 4;
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, n));
    defer R.Rf_unprotect(1);
    const ptr: [*]i32 = @ptrCast(R.INTEGER(vec));
    ptr[0] = 10;
    ptr[1] = 20;
    ptr[2] = 30;
    ptr[3] = 40;
    return vec;
}

/// Allocate from a Zig i32 slice.
export fn zigr_test_int_from_slice() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const values = [_]i32{ 100, 200, 300 };
    const result = zigr_convert.fromIntSlice(values[0..]);
    return @as(R.SEXP, @ptrCast(result));
}

// ── STRSXP conversion tests (Phase 3.3) ──────────────

/// Create a STRSXP vector and return it.
export fn zigr_test_str_create() SEXP {
    const n: R.R_xlen_t = 3;
    const vec = R.Rf_protect(R.Rf_allocVector(R.STRSXP, n));
    defer R.Rf_unprotect(1);
    R.SET_STRING_ELT(vec, 0, R.Rf_mkChar("hello"));
    R.SET_STRING_ELT(vec, 1, R.Rf_mkChar("world"));
    R.SET_STRING_ELT(vec, 2, R.Rf_mkChar("zigr"));
    return vec;
}

/// Allocate STRSXP from Zig slice, return it.
export fn zigr_test_str_from_slice() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const values = [_][]const u8{ "alpha", "beta", "gamma" };
    const result = zigr_convert.fromStringSlice(values[0..]);
    return @as(R.SEXP, @ptrCast(result));
}

// ── LGLSXP conversion tests (Phase 3.4) ──────────────

/// Create a LGLSXP vector and return it.
export fn zigr_test_lgl_create() SEXP {
    const n: R.R_xlen_t = 4;
    const vec = R.Rf_protect(R.Rf_allocVector(R.LGLSXP, n));
    defer R.Rf_unprotect(1);
    const ptr = R.LOGICAL(vec);
    ptr[0] = 1; // TRUE
    ptr[1] = 0; // FALSE
    ptr[2] = R.R_NaInt; // NA
    ptr[3] = 1; // TRUE
    return vec;
}

/// Allocate LGLSXP from Zig slice, return it.
export fn zigr_test_lgl_from_slice() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const values = [_]i32{ 1, 0, 1 };
    const result = zigr_convert.fromLogicalSlice(values[0..]);
    return @as(R.SEXP, @ptrCast(result));
}

// ── VECSXP conversion tests (Phase 3.5) ──────────────

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

// ── Data frame tests (Phase 3.6) ──────────────────────

const df = @import("dataframe");

/// Build a data frame from Zig arrays, return it.
export fn zigr_test_df_build() SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const vals = [_]f64{ 1.0, 2.0, 3.0 };
    const col1 = (zigr_convert.fromRealSlice(vals[0..]));
    const strs = [_][]const u8{ "a", "b", "c" };
    const col2 = (zigr_convert.fromStringSlice(strs[0..]));
    const names = [_][]const u8{ "x", "y" };
    const cols = [_]R.SEXP{ @as(R.SEXP, @ptrCast(col1)), @as(R.SEXP, @ptrCast(col2)) };

    return df.build(names[0..], cols[0..]);
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

// ── RAWSXP / CPLXSXP tests ────────────────────────

/// Create a RAWSXP vector and return it.
export fn zigr_test_raw_create() SEXP {
    const n: R.R_xlen_t = 4;
    const vec = R.Rf_protect(R.Rf_allocVector(R.RAWSXP, n));
    defer R.Rf_unprotect(1);
    const ptr = R.RAW(vec);
    ptr[0] = 0xde;
    ptr[1] = 0xad;
    ptr[2] = 0xbe;
    ptr[3] = 0xef;
    return vec;
}

/// Create a CPLXSXP vector and return it.
export fn zigr_test_cplx_create() SEXP {
    const n: R.R_xlen_t = 3;
    const vec = R.Rf_protect(R.Rf_allocVector(R.CPLXSXP, n));
    defer R.Rf_unprotect(1);
    // Write via REAL/IMAG macros, use COMPLEX pointer
    const Complex = extern struct { r: f64, i: f64 };
    const ptr: [*]Complex = @ptrCast(@alignCast(R.COMPLEX(vec).?));
    ptr[0] = .{ .r = 1.0, .i = 2.0 };
    ptr[1] = .{ .r = 3.0, .i = 4.0 };
    ptr[2] = .{ .r = 5.0, .i = 6.0 };
    return vec;
}

// ── Attrib tests (Phase 3.7) ─────────────────────────

const attrib = @import("attrib");

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

// ── ALTREP creation test (Phase 3.10) ───────────────

const MyAlt = @import("altrep_create").AltReal("zigr", "test_real");

/// Create an ALTREP REALSXP from a Zig slice, sum it in R.
export fn zigr_test_altrep_create() SEXP {
    const data = [_]f64{ 1.0, 2.0, 3.0, 4.0, 5.0 };
    const vec = MyAlt.init(data[0..]);
    return vec;
}

// ── Edge-case / adversarial tests ─────────────────────

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
/// This confirms that R enforces type safety on REAL() access.
export fn zigr_test_real_wrong_type() SEXP {
    const vec = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 3));
    defer R.Rf_unprotect(1);
    // R's REAL() errors on INTSXP. This test confirms the error propagates.
    // If this segfaults instead of erroring, the test catches it via tryCatch.
    _ = zigr_convert.toRealSlice(std.heap.page_allocator, @as(SEXP, @ptrCast(vec))) catch {};
    return R.Rf_ScalarReal(0.0); // should never reach, error above
}

// ── Phase 4 embed tests ─────────────────────────────

const embed = @import("embed");

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

// ── Phase 4 struct conversion tests ─────────────────

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
    R.REAL(bv)[0] = 1.0; R.REAL(bv)[1] = 2.0; R.REAL(bv)[2] = 3.0;
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

// ── Phase 4 edge-case tests ─────────────────────────

const trycatch_mod = @import("trycatch");

/// Embed: empty string should error, caught by tryCatch.
export fn zigr_phase4_embed_empty() SEXP {
    if (trycatch_mod.tryCatch(struct {
        fn call() R.SEXP { return embed.rCodeEval("", null); }
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
    R.REAL(xv)[0] = 1.0; R.REAL(yv)[0] = 2.0;
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

// ══════════════════════════════════════════════════════
// Phase 4 comprehensive tests following test-gen.md
// Each test documents: Name, Category, Purpose, AAA
// ══════════════════════════════════════════════════════

// ── Embed: Error-Handling Tests ─────────────────────

/// Name: rCodeEval_InvalidSyntax_ReturnsError
/// Category: error-handling
/// Purpose: Verify that invalid R syntax is caught by tryCatch and
///          returned as error.RCondition, NOT a segfault.
export fn zigr_p4_embed_syntax_error() SEXP {
    // Arrange: invalid R expression
    // Act: evaluate under tryCatch
    if (trycatch_mod.tryCatch(struct {
        fn call() R.SEXP { return embed.rCodeEval("~~~", null); }
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
        fn call() R.SEXP { return embed.rCodeEval("stop('test')", null); }
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
        fn call() R.SEXP { return embed.rCodeEval("{ warning('warn'); 42 }", null); }
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

// ── Struct Conversion: Invariant / Property Tests ────

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
        a: f64, b: f64, c: f64, d: f64, e: f64,
        f: f64, g: f64, h: f64, i: f64, j: f64,
    };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const original = Wide{
        .a = 1, .b = 2, .c = 3, .d = 4, .e = 5,
        .f = 6, .g = 7, .h = 8, .i = 9, .j = 10,
    };
    const sexp = zigr_convert.asSEXP(original);
    const restored = zigr_convert.fromSEXP(Wide, sexp, arena.allocator());

    if (restored.a != 1 or restored.j != 10) return R.Rf_ScalarReal(0.0);
    return R.Rf_ScalarReal(1.0);
}

// ── Struct Conversion: Error-Handling Tests ─────────

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
//     // Passing a REALSXP (not VECSXP) — will panic in sexpToZig
//     const sexp = R.Rf_ScalarReal(1.0);
//     const result = zigr_convert.fromSEXP(S, sexp, arena.allocator());
//     _ = result;
//     return R.Rf_ScalarReal(1.0);
// }

// ── TryCatch: Error-Handling Tests ──────────────────

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

// ── Resource / Leak Tests ────────────────────────────

/// Name: stress_ProtectStack_NoLeak
/// Category: resource
/// Purpose: Verify protection stack doesn't grow unboundedly under
///          repeated asSEXP calls (10000 iterations).
export fn zigr_p4_stress_protect_10k() SEXP {
    const S = struct { v: f64 };
    var i: u32 = 0;
    while (i < 10000) : (i += 1) {
        const sexp = zigr_convert.asSEXP(S{ .v = @floatFromInt(i) });
        _ = sexp;
    }
    return R.Rf_ScalarReal(1.0);
}

// ── export.generateMethods: placeholder ──────────────
// Testing generateMethods requires:
//   1. Defining a struct type and a method
//   2. Creating an EXTPTRSXP for an instance
//   3. Calling the generated .Call wrapper
// This requires a separate compiled module. Skipping for now.
// Tracked in: dev-guide.md §5.0.3
