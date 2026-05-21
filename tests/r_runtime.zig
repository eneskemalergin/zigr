//! Tests that require R to be running.
//! Compiled into a shared library and loaded by R via tests/run_r_tests.R.
//! Not run by `zig build test` (those tests cannot link libR).

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
export fn zigr_isnull_check(vec: SEXP) i32 {
    if (R.Rf_isNull(vec) != 0) return 1;
    return 0;
}

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
