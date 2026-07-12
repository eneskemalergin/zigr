//! Factor construction using R's string matching and collation rules.
//!
//! ALTREP input is copied to an ordinary string vector before R sorts and matches it.
//! Construction can allocate and longjmp; callers keep input reachable across the call.
//! Returned factors are unprotected and independent of the input codes.

const std = @import("std");
const R = @import("R");
const cleanup = @import("cleanup");
const protect = @import("protect.zig");

pub const FactorError = error{
    WrongType,
    TooLong,
};

fn factorCall(data: ?*anyopaque) R.SEXP {
    const vec: R.SEXP = @ptrCast(@alignCast(data.?));
    var input = protect.scoped(vec);
    defer input.deinit();

    const len = R.XLENGTH(input.get());
    const n: c_int = @intCast(len);

    var working = protect.scoped(if (R.ALTREP(input.get()) != 0) R.Rf_allocVector(R.STRSXP, len) else input.get());
    defer working.deinit();
    if (R.ALTREP(input.get()) != 0) {
        for (0..@as(usize, @intCast(n))) |i| {
            R.SET_STRING_ELT(working.get(), @intCast(i), R.STRING_ELT(input.get(), @intCast(i)));
        }
    }

    var first = protect.scoped(R.Rf_match(working.get(), working.get(), 0));
    defer first.deinit();

    const first_ptr: [*]const c_int = @ptrCast(R.INTEGER(first.get()));
    var level_count: R.R_xlen_t = 0;
    for (0..@as(usize, @intCast(n))) |i| {
        if (R.STRING_ELT(working.get(), @intCast(i)) == R.R_NaString) continue;
        if (first_ptr[i] == @as(c_int, @intCast(i + 1))) level_count += 1;
    }

    var unique = protect.scoped(R.Rf_allocVector(R.STRSXP, level_count));
    defer unique.deinit();
    var level_index: R.R_xlen_t = 0;
    for (0..@as(usize, @intCast(n))) |i| {
        const value = R.STRING_ELT(working.get(), @intCast(i));
        if (value == R.R_NaString) continue;
        if (first_ptr[i] != @as(c_int, @intCast(i + 1))) continue;
        R.SET_STRING_ELT(unique.get(), level_index, value);
        level_index += 1;
    }

    var order = protect.scoped(R.Rf_allocVector(R.INTSXP, level_count));
    defer order.deinit();
    const order_ptr: [*]c_int = @ptrCast(R.INTEGER(order.get()));
    for (0..@as(usize, @intCast(level_count))) |i| order_ptr[i] = @intCast(i);
    if (level_count > 1) R.R_orderVector1(order_ptr, @intCast(level_count), unique.get(), @as(R.Rboolean, 1), @as(R.Rboolean, 0));

    var levels = protect.scoped(R.Rf_allocVector(R.STRSXP, level_count));
    defer levels.deinit();
    for (0..@as(usize, @intCast(level_count))) |i| {
        R.SET_STRING_ELT(levels.get(), @intCast(i), R.STRING_ELT(unique.get(), order_ptr[i]));
    }

    var codes = protect.scoped(R.Rf_match(levels.get(), working.get(), R.R_NaInt));
    defer codes.deinit();
    _ = R.Rf_setAttrib(codes.get(), R.R_LevelsSymbol, levels.get());

    var class = protect.scoped(R.Rf_allocVector(R.STRSXP, 1));
    defer class.deinit();
    R.SET_STRING_ELT(class.get(), 0, R.Rf_mkChar("factor"));
    _ = R.Rf_setAttrib(codes.get(), R.R_ClassSymbol, class.get());

    return codes.get();
}

pub fn asFactorChecked(vec: R.SEXP) FactorError!R.SEXP {
    if (vec == null or R.TYPEOF(vec) != R.STRSXP) return error.WrongType;
    const len = R.XLENGTH(vec);
    if (len < 0 or len > std.math.maxInt(c_int)) return error.TooLong;
    return cleanup.protectCallData(factorCall, @ptrCast(vec));
}

pub fn asFactor(vec: R.SEXP) R.SEXP {
    return asFactorChecked(vec) catch R.R_NilValue;
}
