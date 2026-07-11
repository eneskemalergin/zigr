//! S4 helpers.
//!
//! `newS4Object` skips R's initializer, so use it only for simple classes
//! whose representation needs no setup.

const R = @import("R");
const symbols = @import("symbols.zig");

fn toSym(name: []const u8) R.SEXP {
    return symbols.install(name);
}

pub fn isS4(sexp: R.SEXP) bool {
    return R.Rf_isS4(sexp) != 0;
}

pub fn setS4Object(sexp: R.SEXP, value: bool) void {
    _ = R.Rf_asS4(sexp, if (value) @as(R.Rboolean, 1) else 0, 1);
}

pub fn hasSlot(sexp: R.SEXP, name: []const u8) bool {
    return R.R_has_slot(sexp, toSym(name)) != 0;
}

pub fn getSlot(sexp: R.SEXP, name: []const u8) R.SEXP {
    return R.R_do_slot(sexp, toSym(name));
}

pub fn setSlot(sexp: R.SEXP, name: []const u8, value: R.SEXP) void {
    _ = R.R_do_slot_assign(sexp, toSym(name), value);
}

/// Skips custom initialization and validity hooks. The result is unprotected.
pub fn newS4Object(class_name: []const u8, slot_count: i32) R.SEXP {
    const obj = R.Rf_protect(R.Rf_allocVector(R.VECSXP, slot_count));
    _ = R.Rf_asS4(obj, 1, 1);
    const cls = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    R.SET_STRING_ELT(cls, 0, R.Rf_mkCharLenCE(
        @ptrCast(class_name.ptr),
        @intCast(class_name.len),
        @as(R.cetype_t, @intCast(R.CE_UTF8)),
    ));
    _ = R.Rf_classgets(obj, cls);
    R.Rf_unprotect(2);
    return obj;
}
