//! S4 object support.
//!
//! Wraps R's S4 detection and slot access functions.
//! Names are self-explanatory (isS4, getSlot, setSlot, hasSlot).

const R = @import("R");
const symbols = @import("symbols.zig");

fn toSym(name: []const u8) R.SEXP {
    return symbols.install(name);
}

pub fn isS4(sexp: R.SEXP) bool {
    return R.Rf_isS4(sexp) != 0;
}

/// Sets both the S4 bit and the object bit so R recognizes it as S4.
pub fn setS4Object(sexp: R.SEXP, value: bool) void {
    _ = R.Rf_asS4(sexp, if (value) @as(R.Rboolean, 1) else 0, 1);
}

pub fn hasSlot(sexp: R.SEXP, name: []const u8) bool {
    return R.R_has_slot(sexp, toSym(name)) != 0;
}

/// Returns R_NilValue if the slot does not exist (no error signaled).
pub fn getSlot(sexp: R.SEXP, name: []const u8) R.SEXP {
    return R.R_do_slot(sexp, toSym(name));
}

pub fn setSlot(sexp: R.SEXP, name: []const u8, value: R.SEXP) void {
    _ = R.R_do_slot_assign(sexp, toSym(name), value);
}
