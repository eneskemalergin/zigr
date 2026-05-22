//! S4 object support.
//!
//! Wraps R's S4 object functions: detection, slot access, and the S4
//! bit on SEXP objects.

const R = @import("R");

/// True if the SEXP is an S4 object.
pub fn isS4(sexp: R.SEXP) bool {
    return R.Rf_isS4(sexp) != 0;
}

/// Set or unset the S4 bit on a SEXP.
pub fn setS4Object(sexp: R.SEXP, value: bool) void {
    _ = R.Rf_asS4(sexp, if (value) @as(R.Rboolean, 1) else 0, 0);
}

/// True if the object has a slot with the given name.
pub fn hasSlot(sexp: R.SEXP, name: []const u8) bool {
    const sym = R.Rf_install(@ptrCast(name.ptr));
    return R.R_has_slot(sexp, sym) != 0;
}

/// Get the value of a slot by name. Returns R_NilValue if the slot
/// does not exist.
pub fn getSlot(sexp: R.SEXP, name: []const u8) R.SEXP {
    const sym = R.Rf_install(@ptrCast(name.ptr));
    return R.R_do_slot(sexp, sym);
}

/// Set the value of a slot by name.
pub fn setSlot(sexp: R.SEXP, name: []const u8, value: R.SEXP) void {
    const sym = R.Rf_install(@ptrCast(name.ptr));
    _ = R.R_do_slot_assign(sexp, sym, value);
}
