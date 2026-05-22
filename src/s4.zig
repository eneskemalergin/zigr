//! S4 object support.
//!
//! Wraps R's S4 object functions: detection, slot access, and the S4
//! bit on SEXP objects.

const R = @import("R");

fn toSym(name: []const u8) R.SEXP {
    var buf: [256:0]u8 = undefined;
    const n = @min(name.len, buf.len - 1);
    @memcpy(buf[0..n], name[0..n]);
    buf[n] = 0;
    return R.Rf_install(@ptrCast(&buf));
}

/// True if the SEXP is an S4 object.
pub fn isS4(sexp: R.SEXP) bool {
    return R.Rf_isS4(sexp) != 0;
}

/// Set or unset the S4 bit on a SEXP. Also toggles the object bit and
/// class attribute so R recognizes the object as S4 (complete=1).
pub fn setS4Object(sexp: R.SEXP, value: bool) void {
    _ = R.Rf_asS4(sexp, if (value) @as(R.Rboolean, 1) else 0, 1);
}

/// True if the object has a slot with the given name.
pub fn hasSlot(sexp: R.SEXP, name: []const u8) bool {
    return R.R_has_slot(sexp, toSym(name)) != 0;
}

/// Get the value of a slot by name. Returns R_NilValue if the slot
/// does not exist.
pub fn getSlot(sexp: R.SEXP, name: []const u8) R.SEXP {
    return R.R_do_slot(sexp, toSym(name));
}

/// Set the value of a slot by name.
pub fn setSlot(sexp: R.SEXP, name: []const u8, value: R.SEXP) void {
    _ = R.R_do_slot_assign(sexp, toSym(name), value);
}
