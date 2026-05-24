// Zero-copy access to R vector data.
// Returns slices into R's memory directly -- no allocation, no copy.
// The caller must not hold the slice across a GC-triggering call.

const R = @import("R");
const sexp = @import("sexp.zig");

const Rcomplex = extern struct { r: f64, i: f64 };

/// Read-only slice of a REALSXP. No copy.
pub fn real(sexp_: R.SEXP) []const f64 {
    return R.REAL(sexp_)[0..sexp.xlength(sexp_)];
}

/// Read-only slice of an INTSXP. No copy.
pub fn int(sexp_: R.SEXP) []const i32 {
    return R.INTEGER(sexp_)[0..sexp.xlength(sexp_)];
}

/// Read-only slice of a LGLSXP. No copy.
pub fn logical(sexp_: R.SEXP) []const i32 {
    return R.LOGICAL(sexp_)[0..sexp.xlength(sexp_)];
}

/// Mutable slice of a REALSXP. No copy.
pub fn realMut(sexp_: R.SEXP) []f64 {
    return R.REAL(sexp_)[0..sexp.xlength(sexp_)];
}

/// Mutable slice of an INTSXP. No copy.
pub fn intMut(sexp_: R.SEXP) []i32 {
    return R.INTEGER(sexp_)[0..sexp.xlength(sexp_)];
}

/// Read-only slice of a RAWSXP. No copy.
pub fn raw(sexp_: R.SEXP) []const u8 {
    return R.RAW(sexp_)[0..sexp.xlength(sexp_)];
}

/// Mutable slice of a RAWSXP. No copy.
pub fn rawMut(sexp_: R.SEXP) []u8 {
    return R.RAW(sexp_)[0..sexp.xlength(sexp_)];
}

/// Read-only slice of a CPLXSXP (Rcomplex). No copy.
pub fn complex(sexp_: R.SEXP) []const Rcomplex {
    return @as([*]const Rcomplex, @ptrCast(@alignCast(R.COMPLEX(sexp_).?)))[0..sexp.xlength(sexp_)];
}

/// Mutable slice of a CPLXSXP. No copy.
pub fn complexMut(sexp_: R.SEXP) []Rcomplex {
    return @as([*]Rcomplex, @ptrCast(@alignCast(R.COMPLEX(sexp_).?)))[0..sexp.xlength(sexp_)];
}

/// Returns (rows, cols) for a matrix SEXP.
pub fn dims(sexp_: R.SEXP) struct { rows: i32, cols: i32 } {
    return .{ .rows = R.Rf_nrows(sexp_), .cols = R.Rf_ncols(sexp_) };
}
