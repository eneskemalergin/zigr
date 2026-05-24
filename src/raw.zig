// Zero-copy access to R vector data.
// Returns slices into R's memory directly -- no allocation, no copy.
// The caller must not hold the slice across a GC-triggering call.

const R = @import("R");

/// Read-only slice of a REALSXP. No copy.
pub fn real(sexp: R.SEXP) []const f64 {
    return R.REAL(sexp)[0..@as(usize, @intCast(R.XLENGTH(sexp)))];
}

/// Read-only slice of an INTSXP. No copy.
pub fn int(sexp: R.SEXP) []const i32 {
    return R.INTEGER(sexp)[0..@as(usize, @intCast(R.XLENGTH(sexp)))];
}

/// Read-only slice of a LGLSXP. No copy.
pub fn logical(sexp: R.SEXP) []const i32 {
    return R.LOGICAL(sexp)[0..@as(usize, @intCast(R.XLENGTH(sexp)))];
}

/// Mutable slice of a REALSXP. No copy.
pub fn realMut(sexp: R.SEXP) []f64 {
    return R.REAL(sexp)[0..@as(usize, @intCast(R.XLENGTH(sexp)))];
}

/// Mutable slice of an INTSXP. No copy.
pub fn intMut(sexp: R.SEXP) []i32 {
    return R.INTEGER(sexp)[0..@as(usize, @intCast(R.XLENGTH(sexp)))];
}

/// Read-only slice of a RAWSXP. No copy.
pub fn raw(sexp: R.SEXP) []const u8 {
    return R.RAW(sexp)[0..@as(usize, @intCast(R.XLENGTH(sexp)))];
}

/// Mutable slice of a RAWSXP. No copy.
pub fn rawMut(sexp: R.SEXP) []u8 {
    return R.RAW(sexp)[0..@as(usize, @intCast(R.XLENGTH(sexp)))];
}

/// Read-only slice of a CPLXSXP (Rcomplex). No copy.
pub fn complex(sexp: R.SEXP) []const R.Rcomplex {
    return R.COMPLEX(sexp)[0..@as(usize, @intCast(R.XLENGTH(sexp)))];
}

/// Mutable slice of a CPLXSXP. No copy.
pub fn complexMut(sexp: R.SEXP) []R.Rcomplex {
    return R.COMPLEX(sexp)[0..@as(usize, @intCast(R.XLENGTH(sexp)))];
}

/// Returns (rows, cols) for a matrix SEXP.
pub fn dims(sexp: R.SEXP) struct { rows: i32, cols: i32 } {
    return .{ .rows = R.Rf_nrows(sexp), .cols = R.Rf_ncols(sexp) };
}
