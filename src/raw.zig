//! Checked borrowed R vector views.
//!
//! Slices borrow R storage and die at the next GC-triggering R call. Nonempty
//! views do not materialize ALTREP values: they fail when R has no direct pointer.
//! Mutable views modify the supplied vector in place without copying it, so
//! callers must ensure that mutation respects R's copy-on-write rules.

const R = @import("R");
const sexp = @import("sexp.zig");
const Rcomplex = @import("convert.zig").Rcomplex;

/// Failure modes returned by checked raw vector views.
pub const RawViewError = error{
    NullPointer,
    ExpectedReal,
    ExpectedInteger,
    ExpectedLogical,
    ExpectedRaw,
    ExpectedComplex,
    DirectPointerUnavailable,
    NegativeLength,
    MisalignedData,
};

fn typedData(comptime T: type, comptime expected: c_int, comptime type_error: RawViewError, sexp_: R.SEXP) RawViewError![]const T {
    if (sexp_ == null) return error.NullPointer;
    if (sexp.typeTag(sexp_) != expected) return type_error;

    const raw_len = sexp.fastLength(sexp_);
    if (raw_len < 0) return error.NegativeLength;
    const n: usize = @intCast(raw_len);
    if (n == 0) return &.{};

    const ptr = sexp.fastDataPtr(sexp_) orelse return error.DirectPointerUnavailable;
    if (@intFromPtr(ptr) % @alignOf(T) != 0) return error.MisalignedData;
    return @as([*]const T, @ptrCast(@alignCast(ptr)))[0..n];
}

fn typedDataMut(comptime T: type, comptime expected: c_int, comptime type_error: RawViewError, sexp_: R.SEXP) RawViewError![]T {
    return @constCast(try typedData(T, expected, type_error, sexp_));
}

/// Returns a direct REALSXP view without materializing ALTREP storage.
pub fn real(sexp_: R.SEXP) RawViewError![]const f64 {
    return typedData(f64, R.REALSXP, error.ExpectedReal, sexp_);
}

/// Returns a direct INTSXP view without materializing ALTREP storage.
pub fn int(sexp_: R.SEXP) RawViewError![]const i32 {
    return typedData(i32, R.INTSXP, error.ExpectedInteger, sexp_);
}

/// Returns a direct LGLSXP view without materializing ALTREP storage.
pub fn logical(sexp_: R.SEXP) RawViewError![]const i32 {
    return typedData(i32, R.LGLSXP, error.ExpectedLogical, sexp_);
}

/// Returns a mutable direct REALSXP view without materializing ALTREP storage.
pub fn realMut(sexp_: R.SEXP) RawViewError![]f64 {
    return typedDataMut(f64, R.REALSXP, error.ExpectedReal, sexp_);
}

/// Returns a mutable direct INTSXP view without materializing ALTREP storage.
pub fn intMut(sexp_: R.SEXP) RawViewError![]i32 {
    return typedDataMut(i32, R.INTSXP, error.ExpectedInteger, sexp_);
}

/// Returns a direct RAWSXP view without materializing ALTREP storage.
pub fn raw(sexp_: R.SEXP) RawViewError![]const u8 {
    return typedData(u8, R.RAWSXP, error.ExpectedRaw, sexp_);
}

/// Returns a mutable direct RAWSXP view without materializing ALTREP storage.
pub fn rawMut(sexp_: R.SEXP) RawViewError![]u8 {
    return typedDataMut(u8, R.RAWSXP, error.ExpectedRaw, sexp_);
}

/// Returns a direct CPLXSXP view without materializing ALTREP storage.
pub fn complex(sexp_: R.SEXP) RawViewError![]const Rcomplex {
    return typedData(Rcomplex, R.CPLXSXP, error.ExpectedComplex, sexp_);
}

/// Returns a mutable direct CPLXSXP view without materializing ALTREP storage.
pub fn complexMut(sexp_: R.SEXP) RawViewError![]Rcomplex {
    return typedDataMut(Rcomplex, R.CPLXSXP, error.ExpectedComplex, sexp_);
}

pub fn dims(sexp_: R.SEXP) struct { rows: R.R_xlen_t, cols: R.R_xlen_t } {
    return .{ .rows = R.R_nrow(sexp_), .cols = R.R_ncol(sexp_) };
}
