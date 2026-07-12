//! ALTREP vector access.
//!
//! Direct data access can materialize a vector; element access need not.

const R = @import("R");
const sexp_mod = @import("sexp.zig");

pub fn isAltRep(sexp: R.SEXP) bool {
    return sexp != null and R.ALTREP(sexp) != 0;
}

pub fn data1(sexp: R.SEXP) R.SEXP {
    return R.R_altrep_data1(sexp);
}

pub fn data2(sexp: R.SEXP) R.SEXP {
    return R.R_altrep_data2(sexp);
}

fn symbolName(symbol: R.SEXP) []const u8 {
    if (symbol == R.R_NilValue or R.TYPEOF(symbol) != R.SYMSXP) return "";
    const print_name = R.PRINTNAME(symbol);
    if (print_name == null or R.TYPEOF(print_name) != R.CHARSXP) return "";
    return sexp_mod.charsxpBytes(print_name);
}

/// Returns call-scoped UTF-8 bytes for the interned class-name symbol.
/// Copy the slice if it must outlive the current `.Call` or `.External` entry.
pub fn className(sexp: R.SEXP) []const u8 {
    if (!isAltRep(sexp)) return "";
    return symbolName(R.R_altrep_class_name(sexp));
}

/// Returns call-scoped UTF-8 bytes for the interned package-name symbol.
/// Copy the slice if it must outlive the current `.Call` or `.External` entry.
pub fn classPackage(sexp: R.SEXP) []const u8 {
    if (!isAltRep(sexp)) return "";
    return symbolName(R.R_altrep_class_package(sexp));
}
