//! ALTREP vector access.
//!
//! Direct data access can materialize a vector; element access need not.

const std = @import("std");
const R = @import("R");
const sexp_mod = @import("sexp.zig");

pub fn isAltRep(sexp: R.SEXP) bool {
    return R.ALTREP(sexp) != 0;
}

pub fn data1(sexp: R.SEXP) R.SEXP {
    return R.R_altrep_data1(sexp);
}

pub fn data2(sexp: R.SEXP) R.SEXP {
    return R.R_altrep_data2(sexp);
}

/// R interns the class name, so this borrowed slice survives GC.
pub fn className(sexp: R.SEXP) []const u8 {
    if (!isAltRep(sexp)) return "";
    const cn = R.R_altrep_class_name(sexp);
    if (cn == R.R_NilValue) return "";
    return sexp_mod.charsxpBytes(cn);
}
