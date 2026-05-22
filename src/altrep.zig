//! ALTREP vector detection and access.
//!
//! R's ALTREP framework allows vectors to define custom storage backends.
//! Reading via REAL() or INTEGER() forces materialization of the entire
//! vector. Use data1/data2 to access backing data for known classes, or
//! use INTEGER_ELT / REAL_ELT for element-by-element access that
//! dispatches through the ALTREP method table without materializing.

const std = @import("std");
const R = @import("R");

/// True if the SEXP is an ALTREP vector (has a custom data backend).
pub fn isAltRep(sexp: R.SEXP) bool {
    return R.ALTREP(sexp) != 0;
}

/// Get the first data pointer from an ALTREP vector. The meaning depends
/// on the ALTREP class — for compact_intseq (1:n) this is the start value
/// as a SEXP-wrapped integer. For other classes it may be a pointer to
/// external memory.
pub fn data1(sexp: R.SEXP) R.SEXP {
    return R.R_altrep_data1(sexp);
}

/// Get the second data pointer from an ALTREP vector. For compact_intseq
/// this is the step/increment. For memory-mapped vectors this may be the
/// file path or offset.
pub fn data2(sexp: R.SEXP) R.SEXP {
    return R.R_altrep_data2(sexp);
}

/// The ALTREP class name as a string. Returns empty slice if not ALTREP.
pub fn className(sexp: R.SEXP) []const u8 {
    if (!isAltRep(sexp)) return "";
    const cn = R.R_altrep_class_name(sexp);
    if (cn == R.R_NilValue) return "";
    return std.mem.sliceTo(R.R_CHAR(cn), 0);
}
