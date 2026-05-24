//! R serialization helpers.
//!
//! Convert any R object to a raw vector and back. ALTREP objects are
//! materialized during serialization and restored as regular vectors
//! on deserialization. The returned SEXP is unprotected: callers must
//! protect it if used across GC-triggering calls.

const R = @import("R");

/// Serialize any R object to a RAWSXP. The result owns the serialized
/// bytes; protect it if the caller will use it across further R API
/// calls that may allocate.
pub fn toVector(sexp: R.SEXP) R.SEXP {
    return R.R_SerializeToVector(sexp);
}

/// Deserialize a RAWSXP produced by toVector back to the original R
/// object. The returned SEXP is unprotected: protect it before passing
/// to functions that may trigger GC.
pub fn fromVector(sexp: R.SEXP) R.SEXP {
    return R.R_UnserializeFromVector(sexp);
}
