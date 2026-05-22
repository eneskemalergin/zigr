//! R serialization helpers.

const R = @import("R");

/// Serialize any R object to a raw vector. Result is unprotected.
pub fn toVector(sexp: R.SEXP) R.SEXP {
    return R.R_SerializeToVector(sexp);
}

/// Deserialize a raw vector back to the original R object. Unprotected.
pub fn fromVector(sexp: R.SEXP) R.SEXP {
    return R.R_UnserializeFromVector(sexp);
}
