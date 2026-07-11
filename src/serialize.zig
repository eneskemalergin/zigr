//! R serialization helpers.
//!
//! Results are unprotected after R serialization calls.

const R = @import("R");

pub fn toVector(sexp: R.SEXP) R.SEXP {
    return R.R_SerializeToVector(sexp);
}

pub fn fromVector(sexp: R.SEXP) R.SEXP {
    return R.R_UnserializeFromVector(sexp);
}
