//! SIMD lane counts.
//!
//! Wide vectors reduce loop overhead; Zig lowers them for narrower targets.

pub const f64_lanes = 8;
pub const i32_lanes = 8;
