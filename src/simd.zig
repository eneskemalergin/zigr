//! SIMD lane configuration.
//!
//! Single constant shared by all @Vector operations. Picked to minimize
//! loop overhead on the widest range of targets. The compiler splits
//! wide vectors automatically for narrower SIMD units (e.g. @Vector(8,f64)
//! on 128-bit NEON becomes four 2-wide ops).
//!
//! 8 was chosen empirically: on this AVX2 test machine it's faster than 4
//! (3.56ms vs 4.26ms for vectorsum of 1e7) because fewer loop iterations
//! matter more than fitting in a single instruction.

pub const f64_lanes = 8;
pub const i32_lanes = 8;
