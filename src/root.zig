//! Zig bindings for R's C API.
//!
//! Maps R's SEXP type system into Zig types (sexp.zig).
//! Wraps PROTECT/UNPROTECT so you do not leak (protect.zig).
//! Converts between Zig and R data layouts (convert.zig).

pub const sexp = @import("sexp.zig");
pub const protect = @import("protect.zig");
pub const convert = @import("convert.zig");

test {
    _ = sexp;
    _ = protect;
    _ = convert;
}
