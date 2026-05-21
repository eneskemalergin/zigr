//! Zig bindings for R's C API.

pub const sexp = @import("sexp.zig");
pub const protect = @import("protect.zig");
pub const convert = @import("convert.zig");
const err = @import("error.zig");
pub const @"error" = err;
pub const interrupt = @import("interrupt.zig");
pub const rng = @import("rng.zig");
pub const memory = @import("memory.zig");
pub const reverse_ffi = @import("reverse_ffi.zig");

test {
    _ = sexp;
    _ = protect;
    _ = convert;
    _ = err;
    _ = interrupt;
    _ = memory;
    _ = rng;
    _ = reverse_ffi;
}
