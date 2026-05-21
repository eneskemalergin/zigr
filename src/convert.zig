//! Convert between Zig native types and R SEXPs.
//!
//! R works in terms of SEXPs. Zig works in terms of slices and structs.
//! These functions bridge the gap - they allocate R vectors from slices
//! and project R vector contents into Zig memory. Every allocation goes
//! through an explicit allocator so callers control lifetime.

const std = @import("std");
const SEXP = @import("sexp.zig").SEXP;

/// Project an R vector into a Zig slice. The slice borrows from the SEXP:
/// writing to it modifies R's memory directly. Do not hold this slice
/// across an R API call that could trigger GC.
pub fn toSlice(sexp: SEXP, comptime T: type) ![]T {
    _ = sexp;
    @compileError("not yet implemented");
}

/// Allocate an R vector from a Zig slice and return its SEXP. The caller
/// must protect the returned SEXP if it needs to survive past the next
/// R allocation. The slice data is copied, so you can free it after.
pub fn fromSlice(allocator: std.mem.Allocator, comptime T: type, slice: []const T) !SEXP {
    _ = allocator;
    _ = slice;
    @compileError("not yet implemented");
}

// No tests yet. Every function here is @compileError, so any caller
// that compiles has already validated the signatures. Tests come when
// I implement the first real conversion.
