//! R-managed memory allocator.
//!
//! Provides a std.mem.Allocator backed by R's checked memory functions
//! (R_chk_calloc / R_chk_free).  CRAN's memory checker tracks
//! allocations made through these functions.
//!
//! When to use which allocator:
//!   RAllocator        - Use for CRAN-tracked memory within a single .Call invocation.
//!   ArenaAllocator    - Use for conversion scratch buffers. Freed as a group at call end.
//!   c_allocator       - Use for long-lived wrappers (ALTREP backing, EXTPTRSXP) that
//!                       must outlive .Call and survive GC finalizers (R_chk_* is inappropriate).
//!   TwoTierArena      - Stack-first arena (8 KB buffer, heap spill) used by export wrappers.

const std = @import("std");
const R = @import("R");

const AllocContext = struct {};

fn rAlloc(_: *anyopaque, len: usize, _: std.mem.Alignment, _: usize) ?[*]u8 {
    return @as(?[*]u8, @ptrCast(R.R_chk_calloc(len, 1)));
}

fn rFree(_: *anyopaque, buf: []u8, _: std.mem.Alignment, _: usize) void {
    R.R_chk_free(@as(?*anyopaque, @ptrCast(buf.ptr)));
}

fn rResize(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) bool {
    return false;
}

fn rRemap(_: *anyopaque, memory: []u8, _: std.mem.Alignment, new_len: usize, _: usize) ?[*]u8 {
    return @as(?[*]u8, @ptrCast(R.R_chk_realloc(@as(?*anyopaque, @ptrCast(memory.ptr)), new_len)));
}

pub const RAllocator = std.mem.Allocator{
    .ptr = @ptrCast(@constCast(&(AllocContext{}))),
    .vtable = &.{
        .alloc = rAlloc,
        .free = rFree,
        .resize = rResize,
        .remap = rRemap,
    },
};
