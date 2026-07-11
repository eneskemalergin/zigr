//! R-managed memory allocator.
//!
//! Provides a std.mem.Allocator backed by R's checked memory functions
//! (R_chk_calloc / R_chk_free).  CRAN's memory checker tracks
//! allocations made through these functions.
//!
//! When to use which allocator:
//!   RAllocator        - CRAN-tracked, fundamentally aligned memory within one .Call.
//!   ArenaAllocator    - Caller-owned conversion scratch with ordinary Zig cleanup.
//!   UnwindArena       - Generated-boundary scratch that must also survive R longjmp.
//!   c_allocator       - Use for long-lived wrappers (ALTREP backing, EXTPTRSXP) that
//!                       must outlive .Call and survive GC finalizers (R_chk_* is inappropriate).
//!   TwoTierArena      - Stack-first arena (8 KB buffer, heap spill) used by export wrappers.

const std = @import("std");
const R = @import("R");
const cleanup = @import("cleanup");

const AllocContext = struct {};

fn rAlloc(_: *anyopaque, len: usize, alignment: std.mem.Alignment, _: usize) ?[*]u8 {
    // R_chk_calloc has the same fundamental-alignment contract as C calloc.
    // Do not claim support for over-aligned Zig types through this allocator.
    if (std.mem.Alignment.compare(alignment, .gt, .of(std.c.max_align_t))) return null;
    const actual_len = @max(len, @alignOf(std.c.max_align_t));
    return @as(?[*]u8, @ptrCast(R.R_chk_calloc(actual_len, 1)));
}

fn rFree(_: *anyopaque, buf: []u8, _: std.mem.Alignment, _: usize) void {
    R.R_chk_free(@as(?*anyopaque, @ptrCast(buf.ptr)));
}

fn rResize(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) bool {
    return false;
}

fn rRemap(_: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, _: usize) ?[*]u8 {
    // C realloc preserves fundamental alignment but cannot promise an
    // over-aligned Zig allocation. Keep the fast remap for ordinary types;
    // force std.mem.Allocator's safe allocate/copy/free fallback otherwise.
    if (std.mem.Alignment.compare(alignment, .gt, .of(std.c.max_align_t))) return null;
    const actual_len = @max(new_len, @alignOf(std.c.max_align_t));
    return @as(?[*]u8, @ptrCast(R.R_chk_realloc(@as(?*anyopaque, @ptrCast(memory.ptr)), actual_len)));
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

/// Call-scoped native scratch arena with normal-return and R-longjmp cleanup.
///
/// The backing `ArenaAllocator` is created only on the first allocation. Its
/// mutable state lives inline in `cleanup`'s thread-local frame, rather than
/// in a stack-local pointer, so the same state is valid when
/// `R_UnwindProtect` invokes cleanup after a non-local exit. Use it only
/// under `cleanup.protectCall`/`protectCallData` when an R call may longjmp.
/// On ordinary return, call `deinit()` exactly once.
pub const UnwindArena = struct {
    const State = struct {
        arena: std.heap.ArenaAllocator,

        fn fire(self: *@This()) void {
            self.arena.deinit();
        }
    };

    state: ?*State = null,

    pub fn init() UnwindArena {
        return .{};
    }

    pub fn allocator(self: *UnwindArena) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = allocFn,
                .free = freeFn,
                .resize = resizeFn,
                .remap = remapFn,
            },
        };
    }

    /// Releases all allocated scratch on the normal path and disarms the
    /// matching inline cleanup frame. This is a no-op when nothing allocated.
    pub fn deinit(self: *UnwindArena) void {
        const state = self.state orelse return;
        cleanup.popFrame();
        state.arena.deinit();
        self.state = null;
    }

    fn ensureState(self: *UnwindArena) *State {
        if (self.state) |state| return state;
        const state = cleanup.pushFrameInline(
            State,
            .{ .arena = std.heap.ArenaAllocator.init(std.heap.page_allocator) },
            State.fire,
        );
        self.state = state;
        return state;
    }

    fn allocFn(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ra: usize) ?[*]u8 {
        const self: *UnwindArena = @ptrCast(@alignCast(ctx));
        return self.ensureState().arena.allocator().rawAlloc(len, alignment, ra);
    }

    fn freeFn(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize) void {
        // Call-arena allocations are released together by `deinit`. Keeping
        // this a true no-op also means a higher inline conversion cleanup can
        // safely run after a longjmp without dereferencing its dead wrapper
        // frame through the allocator context.
    }

    fn resizeFn(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) bool {
        return false;
    }

    fn remapFn(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) ?[*]u8 {
        return null;
    }
};

test "UnwindArena allocates lazily and releases on normal return" {
    var arena = UnwindArena.init();
    defer arena.deinit();
    try std.testing.expect(arena.state == null);

    const bytes = try arena.allocator().alloc(u8, 32);
    bytes[0] = 1;
    try std.testing.expect(arena.state != null);
}
