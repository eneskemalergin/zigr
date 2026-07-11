//! R-aware native allocators.
//!
//! Long-lived objects use `c_allocator`: R's checked allocator cannot serve
//! GC finalizers that run after the originating R call.

const std = @import("std");
const R = @import("R");
const cleanup = @import("cleanup");

const AllocContext = struct {};

fn rAlloc(_: *anyopaque, len: usize, alignment: std.mem.Alignment, _: usize) ?[*]u8 {
    // R's checked allocator has C's fundamental-alignment limit.
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
    // Over-aligned realloc must use Zig's allocate-copy-free fallback.
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

/// Its state lives in the cleanup stack so R longjmp can release it safely.
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
        // A later longjmp must not dereference a dead allocator context.
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
