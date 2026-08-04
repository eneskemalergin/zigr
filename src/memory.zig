//! R-aware native allocators.
//!
//! Long-lived objects use `c_allocator`: R's checked allocator cannot serve
//! GC finalizers that run after the originating R call.

const std = @import("std");
const R = @import("R");
const cleanup = @import("cleanup");

const AllocContext = struct {};

/// Byte totals include successful allocation, resize, and remap deltas.
pub const AllocationStats = struct {
    allocations: usize = 0,
    frees: usize = 0,
    resizes: usize = 0,
    remaps: usize = 0,
    bytes_allocated: usize = 0,
    bytes_freed: usize = 0,
    live_bytes: usize = 0,
    peak_live_bytes: usize = 0,
};

/// Counts successful operations; the wrapper and parent must outlive its allocator.
pub const CountingAllocator = struct {
    parent: std.mem.Allocator,
    stats: AllocationStats = .{},

    pub fn init(parent: std.mem.Allocator) CountingAllocator {
        return .{ .parent = parent };
    }

    pub fn allocator(self: *CountingAllocator) std.mem.Allocator {
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

    fn recordGrowth(self: *CountingAllocator, amount: usize) void {
        self.stats.bytes_allocated += amount;
        self.stats.live_bytes += amount;
        self.stats.peak_live_bytes = @max(self.stats.peak_live_bytes, self.stats.live_bytes);
    }

    fn recordShrink(self: *CountingAllocator, amount: usize) void {
        self.stats.bytes_freed += amount;
        self.stats.live_bytes -= amount;
    }

    fn allocFn(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ra: usize) ?[*]u8 {
        const self: *CountingAllocator = @ptrCast(@alignCast(ctx));
        const result = self.parent.rawAlloc(len, alignment, ra) orelse return null;
        self.stats.allocations += 1;
        self.recordGrowth(len);
        return result;
    }

    fn freeFn(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, ra: usize) void {
        const self: *CountingAllocator = @ptrCast(@alignCast(ctx));
        self.parent.rawFree(buf, alignment, ra);
        self.stats.frees += 1;
        self.recordShrink(buf.len);
    }

    fn resizeFn(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, ra: usize) bool {
        const self: *CountingAllocator = @ptrCast(@alignCast(ctx));
        if (!self.parent.rawResize(buf, alignment, new_len, ra)) return false;
        self.stats.resizes += 1;
        if (new_len > buf.len) {
            self.recordGrowth(new_len - buf.len);
        } else {
            self.recordShrink(buf.len - new_len);
        }
        return true;
    }

    fn remapFn(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, ra: usize) ?[*]u8 {
        const self: *CountingAllocator = @ptrCast(@alignCast(ctx));
        const result = self.parent.rawRemap(buf, alignment, new_len, ra) orelse return null;
        self.stats.remaps += 1;
        if (new_len > buf.len) {
            self.recordGrowth(new_len - buf.len);
        } else {
            self.recordShrink(buf.len - new_len);
        }
        return result;
    }
};

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
    unwind_frame: ?cleanup.FrameHandle = null,

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
        self.state = null;
        if (self.unwind_frame) |frame| {
            self.unwind_frame = null;
            if (!cleanup.releaseFrame(frame)) return;
        }
        state.arena.deinit();
    }

    fn ensureState(self: *UnwindArena) *State {
        if (self.state) |state| return state;
        const registration = cleanup.pushFrameInlineWithHandle(
            State,
            .{ .arena = std.heap.ArenaAllocator.init(std.heap.page_allocator) },
            State.fire,
        );
        self.state = registration.state;
        self.unwind_frame = registration.handle;
        return registration.state;
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

test "UnwindArena deinit preserves a newer cleanup frame" {
    const Guard = struct {
        fn fire(_: *@This()) void {}
    };

    var arena = UnwindArena.init();
    _ = try arena.allocator().alloc(u8, 1);

    const newer = cleanup.pushFrameInlineWithHandle(Guard, .{}, Guard.fire);
    arena.deinit();
    const preserved = cleanup.releaseFrame(newer.handle);
    if (!preserved) cleanup.popFrame();
    try std.testing.expect(preserved);
}

test "CountingAllocator records successful allocation lifetime" {
    var counting = CountingAllocator.init(std.testing.allocator);
    const allocator = counting.allocator();

    const bytes = try allocator.alloc(u8, 32);
    try std.testing.expectEqual(1, counting.stats.allocations);
    try std.testing.expectEqual(32, counting.stats.live_bytes);
    try std.testing.expectEqual(32, counting.stats.peak_live_bytes);

    allocator.free(bytes);
    try std.testing.expectEqual(1, counting.stats.frees);
    try std.testing.expectEqual(0, counting.stats.live_bytes);
    try std.testing.expectEqual(32, counting.stats.bytes_freed);
}

test "CountingAllocator ignores failed allocations" {
    var storage: [8]u8 = undefined;
    var fixed = std.heap.FixedBufferAllocator.init(&storage);
    var counting = CountingAllocator.init(fixed.allocator());

    const result = counting.allocator().alloc(u8, 16);

    try std.testing.expectError(error.OutOfMemory, result);
    try std.testing.expectEqual(0, counting.stats.allocations);
    try std.testing.expectEqual(0, counting.stats.live_bytes);
}

test "CountingAllocator records successful resize and remap growth" {
    var storage: [64]u8 = undefined;
    var fixed = std.heap.FixedBufferAllocator.init(&storage);
    var counting = CountingAllocator.init(fixed.allocator());
    const allocator = counting.allocator();
    var bytes = try allocator.alloc(u8, 8);

    try std.testing.expect(allocator.resize(bytes, 16));
    bytes = bytes.ptr[0..16];
    bytes = allocator.remap(bytes, 24) orelse return error.OutOfMemory;
    try std.testing.expectEqual(1, counting.stats.resizes);
    try std.testing.expectEqual(1, counting.stats.remaps);
    try std.testing.expectEqual(24, counting.stats.live_bytes);
    try std.testing.expectEqual(24, counting.stats.peak_live_bytes);

    allocator.free(bytes);
    try std.testing.expectEqual(0, counting.stats.live_bytes);
}
