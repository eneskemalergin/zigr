const std = @import("std");
const print = std.debug.print;
const Alignment = std.mem.Alignment;

const CountingAllocator = struct {
    parent: std.mem.Allocator,
    alloc_count: usize = 0,
    free_count: usize = 0,
    resize_count: usize = 0,
    bytes_allocated: usize = 0,
    bytes_freed: usize = 0,

    fn allocator(self: *@This()) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = allocFn,
                .resize = resizeFn,
                .remap = remapFn,
                .free = freeFn,
            },
        };
    }

    fn allocFn(ctx: *anyopaque, len: usize, alignment: Alignment, ra: usize) ?[*]u8 {
        const self = @as(*@This(), @ptrCast(@alignCast(ctx)));
        self.alloc_count += 1;
        self.bytes_allocated += len;
        return self.parent.rawAlloc(len, alignment, ra);
    }

    fn resizeFn(ctx: *anyopaque, buf: []u8, alignment: Alignment, new_len: usize, ra: usize) bool {
        const self = @as(*@This(), @ptrCast(@alignCast(ctx)));
        self.resize_count += 1;
        return self.parent.rawResize(buf, alignment, new_len, ra);
    }

    fn remapFn(ctx: *anyopaque, buf: []u8, alignment: Alignment, new_len: usize, ra: usize) ?[*]u8 {
        const self = @as(*@This(), @ptrCast(@alignCast(ctx)));
        return self.parent.rawRemap(buf, alignment, new_len, ra);
    }

    fn freeFn(ctx: *anyopaque, buf: []u8, alignment: Alignment, ra: usize) void {
        const self = @as(*@This(), @ptrCast(@alignCast(ctx)));
        self.free_count += 1;
        self.bytes_freed += buf.len;
        self.parent.rawFree(buf, alignment, ra);
    }
};

fn run_vectorsum(allocator: std.mem.Allocator, n: usize) !f64 {
    const buf = try allocator.alloc(f64, n);
    defer allocator.free(buf);
    for (0..n) |i| buf[i] = @floatFromInt(i);
    var total: f64 = 0.0;
    for (buf) |v| total += v;
    return total;
}

fn run_matrix_mult(allocator: std.mem.Allocator, n: usize) !f64 {
    const a = try allocator.alloc(f64, n * n);
    defer allocator.free(a);
    const b = try allocator.alloc(f64, n * n);
    defer allocator.free(b);
    const c = try allocator.alloc(f64, n * n);
    defer allocator.free(c);

    for (0..n * n) |i| {
        a[i] = @floatFromInt(i % 100);
        b[i] = @floatFromInt(i % 50);
        c[i] = 0.0;
    }

    for (0..n) |i| {
        for (0..n) |k| {
            const aik = a[i * n + k];
            for (0..n) |j| {
                c[i * n + j] += aik * b[k * n + j];
            }
        }
    }

    var total: f64 = 0.0;
    for (c) |v| total += v;
    return total;
}

pub fn main() !void {
    {
        var ca = CountingAllocator{ .parent = std.heap.c_allocator };
        const alloc = ca.allocator();
        const result = try run_vectorsum(alloc, 10_000_000);
        const expected = @as(f64, 4999999.5) * 10_000_000.0;
        const ok = @abs(result - expected) / expected < 1e-10;
        print("task47_vectorsum:\n", .{});
        print("  correct: {}\n", .{ok});
        print("  c_alloc_count: {}\n", .{ca.alloc_count});
        print("  c_free_count: {}\n", .{ca.free_count});
        print("  c_resize_count: {}\n", .{ca.resize_count});
        print("  c_bytes_allocated: {}\n", .{ca.bytes_allocated});
        print("  c_bytes_freed: {}\n", .{ca.bytes_freed});
        print("  c_bytes_resident: {}\n", .{ca.bytes_allocated - ca.bytes_freed});
    }

    {
        var ca = CountingAllocator{ .parent = std.heap.c_allocator };
        const alloc = ca.allocator();
        const result = try run_matrix_mult(alloc, 256);
        _ = result;
        print("task47_matrix_mult:\n", .{});
        print("  c_alloc_count: {}\n", .{ca.alloc_count});
        print("  c_free_count: {}\n", .{ca.free_count});
        print("  c_resize_count: {}\n", .{ca.resize_count});
        print("  c_bytes_allocated: {}\n", .{ca.bytes_allocated});
        print("  c_bytes_freed: {}\n", .{ca.bytes_freed});
        print("  c_bytes_resident: {}\n", .{ca.bytes_allocated - ca.bytes_freed});
    }
}
