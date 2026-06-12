const std = @import("std");
const R = @import("R");
const raw = @import("zigr").raw;

const SEXP = R.SEXP;

fn radixSortF64(items: []f64) void {
    if (items.len < 2) return;

    const n = items.len;
    const buf = @as([*]u64, @ptrCast(@alignCast(items.ptr)))[0..n];
    const sign_bit: u64 = 1 << 63;

    // Transform: flip sign bit for positives, flip all bits for negatives
    for (buf) |*b| {
        const v = b.*;
        b.* = if (v & sign_bit != 0) ~v else v ^ sign_bit;
    }

    // Allocate temp once, reuse across all 8 passes of ping-pong sort.
    // Use c_allocator (malloc), no need for zeroed memory.
    var stack_temp: [256]u64 = undefined;
    const temp = if (n <= 256) blk: {
        break :blk stack_temp[0..n];
    } else blk: {
        const ptr = std.heap.c_allocator.alloc(u64, n) catch unreachable;
        break :blk ptr;
    };
    const needs_free = n > 256;

    // LSD radix sort with ping-pong buffers: read from `src`, write to `dst`,
    // swap each pass. After 8 passes (even) the data is back in `buf`.
    var src: []u64 = buf;
    var dst: []u64 = temp;
    var counts: [256]usize = undefined;
    var shift: usize = 0;
    while (shift < 64) : (shift += 8) {
        const s: u6 = @intCast(shift);
        @memset(counts[0..], 0);
        for (src) |v| counts[(v >> s) & 0xFF] += 1;

        var total: usize = 0;
        for (&counts) |*c| {
            const old = c.*;
            c.* = total;
            total += old;
        }

        for (src) |v| {
            const digit = (v >> s) & 0xFF;
            dst[counts[digit]] = v;
            counts[digit] += 1;
        }

        const swap = src;
        src = dst;
        dst = swap;
    }

    if (needs_free) std.heap.c_allocator.free(temp);

    // Restore: flip sign bit back for positives, flip all bits for negatives
    for (buf) |*b| {
        const v = b.*;
        b.* = if (v & sign_bit != 0) v ^ sign_bit else ~v;
    }
}

export fn zigr_bench_sort(vec_sexp: SEXP) SEXP {
    const src = raw.real(vec_sexp);
    const n = src.len;

    const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(n)));
    defer R.Rf_unprotect(1);
    const rp = raw.realMut(result);
    @memcpy(rp, src);

    radixSortF64(rp);

    return result;
}
