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

    // Allocate temp once, reuse across all 8 passes
    var stack_temp: [256]u64 = undefined;
    const temp_alloc = if (n <= 256) blk: {
        break :blk stack_temp[0..n];
    } else blk: {
        const ptr = @as([*]u64, @ptrCast(@alignCast(R.R_chk_calloc(n, @sizeOf(u64)) orelse unreachable)));
        break :blk ptr[0..n];
    };
    const needs_free = n > 256;

    // LSD radix sort, 8 bits per pass, 8 passes for 64-bit
    var counts: [256]usize = undefined;
    var shift: usize = 0;
    while (shift < 64) : (shift += 8) {
        const s: u6 = @intCast(shift);
        @memset(counts[0..], 0);
        for (buf) |v| counts[(v >> s) & 0xFF] += 1;

        var total: usize = 0;
        for (&counts) |*c| {
            const old = c.*;
            c.* = total;
            total += old;
        }

        for (buf) |v| {
            const digit = (v >> s) & 0xFF;
            temp_alloc[counts[digit]] = v;
            counts[digit] += 1;
        }

        @memcpy(buf, temp_alloc);
    }

    if (needs_free) R.R_chk_free(@ptrCast(temp_alloc.ptr));

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
