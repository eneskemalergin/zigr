const std = @import("std");
const simd = @import("simd");
const R = @import("R");
const raw = @import("raw");

const SEXP = R.SEXP;

const NUM_THREADS: usize = 4;

fn sum_chunk(data: []const f64) f64 {
    var total: f64 = 0.0;
    var i: usize = 0;
    const lanes = simd.f64_lanes;
    if (data.len >= lanes) {
        var vec: @Vector(lanes, f64) = @splat(0.0);
        const end = data.len - (data.len % lanes);
        while (i < end) : (i += lanes) {
            vec += data[i..][0..lanes].*;
        }
        total += @reduce(.Add, vec);
    }
    while (i < data.len) : (i += 1) total += data[i];
    return total;
}

const ThreadContext = struct {
    chunk: []const f64,
    result: f64,
};

fn threadWorker(ctx: *ThreadContext) void {
    ctx.result = sum_chunk(ctx.chunk);
}

export fn zigr_bench_parallel(vec: SEXP) SEXP {
    const slice = raw.real(vec);
    const chunk_size = slice.len / NUM_THREADS;

    var contexts: [NUM_THREADS]ThreadContext = undefined;
    var threads: [NUM_THREADS]std.Thread = undefined;
    var thread_count: usize = 0;

    for (&contexts, &threads, 0..) |*ctx, *t, i| {
        const start = i * chunk_size;
        const end = if (i == NUM_THREADS - 1) slice.len else start + chunk_size;
        ctx.* = .{ .chunk = slice[start..end], .result = 0.0 };
        t.* = std.Thread.spawn(.{}, threadWorker, .{ctx}) catch {
            ctx.result = sum_chunk(ctx.chunk);
            continue;
        };
        thread_count = i + 1;
    }

    for (threads[0..thread_count]) |*t| t.join();

    var grand_total: f64 = 0.0;
    for (contexts) |ctx| grand_total += ctx.result;

    return R.Rf_ScalarReal(grand_total);
}
