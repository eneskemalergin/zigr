const std = @import("std");
const R = @import("R");
const simd = @import("simd");
const raw = @import("raw");

const SEXP = R.SEXP;
const num_threads: usize = 4;
const scalar_threshold: usize = 4_096;
const parallel_threshold: usize = 1_000_000;

fn scalarSum(data: []const f64) f64 {
    var total: f64 = 0.0;
    for (data) |value| total += value;
    return total;
}

fn simdSum(data: []const f64) f64 {
    var total: f64 = 0.0;
    var i: usize = 0;
    const lanes = simd.f64_lanes;

    if (data.len >= lanes) {
        var vec_total: @Vector(lanes, f64) = @splat(0.0);
        const end = data.len - (data.len % lanes);
        while (i < end) : (i += lanes) {
            vec_total += data[i..][0..lanes].*;
        }
        total += @reduce(.Add, vec_total);
    }

    while (i < data.len) : (i += 1) total += data[i];
    return total;
}

const ThreadContext = struct {
    chunk: []const f64,
    result: f64,
};

fn threadWorker(ctx: *ThreadContext) void {
    ctx.result = simdSum(ctx.chunk);
}

fn parallelSum(data: []const f64) f64 {
    const chunk_size = data.len / num_threads;
    var contexts: [num_threads]ThreadContext = undefined;
    var threads: [num_threads]std.Thread = undefined;
    var thread_count: usize = 0;

    for (&contexts, &threads, 0..) |*ctx, *thread, i| {
        const start = i * chunk_size;
        const end = if (i == num_threads - 1) data.len else start + chunk_size;
        ctx.* = .{ .chunk = data[start..end], .result = 0.0 };
        thread.* = std.Thread.spawn(.{}, threadWorker, .{ctx}) catch {
            ctx.result = simdSum(ctx.chunk);
            continue;
        };
        thread_count = i + 1;
    }

    for (threads[0..thread_count]) |*thread| thread.join();

    var total: f64 = 0.0;
    for (contexts) |ctx| total += ctx.result;
    return total;
}

fn dispatchSum(data: []const f64) f64 {
    if (data.len < scalar_threshold) return scalarSum(data);
    if (data.len < parallel_threshold) return simdSum(data);
    return parallelSum(data);
}

export fn zigr_bench_scale_law(inputs_sexp: SEXP) SEXP {
    const n_inputs = @as(usize, @intCast(R.Rf_xlength(inputs_sexp)));
    const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(n_inputs)));
    defer R.Rf_unprotect(1);

    const out = R.REAL(result);
    for (0..n_inputs) |i| {
        out[i] = dispatchSum(raw.real(R.VECTOR_ELT(inputs_sexp, @as(isize, @intCast(i)))));
    }

    _ = R.Rf_setAttrib(result, R.R_NamesSymbol, R.Rf_getAttrib(inputs_sexp, R.R_NamesSymbol));
    return result;
}
