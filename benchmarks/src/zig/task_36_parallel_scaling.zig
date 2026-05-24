const std = @import("std");
const R = @import("R");
const raw = @import("raw");

const SEXP = R.SEXP;
const thread_counts = [_]usize{ 1, 2, 4, 8, 16 };

const ThreadContext = struct {
    chunk: []const f64,
    result: f64,
};

fn sumChunk(data: []const f64) f64 {
    var total: f64 = 0.0;
    for (data) |value| total += value;
    return total;
}

fn threadWorker(ctx: *ThreadContext) void {
    ctx.result = sumChunk(ctx.chunk);
}

fn parallelSum(data: []const f64, requested_threads: usize) f64 {
    if (data.len == 0) return 0.0;

    const actual_threads = @min(requested_threads, data.len);
    if (actual_threads <= 1) return sumChunk(data);

    var contexts: [16]ThreadContext = undefined;
    var threads: [16]std.Thread = undefined;
    var started: usize = 0;
    const chunk_size = data.len / actual_threads;

    for (0..actual_threads) |index| {
        const start = index * chunk_size;
        const end = if (index + 1 == actual_threads) data.len else (index + 1) * chunk_size;
        contexts[index] = .{ .chunk = data[start..end], .result = 0.0 };
        threads[started] = std.Thread.spawn(.{}, threadWorker, .{&contexts[index]}) catch {
            contexts[index].result = sumChunk(contexts[index].chunk);
            continue;
        };
        started += 1;
    }

    for (threads[0..started]) |*thread| thread.join();

    var total: f64 = 0.0;
    for (contexts[0..actual_threads]) |ctx| total += ctx.result;
    return total;
}

export fn zigr_bench_parallel_scaling(vec: SEXP) SEXP {
    const data = raw.real(vec);
    const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, thread_counts.len));
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, thread_counts.len));
    const out = R.REAL(result);

    inline for (thread_counts, 0..) |thread_count, index| {
        out[index] = parallelSum(data, thread_count);
    }

    R.SET_STRING_ELT(names, 0, R.Rf_mkChar("threads_1"));
    R.SET_STRING_ELT(names, 1, R.Rf_mkChar("threads_2"));
    R.SET_STRING_ELT(names, 2, R.Rf_mkChar("threads_4"));
    R.SET_STRING_ELT(names, 3, R.Rf_mkChar("threads_8"));
    R.SET_STRING_ELT(names, 4, R.Rf_mkChar("threads_16"));
    _ = R.Rf_setAttrib(result, R.R_NamesSymbol, names);
    R.Rf_unprotect(2);
    return result;
}
