const R = @import("R");
const zigr = @import("zigr");

comptime {
    _ = @import("task_28_cholesky.zig");
}

extern fn zigr_bench_cholesky(R.SEXP) R.SEXP;

const CholeskyExports = zigr.@"export".generateExports(&.{
    .{ .name = "zigr_bench_cholesky", .func = zigr_bench_cholesky },
}, &.{});

export fn R_init_zigr_benchmarks_task28(info: *R.DllInfo) callconv(.c) void {
    CholeskyExports.init(info);
    _ = R.R_forceSymbols(info, 1);
}

export fn R_unload_zigr_benchmarks_task28(info: *R.DllInfo) callconv(.c) void {
    CholeskyExports.unload(info);
}
