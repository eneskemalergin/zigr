const R = @import("R");

comptime {
    _ = @import("task_12_cholesky.zig");
}

export fn R_init_zigr_benchmarks_task12(info: *R.DllInfo) void {
    _ = info;
}
