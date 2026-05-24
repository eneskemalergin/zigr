const R = @import("R");

comptime {
    _ = @import("task_01_fib.zig");
    _ = @import("task_02_vectorsum.zig");
    _ = @import("task_03_transpose.zig");
    _ = @import("task_04_strings.zig");
    _ = @import("task_05_dataframe.zig");
    _ = @import("task_06_na_prop.zig");
    _ = @import("task_07_parallel.zig");
    _ = @import("task_09_protect.zig");
    _ = @import("task_10_blas_matmul.zig");
    _ = @import("task_11_crossprod.zig");
    _ = @import("task_12_cholesky.zig");
    _ = @import("task_13_lm.zig");
    _ = @import("task_14_rowsums.zig");
    _ = @import("task_15_elem_ops.zig");
    _ = @import("task_16_rowcol_means.zig");
    _ = @import("task_17_broadcast.zig");
    _ = @import("task_18_sort.zig");
    _ = @import("task_19_cumsum.zig");
    _ = @import("task_20_rnorm.zig");
    _ = @import("task_21_string_nchar.zig");
    _ = @import("task_22_which_na.zig");
    _ = @import("task_23_altrep_sum.zig");
    _ = @import("task_24_altrep_read.zig");
    _ = @import("task_25_altrep_create.zig");
    _ = @import("task_26_comptime_dispatch.zig");
    _ = @import("task_27_struct_convert.zig");
    _ = @import("task_28_na_prop_vary.zig");
    _ = @import("task_29_scale_law.zig");
    _ = @import("task_30_arena_vs_rmalloc.zig");
    _ = @import("task_31_prot_overhead.zig");
    _ = @import("task_32_longjmp_safety.zig");
    _ = @import("task_34_translate_c_cost.zig");
    _ = @import("task_35_string_variants.zig");
    _ = @import("task_36_parallel_scaling.zig");
    _ = @import("task_37_memory_bandwidth.zig");
    _ = @import("task_38_owned_altrep_real_sum.zig");
    _ = @import("task_39_owned_altrep_int_sum.zig");
    _ = @import("task_40_owned_altrep_logical_sum.zig");
    _ = @import("task_41_owned_altrep_int_min.zig");
    _ = @import("task_42_owned_altrep_int_max.zig");
    _ = @import("task_43_owned_altrep_int_argmin.zig");
    _ = @import("task_44_owned_altrep_int_argmax.zig");
    _ = @import("task_45_owned_altrep_logical_min.zig");
    _ = @import("task_46_owned_altrep_logical_max.zig");
    _ = @import("task_47_owned_altrep_logical_argmin.zig");
    _ = @import("task_48_owned_altrep_logical_argmax.zig");
}

export fn R_init_zigr_benchmarks(info: *R.DllInfo) void {
    _ = info;
}
