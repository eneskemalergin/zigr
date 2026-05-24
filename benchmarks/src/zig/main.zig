const R = @import("R");

comptime {
    // Layer 1: Primitives (1-6, 25)
    _ = @import("task_01_vectorsum.zig");
    _ = @import("task_02_elem_ops.zig");
    _ = @import("task_03_memcpy_bandwidth.zig");
    _ = @import("task_04_sort.zig");
    _ = @import("task_05_fib_recursive.zig");
    _ = @import("task_06_broadcast.zig");
    _ = @import("task_25_l1_arithmetic.zig");

    // Layer 2: R API overhead (7a, 7b, 8-11)
    _ = @import("task_07a_protect_shallow.zig");
    _ = @import("task_07b_protect_scaling.zig");
    _ = @import("task_08_type_dispatch.zig");
    _ = @import("task_09_longjmp_safety.zig");
    _ = @import("task_10_sexp_create.zig");
    _ = @import("task_11_sexp_inspect.zig");

    // Layer 3: Data structures (12-24)
    _ = @import("task_12_matrix_transpose.zig");
    _ = @import("task_13_matrix_rowsums.zig");
    _ = @import("task_14_matrix_rowcol_means.zig");
    _ = @import("task_15_dataframe_filter.zig");
    _ = @import("task_16_list_access.zig");
    _ = @import("task_17_string_concat.zig");
    _ = @import("task_18_string_nchar.zig");
    _ = @import("task_19_string_encoding.zig");
    _ = @import("task_20_factor_ops.zig");
    _ = @import("task_21_attrib_ops.zig");
    _ = @import("task_22_s4_slot_access.zig");
    _ = @import("task_23_na_propagation.zig");
    _ = @import("task_24_long_vector_idx.zig");

    // Layer 4: Numerical (26-29)
    _ = @import("task_26_matmul.zig");
    _ = @import("task_27_crossprod.zig");
    _ = @import("task_28_cholesky.zig");
    _ = @import("task_29_lm_fit.zig");

    // Layer 5: ALTREP (30-37)
    _ = @import("task_30_altrep_create.zig");
    _ = @import("task_31_altrep_materialize.zig");
    _ = @import("task_32_altrep_elt_walk.zig");
    _ = @import("task_33_altrep_region_read.zig");
    _ = @import("task_34_altrep_sum_via_R.zig");
    _ = @import("task_35_altrep_sum_native.zig");
    _ = @import("task_36_altrep_min_max.zig");
    _ = @import("task_37_altrep_no_na_query.zig");

    // Layer 6: Integration (38-43)
    _ = @import("task_38_struct_convert.zig");
    _ = @import("task_39_r_eval.zig");
    _ = @import("task_40_r_tryeval.zig");
    _ = @import("task_41_serialize_roundtrip.zig");
    _ = @import("task_42_external_ptr.zig");
    _ = @import("task_43_rng_stress.zig");
}

export fn R_init_zigr_benchmarks(info: *R.DllInfo) void {
    _ = info;
}
