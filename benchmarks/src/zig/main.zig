const R = @import("R");
const zigr = @import("zigr");

comptime {
    _ = @import("task_01_vectorsum.zig");
    _ = @import("task_02_elem_ops.zig");
    _ = @import("task_03_memcpy_bandwidth.zig");
    _ = @import("task_04_sort.zig");
    _ = @import("task_05_fib_recursive.zig");
    _ = @import("task_06_broadcast.zig");
    _ = @import("task_07a_protect_shallow.zig");
    _ = @import("task_07b_protect_scaling.zig");
    _ = @import("task_08_type_dispatch.zig");
    _ = @import("task_09_longjmp_safety.zig");
    _ = @import("task_10_sexp_create.zig");
    _ = @import("task_11_sexp_inspect.zig");
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
    _ = @import("task_25_l1_arithmetic.zig");
    _ = @import("task_26_matmul.zig");
    _ = @import("task_27_crossprod.zig");
    _ = @import("task_28_cholesky.zig");
    _ = @import("task_29_lm_fit.zig");
    _ = @import("task_30_altrep_create.zig");
    _ = @import("task_31_altrep_materialize.zig");
    _ = @import("task_32_altrep_elt_walk.zig");
    _ = @import("task_33_altrep_region_read.zig");
    _ = @import("task_34_altrep_sum_via_R.zig");
    _ = @import("task_35_altrep_sum_native.zig");
    _ = @import("task_36_altrep_min_max.zig");
    _ = @import("task_37_altrep_no_na_query.zig");
    _ = @import("task_38_struct_convert.zig");
    _ = @import("task_39_r_eval.zig");
    _ = @import("task_40_r_tryeval.zig");
    _ = @import("task_41_serialize_roundtrip.zig");
    _ = @import("task_42_external_ptr.zig");
    _ = @import("task_43_rng_stress.zig");
}

extern fn zigr_bench_vectorsum(R.SEXP) R.SEXP;
extern fn zigr_bench_elem_ops(R.SEXP) R.SEXP;
extern fn zigr_bench_memcpy_bandwidth(R.SEXP) R.SEXP;
extern fn zigr_bench_sort(R.SEXP) R.SEXP;
extern fn zigr_bench_fib_recursive(R.SEXP) R.SEXP;
extern fn zigr_bench_broadcast(R.SEXP, R.SEXP) R.SEXP;
extern fn zigr_bench_protect_shallow(R.SEXP) R.SEXP;
extern fn zigr_bench_protect_scaling(R.SEXP) R.SEXP;
extern fn zigr_bench_type_dispatch(R.SEXP) R.SEXP;
extern fn zigr_bench_longjmp_safety(R.SEXP) R.SEXP;
extern fn zigr_bench_sexp_create(R.SEXP) R.SEXP;
extern fn zigr_bench_sexp_inspect(R.SEXP) R.SEXP;
extern fn zigr_bench_matrix_transpose(R.SEXP) R.SEXP;
extern fn zigr_bench_matrix_rowsums(R.SEXP) R.SEXP;
extern fn zigr_bench_matrix_rowcol_means(R.SEXP) R.SEXP;
extern fn zigr_bench_dataframe_filter(R.SEXP) R.SEXP;
extern fn zigr_bench_list_access(R.SEXP) R.SEXP;
extern fn zigr_bench_string_concat(R.SEXP) R.SEXP;
extern fn zigr_bench_string_nchar(R.SEXP) R.SEXP;
extern fn zigr_bench_string_encoding(R.SEXP) R.SEXP;
extern fn zigr_bench_factor_ops(R.SEXP) R.SEXP;
extern fn zigr_bench_attrib_ops(R.SEXP) R.SEXP;
extern fn zigr_bench_s4_slot_access(R.SEXP) R.SEXP;
extern fn zigr_bench_na_prop(R.SEXP) R.SEXP;
extern fn zigr_bench_long_vector_idx(R.SEXP) R.SEXP;
extern fn zigr_bench_l1_arithmetic(R.SEXP) R.SEXP;
extern fn zigr_bench_blas_matmul(R.SEXP, R.SEXP) R.SEXP;
extern fn zigr_bench_crossprod(R.SEXP) R.SEXP;
extern fn zigr_bench_cholesky(R.SEXP) R.SEXP;
extern fn zigr_bench_lm(R.SEXP, R.SEXP) R.SEXP;
extern fn zigr_bench_altrep_create(R.SEXP) R.SEXP;
extern fn zigr_bench_altrep_materialize(R.SEXP) R.SEXP;
extern fn zigr_bench_altrep_elt_walk(R.SEXP) R.SEXP;
extern fn zigr_bench_altrep_region_read(R.SEXP) R.SEXP;
extern fn zigr_bench_altrep_sum_via_R(R.SEXP) R.SEXP;
extern fn zigr_bench_altrep_sum_native(R.SEXP) R.SEXP;
extern fn zigr_bench_altrep_min_max(R.SEXP) R.SEXP;
extern fn zigr_bench_altrep_no_na_query(R.SEXP) R.SEXP;
extern fn zigr_bench_struct_convert(R.SEXP) R.SEXP;
extern fn zigr_bench_r_eval(R.SEXP) R.SEXP;
extern fn zigr_bench_r_tryeval(R.SEXP) R.SEXP;
extern fn zigr_bench_serialize_roundtrip(R.SEXP) R.SEXP;
extern fn zigr_bench_external_ptr(R.SEXP) R.SEXP;
extern fn zigr_bench_rng_stress(R.SEXP) R.SEXP;

const FixtureState = struct { value: i32 };
var fixture_state = FixtureState{ .value = 0 };

fn fixtureScalar(value: f64) f64 {
    return value;
}

fn fixtureVector(values: []const f64) f64 {
    var total: f64 = 0;
    for (values) |value| total += value;
    return total;
}

fn fixtureNew() R.SEXP {
    fixture_state.value = 0;
    return R.R_MakeExternalPtr(&fixture_state, R.R_NilValue, R.R_NilValue);
}

fn fixtureMethod(receiver: R.SEXP, amount: i32) i32 {
    if (R.TYPEOF(receiver) != R.EXTPTRSXP) {
        R.Rf_error("zigr fixture method expected an external pointer");
        unreachable;
    }
    const raw = R.R_ExternalPtrAddr(receiver) orelse {
        R.Rf_error("zigr fixture method received a cleared external pointer");
        unreachable;
    };
    const state: *FixtureState = @ptrCast(@alignCast(raw));
    state.value += amount;
    return state.value;
}

fn fixtureError(_: R.SEXP) void {
    R.Rf_error("zigr fixture error: expected failure");
}

fn fixtureExternal(value: f64) f64 {
    return value + 1.0;
}

const FixtureExports = zigr.@"export".generateExports(&.{
    .{ .name = "zigr_fixture_scalar", .func = fixtureScalar },
    .{ .name = "zigr_fixture_vector", .func = fixtureVector },
    .{ .name = "zigr_fixture_new", .func = fixtureNew },
    .{ .name = "zigr_fixture_method", .func = fixtureMethod },
    .{ .name = "zigr_fixture_error", .func = fixtureError },
}, &.{
    .{ .name = "zigr_fixture_external", .func = fixtureExternal },
});

fn directMethodDef(comptime name: []const u8, comptime func: anytype) R.R_CallMethodDef {
    const func_info = @typeInfo(@TypeOf(func)).@"fn";
    return .{
        .name = @ptrCast(name.ptr),
        .fun = @ptrCast(&func),
        .numArgs = @intCast(func_info.params.len),
    };
}

var package_call_defs: [44 + FixtureExports.call_defs.len]R.R_CallMethodDef = undefined;
var package_initialized = false;

fn initPackage(info: *R.DllInfo) callconv(.c) void {
    if (package_initialized) return;
    package_initialized = true;

    FixtureExports.init(info);
    package_call_defs[0] = directMethodDef("zigr_bench_vectorsum", zigr_bench_vectorsum);
    package_call_defs[1] = directMethodDef("zigr_bench_elem_ops", zigr_bench_elem_ops);
    package_call_defs[2] = directMethodDef("zigr_bench_memcpy_bandwidth", zigr_bench_memcpy_bandwidth);
    package_call_defs[3] = directMethodDef("zigr_bench_sort", zigr_bench_sort);
    package_call_defs[4] = directMethodDef("zigr_bench_fib_recursive", zigr_bench_fib_recursive);
    package_call_defs[5] = directMethodDef("zigr_bench_broadcast", zigr_bench_broadcast);
    package_call_defs[6] = directMethodDef("zigr_bench_protect_shallow", zigr_bench_protect_shallow);
    package_call_defs[7] = directMethodDef("zigr_bench_protect_scaling", zigr_bench_protect_scaling);
    package_call_defs[8] = directMethodDef("zigr_bench_type_dispatch", zigr_bench_type_dispatch);
    package_call_defs[9] = directMethodDef("zigr_bench_longjmp_safety", zigr_bench_longjmp_safety);
    package_call_defs[10] = directMethodDef("zigr_bench_sexp_create", zigr_bench_sexp_create);
    package_call_defs[11] = directMethodDef("zigr_bench_sexp_inspect", zigr_bench_sexp_inspect);
    package_call_defs[12] = directMethodDef("zigr_bench_matrix_transpose", zigr_bench_matrix_transpose);
    package_call_defs[13] = directMethodDef("zigr_bench_matrix_rowsums", zigr_bench_matrix_rowsums);
    package_call_defs[14] = directMethodDef("zigr_bench_matrix_rowcol_means", zigr_bench_matrix_rowcol_means);
    package_call_defs[15] = directMethodDef("zigr_bench_dataframe_filter", zigr_bench_dataframe_filter);
    package_call_defs[16] = directMethodDef("zigr_bench_list_access", zigr_bench_list_access);
    package_call_defs[17] = directMethodDef("zigr_bench_string_concat", zigr_bench_string_concat);
    package_call_defs[18] = directMethodDef("zigr_bench_string_nchar", zigr_bench_string_nchar);
    package_call_defs[19] = directMethodDef("zigr_bench_string_encoding", zigr_bench_string_encoding);
    package_call_defs[20] = directMethodDef("zigr_bench_factor_ops", zigr_bench_factor_ops);
    package_call_defs[21] = directMethodDef("zigr_bench_attrib_ops", zigr_bench_attrib_ops);
    package_call_defs[22] = directMethodDef("zigr_bench_s4_slot_access", zigr_bench_s4_slot_access);
    package_call_defs[23] = directMethodDef("zigr_bench_na_prop", zigr_bench_na_prop);
    package_call_defs[24] = directMethodDef("zigr_bench_long_vector_idx", zigr_bench_long_vector_idx);
    package_call_defs[25] = directMethodDef("zigr_bench_l1_arithmetic", zigr_bench_l1_arithmetic);
    package_call_defs[26] = directMethodDef("zigr_bench_blas_matmul", zigr_bench_blas_matmul);
    package_call_defs[27] = directMethodDef("zigr_bench_crossprod", zigr_bench_crossprod);
    package_call_defs[28] = directMethodDef("zigr_bench_cholesky", zigr_bench_cholesky);
    package_call_defs[29] = directMethodDef("zigr_bench_lm", zigr_bench_lm);
    package_call_defs[30] = directMethodDef("zigr_bench_altrep_create", zigr_bench_altrep_create);
    package_call_defs[31] = directMethodDef("zigr_bench_altrep_materialize", zigr_bench_altrep_materialize);
    package_call_defs[32] = directMethodDef("zigr_bench_altrep_elt_walk", zigr_bench_altrep_elt_walk);
    package_call_defs[33] = directMethodDef("zigr_bench_altrep_region_read", zigr_bench_altrep_region_read);
    package_call_defs[34] = directMethodDef("zigr_bench_altrep_sum_via_R", zigr_bench_altrep_sum_via_R);
    package_call_defs[35] = directMethodDef("zigr_bench_altrep_sum_native", zigr_bench_altrep_sum_native);
    package_call_defs[36] = directMethodDef("zigr_bench_altrep_min_max", zigr_bench_altrep_min_max);
    package_call_defs[37] = directMethodDef("zigr_bench_altrep_no_na_query", zigr_bench_altrep_no_na_query);
    package_call_defs[38] = directMethodDef("zigr_bench_struct_convert", zigr_bench_struct_convert);
    package_call_defs[39] = directMethodDef("zigr_bench_r_eval", zigr_bench_r_eval);
    package_call_defs[40] = directMethodDef("zigr_bench_r_tryeval", zigr_bench_r_tryeval);
    package_call_defs[41] = directMethodDef("zigr_bench_serialize_roundtrip", zigr_bench_serialize_roundtrip);
    package_call_defs[42] = directMethodDef("zigr_bench_external_ptr", zigr_bench_external_ptr);
    package_call_defs[43] = directMethodDef("zigr_bench_rng_stress", zigr_bench_rng_stress);
    inline for (0..FixtureExports.call_defs.len) |i| {
        package_call_defs[44 + i] = FixtureExports.call_defs[i];
    }
    package_call_defs[package_call_defs.len - 1] = .{ .name = null, .fun = null, .numArgs = 0 };

    _ = R.R_registerRoutines(
        info,
        null,
        @as([*c]const R.R_CallMethodDef, @ptrCast(&package_call_defs[0])),
        null,
        @as([*c]const R.R_ExternalMethodDef, @ptrCast(&FixtureExports.ext_defs[0])),
    );
    _ = R.R_useDynamicSymbols(info, 0);
    _ = R.R_forceSymbols(info, 1);
}

comptime {
    _ = FixtureExports;
}

export fn R_init_zigr_benchmarks(info: *R.DllInfo) callconv(.c) void {
    initPackage(info);
}

export fn R_unload_zigr_benchmarks(info: *R.DllInfo) callconv(.c) void {
    FixtureExports.unload(info);
}
