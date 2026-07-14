const std = @import("std");
const R = @import("R");
const zigr = @import("zigr");
const convert = zigr.convert;
const sexp = zigr.sexp;

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
    _ = @import("task_48_weakref_lifecycle.zig");
}

const task_49_owned_altrep_create = @import("task_49_owned_altrep_create.zig");

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
extern fn zigr_bench_weakref_lifecycle(R.SEXP) R.SEXP;
extern fn zigr_bench_owned_altrep_create(R.SEXP) R.SEXP;

const FixtureState = struct { value: i32 };
var fixture_state = FixtureState{ .value = 0 };

fn deinitFixtureState(_: *FixtureState) void {}

fn fixtureTag() R.SEXP {
    return zigr.externalptr.typeTag(FixtureState);
}

fn fixtureScalar(value: f64) f64 {
    return value;
}

fn fixtureIntScalar(value: i32) i32 {
    return value;
}

fn fixtureBoolScalar(value: bool) bool {
    return value;
}

fn fixtureScalarAfterAllocation(value: f64) f64 {
    _ = R.Rf_allocVector(R.INTSXP, 1);
    return value;
}

fn fixtureVector(values: []const f64) f64 {
    var total: f64 = 0;
    for (values) |value| total += value;
    return total;
}

fn fixtureNew() R.SEXP {
    return zigr.externalptr.createTyped(FixtureState, .{ .value = 0 }, deinitFixtureState);
}

fn generatedFixtureMethod(state: *FixtureState, amount: i32) i32 {
    state.value += amount;
    return state.value;
}

fn fixtureError(_: R.SEXP) void {
    R.Rf_error("zigr fixture error: expected failure");
}

fn fixtureExternal(value: f64) f64 {
    return value + 1.0;
}

fn fixtureZero() f64 {
    return 1.0;
}

fn fixtureOptional(value: ?f64) i32 {
    return if (value == null) 0 else 1;
}

fn fixtureOptionalInt(value: ?i32) i32 {
    return if (value == null) 0 else 1;
}

fn fixtureOptionalBool(value: ?bool) i32 {
    return if (value == null) 0 else 1;
}

fn fixtureIntVector(values: []const i32) f64 {
    var total: f64 = 0;
    for (values) |value| total += @floatFromInt(value);
    return total;
}

fn fixtureStringView(values: convert.StringSliceView) i32 {
    var total: i32 = 0;
    for (0..values.len) |i| {
        if (!values.at(i).is_na) total += 1;
    }
    return total;
}

fn fixtureRaw(values: []const u8) i32 {
    var total: i32 = 0;
    for (values) |value| total += @intCast(value);
    return total;
}

fn fixtureComplex(values: []const convert.Rcomplex) f64 {
    var total: f64 = 0;
    for (values) |value| total += value.r;
    return total;
}

// Four passes keep cache setup visible instead of hiding it in repetition.
const string_repeated_passes = 4;

fn fixtureStringViewOne(values: convert.StringSliceView) i32 {
    var total: i32 = 0;
    var iterator = values.iterator();
    while (iterator.next()) |value| {
        if (!value.is_na) total += @intCast(value.len);
    }
    return total;
}

fn fixtureStringCacheBuild(values: convert.CachedStringSliceView) i32 {
    var cached = values;
    defer cached.deinit();
    return @intCast(cached.len);
}

fn fixtureStringCacheOne(values: convert.CachedStringSliceView) i32 {
    var cached = values;
    defer cached.deinit();
    var total: i32 = 0;
    var iterator = cached.iterator();
    while (iterator.next()) |value| {
        if (!value.is_na) total += @intCast(value.len);
    }
    return total;
}

fn fixtureStringHeadersOne(values: []const []const u8) i32 {
    var total: i32 = 0;
    for (values) |value| total += @intCast(value.len);
    return total;
}

fn fixtureStringViewRepeated(values: convert.StringSliceView) i32 {
    var total: i32 = 0;
    for (0..string_repeated_passes) |_| {
        var iterator = values.iterator();
        while (iterator.next()) |value| {
            if (!value.is_na) total += @intCast(value.len);
        }
    }
    return total;
}

fn fixtureStringCacheRepeated(values: convert.CachedStringSliceView) i32 {
    var cached = values;
    defer cached.deinit();
    var total: i32 = 0;
    for (0..string_repeated_passes) |_| {
        var iterator = cached.iterator();
        while (iterator.next()) |value| {
            if (!value.is_na) total += @intCast(value.len);
        }
    }
    return total;
}

fn fixtureStringHeadersRepeated(values: []const []const u8) i32 {
    var total: i32 = 0;
    for (0..string_repeated_passes) |_| {
        for (values) |value| total += @intCast(value.len);
    }
    return total;
}

fn fixtureRawView(values: convert.RawSliceView) i32 {
    var view = values;
    defer view.deinit();
    var total: i32 = 0;
    for (view.constSlice()) |value| total += @intCast(value);
    return total;
}

fn fixtureRawCopy(values: []const u8) i32 {
    var total: i32 = 0;
    for (values) |value| total += @intCast(value);
    return total;
}

fn fixtureComplexView(values: []const convert.Rcomplex) f64 {
    var total: f64 = 0;
    for (values) |value| total += value.r + value.i;
    return total;
}

fn fixtureComplexReturn(values: []const convert.Rcomplex) []const convert.Rcomplex {
    return values;
}

const FixtureSchema = struct {
    id: i32,
    count: i32,
    ratio: f64,
    enabled: bool,
};

fn validateDirectSchema(value: R.SEXP) void {
    if (R.TYPEOF(value) != R.VECSXP or R.XLENGTH(value) != 4) {
        R.Rf_error("zigr fixture expected a four-field named list");
        unreachable;
    }
    if (R.R_getAttribCount(value) != 1 or !R.R_hasAttrib(value, R.R_NamesSymbol)) {
        R.Rf_error("zigr fixture expected only a names attribute");
        unreachable;
    }
    const names = R.Rf_getAttrib(value, R.R_NamesSymbol);
    if (R.TYPEOF(names) != R.STRSXP or R.XLENGTH(names) != 4 or R.R_getAttribCount(names) != 0) {
        R.Rf_error("zigr fixture expected four field names");
        unreachable;
    }
    const fields = [_][]const u8{ "id", "count", "ratio", "enabled" };
    for (fields, 0..) |field, i| {
        const name = R.STRING_ELT(names, @intCast(i));
        if (name == R.R_NaString or !std.mem.eql(u8, sexp.charsxpBytes(name), field)) {
            R.Rf_error("zigr fixture received an unexpected field name");
            unreachable;
        }
    }
    const id = R.VECTOR_ELT(value, 0);
    const count = R.VECTOR_ELT(value, 1);
    const ratio = R.VECTOR_ELT(value, 2);
    const enabled = R.VECTOR_ELT(value, 3);
    if (R.TYPEOF(id) != R.INTSXP or R.XLENGTH(id) != 1 or R.INTEGER(id)[0] == R.R_NaInt) {
        R.Rf_error("zigr fixture expected a non-missing integer id");
        unreachable;
    }
    if (R.TYPEOF(count) != R.INTSXP or R.XLENGTH(count) != 1 or R.INTEGER(count)[0] == R.R_NaInt) {
        R.Rf_error("zigr fixture expected a non-missing integer count");
        unreachable;
    }
    if (R.TYPEOF(ratio) != R.REALSXP or R.XLENGTH(ratio) != 1 or R.ISNA(R.REAL(ratio)[0]) != 0) {
        R.Rf_error("zigr fixture expected a non-missing real ratio");
        unreachable;
    }
    if (R.TYPEOF(enabled) != R.LGLSXP or R.XLENGTH(enabled) != 1 or R.LOGICAL(enabled)[0] == R.R_NaInt) {
        R.Rf_error("zigr fixture expected a non-missing logical enabled");
        unreachable;
    }
}

fn fixtureSchema(value: R.SEXP) R.SEXP {
    const schema = convert.fromSEXP(FixtureSchema, value, std.heap.page_allocator);
    _ = schema;
    return value;
}

fn fixtureWrongTag() R.SEXP {
    return R.R_MakeExternalPtr(&fixture_state, R.Rf_install("zigr_fixture_wrong_tag"), R.R_NilValue);
}

fn fixtureTaggedRaw() R.SEXP {
    return R.R_MakeExternalPtr(&fixture_state, fixtureTag(), R.R_NilValue);
}

fn fixtureCleared() R.SEXP {
    const result = zigr.externalptr.makeTyped(FixtureState, &fixture_state, R.R_NilValue);
    R.R_ClearExternalPtr(result);
    return result;
}

fn fixtureMisaligned() R.SEXP {
    const address = @intFromPtr(&fixture_state) + 1;
    const misaligned: *anyopaque = @ptrFromInt(address);
    return zigr.externalptr.makeTypedRaw(FixtureState, misaligned, R.R_NilValue);
}

fn directZero() R.SEXP {
    return R.Rf_ScalarReal(1.0);
}

fn directScalar(value: R.SEXP) R.SEXP {
    if (R.TYPEOF(value) != R.REALSXP or R.XLENGTH(value) != 1 or R.ISNA(R.REAL(value)[0]) != 0) {
        R.Rf_error("zigr fixture expected one non-missing REAL value");
        unreachable;
    }
    return R.Rf_ScalarReal(R.REAL(value)[0]);
}

fn directOptional(value: R.SEXP) R.SEXP {
    if (value == R.R_NilValue) return R.Rf_ScalarInteger(0);
    if (R.TYPEOF(value) != R.REALSXP or R.XLENGTH(value) != 1) {
        R.Rf_error("zigr fixture expected NULL or one REAL value");
        unreachable;
    }
    if (R.ISNA(R.REAL(value)[0]) != 0) return R.Rf_ScalarInteger(0);
    return R.Rf_ScalarInteger(1);
}

fn directNumeric(value: R.SEXP) R.SEXP {
    if (R.TYPEOF(value) != R.REALSXP) {
        R.Rf_error("zigr fixture expected REALSXP");
        unreachable;
    }
    var total: f64 = 0;
    const values = R.REAL(value);
    for (values[0..@as(usize, @intCast(R.XLENGTH(value)))]) |item| total += item;
    return R.Rf_ScalarReal(total);
}

fn directIntVector(value: R.SEXP) R.SEXP {
    if (R.TYPEOF(value) != R.INTSXP) {
        R.Rf_error("zigr fixture expected an integer ALTREP");
        unreachable;
    }
    var total: f64 = 0;
    const n = @as(usize, @intCast(R.XLENGTH(value)));
    if (R.DATAPTR_OR_NULL(value)) |raw| {
        const values: [*]const i32 = @ptrCast(@alignCast(raw));
        for (values[0..n]) |item| total += @floatFromInt(item);
    } else {
        var buffer: [4096]i32 = undefined;
        var offset: R.R_xlen_t = 0;
        const length = R.XLENGTH(value);
        while (offset < length) {
            const want = @min(length - offset, @as(R.R_xlen_t, @intCast(buffer.len)));
            const got = R.INTEGER_GET_REGION(value, offset, want, buffer[0..].ptr);
            if (got == 0) {
                R.Rf_error("zigr fixture could not read an integer ALTREP region");
                unreachable;
            }
            for (buffer[0..@as(usize, @intCast(got))]) |item| total += @floatFromInt(item);
            offset += got;
        }
    }
    return R.Rf_ScalarReal(total);
}

fn directString(value: R.SEXP) R.SEXP {
    if (R.TYPEOF(value) != R.STRSXP) {
        R.Rf_error("zigr fixture expected STRSXP");
        unreachable;
    }
    var total: i32 = 0;
    for (0..@as(usize, @intCast(R.XLENGTH(value)))) |i| {
        if (R.STRING_ELT(value, @intCast(i)) != R.R_NaString) total += 1;
    }
    return R.Rf_ScalarInteger(total);
}

fn directRaw(value: R.SEXP) R.SEXP {
    if (R.TYPEOF(value) != R.RAWSXP) {
        R.Rf_error("zigr fixture expected RAWSXP");
        unreachable;
    }
    var total: i32 = 0;
    for (0..@as(usize, @intCast(R.XLENGTH(value)))) |i| total += @intCast(R.RAW(value)[i]);
    return R.Rf_ScalarInteger(total);
}

fn directComplex(value: R.SEXP) R.SEXP {
    if (R.TYPEOF(value) != R.CPLXSXP) {
        R.Rf_error("zigr fixture expected CPLXSXP");
        unreachable;
    }
    const values = R.COMPLEX(value) orelse {
        R.Rf_error("zigr fixture has no complex data pointer");
        unreachable;
    };
    const values_ptr: [*]const convert.Rcomplex = @ptrCast(@alignCast(values));
    var total: f64 = 0;
    for (0..@as(usize, @intCast(R.XLENGTH(value)))) |i| total += values_ptr[i].r;
    return R.Rf_ScalarReal(total);
}

fn directSchema(value: R.SEXP) R.SEXP {
    validateDirectSchema(value);
    return value;
}

fn directMethodStatePtr(receiver: R.SEXP) *FixtureState {
    return zigr.externalptr.checkedPointer(FixtureState, receiver) catch |pointer_err| zigr.externalptr.signalPointerError(pointer_err);
}

fn directMethod(receiver: R.SEXP, amount: R.SEXP) R.SEXP {
    const state = directMethodStatePtr(receiver);
    if (R.TYPEOF(amount) != R.INTSXP or R.XLENGTH(amount) != 1 or R.INTEGER(amount)[0] == R.R_NaInt) {
        R.Rf_error("zigr fixture expected one non-missing integer");
        unreachable;
    }
    state.value += R.INTEGER(amount)[0];
    return R.Rf_ScalarInteger(state.value);
}

fn directExternal(args: R.SEXP) R.SEXP {
    if (args == R.R_NilValue) {
        R.Rf_error("zigr fixture expected one REAL value");
        unreachable;
    }
    const value = R.CAR(R.CDR(args));
    if (R.TYPEOF(value) != R.REALSXP or R.XLENGTH(value) != 1) {
        R.Rf_error("zigr fixture expected one REAL value");
        unreachable;
    }
    return R.Rf_ScalarReal(R.REAL(value)[0] + 1.0);
}

const FixtureExports = zigr.@"export".generateExports(&.{
    .{ .name = "zigr_fixture_scalar", .func = fixtureScalar },
    .{ .name = "zigr_fixture_int_scalar", .func = fixtureIntScalar },
    .{ .name = "zigr_fixture_bool_scalar", .func = fixtureBoolScalar },
    .{ .name = "zigr_fixture_scalar_after_allocation", .func = fixtureScalarAfterAllocation },
    .{ .name = "zigr_fixture_vector", .func = fixtureVector },
    .{ .name = "zigr_fixture_new", .func = fixtureNew },
    .{ .name = "zigr_fixture_error", .func = fixtureError },
    .{ .name = "zigr_fixture_zero", .func = fixtureZero },
    .{ .name = "zigr_fixture_optional", .func = fixtureOptional },
    .{ .name = "zigr_fixture_optional_int", .func = fixtureOptionalInt },
    .{ .name = "zigr_fixture_optional_bool", .func = fixtureOptionalBool },
    .{ .name = "zigr_fixture_int_vector", .func = fixtureIntVector },
    .{ .name = "zigr_fixture_string_view", .func = fixtureStringView },
    .{ .name = "zigr_fixture_raw", .func = fixtureRaw },
    .{ .name = "zigr_fixture_complex", .func = fixtureComplex },
    .{ .name = "zigr_string_view_one", .func = fixtureStringViewOne },
    .{ .name = "zigr_string_cache_build", .func = fixtureStringCacheBuild },
    .{ .name = "zigr_string_cache_one", .func = fixtureStringCacheOne },
    .{ .name = "zigr_string_headers_one", .func = fixtureStringHeadersOne },
    .{ .name = "zigr_string_view_repeated", .func = fixtureStringViewRepeated },
    .{ .name = "zigr_string_cache_repeated", .func = fixtureStringCacheRepeated },
    .{ .name = "zigr_string_headers_repeated", .func = fixtureStringHeadersRepeated },
    .{ .name = "zigr_raw_view", .func = fixtureRawView },
    .{ .name = "zigr_raw_copy", .func = fixtureRawCopy },
    .{ .name = "zigr_complex_view", .func = fixtureComplexView },
    .{ .name = "zigr_complex_return", .func = fixtureComplexReturn },
    .{ .name = "zigr_fixture_schema", .func = fixtureSchema },
    .{ .name = "zigr_fixture_wrong_tag", .func = fixtureWrongTag },
    .{ .name = "zigr_fixture_tagged_raw", .func = fixtureTaggedRaw },
    .{ .name = "zigr_fixture_cleared", .func = fixtureCleared },
    .{ .name = "zigr_fixture_misaligned", .func = fixtureMisaligned },
}, &.{
    .{ .name = "zigr_fixture_external", .func = fixtureExternal },
});

const FixtureMethods = zigr.@"export".generateMethods(FixtureState, &.{
    .{ .name = "increment", .func = generatedFixtureMethod },
}, &.{});

fn directMethodDef(comptime name: []const u8, comptime func: anytype) R.R_CallMethodDef {
    const func_info = @typeInfo(@TypeOf(func)).@"fn";
    return .{
        .name = @ptrCast(name.ptr),
        .fun = @ptrCast(&func),
        .numArgs = @intCast(func_info.params.len),
    };
}

fn directExternalMethodDef(comptime name: []const u8, comptime func: anytype) R.R_ExternalMethodDef {
    const func_info = @typeInfo(@TypeOf(func)).@"fn";
    return .{
        .name = @ptrCast(name.ptr),
        .fun = @ptrCast(&func),
        .numArgs = @intCast(func_info.params.len),
    };
}

const advanced_task_count = 46;
const direct_boundary_call_count = 10;
const generated_fixture_call_count = FixtureExports.call_defs.len - 1;
const generated_method_call_count = FixtureMethods.call_defs.len - 1;
var package_call_defs: [advanced_task_count + direct_boundary_call_count + generated_fixture_call_count + generated_method_call_count + 1]R.R_CallMethodDef = undefined;
var package_external_defs: [FixtureExports.ext_defs.len + 1]R.R_ExternalMethodDef = undefined;
var package_initialized = false;

fn initPackage(info: *R.DllInfo) callconv(.c) void {
    if (package_initialized) return;
    task_49_owned_altrep_create.register(info);
    package_initialized = true;

    FixtureExports.init(info);
    FixtureMethods.init(info);
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
    package_call_defs[44] = directMethodDef("zigr_bench_weakref_lifecycle", zigr_bench_weakref_lifecycle);
    package_call_defs[45] = directMethodDef("zigr_bench_owned_altrep_create", zigr_bench_owned_altrep_create);
    package_call_defs[46] = directMethodDef("zigr_direct_zero", directZero);
    package_call_defs[47] = directMethodDef("zigr_direct_scalar", directScalar);
    package_call_defs[48] = directMethodDef("zigr_direct_optional", directOptional);
    package_call_defs[49] = directMethodDef("zigr_direct_numeric", directNumeric);
    package_call_defs[50] = directMethodDef("zigr_direct_int_vector", directIntVector);
    package_call_defs[51] = directMethodDef("zigr_direct_string", directString);
    package_call_defs[52] = directMethodDef("zigr_direct_raw", directRaw);
    package_call_defs[53] = directMethodDef("zigr_direct_complex", directComplex);
    package_call_defs[54] = directMethodDef("zigr_direct_schema", directSchema);
    package_call_defs[55] = directMethodDef("zigr_direct_method", directMethod);
    inline for (0..generated_fixture_call_count) |i| {
        package_call_defs[advanced_task_count + direct_boundary_call_count + i] = FixtureExports.call_defs[i];
    }
    inline for (0..generated_method_call_count) |i| {
        package_call_defs[advanced_task_count + direct_boundary_call_count + generated_fixture_call_count + i] = FixtureMethods.call_defs[i];
    }
    package_call_defs[package_call_defs.len - 1] = .{ .name = null, .fun = null, .numArgs = 0 };
    inline for (0..FixtureExports.ext_defs.len - 1) |i| {
        package_external_defs[i] = FixtureExports.ext_defs[i];
    }
    package_external_defs[FixtureExports.ext_defs.len - 1] = directExternalMethodDef("zigr_direct_external", directExternal);
    package_external_defs[package_external_defs.len - 1] = .{ .name = null, .fun = null, .numArgs = 0 };

    _ = R.R_registerRoutines(
        info,
        null,
        @as([*c]const R.R_CallMethodDef, @ptrCast(&package_call_defs[0])),
        null,
        @as([*c]const R.R_ExternalMethodDef, @ptrCast(&package_external_defs[0])),
    );
    _ = R.R_useDynamicSymbols(info, 0);
    _ = R.R_forceSymbols(info, 1);
}

comptime {
    _ = FixtureExports;
    _ = FixtureMethods;
}

export fn R_init_zigr_benchmarks(info: *R.DllInfo) callconv(.c) void {
    initPackage(info);
}

export fn R_unload_zigr_benchmarks(info: *R.DllInfo) callconv(.c) void {
    FixtureExports.unload(info);
    FixtureMethods.unload(info);
}
