//! Normalized package fixture for the product capability matrix.
//!
//! Every supported cell uses zigr's public conversion, export, method, and
//! ownership APIs. R owns returned SEXPs and typed state after construction.

const R = @import("R");
const zigr = @import("zigr");
const std = @import("std");

const convert = zigr.convert;
const raw = zigr.raw;

fn callR(code: [:0]const u8, args: []const R.SEXP) R.SEXP {
    var text = zigr.protect.scoped(R.Rf_mkString(code.ptr));
    defer text.deinit();
    const parse_fun = zigr.eval.findFunctionIn("parse", R.R_BaseEnv);
    const parse_args = [_]zigr.lang.Argument{.{ .name = "text", .value = text.get() }};
    var parsed = zigr.protect.scoped(zigr.eval.callTaggedIn(parse_fun, &parse_args, R.R_BaseEnv) catch
        zigr.@"error".signal("benchmark R expression construction failed"));
    defer parsed.deinit();
    var function = zigr.protect.scoped(R.Rf_eval(zigr.sexp.checked.vectorElt(parsed.get(), 0), R.R_BaseEnv));
    defer function.deinit();
    return zigr.eval.callFunctionIn(function.get(), args, R.R_BaseEnv) catch
        zigr.@"error".signal("benchmark R function call failed");
}

const FixtureSchema = struct {
    id: i32,
    count: i32,
    ratio: f64,
    enabled: bool,
};

const FixtureState = struct {
    value: i32,
};

const NestedOutput = struct {
    value: i32,
};

const FixtureLifecycleCounts = struct {
    constructor: i32 = 0,
    method: i32 = 0,
    error_count: i32 = 0,
    finalizer: i32 = 0,
};

var lifecycle_counts = FixtureLifecycleCounts{};

fn fixtureZero() i32 {
    return 1;
}

fn fixtureScalar(value: f64) f64 {
    return value;
}

fn fixtureNumeric(value: R.SEXP) R.SEXP {
    const input = zigr.rvector.RVector(f64).init(value) catch |err| convert.signalError(err);
    return input.mulScalar(2.0);
}

fn fixtureAltrepInteger(value: []const i32) f64 {
    var total: f64 = 0.0;
    for (value) |element| {
        if (element == R.R_NaInt) return R.R_NaReal;
        total += @floatFromInt(element);
    }
    return total;
}

fn fixtureStrings(value: convert.StringMissingnessView) i32 {
    var count: i32 = 0;
    var iterator = value.iterator();
    while (iterator.next()) |element| {
        if (!element.is_na) count += 1;
    }
    return count;
}

fn fixtureRaw(value: []const u8) []const u8 {
    return value;
}

fn fixtureComplex(value: []const convert.Rcomplex) []const convert.Rcomplex {
    return value;
}

fn fixtureSchema(value: R.SEXP) R.SEXP {
    var arena = zigr.memory.UnwindArena.init();
    defer arena.deinit();
    _ = convert.fromSEXP(FixtureSchema, value, arena.allocator());
    return value;
}

fn fixtureStateDeinit(_: *FixtureState) void {
    lifecycle_counts.finalizer += 1;
}

fn fixtureNew() R.SEXP {
    lifecycle_counts.constructor += 1;
    return zigr.externalptr.createTyped(FixtureState, .{ .value = 0 }, fixtureStateDeinit);
}

fn fixtureIncrement(state: *FixtureState, amount: i32) i32 {
    lifecycle_counts.method += 1;
    state.value += amount;
    return state.value;
}

fn fixtureRead(state: *FixtureState) i32 {
    lifecycle_counts.method += 1;
    return state.value;
}

fn fixtureError(_: f64) void {
    lifecycle_counts.error_count += 1;
    zigr.@"error".signal("fixture error");
}

fn fixtureLifecycleReset() void {
    lifecycle_counts = .{};
}

fn fixtureLifecycleCounts() R.SEXP {
    const values = [_]i32{
        lifecycle_counts.constructor,
        lifecycle_counts.method,
        lifecycle_counts.error_count,
        lifecycle_counts.finalizer,
    };
    var result = zigr.protect.scoped(convert.fromIntSlice(&values));
    defer result.deinit();
    zigr.attrib.setNames(result.get(), &.{ "constructor", "method", "error", "finalizer" });
    return result.get();
}

fn fixtureOutputs() R.SEXP {
    const numeric_values = [_]f64{ 1.5, R.R_NaReal };
    const string_values = [_][]const u8{"fixture"};
    const raw_values = [_]u8{ 1, 2, 3 };
    const complex_values = [_]convert.Rcomplex{
        .{ .r = 1.0, .i = 2.0 },
        .{ .r = R.R_NaReal, .i = R.R_NaReal },
    };
    const logical_values = [_]i32{ 0, 1, R.R_NaInt };

    var numeric = zigr.protect.scoped(convert.fromRealSlice(&numeric_values));
    defer numeric.deinit();
    var string = zigr.protect.scoped(convert.fromStringSlice(&string_values));
    defer string.deinit();
    var raw_output = zigr.protect.scoped(convert.fromRawSlice(&raw_values));
    defer raw_output.deinit();
    var complex = zigr.protect.scoped(convert.fromComplexSlice(&complex_values));
    defer complex.deinit();
    var logical = zigr.protect.scoped(convert.fromLogicalSlice(&logical_values));
    defer logical.deinit();
    var nested = zigr.protect.scoped(convert.asSEXP(NestedOutput{ .value = 7 }));
    defer nested.deinit();

    const values = [_]R.SEXP{
        numeric.get(),
        string.get(),
        raw_output.get(),
        complex.get(),
        logical.get(),
        nested.get(),
    };
    var result = zigr.protect.scoped(convert.fromListSlice(&values));
    defer result.deinit();
    zigr.attrib.setNames(result.get(), &.{ "numeric", "string", "raw", "complex", "logical", "list" });
    return result.get();
}

fn benchVectorSum(value: []const f64) f64 {
    var total: f64 = 0.0;
    for (value) |element| total += element;
    return total;
}

fn benchNumericTransform(value: R.SEXP) R.SEXP {
    return fixtureNumeric(value);
}

fn benchBroadcast(value: []const f64, scalar: f64) f64 {
    var total: f64 = 0.0;
    var correction: f64 = 0.0;
    for (value) |element| {
        const adjusted = element + scalar - correction;
        const next = total + adjusted;
        correction = (next - total) - adjusted;
        total = next;
    }
    return total;
}

fn benchSort(value: []const f64) R.SEXP {
    var arena = zigr.memory.UnwindArena.init();
    defer arena.deinit();
    const result = arena.allocator().dupe(f64, value) catch zigr.@"error".signal("sort result allocation failed");
    std.mem.sort(f64, result, {}, std.sort.asc(f64));
    return convert.fromRealSlice(result);
}

fn benchMissingMean(value: []const f64) f64 {
    var total: f64 = 0.0;
    var count: usize = 0;
    for (value) |element| if (!R.ISNAN(element)) {
        total += element;
        count += 1;
    };
    return total / @as(f64, @floatFromInt(count));
}

fn benchTranspose(value: R.SEXP) R.SEXP {
    const rows: usize = @intCast(R.Rf_nrows(value));
    const columns: usize = @intCast(R.Rf_ncols(value));
    const source = raw.real(value) catch |raw_error| zigr.@"error".signal(@errorName(raw_error));
    var result = zigr.protect.scoped(R.Rf_allocMatrix(R.REALSXP, @intCast(columns), @intCast(rows)));
    defer result.deinit();
    const output = raw.realMut(result.get()) catch |raw_error| zigr.@"error".signal(@errorName(raw_error));
    for (0..columns) |column| {
        for (0..rows) |row| output[column + row * columns] = source[row + column * rows];
    }
    return result.get();
}

fn benchRowcol(value: R.SEXP) R.SEXP {
    const rows: usize = @intCast(R.Rf_nrows(value));
    const columns: usize = @intCast(R.Rf_ncols(value));
    const source = raw.real(value) catch |raw_error| zigr.@"error".signal(@errorName(raw_error));
    var row_means = zigr.protect.scoped(R.Rf_allocVector(R.REALSXP, @intCast(rows)));
    defer row_means.deinit();
    var column_sums = zigr.protect.scoped(R.Rf_allocVector(R.REALSXP, @intCast(columns)));
    defer column_sums.deinit();
    const row_output = raw.realMut(row_means.get()) catch |raw_error| zigr.@"error".signal(@errorName(raw_error));
    const column_output = raw.realMut(column_sums.get()) catch |raw_error| zigr.@"error".signal(@errorName(raw_error));
    @memset(row_output, 0.0);
    @memset(column_output, 0.0);
    for (0..columns) |column| for (0..rows) |row| {
        const element = source[row + column * rows];
        row_output[row] += element;
        column_output[column] += element;
    };
    for (row_output) |*element| element.* /= @floatFromInt(columns);
    const values = [_]R.SEXP{ row_means.get(), column_sums.get() };
    var result = zigr.protect.scoped(convert.fromListSlice(&values));
    defer result.deinit();
    zigr.attrib.setNames(result.get(), &.{ "row_means", "column_sums" });
    return result.get();
}

const dgemm_ = @extern(*const fn ([*c]const u8, [*c]const u8, [*c]const c_int, [*c]const c_int, [*c]const c_int, [*c]const f64, [*c]const f64, [*c]const c_int, [*c]const f64, [*c]const c_int, [*c]const f64, [*c]f64, [*c]const c_int, c_int, c_int) callconv(.c) void, .{ .name = "dgemm_" });

fn benchMatmul(x: R.SEXP, y: R.SEXP) R.SEXP {
    const rows = R.Rf_nrows(x);
    const columns = R.Rf_ncols(y);
    const inner = R.Rf_ncols(x);
    var result = zigr.protect.scoped(R.Rf_allocMatrix(R.REALSXP, rows, columns));
    defer result.deinit();
    const alpha: f64 = 1.0;
    const beta: f64 = 0.0;
    const notrans: u8 = 'N';
    const x_data = raw.real(x) catch |raw_error| zigr.@"error".signal(@errorName(raw_error));
    const y_data = raw.real(y) catch |raw_error| zigr.@"error".signal(@errorName(raw_error));
    const result_data = raw.realMut(result.get()) catch |raw_error| zigr.@"error".signal(@errorName(raw_error));
    dgemm_(&notrans, &notrans, &rows, &columns, &inner, &alpha, x_data.ptr, &rows, y_data.ptr, &inner, &beta, result_data.ptr, &rows, 1, 1);
    return result.get();
}

fn benchDataframe(value: R.SEXP) R.SEXP {
    const frame = zigr.dataframe.DataFrame.wrap(value) orelse zigr.@"error".signal("data-frame input expected");
    const x = raw.real(frame.column("x") orelse zigr.@"error".signal("missing x")) catch |raw_error| zigr.@"error".signal(@errorName(raw_error));
    const y = raw.real(frame.column("y") orelse zigr.@"error".signal("missing y")) catch |raw_error| zigr.@"error".signal(@errorName(raw_error));
    const groups = raw.int(frame.column("grp") orelse zigr.@"error".signal("missing grp")) catch |raw_error| zigr.@"error".signal(@errorName(raw_error));
    var sums = [_]f64{0.0} ** 10;
    for (x, y, groups) |x_value, y_value, group| {
        if (!R.ISNAN(x_value) and x_value > 0.0) sums[@intCast(group - 1)] += x_value / y_value;
    }
    const group_values = [_]i32{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    var group_output = zigr.protect.scoped(convert.fromIntSlice(&group_values));
    defer group_output.deinit();
    var sum_output = zigr.protect.scoped(convert.fromRealSlice(&sums));
    defer sum_output.deinit();
    const columns = [_]R.SEXP{ group_output.get(), sum_output.get() };
    return zigr.dataframe.build(&.{ "grp", "z_sum" }, &columns);
}

fn benchListSum(value: R.SEXP) f64 {
    var arena = zigr.memory.UnwindArena.init();
    defer arena.deinit();
    const items = convert.toListSlice(arena.allocator(), value) catch zigr.@"error".signal("list input expected");
    var total: f64 = 0.0;
    for (items) |item| {
        const values = raw.real(item) catch |raw_error| zigr.@"error".signal(@errorName(raw_error));
        for (values) |element| total += element;
    }
    return total;
}
fn benchStringConcat(value: R.SEXP) R.SEXP {
    var separator = zigr.protect.scoped(R.Rf_mkString(", "));
    defer separator.deinit();
    const function = zigr.eval.findFunctionIn("paste", R.R_BaseEnv);
    const arguments = [_]zigr.lang.Argument{
        .{ .value = value },
        .{ .name = "collapse", .value = separator.get() },
    };
    return zigr.eval.callTaggedIn(function, &arguments, R.R_BaseEnv) catch
        zigr.@"error".signal("string concatenation call failed");
}
fn benchStringMetadata(value: convert.StringMetadataView) R.SEXP {
    var counts = [_]i32{ 0, 0, 0, 0, 0 };
    var iterator = value.iterator();
    while (iterator.next()) |element| {
        if (element.is_na) {
            counts[4] += 1;
            continue;
        }
        counts[0] += @intCast(R.XLENGTH(element.charsxp));
        if (element.encoding_mark == R.CE_UTF8) counts[1] += 1 else if (element.encoding_mark == R.CE_LATIN1) counts[2] += 1 else if (element.encoding_mark == R.CE_BYTES) counts[3] += 1;
    }
    var result = zigr.protect.scoped(convert.fromIntSlice(&counts));
    defer result.deinit();
    zigr.attrib.setNames(result.get(), &.{ "bytes", "utf8", "latin1", "bytes_marked", "missing" });
    return result.get();
}
fn benchFactor(value: R.SEXP) R.SEXP {
    var labels: [100][10]u8 = undefined;
    var level_slices: [100][]const u8 = undefined;
    for (0..100) |index| {
        level_slices[index] = std.fmt.bufPrint(&labels[index], "level_{d:0>3}", .{index + 1}) catch
            zigr.@"error".signal("factor level construction failed");
    }
    var levels = zigr.protect.scoped(convert.fromStringSlice(&level_slices));
    defer levels.deinit();
    const function = zigr.eval.findFunctionIn("factor", R.R_BaseEnv);
    const arguments = [_]zigr.lang.Argument{
        .{ .value = value },
        .{ .name = "levels", .value = levels.get() },
    };
    return zigr.eval.callTaggedIn(function, &arguments, R.R_BaseEnv) catch
        zigr.@"error".signal("factor construction failed");
}
fn benchAttributes(value: R.SEXP) R.SEXP {
    var result = zigr.protect.scoped(R.Rf_duplicate(value));
    defer result.deinit();
    zigr.attrib.setClass(result.get(), "bench_class");
    var creator = zigr.protect.scoped(R.Rf_mkString("zigr_bench"));
    defer creator.deinit();
    const creator_symbol = zigr.lang.symbol("creator");
    zigr.attrib.setAttrib(result.get(), creator_symbol, creator.get());
    _ = R.Rf_getAttrib(result.get(), R.R_ClassSymbol);
    _ = zigr.attrib.getAttrib(result.get(), creator_symbol);
    return result.get();
}
fn benchS4(value: R.SEXP) R.SEXP {
    var class = zigr.protect.scoped(R.Rf_mkString("BenchS4"));
    defer class.deinit();
    const new_function = zigr.eval.findFunction("new");
    const new_arguments = [_]zigr.lang.Argument{
        .{ .value = class.get() },
        .{ .name = "slot_x", .value = value },
    };
    var object = zigr.protect.scoped(zigr.eval.callTaggedIn(new_function, &new_arguments, R.R_GlobalEnv) catch
        zigr.@"error".signal("S4 construction failed"));
    defer object.deinit();
    var slot_name = zigr.protect.scoped(R.Rf_mkString("slot_x"));
    defer slot_name.deinit();
    return zigr.eval.callIn("slot", &.{ object.get(), slot_name.get() }, R.R_GlobalEnv);
}

fn benchLogicalCounts(value: convert.LogicalSliceView) R.SEXP {
    var counts = [_]i32{ 0, 0, 0 };
    for (value.constSlice()) |element| {
        if (element == R.R_NaInt) counts[2] += 1 else if (element == 1) counts[1] += 1 else counts[0] += 1;
    }
    var result = zigr.protect.scoped(convert.fromIntSlice(&counts));
    defer result.deinit();
    zigr.attrib.setNames(result.get(), &.{ "false", "true", "missing" });
    return result.get();
}

fn benchRawCopy(value: R.SEXP) R.SEXP {
    const input = zigr.rvector.RVector(u8).init(value) catch |err| convert.signalError(err);
    return input.copy();
}

fn benchComplexConjugate(value: []const convert.Rcomplex) R.SEXP {
    var arena = zigr.memory.UnwindArena.init();
    defer arena.deinit();
    const result = arena.allocator().alloc(convert.Rcomplex, value.len) catch zigr.@"error".signal("complex result allocation failed");
    for (value, result) |source, *destination| destination.* = .{ .r = source.r, .i = -source.i };
    return convert.fromComplexSlice(result);
}

fn benchSchema(value: R.SEXP) R.SEXP {
    return fixtureSchema(value);
}
fn benchAltrepSum(value: convert.VectorAccess(i32, .one_pass)) f64 {
    var access = value;
    defer access.deinit();
    var total: f64 = 0.0;
    while (access.next() catch |err| convert.signalError(err)) |chunk| {
        for (chunk) |element| {
            if (element == R.R_NaInt) return R.R_NaReal;
            total += @floatFromInt(element);
        }
    }
    return total;
}
fn benchAltrepIndex(value: convert.VectorAccess(i32, .one_pass)) f64 {
    var access = value;
    defer access.deinit();
    var total: f64 = 0.0;
    var offset: usize = 0;
    var next_index: usize = 0;
    while (access.next() catch |err| convert.signalError(err)) |chunk| {
        for (chunk, 0..) |element, index| {
            if (offset + index == next_index) {
                total += @floatFromInt(element);
                next_index += 10000;
            }
        }
        offset += chunk.len;
    }
    return total;
}
fn benchAltrepMaterialize(value: convert.VectorAccess(i32, .random_access)) []const i32 {
    return value.contiguousSlice() orelse unreachable;
}
fn benchEval(value: R.SEXP) R.SEXP {
    return callR("function(x) eval(quote(sum(x)+mean(x)),list2env(list(x=x),parent=baseenv()))", &.{value});
}
fn benchSerialize(value: R.SEXP) R.SEXP {
    var bytes = zigr.protect.scoped(zigr.serialize.toVector(value));
    defer bytes.deinit();
    return zigr.serialize.fromVector(bytes.get());
}
fn benchRng(n: i32) R.SEXP {
    var count = zigr.protect.scoped(R.Rf_ScalarInteger(n));
    defer count.deinit();
    var package_name = zigr.protect.scoped(R.Rf_mkString("stats"));
    defer package_name.deinit();
    var namespace = zigr.protect.scoped(zigr.eval.callIn("getNamespace", &.{package_name.get()}, R.R_BaseEnv));
    defer namespace.deinit();
    return zigr.eval.callIn("rnorm", &.{count.get()}, namespace.get());
}
fn benchOutputs() R.SEXP {
    return fixtureOutputs();
}

const FixtureExports = zigr.@"export".generateExports(&.{
    .{ .name = "fixture_zero", .func = fixtureZero },
    .{ .name = "fixture_scalar", .func = fixtureScalar },
    .{ .name = "fixture_numeric", .func = fixtureNumeric },
    .{ .name = "fixture_altrep_integer", .func = fixtureAltrepInteger },
    .{ .name = "fixture_strings", .func = fixtureStrings },
    .{ .name = "fixture_raw", .func = fixtureRaw },
    .{ .name = "fixture_complex", .func = fixtureComplex },
    .{ .name = "fixture_schema", .func = fixtureSchema },
    .{ .name = "fixture_new", .func = fixtureNew },
    .{ .name = "fixture_error", .func = fixtureError },
    .{ .name = "fixture_lifecycle_reset", .func = fixtureLifecycleReset },
    .{ .name = "fixture_lifecycle_counts", .func = fixtureLifecycleCounts },
    .{ .name = "fixture_outputs", .func = fixtureOutputs },
    .{ .name = "bench_vector_sum", .func = benchVectorSum },
    .{ .name = "bench_numeric_transform", .func = benchNumericTransform },
    .{ .name = "bench_broadcast", .func = benchBroadcast },
    .{ .name = "bench_sort", .func = benchSort },
    .{ .name = "bench_missing_mean", .func = benchMissingMean },
    .{ .name = "bench_transpose", .func = benchTranspose },
    .{ .name = "bench_rowcol", .func = benchRowcol },
    .{ .name = "bench_matmul", .func = benchMatmul },
    .{ .name = "bench_dataframe", .func = benchDataframe },
    .{ .name = "bench_list_sum", .func = benchListSum },
    .{ .name = "bench_string_concat", .func = benchStringConcat },
    .{ .name = "bench_string_metadata", .func = benchStringMetadata },
    .{ .name = "bench_factor", .func = benchFactor },
    .{ .name = "bench_attributes", .func = benchAttributes },
    .{ .name = "bench_s4", .func = benchS4 },
    .{ .name = "bench_logical_counts", .func = benchLogicalCounts },
    .{ .name = "bench_raw_copy", .func = benchRawCopy },
    .{ .name = "bench_complex_conjugate", .func = benchComplexConjugate },
    .{ .name = "bench_schema", .func = benchSchema },
    .{ .name = "bench_altrep_sum", .func = benchAltrepSum },
    .{ .name = "bench_altrep_index", .func = benchAltrepIndex },
    .{ .name = "bench_altrep_materialize", .func = benchAltrepMaterialize },
    .{ .name = "bench_eval", .func = benchEval },
    .{ .name = "bench_serialize", .func = benchSerialize },
    .{ .name = "bench_rng", .func = benchRng },
    .{ .name = "bench_outputs", .func = benchOutputs },
}, &.{});

const FixtureMethods = zigr.@"export".generateMethods(FixtureState, &.{
    .{ .name = "increment", .func = fixtureIncrement },
    .{ .name = "read", .func = fixtureRead },
}, &.{});

const call_count = FixtureExports.call_defs.len - 1 + FixtureMethods.call_defs.len - 1;
var call_defs: [call_count + 1]R.R_CallMethodDef = undefined;

fn initPackage(info: *R.DllInfo) void {
    FixtureExports.init(info);
    FixtureMethods.init(info);
    inline for (0..FixtureExports.call_defs.len - 1) |index| {
        call_defs[index] = FixtureExports.call_defs[index];
    }
    inline for (0..FixtureMethods.call_defs.len - 1) |index| {
        call_defs[FixtureExports.call_defs.len - 1 + index] = FixtureMethods.call_defs[index];
    }
    call_defs[call_count] = .{ .name = null, .fun = null, .numArgs = 0 };
    _ = R.R_registerRoutines(info, null, @ptrCast(&call_defs[0]), null, null);
    _ = R.R_useDynamicSymbols(info, 0);
    _ = R.R_forceSymbols(info, 1);
}

export fn R_init_zigrFixture(info: *R.DllInfo) callconv(.c) void {
    initPackage(info);
}

export fn R_unload_zigrFixture(info: *R.DllInfo) callconv(.c) void {
    FixtureExports.unload(info);
    FixtureMethods.unload(info);
}
