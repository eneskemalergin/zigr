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

fn realOnePass(value: R.SEXP) convert.VectorAccess(f64, .one_pass) {
    return convert.toVectorAccess(f64, .one_pass, std.heap.page_allocator, value) catch |err|
        convert.signalError(err);
}

fn complexOnePass(value: R.SEXP) convert.VectorAccess(convert.Rcomplex, .one_pass) {
    return convert.toVectorAccess(convert.Rcomplex, .one_pass, std.heap.page_allocator, value) catch |err|
        convert.signalError(err);
}

fn directReal(value: R.SEXP) ?[]const f64 {
    if (value == null or R.TYPEOF(value) != R.REALSXP or R.ALTREP(value) != 0) return null;
    const length = R.XLENGTH(value);
    if (length < 0) zigr.@"error".signal("real length is negative");
    return R.REAL(value)[0..@as(usize, @intCast(length))];
}

fn benchVectorSum(value: R.SEXP) f64 {
    return convert.sum(value);
}

fn benchNumericTransform(value: R.SEXP) R.SEXP {
    return fixtureNumeric(value);
}

fn benchBroadcast(value: R.SEXP, scalar: f64) f64 {
    if (directReal(value)) |data| {
        var total: f80 = 0.0;
        var index: usize = 0;
        while (index + 4 <= data.len) : (index += 4) {
            total += @floatCast(data[index] + scalar);
            total += @floatCast(data[index + 1] + scalar);
            total += @floatCast(data[index + 2] + scalar);
            total += @floatCast(data[index + 3] + scalar);
        }
        while (index < data.len) : (index += 1) total += @floatCast(data[index] + scalar);
        return @floatCast(total);
    }
    var input = realOnePass(value);
    defer input.deinit();
    var total: f80 = 0.0;
    while (input.next() catch |err| convert.signalError(err)) |chunk| {
        for (chunk) |element| total += @floatCast(element + scalar);
    }
    return @floatCast(total);
}

fn radixSortRealSmall(values: []f64, scratch: []u64) void {
    const output_bits = @as([*]u64, @ptrCast(values.ptr))[0..values.len];
    const sign_bit = @as(u64, 1) << 63;
    for (output_bits) |*value| value.* = if (value.* & sign_bit != 0) ~value.* else value.* ^ sign_bit;

    const digit_bits = 11;
    const digit_mask = (1 << digit_bits) - 1;
    var counts: [6][1 << digit_bits]u16 = undefined;
    @memset(std.mem.asBytes(&counts), 0);
    for (output_bits) |value| {
        inline for (0..counts.len) |pass| {
            const shift: u6 = @intCast(pass * digit_bits);
            counts[pass][@intCast((value >> shift) & digit_mask)] += 1;
        }
    }

    var source = output_bits;
    var destination = scratch;
    inline for (0..counts.len) |pass| {
        var offset: u16 = 0;
        for (&counts[pass]) |*count| {
            const length = count.*;
            count.* = offset;
            offset += length;
        }
        const shift: u6 = @intCast(pass * digit_bits);
        for (source) |value| {
            const digit: usize = @intCast((value >> shift) & digit_mask);
            destination[counts[pass][digit]] = value;
            counts[pass][digit] += 1;
        }
        const previous = source;
        source = destination;
        destination = previous;
    }

    for (output_bits) |*value| value.* = if (value.* & sign_bit != 0) value.* ^ sign_bit else ~value.*;
}

fn radixSortReal(values: []f64) void {
    if (values.len < 2) return;

    if (values.len < 1 << 16) {
        const scratch = zigr.memory.RAllocator.alloc(u64, values.len) catch |err| convert.signalError(err);
        defer zigr.memory.RAllocator.free(scratch);
        radixSortRealSmall(values, scratch);
        return;
    }

    const scratch = std.heap.page_allocator.alloc(u64, values.len) catch |err| convert.signalError(err);
    defer std.heap.page_allocator.free(scratch);
    const counts = std.heap.page_allocator.alloc(usize, 1 << 16) catch |err| {
        std.heap.page_allocator.free(scratch);
        convert.signalError(err);
    };
    defer std.heap.page_allocator.free(counts);

    const output_bits = @as([*]u64, @ptrCast(values.ptr))[0..values.len];
    const sign_bit = @as(u64, 1) << 63;
    for (output_bits) |*value| value.* = if (value.* & sign_bit != 0) ~value.* else value.* ^ sign_bit;

    var source = output_bits;
    var destination = scratch;
    var shift: usize = 0;
    while (shift < 64) : (shift += 16) {
        @memset(counts, 0);
        const bit_shift: u6 = @intCast(shift);
        for (source) |value| counts[@intCast((value >> bit_shift) & 0xffff)] += 1;
        var offset: usize = 0;
        for (counts) |*count| {
            const length = count.*;
            count.* = offset;
            offset += length;
        }
        for (source) |value| {
            const digit: usize = @intCast((value >> bit_shift) & 0xffff);
            destination[counts[digit]] = value;
            counts[digit] += 1;
        }
        const previous = source;
        source = destination;
        destination = previous;
    }

    for (output_bits) |*value| value.* = if (value.* & sign_bit != 0) value.* ^ sign_bit else ~value.*;
}

fn realAscending(_: void, left: f64, right: f64) bool {
    const left_nan = std.math.isNan(left);
    const right_nan = std.math.isNan(right);
    if (left_nan) return false;
    if (right_nan) return true;
    return left < right;
}

fn benchSort(value: R.SEXP) R.SEXP {
    const input = zigr.rvector.RVector(f64).init(value) catch |err| convert.signalError(err);
    var result = convert.ResultBuilder(f64).init(input.len());
    defer result.deinit();
    var access = convert.toVectorAccess(f64, .one_pass, std.heap.page_allocator, value) catch |err| convert.signalError(err);
    defer access.deinit();
    const output = result.mutableSlice();
    var offset: usize = 0;
    while (access.next() catch |err| convert.signalError(err)) |chunk| {
        @memcpy(output[offset .. offset + chunk.len], chunk);
        offset += chunk.len;
    }
    var has_nan = false;
    for (output) |element| has_nan = has_nan or std.math.isNan(element);
    if (has_nan) {
        std.mem.sort(f64, output, {}, realAscending);
    } else {
        radixSortReal(output);
    }
    return result.finish();
}

fn benchMissingMean(value: R.SEXP) f64 {
    return convert.mean_narm(value);
}

fn transposeInto(output: []f64, source: []const f64, rows: usize, columns: usize) void {
    const block_size: usize = 32;
    var column_block: usize = 0;
    while (column_block < columns) : (column_block += block_size) {
        const column_end = @min(column_block + block_size, columns);
        var row_block: usize = 0;
        while (row_block < rows) : (row_block += block_size) {
            const row_end = @min(row_block + block_size, rows);
            for (row_block..row_end) |row| {
                for (column_block..column_end) |column| {
                    output[column + row * columns] = source[row + column * rows];
                }
            }
        }
    }
}

fn benchTranspose(value: R.SEXP) R.SEXP {
    const rows: usize = @intCast(R.Rf_nrows(value));
    const columns: usize = @intCast(R.Rf_ncols(value));
    var result = zigr.protect.scoped(R.Rf_allocMatrix(R.REALSXP, @intCast(columns), @intCast(rows)));
    defer result.deinit();
    const output = raw.realMut(result.get()) catch |raw_error| zigr.@"error".signal(@errorName(raw_error));
    if (directReal(value)) |source| {
        transposeInto(output, source, rows, columns);
        return result.get();
    }
    var source_access = convert.toVectorAccess(f64, .random_access, std.heap.page_allocator, value) catch |err|
        convert.signalError(err);
    defer source_access.deinit();
    const source = source_access.contiguousSlice() orelse convert.signalError(error.DirectPointerUnavailable);
    transposeInto(output, source, rows, columns);
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
    if (value == null or R.TYPEOF(value) != R.VECSXP) convert.signalError(error.ExpectedList);
    const length = R.XLENGTH(value);
    if (length < 0) zigr.@"error".signal("list length is negative");
    var total: c_longdouble = 0.0;
    var na_seen = false;
    var nan_seen = false;
    for (0..@as(usize, @intCast(length))) |index| {
        const item = R.VECTOR_ELT(value, @intCast(index));
        const item_total = convert.sum(item);
        if (std.math.isNan(item_total)) {
            if (R.ISNA(item_total) != 0) na_seen = true else nan_seen = true;
            continue;
        }
        total += @as(c_longdouble, @floatCast(item_total));
    }
    if (na_seen) return R.R_NaReal;
    if (nan_seen) return R.R_NaN;

    const largest_f64: c_longdouble = std.math.floatMax(f64);
    if (total > largest_f64) return std.math.inf(f64);
    if (total < -largest_f64) return -std.math.inf(f64);
    return @floatCast(total);
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

fn benchComplexConjugate(value: R.SEXP) R.SEXP {
    if (value == null) zigr.@"error".signal("complex input is null");
    const source_length = R.XLENGTH(value);
    if (source_length < 0) zigr.@"error".signal("complex length is negative");
    var result = zigr.protect.scoped(R.Rf_allocVector(R.CPLXSXP, @intCast(source_length)));
    defer result.deinit();
    const output = raw.complexMut(result.get()) catch |raw_error| zigr.@"error".signal(@errorName(raw_error));
    var offset: usize = 0;
    if (value != null and R.TYPEOF(value) == R.CPLXSXP and R.ALTREP(value) == 0) {
        const source = raw.complex(value) catch |raw_error| zigr.@"error".signal(@errorName(raw_error));
        for (source) |input_value| {
            output[offset] = .{ .r = input_value.r, .i = -input_value.i };
            offset += 1;
        }
    } else {
        var source = complexOnePass(value);
        defer source.deinit();
        while (source.next() catch |err| convert.signalError(err)) |chunk| {
            for (chunk) |input_value| {
                output[offset] = .{ .r = input_value.r, .i = -input_value.i };
                offset += 1;
            }
        }
    }
    return result.get();
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
fn benchAltrepMaterialize(value: R.SEXP) R.SEXP {
    const input = zigr.rvector.RVector(i32).init(value) catch |err| convert.signalError(err);
    return input.copy();
}
fn benchEval(value: R.SEXP) R.SEXP {
    return callR("function(x) eval(quote(sum(x)+mean(x)),list2env(list(x=x),parent=baseenv()))", &.{value});
}
fn benchSerialize(value: R.SEXP) R.SEXP {
    var bytes = zigr.protect.scoped(zigr.serialize.toVector(value));
    defer bytes.deinit();
    return zigr.serialize.fromVector(bytes.get());
}
threadlocal var rng_count: i32 = 0;

fn drawRng() R.SEXP {
    if (rng_count < 0) zigr.@"error".signal("RNG count must be non-negative");
    const result = R.Rf_allocVector(R.REALSXP, @intCast(rng_count));
    const values = R.REAL(result);
    for (0..@as(usize, @intCast(rng_count))) |index| values[index] = zigr.rng.normal();
    return result;
}

fn benchRng(n: i32) R.SEXP {
    rng_count = n;
    return zigr.rng.withRng(drawRng);
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
