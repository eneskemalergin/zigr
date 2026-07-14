//! Normalized package fixture for the P4 product capability matrix.
//!
//! Every supported cell uses zigr's public conversion, export, method, and
//! ownership APIs. R owns returned SEXPs and typed state after construction.

const R = @import("R");
const zigr = @import("zigr");

const convert = zigr.convert;

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

fn fixtureNumeric(value: []const f64) R.SEXP {
    var arena = zigr.memory.UnwindArena.init();
    defer arena.deinit();
    const result = arena.allocator().alloc(f64, value.len) catch
        zigr.@"error".signal("numeric fixture allocation failed");
    for (value, result) |source, *destination| destination.* = source * 2.0;
    return convert.fromRealSlice(result);
}

fn fixtureAltrepInteger(value: []const i32) f64 {
    var total: f64 = 0.0;
    for (value) |element| {
        if (element == R.R_NaInt) return R.R_NaReal;
        total += @floatFromInt(element);
    }
    return total;
}

fn fixtureStrings(value: convert.StringSliceView) i32 {
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
    var raw = zigr.protect.scoped(convert.fromRawSlice(&raw_values));
    defer raw.deinit();
    var complex = zigr.protect.scoped(convert.fromComplexSlice(&complex_values));
    defer complex.deinit();
    var logical = zigr.protect.scoped(convert.fromLogicalSlice(&logical_values));
    defer logical.deinit();
    var nested = zigr.protect.scoped(convert.asSEXP(NestedOutput{ .value = 7 }));
    defer nested.deinit();

    const values = [_]R.SEXP{
        numeric.get(),
        string.get(),
        raw.get(),
        complex.get(),
        logical.get(),
        nested.get(),
    };
    var result = zigr.protect.scoped(convert.fromListSlice(&values));
    defer result.deinit();
    zigr.attrib.setNames(result.get(), &.{ "numeric", "string", "raw", "complex", "logical", "list" });
    return result.get();
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
