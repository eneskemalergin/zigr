const R = @import("R");
const zigr = @import("zigr");

const externalptr = zigr.externalptr;

const BenchmarkState = struct { value: i32 };

fn deinitBenchmarkState(_: *BenchmarkState) void {}

export fn zigr_bench_external_ptr(value: R.SEXP) R.SEXP {
    const expected = R.Rf_asInteger(value);
    var result = zigr.protect.scoped(externalptr.createTyped(
        BenchmarkState,
        .{ .value = expected },
        deinitBenchmarkState,
    ));
    defer result.deinit();
    const restored = externalptr.checkedPointer(BenchmarkState, result.get()) catch |pointer_error|
        zigr.@"error".signal(externalptr.errorMessage(pointer_error));
    if (restored.value != expected) zigr.@"error".signal("external-pointer benchmark lost its typed state");
    return result.get();
}
