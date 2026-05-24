const R = @import("R");
const raw = @import("zigr").raw;

const SEXP = R.SEXP;
const repeats: usize = 4096;
const strategy_names = [_][:0]const u8{ "unsafe", "manual", "batch", "preserve", "reprotect" };

fn fillAndSum(vec: SEXP, input: []const f64, bias: f64) f64 {
    const data = R.REAL(vec);
    var total: f64 = 0.0;

    for (input, 0..) |value, index| {
        const adjusted = value + bias;
        data[index] = adjusted;
        total += adjusted;
    }
    return total;
}

fn setNames(result: SEXP) void {
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, strategy_names.len));
    defer R.Rf_unprotect(1);

    for (strategy_names, 0..) |name, index| {
        R.SET_STRING_ELT(names, @intCast(index), R.Rf_mkChar(name));
    }

    _ = R.Rf_setAttrib(result, R.R_NamesSymbol, names);
}

export fn zigr_bench_prot_overhead(vec: SEXP) SEXP {
    const input = raw.real(vec);
    const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, strategy_names.len));
    defer R.Rf_unprotect(1);

    const out = R.REAL(result);

    var unsafe_total: f64 = 0.0;
    for (0..repeats) |repeat_index| {
        const bias = (@as(f64, @floatFromInt(repeat_index)) + 1.0) * 0.001;
        const temp = R.Rf_allocVector(R.REALSXP, @intCast(input.len));
        unsafe_total += fillAndSum(temp, input, bias);
    }
    out[0] = unsafe_total;

    var manual_total: f64 = 0.0;
    for (0..repeats) |repeat_index| {
        const bias = (@as(f64, @floatFromInt(repeat_index)) + 1.0) * 0.001;
        const temp = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(input.len)));
        manual_total += fillAndSum(temp, input, bias);
        R.Rf_unprotect(1);
    }
    out[1] = manual_total;

    var batch_total: f64 = 0.0;
    for (0..repeats) |repeat_index| {
        const bias = (@as(f64, @floatFromInt(repeat_index)) + 1.0) * 0.001;
        const temp = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(input.len)));
        batch_total += fillAndSum(temp, input, bias);
    }
    R.Rf_unprotect(@intCast(repeats));
    out[2] = batch_total;

    var preserve_total: f64 = 0.0;
    for (0..repeats) |repeat_index| {
        const bias = (@as(f64, @floatFromInt(repeat_index)) + 1.0) * 0.001;
        const temp = R.Rf_allocVector(R.REALSXP, @intCast(input.len));
        R.R_PreserveObject(temp);
        preserve_total += fillAndSum(temp, input, bias);
        R.R_ReleaseObject(temp);
    }
    out[3] = preserve_total;

    var reprotect_total: f64 = 0.0;
    var protect_index: R.PROTECT_INDEX = undefined;
    R.R_ProtectWithIndex(R.R_NilValue, &protect_index);
    defer R.Rf_unprotect(1);
    for (0..repeats) |repeat_index| {
        const bias = (@as(f64, @floatFromInt(repeat_index)) + 1.0) * 0.001;
        const temp = R.Rf_allocVector(R.REALSXP, @intCast(input.len));
        R.R_Reprotect(temp, protect_index);
        reprotect_total += fillAndSum(temp, input, bias);
    }
    out[4] = reprotect_total;

    setNames(result);
    return result;
}
