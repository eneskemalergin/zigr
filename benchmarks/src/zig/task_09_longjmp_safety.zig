const R = @import("R");
const raw = @import("zigr").raw;

const SEXP = R.SEXP;
const repeats: usize = 512;
const strategy_names = [_][:0]const u8{ "direct", "try_ok", "try_err", "unwind_ok" };

const UnwindState = struct {
    input: []const f64,
    bias: f64,
};

fn adjustedSum(input: []const f64, bias: f64) f64 {
    var total: f64 = 0.0;
    for (input) |value| total += value + bias;
    return total;
}

fn fillAdjusted(vec: SEXP, input: []const f64, bias: f64) void {
    const data = R.REAL(vec);
    for (input, 0..) |value, index| data[index] = value + bias;
}

fn setNames(result: SEXP) void {
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, strategy_names.len));
    defer R.Rf_unprotect(1);

    for (strategy_names, 0..) |name, index| {
        R.SET_STRING_ELT(names, @intCast(index), R.Rf_mkChar(name));
    }
    _ = R.Rf_setAttrib(result, R.R_NamesSymbol, names);
}

fn unwindNoop(_: ?*anyopaque, _: R.Rboolean) callconv(.c) void {}

fn unwindOkCallback(data: ?*anyopaque) callconv(.c) SEXP {
    const state = @as(*const UnwindState, @ptrCast(@alignCast(data.?)));
    return R.Rf_ScalarReal(adjustedSum(state.input, state.bias));
}

export fn zigr_bench_longjmp_safety(vec: SEXP) SEXP {
    const input = raw.real(vec);
    const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, strategy_names.len));
    defer R.Rf_unprotect(1);

    const out = R.REAL(result);
    const sum_sym = R.Rf_install("sum");
    const stop_sym = R.Rf_install("stop");
    const stop_msg = R.Rf_protect(R.Rf_mkString("task32"));
    defer R.Rf_unprotect(1);
    const stop_call = R.Rf_protect(R.Rf_lang2(stop_sym, stop_msg));
    defer R.Rf_unprotect(1);

    var direct_total: f64 = 0.0;
    var try_ok_total: f64 = 0.0;
    var try_err_total: f64 = 0.0;
    var unwind_ok_total: f64 = 0.0;

    for (0..repeats) |repeat_index| {
        const bias = (@as(f64, @floatFromInt(repeat_index)) + 1.0) * 0.001;
        direct_total += adjustedSum(input, bias);

        const temp = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(input.len)));
        fillAdjusted(temp, input, bias);
        const expr = R.Rf_protect(R.Rf_lang2(sum_sym, temp));
        var err: c_int = 0;
        const eval_result = R.R_tryEvalSilent(expr, R.R_GlobalEnv, &err);
        try_ok_total += R.REAL(eval_result)[0];
        R.Rf_unprotect(2);

        err = 0;
        _ = R.R_tryEvalSilent(stop_call, R.R_GlobalEnv, &err);
        if (err != 0) try_err_total += 1.0;

        var state = UnwindState{ .input = input, .bias = bias };
        const cont = R.Rf_protect(R.R_MakeUnwindCont());
        const unwind_result = R.R_UnwindProtect(unwindOkCallback, &state, unwindNoop, null, cont);
        unwind_ok_total += R.REAL(unwind_result)[0];
        R.Rf_unprotect(1);
    }

    out[0] = direct_total;
    out[1] = try_ok_total;
    out[2] = try_err_total;
    out[3] = unwind_ok_total;
    setNames(result);
    return result;
}
