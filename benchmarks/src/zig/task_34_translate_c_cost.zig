const R = @import("R");
const raw = @import("raw");

const SEXP = R.SEXP;
const repeats: usize = 512;
const strategy_names = [_][:0]const u8{ "abs", "log", "exp", "sqrt" };

fn setNames(result: SEXP) void {
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, strategy_names.len));
    defer R.Rf_unprotect(1);

    for (strategy_names, 0..) |name, index| {
        R.SET_STRING_ELT(names, @intCast(index), R.Rf_mkChar(name));
    }
    _ = R.Rf_setAttrib(result, R.R_NamesSymbol, names);
}

export fn zigr_bench_translate_c_cost(vec: SEXP) SEXP {
    const input = raw.real(vec);
    const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, strategy_names.len));
    defer R.Rf_unprotect(1);
    const out = R.REAL(result);

    var abs_total: f64 = 0.0;
    var log_total: f64 = 0.0;
    var exp_total: f64 = 0.0;
    var sqrt_total: f64 = 0.0;

    for (0..repeats) |repeat_index| {
        const bias = (@as(f64, @floatFromInt(repeat_index)) + 1.0) * 0.001;
        for (input) |value| {
            const shifted = value + bias;
            abs_total += @abs(shifted - 0.75);
            log_total += @log(shifted);
            exp_total += @exp(shifted);
            sqrt_total += @sqrt(shifted);
        }
    }

    out[0] = abs_total;
    out[1] = log_total;
    out[2] = exp_total;
    out[3] = sqrt_total;
    setNames(result);
    return result;
}
