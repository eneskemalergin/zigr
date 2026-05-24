const R = @import("R");
const raw = @import("raw");

const SEXP = R.SEXP;
const strategy_names = [_][:0]const u8{ "abs", "log", "exp", "sqrt" };
const c_kernel = @extern(*const fn ([*c]const f64, usize, [*c]f64) callconv(.c) void, .{ .name = "zigr_task34_kernel" });

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

    c_kernel(input.ptr, input.len, out);
    setNames(result);
    return result;
}
