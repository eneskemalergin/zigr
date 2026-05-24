const R = @import("R");
const raw = @import("raw");

const SEXP = R.SEXP;
const repeats: usize = 256;

fn dispatchSum(sexp: SEXP) f64 {
    switch (R.TYPEOF(sexp)) {
        R.REALSXP => {
            const data = raw.real(sexp);
            var total: f64 = 0.0;
            for (data) |value| total += value;
            return total;
        },
        R.INTSXP => {
            const data = raw.int(sexp);
            var total: f64 = 0.0;
            for (data) |value| total += @as(f64, @floatFromInt(value));
            return total;
        },
        R.LGLSXP => {
            const data = raw.logical(sexp);
            var total: f64 = 0.0;
            for (data) |value| total += if (value != 0) 1.0 else 0.0;
            return total;
        },
        else => return 0.0,
    }
}

export fn zigr_bench_comptime_dispatch(inputs_sexp: SEXP) SEXP {
    const n_inputs = @as(usize, @intCast(R.XLENGTH(inputs_sexp)));
    var total: f64 = 0.0;

    for (0..repeats) |_| {
        for (0..n_inputs) |i| {
            total += dispatchSum(R.VECTOR_ELT(inputs_sexp, @as(isize, @intCast(i))));
        }
    }

    return R.Rf_ScalarReal(total);
}
