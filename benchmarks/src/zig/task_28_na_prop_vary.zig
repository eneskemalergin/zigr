const R = @import("R");
const convert = @import("convert");

const SEXP = R.SEXP;

export fn zigr_bench_na_prop_vary(inputs_sexp: SEXP) SEXP {
    const n_inputs = @as(usize, @intCast(R.Rf_xlength(inputs_sexp)));
    const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(n_inputs)));
    defer R.Rf_unprotect(1);

    const out = R.REAL(result);
    for (0..n_inputs) |i| {
        out[i] = convert.mean_narm(R.VECTOR_ELT(inputs_sexp, @as(isize, @intCast(i))));
    }

    _ = R.Rf_setAttrib(result, R.R_NamesSymbol, R.Rf_getAttrib(inputs_sexp, R.R_NamesSymbol));
    return result;
}
