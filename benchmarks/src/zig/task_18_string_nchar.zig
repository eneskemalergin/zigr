const R = @import("R");
const sexp = @import("zigr").sexp;
const SEXP = R.SEXP;

export fn zigr_bench_string_nchar(vec: SEXP) SEXP {
    const n = sexp.xlength(vec);
    var total: i64 = 0;
    for (0..n) |i| {
        const elt = sexp.fastVectorElt(vec, i);
        if (elt == R.R_NaString) continue;
        total += @as(i64, @intCast(sexp.fastLength(elt)));
    }
    return R.Rf_ScalarInteger(@intCast(total));
}
