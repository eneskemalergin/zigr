const R = @import("R");
const sexp = @import("zigr").sexp;

export fn zigr_bench_string_encoding(vec: R.SEXP) R.SEXP {
    const n = sexp.xlength(vec);
    var total: i32 = 0;
    for (0..n) |i| {
        const elt = sexp.fastVectorElt(vec, i);
        total += sexp.fastGetCharCE(elt);
    }
    return R.Rf_ScalarInteger(total);
}
