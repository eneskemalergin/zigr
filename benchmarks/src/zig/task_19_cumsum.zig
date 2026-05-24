const R = @import("R");
const raw = @import("raw");

const SEXP = R.SEXP;

export fn zigr_bench_cumsum(vec_sexp: SEXP) SEXP {
    const src = raw.real(vec_sexp);
    const n = src.len;

    const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(n)));
    defer R.Rf_unprotect(1);
    const rp = raw.realMut(result);

    var total: f64 = 0.0;
    var i: usize = 0;

    while (i + 3 < n) {
        total += src[i];
        rp[i] = total;
        total += src[i + 1];
        rp[i + 1] = total;
        total += src[i + 2];
        rp[i + 2] = total;
        total += src[i + 3];
        rp[i + 3] = total;
        i += 4;
    }

    while (i < n) : (i += 1) {
        total += src[i];
        rp[i] = total;
    }

    return result;
}
