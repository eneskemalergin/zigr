const R = @import("R");
const zigr = @import("zigr");
const sexp = zigr.sexp;
const attrib = zigr.attrib;

export fn zigr_bench_attrib_ops(vec: R.SEXP) R.SEXP {
    const cls_val = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    R.SET_STRING_ELT(cls_val, 0, R.Rf_mkChar("bench_class"));
    _ = R.Rf_setAttrib(vec, R.R_ClassSymbol, cls_val);
    R.Rf_unprotect(1);

    const cr_sym = R.Rf_install("creator");
    const cr_val = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    R.SET_STRING_ELT(cr_val, 0, R.Rf_mkChar("zigr_bench"));
    _ = R.Rf_setAttrib(vec, cr_sym, cr_val);
    R.Rf_unprotect(1);

    const got_cls = R.Rf_getAttrib(vec, R.R_ClassSymbol);
    const got_cr = R.Rf_getAttrib(vec, cr_sym);

    var total: i32 = 0;
    const nc = sexp.fastLength(got_cls);
    for (0..@as(usize, @intCast(nc))) |i| {
        const elt = sexp.fastVectorElt(got_cls, @intCast(i));
        total += @as(i32, @intCast(sexp.fastLength(elt)));
    }
    const ncr = sexp.fastLength(got_cr);
    for (0..@as(usize, @intCast(ncr))) |i| {
        const elt = sexp.fastVectorElt(got_cr, @intCast(i));
        total += @as(i32, @intCast(sexp.fastLength(elt)));
    }
    return R.Rf_ScalarInteger(total);
}
