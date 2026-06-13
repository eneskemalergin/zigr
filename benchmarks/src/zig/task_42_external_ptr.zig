const R = @import("R");

export fn zigr_bench_external_ptr(_: R.SEXP) R.SEXP {
    var dummy: u8 = 0;
    const ptr = R.Rf_protect(R.R_MakeExternalPtr(&dummy, R.R_NilValue, R.R_NilValue));
    R.Rf_unprotect(1);
    return ptr;
}
