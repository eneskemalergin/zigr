const R = @import("R");
const SEXP = R.SEXP;

export fn zigr_bench_struct_convert(vec: SEXP) SEXP {
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 10));
    defer R.Rf_unprotect(1);
    const field_names = [_][:0]const u8{ "id", "count", "level", "flag", "enabled", "ratio", "offset", "scale", "weights", "indices" };
    for (field_names, 0..) |name, i| R.SET_STRING_ELT(names, @intCast(i), R.Rf_mkChar(name));

    const result = R.Rf_protect(R.Rf_allocVector(R.VECSXP, 10));
    defer R.Rf_unprotect(1);
    _ = R.Rf_setAttrib(result, R.R_NamesSymbol, names);

    _ = R.SET_VECTOR_ELT(result, 0, R.Rf_ScalarInteger(R.Rf_asInteger(R.VECTOR_ELT(vec, 0))));
    _ = R.SET_VECTOR_ELT(result, 1, R.Rf_ScalarInteger(R.Rf_asInteger(R.VECTOR_ELT(vec, 1))));
    _ = R.SET_VECTOR_ELT(result, 2, R.Rf_ScalarInteger(R.Rf_asInteger(R.VECTOR_ELT(vec, 2))));
    _ = R.SET_VECTOR_ELT(result, 3, R.Rf_ScalarLogical(R.Rf_asLogical(R.VECTOR_ELT(vec, 3))));
    _ = R.SET_VECTOR_ELT(result, 4, R.Rf_ScalarLogical(R.Rf_asLogical(R.VECTOR_ELT(vec, 4))));
    _ = R.SET_VECTOR_ELT(result, 5, R.Rf_ScalarReal(R.Rf_asReal(R.VECTOR_ELT(vec, 5))));
    _ = R.SET_VECTOR_ELT(result, 6, R.Rf_ScalarReal(R.Rf_asReal(R.VECTOR_ELT(vec, 6))));
    _ = R.SET_VECTOR_ELT(result, 7, R.Rf_ScalarReal(R.Rf_asReal(R.VECTOR_ELT(vec, 7))));
    _ = R.SET_VECTOR_ELT(result, 8, R.VECTOR_ELT(vec, 8));
    _ = R.SET_VECTOR_ELT(result, 9, R.VECTOR_ELT(vec, 9));

    return result;
}
