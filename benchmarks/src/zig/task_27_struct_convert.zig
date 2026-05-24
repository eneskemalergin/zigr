const std = @import("std");
const R = @import("R");
const convert = @import("convert");

const SEXP = R.SEXP;

const field_names = [_][:0]const u8{
    "id",
    "count",
    "level",
    "flag",
    "enabled",
    "ratio",
    "offset",
    "scale",
    "weights",
    "indices",
};

fn cachedFieldNames() SEXP {
    const Cache = struct {
        var names: ?SEXP = null;
    };

    if (Cache.names) |names| return names;

    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, field_names.len));
    defer R.Rf_unprotect(1);
    for (field_names, 0..) |name, index| {
        R.SET_STRING_ELT(names, @intCast(index), R.Rf_mkChar(name));
    }
    R.R_PreserveObject(names);
    Cache.names = names;
    return names;
}

fn expectNamedList(input: SEXP) SEXP {
    if (R.TYPEOF(input) != R.VECSXP) convert.signalError(error.ExpectedNamedList);
    const names = R.Rf_getAttrib(input, R.R_NamesSymbol);
    if (names == R.R_NilValue or R.TYPEOF(names) != R.STRSXP or R.XLENGTH(names) != R.XLENGTH(input)) {
        convert.signalError(error.ExpectedNamedList);
    }
    return names;
}

fn expectFieldOrder(input: SEXP) void {
    const names = expectNamedList(input);
    const name_view = convert.toStringSliceView(names) catch |err| convert.signalError(err);
    if (name_view.len != field_names.len) convert.signalError(error.ExpectedNamedList);
    for (0..name_view.len) |index| {
        const name = name_view.at(index);
        if (name.is_na or !std.mem.eql(u8, name.bytes, field_names[index])) {
            convert.signalError(error.MissingField);
        }
    }
}

fn intScalarFast(sexp: SEXP) i32 {
    if (R.TYPEOF(sexp) != R.INTSXP or R.XLENGTH(sexp) == 0) convert.signalError(error.ExpectedInteger);
    const value = R.INTEGER(sexp)[0];
    if (value == R.R_NaInt) convert.signalError(error.ScalarNA);
    return value;
}

fn boolScalarFast(sexp: SEXP) bool {
    if (R.TYPEOF(sexp) != R.LGLSXP or R.XLENGTH(sexp) == 0) convert.signalError(error.ExpectedLogical);
    const value = R.LOGICAL(sexp)[0];
    if (value == R.R_NaInt) convert.signalError(error.ScalarNA);
    return value != 0;
}

fn realScalarFast(sexp: SEXP) f64 {
    if (R.TYPEOF(sexp) != R.REALSXP or R.XLENGTH(sexp) == 0) convert.signalError(error.ExpectedReal);
    const value = R.REAL(sexp)[0];
    if (R.ISNA(value) != 0) convert.signalError(error.ScalarNA);
    return value;
}

export fn zigr_bench_struct_convert(input_sexp: SEXP) SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    expectFieldOrder(input_sexp);

    const weights_in = R.VECTOR_ELT(input_sexp, 8);
    const indices_in = R.VECTOR_ELT(input_sexp, 9);
    const id_in = R.VECTOR_ELT(input_sexp, 0);
    const count_in = R.VECTOR_ELT(input_sexp, 1);
    const level_in = R.VECTOR_ELT(input_sexp, 2);
    const flag_in = R.VECTOR_ELT(input_sexp, 3);
    const enabled_in = R.VECTOR_ELT(input_sexp, 4);
    const ratio_in = R.VECTOR_ELT(input_sexp, 5);
    const offset_in = R.VECTOR_ELT(input_sexp, 6);
    const scale_in = R.VECTOR_ELT(input_sexp, 7);

    _ = intScalarFast(id_in);
    _ = intScalarFast(count_in);
    _ = intScalarFast(level_in);
    _ = boolScalarFast(flag_in);
    _ = boolScalarFast(enabled_in);
    _ = realScalarFast(ratio_in);
    _ = realScalarFast(offset_in);
    _ = realScalarFast(scale_in);

    const weights = convert.toRealSliceView(arena.allocator(), weights_in) catch |err| convert.signalError(err);
    const indices = convert.toIntSliceView(arena.allocator(), indices_in) catch |err| convert.signalError(err);

    const result = R.Rf_protect(R.Rf_allocVector(R.VECSXP, field_names.len));
    defer R.Rf_unprotect(1);

    _ = R.SET_VECTOR_ELT(result, 0, id_in);
    _ = R.SET_VECTOR_ELT(result, 1, count_in);
    _ = R.SET_VECTOR_ELT(result, 2, level_in);
    _ = R.SET_VECTOR_ELT(result, 3, flag_in);
    _ = R.SET_VECTOR_ELT(result, 4, enabled_in);
    _ = R.SET_VECTOR_ELT(result, 5, ratio_in);
    _ = R.SET_VECTOR_ELT(result, 6, offset_in);
    _ = R.SET_VECTOR_ELT(result, 7, scale_in);

    const weights_out = R.Rf_allocVector(R.REALSXP, @intCast(weights.len));
    @memcpy(R.REAL(weights_out)[0..weights.len], weights);
    _ = R.SET_VECTOR_ELT(result, 8, weights_out);

    const indices_out = R.Rf_allocVector(R.INTSXP, @intCast(indices.len));
    @memcpy(R.INTEGER(indices_out)[0..indices.len], indices);
    _ = R.SET_VECTOR_ELT(result, 9, indices_out);

    _ = R.Rf_setAttrib(result, R.R_NamesSymbol, cachedFieldNames());
    return result;
}
