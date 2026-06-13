const R = @import("R");

export fn zigr_bench_struct_convert(vec: R.SEXP) R.SEXP {
    const id = @as(f64, @floatFromInt(R.Rf_asInteger(R.VECTOR_ELT(vec, 0))));
    const count = @as(f64, @floatFromInt(R.Rf_asInteger(R.VECTOR_ELT(vec, 1))));
    const level = @as(f64, @floatFromInt(R.Rf_asInteger(R.VECTOR_ELT(vec, 2))));
    const flag = @as(f64, @floatFromInt(R.Rf_asLogical(R.VECTOR_ELT(vec, 3))));
    const enabled = @as(f64, @floatFromInt(R.Rf_asLogical(R.VECTOR_ELT(vec, 4))));
    const ratio = R.Rf_asReal(R.VECTOR_ELT(vec, 5));
    const offset = R.Rf_asReal(R.VECTOR_ELT(vec, 6));
    const scale = R.Rf_asReal(R.VECTOR_ELT(vec, 7));
    const weights = R.VECTOR_ELT(vec, 8);
    const indices = R.VECTOR_ELT(vec, 9);
    const wn = R.XLENGTH(weights);
    const wp: [*]const f64 = @ptrCast(R.REAL(weights));
    var ws: f64 = 0.0;
    for (0..@as(usize, @intCast(wn))) |i| ws += wp[i];
    const isn = R.XLENGTH(indices);
    const ip: [*]const c_int = @ptrCast(R.INTEGER(indices));
    var is: f64 = 0.0;
    for (0..@as(usize, @intCast(isn))) |i| is += @as(f64, @floatFromInt(ip[i]));
    return R.Rf_ScalarReal(id + count + level + flag + enabled + ratio + offset + scale + ws + is);
}
