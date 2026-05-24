const R = @import("R");
const dataframe = @import("zigr").dataframe;
const raw = @import("zigr").raw;

const SEXP = R.SEXP;

export fn zigr_bench_dataframe(df_sexp: SEXP) SEXP {
    const df = dataframe.DataFrame.wrap(df_sexp) orelse return R.R_NilValue;
    const nrows = @as(usize, @intCast(df.rowCount()));

    const x_idx = df.columnIndex("x") orelse return R.R_NilValue;
    const y_idx = df.columnIndex("y") orelse return R.R_NilValue;
    const grp_idx = df.columnIndex("grp") orelse return R.R_NilValue;

    const x_col = df.columnByIndex(x_idx);
    const y_col = df.columnByIndex(y_idx);
    const grp_col = df.columnByIndex(grp_idx);

    const x_slice = raw.real(x_col);
    const y_slice = raw.real(y_col);
    const grp_raw = raw.int(grp_col);

    var max_grp: i32 = 0;
    for (0..nrows) |i| {
        if (x_slice[i] > 0.0 and grp_raw[i] > max_grp) max_grp = grp_raw[i];
    }

    const ngroups = @as(usize, @intCast(max_grp));
    const grp_out = R.Rf_protect(R.Rf_allocVector(R.INTSXP, @intCast(ngroups)));
    defer R.Rf_unprotect(1);
    const sum_out = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(ngroups)));
    defer R.Rf_unprotect(1);

    const grp_out_slice = raw.intMut(grp_out);
    const sums = raw.realMut(sum_out);
    @memset(sums, 0.0);

    for (0..ngroups) |i| grp_out_slice[i] = @intCast(i + 1);

    for (0..nrows) |i| {
        if (x_slice[i] > 0.0) {
            sums[@as(usize, @intCast(grp_raw[i] - 1))] += x_slice[i] / y_slice[i];
        }
    }

    const cols = [_]SEXP{ grp_out, sum_out };
    const result = R.Rf_protect(dataframe.build(&.{ "grp", "z_sum" }, cols[0..]));
    defer R.Rf_unprotect(1);
    return result;
}
