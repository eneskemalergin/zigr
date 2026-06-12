const R = @import("R");
const sexp = @import("zigr").sexp;
const SEXP = R.SEXP;

export fn zigr_bench_type_dispatch(arg: SEXP) SEXP {
    const t0 = R.VECTOR_ELT(arg, 0);
    const t1 = R.VECTOR_ELT(arg, 1);
    const t2 = R.VECTOR_ELT(arg, 2);
    var total: i32 = 0;
    for (0..2048) |_| {
        total += switch (sexp.typeTag(t0)) {
            14 => 1,
            13 => 2,
            16 => 3,
            else => 0,
        };
        total += switch (sexp.typeTag(t1)) {
            14 => 1,
            13 => 2,
            16 => 3,
            else => 0,
        };
        total += switch (sexp.typeTag(t2)) {
            14 => 1,
            13 => 2,
            16 => 3,
            else => 0,
        };
    }
    return R.Rf_ScalarInteger(total);
}
