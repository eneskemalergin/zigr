const R = @import("R");
const sexp = @import("zigr").sexp;
const SEXP = R.SEXP;

fn isVectorTag(tag: u5) bool {
    return switch (tag) {
        10, 13, 14, 15, 16, 19, 20, 24 => true,
        else => false,
    };
}

export fn zigr_bench_sexp_inspect(arg: SEXP) SEXP {
    const n = sexp.xlength(arg);
    var elts: [5]SEXP = undefined;
    for (0..n) |i| elts[i] = R.VECTOR_ELT(arg, @intCast(i));

    var per_elt: [5]i32 = undefined;
    for (0..n) |i| {
        const tag = sexp.typeTag(elts[i]);
        per_elt[i] = @as(i32, tag) + @as(i32, @intFromBool(isVectorTag(tag))) + @as(i32, @intFromBool(tag == 14));
    }

    var total: i32 = 0;
    for (0..10000) |_| {
        for (0..n) |i| total += per_elt[i];
    }
    return R.Rf_ScalarInteger(total);
}
