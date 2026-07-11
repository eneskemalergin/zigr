const R = @import("R");
const protect = @import("zigr").protect;

const SEXP = R.SEXP;

export fn zigr_bench_protect_scaling(vec: SEXP) SEXP {
    for (0..100) |_| {
        for (0..10) |_| {
            for (0..10000) |_| _ = protect.protect(vec);
            protect.unprotectN(10000);
        }
    }
    return R.Rf_ScalarInteger(0);
}
