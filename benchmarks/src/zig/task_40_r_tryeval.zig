const R = @import("R");
const zigr = @import("zigr");
const zigr_eval = zigr.eval;
const lang = zigr.lang;
const protect = zigr.protect;

export fn zigr_bench_r_tryeval(_: R.SEXP) R.SEXP {
    var count: c_int = 0;
    for (0..512) |_| {
        var message = protect.scoped(R.Rf_mkString("task40"));
        defer message.deinit();
        var call = protect.scoped(lang.buildNamedCall("stop", .{message.get()}));
        defer call.deinit();
        if (zigr_eval.tryEvalSilent(call.get(), R.R_GlobalEnv) == null) count += 1;
    }
    return R.Rf_ScalarInteger(count);
}
