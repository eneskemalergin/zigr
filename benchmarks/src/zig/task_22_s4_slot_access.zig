const std = @import("std");
const R = @import("R");
const s4 = @import("zigr").s4;

var class_defined = std.atomic.Value(bool).init(false);

export fn zigr_bench_s4_slot_access(vec: R.SEXP) R.SEXP {
    if (!class_defined.load(.monotonic)) {
        const class_name = R.Rf_protect(R.Rf_mkString("BenchS4"));
        const slot_type = R.Rf_protect(R.Rf_mkString("numeric"));
        const rep_call = R.Rf_protect(R.Rf_lang2(R.Rf_install("representation"), slot_type));
        R.SET_TAG(R.CDR(rep_call), R.Rf_install("slot_x"));
        const sc_call = R.Rf_protect(R.Rf_lang3(R.Rf_install("setClass"), class_name, rep_call));
        var err: c_int = 0;
        _ = R.R_tryEvalSilent(sc_call, R.R_GlobalEnv, &err);
        R.Rf_unprotect(4);
        if (err != 0) return vec;
        class_defined.store(true, .monotonic);
    }

    const obj = R.Rf_protect(s4.newObject("BenchS4"));
    const assigned = R.Rf_protect(s4.setSlot(obj, "slot_x", vec));
    const result = s4.getSlot(assigned, "slot_x");
    R.Rf_unprotect(2);
    return result;
}
