#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_s4_slot_access(SEXP arg) {
    int err = 0;
    SEXP new_call = PROTECT(Rf_lang2(Rf_install("new"), Rf_mkString("BenchS4")));
    SEXP obj = PROTECT(R_tryEvalSilent(new_call, R_GlobalEnv, &err));
    UNPROTECT(1);
    if (err != 0) { UNPROTECT(1); return arg; }

    SEXP slot_sym = Rf_install("slot_x");
    R_do_slot_assign(obj, slot_sym, arg);
    SEXP result = R_do_slot(obj, slot_sym);
    UNPROTECT(1);
    return result;
}
