#include <R.h>
#include <Rinternals.h>

SEXP c_call_bench_s4_slot_access(SEXP arg) {
    SEXP class_expr = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(class_expr, 0, Rf_mkChar("setClass(\"BenchS4\", representation(slot_x = \"numeric\"))"));
    SEXP parse_call = PROTECT(Rf_lang2(Rf_install("parse"), class_expr));
    int err = 0;
    SEXP parsed = R_tryEvalSilent(parse_call, R_GlobalEnv, &err);
    if (err == 0) R_tryEvalSilent(parsed, R_GlobalEnv, &err);
    UNPROTECT(2);

    SEXP new_call = PROTECT(Rf_lang2(Rf_install("new"), Rf_mkString("BenchS4")));
    err = 0;
    SEXP obj = PROTECT(R_tryEvalSilent(new_call, R_GlobalEnv, &err));
    UNPROTECT(1);
    if (err != 0) { UNPROTECT(0); return arg; }

    SEXP slot_sym = Rf_install("slot_x");
    R_do_slot_assign(obj, slot_sym, arg);
    SEXP result = R_do_slot(obj, slot_sym);
    UNPROTECT(1);
    return result;
}
