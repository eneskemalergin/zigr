#include <R.h>
#include <Rinternals.h>

#define REPEATS 512

static double adjusted_sum(const double *x, int n, double bias) {
    double total = 0.0;
    for (int i = 0; i < n; i++) total += x[i] + bias;
    return total;
}

static void fill_adjusted(double *out, const double *x, int n, double bias) {
    for (int i = 0; i < n; i++) out[i] = x[i] + bias;
}

typedef struct {
    const double *x;
    int n;
    double bias;
} unwind_data_t;

static SEXP unwind_callback(void *data) {
    unwind_data_t *ud = (unwind_data_t *)data;
    return Rf_ScalarReal(adjusted_sum(ud->x, ud->n, ud->bias));
}

static void unwind_noop(void *data, Rboolean jump) {}

SEXP c_call_bench_longjmp_safety(SEXP arg) {
    int n = LENGTH(arg);
    const double *x = REAL(arg);

    double direct_total = 0.0, try_ok_total = 0.0;
    double try_err_total = 0.0, unwind_ok_total = 0.0;

    SEXP sum_sym = Rf_install("sum");
    SEXP stop_sym = Rf_install("stop");

    for (int rep = 0; rep < REPEATS; rep++) {
        double bias = (rep + 1.0) * 0.001;
        direct_total += adjusted_sum(x, n, bias);

        SEXP tmp = PROTECT(Rf_allocVector(REALSXP, n));
        fill_adjusted(REAL(tmp), x, n, bias);
        SEXP expr = PROTECT(Rf_lang2(sum_sym, tmp));
        int err = 0;
        SEXP res = R_tryEvalSilent(expr, R_GlobalEnv, &err);
        if (err == 0) try_ok_total += REAL(res)[0];
        UNPROTECT(2);

        err = 0;
        SEXP stop_call = PROTECT(Rf_lang2(stop_sym, Rf_mkString("task32")));
        R_tryEvalSilent(stop_call, R_GlobalEnv, &err);
        UNPROTECT(1);
        if (err != 0) try_err_total += 1.0;

        unwind_data_t ud = { .x = x, .n = n, .bias = bias };
        SEXP cont = PROTECT(R_MakeUnwindCont());
        SEXP ures = R_UnwindProtect(unwind_callback, &ud, unwind_noop, NULL, cont);
        unwind_ok_total += REAL(ures)[0];
        UNPROTECT(1);
    }

    const char *names[] = {"direct", "try_ok", "try_err", "unwind_ok"};
    int nstrat = 4;
    SEXP out = PROTECT(Rf_allocVector(REALSXP, nstrat));
    SEXP nms = PROTECT(Rf_allocVector(STRSXP, nstrat));
    double *outd = REAL(out);
    outd[0] = direct_total;
    outd[1] = try_ok_total;
    outd[2] = try_err_total;
    outd[3] = unwind_ok_total;
    for (int i = 0; i < nstrat; i++)
        SET_STRING_ELT(nms, i, Rf_mkChar(names[i]));
    Rf_setAttrib(out, R_NamesSymbol, nms);
    UNPROTECT(2);
    return out;
}
