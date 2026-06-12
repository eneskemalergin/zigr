#include <R.h>
#include <Rinternals.h>
#include <string.h>

SEXP c_call_bench_string_concat(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    R_xlen_t total = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP elt = STRING_ELT(arg, i);
        total += (elt == NA_STRING) ? 2 : LENGTH(elt);
    }
    if (n > 1) total += (n - 1) * 2;

    char *buf = (char *)R_alloc(total, 1);
    char *pos = buf;
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP elt = STRING_ELT(arg, i);
        if (elt == NA_STRING) {
            memcpy(pos, "NA", 2); pos += 2;
        } else {
            R_xlen_t len = LENGTH(elt);
            memcpy(pos, CHAR(elt), len);
            pos += len;
        }
        if (i + 1 < n) { memcpy(pos, ", ", 2); pos += 2; }
    }

    SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
    SEXP cs = Rf_mkCharLenCE(buf, pos - buf, CE_UTF8);
    SET_STRING_ELT(out, 0, cs);
    UNPROTECT(1);
    return out;
}
