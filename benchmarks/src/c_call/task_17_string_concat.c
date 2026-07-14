#include <R.h>
#include <Rinternals.h>
#include <string.h>

static const char *string_bytes(SEXP value) {
    return getCharCE(value) == CE_BYTES ? CHAR(value) : Rf_translateCharUTF8(value);
}

SEXP c_call_bench_string_concat(SEXP arg) {
    R_xlen_t n = XLENGTH(arg);
    R_xlen_t total = 0;
    int output_encoding = CE_UTF8;
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP elt = STRING_ELT(arg, i);
        if (elt == NA_STRING) {
            total += 2;
        } else {
            if (getCharCE(elt) == CE_BYTES) output_encoding = CE_BYTES;
            total += (R_xlen_t) strlen(string_bytes(elt));
        }
    }
    if (n > 1) total += (n - 1) * 2;

    char *buf = (char *)R_alloc(total, 1);
    char *pos = buf;
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP elt = STRING_ELT(arg, i);
        if (elt == NA_STRING) {
            memcpy(pos, "NA", 2); pos += 2;
        } else {
            const char *bytes = string_bytes(elt);
            R_xlen_t len = (R_xlen_t) strlen(bytes);
            memcpy(pos, bytes, len);
            pos += len;
        }
        if (i + 1 < n) { memcpy(pos, ", ", 2); pos += 2; }
    }

    SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
    SEXP cs = Rf_mkCharLenCE(buf, pos - buf, (cetype_t) output_encoding);
    SET_STRING_ELT(out, 0, cs);
    UNPROTECT(1);
    return out;
}
