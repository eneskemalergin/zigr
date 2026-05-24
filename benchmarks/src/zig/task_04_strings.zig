const std = @import("std");
const R = @import("R");

const SEXP = R.SEXP;

export fn zigr_bench_strings(vec: SEXP, sep_sexp: SEXP) SEXP {
    const n = @as(usize, @intCast(R.XLENGTH(vec)));
    const sep = std.mem.sliceTo(R.R_CHAR(R.STRING_ELT(sep_sexp, 0)), 0);

    var total: usize = 0;
    for (0..n) |i| {
        const elt = R.STRING_ELT(vec, @intCast(i));
        if (elt != R.R_NaString) total += std.mem.len(R.R_CHAR(elt));
        total += sep.len;
    }

    const buf = R.R_alloc(total, 1);
    var pos: usize = 0;
    for (0..n) |i| {
        const elt = R.STRING_ELT(vec, @intCast(i));
        if (elt != R.R_NaString) {
            const s = std.mem.sliceTo(R.R_CHAR(elt), 0);
            @memcpy(buf[pos..][0..s.len], s);
            pos += s.len;
        }
        @memcpy(buf[pos..][0..sep.len], sep);
        pos += sep.len;
    }

    const out = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    defer R.Rf_unprotect(1);
    const cs = R.Rf_mkCharLenCE(buf, @intCast(pos), @as(R.cetype_t, @intCast(R.CE_UTF8)));
    R.SET_STRING_ELT(out, 0, cs);

    return out;
}
