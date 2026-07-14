const std = @import("std");
const R = @import("R");
const sexp = @import("zigr").sexp;
const SEXP = R.SEXP;
const sep = ", ";

export fn zigr_bench_string_concat(vec: SEXP) SEXP {
    const n = sexp.xlength(vec);

    var total: usize = 0;
    var output_encoding: R.cetype_t = @intCast(R.CE_UTF8);
    for (0..n) |i| {
        const elt = sexp.fastVectorElt(vec, i);
        if (elt == R.R_NaString) {
            total += 2;
        } else {
            if (sexp.fastGetCharCE(elt) == R.CE_BYTES) output_encoding = @intCast(R.CE_BYTES);
            total += sexp.charsxpBytes(elt).len;
        }
    }
    if (n > 1) total += (n - 1) * sep.len;

    // This buffer outlives R allocations made while assembling the result.
    const buf = std.heap.c_allocator.alloc(u8, total) catch unreachable;
    defer std.heap.c_allocator.free(buf);

    var pos: usize = 0;
    for (0..n) |i| {
        const elt = sexp.fastVectorElt(vec, i);
        if (elt == R.R_NaString) {
            @memcpy(buf[pos..][0..2], "NA");
            pos += 2;
        } else {
            const bytes = sexp.charsxpBytes(elt);
            const len = bytes.len;
            if (len > 0) {
                @memcpy(buf[pos..][0..len], bytes);
                pos += len;
            }
        }
        if (i + 1 < n) {
            @memcpy(buf[pos..][0..sep.len], sep);
            pos += sep.len;
        }
    }

    const out = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    defer R.Rf_unprotect(1);
    const cs = R.Rf_mkCharLenCE(buf.ptr, @intCast(pos), output_encoding);
    R.SET_STRING_ELT(out, 0, cs);
    return out;
}
