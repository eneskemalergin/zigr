const std = @import("std");
const R = @import("R");
const convert = @import("zigr").convert;

const SEXP = R.SEXP;
const na_text = "NA";

export fn zigr_bench_strings(vec: SEXP, sep_sexp: SEXP) SEXP {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const strings = convert.toCachedStringSliceView(arena.allocator(), vec) catch |err| convert.signalError(err);
    const sep_view = convert.toStringSliceView(sep_sexp) catch |err| convert.signalError(err);
    const sep = sep_view.at(0).bytes;
    const n = strings.len;

    var total: usize = 0;
    for (0..n) |i| {
        const value = strings.at(i);
        total += if (value.is_na) na_text.len else value.bytes.len;
    }
    if (n > 1) total += (n - 1) * sep.len;

    const buf = R.R_alloc(total, 1);
    var pos: usize = 0;
    for (0..n) |i| {
        const value = strings.at(i);
        const part = if (value.is_na) na_text else value.bytes;
        if (part.len > 0) {
            @memcpy(buf[pos..][0..part.len], part);
            pos += part.len;
        }
        if (i + 1 < n and sep.len > 0) {
            @memcpy(buf[pos..][0..sep.len], sep);
            pos += sep.len;
        }
    }

    const out = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    defer R.Rf_unprotect(1);
    const cs = R.Rf_mkCharLenCE(buf, @intCast(pos), @as(R.cetype_t, @intCast(R.CE_UTF8)));
    R.SET_STRING_ELT(out, 0, cs);

    return out;
}
