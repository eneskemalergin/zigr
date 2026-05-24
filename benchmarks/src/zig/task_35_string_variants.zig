const std = @import("std");
const R = @import("R");

const SEXP = R.SEXP;
const result_names = [_][:0]const u8{ "concat", "nchar_sum", "prefix_match", "extract_substr", "to_upper" };
const comma = ",";

fn setListNames(result: SEXP) void {
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, result_names.len));
    defer R.Rf_unprotect(1);

    for (result_names, 0..) |name, index| {
        R.SET_STRING_ELT(names, @intCast(index), R.Rf_mkChar(name));
    }
    _ = R.Rf_setAttrib(result, R.R_NamesSymbol, names);
}

fn hasPrefixAbc(s: []const u8) bool {
    return s.len >= 3 and std.mem.eql(u8, s[0..3], "abc");
}

export fn zigr_bench_string_variants(vec: SEXP) SEXP {
    const n = @as(usize, @intCast(R.XLENGTH(vec)));
    var concat_len: usize = 0;
    var valid_count: usize = 0;
    var nchar_sum: i64 = 0;
    var prefix_match: i64 = 0;

    for (0..n) |index| {
        const elt = R.STRING_ELT(vec, @intCast(index));
        if (elt == R.R_NaString) continue;
        const s = std.mem.sliceTo(R.R_CHAR(elt), 0);
        concat_len += s.len;
        valid_count += 1;
        nchar_sum += @as(i64, @intCast(s.len));
        if (hasPrefixAbc(s)) prefix_match += 1;
    }
    if (valid_count > 0) concat_len += valid_count - 1;

    const result = R.Rf_protect(R.Rf_allocVector(R.VECSXP, result_names.len));
    defer R.Rf_unprotect(7);
    const concat = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    const nchar_value = R.Rf_protect(R.Rf_ScalarInteger(@intCast(nchar_sum)));
    const prefix_value = R.Rf_protect(R.Rf_ScalarInteger(@intCast(prefix_match)));
    const extract = R.Rf_protect(R.Rf_allocVector(R.STRSXP, @intCast(n)));
    const upper = R.Rf_protect(R.Rf_allocVector(R.STRSXP, @intCast(n)));
    const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, result_names.len));

    if (concat_len == 0) {
        R.SET_STRING_ELT(concat, 0, R.Rf_mkChar(""));
    } else {
        const buf = R.R_alloc(concat_len + 1, 1);
        var pos: usize = 0;
        var added: usize = 0;
        for (0..n) |index| {
            const elt = R.STRING_ELT(vec, @intCast(index));
            if (elt == R.R_NaString) continue;
            const s = std.mem.sliceTo(R.R_CHAR(elt), 0);
            if (added > 0) {
                buf[pos] = ',';
                pos += 1;
            }
            @memcpy(buf[pos..][0..s.len], s);
            pos += s.len;
            added += 1;
        }
        buf[concat_len] = 0;
        R.SET_STRING_ELT(concat, 0, R.Rf_mkCharLenCE(buf, @intCast(concat_len), @as(R.cetype_t, @intCast(R.CE_UTF8))));
    }

    for (0..n) |index| {
        const elt = R.STRING_ELT(vec, @intCast(index));
        if (elt == R.R_NaString) {
            R.SET_STRING_ELT(extract, @intCast(index), R.R_NaString);
            R.SET_STRING_ELT(upper, @intCast(index), R.R_NaString);
            continue;
        }

        const s = std.mem.sliceTo(R.R_CHAR(elt), 0);
        const sub_len = @min(s.len, 3);
        if (sub_len == 0) {
            R.SET_STRING_ELT(extract, @intCast(index), R.Rf_mkChar(""));
        } else {
            R.SET_STRING_ELT(extract, @intCast(index), R.Rf_mkCharLenCE(s.ptr, @intCast(sub_len), @as(R.cetype_t, @intCast(R.CE_UTF8))));
        }

        if (s.len == 0) {
            R.SET_STRING_ELT(upper, @intCast(index), R.Rf_mkChar(""));
        } else {
            const upper_buf = R.R_alloc(s.len + 1, 1);
            for (s, 0..) |byte, offset| upper_buf[offset] = std.ascii.toUpper(byte);
            upper_buf[s.len] = 0;
            R.SET_STRING_ELT(upper, @intCast(index), R.Rf_mkCharLenCE(upper_buf, @intCast(s.len), @as(R.cetype_t, @intCast(R.CE_UTF8))));
        }
    }

    for (result_names, 0..) |name, index| {
        R.SET_STRING_ELT(names, @intCast(index), R.Rf_mkChar(name));
    }
    _ = R.SET_VECTOR_ELT(result, 0, concat);
    _ = R.SET_VECTOR_ELT(result, 1, nchar_value);
    _ = R.SET_VECTOR_ELT(result, 2, prefix_value);
    _ = R.SET_VECTOR_ELT(result, 3, extract);
    _ = R.SET_VECTOR_ELT(result, 4, upper);
    _ = R.Rf_setAttrib(result, R.R_NamesSymbol, names);
    return result;
}
