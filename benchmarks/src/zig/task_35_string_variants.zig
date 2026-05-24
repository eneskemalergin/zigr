const std = @import("std");
const R = @import("R");
const convert = @import("convert");

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
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const strings = convert.toCachedStringSliceView(arena.allocator(), vec) catch |err| convert.signalError(err);
    const n = strings.len;
    var concat_len: usize = 0;
    var valid_count: usize = 0;
    var nchar_sum: i64 = 0;
    var prefix_match: i64 = 0;

    for (0..n) |index| {
        const value = strings.at(index);
        if (value.is_na) continue;
        const s = value.bytes;
        concat_len += value.len;
        valid_count += 1;
        nchar_sum += @as(i64, @intCast(value.len));
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
            const value = strings.at(index);
            if (value.is_na) continue;
            const s = value.bytes;
            if (added > 0) {
                buf[pos] = ',';
                pos += 1;
            }
            @memcpy(buf[pos..][0..value.len], s);
            pos += value.len;
            added += 1;
        }
        buf[concat_len] = 0;
        R.SET_STRING_ELT(concat, 0, R.Rf_mkCharLenCE(buf, @intCast(concat_len), @as(R.cetype_t, @intCast(R.CE_UTF8))));
    }

    for (0..n) |index| {
        const value = strings.at(index);
        if (value.is_na) {
            R.SET_STRING_ELT(extract, @intCast(index), R.R_NaString);
            R.SET_STRING_ELT(upper, @intCast(index), R.R_NaString);
            continue;
        }

        const s = value.bytes;
        const sub_len = @min(value.len, 3);
        if (sub_len == 0) {
            R.SET_STRING_ELT(extract, @intCast(index), R.Rf_mkChar(""));
        } else {
            R.SET_STRING_ELT(extract, @intCast(index), R.Rf_mkCharLenCE(s.ptr, @intCast(sub_len), @as(R.cetype_t, @intCast(R.CE_UTF8))));
        }

        if (value.len == 0) {
            R.SET_STRING_ELT(upper, @intCast(index), R.Rf_mkChar(""));
        } else {
            const upper_buf = R.R_alloc(value.len + 1, 1);
            for (s, 0..) |byte, offset| upper_buf[offset] = std.ascii.toUpper(byte);
            upper_buf[value.len] = 0;
            R.SET_STRING_ELT(upper, @intCast(index), R.Rf_mkCharLenCE(upper_buf, @intCast(value.len), @as(R.cetype_t, @intCast(R.CE_UTF8))));
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
