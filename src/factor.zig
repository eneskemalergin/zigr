//! Factor construction.
//!
//! ALTREP input uses R element access because its backing pointer may be absent.

const std = @import("std");
const R = @import("R");
const sexp = @import("sexp.zig");

const max_levels = 1024;

/// Uses R's default alphabetical level order.
pub fn asFactor(vec: R.SEXP) R.SEXP {
    if (vec == null) return R.R_NilValue;
    if (sexp.typeTag(vec) != 16) return R.R_NilValue; // STRSXP = 16

    const is_alt = R.ALTREP(vec) != 0;
    const n_raw: R.R_xlen_t = if (is_alt) R.XLENGTH(vec) else sexp.fastLength(vec);
    if (n_raw < 0) return R.R_NilValue;
    const n = @as(usize, @intCast(n_raw));

    const getElt = struct {
        fn at(v: R.SEXP, i: usize, alt: bool) R.SEXP {
            return if (alt) R.STRING_ELT(v, @intCast(i)) else sexp.fastVectorElt(v, @intCast(i));
        }
    }.at;

    var level_ptrs: [max_levels]R.SEXP = undefined;
    var level_count: u32 = 0;

    for (0..n) |i| {
        const elt = getElt(vec, i, is_alt);
        if (elt == null or elt == R.R_NaString) continue;

        // R interns CHARSXPs, so pointer equality is valid.
        var found = false;
        for (0..level_count) |j| {
            if (level_ptrs[j] == elt) {
                found = true;
                break;
            }
        }
        if (!found) {
            if (level_count >= max_levels) return R.R_NilValue;
            level_ptrs[level_count] = elt;
            level_count += 1;
        }
    }

    if (level_count > 1) {
        var i: usize = 0;
        while (i < level_count - 1) {
            var swapped = false;
            var j: usize = 0;
            while (j < level_count - 1 - i) {
                const c1 = level_ptrs[j];
                const c2 = level_ptrs[j + 1];
                const a_slice = sexp.charsxpBytes(c1);
                const b_slice = sexp.charsxpBytes(c2);
                const order = std.mem.order(u8, a_slice, b_slice);
                if (order == .gt) {
                    level_ptrs[j] = c2;
                    level_ptrs[j + 1] = c1;
                    swapped = true;
                }
                j += 1;
            }
            if (!swapped) break;
            i += 1;
        }
    }

    const codes = R.Rf_protect(R.Rf_allocVector(R.INTSXP, @intCast(n)));
    defer R.Rf_unprotect(1);
    const codes_ptr: [*]c_int = @ptrCast(R.INTEGER(codes));

    for (0..n) |i| {
        const elt = getElt(vec, i, is_alt);
        if (elt == null or elt == R.R_NaString) {
            codes_ptr[i] = R.R_NaInt;
            continue;
        }
        var code: c_int = R.R_NaInt;
        for (0..level_count) |j| {
            if (level_ptrs[j] == elt) {
                code = @as(c_int, @intCast(j + 1));
                break;
            }
        }
        codes_ptr[i] = code;
    }

    const class = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    R.SET_STRING_ELT(class, 0, R.Rf_mkChar("factor"));
    _ = R.Rf_setAttrib(codes, R.R_ClassSymbol, class);
    R.Rf_unprotect(1);

    const lvls = R.Rf_protect(R.Rf_allocVector(R.STRSXP, @intCast(level_count)));
    for (0..level_count) |j| {
        R.SET_STRING_ELT(lvls, @intCast(j), level_ptrs[j]);
    }
    _ = R.Rf_setAttrib(codes, R.R_LevelsSymbol, lvls);
    R.Rf_unprotect(1);

    return codes;
}
