//! Data frame creation and access.
//!
//! R data.frames are VECSXP with class "data.frame", row.names, and
//! column names stored as the names attribute on the list.

const std = @import("std");
const R = @import("R");
const protect = @import("protect.zig");
const xlength = @import("sexp.zig").xlength;
const sexp_mod = @import("sexp.zig");

/// Wraps an R data.frame with named column access.
pub const DataFrame = struct {
    sexp: R.SEXP,

    /// Wrap an existing SEXP as a DataFrame. Returns null if the
    /// SEXP is not a data frame.
    pub fn wrap(sexp: R.SEXP) ?DataFrame {
        if (!sexp_isDataFrame(sexp)) return null;
        return DataFrame{ .sexp = sexp };
    }

    /// Number of columns in the data frame.
    pub fn columnCount(self: DataFrame) i64 {
        return R.XLENGTH(self.sexp);
    }

    /// Number of rows. Returns 0 if the data frame has no columns
    /// or the first column is null.
    pub fn rowCount(self: DataFrame) i64 {
        const ncols = self.columnCount();
        if (ncols == 0) return 0;
        const col0 = R.VECTOR_ELT(self.sexp, 0);
        if (col0 == R.R_NilValue) return 0;
        return R.XLENGTH(col0);
    }

    pub fn columnNames(self: DataFrame, allocator: std.mem.Allocator) ![][]const u8 {
        const ns = R.Rf_getAttrib(self.sexp, R.R_NamesSymbol);
        if (ns == R.R_NilValue) return allocator.alloc([]const u8, 0);
        const nlen = R.XLENGTH(ns);
        const n = @as(usize, @intCast(if (nlen < 0) @as(R.R_xlen_t, 0) else nlen));
        const result = try allocator.alloc([]const u8, n);
        for (0..n) |i| {
            const elt = R.STRING_ELT(ns, @intCast(i));
            result[i] = if (elt == R.R_NaString) "" else sexp_mod.charsxpBytes(elt);
        }
        return result;
    }

    /// Use for repeated column access: call once, then columnByIndex in a loop.
    pub fn columnIndex(self: DataFrame, name: []const u8) ?i64 {
        const ncols = self.columnCount();
        const ns = R.Rf_getAttrib(self.sexp, R.R_NamesSymbol);
        if (ns == R.R_NilValue) return null;
        for (0..@as(usize, @intCast(ncols))) |i| {
            const elt = R.STRING_ELT(ns, @intCast(i));
            if (elt == R.R_NaString) continue;
            const cn = sexp_mod.charsxpBytes(elt);
            if (std.mem.eql(u8, cn, name)) return @intCast(i);
        }
        return null;
    }

    /// Faster than column() when the index is already known.
    pub fn columnByIndex(self: DataFrame, index: i64) R.SEXP {
        return R.VECTOR_ELT(self.sexp, @intCast(index));
    }

    /// NA column names are skipped. For repeated lookups, use columnIndex + columnByIndex.
    pub fn column(self: DataFrame, name: []const u8) ?R.SEXP {
        const idx = self.columnIndex(name) orelse return null;
        return self.columnByIndex(idx);
    }

    /// Caller owns the map and must deinit it.
    pub fn columnMap(self: DataFrame, allocator: std.mem.Allocator) !std.StringHashMap(i64) {
        var map = std.StringHashMap(i64).init(allocator);
        const ncols = self.columnCount();
        const ns = R.Rf_getAttrib(self.sexp, R.R_NamesSymbol);
        if (ns == R.R_NilValue) return map;
        for (0..@as(usize, @intCast(ncols))) |i| {
            const elt = R.STRING_ELT(ns, @intCast(i));
            if (elt == R.R_NaString) continue;
            const cn = sexp_mod.charsxpBytes(elt);
            try map.put(cn, @intCast(i));
        }
        return map;
    }
};

fn sexp_isDataFrame(sexp: R.SEXP) bool {
    if (sexp_mod.typeTag(sexp) != 19) return false;
    const cls = R.Rf_getAttrib(sexp, R.R_ClassSymbol);
    if (cls == R.R_NilValue) return false;
    const n = R.XLENGTH(cls);
    for (0..@as(usize, @intCast(n))) |i| {
        const elt = R.STRING_ELT(cls, @intCast(i));
        if (elt == R.R_NaString) continue;
        const cn = sexp_mod.charsxpBytes(elt);
        if (std.mem.eql(u8, cn, "data.frame")) return true;
    }
    return false;
}

/// Build a data frame from column names and SEXP columns.
///
/// **Safety**: Returns an unprotected SEXP. The caller MUST protect it immediately. R's GC collects unprotected SEXPs between this return and the caller's protect.
pub fn build(names: []const []const u8, columns: []const R.SEXP) R.SEXP {
    if (names.len == 0 or columns.len == 0) return R.R_NilValue;
    if (names.len != columns.len) return R.R_NilValue;
    const ncols: R.R_xlen_t = @intCast(names.len);

    var vec = protect.scoped(R.Rf_allocVector(R.VECSXP, ncols));
    var cnames = protect.scoped(R.Rf_allocVector(R.STRSXP, ncols));
    var cls = protect.scoped(R.Rf_allocVector(R.STRSXP, 1));
    var rn = protect.scoped(R.Rf_allocVector(R.INTSXP, 2));
    defer {
        rn.deinit();
        cls.deinit();
        cnames.deinit();
        vec.deinit();
    }

    for (0..@as(usize, @intCast(ncols))) |i| {
        _ = R.SET_VECTOR_ELT(vec.get(), @intCast(i), columns[i]);
    }

    for (0..@as(usize, @intCast(ncols))) |i| {
        const cs = R.Rf_mkCharLenCE(@ptrCast(names[i].ptr), @intCast(names[i].len), @as(R.cetype_t, @intCast(R.CE_UTF8)));
        R.SET_STRING_ELT(cnames.get(), @intCast(i), cs);
    }
    _ = R.Rf_namesgets(vec.get(), cnames.get());

    R.SET_STRING_ELT(cls.get(), 0, R.Rf_mkChar("data.frame"));
    _ = R.Rf_classgets(vec.get(), cls.get());

    const nrows = xlength(columns[0]);
    if (nrows <= std.math.maxInt(c_int)) {
        const rnp = R.INTEGER(rn.get());
        rnp[0] = R.R_NaInt;
        rnp[1] = -@as(c_int, @intCast(nrows));
        _ = R.Rf_setAttrib(vec.get(), R.R_RowNamesSymbol, rn.get());
    } else {
        _ = R.Rf_setAttrib(vec.get(), R.R_RowNamesSymbol, R.R_NilValue);
    }

    return vec.get();
}
