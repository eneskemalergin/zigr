//! Data frame creation and access.
//!
//! R data.frames are VECSXP with class "data.frame", row.names, and
//! column names stored as the names attribute on the list.

const std = @import("std");
const R = @import("R");

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

    /// Get column names as an allocated slice of strings.
    pub fn columnNames(self: DataFrame, allocator: std.mem.Allocator) ![][]const u8 {
        const ns = R.Rf_getAttrib(self.sexp, R.R_NamesSymbol);
        const n = @as(usize, @intCast(R.XLENGTH(ns)));
        const result = try allocator.alloc([]const u8, n);
        for (0..n) |i| {
            const elt = R.STRING_ELT(ns, @intCast(i));
            result[i] = if (elt == R.R_NaString) "" else std.mem.sliceTo(R.R_CHAR(elt), 0);
        }
        return result;
    }

    /// Get the numeric index of a column by name. Returns null if not
    /// found. Use this for repeated column lookups: call columnIndex once,
    /// then columnByIndex in a loop.
    pub fn columnIndex(self: DataFrame, name: []const u8) ?i64 {
        const ncols = self.columnCount();
        const ns = R.Rf_getAttrib(self.sexp, R.R_NamesSymbol);
        for (0..@as(usize, @intCast(ncols))) |i| {
            const elt = R.STRING_ELT(ns, @intCast(i));
            if (elt == R.R_NaString) continue;
            const cn = std.mem.sliceTo(R.R_CHAR(elt), 0);
            if (std.mem.eql(u8, cn, name)) return @intCast(i);
        }
        return null;
    }

    /// Get a column SEXP by its numeric index (zero-based).
    /// Faster than column() when the index was obtained from columnIndex().
    pub fn columnByIndex(self: DataFrame, index: i64) R.SEXP {
        return R.VECTOR_ELT(self.sexp, @intCast(index));
    }

    /// Look up a column by name. Returns null if the column does
    /// not exist. NA column names are skipped during lookup.
    /// For repeated lookups, use columnIndex + columnByIndex instead.
    pub fn column(self: DataFrame, name: []const u8) ?R.SEXP {
        const idx = self.columnIndex(name) orelse return null;
        return self.columnByIndex(idx);
    }

    /// Build a StringHashMap mapping column names to indices.
    /// Caller owns the hash map and must deinit it.
    /// Useful for O(1) lookups when accessing many columns.
    pub fn columnMap(self: DataFrame, allocator: std.mem.Allocator) !std.StringHashMap(i64) {
        var map = std.StringHashMap(i64).init(allocator);
        const ncols = self.columnCount();
        const ns = R.Rf_getAttrib(self.sexp, R.R_NamesSymbol);
        for (0..@as(usize, @intCast(ncols))) |i| {
            const elt = R.STRING_ELT(ns, @intCast(i));
            if (elt == R.R_NaString) continue;
            const cn = std.mem.sliceTo(R.R_CHAR(elt), 0);
            try map.put(cn, @intCast(i));
        }
        return map;
    }
};

fn sexp_isDataFrame(sexp: R.SEXP) bool {
    if (R.TYPEOF(sexp) != R.VECSXP) return false;
    const cls = R.Rf_getAttrib(sexp, R.R_ClassSymbol);
    if (cls == R.R_NilValue) return false;
    const n = R.XLENGTH(cls);
    for (0..@as(usize, @intCast(n))) |i| {
        const elt = R.STRING_ELT(cls, @intCast(i));
        if (elt == R.R_NaString) continue;
        const cn = std.mem.sliceTo(R.R_CHAR(elt), 0);
        if (std.mem.eql(u8, cn, "data.frame")) return true;
    }
    return false;
}

/// Build a data frame from column names and SEXP columns.
///
/// **Safety**: Returns an unprotected SEXP (standard R pattern). The caller
/// MUST protect it immediately: `protect(dataframe.build(names, cols))`.
/// Between this return and the caller's protect, the data frame could be
/// collected by R's GC.
pub fn build(names: []const []const u8, columns: []const R.SEXP) R.SEXP {
    if (names.len == 0 or columns.len == 0) return R.R_NilValue;
    const ncols: R.R_xlen_t = @intCast(names.len);

    // Batch protect: allocate all SEXPs, then set attributes
    const vec = R.Rf_protect(R.Rf_allocVector(R.VECSXP, ncols));
    const cnames = R.Rf_protect(R.Rf_allocVector(R.STRSXP, ncols));
    const cls = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
    const rn = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 2));
    defer R.Rf_unprotect(4);

    for (0..@as(usize, @intCast(ncols))) |i| {
        _ = R.SET_VECTOR_ELT(vec, @intCast(i), columns[i]);
    }

    for (0..@as(usize, @intCast(ncols))) |i| {
        const cs = R.Rf_mkCharLenCE(@ptrCast(names[i].ptr), @intCast(names[i].len), @as(R.cetype_t, @intCast(R.CE_UTF8)));
        R.SET_STRING_ELT(cnames, @intCast(i), cs);
    }
    _ = R.Rf_namesgets(vec, cnames);

    R.SET_STRING_ELT(cls, 0, R.Rf_mkChar("data.frame"));
    _ = R.Rf_classgets(vec, cls);

    const nrows: R.R_xlen_t = @intCast(R.XLENGTH(columns[0]));
    const rnp = R.INTEGER(rn);
    rnp[0] = R.R_NaInt;
    rnp[1] = @intCast(-nrows);
    _ = R.Rf_setAttrib(vec, R.R_RowNamesSymbol, rn);

    return vec;
}
