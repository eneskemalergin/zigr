//! Data frame helpers.

const std = @import("std");
const R = @import("R");
const cleanup = @import("cleanup");
const protect = @import("protect.zig");
const xlength = @import("sexp.zig").xlength;
const sexp_mod = @import("sexp.zig");

pub const DataFrameError = error{
    NameColumnCount,
    NullColumn,
    ColumnLength,
    RowCountTooLarge,
    InvalidName,
    MalformedNames,
};

const HeaderCleanup = struct {
    allocator: std.mem.Allocator,
    values: [][]const u8,

    fn fire(self: *@This()) void {
        self.allocator.free(self.values);
    }
};

const MapCleanup = struct {
    map: std.StringHashMap(i64),

    fn fire(self: *@This()) void {
        self.map.deinit();
    }
};

pub const DataFrame = struct {
    sexp: R.SEXP,

    pub fn wrap(sexp: R.SEXP) ?DataFrame {
        if (!sexp_isDataFrame(sexp)) return null;
        return DataFrame{ .sexp = sexp };
    }

    pub fn columnCount(self: DataFrame) i64 {
        return R.XLENGTH(self.sexp);
    }

    pub fn rowCount(self: DataFrame) i64 {
        const ncols = self.columnCount();
        if (ncols == 0) return 0;
        const col0 = R.VECTOR_ELT(self.sexp, 0);
        if (col0 == R.R_NilValue) return 0;
        return R.XLENGTH(col0);
    }

    /// Headers are caller-owned; bytes borrow from the data frame.
    pub fn columnNames(self: DataFrame, allocator: std.mem.Allocator) (DataFrameError || std.mem.Allocator.Error)![][]const u8 {
        const ns = R.Rf_getAttrib(self.sexp, R.R_NamesSymbol);
        if (ns == R.R_NilValue) return allocator.alloc([]const u8, 0);
        if (R.TYPEOF(ns) != R.STRSXP or R.XLENGTH(ns) != self.columnCount()) return error.MalformedNames;
        const n = @as(usize, @intCast(R.XLENGTH(ns)));
        const result = try allocator.alloc([]const u8, n);
        _ = cleanup.pushFrameInline(HeaderCleanup, .{ .allocator = allocator, .values = result }, HeaderCleanup.fire);
        defer cleanup.popFrame();
        for (0..n) |i| {
            const elt = R.STRING_ELT(ns, @intCast(i));
            result[i] = if (elt == R.R_NaString) "" else sexp_mod.charsxpBytes(elt);
        }
        return result;
    }

    pub fn columnIndex(self: DataFrame, name: []const u8) ?i64 {
        const ncols = self.columnCount();
        const ns = R.Rf_getAttrib(self.sexp, R.R_NamesSymbol);
        if (ns == R.R_NilValue or R.TYPEOF(ns) != R.STRSXP or R.XLENGTH(ns) != ncols) return null;
        for (0..@as(usize, @intCast(ncols))) |i| {
            const elt = R.STRING_ELT(ns, @intCast(i));
            if (elt == R.R_NaString) continue;
            const cn = sexp_mod.charsxpBytes(elt);
            if (std.mem.eql(u8, cn, name)) return @intCast(i);
        }
        return null;
    }

    pub fn columnAt(self: DataFrame, index: usize) ?R.SEXP {
        if (index >= @as(usize, @intCast(self.columnCount()))) return null;
        return R.VECTOR_ELT(self.sexp, @intCast(index));
    }

    /// Caller assumes `index` is non-negative and in bounds.
    pub fn columnByIndex(self: DataFrame, index: i64) R.SEXP {
        return R.VECTOR_ELT(self.sexp, @intCast(index));
    }

    pub fn column(self: DataFrame, name: []const u8) ?R.SEXP {
        const idx = self.columnIndex(name) orelse return null;
        return self.columnByIndex(idx);
    }

    /// Map keys borrow R string storage and cannot outlive the data frame.
    pub fn columnMap(self: DataFrame, allocator: std.mem.Allocator) !std.StringHashMap(i64) {
        var map = std.StringHashMap(i64).init(allocator);
        errdefer map.deinit();
        const ncols = self.columnCount();
        const ns = R.Rf_getAttrib(self.sexp, R.R_NamesSymbol);
        if (ns == R.R_NilValue or R.TYPEOF(ns) != R.STRSXP or R.XLENGTH(ns) != ncols) return map;
        try map.ensureTotalCapacity(@intCast(ncols));
        _ = cleanup.pushFrameInline(MapCleanup, .{ .map = map }, MapCleanup.fire);
        defer cleanup.popFrame();
        for (0..@as(usize, @intCast(ncols))) |i| {
            const elt = R.STRING_ELT(ns, @intCast(i));
            if (elt == R.R_NaString) continue;
            const cn = sexp_mod.charsxpBytes(elt);
            map.putAssumeCapacity(cn, @intCast(i));
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

/// Shares `columns`; callers keep them reachable during construction. The result is unprotected.
pub fn buildChecked(names: []const []const u8, columns: []const R.SEXP) DataFrameError!R.SEXP {
    if (names.len != columns.len) return error.NameColumnCount;
    for (names) |name| {
        if (std.mem.indexOfScalar(u8, name, 0) != null) return error.InvalidName;
    }

    const nrows: usize = if (columns.len == 0) 0 else blk: {
        if (columns[0] == null) return error.NullColumn;
        break :blk xlength(columns[0]);
    };
    if (nrows > std.math.maxInt(c_int)) return error.RowCountTooLarge;
    for (columns) |column| {
        if (column == null) return error.NullColumn;
        if (xlength(column) != nrows) return error.ColumnLength;
    }

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

    const rnp = R.INTEGER(rn.get());
    rnp[0] = R.R_NaInt;
    rnp[1] = -@as(c_int, @intCast(nrows));
    _ = R.Rf_setAttrib(vec.get(), R.R_RowNamesSymbol, rn.get());

    return vec.get();
}

pub fn build(names: []const []const u8, columns: []const R.SEXP) R.SEXP {
    return buildChecked(names, columns) catch R.R_NilValue;
}
