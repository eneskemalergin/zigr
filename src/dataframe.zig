//! Checked data-frame construction and borrowed column access.
//!
//! Construction shares caller-owned columns, can allocate and longjmp, and
//! returns an unprotected frame. It reads column length and the first dimension
//! element but never requests column payload storage. Callers keep columns
//! reachable across the call. Name extraction iterates ALTSTRING elements.

const std = @import("std");
const R = @import("R");
const cleanup = @import("cleanup");
const r_error = @import("error");
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
    values: ?[][]const u8 = null,

    fn fire(self: *@This()) void {
        if (self.values) |values| self.allocator.free(values);
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
        return R.Rf_nrows(col0);
    }

    /// Headers are caller-owned; bytes borrow from the data frame.
    pub fn columnNames(self: DataFrame, allocator: std.mem.Allocator) (DataFrameError || std.mem.Allocator.Error)![][]const u8 {
        const ns = R.Rf_getAttrib(self.sexp, R.R_NamesSymbol);
        if (ns == R.R_NilValue) return allocator.alloc([]const u8, 0);
        if (R.TYPEOF(ns) != R.STRSXP or R.XLENGTH(ns) != self.columnCount()) return error.MalformedNames;
        const n = @as(usize, @intCast(R.XLENGTH(ns)));
        const state = cleanup.pushFrameInline(HeaderCleanup, .{ .allocator = allocator }, HeaderCleanup.fire);
        errdefer cleanup.popFrame();
        const result = try allocator.alloc([]const u8, n);
        state.values = result;
        for (0..n) |i| {
            const elt = R.STRING_ELT(ns, @intCast(i));
            result[i] = if (elt == R.R_NaString) "" else sexp_mod.charsxpBytes(elt);
        }
        cleanup.popFrame();
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
        const ncols = self.columnCount();
        const ns = R.Rf_getAttrib(self.sexp, R.R_NamesSymbol);
        if (ns == R.R_NilValue or R.TYPEOF(ns) != R.STRSXP or R.XLENGTH(ns) != ncols) {
            return std.StringHashMap(i64).init(allocator);
        }
        const state = cleanup.pushFrameInline(MapCleanup, .{ .map = std.StringHashMap(i64).init(allocator) }, MapCleanup.fire);
        errdefer {
            state.map.deinit();
            cleanup.popFrame();
        }
        try state.map.ensureTotalCapacity(@intCast(ncols));
        for (0..@as(usize, @intCast(ncols))) |i| {
            const elt = R.STRING_ELT(ns, @intCast(i));
            if (elt == R.R_NaString) continue;
            const cn = sexp_mod.charsxpBytes(elt);
            state.map.putAssumeCapacity(cn, @intCast(i));
        }
        const result = state.map;
        cleanup.popFrame();
        return result;
    }
};

fn sexp_isDataFrame(sexp: R.SEXP) bool {
    if (sexp_mod.typeTag(sexp) != 19) return false;
    const cls = R.Rf_getAttrib(sexp, R.R_ClassSymbol);
    if (cls == R.R_NilValue or R.TYPEOF(cls) != R.STRSXP) return false;
    const n = R.XLENGTH(cls);
    for (0..@as(usize, @intCast(n))) |i| {
        const elt = R.STRING_ELT(cls, @intCast(i));
        if (elt == R.R_NaString) continue;
        const cn = sexp_mod.charsxpBytes(elt);
        if (std.mem.eql(u8, cn, "data.frame")) return true;
    }
    return false;
}

fn columnRowCount(column: R.SEXP) DataFrameError!usize {
    if (column == null) return error.NullColumn;
    const dim = R.Rf_getAttrib(column, R.R_DimSymbol);
    if (dim != R.R_NilValue) {
        if (R.TYPEOF(dim) != R.INTSXP or R.XLENGTH(dim) < 1) return error.ColumnLength;
        const rows = R.INTEGER_ELT(dim, 0);
        if (rows < 0) return error.ColumnLength;
        return @intCast(rows);
    }
    const rows = xlength(column);
    if (rows > std.math.maxInt(c_int)) return error.RowCountTooLarge;
    return rows;
}

const BuildRequest = struct {
    names: []const []const u8,
    columns: []const R.SEXP,
    nrows: usize,
};

fn buildCall(data: ?*anyopaque) R.SEXP {
    const request: *BuildRequest = @ptrCast(@alignCast(data.?));
    const ncols: R.R_xlen_t = @intCast(request.names.len);

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
        _ = R.SET_VECTOR_ELT(vec.get(), @intCast(i), request.columns[i]);
    }

    for (0..@as(usize, @intCast(ncols))) |i| {
        const cs = R.Rf_mkCharLenCE(@ptrCast(request.names[i].ptr), @intCast(request.names[i].len), @as(R.cetype_t, @intCast(R.CE_UTF8)));
        R.SET_STRING_ELT(cnames.get(), @intCast(i), cs);
    }
    _ = R.Rf_namesgets(vec.get(), cnames.get());

    R.SET_STRING_ELT(cls.get(), 0, R.Rf_mkChar("data.frame"));
    _ = R.Rf_classgets(vec.get(), cls.get());

    const rnp = R.INTEGER(rn.get());
    rnp[0] = R.R_NaInt;
    rnp[1] = -@as(c_int, @intCast(request.nrows));
    _ = R.Rf_setAttrib(vec.get(), R.R_RowNamesSymbol, rn.get());

    return vec.get();
}

/// Shares `columns`; callers keep them reachable during construction. The result is unprotected.
pub fn buildChecked(names: []const []const u8, columns: []const R.SEXP) DataFrameError!R.SEXP {
    if (names.len != columns.len) return error.NameColumnCount;
    for (names) |name| {
        if (name.len == 0 or name.len > std.math.maxInt(c_int) or std.mem.indexOfScalar(u8, name, 0) != null) return error.InvalidName;
    }

    const nrows = if (columns.len == 0) 0 else try columnRowCount(columns[0]);
    for (columns) |column| {
        if (try columnRowCount(column) != nrows) return error.ColumnLength;
    }

    var request = BuildRequest{ .names = names, .columns = columns, .nrows = nrows };
    return cleanup.protectCallData(buildCall, @ptrCast(&request));
}

fn buildErrorMessage(build_error: DataFrameError) []const u8 {
    return switch (build_error) {
        error.NameColumnCount => "data-frame names and columns must have the same length",
        error.NullColumn => "data-frame column is null",
        error.ColumnLength => "data-frame columns must have equal row counts",
        error.RowCountTooLarge => "data-frame row count exceeds R integer limits",
        error.InvalidName => "data-frame column name is invalid",
        error.MalformedNames => "data-frame names are malformed",
    };
}

/// Builds a data frame or raises an R error when validation fails.
pub fn build(names: []const []const u8, columns: []const R.SEXP) R.SEXP {
    return buildChecked(names, columns) catch |build_error| r_error.signal(buildErrorMessage(build_error));
}
