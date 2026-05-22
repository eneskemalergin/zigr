// Cross-compilation check: verify R header translation works for the target.
// Imported by build.zig's `zig build check` step. Does not link against -lR.
// Unresolved R symbols are resolved at runtime when R loads the .so.
const R = @import("R");

comptime {
    _ = R.SEXP;
    _ = R.Rf_protect;
    _ = R.Rf_unprotect;
    _ = R.INTEGER;
    _ = R.REAL;
    _ = R.STRING_ELT;
    _ = R.Rf_allocVector;
    _ = R.R_NilValue;
    _ = R.R_xlen_t;
    _ = R.SEXPTYPE;
    _ = R.Rboolean;
    _ = R.Rf_isNull;
    _ = R.Rf_isReal;
    _ = R.Rf_isInteger;
    _ = R.Rf_isString;
    _ = R.Rf_length;
}
