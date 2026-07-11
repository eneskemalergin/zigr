//! Cross-compilation entry point.
//!
//! R symbols remain unresolved until R loads the shared object.
comptime {
    _ = @import("root.zig");
}
