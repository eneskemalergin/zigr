//! Cross-compilation check: verify the full zigr module graph compiles
//! for the target.  Imported by build.zig's `zig build check` step.
//! Does not link against -lR.  Unresolved R symbols are resolved at
//! runtime when R loads the .so.
comptime {
    _ = @import("root.zig");
}
