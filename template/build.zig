// zigr template build.zig
// Drop this into your R package root. It compiles Zig sources into a
// shared library that R's .Call() can load. No Makevars needed.
//
// Usage:
//   export R_HOME=/usr/lib/R && zig build
//   zig build -Dr-home=/path/to/R
//
// Cross-compile:
//   zig build -Dtarget=x86_64-linux-gnu
//   zig build -Dtarget=aarch64-macos-gnu
//   zig build -Dtarget=x86_64-windows-gnu

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // R headers live under $R_HOME/include. On Linux that's usually
    // /usr/lib/R, on macOS /Library/Frameworks/R.framework/Resources.
    // Accept either a build option or R_HOME from the environment.
    const r_home = b.option([]const u8, "r-home", "Path to R home directory") orelse
        (b.graph.environ_map.get("R_HOME") orelse
            @panic("pass -Dr-home=/path/to/R or set the R_HOME environment variable"));

    const r_include = b.pathJoin(&.{ r_home, "include" });
    const r_lib = b.pathJoin(&.{ r_home, "lib" });

    const dylib = b.addSharedLibrary(.{
        .name = "my_r_package",
        .root_source_file = b.path("src/zig/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    dylib.addIncludePath(.{ .cwd_relative = r_include });
    dylib.addLibraryPath(.{ .cwd_relative = r_lib });
    dylib.linkSystemLibrary("R");

    // R looks for the .so at src/<pkgname>.so inside the package dir.
    const install = b.addInstallArtifact(dylib, .{
        .dest_dir = .{ .override = .{ .custom = "src" } },
    });

    b.default_step.dependOn(&install.step);

    // Tests compile against R headers too (for SEXP definitions).
    const test_lib = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/zig/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    test_lib.addIncludePath(.{ .cwd_relative = r_include });

    const run_tests = b.addRunArtifact(test_lib);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_tests.step);
}
