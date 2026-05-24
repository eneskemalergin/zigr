const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const r_include = if (b.option([]const u8, "r-include", "R include path")) |v| v else if (b.graph.environ_map.get("R_INCLUDE")) |ri| ri else if (b.graph.environ_map.get("R_HOME")) |rh| b.pathJoin(&.{ rh, "include" }) else @panic("pass -Dr-include=<path>, or set R_INCLUDE or R_HOME");
    const r_lib = if (b.option([]const u8, "r-lib", "R library path")) |v| v else if (b.graph.environ_map.get("R_LIB")) |rl| rl else if (b.graph.environ_map.get("R_HOME")) |rh| b.pathJoin(&.{ rh, "lib" }) else "/usr/lib/R/lib";

    const zigr_dep = b.dependency("zigr", .{
        .target = target,
        .optimize = optimize,
        .@"r-include" = r_include,
    });

    const pkg_name = b.option([]const u8, "pkg-name", "R package name") orelse "mypackage";

    const lib = b.addLibrary(.{
        .linkage = .dynamic,
        .name = pkg_name,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/zig/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "R", .module = zigr_dep.module("R") },
                .{ .name = "zigr", .module = zigr_dep.module("zigr") },
            },
        }),
    });
    lib.root_module.linkSystemLibrary("R", .{});
    lib.root_module.addLibraryPath(.{ .cwd_relative = r_lib });
    if (target.result.os.tag == .windows) lib.dll_export_fns = true;

    const so_suffix = switch (target.result.os.tag) {
        .windows => ".dll",
        else => ".so",
    };
    const install_name = b.fmt("{s}{s}", .{ pkg_name, so_suffix });
    const install = b.addInstallFileWithDir(lib.getEmittedBin(), .{ .custom = "src" }, install_name);
    b.getInstallStep().dependOn(&install.step);
}
