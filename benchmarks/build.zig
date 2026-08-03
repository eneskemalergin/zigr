const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const enable_direct_sexp = b.option(bool, "direct-sexp", "Enable the private R 4.6 x86_64 SEXP layout") orelse false;
    const force_checked_sexp = b.option(bool, "checked-sexp", "Force checked R API SEXP access") orelse false;

    const build_options = b.addOptions();
    build_options.addOption(bool, "enable_direct_sexp", enable_direct_sexp);
    build_options.addOption(bool, "force_checked_sexp", force_checked_sexp);

    const r_include = blk: {
        const opt = b.option([]const u8, "r-include", "Path to R header directory");
        if (opt) |p| break :blk p;
        if (b.graph.environ_map.get("R_INCLUDE")) |env| break :blk env;
        if (b.graph.environ_map.get("R_HOME")) |rh| break :blk b.pathJoin(&.{ rh, "include" });
        @panic("pass -Dr-include=<path>, or set R_INCLUDE or R_HOME");
    };

    const r_lib = blk: {
        const opt = b.option([]const u8, "r-lib", "Path to R library directory");
        if (opt) |p| break :blk p;
        if (b.graph.environ_map.get("R_LIB")) |env| break :blk env;
        if (b.graph.environ_map.get("R_HOME")) |rh| break :blk b.pathJoin(&.{ rh, "lib" });
        break :blk "/usr/lib/R/lib";
    };

    const r_headers = b.addTranslateC(.{
        .root_source_file = b.path("../src/r_imports.h"),
        .target = target,
        .optimize = optimize,
    });
    r_headers.addIncludePath(.{ .cwd_relative = r_include });
    const r_mod = r_headers.addModule("R");

    const simd_mod = b.addModule("simd", .{
        .root_source_file = b.path("../src/simd.zig"),
    });
    const err_mod = b.addModule("error", .{
        .root_source_file = b.path("../src/error.zig"),
        .imports = &.{.{ .name = "R", .module = r_mod }},
    });
    const cleanup_mod = b.addModule("cleanup", .{
        .root_source_file = b.path("../src/cleanup.zig"),
        .imports = &.{
            .{ .name = "R", .module = r_mod },
            .{ .name = "error", .module = err_mod },
        },
    });
    const zigr_mod = b.addModule("zigr", .{
        .root_source_file = b.path("../src/root.zig"),
        .imports = &.{
            .{ .name = "R", .module = r_mod },
            .{ .name = "cleanup", .module = cleanup_mod },
            .{ .name = "error", .module = err_mod },
            .{ .name = "simd", .module = simd_mod },
            .{ .name = "build_options", .module = build_options.createModule() },
        },
    });
    const fixture_static_lib = b.addLibrary(.{
        .linkage = .static,
        .name = "zigrFixture",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/zig/fixture.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "R", .module = r_mod },
                .{ .name = "zigr", .module = zigr_mod },
            },
        }),
    });
    fixture_static_lib.root_module.addLibraryPath(.{ .cwd_relative = r_lib });
    fixture_static_lib.root_module.linkSystemLibrary("R", .{});
    b.getInstallStep().dependOn(&b.addInstallArtifact(fixture_static_lib, .{}).step);
}
