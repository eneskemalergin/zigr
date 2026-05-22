const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

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
        .root_source_file = b.path("src/r_imports.h"),
        .target = target,
        .optimize = optimize,
    });
    r_headers.addIncludePath(.{ .cwd_relative = r_include });
    const r_mod = r_headers.addModule("R");

    const cleanup_mod = b.addModule("cleanup", .{
        .root_source_file = b.path("src/cleanup.zig"),
        .target = target,
        .imports = &.{.{ .name = "R", .module = r_mod }},
    });

    const zigr = b.addModule("zigr", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .imports = &.{
            .{ .name = "R", .module = r_mod },
            .{ .name = "cleanup", .module = cleanup_mod },
        },
    });

    // Standalone tests
    const zigr_tests = b.addTest(.{ .root_module = zigr });
    zigr_tests.root_module.addLibraryPath(.{ .cwd_relative = r_lib });
    zigr_tests.root_module.linkSystemLibrary("R", .{});

    const run_zigr_tests = b.addRunArtifact(zigr_tests);
    const test_step = b.step("test", "Run zigr tests");
    test_step.dependOn(&run_zigr_tests.step);

    // R runtime test .so. Build via `zig build rtest` when R is available.
    if (b.findProgram(&.{"Rscript"}, &.{})) |_| {
        const mod = b.createModule(.{
            .root_source_file = .{ .cwd_relative = "src/error.zig" },
            .target = target,
            .optimize = optimize,
            .imports = &.{.{ .name = "R", .module = r_mod }},
        });
        const imod = b.createModule(.{
            .root_source_file = .{ .cwd_relative = "src/interrupt.zig" },
            .target = target,
            .optimize = optimize,
            .imports = &.{.{ .name = "R", .module = r_mod }},
        });
        const rmod = b.createModule(.{
            .root_source_file = .{ .cwd_relative = "src/reverse_ffi.zig" },
            .target = target,
            .optimize = optimize,
            .imports = &.{.{ .name = "R", .module = r_mod }},
        });
        const mmod = b.createModule(.{
            .root_source_file = .{ .cwd_relative = "src/memory.zig" },
            .target = target,
            .optimize = optimize,
            .imports = &.{.{ .name = "R", .module = r_mod }},
        });
        const rngmod = b.createModule(.{
            .root_source_file = .{ .cwd_relative = "src/rng.zig" },
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "R", .module = r_mod },
                .{ .name = "cleanup", .module = cleanup_mod },
            },
        });

        const so = b.addLibrary(.{
            .linkage = .dynamic,
            .name = "zigr_r_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("tests/r_runtime.zig"),
                .target = target,
                .optimize = optimize,
                .imports = &.{
                    .{ .name = "R", .module = r_mod },
                    .{ .name = "cleanup", .module = cleanup_mod },
                    .{ .name = "error", .module = mod },
                    .{ .name = "interrupt", .module = imod },
                    .{ .name = "reverse_ffi", .module = rmod },
                    .{ .name = "memory", .module = mmod },
                    .{ .name = "rng", .module = rngmod },
                },
            }),
        });
        so.root_module.addLibraryPath(.{ .cwd_relative = r_lib });
        so.root_module.linkSystemLibrary("R", .{});

        const install_so = b.addInstallArtifact(so, .{});
        const rtest_step = b.step("rtest", "Build R runtime test .so");
        rtest_step.dependOn(&install_so.step);
    } else |_| {}
}
