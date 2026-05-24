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

    const err_mod = b.addModule("error", .{
        .root_source_file = b.path("src/error.zig"),
        .target = target,
        .imports = &.{.{ .name = "R", .module = r_mod }},
    });
    const cleanup_mod = b.addModule("cleanup", .{
        .root_source_file = b.path("src/cleanup.zig"),
        .target = target,
        .imports = &.{
            .{ .name = "R", .module = r_mod },
            .{ .name = "error", .module = err_mod },
        },
    });
    const simd_mod = b.addModule("simd", .{
        .root_source_file = b.path("src/simd.zig"),
        .target = target,
        .imports = &.{},
    });
    const zigr = b.addModule("zigr", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .imports = &.{
            .{ .name = "R", .module = r_mod },
            .{ .name = "cleanup", .module = cleanup_mod },
            .{ .name = "error", .module = err_mod },
            .{ .name = "simd", .module = simd_mod },
        },
    });

    // Cross-compilation check: compile zigr modules without linking against
    // R. Verifies header translation + source compilation for any target.
    // Unresolved R symbols are resolved by R at dynamic-link time.
    const cross_check = b.addObject(.{
        .name = "zigr_check",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/cross_check.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "R", .module = r_mod },
                .{ .name = "cleanup", .module = cleanup_mod },
            },
        }),
    });

    // Format check: verify all .zig files match zig fmt.
    const fmt_step = b.step("fmt", "Check zig fmt compliance");
    const fmt_check = b.addFmt(.{ .paths = &.{"."}, .check = true });
    fmt_step.dependOn(&fmt_check.step);

    const check_step = b.step("check", "Cross-compilation check: compile zigr for any target");
    check_step.dependOn(&cross_check.step);
    check_step.dependOn(&fmt_check.step);

    // Standalone tests
    const zigr_tests = b.addTest(.{ .root_module = zigr });
    zigr_tests.root_module.addLibraryPath(.{ .cwd_relative = r_lib });
    zigr_tests.root_module.linkSystemLibrary("R", .{});

    const run_zigr_tests = b.addRunArtifact(zigr_tests);
    const test_step = b.step("test", "Run zigr tests");
    test_step.dependOn(&run_zigr_tests.step);

    // R runtime test .so. Build via `zig build rtest` when R is available.
    if (b.findProgram(&.{"Rscript"}, &.{})) |_| {
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
                    .{ .name = "zigr", .module = zigr },
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
