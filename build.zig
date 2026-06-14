const std = @import("std");

fn rBuild(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
) struct { r_mod: *std.Build.Module, r_lib: []const u8, has_r: bool } {
    const r_include = b.option([]const u8, "r-include", "Path to R header directory") orelse
        b.graph.environ_map.get("R_INCLUDE") orelse
        blk: {
            if (b.graph.environ_map.get("R_HOME")) |rh| break :blk b.pathJoin(&.{ rh, "include" });
            break :blk null;
        };

    const r_lib = b.option([]const u8, "r-lib", "Path to R library directory") orelse
        b.graph.environ_map.get("R_LIB") orelse
        blk: {
            if (b.graph.environ_map.get("R_HOME")) |rh| break :blk b.pathJoin(&.{ rh, "lib" });
            break :blk "/usr/lib/R/lib";
        };

    if (r_include) |ri| {
        const r_headers = b.addTranslateC(.{
            .root_source_file = b.path("src/r_imports.h"),
            .target = target,
            .optimize = optimize,
        });
        r_headers.addIncludePath(.{ .cwd_relative = ri });
        const r_mod = r_headers.addModule("R");
        return .{ .r_mod = r_mod, .r_lib = r_lib, .has_r = true };
    }

    // R headers not available. Only `zig build fmt` works.
    return .{ .r_mod = undefined, .r_lib = r_lib, .has_r = false };
}

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Format check: works without R.
    const fmt_step = b.step("fmt", "Check zig fmt compliance");
    const fmt_check = b.addFmt(.{ .paths = &.{"."}, .check = true });
    fmt_step.dependOn(&fmt_check.step);

    // R-dependent steps: resolve lazily.
    const r = rBuild(b, target, optimize);
    if (!r.has_r) {
        const check_step = b.step("check", "Cross-compilation check (needs R headers)");
        _ = check_step;
        return;
    }

    const err_mod = b.addModule("error", .{
        .root_source_file = b.path("src/error.zig"),
        .target = target,
        .imports = &.{.{ .name = "R", .module = r.r_mod }},
    });
    const cleanup_mod = b.addModule("cleanup", .{
        .root_source_file = b.path("src/cleanup.zig"),
        .target = target,
        .imports = &.{
            .{ .name = "R", .module = r.r_mod },
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
            .{ .name = "R", .module = r.r_mod },
            .{ .name = "cleanup", .module = cleanup_mod },
            .{ .name = "error", .module = err_mod },
            .{ .name = "simd", .module = simd_mod },
        },
    });

    // Cross-compilation check: compile zigr modules for any target.
    const cross_check = b.addObject(.{
        .name = "zigr_check",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/cross_check.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "R", .module = r.r_mod },
                .{ .name = "cleanup", .module = cleanup_mod },
                .{ .name = "error", .module = err_mod },
                .{ .name = "simd", .module = simd_mod },
            },
        }),
    });

    const check_step = b.step("check", "Cross-compilation check: compile zigr for any target");
    check_step.dependOn(&cross_check.step);

    // Standalone tests
    const zigr_tests = b.addTest(.{ .root_module = zigr });
    zigr_tests.root_module.addLibraryPath(.{ .cwd_relative = r.r_lib });
    zigr_tests.root_module.linkSystemLibrary("R", .{});

    const run_zigr_tests = b.addRunArtifact(zigr_tests);
    const test_step = b.step("test", "Run zigr tests");
    test_step.dependOn(&run_zigr_tests.step);

    // R runtime test .so. Built via `zig build rtest`.
    if (b.findProgram(&.{"Rscript"}, &.{})) |_| {
        const so = b.addLibrary(.{
            .linkage = .dynamic,
            .name = "zigr_r_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("tests/r_runtime.zig"),
                .target = target,
                .optimize = optimize,
                .imports = &.{
                    .{ .name = "R", .module = r.r_mod },
                    .{ .name = "cleanup", .module = cleanup_mod },
                    .{ .name = "zigr", .module = zigr },
                },
            }),
        });
        so.root_module.addLibraryPath(.{ .cwd_relative = r.r_lib });
        so.root_module.linkSystemLibrary("R", .{});

        const install_so = b.addInstallArtifact(so, .{});
        const rtest_step = b.step("rtest", "Build R runtime test .so");
        rtest_step.dependOn(&install_so.step);
    } else |_| {}
}
