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
        },
    });

    const bench_lib = b.addLibrary(.{
        .linkage = .dynamic,
        .name = "zigr_benchmarks",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/zig/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "R", .module = r_mod },
                .{ .name = "zigr", .module = zigr_mod },
                .{ .name = "simd", .module = simd_mod },
            },
        }),
    });
    bench_lib.lto = .full;
    bench_lib.root_module.addLibraryPath(.{ .cwd_relative = r_lib });
    bench_lib.root_module.linkSystemLibrary("R", .{});
    bench_lib.root_module.linkSystemLibrary("blas", .{});
    bench_lib.root_module.linkSystemLibrary("dl", .{});
    bench_lib.root_module.linkSystemLibrary("m", .{});

    b.installArtifact(bench_lib);

    const task12_lib = b.addLibrary(.{
        .linkage = .dynamic,
        .name = "zigr_benchmarks_task28",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/zig/task_28_only_main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{.{ .name = "R", .module = r_mod }},
        }),
    });
    task12_lib.lto = .full;
    task12_lib.root_module.addLibraryPath(.{ .cwd_relative = r_lib });
    task12_lib.root_module.linkSystemLibrary("R", .{});
    task12_lib.root_module.linkSystemLibrary("blas", .{});

    b.installArtifact(task12_lib);
}
