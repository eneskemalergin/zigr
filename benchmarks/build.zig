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

    // ── R C API via translate-c ──
    const r_headers = b.addTranslateC(.{
        .root_source_file = b.path("../src/r_imports.h"),
        .target = target,
        .optimize = optimize,
    });
    r_headers.addIncludePath(.{ .cwd_relative = r_include });
    const c_mod = r_headers.addModule("R");

    const raw_mod = b.createModule(.{
        .root_source_file = b.path("../src/raw.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{.{ .name = "R", .module = c_mod }},
    });

    const dataframe_mod = b.createModule(.{
        .root_source_file = .{ .cwd_relative = "../src/dataframe.zig" },
        .target = target,
        .optimize = optimize,
        .imports = &.{.{ .name = "R", .module = c_mod }},
    });

    const cleanup_mod = b.addModule("cleanup", .{
        .root_source_file = .{ .cwd_relative = "../src/cleanup.zig" },
        .target = target,
        .optimize = optimize,
        .imports = &.{.{ .name = "R", .module = c_mod }},
    });

    const simd_mod = b.addModule("simd", .{
        .root_source_file = .{ .cwd_relative = "../src/simd.zig" },
        .target = target,
        .optimize = optimize,
    });

    const convert_mod = b.createModule(.{
        .root_source_file = b.path("../src/convert.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "R", .module = c_mod },
            .{ .name = "cleanup", .module = cleanup_mod },
            .{ .name = "simd", .module = simd_mod },
        },
    });

    const altrep_create_mod = b.createModule(.{
        .root_source_file = .{ .cwd_relative = "../src/altrep_create.zig" },
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "R", .module = c_mod },
            .{ .name = "cleanup", .module = cleanup_mod },
            .{ .name = "simd", .module = simd_mod },
        },
    });

    // ── Benchmark shared library ──────────────────────────────
    const bench_lib = b.addLibrary(.{
        .linkage = .dynamic,
        .name = "zigr_benchmarks",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/zig/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "R", .module = c_mod },
                .{ .name = "convert", .module = convert_mod },
                .{ .name = "raw", .module = raw_mod },
                .{ .name = "dataframe", .module = dataframe_mod },
                .{ .name = "cleanup", .module = cleanup_mod },
                .{ .name = "simd", .module = simd_mod },
                .{ .name = "altrep_create", .module = altrep_create_mod },
            },
        }),
    });
    bench_lib.lto = .full;
    bench_lib.root_module.addCSourceFile(.{
        .file = b.path("src/zig/task_34_math_shim.c"),
        .flags = &.{ "-fno-builtin", "-fno-lto" },
    });

    bench_lib.root_module.addLibraryPath(.{ .cwd_relative = r_lib });
    bench_lib.root_module.linkSystemLibrary("R", .{});
    bench_lib.root_module.linkSystemLibrary("blas", .{});
    bench_lib.root_module.linkSystemLibrary("dl", .{});
    bench_lib.root_module.linkSystemLibrary("m", .{});

    b.installArtifact(bench_lib);

    const task12_lib = b.addLibrary(.{
        .linkage = .dynamic,
        .name = "zigr_benchmarks_task12",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/zig/task_12_only_main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{.{ .name = "R", .module = c_mod }},
        }),
    });
    task12_lib.lto = .full;

    task12_lib.root_module.addLibraryPath(.{ .cwd_relative = r_lib });
    task12_lib.root_module.linkSystemLibrary("R", .{});
    task12_lib.root_module.linkSystemLibrary("blas", .{});

    b.installArtifact(task12_lib);
}
