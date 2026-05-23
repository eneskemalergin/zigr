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

    // Cross-compilation check: compile zigr modules to a static library
    // without linking against R. Verifies header translation + source
    // compilation for any Zig target. The resulting .o/.a has unresolved
    // R symbols resolved at runtime by R's dynamic linker.
    // Cross-compilation check: verifies R header translation + Zig source
    // compile for any target. Does NOT link against -lR; unresolved symbols
    // are resolved by R at dynamic-link time.
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

    const check_step = b.step("check", "Cross-compilation check: compile zigr for any target");
    check_step.dependOn(&cross_check.step);

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
                    .{ .name = "memory", .module = mmod },
                    .{ .name = "rng", .module = rngmod },
                    .{ .name = "convert", .module = b.createModule(.{
                        .root_source_file = .{ .cwd_relative = "src/convert.zig" },
                        .target = target,
                        .optimize = optimize,
                        .imports = &.{
                            .{ .name = "R", .module = r_mod },
                            .{ .name = "cleanup", .module = cleanup_mod },
                        },
                    }) },
                    .{ .name = "dataframe", .module = b.createModule(.{
                        .root_source_file = .{ .cwd_relative = "src/dataframe.zig" },
                        .target = target,
                        .optimize = optimize,
                        .imports = &.{.{ .name = "R", .module = r_mod }},
                    }) },
                    .{ .name = "attrib", .module = b.createModule(.{
                        .root_source_file = .{ .cwd_relative = "src/attrib.zig" },
                        .target = target,
                        .optimize = optimize,
                        .imports = &.{.{ .name = "R", .module = r_mod }},
                    }) },
                    .{ .name = "s4", .module = b.createModule(.{
                        .root_source_file = .{ .cwd_relative = "src/s4.zig" },
                        .target = target,
                        .optimize = optimize,
                        .imports = &.{.{ .name = "R", .module = r_mod }},
                    }) },
                    .{ .name = "altrep", .module = b.createModule(.{
                        .root_source_file = .{ .cwd_relative = "src/altrep.zig" },
                        .target = target,
                        .optimize = optimize,
                        .imports = &.{.{ .name = "R", .module = r_mod }},
                    }) },
                    .{ .name = "altrep_create", .module = b.createModule(.{
                        .root_source_file = .{ .cwd_relative = "src/altrep_create.zig" },
                        .target = target,
                        .optimize = optimize,
                        .imports = &.{
                            .{ .name = "R", .module = r_mod },
                            .{ .name = "cleanup", .module = cleanup_mod },
                        },
                    }) },
                    .{ .name = "externalptr", .module = b.createModule(.{
                        .root_source_file = .{ .cwd_relative = "src/externalptr.zig" },
                        .target = target,
                        .optimize = optimize,
                        .imports = &.{.{ .name = "R", .module = r_mod }},
                    }) },
                    .{ .name = "trycatch", .module = b.createModule(.{
                        .root_source_file = .{ .cwd_relative = "src/trycatch.zig" },
                        .target = target,
                        .optimize = optimize,
                        .imports = &.{.{ .name = "R", .module = r_mod }},
                    }) },
                    .{ .name = "serialize", .module = b.createModule(.{
                        .root_source_file = .{ .cwd_relative = "src/serialize.zig" },
                        .target = target,
                        .optimize = optimize,
                        .imports = &.{.{ .name = "R", .module = r_mod }},
                    }) },
                    .{ .name = "weakref", .module = b.createModule(.{
                        .root_source_file = .{ .cwd_relative = "src/weakref.zig" },
                        .target = target,
                        .optimize = optimize,
                        .imports = &.{.{ .name = "R", .module = r_mod }},
                    }) },
                    .{ .name = "sexp", .module = b.createModule(.{
                        .root_source_file = .{ .cwd_relative = "src/sexp.zig" },
                        .target = target,
                        .optimize = optimize,
                        .imports = &.{.{ .name = "R", .module = r_mod }},
                    }) },
                    .{ .name = "protect", .module = b.createModule(.{
                        .root_source_file = .{ .cwd_relative = "src/protect.zig" },
                        .target = target,
                        .optimize = optimize,
                        .imports = &.{.{ .name = "R", .module = r_mod }},
                    }) },
                    .{ .name = "lang", .module = b.createModule(.{
                        .root_source_file = .{ .cwd_relative = "src/lang.zig" },
                        .target = target,
                        .optimize = optimize,
                        .imports = &.{
                            .{ .name = "R", .module = r_mod },
                            .{ .name = "protect", .module = b.createModule(.{
                                .root_source_file = .{ .cwd_relative = "src/protect.zig" },
                                .target = target,
                                .optimize = optimize,
                                .imports = &.{.{ .name = "R", .module = r_mod }},
                            }) },
                        },
                    }) },
                    .{ .name = "embed", .module = b.createModule(.{
                        .root_source_file = .{ .cwd_relative = "src/embed.zig" },
                        .target = target,
                        .optimize = optimize,
                        .imports = &.{.{ .name = "R", .module = r_mod }},
                    }) },
                    .{ .name = "eval", .module = b.createModule(.{
                        .root_source_file = .{ .cwd_relative = "src/eval.zig" },
                        .target = target,
                        .optimize = optimize,
                        .imports = &.{
                            .{ .name = "R", .module = r_mod },
                            .{ .name = "cleanup", .module = cleanup_mod },
                            .{ .name = "lang", .module = b.createModule(.{
                                .root_source_file = .{ .cwd_relative = "src/lang.zig" },
                                .target = target,
                                .optimize = optimize,
                                .imports = &.{
                                    .{ .name = "R", .module = r_mod },
                                    .{ .name = "protect", .module = b.createModule(.{
                                        .root_source_file = .{ .cwd_relative = "src/protect.zig" },
                                        .target = target,
                                        .optimize = optimize,
                                        .imports = &.{.{ .name = "R", .module = r_mod }},
                                    }) },
                                },
                            }) },
                        },
                    }) },
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
