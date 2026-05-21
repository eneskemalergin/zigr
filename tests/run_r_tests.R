#!/usr/bin/env Rscript
# Run zigr's R-dependent tests.
# Builds a test .so from tests/r_runtime.zig, loads it in R, and verifies
# that R API symbols resolve at runtime.

zig <- Sys.getenv("ZIG", "./zig-0.16.0/zig")
r_include <- Sys.getenv("R_INCLUDE", file.path(R.home(), "include"))

tmp_dir <- tempfile("zigr_test_")
dir.create(tmp_dir)
dir.create(file.path(tmp_dir, "src"))
file.copy("tests/r_runtime.zig", file.path(tmp_dir, "r_runtime.zig"))
file.copy("src/r_imports.h", file.path(tmp_dir, "src", "r_imports.h"))

writeLines(c(
  'const std = @import("std");',
  'pub fn build(b: *std.Build) void {',
  '    const target = b.standardTargetOptions(.{});',
  '    const optimize = b.standardOptimizeOption(.{});',
  '    const rh = b.addTranslateC(.{',
  '        .root_source_file = b.path("src/r_imports.h"),',
  '        .target = target,',
  '        .optimize = optimize,',
  '    });',
  paste0('    rh.addIncludePath(.{ .cwd_relative = "', r_include, '" });'),
  '    const test_so = b.addLibrary(.{',
  '        .linkage = .dynamic,',
  '        .name = "zigr_r_test",',
  '        .root_module = b.createModule(.{',
  '            .root_source_file = b.path("r_runtime.zig"),',
  '            .target = target,',
  '            .optimize = optimize,',
  '            .imports = &.{',
  '                .{ .name = "R", .module = rh.addModule("R") },',
  '            },',
  '        }),',
  '    });',
  '    b.installArtifact(test_so);',
  '}'
), file.path(tmp_dir, "build.zig"))

cat("Building test .so...\n")
build_cmd <- paste(zig, "build", "--build-file", file.path(tmp_dir, "build.zig"), "-Doptimize=ReleaseFast")
cat("  ", build_cmd, "\n")
result <- system(build_cmd, intern = TRUE)
cat(result, sep = "\n")

so_path <- file.path(tmp_dir, "zig-out", "lib", "libzigr_r_test.so")
if (!file.exists(so_path)) {
  so_path <- Sys.glob(file.path(tmp_dir, "zig-out", "*", "*.so"))[1]
}
if (!file.exists(so_path)) {
  stop("Test .so not found in zig-out/")
}

cat("Loading .so:", so_path, "\n")
dyn.load(so_path)

cat("\n=== Test results ===\n")
r <- try(.Call("zigr_test_protect"))
if (inherits(r, "try-error")) {
  cat("FAIL: zigr_test_protect\n")
  quit(status = 1)
} else {
  cat("PASS: zigr_test_protect (R symbols resolved correctly)\n")
}

cat("\nAll tests passed.\n")
