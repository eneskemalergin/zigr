<!-- markdownlint-disable MD024 MD033 -->
# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.2] - 2026-05-21

### Added

- SEXPTYPE enum with all R type tags and catch-all.
- 12 SEXP classification helpers (`typeOf`, `isVector`, `isMatrix`, `isFactor`, `isNumeric`, `isInteger`, `isReal`, `isString`, `isNull`, `isEnvironment`, `isFunction`, `isS4`, `isDataFrame`).
- R header translation via build.zig translate-c, cached across builds.
- Real `Rf_protect` / `Rf_unprotect` / `R_ProtectWithIndex` / `R_Reprotect` calls.
- `R_UnwindProtect` bridge with thread-local cleanup stack for longjmp safety.
- Error signaling module (`signal`, `warn`, `signalIf`).
- Interrupt and stack checking (`checkInterrupt`, `checkStack`, `checkStack2`).
- R-managed memory allocator (`RAllocator` backed by `R_chk_calloc` / `R_chk_free`).
- RNG state management (`acquire`, `release`, `withRng`).
- Reverse FFI (`symbol`, `lang2`/`lang3`/`lang4`, `eval`, `defineVar`, `findVar`).
- Cross-compilation targets: `x86_64-linux-gnu`, `aarch64-linux-gnu`, `x86_64-windows-gnu`.
- R runtime test suite: 22 tests covering allocation, PROTECT stress, NA handling, error signaling, type queries, longjmp cleanup, reverse FFI, RNG, and R-managed memory.

### Fixed

- `i32` vs `R_xlen_t` integer width mismatch caught by adversarial tests.
- Return type mismatch (bare `i32` vs `SEXP`).
- Stack imbalance in `R_ProtectWithIndex` / `Rf_unprotect` usage.
- Hidden type coercion bug: `?*anyopaque` vs `?*struct_SEXPREC` on all 13 classification helpers.

### Build

- clean: 2.1s, incremental: 0.13s.
- Test .so: 3.4M (22 exports, full R API coverage).
- 3/3 cross-compile targets.

## [0.0.1] - 2026-05-21

### Added

- Project scaffold: build.zig, build.zig.zon, src/.
- SEXP type definitions, PROTECT stubs, conversion stubs.
- template/build.zig for R packages.
- Development guide and benchmark specifications.
- Zig 0.16.0 compiler bundled for reproducible builds.
- Baseline performance measurements for C, Rcpp, cpp11, extendr, and Zig.
