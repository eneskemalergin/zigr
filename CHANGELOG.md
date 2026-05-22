<!-- markdownlint-disable MD024 MD033 -->
# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.4] - 2026-05-22

### Added

- `protect()` now returns `SEXP`, enabling `protect(allocVector(...))` chaining.
- 11 new SEXP classification helpers: `isLogical`, `isComplex`, `isSymbol`, `isList`, `isLanguage`, `isPairList`, `isObject`, `isPrimitive`, `isArray`, `isNumber`, `isExpression`.
- `AltReal.register(info)` accepts a DllInfo pointer for proper CRAN symbol registration.
- `AltReal` now implements Get_region, Sum, Min, Max, Is_sorted, No_NA methods for faster R summary operations.
- `AltReal` Sum, Min, Max, Is_sorted now use `@Vector(8, f64)` SIMD. Measured 2.7x (sum), 1.6x (min/max), 1.5x (isSorted) speedup on 10M elements.
- Reverse FFI additions: `findVarInFrame`, `lang5`, `lang6`, `tryEval`, `tryEvalSilent`.
- `DataFrame.columnIndex` and `columnByIndex` for efficient repeated column lookups without linear name scan.
- `reverse_ffi.symbol` now caches the last 64 installed symbols in a static array, avoiding repeated `Rf_install` C calls.

### Fixed

- `protectWithIndex` no longer reads uninitialized index value before write.
- `RAllocator.resize` returns `false` instead of calling `R_chk_realloc` that could invalidate the old pointer.
- All C-string-taking functions (`signal`, `warn`, `symbol`, `setClass`, `hasSlot`, `getSlot`, `setSlot`) now ensure null termination, was reading past the end of runtime-constructed slices.
- `PROMSXP` enum variant renamed from `prompt` to `prom` (was not a real SEXPTYPE name).
- `toRealSlice`, `toIntSlice`, `toLogicalSlice`, `toRawSlice`, `toComplexSlice` now always allocate and return owned slices, removed inconsistent borrow-vs-alloc ALTREP bifurcation.
- `toRawSlice` and `toComplexSlice` return `[]const T` to prevent mutation of R-managed memory. Both now require an allocator parameter.
- `setS4Object` now sets the object bit and S4 class attribute (complete=1) instead of only the S4 bit.
- `Rf_findVar` resolved via `@extern` instead of fragile standalone `extern fn`.
- `FREESXP` enum variant renamed from `_fresh` to `_free` to match R internals naming.
- `toRealSlice`, `toIntSlice`, `toLogicalSlice` now use `@memcpy` from data pointer for non-ALTREP (zero C FFI, matches C baseline) and `*_GET_REGION` (single C call) for ALTREP, instead of per-element `*_ELT`.
- `fromLogicalSlice` uses `@memcpy` instead of per-element write.
- Removed 6 unnecessary cleanup frames from `from*` functions that wrap only pure `@memcpy` (keep only `fromStringSlice` where `Rf_mkCharLenCE` can longjmp).
- `AltReal.init` now has a cleanup frame that frees the `SliceWrap` allocation if `R_MakeExternalPtr` or `R_new_altrep` longjmps (was a leak).
- `dataframe.build` now has a prominent safety note documenting its unprotected return pattern.
- `protectCall` now restores caller cleanup frames after `on_return` clears the stack, fixing a design fragility where caller frames were silently dropped.

## [0.0.3] - 2026-05-21

### Added

- Type conversion: `toRealSlice`, `fromRealSlice`, `toIntSlice`, `fromIntSlice`, `toStringSlice`, `fromStringSlice`, `toLogicalSlice`, `fromLogicalSlice`, `toListSlice`, `fromListSlice`, `toRawSlice`, `fromRawSlice`, `toComplexSlice`, `fromComplexSlice`.
- Data frame module: `DataFrame.wrap`, `.columnCount`, `.rowCount`, `.columnNames`, `.column`, `build`.
- Attribute handling: `setNames`, `getClass`, `setClass`, `setDim`, `getAttrib`, `setAttrib`.
- S4 object support: `isS4`, `setS4Object`, `hasSlot`, `getSlot`, `setSlot`.
- ALTREP consumption: `isAltRep`, `data1`, `data2`, `className`.
- ALTREP creation: `AltReal` comptime class generator with Length, Elt, Dataptr, Duplicate callbacks.
- External pointer wrappers: `make`, `addr`, `tag`, `registerFinalizer`, `create`.
- R runtime tests expanded to 10 tests covering conversion, data frames, attributes, and ALTREP.

### Fixed

- SEXPTYPE enum values corrected to match Rinternals.h (were offset by 4+).
- `Rf_isFrame` not in public R API, which replaced with manual class attribute check.
- Unused allocator parameters documented as reserved for future use.
- Non-fallible functions return plain types, not `!T`.
- `depth` in protect.zig made thread-local.
- `cont` token now preserved via `R_PreserveObject` / `R_ReleaseObject`.
- `StackChecker` gated on Debug/ReleaseSafe builds only.

### Build

- debug: 2.0s clean, 0.66s incremental.
- ReleaseFast rtest .so: 459K stripped (34 exports).
- ReleaseSmall rtest .so: 157K (34 exports).
- 3/3 cross-compile targets.
- 10 R runtime tests at 0.0.3, 22 at 0.0.2, 34 total.

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
- Test .so: 3.4M (22 exports, Debug build).
- 3/3 cross-compile targets.

## [0.0.1] - 2026-05-21

### Added

- Project scaffold: build.zig, build.zig.zon, src/.
- SEXP type definitions, PROTECT stubs, conversion stubs.
- template/build.zig for R packages.
- Development guide and benchmark specifications.
- Zig 0.16.0 compiler bundled for reproducible builds.
- Baseline performance measurements for C, Rcpp, cpp11, extendr, and Zig.
