<!-- markdownlint-disable MD024 MD033 -->
# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

- CI: dropped benchmarks job, added `cache: true` to all `setup-r` calls, benchmarks removed from v0.0.10 Added entry
- CI: set `FORCE_JAVASCRIPT_ACTIONS_TO_NODE24: true` for mlugg/setup-zig@v2 Node 20 deprecation.

## [0.0.10] - 2026-06-13

### Added

- Benchmark harness: H.2 validation, R baselines (07a/07b/10/11/16), extendr FFI workaround (all 6 runners at 44/44)
- Comparative metrics pipeline
- System diagnostics (L7, tasks 44-47)
- CI workflow: fmt, cross-compile (5 targets), 3-platform tests

### Fixed

- **Benchmark harness**: crossprod copy-loop direction and struct_convert return type in all 5 native backends; R reference gaps in tasks 14, 15, 34, 37; CSV corruption from error-message commas; runner-name extraction in `export_comparative_metrics.R`; extendr 4 FFI rewires
- **Build system**: LTO gated behind `.linux`; `blas`/`dl`/`m` gated behind `!= .windows`; fmt dependency removed from check step
- **CI workflow**: `actions/checkout` v4->v6; cross-check was a no-op (R_HOME unset); Windows R_LIB set to `$R_HOME/bin/x64`
- **System diagnostics**: zig binary PATH fallback from hardcoded path; incremental build time no longer overwritten by post-restore rebuild

## [0.0.9] - 2026-06-12

### Added

- `src/s4.zig`: `newS4Object` for constructing S4 objects (74 LOC)
- `src/factor.zig`: Factor creation module with `asFactor` (109 LOC)
- `src/symbols.zig`: Open-addressing symbol cache with Wyhash
- `src/cleanup.zig`: `pushFrameInline` (64-byte inline buffer), eliminates P0 stack-escape UB
- `src/export.zig`: `.External` interface support, optional type mapping (`?f64`, `?i32`, `?bool`), 8-param arity
- Per-runner summary export for all 6 runners

### Fixed

- `AllocSliceCleanup` stack-variable UB (cleanup.zig:100): replaced with `pushFrameInline`
- `toComplex/Real/Int/Logical/RawSlice`: rewritten for ALTREP `*_GET_REGION` with offset/count loops
- `dataframe.zig`: null-guards for unnamed frames, mismatch check in `build`
- `rng.zig:withRng`: acquire/pushFrame ordering (acquire first)
- `convert.zig:cumsum`: switched from `R.REAL(sexp)` to `RealChunkIter` (avoid ALTREP materialization)
- `raw.zig:complex`: returns `&[0]Rcomplex{}` when `COMPLEX` returns null
- `protect.zig`: `unprotectN` for batch pops, comptime-gated depth tracking
- Build: `r_include` lazy, `c_call/Makefile` SRC list, `.gitignore`, thread-local hardening

### Changed

- Benchmark backends massively expanded: extendr (+460 LOC), savvy (+479), rcpp (+357). All 6 runners cover 44 tasks.
- `dataframe.zig`: `build` for constructing data frames
- `attrib.zig`: `setNames` null-guard
- `convert.zig`: SIMD `sumInt` with `@Vector(8, i64)` reduction
- `sexp.zig`: `fastVectorElt`, `fastCharData`, `xlength`/`tryXlength` split
- `eval.zig`: `rEval` uses `R_getVar` (R 4.6 API)
- `tests/r_runtime.zig`: 165 exported tests (was 145)

### Performance

- `sumInt`: `@Vector(8, i64)` SIMD reduction
- `min`/`max`: `chunkHasNA` fast path
- Benchmark data: zigr leads geomean vs R at 0.224x across 44 tasks (4.5x faster)

## [0.0.8] - 2026-05-24

### Added

- `tests/r_runtime.zig` and `tests/run_r_tests.R`: tests covering `raw.real()`, `raw.int()`, `raw.realMut()`, `raw.intMut()`, `raw.raw()`, `raw.complex()`, and `raw.dims()`.

### Fixed

- `src/export.zig`: `generateMethods` now replaces dots in `@typeName(T)` with underscores so generated C identifiers are valid.
- `src/export.zig`, `src/altrep_create.zig`, `src/externalptr.zig`: external pointer addresses now checked for null and valid EXTPTRSXP type before unwrapping.
- `src/convert.zig`, `src/raw.zig`: all `XLENGTH` to `usize` casts wrapped in a safety helper that panics on negative lengths, preventing silent wrap in release builds.
- `src/convert.zig`: `StringHashMapUnmanaged` init changed from `= .{}` to `= .empty` for Zig 0.16 compliance.
- `examples/hellozigr/R/hello.R`: removed dead `r_norm` function that referenced a nonexistent Zig export.
- `build.zig.zon` version bumped to 0.0.8 to match README and CHANGELOG.
- `README.md`: module count corrected from 23 to 25, bundled binary size corrected from 40 MB to 165 MB, added missing `rvector.zig` to project tree.

- `benchmarks/src/zig/task_10_blas_matmul.zig`: simplified result allocation.
- `benchmarks/run_benchmarks.R`: runs benchmark subprocesses with `OPENBLAS_NUM_THREADS=1`.
- `benchmarks/src/zig/task_12_cholesky.zig`: factors directly in the final R matrix instead of using a scratch buffer.
- `benchmarks/build.zig`, `benchmarks/src/zig/task_12_only_main.zig`, `benchmarks/runner_subprocess.R`, and `benchmarks/runners/zigr.json`: route `12_cholesky` through an isolated zigr benchmark library.
- `src/convert.zig`, `benchmarks/src/zig/task_04_strings.zig`, `benchmarks/src/zig/task_21_string_nchar.zig`, and `benchmarks/src/zig/task_35_string_variants.zig`: added reusable string-view helpers with cached element metadata and ported string-heavy benchmarks onto them.
- `benchmarks/src/zig/task_27_struct_convert.zig`: replaced the generic reflective conversion path with a handwritten fixed-slot path and cached output names, removing most of the remaining struct-conversion overhead.

### Performance

- Focused checks put `27_struct_convert` and `35_string_variants` ahead of savvy.
- Full rebuilt benchmark refresh brings `12_cholesky` back near Rust/C parity.

## [0.0.7] - 2026-05-23

### Added

- `src/simd.zig`: Centralized SIMD lane width.
- ALTREP benchmarks: altrep_sum (3000x vs C), altrep_read (O(1) via method table).
- Matrix transpose benchmark (replaced naive matmul).
- `analysis/compare.R`: Cross-runner comparison table with CSV output.
- R runtime boundary tests for wrong-type inputs, malformed named lists, scalar `NA`, and optional scalar `NA` to `null`.

### Fixed

- `src/lang.zig`: Symbol cache O(n) scan to O(1) open addressing.
- `src/export.zig`: Two-tier arena (8KB stack, heap spill).
- `src/raw.zig`: `logical()` now reads `LOGICAL()` instead of `INTEGER()`.
- `src/convert.zig` and `src/export.zig`: Public conversions now type-check scalars and slices before dereference.
- `src/convert.zig`: `fromSEXP` now rejects malformed named lists and signals R errors instead of panicking.
- `src/convert.zig`: Scalar `NA` is rejected for required scalars and mapped to `null` for `?f64`, `?i32`, and `?bool`.
- `src/convert.zig`: `pmin` and `pmax` now recycle shorter inputs correctly.
- `src/export.zig`: Zig 0.16 export generator fixes for pointer-size enums, comptime loops, and `R_useDynamicSymbols`.
- `src/protect.zig`: Depth tracking is comptime-gated (zero cost in ReleaseFast). Added `unprotectN`.
- Removed `Rf_allocSExp` and `Rf_applyClosure` from lang.zig and eval.zig (crash on R 4.6).
- Added `R_useDynamicSymbols` to export generator (CRAN requirement).
- `src/embed.zig`: 4096 byte buffer to dynamic `R_chk_calloc`.
- `PROTECT_INDEX` cast from `i32` to native type.
- Removed duplicate `isDataFrame` from sexp.zig.
- Deleted `reverse_ffi.zig`, merged into eval.zig.
- Reduced `cleanup.zig` MAX_NESTING from 64 to 16.
- Stripped `r_imports.h` to 6 headers (removed BLAS/LAPACK).
- 5 unfair benchmark implementations fixed (C strcat, R_alloc, qsort, named lists; Rcpp qsort).
- C Makefile now uses R's default CFLAGS (from R CMD config).
- Benchmark harness uses microbenchmark (nanosecond precision, no PARTIAL status).
- JSON stub benchmark removed (never worked, no std.Io context).
- Rcpp NA check uses `std::isnan` to match C (was using slower `ISNA`).
- `examples/template`: Fixed Zig 0.16 package fingerprint, local dependency path, and install-file name formatting.

### Performance

- vectorsum: SIMD `@Vector(8)` (2.6x vs C at R default flags).
- na_prop: Branchless `@Vector` + `@select` NA masking (18x vs C).
- rowsums: SIMD column load (flipped from 17% behind to 15% ahead of C).
- altrep_sum: O(1) method table delegation (3000x vs C's O(n) materialization).
- argmin/argmax in convert.zig: SIMD `@Vector` min-finding with index tracking.

## [0.0.6] - 2026-05-22

### Added

- R package template (`template/`): `build.zig` with R_HOME detection, cross-compilation targets, zigr vendoring, and `src/<pkg>.so` installation.
- Minimal R package example (`examples/hellozigr/`): DESCRIPTION, NAMESPACE, configure, Makevars, R wrapper, man pages, and Zig source demonstrating `generateExports`, type conversion, and reverse FFI.
- Benchmark harness (`benchmarks/`): runner-agnostic design with `runner.json` config files per runner, convergence detection (CV < 2% over last N iterations), and standardized CSV schema. Three runners kept in-repo (zigr, C_Call, Rcpp); full multi-runner suite maintained separately.
- BLAS/LAPACK tasks: 5 new matrix/linear algebra benchmarks (BLAS matmul, cross-product X'X, Cholesky decomposition, linear model fit via dgemm+dpotrf+dtrsm, row sums) added to all 3 runners. All verified producing identical results across zigr, C, and Rcpp.
- Vectorized workloads: 6 new tasks covering element-wise ops (abs/log/exp/sqrt), row/column means and sums, vector-scalar broadcasting, sorting, cumulative sum, and random normal generation. All verified identical across zigr, C, Rcpp, and R baseline.

### Fixed

- Export generator wrapper ABI: `fn([*]SEXP)` changed to `fn(SEXP,...,SEXP)` to match R's .Call dispatch (individual SEXPs, not array pointer). Fixes segfault in registered functions.
- `R_registerRoutines` parameter order fixed: was passing .Call table as .C slot. Tables moved to static storage (stack-allocated tables became dangling after `R_init_` returned).
- `pkg` param removed from `generateExports` / `generateMethods`. `@export` now done in user's root module (`@export` from dependency does not produce visible symbols).
- Benchmark Zig task files updated for current zigr API (`toRealSlice`/`toIntSlice` now take allocator, return error unions; `dataframe.build` API change; `ArrayList` init fix for Zig 0.16).
- `template/` and `examples/hellozigr/src/build.zig`: fixed for current `generateExports` API (no `pkg` param, `@export` in user root module); eliminated hardcoded `/usr/lib/R/lib` path (uses `r_lib` option, `R_LIB`, or `R_HOME`).
- `build.zig.zon`: version updated from 0.0.5 to 0.0.6.
- C_Call baseline runner: split from monolithic `bench.c` into per-task `.c` files + `register.c`; removed `-march=native` for fair comparison; fixed dead PROTECT/UNPROTECT in dataframe task; fixed `R_INCLUDE` path resolution.
- C_C legacy runner: added `fib` and `parallel` tasks (was only vectorsum + na_prop); removed `-march=native`.
- Fortran runner: removed duplicate `fib` (was in both `main.f90` and `tasks.f90`, causing linker error); fixed `R_INCLUDE` path resolution.
- Rust_raw runner: added `matmul`, `strings`, `dataframe` tasks (was only 5 tasks).
- savvy runner: documented as BROKEN (crate version mismatch), added `runner.json` with status note.
- Cleaned up stale "zig" runner results (renamed to "zigr").

### Testing

- Cross-compilation verified: `x86_64-linux-gnu` (ELF), `aarch64-macos-none` (Mach-O), `x86_64-windows-gnu` (COFF). Header translation + source compilation pass for all three CRAN targets from Linux host without additional toolchains. Added `zig build check` for cross-compilation verification. Full R .so linking requires target R headers and shared library. Validation script at `scripts/cross-compile-test.sh`.

## [0.0.5] - 2026-05-22

### Added

- Comptime export generator (`export.generateExports`): generates C wrappers, `R_init_<pkg>`, `R_registerRoutines`, CRAN-compliant `R_useDynamicSymbols(info, FALSE)`. Zero-length vectors to scalar parameters now panic with a clear message instead of reading garbage.
- Language node module (`lang`): `car`, `cdr`, `setCar`, `setCdr`, `tag`, `setTag`, `cons`, `consList`, `symbol`, `call1`-`call6`, `allocSExp`, `dataCons`, `list1`-`list6`.
- R evaluation module (`eval`): `rEval`, `findVar`, `findVarName`, `findFunction`, `call`, `setVar`. All wrapped in `R_UnwindProtect` for longjmp safety.
- R condition handling module (`trycatch`): `tryCatch`, `tryCatchError`, `extractMessage` via `R_tryCatch`.
- Serialization helpers (`serialize`): `toVector` and `fromVector` via `R_SerializeToVector` / `R_UnserializeFromVector`.
- Weak reference module (`weakref`): `make`, `key`, `value` via `R_MakeWeakRefC`, `R_WeakRefKey`, `R_WeakRefValue`.
- Export generator: `.External` interface support via second `external_exports` array.
- Export generator: optional type support (`?f64`, `?i32`, `?bool`) R NULL maps to `null`.
- Export generator: `R_unload_<pkg>` generated alongside `R_init_<pkg>`.
- Export generator: arity extended to 8 params for both `.Call` and `.External`.
- Export generator: better error messages via `signalErrorMsg(prefix, detail)`.
- `eval` module: `applyClosure`, `topLevelExec`, `baseEnv`, `emptyEnv` constants.
- `lang.allocSExp` now accepts `SEXPTYPE` enum instead of raw `unsigned int`.
- `callconv(.C)` fixed to `callconv(.c)` (Zig 0.16 CallingConvention has lowercase `c` only).
- `altrep_create.altGetRegion` buffer type fixed to `[*c]f64` (nullable pointer).
- `altrep_create` cleanup frame alignment fixed with `@alignCast`.
- R code embedding module (`embed`): `rCodeEval` and `rRawEval` via `R_ParseEvalString` / `R_ParseVector`. Wrapped in `R_UnwindProtect`.
- Struct-to-SEXP conversion (`convert.asSEXP` / `convert.fromSEXP`): converts Zig structs to/from R named lists using `@typeInfo` reflection. Supports nested structs, slices, scalars, optionals, SEXP.
- Method/self exports (`export.generateMethods`): first param receives `*T` from EXTPTRSXP. Method names prefixed with `T__`

### Fixed

- `trycatch.tryCatch` no longer segfaults: replaced `void*` comptime function pointer hack with comptime-generated trampoline per call site.
- `trycatch.tryCatch` now actually catches conditions: was passing `R_NilValue` (catch nothing), now passes `"condition"` STRSXP (catch all).
- `convert.fromSEXP` now takes explicit `arena` parameter. Previous version returned struct with slice fields pointing into freed arena memory (use-after-free).
- `zig fmt` requirements: `@typeInfo` field names in Zig 0.16 use `@"struct"`, `@"optional"`, `@"fn"` syntax, not `.Struct`, `.Optional`, `.Fn`.
- `HandlerState` struct field default `= R.R_NilValue` fails in ReleaseFast (not comptime-known). Changed to explicit `undefined` initialization.

### Testing

- Expanded test suite to 26 tests covering happy-path, edge-case, error-handling, invariant, and resource categories. All pass in a live R 4.6 session.

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
