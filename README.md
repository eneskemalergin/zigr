<!-- markdownlint-disable MD033 MD036 MD041 MD045 -->
<p align="center">
    <img src="assets/logo-readme.svg" alt="zigr" width="180" />
</p>

<p align="center">
    <strong>Zig bindings for R's C API</strong>
</p>

<p align="center">
    <img src="https://img.shields.io/badge/version-0.0.10-0f766e?style=for-the-badge" alt="Version 0.0.10" />
    <img src="https://img.shields.io/badge/zig-0.16.0-0f766e?style=for-the-badge&logo=zig&logoColor=white" alt="Zig 0.16.0" />
    <img src="https://img.shields.io/badge/r-4.6%2B-0f766e?style=for-the-badge&logo=r&logoColor=white" alt="R 4.6+" />
    <a href="LICENSE"><img src="https://img.shields.io/badge/license-MIT-7c3aed?style=for-the-badge" alt="License MIT" /></a>
</p>

<p align="center">
    <img src="https://img.shields.io/badge/build-zig%20build%20test-16a34a?style=for-the-badge&logo=zig&logoColor=white" alt="zig build test" />
    <img src="https://img.shields.io/github/actions/workflow/status/eneskemalergin/zigr/ci.yml?style=for-the-badge&label=CI&logo=github&logoColor=white" alt="CI" />
    <img src="https://img.shields.io/badge/cross--check-5%2F5-16a34a?style=for-the-badge" alt="Cross-compilation 5/5" />
</p>

<p align="center">
    <img src="https://img.shields.io/badge/status-experimental-f59e0b?style=for-the-badge" alt="Experimental" />
</p>

Cross-compile R extensions to all three CRAN targets from a single binary. No Cargo, no Makevars, no cross-toolchain setup.

---

R extensions need compiled code. Normally that means three different build systems depending on backend: autotools + configure for C, R CMD SHLIB for Rcpp, Cargo + rustup for extendr. Each requires a different cross-compilation setup.

zigr is a Zig library that wraps R's C API as Zig structs plus a build.zig that links R headers. Write Zig, run `zig build`, get a shared library. That is the whole flow.

## Why cross-compilation matters

R packages on CRAN need to ship binaries for x86_64 Linux, aarch64 macOS, and x86_64 Windows. Building for Windows from Linux normally means you install MinGW-w64 or a Windows cross-toolchain. For Rcpp you need a cross-compiled libstdc++. For extendr you need Rust std for the Windows target.

Zig ships its own target libs in the compiler binary. One file, 165 MB. `zig build -Dtarget=x86_64-windows-gnu` produces a working Windows .dll from Linux, no extra tools. macOS aarch64 from Linux works the same way.

The Zig 0.16.0 binary is bundled in this repo. No download, no PATH changes.

## Build

```bash
git clone https://github.com/eneskemalergin/zigr
cd zigr
zig build test
```

The test and rtest steps require R development headers on the system. The build resolves R paths in this order (CLI option, environment variable, default):

| Path          | CLI option           | Environment variable | Fallback                            |
| ------------- | -------------------- | -------------------- | ----------------------------------- |
| R include dir | `-Dr-include=<path>` | `$R_INCLUDE`         | `$R_HOME/include`                   |
| R library dir | `-Dr-lib=<path>`     | `$R_LIB`             | `$R_HOME/lib` then `/usr/lib/R/lib` |

On Debian/Ubuntu, `R_HOME` is typically `/usr/lib/R` but the include directory is at `/usr/share/R/include` instead of `$R_HOME/include`. Either set `R_INCLUDE` explicitly or use `-Dr-include=/usr/share/R/include`.

```bash
export R_HOME=/usr/lib/R
export R_INCLUDE=/usr/share/R/include
zig build check   # verify setup + formatting
zig build test    # run standalone tests
Rscript tests/run_r_tests.R  # build and run the live R runtime suite
```

## What you get

22 public modules covering the full R C API surface. The comptime export generator (`generateExports`) produces registration tables and `init`/`unload` hooks for the package root to call from its CRAN entry points. It does not emit the package-specific `R_init_<pkg>` symbol itself. The generated init hook registers routines and disables dynamic lookup.

- SEXP types and 24 classification helpers
- PROTECT/UNPROTECT helpers and an R_UnwindProtect bridge; generated wrappers release call-scoped scratch on normal return and R errors
- Type conversion (real, int, string, logical, raw, complex)
- Explicit borrowed-or-owned export views for numeric and complex inputs, plus a header-free read-only string view via `convert.StringSliceView`
- SIMD vector math via `@Vector(8, f64)` -> sum, mean, norm2, min, max, argmin, argmax, sum_narm, mean_narm, pmin, pmax, cumsum
- ALTREP consumption and creation (real, integer, logical classes)
- Data frames, attributes, S4 objects, external pointers, weak references
- R code evaluation (`rCodeEval`, `rRawEval`)
- Condition handling (`tryCatch`, `tryCatchError`)
- Serialization, RNG management, error/warning signaling
- R-managed memory allocator and opt-in allocator counters for diagnostics
- Fixed struct schemas (`asSEXP`/`tryFromSEXP`/`fromSEXP`) with declaration-order names and no runtime name map
- generateExports (`.Call` and `.External`) and typed external-pointer methods
- Zero dependencies (build.zig.zon is empty)

### Fixed schemas

`asSEXP` emits a `VECSXP` with declaration-order names and no other list attributes. `tryFromSEXP` accepts only that shape: the field count and every name must match, and the names vector itself must have no attributes. Optional fields must still be present; `NULL` and typed `NA` use the existing optional-scalar rules.

Use `tryFromSEXP` when Zig code needs a conversion error. Use `fromSEXP` inside an R entry point when that error should become an R error. Generated exports deliberately keep a struct boundary explicit: accept or return `R.SEXP`, then call the conversion helper in the package adapter. Protect an `asSEXP` result before any later R allocation.

Generated wrappers run inside `R_UnwindProtect`. Conversion errors become R errors, and call-scoped native storage is released when R longjmps. Handle Zig error unions inside your function or an explicit adapter before returning to the generated boundary. A Zig panic cannot be caught there, so exported functions must not panic. Returned slices are converted to R objects before call-scoped storage is released; do not retain borrowed R slices after the call.

### R runtime services

The core service layer stays close to the R C API:

- `attrib` provides checked string attributes and allocating setters over `Rf_getAttrib`, `Rf_setAttrib`, `Rf_namesgets`, `Rf_classgets`, and `Rf_dimgets`. Returned header arrays are caller-freed; their string bytes remain R-owned. `getString` maps `NA` to empty, while `getOptionalString` preserves it as `null`. R allocation and ALTREP access can longjmp, so native cleanup needs an outer unwind boundary.
- `symbols.install` wraps `Rf_install` with a fixed 64-entry thread-local cache. R owns and roots each symbol for the session, so callers do not protect it. Installation can longjmp. Names containing NUL or longer than 255 bytes become R errors instead of being truncated.
- `lang` exposes unchecked pairlist access for raw interop and allocating call constructors over `Rf_cons` and `Rf_lang*`. Constructed calls are returned unprotected, so protect them before another R allocation.
- `eval` wraps lookup, call evaluation, `R_tryEval`, and `R_tryEvalSilent`. Results are borrowed, unprotected `SEXP` values. Evaluation can longjmp; use a generated entry point or another unwind boundary when native cleanup is live.
- `interrupt` is a thin wrapper over `R_CheckUserInterrupt`, `R_CheckStack`, and `R_CheckStack2`. These checks can longjmp and do not create their own unwind boundary.
- `memory.CountingAllocator` wraps an allocator when you need allocation diagnostics. Its counts include only successful operations made through that wrapper; they do not include R heap objects or unrelated libc allocation. Keep it out of the allocator passed to timed code unless allocator overhead is the workload.

Raw `R.SEXP` parameters and returns remain the escape hatch when the typed conversion layer does not cover an R object. They add no ownership or type guarantee.

### SEXP ABI selection

`sexp.active_abi_contract` selects one access policy at compile time. Little-endian 64-bit targets built with R 4.x headers use the named `r4_64` layout contract. Other pointer widths, byte orders, and R major versions use `checked_r_api`, which routes type, length, data-pointer, vector-element, character-data, and character-encoding access through R's public C API.

The direct contract keeps its offsets in one private layout definition. ALTREP length and data access still use R accessors even when the direct contract is active. `sexp.checked` exposes the fallback operations for diagnostics and explicit safe access. Its vector-element helper rejects null, wrong-kind, and out-of-bounds input instead of indexing it.

The live R suite compares the active path with the checked path on the native R 4.x 64-bit target. A compile-only 32-bit check forces the fallback branch. That proves the fallback compiles; it is not a 32-bit R runtime or cross-target ABI claim.

### Native-state methods

I keep native state explicit. `generateMethods(T, ...)` accepts a method only when its first parameter is exactly `*T`; both `.Call` and `.External` take that receiver first and validate its R type, per-type tag, typed protected metadata, address, and alignment before casting it.

Use `externalptr.makeTyped(T, ptr, backing)` for a borrowed `*T`. `backing` remains reachable through the pointer's typed metadata and keeps R-owned state alive. `makeTypedRaw` is the explicit interop escape hatch for an erased address. The caller still owns the native lifetime: these checks do not prove that arbitrary foreign C memory remains valid. Use `externalptr.createTyped` when R should own a `c_allocator` value; its finalizer clears the address before running `deinit` and freeing it.

### Generated exports

I keep the generated layer small. `generateExports` builds registered `.Call` and `.External` tables from ordinary Zig functions. `generateMethods` does the same for functions whose first parameter is exactly `*T`. The package root still owns `R_init_<package>` and calls each generated `init` hook. Registration records the Zig parameter count as the R arity and disables dynamic symbol lookup.

`.Call` passes each R argument directly. `.External` receives R's pairlist and the wrapper extracts the declared arguments before conversion. A generated function supports up to eight parameters. A generated method supports the receiver plus four parameters; the receiver is the first R argument for both call styles. Method symbols are registered as `<zig_type>__<name>`, with dots in the Zig type name replaced by underscores.

| Zig boundary type | Parameter | Return | R contract |
| --- | :---: | :---: | --- |
| `f64`, `i32`, `bool` | yes | yes | Exactly one REAL, INTEGER, or LOGICAL value; required typed `NA` is an error |
| `?f64`, `?i32`, `?bool` | yes | yes | `NULL` or one typed `NA` becomes `null`; a null return becomes `NULL`; real `NaN` remains a value |
| `[]const f64`, `[]const i32` | yes | yes | Ordinary input is borrowed; ALTREP input may be copied into call-scoped storage |
| `StringSliceView`, `CachedStringSliceView`, `[]const []const u8` | yes | no, no, yes | The two view types preserve `NA` and encoding metadata; the slice-of-slices form owns call-scoped headers and discards that metadata |
| `RawSliceView`, `[]const u8` | yes | no, yes | Raw bytes are not strings; the view borrows ordinary RAWSXP storage and may own an ALTREP fallback |
| `[]const convert.Rcomplex` | yes | yes | Uses R's complex layout and preserves component-level NA/NaN values |
| `void` | no | yes | Returns `NULL` |
| `R.SEXP` | yes | yes | Direct escape hatch with no conversion, ownership, or type guarantee |

Structs are deliberately absent from this table. Use an `R.SEXP` adapter and call `convert.tryFromSEXP`, `convert.fromSEXP`, or `convert.asSEXP` when a fixed named-list schema belongs at the boundary. Generated functions must handle Zig error unions themselves, and they must not panic.

```zig
const R = @import("R");
const zigr = @import("zigr");

fn sum(values: []const f64) f64 {
    var total: f64 = 0;
    for (values) |value| total += value;
    return total;
}

const Exports = zigr.@"export".generateExports(
    &.{.{ .name = "my_sum", .func = sum }},
    &.{},
);

export fn R_init_mypackage(info: *R.DllInfo) callconv(.c) void {
    Exports.init(info);
}
```

The R wrapper can then use `.Call("my_sum", x)`. Package code should normally add R-side defaults, coercion, and user-facing error context there instead of making the native core guess.

### Boundary ownership

Borrowed numeric, raw, complex, and string views are valid only for the enclosing generated call. A direct ordinary vector does not copy its payload. An ALTREP fallback and copied string metadata live in the wrapper's two-tier arena: the first 8 KiB is fixed call storage, and overflow uses unwind-safe native allocation. Neither tier may escape the call.

Returned vectors and strings are new R objects. Fixed-schema lists are also R-owned, but callers must protect an unprotected constructor result before another allocating R call. `RAllocator` is for explicit R-managed allocations; `CountingAllocator` is diagnostic-only and does not count R heap or unrelated libc work. Native state created with `createTyped` is owned by its finalizer. Borrowed pointers created with `makeTyped` remain the caller's lifetime responsibility.

Every generated wrapper runs inside `R_UnwindProtect`. Conversion scratch, arena spill, protected temporaries, and registered cleanup are released on normal return and R longjmp. Zig `defer` alone is not a longjmp boundary. A finalizer clears its external pointer before destruction so a repeated finalizer cannot free the same value twice.

### What each reference proves

| Path | Role here | What I use it for | What it does not prove |
| --- | --- | --- | --- |
| R | Semantic correctness reference | Types, values, attributes, NA/NaN, and encoding | Native boundary cost |
| Handwritten C | ABI and R C API reference | Registration, protection, pointer checks, and comparable C entry points | zigr's generated-wrapper cost |
| Handwritten Zig | Kernel reference | The canonical compute baseline and the cost beneath generated glue | The public generated API |
| Generated zigr | Public target | Conversion, unwind, ownership, methods, and package-shaped registration | Higher-level package ergonomics or all-platform readiness |
| Savvy | Architectural reference | Generated registration, typed ownership distinctions, result and unwind design | A direct performance baseline for zigr; most local Savvy rows use raw FFI |

The published kernel report in `benchmarks/README.md` records canonical run `p0-7-20260710-full`. The published generated-boundary report there records focused run `20260711T232455Z-pid2`. Local raw runs and the promotion pointer live under the ignored `benchmarks/results/` tree, so I do not present those paths as files shipped by the repository. The two reports answer different questions and are not combined into one score.

### Acceptance checks

I use the existing flows for acceptance:

```bash
zig build fmt
zig build check -Doptimize=ReleaseSafe
zig build test -Doptimize=ReleaseSafe
Rscript tests/run_r_tests.R
cd benchmarks
Rscript check_coverage.R
Rscript run_benchmarks.R --runners=r,c_call,zigr --tasks=50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75
```

The command above is the generated-versus-handwritten development smoke. It validates the run artifacts before marking the run complete, but it deliberately omits representation rows and cannot be exported as a budget baseline. A boundary baseline uses every boundary and representation row:

```bash
Rscript run_benchmarks.R --runners=r,c_call,zigr --tasks=50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86 --build
Rscript export_boundary_metrics.R --run-dir=results/runs/<run_id>
```

The exporter validates the complete artifact again and rejects a stale budget policy or a failed budget. A full six-runner release baseline still uses an unfiltered `run_benchmarks.R` invocation. Every path keeps the unchanged adaptive policy. Error, longjmp, GC, and finalizer cases stay in the runtime suite instead of timed rows. Before accepting a change I also require `git diff --check`.

This bare core does not close the later work. Cross-target ABI fallback and compatibility belong to portability work; advanced ALTREP classes belong to integrations; reflective schemas, coercion, and higher-level objects belong to ergonomics; package workloads and end-to-end memory belong to application benchmarks; all-platform release and CRAN readiness belong to release engineering.

## CI

Every push and pull request runs:

- `zig fmt` (format compliance)
- Cross-compilation check (5 targets: x86_64-linux, aarch64-linux, x86_64-windows, aarch64-windows, aarch64-macos)
- `zig build test` (unit tests on ubuntu, macOS experimental, Windows experimental)
- Live R runtime tests on Ubuntu, including generated wrappers, GC, finalizers, and unwind recovery

macOS and Windows builds use `continue-on-error`. Native cross-compilation from Linux covers all three CRAN targets plus aarch64 variants.

## Performance

Results against 5 other backends (C, Rcpp, extendr, savvy, R) are in `benchmarks/README.md`.

- The published canonical baseline covers 36 comparable tasks: zigr is `0.212x` versus R by geomedian (`0.263x` by median), and `1.082x` versus the best native runner by geomedian (`1.003x` by median), with 17 aggregate wins or ties. These are handwritten/direct-entry results; generated API cost is measured separately. SIMD is the main reason: `@Vector(8, f64)` costs nothing to write and the compiler handles ISA dispatch.
- ALTREP method delegation (Sum, Min, Max as O(1) callbacks) means R never materializes zigr-backed vectors. This is not a speed win. It is a design win: R asks for the sum, zigr returns it without iterating.
- String ops are slower because each `CHAR()` call produces a new Zig slice header. The header-free `StringSliceView` avoids those Zig headers but requires adapter code in export functions; R encoding translation may still use call-scoped storage.

## Philosophy

zigr is not trying to be Rcpp with Zig syntax. It does not wrap every R type in a class hierarchy or hide the SEXP behind a generic `Robj`. It gives you the R C API directly, with comptime helpers that remove the repetitive parts (export registration, type conversion, protection).

You write explicit loops, explicit `Rf_protect`, explicit `REAL()` slice access. This is more typing. It is also faster because nothing is hidden.

## Project structure

```bash
build.zig              Module definition + tests
build.zig.zon          Package manifest (zero dependencies)
src/
├── root.zig           Library entry, re-exports all submodules
├── sexp.zig           SEXPTYPE enum, 24 classification helpers
├── protect.zig        PROTECT/UNPROTECT with depth tracking
├── cleanup.zig        R_UnwindProtect bridge + cleanup stack
├── convert.zig        Type conversion + fixed struct schemas
├── error.zig          Rf_error / Rf_warning signaling
├── interrupt.zig      R_CheckUserInterrupt, stack checking
├── memory.zig         RAllocator, unwind arena, and opt-in allocation counters
├── rng.zig            GetRNGstate / PutRNGstate wrappers
├── dataframe.zig      DataFrame wrapper, build
├── attrib.zig         getAttrib, setAttrib, setNames, setClass, setDim
├── s4.zig             S4 object detection and slot access
├── altrep.zig         ALTREP detection, data1/data2, class name
├── altrep_create.zig  Comptime ALTREP class generator
├── externalptr.zig    R_MakeExternalPtr wrappers with finalizers
├── trycatch.zig       R_tryCatch wrapper
├── serialize.zig      R_SerializeToVector / R_UnserializeFromVector
├── weakref.zig        R_MakeWeakRefC, R_WeakRefKey/Value
├── embed.zig          rCodeEval / rRawEval
├── export.zig         Comptime export generator
├── lang.zig           CAR, CDR, CONS, symbols, calls
├── eval.zig           rEval, findVar, findFunction, call, setVar
├── raw.zig            Zero-copy vector data access
├── rvector.zig        Typed RVector wrapper with arithmetic
├── simd.zig           SIMD lane configuration
└── cross_check.zig    Cross-compilation verification
```

## To use zigr in your R package

Write a `build.zig` that imports zigr's module and links R. Use the repo's `build.zig` as a reference. Point zig at R_HOME:

```bash
export R_HOME=/usr/lib/R && zig build
# or
zig build -Dr-home=/usr/lib/R
```

For exported read-only character vectors, prefer `zigr.convert.StringSliceView` over `[]const []const u8`. The old slice-of-slices form still works, but it allocates Zig slice headers. `StringSliceView` avoids those headers and lets you iterate element-by-element; encoding translation may still use R-managed storage for the enclosing call.

```zig
const zigr = @import("zigr");

fn stringTotalBytes(strings: zigr.convert.StringSliceView) i32 {
    var total: usize = 0;
    var it = strings.iterator();
    while (it.next()) |s| total += s.len;
    return @intCast(total);
}
```

## License

MIT

---

<p align="center">
    <em>Silent, static weight,</em><br />
    <em>Data dances through the bridge,</em><br />
    <em>Logic blooms in light.</em>
</p>
