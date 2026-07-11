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
zig build rtest   # build R runtime test .so (run via Rscript tests/run_r_tests.R)
```

## What you get

22 public modules covering the full R C API surface. The comptime export generator (`generateExports`) produces registration tables and `init`/`unload` hooks for the package root to call from its CRAN entry points. It does not emit the package-specific `R_init_<pkg>` symbol itself. The generated init hook registers routines and disables dynamic lookup.

- SEXP types and 24 classification helpers
- PROTECT/UNPROTECT helpers and an R_UnwindProtect bridge; generated arena-backed cleanup is being hardened in P1
- Type conversion (real, int, string, logical, raw, complex)
- Zero-copy export views for numeric, complex, and read-only string inputs via `convert.StringSliceView`
- SIMD vector math via `@Vector(8, f64)` -> sum, mean, norm2, min, max, argmin, argmax, sum_narm, mean_narm, pmin, pmax, cumsum
- ALTREP consumption and creation (real, integer, logical classes)
- Data frames, attributes, S4 objects, external pointers, weak references
- R code evaluation (`rCodeEval`, `rRawEval`)
- Condition handling (`tryCatch`, `tryCatchError`)
- Serialization, RNG management, error/warning signaling
- R-managed memory allocator
- Struct-to-SEXP reflection (`asSEXP`/`fromSEXP`)
- generateExports (`.Call` and `.External`) and generateMethods (EXTPTRSXP)
- Zero dependencies (build.zig.zon is empty)

## CI

Every push and pull request runs:

- `zig fmt` (format compliance)
- Cross-compilation check (5 targets: x86_64-linux, aarch64-linux, x86_64-windows, aarch64-windows, aarch64-macos)
- `zig build test` (unit tests on ubuntu, macOS experimental, Windows experimental)
- System diagnostics (build time, binary size, cross-compile time, memory allocation count)
- Sanity check (zigr runner on tasks 1 and 18)

macOS and Windows builds use `continue-on-error`. Native cross-compilation from Linux covers all three CRAN targets plus aarch64 variants.

## Performance

Results against 5 other backends (C, Rcpp, extendr, savvy, R) are in `benchmarks/README.md`.

- The published P0 baseline (`p0-7-20260710-full`) covers 36 comparable tasks: zigr is `0.212x` versus R by geomedian (`0.263x` by median), and `1.082x` versus the best native runner by geomedian (`1.003x` by median), with 17 aggregate wins or ties. These are handwritten/direct-entry benchmark results; generated public API performance is a P1 workstream. SIMD is the main reason: `@Vector(8, f64)` costs nothing to write and the compiler handles ISA dispatch.
- ALTREP method delegation (Sum, Min, Max as O(1) callbacks) means R never materializes zigr-backed vectors. This is not a speed win. It is a design win: R asks for the sum, zigr returns it without iterating.
- String ops are slower because each `CHAR()` call produces a new Zig slice header. The zero-copy `StringSliceView` avoids this but requires adapter code in export functions.

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
├── convert.zig        Type conversion + struct reflection
├── error.zig          Rf_error / Rf_warning signaling
├── interrupt.zig      R_CheckUserInterrupt, stack checking
├── memory.zig         RAllocator (R_chk_calloc / R_chk_free)
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

For exported read-only character vectors, prefer `zigr.convert.StringSliceView` over `[]const []const u8`. The old slice-of-slices form still works, but it has to allocate Zig slice headers. `StringSliceView` keeps the call zero-copy and lets you iterate element-by-element.

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
