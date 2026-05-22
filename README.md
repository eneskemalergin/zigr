<!-- markdownlint-disable MD033 MD036 MD041 MD045 -->
<p align="center">
    <img src="assets/logo-readme.svg" alt="zigr" width="180" />
</p>

<p align="center">
    <strong>Zig bindings for R's C API</strong>
</p>

<p align="center">
    <img src="https://img.shields.io/badge/version-0.0.6-0f766e?style=for-the-badge" alt="Version 0.0.6" />
    <img src="https://img.shields.io/badge/zig-0.16.0-0f766e?style=for-the-badge&logo=zig&logoColor=white" alt="Zig 0.16.0" />
    <img src="https://img.shields.io/badge/r-4.6%2B-0f766e?style=for-the-badge&logo=r&logoColor=white" alt="R 4.6+" />
    <a href="LICENSE"><img src="https://img.shields.io/badge/license-MIT-7c3aed?style=for-the-badge" alt="License MIT" /></a>
</p>

<p align="center">
    <img src="https://img.shields.io/badge/build-zig%20build%20test-16a34a?style=for-the-badge&logo=zig&logoColor=white" alt="zig build test" />
    <img src="https://img.shields.io/badge/status-experimental-f59e0b?style=for-the-badge" alt="Experimental" />
</p>

Write R extensions in Zig. Compile with `zig build`. No custom Makevars.

R extensions need compiled code. Normally that means C, C++ with Rcpp, or Rust with extendr. zigr is the Zig version: R's SEXP types as Zig structs, a protection stack mapping to PROTECT/UNPROTECT, and a build.zig that links R headers. No autotools, no Makefile wrangling.

## Status

Experimental but functional. All core R extension APIs are implemented and tested against R 4.6 in a live R session: SEXP type wrappers, PROTECT/UNPROTECT with R_UnwindProtect longjmp safety, ALTREP consumption and creation, type conversion (real, int, string, logical, raw, complex), data frames, attributes, S4 objects, external pointers, weak references, reverse FFI, error/warning signaling, RNG management, R-managed memory allocators, condition handling (tryCatch), R code evaluation, serialization, and a comptime export generator (R_init + R_registerRoutines + R_useDynamicSymbols for CRAN compliance).

## Build

Requires [Zig 0.16](https://ziglang.org/learn/getting_started/).

```bash
git clone https://github.com/eneskemalergin/zigr
cd zigr
zig build test
```

## Project structure

```bash
build.zig              Module definition + tests
build.zig.zon          Package manifest (zero dependencies)
src/
├── root.zig           Library entry, re-exports 20+ submodules
├── sexp.zig           SEXPTYPE enum, 24 classification helpers
├── protect.zig        PROTECT/UNPROTECT with depth tracking
├── cleanup.zig        R_UnwindProtect bridge + cleanup stack
├── convert.zig        Type conversion (REALSXP, INTSXP, STRSXP, LGLSXP, VECSXP, RAWSXP, CPLXSXP) + struct to/from R list
├── error.zig          Rf_error / Rf_warning signaling
├── interrupt.zig      R_CheckUserInterrupt, stack checking
├── memory.zig         RAllocator (R_chk_calloc / R_chk_free)
├── rng.zig            GetRNGstate / PutRNGstate wrappers
├── reverse_ffi.zig    Rf_eval, Rf_lang2-6, Rf_findVar, Rf_defineVar
├── dataframe.zig      DataFrame wrapper, build
├── attrib.zig         getAttrib, setAttrib, setNames, setClass, setDim
├── s4.zig             S4 object detection and slot access
├── altrep.zig         ALTREP detection, data1/data2, class name
├── altrep_create.zig  Comptime ALTREP class generator (AltReal)
├── externalptr.zig    R_MakeExternalPtr wrappers with finalizers
├── trycatch.zig       R_tryCatch wrapper (catch R errors from Zig)
├── serialize.zig      R_SerializeToVector / R_UnserializeFromVector
├── weakref.zig        R_MakeWeakRefC, R_WeakRefKey/Value
├── embed.zig          rCodeEval / rRawEval (evaluate R code from Zig)
├── export.zig         Comptime export generator (R_init_, R_registerRoutines)
├── lang.zig           CAR, CDR, CONS, symbols, call1-6, list1-6, allocSExp
├── eval.zig           rEval, findVar, findFunction, call, setVar, applyClosure
```

## Why Zig and not Rust?

Rust has extendr and savvy both are solid. The Rust-to-R pipeline runs through Cargo, which means the user needs a Rust toolchain. Cross-compiling (especially Windows from Linux) adds more setup. Zig's compiler is a single static binary that cross-compiles to the three CRAN targets (Linux, macOS, Windows) without additional toolchains. Zig's comptime replaces proc macros, so the build graph is simpler: no build.rs, no proc macro step.

Vector sum of 1e7 doubles, compiled with each toolchain's defaults. This is a micro-benchmark: C (-O2) uses a plain while loop, Zig (ReleaseSafe) uses an explicit @Vector(8) SIMD path, Rcpp and extendr use their default iterator patterns.

| Runner                     | Time       | vs C time |
| -------------------------- | ---------- | --------- |
| C -O2 (scalar while loop)  | 9.27ms     | 1.00x     |
| Zig ReleaseSafe @Vector(8) | **3.91ms** | **0.42x** |
| Rcpp sugar                 | 31.60ms    | 3.41x     |
| extendr iter               | 59.00ms    | 6.37x     |

On this test, the explicit SIMD path is 2.4x faster than C's scalar loop. C could match the time with `-O3 -ffast-math` or explicit SIMD intrinsics, but those are not the defaults that R users get through R CMD SHLIB. The point is not that Zig is inherently faster at math. The point is that Zig's default toolchain makes SIMD easy to write, and the resulting binaries are small and cross-compiled without ceremony.

## To use zigr in your R package

Copy `template/build.zig` to your package root, point it at R_HOME, and write Zig code in `src/zig/`.

The template accepts R_HOME in two ways:

```bash
export R_HOME=/usr/lib/R && zig build
# or
zig build -Dr-home=/usr/lib/R
```

## License

MIT

---

<p align="center">
    <em>Silent, static weight,</em><br />
    <em>Data dances through the bridge,</em><br />
    <em>Logic blooms in light.</em>
</p>
