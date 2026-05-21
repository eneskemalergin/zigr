<!-- markdownlint-disable MD033 MD036 MD041 MD045 -->
<p align="center">
    <strong>zigr</strong>
</p>

<p align="center">
    <strong>Zig bindings for R's C API</strong>
</p>

<p align="center">
    <a href="#build"><img src="https://img.shields.io/badge/version-0.0.1-0f766e?style=for-the-badge" alt="Version 0.0.1" /></a>
    <a href="#build"><img src="https://img.shields.io/badge/zig-0.16.0-0f766e?style=for-the-badge&logo=zig&logoColor=white" alt="Zig 0.16.0" /></a>
    <a href="#build"><img src="https://img.shields.io/badge/r-4.6%2B-0f766e?style=for-the-badge&logo=r&logoColor=white" alt="R 4.6+" /></a>
    <a href="LICENSE"><img src="https://img.shields.io/badge/license-MIT-7c3aed?style=for-the-badge" alt="License MIT" /></a>
</p>

<p align="center">
    <a href="#build"><img src="https://img.shields.io/badge/build-zig%20build%20test-16a34a?style=for-the-badge&logo=zig&logoColor=white" alt="zig build test" /></a>
    <a href="plan/dev-guide.md"><img src="https://img.shields.io/badge/status-experimental-f59e0b?style=for-the-badge" alt="Experimental" /></a>
</p>

Write R extensions in Zig. Compile with `zig build`. Skip the Makevars.

R extensions need compiled code. Normally that means C, C++ with Rcpp, or Rust with extendr. zigr is the Zig version: R's SEXP types as Zig structs, a protection stack mapping to PROTECT/UNPROTECT, and a build.zig that links R headers. No autotools, no Cargo, no R CMD INSTALL dance.

## Status

Experimental. This compiles and runs tests. The SEXP wrappers exist. The conversion layer (Zig types to and from R vectors) is stubs. Real R interop needs the R dynamic library linked at runtime, which is the next block of work.

## Build

Requires [Zig 0.16](https://ziglang.org/learn/getting_started/).

```bash
git clone https://github.com/yourname/zigr
cd zigr
zig build test
```

## Project structure

```bash
build.zig              Module definition + tests
build.zig.zon          Package manifest (zero dependencies)
src/
├── root.zig           Library entry, re-exports submodules
├── sexp.zig           SEXPTYPE enum, VECTOR_SEXPREC, type stubs
├── protect.zig        PROTECT/UNPROTECT wrapper (stub, needs libR)
└── convert.zig        Type conversion (stub, not yet implemented)
template/
└── build.zig          Starter build.zig for R packages
```

## Why Zig and not Rust?

Rust has extendr and savvy both are solid. But the Rust-to-R pipeline runs through Cargo, which means the user needs a Rust toolchain. Cross-compiling (especially Windows from Linux) adds more setup. Zig's compiler is a single static binary that cross-compiles to any target by default. For an R package shipping to Windows, macOS, and Linux, that matters. Zig's comptime replaces proc macros, so the build graph is simpler: no build.rs, no proc macro step.

Measured on a Linux x86_64 machine with R 4.6 and gcc 11.4. Vector sum of 1e7 doubles, compiled with R CMD SHLIB defaults (-O2).

| Runner                     | Time       | vs C      |
| -------------------------- | ---------- | --------- |
| C -O2 while                | 9.27ms     | 1.00x     |
| Zig ReleaseSafe @Vector(8) | **3.91ms** | **0.42x** |
| Rcpp sugar                 | 31.60ms    | 3.41x     |
| extendr iter               | 59.00ms    | 6.37x     |

Zig in safe mode with explicit SIMD is 2.4x faster than C's default -O2. Strict IEEE 754, no fast-math. The @Vector intrinsic drives the speed, not the compiler flag.

## To use zigr in your R package

Copy `template/build.zig` to your package root, point it at R_HOME, and write Zig code in `src/zig/`.

## License

MIT

---

<p align="center">
    <em>Single binary,</em><br />
    <em>Cross-compile to every arch,</em><br />
    <em>R calls Zig now.</em>
</p>
