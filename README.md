<!-- markdownlint-disable MD033 MD036 MD041 MD045 -->
<p align="center">
    <img src="assets/logo-readme.svg" alt="zigr" width="180" />
</p>

<p align="center">
    <strong>Zig bindings for R's C API</strong>
</p>

<p align="center">
    <img src="https://img.shields.io/badge/version-0.0.11-0f766e?style=for-the-badge" alt="Version 0.0.11" />
    <img src="https://img.shields.io/badge/zig-0.16.0-0f766e?style=for-the-badge&logo=zig&logoColor=white" alt="Zig 0.16.0" />
    <img src="https://img.shields.io/badge/r-4.6%2B-0f766e?style=for-the-badge&logo=r&logoColor=white" alt="R 4.6+" />
    <a href="LICENSE"><img src="https://img.shields.io/badge/license-MIT-7c3aed?style=for-the-badge" alt="License MIT" /></a>
</p>

<p align="center">
    <img src="https://img.shields.io/badge/build-zig%20build%20test-16a34a?style=for-the-badge&logo=zig&logoColor=white" alt="zig build test" />
    <img src="https://img.shields.io/github/actions/workflow/status/eneskemalergin/zigr/ci.yml?style=for-the-badge&label=CI&logo=github&logoColor=white" alt="CI" />
    <img src="https://img.shields.io/badge/compile--check-5%2F5-16a34a?style=for-the-badge" alt="Compile-check 5/5" />
</p>

<p align="center">
    <img src="https://img.shields.io/badge/status-experimental-f59e0b?style=for-the-badge" alt="Experimental" />
</p>

Compile-check R extension source for the declared Linux, macOS, and Windows targets with one Zig toolchain. Producing a loadable foreign extension still requires target-matched R libraries plus link, load, and runtime validation.

---

R extensions need compiled code. Normally that means three different build systems depending on backend: autotools + configure for C, R CMD SHLIB for Rcpp, Cargo + rustup for extendr. Each requires a different cross-compilation setup.

zigr is a Zig library that wraps a focused R C API surface and provides build primitives for package authors. This repository compile-checks the library and builds its runtime-test shared library; a package supplies its own `R_init_<pkg>` entry point and final artifact configuration.

## Why cross-compilation matters

R packages on CRAN need to ship binaries for x86_64 Linux, aarch64 macOS, and x86_64 Windows. Building for Windows from Linux normally means you install MinGW-w64 or a Windows cross-toolchain. For Rcpp you need a cross-compiled libstdc++. For extendr you need Rust std for the Windows target.

Zig ships compiler support for its target triples. zigr currently uses that support to compile-check five 64-bit targets from Linux. A foreign `.dll` or `.dylib` is not considered supported until it is linked against the matching R runtime, loaded, and exercised on that target.

Install Zig 0.16.0 or put it on `PATH`. A local `zig-0.16.0/` directory may exist in a development workspace, but it is git-ignored and is not included in a fresh clone.

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
zig build check   # compile-check the selected target
zig build test    # run standalone tests
Rscript tests/run_r_tests.R  # build and run the live R runtime suite
```

Run `zig build fmt` separately for formatting. Commands that compile zigr fail with a direct R-header diagnostic when neither `R_INCLUDE`, `R_HOME/include`, nor `-Dr-include` resolves.

## What you get

24 public modules covering zigr's declared R extension surface. The comptime export generator (`generateExports`) produces registration tables and `init`/`unload` hooks for the package root to call from its CRAN entry points. It does not emit the package-specific `R_init_<pkg>` symbol itself. The generated init hook registers routines and disables dynamic lookup.

- SEXP types and 22 `is*` classification helpers
- PROTECT/UNPROTECT helpers and an R_UnwindProtect bridge; generated wrappers release call-scoped scratch on normal return and R errors
- Type conversion (real, int, string, logical, raw, complex)
- Protected direct result builders for final numeric, integer, logical, raw, complex, string, list, and fixed-schema R storage
- Explicit borrowed-or-owned export views for numeric and complex inputs, plus header-free read-only string projections via `convert.StringIdentityView`, `StringMissingnessView`, `StringBytesView`, `StringEncodingView`, `StringTranslatedTextView`, and `StringMetadataView`
- SIMD vector math via `@Vector(8, f64)` -> sum, mean, norm2, min, max, argmin, argmax, sum_narm, mean_narm, pmin, pmax, cumsum
- ALTREP consumption and owned creation for real, integer, logical, raw, complex, and string vectors
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

### Direct result builders

`convert.ResultBuilder(T)` allocates one protected final R vector, exposes type-correct writable storage, and transfers the completed result with `finish()`. Supported atomic types are `f64`, `i32`, `bool`, `u8`, and `Rcomplex`; logical storage is exposed as R's `i32` representation. `StringResultBuilder` and `ListResultBuilder` keep their result protected while character or nested values are filled. Call `deinit()` when abandoning a partial result. `initFromInput()` validates the input type and length before allocating the matching final output, so a direct kernel does not need a full-sized native copy-back buffer. R-allocation and fill operations still require an enclosing generated unwind boundary.

### Serialization

`serialize.toVector` writes portable XDR using R serialization version 3; `toVectorVersion` also permits version 2 explicitly. Serialization rejects null input with an R error. `serialize.fromVectorChecked` returns a Zig error for null or non-raw input before decoding, while malformed raw data raises an R error. Serialized and restored values are returned unprotected, and both directions can allocate and longjmp. R classifies the custom persistent-stream API used here as highly experimental, so builds stay pinned to the declarations in their installed R headers.

### Weak references

`weakref.makeChecked` accepts `R_NilValue`, environment, external-pointer, and bytecode keys. A null finalizer creates an R weak reference without a callable C finalizer. A live key keeps the value reachable; collection clears both borrowed fields to `R_NilValue`. C finalizers receive the original key after the fields are cleared, run at most once, and must not signal an R error, longjmp, or retain the key. Constructors return an unprotected value and can allocate and longjmp.

Use `tryFromSEXP` when Zig code needs a conversion error. Use `fromSEXP` inside an R entry point when that error should become an R error. Generated exports deliberately keep a struct boundary explicit: accept or return `R.SEXP`, then call the conversion helper in the package adapter. Protect an `asSEXP` result before any later R allocation.

Generated wrappers run inside `R_UnwindProtect`. Conversion errors become R errors, and call-scoped native storage is released when R longjmps. Handle Zig error unions inside your function or an explicit adapter before returning to the generated boundary. A Zig panic cannot be caught there, so exported functions must not panic. Returned slices are converted to R objects before call-scoped storage is released; do not retain borrowed R slices after the call.

### R runtime services

The core service layer stays close to the R C API:

Call these services only from R's main thread. R's C API, ALTREP callbacks, protection stack, and runtime state are not safe for general worker-thread use.

- `attrib` provides checked string attributes and allocating setters over `Rf_getAttrib`, `Rf_setAttrib`, `Rf_namesgets`, `Rf_classgets`, and `Rf_dimgets`. Returned header arrays are caller-freed; their string bytes remain R-owned. `getString` maps `NA` to empty, while `getOptionalString` preserves it as `null`. R allocation and ALTREP access can longjmp, so native cleanup needs an outer unwind boundary.
- `dataframe.buildChecked` validates column counts, equal row counts, non-empty names, C string-length limits, and compact row-name limits before allocating. Matrix and array columns use their first dimension rather than total element length. It shares the supplied columns instead of copying them, so callers keep those columns reachable during construction and follow R copy-on-write rules afterward. The result is unprotected.
- `factor.asFactorChecked` uses R's string matching and locale collation, preserves `NA`, and returns an independent integer factor. Inputs longer than the integer-code limit are rejected. ALTREP strings are copied once to an ordinary working vector because R's order and match routines require direct string storage. The result is unprotected.
- `s4.newObjectChecked` resolves a registered class through R's methods registry and creates an object from its prototype. It does not run `initialize` or validity methods. Checked slot access distinguishes non-S4 objects and missing slots; slot assignment returns the possibly replaced object. Results are unprotected, and class lookup or raw slot operations can longjmp.
- `symbols.install` wraps `Rf_install` with a fixed 64-entry thread-local cache. R owns and roots each symbol for the session, so callers do not protect it. Installation can longjmp. Names containing NUL or longer than 255 bytes become R errors instead of being truncated.
- `lang` exposes unchecked pairlist access for raw interop and allocating call constructors over `Rf_cons` and `Rf_lang*`. `Argument` adds explicit R argument tags, while checked builders reject null pointers and invalid tag names before allocating. Inputs stay caller-rooted during construction. Constructed calls are returned unprotected.
- `eval` wraps lookup, positional and tagged calls, `R_tryEval`, and `R_tryEvalSilent`. `callIn`, `callFunctionIn`, and `callTaggedIn` make the evaluation environment explicit; the shorter `call` helper uses `R_GlobalEnv`. Results are borrowed, unprotected `SEXP` values. Function lookup and evaluation can longjmp, so use a generated entry point or another unwind boundary when native cleanup is live.
- `interrupt` is a thin wrapper over `R_CheckUserInterrupt`, `R_CheckStack`, and `R_CheckStack2`. These checks can longjmp and do not create their own unwind boundary.
- `rng.withRng` balances `GetRNGstate` and `PutRNGstate` on normal return and R longjmp. Nested scopes are rejected because R's RNG state API is not a reentrant stack.
- `memory.CountingAllocator` wraps an allocator when you need allocation diagnostics. Its counts include only successful operations made through that wrapper; they do not include R heap objects or unrelated libc allocation. Keep it out of the allocator passed to timed code unless allocator overhead is the workload.

ALTREP behavior is explicit across these integrations:

- Data-frame columns, raw attributes, language arguments, S4 slot values, and external-pointer backing are retained without requesting payload storage.
- Data-frame row validation reads length and at most the first dimension element without requesting contiguous dimension storage.
- Factor construction copies ALTSTRING elements into an ordinary vector. Attribute string readers likewise iterate `STRING_ELT` because their result is a native header array.
- Evaluation follows the called R function. Serialization may invoke class callbacks, while unserialization reads ALTREP raw streams through `RAW_GET_REGION`. Weak-reference creation may use R's duplication semantics.

Raw `R.SEXP` parameters and returns remain the escape hatch when the typed conversion layer does not cover an R object. They add no ownership or type guarantee.

### SEXP ABI selection

`sexp.active_abi_contract` selects one access policy at compile time. `checked_r_api` is the default and routes type, length, data-pointer, vector-element, character-data, and character-encoding access through R's public C API. The private `r4_6_x86_64` layout is available only when explicitly requested for a matching header and target shape.

| Contract | Selection | Checked by |
| --- | --- | --- |
| `r4_6_x86_64` | `-Ddirect-sexp=true`, R 4.6 headers, and a little-endian x86_64 target | Same-host live R checks; the opt-in does not attest to the loaded R runtime ABI |
| `checked_r_api` | Default, `-Dchecked-sexp=true`, or every other header/target shape | Default live R checks and five-target compile checks |

The direct contract keeps its offsets in one private layout definition. It is an explicit compatibility risk because translated headers do not verify the R runtime that loads the binary. ALTREP length and data access still use R accessors even when the direct contract is active. `sexp.checked` exposes the fallback operations for diagnostics and explicit safe access. Its type tag returns `-1` for a null pointer, and its vector-element helper rejects null, wrong-kind, and out-of-bounds input instead of indexing it.

Use `zig build abi-info` to print the selected contract and translated R header version. Pass `-Ddirect-sexp=true` only when you intentionally accept the private-layout compatibility condition; `-Dchecked-sexp=true` also forces the checked path. The offsets cannot come from public `offsetof` probes because installed R headers keep `SEXPREC` opaque. zigr therefore keeps the two R 4.x offsets private, gates them by pointer width, byte order, and R major version, and checks them against public accessors in the live runtime suite.

CI runs the live R suite with the default checked contract and with the explicit direct opt-in. The non-x86_64 compile targets select the checked branch. These checks prove same-host semantic parity and checked-branch compilation; they are not cross-target link, load, or runtime claims.

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
| `RealSliceView`, `IntegerSliceView`, `ComplexSliceView` | yes | no | Read-only views borrow ordinary input storage and may own an ALTREP fallback for the enclosing call |
| `LogicalSliceView` | yes | no | Read-only logical view preserves `0`, `1`, and `R_NaInt`; ordinary storage is borrowed and ALTREP fallback is call-scoped |
| `[]const f64`, `[]const i32` | yes | yes | Convenience forms use the same borrowed ordinary input path and may copy ALTREP input into call-scoped storage |
| `StringIdentityView`, `StringMissingnessView`, `StringBytesView`, `StringEncodingView`, `StringTranslatedTextView`, `StringMetadataView` | yes | no | Each generated projection requests only its declared fields; bytes and translated text borrow R-owned storage for the current call |
| `StringSliceView`, `CachedStringSliceView`, `[]const []const u8` | yes | no, no, yes | Compatibility forms: the broad views preserve `NA` and encoding metadata; the slice-of-slices form owns call-scoped headers and discards that metadata |
| `RawSliceView`, `[]const u8` | yes | no, yes | Raw bytes are not strings; the view borrows ordinary RAWSXP storage and may own an ALTREP fallback |
| `convert.VectorAccess(T, .one_pass)`, `.repeated_pass`, `.random_access` | yes | no | Explicit ALTREP access contract: direct storage is borrowed; one-pass non-direct input streams bounded regions; repeated/random input deliberately materializes a call-scoped native view |
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

The direct benchmark suite measures the same retained events across all seven runners. R and registered C remain correctness controls, and the benchmark does not turn those measurements into a product ranking.

## CI

Every push and pull request runs:

- `zig fmt` (format compliance)
- One shared-cache Debug compile-check job (5 targets: x86_64-linux, aarch64-linux, x86_64-windows, aarch64-windows, aarch64-macos)
- `zig build test` (required unit tests on Ubuntu, macOS, and Windows with R 4.6.1)
- Live R runtime tests on Ubuntu, including generated wrappers, GC, finalizers, and unwind recovery

All three native jobs are required. The cross targets run sequentially in one bounded Ubuntu job so R and Zig are installed once while every target still reports as a separate step. The Linux-hosted job proves source compilation only; native Windows and macOS jobs separately prove their unit suites.

## Performance

The benchmark harness reports per-task, per-runner distributions from the direct suite. It does not publish aggregate winners or separate report tracks. A complete source-matched run is still required before making any performance claim.

## Philosophy

zigr is not trying to be Rcpp with Zig syntax. It does not wrap every R type in a class hierarchy or hide the SEXP behind a generic `Robj`. It gives you the R C API directly, with comptime helpers that remove the repetitive parts (export registration, type conversion, protection).

You write explicit loops, explicit `Rf_protect`, explicit `REAL()` slice access. This is more typing. It is also faster because nothing is hidden.

## Project structure

```bash
build.zig              Module definition + tests
build.zig.zon          Package manifest (zero dependencies)
src/
├── root.zig           Library entry, re-exports all submodules
├── sexp.zig           SEXPTYPE enum, guarded access, 22 is* helpers
├── protect.zig        PROTECT/UNPROTECT with depth tracking
├── cleanup.zig        R_UnwindProtect bridge + cleanup stack
├── convert.zig        Type conversion + fixed struct schemas
├── error.zig          Rf_error / Rf_warning signaling
├── interrupt.zig      R_CheckUserInterrupt, stack checking
├── memory.zig         RAllocator, unwind arena, and opt-in allocation counters
├── rng.zig            GetRNGstate / PutRNGstate wrappers
├── dataframe.zig      DataFrame wrapper, build
├── factor.zig         Factor construction and validation
├── attrib.zig         getAttrib, setAttrib, setNames, setClass, setDim
├── s4.zig             S4 object detection and slot access
├── altrep.zig         ALTREP detection, data1/data2, class name
├── altrep_create.zig  Comptime ALTREP class generator
├── externalptr.zig    R_MakeExternalPtr wrappers with finalizers
├── trycatch.zig       R_tryCatch wrapper
├── serialize.zig      Public R persistent-stream serialization
├── weakref.zig        Checked weak-reference lifecycle helpers
├── embed.zig          rCodeEval / rRawEval
├── export.zig         Comptime export generator
├── lang.zig           CAR, CDR, CONS, symbols, calls
├── symbols.zig        Open-addressing symbol cache
├── eval.zig           rEval, findVar, findFunction, call, setVar
├── raw.zig            Zero-copy vector data access
├── rvector.zig        Typed RVector wrapper with arithmetic and direct copies
├── simd.zig           SIMD lane configuration
└── cross_check.zig    Cross-compilation verification
```

## To use zigr in your R package

Write a `build.zig` that imports zigr's module and links R. Use the repo's `build.zig` as a reference. Set R paths through the environment or the supported build options:

```bash
export R_HOME=/usr/lib/R && zig build
# or
zig build -Dr-include=/usr/share/R/include -Dr-lib=/usr/lib/R/lib
```

For exported read-only character vectors, choose the narrowest projection the kernel needs. `StringMissingnessView` performs only `NA_STRING` identity checks; `StringBytesView` borrows stored CHARSXP bytes with a length; `StringEncodingView` reads only the encoding mark; `StringTranslatedTextView` borrows R-managed UTF-8 translation or preserves CE_BYTES as stored bytes; and `StringMetadataView` combines identity, missingness, and encoding for metadata kernels. All borrowed state expires before the next allocating R call, callback, thread crossing, or return. `StringSliceView` remains the broad compatibility form; `[]const []const u8` still allocates call-scoped Zig headers.

```zig
const zigr = @import("zigr");

fn stringTotalBytes(strings: zigr.convert.StringBytesView) i32 {
    var total: usize = 0;
    var it = strings.iterator();
    while (it.next()) |s| total += s.bytes.len;
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
