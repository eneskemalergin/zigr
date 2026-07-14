<!-- markdownlint-disable MD024 -->

# zigr Benchmark Harness

Seven active runner backends. The published report below records the earlier six-runner canonical run `p0-7-20260710-full`; a local promotion writes `results/CANONICAL_RUN.json`, but `results/` is intentionally ignored because raw samples are large and regenerated. Its 44-task comparison stays separate from the 26 boundary rows and 11 representation rows. extendr uses raw FFI wrappers for four tasks because its R-object wrapper double-panicked there.

This is a runner-implementation and kernel report, not a full-tool comparison. R is the semantic reference, and handwritten C is a lower-bound control. The separate F01 through F12 suite now has package-shaped public fixtures for all five products, pure-R references, two optimized base-R baselines, and registered C controls. Those fixtures are not part of the historical aggregate. Most legacy Savvy tasks use handwritten raw R FFI, extendr has four raw-FFI substitutions, and the legacy Rcpp runner is not a complete generated package workflow. The aggregate cannot establish that zigr is better than Rcpp, cpp11, extendr, or Savvy.

- **r** (R controls): 17 AST-validated pure-R rows, 55 optimized base-R or runtime rows, and 11 explicit unrepresentable dispositions backed by reference functions in `src/r/run_all.R`
- **zigr** (Zig): 46 direct task files plus registered boundary fixtures under `src/zig/`, built via `build.zig`
- **c_call** (C): 44 task files plus boundary fixtures in `register.c` under `src/c_call/`
- **rcpp** (C++): Single `main.cpp` under `src/cpp/`
- **cpp11** (C++): Installable `zigrCpp11` fixture under `src/cpp11/`, with typed annotated source and committed cpp11-generated R/C++ registration glue
- **savvy** (Rust): `rust/src/lib.rs` + `init.c` under `src/savvy/`; most canonical tasks use handwritten raw R FFI behind Savvy-style C result handling, so they compare runner implementations rather than the cost of Savvy's typed wrappers
- **extendr** (Rust): `rust/src/lib.rs` + `entrypoint.c` under `src/extendr/`, with raw FFI entrypoints for 4 tasks

## First-wave product fixtures

The normalized fixture suite lives beside the legacy task sources. zigr uses `generateExports` and `generateMethods`; Rcpp uses attributes and Modules; cpp11 uses its register annotation and retained generated glue; extendr uses attributes, its module generator, and retained R wrappers; Savvy uses attributes and retained bindgen C and R outputs. `build_all.sh` installs each fixture as a package and runs the live gate in one isolated R process per tool, which matches runner isolation in the benchmark harness.

The source gate covers all 84 runner and fixture cells. It accepts 46 generated typed product paths, ten generated public adapters, and 23 reference or control paths. The adapters are zigr's P1-approved explicit F09 fixed-schema path and nine Rcpp entries whose annotated `Rcpp::RObject` parameters apply exact Rcpp type guards before constructing typed Rcpp views. Five cells remain explicit gaps: zigr's generator has no typed F08 logical-slice parameter, pure R cannot construct F10 native state, and cpp11 0.5.5 cannot express F07 complex vectors, the complete F10 generated typed-method and tag contract, or the complex-bearing F12 output set. No product gap is filled with raw FFI or zigr's untyped `R.SEXP` escape hatch.

The current fixture gate proves source identity, retained generated artifacts, package registration, load and reload behavior, and the complete correctness-only contract before timing. The shared matrix has 24 valid and 28 invalid value cases across the fully supported fixture set. It checks exact type, length, attributes, classes, NA versus NaN, signed zero, string encoding, input preservation, non-aliased output storage, 28 R-visible output objects, and three non-empty copy events. F12 construction runs once with forced GC active and is checked again after return under allocation pressure. Every applicable call is compared with the authored pure-R oracle and registered C control; F03 and F04 also use separately labeled optimized base-R cross-checks.

The lifecycle gate repeats every F11 invalid family eight times and requires recovery after each call. F10 repeats wrong receiver, wrong pointer type, wrong amount type, wrong length, missing amount, and cleared pointer cases, then requires one constructor, five successful method calls, and exactly one finalizer. The instrumented 257-element ALTREP input records zigr and C using one region callback, cpp11 using seven region callbacks, extendr using 257 element callbacks, and Rcpp and Savvy materializing through one pointer callback. These observed strategy differences remain visible instead of being averaged away.

The evidence matrix now contains nine exact Tier A product cells, 47 valid Tier B strategy cells, 23 Tier C reference or control cells, and five gaps. F01 and F12 are the only Tier A groups. F02, F05, F07, and F08 remain semantically correct but are Tier B because the cross-product group contains Rcpp's strict public adapter boundary rather than a typed generated parameter. Timing remains disabled for all fixture cells until the source-matched rebuild and measurement stage.

## Task matrix

83 manifest rows: 44 core tasks (`01` through `43`, with `07a`/`07b` splitting task 07), two P3 integration diagnostics (`48` and `49`), 26 boundary rows (`50` through `75`), and 11 representation rows (`76` through `86`).

- **Core compute** (tasks 01-06): vector sum, element-wise ops, memory bandwidth, sort, recursion, broadcast
- **R API mechanics** (tasks 07a, 07b, 08-11): PROTECT shallow/scaling, type dispatch, longjmp, SEXP create/inspect
- **Objects and strings** (tasks 12-24): matrix ops, data frame filter, list access, string ops, factor ops, attributes, S4 slots, NA propagation, long vector indexing
- **Numerical routines** (tasks 25-29): L1 arithmetic, matmul, crossprod, Cholesky, linear model
- **ALTREP behavior** (tasks 30-37, 49): comparative compact creation, materialization, element walk, region read, sum (R and native), min/max, no-NA query, and a zigr-owned callback sequence
- **Runtime services** (tasks 38-41, 43, 48): struct convert, R eval, try eval, serialize roundtrip, RNG stress, weak-reference create/access
- **External pointer** (task 42): structural validation across runner-specific ownership strategies
- **Diagnostics** (tasks 44-47): build time, binary size, cross-compile time, and memory allocation counts (zigr only)
- **Boundary pairs** (tasks 50-75): generated and handwritten zero-arg, scalar, optional, numeric, ALTREP, string, raw, complex, fixed-schema, external-pointer, and `.External` calls. The generated schema row uses an explicit `SEXP` adapter and the handwritten row validates the same fixed contract. They are `api_overhead` and `non_comparable`; `analysis_summary.csv` keeps each variant separate.
- **Representation rows** (tasks 76-86): one and four passes through strings as views, cached metadata, or headers; raw views and copies; complex views and returns. The string input is ordinary ASCII so the timing stays about representation rather than translation. These rows are `api_overhead` and `non_comparable` because the ownership models differ.

The boundary fixtures run in `r`, `c_call`, and `zigr`; cpp11 also runs the applicable generated zero, scalar, optional, numeric, string, raw, schema, and external-state rows plus task 42. Its F04 semantic check stays outside task 62 because that historical row declares zigr's contiguous-copy strategy; the normalized fixture evidence records cpp11's region access separately as Tier B. Its complex and `.External` rows remain unsupported instead of using raw substitutes. The weak-reference and owned-ALTREP diagnostics run only in `zigr`. The representation rows run in `r` and `zigr`; other runners report unsupported rows as `N/A` instead of timing a substitute with different ownership.

The materialized numeric rows use ordinary REALSXPs. The integer ALTREP rows compare zigr's contiguous copy with a handwritten region stream, so they describe a conversion choice rather than wrapper overhead. The generated schema row parses a declared fixed schema through `SEXP`; the handwritten and R rows validate the same plain names and scalar-field contract. The generated method row times a valid typed `.Call` receiver with the same small state update as its handwritten pair. Missing or tampered metadata, wrong tags, foreign pointers, cleared pointers, GC retention, and finalizers are preflight and runtime safety cases, not timing claims.

Task 30 retains its original cross-runner compact-sequence contract and aggregate eligibility. For zigr, task 42 constructs and validates an owned typed external pointer, task 43 enters through `rng.withRng`, and task 48 constructs and reads checked weak references. Task 49 constructs copied integer storage through a class registered by `R_init_zigr_benchmarks`, then exercises R summaries, chunked regions, deep and shallow duplication, sortedness and no-NA metadata, and version-3 serialization with same-class restoration. Its named result records the exact summary, region, duplicate, metadata, and restored boundary values. The P3 diagnostics are non-comparable and have no optimization budgets.

Direct ReleaseFast run `20260713T051441Z-pid2` passes the reviewed task 49 callback sequence with a `29.9998 ms` median and `19.71%` CV over 500 samples. Forced `checked_r_api` run `20260713T051507Z-pid2` passes the same source and result contract with a `30.3999 ms` median and `19.12%` CV over 500 samples. Each run rebuilt its library, and its schema-2 manifest records the checked-SEXP mode. Both runs are filtered diagnostics and do not replace or alter the published canonical aggregate.

`task_manifest.csv` is the canonical task-policy source. It owns stable task IDs, task groups, display names, workload categories, expected result contracts, correctness policy, comparability, aggregate membership, and exclusion notes. The executable argument closures remain in `runner_subprocess.R` as task specs selected by manifest ID; the `input_factory` value `task_spec.args` names that adapter boundary without duplicating R code in CSV.

`evidence_manifest.json` is the normalized evidence companion. It expands to one disposition for every combination of 83 tasks and seven runners, plus every combination of F01 through F12 and the same runners. Each cell records the implementation role, evidence use, path kind and public path, representation strategy, kernel and contract identity, fixture version, comparison tier, mutation policy, setup policy, timing eligibility, reason, and owner. The frozen dispositions are `applicable`, `control_only`, `product_gap`, `not_meaningful_for_product`, `fixture_not_implemented`, `fixture_invalid`, and `supported_and_executable`.

Runner JSON files own executable symbols, not applicability. `optional_tasks` is generated in memory from non-executable evidence cells and checked against the export map. The R map is additionally split into timed pure-R and optimized rows while retaining reference-only functions for native correctness. Fixture semantics now support the nine Tier A and 47 Tier B product classifications, but every fixture row remains timing-ineligible until a source-matched rebuilt measurement. Historical diagnostic rows are classified, but old timings cannot support a current product claim; source-matched correctness and measurement remain a later stage.

The historical task matrix is now classified cell by cell: 14 Tier A product cells, nine Tier B strategy cells, 142 Tier C R or C controls, 203 Tier D diagnostics, and 213 explicit gaps. The exact Tier A groups are the matching zigr and cpp11 generated paths for tasks 50, 52, 54, 56, 58, 60, and 64. Tier B retains zigr tasks 62, 66, 68, 70, 72, and 74 plus cpp11 tasks 42, 66, and 70. cpp11 task 72 is a source-valid product path but remains diagnostic because its state contract does not match the generated-method comparison. Of the 70 registered C rows, the seven native-invariant tasks are diagnostic controls and the other 63 are kernel controls. The legacy Rcpp, extendr, and Savvy rows remain implementation diagnostics rather than product evidence. Timing eligibility is false for every one of these 581 task cells until the source-matched correctness and measurement stage.

`source_ledger.json` is the centralized source and tool specification. It expands at run time into exact repository source and build digests, generated-glue identity, installed R package source identity, Cargo lock packages and selected features, compiler and profile settings, R/BLAS/LAPACK identity, artifact dependency closure, and one source-path classification for every selected runner and task. It also records the source, build, and generated-glue identities for all seven normalized fixture paths. Macro and comptime output that is not retained is labeled as unretained and tied to its generator source. The legacy source verifier accepts the current 13 generated zigr boundary rows and 11 cpp11 rows as source-valid product paths, while the evidence layer keeps cpp11 task 72 diagnostic-only. It rejects the current Rcpp and Savvy rows as product evidence, identifies the four raw extendr substitutions, keeps all 70 registered C rows as controls, and preserves the 17 pure, 55 optimized, and 11 unrepresentable timed-R split.

Each runner record keeps both the authored recipe and the resolved top-level build invocation, including its executable, arguments, working directory, relevant environment overrides, and whether that invocation ran in the current run. A prebuilt run records `executed_in_run=false`; it does not claim that the artifact was rebuilt from the recorded source. P4.6 owns that source-matched rebuild gate.

Exact transitive artifact dependency capture currently uses Linux `ldd`. The ledger stops on other hosts instead of writing a partial dependency identity.

The manifest distinguishes `r_reference` and `native_invariant` correctness policies and retains the frozen `nondeterministic` vocabulary for compatibility. API-overhead tasks remain `non_comparable` for the primary aggregate while staying visible in task-level reports. Task 43 now uses a deterministic seed and compares both returned values and post-call R RNG state; it is no longer validated as an unconstrained nondeterministic result. Runner configurations and task specs must match both manifests before timing starts. Run `Rscript check_coverage.R` for the focused preflight; it runs the evidence, harness-trust, source-ledger, and product-fixture source suites plus the R task-spec check without native builds. `run_benchmarks.R` runs this preflight before it creates measured artifacts. `build_all.sh` repeats the source gates before native compilation, installs the five product fixtures, and runs the isolated live fixture gate.

Input generators remain in one place in `runner_subprocess.R`. Before runner execution, that source derives one seed per selected task from the run seed, task ID, and fixture version, then writes a canonical recipe manifest. Its structural fingerprint covers values, types, attributes, encodings, classes, dimensions, ALTREP intent, and RNG state; the recipe also records its mutation policy. Every runner receives the same manifest path and digest, regenerates each recipe, and rejects a mismatch before correctness.

## Correctness validation

Before any runner records timing, a validation-only pass checks all selected rows. Every native task is validated against the declared R reference or its manifest result contract, and every executable R row validates its own result contract. The native path and reference receive separate deterministic argument instances. The return value comparison checks type, attributes, recursive list structure, numeric tolerance, exact missing-value kind (NA versus NaN), and character encoding. Reference errors, native errors, missing references, contract mismatches, input mutation, registration errors, artifact drift, and value mismatches stop the run before timing.

Correctness, reference, cold, warmup, and timed phases use separate instances. Immutable inputs may be reused within warmup or timed sampling only after a mutation probe passes. Sort and attribute mutation receive fresh arguments outside every timed sample, RNG work resets the declared stream outside every sample, and external-state methods construct a fresh registered receiver outside every sample. Fixture construction, duplication, and reset work are not timed.

Each summary and raw timing file records `correctness_status`, `correctness_policy`, `correctness_message`, and the effective `call_type`; `.Call` and `.External` rows are therefore not conflated. Summaries also record run and task seeds, input fingerprint, contract and kernel identity, path and evidence class, representation and mutation policy, R provenance and source digest, tool identity, generated-glue kind and digest, repository source, build, dependency, linked-library, configured-artifact, and per-row source-verification digests, plus the normalized disposition. `PASS` permits timing, `REFERENCE` identifies a validated pure-R row, and `NOT_VALIDATED` is never eligible for comparative aggregation. A correctness or timing `FAIL` causes the runner subprocess and parent run to fail; it cannot become a complete, exportable, or promotable run. `N/A` requires the exact runner-specific non-executable disposition and its normalized reason. Native-only tasks use structural result contracts from `task_manifest.csv`.

The R runner no longer times substitute stubs for tasks 07a through 11 or task 42, and it does not time the native-state method rows 72 and 73. Those rows are explicit unrepresentable dispositions. Tasks 48 and 49 remain zigr-specific gaps. Native runners use structural invariants for tasks 07a through 11, 42, 48, and 49. Task 43 compares an exact deterministic vector and the resulting R RNG state. The method rows retain a small pure-R value invariant for native correctness without presenting that invariant as a timed native-state implementation.

## Pipeline

```sh
# Build all native runners
bash build_all.sh

# Re-run the existing harness registration and task-coverage preflight for cpp11
Rscript runner_subprocess.R --runner=cpp11 --check-only

# Run all configured runners into a unique results/runs/<run_id> directory
Rscript run_benchmarks.R

# Quick subset
Rscript run_benchmarks.R --runners=zigr,c_call
Rscript run_benchmarks.R --tasks=1,2,6
Rscript run_benchmarks.R --runners=r,c_call,zigr --tasks=50,51,52,53
Rscript run_benchmarks.R --runners=r,zigr --tasks=52 --seed=20260713
Rscript run_benchmarks.R --build    # rebuild then run

# Export a complete focused boundary run
Rscript export_boundary_metrics.R --run-dir=results/runs/<run_id>

# Promote a completed full-matrix run explicitly
Rscript promote_run.R --run-dir=results/runs/<run_id>

# Summarize one completed run
Rscript analysis/summarize.R --run-dir=results/runs/<run_id>

# Run system diagnostics (zigr-only)
Rscript run_system_tasks.R
```

`run_benchmarks.R` creates a schema-2 run manifest with an environment-schema-3 tool and source ledger plus the canonical input recipe artifact. It runs every selected runner in validation-only mode and only then starts measured runner subprocesses. It validates the completed artifacts and exports comparisons from that one directory. Failed or interrupted runs remain `incomplete` and cannot be exported or promoted; a later project run marks `running` manifests older than six hours as incomplete while retaining their artifacts. Existing root-level CSVs are legacy evidence.

Each run manifest records a source-tree digest, host and CPU identity, exact R, Zig, Rust, Cargo, C, and C++ identities, declared targets and profiles, compile and link flags, R headers and library, BLAS and LAPACK, package and crate sources, generated glue, transitive shared libraries, runner configuration, all configured artifact fingerprints, the canonical input identity, per-row R source and backend provenance, and one normalized disposition and source classification per selected runner and task. Set `ZIGR_TARGET`, `ZIGR_OPTIMIZE`, `ZIGR_CPU_FEATURES`, and `ZIGR_CHECKED_SEXP` when the build differs from the native `ReleaseFast` defaults.

## Output files

The pipeline writes per-task timing CSVs, per-runner summaries, a run manifest, and cross-runner comparisons under one run directory. Comparative export also writes `task_comparisons.csv` with noise and median-interval fields plus `category_metrics.csv`. Boundary export writes `boundary_metrics.csv`, `boundary_budgets.csv`, and `representation_budgets.csv`. `analysis/summarize.R` writes `analysis_summary.csv` beside the selected run.

## Generated boundary budgets

The boundary budget baseline is complete focused run `20260711T232455Z-pid2`: rebuilt ReleaseFast libraries, native target, Zig 0.16.0, R 4.6.1, budget policy `2026-07-11-1`, and the same adaptive policy as the canonical suite. C passed all 26 boundary rows and reported the 11 zigr-only representation rows as `N/A`; R and zigr passed all 37 selected rows. This run locks generated-boundary budgets but does not replace the six-runner primary aggregate below.

Most small boundary pairs are below the 0.01 ms timer floor, so their ratios remain visible but do not decide acceptance. The large ordinary numeric pair is above the floor and low-noise: generated `0.08410 ms`, handwritten `0.08332 ms`, absolute overhead `0.00078 ms`, ratio `1.00936x`, and CV 7.48%/9.02%. It passes the `0.01 ms` and `1.10x` limits. The integer ALTREP rows use different ownership strategies, so the generated `0.31285 ms` contiguous copy is budgeted on its own and is not judged against the handwritten region stream.

The category rows below publish the generated warm median beside the separately measured cold call and endpoint RSS. The first zero-argument call includes process-local lazy initialization, so its `0.836 ms` cold value and 188 KiB endpoint delta are not treated as steady-state wrapper cost. Every other endpoint delta was zero. RSS is still a post-GC endpoint observation, not peak memory.

| Boundary category   | Warm median | Absolute delta vs handwritten | Cold call | Endpoint RSS | Samples | Comparison          |
| ------------------- | ----------: | ----------------------------: | --------: | -----------: | ------: | ------------------- |
| Zero argument       | 0.001680 ms |                   0.000030 ms |  0.836 ms |      188 KiB |     500 | Below floor         |
| Scalar              | 0.001720 ms |                   0.000110 ms |  0.017 ms |        0 KiB |     500 | Below floor         |
| Optional `NULL`     | 0.001710 ms |                   0.000160 ms |  0.016 ms |        0 KiB |     500 | Below floor         |
| Optional typed `NA` | 0.001760 ms |                   0.000125 ms |  0.016 ms |        0 KiB |     500 | Below floor         |
| Small numeric       | 0.001810 ms |                   0.000220 ms |  0.016 ms |        0 KiB |     500 | Below floor         |
| Large numeric       | 0.084100 ms |                   0.000780 ms |  0.108 ms |        0 KiB |     500 | Eligible            |
| Integer ALTREP      | 0.312850 ms |                   0.202570 ms |  0.396 ms |        0 KiB |     500 | Different ownership |
| String view         | 0.002130 ms |                   0.000460 ms |  0.018 ms |        0 KiB |     500 | Below floor         |
| Raw                 | 0.001780 ms |                   0.000020 ms |  0.018 ms |        0 KiB |     500 | Below floor         |
| Complex             | 0.001740 ms |                   0.000140 ms |  0.033 ms |        0 KiB |     500 | Below floor         |
| Fixed schema        | 0.001940 ms |                   0.000150 ms |  0.017 ms |        0 KiB |     500 | Below floor         |
| Typed method        | 0.001815 ms |                   0.000135 ms |  0.016 ms |        0 KiB |     500 | Below floor         |
| `.External`         | 0.001770 ms |                   0.000150 ms |  0.017 ms |        0 KiB |     500 | Below floor         |

| Class                        | Current maximum generated median |  Budget | Eligible overhead current / budget | Eligible ratio current / budget |
| ---------------------------- | -------------------------------: | ------: | ---------------------------------: | ------------------------------: |
| Safe wrapper                 |                       0.00177 ms | 0.01 ms |                                N/A |                             N/A |
| Scalar and optional          |                       0.00176 ms | 0.01 ms |                                N/A |                             N/A |
| Borrowed numeric/raw/complex |                       0.08410 ms | 0.12 ms |                  0.00078 / 0.01 ms |                1.00936x / 1.10x |
| Copied ALTREP                |                       0.31285 ms | 0.40 ms |                                N/A |                             N/A |
| String view                  |                       0.00213 ms | 0.01 ms |                                N/A |                             N/A |
| Fixed schema                 |                       0.00194 ms | 0.01 ms |                                N/A |                             N/A |
| Typed method                 |                       0.00182 ms | 0.01 ms |                                N/A |                             N/A |

Representation budgets retain setup work and ownership differences instead of combining them into one score:

| Task                        | Current median |  Budget | Signal                 |
| --------------------------- | -------------: | ------: | ---------------------- |
| String view, one pass       |     0.38325 ms | 0.50 ms | above floor, CV 4.55%  |
| String cache build          |     0.96667 ms | 1.20 ms | above floor, CV 5.85%  |
| String cache plus one pass  |     0.98778 ms | 1.25 ms | above floor, CV 5.67%  |
| String headers, one pass    |     0.55555 ms | 0.70 ms | above floor, CV 6.61%  |
| String view, four passes    |     1.54981 ms | 1.90 ms | above floor, CV 3.19%  |
| String cache, four passes   |     1.05715 ms | 1.35 ms | above floor, CV 7.00%  |
| String headers, four passes |     0.57154 ms | 0.75 ms | above floor, CV 6.37%  |
| Raw view                    |     0.00714 ms | 0.01 ms | below floor, CV 79.07% |
| Raw copy                    |     0.15097 ms | 0.20 ms | above floor, CV 16.53% |
| Complex view                |     0.02928 ms | 0.05 ms | above floor, CV 40.63% |
| Complex return              |     0.04378 ms | 0.06 ms | above floor, CV 36.68% |

Every row reached the 500-sample cap rather than the 1% rolling-CV stop. New runs store full-precision samples; summaries remain rounded for display, while intervals, CV classification, and budget decisions use the stored samples. The reports retain exact median intervals, timer-floor status, cold-call time, endpoint RSS, sample count, and stopping condition. Complex view and return remain noisy diagnostics even though their medians are above the timer floor. Error, longjmp, finalizer, and forced-GC behavior remain runtime safety gates, not performance budgets.

ReleaseSafe allocation diagnostics remain outside these ReleaseFast timings. The ordinary numeric view recorded zero allocator calls. A 1,024-element compact integer ALTREP recorded one 4,096-byte allocation and one matching free. A three-element cached string view recorded one metadata allocation and one matching free. Fixed scalar-schema parsing recorded zero allocator calls. The cleanup diagnostic recorded one cleanup frame, one unwind boundary, and protection depth one; separate runtime cases exercise the full 16-frame capacity. Forced-GC ownership tests cover generated numeric, raw, string, cached-string, fixed-schema, call-expression, fixed-tier, and spilled results. Typed external-pointer tests require one finalization and verify an idempotent second finalizer call. These are exact safety assertions from the then-current 238-test ReleaseSafe runtime suite, not inferred counts from RSS.

### SEXP ABI comparison

The primary budgets stay on the direct contract, and I measure the checked fallback separately. Primary ReleaseFast run `20260712T045623Z-pid908112` passed every boundary and representation budget through the fail-closed exporter. Forced `checked_r_api` run `20260712T050028Z-pid912770` passed correctness for all 37 selected R and zigr rows; C passed its 26 shared rows and reported the representation rows as declared `N/A`. Both runs record source digest `b4f9fb75ff710215c49ebd1ada149411` and their explicit ABI selection.

| Path | Direct median | Checked median | Checked/direct |
| --- | ---: | ---: | ---: |
| Large generated numeric | 0.08407 ms | 0.08418 ms | 1.001x |
| String view, one pass | 0.36254 ms | 0.58905 ms | 1.625x |
| String cache build | 0.93618 ms | 0.96640 ms | 1.032x |
| String cache plus one pass | 0.96523 ms | 0.99134 ms | 1.027x |
| String headers, one pass | 0.54001 ms | 0.54853 ms | 1.016x |
| String view, four passes | 1.40232 ms | 2.30151 ms | 1.641x |
| String cache, four passes | 1.03172 ms | 1.06125 ms | 1.029x |
| String headers, four passes | 0.56008 ms | 0.57281 ms | 1.023x |
| Raw copy | 0.15131 ms | 0.15109 ms | 0.999x |
| Complex view | 0.02930 ms | 0.02921 ms | 0.997x |
| Complex return | 0.03944 ms | 0.03687 ms | 0.935x |

The header-free string view is the material fallback cost. `checked.vectorElt` validates kind and bounds for each element, so repeated uncached access pays for repeated R API calls. Cached metadata and copied header paths decide representation once and remain near the direct path. I keep this cost visible rather than weakening fallback validation. The raw-view row remains below the timer floor and is omitted from the ratio table.

## Results (canonical baseline)

The current canonical run completed on 2026-07-10 with `ReleaseFast`, native target, Zig 0.16.0, R 4.6.1, and single-threaded OpenBLAS. All six runners passed all 44 tasks: `264 PASS, 0 FAIL, 0 N/A`. The primary aggregate contains 36 manifest-approved tasks; eight strategy-sensitive or nondeterministic tasks remain visible in task reports but are excluded from aggregate ratios.

This section preserves the historical contract and result as published. It predates the current seven-runner classification, repaired task contracts, and deterministic task-43 validation and must not be relabeled as current evidence.

### Comparative metrics

Ratios are runner median divided by the reference median; lower is better. Aggregate ratios use only the 36 manifest-approved tasks.

| Runner  | Tasks won | Median vs R | Geomedian vs R | Median vs best native | Geomedian vs best native | Meaningful native wins | Low-noise wins |
| ------- | --------- | ----------- | -------------- | --------------------- | ------------------------ | ---------------------- | -------------- |
| r       | 5         | 1.000       | 1.000          | NA                    | NA                       | NA                     | NA             |
| c_call  | 1         | 0.398       | 0.299          | 1.125                 | 1.525                    | 0                      | 0              |
| rcpp    | 1         | 0.397       | 0.303          | 1.151                 | 1.548                    | 1                      | 1              |
| extendr | 3         | 0.389       | 0.321          | 1.212                 | 1.638                    | 2                      | 1              |
| savvy   | 9         | 0.388       | 0.283          | 1.100                 | 1.445                    | 0                      | 7              |
| zigr    | 17        | 0.263       | 0.212          | 1.003                 | 1.082                    | 16                     | 10             |

`Tasks won` includes exact median ties. The low-noise set contains 19 aggregate tasks with CV <= 20%. zigr's aggregate result is approximately 3.8x faster than R by median and 4.7x by geomedian, while remaining 1.003x slower by median and 1.082x slower by geomedian than the per-task best native runner.

### Category results for zigr

| Category      | Tasks | Aggregate tasks | Low-noise aggregate tasks | Median vs R | Geomedian vs R | Median vs best native | Geomedian vs best native |
| ------------- | ----: | --------------: | ------------------------: | ----------: | -------------: | --------------------: | -----------------------: |
| kernels       |    11 |              11 |                         8 |       0.297 |          0.222 |                 1.110 |                    1.110 |
| boundary      |    13 |              13 |                         3 |       0.166 |          0.108 |                 1.043 |                    1.043 |
| altrep        |     8 |               8 |                         6 |       1.232 |          0.519 |                 1.149 |                    1.116 |
| r_runtime     |     6 |               4 |                         2 |       0.975 |          0.805 |                 1.010 |                    1.065 |
| synthetic_api |     6 |               0 |                         0 |          NA |             NA |                    NA |                       NA |

### Task-level wins and parity

zigr is the fastest native runner on 17 aggregate tasks: `01_vectorsum`, `04_sort`, `06_broadcast`, `13_matrix_rowsums`, `14_matrix_rowcol_means`, `15_dataframe_filter`, `16_list_access`, `17_string_concat`, `18_string_nchar`, `19_string_encoding`, `20_factor_ops`, `22_s4_slot_access`, `23_na_propagation`, `25_l1_arithmetic`, `26_matmul`, `29_lm_fit`, and `33_altrep_region_read`.

### Material losses and low-noise review

| Task                  | Best native | zigr vs best | Signal                        |
| --------------------- | ----------- | -----------: | ----------------------------- |
| 03_memcpy_bandwidth   | extendr     |       1.868x | noisy, CV 42.92%              |
| 21_attrib_ops         | savvy       |       1.429x | below timer floor, CV 151.69% |
| 05_fib_recursive      | rcpp        |       1.424x | low-noise, CV 6.69%           |
| 38_struct_convert     | extendr     |       1.258x | below timer floor, CV 147.91% |
| 30_altrep_create      | savvy       |       1.207x | below timer floor, CV 156.81% |
| 34_altrep_sum_via_R   | c_call      |       1.182x | below timer floor, CV 133.75% |
| 32_altrep_elt_walk    | extendr     |       1.156x | low-noise, CV 3.27%           |
| 36_altrep_min_max     | savvy       |       1.150x | low-noise, CV 5.21%           |
| 35_altrep_sum_native  | savvy       |       1.149x | low-noise, CV 3.84%           |
| 02_elem_ops           | c_call      |       1.148x | noisy, CV 35.92%              |
| 24_long_vector_idx    | savvy       |       1.145x | noisy/below floor, CV 60.29%  |
| 37_altrep_no_na_query | savvy       |       1.101x | low-noise, CV 9.06%           |

The remaining low-noise losses are near parity: `39_r_eval` (1.001x), `31_altrep_materialize` (1.005x), and `28_cholesky` (1.007x). `09_longjmp_safety` and `43_rng_stress` are also low-noise task-level losses but are excluded because their strategies or outputs are not directly comparable. Task `07b_protect_scaling` is not evidence about binding design: zigr executes the protection loop, while the Savvy fixture returns a scalar without performing it. The task remains excluded, and its cross-runner timing must not be interpreted.

### Exclusions and skipped correctness comparisons

| Tasks      | Policy           | Reason                                                               |
| ---------- | ---------------- | -------------------------------------------------------------------- |
| 07a, 07b   | native invariant | R API protection loops have no shared R semantic reference           |
| 08, 10, 11 | native invariant | Direct SEXP inspection/allocation has no shared R semantic reference |
| 09         | native invariant | Native error and unwind strategies differ                            |
| 42         | native invariant | Pointer identity and finalization are not shared R values            |
| 43         | nondeterministic | Random output is not compared by value                               |

These tasks still passed their declared structural or native-invariant correctness contracts and remain available in `task_comparisons.csv`; they simply do not define the primary aggregate.

### System diagnostics

These tasks are zigr-only, not comparative. They measure build and memory characteristics of the zigr backend. Run via `run_system_tasks.R`.

The table below is retained diagnostic evidence; the canonical run covers the 44-task comparison and does not rerun these standalone checks.

| Task                  | Metric                          | Result                |
| --------------------- | ------------------------------- | --------------------- |
| 44_build_time         | cold build (clean cache)        | 2.41s                 |
| 44_build_time         | warm build (no changes)         | 0.08s                 |
| 44_build_time         | incremental (one file modified) | 0.86s                 |
| 45_binary_size        | x86_64-linux .so                | 179 KB                |
| 45_binary_size        | aarch64-linux .so               | N/A (link fails)      |
| 45_binary_size        | x86_64-windows .dll             | N/A (link fails)      |
| 46_cross_compile_time | x86_64-linux (warm from t45)    | 1.04s                 |
| 46_cross_compile_time | aarch64-linux (cold)            | 0.42s                 |
| 46_cross_compile_time | x86_64-windows (cold)           | 0.06s (lib not found) |
| 46_cross_compile_time | aarch64-macos (cold)            | 0.13s (LTO needs LLD) |
| 47_mem_alloc_count    | vectorsum (10M)                 | 1 alloc, 80 MB        |

Cross-target .so sizes are N/A because R and BLAS shared libraries on this host are x86_64 binaries, incompatible with foreign targets. zigr's compilation step succeeds for aarch64-linux (the translate-c output is target-independent); linking fails because native `.so` files cannot be linked into cross-target output. A full cross-compilation pipeline would require R headers and libraries for each target.

Task 47 is a standalone allocator smoke test. It counts allocations made by two synthetic Zig kernels through `c_allocator`; it does not measure the generated R boundary, R heap allocation, arena spill, or external-pointer creation. Boundary allocation diagnostics use `memory.CountingAllocator` in the ReleaseSafe runtime suite and remain outside canonical ReleaseFast timings.

### Cross-compilation status for CI

Zig can cross-compile the zigr source for any LLVM target from any host. The translate-c step produces target-independent output from the R headers. The practical limitation is linking: the shared library must link against `libR.so` (or `.dylib`/`.dll`) for the target platform, which requires either:

- A native build on each target (R installed), or
- Target-specific R libraries available on the build host

A GitHub Actions workflow building zigr on Linux (x86_64), macOS (aarch64), and Windows (x86_64) is feasible: each runner installs R via its package manager, then runs `zig build`. Cross-compilation from a single Linux host can verify compilation for all targets but cannot produce loadable `.so` files without target-specific R libraries.

## Methodology

- Timing: 10 warm-up iterations, then `microbenchmark` blocks of 10. The runner stops when CV over the last 5 blocks falls below 1%, or at 500 measured iterations; summaries record the warmups, block size, measured sample count (`n_iterations`), stopping condition, and stopping-window CV.
- Timer floor: medians below the fixed 0.01 ms floor are labeled `below_floor`; the floor is a reporting label and does not force extra repeats.
- RSS: post-GC VmRSS endpoint delta (max 0, after - before), read from `/proc/self/status VmRSS`; it is not a peak-memory measurement.
- Cold start: one post-correctness call before warm-up, logged separately with the run ID; it is a call cold-start measure, not process launch time.
- GC control: full GC before warm-up and both RSS endpoints; no forced GC occurs between timed samples.
- Allocation: Task 47's standalone allocator counts remain a separate diagnostic; RSS is never used as an allocation or timing substitute.
- BLAS: single-threaded (`OPENBLAS_NUM_THREADS=1`)
- Low-noise threshold: CV <= 20% across all runners
- Meaningful margin: gap must exceed 5% (ratio > 1.05)
- Confidence intervals: exact 95% order-statistic intervals for per-task medians, computed from existing samples without rerunning tasks; task-level ratio bounds use those intervals, while aggregate rankings remain descriptive over the fixed manifest task set.
- Category reports: `category_metrics.csv` groups results into kernels, boundary, ALTREP, R runtime, and synthetic API categories.
