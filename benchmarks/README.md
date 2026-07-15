# zigr benchmark framework

This directory measures zigr against R and native integration methods while keeping product comparisons separate from language controls and diagnostics. A timing is publishable only after source, artifact, input, correctness, lifecycle, and measurement identities pass the trust gates.

The framework has seven backends: R, registered C, zigr, Rcpp, cpp11, extendr, and Savvy. Not every historical task is a product comparison. The normalized fixture suite is the primary cross-product evidence; the historical task matrix remains visible as control and diagnostic evidence.

## Benchmark suites

### Normalized product fixtures

F01 through F12 exercise package-shaped public APIs for zigr, Rcpp, cpp11, extendr, and Savvy, with R references and registered-C controls.

- F01: zero-argument output
- F02: scalar conversion
- F03: numeric vector conversion
- F04: compact integer ALTREP access
- F05: encoded and missing strings
- F06: raw vectors
- F07: complex vectors
- F08: logical values and missingness
- F09: fixed-schema objects
- F10: external state and methods
- F11: invalid-input recovery
- F12: multi-output construction and lifecycle behavior

F01 through F10 and F12 can be timed after correctness validation. F11 is correctness-only. Unsupported product paths remain explicit gaps; the framework does not replace them with raw FFI substitutes.

### Historical task matrix

The task manifest contains 83 rows:

- Tasks 01–43: historical compute, R API, object, numerical, ALTREP, and runtime controls.
- Tasks 48–49: zigr lifecycle diagnostics.
- Tasks 50–75: generated and handwritten boundary pairs.
- Tasks 76–86: string, raw, and complex representation strategies.

The historical Rcpp, extendr, and Savvy runners are implementation diagnostics, not complete product-package comparisons. Product conclusions must come from Tier A or disclosed Tier B evidence.

C historical kernels are consolidated in `src/c_call/tasks.c`. Zig historical kernels are consolidated in `src/zig/tasks.zig`; each Zig task retains a private namespace inside that module.

## Evidence tiers

- Tier A: matching product paths, contracts, kernels, representations, and setup.
- Tier B: valid product paths with a disclosed strategy or adapter difference.
- Tier C: R baselines and registered-C controls.
- Tier D: implementation diagnostics.
- Gap: unsupported or non-meaningful cells.

`evidence_manifest.json` expands into every runner/task and runner/fixture disposition. Timing eligibility is derived from those dispositions and checked again immediately before measurement.

## Maintained configuration

| File | Ownership |
| --- | --- |
| `task_manifest.csv` | Task IDs, workload groups, result contracts, correctness policy, and aggregate eligibility |
| `runners.json` | Runner libraries, call types, registered packages, fixtures, and executable symbol maps |
| `evidence_manifest.json` | Comparison roles, tiers, strategies, applicability, gaps, and ownership |
| `source_ledger.json` | Source sets, generated glue, build recipes, tools, and verification policy |
| `results/CANONICAL_RUN.json` | Compact tracked receipt for the accepted run |

These files intentionally remain separate: execution configuration, comparison policy, and provenance have different owners and drift rules.

## Framework map

| File | Responsibility |
| --- | --- |
| `lib/specification.R` | Task recipes, manifests, evidence expansion, budgets, and report contracts |
| `lib/measurement.R` | Deterministic inputs, bounded pilot sizing, fixed timing, raw samples, RSS, and fixture validation |
| `lib/provenance.R` | Source verification, toolchains, generated glue, environment, and artifact identity |
| `lib/run_manifest.R` | Run state, completion seals, retention, and artifact validation |
| `lib/product_fixtures.R` | Product package gates, semantic cases, lifecycle checks, and capability gaps |
| `run_benchmarks.R` | Correctness, bounded batch scheduling, timeouts, consolidation, and run-state transitions |
| `benchmark_worker.R` | Fixed-count task or fixture batch execution selected with `--kind` |
| `export_comparative_metrics.R` | Current full-matrix evidence reports |
| `export_boundary_metrics.R` | Boundary and representation regression budgets |
| `promote_run.R` | Acceptance validation and compact receipt creation |
| `check_coverage.R` | No-native trust suite and task preflight |

## Build and validate

```sh
# Build runners and normalized fixture packages
bash build_all.sh

# Run the full no-native trust suite
Rscript check_coverage.R

# Validate a selected task without measuring it
Rscript benchmark_worker.R --kind=task --runner=cpp11 --tasks=52 --check-only

# Remove native build products and caches; retained results are untouched
bash build_all.sh clean
```

## Collect a run

```sh
# Full seven-runner timed matrix
Rscript run_benchmarks.R

# Rebuild before collection
Rscript run_benchmarks.R --build

# Retain correctness evidence without timing
Rscript run_benchmarks.R --correctness-only

# Focused diagnostic run
Rscript run_benchmarks.R --runners=r,zigr --tasks=48,49

# Historical tasks only
Rscript run_benchmarks.R --suite=tasks --runners=r,zigr --tasks=48,49

# Normalized product fixtures only
Rscript run_benchmarks.R --suite=fixtures --runners=r,zigr
```

Every selected task receives a deterministic seed derived from the run seed, task ID, and fixture version. Correctness, cold-start, warmup, and timed phases receive isolated inputs. Mutation, RNG, external state, ALTREP intent, and input fingerprints are enforced by policy.

Timed collection uses one equal-floor pilot for every eligible comparison group, then freezes one symmetric fixed-count confirmation stage. Group and tool order are reproducible, batches and the total run have declared limits, and a timed-out group receives at most one smaller retry while later batches continue. Pilot evidence remains diagnostic and cannot produce a comparative claim. `--suite=tasks`, `--suite=fixtures`, and the default `--suite=all` use the same workers, timing policy, and artifact validators.

Single-suite, runner-filtered, task-filtered, and correctness-only runs are diagnostic evidence and record `promotion_eligible` as `false`. Promotion requires an unfiltered timed `--suite=all` run with the complete runner and task matrix.

## Export and promotion

```sh
Rscript export_comparative_metrics.R --run-dir=results/runs/<run_id>
Rscript export_boundary_metrics.R --run-dir=results/runs/<run_id>
Rscript promote_run.R --run-dir=results/runs/<run_id> --dry-run
Rscript promote_run.R --run-dir=results/runs/<run_id>
```

Comparative export requires the current complete runner and task matrix. It writes separate product, strategy, R-baseline, control, diagnostic, capability, safety, and analysis reports. Boundary export writes boundary measurements and regression-budget decisions.

Promotion accepts only an unfiltered timed run collected under the current source tree, evidence, timing policy, and boundary budget policy. It regenerates reports in isolation, reruns safety proof, and updates only `results/CANONICAL_RUN.json`.

## Result retention

Raw samples and generated reports are local and ignored by Git. Schema-3 runs declare the `grouped-v1` artifact layout. A timed `--suite=all` run retains two shared sample tables, two shared summary tables, two shared correctness tables, the input manifest, and the run manifest: eight core files regardless of runner, task, fixture, phase, or iteration count. A focused suite publishes only its own summary, correctness, and optional sample table; task runs also retain their canonical input manifest. Runner, task, and fixture identities are columns, and cold-start observations are rows in the task samples rather than separate files. Schema-2 runs remain readable through their original per-cell layout.

Run manifests retain execution-critical dispositions and provenance digests. Detailed policy remains in the source-sealed manifests and canonical input artifact instead of being copied into every run record.

Keep only the runs needed for active investigation:

```sh
Rscript run_benchmarks.R --prune-runs=3
```

The accepted run ID is protected from pruning. Build cleanup and run retention are deliberately separate operations.

## Tests

The trust gate has three responsibility-based suites:

- `tests/test_specification.R`: manifests, evidence, reports, source policy, configuration, and adversarial drift.
- `tests/test_measurement.R`: deterministic fuzz, phase isolation, timing samples, run seals, and retained-artifact drift.
- `tests/test_product_fixtures.R`: product semantics, invalid inputs, metadata, lifecycle behavior, and honest gaps.

A benchmark failure, missing artifact, source drift, invalid disposition, input mutation, registration error, or correctness mismatch prevents completion and promotion.

## Platform boundary

Exact linked-library identity currently uses Linux `ldd`. Unsupported hosts fail closed instead of emitting partial provenance.
