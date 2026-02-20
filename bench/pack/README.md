# Bench Pack Corpus

`bench/pack/corpus.json` is the shared workload contract for cross-runtime benchmarking.

## Goals

- Define one canonical workload list for micro + Maxima suites.
- Keep benchmark naming consistent across Habu, SBCL, and OCaml runners.
- Let runner tooling consume one manifest instead of hard-coded name lists.

## Schema

- `schema_version`: format version.
- `baseline_date`: parity baseline date.
- `suites.<suite>.source`: authoritative harness files.
- `suites.<suite>.runner_targets`: expected runtime targets.
- `suites.<suite>.workloads[]`: workload entries (`name`, `category`).

## Current Status

- Habu + SBCL workloads are wired to existing harnesses.
- OCaml target is defined in the corpus and is implemented in the next bench-pack runner dot.
