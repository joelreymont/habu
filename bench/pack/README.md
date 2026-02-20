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

- Habu + SBCL workloads are wired through shared adapters in `tools/bench_pack_runner.py`.
- OCaml workload/GC adapters are implemented as command-template hooks.

## Normalized Runner Output

`tools/bench_pack_runner.py` normalizes runtime runs to:
- `status` / `error`
- `workload_order`
- `workloads.<name> = {name, status, ns, error}`
- `payload` (raw runtime JSON)
- `rss_bytes` (for GC runs when available)

## OCaml Command Hooks

Set runner commands with environment variables:
- `HABU_OCAML_MICRO_CMD`
- `HABU_OCAML_MAXIMA_CMD`
- `HABU_OCAML_GC_CMD`

Templates may use:
- `{iters}`
- `{scale}`
- `{heap_mb}`
- `{nursery_mb}`
- `{live_mb}`
