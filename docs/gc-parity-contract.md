# GC Parity Contract

This document defines the machine-checkable GC parity contract for Habu.

## Scope

- Runtime comparison target: Habu vs SBCL (`tools/gc-compare`).
- Optional secondary target: OCaml (enabled when OCaml runner commands are configured).
- Workload class: GC stress allocation workload plus optional Maxima telemetry.

## Gate Types

1. Absolute parity gates (`--gate-level`)
- Levels: `milestone_2x_from_baseline`, `competitive`, `parity`.
- Evaluated from `gates.vs_sbcl.<level>` in `tools/gc-compare` output.
- `--fail-on-gates` exits non-zero when the selected level fails.

2. Regression gates (`--fail-on-regressions`)
- Compared against `bench/gc-regression-baseline.json`.
- Metrics enforced:
  - `avg_pause_ratio` (higher is better)
  - `p95_pause_ratio` (higher is better)
  - `throughput_ratio` (higher is better)
  - `rss_ratio` (lower is better)
- Slack: `--regression-slack=<fraction>` (default `0.15`).
- Exit status: non-zero when any regression check fails.

## Canonical Commands

Local parity snapshot:

```bash
tools/gc-compare --json --iters=20 --live-mb=8 --heap-mb=64 \
  --gate-level=milestone_2x_from_baseline
```

Local regression gate:

```bash
tools/gc-compare --json --iters=20 --live-mb=8 --heap-mb=64 \
  --gate-level=milestone_2x_from_baseline \
  --regression-baseline=bench/gc-regression-baseline.json \
  --regression-slack=0.15 \
  --fail-on-regressions
```

Build-system entrypoint:

```bash
zig build gc-parity
```

## CI Enforcement

Workflow: `.github/workflows/gc-parity.yml`

- Runs `tools/gc-compare` with regression gating flags.
- Always uploads `bench/results/gc-parity-ci.json`.
- Fails the job when regression gate fails.

## Output Fields

`tools/gc-compare --json` publishes:

- `metrics`: primary ratio and telemetry values.
- `gates`: absolute gate evaluations (`vs_sbcl`, optional `vs_ocaml`).
- `parity_diff`: selected-gate per-metric deltas to target.
- `trend`: CI-consumable per-metric series for selected gate.
- `regression`: baseline/slack regression checks and pass/fail.
- `runners`: normalized runtime runner payloads and statuses.

## Interpretation

- `gate_pass == false` with `regression.pass == true`:
  - No immediate regression; still below long-term parity objective.
- `regression.pass == false`:
  - Immediate performance regression; CI failure is expected.
