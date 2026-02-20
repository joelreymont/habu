# Maxima Loader

`lib/maxima-loader.lisp` provides a staged loader for Maxima source files from `/tmp/maxima/src`.

## Files

- `lib/maxima-stubs.lisp`: package/function stubs needed before Maxima core modules load.
- `lib/maxima-loader.lisp`: ordered file list + `maxima-load-all` entrypoint.

## Usage

Load definitions only:

```lisp
(load "lib/maxima-loader.lisp")
```

Run the loader explicitly:

```lisp
(maxima-load-all)
```

Run with internal/system-only controls:

```lisp
(maxima-load-all
  :files '("lmdcls" "letmac")
  :verbose nil
  :habu-stop-on-error t
  :habu-trace t
  :habu-required-bindings '(maxima::partition maxima::$integrate))
```

## Notes

- Loader is intentionally manual-entrypoint only; it does not auto-run on file import.
- Loader auto-detects source roots from:
  - `/tmp/maxima/src/`
  - `/tmp/maxima/src/src/`
  - `/tmp/maxima/`
- Recent package-system fixes removed the prior `StackOverflow` path in deep
  Maxima loads (`db.lisp`, `compar.lisp`, `limit.lisp`) by correcting
  `defpackage`/`shadow`/inherited-symbol behavior.
- Default runtime heap was raised to 256MB (`src/main.zig`, `src/runtime/heap.zig`) for larger Maxima workloads.
- For live `$integrate` execution (not just `fboundp` checks), the working
  subset must include `suprv1`, `sinint`, and `sin` in addition to the
  `schatc` dependency chain (`m2`/`schatchen-cond`).
- Internal/system-only keyword controls are supported on `maxima-load-all`:
  - `:habu-stop-on-error` stops at first failed module load.
  - `:habu-trace` prints per-module source path traces.
  - `:habu-reset-context` toggles `MAXIMA::CONTEXT` realignment.
  - `:habu-required-bindings` computes missing function/macro bindings and
    returns them as an extra value (`*maxima-last-missing-bindings*`).
  - `:verbose` toggles summary/log printing.
- `(load ...)` now aborts on the first unhandled form error in a file instead
  of silently skipping failed forms. This keeps per-file loader failure counts
  and missing-binding reports trustworthy.

## Troubleshooting

- If a large-file load regresses, first validate package identity invariants:
  `(eq (find-symbol "FUNCTIONP" "MAXIMA") (find-symbol "FUNCTIONP" "COMMON-LISP"))`
  must be `nil` after `lib/maxima-stubs.lisp` is loaded.
- Integration tests in `src/tests/integration.zig` skip Maxima-specific gates
  when `/tmp/maxima/src/lmdcls.lisp` is missing (no Maxima source checkout).
- `maxima-load-all` now fails fast with a single summary when the detected
  root lacks `lmdcls.lisp`, instead of emitting per-file `FileNotFound` spam.
- `maxima-load-all` returns additional internal values after `(ok total fail)`:
  missing requested bindings, then attempted file count.
- Keep stream formwise probes (`with-open-file` + `read`) as advisory only
  until stream `read` semantics are fully fixed.

## Perf Gates

- GC parity CI uses `.github/workflows/gc-parity.yml` and runs:

```bash
tools/gc-compare --json --iters=20 --live-mb=8 --heap-mb=64 \
  --gate-level=milestone_2x_from_baseline \
  --regression-baseline=bench/gc-regression-baseline.json \
  --regression-slack=0.15 \
  --fail-on-regressions
```

- Self-improvement loop for Maxima + micro workloads:

```bash
tools/perf-loop --json --iters=1 --scale=1 --gc-iters=30 --gc-runs=3
```
