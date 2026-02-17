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

## Notes

- Loader is intentionally manual-entrypoint only; it does not auto-run on file import.
- Recent package-system fixes removed the prior `StackOverflow` path in deep
  Maxima loads (`db.lisp`, `compar.lisp`, `limit.lisp`) by correcting
  `defpackage`/`shadow`/inherited-symbol behavior.
- Default runtime heap was raised to 256MB (`src/main.zig`, `src/runtime/heap.zig`) for larger Maxima workloads.
- For live `$integrate` execution (not just `fboundp` checks), the working
  subset must include `suprv1`, `sinint`, and `sin` in addition to the
  `schatc` dependency chain (`m2`/`schatchen-cond`).

## Troubleshooting

- If a large-file load regresses, first validate package identity invariants:
  `(eq (find-symbol "FUNCTIONP" "MAXIMA") (find-symbol "FUNCTIONP" "COMMON-LISP"))`
  must be `nil` after `lib/maxima-stubs.lisp` is loaded.
- Integration tests in `src/tests/integration.zig` skip Maxima-specific gates
  when `/tmp/maxima/src/lmdcls.lisp` is missing (no Maxima source checkout).
- Keep stream formwise probes (`with-open-file` + `read`) as advisory only
  until stream `read` semantics are fully fixed.
