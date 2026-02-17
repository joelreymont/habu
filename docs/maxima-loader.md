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

## Troubleshooting

- If a large-file load regresses, first validate package identity invariants:
  `(eq (find-symbol "FUNCTIONP" "MAXIMA") (find-symbol "FUNCTIONP" "COMMON-LISP"))`
  must be `nil` after `lib/maxima-stubs.lisp` is loaded.
- Keep stream formwise probes (`with-open-file` + `read`) as advisory only
  until stream `read` semantics are fully fixed.
