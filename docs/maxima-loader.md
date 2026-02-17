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
- Some deep modules can trigger VM `StackOverflow` in current runtime; keeping execution explicit allows staged debugging and per-module triage.
- Default runtime heap was raised to 256MB (`src/main.zig`, `src/runtime/heap.zig`) for larger Maxima workloads.
