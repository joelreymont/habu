# Exact Modular Builds

`tools/hb-build.f` accepts a modular entry file directly. The build composes the
entry with the same loader order as native Habu before linting, cache lookup, or
AOT compilation.

## Loader Semantics

- `include` and `included` expand at their source position every time.
- `require` and `required` expand once per exact path string.
- `provided` adds the exact path string to the same registry without reading it.
- Package and compiler state flow through a dependency exactly where the loader
  occurs. Dependencies are never hoisted or concatenated ahead of the entry.

Only literal loader paths are buildable. Dynamic or compiled stack loaders,
unsupported string openers, loader shadowing or retirement, missing inputs,
cycles, malformed or NUL paths, and capacity overflow reject before cache lookup
or compilation.

## Frozen Transaction

Composition freezes every discovered path and content byte once. The resulting
in-memory plan owns:

- ordered loader events and exact registry transitions, held as nominal enums
  until one bounded canonical serialization step;
- file paths, content lengths, and content digests;
- the composed source bytes;
- source-map rows from composed spans to original file, byte, line, and column;
- composition and map digests.

Cache keys consume this authenticated plan directly. A cache hit therefore does
not materialize temporary source or map files. Lints and the maker materialize
the already-frozen source and map only after a cache miss needs them. Changing an
input after composition cannot change any consumer in that transaction; a new
entry assignment starts a new transaction and observes the edit.

## Diagnostics

The composer injects checked `DIAG-FILE!` and `DIAG-ORIGIN!` markers from the
canonical lexer metadata. Checker text and `--json-errors` diagnostics therefore
name the original dependency path and definition location, not the temporary
composed file.

Composition failures are emitted on stderr at the original loader site. Text
diagnostics include the error code, file, line, column, byte, reason, and ordered
include chain. JSON diagnostics use `schema_version: 1` and include the same site
plus an `include_chain` array.

The materialized map begins with `HABUMAP1`. Each tab-separated row records:

1. composed output byte start;
2. mapped byte length;
3. original source byte start;
4. original line;
5. original column;
6. the original path encoded as hexadecimal bytes.

Synthetic diagnostic markers have no source-map row; only original source spans
are mapped.
