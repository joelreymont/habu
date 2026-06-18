# Standard Library

The standard library lives under `lib/`. `lib/std.manifest` is the canonical
machine-readable layout and signature index for that tree. The initial manifest
reserves module ownership only; public word rows are added only after checked
source exists.

## Layout

Planned module files:

- `lib/errors.f`
- `lib/array.f`
- `lib/string.f`
- `lib/regex.f`
- `lib/map.f`
- `lib/fs.f`
- `lib/process.f`
- `lib/argv.f`
- `lib/test.f`
- `lib/property.f`
- `lib/build.f`
- `lib/time.f`
- `lib/date.f`

Each module gets a focused test file named in the manifest and documentation in
this file. Source files stay one concern per file, and new public/library words
default to checked typed definitions.

## Handle Representation

The checker currently has pointer types, not nominal handle types. v1 memory-backed handles use `ptr u8 n`: the pointer is the storage base and `n` is the byte capacity or active length specified by the owning module. Public signatures must keep that representation visible until dedicated concrete handle types exist.

Opaque `addr` values are boundary-only. A module may use `addr` only for values
that checked code never dereferences, or behind a named audited `TRUST` wrapper
that converts the boundary value into a typed pointer contract with focused
tests. Regex and map prose may call values `rx` or `map`, but manifest effects
and source signatures remain typed as `ptr u8 n`.

## Array

`lib/array.f` provides checked helpers for cell arrays. Array words take a base
cell pointer and an element count; indexed access additionally takes a zero-based
index. `A@ ( ptr a n n -- a )` fetches `arr[index]`, and
`A! ( a ptr a n n -- )` stores one element. `A-CHECK-INDEX ( n n -- )` throws
`E-A-BOUNDS` when an index is negative or outside `[0, len)`.
`A-CHECK-RANGE ( n n n -- )` validates `len start count` and allows empty ranges
at either end, while rejecting negative lengths, negative starts, negative
counts, starts past `len`, and ranges that overrun `len`.
`A-CHECK-NONEMPTY ( n -- )` throws `E-A-BOUNDS` for negative lengths and
`E-A-EMPTY` for zero length.

Numeric scalar kernels are `A-SUM`, `A-MIN`, `A-MAX`, `A-COUNT-EVEN`, and
`A-ARGMAX`. `A-MIN`, `A-MAX`, and `A-ARGMAX` require a non-empty array and throw
`E-A-EMPTY` for length zero; `A-ARGMAX` returns the smallest index when multiple
elements tie for the maximum. Mutating kernels are `A-REVERSE!`,
`A-PREFIX-SUM!`, `A-RUNMAX!`, and `A-FILL!`; empty arrays are valid no-ops for
these words.

## Date And Time

`lib/time.f` exposes checked public wrappers around the native clock primitives:

```forth
TIME-EPOCH-SECONDS  ( -- n )
TIME-MONO-NS        ( -- n )
```

`TIME-EPOCH-SECONDS` returns UTC Unix seconds from `epoch-seconds`.
`TIME-MONO-NS` returns monotonic nanoseconds from `mono-ns`; callers should only
compare ordering or elapsed time, never exact values.

`lib/date.f` exposes checked Gregorian UTC helpers:

```forth
DATE-DIGIT?       ( n -- bool )
LEAP-YEAR?        ( n -- bool )
MONTH-DAYS        ( n n -- n )
VALID-YMD?        ( n n n -- bool )
YMD>DAYS          ( n n n -- n )
DAYS>YMD          ( n -- n n n )
DATE-N            ( ptr u8 n n -- n bool )
PARSE-YMD         ( ptr u8 n -- n bool )
DATE-WIDTH!       ( n n ptr u8 n -- )
FORMAT-YMD        ( n ptr u8 n -- ptr u8 n )
FORMAT-EPOCH-UTC  ( n ptr u8 n -- ptr u8 n )
```

`PARSE-YMD` accepts exactly `YYYY-MM-DD` and returns the Unix epoch day plus a
success flag. `FORMAT-YMD` writes `YYYY-MM-DD`; `FORMAT-EPOCH-UTC` writes
`YYYY-MM-DDTHH:MM:SSZ`. Formatters use caller-provided buffers and throw
`E-TIME-CAPACITY` when the buffer is too small. `FORMAT-EPOCH-UTC` also throws
`E-TIME-RANGE` for negative epoch seconds. Load `lib/errors.f` before
`lib/date.f` when using formatter error codes.

## Manifest Format

`lib/std.manifest` is UTF-8 TSV with schema version `1` and this exact header:

```text
schema_version	module	file	kind	word	effect	test	doc	owner	status	notes
```

Columns:

- `schema_version`: currently `1`.
- `module`: lowercase stable module name.
- `file`: stable `lib/<module>.f` source path.
- `kind`: `module` or `word`.
- `word`: public word name for `word` rows; empty for `module` rows.
- `effect`: normalized checked effect for `word` rows; empty for `module` rows.
- `test`: focused test path that owns the row.
- `doc`: documentation path for the row.
- `owner`: stable ownership label for future parallel workers.
- `status`: `planned`, `active`, or `published`.
- `notes`: short human context, without tabs.

`module` rows reserve file ownership and leave `word` and `effect` empty. `word`
rows describe only public checked definitions that exist in source. The `effect`
field must match the normalized `signature` emitted by:

```sh
bin/hb /tmp/public-signatures.f lib/<module>.f
```

where `/tmp/public-signatures.f` is built from `tools/lint/lib.f` followed by
`tools/public-signatures.f`, as in `tools/stdlib-manifest-test.sh`.

Run the focused check with:

```sh
./tools/stdlib-manifest-test.sh
```
