# Standard Library

The standard library lives under `lib/`. `lib/std.manifest` is the canonical
machine-readable layout and signature index for that tree. This file is the
authoritative LLM-facing stdlib surface: prompts, examples, benchmark tasks, and
future module implementations must use the effects and boundary contracts here.
The initial manifest reserves module ownership only; public word rows are added
only after checked source exists.

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

## LLM Surface

LLM-facing code should call the highest-level checked word that matches the
task, and should only reach for unchecked host/runtime primitives at the audited
boundaries named below. The surface below includes active source-backed words
and planned API contracts. Source-backed public word rows are the only published
rows in `lib/std.manifest`; planned contracts here define the target API shape
for implementation dots and benchmark prompts.

Typed examples in prompts must use the current checked grammar exactly. Array
views and cell-backed map storage use `ptr a n`; byte strings, regex bytecode
buffers, map keys, paths, and capture buffers use `ptr u8 n`. Quotation effects
are written in brackets, for example `[ ptr u8 n -- ]`.

## Handle Representation

The checker currently has pointer types, not nominal handle types. Byte-oriented
v1 memory-backed handles use `ptr u8 n`: the pointer is the storage base and
`n` is the byte capacity or active length specified by the owning module.
Cell-oriented storage such as arrays and fixed-capacity map slot storage uses
`ptr a n`: the pointer is the cell storage base and `n` is the element or slot
capacity. Public signatures must keep that representation visible until
dedicated concrete handle types exist.

Opaque `addr` values are boundary-only. A module may use `addr` only for values
that checked code never dereferences, or behind a named audited `TRUST` wrapper
that converts the boundary value into a typed pointer contract with focused
tests. Regex prose may call values `rx`, but manifest effects and source
signatures remain typed as `ptr u8 n`; map prose may call values `map`, but
manifest effects and source signatures remain typed as `ptr a n` for storage
and `ptr u8 n` for keys.

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

Numeric scalar kernels are `A-SUM`, `A-MIN`, `A-MAX`, `A-COUNT-EVEN`,
`A-ARGMAX`, and `A-MAX-INDEX`. `A-MIN`, `A-MAX`, `A-ARGMAX`, and `A-MAX-INDEX`
require a non-empty array and throw `E-A-EMPTY` for length zero; `A-ARGMAX` and
`A-MAX-INDEX` return the smallest index when multiple elements tie for the
maximum. Mutating kernels are `A-REVERSE-RANGE!`, `A-REVERSE!`,
`A-PREFIX-SUM!`, `A-RUNMAX!`, and `A-FILL!`; empty arrays are valid no-ops for
these words.

Quotation combinators make common LLM-generated loops explicit and checked.
`A-MAP!` and `A-MAPI!` update cells in place, `A-FOLD` and `A-FOLDI` reduce
cells with an accumulator, `A-SCAN!` writes a prefix scan from an explicit seed,
`A-SCAN1!` uses the first cell as the seed, and `A-FIND-INDEX` /
`A-FIND-INDEXI` return the first matching index or `-1`. Index-aware quotations
receive the zero-based index before the value.

Convenience helpers keep common index math checked: `A+!` adds to one element,
`A-SWAP` swaps two checked indexes, `LAST-INDEX` returns `len - 1` for a
non-empty array, `MIRROR-INDEX` returns `len - 1 - index`, and `EVEN?` returns a
Forth boolean for integer parity.

```forth
A-CHECK-INDEX     ( n n -- )
A-CHECK-RANGE     ( n n n -- )
A-CHECK-NONEMPTY  ( n -- )
A@                ( ptr a n n -- a )
A!                ( a ptr a n n -- )
A+!               ( n ptr a n n -- )
A-SWAP            ( ptr a n n n -- )
LAST-INDEX        ( n -- n )
MIRROR-INDEX      ( n n -- n )
EVEN?             ( n -- bool )
A-SUM             ( ptr n n -- n )
A-MIN             ( ptr n n -- n )
A-MAX             ( ptr n n -- n )
A-COUNT-EVEN      ( ptr n n -- n )
A-ARGMAX          ( ptr n n -- n )
A-MAX-INDEX       ( ptr n n -- n )
A-REVERSE-RANGE!  ( ptr a n n n -- )
A-REVERSE!        ( ptr a n -- )
A-PREFIX-SUM!     ( ptr n n -- )
A-RUNMAX!         ( ptr n n -- )
A-FILL!           ( a ptr a n -- )
A-MAP!            ( ptr a n [ a -- a ] -- )
A-MAPI!           ( ptr a n [ n a -- a ] -- )
A-FOLD            ( ptr a n b [ b a -- b ] -- b )
A-FOLDI           ( ptr a n b [ b n a -- b ] -- b )
A-SCAN!           ( ptr n n n [ n n -- n ] -- )
A-SCAN1!          ( ptr n n [ n n -- n ] -- )
A-FIND-INDEX      ( ptr a n [ a -- bool ] -- n )
A-FIND-INDEXI     ( ptr a n [ n a -- bool ] -- n )
```

## String

`lib/string.f` provides checked byte-string helpers. Inputs are byte pointers
plus lengths; no word assumes NUL termination unless its name says `PATHZ` or a
module boundary explicitly says it owns path conversion. `SB-*` words operate on
the shared bounded string-builder buffer and throw `E-STR-CAPACITY` or
`E-STR-BOUNDS` instead of truncating silently. `STR>NUMBER?` parses a signed
i64 and returns `0 false` on invalid or out-of-range input.

```forth
STR-TRUE        ( -- bool )
STR-FALSE       ( -- bool )
BYTE-COPY       ( ptr u8 ptr u8 n -- )
ASCII-LOWER     ( n -- n )
ASCII-UPPER     ( n -- n )
STR=            ( ptr u8 n ptr u8 n -- bool )
STR=CI          ( ptr u8 n ptr u8 n -- bool )
STARTS-WITH?    ( ptr u8 n ptr u8 n -- bool )
ENDS-WITH?      ( ptr u8 n ptr u8 n -- bool )
FIND-SUB        ( ptr u8 n ptr u8 n -- n )
CONTAINS?       ( ptr u8 n ptr u8 n -- bool )
INDEX-OF        ( ptr u8 n n -- n )
COUNT-CHAR      ( ptr u8 n n -- n )
LTRIM           ( ptr u8 n -- ptr u8 n )
RTRIM           ( ptr u8 n -- ptr u8 n )
TRIM            ( ptr u8 n -- ptr u8 n )
SB-CHECK-ROOM   ( n -- )
SB-RESET        ( -- )
SB-APPEND       ( ptr u8 n -- )
SB-APPEND-C     ( n -- )
SB$             ( -- ptr u8 n )
SPLIT-NEXT      ( ptr u8 n n n -- ptr u8 n n bool )
STR-DIGIT?      ( n -- bool )
STR-DIGIT-VALUE ( n -- n )
STR-DIGITS?     ( ptr u8 n -- bool )
STR-DIGITS<=    ( ptr u8 n ptr u8 n -- bool )
STR-PARSE-POS   ( ptr u8 n -- n bool )
STR-PARSE-NEG   ( ptr u8 n -- n bool )
STR>NUMBER?     ( ptr u8 n -- n bool )
```

`FIND-SUB` and `INDEX-OF` return `-1` on no match. Builder words append to the
module's current string-builder buffer and throw a named capacity error when the
next append would exceed that buffer; they never truncate silently. `SPLIT-NEXT`
returns the next field, the next scan index, and a success flag.

## Regex

`lib/regex.f` exposes a bounded capture-free regex subset for LLM tasks:
literals, `.`, `^`, `$`, character classes and negated classes, escaped
metacharacters, and `?`, `*`, `+`. v1 excludes captures, backreferences,
lookaround, and alternation unless a bounded NFA plan is implemented first.

```forth
RX-COMPILE   ( ptr u8 n ptr u8 n -- n )
RX-MATCH?    ( ptr u8 n ptr u8 n -- bool )
RX-FIND      ( ptr u8 n ptr u8 n -- n n bool )
RX-COUNT     ( ptr u8 n ptr u8 n -- n )
```

`RX-COMPILE` takes pattern bytes plus a caller-provided bytecode buffer and
capacity, then returns the compiled byte length. Match, find, and count take
input text plus compiled bytecode pointer/length. Malformed patterns and
bytecode capacity overflow throw named regex errors; they do not return a
partial regex or an unchecked `addr`.

## Map

`lib/map.f` provides a fixed-capacity open-addressed string-key map foundation.
The source-backed surface uses `ptr a n` cell storage: `MAP-CELLS` returns the
cell count to allocate for a capacity, and `MAP-INIT` initializes that storage.
Key strings use `ptr u8 n`.

The current published words expose the checked storage layout needed by the
lookup/update layer:

```forth
MAP-CHECK-CAP       ( n -- )
MAP-CELLS           ( n -- n )
MAP-EMPTY?          ( n -- bool )
MAP-DELETED?        ( n -- bool )
MAP-OCCUPIED?       ( n -- bool )
MAP-CAP@            ( ptr a -- n )
MAP-CAP!            ( n ptr a -- )
MAP-COUNT@          ( ptr a -- n )
MAP-DELETED@        ( ptr a -- n )
MAP-COUNT!          ( n ptr a -- )
MAP-DELETED!        ( n ptr a -- )
MAP-SLOTS           ( ptr a -- ptr a )
MAP-CHECK-INDEX     ( ptr a n -- )
MAP-SLOT            ( ptr a n -- ptr a )
MAP-SLOT-FIELD      ( ptr a n n -- ptr a )
MAP-SLOT-STATE@     ( ptr a n -- n )
MAP-SLOT-STATE!     ( n ptr a n -- )
MAP-SLOT-HASH@      ( ptr a n -- n )
MAP-SLOT-HASH!      ( n ptr a n -- )
MAP-SLOT-KEY-A@     ( ptr a n -- ptr u8 )
MAP-SLOT-KEY-A!     ( ptr u8 ptr a n -- )
MAP-SLOT-KEY-U@     ( ptr a n -- n )
MAP-SLOT-KEY-U!     ( n ptr a n -- )
MAP-SLOT-VALUE@     ( ptr a n -- a )
MAP-SLOT-VALUE!     ( a ptr a n -- )
MAP-SLOT-CLEAR      ( ptr a n -- )
MAP-CLEAR           ( ptr a -- )
MAP-INIT            ( ptr a n -- )
MAP-HASH            ( ptr u8 n -- n )
MAP-INDEX           ( n n -- n )
MAP-PROBE           ( n n n -- n )
```

The planned high-level API builds on the same cell storage and counted byte
keys:

```forth
MAP-HAS?    ( ptr a n ptr u8 n -- bool )
MAP-GET     ( ptr a n ptr u8 n -- n bool )
MAP-SET     ( n ptr a n ptr u8 n -- )
MAP-COUNT   ( ptr a n -- n )
MAP-EACH    ( ptr a n [ ptr u8 n n -- ] -- )
```

`MAP-GET` returns value plus present flag. `MAP-SET` inserts or replaces one
numeric value. `MAP-EACH` invokes the quotation once per occupied slot; v1 does
not guarantee lexical key ordering. Capacity, malformed storage, and full-table
states throw named errors such as `E-MAP-BAD-CAP` and `E-MAP-FULL`.

## Files

`lib/fs.f` promotes the native filesystem helper surface from `tools/fs.f`.
Public path words accept counted byte strings and own any private NUL-terminated
copy needed for syscalls.

```forth
EXISTS?      ( ptr u8 n -- bool )
FILE?        ( ptr u8 n -- bool )
DIR?         ( ptr u8 n -- bool )
BASENAME     ( ptr u8 n -- ptr u8 n )
JOIN-PATH    ( ptr u8 n ptr u8 n ptr u8 -- n )
WALK-FILES   ( ptr u8 n [ ptr u8 n -- ] -- )
READ-ALL     ( ptr u8 n ptr u8 n -- n )
WRITE-ALL    ( ptr u8 n ptr u8 n -- )
APPEND-FILE  ( ptr u8 n ptr u8 n -- )
```

`WALK-FILES` must be implemented either as a checked quotation combinator or as
one audited `TRUST` boundary with focused tests proving callback invocation,
recursion-buffer isolation, and error behavior. Traversal is depth-first and
calls the quotation for regular files only. Within one directory, entries are
visited in the order returned by the platform directory stream; callers that
need lexical order must collect and sort separately. Recursive walks use
per-depth recursion buffers, so a child walk cannot corrupt the parent directory
record. The path pointer passed to the callback is valid only for that callback;
copy it before storing it.

`JOIN-PATH`, `READ-ALL`, `WRITE-ALL`, and `APPEND-FILE` are bounded by caller
buffers or syscall results. They throw named filesystem errors on path overflow,
stat/open/read/write failure, directory-depth overflow, and output capacity
overflow.

## Processes

`lib/process.f` wraps native process primitives in checked contracts. Public
wrappers accept counted paths/commands, own conversion to private `pathz`
buffers, and never require LLM code to build C strings by hand.

```forth
RUN-RC       ( ptr u8 n -- n )
SPAWN-IO     ( ptr u8 n n n n -- n )
WAIT-RC      ( n -- n )
RUN-CAPTURE  ( ptr u8 n ptr u8 n ptr u8 n n -- n n n )
```

`SPAWN-IO` takes a counted executable path followed by stdin, stdout, and stderr
fds. Negative fd values mean inherit/default; nonnegative fd values are passed
through explicitly. Parent-only pipe and PTY fds must be marked `FD_CLOEXEC`
before spawning, then closed by the parent after the child no longer needs them.
Every spawn path must close all fds it owns on success and failure.

`RUN-CAPTURE` takes command string, stdout buffer/capacity, stderr
buffer/capacity, and timeout milliseconds. It returns stdout length, stderr
length, and rc in that order. Captures are bounded by the caller capacities; if
either stream would exceed its capacity, the word throws `E-PROC-TRUNCATED`
rather than truncating silently. On timeout, it terminates or otherwise reaps the
child before throwing `E-PROC-TIMEOUT`. Process wrappers throw named errors for
path conversion failure, spawn failure, wait failure, capture drain failure,
timeout, and output capacity overflow.

`RUN-RC` must be checker-modeled or implemented as an audited checked wrapper
around modeled primitives. Until that exists, checked examples should use
`SPAWN-IO WAIT-RC` rather than unchecked runtime words.

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

## Argv

`lib/argv.f` provides checked command-line parsing for `hb script.f args...`
scripts. It reads `SCRIPT-ARGC` and `SCRIPT-ARGV$` by default, or an in-memory
mock argv set for focused tests. `ARGV-PARSE` recognizes `--json`, `-o OUT`,
and `--`; tokens after `--` are always positionals, even when they begin with a
dash. Unknown dash-prefixed options and missing option values throw
`ARGV-E-USAGE` after emitting the configured usage text unless quiet mode is
enabled.

```forth
ARGV-USAGE!             ( ptr u8 n -- )
ARGV-QUIET!             ( n -- )
ARGV-USE-SCRIPT         ( -- )
ARGV-MOCK-CLEAR         ( -- )
ARGV-MOCK+              ( ptr u8 n -- )
ARGV-COUNT              ( -- n )
ARGV-TOK$               ( n -- ptr u8 n )
ARGV-TOK=               ( n ptr u8 n -- bool )
ARGV-PARSE              ( -- )
ARGV-EXPECT-POS         ( n n -- )
ARGV-EXPECT-POS-EXACT   ( n -- )
ARGV-POS#               ( -- n )
ARGV-POS$               ( n -- ptr u8 n )
ARGV-POSZ               ( n -- ptr u8 )
ARGV-JSON?              ( -- bool )
ARGV-OUT-DEFAULT!       ( ptr u8 n -- )
ARGV-OUT!               ( ptr u8 n -- )
ARGV-OUT?               ( -- bool )
ARGV-OUT$               ( -- ptr u8 n )
ARGV-OUTZ               ( -- ptr u8 )
ARGV-REQUIRE-OUT        ( -- )
ARGV-PATHZ              ( ptr u8 n -- ptr u8 )
ARGV-ZCOPY              ( ptr u8 n ptr u8 n -- ptr u8 )
```

Drivers set usage/defaults, call `ARGV-PARSE`, validate positional arity with
`ARGV-EXPECT-POS` or `ARGV-EXPECT-POS-EXACT`, then read counted outputs through
`ARGV-POS$`, `ARGV-OUT$`, and `ARGV-JSON?`. Path-oriented syscall wrappers may
use `ARGV-POSZ`, `ARGV-OUTZ`, or `ARGV-PATHZ`; these copy into the module-owned
path buffer and throw `ARGV-E-INTERNAL` on capacity failure.

Mocks keep parser tests self-hosted: `ARGV-MOCK-CLEAR` enables mock mode and
empties the mock list, `ARGV-MOCK+` appends one counted token, and
`ARGV-USE-SCRIPT` restores real script argv. `ARGV-QUIET!` suppresses usage
writes while still throwing exact error codes, so tests can assert
`ARGV-E-USAGE` deterministically.

## Test Property And Build Helpers

`lib/test.f`, `lib/property.f`, and `lib/build.f` provide reusable checked
helpers for scripts and fixtures. The public surface is checked; unchecked
metaprogramming, `evaluate`, source-string generation, raw argv/envp cells, and
process exits stay in small named boundary words with `TRUST` audit entries
where the checker cannot express the contract.

```forth
T-RESET         ( -- )
T-CASES         ( -- n )
T-FAILURES      ( -- n )
T-ASSERT        ( bool -- )
T=              ( n n -- )
T<>             ( n n -- )
TTRUE           ( bool -- )
TFALSE          ( bool -- )
T-STR=          ( ptr u8 n ptr u8 n -- bool )
T$=             ( ptr u8 n ptr u8 n -- )
T$<>            ( ptr u8 n ptr u8 n -- )
TTHROWS         ( a n -- )
T-REPORT        ( -- )
PROP-SEED!      ( n -- )
PROP-RND        ( -- n )
PROP-RND%       ( n -- n )
PROP-RUN        ( n n [ -- ] -- )
SHRINK-TRAIL    ( ptr u8 n [ ptr u8 n -- bool ] -- ptr u8 n )
BUILD-STEP      ( ptr u8 n [ -- n ] -- )
BUILD-CHECK     ( ptr u8 n -- )
BUILD-ARTIFACT  ( ptr u8 n ptr u8 n -- ptr u8 n )
BUILD-RUN       ( ptr u8 n ptr u8 n -- n )
```

`lib/test.f` assertions throw named test errors and keep one final report path;
they never mask assertion failures. `TTHROWS` takes an execution token `a`
created by tick (`' WORD`) plus an expected throw code; its raw `catch` use is
isolated behind the audited trusted boundary `TTHROWS-RAW`.
`lib/property.f` owns deterministic PRNG state, seed/count parsing, generator
helpers, and shrinking utilities. Property execution may call an audited
`evaluate` boundary for generated checked source, but pure generators and shrink
predicates remain checked helpers. `lib/build.f` owns build step modeling,
source validation, artifact path construction, and fail-closed status reporting;
raw process exits are only allowed at the final CLI/script boundary.

## Build Shell Boundary

Shell wrappers may only set final environment values, create and export private
`HB_TMP`, launch `bin/hb`, install already-validated final artifacts, and
propagate exit status. Shell must not own durable build policy, source
validation, step graph decisions, artifact expectations, checker certification,
fixpoint comparison, or fallback logic.

All build policy, step graph, expected artifacts, and fail-closed checks belong
in Habu scripts and libraries. Habu build helpers are responsible for validating
user source, proving checked definitions, detecting missing artifacts, and
reporting named failures. Shell may allocate private temporary space and pass it
to Habu; Habu decides what work happens inside that space.

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
