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

`lib/regex.f` exposes a bounded capture-free regex scanner and matcher for LLM
tasks: literals, `.`, `^`, `$`, character classes and negated classes, escaped
metacharacters, and `?`, `*`, `+`. v1 excludes captures, backreferences,
lookaround, and alternation unless a bounded NFA plan is implemented first. Regex
bytecode uses caller-provided `ptr u8 n` storage; matches never return unchecked
`addr` handles.

```forth
RX-ESCAPABLE?         ( n -- bool )
RX-UNSUPPORTED-META?  ( n -- bool )
RX-CHECK-BYTE         ( n -- )
RX-NEED               ( n n n -- )
RX-EMIT-1             ( n ptr u8 n n -- n )
RX-EMIT-LIT           ( n ptr u8 n n -- n )
RX-EMIT-RANGE         ( n ptr u8 n ptr u8 n n -- n )
RX-SCAN-CLASS-BODY    ( ptr u8 n n -- n )
RX-SCAN-CLASS         ( ptr u8 n n -- n n n )
RX-EMIT-CLASS-DONE    ( ptr u8 ptr u8 n n n n n -- n n )
RX-EMIT-CLASS         ( ptr u8 n n ptr u8 n n -- n n )
RX-SCAN-ESCAPE        ( ptr u8 n n ptr u8 n n -- n n )
RX-SCAN-ONE           ( ptr u8 n n ptr u8 n n -- n n )
RX-COMPILE            ( ptr u8 n ptr u8 n -- n )
RX-CHECK-MATCH-ARGS   ( n n -- )
RX-FLAGS-CLEAR        ( ptr u8 n -- )
RX-FLAG?              ( ptr u8 n -- bool )
RX-ANY-FLAG?          ( ptr u8 n -- bool )
RX-ADD-STATE          ( ptr u8 n n -- )
RX-QUANT?             ( n -- bool )
RX-ZERO-QUANT?        ( n -- bool )
RX-CONSUMING?         ( n -- bool )
RX-ANCHOR?            ( n -- bool )
RX-FIXED-ATOM-LEN     ( n n n -- n )
RX-CLASS-RAW-LEN      ( ptr u8 n n -- n )
RX-ATOM-LEN           ( ptr u8 n n -- n )
RX-ATOM-END           ( ptr u8 n n -- n )
RX-QUANT-AT           ( ptr u8 n n -- n )
RX-AFTER-ATOM-QUANT   ( ptr u8 n n -- n )
RX-VALIDATE-STEP      ( ptr u8 n n -- n )
RX-VALIDATE           ( ptr u8 n -- )
RX-CLASS-RANGE-CAND?  ( ptr u8 n n -- bool )
RX-CLASS-RANGE-MATCH? ( n ptr u8 n -- bool )
RX-CLASS-ESC-MATCH?   ( n ptr u8 n n -- bool )
RX-CLASS-MEMBER?      ( n ptr u8 n -- bool )
RX-ATOM-CHAR-MATCH?   ( n ptr u8 n n -- bool )
RX-ANCHOR-MATCH?      ( n n n -- bool )
RX-CLOSE-ONE          ( ptr u8 n n n ptr u8 n -- )
RX-CLOSE              ( ptr u8 n n n ptr u8 -- )
RX-RESET-STATES       ( n -- )
RX-NEXT>ACTIVE        ( n -- )
RX-ADD-CONSUME-TARGET ( ptr u8 n n ptr u8 -- )
RX-CONSUME-STATE      ( ptr u8 ptr u8 n n ptr u8 ptr u8 n -- )
RX-CONSUME-CHAR       ( ptr u8 ptr u8 n n -- )
RX-ACCEPT?            ( n -- bool )
RX-PREFIX-LEN         ( ptr u8 n ptr u8 n n -- n bool )
RX-PREPARE            ( n ptr u8 n -- )
RX-MATCH?             ( ptr u8 n ptr u8 n -- bool )
RX-FIND-FROM          ( ptr u8 n ptr u8 n n -- n n bool )
RX-FIND               ( ptr u8 n ptr u8 n -- n n bool )
RX-COUNT              ( ptr u8 n ptr u8 n -- n )
```

`RX-COMPILE` takes pattern bytes plus a caller-provided bytecode buffer and
capacity, then returns the compiled byte length. Malformed patterns and bytecode
capacity overflow throw named regex errors; they do not return a partial regex
or an unchecked `addr`. `RX-MATCH?` is whole-input matching, `RX-FIND` returns
`offset length true` or `0 0 false`, and `RX-COUNT` counts non-overlapping
matches, advancing one byte after zero-length matches to avoid hangs.

## Map

`lib/map.f` provides a fixed-capacity open-addressed string-key map.
The source-backed surface uses `ptr a n` cell storage: `MAP-CELLS` returns the
cell count to allocate for a capacity, and `MAP-INIT` initializes that storage.
Key strings use `ptr u8 n`.

The published words expose checked storage layout plus lookup/update helpers:

```forth
MAP-CHECK-CAP       ( n -- )
MAP-CHECK-LEN       ( n -- )
MAP-CELLS           ( n -- n )
MAP-EMPTY?          ( n -- bool )
MAP-DELETED?        ( n -- bool )
MAP-OCCUPIED?       ( n -- bool )
MAP-CAP@            ( ptr a -- n )
MAP-CAP!            ( n ptr a -- )
MAP-CHECK-HANDLE    ( ptr a n -- )
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
MAP-SLOT-MATCH?     ( ptr a n n ptr u8 n -- bool )
MAP-REMEMBER-FREE   ( n n -- n )
MAP-LOCATE-SLOT     ( n ptr a n ptr u8 n n -- n n n )
MAP-LOCATE          ( ptr a n ptr u8 n -- n n n )
MAP-SLOT-INSERT     ( a ptr a n n ptr u8 n -- )
MAP-HAS?    ( ptr a n ptr u8 n -- bool )
MAP-GET     ( ptr a n ptr u8 n -- n bool )
MAP-SET     ( n ptr a n ptr u8 n -- )
MAP-EACH    ( ptr a n [ ptr u8 n n -- ] -- )
```

`MAP-GET` returns value plus present flag. `MAP-SET` inserts or replaces one
numeric value. Capacity, malformed storage, and full-table states throw named
errors such as `E-MAP-BAD-CAP` and `E-MAP-FULL`.

## Files

`lib/fs.f` promotes the native filesystem helper surface from `tools/fs.f`.
Public path words accept counted byte strings and own any private NUL-terminated
copy needed for syscalls.

The current source-backed surface covers path predicates, stat mode, basename,
bounded path joining, bounded file I/O, and recursive file walking:

```forth
FS-FALSE           ( -- bool )
FS-TRUE            ( -- bool )
FS-U16@            ( ptr u8 -- n )
FS-CHECK-JOIN-CAP       ( n -- )
FS-PATHZ                ( ptr u8 n -- ptr u8 )
EXISTS?                 ( ptr u8 n -- bool )
FS-TRY-STAT-MODE        ( ptr u8 n -- n )
STAT-MODE               ( ptr u8 n -- n )
FILE?                   ( ptr u8 n -- bool )
DIR?                    ( ptr u8 n -- bool )
BASENAME                ( ptr u8 n -- ptr u8 n )
JOIN-PATH               ( ptr u8 n ptr u8 n ptr u8 -- n )
READ-ALL                ( ptr u8 n ptr u8 n -- n )
FS-WRITE-BY-FLAGS       ( ptr u8 n ptr u8 n n -- )
WRITE-ALL               ( ptr u8 n ptr u8 n -- )
APPEND-FILE             ( ptr u8 n ptr u8 n -- )
FS-SKIP-DIR?            ( ptr u8 n -- bool )
FS-SKIP-ENTRY?          ( ptr u8 n -- bool )
FS-WALK-PATH            ( ptr u8 n [ ptr u8 n -- ] -- )
WALK-FILES   ( ptr u8 n [ ptr u8 n -- ] -- )
```

`WALK-FILES` walks regular files depth-first, skips `.git`, `.jj`, and
`.dots`, uses per-depth buffers, and closes active directory descriptors before
throwing explicit filesystem errors.

`READ-ALL` reads a regular file into caller storage and returns the byte count.
The caller supplies the explicit output cap. Files larger than the cap throw
`E-FS-CAPACITY`; open and I/O failures throw `E-FS-OPEN` or `E-FS-IO`.
`WRITE-ALL` creates/truncates a regular file, and `APPEND-FILE` creates/appends
to a regular file. Both write the full counted input or throw a named filesystem
error.

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
PROC-WAIT-RAW       ( n -- n )
PROC-SPAWN-RAW      ( ptr u8 n n n -- n )
PROC-ZCOPY          ( ptr u8 n ptr u8 n -- ptr u8 )
PATHZ               ( ptr u8 n -- ptr u8 )
WAIT-RC             ( n -- n )
SPAWN-IO            ( ptr u8 n n n n -- n )
RUN-RC              ( ptr u8 n -- n )
FD-CLOEXEC!         ( n -- )
PIPE-PAIR           ( -- n n )
PROC-PFD!           ( n n -- )
POLL-IN             ( n n -- n )
POLL-IN-OR-TIMEOUT  ( n n -- n )
RUN-CAPTURE  ( ptr u8 n ptr u8 n ptr u8 n n -- n n n )
```

`PATHZ` copies a counted path into the module's private NUL-terminated path
buffer and throws `E-PROC-OUTPUT` if the path does not fit. `RUN-RC` composes the
checked `SPAWN-IO` and `WAIT-RC` wrappers rather than the unchecked runtime
`run-rc` primitive. `SPAWN-IO` and `WAIT-RC` throw `E-PROC-SPAWN` and
`E-PROC-WAIT` for primitive failures.

`SPAWN-IO` takes a counted executable path followed by stdin, stdout, and stderr
fds. Negative fd values mean inherit/default; nonnegative fd values are passed
through explicitly. `PIPE-PAIR` creates a pipe as read fd then write fd.
Parent-only pipe and PTY fds must be marked close-on-exec with `FD-CLOEXEC!`
before spawning; this sets the Darwin `FD_CLOEXEC` flag. Parent code then closes
the fd after the child no longer needs it.
Every spawn path must close all fds it owns on success and failure. `POLL-IN`
polls one fd for readable input and returns the raw poll result;
`POLL-IN-OR-TIMEOUT` throws `E-PROC-TIMEOUT` for a zero poll result and
`E-PROC-OUTPUT` for poll failure.

`PROC-WAIT-RAW` and `PROC-SPAWN-RAW` are raw primitive aliases captured before
the checked wrapper names are defined. Application code should prefer
`SPAWN-IO`, `WAIT-RC`, and `RUN-RC`.

`RUN-CAPTURE` takes command string, stdout buffer/capacity, stderr
buffer/capacity, and timeout milliseconds. It returns stdout length, stderr
length, and rc in that order. Captures are bounded by the caller capacities; if
either stream would exceed its capacity, the word throws `E-PROC-TRUNCATED`
rather than truncating silently. On timeout, it terminates or otherwise reaps the
child before throwing `E-PROC-TIMEOUT`. Process wrappers throw named errors for
path conversion failure, spawn failure, wait failure, capture drain failure,
timeout, and output capacity overflow.

`RUN-CAPTURE` remains a planned layer until Habu has the primitive support needed
to terminate and reap timed-out children without leaking a process.

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
PROP-SEED@      ( -- n )
PROP-COUNT@     ( -- n )
PROP-DEFAULTS   ( -- n n )
PROP-RUN-RESET  ( n n -- )
PROP-RND        ( -- n )
PROP-RND%       ( n -- n )
PROP-BUF-RESET  ( -- )
PROP-BUF+       ( ptr u8 n -- )
PROP-BUF-C+     ( n -- )
PROP-DIGIT+     ( n -- )
PROP-BUF$       ( -- ptr u8 n )
PROP-GEN-START  ( n -- )
PROP-GEN-STEP   ( ptr u8 n n n -- )
PROP-DROP-LAST  ( -- bool )
PROP-SHRINK     ( [ -- bool ] -- )
BUILD-STEP      ( ptr u8 n [ -- n ] -- )
BUILD-CHECK     ( ptr u8 n -- )
BUILD-ARTIFACT  ( ptr u8 n ptr u8 n -- ptr u8 n )
BUILD-RUN       ( ptr u8 n ptr u8 n -- n )
```

`lib/test.f` assertions throw named test errors and keep one final report path;
they never mask assertion failures. `TTHROWS` takes an execution token `a`
created by tick (`' WORD`) plus an expected throw code; its raw `catch` use is
isolated behind the audited trusted boundary `TTHROWS-RAW`.
`lib/property.f` owns deterministic PRNG state, seed/count bounds, bounded
source buffers, modeled generator depth, and token-tail shrinking utilities.
Property execution may call an audited `evaluate` boundary for generated checked
source, but pure generators and shrink predicates remain checked helpers.
`lib/build.f` owns build step modeling,
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
