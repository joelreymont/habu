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
- `lib/table.f`
- `lib/vector.f`
- `lib/string.f`
- `lib/json-write.f`
- `lib/regex.f`
- `lib/map.f`
- `lib/memory.f`
- `lib/ffi-abi.f`
- `lib/ffi.f`
- `lib/fs.f`
- `lib/source.f`
- `lib/object.f`
- `lib/object-cache.f`
- `lib/object-link.f`
- `lib/process.f`
- `lib/process-fork.f`
- `lib/process-argv.f`
- `lib/process-env.f`
- `lib/process-command.f`
- `lib/process-cwd.f`
- `lib/argv.f`
- `lib/test.f`
- `lib/test/assert.f`
- `lib/test/suite.f`
- `lib/test/runner.f`
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

## Execution Vectors

Use checked deferred words for late-bound callbacks and backend hooks:
`defer ACTION ( effect )` declares the stable call surface, and checked code
installs an implementation with `[: IMPL ;] is ACTION`. Do not model dispatch as
`variable`/`@ execute` or raw `[']` storage; the checker cannot prove those xt
cells preserve the declared effect. An unset deferred word exits with the
execution-vector error instead of silently jumping through zero.

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

## PTX

`lib/ptx/` is a research sub-library, not a flat `lib/std.manifest` module.
`lib/ptx/header.f` provides the checked PTX kernel header vocabulary used by
`docs/ptx-sketch.md`. `KERNEL:` is a compiler keyword alias for `:`; load
`lib/errors.f lib/ptx/header.f` before kernel sources. `%BLOCK` validates legal
CUDA block sizes (multiple of 32 and `1 <= n <= 1024`). `GRID:` and `WHERE` are
compile-time header markers consumed before the checked kernel body. `lib/ptx/launch.f`
provides checked host launch guards such as `PTX-ROW-LAUNCH-CHECK ( rows cols block -- )`
so CUDA launch code rejects invalid row dimensions before calling the driver.

## Array

`lib/array.f` provides checked helpers for cell arrays. Public array helpers use
nominal role types: array lengths are `len`, valid indexes are `idx`, and range
counts are `count`. Enter these roles explicitly with `>LEN`, `>IDX`, and
`>COUNT` at call boundaries. `A@ ( ptr a len idx -- a )` fetches `arr[index]`,
and `A! ( a ptr a len idx -- )` stores one element.
`A-CHECK-INDEX ( len idx -- )` throws `E-A-BOUNDS` when an index is negative or
outside `[0, len)`. `A-CHECK-RANGE ( len idx count -- )` validates
`len start count` and allows empty ranges at either end, while rejecting negative
lengths, negative starts, negative counts, starts past `len`, and ranges that
overrun `len`. `A-CHECK-NONEMPTY ( len -- )` throws `E-A-BOUNDS` for negative
lengths and `E-A-EMPTY` for zero length. `A-CHECK-WHOLE ( len -- )` accepts
empty arrays and throws `E-A-BOUNDS` only for negative lengths.

Use `A-LEN`, `A-IDX`, and `A-COUNT` to refine raw integers into checked array
roles at module boundaries. They reject negative inputs before values reach the
role-typed array words.

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
A-LEN             ( n -- len )
A-IDX             ( n -- idx )
A-COUNT           ( n -- count )
A-CHECK-INDEX     ( len idx -- )
A-CHECK-RANGE     ( len idx count -- )
A-CHECK-NONEMPTY  ( len -- )
A-CHECK-WHOLE     ( len -- )
A@                ( ptr a len idx -- a )
A!                ( a ptr a len idx -- )
A+!               ( n ptr a len idx -- )
A-SWAP            ( ptr a len idx idx -- )
LAST-INDEX        ( len -- idx )
MIRROR-INDEX      ( len idx -- idx )
EVEN?             ( n -- bool )
A-SUM             ( ptr n len -- n )
A-MIN             ( ptr n len -- n )
A-MAX             ( ptr n len -- n )
A-COUNT-EVEN      ( ptr n len -- count )
A-ARGMAX          ( ptr n len -- idx )
A-MAX-INDEX       ( ptr n len -- idx )
A-REVERSE-RANGE!  ( ptr a len idx count -- )
A-REVERSE!        ( ptr a len -- )
A-PREFIX-SUM!     ( ptr n len -- )
A-RUNMAX!         ( ptr n len -- )
A-FILL!           ( a ptr a len -- )
A-MAP!            ( ptr a len [ a -- a ] -- )
A-MAPI!           ( ptr a len [ idx a -- a ] -- )
A-FOLD            ( ptr a len b [ b a -- b ] -- b )
A-FOLDI           ( ptr a len b [ b idx a -- b ] -- b )
A-SCAN!           ( ptr n len n [ n n -- n ] -- )
A-SCAN1!          ( ptr n len [ n n -- n ] -- )
A-FIND-INDEX      ( ptr a len [ a -- bool ] -- n )
A-FIND-INDEXI     ( ptr a len [ idx a -- bool ] -- n )
```

Habu intentionally has no public SwiftForth-style relative linked-list module.
For ordinary collections, use arrays or maps. For fixed layout nodes, use the
structure DSL and publish checked accessors. For dispatch tables, use checked
`case/of/endof/endcase` or checked execution vectors; do not encode dispatch as
raw relative dictionary links.

## Table

`lib/table.f` provides checked helpers for fixed-capacity cell tables. A table is
plain `ptr a` storage with an explicit row count and field count supplied to each
operation. Row and field capacities are `count`; row and field positions are
`idx`; counted byte-string lengths are `len`. `TBL-FIELD ( ptr a count count idx idx -- ptr a )`
returns the checked address for `table[row][field]`; rows outside `[0, rows)`
throw `E-TBL-BOUNDS`, and fields outside `[0, fields)` throw `E-TBL-FIELD`.

Typed accessors make common tool records explicit without inventing nominal
handles yet: numeric fields use `TBL-N@` / `TBL-N!`, booleans use `TBL-BOOL@` /
`TBL-BOOL!`, byte pointers use `TBL-A@` / `TBL-A!`, and counted byte-string
pairs use `TBL-PAIR$` / `TBL-PAIR!`. Pair fields occupy two adjacent cells and
are rejected unless both cells fit in the record width.

```forth
TBL-CHECK-ROW    ( count idx -- )
TBL-CHECK-FIELD  ( count idx -- )
TBL-CHECK-PAIR   ( count idx -- )
TBL-CELLS        ( count count -- count )
TBL-FIELD        ( ptr a count count idx idx -- ptr a )
TBL-CELL@        ( ptr a count count idx idx -- a )
TBL-CELL!        ( a ptr a count count idx idx -- )
TBL-N@           ( ptr a count count idx idx -- n )
TBL-N!           ( n ptr a count count idx idx -- )
TBL-BOOL@        ( ptr a count count idx idx -- bool )
TBL-BOOL!        ( bool ptr a count count idx idx -- )
TBL-A@           ( ptr a count count idx idx -- ptr u8 )
TBL-A!           ( ptr u8 ptr a count count idx idx -- )
TBL-PAIR!        ( ptr u8 len ptr a count count idx idx -- )
TBL-PAIR$        ( ptr a count count idx idx -- ptr u8 len )
```

## Vector

`lib/vector.f` provides growable cell-vector storage backed by `lib/memory.f`.
A vector handle is caller-owned header storage: allocate
`VEC-HEADER-CELLS cells` in DATA or another cell area, then initialize it with
`VEC-INIT ( ptr a count -- )`. Capacity arguments are `count`, active lengths
are `len`, and element positions are `idx`.

Vectors store generic cells. Numeric values use `VEC-N@` / `VEC-N!`, byte
pointers use `VEC-A@` / `VEC-A!`, and generic cell code can use `VEC@` /
`VEC!`. `VEC-PUSH` grows by doubling capacity when the active length reaches
the current capacity; it throws `E-VEC-CAPACITY` only for invalid or overflowing
capacity requests and `E-VEC-BOUNDS` for invalid indexes or impossible lengths.

Use `VEC-COUNT`, `VEC-CAP-COUNT`, `VEC-LEN`, and `VEC-IDX` to refine raw
integers before calling role-typed vector words. `VEC-CAP-COUNT` rejects zero
because allocation and initialization require positive capacity; `VEC-COUNT`
allows zero for requested active length.

The data pointer field is checked with the structure DSL's `PTR-FIELD:`, which
constructs the typed `ptr ptr x` address used by normal `@` and `!`. Numeric
header fields and vector slots use `VEC-CELL-FIELD` so indexed cell addresses
remain typed as `ptr a`. Bounds, growth, copying, length, capacity, and
iteration behavior are checked Forth.

```forth
VEC-COUNT           ( n -- count )
VEC-CAP-COUNT       ( n -- count )
VEC-LEN             ( n -- len )
VEC-IDX             ( n -- idx )
VEC-CHECK-NEED      ( count -- )
VEC-CHECK-CAP       ( count -- )
VEC-CHECK-LEN       ( len -- )
VEC-CELLS>BYTES     ( count -- n )
VEC-ALLOC-CELLS     ( count -- ptr a )
VEC-CELL-FIELD      ( ptr a n -- ptr a )
VEC-DATA-FIELD      ( ptr a -- ptr ptr a )
VEC-DATA@           ( ptr a -- ptr a )
VEC-DATA!           ( ptr a ptr a -- )
VEC-LEN@            ( ptr a -- len )
VEC-CAP@            ( ptr a -- count )
VEC-CAP!            ( count ptr a -- )
VEC-LEN!            ( len ptr a -- )
VEC-INIT            ( ptr a count -- )
VEC-CLEAR           ( ptr a -- )
VEC-CHECK-INDEX     ( ptr a idx -- )
VEC@                ( ptr a idx -- a )
VEC!                ( a ptr a idx -- )
VEC-N@              ( ptr a idx -- n )
VEC-N!              ( n ptr a idx -- )
VEC-A@              ( ptr a idx -- ptr u8 )
VEC-A!              ( ptr u8 ptr a idx -- )
VEC-COPY-CELLS      ( ptr a ptr a len -- )
VEC-INSTALL-RESIZE  ( ptr a count ptr a -- )
VEC-CHECK-RESIZE-CAP ( ptr a count -- )
VEC-RESIZE          ( ptr a count -- )
VEC-GROW-CAP        ( ptr a count -- count )
VEC-ENSURE          ( ptr a count -- )
VEC-PUSH-AT         ( a ptr a n -- idx )
VEC-PUSH            ( a ptr a -- idx )
VEC-PUSH-N          ( n ptr a -- idx )
VEC-PUSH-A          ( ptr u8 ptr a -- idx )
VEC-EACH            ( R ptr a [ R idx a -- R ] -- R )
```

## FFI ABI

`lib/ffi-abi.f` is the target-independent AAPCS64 foreign-call surface. It owns
the typed scratch buffers for integer/pointer registers, floating-point
registers, stack spill slots, out-parameter readback, and CUDA-style
`void** kernelParams` packing. These helpers do not require a dynamic loader and
are part of the local checked gate on macOS and Linux.

Scratch storage is single-threaded and call-scoped: prepare one call, execute it,
then prepare the next. `FFI-KPARAM-N+` stores scalar parameters in library-owned
cells that remain stable until the next `FFI-KPARAM-RESET`; `FFI-KPARAM+` stores a
caller-owned parameter pointer, so the caller owns that pointed-to lifetime.

`lib/ffi.f` is the dynamic loader layer over `lib/ffi-abi.f`. On Linux/aarch64 it
calls `dlopen` and `dlsym` through loader-resolved dynamic ELF slots
(`DLOPEN-SLOT`, `DLSYM-SLOT`). On macOS/aarch64 the Mach-O writer emits a
`__DATA_CONST,__got` page and `LC_DYLD_CHAINED_FIXUPS` imports for libSystem
`_dlopen` and `_dlsym`; the same checked `DLOPEN`/`DLSYM` words read those
resolved slots.

```forth
FFI-ARG!          ( n n -- )
FFI-PTR-ARG!      ( ptr a n -- )
FFI-FARG!         ( r n -- )
FFI-STACK!        ( n n -- )
FFI-FSTACK!       ( r n -- )
FFI-X8!           ( n -- )
FFI-OUT@          ( ptr n -- n )
FFI-OUT!          ( n ptr n -- )
FFI-KPARAM-COUNT  ( -- n )
FFI-KPARAM-RESET  ( -- )
FFI-KPARAM+       ( ptr a -- )
FFI-KPARAM-N+     ( n -- )
FFI-KPARAMS       ( -- ptr n n )
FFI-KPARAMS>N     ( -- n )
CALL0             ( n -- n )
CALL1             ( n n -- n )
CALL2             ( n n n -- n )
CALL3             ( n n n n -- n )
CALL4             ( n n n n n -- n )
CALL5             ( n n n n n n -- n )
CALL6             ( n n n n n n n -- n )
FFI-CALLN         ( n n -- n )
FFI-CALLABI       ( n n -- n )
FFI-CALLABI-R     ( n n -- r )
>CSTR             ( ptr u8 n ptr u8 -- )
RTLD-NOW          ( -- n )
DLOPEN            ( ptr u8 n -- n )
DLSYM             ( n ptr u8 -- n )
```

## Core Bytes

`src/core/bytes.f` provides small checked byte-buffer helpers that are part of
the native prelude. They are available before stdlib and tool modules so low
level code does not depend on broad library ordering such as loading
`lib/string.f` before `lib/ffi.f`.

```forth
BYTE+           ( ptr u8 n -- ptr u8 )
BYTE-COPY-LEN   ( ptr u8 ptr u8 len -- )
BYTE-COPY       ( ptr u8 ptr u8 n -- )
```

## String

`lib/string.f` provides checked byte-string helpers. Inputs are byte pointers
plus lengths; no word assumes NUL termination unless its name says `PATHZ` or a
module boundary explicitly says it owns path conversion. `SB-*` words operate on
the shared bounded string-builder buffer and throw `E-STR-CAPACITY` or
`E-STR-BOUNDS` instead of truncating silently. `STR>NUMBER?` parses a signed
i64 and returns `0 false` on invalid or out-of-range input.
`STR-LEN`, `STR-OFF`, and `STR-COUNT` refine raw integers into nominal string
roles and reject negative values. Typed variants such as `SB-APPEND-LEN` keep
already-refined lengths from being laundered through plain `n`. `BUFFER:`
defines caller-owned byte buffers; `BUF-*` helpers reset, read,
and append into a caller-owned `(buffer, capacity, length-cell)` triple and throw
instead of truncating or overflowing.

```forth
STR-LEN         ( n -- len )
STR-OFF         ( n -- off )
STR-COUNT       ( n -- count )
STR-TRUE        ( -- bool )
STR-FALSE       ( -- bool )
BUFFER:         ( n -- )
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
SB-CHECK-LEN-ROOM ( len -- )
SB-CHECK-ROOM   ( n -- )
SB-RESET        ( -- )
SB-APPEND-LEN   ( ptr u8 len -- )
SB-APPEND       ( ptr u8 n -- )
SB-APPEND-C     ( n -- )
SB$             ( -- ptr u8 n )
BUF-CHECK-LEN   ( len len ptr len -- )
BUF-RESET       ( ptr len -- )
BUF-LEN@        ( ptr len -- n )
BUF-APPEND-LEN  ( ptr u8 len ptr u8 len ptr len -- )
BUF-APPEND      ( ptr u8 n ptr u8 n ptr len -- )
BUF-APPEND-C    ( n ptr u8 n ptr len -- )
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
next append would exceed that buffer; they never truncate silently. Caller-owned
buffer appends use the same rule and keep the current length in a `ptr len`
cell. `SPLIT-NEXT` returns the next field, the next scan index, and a success
flag.

## JSON Write

`lib/json-write.f` is a checked emit-only JSON vocabulary for fixtures, benchmark
rows, and native tools that do not need the full parser DOM from `tools/json.f`.
It owns an OS-backed growable output buffer, emits compact JSON, escapes string
control bytes/quotes/backslashes, and throws `E-JW-CAPACITY` or `E-JW-BYTE`
instead of truncating or emitting invalid bytes. Commas remain explicit so object
and array shape is visible in code. Load it after `lib/memory.f`.
`JW-LEN` refines raw byte counts into `len`, and the `*-LEN` variants preserve
that role through capacity checking and raw JSON appends.

```forth
JW-BUF-FIELD   ( -- ptr ptr u8 )
JW-BUF@        ( -- ptr u8 )
JW-BUF!        ( ptr u8 -- )
JW-BUF         ( -- ptr u8 )
JW-LEN         ( n -- len )
JW-CAP         ( -- n )
JW-STORE-SPAN  ( ptr u8 n -- )
JW-MIN-ONE     ( n -- n )
JW-NEED-CAP-LEN ( len -- n )
JW-NEED-CAP    ( n -- n )
JW-COPY-OLD    ( ptr u8 -- )
JW-GROW        ( n -- )
JW-CHECK-LEN-ROOM ( len -- )
JW-CHECK-ROOM  ( n -- )
JW-ENSURE-INITIAL ( -- )
JW-RESET       ( -- )
JW-C           ( n -- )
JW-RAW-LEN     ( ptr u8 len -- )
JW-RAW         ( ptr u8 n -- )
JW-HEX         ( n -- n )
JW-U00         ( n -- )
JW-ESC-C       ( n -- )
JW-STRING      ( ptr u8 n -- )
JW-KEY         ( ptr u8 n -- )
JW-OBJECT-START ( -- )
JW-OBJECT-END   ( -- )
JW-ARRAY-START  ( -- )
JW-ARRAY-END    ( -- )
JW-COMMA        ( -- )
JW-NULL       ( -- )
JW-BOOL       ( bool -- )
JW-U          ( n -- )
JW-FIELD-RAW  ( ptr u8 n ptr u8 n -- )
JW-FIELD-S    ( ptr u8 n ptr u8 n -- )
JW-FIELD-U    ( ptr u8 n n -- )
JW-FIELD-BOOL ( ptr u8 n bool -- )
JW-FIELD-NULL ( ptr u8 n -- )
JW$           ( -- ptr u8 n )
```

Prefer these words over constructing quoted JSON literals by hand. Use
`JW-FIELD-S` when the value is arbitrary text and `JW-FIELD-RAW` only for a
known-valid JSON fragment such as a prevalidated number lexeme.

## Regex

`lib/regex.f` exposes a bounded capture-free regex scanner and matcher for LLM
tasks: literals, `.`, `^`, `$`, character classes and negated classes, escaped
metacharacters, and `?`, `*`, `+`. v1 excludes captures, backreferences,
lookaround, and alternation unless a bounded NFA plan is implemented first. Regex
bytecode uses caller-provided `ptr u8 len` storage. Matches never return
unchecked `addr` handles; offsets are `off`, lengths are `len`, and counts are
`count`.

```forth
RX-BYTE-IN?           ( n ptr u8 n -- bool )
RX-ESCAPABLE?         ( n -- bool )
RX-UNSUPPORTED-META?  ( n -- bool )
RX-CHECK-BYTE         ( n -- )
RX-NEED               ( len off len -- )
RX-EMIT-1             ( n ptr u8 len off -- off )
RX-EMIT-LIT           ( n ptr u8 len off -- off )
RX-EMIT-RANGE         ( n ptr u8 len ptr u8 len off -- off )
RX-SCAN-CLASS-BODY    ( ptr u8 len off -- off )
RX-SCAN-CLASS         ( ptr u8 len off -- off off n )
RX-EMIT-CLASS-DONE    ( ptr u8 ptr u8 len off off off n -- off off )
RX-EMIT-CLASS         ( ptr u8 len off ptr u8 len off -- off off )
RX-SCAN-ESCAPE        ( ptr u8 len off ptr u8 len off -- off off )
RX-META-TOKEN         ( n -- n bool )
RX-EMIT-SINGLE-TOKEN  ( n off ptr u8 len off -- off off )
RX-SCAN-ONE           ( ptr u8 len off ptr u8 len off -- off off )
RX-COMPILE            ( ptr u8 len ptr u8 len -- len )
RX-CHECK-MATCH-ARGS   ( len len -- )
RX-FLAGS-CLEAR        ( ptr u8 len -- )
RX-FLAG?              ( ptr u8 off -- bool )
RX-ANY-FLAG?          ( ptr u8 len -- bool )
RX-ADD-STATE          ( ptr u8 len off -- )
RX-QUANT?             ( n -- bool )
RX-ZERO-QUANT?        ( n -- bool )
RX-CONSUMING?         ( n -- bool )
RX-ANCHOR?            ( n -- bool )
RX-FIXED-ATOM-LEN     ( len off len -- len )
RX-CLASS-RAW-LEN      ( ptr u8 len off -- len )
RX-ATOM-LEN           ( ptr u8 len off -- len )
RX-ATOM-END           ( ptr u8 len off -- off )
RX-QUANT-AT           ( ptr u8 len off -- n )
RX-AFTER-ATOM-QUANT   ( ptr u8 len off -- off )
RX-VALIDATE-STEP      ( ptr u8 len off -- off )
RX-VALIDATE           ( ptr u8 len -- )
RX-CLASS-RANGE-CAND?  ( ptr u8 len off -- bool )
RX-CLASS-RANGE-MATCH? ( n ptr u8 off -- bool )
RX-CLASS-ESC-MATCH?   ( n ptr u8 len off -- bool )
RX-CLASS-MEMBER?      ( n ptr u8 len -- bool )
RX-ATOM-CHAR-MATCH?   ( n ptr u8 len off -- bool )
RX-ANCHOR-MATCH?      ( n off len -- bool )
RX-CLOSE-ANCHOR       ( n ptr u8 len off len ptr u8 off -- )
RX-CLOSE-ZERO-QUANT   ( ptr u8 len off ptr u8 -- )
RX-CLOSE-ONE          ( ptr u8 len off len ptr u8 off -- )
RX-CLOSE              ( ptr u8 len off len ptr u8 -- )
RX-RESET-STATES       ( len -- )
RX-NEXT>ACTIVE        ( len -- )
RX-ADD-QUANT-TARGET   ( n ptr u8 len off ptr u8 -- )
RX-ADD-CONSUME-TARGET ( ptr u8 len off ptr u8 -- )
RX-CONSUME-STATE      ( ptr u8 ptr u8 len off ptr u8 ptr u8 off -- )
RX-CONSUME-CHAR       ( ptr u8 ptr u8 len off -- )
RX-ACCEPT?            ( len -- bool )
RX-PREFIX-LEN         ( ptr u8 len ptr u8 len off -- len bool )
RX-PREPARE            ( len ptr u8 len -- )
RX-MATCH?             ( ptr u8 len ptr u8 len -- bool )
RX-FIND-FROM          ( ptr u8 len ptr u8 len off -- off len bool )
RX-FIND               ( ptr u8 len ptr u8 len -- off len bool )
RX-COUNT              ( ptr u8 len ptr u8 len -- count )
```

`RX-COMPILE` takes pattern bytes plus a caller-provided bytecode buffer and
capacity, then returns the compiled byte length. Malformed patterns and bytecode
capacity overflow throw named regex errors; they do not return a partial regex
or an unchecked `addr`. `RX-MATCH?` is whole-input matching, `RX-FIND` returns
`off len true` or `0 >OFF 0 >LEN false`, and `RX-COUNT` counts non-overlapping
matches, advancing one byte after zero-length matches to avoid hangs.

## Map

`lib/map.f` provides a fixed-capacity open-addressed string-key map.
The source-backed surface uses caller-owned `ptr a` cell storage. Capacities and
stored counts are `count`, slot indexes are `idx`, slot field offsets are `off`,
and key lengths are `len`. `MAP-CELLS` returns the cell count to allocate for a
capacity, and `MAP-INIT` initializes that storage. Key strings use `ptr u8 len`.

The published words expose checked storage layout plus lookup/update helpers:

```forth
MAP-CHECK-CAP       ( count -- )
MAP-CHECK-LEN       ( len -- )
MAP-CELLS           ( count -- count )
MAP-EMPTY?          ( n -- bool )
MAP-DELETED?        ( n -- bool )
MAP-OCCUPIED?       ( n -- bool )
MAP-CAP@            ( ptr a -- count )
MAP-CAP!            ( count ptr a -- )
MAP-CHECK-HANDLE    ( ptr a count -- )
MAP-COUNT@          ( ptr a -- count )
MAP-DELETED@        ( ptr a -- count )
MAP-COUNT!          ( count ptr a -- )
MAP-DELETED!        ( count ptr a -- )
MAP-SLOTS           ( ptr a -- ptr a )
MAP-CHECK-INDEX     ( ptr a idx -- )
MAP-SLOT            ( ptr a idx -- ptr a )
MAP-SLOT-FIELD      ( ptr a idx off -- ptr a )
MAP-SLOT-STATE@     ( ptr a idx -- n )
MAP-SLOT-STATE!     ( n ptr a idx -- )
MAP-SLOT-HASH@      ( ptr a idx -- n )
MAP-SLOT-HASH!      ( n ptr a idx -- )
MAP-SLOT-KEY-A@     ( ptr a idx -- ptr u8 )
MAP-SLOT-KEY-A!     ( ptr u8 ptr a idx -- )
MAP-SLOT-KEY-U@     ( ptr a idx -- len )
MAP-SLOT-KEY-U!     ( len ptr a idx -- )
MAP-SLOT-VALUE@     ( ptr a idx -- a )
MAP-SLOT-VALUE!     ( a ptr a idx -- )
MAP-SLOT-CLEAR      ( ptr a idx -- )
MAP-CLEAR           ( ptr a -- )
MAP-INIT            ( ptr a count -- )
MAP-HASH            ( ptr u8 len -- n )
MAP-INDEX           ( n count -- idx )
MAP-PROBE           ( n count count -- idx )
MAP-SLOT-MATCH?     ( ptr a idx n ptr u8 len -- bool )
MAP-REMEMBER-FREE   ( n idx -- n )
MAP-LOCATE-SLOT     ( n ptr a idx ptr u8 len n -- n n n )
MAP-LOCATE          ( ptr a count ptr u8 len -- n n n )
MAP-SLOT-INSERT     ( a ptr a idx n ptr u8 len -- )
MAP-HAS?    ( ptr a count ptr u8 len -- bool )
MAP-GET     ( ptr a count ptr u8 len -- n bool )
MAP-SET     ( n ptr a count ptr u8 len -- )
MAP-EACH    ( ptr a count [ ptr u8 len n -- ] -- )
```

`MAP-GET` returns value plus present flag. `MAP-SET` inserts or replaces one
numeric value. Capacity, malformed storage, and full-table states throw named
errors such as `E-MAP-BAD-CAP` and `E-MAP-FULL`.

## Memory

`lib/memory.f` provides checked OS-backed byte buffers for large composed tools.
Use this module when a tool needs source bundles, JSONL streams, report tables,
or many 64K scratch buffers. Do not split tools merely to avoid DATA pressure:
`create ... allot` is for dictionary-sized static storage, while `MEM-*`
allocation is for runtime-sized storage backed by anonymous `mmap`.

The raw `mmap` primitive remains a boundary because it can return `-1`. The
published allocation words validate sizes, convert successful mappings to typed
`ptr u8` storage through the audited internal `MEM-ALLOC-PTR` boundary, and
throw `E-MEM-SIZE` or `E-MEM-MAP` on failure.

```forth
MEM-CHECK-SIZE        ( n -- )
MEM-CHECK-64K-COUNT   ( n -- )
MEM-CHECK-CELL-COUNT  ( count -- )
MEM-64K-BYTES         ( n -- n )
MEM-64K-COUNT-FOR     ( n -- n )
MEM-64K-SPAN-BYTES    ( n -- n )
MEM-CELLS>BYTES       ( count -- n )
MEM-MMAP-RC           ( n -- n )
MEM-ALLOC-BYTES       ( n -- ptr u8 n )
MEM-ALLOC-CELLS       ( count -- ptr a )
MEM-ALLOC-64K-BUFFERS ( n -- ptr u8 n )
MEM-ALLOC-64K-SPAN    ( n -- ptr u8 n )
MEM-ALLOC-64K         ( -- ptr u8 n )
```

`MEM-MAP-SHARED` is the named shared-mapping flag for checked device or file
mappings that use the raw `mmap` primitive directly.

`MEM-ALLOC-CELLS` validates a positive checked `count`, computes the byte size
with overflow bounded by `MEM-MAX-N`, and returns a typed cell span. Use it for
tables or vectors that store normal cells; use `ptr-field` when the table handle
itself is stored in another cell.

`MEM-ALLOC-64K-BUFFERS` returns one contiguous byte span sized for the caller's
chosen `n` 64K buffers. The returned length is the capacity in bytes; callers
index individual 64K slots as `base index MEM-64K * +`. Callers may keep any
number of spans live at once. Habu code must not encode a repo-local "maximum
number of 64K buffers" outside the explicit overflow validation in this library
and the OS mapping result.

`MEM-ALLOC-64K-SPAN` takes a minimum byte need, rounds it up to the smallest
whole number of 64K buffers, and returns the pointer plus rounded capacity. Use
it for source/report buffers whose exact required size is known only at runtime.

## Files

`lib/fs.f` is the canonical filesystem helper surface. Public path words accept
counted byte strings and own any private NUL-terminated copy needed for syscalls.

The current source-backed surface covers path predicates, stat mode and size,
basename, bounded path joining, bounded file I/O, file mutation, and recursive file
walking:

```forth
FS-FALSE           ( -- bool )
FS-TRUE            ( -- bool )
FS-U16@            ( ptr u8 -- n )
FS-U64@            ( ptr u8 -- n )
FS-CHECK-JOIN-CAP       ( n -- )
FS-PATHZ-INTO           ( ptr u8 n ptr u8 -- ptr u8 )
FS-PATHZ                ( ptr u8 n -- ptr u8 )
EXISTS?                 ( ptr u8 n -- bool )
FS-STAT-MODE@           ( -- n )
FS-STAT-SIZE@           ( -- n )
FS-STAT-MTIME-SEC@      ( -- n )
FS-STAT-MTIME-NS@       ( -- n )
FS-STAT-CTIME-SEC@      ( -- n )
FS-STAT-CTIME-NS@       ( -- n )
FS-TRY-STAT             ( ptr u8 n -- bool )
FS-TRY-LSTAT            ( ptr u8 n -- bool )
FS-TRY-STAT-MODE        ( ptr u8 n -- n )
FS-TRY-LSTAT-MODE       ( ptr u8 n -- n )
STAT-MODE               ( ptr u8 n -- n )
FILE-SIZE               ( ptr u8 n -- n )
FILE-META               ( ptr u8 n -- n n n n n )
FILE?                   ( ptr u8 n -- bool )
DIR?                    ( ptr u8 n -- bool )
SYMLINK?                ( ptr u8 n -- bool )
EXECUTABLE?             ( ptr u8 n -- bool )
BASENAME                ( ptr u8 n -- ptr u8 n )
JOIN-PATH               ( ptr u8 n ptr u8 n ptr u8 -- n )
READ-LINK               ( ptr u8 n ptr u8 n -- n )
READ-ALL                ( ptr u8 n ptr u8 n -- n )
FS-WRITE-BY-FLAGS       ( ptr u8 n ptr u8 n n -- )
WRITE-ALL               ( ptr u8 n ptr u8 n -- )
APPEND-FILE             ( ptr u8 n ptr u8 n -- )
OPEN-APPEND-FD          ( ptr u8 n -- n )
FS-MUT-PATHZ2           ( ptr u8 n -- ptr u8 )
REMOVE-FILE             ( ptr u8 n -- )
RENAME-FILE             ( ptr u8 n ptr u8 n -- )
CHMOD-X                 ( ptr u8 n -- )
CHMOD-MODE              ( ptr u8 n n -- )
MAKE-SYMLINK            ( ptr u8 n ptr u8 n -- )
MKDIR-MODE              ( ptr u8 n n -- )
MAKE-DIR                ( ptr u8 n -- )
REMOVE-DIR              ( ptr u8 n -- )
REMOVE-TREE             ( ptr u8 n -- )
MAKE-DIRS               ( ptr u8 n -- )
COPY-FILE               ( ptr u8 n ptr u8 n n -- )
COPY-FILE-STREAM        ( ptr u8 n ptr u8 n -- )
ATOMIC-WRITE-FILE       ( ptr u8 n ptr u8 n -- )
MAKE-TEMP-DIR           ( ptr u8 n ptr u8 n -- ptr u8 n )
TMPDIR-MKDIR            ( ptr u8 n -- ptr u8 n )
CLEANUP-RESET           ( -- )
CLEANUP+                ( ptr u8 n -- )
CLEANUP-DIR+            ( ptr u8 n -- )
CLEANUP-TREE+           ( ptr u8 n -- )
CLEANUP-RUN             ( -- )
FS-SKIP-DIR?            ( ptr u8 n -- bool )
FS-SKIP-SELF-ENTRY?     ( ptr u8 n -- bool )
FS-SKIP-ENTRY?          ( ptr u8 n -- bool )
FS-CHECK-WALK-DESCEND   ( -- )
FS-OPEN-WALK-DIR        ( ptr u8 n -- )
FS-CLOSE-CUR-DIR        ( -- )
FS-DIR-BLOCK-BEGIN      ( -- )
FS-DIR-MORE?            ( -- bool )
FS-LOAD-ENTRY           ( -- )
FS-ADVANCE-ENTRY        ( -- )
FS-DESCEND-PATH         ( ptr u8 n ptr u8 n -- ptr u8 n )
FS-ASCEND-PATH          ( -- )
FS-WALK-PATH            ( ptr u8 n [ ptr u8 n -- ] -- )
WALK-FILES   ( ptr u8 n [ ptr u8 n -- ] -- )
```

`WALK-FILES` walks regular files depth-first, skips `.git`, `.jj`, and
`.dots`, uses per-depth buffers, and closes active directory descriptors before
throwing explicit filesystem errors.

`READ-ALL` reads a regular file into caller storage and returns the byte count.
The caller supplies the explicit output cap. Files larger than the cap throw
`E-FS-CAPACITY`; open and I/O failures throw `E-FS-OPEN` or `E-FS-IO`.
Use `FS-O-WRONLY` or `FS-O-RDWR` when a caller must pass access-mode flags
directly to the checked `open` primitive.
`WRITE-ALL` creates/truncates a regular file, and `APPEND-FILE` creates/appends
to a regular file. Both write the full counted input or throw a named filesystem
error. `OPEN-APPEND-FD` opens the same append-only regular-file target and
returns an fd for callers that need to stream child process output directly into
a file.
`SYMLINK?` uses `lstat64`, so it detects a link itself rather than following the
target. `READ-LINK` reads the target bytes into caller storage and returns the
byte count without appending a NUL; missing, non-link, I/O, and capacity failures
throw named filesystem errors.

`lib/fs-mutate.f` is layered after the native engine contains mutation
primitives such as `unlink`, `rename`, `chmod`, `mkdir`, and `rmdir`. It owns
counted-path wrappers for files and directories. Public wrappers avoid the exact
primitive names because the dictionary is case-insensitive: use `MAKE-DIR`,
`REMOVE-DIR`, and `MAKE-DIRS`, not uppercase shadows of `mkdir` or `rmdir`.
`CHMOD-MODE` applies an explicit permission mode to one counted path, while
`CHMOD-X` preserves existing permission bits and adds executable bits.
`MAKE-SYMLINK` creates a link from counted target bytes to a counted link path.
`RENAME-FILE` and `MAKE-SYMLINK` use a second private pathz buffer so preparing
the destination path cannot overwrite the source path. `COPY-FILE` reads through
an explicit caller capacity and throws `E-FS-CAPACITY` instead of truncating.
`COPY-FILE-STREAM` copies through the module chunk buffer, so callers can copy
large files without sizing a whole-file scratch buffer. `ATOMIC-WRITE-FILE`
writes a sibling `.tmp` file and renames it over the destination.
`REMOVE-TREE` recursively removes one counted path, using the same per-depth walk
buffers, directory-entry helpers, child-path enter/leave helpers, and close
handling as `WALK-FILES`, while keeping mutation policy in `lib/fs-mutate.f`.
It throws named filesystem errors rather than ignoring partial deletion failures.
If a tree contains a symlink to a directory, `REMOVE-TREE` unlinks the symlink
itself and never descends into the target.

`MAKE-TEMP-DIR` creates a private unique directory under an explicit base path,
retrying bounded deterministic candidates if a name already exists.
`TMPDIR-MKDIR` uses `$TMPDIR` or `/tmp`. Cleanup registrations copy counted
paths into owned storage and `CLEANUP-RUN` removes them in reverse order, so
nested directory cleanups can register parent before child and still remove child first.
`CLEANUP-TREE+` registers a recursive tree cleanup for temporary workspaces.
Keeping these words outside core `lib/fs.f` keeps path inspection/read helpers
separate from mutation and cleanup policy.

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

`FILE-META` requires a regular file and returns size, mtime seconds, mtime
nanoseconds, ctime seconds, and ctime nanoseconds from the normalized shared stat
layout. Content-key caching uses this metadata to skip rehashing unchanged files.

## Content Keys

`lib/content-key.f` builds stable manifest hashes for gate and builder caches.
Callers append version strings and source files, then hash the accumulated
manifest into a binary or hex digest:

```forth
CK-RESET          ( -- )
CK-TEXT+          ( ptr u8 n -- )
CK-DIGEST+        ( ptr u8 -- )
CK-FILE+          ( ptr u8 n -- )
CK-FINAL          ( ptr u8 -- )
CK-FINAL-HEX      ( ptr u8 -- )
```

`CK-FILE+` records the path in the manifest but hashes file content through a
metadata-validated per-file digest cache when one is configured. `CK-CACHE-PATH!`
sets an explicit cache file, `CK-CACHE-ROOT!` uses `content-key.cache` under a
root directory, and `CK-CACHE-CLEAR!` clears the explicit setting. The test
suite installs this root in-process; content-key does not read environment
variables.

## Object Records

`lib/object.f` owns the `OBJ` package: a deterministic object-record codec for
the future linkable Habu build path. It is not wired into `hb-build` yet. The
current build caches store finished executables and maker artifacts; object
records are the typed pre-link contract for a later compiler/linker slice.

```forth
OBJ:RESET      ( -- )
OBJ:SOURCE!    ( ptr u8 n -- )
OBJ:TARGET!    ( ptr u8 n -- )
OBJ:CHECKER!   ( ptr u8 n -- )
OBJ:COMPILER!  ( ptr u8 n -- )
OBJ:REQUIRE+   ( ptr u8 n -- )
OBJ:TEXT+      ( ptr u8 n -- )
OBJ:DATA+      ( ptr u8 n -- )
OBJ:PACKAGE+   ( ptr u8 n ptr u8 n -- )
OBJ:EXPORT+    ( ptr u8 n ptr u8 n -- )
OBJ:DEF+       ( ptr u8 n n ptr u8 n -- )
OBJ:IMPORT+    ( ptr u8 n ptr u8 n -- )
OBJ:TYPE+      ( ptr u8 n ptr u8 n -- )
OBJ:RELOC+     ( ptr u8 n n ptr u8 n -- )
OBJ:NORET+     ( ptr u8 n -- )
OBJ:BYTES$     ( -- ptr u8 n )
OBJ:MAX-BYTES  ( -- n )
OBJ:ROW-COUNT  ( -- n )
OBJ:ROW$       ( n -- ptr u8 n )
OBJ:ROW-TAG$   ( n -- ptr u8 n )
OBJ:ROW-FIELD# ( n -- n )
OBJ:ROW-FIELD$ ( n n -- ptr u8 n )
OBJ:LOAD       ( ptr u8 n -- )
OBJ:KEY-HEX    ( ptr u8 -- )
```

`OBJ:SOURCE!` requires a 64-byte hex source digest. Field strings reject tabs,
newlines, control bytes, and empty fields so the tab-separated format remains
canonical. `OBJ:TEXT+` and `OBJ:DATA+` encode binary section bytes as lowercase
hex records, and `OBJ:LOAD` rejects malformed section hex. `OBJ:BYTES$` and
`OBJ:KEY-HEX` require source, target, checker, and compiler ABI fields before
returning data. `OBJ:DEF+` records an address-bearing text definition as
symbol, text byte offset, and effect. Row accessors expose the validated record
body without the magic header: indexes are zero-based, fields exclude the tag,
and bad row or field indexes throw `E-OBJ-FIELD`. `OBJ:LOAD` validates and
restores a serialized record; `OBJ:KEY-HEX` hashes the canonical bytes through
`lib/content-key.f`.

`lib/object-cache.f` owns the `OBJSTORE` package: a content-addressed file
store for validated object records. It is intentionally separate from the
object codec and from build/link integration.

```forth
OBJSTORE:ROOT!   ( ptr u8 n -- )
OBJSTORE:ROOT$   ( -- ptr u8 n )
OBJSTORE:PATH$   ( ptr u8 n -- ptr u8 n )
OBJSTORE:EXISTS? ( ptr u8 n -- bool )
OBJSTORE:STORE   ( -- ptr u8 n )
OBJSTORE:LOAD    ( ptr u8 n -- )
```

`OBJSTORE:STORE` validates the current `OBJ` record, hashes it with
`OBJ:KEY-HEX`, creates the root directory tree, and atomically writes
`<root>/<64-hex>.hbo`. `OBJSTORE:LOAD` reads by key and validates through
`OBJ:LOAD`; missing files throw filesystem errors, and malformed object bytes
throw object schema errors.

`lib/object-link.f` owns the `OBJLINK` package: the checked symbol validation
pass for a future object linker. It copies export/import names out of the
current `OBJ` record, rejects duplicate exports, and checks imports after all
objects have been added.

```forth
OBJLINK:RESET        ( -- )
OBJLINK:EXPORT-COUNT ( -- n )
OBJLINK:IMPORT-COUNT ( -- n )
OBJLINK:DEF-COUNT    ( -- n )
OBJLINK:RELOC-COUNT  ( -- n )
OBJLINK:OBJECT-COUNT ( -- n )
OBJLINK:TEXT-SIZE    ( -- n )
OBJLINK:DATA-SIZE    ( -- n )
OBJLINK:TEXT$        ( -- ptr u8 n )
OBJLINK:DATA$        ( -- ptr u8 n )
OBJLINK:OBJECT-TEXT-BASE ( n -- n )
OBJLINK:OBJECT-DATA-BASE ( n -- n )
OBJLINK:OBJECT-TEXT-SIZE ( n -- n )
OBJLINK:OBJECT-DATA-SIZE ( n -- n )
OBJLINK:EXPORT$      ( n -- ptr u8 n )
OBJLINK:IMPORT$      ( n -- ptr u8 n )
OBJLINK:DEF$         ( n -- ptr u8 n )
OBJLINK:EXPORT-EFFECT$ ( n -- ptr u8 n )
OBJLINK:IMPORT-EFFECT$ ( n -- ptr u8 n )
OBJLINK:DEF-EFFECT$ ( n -- ptr u8 n )
OBJLINK:RELOC-KIND$  ( n -- ptr u8 n )
OBJLINK:RELOC-SYM$   ( n -- ptr u8 n )
OBJLINK:DEF-ADDR     ( n -- n )
OBJLINK:RELOC-PATCH  ( n -- n )
OBJLINK:RELOC-TARGET ( n -- n )
OBJLINK:EXPORT-FIND? ( ptr u8 n -- bool )
OBJLINK:DEF-FIND?    ( ptr u8 n -- bool )
OBJLINK:EXPORT+      ( ptr u8 n ptr u8 n -- )
OBJLINK:IMPORT+      ( ptr u8 n ptr u8 n -- )
OBJLINK:ADD          ( -- )
OBJLINK:CHECK        ( -- )
```

`OBJLINK:ADD` consumes the currently loaded `OBJ` record. It copies symbol names
and effect strings into bounded storage so later `OBJ:LOAD` calls cannot
invalidate link metadata. It decodes validated `text`/`data` rows into bounded
merged section buffers, records per-object text/data base and size rows before
advancing the merged section totals, and exposes the merged bytes through
`OBJLINK:TEXT$` and `OBJLINK:DATA$`. `def` rows record merged text addresses,
reject duplicate definitions, and reject object-local definition offsets outside
that object's text section. `reloc` rows record merged patch addresses and
resolve their target addresses from `def` rows during
`OBJLINK:CHECK`; reading an unresolved relocation target throws `E-OBJ-SCHEMA`.
`OBJLINK:CHECK` throws `E-OBJ-SCHEMA` for unresolved imports, import/export
effect mismatches, or relocation targets; duplicate exports/defs throw during
`OBJLINK:ADD`, relocation or definition offsets outside the current object's
text section throw `E-OBJ-SCHEMA`, and table/storage overflow throws
`E-OBJ-CAPACITY`.

## Source Materialization

`lib/source.f` provides checked helpers for bounded source assembly and small
source-list transforms. It is layered after `lib/errors.f`, `lib/string.f`, and
`lib/fs.f`. Callers supply output buffers and capacities; overflow throws
`E-FS-CAPACITY`, and file/stdin I/O failures throw named filesystem errors.

```forth
SOURCE-READ-PROBE              ( -- )
READ-STDIN-ALL                 ( ptr u8 len -- len )
SOURCE-APPEND-BYTES            ( ptr u8 len ptr u8 len ptr len -- )
SOURCE-APPEND-C                ( n ptr u8 len ptr len -- )
SOURCE-PATH-A@                 ( ptr a idx -- ptr u8 )
SOURCE-PATH-U@                 ( ptr a idx -- len )
SOURCE-APPEND-FILE             ( ptr u8 len ptr u8 len ptr len -- )
SOURCE-APPEND-PROVIDED         ( ptr u8 len ptr u8 len ptr len -- )
SOURCE-APPEND-SOURCE-FILE      ( ptr u8 len ptr u8 len ptr len -- )
CONCAT-FILES                   ( ptr a ptr a count ptr u8 len -- len )
WRITE-SOURCE-LIST              ( ptr a ptr a count ptr u8 len -- )
SOURCE-FINAL-LINE-START        ( ptr u8 len -- off )
INSERT-BEFORE-FINAL-LINE       ( ptr u8 len ptr u8 len ptr u8 len -- len )
SOURCE-LINE-END                ( ptr u8 len off -- off )
SOURCE-LINE-SKIP-WS            ( ptr u8 len -- off )
SOURCE-EXPORT-LINE?            ( ptr u8 len -- bool )
SOURCE-APPEND-COMMENTED-EXPORT ( ptr u8 len ptr u8 len ptr len -- )
SOURCE-APPEND-COMMENT-LINE     ( ptr u8 len ptr u8 len ptr len -- )
COMMENT-EXPORTS                ( ptr u8 len ptr u8 len -- len )
SOURCE-LS-CLOSE                ( -- )
SOURCE-LS-THROW                ( n -- )
SOURCE-LS-OPEN                 ( ptr u8 n -- )
SOURCE-LS-READ                 ( -- n )
SOURCE-LS-TRIM-CR              ( ptr u8 n -- ptr u8 n )
SOURCE-LS-APPEND               ( n ptr u8 n -- )
SOURCE-LS-EMIT                 ( ptr u8 [ ptr u8 n n -- ] -- )
SOURCE-LS-BYTE                 ( n ptr u8 n [ ptr u8 n n -- ] -- )
SOURCE-LS-CHUNK                ( n ptr u8 n [ ptr u8 n n -- ] -- )
SOURCE-LS-DRAIN                ( ptr u8 n [ ptr u8 n n -- ] -- )
SOURCE-FILE-LINES              ( ptr u8 n ptr u8 n [ ptr u8 n n -- ] -- )
```

`CONCAT-FILES` concatenates counted path entries from parallel pointer/length
tables into a caller buffer. `WRITE-SOURCE-LIST` writes source-list material
with a `provided` marker before each file so later `required` calls do not
reload already concatenated dependencies. `INSERT-BEFORE-FINAL-LINE` inserts a counted byte string
before the final line of another counted byte string; when the source has no
line break, insertion happens at the beginning. `COMMENT-EXPORTS` rewrites lines
whose first non-space byte sequence starts with `EXPORT ` by replacing leading
whitespace with `\ `, preserving all other bytes.

`READ-STDIN-ALL` reads fd 0 into a caller buffer and probes one extra byte when
the buffer fills so overflow fails closed instead of truncating. Use explicit
source-list mode for tools that need stdin data:
`bin/hb --load lib/source.f tool.f -- args... < data`. In that form the loader
reads source files from argv and leaves fd 0 for `READ-STDIN-ALL`.

`SOURCE-FILE-LINES` streams a counted file path through a caller-owned line
buffer and a callback `( ptr u8 n n -- )`, where the final `n` is the 1-based
line number. It emits empty lines, emits a final partial line without requiring
a trailing newline, strips a trailing carriage return before `\n`, and throws
`E-FS-CAPACITY` rather than truncating a line that exceeds the supplied line
buffer. `SOURCE-LS-*` words are the checked implementation steps behind that
streaming API.

## Processes

`lib/process.f` wraps native process primitives in checked contracts. Public
wrappers accept counted paths/commands, own conversion to private `pathz`
buffers, and never require LLM code to build C strings by hand.

```forth
PROC-WAIT-RAW       ( pid -- rc )
PROC-WAIT-STATUS-RAW ( pid -- n )
PROC-SPAWN-RAW      ( ptr u8 fd fd fd -- pid )
PROC-KILL-RAW       ( pid n -- rc )
PROC-ZCOPY          ( ptr u8 len ptr u8 len -- ptr u8 )
PROC-PATHZ          ( ptr u8 len -- ptr u8 )
PROC-WAIT-STATUS         ( pid -- n )
PROC-STATUS>OUTCOME ( n -- n n )
PROC-OUTCOME>RC     ( n n -- rc )
PROC-STATUS>RC      ( n -- rc )
PROC-WAIT-OUTCOME        ( pid -- n n )
PROC-WAIT-RC             ( pid -- rc )
PROC-SPAWN-IO            ( ptr u8 len fd fd fd -- pid )
PROC-RUN-RC              ( ptr u8 len -- rc )
PROC-RUN-IO-RC           ( ptr u8 len fd fd fd -- rc )
FD-CLOEXEC!         ( fd -- )
FD-NOSIGPIPE!       ( fd -- )
PIPE-PAIR           ( -- fd fd )
PROC-PFD-SLOT       ( idx -- ptr a )
PROC-PFD-AT!        ( fd n idx -- )
PROC-PFD!           ( fd n -- )
PROC-PFD-REVENTS    ( idx -- n )
POLL-IN             ( fd ms -- count )
POLL-IN-OR-TIMEOUT  ( fd ms -- count )
PROC-CAPTURE-RESET       ( -- )
PROC-CLOSE-CELL          ( ptr fd -- )
PROC-CLOSE-CAPTURE-FDS   ( -- )
PROC-REAP-CAPTURE        ( -- )
PROC-REAP-CAPTURE-TIMEOUT ( -- )
PROC-KILL-CAPTURE        ( -- )
PROC-THROW-CAPTURE       ( n -- )
PROC-OPEN-PIPE           ( ptr a ptr a -- )
PROC-CLOEXEC-CELL        ( ptr a -- )
PROC-SETUP-CAPTURE-FDS   ( -- )
PROC-CAPTURE-DEADLINE!   ( ms -- )
PROC-REMAINING-MS        ( -- ms )
PROC-POLL-CAPTURE        ( ms -- count )
PROC-POLL-CAPTURE-OUTCOME ( ms -- count )
PROC-READ-STREAM         ( ptr fd ptr u8 len ptr len -- )
PROC-PROBE-FULL-STREAM   ( ptr fd -- )
PROC-READ-OR-PROBE-STREAM ( ptr fd ptr u8 len ptr len -- )
PROC-DRAIN-READY         ( ptr u8 len ptr u8 len -- )
PROC-CAPTURE-DONE?       ( -- bool )
PROC-RUN-CAPTURE-LOOP    ( ptr u8 len ptr u8 len -- )
PROC-RUN-CAPTURE-OUTCOME-LOOP ( ptr u8 len ptr u8 len -- )
PROC-SPAWN-CAPTURE       ( ptr u8 -- )
RUN-CAPTURE  ( ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )
RUN-CAPTURE-OUTCOME  ( ptr u8 len ptr u8 len ptr u8 len ms -- len len n n )
```

`PROC-PATHZ` copies a counted path into the module's private NUL-terminated path
buffer and throws `E-PROC-OUTPUT` if the path does not fit. `PROC-RUN-RC` composes the
checked `PROC-SPAWN-IO` and `PROC-WAIT-RC` wrappers rather than the unchecked runtime
`run-rc` primitive. `PROC-RUN-IO-RC` is the same checked run-and-wait path with
explicit stdin, stdout, and stderr fds. `PROC-SPAWN-IO` and `PROC-WAIT-RC` throw
`E-PROC-SPAWN` and `E-PROC-WAIT` for primitive failures.

`PROC-WAIT-STATUS` returns the raw Darwin wait status for a pid and throws
`E-PROC-WAIT` on primitive failure. `PROC-WAIT-OUTCOME` decodes that status into
`kind code`; `kind` is `PROC-OUTCOME-EXIT`, `PROC-OUTCOME-SIGNAL`, or
`PROC-OUTCOME-TIMEOUT`, and `code` is the exit code or signal number.
`PROC-OUTCOME>RC` preserves the historical exit-code API for normal exits and
maps non-exit outcomes to `128 + signal`; `PROC-STATUS>RC` and `PROC-WAIT-RC` use it.

`PROC-SPAWN-IO` takes a counted executable path followed by stdin, stdout, and stderr
`fd` roles. Negative fd values mean inherit/default; nonnegative fd values are passed
through explicitly. `PIPE-PAIR` creates a pipe as read fd then write fd.
Parent-only pipe and PTY fds must be marked close-on-exec with `FD-CLOEXEC!`
before spawning; this sets the Darwin `FD_CLOEXEC` flag. Parent write fds that
may outlive the peer reader use `FD-NOSIGPIPE!` so failed writes return an
ordinary syscall failure instead of terminating the parent. Parent code then
closes the fd after the child no longer needs it.
Every spawn path must close all fds it owns on success and failure. `POLL-IN`
polls one fd for readable input and returns the raw poll result as `count`;
`POLL-IN-OR-TIMEOUT` throws `E-PROC-TIMEOUT` for a zero poll result and
`E-PROC-OUTPUT` for poll failure.

`PROC-WAIT-RAW` and `PROC-SPAWN-RAW` are raw primitive aliases captured before
the checked wrapper names are defined. A failed raw spawn returns a negative
target code (`-errno` on macOS; the Linux exec-failure handshake still reports
negative failure), while checked wrappers convert any negative pid to
`E-PROC-SPAWN`. Application code should prefer `PROC-SPAWN-IO`,
`PROC-WAIT-RC`, and `PROC-RUN-RC`.

`lib/process-fork.f` layers fork support after native refresh because older
engines do not have the `fork` primitive during the build prelude.

```forth
PROC-FORK-RAW ( -- pid )
PROC-FORK     ( -- pid )
```

`PROC-FORK` forks the current image without exec; the parent receives the child
pid and the child receives pid 0. It is intended for isolated test workers and
other copy-on-write process boundaries where the already-loaded dictionary must
be reused. The child must exit or die after its worker body; returning into the
parent's control path is a bug. Parent code reaps the child with `PROC-WAIT-RC`
or `PROC-WAIT-OUTCOME`. A failed raw fork returns a negative target code;
`PROC-FORK` converts that to `E-PROC-SPAWN`.

`lib/process-argv.f` layers argument-vector support on top of `lib/process.f`.
It is loaded after the native engine rebuild because older seeds do not know
the raw `spawn-argv-io` primitive. It owns bounded argv table and string buffers,
so callers append counted extra args and never hand-build C argv storage.

```forth
PROC-SPAWN-ARGV-RAW   ( ptr u8 ptr a fd fd fd -- pid )
PROC-ARGV-RESET       ( -- )
PROC-ARGV-SLOT        ( idx -- ptr a )
PROC-ARGV-CHECK-EXTRA ( -- )
PROC-ARGV-ZCOPY       ( ptr u8 len -- ptr u8 )
PROC-ARGV+            ( ptr u8 len -- )
PROC-ARGV-PREPARE     ( ptr u8 len -- ptr u8 ptr a )
PROC-SPAWN-ARGV-IO         ( ptr u8 len fd fd fd -- pid )
PROC-RUN-ARGV-IO-RC        ( ptr u8 len fd fd fd -- rc )
PROC-ARGV-CHECK-PATH  ( ptr u8 len -- )
PROC-SPAWN-ARGV-CAPTURE ( ptr u8 ptr a -- )
RUN-ARGV-CAPTURE      ( ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )
RUN-ARGV-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ms -- len len n n )
RUN-ARGV-STDIN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )
RUN-ARGV-STDIN-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len n n )
```

Use `PROC-ARGV-RESET`, append zero or more extra args with `PROC-ARGV+`, then
call `PROC-SPAWN-ARGV-IO`, `PROC-RUN-ARGV-IO-RC`, `RUN-ARGV-CAPTURE`, or
`RUN-ARGV-STDIN-CAPTURE` with the executable path. Use the `*-OUTCOME` variants
when the caller needs timeout/signal/exit classification instead of an rc-only
result.
`argv[0]` is always the executable path. `PROC-SPAWN-ARGV-IO` resets argv state after
the primitive spawn returns, throws `E-PROC-SPAWN` on spawn failure, and reuses
the same fd inheritance rules as `PROC-SPAWN-IO`.

`RUN-CAPTURE` and `RUN-ARGV-CAPTURE` take stdout buffer/capacity, stderr
buffer/capacity, and timeout `ms`. `RUN-CAPTURE` runs a counted
executable path with no extra args; `RUN-ARGV-CAPTURE` runs the current prepared
argv vector and then resets argv state. Both return stdout length, stderr length,
and rc in that order. Captures are bounded by the caller capacities; if either
stream would exceed its capacity, the word throws `E-PROC-TRUNCATED` rather than
truncating silently. Exact-capacity output is accepted when the next read
observes EOF. On timeout, it sends `SIGKILL` through the checked
`PROC-KILL-RAW` boundary, waits for the child, closes owned fds, and then throws
`E-PROC-TIMEOUT`. Truncation and other capture failures also clean up all owned
fds and terminate/reap the active child before throwing a named process error.
The `*-OUTCOME` capture variants return stdout length, stderr length, outcome
kind, and outcome code. They classify timeout as `PROC-OUTCOME-TIMEOUT` instead
of throwing `E-PROC-TIMEOUT`; output truncation and other harness failures still
throw named process errors.
`RUN-ARGV-STDIN-CAPTURE` additionally writes a bounded caller-provided stdin
buffer into the child while draining stdout and stderr. The stdin write fd is
nonblocking and no-SIGPIPE; partial writes advance by the actual byte count, and
an early child close of stdin closes the parent write fd while capture continues
to the child's final rc/outcome. Its outcome variant keeps the same stdin
behavior and returns kind/code.

`lib/process-env.f` is a post-rebuild layer on top of `lib/process-argv.f` for
explicit child environments and PATH lookup. Keeping it separate preserves the
native seed path: old seeds can still load `process-argv` for the `bin/hb`
fixpoint refresh
before the newer `spawn-argv-env-io` primitive exists.

```forth
PROC-SPAWN-ARGV-ENV-RAW   ( ptr u8 ptr a ptr a fd fd fd -- pid )
PROC-ENV-RESET            ( -- )
PROC-ENV-ENTRY+           ( ptr u8 len -- )
PROC-ENV+                 ( ptr u8 len ptr u8 len -- )
PROC-ENV-DEFAULT-RESET    ( -- )
PROC-ENV-DEFAULT+         ( ptr u8 len ptr u8 len -- )
PROC-ENV-DEFAULT$?        ( ptr u8 len -- ptr u8 len bool )
PROC-ENV-PREPARE          ( -- ptr a )
PROC-ENV-INHERIT-MISSING  ( -- )
PROC-SPAWN-ARGV-ENV-IO         ( ptr u8 len fd fd fd -- pid )
PROC-RUN-ARGV-ENV-IO-RC        ( ptr u8 len fd fd fd -- rc )
RUN-ARGV-ENV-CAPTURE      ( ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )
RUN-ARGV-ENV-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ms -- len len n n )
RUN-ARGV-ENV-STDIN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )
RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len n n )
FIND-EXECUTABLE-IN-PATH   ( ptr u8 len ptr u8 len ptr u8 -- len bool )
FIND-EXECUTABLE           ( ptr u8 len ptr u8 -- len bool )
RESOLVE-EXECUTABLE        ( ptr u8 len ptr u8 -- len )
```

Call `PROC-ENV-RESET`, append exact `NAME=VALUE` entries with
`PROC-ENV-ENTRY+` or checked name/value pairs with `PROC-ENV+`, and then run one
of the env-aware wrappers. The child receives exactly the prepared env vector.
Call `PROC-ENV-INHERIT-MISSING` after explicit overrides to copy parent envp
entries whose names are not already present; explicit entries win and duplicate
names are skipped. `FIND-EXECUTABLE-IN-PATH` accepts an explicit PATH byte string
for deterministic tests, while `FIND-EXECUTABLE` reads the current process
`PATH`. `RESOLVE-EXECUTABLE` throws `E-PROC-PATH` when lookup fails.
Resident test runners and other in-process harnesses can install inherited
defaults with `PROC-ENV-DEFAULT+`; `PROC-ENV-INHERIT-MISSING` copies those
defaults before host envp entries, while explicit `PROC-ENV+` entries still win.
Use `PROC-ENV-DEFAULT$?` when an in-process fixture needs to read the same
prepared default that would be passed to a child process. Use
`PROC-ENV-DEFAULT-RESET` at harness setup boundaries.

`lib/process-command.f` adds a checked command-owned runner above argv/env. It
keeps separate command arg, env, stdin, stdout, stderr, and outcome storage, then
transfers that state into the existing `lib/process-argv.f`/`lib/process-env.f`
spawn wrappers for one run. The primitive spawn calls stay in those existing
audited boundaries.

```forth
PROC-CMD-RESET       ( -- )
PROC-CMD-ARG+        ( ptr u8 len -- )
PROC-CMD-ENV-ENTRY+  ( ptr u8 len -- )
PROC-CMD-ENV+        ( ptr u8 len ptr u8 len -- )
PROC-CMD-ENV-INHERIT ( -- )
PROC-CMD-ENV-HERMETIC ( -- )
PROC-CMD-IN-RESET    ( -- )
PROC-CMD-IN!         ( ptr u8 len -- )
PROC-CMD-RUN-OUTCOME ( ptr u8 len ms -- n n )
PROC-CMD-RUN-RC      ( ptr u8 len ms -- rc )
PROC-CMD-OUT$        ( -- ptr u8 n )
PROC-CMD-ERR$        ( -- ptr u8 n )
PROC-CMD-OUTCOME@    ( -- n n )
PROC-CMD-RC@         ( -- rc )
```

Call `PROC-CMD-RESET`, append extra args with `PROC-CMD-ARG+`, append explicit
environment entries with `PROC-CMD-ENV+` or `PROC-CMD-ENV-ENTRY+`, optionally
replace the default inherited environment with `PROC-CMD-ENV-HERMETIC`, and set
bounded stdin with `PROC-CMD-IN!`. `PROC-CMD-RUN-OUTCOME` validates the path and
timeout before transferring state into the lower-level argv/env buffers, captures
bounded stdout/stderr into command-owned buffers, stores `kind code`, and returns
that same outcome pair. `PROC-CMD-RUN-RC` returns `PROC-OUTCOME>RC` conversion
for callers that still need rc semantics. `PROC-CMD-OUT$`, `PROC-CMD-ERR$`,
`PROC-CMD-OUTCOME@`, and `PROC-CMD-RC@` expose the stored result after the run.

`lib/process-cwd.f` is a post-env layer for running prepared argv/envp children
with a child-only working directory. It uses the native
`spawn-argv-env-cwd-io` boundary so the parent process cwd never changes. The cwd
is copied into a separate NUL buffer from the executable path to avoid
overwriting `argv[0]`.

```forth
PROC-SPAWN-ARGV-ENV-CWD-RAW ( ptr u8 ptr a ptr a ptr u8 fd fd fd -- pid )
PROC-CWDZ                   ( ptr u8 len -- ptr u8 )
PROC-SPAWN-ARGV-ENV-CWD-IO      ( ptr u8 len ptr u8 len fd fd fd -- pid )
PROC-RUN-ARGV-ENV-CWD-IO-RC     ( ptr u8 len ptr u8 len fd fd fd -- rc )
PROC-SPAWN-ARGV-ENV-CWD-CAPTURE ( ptr u8 ptr a ptr a ptr u8 -- )
PROC-SPAWN-ARGV-ENV-CWD-STDIN-CAPTURE ( ptr u8 ptr a ptr a ptr u8 -- )
RUN-ARGV-ENV-CWD-CAPTURE   ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )
RUN-ARGV-ENV-CWD-STDIN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )
```

Call `PROC-ARGV-RESET`/`PROC-ENV-RESET`, append prepared args/env entries, then
use the cwd-aware run helper with executable path, cwd path, output buffers, and
timeout. The helpers reset argv/env state after the native spawn attempt;
missing or invalid cwd paths throw `E-PROC-SPAWN`, while empty or over-capacity
cwd strings throw `E-PROC-OUTPUT` before spawning.

## Tasking

`lib/task.f` provides pthread-backed CPU tasks on macOS/aarch64 and
Linux/aarch64 in package `TASK`. Load it with `require lib/task.f`; the module
owns its `errors`/`memory`/`ffi` dependencies.

```forth
TASK:TASK          ( n -- )
TASK:MIN-STACK     ( -- n )
TASK:CONSTRUCT     ( ptr a -- )
TASK:ACTIVATE      ( n ptr a -- )
TASK:SELF          ( -- ptr a )
TASK:SELF-N        ( -- n )
TASK:PAUSE         ( -- )
TASK:HALT          ( ptr a -- )
TASK:KILL          ( ptr a -- )
TASK:DONE?         ( ptr a -- bool )
TASK:#USER         ( -- n )
TASK:+USER         ( n n -- n )
TASK:HIS           ( ptr a ptr a -- ptr a )
TASK:FACILITY      ( -- )
TASK:FACILITY-INIT ( ptr a -- )
TASK:GET           ( ptr a -- )
TASK:RELEASE       ( ptr a -- )
```

Tasks execute precompiled XTs only. Dictionary/code mutation while tasks are
live is fail-closed with exit code `$4F`; on Linux this uses process-wide
`exit_group` so failed tasking programs do not leave worker threads running.
Use `TASK:+USER` for task-local cells, ordinary aligned cells plus atomics for
shared state, `TASK:FACILITY` for owner-tracked mutex storage, and `TASK:KILL`
for teardown. Worker `die` is process-fatal with the explicit code/message;
uncaught worker `throw` is process-fatal with `task: unhandled throw`.

The public tasking surface tracks the local SwiftForth manual capture in
`docs/swiftforth-task-api.md`, with `TASK:ACTIVATE` using a checked XT instead
of SwiftForth's source-body parsing form.

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
scripts and multi-source tools. `tools/argv.f` is a compatibility path to the
same module. The parser reads `SCRIPT-ARGC` and `SCRIPT-ARGV$` by default, or an
in-memory mock argv set for focused tests. `ARGV-PARSE` recognizes `--json`,
`--json-errors`, `--label NAME`, `--strict-signatures`, `--all-errors`,
`--strict-boundary`, `-o OUT`, and `--`; tokens after `--` are always
positionals, even when they begin with a dash. Unknown dash-prefixed options and
missing option values throw `ARGV-E-USAGE` after emitting the configured usage
text unless quiet mode is enabled.

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
ARGV-STRICT-SIGNATURES? ( -- bool )
ARGV-ALL-ERRORS?        ( -- bool )
ARGV-STRICT-BOUNDARY?   ( -- bool )
ARGV-LABEL-DEFAULT!     ( ptr u8 n -- )
ARGV-LABEL!             ( ptr u8 n -- )
ARGV-LABEL?             ( -- bool )
ARGV-LABEL$             ( -- ptr u8 n )
ARGV-OUT-DEFAULT!       ( ptr u8 n -- )
ARGV-OUT!               ( ptr u8 n -- )
ARGV-OUT?               ( -- bool )
ARGV-OUT$               ( -- ptr u8 n )
ARGV-OUTZ               ( -- ptr u8 )
ARGV-REQUIRE-OUT        ( -- )
ARGV-REQUIRE-LABEL      ( -- )
ARGV-PATHZ              ( ptr u8 n -- ptr u8 )
ARGV-ZCOPY              ( ptr u8 n ptr u8 n -- ptr u8 )
```

Drivers set usage/defaults, call `ARGV-PARSE`, validate positional arity with
`ARGV-EXPECT-POS` or `ARGV-EXPECT-POS-EXACT`, then read counted outputs through
`ARGV-POS$`, `ARGV-LABEL$`, `ARGV-OUT$`, and the flag predicates.
Path-oriented syscall wrappers may use `ARGV-POSZ`, `ARGV-OUTZ`, or
`ARGV-PATHZ`; these copy into the module-owned path buffer and throw
`ARGV-E-INTERNAL` on capacity failure.

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
T-LABEL-CLEAR   ( -- )
T-LABEL$        ( -- ptr u8 n )
T-LABEL         ( ptr u8 n -- )
T-LABEL.        ( -- )
T-FAIL+         ( -- )
T-ASSERT-DETAIL ( ptr u8 n -- )
T-ASSERT        ( bool -- )
T=              ( n n -- )
T<>             ( n n -- )
TTRUE           ( bool -- )
TFALSE          ( bool -- )
T-STR=          ( ptr u8 n ptr u8 n -- bool )
T$=             ( ptr u8 n ptr u8 n -- )
T$<>            ( ptr u8 n ptr u8 n -- )
TTHROWS         ( a n -- )
TTHROWSQ        ( [ -- ] n -- )
T-REPORT        ( -- )
GT-RESET        ( -- )
GT-START        ( ptr u8 n -- )
GT-CLEANUP      ( -- )
GT-PATH         ( ptr u8 n ptr u8 -- n )
GT-RUN          ( ptr u8 n n -- )
GT-RUN-DEFAULT  ( ptr u8 n -- )
GT-PROGRESS-RUN ( ptr u8 n -- )
GT-U-TYPE       ( n -- )
GT-PROGRESS-PASS ( ptr u8 n -- )
GT-RC@          ( -- n )
GT-RC=          ( n ptr u8 n -- )
GT-RC-NONZERO   ( ptr u8 n -- )
GT-TIMEOUT      ( ptr u8 n -- )
GT-STDOUT=      ( ptr u8 n ptr u8 n -- )
GT-STDERR=      ( ptr u8 n ptr u8 n -- )
GT-STDOUT-HAS   ( ptr u8 n ptr u8 n -- )
GT-STDERR-HAS   ( ptr u8 n ptr u8 n -- )
GT-FAIL+        ( ptr u8 n -- )
GT-FAILURES     ( -- n )
GT-FAIL-NAME$   ( n -- ptr u8 n )
GT-REPORT       ( -- )
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
BUILD-FALSE       ( -- bool )
BUILD-TRUE        ( -- bool )
BUILD-WHITE?      ( n -- bool )
BUILD-FIND-CHAR   ( n n -- n )
BUILD-SKIP-WHITE  ( n -- n )
BUILD-CHECK-ONE   ( n n -- )
BUILD-READ-SOURCE ( ptr u8 n -- )
BUILD-CHECK-NEXT  ( n -- n )
BUILD-CHECK       ( ptr u8 n -- )
BUILD-EXPECT      ( ptr u8 n -- )
BUILD-ARTIFACT    ( ptr u8 n ptr u8 n -- ptr u8 n )
BUILD-STEP      ( ptr u8 n [ -- n ] -- )
BUILD-RUN       ( ptr u8 n ptr u8 n -- n )
BUILD-STEP-CHECK-OFF ( n -- )
BUILD-STEP-FIELD     ( ptr a n -- ptr a )
BUILD-STEP-A!        ( ptr u8 ptr a n -- )
BUILD-STEP-A@        ( ptr a n -- ptr u8 )
BUILD-STEP-N!        ( n ptr a n -- )
BUILD-STEP-N@        ( ptr a n -- n )
BUILD-STEP-PAIR!     ( ptr u8 n ptr a n -- )
BUILD-STEP-PAIR$     ( ptr a n -- ptr u8 n )
BUILD-STEP-EMPTY!    ( ptr a n -- )
BUILD-STEP-CLEAR     ( ptr a -- )
BUILD-STEP-NAME!     ( ptr u8 n ptr a -- )
BUILD-STEP-COMMAND!  ( ptr u8 n ptr a -- )
BUILD-STEP-ARGV!     ( ptr u8 n ptr a -- )
BUILD-STEP-TMP!      ( ptr u8 n ptr a -- )
BUILD-STEP-ARTIFACT! ( ptr u8 n ptr a -- )
BUILD-STEP-NAME$     ( ptr a -- ptr u8 n )
BUILD-STEP-COMMAND$  ( ptr a -- ptr u8 n )
BUILD-STEP-ARGV$     ( ptr a -- ptr u8 n )
BUILD-STEP-TMP$      ( ptr a -- ptr u8 n )
BUILD-STEP-ARTIFACT$ ( ptr a -- ptr u8 n )
BUILD-STEP-RC@       ( ptr a -- n )
BUILD-STEP-RC!       ( n ptr a -- )
BUILD-STEP-VALIDATE  ( ptr a -- )
BUILD-STEP-RUN       ( ptr a -- n )
```

`lib/test.f` is the public checked test framework interface. It loads assertion
words from `lib/test/assert.f` and publishes suite orchestration through package
`TEST`. Assertion words throw named test errors and keep one final report path;
they never mask assertion failures. `T-LABEL` attaches a bounded case label to
the next assertion, and successful or failed assertions clear the label after
printing details. `TTHROWSQ` takes a stack-preserving quotation plus an expected
throw code and uses the checker's modeled `catch` effect. `TTHROWS` keeps the
audited execution-token boundary for top-level test scripts, where `[: ;]`
quotation syntax is unavailable.

`TEST:*` defines reusable suite/group/test orchestration. Project adapters
install typed hooks with `TEST:SETUP!`, `TEST:TEARDOWN!`, `TEST:DRAIN!`,
`TEST:ARGS-BEGIN!`, `TEST:ARG+!`, `TEST:SELECT?!`, `TEST:RUNNER!`, and
`TEST:STDIN-RUNNER!`; test files declare named parallel or sequential groups
with `TEST:GROUP-PARALLEL` / `TEST:GROUP-SEQUENTIAL`, define `TEST:SUITE` or
`TEST:SUITE-STDIN` entries, close each entry with `TEST:END-SUITE`, and execute
once with `TEST:RUN`. Fixture helper words should live in a private package, not
global stemmed names.
`lib/property.f` owns deterministic PRNG state, seed/count bounds, bounded
source buffers, modeled generator depth, and token-tail shrinking utilities.
Property execution may call an audited `evaluate` boundary for generated checked
source, but pure generators and shrink predicates remain checked helpers.
`lib/test/runner.f` layers reusable process-fixture helpers on top of `lib/fs.f`,
`lib/fs-mutate.f`, `lib/process.f`, and `lib/process-argv.f`. It creates a
cleanup-tracked temporary root, runs prepared argv captures with bounded stdout
and stderr buffers, classifies exit/signal/timeout outcomes, and accumulates
named failures so test scripts can report all local expectation failures before
exiting. Test runner paths are counted byte strings; stdout/stderr assertions
never truncate silently because process capture still enforces bounded output.
Test scripts should call `GT-PROGRESS-RUN` immediately before long subchecks and
`GT-PROGRESS-PASS` after successful completion. Long poll loops should cap their
poll timeout with `GT-PROGRESS-SLICE-MS` and call `GT-PROGRESS-WAIT` on quiet
polls so silent children still produce regular heartbeat lines. The progress
helpers print only runner labels and elapsed milliseconds; successful child
stdout/stderr remains captured unless the caller deliberately streams it.
Streaming callers that do forward child output should route capture buffers
through `GT-FLUSH-LINES-FD` during polling and `GT-FLUSH-REMAINDER-FD` at process
exit. This keeps parent progress records serialized at line boundaries: a child
line written in several chunks cannot be split by a parent heartbeat, while a
final unterminated child fragment is still emitted before PASS/FAIL.
`lib/build.f` owns build step modeling, checked source certification, artifact
path construction, and fail-closed status reporting. `BUILD-CHECK` requires a
counted source path that names a file, scans colon definitions in bounded module
storage, and certifies each definition with `CHECK!`; missing, malformed, or
uncheckable source throws `E-BUILD-SOURCE`. `BUILD-EXPECT` requires a counted
artifact path that names a file. `BUILD-ARTIFACT` joins a build root and artifact
name into the module-owned bounded path buffer, throwing `E-BUILD-PATH` for empty
or too-long components. `BUILD-STEP` runs a checked quotation returning an rc and
throws `E-BUILD-STATUS` on nonzero status. `BUILD-RUN` runs a counted command
path, throws `E-BUILD-COMMAND` if the command is not a file, throws
`E-BUILD-STATUS` on nonzero rc, and throws `E-BUILD-PATH` if the expected artifact
file is absent after a successful command. Raw process exits are only allowed at
the final CLI/script boundary.

Build step records are `BUILD-STEP-CELLS cells` caller-owned storage with counted
fields for name, executable command path, argv metadata, private temp path,
required artifact path, and last rc. `BUILD-STEP-VALIDATE` rejects missing command
files, missing temp directories, and empty artifact paths before execution.
`BUILD-STEP-RUN` validates the record, runs the command path through `BUILD-RUN`,
requires the artifact, stores the rc, and returns it. Argv is modeled as metadata
until the process layer grows argv-vector spawning; it is still part of the
checked build contract so drivers can preserve intended command arguments.

`lib/codesign.f` owns checked executable promotion and target signing policy for
build drivers that already produced an artifact. On macOS it runs
`/usr/bin/codesign` through the checked argv process layer and throws
`E-BUILD-COMMAND` when the tool is absent. On Linux there is no fake signing
tool: signing force/ensure means chmod executable, and verification means the
artifact exists and is executable. All targets throw `E-BUILD-PATH` for missing
artifacts and `E-BUILD-STATUS` for nonzero signing or verification status when a
target signing tool is actually invoked:

```forth
CODESIGN-TOOL                ( -- ptr u8 n )
CODESIGN-RC0                 ( n -- )
CODESIGN-EXPECT-TOOL         ( -- )
CODESIGN-EXPECT-FILE         ( ptr u8 n -- )
CODESIGN-EXPECT-EXECUTABLE   ( ptr u8 n -- )
CODESIGN-RUN                 ( -- n )
CODESIGN-VERIFY-RC           ( ptr u8 n -- n )
CODESIGN-VERIFY              ( ptr u8 n -- )
CODESIGN-FORCE               ( ptr u8 n -- )
CODESIGN-ENSURE              ( ptr u8 n -- )
PROMOTE-EXECUTABLE           ( ptr u8 n ptr u8 n -- )
PROMOTE-SIGNED-EXECUTABLE    ( ptr u8 n ptr u8 n -- )
```

`PROMOTE-EXECUTABLE` chmods the source artifact executable, renames it to the
destination, and verifies the destination file exists. The source path is gone
after successful promotion. `CODESIGN-ENSURE` verifies an executable path; on
macOS, when verification fails, it forces an ad-hoc signature and verifies
again. On Linux it ensures the executable bit and verifies the path.
`PROMOTE-SIGNED-EXECUTABLE` applies the target signing policy to the source,
promotes it, and verifies the promoted executable.

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
bin/hb --load \
  lib/errors.f lib/memory.f lib/vector.f \
  tools/lint/text.f tools/lint/intern.f tools/lint/token.f tools/lint/lib.f \
  tools/public-signatures.f -- lib/<module>.f
```

The manifest, docs, source coverage, and signature drift are validated by
`tools/stdlib-manifest-test.f`.

Run the focused check with:

```sh
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/process.f lib/process-argv.f tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/stdlib-manifest-test.f
```
