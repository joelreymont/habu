# Standard Library

The standard library lives under `lib/`. Checked source and its real consumers
define the operational surface. This file is the authoritative LLM-facing stdlib
guide: prompts, examples, benchmark tasks, and future module implementations must
use the effects and boundary contracts here.

## Layout

Planned module files:

- `lib/errors.f`
- `lib/array.f`
- `lib/vector.f`
- `lib/string.f`
- `lib/json-write.f`
- `lib/regex.f`
- `lib/map.f`
- `lib/memory.f`
- `lib/ffi-abi.f`
- `lib/fs.f`
- `lib/fs-root.f`
- `lib/build-cache.f`
- `lib/source.f`
- `lib/object.f`
- `lib/object-cache.f`
- `lib/object-index.f`
- `lib/object-resolve.f`
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
- `lib/test/subject.f`
- `lib/property.f`
- `lib/build.f`
- `lib/time.f`
- `lib/date.f`

Each module owns focused tests and documentation in this file. Source files stay
one concern per file, and new public/library words default to checked typed
definitions.

## LLM Surface

LLM-facing code should call the highest-level checked word that matches the
task, and should only reach for unchecked host/runtime primitives at the audited
boundaries named below. The surface below includes active source-backed words
and planned API contracts. Checked definitions in source are the published
surface; planned contracts here define the target API shape for implementation
dots and benchmark prompts.

Typed examples in prompts must use the current checked grammar exactly. Array
views use `ptr a n`; map storage uses `ptr n count`; byte strings, regex bytecode
buffers, map keys, paths, and capture buffers use `ptr u8 n`. Quotation effects
are written in brackets, for example `[ ptr u8 n -- ]`.

## Execution Vectors

Use checked deferred words for late-bound callbacks and backend hooks:
`defer ACTION ( effect )` declares the stable call surface, and checked code
installs an implementation with `[: IMPL ;] is ACTION`. Do not model dispatch as
`variable`/`@ execute` or raw `[']` storage; the checker cannot prove those xt
cells preserve the declared effect. For a fixed native callback cell, store one
checked vector bridge in the cell and retarget the vector with `is`. An unset
deferred word exits with the execution-vector error instead of silently jumping
through zero.

## Handle Representation

The checker currently has pointer types, not nominal handle types. Byte-oriented
v1 memory-backed handles use `ptr u8 n`: the pointer is the storage base and
`n` is the byte capacity or active length specified by the owning module.
Generic cell arrays use `ptr a n`; fixed-capacity map numeric storage uses
`ptr n count`. Public signatures keep those concrete representations visible.

Opaque `addr` values are boundary-only. A module may use `addr` only for values
that checked code never dereferences. When the checker cannot express a
boundary, add a checker-owned `PRIM:` axiom; unchecked colon bodies are
forbidden. Regex prose may call values `rx`, but source signatures remain typed
as `ptr u8 n`; map prose may call values `map`, but source signatures remain
typed as `ptr n count` for storage and `ptr u8 len` for keys.

## PTX

`lib/ptx/` is a separate research sub-library with its own source consumers and
tests.
`lib/ptx/header.f` provides the checked PTX kernel header vocabulary used by
`docs/ptx-sketch.md`. `KERNEL:` is a compiler keyword alias for `:`; load
`lib/errors.f lib/ptx/header.f` before kernel sources. `%BLOCK` validates legal
CUDA block sizes (multiple of 32 and `1 <= n <= 1024`). `GRID:` and `WHERE` are
compile-time header markers consumed before the checked kernel body. `lib/ptx/launch.f`
provides checked host launch guards such as `PTX-ROW-LAUNCH-CHECK ( rows cols block -- )`
so CUDA launch code rejects invalid row dimensions before calling the driver.

`lib/ptx/cg-attention.f` publishes package `ATTN`. `ATTN:CHECKED` relates Q,
K, V, and O as `[Q,D]` matrices and threads a nominal phase token through
`ATTN:STAGE-Q`, `ATTN:SCORE`, `ATTN:SOFTMAX`, `ATTN:OUTPUT`, and
`ATTN:FINISH`. Skipped or reordered phases and mismatched matrix shapes are
checker errors. `ATTN:EMIT` runs that same checked body to produce the fused
sm_87 PTX module.

## Array

`lib/array.f` provides checked helpers for cell arrays in `package ARRAY`. Call
them across package boundaries with the qualifier, for example `ARRAY:A-SUM` and
`ARRAY:A@`; the tail names used in the descriptions below are the in-package
names. Public array helpers use nominal role types: array lengths are `len`, valid indexes are `idx`, and range
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

Scratch storage is task-local DATA. Every pthread task owns its integer, float,
stack, x0..x8 extent, stack-extent, and kernel-parameter tables. A task may pause
after staging without another task corrupting the pending call. Calls still must
not nest within one task. `FFI:KPARAM-VALUE+` stores a scalar in task-owned
storage until `FFI:KPARAM-RESET`; `FFI:KPARAM+` stores a caller-owned pointer.

The checked call surface consists only of explicit per-symbol `TRUSTED:` words.
There is no binding generator and no public universal call word. Each wrapper is
a manifest-reviewed assertion of one external ABI contract: it resolves one
symbol, fixes every value/read/write direction and writable extent, invokes a
checker-`TRUSTED`-only trampoline, and returns either one machine result or no
result. Multiple-result C ABIs require an explicit x8/sret wrapper and writable
extent; declaring multiple stack outputs over one x0 return is rejected.

The staging words (`FFI:VALUE!`, `FFI:READABLE!`, `FFI:WRITABLE!`, and the mixed
ABI register/stack variants) grant no foreign-call authority. The raw integer
and mixed-ABI trampolines are checker `TRUSTED`-only capabilities. Exact audited
mixed-ABI boundaries use separate extent tables for x0..x8 and caller-packed
stack slots. x8 is the AAPCS64 indirect-result register: sret writers must use
`FFI:X8-WRITABLE!` with the complete result extent. Stack writers use the
corresponding stack slot and extent; neither table aliases the other.

`lib/ffi-abi.f` owns the sealed `FFI` package. On Linux/aarch64 the package
calls `dlopen` and `dlsym` through loader-resolved dynamic ELF slots
(`DLOPEN-SLOT`, `DLSYM-SLOT`). On macOS/aarch64 the Mach-O writer emits a
`__DATA_CONST,__got` page and `LC_DYLD_CHAINED_FIXUPS` imports for libSystem
`_dlopen` and `_dlsym`; the same checked `FFI:DLOPEN`/`FFI:DLSYM` words read those
resolved slots. The exact package bindings are `FFI:DLOPEN` and `FFI:DLSYM`.
No global loader or marshalling aliases exist. `FFI`, `CUDA`, and `TASK` seal
both wordlists after definition, so later source cannot reopen them, add a call,
or redirect a symbol.

`FFI:DLSYM` uses a dedicated task-DATA loader block, so it cannot overwrite a
staged call. Wrappers still resolve before staging to keep the foreign-call
transaction linear. Long-lived libraries such as `TASK` resolve required symbols at load and
store them as private constants. Optional libraries such as CUDA resolve inside
each explicit wrapper before `FFI:RESET`, then stage and call without exporting a
mutable function-pointer cell.

An exact binding has this shape:

```forth
TRUSTED: WRITE-ONE ( ptr a n -- n ) {: out:ptr value:n :}
   s" write_one" SYMBOL {: fn:n :}
   FFI:RESET
   out 8 0 FFI:WRITABLE!
   value 1 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 2 fn ffi-call-bounded ;
```

The wrapper's checked callers cannot reclassify `out` or change its eight-byte
extent. The boundary's source-local rationale owns the symbol contract; focused
tests cover the writer guard and the checked public effect.

```forth
FFI:RESET         ( -- )
FFI:VALUE!        ( n n -- )
FFI:READABLE!     ( ptr a n -- )
FFI:WRITABLE!     ( ptr a n n -- )
FFI:FLOAT!        ( r n -- )
FFI:STACK-VALUE!  ( n n -- )
FFI:STACK-FLOAT!  ( r n -- )
FFI:STACK-READABLE! ( ptr a n -- )
FFI:STACK-WRITABLE! ( ptr a n n -- )
FFI:X8-VALUE!     ( n -- )
FFI:X8-READABLE!  ( ptr a -- )
FFI:X8-WRITABLE!  ( ptr a n -- )
FFI:ARGS          ( -- ptr a )
FFI:FLOATS        ( -- ptr r )
FFI:STACK         ( -- ptr a )
FFI:REG-LENS      ( -- ptr n )
FFI:STACK-LENS    ( -- ptr n )
FFI:OUT@          ( ptr n -- n )
FFI:OUT!          ( n ptr n -- )
FFI:KPARAM-COUNT  ( -- n )
FFI:KPARAM-RESET  ( -- )
FFI:KPARAM+       ( ptr a -- )
FFI:KPARAM-VALUE+ ( n -- )
FFI:KPARAMS       ( -- ptr n n )
FFI:KPARAMS>CELL  ( -- n )
FFI:CSTR          ( ptr u8 n ptr u8 -- )
FFI:NOW           ( -- n )
FFI:DLOPEN        ( ptr u8 n -- n )
FFI:DLSYM         ( n ptr u8 -- n )
```

## IEEE-754 Scalar Conversion

`lib/ieee754.f` publishes scalar bit reinterpretation and integer rounding
shared by reduced-precision floating-point encoders. `IEEE754:F64>BITS ( r -- n )`
reinterprets a Habu `f64` as its exact 64-bit IEEE-754 pattern, and
`IEEE754:BITS>F64 ( n -- r )` performs the inverse reinterpretation. These are
bit casts, not numeric conversions: every bit is preserved, including signed
zero and NaN payload bits. The published words are ordinary checked wrappers.
They call the private audited `F64>BITS-RAW` and `BITS>F64-RAW` boundaries
because the checker has no typed primitive for moving one cell between the
floating-point and integer stacks.

`IEEE754:ROUND-SHIFT-EVEN ( n n -- n )` rounds a nonnegative significand right
by a nonnegative bit count using round-to-nearest, ties-to-even. A zero shift
returns the input, and a shift greater than 63 returns zero. A negative
significand or shift throws `IEEE754:E-ROUND-DOMAIN`; valid inputs have no
other error result.

`lib/float32.f` publishes scalar IEEE-754 binary32 conversion. `F32:NARROW
( r -- n )` accepts every Habu `f64` value and returns its binary32 bit pattern,
rounded to nearest with ties to even. It preserves signed zero, gradually
underflows representable binary32 subnormals, rounds smaller values to signed
zero, and converts overflow to signed infinity. NaNs retain the sign and the
representable high payload bits and are made quiet. `F32:WIDEN ( n -- r )`
interprets the low 32 bits as a binary32 pattern and widens it exactly,
preserving signed zero, normal and subnormal values, infinities, and NaN sign,
payload, and quiet/signaling state. Higher input bits do not participate in the
binary32 pattern. Neither conversion throws for an IEEE-754 input.

Both packages are deliberately scalar-only. They expose no pointer, byte-load,
byte-store, packing, or unpacking surface. `lib/float32-buffer.f` owns the raw
little-endian bridge as `F32-BUF:STORE`, `F32-BUF:LOAD`, `F32-BUF:PACK`, and
`F32-BUF:UNPACK`; callers own the buffer capacity required by each operation.
Bounded marshalling belongs to the `MEM` span and subspan APIs, where capacity
and access width can be checked before memory is touched.

## Core Bytes

`src/core/bytes.f` provides small checked byte-buffer helpers that are part of
the native prelude. They are available before stdlib and tool modules so low
level code does not depend on broad library ordering such as loading
`lib/string.f` before `lib/ffi-abi.f`.

```forth
BYTE-VIEW       ( ptr a -- ptr u8 )
BYTE+           ( ptr u8 n -- ptr u8 )
BYTE@           ( ptr u8 n -- n )
BYTE-COPY-LEN   ( ptr u8 ptr u8 len -- )
BYTE-COPY       ( ptr u8 ptr u8 n -- )
```

`BYTE-VIEW` preserves the pointer address while exposing byte-granularity
access. The resulting `ptr u8` supports `c@` and `c!`; checked code rejects
cell-sized `@` and `!` through that view.

## String

`lib/string.f` provides checked byte-string helpers. Inputs are byte pointers
plus lengths; no word assumes NUL termination unless its name says `PATHZ` or a
module boundary explicitly says it owns path conversion. `SB-*` words operate on
the shared bounded string-builder buffer and throw `E-STR-CAPACITY` or
`E-STR-BOUNDS` instead of truncating silently. `STR>NUMBER?` parses a signed
i64 and returns `option<n>`: SOME value, or NONE on invalid or out-of-range
input.
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
FIND-SUB        ( ptr u8 n ptr u8 n -- option<idx> )
CONTAINS?       ( ptr u8 n ptr u8 n -- bool )
INDEX-OF        ( ptr u8 n n -- option<idx> )
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
STR-PARSE-POS   ( ptr u8 n -- option<n> )
STR-PARSE-NEG   ( ptr u8 n -- option<n> )
STR>NUMBER?     ( ptr u8 n -- option<n> )
```

`FIND-SUB` and `INDEX-OF` return `option<idx>` (SOME index, else NONE — every
caller MATCHes the absent case; an empty `FIND-SUB` needle is SOME 0). Builder
words append to the
module's current string-builder buffer and throw a named capacity error when the
next append would exceed that buffer; they never truncate silently. Caller-owned
buffer appends use the same rule and keep the current length in a `ptr len`
cell. `SPLIT-NEXT` returns the next field, the next scan index, and a success
flag.

## UTF-8 Scalar

`lib/utf8-scalar.f` provides one reentrant decoder in `package UTF8`.
`UTF8:NEXT` takes a counted byte span and an explicit absolute cursor. Its
`UTF8:scalar-step` result has two exhaustive arms: `scalar` carries the valid
Unicode scalar and next cursor, while `raw-byte` carries the exact lead byte and
cursor plus one. Malformed, truncated, non-shortest, surrogate, and out-of-range
sequences use `raw-byte`; a cursor outside the span throws `E-STR-BOUNDS` before
any read. The package owns no mutable cursor, scratch cell, or return buffer.

```forth
UTF8:NEXT ( ptr u8 n n -- scalar-step )
```

## JSON Write

`lib/json-write.f` is a checked emit-only JSON vocabulary for fixtures, benchmark
rows, and native tools that do not need the full parser DOM from `tools/json.f`.
It owns an OS-backed growable output buffer, emits compact JSON, escapes string
control bytes/quotes/backslashes, and throws `E-JW-CAPACITY` or `E-JW-BYTE`
instead of truncating or emitting invalid bytes. Commas remain explicit so object
and array shape is visible in code. Load it after `lib/memory.f`. The module
lives in `package JSON-WRITE`; the growable output buffer, the capacity/length
refinement helpers, and the single-byte and escape emitters are package-private,
so callers use only the qualified public emitters below.

```forth
JSON-WRITE:RESET        ( -- )
JSON-WRITE:RAW          ( ptr u8 n -- )
JSON-WRITE:STRING       ( ptr u8 n -- )
JSON-WRITE:KEY          ( ptr u8 n -- )
JSON-WRITE:OBJECT-START ( -- )
JSON-WRITE:OBJECT-END   ( -- )
JSON-WRITE:ARRAY-START  ( -- )
JSON-WRITE:ARRAY-END    ( -- )
JSON-WRITE:COMMA        ( -- )
JSON-WRITE:NULL         ( -- )
JSON-WRITE:BOOL         ( bool -- )
JSON-WRITE:U            ( n -- )
JSON-WRITE:FIELD-RAW    ( ptr u8 n ptr u8 n -- )
JSON-WRITE:FIELD-S      ( ptr u8 n ptr u8 n -- )
JSON-WRITE:FIELD-U      ( ptr u8 n n -- )
JSON-WRITE:FIELD-BOOL   ( ptr u8 n bool -- )
JSON-WRITE:FIELD-NULL   ( ptr u8 n -- )
JSON-WRITE:$            ( -- ptr u8 n )
```

Prefer these words over constructing quoted JSON literals by hand. Use
`JSON-WRITE:FIELD-S` when the value is arbitrary text and `JSON-WRITE:FIELD-RAW`
only for a known-valid JSON fragment such as a prevalidated number lexeme.

## JSON Read

`lib/json-read.f` is a checked, zero-allocation pull reader. Each parse has an
explicit linear `JR:reader`; there is no current-reader global, so independent
readers may be interleaved or nested under `catch` without sharing cursor,
nesting, token, number, or string-decode state.

The caller allocates `JR:STORAGE-BYTES` bytes at a cell-aligned address, keeps
that storage and the borrowed source live and exclusive until `JR:CLOSE`, and
does not reuse either as mutable reader backing while the token is live. `INIT`
rejects null or misaligned storage, insufficient capacity, a negative source
length, and a null source paired with a positive length before minting the
token, using `JR:E-STORAGE`, `JR:E-CAPACITY`, and `JR:E-SOURCE`. The package is
sealed after assembly so callers cannot reopen it to reach the private mint or
projection leaves. The current type system cannot prove the backing extent,
lifetime, or exclusivity from raw host storage; those promises remain the one
audited private representation boundary owned by
`habu-add-bounded-host-b40b048f`.

```forth
JR:STORAGE-BYTES ( -- n )
JR:INIT       ( ptr a n ptr u8 n -- JR:reader )
JR:CLOSE      ( JR:reader -- )
JR:TOKEN      ( JR:reader -- JR:reader n )
JR:SPAN$      ( JR:reader -- JR:reader ptr u8 n )
JR:NEXT       ( JR:reader -- JR:reader n )
JR:INT        ( JR:reader -- JR:reader n )
JR:FLOAT      ( JR:reader -- JR:reader r )
JR:STR        ( JR:reader ptr u8 n -- JR:reader n )
JR:SKIP-VALUE ( JR:reader -- JR:reader )
JR:FIND-KEY   ( JR:reader ptr u8 n -- JR:reader bool )
```

Every operation returns the same reader token except `CLOSE`, which consumes
it. `SPAN$` borrows the raw source span and leaves escapes encoded; `STR`
decodes the current string into non-null caller storage and rejects negative or
insufficient capacity with `E-JR-STATE`. `NEXT` accepts a string token only
after validating every escape, UTF-16 surrogate pair, and raw UTF-8 scalar.
Token kinds remain the public `JR:T-*` constants.
`FIND-KEY` is valid only while positioned to search the current object. It
captures that object's depth, skips each unmatched value completely, and stops
at the matching object close; array, top-level scalar, and after-key phases
throw `E-JR-STATE`. Key comparison streams decoded bytes through the same
unescape path as `STR`, so valid key length is not bounded by reader storage.

## Codegen

`lib/codegen.f` provides one bounded generated-source buffer descriptor:
`[cap][len][bytes...]`, exposed as `ptr n`. `BUFFER` is its sole definer.
Overflow throws `E-CG-CAP`; a negative decimal throws `E-CG-VALUE`.

```forth
CODEGEN:BUFFER         ( n -- )
CODEGEN:RESET          ( ptr n -- )
CODEGEN:APPEND-BYTE    ( n ptr n -- )
CODEGEN:APPEND-STRING  ( ptr u8 n ptr n -- )
CODEGEN:APPEND-DECIMAL ( n ptr n -- )
CODEGEN:CONTENTS       ( ptr n -- ptr u8 n )
```

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
The source-backed surface uses caller-owned `ptr n` cell storage. Capacities and
stored counts are `count`, slot indexes are `idx`, slot field offsets are `off`,
and key lengths are `len`. `MAP-CELLS` returns the cell count to allocate for a
capacity, and `MAP-INIT` initializes that storage. Values are `n`. Key strings
use `ptr u8 len`; their address cells are accessed through `ptr-field`.

Per-slot lifecycle state is the `slot-state` enum family (`empty`, `deleted`,
`occupied`) with generated constructors `SLOT--STATE:EMPTY`,
`SLOT--STATE:DELETED`, and `SLOT--STATE:OCCUPIED`. The checker forces every
consumer through `MATCH slot-state` or the `MAP-*?` predicates. The nominal
getter/setter API prevents checked callers from laundering state through `n`.
The caller still owns raw map storage and `MAP-SLOT-FIELD` intentionally exposes
its representation. `MAP-SLOT-STATE@` is therefore a validating decoder: raw
tags 0/1/2 become constructors and every other tag throws `ENGINE-ERROR:BAD-TAG`.

The lookup verdict is the `map-loc` sum family: `full` (table exhausted,
no payload), `free idx` (insertion slot), and `found idx` (hit slot), with
generated constructors `MAP--LOC:FULL`, `MAP--LOC:FREE`, and `MAP--LOC:FOUND`.
The carried `idx` payload replaces the old `-1` index placeholder, and every
consumer dispatches through exhaustive `MATCH map-loc`.

The published words expose checked storage layout plus lookup/update helpers:

```forth
MAP-CHECK-CAP       ( count -- )
MAP-CHECK-LEN       ( len -- )
MAP-CELLS           ( count -- count )
MAP-EMPTY?          ( slot-state -- bool )
MAP-DELETED?        ( slot-state -- bool )
MAP-OCCUPIED?       ( slot-state -- bool )
MAP-CAP@            ( ptr n -- count )
MAP-CAP!            ( count ptr n -- )
MAP-CHECK-HANDLE    ( ptr n count -- )
MAP-COUNT@          ( ptr n -- count )
MAP-DELETED@        ( ptr n -- count )
MAP-COUNT!          ( count ptr n -- )
MAP-DELETED!        ( count ptr n -- )
MAP-SLOTS           ( ptr n -- ptr n )
MAP-CHECK-INDEX     ( ptr n idx -- )
MAP-SLOT            ( ptr n idx -- ptr n )
MAP-SLOT-FIELD      ( ptr n idx off -- ptr n )
MAP-SLOT-STATE@     ( ptr n idx -- slot-state )
MAP-SLOT-STATE!     ( slot-state ptr n idx -- )
MAP-SLOT-HASH@      ( ptr n idx -- n )
MAP-SLOT-HASH!      ( n ptr n idx -- )
MAP-SLOT-KEY-A@     ( ptr n idx -- ptr u8 )
MAP-SLOT-KEY-A!     ( ptr u8 ptr n idx -- )
MAP-SLOT-KEY-U@     ( ptr n idx -- len )
MAP-SLOT-KEY-U!     ( len ptr n idx -- )
MAP-SLOT-VALUE@     ( ptr n idx -- n )
MAP-SLOT-VALUE!     ( n ptr n idx -- )
MAP-SLOT-CLEAR      ( ptr n idx -- )
MAP-CLEAR           ( ptr n -- )
MAP-INIT            ( ptr n count -- )
MAP-HASH            ( ptr u8 len -- n )
MAP-INDEX           ( n count -- idx )
MAP-PROBE           ( n count count -- idx )
MAP-SLOT-MATCH?     ( ptr n idx n ptr u8 len -- bool )
MAP-REMEMBER-FREE   ( n idx -- n )
MAP-LOCATE-SLOT     ( n ptr n idx ptr u8 len n -- n map-loc )
MAP-LOCATE          ( ptr n count ptr u8 len -- map-loc n )
MAP-SLOT-INSERT     ( n ptr n idx n ptr u8 len -- )
MAP-HAS?    ( ptr n count ptr u8 len -- bool )
MAP-GET     ( ptr n count ptr u8 len -- option<n> )
MAP-SET     ( n ptr n count ptr u8 len -- )
MAP-EACH    ( ptr n count [ ptr u8 len n -- ] -- )
```

`MAP-GET` returns `SOME` with the stored value when the key is present, else
`NONE`. `MAP-SET` inserts or replaces one numeric value. Capacity, malformed
storage, and full-table states throw named errors such as `E-MAP-BAD-CAP` and
`E-MAP-FULL`.

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

MEM:ALLOC-BYTES       ( CAD-NUM:alloc-byte-len -- ptr u8 CAD-NUM:alloc-byte-len )
MEM:RELEASE-BYTES     ( ptr u8 CAD-NUM:alloc-byte-len -- )
MEM:UNMAP             ( ptr u8 CAD-NUM:byte-len -- )
MEM:WITH-BYTES        ( R CAD-NUM:alloc-byte-len [ R ptr u8 CAD-NUM:alloc-byte-len -- S ] -- S )
```

`MEM:RELEASE-BYTES` consumes the exact extent returned by `MEM:ALLOC-BYTES`.
`MEM:UNMAP` releases a validated mapped byte range without fabricating an
allocation extent. Both use one private `munmap` sink; kernel refusal writes
`memory: unmap failed` to standard error and exits 71, bypassing `catch`.
`MEM:WITH-BYTES` releases after normal return or a body throw, restores its
outer frame after successful release, then rethrows the body code.

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
CONTENT-KEY:RESET       ( -- )
CONTENT-KEY:TEXT+       ( ptr u8 n -- )
CONTENT-KEY:DIGEST+     ( ptr u8 -- )
CONTENT-KEY:FILE+       ( ptr u8 n -- )
CONTENT-KEY:FINAL       ( ptr u8 -- )
CONTENT-KEY:FINAL-HEX   ( ptr u8 -- )
```

`CONTENT-KEY:FILE+` records the path in the manifest but hashes file content
through a metadata-validated per-file digest cache when one is configured.
`CONTENT-KEY:CACHE-PATH!` sets an explicit cache file, `CONTENT-KEY:CACHE-ROOT!`
uses `content-key.cache` under a root directory, and `CONTENT-KEY:CACHE-CLEAR!`
clears the explicit setting. The test suite installs this root in-process;
content-key does not read environment variables.

## Object Records

`lib/object.f` owns the `OBJ` package: a deterministic object-record codec for
the linkable Habu build path. `hb-build` can already consume a cache hit by
source digest plus target/checker/compiler ABI, link the object text, and write a
native executable. Source-to-object emission is still the remaining compiler
producer slice; current non-object builds continue to use the executable and
maker artifact caches.

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
OBJ:ENTRY+     ( ptr u8 n n ptr u8 n -- )
OBJ:IMPORT+    ( ptr u8 n ptr u8 n -- )
OBJ:TYPE+      ( ptr u8 n ptr u8 n -- )
OBJ:RELOC+     ( ptr u8 n n ptr u8 n -- )
OBJ:NORET+     ( ptr u8 n -- )
OBJ:BYTES$     ( -- ptr u8 n )
OBJ:SOURCE$    ( -- ptr u8 n )
OBJ:TARGET$    ( -- ptr u8 n )
OBJ:CHECKER$   ( -- ptr u8 n )
OBJ:COMPILER$  ( -- ptr u8 n )
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
symbol, text byte offset, and effect. `OBJ:ENTRY+` records a selected non-MAIN
entry (name, test mode, and forged seed cells as big-endian u64 hex) so a
preseeded object is a distinct artifact from a normal MAIN object. Header
accessors return the validated
source, target, checker, and compiler ABI fields. Row accessors expose the
validated record body without the magic header: indexes are zero-based, fields
exclude the tag, and bad row or field indexes throw `E-OBJ-FIELD`. `OBJ:LOAD`
validates and restores a serialized record; `OBJ:KEY-HEX` hashes the canonical
bytes through `lib/content-key.f`.

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
`<root>/<64-hex>.hbo`. `OBJSTORE:LOAD` reads by key, validates through
`OBJ:LOAD`, and recomputes the loaded object's key. Missing files throw
filesystem errors; malformed or wrong-key object bytes throw object schema
errors.

`lib/object-index.f` owns the `OBJIDX` package: a source+ABI index from a
deterministic source key to the content-addressed `OBJ` key. This is the lookup
layer that lets a later compiler integration ask whether a source object exists
before recompiling it.

```forth
OBJIDX:ROOT!          ( ptr u8 n -- )
OBJIDX:ROOT$          ( -- ptr u8 n )
OBJIDX:PATH$          ( ptr u8 n -- ptr u8 n )
OBJIDX:SOURCE-KEY-HEX ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 -- )
OBJIDX:EXISTS?        ( ptr u8 n -- bool )
OBJIDX:STORE          ( ptr u8 n ptr u8 n -- )
OBJIDX:LOAD           ( ptr u8 n -- ptr u8 n bool )
```

`OBJIDX:SOURCE-KEY-HEX` hashes the 64-byte source digest plus target, checker,
and compiler ABI strings. `OBJIDX:STORE` validates both source and object keys
and atomically writes `<root>/<source-key>.idx`; `OBJIDX:LOAD` returns the object
key and true for a hit, or an empty slice and false for a miss. Malformed keys or
index files throw `E-OBJ-FIELD`.

`lib/object-resolve.f` owns the `OBJRES` package: the checked resolver that
combines `OBJIDX` and `OBJSTORE` for source+ABI object-cache lookups.

```forth
OBJRES:ROOT! ( ptr u8 n -- )
OBJRES:ROOT$ ( -- ptr u8 n )
OBJRES:STORE ( -- ptr u8 n )
OBJRES:LOAD  ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- bool )
```

`OBJRES:ROOT!` sets the shared root for `.idx` and `.hbo` entries. `STORE`
stores the current validated object and indexes it by its own source, target,
checker, and compiler fields. `LOAD` takes source digest, target ABI, checker
ABI, and compiler ABI. It returns false only when no source index exists; an
index pointing at a missing, malformed, wrong-key, or wrong-ABI object fails
closed through filesystem errors or `E-OBJ-SCHEMA`.

`lib/object-link.f` owns the `OBJLINK` package: the checked symbol validation
pass for a future object linker. It copies export/import names out of the
current `OBJ` record, rejects duplicate exports, and checks imports after all
objects have been added.

```forth
OBJLINK:RESET        ( -- )
OBJLINK:PACKAGE-COUNT ( -- n )
OBJLINK:REQUIRE-COUNT ( -- n )
OBJLINK:TYPE-COUNT  ( -- n )
OBJLINK:NORET-COUNT ( -- n )
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
OBJLINK:PACKAGE$     ( n -- ptr u8 n )
OBJLINK:PACKAGE-VIS$ ( n -- ptr u8 n )
OBJLINK:REQUIRE$     ( n -- ptr u8 n )
OBJLINK:TYPE$        ( n -- ptr u8 n )
OBJLINK:TYPE-KIND$   ( n -- ptr u8 n )
OBJLINK:NORET$       ( n -- ptr u8 n )
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
OBJLINK:APPLY        ( -- )
```

`OBJLINK:ADD` consumes the currently loaded `OBJ` record. It copies package,
require, type, no-return, symbol, and effect strings into bounded storage so later `OBJ:LOAD`
calls cannot invalidate link metadata. It decodes validated `text`/`data` rows
into bounded merged section buffers, records per-object text/data base and size
rows before advancing the merged section totals, and exposes the merged bytes
through `OBJLINK:TEXT$` and `OBJLINK:DATA$`. `def` rows record merged text
addresses, reject duplicate definitions, and reject object-local definition
offsets outside that object's text section. `reloc` rows record merged patch
addresses and resolve their target addresses from `def` rows during
`OBJLINK:CHECK`; reading an unresolved relocation target throws `E-OBJ-SCHEMA`.
`OBJLINK:CHECK` throws `E-OBJ-SCHEMA` for unresolved imports, import/export
effect mismatches, or relocation targets; duplicate exports/defs throw during
`OBJLINK:ADD`, relocation or definition offsets outside the current object's
text section throw `E-OBJ-SCHEMA`, and table/storage overflow throws
`E-OBJ-CAPACITY`. `OBJLINK:APPLY` runs `OBJLINK:CHECK`, then applies supported
`abs64` relocations to the merged text buffer in little-endian form; unknown
relocation kinds or patch ranges past the text buffer throw `E-OBJ-SCHEMA`.

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
SOURCE-LINE-LEAD$              ( ptr u8 len -- ptr u8 n )
SOURCE-PACKAGE-OPEN-LINE?      ( ptr u8 len -- bool )
SOURCE-PACKAGE-CLOSE-LINE?     ( ptr u8 len -- bool )
SOURCE-LINE-PKG-TRACK          ( ptr u8 len -- )
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
line break, insertion happens at the beginning. `COMMENT-EXPORTS` rewrites TOP-LEVEL lines
whose first non-space byte sequence starts with `EXPORT ` by replacing leading
whitespace with `\ `, preserving all other bytes. Lines inside a
`package ... ;package`/`;package` block pass through untouched: there
`EXPORT NAME` is the package re-export declaration, not the hb-build --repl
directive (the line tracker counts line-leading openers/closers).

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
PROC-WAIT-STATUS-RAW ( pid -- n )
PROC-SPAWN-RAW      ( ptr u8 fd fd fd -- pid )
PROC-KILL-RAW       ( pid n -- rc )
PROC-ZCOPY          ( ptr u8 len ptr u8 len -- ptr u8 )
PROC-PATHZ          ( ptr u8 len -- ptr u8 )
PROC-WAIT-STATUS         ( pid -- n )
PROC-STATUS>OUTCOME ( n -- outcome )
PROC-OUTCOME>RC     ( outcome -- rc )
PROC-STATUS>RC      ( n -- rc )
PROC-WAIT-OUTCOME        ( pid -- outcome )
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
RUN-CAPTURE  ( ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
RUN-CAPTURE-OUTCOME  ( ptr u8 len ptr u8 len ptr u8 len ms -- len len outcome )
```

The `PROCESS-TRACE` package exposes the observation seam used by the native
gate. Its default hooks are no-ops, so normal process behavior is unchanged.
`EXECUTED` invokes the exec hook exactly once only for a nonnegative spawn
result; a failed spawn invokes the clear hook and emits no event. `FORKED`
invokes the fork hook exactly once only in the parent of a successful fork;
the child invokes the child hook, while a failed fork invokes the clear hook;
neither emits an event. A throwing successful exec/fork hook kills and reaps
the new child before propagating the exact hook throw, so observation failure
cannot orphan a process. `REAPER` classifies the next raw fork as reaper work,
and every fork outcome resets that role. The package initializes all hooks to
no-ops and the role to direct. Bind the public defer targets with direct literal
`is`:

```forth
PROCESS-TRACE:EXEC-HOOK   ( ptr u8 n -- )
PROCESS-TRACE:FORK-HOOK   ( ptr u8 n -- )
PROCESS-TRACE:CLEAR-HOOK  ( -- )
PROCESS-TRACE:CHILD-HOOK  ( -- )
PROCESS-TRACE:EXECUTED    ( ptr u8 pid -- pid )
PROCESS-TRACE:FORKED      ( pid -- pid )
PROCESS-TRACE:REAPER      ( -- )
```

`PROC-PATHZ` copies a counted path into the module's private NUL-terminated path
buffer and throws `E-PROC-OUTPUT` if the path does not fit. `PROC-RUN-RC` composes the
checked `PROC-SPAWN-IO` and `PROC-WAIT-RC` wrappers rather than the unchecked runtime
`run-rc` primitive. `PROC-RUN-IO-RC` is the same checked run-and-wait path with
explicit stdin, stdout, and stderr fds. `PROC-SPAWN-IO` and `PROC-WAIT-RC` throw
`E-PROC-SPAWN` and `E-PROC-WAIT` for primitive failures.

`PROC-WAIT-STATUS` returns the raw Darwin wait status for a pid and throws
`E-PROC-WAIT` on primitive failure. `PROC-WAIT-OUTCOME` decodes that status into
the `outcome` sum family: `exited` carrying the exit code, `signaled` carrying
the signal number, or `timeout` (capture deadline; always SIGKILL-reaped, no
payload), with generated constructors `OUTCOME:EXITED`, `OUTCOME:SIGNALED`, and
`OUTCOME:TIMEOUT`. Consumers dispatch through exhaustive `MATCH outcome`.
`PROC-OUTCOME>RC` flattens an outcome to the historical rc: the exit code for
normal exits, `128 + signal` for signal deaths and timeouts; `PROC-STATUS>RC`
and `PROC-WAIT-RC` use it. The whole `-OUTCOME` capture API returns the sum:
`PROC-CAPTURE-OUTCOME@` and the `RUN-*-OUTCOME` runners yield
`( -- len len outcome )` and every consumer dispatches by `MATCH outcome` (or
the `lib/test/outcome.f` assert helpers). The capture machine stores no pair
state: it keeps only the raw wait status plus a timed-out flag, and
`PROC-CAPTURE-OUTCOME ( -- outcome )` derives the sum on demand.

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

`PROC-SPAWN-RAW` is a raw primitive alias captured before the checked wrapper
names are defined. A failed raw spawn returns a negative target code (`-errno`
on macOS; the Linux exec-failure handshake still reports negative failure),
while checked wrappers convert any negative pid to `E-PROC-SPAWN`. Application
code should prefer `PROC-SPAWN-IO`, `PROC-WAIT-RC`, and `PROC-RUN-RC`. There is
deliberately no raw `wait-rc` wrapper: the primitive reports WEXITSTATUS only,
so a signal-killed child would read as rc 0 (a swallowed crash). Wait through
`PROC-WAIT-RC` / `PROC-WAIT-OUTCOME`, which decode signal deaths as 128+sig.

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

Capture spawns can carry a death reaper. `PROC-REAP-ARM ( pid -- pid )` is a
typed execution vector consulted by every `PROC-RUN-*` capture spawn (via
`PROC-CAPTURE-PID!`): the default vector arms nothing; `lib/process-fork.f`
installs the live vector, which arms a co-located `PROC-SPAWN-REAPER` in the
child's process group watching the fd published in `PROC-REAP-WATCH-FD`
(-1 = no context). A pool worker publishes its worker-alive read end there, so
a quiet capture child — its own group leader, invisible to the worker's
group-kill — dies with the worker instead of lingering. Every capture
terminator calls `PROC-REAP-DISARM`, which kills and waits the reaper by its
specific pid, so no reaper outlives its capture and `wait(-1)` callers never
see a stray child.

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
RUN-ARGV-CAPTURE      ( ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
RUN-ARGV-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ms -- len len outcome )
RUN-ARGV-STDIN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
RUN-ARGV-STDIN-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len outcome )
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
argv vector and then resets argv state. Both return a
`result<pcap:captured,pcap:failed>`: a clean exit is `ok(captured)` carrying the
stdout and stderr byte lengths; a nonzero completion is `err(failed)` carrying
the same two lengths PLUS the completion code (a nonzero exit code, or
128+signal) — the captured output is valid on both arms, and rc in that order is
retired in favor of the exhaustive `MATCH`. Captures are bounded by the caller capacities; if either
stream would exceed its capacity, the word throws `E-PROC-TRUNCATED` rather than
truncating silently. Exact-capacity output is accepted when the next read
observes EOF. On timeout, it sends `SIGKILL` through the checked
`PROC-KILL-RAW` boundary, waits for the child, closes owned fds, and then throws
`E-PROC-TIMEOUT`. Truncation and other capture failures also clean up all owned
fds and terminate/reap the active child before throwing a named process error.
The `*-OUTCOME` capture variants return stdout length, stderr length, and the
`outcome` sum. They classify timeout as the `timeout` outcome instead
of throwing `E-PROC-TIMEOUT`; output truncation and other harness failures still
throw named process errors.
`RUN-ARGV-STDIN-CAPTURE` additionally writes a bounded caller-provided stdin
buffer into the child while draining stdout and stderr. The stdin write fd is
nonblocking and no-SIGPIPE; partial writes advance by the actual byte count, and
an early child close of stdin closes the parent write fd while capture continues
to the child's final rc/outcome. Its outcome variant keeps the same stdin
behavior and returns the outcome sum.

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
PROC-ENV-SET              ( ptr u8 len ptr u8 len -- )
PROC-ENV-DEFAULT-RESET    ( -- )
PROC-ENV-DEFAULT+         ( ptr u8 len ptr u8 len -- )
PROC-ENV-DEFAULT$?        ( ptr u8 len -- ptr u8 len bool )
PROC-ENV-PREPARE          ( -- ptr a )
PROC-ENV-INHERIT-MISSING  ( -- )
PROC-SPAWN-ARGV-ENV-IO         ( ptr u8 len fd fd fd -- pid )
PROC-RUN-ARGV-ENV-IO-RC        ( ptr u8 len fd fd fd -- rc )
RUN-ARGV-ENV-CAPTURE      ( ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
RUN-ARGV-ENV-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ms -- len len outcome )
RUN-ARGV-ENV-STDIN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len outcome )
FIND-EXECUTABLE-IN-PATH   ( ptr u8 len ptr u8 len ptr u8 -- option<len> )
FIND-EXECUTABLE           ( ptr u8 len ptr u8 -- option<len> )
RESOLVE-EXECUTABLE        ( ptr u8 len ptr u8 -- len )
```

Call `PROC-ENV-RESET`, append exact `NAME=VALUE` entries with
`PROC-ENV-ENTRY+` or checked name/value pairs with `PROC-ENV+`, and then run one
of the env-aware wrappers. The child receives exactly the prepared env vector.
Call `PROC-ENV-INHERIT-MISSING` after explicit overrides to copy parent envp
entries whose names are not already present; explicit entries win and duplicate
names are skipped. `PROC-ENV-SET` replaces an existing row by name in place
(for example one already copied from the parent's own environment) and appends
only when the name is absent, so exactly one row for the name reaches the
child; `PROC-ENV+` always appends and never deduplicates. `FIND-EXECUTABLE-IN-PATH` accepts an explicit PATH byte string
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
PROC-CMD-RUN-RC      ( ptr u8 len ms -- result<n,n> )
PROC-CMD-OUT$        ( -- ptr u8 n )
PROC-CMD-ERR$        ( -- ptr u8 n )
PROC-CMD-OUTCOME@    ( -- n n )
PROC-CMD-RC@         ( -- result<n,n> )
```

Call `PROC-CMD-RESET`, append extra args with `PROC-CMD-ARG+`, append explicit
environment entries with `PROC-CMD-ENV+` or `PROC-CMD-ENV-ENTRY+`, optionally
replace the default inherited environment with `PROC-CMD-ENV-HERMETIC`, and set
bounded stdin with `PROC-CMD-IN!`. `PROC-CMD-RUN-OUTCOME` validates the path and
timeout before transferring state into the lower-level argv/env buffers, captures
bounded stdout/stderr into command-owned buffers, stores the decomposed outcome, and returns
that same outcome pair. `PROC-CMD-RUN-RC` wraps the `PROC-OUTCOME>RC` completion
in a `result<n,n>` (ok on a clean exit, err carrying the nonzero code) for
callers that branch on success/failure. `PROC-CMD-OUT$`, `PROC-CMD-ERR$`,
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
RUN-ARGV-ENV-CWD-CAPTURE   ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
RUN-ARGV-ENV-CWD-STDIN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
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
TASK:PREPARE       ( ptr a -- )
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

`lib/time.f` exposes checked public wrappers around the native clock primitives
through `package TIME`:

```forth
TIME:EPOCH-SECONDS  ( -- n )
TIME:MONO-NS        ( -- n )
```

`TIME:EPOCH-SECONDS` returns UTC Unix seconds from `epoch-seconds`.
`TIME:MONO-NS` returns monotonic nanoseconds from `mono-ns`; callers should only
compare ordering or elapsed time, never exact values.

`lib/date.f` exposes checked Gregorian UTC helpers through `package DATE`:

```forth
DATE:DIGIT?            ( n -- bool )
DATE:LEAP-YEAR?        ( n -- bool )
DATE:MONTH-DAYS        ( n n -- n )
DATE:VALID-YMD?        ( n n n -- bool )
DATE:YMD>DAYS          ( n n n -- n )
DATE:DAYS>YMD          ( n -- n n n )
DATE:N                 ( ptr u8 n n -- option<n> )
DATE:PARSE-YMD         ( ptr u8 n -- option<n> )
DATE:WIDTH!            ( n n ptr u8 n -- )
DATE:FORMAT-YMD        ( n ptr u8 n -- ptr u8 n )
DATE:FORMAT-EPOCH-UTC  ( n ptr u8 n -- ptr u8 n )
```

`DATE:N` returns `SOME` with the parsed field or `NONE` on a non-digit.
`DATE:PARSE-YMD` accepts exactly `YYYY-MM-DD` and returns `SOME` with the Unix epoch
day, or `NONE` on malformed input. `DATE:FORMAT-YMD` writes `YYYY-MM-DD`; `DATE:FORMAT-EPOCH-UTC` writes
`YYYY-MM-DDTHH:MM:SSZ`. Formatters use caller-provided buffers and throw
`E-TIME-CAPACITY` when the buffer is too small. `DATE:FORMAT-EPOCH-UTC` also throws
`E-TIME-RANGE` for negative epoch seconds. Load `lib/errors.f` before
`lib/date.f` when using formatter error codes.

## Argv

`lib/argv.f` provides checked command-line parsing for `hb script.f args...`
scripts and multi-source tools. The tool CLIs and the stdlib share this one
packaged module and call it through the qualified `ARGV:` API. The parser reads
`SCRIPT-ARGC` and `SCRIPT-ARGV$` by
default, or an
in-memory mock argv set for focused tests. `ARGV:PARSE` recognizes `--json`,
`--json-errors`, `--label NAME`, `--strict-signatures`, `--all-errors`,
`--strict-boundary`, `-o OUT`, and `--`; tokens after `--` are always
positionals, even when they begin with a dash. Unknown dash-prefixed options and
missing option values throw `ARGV:E-USAGE` after emitting the configured usage
text unless quiet mode is enabled.

```forth
ARGV:USAGE!             ( ptr u8 n -- )
ARGV:QUIET!             ( n -- )
ARGV:FAIL               ( ptr u8 n -- )
ARGV:RESET              ( -- )
ARGV:USE-SCRIPT         ( -- )
ARGV:MOCK-CLEAR         ( -- )
ARGV:MOCK+              ( ptr u8 n -- )
ARGV:COUNT              ( -- n )
ARGV:TOK$               ( n -- ptr u8 n )
ARGV:TOK=               ( n ptr u8 n -- bool )
ARGV:PARSE              ( -- )
ARGV:EXPECT-POS         ( n n -- )
ARGV:EXPECT-POS-EXACT   ( n -- )
ARGV:POS#               ( -- n )
ARGV:POS$               ( n -- ptr u8 n )
ARGV:POSZ               ( n -- ptr u8 )
ARGV:JSON?              ( -- bool )
ARGV:STRICT-SIGNATURES? ( -- bool )
ARGV:ALL-ERRORS?        ( -- bool )
ARGV:STRICT-BOUNDARY?   ( -- bool )
ARGV:LABEL-DEFAULT!     ( ptr u8 n -- )
ARGV:LABEL!             ( ptr u8 n -- )
ARGV:LABEL?             ( -- bool )
ARGV:LABEL$             ( -- ptr u8 n )
ARGV:OUT-DEFAULT!       ( ptr u8 n -- )
ARGV:OUT!               ( ptr u8 n -- )
ARGV:OUT?               ( -- bool )
ARGV:OUT$               ( -- ptr u8 n )
ARGV:OUTZ               ( -- ptr u8 )
ARGV:REQUIRE-OUT        ( -- )
ARGV:REQUIRE-LABEL      ( -- )
ARGV:PATHZ              ( ptr u8 n -- ptr u8 )
ARGV:ZCOPY              ( ptr u8 n ptr u8 n -- ptr u8 )
```

Drivers set usage/defaults, call `ARGV:PARSE`, validate positional arity with
`ARGV:EXPECT-POS` or `ARGV:EXPECT-POS-EXACT`, then read counted outputs through
`ARGV:POS$`, `ARGV:LABEL$`, `ARGV:OUT$`, and the flag predicates.
Path-oriented syscall wrappers may use `ARGV:POSZ`, `ARGV:OUTZ`, or
`ARGV:PATHZ`; these copy into the module-owned path buffer and throw
`ARGV:E-INTERNAL` on capacity failure.

Mocks keep parser tests self-hosted: `ARGV:MOCK-CLEAR` enables mock mode and
empties the mock list, `ARGV:MOCK+` appends one counted token, and
`ARGV:USE-SCRIPT` restores real script argv. `ARGV:QUIET!` suppresses usage
writes while still throwing exact error codes, so tests can assert
`ARGV:E-USAGE` deterministically.

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
PROP:SEED!      ( n -- )
PROP:SEED@      ( -- n )
PROP:COUNT@     ( -- n )
PROP:DEFAULTS   ( -- n n )
PROP:RUN-RESET  ( n n -- )
PROP:RND        ( -- n )
PROP:RND%       ( n -- n )
PROP:BUF-RESET  ( -- )
PROP:BUF+       ( ptr u8 n -- )
PROP:BUF-C+     ( n -- )
PROP:DIGIT+     ( n -- )
PROP:BUF$       ( -- ptr u8 n )
PROP:GEN-START  ( n -- )
PROP:GEN-STEP   ( ptr u8 n n n -- )
PROP:DROP-LAST  ( -- bool )
PROP:SHRINK     ( [ -- bool ] -- )
BUILD:CHECK          ( ptr u8 n -- )
BUILD:ARTIFACT       ( ptr u8 n ptr u8 n -- ptr u8 n )
BUILD:STEP           ( ptr u8 n [ -- n ] -- )
BUILD:RUN            ( ptr u8 n ptr u8 n -- n )
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

`lib/test/budget.f` keeps child timeouts and performance ratchets separate.
`T-BUDGET-MS ( n -- n )` scales a nominal timeout by `HB_LOAD_PCT`, which
includes measured host load plus the gate's structural pool-pressure floor, so
a healthy-but-slow child does not read as hung. `TEST-BUDGET:PERF-MS ( n -- n )`
scales a performance ratchet by measured `HB_CAL_PCT` only, so an idle full gate
still enforces its nominal phase limits. Both factors are clamped to 100..300
percent; standalone runs without an exported factor self-calibrate. Declare
budgets as named nominal constants behind the appropriate scaling word.

`TEST:*` defines reusable suite/group/test orchestration. Project adapters bind
the public defer targets `TEST:SETUP`, `TEST:TEARDOWN`, `TEST:DRAIN`,
`TEST:ARGS-BEGIN`, `TEST:ARG+`, `TEST:SELECT?`, `TEST:RUNNER`, and
`TEST:STDIN-RUNNER` with direct literal `is`; test files declare named groups with
`TEST:GROUP SEQ name` or `TEST:GROUP PARA name` (the `SEQ`/`PARA` mode is a
mandatory positional token before the group name), define `TEST:SUITE` or
`TEST:SUITE-STDIN` entries, close each entry with `TEST:;SUITE`, close the group
with `TEST:;GROUP`, and execute once with `TEST:RUN`. Fixture helper words should
live in a private package, not global stemmed names.
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

`lib/test/subject.f` publishes `SUBJECT:RUN`, which evaluates one counted source
inside a disposable fork child, captures bounded stdout and stderr, enforces a
deadline, and returns a typed process outcome. Copy-on-write isolation prevents
dictionary and engine-state mutations from escaping the child. Its dynamic
`evaluate` and raw child stack/handler initialization remain audited boundaries
owned by `habu-type-isolated-dynamic-244c0e2c`; that capability dot replaces
both with a digest-bound typed source artifact and explicit isolated execution
context.

`lib/fs-root.f` publishes `FS:WRITABLE-ROOT? ( ptr u8 n -- bool )`. It returns
true only for an existing directory to which the process has both write and
search access. Write-only directories are unusable as cache roots.

`lib/build-cache.f` owns the one canonical persistent build-cache root. Resolve
it with this package surface:

```forth
BUILD-CACHE:RESET    ( -- )
BUILD-CACHE:ROOT!    ( ptr u8 n -- )
BUILD-CACHE:ROOT$    ( -- ptr u8 n )
BUILD-CACHE:SOURCE   ( -- BUILD-CACHE:source )
BUILD-CACHE:RESOLVE  ( -- ptr u8 n BUILD-CACHE:source )
BUILD-CACHE:SOURCE$  ( BUILD-CACHE:source -- ptr u8 n )
BUILD-CACHE:SELECTED?       ( -- bool )
BUILD-CACHE:SELECTED-ROOT$  ( -- ptr u8 n )
BUILD-CACHE:SELECTED-SOURCE ( -- BUILD-CACHE:source )
BUILD-CACHE:CAUSE           ( -- n )
BUILD-CACHE:CAUSE$          ( -- ptr u8 n )
```

Environment resolution uses exactly the first non-empty tier:
`HABU_BUILD_CACHE`, `XDG_CACHE_HOME/habu-build`,
`HOME/.cache/habu-build`, then `TMPDIR/habu-build`. An empty variable does not
select its tier. When all four variables are empty, resolution throws
`E-BUILD-PATH`. The selected directory is created recursively; an existing
non-directory, an unwritable directory, or a creation failure also throws
`E-BUILD-PATH` without consulting a lower tier. `BUILD-CACHE:ROOT!` is the
explicit programmatic override used by build clients and isolated fixtures; its
typed source is `explicit`. A failed selection retains its selected source,
complete attempted root, and underlying filesystem cause even when the root is
too long for a filesystem operation. The `SELECTED-*` and `CAUSE*` accessors
read that separately owned evidence without attempting resolution again;
`source` also has the diagnostic-only `none` variant for the no-tier case.

`tools/hb-build.f --report-json ...` emits one `hb-build-report` JSON object on
success. Version 1 contains `cache_root`, `cache_source`, `artifact_hit`,
`object_hit`, `maker_hit`, `maker_built`, `maker_ran`, and `elapsed_ns`.
`HB-BUILD:CACHE-ROOT$`, `CACHE-SOURCE`, `ARTIFACT-HIT?`, `OBJECT-HIT?`,
`MAKER-HIT?`, `MAKER-BUILT?`, `MAKER-RAN?`, and `ELAPSED-NS` expose the same
captured typed state to checked in-process clients; `HB-BUILD:REPORT$` renders
that state. Build clients consume this surface instead of timing or inspecting
child-private trace cells. `HB-BUILD:RESET` invalidates the report at build
start, `HB-BUILD:VALID?` reports whether a build completed, and every report
accessor throws `E-BUILD-STATUS` while invalid so a failed build cannot expose a
prior success.

A cache-root preparation failure exits with the build failure status and emits
one structured explanation naming `E-BUILD-PATH`, the selected source and root,
and the retained underlying cause. `--json-errors` emits the versioned
`hb-build-error` JSON form; text mode emits the same labelled fields.
Both renderers grow to fit the retained root, so diagnostic formatting cannot
replace the owning `E-BUILD-PATH` failure with a string-capacity error.
Text mode JSON-quotes the root, escaping control characters so one failure is
always exactly one labelled output line.

`lib/build.f` lives in `package BUILD` and owns checked source certification,
artifact path construction, and fail-closed status reporting. `BUILD:CHECK`
requires a counted source path that names a file, scans
colon definitions in bounded module storage, and certifies each definition with
`CHECK!`; missing, malformed, or uncheckable source throws `E-BUILD-SOURCE`.
`BUILD:ARTIFACT` joins a build root and artifact name into the module-owned
bounded path buffer, throwing `E-BUILD-PATH` for empty or too-long components.
`BUILD:STEP` runs a checked quotation returning an rc and throws
`E-BUILD-STATUS` on nonzero status. `BUILD:RUN` runs a counted command path,
throws `E-BUILD-COMMAND` if the command is not a file, throws `E-BUILD-STATUS`
on nonzero rc, and throws `E-BUILD-PATH` if the expected artifact file is absent
after a successful command. The artifact-existence check, the source scanner,
the `CHECK!` trust boundary, and every buffer and state cell are package-private.
Raw process exits are only allowed at the final CLI/script boundary.

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
