# habu stack-effect syntax

A checked definition declares its effect in the `( … )` immediately after the
name: `: SQUARE ( i64 -- i64 ) DUP * ;`. The checker infers the body's effect and
unifies it with the declaration; a mismatch refuses the definition. Source
comments and audited `TRUST` rows are parsed by the checker boundary adapter in
`src/core/checker.f`; checker-owned primitive, literal, memory, and control-flow
effects are built structurally, not reparsed from strings.

## Grammar

```
sig    = stack '--' stack ( '|' stack '--' stack )?
stack  = rowvar? type*
type   = conname | role | declared-role | tyvar | 'ptr' type | '[' stack '--' stack ']'
conname= i64 u8 u32 cell bool char str addr
role   = idx len count off fd rc pid ms ns tok reg label va symidx asm img snap
declared-role = a multi-character token declared by `DEFTYPE` or `DEFLINEAR`
tyvar  = a..z          (same letter → same type var, per signature)
rowvar = A..Z          (same letter → same row var; leading = the stack tail)
```

- The part before `|` is the **data** stack; the optional part after `|` is the
  **return** stack. Four rows total: `( Din Rin -- Dout Rout )`.
- A **row var** (`R`, `S`, …) at the front of a stack stands for "the rest of the
  stack below" — row polymorphism. Stacks with no leading row var share one
  implicit data row (and one implicit return row).
- The implicit row in a checked definition is sealed for the body: callees may
  preserve it, but may not bind it by consuming below the declared inputs. This
  rejects hidden underflow such as a trusted `img -- img` boundary called from a
  word declared `( -- )`.
- A **type var** (`a`, `b`, …) is a fresh polymorphic type; reusing the same
  letter in one signature means the same type.
- Whitespace-delimited. Don't nest `( )` (the inner `)` closes the comment).

## Types

| type | meaning |
| ---- | ------- |
| `i64 u8 u32 cell` | integers of given width (`cell` = machine word) |
| `bool` | a flag (distinct from `i64` — comparisons return `bool`) |
| `char str addr` | character, string body (`c-addr u` as one value), raw address |
| `idx len count off fd rc pid ms ns tok reg label va symidx asm img snap` | nominal roles; distinct from each other and from plain `n` |
| `ptr<τ>` written `ptr τ` | typed pointer; `@`/`!` move `τ` |
| `[ S -- S' ]` | a quotation / `xt` carrying its own effect |

Nominal roles are for same-representation values whose meanings must not mix:
array indexes vs lengths, file descriptors vs return codes, elapsed milliseconds
vs nanoseconds, token indexes vs counts, registers vs labels, virtual addresses
vs symbol indexes, and image-build phases such as assembled code, executable
image, and snapshot header state. They are fail-closed concrete types: `idx`
does not unify with `len`, and neither unifies with a plain `n`. Introduce or
remove a role only through an explicit checked constructor/coercion word or an
audited boundary effect; do not rely on generic integer operations to launder a
role. Unchecked native emitters should still expose these roles in their `TRUST`
effects, so checked callers reject register/fd/label swaps and out-of-order
build phases before raw codegen.

`DEFTYPE name` declares a new nominal cell type for later signatures. The name is
global, explicit, and fail-closed: it cannot reuse a built-in type, parametric
constructor, atom prefix, or one-letter type variable. Unknown type tokens still
reject with `E-UNKNOWN-SIGNATURE-TYPE`; Habu does not silently intern typos.

A user-declared nominal gets the **same** strict treatment as the built-in roles:
it is distinct from `n` and from every other nominal, and never widens either
direction. `DEFTYPE` auto-derives its explicit converter pair — `>NAME
( n -- NAME )` and `NAME>N ( NAME -- n )` — as no-op identity casts, exactly like
`>IDX`/`IDX>N`. Those converters are the only way across the boundary; there is no
implicit collapse to `n`. So `deftype frame-idx` immediately yields checked
`>FRAME-IDX`/`FRAME-IDX>N`, and a mismatch renders the declared name (e.g.
`expected: n actual: frame-idx`), not `?`. This gives application code (camera
serials, frame indexes, exposure-µs, GMSL channels) compile-checked distinct
integers at zero runtime cost, without an engine edit or fixpoint rebuild.

`DEFLINEAR name` declares a nominal noncopyable cell type — a **linear-once**
resource that must be used (consumed or passed on) exactly once. Use it for owner
or lifetime tokens around arena-backed records, and for acquire/release framing
(evaluate/include frames, mmap slots, snapshot phases).

The checker enforces this by **conservation**: at every step whose declared
effect does *not* itself name a linear type, the number of live linear values on
the combined data+return stack may not change. So a generic word that would
duplicate a linear (`dup`, `over`, `tuck`, `2dup`, a `PICK`/`ROLL` copy), drop it
(`drop`, `nip`), or move it through untyped memory (`@`/`!`/`c@`/`c!`) or a
value-record copy is rejected, because the linear count would rise or fall. Only a
word (or quotation) whose declared effect *explicitly* names the linear type is an
audited producer/consumer and may change the count. Conservation holds across
control flow (both `IF` arms and stack-neutral loop bodies must agree) and across
**quotation application** — `[: dup ;] execute` and `[: drop ;] execute` are
rejected on a linear exactly as the bare `dup`/`drop` are, including nested
`execute`. Passing a linear through unchanged (`( own -- own )`, a `swap`/`rot`
reorder, `>r`/`r>`) is fine — that is a move, not a copy.

Acquire/release pairing is therefore proven, not conventional: a word that
acquires a linear frame and forgets to release it fails the definition's output
balance (the frame leaks onto the declared-empty stack), and releasing twice
underflows. A resource can neither be silently dropped nor duplicated.

Conservation alone only sees linear *cons* on the stack, so it is blind to
*polymorphic laundering* — a value duplicated or dropped while its type is still
a polymorphic variable that only later unifies with a linear con. The required
**linear kind discipline** is not implemented yet; it is tracked by
`habu-infer-linear-kinds-1f77b4c4` and must close the gap on two fronts:

- **Polarity-aware multiplicity at effect application.** When applying a word or
  primitive, any type variable in its effect that binds to a linear con must
  occur equally on the input and output sides across the *whole* effect,
  **including quotation sub-effects** (a quotation argument's rows flip polarity:
  the word supplies the quotation's inputs and receives its outputs). A move is
  1-in / 1-out (`swap`, `DIP`, `>r`/`r>`, a passthrough `( own -- own )`); a copy
  or drop is not. So `[: FREE ;] KEEP` rejects — `KEEP ( R a [R a -- S] -- S a )`
  feeds `a` to the consumer quotation *and* returns it (1-in / 2-out) — and so do
  `BI`/`TRI`, which fan `a` into several quotations.
- **Deferred taint within one body.** A variable copied or dropped while still
  polymorphic is tainted; if it later unifies with a linear con, the linear was
  laundered and the definition is rejected. This catches `[: dup FREE ;] execute`
  and `[: over FREE ;] execute`, where the copy happens before `FREE` binds the
  variable to the linear.

The implementation must make both checks additive to concrete-count
conservation and inert unless a `DEFLINEAR` type is in scope, so non-linear
polymorphic code (`[: dup ;] execute` on a plain value, `KEEP`/`DIP` over
non-linear data) remains unaffected. Until that dot lands, `KEEP`/`BI`/`TRI`
and self-duplicating quotations are not sound linear capability boundaries.

`VALUE-RECORD name field type ... END-VALUE-RECORD` declares a legacy by-value
record token for signatures. The token expands to TOUCHABLE `field<rec,name,t>`
cells: ordinary one-cell primitives destructure them (`drop`/`nip`/`over`),
and a field coerces to its inner type at an output boundary, so
`( n n -- point )` and `( point -- n n )` can be certified with empty runtime
bodies, while `( point -- rect )` rejects even if both records have the same
cell shape (field identity is record+name). Record fields may be polymorphic
or parametric signature types; accessors, updaters, copies, and destructors
are normal checked words over the expanded touchable cells. This is the
compatibility layer — contrast `PRODUCT` (docs/type-families.md §9.4), whose
fields are checker-owned HIDDEN cells that only the generated
`PKG:MAKE`/`PKG:UNMAKE` words may assemble or split; new by-value records
should use `PRODUCT`.

## Pointer types and arithmetic

`ptr τ` is a typed pointer whose pointee `τ` records the element the pointer
addresses. Pointer arithmetic is **pointee-polymorphic**: `+`, `-`, `1+`, `1-`,
`cell+`, and `char+` step a `ptr τ` by an integer offset and preserve `τ`
(`( ptr a n -- ptr a )`); `n + ptr a` is also `ptr a`; and `ptr a - ptr a` is the
integer distance `n`. Adding two pointers (`ptr a + ptr a`) and scaling a pointer
(`ptr a * n`) are rejected — neither is a modeled axiom.

A **byte view** re-reads any pointer as a byte span: a plain checked identity word
`: NAME ( ptr a -- ptr u8 ) ;` certifies, because the declared input pointee stays
a generalized variable while the output is `ptr u8`. This is the checked
replacement for hand-`TRUST`ed `*-BYTE+` reinterpret helpers; a `ptr u8` result
uses `c@`/`c!`, and cell `@`/`!` on it is rejected.

A pointer's pointee element type is **invariant**. The structural integer widening
that lets a `u8` value flow into a `cell`/`u32` slot at the *top level* of a stack
cell does **not** apply inside a `ptr`: a concrete `ptr u8` never satisfies
`ptr cell` or `ptr u32`, and two different concrete pointees never unify
(`ptr u8` vs `ptr cell` is a type error, in either argument order, and at any
nesting depth). This keeps the byte-span / cell-span distinction sound — a byte
pointer cannot be laundered into a cell pointer and then cell-loaded. The checker
enforces this by unifying pointer pointees strictly (equality plus type-variable
binding, no widening) while still widening top-level scalar cells.

## Examples (from `src/prims.fs`)

```
DUP    ( R a -- R a a )            \ row-polymorphic: any one value, duplicated
SWAP   ( R a b -- R b a )
+      ( R i64 i64 -- R i64 )
<      ( R i64 i64 -- R bool )     \ comparisons yield bool, not i64
@      ( R ptr a -- R a )
DEPTH  ( R -- R n )
WITHIN ( R i64 i64 i64 -- R bool )
>R     ( R a -- R | S -- S a )     \ moves a value data→return stack
R>     ( R -- R a | S a -- S )
EXECUTE( R [ R -- S ] -- S )       \ run a quotation
DIP    ( R a [ R -- S ] -- S a )   \ run a quotation under the top item
KEEP   ( R a [ R a -- S ] -- S a ) \ run with a copy, keep original
BI     ( R a [ R a -- R b ] [ R b a -- R b c ] -- R b c )
TRI    ( R a [ R a -- R b ] [ R b a -- R b c ] [ R b c a -- R b c d ] -- R b c d )
TIMES  ( R i64 [ R -- R ] -- R )   \ counted iterate (trusted runtime boundary)
EACH   ( R ptr a i64 [ R a -- R ] -- R )
MAP    ( R ptr a i64 [ R a -- R a ] -- R )
FOLD   ( R ptr a i64 b [ R b a -- R b ] -- R b )
?DUP-IF( R a [ R a -- R ] -- R )   \ typeable fusion of `?DUP IF … THEN`
```

`?DUP-IF` is the checkable form of the idiom `?DUP IF … THEN`: it consumes the
value and a quotation over it, and the run (nonzero) and skip (zero) paths both
converge to `R`. The naked `?DUP` stays untypeable (its arity depends on the
runtime value). A quotation that leaves an extra item is rejected by the
occurs-check (the output row would have to contain itself).

User-level: `: ABSV ( i64 -- i64 ) DUP 0< IF NEGATE THEN ;` — the surface form
omits the leading `R` and the return clause; the checker supplies fresh rows.

## Control flow (modeled by the checker, `src/control.fs`)

`IF/ELSE/THEN`, `BEGIN/UNTIL/AGAIN/WHILE/REPEAT`, `DO/?DO/LOOP/+LOOP`, `I`/`J`
(valid at loop depth ≥1 / ≥2), `EXIT` (asserts current = declared output),
`RECURSE` (fresh instantiation of the word's own effect). Both `IF` arms must have
equal effect; loop bodies must be stack-neutral.

## Exceptional control flow

The public signature grammar describes a word's normal data and return stack
effect. The checker also tracks a separate internal exceptional edge for
catchable `throw` paths:

- `die` is process no-return. It consumes its message/code and kills the current
  normal path; wrappers whose every path reaches `die` are recorded as
  non-returning.
- `throw` is catchable no-normal-return. It consumes the throw code, records an
  exceptional edge for the current scope, and kills only the current normal path.
  It must not be recorded as process no-return.
- A word can have both a normal effect and one or more `throw` paths. Calls use
  the normal effect for the continuing path while preserving the exceptional
  edge for an enclosing `catch`.
- A word whose every path reaches `throw` is a catchable throwing word. Calls to
  it kill the caller's current normal path, but the edge remains catchable by an
  enclosing `catch`.
- `catch` consumes an execution token / quotation. Its normal path requires the
  quotation to be stack-preserving on both data and return stacks, then pushes a
  throw code (`0` for normal completion). Its exceptional path restores the stack
  shape that existed before invoking the quotation, then pushes the thrown code.

This model is the reason a checked guard may be written directly:

```
: REQUIRE-NONEMPTY ( len -- ) dup 0 <= if E-A-EMPTY throw then drop ;
: HEAD ( ptr i64 len -- i64 ) REQUIRE-NONEMPTY @ ;
```

No dummy value should be pushed after `throw` merely to satisfy a branch join.
If a branch only throws, it contributes no normal output to the join.
Tokens after a terminated path (`throw`, `die`, `exit`, `leave`, or
unconditional loop back-edges) are rejected unless they are structural closers
that merge the dead path, such as `else`, `then`, `loop`, `+loop`, `repeat`,
`again`, or `;]`.

Checked definitions with explicit higher-order effects record those effects for
later callers; use `TRUST` only when the body itself cannot be checked.

## Escape hatches

- `TRUSTED: NAME ( eff ) body ;` — record `eff` for `NAME` **without** checking
  the body, then compile it normally. For metaprogramming words (`evaluate`,
  parsing, dictionary ops, raw memory) the checker can't follow.
- To chart an **already-defined** word's effect (so the checker can use it as a
  leaf) without redefining it in native habu: `s" name" s" eff" TRUST`.
- Nominal scalar roles (`idx`, `len`, `count`, `off`, `fd`, `rc`, `pid`, `ms`,
  `ns`, `tok`, `reg`, `label`, `va`, `symidx`) are entered and left through
  audited no-op conversions:
  `>IDX IDX>N`, `>LEN LEN>N`, `>COUNT COUNT>N`, `>OFF OFF>N`, `>FD FD>N`,
  `>RC RC>N`, `>PID PID>N`, `>MS MS>N`, `>NS NS>N`, `>TOK TOK>N`,
  `>REG REG>N`, `>LABEL LABEL>N`, `>VA VA>N`, and `>SYMIDX SYMIDX>N`.
  These are runtime identity casts over one cell; their only purpose is making
  semantic role changes explicit to the checker. A `DEFTYPE`-declared nominal gets
  the same pair auto-derived (`>NAME NAME>N`) at declaration time.
- Library boundaries should prefer checked refinement constructors over raw
  role casts. Examples: `A-LEN`, `A-IDX`, `A-COUNT`, `VEC-LEN`, `VEC-IDX`,
  `VEC-COUNT`, `STR-LEN`, `STR-OFF`, `STR-COUNT`, `JW-LEN`, `M-LEN`, and
  `M-OFF`. These constructors reject negative, overflowing, or out-of-capacity
  inputs before the nominal role reaches checked code.
- Phase roles (`asm`, `img`, `snap`) are nominal ordering cells whose payload is
  ignored. `ASM-CODE` produces `asm`; `BUILD-IMAGE` consumes it and produces
  `img`; `CODESIG2` preserves `img`; `DRV-WRITE-IMAGE` consumes `img` when the
  executable bytes are written. `BUILD-SNAP-HDR` produces `snap`, and
  `SNAP-WRITE` consumes the current `snap` token. These roles are not public
  numeric casts.
- Trusted defining words use `TRUSTED: NAME ( definer-eff ) create ... does>
  ( created-eff ) body ;`. `definer-eff` is the effect of invoking the defining
  word itself; the `created-eff` immediately after `does>` is recorded for each
  word produced by runtime `create` while that definer runs. If a trusted definer
  contains `does>`, the created effect must appear immediately after `does>`.
- For `CREATE...DOES>`, if `created-eff` is `( in -- out )`, the native checker
  verifies the `DOES>` body as `( in ptr a -- out )`: the created word pushes its
  data-field pointer before entering the `DOES>` body. Use typed pointer steps
  such as `cell+`, not raw integer `+`, when moving through that data field.
- **Raw storage definers publish RAW cells.** `here`, `create`, `variable`, and
  `constant` hand back a generic polymorphic value/pointer, which would otherwise
  let raw dictionary storage *mint any nominal family*: a `variable V` fetched by
  `V @` is a fresh var that could bind an arity-0 `CAD-KIND` id (target,
  toolchain, region, node) in value position, forging a validated identity. To
  stop this the checker gives every type var a **kind** — `TVK-ANY` (ordinary) or
  `TVK-RAW` — and marks the cells these definers publish `TVK-RAW`. A `TVK-RAW`
  var admits a plain scalar (and a plain pointer, checked recursively), and a
  fetch/store meets the kind through unification (with rollback + snapshot
  persistence), but it **rejects a nominal-family or layout value** — so
  `: N>ID ( n -- CAD-KIND:region ) V ! V @ ;` no longer certifies, while a
  numeric `variable`/`constant`/`here` round-trip still does. This is the
  value-position mirror of the pointee-side `ptr family` seal. Nominal *role*
  atoms (`idx`/`len`/`label`/… and `DEFTYPE` names) and execution tokens stay
  admitted in raw storage for now, because the engine's own codegen keeps labels
  and xts in raw scratch cells; fencing those out as well needs that role/xt
  scratch migrated to typed cells first (tracked follow-on). The `here` seal is a
  baked primitive effect; `create`/`variable`/`constant` are sealed through the
  verify-source definer registration (`RAW-TRUST-NEXT`).
- **`xt<effect>` storage cells are the typed alternative to raw xt scratch.**
  `TYPED-VARIABLE HK [ in -- out ]` (and `n TYPED-BUFFER HK [ in -- out ]`)
  declares a persistent monomorphic *code cell*: the generated accessor's declared
  `( -- ptr [ in -- out ] )` bakes the effect `E` as a concrete `T-QUOT` into its
  usig scheme, so every occurrence of `HK @` recovers `xt<E>` (freshened
  alpha-equivalently, never the erased raw address var of a `variable`). A typed
  store `[: W ;] HK !` fit-checks `W`'s certified effect against `E` through the
  ordinary `ptr` unification and **rejects** on mismatch; `HK @ execute`
  fit-checks the row against `E` exactly like executing a literal `xt<E>` (reuses
  `RSEXEC`). Admissibility is gated by `CHECKER-STORAGE-INFO` (a closed quotation
  cell, width 1; a malformed quotation body rejects). The **tick** store
  `['] W HK !` also works (dot `habu-typed-xt-cells-08e1dc2c`): `BTICK-TOK`'s
  lookahead treats a typed-xt-cell accessor as an xt sink, so the tick retypes to
  `xt<effect(W)>` and the same `ptr` unification fit-checks `E` — a matching store
  certifies, a mismatch rejects on the effect (not the old plain-`n` erasure), and
  a plain number still rejects. This is candidate-path only, like every direct-tick
  retype (the reconstructed `--load` body drops the tick target). A **buffer slot**
  tick store `['] W idx HKB !` stays out of scope — the index token splits the tick
  from the accessor, past the single-token lookahead — so use the quotation store
  `[: W ;] idx HKB !` for buffers. This is the sound alternative to the raw xt
  scratch above: `variable V  ' W V !  : F V @ execute ;` now REJECTS at check
  time (see the opaque-execute rule below), whereas the typed cell carries `E`
  end to end and executes with a statically known effect.
- **Executing an xt of unknown provenance from memory is a checked-code error
  (`E-EXEC-OPAQUE-XT`).** `execute` recovers the effect of the popped execution
  token from its checker type. A quotation value carries its effect (`[: … ;]`, a
  `[ in -- out ]` quotation parameter, `['] W` which retypes to `xt<effect(W)>`,
  or an `xt<effect>` storage cell whose `@` recovers the quotation), so
  `execute`/`catch` fit-check the row and stay checked. But an xt fetched from an
  untyped cell — a raw `variable`/`create` fetch — is only a bare type variable:
  its real stack effect is erased at the store, so executing OR catching it would
  launder whatever the stored xt actually does (mint a type, write a protected
  registry, …) past the checker — `catch` is exactly as unsound as `execute`,
  since it runs the same opaque body. The `RSEXEC` `T-VAR` branch rejects the
  execute and the `RSCATCH` `T-VAR` branch rejects the catch
  (`src/core/checker.f`), both with `E-EXEC-OPAQUE-XT` (repair class
  `fix_opaque_execute`); the reason names the `execute` or `catch` token it fired
  on and points at the three typed routes: a quotation parameter, a `defer` bound
  with `is`, or a typed `xt<effect>` cell (dots
  `habu-checker-exec-of-5923c543`, `habu-flip-rscatch-opaque-5da02bd5`). Two
  standing boundaries remain, both named `TRUSTED:` words: the metabuild
  primitive-body emitter (`FP-EMIT` in `src/habu/habu1.f`), a data-driven xt
  running raw machine-code emission that is not typed Habu; and the task
  scheduler's run of a per-task `( -- )` body under catch (`TASK-RUN-USER` in
  `lib/task.f`), stored in a task control-block field so its effect is unknown at
  the catch site.
- **Declared polymorphic effects stay parametric.** A quantifier in a declared
  effect (`a`, `ptr a`, …) is a promise that the word works for *every* type at
  that position. The body may not quietly break that promise. After a definition's
  body checks against its `( in -- out )`, the checker (`NP-CHECK` in
  `src/core/checker.f`) re-inspects every declared quantifier and rejects two
  laundering shapes with `E-NONPARAMETRIC-EFFECT` (repair class
  `fix_parametric_effect`):
  1. **Specialization to a sealed family.** If a body forces a declared quantifier
     to resolve to a sealed identity-bearing family — an arity-0 nominal scalar
     (`CAD-KIND` target/toolchain/region/node) or a layout family — the checker
     rejects and names the forged family, so `: F ( a -- a ) EFF-ID ;` (with
     `EFF-ID ( region -- region )`) no longer launders `region` behind a generic
     `a`. Plain-scalar widening (`a := n`/`u8`) stays legal, so the pervasive
     `( ptr a -- n ) @` fetch corpus is untouched. The pointer instance
     (`ptr family` erased to `ptr a`) is the same violation seen in pointee
     position, already caught by the `NOMPTR-BLOCK` mismatch (which names the
     family) before this pass runs.
  2. **Quantifier aliasing.** If a body unifies two *distinct* declared
     quantifiers into one variable, injectivity of the quantifier map is broken;
     the checker rejects and names both letters, so `: F ( a b -- a ) MERGE ;`
     (with `MERGE ( g g -- g )`) no longer certifies under a two-variable face.
  The pass runs only when the body otherwise certified, makes no new bindings
  (the body's speculative binds already roll back on the trail, and the next
  definition's reset clears every specialization record), so a rejected signature
  never persists and multi-error checking continues cleanly.
- **Literal-argument `PICK`/`ROLL` are folded** to a concrete shuffle at check
  time: `0 PICK`≡`DUP`, `1 PICK`≡`OVER`, `2 PICK ( a b c -- a b c a )`;
  `1 ROLL`≡`SWAP`, `2 ROLL`≡`ROT`. A **dynamic** (runtime-computed) index can't be
  folded and stays untypeable; keep it outside checked code or behind a named,
  tested `TRUSTED:` boundary. See `src/pickroll.fs`.
- Words the checker can't type (variadic `?DUP`, dynamic `PICK`/`ROLL`)
  must stay outside checked code or behind `TRUSTED:`.

## Primitive axiom set

The checker's typing rests on one explicit, minimal trust root: the **primitive
effect table** (`PES`) in `src/core/checker.f`. Each `PRIM: name … PRIM;` row
declares one primitive's stack effect as an axiom the checker takes on faith —
there is no Forth body to infer from. These rows are the only place a built-in
word's effect is asserted; everything the checked language proves is derived by
inference from them. Overloaded primitives (`+`, `-`, `cell+`, `char+`,
comparisons, `and`/`or`/`xor`) contribute one axiom row per pointer/integer/bool
variant.

The axiom set is audited two ways:

- **Per-row proof recipes** — `test/prop-test-core.f` (`AX-CENSUS`, run by the
  prop/debug gate phase) carries an audited recipe ledger at the end of the file,
  with exactly one `\ AXR …` line per live `PES` slot. Each recipe restates the
  row's identity — defining package, primitive name, declared arity, and the
  per-slot typed operands — and one proof kind: executable generic, owned-memory,
  floating-point, or fail-closed `noexec`. At census time the `AXR` package parses
  the ledger from this source, binds each recipe to its live row by slot index,
  and cross-checks package, name, and both arities against the live table. A
  missing, duplicate, or stale recipe, or an identity/arity mutation, fails the
  census naming the exact row, so a new axiom cannot land or drift without an
  audited recipe. A self-test with teeth proves those rejections fire by mutating
  a slot, an arity, and a `noexec` row's identity in turn.
  For each *executable* row — stack shuffles, integer/bitwise/comparison
  arithmetic, cell/char pointer stepping, non-atomic memory access on an owned
  buffer, engine-state reads, and floating point — the census first compiles a
  checked candidate from the recipe's exact typed operands and result types (so
  the recipe's declared types must agree with the axiom the checker enforces),
  then executes the primitive in-process. A distinctive value-provenance canary is
  planted below the operand row; the runner reports a trap if the primitive
  reaches below its declared inputs and clobbers the canary. The census asserts
  the measured out-arity equals the arity the axiom *declares*, so a lying axiom
  (declared arity ≠ runtime behaviour) or a primitive that consumes an undeclared
  cell fails even when the final depth happens to match; the `AX-SELFTEST`
  fabricated-declaration check proves the arity comparison has teeth.
- **Non-executable axioms** — syscalls, process/control words (`throw`, `die`,
  `fork`, `spawn-*`), parser literals (`s"`, `char`, `[']`), defining words
  (`create`, `variable`, `constant`), engine/checker introspection (`checker-*`,
  `diag-*`, `parse-name`), fail-closed checker-substrate table accessors that
  `76 die` on an out-of-range dummy index (`wf-tokix@`/`wf-pos@`/`wf-fam@`/
  `wf-width@` via `WF-ROW@`, `tfam-width@` via `TF-REC@`), the seal watermark
  capture (`seal-capture` rewrites live seal state, like `cp!`/`ndict!`),
  image/FFI, and atomic RMW ops cannot be run in-process with dummy operands.
  Their declared arity is pinned instead by the native self-rebuild fixpoint
  (the engine is rebuilt from source through these primitives) and the
  behavioral gate (the rebuilt engine runs real programs). Their recipe is an
  explicit fail-closed `noexec` row bound to that exact slot rather than a
  name/prefix allowlist, so an identity change must update that row and a stale
  classification cannot be inherited by an adjacent or same-spelled overload;
  exact source/live identity stays independently ratcheted by `PEINV` below. The
  substrate's zero-arg high-water readers (`wf-n@`,
  `tfam-n@`, `sumv-n@`, `tf-str-u@`, `tf-pk-n@`, `schema-n@`,
  `schema-root-n@`) are pure variable reads and ARE difftested, matching
  `ndict@`/`cp@`.

Axiom-set size is tracked separately from discharged `TRUSTED`: the census
prints the live `PES` row count (`prim-axiom: N axioms (D difftested, X
noexec)`), and the trusted-inventory `prim-axiom` class (`TRUSTED.md`) counts the
checker's axiom-model trust sites (nominal role casts, structure/record effects,
and the census readers) apart from the general `TRUSTED`/`TRUST` ratchet.

### Inventory ratchet (`tools/primitive-effect-inventory.f`)

The census proves each live axiom's arity is honest and classified, and the
trusted-inventory `prim-axiom` class counts the *trust sites* that read the table.
Neither ratchets the authoritative rows themselves, so an axiom could be added,
deleted, duplicated, or reordered with no audited migration. `PEINV` closes that
gap. It streams the three `PRIM:`/`PPRIM:`-bearing boot-prefix sources
(`src/core/checker.f`, `src/core/sumtype.f`, `src/core/layout-buffer.f`, in
`tools/boot-pin.f` `BP-EACH` load order — which is the live table order) and gives
each row a **stable identity**: the canonical tuple
`<kind> <defining-package> <word-spelling> <flags> <normalized-effect-tokens>`
(`kind` `prim`|`pprim`; package `-` for a bare `PRIM:`; spelling and effect tokens
folded lowercase; flags `trusted-only` when `PRIM-TRUSTED-ONLY!` marks the row).
Identity never depends on a path, line, ordinal, or `PES` address, so
case/whitespace/comment-only edits preserve it.

- **`baseline TRUSTED.md`** compares the parsed rows against the committed
  `primitive-effect-inventory-manifest` block (an ordered list — *not* sorted, so
  a pure reorder is detectable and the exact row can be named). Comparison is
  occurrence-aware (multiset): an identical axiom may repeat legitimately
  (`path0`/`PATH0` — the same case-insensitive symbol with an identical effect is
  declared in two `checker.f` sections), and the manifest records the repeat, so
  the ratchet fails only on an occurrence beyond the committed multiplicity (an
  added or duplicated row), a shortfall (a deleted row), or a reordered position.
- **`strict`** additionally cross-checks the parsed rows against the live `#PE`
  registry — package/name, declared in/out arity, and the `PE-TRUSTED-ONLY` flag —
  row-for-row, proving the source parse is faithful to the in-image table.
- **`manifest`** emits the canonical block; regenerating it is the explicit
  migration a legitimate axiom-set change must commit.

This count of authoritative axiom rows stays distinct from the trust-site classes,
so permanent trust owners and the primitive rows they read remain separate
quantities.

## Typed depth introspection

Stack-snapshot assertions historically could not be typed: `T{ code -> expected
}T` captures an arbitrary-length stack tail whose size is a runtime `depth`
value, so `T{`/`->`/`}T` are trusted words with no checked contract on the
asserted computation or its shape.

The checked replacement expresses the actual and expected computations as two
quotations that must leave the **same row shape**:

```
SNAP= ( [ R -- S ] [ R -- S ] -- )
```

The checker types this signature directly: each quotation argument carries its
own inferred effect, and the shared output row `S` forces both to leave an
identical stack shape. A mismatch is a `CHECK`-time type error, not a runtime
surprise:

```
: CASES ( -- )
   [: 1 2 + ;] [: 3 ;]  SNAP=   \ certifies: both leave one cell
   [: 1 2 ;]   [: 3 ;]  SNAP=   \ rejected: [ -- n n ] vs [ -- n ]
```

Because a quotation is compile-only, `SNAP=` assertions live inside a checked
test word — which is exactly what subjects the asserted code and its shape to the
checker. At runtime `SNAP=` executes each quotation and compares the produced
cells through the same judge path as `T{ }T`. Only the depth-marked drain of each
quotation's output row stays trusted (one word reusing the existing `->`/`}T`
drains), so the comparator adds no new drain primitive while making every
asserted computation and its shape checkable.

Migration: rewriting `T{ code -> expected }T` to `[: code ;] [: expected ;]
SNAP=` inside checked test words retires the three untyped `T{`/`->`/`}T` words
for a net trusted-count drop and upgrades every snapshot assertion from
runtime-only to shape-checked (tracked as habu-shared-t-t-470833e6).

## Notes

- `CHECKING-ON?` toggles the override; with it off, `:` is the plain native colon
  (used to load infrastructure that isn't checkable habu).
- A body using a word with no charted effect raises `E-UNCHECKED`; checked
  build paths must treat that as a refusal unless the call is behind a named,
  tested `TRUSTED:` boundary. A genuine type error also refuses the definition.
- `EFFECT-OF ( a u -- ea eu | 0 )` returns the canonical effect string for a
  charted name, or a single `0` if absent (note the asymmetric stack effect).

## CAD semantic effect vocabulary (package `CAD-EFFECT`)

Everything above is the **stack** effect: the shape of the data/return stacks a
word transforms. A correct stack effect proves nothing about a word's **semantic**
effect — a balanced `( -- )` word can read a parameter, mutate device state, draw a
random number, launch a kernel, or publish an artifact. Rewrite, fusion,
recomputation, caching, and pass scheduling are unsound if a stateful, random, IO,
device, atomic, collective, allocation, or publication operation is treated as
pure. `CAD-EFFECT` (MODEL-CAD-V2-PLAN.md R8; dot
`habu-define-finite-cad-0bdf52ad`) names the finite semantic effects so the checker,
op registry, and optimizer can reason over them instead of trusting the author. It
is `src/cad/effect-types.f` (the vocabulary + truth tables) and `src/cad/effect.f`
(the canonical row algebra over the `lib/nominal` immutable substrate). A general
effect calculus is *not* required; the finite CAD vocabulary is.

### Atoms and slot kinds

`effect-atom` is a closed ten-value sum. `pure` is special — it is the absence of
any effect and never appears in a row; the unique canonical empty row (`PURE`)
carries no binding. The nine effectful atoms bind row entries:

| atom | meaning |
| --- | --- |
| `pure` | no semantic effect (the empty row; never a binding) |
| `param-read` | reads an immutable, digest-bound parameter |
| `state-write` | mutates addressable state |
| `random` | consumes an RNG / nondeterministic source |
| `host-io` | host-side input/output |
| `device-launch` | launches a device kernel |
| `atomic` | atomic / reduction update |
| `collective` | collective / barrier participation |
| `allocation` | allocation / free |
| `publication` | persistent artifact publication |

`slot-kind` is a closed four-value sum naming which part of an op a binding refers
to: `operand`, `attribute`, `capability`, `capture`.

### Conservative truth tables

Four finite tables answer distinct legality questions over the atoms; `?`
predicates take a typed `effect-atom` (`COMMUTE?` a symmetric pair). Conservative
means a safe over-approximation: when unsure, forbid, because over-restricting only
loses optimization while under-restricting is unsound.

- `DUP-OK? ( effect-atom -- bool )` — may an op with this atom be duplicated /
  recomputed? True only for `pure` and `param-read`.
- `CACHEABLE? ( effect-atom -- bool )` — may its result be memoised on resolved
  inputs? True only for `pure` and `param-read`.
- `BARRIER? ( effect-atom -- bool )` — is it a conservative reorder / fusion
  barrier? True for every effectful atom (`state-write` … `publication`).
- `COMMUTE? ( effect-atom effect-atom -- bool )` — may two atoms reorder past each
  other? True only when one is `pure`, or both are `param-read`.

For this closed conservative vocabulary `DUP-OK?`, `CACHEABLE?`, and the negation
of `BARRIER?` coincide on the `{pure, param-read}` safe set; they stay independent
tables because they answer distinct questions and a later resolved-binding
refinement (e.g. a digest-bound deterministic device launch, dots
`habu-persist-cad-semantic-028c0881` / `habu-enforce-effect-aware-cf9181b8`) may
diverge them.

### The effect row and its algebra

A **row** is a canonical, sorted-unique set of bindings held as an opaque one-cell
`effect-row` handle over the `lib/nominal` immutable substrate — there is no wide
by-value product and no small composition bound (a row scales past 4096 bindings;
only a configured resource budget stops it). A binding's identity key is
`(atom, site-path, slot-kind, slot-index)`; nothing else — no runtime digest,
address, generation, sequence, authority instance, handle number, or
allocation-order artifact — enters identity, so canonical equality, serialization,
and cache keys ignore the handle and the build order.

- `NEW`/`EMIT ( nom-builder effect-atom slot-kind n -- nom-builder )`/`FREEZE`
  build a row transactionally; a local `EMIT` starts with an empty site path.
- `PURE` is the unique empty row.
- `UNION` is the canonical merge — associative, commutative, idempotent, and
  content-interned, so repeated identical entries collapse.
- `REMAP ( effect-row n -- effect-row )` prefixes one caller/call-site ordinal onto
  every path capture-free, preserving each binding's exact atom/slot-kind/slot-index.
  Two callees that both bind local slot zero stay **distinct** once remapped into
  different sites, while replaying the same remap is idempotent.
- Different atoms may share a site/slot (`state-write` + `atomic` on operand 0), and
  one atom may bind many slots (`weight` + `bias` parameter reads on operands 0/1).
- `EQUAL?`, `SIZE`, `KEY` (32-byte content key), `ENCODE`/`DECODE`,
  `SNAPSHOT`/`RESTORE` are the canonical identity/serialization surface;
  `VALIDATE` rejects a row whose bindings are not well-formed effect bindings.
- Row-level classifiers fold the atom tables over exactly the atoms a row contains:
  `ROW-DUP-OK?`, `ROW-CACHEABLE?`, `ROW-BARRIER?`, `ROWS-COMMUTE?`.

Rejections are transactional and carry a typed diagnostic: a forged/stale handle
(`E-NOM-HANDLE`), a malformed/noncanonical wire row (`E-NOM-WIRE` /
`E-CADEFF-MALFORMED`), a protocol-count overflow (`E-NOM-WIRE`), an
allocator/resource failure (`E-NOM-BUDGET`), a direct duplicate insertion
(`E-CADEFF-DUPLICATE`), the non-bindable pure atom (`E-CADEFF-ATOM`), a negative
slot index (`E-CADEFF-INDEX`), or a negative call-site ordinal (`E-CADEFF-SITE`).

Because NOM declares its `row`/`path`/`binding` handle types in package NOM's
private section, no external signature can name a `row`; `effect-row` is
CAD-EFFECT's public, nameable brand, and the two audited identity casts
`NOM:ROW>EFF` / `NOM:EFF>ROW` (the only words that name `row`, written in a package
NOM reopen) bridge them. They retire when the substrate exports its handle type
names publicly.

This dot owns the static vocabulary and row algebra only. Checker persistence
(`habu-persist-cad-semantic-028c0881`), the mandatory Maki op-schema row
(`habu-require-maki-op-b14ccc89`), explicit capability tokens
(`habu-add-explicit-cad-58a05453`), runtime binding resolution, effect-aware
fusion/recompute legality (`habu-enforce-effect-aware-cf9181b8`), and cache-key
integration are later dots that consume this vocabulary.

The section above is the substrate (design step **R8-0**: the finite atoms,
slot kinds, conservative tables, and canonical row algebra delivered by
`habu-define-finite-cad-0bdf52ad`). The remaining design steps below specify how
that substrate becomes a mandatory, checker-propagated, planner-consulted, and
cache-keying capability-effect system. They are the design contract for the
implementation leaves; each numbered step names its owning dot.

## R8 capability-effect design (MODEL-CAD-V2-PLAN.md § R8)

R8 has **two deliberately separate representations**. The **static op-schema
effect row** (R8-1) is a compile-time slot declaration: an atom, a stable site
path, a slot kind, and a slot index, and *nothing execution-specific*. The
**resolved semantic binding set** (R8-4) is execution-specific: it pairs each
static binding with the exact semantic fact (a parameter digest, a state owner
plus generation, an RNG sequence, a device authority) that a given invocation
supplies, or returns a typed unresolved/uncacheable reason. The checker and
registry only ever see the static row; the resolver and cache owners consume the
resolved set. Keeping the two apart is what makes the static row a sound,
process-stable identity and the resolved set a precise cache discriminant.

### R8-1 The finite sealed static op-schema effect row (dot `habu-require-maki-op-b14ccc89`, substrate `habu-define-finite-cad-0bdf52ad`)

**Sealed finite vocabulary.** The ten R8 effect classes are a *closed*
`effect-atom` sum — no user-extensible atoms, no lattice beyond the conservative
tables, no ambient effect variables. The plan's list maps one-to-one onto the
vocabulary spellings above:

| R8 class | atom | binds a row entry? |
| --- | --- | --- |
| pure | `pure` | no — the absence of effect; the unique empty row `PURE` |
| parameter read | `param-read` | yes |
| state write | `state-write` | yes |
| random | `random` | yes |
| host IO | `host-io` | yes |
| device launch | `device-launch` | yes |
| atomic / reduction | `atomic` | yes |
| collective / barrier | `collective` | yes |
| allocation / free | `allocation` | yes |
| persistent publication | `publication` | yes |

**Schema encoding (fields).** A row is a canonical sorted-unique set of
**binding records**; each record is a self-describing tagged tuple whose *only*
identity fields are:

| field | width / form | meaning |
| --- | --- | --- |
| atom tag | 4-bit discriminant in a tagged byte (1..9; 0 = `pure`, never stored) | which effect class |
| slot-kind tag | 2-bit discriminant in a tagged byte (`operand`/`attribute`/`capability`/`capture`) | which part of the op the binding refers to |
| slot index | varint, non-negative | which operand/attribute/capability/capture slot |
| site path | length-prefixed varint sequence of non-negative call-site ordinals; empty for a local declaration | the stable lexical/call-site path |

No runtime digest, address, generation, sequence, authority instance, handle
number, arena offset, or allocation-order artifact is a field — those are R8-4
resolved facts, not schema fields. The public row value is a one-cell opaque
`effect-row` handle over the `lib/nominal` immutable arena; the handle number is
an implementation reference only. Canonical identity is the **content** of the
sorted binding records: `KEY` is a 32-byte content key (the plan's
content-addressed intern form) over the canonical `ENCODE` bytes, and schema
identity, serialization, diagnostics, replay, AOT, fixpoint bytes, and cache
keys derive from it, never from the handle or build order. A versioned wire
count and the checked allocation/resource budget are the only protocol/policy
bounds; their overflow or exhaustion returns a typed diagnostic (`E-NOM-WIRE` /
`E-NOM-BUDGET`), not a silent cap. There is no small by-value composition bound
(a row scales past 4096-binding composition).

**Canonical composition rule.** Two rows compose by capture-avoiding remap then
canonical union:

1. `REMAP ( effect-row n -- effect-row )` prepends one caller/call-site ordinal
   onto every binding's site path, capture-free, preserving each binding's exact
   atom, slot kind, and slot index. Raw union of two callee-local rows is
   **unsound** because both callees may name their first resource `slot 0` under
   the empty path; the checker MUST `REMAP` each callee into a distinct
   caller/call-site namespace before union.
2. `UNION` merges the remapped canonical sequences by content interning, so it is
   associative, commutative, and idempotent, and repeated identical records
   collapse. A *direct* duplicate insertion into a builder rejects
   (`E-CADEFF-DUPLICATE`) — only `UNION` deduplicates.

Consequences the composition rule guarantees: one atom may hold **many** bindings
(weight and bias parameter reads on operand slots 0 and 1 stay distinct), and
**different** atoms may share one site/slot (a `state-write` and an `atomic`
reduction on operand 0 both record their fact). Two callees that each bound local
slot 0 stay distinct once remapped into different sites, while replaying the same
remap is idempotent (`REMAP` is deterministic; the row is order-independent).

**Non-goals (this is NOT a general effect calculus).** R8 deliberately does not
provide: effect polymorphism or effect inference over an open effect set; effect
handlers, masking, or scoped effect elimination; a user-extensible or
package-local atom vocabulary; a join lattice richer than the closed
`DUP-OK?`/`CACHEABLE?`/`BARRIER?`/`COMMUTE?` tables; or *any* runtime value inside
a static row. The finite closed CAD vocabulary is sufficient for rewrite, fusion,
recompute, cache, and schedule legality, and its finiteness is what keeps the
static row a decidable, sealable, content-addressed identity.

### R8-2 Checker propagation over the existing stack-effect machinery (dot `habu-persist-cad-semantic-028c0881`)

The effect row rides the *existing* checked stack-effect machinery as sealed
side-metadata; it introduces no ambient-effect inference. The mechanisms it
reuses (see `docs/type-families.md`):

- **Opaque nominal handle, not a value.** `effect-row` is an arity-0 nominal
  brand (an `evidence-family`-style token, like `aligned<ptr,t,align-16>`): the
  checker threads one cell it never destructures. Primitive rows, stored-word
  metadata, and quotation metadata each carry an `effect-row` handle alongside
  the stack/return/linear/control facts already recorded per word, the same way
  an `xt<effect>` storage cell bakes a concrete effect into a word's unified
  signature scheme and a `TYPED-VARIABLE`'s accessor recovers it.
- **REMAP-then-UNION at every call and quotation boundary.** Composing a callee's
  or an applied quotation's effect is `CAD-EFFECT:REMAP` into the caller's stable
  lexical-site namespace followed by `UNION`, mirroring how the checker composes
  stack effects by unification. Only successful calls union; latent quotation
  effects and failed-overload speculation roll back on the trail exactly as
  stack-effect binding does.
- **Parametricity/forgery seal (the `NP-CHECK` / `NP-MINT-CHECK` analogue).**
  `NP-CHECK` re-inspects a definition's declared quantifiers after the body and
  rejects a body that specialized a quantifier to a sealed family or minted
  input-unbound output vars (`E-NONPARAMETRIC-EFFECT`). The CAD row gets the same
  post-body discipline: the composed row of the body must satisfy the declared
  row — a body may not silently *add* an effect (a `( -- )`-declared word that
  reaches device or IO authority rejects) nor *drop* one it performed. The
  declared row is a sealed contract, not an inferred summary.
- **Raw-storage nominal seal (`TVK-RAW`).** `here`/`create`/`variable`/`constant`
  publish `TVK-RAW` cells that already reject a nominal-family value, so a raw
  dictionary cell cannot mint or launder an `effect-row`. Persistence therefore
  stores rows *only* through the typed nominal handle path (`RAW-TRUST-NEXT`
  registration), never a raw cell; a forged or raw-stored row rejects before
  metadata mutation.
- **Order-sensitive capability typestate (the `CPPSLOT` precedent).** The
  cp.async slot typestate `CPPSLOT:COMMIT -> CPPSLOT:WAIT -> CPPSLOT:READ`
  (`cpp-committed<p>` → `cpp-ready<p>`) shows the checker already threading a
  linear, ordered typestate token. The R8-3 capability tokens for mutable /
  order-sensitive resources (`state-write`, `atomic`) are the same shape: linear
  tokens whose acquire/use/release order the checker proves by conservation.
- **Collective / barrier ties to M5/M5b.** A `collective` atom is the
  semantic-effect-level counterpart of the structural `CTL-BARRIER` flag
  (`bar.sync`, cp.async wait) that M5 marks at the `E-ADD-EFFECT` choke point and
  that `PTX-BARRIER!` sets explicitly. `BARRIER?`-true atoms forbid reorder/fusion
  the way `ALL-CF-UNIFORM?` forbids a barrier under divergent control
  (`E-DIVERGENT-BARRIER`); the CAD `collective` binding and the M5 barrier flag
  are the same prohibition at two layers.

Throughout, the checker sees **only schema fields** — atom, site path, slot kind,
slot index. No digest, generation, address, or authority instance is ever placed
in checker metadata; embedding a runtime value in a schema is a category error
that R8-4 exists to prevent.

### R8-3 Mandatory registration, capability tokens, and planner legality

**Mandatory Maki registration (dot `habu-require-maki-op-b14ccc89`).** Every op
schema in `maki/op-registry.f` MUST carry one sealed canonical `effect-row`,
stored only through the typed nominal handle path. Registration validates every
referenced operand/attribute/capability slot against the schema's arity and kind,
derives schema identity from canonical row contents (not the handle), and
classifies the existing op census with no hidden default. This extends the
registry's existing fail-closed membership gate: just as `OPR-REF` throws
`E-OPR-INCOMPLETE` when an op has no reference oracle, a missing, forged, stale,
cross-owner, raw-stored, duplicate, wrong-kind/out-of-range, or inconsistent-PURE
row rejects registration. Weight and bias bindings remain distinct; changing an
atom or slot changes schema identity while changing the handle, insertion order,
allocation order, or a later invocation artifact does not.

**Explicit capability tokens (dot `habu-add-explicit-cad-58a05453`).** Raw
random/state/atomic/publication/IO/device primitives expose only stack effects
today; checked callers get typed authority through sealed package-owned opaque
tokens built on the `CAPTOK` precedent (`maki/db/capability.f`): a
`CAPTOK:grant` is a nominal arity-0 handle over an append-only authority pool
whose *only* mints are `ROOT` (the trusted authority-injection boundary) and
subset-only `ATTENUATE`, so a raw `n` can never stand where a grant is required
and authority only ever narrows. The CAD capability tokens follow that shape:
**linear** for mutable / order-sensitive resources (`state-write`, `atomic`,
`allocation`) and **one-shot** for `publication`; checked wrappers consume or
thread the exact token while the raw primitive stays inaccessible. Missing
authority, duplication, drop, laundering, double publication, wrong resource, and
exception-path leaks reject; legal scoped use certifies. Capability owner, scope,
generation, digest, and sequence are typed inputs to the R8-4 resolver **only** —
never baked into a static row.

**Planner legality (dot `habu-enforce-effect-aware-cf9181b8`).** The optimizer
consumes the sealed row plus its resolved bindings before class/backend legality
and folds the atom tables (`ROW-DUP-OK?`, `ROW-CACHEABLE?`, `ROW-BARRIER?`,
`ROWS-COMMUTE?`) over exactly the atoms a row contains. Each atom forbids a
specific set of rewrite/fusion/recompute/cache moves:

| atom | `DUP-OK?` | `CACHEABLE?` | `BARRIER?` | forbidden planner moves |
| --- | --- | --- | --- | --- |
| `pure` | yes | yes | no | none |
| `param-read` | yes iff digest-resolved | yes iff digest-resolved | no iff resolved, else yes | duplicate/memoise an *unresolved* read (conservative barrier) |
| `state-write` | no | no | yes | duplicate or recompute (double-applies); reorder across another state/atomic on the same owner |
| `random` | no | no | yes | duplicate or recompute (advances / redraws the RNG) |
| `host-io` | no | no | yes | duplicate, hoist, sink, or reorder host IO; run in a pure/analysis pass with no IO authority |
| `device-launch` | no | no (until digest-bound deterministic) | yes | duplicate; move a launch across a host sync or into a pure pass |
| `atomic` | no | no | yes | reorder past a `state-write` or another `atomic` on the same site |
| `collective` | no | no | yes | duplicate (double-participate); move under divergent control (M5) |
| `allocation` | no | no | yes | duplicate (double alloc/free); reorder past an aliasing lifetime edge |
| `publication` | no | no | yes | fuse, recompute, or replay a publish; publish without the one-shot token |

The legality *table* is static and never changes with a resolved fact; a resolved
digest changes only *binding identity* (R8-4), so a param-read that fails to
resolve degrades to a conservative barrier without mutating the table.
Effect-barrier splits emit a structured reason naming the effect, stable site,
slot, and bound resource.

### R8-4 The separate runtime resolver and the cache-identity rule (dots `habu-resolve-runtime-cad-2864336f`, `habu-census-cad-effect-3240237b`, `habu-define-complete-cad-90a9945c`, `habu-key-caches-by-fddcea19`)

**The resolver (dot `habu-resolve-runtime-cad-2864336f`, `maki/effect-bindings.f`).**
One checked resolver combines a sealed static row with typed invocation operands,
attributes, capability tokens, the row's stable semantic site path, and canonical
Artifact metadata, and produces a sorted set of
`(atom, site-path, slot-kind, slot-index, semantic-fact)` entries, or a typed
uncacheable/unresolved reason. Resolution per class: `param-read` resolves an
immutable payload digest; `state-write`/`random` bind owner plus
generation/sequence; `host-io`/`device-launch`/`allocation`/`atomic`/
`collective`/`publication` bind the exact authority facts policy permits or return
a typed reason. Site paths derive from canonical revision/node/call structure,
never an address or insertion counter, and no address or build order enters the
output. **Only an exact repeated full tuple is idempotent**; the same atom/site/
slot resolving to two different facts is a typed conflict; different sites or
slots stay distinct even when their local slot numbers or payload digests
coincide (weight vs bias, and two call paths, never collide). Every static
binding resolves exactly once or returns a typed reason, and no cache owner may
construct or project the set privately.

**Cache identity (dots `habu-census-cad-effect-3240237b`,
`habu-define-complete-cad-90a9945c`, `habu-key-caches-by-fddcea19`).** A
read-only census first enumerates every cache/promotion key reachable from Model
CAD execution (schedule replay, artifact compilation, result lookup, evidence,
persistent store rows, promotion) and freezes each one's semantic dependency
domain in `MODEL-CAD-V2-PLAN.md`. `habu-define-complete-cad-90a9945c`
(`maki/effect-projection.f`) then defines, per cache/artifact class, a **sealed
versioned projection policy** and a checked projector that returns a projection
digest **plus completeness evidence** from the *full* resolved set. The
cache-identity rule the projector enforces:

- Every runtime parameter or capability-controlled input **relevant** to a
  cache's artifact class changes (or explicitly disables) that cache's identity.
- Every **omitted** fact carries an explicit, independently tested **irrelevance
  proof**; an omission without one is not permitted.
- An unknown atom or domain, or any unproved omission, falls back to the full
  digest or a typed **uncacheable** result — never a silent partial key.
- The projection **policy version** participates in the key.
- Cache owners consume the projection; they may not build policies or filter
  bindings ad hoc.
- Address and traversal-order changes never change a key; replay and persistence
  round-trip the projection digest plus policy version, and an old row lacking
  them rejects or migrates explicitly.

`habu-key-caches-by-fddcea19` is the integration dot: it coordinates the disjoint
census migration leaves, each consuming the completeness-proven projection for its
declared domain, and edits no consumer source ad hoc.

**Adversarial static / runtime / projection mutation matrix.** Every row is a
required negative or identity test; the "layer" says which owner enforces it.

| # | Mutation | Layer | Required outcome |
| --- | --- | --- | --- |
| 1 | Forged / fabricated row handle | static | reject `E-NOM-HANDLE` |
| 2 | Stale (freed-arena) handle | static | reject `E-NOM-HANDLE` |
| 3 | Cross-owner handle substitution | static | reject (sealed owner) |
| 4 | Malformed / noncanonical wire row | static | reject `E-NOM-WIRE` / `E-CADEFF-MALFORMED` |
| 5 | Protocol-count overflow | static | reject `E-NOM-WIRE` |
| 6 | Allocator / resource exhaustion | static | reject `E-NOM-BUDGET` |
| 7 | Direct duplicate binding insertion | static | reject `E-CADEFF-DUPLICATE`; `UNION` deduplicates instead |
| 8 | `pure` atom stored as a binding | static | reject `E-CADEFF-ATOM` |
| 9 | Negative slot index | static | reject `E-CADEFF-INDEX` |
| 10 | Negative call-site ordinal | static | reject `E-CADEFF-SITE` |
| 11 | Two callees both bind local slot 0, unioned without remap | checker | unsound; distinct only after `REMAP`, so the checker must remap first |
| 12 | Replay the same remapped binding | static | idempotent — no change |
| 13 | Reorder / rehash bindings by build order | static | no identity change (content-addressed `KEY`) |
| 14 | Op schema registered with no effect row | registry | reject registration (mandatory row) |
| 15 | Row stored through a raw variable/create/constant cell | checker | reject (`TVK-RAW` nominal seal) |
| 16 | `( -- )`-declared word reaches device / IO authority | checker | reject (composed row ≠ declared row) |
| 17 | Missing capability token at a state / publication site | capability | reject (missing authority) |
| 18 | Duplicate or drop of a linear capability token | capability | reject (linear conservation) |
| 19 | Reuse a one-shot publication token (double publish) | capability | reject (one-shot) |
| 20 | Duplicate / recompute a `random` or `state-write` op | planner | reject (`DUP-OK?` false) |
| 21 | Reorder an `atomic` across a `state-write` on the same site | planner | reject (`COMMUTE?` false / barrier) |
| 22 | Hoist, sink, or move a `host-io` or `device-launch` | planner | reject (`BARRIER?` true) |
| 23 | Fuse or recompute a `publication` | planner | reject (never fuses / recomputes) |
| 24 | Mutate a weight parameter digest | runtime | resolved fact changes → affected projected key changes |
| 25 | Mutate a bias digest independently | runtime | distinct key change (weight and bias distinct) |
| 26 | Advance a mutable state generation | runtime | affected key changes |
| 27 | Advance an RNG sequence | runtime | affected key changes |
| 28 | Change target / device authority | runtime | affected key changes |
| 29 | Change publication scope | runtime | affected key changes |
| 30 | Same atom/site/slot resolves to two different facts | runtime | typed conflict (reject) |
| 31 | Missing / stale / wrong-kind artifact or capability at resolve | runtime | typed unresolved / uncacheable reason |
| 32 | Reorder or re-address the resolved set | runtime | no key change (address / order independent) |
| 33 | Omit a binding from a projection with no irrelevance proof | projection | fall back to full digest or typed uncacheable |
| 34 | Omit a binding under a tested irrelevance rule | projection | key unchanged, and the rule is independently tested |
| 35 | Unknown atom / domain in a projection | projection | full digest or typed uncacheable |
| 36 | Bump the projection policy version | projection | key changes (policy version participates) |
| 37 | Replay / persist a row without projection digest + policy version | cache | reject or explicit migrate |

### R8-5 Implementation leaves and dependency order

The R8 model is decomposed into these leaves; each owns the spec step named and
is annotated with a dated design-reference note in its dot file. The dependency
edges (a dot's `blocks:` list names its blockers) already form the correct DAG
and were audited during this design phase — no edge repair was required.

| leaf dot | owns | blocked by (prerequisites) |
| --- | --- | --- |
| `habu-define-finite-cad-0bdf52ad` (closed) | R8-0 vocabulary, tables, row algebra | immutable-nominal arena |
| `habu-seal-cad-effect-49cac404` | R8-1 sealed authority boundary | define-finite, seal-owners-syntax |
| `habu-persist-cad-semantic-028c0881` | R8-2 checker metadata + site substitution | seal |
| `habu-require-maki-op-b14ccc89` | R8-1/R8-3 mandatory static registry rows | seal |
| `habu-add-explicit-cad-58a05453` | R8-3 capability tokens | persist (checker), linear-once-resource |
| `habu-resolve-runtime-cad-2864336f` | R8-4 runtime resolver | require-maki-op, add-explicit-cad, canonical-artifact |
| `habu-enforce-effect-aware-cf9181b8` | R8-3 planner legality | resolve-runtime |
| `habu-census-cad-effect-3240237b` | R8-4 cache-identity census | resolve-runtime |
| `habu-define-complete-cad-90a9945c` | R8-4 sealed projection policy | census |
| `habu-key-caches-by-fddcea19` | R8-4 cache-key integration | define-complete + census leaves |

Ordering: `define-finite → seal → {persist, require-maki-op} → add-explicit-cad
→ resolve-runtime → {census, enforce-effect-aware}`, then `census →
define-complete → key-caches`. The planner (`enforce-effect-aware`) consumes
resolved bindings, so it correctly branches off `resolve-runtime` rather than
sitting strictly before it; the plan's linear listing collapses that branch.

## R8 acceptance mapping

Each acceptance criterion of the design dot and of `MODEL-CAD-V2-PLAN.md § R8`
maps to a numbered spec step and, where applicable, the mutation-matrix rows that
witness it.

| acceptance criterion | spec step | matrix rows |
| --- | --- | --- |
| Multiple parameter/state bindings compose canonically with stable site paths | R8-1 composition rule | 7, 11, 12 |
| One atom binds many slots (weight and bias reads stay distinct) | R8-1 composition rule | 24, 25 |
| Different atoms may share a site/slot (state-write + atomic) | R8-1 composition rule | — |
| Two callees binding local slot 0 stay distinct after remap; replay is idempotent | R8-1 / R8-2 REMAP | 11, 12 |
| Canonical identity ignores handle and build order | R8-1 content `KEY` | 13, 32 |
| Random / stateful duplication rejects | R8-3 planner | 20 |
| Atomic reorder rejects | R8-3 planner | 21 |
| Writes/atomics cannot cross illegal reorder/fusion boundaries | R8-3 planner | 21, 22 |
| Pure analysis needs no IO/device token; pure passes run without authority | R8-3 capability | 16, 22 |
| Publication requires one-shot authority; analysis cannot publish | R8-3 capability | 19, 23 |
| Every relevant runtime input changes or disables cache identity | R8-4 cache identity | 24–29, 31 |
| Every omission has a tested irrelevance proof | R8-4 projection | 33, 34 |
| Unknown atom/domain falls back to full digest or typed uncacheable | R8-4 projection | 35 |
| Address/order changes do not change a key | R8-4 resolver / projection | 13, 32 |
| Replay/persistence round-trip projection digest + policy version | R8-4 cache | 37 |
| Not a general ambient effect calculus | R8-1 non-goals | — |
| No runtime values embedded in a static schema | R8-1 fields / R8-2 | 15, 30 |
| Package sealing preserves canonical row authority across snapshot/replay/fixpoint | R8-1 / R8-2 seals | 1–4, 15 |
