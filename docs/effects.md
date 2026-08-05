# habu stack-effect syntax

A checked definition declares its effect in the `( … )` immediately after the
name: `: SQUARE ( i64 -- i64 ) DUP * ;`. The checker infers the body's effect and
unifies it with the declaration; a mismatch refuses the definition. Source
comments and audited `TRUST` rows are parsed by the checker boundary adapter in
`src/core/checker.f`; checker-owned primitive, literal, memory, and control-flow
effects are built structurally, not reparsed from strings.

## Grammar

The parser is `PSIG` / `PSIDE` / `PSTACK` / `SIG-TYPE` in `src/core/checker.f`.

```
sig      = side '--' side
side     = stack ( '|' stack )?
stack    = rowvar? type*
type     = conname | role | linear-name | family | tyvar
         | 'ptr' type
         | '[' quot-sig ']'
quot-sig = stack '--' stack ( '|' stack '--' stack )?
family   = ( package ':' )? tail ( '<' type ( ',' type )* '>' )?
conname  = n f r i64 u8 u16 u32 cell char bool addr str f32
role     = idx len count off fd rc pid ms ns tok reg label va symidx asm img snap
linear-name = the exact token given to `DEFLINEAR`
tyvar    = a..z, except the reserved `n`, `f`, `r`
rowvar   = A..Z          (same letter → same row var; leading position only)
```

### The two return-stack conventions

There are two of them, and they are not the same shape. Getting them backwards
is the single most damaging mistake in this grammar, so both are spelled out.

**Top level — the bar goes inside each side.** `PSIDE` parses one side as
`data-stack ( '|' return-stack )?`, and the top-level `--` separates the two
sides:

```
( Din | Rin -- Dout | Rout )
>R  ( R a | S -- R | S a )      \ certifies
R>  ( R | S a -- R a | S )      \ certifies
```

A signature with no bar anywhere has no return-stack clause at all, and the
checker ignores those rows — that is the ordinary case.

**If you use the return clause, put a bar on both sides.** The grammar lets you
write only one, and the declaration is accepted, but the result is an
unusable word. With a bar on just one side the checker allocates a *fresh,
unrelated* row for the missing end, so the declared return effect goes from one
row to a different row that nothing can satisfy. Both `( R n | S -- R )` and
`( R n -- R | S )` declare at exit 0 and then reject at every call site with
exit 70 — the same silent-acceptance failure shape as the truncation trap below.
`( R n | S -- R | S )` is the return-stack-neutral spelling you actually want.

**Quotations — the bar separates two full `in -- out` pairs.** Inside `[ … ]`,
`SIG-PARSE-QUOT` parses `in -- out` and then optionally `| rin -- rout`:

```
[ in -- out ]                   \ return-stack neutral
[ in -- out | rin -- rout ]
( R a [ R a -- R | S -- S a ] -- R )    \ certifies
```

**Do not write the quotation shape at the top level.** `( R a -- R | S -- S a )`
does not mean "moves `a` to the return stack". It **parses silently** and means
something else: `PSIG` reads `Din = R a`, the top-level `--`, then `Dout = R`,
`Rout = S`, and stops — the trailing `-- S a` is never consumed and no error is
raised. Declaring such a word exits 0; the first checked word that *calls* it
fails with exit 70 at the call site. Writing the top-level shape inside a
quotation is the safer error: `[ R a | S -- R | S a ]` is a hard syntax reject
(`checker: bad stored signature`, exit 76).

### Rows, variables, and lexing

- A **row var** (`R`, `S`, …) at the front of a stack stands for "the rest of the
  stack below" — row polymorphism. It is only recognised in leading position; an
  upper-case token anywhere else is looked up as a type name and rejects with
  `unknown type 'R' in signature`. Row vars and type vars are separate
  namespaces (case decides which), and both are scoped to one signature.
- Stacks with no leading row var share one implicit data row (and one implicit
  return row).
- The implicit row in a checked definition is sealed for the body: callees may
  preserve it, but may not bind it by consuming below the declared inputs. This
  rejects hidden underflow such as a trusted `img -- img` boundary called from a
  word declared `( -- )`.
- A **type var** (`a`, `b`, …) is a fresh polymorphic type; reusing the same
  letter in one signature means the same type. `n`, `f`, and `r` are **not**
  type vars — they are the reserved single-letter concrete types below.
- Type, role, and family names are **lower case and case-sensitive**. `I64` and
  `IDX` are not spellings of `i64` and `idx`; they reject as bad signatures.
- Tokens are whitespace-separated, but `<`, `>`, and `,` are also single-token
  delimiters, so a family application may be written with or without spaces
  (`pkg:result<pkg:index>` and `pkg:result < pkg:index >` parse identically).
- Don't nest `( )` — the inner `)` closes the comment.

## Types

The built-in type table is `CT-INIT` in `src/core/checker.f`. It has thirty
entries and this is all of them.

| type | class | width | sign class | meaning |
| ---- | ----- | ----- | ---------- | ------- |
| `n` | int | 64 | generic | the unconstrained integer; interchangeable with every integer type in both directions |
| `cell` | int | 64 | generic | machine word |
| `i64` | int | 64 | signed | signed 64-bit |
| `u8` | int | 8 | unsigned | unsigned byte |
| `u16` | int | 16 | unsigned | unsigned halfword |
| `u32` | int | 32 | unsigned | unsigned word |
| `char` | int | 8 | unsigned | character cell |
| `addr` | int | 64 | **addr** | raw address; its own sign class (see the lattice below) |
| `bool` | bool | 1 | — | a flag; comparisons return this, not an integer |
| `f` | bool | 1 | — | **an alias for `bool`**, not a type variable; it parses to `bool` and renders as `bool` |
| `r` | float | 64 | — | double-precision float; not a type variable |
| `f32` | float | 32 | — | single-precision float; does **not** unify or widen with `r` |
| `str` | object | — | — | declarable, but no shipped primitive produces or consumes it, and no signature in the tree names it. A string literal is `ptr u8 n`, not `str`. |
| `idx len count off fd rc pid ms ns tok reg label va symidx asm img snap` | role | 64 | — | seventeen nominal roles; distinct from each other and from every integer type |
| `ptr τ` | — | 1 | — | typed pointer; `@`/`!` move `τ`. Also spelled `ptr<space,elem>` as a family application. |
| `[ in -- out ]`, `[ in -- out \| rin -- rout ]` | — | 1 | — | a quotation / execution token carrying its own effect. Prose elsewhere in this document writes this as `xt<E>`; that is notation, not a spelling the parser accepts. |
| `pkg:tail`, `pkg:tail<arg,…>` | — | varies | — | a declared type family: `NEWTYPE`, `ENUM`, `STRUCTURE`, `PRODUCT`, `SUMTYPE`, `LAYOUT-BUFFER`, and `DEFTYPE` all mint these |
| the token given to `DEFLINEAR` | linear | 64 | — | a linear-once cell type (see below) |

### Integer widening

Widening is `INT-WIDENS?` / `CON-OK?` in `src/core/checker.f`. It applies only
to **integer-class** types (the first eight rows above), only at the **top level
of a stack cell**, and only in an input or coercion position (`UNIFY-IN` /
`UNIFY-COERCE`) — never inside a `ptr` pointee, never between roles, never
between `bool` and an integer, and never between `r` and `f32`.

`n` is outside the lattice: it unifies with every integer type in either
direction unconditionally. For the other seven, `got` flows into `want` when
`width(got) <= width(want)` **and** one of: either sign class is `generic`
(`cell`); the sign classes are equal; or `got` is unsigned, `want` is signed,
and `got` is strictly narrower.

`addr` sits in a sign class of its own. That is the whole point: `n` and `cell`
reach `addr` (both are generic), but `u8`, `u16`, `u32`, `char`, and `i64` do
not, so a byte or a signed count cannot drift into an address, and an address
cannot drift back into `i64`.

Measured, `Y` = the row type is accepted where the column type is declared:

```
got \ want   n  i64  u8  u16  u32  cell  char  addr
n            Y   Y   Y    Y    Y    Y     Y     Y
i64          Y   Y   .    .    .    Y     .     .
u8           Y   Y   Y    Y    Y    Y     Y     .
u16          Y   Y   .    Y    Y    Y     .     .
u32          Y   Y   .    .    Y    Y     .     .
cell         Y   Y   .    .    .    Y     .     Y
char         Y   Y   Y    Y    Y    Y     Y     .
addr         Y   .   .    .    .    Y     .     Y
```

`bool` and `f` interconvert (they are the same type). Nothing else in the type
table widens at all: every role, every family, every linear type, `str`, `r`,
and `f32` unify only with themselves.

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

An unknown type token is never silently interned. It rejects the definition with
`unknown type 'tok' in signature` on stderr, carrying the structured code
`E-UNKNOWN-SIGNATURE-TYPE`; the other signature-syntax codes are
`E-BARE-PTR-SIGNATURE` (`ptr` with no element type), `E-WRONG-ARITY` (a family
applied to the wrong number of arguments), and `E-BAD-SIGNATURE` (a missing or
misplaced `--`, `|`, or `]`).

## Declaring your own scalar types

`DEFTYPE` and `DEFLINEAR` look like a pair. They are not: they use different
substrates, scope differently, spell differently in a signature, and produce
different diagnostics. Read both before choosing.

### `DEFTYPE NAME` — a package-scoped value nominal

`DEFTYPE` lives in `lib/type/deftype.f` (package `VNOM`); a file that uses it
must `require lib/type/deftype.f` first. The substrate is a **package-scoped
arity-0 type family** — the same machinery as `NEWTYPE` and `maki/extent.f`, not
the built-in type table.

Three consequences follow directly from that substrate.

- **The signature spells a lower-case tail, not the surface name.** The surface
  name is upper case by project convention and `DEFTYPE` folds it to lower case
  to form the family tail. `DEFTYPE FRAME-IDX` declares the type you name as
  `frame-idx` inside the declaring package and `PKG:frame-idx` outside it.
- **It is package-scoped.** `DEFTYPE SERIAL` in package `CAMERA` and `DEFTYPE
  SERIAL` in package `FRAME` are two unrelated types with no collision.
- **A mismatch renders the family application, with its empty argument list.**
  `: F ( n -- frame-idx ) ;` rejects with
  `expected: frame-idx<> actual: n` — the `<>` is part of the rendering.

`DEFTYPE` auto-derives the explicit converter pair `>NAME ( n -- name )` and
`NAME>N ( name -- n )` as no-op identity casts, exactly like `>IDX`/`IDX>N`.
Those converters are the only way across the boundary; there is no implicit
collapse to `n`. They obey ordinary package visibility, so declaring inside a
package without `public` keeps them private and `PKG:>NAME` is `E-UNDEFINED`
from outside.

The name is fail-closed: it cannot reuse a built-in type, a live family, a role,
an atom prefix, or a one-letter type variable. `DEFTYPE IDX` and `DEFTYPE A`
both reject with `bad newtype declaration '…': reserved name` (throw 7110, exit
67). This gives application code (camera serials, frame indexes, exposure-µs,
GMSL channels) compile-checked distinct integers at zero runtime cost, without
an engine edit or fixpoint rebuild.

### `DEFLINEAR name` — a global linear cell type

`DEFLINEAR` is a core word (`src/core/roles.f`, driving `CHECKER-DEFLINEAR`);
nothing has to be required. Its substrate is the **built-in type table itself**:
the declaration appends a `CT-LINEAR` row, exactly beside `idx` and `i64`.

- **The signature spells the name exactly as declared, case-sensitively.**
  `DEFLINEAR own` gives you `own`; writing `OWN` in a signature is
  `unknown type 'OWN' in signature`. Because signature types are lower case
  everywhere else, declare these lower case — as the tree does
  (`nom-builder`, `process-pty-handle`).
- **It is global, not package-scoped.** A second declaration of the same name
  anywhere, or of a name already in the type table, is
  `checker: bad or duplicate signature type` (exit 70).
- **A mismatch renders the bare name.** `: F ( n -- own ) ;` rejects with
  `expected: own actual: n` — no `<>`, because this is a table entry, not a
  family application.
- **No converters are derived.** Producers and consumers are yours to declare;
  they are what makes the type usable at all.
- It is top-level-interpret-only and is rejected inside a checked body.

What the type *means* is the rest of this section. A `DEFLINEAR` type is
noncopyable — a **linear-once** resource that must be used (consumed or passed
on) exactly once. Use it for owner or lifetime tokens around arena-backed
records, and for acquire/release framing (evaluate/include frames, mmap slots,
snapshot phases).

The checker enforces this by **conservation**: at every step whose declared
effect does *not* itself name a linear type, the number of live linear values on
the combined data+return stack may not change. So a generic word that would
duplicate a linear (`dup`, `over`, `tuck`, `2dup`), drop it
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

## Examples

Primitive effects are the `PRIM:` axiom rows in `src/core/checker.f`; the
higher-order combinators are ordinary definitions in `src/core/combinators.f`.
(The `src/prims.fs`, `src/control.fs`, and `src/pickroll.fs` this section used
to cite are the *Gforth bootstrap* sources, now under `bootstrap/src/`. They
model a different, older word set and are not what `bin/hb` runs.)

```
DUP    ( R a -- R a a )            \ row-polymorphic: any one value, duplicated
SWAP   ( R a b -- R b a )
+      ( R n n -- R n )            \ also ( R ptr a n -- R ptr a ) and ( R n ptr a -- R ptr a )
<      ( R n n -- R bool )         \ also ( R ptr a ptr a -- R bool ); comparisons yield bool
@      ( R ptr a -- R a )
DEPTH  ( R -- R n )
>R     ( R a | S -- R | S a )      \ moves a value data→return stack
R>     ( R | S a -- R a | S )
EXECUTE( R [ R -- S ] -- S )       \ run a quotation
DIP    ( R a [ R -- S ] -- S a )   \ run a quotation under the top item
KEEP   ( R a [ R a -- S ] -- S a ) \ run with a copy, keep original
BI     ( R a [ R a -- R b ] [ R b a -- R b c ] -- R b c )
TRI    ( R a [ R a -- R b ] [ R b a -- R b c ] [ R b c a -- R b c d ] -- R b c d )
TIMES  ( R i64 [ R -- R ] -- R )   \ counted iterate (trusted runtime boundary)
EACH   ( R ptr a i64 [ R a -- R ] -- R )
MAP    ( R ptr a i64 [ R a -- R a ] -- R )
FOLD   ( R ptr a i64 b [ R b a -- R b ] -- R b )
```

Two notes on that list. The arithmetic and comparison axioms are declared over
plain `n`, not `i64`; because `n` unifies with every integer type, writing
`: F ( i64 i64 -- i64 ) + ;` still certifies — but `n` is what the axiom says,
and it is what a diagnostic will print. And `>R`/`R>` are not `PRIM:` rows at
all: the checker models the return-stack transfer structurally, and the
signatures above are how you would spell that transfer for a `TRUSTED:` word of
your own.

`WITHIN`, `?DUP`, `?DUP-IF`, `PICK`, and `ROLL` are **not defined in `bin/hb`**.
They exist only in the Gforth bootstrap word set; naming any of them in checked
source is `E-UNDEFINED`. Use `IF`/`ELSE`/`THEN` and named factors instead of a
value-dependent `?DUP`, and `DUP`/`OVER`/`SWAP`/`ROT` or locals instead of a
counted shuffle.

User-level: `: ABSV ( i64 -- i64 ) DUP 0< IF NEGATE THEN ;` — the surface form
omits the leading `R` and the return clause; the checker supplies fresh rows.

## Control flow (modeled by the checker, `src/core/checker.f`)

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
: REQUIRE-NONEMPTY ( len -- ) LEN>N 0 <= if E-A-EMPTY throw then ;
: HEAD ( ptr i64 len -- i64 ) REQUIRE-NONEMPTY @ ;
```

Note the `LEN>N`. `<=` is declared over `n`, and `len` is a nominal role that
does not unify with `n` in either direction, so a guard on a role-typed value
must cross the boundary through the role's explicit converter. Writing
`dup 0 <=` here instead rejects with `expected: a n n actual: len len n` — the
role discipline described above applies to the guard just like anywhere else.

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
  persistence), but it **rejects a nominal-family or layout value**, while a
  numeric round-trip still certifies. This is the value-position mirror of the
  pointee-side `ptr family` seal. Nominal *role* atoms (`idx`/`len`/`label`/…
  and `DEFTYPE` names) and execution tokens stay admitted in raw storage for now,
  because the engine's own codegen keeps labels and xts in raw scratch cells;
  fencing those out as well needs that role/xt scratch migrated to typed cells
  first (tracked follow-on).

  **The seal holds on every path, because it is applied where the cell is
  defined.** `here` is sealed by a baked primitive effect, so it has always held
  everywhere: `: N>ID2 ( n -- CAD-KIND:region ) here ! here @ ;` rejects with
  `expected: CAD-KIND:region<> actual: a` under a plain `bin/hb --load`. The
  defining words are now sealed the same way. Whenever the engine publishes a
  word that owns a cell of raw dictionary storage it registers that word's
  effect through `trust-raw` (`TRUST-RAW`, `src/core/checker.f`) instead of
  `trust`, and `TRUST-RAW` parses the effect in raw-definer mode so every type
  variable in it is minted `TVK-RAW`. That covers all three publication sites in
  `src/habu/habu2.f`: `-- ptr a` for `create` and `variable`
  (`C-CALL-TRUST-LASTC-PTR-A`), `-- a` for `constant` (`C-CALL-TRUST-LASTC-A`),
  and the `does>`-declared created-word effect (`C-CALL-TRUST-LASTC`), which is
  what seals `PTR-VARIABLE` and every user-written `create ... does>` definer.
  So `: N>ID ( n -- CAD-KIND:region ) V ! V @ ;` over a `variable V` rejects
  under `bin/hb --load` with `expected: CAD-KIND:region<> actual: a`, and so
  does the same forge through `create`, `constant`, or a definer whose `does>`
  clause declares a free type variable such as `( -- a )`.

  The point of moving the seal into the registration word is that it can no
  longer depend on which front end ran. It used to be the caller's job: the
  shared source pre-verifier bracketed its own registration with the raw mode
  (`RAW-TRUST-NEXT`, `src/habu/verify-source.f`), and the native `--load` path,
  which is the path every tool and gate actually uses, published created words
  unsealed. `RAW-TRUST-NEXT` still brackets its registration, so it now confirms
  the seal a second time rather than being the only thing that applies it.
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
- **`PICK`, `ROLL`, and `?DUP` are not defined in `bin/hb` at all**, so the
  question of typing them does not arise in checked source: naming one is
  `E-UNDEFINED`. The literal-argument folding described here
  (`0 PICK`≡`DUP`, `1 ROLL`≡`SWAP`, and so on) belongs to the Gforth bootstrap
  engine in `bootstrap/src/pickroll.fs`. Write the shuffle you mean with
  `DUP`/`OVER`/`SWAP`/`ROT`, or name the values with `{: :}` locals.

### Rigid host-allocation identity domains

A host allocation needs identities that `ptr T` and ordinary type vars cannot
name: two equal-sized containers, or a recreated owner, share a type and would
unify, so a stale index could regain authority over reused storage. Three
checker domains name these rigidly (`src/core/checker.f`, dot
`habu-define-rigid-host`):

- **host region** — *which* allocation. A constructor whose output spells
  `fresh-region-*` mints a fresh, rigid region identity **per call**, shared
  across that one call's outputs. Two allocations (or a recreated owner) get
  distinct regions, so they never unify even at equal extent.
- **extent** — the bounds identity, spelled `fresh-extent-*` (the incumbent
  device-span extent, now a first-class domain).
- **mutation generation** — the epoch, spelled `fresh-gen-*`. A mutation that
  invalidates outstanding indices produces a value at a *new* `fresh-gen-*`, so
  an index typed for the old generation no longer matches the container.

Each domain has a **private, per-check counter** with **monotonic non-reuse**
and **exhaustion before wrap**: a counter that reaches `RIGID-MAX` throws
`E-RIGID-EXHAUST` rather than wrapping and re-granting a live id. The numeric id
is never itself the authority — `ATOM-OK?` qualifies it by domain, so a region
id and a generation id that are numerically equal (each the first mint of its
domain) do **not** unify. A mismatch rejects with a named, distinguishable
reason so a stale index, a wrong region, and a wrong extent read differently:
`E-RIGID-REGION-MISMATCH` (`fix_host_region`), `E-RIGID-EXTENT-MISMATCH`
(`fix_host_extent`), `E-RIGID-STALE-GENERATION` (`fix_stale_generation`), and
`E-RIGID-DOMAIN-CONFUSION` (`fix_rigid_domain`). Identities are per-check
template atoms, so they carry into the snapshot, native, and bootstrap checker
paths with the rest of `checker.f`; no runtime counter, address, or
allocation-order number ever becomes persistent authority. Fixtures:
`test/rigid-region-suite.f`.

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

Axiom-set size is reported by the census, which prints the live `PES` row count
(`prim-axiom: N axioms (D difftested, X noexec)`).

### ARM64 contract link (`PRIM-LINK`, `src/core/checker.f`)

A typed ARM64 routine effect schema describes generated register state, but each
callable it lowers ultimately bottoms out in a checked primitive whose stack
effect is one PES axiom row. Binding an emitted primitive/callable contract to
that row must not depend on mutable symbol-interning order or on walking the whole
trust-owner lifecycle: the link needs a **stable key** and a way to detect a row
that has since drifted. `PRIM-LINK` is that package-scoped, read-only query (no
global prefix word; it never mutates a row):

- `PRIM-LINK:COUNT ( pkg-a pkg-u name-a name-u -- n )` — active PES rows sharing
  the key. The key is a row's stable identity coordinates: its **defining package**
  (empty for a bare `PRIM:`) and **word spelling**, interned exactly as `PRIM:` /
  `PPRIM:` intern their symbols.
- `PRIM-LINK:RESOLVE ( … -- bool )` — true iff the key resolves to *exactly one*
  active row; it latches that row and its effect through the shared effect-read
  query state, so a consumer then reads the linked row's arity/family with the same
  `EFFECT-DIN-N` / `EFFECT-DOUT-FAM` readers above.
- `PRIM-LINK:FP ( -- fp )` — the resolved row's identity **fingerprint**: its
  declared din/dout arity, the per-slot `EFAM-*` family of every din then dout
  term, and the `PE-TRUSTED-ONLY` flag, bit-packed (marker-led) into one cell,
  exact while `din + dout <= 24` and fail-closed above. It is a *shape* identity —
  two rows of identical shape share one — so it is always combined with the key.
- `PRIM-LINK:CHECK ( … expect-fp -- bool )` — a sound link: the key resolves to one
  row whose fingerprint equals the contract's recorded `expect-fp`.

Every rejection the acceptance names falls out of this. An **unknown primitive** or
the **wrong package** interns to no live row's symbol (`COUNT 0`). A **duplicate
spelling** — an overloaded prim like `+`, or the `path0`/`PATH0` pair — has no
single immutable row (`COUNT > 1`), so the link is ambiguous and rejected rather
than silently binding the first match. A **row mutation** flips the fingerprint, so
a contract carrying the pre-mutation `expect-fp` fails `CHECK`, making a recorded
link a staleness ratchet against axiom drift. Regression: `test/prim-link-test.f`.

## Typed depth introspection

Stack-snapshot assertions historically could not be typed: `T{ code -> expected
}T` captured an arbitrary-length stack tail whose size is a runtime `depth`
value, so `T{`/`->`/`}T` were trusted words with no checked contract on the
asserted computation or its shape. That migration has landed — those three words
no longer exist in `bin/hb`, and naming one is `E-UNDEFINED`.

The checked replacement expresses the actual and expected computations as two
quotations that must leave the **same row shape**. It lives in
`lib/test/snap.f`, so a file that uses it must `require lib/test/snap.f` first:

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
cells through the same judge path the old `T{ }T` used. Only the depth-marked
drain of each quotation's output row stays trusted, so the comparator adds no new
drain primitive while making every asserted computation and its shape checkable.

Every snapshot assertion is therefore shape-checked rather than runtime-only
(the migration was tracked as habu-shared-t-t-470833e6).

## Notes

- **Turning checking off and on again.** `0 set-check` disables the check hook
  for the rest of the load, so `:` becomes the plain native colon — this is how
  infrastructure that isn't checkable habu gets loaded. Re-enable it by
  reinstalling the hook with `LOWER-CERT-HOOK:INSTALL` (`src/core/check-hook.f`).
  There is no `CHECKING-ON?` word; naming it is `E-UNDEFINED`.
- A body that reaches a word the checker cannot model (`evaluate` and the other
  metaprogramming words) fails the definition; the structured verdict is
  `E-UNCHECKABLE`. Checked build paths must treat that as a refusal unless the
  call is behind a named, tested `TRUSTED:` boundary. A genuine type error also
  refuses the definition.
- There is no `EFFECT-OF` word in `bin/hb`; it belongs to the Gforth bootstrap
  engine (`bootstrap/src/db.fs`) and naming it in checked source is
  `E-UNDEFINED`.

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
