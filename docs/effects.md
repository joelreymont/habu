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

`DEFLINEAR name` declares a nominal noncopyable cell type. Ordinary checked code
may pass a linear value through unchanged, but generic copying, dropping, memory
store/load, and value-record duplication reject unless a called word explicitly
declares the linear type in its own trusted or checked effect. Use this for owner
or lifetime tokens around arena-backed records.

`VALUE-RECORD name field type ... END-VALUE-RECORD` declares a by-value record
token for signatures. The token expands to hidden field types, so
`( n n -- point )` and `( point -- n n )` can be certified with empty runtime
bodies, while `( point -- rect )` rejects even if both records have the same
cell shape. Record fields may be polymorphic or parametric signature types;
accessors, updaters, copies, and destructors are normal checked words over the
expanded stack cells.

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
  semantic role changes explicit to the checker.
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
- **Literal-argument `PICK`/`ROLL` are folded** to a concrete shuffle at check
  time: `0 PICK`≡`DUP`, `1 PICK`≡`OVER`, `2 PICK ( a b c -- a b c a )`;
  `1 ROLL`≡`SWAP`, `2 ROLL`≡`ROT`. A **dynamic** (runtime-computed) index can't be
  folded and stays untypeable; keep it outside checked code or behind a named,
  tested `TRUSTED:` boundary. See `src/pickroll.fs`.
- Words the checker can't type (variadic `?DUP`, dynamic `PICK`/`ROLL`)
  must stay outside checked code or behind `TRUSTED:`.

## Notes

- `CHECKING-ON?` toggles the override; with it off, `:` is the plain native colon
  (used to load infrastructure that isn't checkable habu).
- A body using a word with no charted effect raises `E-UNCHECKED`; checked
  build paths must treat that as a refusal unless the call is behind a named,
  tested `TRUSTED:` boundary. A genuine type error also refuses the definition.
- `EFFECT-OF ( a u -- ea eu | 0 )` returns the canonical effect string for a
  charted name, or a single `0` if absent (note the asymmetric stack effect).
