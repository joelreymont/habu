# habu stack-effect syntax

A checked definition declares its effect in the `( … )` immediately after the
name: `: SQUARE ( i64 -- i64 ) DUP * ;`. The checker infers the body's effect and
unifies it with the declaration; a mismatch refuses the definition. This is the
notation, parsed by `PARSE-SIG` (`src/sigparse.fs`).

## Grammar

```
sig    = stack '--' stack ( '|' stack '--' stack )?
stack  = rowvar? type*
type   = conname | role | tyvar | 'ptr' type | '[' stack '--' stack ']'
conname= i64 u8 u32 cell bool char str addr
role   = idx len count off fd rc pid ms ns tok
tyvar  = a..z          (same letter → same type var, per signature)
rowvar = A..Z          (same letter → same row var; leading = the stack tail)
```

- The part before `|` is the **data** stack; the optional part after `|` is the
  **return** stack. Four rows total: `( Din Rin -- Dout Rout )`.
- A **row var** (`R`, `S`, …) at the front of a stack stands for "the rest of the
  stack below" — row polymorphism. Stacks with no leading row var share one
  implicit data row (and one implicit return row).
- A **type var** (`a`, `b`, …) is a fresh polymorphic type; reusing the same
  letter in one signature means the same type.
- Whitespace-delimited. Don't nest `( )` (the inner `)` closes the comment).

## Types

| type | meaning |
| ---- | ------- |
| `i64 u8 u32 cell` | integers of given width (`cell` = machine word) |
| `bool` | a flag (distinct from `i64` — comparisons return `bool`) |
| `char str addr` | character, string body (`c-addr u` as one value), raw address |
| `idx len count off fd rc pid ms ns tok` | nominal scalar roles; distinct from each other and from plain `n` |
| `ptr<τ>` written `ptr τ` | typed pointer; `@`/`!` move `τ` |
| `[ S -- S' ]` | a quotation / `xt` carrying its own effect |

Nominal roles are for same-representation values whose meanings must not mix:
array indexes vs lengths, file descriptors vs return codes, elapsed milliseconds
vs nanoseconds, and token indexes vs counts. They are fail-closed concrete types:
`idx` does not unify with `len`, and neither unifies with a plain `n`. Introduce
or remove a role only through an explicit checked constructor/coercion word or an
audited boundary effect; do not rely on generic integer operations to launder a
role.

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

## Escape hatches

- `TRUSTED: NAME ( eff ) body ;` — record `eff` for `NAME` **without** checking
  the body, then compile it normally. For metaprogramming words (`evaluate`,
  parsing, dictionary ops, raw memory) the checker can't follow.
- To chart an **already-defined** word's effect (so the checker can use it as a
  leaf) without redefining it in native habu: `s" name" s" eff" TRUST`.
- Trusted definers use `TRUSTED: NAME ( definer-eff ) CREATES
  ( created-eff ) body ;`. `definer-eff` is the effect of invoking the defining
  word itself; `created-eff` is recorded for each word produced by runtime
  `CREATE` while that definer runs. If a trusted definer contains `DOES>`, it must
  declare `CREATES`.
- For `CREATE...DOES>`, if `created-eff` is `( in -- out )`, the native checker
  verifies the `DOES>` body as `( in ptr a -- out )`: the created word pushes its
  data-field pointer before entering the `DOES>` body. Use typed pointer steps
  such as `cell+`, not raw integer `+`, when moving through that data field.
- **Literal-argument `PICK`/`ROLL` are folded** to a concrete shuffle at check
  time: `0 PICK`≡`DUP`, `1 PICK`≡`OVER`, `2 PICK ( a b c -- a b c a )`;
  `1 ROLL`≡`SWAP`, `2 ROLL`≡`ROT`. A **dynamic** (runtime-computed) index can't be
  folded and stays untypeable → `E-UNCHECKED` (native fallback) or behind
  `TRUSTED:`. See `src/pickroll.fs`.
- Words the checker can't type (variadic `?DUP`, dynamic `PICK`/`ROLL`)
  must stay outside checked code or behind `TRUSTED:`.

## Notes

- `CHECKING-ON?` toggles the override; with it off, `:` is the plain native colon
  (used to load infrastructure that isn't checkable habu).
- A body using a word with no charted effect raises `E-UNCHECKED` and falls back
  to the native colon (a warning, not a refusal) — so unmodeled code still
  compiles, it just isn't verified. A genuine type error **refuses** the def.
- `EFFECT-OF ( a u -- ea eu | 0 )` returns the canonical effect string for a
  charted name, or a single `0` if absent (note the asymmetric stack effect).
