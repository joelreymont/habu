# habu stack-effect syntax

A checked definition declares its effect in the `( … )` immediately after the
name: `: SQUARE ( i64 -- i64 ) DUP * ;`. The checker infers the body's effect and
unifies it with the declaration; a mismatch refuses the definition. This is the
notation, parsed by `PARSE-SIG` (`src/sigparse.fs`).

## Grammar

```
sig    = stack '--' stack ( '|' stack '--' stack )?
stack  = rowvar? type*
type   = conname | tyvar | 'ptr' type | '[' stack '--' stack ']'
conname= i64 u8 u32 cell bool char str addr
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
| `ptr<τ>` written `ptr τ` | typed pointer; `@`/`!` move `τ` |
| `[ S -- S' ]` | a quotation / `xt` carrying its own effect |

## Examples (from `src/prims.fs`)

```
DUP    ( R a -- R a a )            \ row-polymorphic: any one value, duplicated
SWAP   ( R a b -- R b a )
+      ( R i64 i64 -- R i64 )
<      ( R i64 i64 -- R bool )     \ comparisons yield bool, not i64
@      ( R ptr a -- R a )
WITHIN ( R i64 i64 i64 -- R bool )
>R     ( R a -- R | S -- S a )     \ moves a value data→return stack
R>     ( R -- R a | S a -- S )
EXECUTE( R [ R -- S ] -- S )       \ run a quotation
DIP    ( R a [ R -- S ] -- S a )   \ run a quotation under the top item
TIMES  ( R i64 [ R -- R ] -- R )   \ counted iterate (quotation effect-neutral)
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

- `TRUSTED: NAME ( eff ) body ;` — chart `eff` under `NAME` **without** checking
  the body, then compile it normally. For metaprogramming words (`evaluate`,
  parsing, dictionary ops, raw memory) the checker can't follow.
- To chart an **already-defined** word's effect (so the checker can use it as a
  leaf) without redefining it: `eff-str name-str CHART` after `PARSE-SIG`. habu's
  own codegen dogfoods this — see `bootstrap/cg/asm.fs`'s `CHART-EFF`.
- **Literal-argument `PICK`/`ROLL` are folded** to a concrete shuffle at check
  time: `0 PICK`≡`DUP`, `1 PICK`≡`OVER`, `2 PICK ( a b c -- a b c a )`;
  `1 ROLL`≡`SWAP`, `2 ROLL`≡`ROT`. A **dynamic** (runtime-computed) index can't be
  folded and stays untypeable → `E-UNCHECKED` (native fallback) or behind
  `TRUSTED:`. See `src/pickroll.fs`.
- Words the checker can't type (variadic `?DUP`, dynamic `PICK`/`ROLL`/`DEPTH`)
  must stay outside checked code or behind `TRUSTED:`.

## Notes

- `CHECKING-ON?` toggles the override; with it off, `:` is the plain native colon
  (used to load infrastructure that isn't checkable habu).
- A body using a word with no charted effect raises `E-UNCHECKED` and falls back
  to the native colon (a warning, not a refusal) — so unmodeled code still
  compiles, it just isn't verified. A genuine type error **refuses** the def.
- `EFFECT-OF ( a u -- ea eu | 0 )` returns the canonical effect string for a
  charted name, or a single `0` if absent (note the asymmetric stack effect).
