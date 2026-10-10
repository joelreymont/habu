# Habu in one card

Read this instead of [forth.md](forth.md). Every refusal below was measured
on `bin/hb`; every other claim condenses the forth.md
heading named at the end of its section; section 9 says which heading to open
when this runs out.

## 1 Names and packages

Our words UPPER-CASE, built-ins as-is (`dup`, `?do`). Hyphens, never
underscores, in word *and* file names. Predicates end `?`, conversions are
`>X`, fetch/store `X@` / `X!`, scope pairs are `FOO … ;FOO`. `$hex` for
machine-adjacent numbers. Every module is a package.

```forth
package HB
: HELPER ( n -- n ) 2 * ;        \ private: visible only while HB is open
public
: COUNT ( -- n ) 5 HELPER ;      \ callers write HB:COUNT
;package
```

- A qualified name is one non-edge colon, `HB:COUNT`, and selects a package's
  **public** wordlist: `HB:HELPER` is `E-UNDEFINED`, and so is a bare `COUNT`
  outside HB. Match the case of qualifier and tail. Qualify only *across*
  boundaries; in HB's own files reopen `package HB` and call bare.
- **A qualified name can never land in a private wordlist.** `: Q:Y ( -- n ) 5 ;`
  inside `package P private` publishes `Y` into wordlist `Q`, where `Q:Y`
  resolves it while `P:Y` stays `E-UNDEFINED`. Define into a package
  by being in it, never by qualifying.
- `using NAME … ;using` imports NAME's publics for bare calls. A bare tail a
  global also owns is `E-USING-SHADOW-GLOBAL` (rc 70) in a definition and
  `ENGINE-ERROR:USING-SHADOW-GLOBAL` (rc 105) at top level or after `'` —
  rename the public. A file loaded under a `using` resolves through it.
  Close a using opened before `package` after `;package`: a `;using` inside
  the package that would close it is `ENGINE-ERROR:USING-OUTER` (rc 104).
  A load file is a using scope too: a `;using` in an included file or
  `evaluate` buffer that would close its includer's using is rc 104 as well.
- `EXPORT NAME` in a public section re-exports an existing word under its own
  tail: same xt, same effect, no body.
- A wordlist is a no-duplicate set, case-insensitively: a second `: R` is
  `E-DUPLICATE-DEFINITION` (rc 78); `undefine R` first to replace one. A
  `does>` definer `R` also claims `R;does` there, and so does `EXPORT R`. The
  new `R` is checked and called as itself even when it spells an engine word:
  the checker's rules for `@`, `!`, `create` and the like follow the engine's
  `INTRINSIC` tag, which a redefinition does not carry.
- A call binds a word the engine holds. A name only the checker knows - a row
  `CHECK!` alone recorded, a word `VERIFY:SOURCE-BUF-IN-SCOPE` scanned - is
  `E-UNDEFINED` in a body. Scanned names bind while that scope is open: inside
  one `CHECKER-SCOPE-START-NEUTRAL` … `CHECKER-SCOPE-DONE` pair each row the
  checker records is a codeless engine record, so there
  `VERIFY:CANDIDATE-IN-SCOPE` (`require src/habu/verify-source.f`) certifies a
  candidate calling one (-1); after the pair closes it answers 1. A
  definition, a new `package` or a `wordlist` made inside the pair dies where
  it is made, naming it, and so does an `undefine` run there, before it
  retires anything: `ENGINE-ERROR:OVERLAY-OPEN` (rc 108).
- Compiler keywords (`I`, `DO`, `IF`, …) cannot be definition names:
  `E-RESERVED-DEFINITION`. Nor can the target predicates `HB-TARGET-LINUX?`,
  `HB-TARGET-MACOS?` and `HB-TARGET-LINUX-X86-64?`, by any definer the lint
  reads (`:`, `TRUSTED:`, `defer`, `constant`, …), since `tools/check.f` reads
  their spelling to skip the target arms the engine never runs; it refuses
  them, not `--load`. A number-shaped name (`: 42`) is refused by
  `tools/check.f` (`E-NUMERIC-DEFINITION`), not by `--load`.

forth.md: **Naming**, **Packages**, **Importing … with `using`**, **Rules
learned by refusal**.

## 2 Effects, locals, quotations

Every definition carries `( before -- after )` and the checker reads it as the
signature. Type tokens only (`n`, `u8`, `bool`, `xt`, `ptr a`, `ptr u8`, `idx`,
`len`, `fd`, `rc`), never role prose like `( got want -- )`.

- A string is `ptr u8 n`, a cell address `ptr a`, `n` only a genuine scalar.
  `ptr a` needs a body that keeps the pointee parametric — a `ptr` local admits
  `@` and `!`, but reading the cell specialises `a`; read it as a number and
  declare `( ptr n -- n )`. `ptr u8` is a byte span — `c@`/`c!`; cell `@` on
  it is `E-MISMATCH`.

  ```forth
  : PEEK-A ( ptr a -- n )    \ E-NONPARAMETRIC-EFFECT: a is specialised
     {: p:ptr :}
     p @ ;

  : PEEK-N ( ptr n -- n )    \ certifies
     {: p:ptr :}
     p @ ;
  ```
- Concrete integers widen when lossless (`u8 → u16 → u32 → cell/i64`). Generic
  `n` accepts structural integer stack cells in either direction: `n` can pass
  to `u32`, while `i64` needs an explicit conversion. Pointer pointees stay
  exact; roles (`idx`, `len`, `fd`) never widen. Booleans are real `bool`s:
  `0 0=`, never a raw `0`/`-1`.
- Locals `{: a b:ptr :}` bind left to right from the deepest item, so
  `1 2 {: a:n b:n :}` gives `a`=1. A local binds **once**: a per-turn value
  lives on the stack or in a cell. Names are at most 16 bytes, 64 per
  definition, block-scoped, bindable after a closed early-exit guard.
- A `{: … :}` group that binds at entry goes on its own line after the line
  holding the name and stack effect, indented as the body: three spaces, as in
  `lib/` and `tools/`. New and changed definitions follow this; existing files
  are not reformatted for it.
- Bind multi-cell values whole: `{: p :}` or `{: r:res<n,n> :}` (arity checked).
  Destructure only to compute; pass the whole local between words.
- Quotations `[: … ;]` are xts, not closures. The token `[ in -- out ]` works as
  a parameter, a `TYPED-VARIABLE` or `TYPED-BUFFER` element, and a
  `STRUCTURE FIELD` value. A derived field accessor returns `ptr [ in -- out ]`.
  `: A ( n [ n -- n ] -- n ) execute ;` certifies and `2 [: 1 + ;] A` runs. A
  quotation may not touch an enclosing local (`E-BAD-LOCAL-SHAPE`, rc 75) —
  pass the value on the stack or through a cell.

forth.md: **Stack comments**, **Words & factoring**.

## 3 What the checker refuses, and what it admits

Refused; every row measured, code from `tools/check.f --json-errors`.

| you write | diagnostic |
|---|---|
| `evaluate` in a checked body (top level is fine) | `E-UNSAFE` — use `evaluate-closed` |
| `variable V  : F ( n -- n ) V ! V @ @ ;` | `E-RAW-CELL-PTR` — § 5 |
| `variable V  : F ( -- ) V @ execute ;` | `E-EXEC-OPAQUE-XT` |
| `@` on a `ptr u8` | `E-MISMATCH` — use `c@` |
| an `if` arm or loop body that changes depth | `E-MISMATCH` at `then`/`repeat` |
| a local read or declared inside `[: … ;]` | `E-BAD-LOCAL-SHAPE`, rc 75 |
| a 33rd `[:` while 32 are open | `E-UNCHECKABLE`; `--load` exits 75, `hb: quotation nesting full at 32 levels` |
| a definition leaving 4097 cells, or returning a quotation that leaves 4095 | `E-UNCHECKABLE`, `effect too deep to record (depth 4097, at most 4096)` |
| a definition taking 256 cells, declared or inferred | `E-UNCHECKABLE`, `input row too wide to record (256 cells, at most 255)` |
| a `trust` row leaving 4097 cells, or one, `TRUSTED:` or `defer` taking 256 | `E-BAD-STORED-SIGNATURE`, `fix_signature_size`, the same reasons |
| a return-stack cell in a signature or quotation type: `( n \| -- \| n )`, `( R n \| S -- R )`, `[ n -- \| U -- U n ]`; only `\| U -- U` (one row variable, both sides) is admitted | `E-BAD-SIGNATURE`, `fix_return_stack`, at both tiers; `E-BAD-STORED-SIGNATURE` for `TRUSTED:`, `defer`, `trust`, `CAST:` — park a value with `>r … r>` inside one definition |
| a quotation literal that pushes a cell its caller pops, or pops one its caller pushed: `[: >r ;] execute r>`, `>r [: r> ;] execute` | `E-REJECTED` at `;]`, `fix_return_stack` |
| a definition with no signature that reads or pops a cell it did not push: `: X r@ ;`, `: X r> ;` | `E-REJECTED` at the token, `fix_return_stack`, at both tiers |
| a family, variant or field name over 255 bytes | `E-BAD-DECLARATION`, `name longer than 255 bytes`; `--load` exits 70 |
| `package` with a name over 255 bytes | `E-STATEMENT-THROW`, throw code 7154; `--load` exits 67 |
| `: IR-ID:X ( -- ) ;`, `variable IR-ID:V` or `package IR-ID`: a definition into, or the reopening of, a package the engine bakes | `E-STATEMENT-THROW`, throw code 84 at the name; `--load` exits 84 |
| a non-preserving `[: G ;] catch`, a read of what its throw left; `i`/`leave` outside a loop; `exit` in a loop, no `unloop` | `E-REJECTED`, `E-STALE-READ` |
| `exit` after a word ending in `die` | `E-DEAD-CODE` |
| `: I ( -- ) ;` | `E-RESERVED-DEFINITION` |
| a definer (`:`, `DEFTYPE`, `create`, `package`, …) or a parsing word (`char`, `'`, a field word) with nothing after it | `E-MISSING-NAME` from `tools/check.f`, rc 70 |
| `\` comment in a `STRUCTURE`/`ENUM` body | `E-BAD-DECLARATION`, rc 70 |
| `4 TYPED-BUFFER B no-such-type`: a type, name or literal count `TYPED-*`, `*LAYOUT-BUFFER` or `DYNAMIC-BUFFER` refuses, or a name or type not on the definer's line | `E-BAD-STORAGE`, rc 70 |
| `( -- ptr a )` for a `variable` | `E-NONPARAMETRIC-EFFECT` |
| a multi-cell value at the prompt | `hb: interpret-mode layout value: NAME` |
| a bare `using` import a global also names, in a body, at top level or after `'` | `E-USING-SHADOW-GLOBAL`, rc 70; at top level or after `'` `--load` exits 105 (`ENGINE-ERROR:USING-SHADOW-GLOBAL`) |
| a top-level word, or `'` of one, nothing defined before it, `1.` or `$GG` among them | `E-UNDEFINED-TOP-LEVEL` at the token, rc 70, before anything runs |
| `: W ( -- ) ;`, `create`, a new `package`, `wordlist` or `undefine W` inside a `CHECKER-SCOPE-START-NEUTRAL` pair | `ENGINE-ERROR:OVERLAY-OPEN`, rc 108 |
| a duplicate tail in one wordlist | `E-DUPLICATE-DEFINITION`, rc 78 |
| a word defined before the check hook with no external row (a `PRIM:` axiom, or a `TRUSTED:` declaration after `src/core/checker.f`) in a checked body (`REG-PROT-CAP`) | `E-UNDEFINED`, rc 70 — **on a sealed from-source prefix boot only**, never on `bin/hb`; `PATH-CAP`, `E-PATH-RANGE` and `SCOPE-FIND-AMBIGUOUS` have rows, another constant is read at top level: `REG-PROT-CAP constant MY-CAP`. An unsealed boot binds a signed `:` word of the prefix to its declaration |
| a body the scan refuses with the hook cell empty (`0 set-check`) at tier 1 | ordinary compilation uses its declaration, with the reason on stderr and no row authority; a native build requires the prefix scan and refuses before publication (forth.md **Rules learned by refusal**) |
| `CHECK!`, `generates:`, a type registration (`CHECKER-DEFLINEAR`, `CHECKER-DEFFAMILY`, a `TYPE-FIELD-OWNER` phase) or another checker scan or store write in a compile callback (`NBACK:OBSERVE!`, `NPUB:WITH-UNIT`) | `E-NCOMP-STATE` (-8570), catchable, before anything changes |

Checked C2 views carry owner and loan lifetimes: shared `read-view<p,q,T>` and
exclusive `mut-view<p,q,a,T>`. They occupy two cells but form one logical value.
`C2-MEM:WITH-MUT` allocates bytes for a callback; `WITH-READ` and
`WITH-MUT-LOAN` lend a child view and restore the original parent on return or
throw. A child cannot escape its callback. `C2-MEM:ALLOC` appends zeroed storage
to an explicit owner. Build that owner with `OWNER-SIZE` / `WITH-MUT`,
`SEED-OWNER` / `WITH-INIT`, then `BIND` and `UNBIND` inside the initialized
callback; [ownership-model.md](ownership-model.md#public-owner-recipe) has a
checked source example. `ALLOC-DISPOSE` also registers a callback that consumes
the new unique view at that owner's close, including when an inner owner is
active. `PUBLISH` consumes a mutable view to make it shared.
A view on either side of `CAST:` is `E-CAST-SCOPE`, except a view representation
cast: only C2-MEM's private section packs `( ptr u8 n -- V )` or unpacks a
mut-view, and any private section may unpack `( read-view<p,q,T> -- ptr u8 n )`.
The cast takes an unqualified name; a qualified one is `E-CAST-SCOPE`.
Ordinary raw pointers remain lifetime-free.
The task-local frame capacity is 32; the 33rd live open throws `E-C2-CAPACITY`
(`-9360`) before acquisition. Live owner or loan image capture throws
`E-C2-CAPTURE` (`-9364`); capture after close succeeds.

`C2-MEM:WITH-INIT` admits a live unique byte view and a complete, non-owning
fixed-cell record. Its callback uses `forall<i inside [l,T],[ ... ]>`: the fresh
initialization scope is inside both the parent scope `l` and the scope
dependencies of `T`. The callback receives `mut-view<p,i,a,init<i,T>>`; its
result cannot let `i` escape. The original byte view returns after the record
is cleared. See forth.md **Rules learned by refusal** and
[ownership-model.md](ownership-model.md) **Typed storage and checker authority**.

`C2-MEM:WITH-RECORDS` takes a unique byte view, a nonnegative count, and a
committed `DERIVE init` fixed-cell product seed. One scope owns `count` copies
and clears the complete extent when its callback returns. The callback carries
an opaque linear `records<p,i,a,T>`; `C2-MEM:WITH-RECORD` loans element `n`
as `mut-view<p,j,a,init<i,T>>` with `j inside i`, then restores the table.
Indices are zero based, and an empty table is valid.

`C2-MEM:WITH-FIELD name` lends a committed initialized record field from a
unique `mut-view<p,i,a,init<i,T>>`. The bare `name` is a compile-time selector
resolved against `T`; it is not a runtime value. The callback receives the
field as `mut-view<p,j,field(a,name),init<i,F>>` with fresh `j inside i`, and
the exact parent view returns. The field keeps initialization lifetime `i`;
neither the child lifetime nor its projected region can escape.

A caught result becomes readable on the success arm of core `code 0=` (or the
false arm of core `code 0<>`) only when `code` is that catch's own status.
Another zero or a shadowed predicate proves nothing. The value remains marked
as replaced for a later outer catch; quotations retain that mark on their
explicit outputs, and loops reject a changed carried proof or mark.

These classic words are absent — naming one is `E-UNDEFINED`.

| absent | instead |
|---|---|
| `pick`, `roll` | a `{: … :}` group, or factor a helper |
| `?dup` | `if … else … then` on an explicit test |
| `within` | `{: v lo hi :} v lo >= v hi < and` |
| `move` | `BYTE-COPY`, `src/core/bytes.f` |
| `fill`, `erase` | none; write it: `u 0 ?do 0 a i + c! loop` |
| `bl` | `STR-SPACE`, `lib/string.f` |
| `*/` | none; `*` then `/` |
| `s>number?` | `STR>NUMBER?`, `lib/string.f` |
| `'` in a compiled body | `[: WORD ;]`; `'` is top level only |

A top-level word that parses or is a `defer` is `W-CHECK-DEFERRED` at the word
under `--verify-only`, verdict `deferred`, exit 0 unless something is refused,
and the check discovers nothing after it: no definition, package, load or use
in the rest of the file or of the file that loads it. State what such a word
reads, after its definition, and the check goes on after its operands:

```forth
parses: PN 1                        \ PN reads one token
parses-through: BLK 0 ( ;BLK )      \ BLK reads through the first ;BLK
parses-through: SUITE 1 ( ;SUITE TEST:;SUITE )
```

The count is of raw blank-delimited tokens, as `parse-name` reads them; the
through form then reads through the first token that equals a listed one,
byte for byte, inclusive (`)` cannot be listed). A `:`, a definer, a comment or
string opener among the operands is data. The word is still
`W-CHECK-DEFERRED`: the row is trusted, as `parse-imm`'s is, never compared with
the body. Only the check reads it, from the source it checks, so a word whose
row it did not read, a resident or precompiled one among them, is undeclared;
`EXPORT` carries a row, a word that calls the declared one does not.

A word that looks its string operand up as a name states it after its
definition, and a string literal right before a call of it is then a use of
the word between its quotes, which references list:

```forth
names: LOOK                         \ LOOK finds ( ptr u8 n ) as XREF-FIND does
: SEEK ( -- ) s" ROOK" LOOK ;       \ ROOK between the quotes is a use of ROOK
```

The engine states it for `XREF-FIND` and `XREF-FIND-INDEX`. The name resolves
as `XREF-FIND` resolves it: `PKG:TAIL` a public word, a bare name the global
one. A computed operand, a literal a caller passes on and a literal with an
escape in it bind nothing. The row is trusted, never compared with the body;
`EXPORT` carries it, a word that calls the declared one declares its own. A
row with no target, naming no word or naming a word whose input row does not
end in the string `( ptr u8 n )`, as `names: dup` does, is `E-NAMES-ROW`.

A word that renders definitions at load time (`FUNCTION:`/`;FUNCTION`,
`CMD:COMMAND`, `TASK:+USER`, anything reaching `INCLUDE-EVALUATE`) makes names
`tools/check.f` leaves to its run, which type-checks their uses there, unless
the source declares them: uses of a `FUNCTION:` word and of a `generates:` row's
word (§ 4) are checked before the run, `--verify-only` included. At top level
any other such name opens that stretch. The renderer reads none of the tokens
after it, so they are checked. A reached renderer no row or group declares is
`W-CHECK-DEFERRED` at its own token, so a `deferred` verdict always locates its
gap.

After a reached call that may render or register nominal types, quiet
verification defers a later use of a missing type with `W-CHECK-DEFERRED` at
that type. It consumes the declaration without publishing a type, layout,
signature or generated words. Known syntax, visibility, arity and count errors
still refuse. Defining a provider, taking its tick or reading an uncalled body
does not create this uncertainty; ordinary loading keeps its existing rules.

A trusted-only tick can outrank closing body checks at tier 0 only while the
verifier knows the compiler tier and checker owner. A resident immediate in a
body or a top-level loader makes later ordering uncertain; `--verify-only`
reports the tick as `W-CHECK-DEFERRED` and leaves dependent source to the one
subject run. Even an original `require`/`include` entry can reach replaceable
source providers, so its spelling or original entry does not preserve tier.

Admitted and measured, the ones worth doubting: `tuck`, `+!`, `unloop exit`,
`>r r@ r> 2>r 2r>`, `RECURSE`, `['] W catch`, `finally`, `defer W ( n -- n )`
plus `[: IMPL ;] is W`, `parse-name`, `MATCH … ;MATCH`, `undefine`, and
`true false 0<> fdup` with no require. An `endcase` default arm producing a
value must leave the selector on top (`30 swap endcase`). `do` always takes its
first turn: `0 0 do … loop` and `-1 0 do … loop` run once. `?do … loop` enters
only while start < limit, signed, so `0 0`, `-1 0` and `MIN-N 0` run zero times
and `u 0 ?do` runs max(u,0); `?do … +loop` skips only equal bounds, so
`0 10 ?do … -1 +loop` counts down eleven turns. `evaluate-closed ( ptr u8 n -- )`
evaluates source in a body: the text's `depth` starts at 0, a token reaching
below it throws 70 and a text that leaves cells throws `E-EVAL-RESIDUE`, the
caller's cells intact either way. An xt the text runs cannot reach them: its
reach under the floor throws 70 too. A text that ends inside a definition it
opened is refused as every source is,
`hb: source ended inside definition: NAME`, rc 74, and the definition is rolled
back (forth.md **Checked code and primitive boundaries** lists the open cases).

forth.md: **Checker & type model**, **Native Forth Gotchas …**.

## 4 Storage definers

Every form loaded and its accessor effect certified.

| form | for | accessor |
|---|---|---|
| `variable V` | a raw scalar, role or atom cell | `V @` / `V !`; `( -- ptr a )` is not declarable |
| `create B 256 allot` | static dictionary storage | `B`, pointee bound by the use (`B 4 type`, `B @`) |
| `n BUFFER: B` | a fixed zeroed byte row | `( -- ptr u8 )`; bytes are never a `TYPED-BUFFER` element |
| `PTR-VARIABLE P` | a global slot holding an address | `( -- ptr ptr a )`; callers declare the pointee: `: F ( -- ptr u8 ) P @ ;` |
| `TYPED-VARIABLE V t` | one typed cell or record | `( -- ptr t )` |
| `TYPED-VARIABLE V ptr t` | a cell holding an address of `t` | `( -- ptr ptr t )` |
| `n TYPED-BUFFER TB t` | a fixed array of `t` | `( n -- ptr t )` |
| `n PTR-U8-TABLE TT` | a fixed table of byte pointers | `( -- ptr ptr u8 )`, indexed with `ptr-field` |
| `DYNAMIC-BUFFER DB t` | a growable mapped array, `u8` a byte row | `( n -- ptr t )` plus `DB-RESERVE` / `DB-RELEASE`; growth moves it — keep indices, reacquire ptrs, release before an image save |
| `n LAYOUT-BUFFER LB fam` | capacity for a declared family | `( n -- ptr fam )` |
| `STRUCTURE p 0 FIELD x n … ;STRUCTURE` | a by-value record; a 34-cell nested native roundtrip is tested | `P:MAKE` / `P:UNMAKE`; under `package PKG` the tail is `PKG-P:MAKE`, hyphens doubled; `STRUCTURE p 0 OPAQUE …` keeps `PKG:p` public and makes the pair PKG's private `P-MAKE` / `P-UNMAKE` |

A definer that writes its word as text (`+USER`, `COMMAND`) states what it makes
with `generates: D ( effect )` after D's definition, so `tools/check.f` checks
that word's uses before the run; what no row states, such as `COMMAND`'s
`NAME#VEC`, is left to the run (§ 3). `FUNCTION:` needs no row.

`PERSISTED-PTR-VARIABLE` and `PERSISTED-PTR-U8-TABLE-VARIABLE` are the
snapshot-marked siblings, the table one `ptr` deeper. Runtime-sized buffers come
from `lib/memory.f` (`MEM:ALLOC-BYTES`). `ptr-field` takes a **cell** index, not
a byte offset.

forth.md: **Structures And Enums**, **Habu Native Tooling Gotchas**.

## 5 A cell that holds an address is declared

`variable`, `create`, `constant`, `here` and every `create … does>` definer
publish raw storage: scalars, roles and atoms only. A pointer in either
direction, an execution token, and `ptr-field` over such a base, is refused.

```forth
variable V                    \ E-RAW-CELL-PTR at the fetch,
: PEEK ( n -- n ) V ! V @ @ ; \ repair class declare_pointer_cell

PTR-VARIABLE V                \ the declared twin certifies
: PEEK ( -- ptr u8 ) V @ ;
```

The declared forms are the `PTR-*` and `TYPED-*` rows of § 4. Such a cell takes
the address it was declared for and refuses the address *of* one.

An integer becomes an address or an xt only through a private `CAST:`
(`CAST: >BYTES ( n -- ptr u8 )` in a package's private section, else
`E-CAST-MINT`); `NULL-PTR BYTE-VIEW -` is the address-to-integer distance.
A layout's fields count as the bare type: a cast into a layout whose `FIELD`
holds a pointer or a quotation is the same mint, and one whose field holds
another package's family is `E-CAST-OWNER` outside that package.

In checked code a `DEFLINEAR` token is minted and erased the same way, by the
package that declared the type: `LINEAR: MINT ( ptr n -- PKG:tok )` and `LINEAR: ERASE
( PKG:tok -- ptr n )` in its private section. Under `public` that is
`E-LINEAR-SCOPE`, as is a qualified `LINEAR:` name in the private section;
in any other package or for a top-level `DEFLINEAR`
`E-LINEAR-OWNER`, and a row that is not one token and one non-linear con (or
pointer to one) `E-LINEAR-PAYLOAD`. `CAST:` refuses a linear side
(`E-CAST-LINEAR`). Both identity declarers preserve the stack below their
operand: `LINEAR: MINT ( R n -- R PKG:tok )` is valid, but different tails
are `E-CAST-ARITY`.

forth.md: **Structures And Enums**; the rule is `docs/effects.md` "Raw storage
never holds an address".

## 6 Errors

- Library codes are named constants in `lib/errors.f`; checker throws also use
  positive codes above 255 in their owning source. Codes 0..255 serve as
  process exit statuses and may be shared. Every other code, negative or above
  255, has one `E-` name across the tree; a file that needs it under its own
  name reads the owner's constant (`E-OWNER constant E-LOCAL`). A library owns
  one inclusive block of about a hundred bounded by its own `E-X-FIRST` /
  `E-X-LAST` (arrays `-2000`, filesystem `-2100`, strings `-2200`, …) and
  reserves that whole range whether or not every code is minted;
  `tools/error-code-lint.f` reports a file minting inside another's.
- A block with codes in a package (`JR:E-SOURCE`) is minted, bounds and all,
  in the file that owns the package (`lib/json-read.f`), and `lib/errors.f`
  says where. The engine bakes `lib/errors.f`, and a package the engine bakes
  has one owning file: no source a product engine loads reopens it
  (`test/baked-owner.f`).
- A fallible word `throw`s a named code, never an out-of-band flag.
- `throw` is catchable and belongs to the checker's exception edge. `die`
  (`ptr u8 n n --`, a real message and exit code) ends the process and is
  no-return, so nothing may follow a call to a word that ends in one. `throw`
  for recoverable and interactive paths, `die` for build makers and CLI
  boundaries.
- Arithmetic is modular and never refuses, with one exception: `/`, `mod` and
  `/mod` throw catchable `E-DIV-ZERO` on a zero divisor; `MIN-N -1 /` wraps to
  `MIN-N`.
- `catch` only at explicit recovery boundaries — REPL and CLI wrappers, test
  assertions, stack-preserving adapters returning the code as data. No
  `catch drop`. A test asserts a refusal by its exact code:
  `[: WORD ;] E-FS-PATH TTHROWSQ` in a checked body, `' WORD TTHROWS` at top
  level.

forth.md: **Errors**, **Integer arithmetic**.

## 7 require and load order

- `require lib/string.f` loads once per image, keyed by canonical path;
  `include` replays. `s" path" required` / `included` are the string forms. A
  path resolves against the root that resolved the requiring file (the
  `--load` entry's directory for the entry), then the working directory, then
  the engine's source root (the working directory when it is a Habu tree, else
  the tree above the running `bin/hb`), so `require lib/…` names the tree
  root. A file found through the working directory keeps that root for its own
  requires: an overlay copy of one tree file, required by a tree file, loads
  beside the tree's copy and dies on the duplicate (exit 78;
  test/aot-capture-bound.f copies the requirer too). Every file requires its
  **own** dependencies.
- A loaded file is a closed program: its top level starts at `depth` 0, a
  token reaching its loader's cells throws 70, a file that ends with cells on
  the stack is `E-EVAL-RESIDUE`, and one that ends inside a definition it
  opened is refused there, rc 74, as every source is. A value crosses a load
  only as a word the file defines.
- At the top level of a stdin session or of a program file run as
  `bin/hb file.f`, `SOURCE-ROOT:CD <dir>` moves the first search root (a bare
  `CD` prints it), `PUSHPATH` / `POPPATH` save and restore it, all public in
  `SOURCE-ROOT`; inside a loaded file (`--load`, `require`) they are refused
  (exit 74): scope a root with `SOURCE-ROOT:WITH`.
- The engine provides `lib/prelude.f`, `errors.f`, `string.f`, `span.f`,
  `memory.f`, `num-types.f`, `num-arithmetic.f`, `image-lifecycle.f` and the
  `src/` files its boot prefix loads (`ENGINE-PROVIDES?`; `tools/check.f`
  refuses one with `E-ENGINE-PROVIDED`):
  their words resolve with no require and a `require` is a no-op. Write it
  anyway: a file states its dependencies.
- Multi-file packages reopen `package NAME` per file; reopening shares scope and
  loads nothing. An engine package is sealed: reopening one exits 84. Run tools
  as `bin/hb --load lib/a.f tool.f -- args`; `SCRIPT-ARGV$` starts after `--`.

forth.md: **Files**, **Packages**.

## 8 Tests

Follow the testing policy in [AGENTS.md](../AGENTS.md). Exercise a complete
feature through its real load/build path and retain a repeatable artifact.
See `test/aot-chain-capture-suite.f` for a producer, saved image, and consumer
flow. Use `require lib/test.f` for assertions and keep fixtures package-scoped.

`T=` / `T<>` scalars, `T$=` strings, `TTRUE` / `TFALSE` flags, `TTHROWSQ` /
`TTHROWS` throw codes. `TEST-EVAL:N` / `FLAG` / `RC` take one cell, a flag or
the throw code out of a source text. Register it as a `SUITE name … ;SUITE`
entry in `test/gate-stdlib-cases.f`; `bin/hb --load test/run.f` runs every
suite.

forth.md: **Testing**, **Verification before committing**.

## 9 Open forth.md at this heading when …

| heading | open it when |
|---|---|
| Checked code and primitive boundaries | `TRUST`, a new `PRIM:`, what `evaluate-closed` leaves open |
| Naming | a collision, reserved names |
| Packages | reopening, include vs require |
| Importing … with `using` | ambiguity, scope end, the 16 limit |
| Structures And Enums | `NEWTYPE`/`SUMTYPE`/`PRODUCT`/`ENUM`, `CAST:`, `LINEAR:` |
| Words & factoring | word size, argument limits, splits |
| Files | one concern per file, script argv |
| Stack comments | the token list, `DEFTYPE`, `DEFLINEAR` |
| Checker & type model | loop frames, higher-order effects, `defer` |
| Errors | the `ENGINE-ERROR` ABI, `die` divergence |
| Integer arithmetic | the wrapping contract, `MIN-N` |
| Engine limits … | 8000-byte body, 255-byte line, 28 `begin`, 32 `[:`, 255-byte family and package names, effect 4096 deep, input row 255 cells |
| Constants | hex versus decimal, `src/config.fs` |
| Testing | groups, hooks, runner rules |
| Diagnosing a checker miss | find the layer that is wrong |
| Verification before committing | what to rebuild and run |
| Comments & hygiene | comment style, debug prints |
| Habu Native Tooling Gotchas | debugger, spawn and env, snapshots |
| Native Forth Gotchas … | compile-only words, `case`, `parse-name` |
