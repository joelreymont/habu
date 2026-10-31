# Habu in one card

Read this instead of [forth.md](forth.md). Every refusal below was measured
on `bin/hb`; every other claim condenses the forth.md
heading named at the end of its section; section 10 says which heading to open
when this runs out.

## 1 Names and packages

Our words UPPER-CASE, built-ins as-is (`dup`, `?do`). Hyphens, never
underscores, in word *and* file names. Predicates end `?`, conversions are
`>X`, fetch/store `X@` / `X!`, scope pairs are `FOO … ;FOO`. `$hex` for
machine-adjacent numbers. Every module is a package, named for its subject,
never an operation on it: `JSON:WRITE-OBJECT-START`, not
`JSON-WRITE:OBJECT-START`; a subject's reading and writing words share it.

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
  global also owns is `E-USING-SHADOW-GLOBAL` in a definition and
  `ENGINE-ERROR:USING-SHADOW-GLOBAL` at top level or after `'` — rename the
  public. A file loaded under a `using` resolves through it. Close a using
  opened before `package` after `;package`: a `;using` inside the package that
  would close it is `ENGINE-ERROR:USING-OUTER`. A load file is a using scope
  too: a `;using` in an included file or `evaluate` buffer that would close its
  includer's using is refused the same way.
- `EXPORT NAME` in a public section re-exports an existing word under its own
  tail: same xt, same effect, no body.
- A wordlist is a no-duplicate set, case-insensitively: a second `: R` is
  `E-DUPLICATE-DEFINITION`; `undefine R` first to replace one. A `does>`
  definer `R` also claims `R;does` there, and so does `EXPORT R`. The new `R`
  is checked and called as itself even when it spells an engine word: the
  checker's rules for `@`, `!`, `create` and the like belong to the engine's
  own word, which a redefinition is not.
- A call binds a word the engine holds. A name only the checker knows, a row
  recorded or scanned without a definition, is `E-UNDEFINED` in a body, except
  that generated definitions inside one checker replay may call rows recorded
  earlier in it. A definition, a new `package` or a `wordlist` made while a
  checker replay is open dies where it is made, naming it, and so does an
  `undefine` run there, before it retires anything:
  `ENGINE-ERROR:OVERLAY-OPEN`.
- Compiler keywords (`I`, `DO`, `IF`, …) cannot be definition names:
  `E-RESERVED-DEFINITION`. Nor can the target predicates `HB-TARGET-LINUX?`,
  `HB-TARGET-MACOS?` and `HB-TARGET-LINUX-X86-64?`, by any definer (`:`,
  `TRUSTED:`, `defer`, `constant`, …): `tools/check.f` refuses them, not
  `--load`. A number-shaped name (`: 42`) is refused by
  `tools/check.f` (`E-NUMERIC-DEFINITION`), not by `--load`.

forth.md: **Naming**, **Packages**, **Importing … with `using`**, **Rules
learned by refusal**.

## 2 Effects, locals, quotations

Every definition carries `( before -- after )` and the checker reads it as the
signature. Type tokens only (`n`, `u8`, `bool`, `xt`, `ptr a`, `ptr u8`, `idx`,
`len`, `fd`, `rc`), never role prose like `( got want -- )`.
`xt` is a ticked engine primitive's token (`['] type`), which checked code
cannot execute: `E-EXEC-OPAQUE-XT`.

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
  `0 0=`, never a raw `0`/`-1`; a `bool` never passes as `n`, so convert it
  with `bool>n` (-1 or 0).
- Locals `{: a b:ptr :}` bind left to right from the deepest item, so
  `1 2 {: a:n b:n :}` gives `a`=1. A local binds **once**: a per-turn value
  lives on the stack or in a cell. Names are at most 16 bytes, 64 per
  definition, block-scoped, bindable after a closed early-exit guard.
- Keep related one-line definitions together. Separate multiline definitions
  with two blank lines; place each word's comment directly above it.
- A `{: … :}` group that binds at entry goes on its own line after the line
  holding the name and stack effect, indented as the body: three spaces, as in
  `src/lib/` and `tools/`. New and changed definitions follow this; existing files
  are not reformatted for it.
- Bind multi-cell values whole: `{: p :}` or `{: r:res<n,n> :}` (arity checked).
  Destructure only to compute; pass the whole local between words.
- Quotations `[: … ;]` are xts, not closures. The token `[ in -- out ]` works as
  a parameter, a `TYPED-VARIABLE` or `TYPED-BUFFER` element, and a
  `STRUCTURE FIELD` value. A derived field accessor returns `ptr [ in -- out ]`.
  `: A ( n [ n -- n ] -- n ) execute ;` certifies and `2 [: 1 + ;] A` runs. A
  quotation may not touch an enclosing local (`E-BAD-LOCAL-SHAPE`) —
  pass the value on the stack or through a cell.

forth.md: **Stack comments**, **Stack effects**, **Words & factoring**.

## 3 What the checker refuses, and what it admits

Refused; every row measured, code from `tools/check.f --json-errors`.
Top-level code is checked too: each token is judged against the live stack's
cells as the same token in a colon body. A checked step runs, as does one
deferred to the run; a refused or uncheckable one prints the body's diagnostic
before the word runs, and `--load` exits 70 (`evaluate-closed` throws 70).
`set-tier`'s row is global, so a program's first line may choose the tier
before any `require`.

| you write | diagnostic |
|---|---|
| `evaluate` in a checked body (top level is fine) | `E-UNSAFE` — use `evaluate-closed` |
| `variable V  : F ( n -- n ) V ! V @ @ ;` | `E-RAW-CELL-PTR` — § 5 |
| `variable V  : F ( -- ) V @ execute ;` | `E-EXEC-OPAQUE-XT` |
| `@` on a `ptr u8` | `E-MISMATCH` — use `c@` |
| an `if` arm or loop body that changes depth | `E-MISMATCH` at `then`/`repeat` |
| a local read or declared inside `[: … ;]` | `E-BAD-LOCAL-SHAPE` |
| a `[:` past the quotation nesting ceiling | `E-UNCHECKABLE`; `--load` exits, `hb: quotation nesting full at <cap> levels` |
| a nested `[:` in a file `tools/bootstrap.sh` compiles | the Gforth recovery build prints `[:` and exits; name the inner body |
| a definition whose effect, a returned quotation's row included, is deeper than the ceiling | `E-UNCHECKABLE`, `effect too deep to record (depth <depth>, at most <cap>)` |
| a definition taking more input cells than the ceiling, declared or inferred | `E-UNCHECKABLE`, `input row too wide to record (<cells> cells, at most <cap>)` |
| a `trust`, `TRUSTED:` or `defer` row past either ceiling | `E-BAD-STORED-SIGNATURE`, `fix_signature_size`, the same reasons |
| a return-stack cell in a signature or quotation type: `( n \| -- \| n )`, `( R n \| S -- R )`, `[ n -- \| U -- U n ]`; only `\| U -- U` (one row variable, both sides) is admitted | `E-BAD-SIGNATURE`, `fix_return_stack`, at both tiers; `E-BAD-STORED-SIGNATURE` for `TRUSTED:`, `defer`, `trust`, `CAST:` — park a value with `>r … r>` inside one definition |
| a quotation literal that pushes a cell its caller pops, or pops one its caller pushed: `[: >r ;] execute r>`, `>r [: r> ;] execute` | `E-REJECTED` at `;]`, `fix_return_stack` |
| a definition with no signature that reads or pops a cell it did not push: `: X r@ ;`, `: X r> ;` | `E-REJECTED` at the token, `fix_return_stack`, at both tiers |
| a family, variant or field name over the name cap | `E-BAD-DECLARATION`, `name longer than <cap> bytes` |
| `package` with a name over the name cap | `E-STATEMENT-THROW` (`E-PACKAGE-NAME-CAP`) |
| `: PKG:X ( -- ) ;`, `variable PKG:V` or `package PKG` where the engine bakes `PKG`: a definition into, or the reopening of, a sealed package | `E-STATEMENT-THROW` at the name |
| a non-preserving `[: G ;] catch` | `E-MISMATCH` at `catch`, with the rows |
| `( -- n ) [: 1 ;] ONCE` with `ONCE ( R [ -- S ] -- S ) execute`: a callable that names its stack, or keeps it where `[ -- S ]` is declared, passed, stored, installed with `is` or joined | `E-MISMATCH`, `callable row escape: …; held row A would escape into B`; declare `[ R -- S ]` |
| a read of what a throw left; `i`/`leave` outside a loop; `exit` in a loop, no `unloop` | `E-STALE-READ`; `E-REJECTED` |
| any token after a call to a word whose body ends in `die` or `throw`, in the same branch: `: STOP ( -- ) 5 throw ; : AFTER ( -- n ) STOP 0 ;` | `E-DEAD-CODE` |
| `: I ( -- ) ;` | `E-RESERVED-DEFINITION` |
| a definer (`:`, `DEFTYPE`, `create`, `package`, …) or a parsing word (`char`, `'`, a field word) with nothing after it | `E-MISSING-NAME` from `tools/check.f` |
| `\` comment in a `STRUCTURE`/`ENUM` body | `E-BAD-DECLARATION` |
| `HH 8 s" x" DYNAMIC-STORAGE:ARENA-NEW`, or any call or tick of `DYNAMIC-STORAGE:ARENA-NEW` or `ARENA-RELEASE`, directly or through a re-export, outside an `ARENA`'s generated NAME-NEW and NAME-RELEASE | `E-ARENA-CALL` |
| `: field-project ( a n -- ptr n ) drop drop V ;`, by any definer, in any package or letter case | `hb: reserved word cannot be a definition name: field-project`, rc 70 |
| `4 TYPED-BUFFER B no-such-type`: a type, name or literal count `TYPED-*`, `*LAYOUT-BUFFER`, `DYNAMIC-BUFFER` or `ARENA` refuses, or a name or type not on the definer's line | `E-BAD-STORAGE` |
| `( -- ptr a )` for a `variable` | `E-NONPARAMETRIC-EFFECT` |
| a structure pointer passed where a callback's context is `ptr a` (a pointer to cells): `: RUN ( ptr a [ ptr a -- ] -- ) execute ;`, `: GO ( ptr box -- ) [: CB ;] RUN ;` | `E-MISMATCH` at `RUN`; declare a pass-through context `a` |
| `( R [ R -- S ] -- S ) drop`, `( R n -- ) drop`, `( R -- S ) 1`: a body that unifies two declared rows, a row with the unnamed row, or binds a row to fixed cells | `E-NONPARAMETRIC-EFFECT`, naming the rows — run the quotation on every path, or declare the row the body leaves |
| a multi-cell value at the prompt | `hb: interpret-mode layout value: NAME` |
| a bare `using` import a global also names, in a body, at top level or after `'` | `E-USING-SHADOW-GLOBAL`; at top level or after `'` `--load` exits with `ENGINE-ERROR:USING-SHADOW-GLOBAL` |
| a top-level word, or `'` of one, nothing defined before it, `1.` or `$GG` among them | `E-UNDEFINED-TOP-LEVEL` at the token, before anything runs |
| `' F execute` or `['] F execute` with F's inputs missing; `0 0 catch` or `0 execute`, a number where an xt belongs | `E-INPUT-UNDERFLOW` at `execute`; `E-MISMATCH` at `catch`, `execute` |
| at top level, a call of a word defined inside `0 set-check` | `E-CAP-TRUSTED`, as in a body; `'` of it is admitted |
| outside its package, at top level or in a body, a call of a primitive whose one row is its package's private one: `0 0 0 xref-retarget` | `E-UNDEFINED`, as a call to an unknown word is; the word never runs; `'` of it at top level is admitted |
| outside its package, in a body, a call of a private primitive that also keeps a global trusted-only row: `0 0 reloc-maps-clear` | `E-CAP-TRUSTED` |
| `: W ( -- ) ;`, `create`, a new `package`, `wordlist` or `undefine W` while a checker replay is open | `ENGINE-ERROR:OVERLAY-OPEN` |
| a duplicate tail in one wordlist | `E-DUPLICATE-DEFINITION` |
| `: USE-INF RG-ONE ;  : T3 USE-INF USE-INF SAME-ID ;` with `RG-ONE ( -- fresh-region-a )`, `SAME-ID ( x x -- )`: two calls of an inferred producer are two regions | `E-RIGID-REGION-MISMATCH` |
| `: K ( mut-view<p,q,fresh-region-b,u8> -- mut-view<p,q,fresh-region-c,u8> ) ;`: a fresh name the body supplies holds an input's region, or one region under two such names (forth.md **Rigid host-allocation identity domains**) | `E-RIGID-REGION-MISMATCH` |
| a word defined before the check hook with no external row (a `PRIM:` axiom, or a `TRUSTED:` declaration after the checker) in a checked body | `E-UNDEFINED` — **on a sealed from-source prefix boot only**, never on `bin/hb`; a top-level step that reads one is refused the same way, so its row goes in `src/core/cell-effects.f` |
| `0 set-check` then `SUMTYPE`, `PRODUCT`, `STRUCTURE` or `ENUM`, which generate constructors; `NEWTYPE` and `DEFTYPE` generate none and are admitted | `E-TDECL-UNCHECKED`, `habu: bad sumtype declaration 'name': checking is suspended; declare it where checking is on at 'name'`, nothing registered — close the window first |
| a body the scan refuses with the hook cell empty (`0 set-check`) at tier 1 | ordinary compilation uses its declaration, with the reason on stderr and no row authority; a native build requires the prefix scan and refuses before publication (forth.md **Rules learned by refusal**) |
| a checker scan, `generates:`, a type registration or another checker store write in a native compile callback | `E-NCOMP-STATE`, catchable, before anything changes |

Checked C2 views carry owner and loan lifetimes: shared `read-view<p,q,T>` and
exclusive `mut-view<p,q,a,T>`. They occupy two cells but form one logical value.
`C2-MEM:WITH-MUT` allocates bytes for a callback; `WITH-READ` and
`WITH-MUT-LOAN` lend a child view and restore the original parent on return or
throw. A child cannot escape its callback. `C2-MEM:ALLOC` appends zeroed storage
to an explicit owner. Build that owner with `OWNER-SIZE` / `WITH-MUT`,
`SEED-OWNER` / `WITH-INIT`, then `BIND` and `UNBIND` inside the initialized
callback; forth.md **Owners and views** has a checked source example. `ALLOC-DISPOSE` also registers a callback that consumes
the new unique view at that owner's close, including when an inner owner is
active. `PUBLISH` consumes a mutable view to make it shared.
A view on either side of `CAST:` is `E-CAST-SCOPE`, except a view representation
cast: only C2-MEM's private section packs `( ptr u8 n -- V )`, retypes a view
callback as `( [ V -- ] -- [ ptr u8 n -- ] )` or unpacks a mut-view, and any
private section may unpack `( read-view<p,q,T> -- ptr u8 n )`.
The cast takes an unqualified name; a qualified one is `E-CAST-SCOPE`.
Ordinary raw pointers remain lifetime-free.
The task-local frame capacity is fixed; one more live open throws
`E-C2-CAPACITY` before acquisition. Live owner or loan image capture throws
`E-C2-CAPTURE`; capture after close succeeds.

`C2-MEM:WITH-INIT` initializes a record in a byte view, `WITH-RECORDS` and
`WITH-RECORD` own a table of records and lend one element, and `WITH-FIELD
name` borrows one initialized field, each under a fresh scope nested in the
parent's that cannot escape its callback. The comments above each word in
`src/lib/c2-memory.f` state what it admits and refuses; forth.md **Owners and
views** has a checked example.

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
| `bl` | `STR-SPACE`, `src/lib/string.f` |
| `*/` | none; `*` then `/` |
| `s>number?` | `STR>NUMBER?`, `src/lib/string.f` |
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
`W-CHECK-DEFERRED`: the row is trusted, never compared with the body. The check reads it from the source it checks, so a word whose row it
did not read, a resident or precompiled one among them, is undeclared;
`EXPORT` carries a row, a word that calls the declared one does not.
The separate dependency discovery walk harvests rows before it follows loads.
It skips a parsing word's operands only when the harvested files have one
declaration of that word and no retirement; otherwise it reads the operands
as source.

A word that looks its string operand up as a name states it after its
definition, and a string literal right before a call of it is then a use of
the word between its quotes, which references list:

```forth
names: LOOK                         \ LOOK finds ( ptr u8 n ) as a name
: SEEK ( -- ) s" ROOK" LOOK ;       \ ROOK between the quotes is a use of ROOK
```

The name resolves as a lookup does: `PKG:TAIL` a public word, a bare name the
global one. A computed operand, a literal a caller passes on and a literal with an
escape in it bind nothing. The row is trusted, never compared with the body;
`EXPORT` carries it, a word that calls the declared one declares its own. A
row with no target, naming no word or naming a word whose input row does not
end in the string `( ptr u8 n )`, as `names: dup` does, is `E-NAMES-ROW`.

A word that renders definitions at load time (`FUNCTION:`/`;FUNCTION`,
`TASK:+USER`, anything reaching `INCLUDE-EVALUATE`) makes names
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

After a resident immediate in a body or a top-level loader, later ordering is
uncertain: `--verify-only` reports a trusted-only tick as `W-CHECK-DEFERRED`
and leaves dependent source to the run. A `require`/`include` can reach
replaceable source providers, so its spelling alone settles nothing.

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
below it throws a stack underflow and a text that leaves cells throws
`E-EVAL-RESIDUE`, the caller's cells intact either way. An xt the text runs
cannot reach them: `' F execute` with F's input under the floor is
`E-INPUT-UNDERFLOW` at `execute`. A text that ends
inside a definition it opened is refused as every source is, `hb: source ended
inside definition: NAME`, and the definition is rolled back (forth.md **Checked code and primitive boundaries** lists the open cases).

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
| `ARENA AR t` | records sharing one lifetime, never moved | family `ar`, held in place in the owner's `FIELD` or `TYPED-VARIABLE` (`@`/`!` on it is `E-IN-PLACE`, a cast reaching its head `E-CAST-OWNER`, a `DYNAMIC-BUFFER`, `ARENA`, `CWORK-VARIABLE`, `CWORK-BUFFER` or `CWORK-SPAN` of it `E-BAD-STORAGE`); `AR-NEW ( ptr ar -- ptr t )` a zeroed record, `AR-RELEASE ( ptr ar -- )` frees them all, and a pointer kept across it can alias the next NEW's record; tasks sharing one arena serialise both; capture refuses an arena still live after every hook has run (`DYNAMIC-STORAGE:E-ARENA-LIVE`), so a hook can release it |
| `n LAYOUT-BUFFER LB fam` | capacity for a declared family | `( n -- ptr fam )` |
| `STRUCTURE p FIELD x n … ;STRUCTURE` | a by-value record | `P:MAKE` / `P:UNMAKE`; under `package PKG` the tail is `PKG-P:MAKE`, hyphens doubled in both names (`package A-B`, `STRUCTURE c-d` gives `A--B-C--D:MAKE`); `STRUCTURE p OPAQUE …` keeps `PKG:p` public and makes the pair PKG's private `P-MAKE` / `P-UNMAKE` |

A word a definer's `create` makes has `-- ptr a` from the moment it is made, at
either tier, in a package or under `evaluate`, until a `does>` the definer then
reaches replaces it with the clause effect; a branch that `exit`s before
`does>` leaves `-- ptr a`.

A definer that writes its word as text (`+USER`) states what it makes with
`generates: D ( effect )` after D's definition, so `tools/check.f` checks that
word's uses before the run; what no row states is left to the run (§ 3). `FUNCTION:` needs no row.

`PTR-U8-TABLE-VARIABLE` is a cell holding a `PTR-U8-TABLE`'s base
(`( -- ptr ptr ptr u8 )`); `PERSISTED-PTR-VARIABLE` and
`PERSISTED-PTR-U8-TABLE-VARIABLE` are the snapshot-marked siblings. Runtime-sized buffers come
from `src/lib/memory.f` (`MEM:ALLOC-BYTES`). `ptr-field` takes a **cell** index, not
a byte offset.

On ARM64, the cells `,` writes from a created body's start up to the first
other allocation are that body's cell grid, closed at the next created word,
the end of the file that wrote them, the start of another file's load (a
`require` of an already loaded file loads nothing), an image capture or a
storage cell read.
`CELL-BODY ( n -- )` declares exactly `n` complete cells in the current live
`create` body, after allocating them, `allot` cells included, and wins over the
`,` run. For example, `: GRID: ( n -- ) create , 2 , 2 CELL-BODY does> ( -- SPAN:span<u8> ) cell+ byte-view CELL SPAN:MAKE ;`
keeps the two-cell body boundary when `does>` changes the word's result to a
span. The count must be positive and the body must end at `here`; invalid
extents throw `E-LAYOUT-BUFFER`. The declaration supplies storage boundaries to image
capture, not pointer or type authority: a complete cell holding a build-time
mapping address still causes stripped-image refusal.

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

Top level obeys the same rule, before the word runs and with the cell left
unwritten: `variable V  here V !` is `E-RAW-CELL-PTR` at `!` and
`create T ' dup ,` is `E-MISMATCH` at `,`. A cell pushed inside a `0 set-check`
window is unknown after it: `check@ 0 set-check here swap set-check PV !` is
`E-STALE-READ` at `!` even for a `PTR-VARIABLE PV`. A return-stack cell pushed
inside the window cannot be read after it, because the interpret row has no
return depth to count it by: `check@ 0 set-check 1 2 2>r set-check 2r>` is
refused at `2r>`. Admitted at top level:
`5 V !`, `1 V +!`, `here PV !` with the pointer kept across a `:` definition or
a `require`, `' X HK !` for `TYPED-VARIABLE HK [ -- ]`, and anything inside a
`0 set-check` window.

An integer or an `xt` becomes an address or a quotation only through a private
`CAST:` (`CAST: >BYTES ( n -- ptr u8 )` in a package's private section, else
`E-CAST-MINT`); `X NULL-PTR -` is the address-to-integer distance for any
pointer `X`, with no view: a distance against `NULL-PTR` or `data-base` binds
neither pointee, so `: D ( ptr a -- n ) NULL-PTR - ;` keeps `a` free.
A cast may take an `xt` (`CAST: XT>N ( xt -- n )`) but never yields one
(`E-CAST-CLASS`), and no storage holds one (`E-BAD-STORAGE`, `xt in a stored
type`).
A layout's fields count as the bare type: a cast into a layout whose `FIELD`
holds a pointer or a quotation is the same mint, and one whose field holds
another package's cell family or `OPAQUE` structure is `E-CAST-OWNER` outside
that package, as a cast to one or to a pointer to one is. So is a cast whose
either side reaches an arena head, through pointers, quotation rows or fields:
`CAST: >U8 ( ptr foos -- ptr u8 )` over an `ARENA FOOS` owner, or its reverse,
outside `DYNAMIC-STORAGE`, private section included.

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

forth.md: **Structures And Enums**; the rule is forth.md **Raw storage never
holds an address**.

## 6 Errors

- External applications and separately distributed libraries allocate codes in
  `-999999..-100000`, inclusive; Habu's runtime, compiler, bundled libraries,
  tools and tests allocate outside it. Loaded dependencies need nonoverlapping
  blocks: package names do not distinguish thrown numbers. The application
  integrator resolves collisions; the checker does not enforce this convention.
- Library codes are named constants in `src/lib/errors.f`; checker throws also use
  positive codes above 255 in their owning source. Codes 0..255 serve as
  process exit statuses and may be shared. Every other code, negative or above
  255, has one `E-` name across the tree; a file that needs it under its own
  name reads the owner's constant (`E-OWNER constant E-LOCAL`). A library owns
  one inclusive block of about a hundred bounded by its own `E-X-FIRST` /
  `E-X-LAST` (arrays, filesystem, strings, …) and
  reserves that whole range whether or not every code is minted;
  `tools/error-code-lint.f` reports a file minting inside another's.
- A block with codes in a package (`JR:E-SOURCE`) is minted, bounds and all,
  in the file that owns the package (`src/lib/json-read.f`), and `src/lib/errors.f`
  says where. The engine bakes `src/lib/errors.f`, and a package the engine bakes
  has one owning file: no source a product engine loads reopens it.
- A fallible word `throw`s a named code, never an out-of-band flag.
- `throw` is catchable and belongs to the checker's exception edge. `die`
  (`ptr u8 n n --`, a real message and exit code) ends the process and is
  no-return, so nothing may follow a call to a word that ends in one. `throw`
  for recoverable and interactive paths, `die` for build makers and CLI
  boundaries.
- Exit 79 (`$4F`) is compilation from a task: a definition, `create`, `allot`,
  `,`, `evaluate`, a load or a package keyword run on a task's, an exposed
  region's or a callback's thread, with nothing on fd 2: `evaluate` is refused
  before its text is read, so a keyword in that text never runs. The main
  thread compiles while tasks run; `cp!`, `ndict!` and `seed-ndict!` exit 79 on
  it too, silently, while any task is live. `NLEASE`'s checks throw
  `NLEASE:E-TASK` on a task's thread, and `NLEASE:IDLE?` answers false there.
- `0 throw` throws `ENGINE-ERROR:THROW-ZERO` (109), never the ANS no-op:
  `[: 0 throw ;] catch` answers 109.
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

- `require src/lib/string.f` loads once per image, keyed by canonical path;
  `include` replays. `s" path" required` / `included` are the string forms. A
  path resolves against the root that resolved the requiring file (for an
  entry, the `--load` entry's or `hb prog.f` program's own directory, or a
  stdin session's working directory), then the engine's source root
  (the working directory when it is a Habu tree, else the tree above the
  running `bin/hb`), then the working directory, so `require src/lib/…` names
  the tree root and a working-directory copy of a Habu library never loads in
  place of one a loaded file requires (a stdin session's own requires are the
  working directory's: it is their root). A file found through the working directory keeps that root for
  its own requires. An entry below its tree requires the tree's files
  relative to itself (`../src/…`): a tree-relative name Habu also has loads
  Habu's file. Every file requires its **own** dependencies.
- A loaded file is a closed program: its top level starts at `depth` 0, a
  token reaching its loader's cells throws a stack underflow, a file that ends
  with cells on the stack is `E-EVAL-RESIDUE`, and one that ends inside a
  definition it opened is refused there, as every source is. A value crosses a load
  only as a word the file defines. `here` is cell-aligned when a loaded file
  starts and when a `require` or `include` that ran a file returns: data never
  spans a load.
- At the top level of a stdin session or of a program file run as
  `bin/hb file.f`, `SOURCE-ROOT:CD <dir>` moves the first search root (a bare
  `CD` prints it), `PUSHPATH` / `POPPATH` save and restore it, all public in
  `SOURCE-ROOT`; inside a loaded file (`--load`, `require`) they are refused:
  scope a root with `SOURCE-ROOT:WITH`.
- The engine provides `src/lib/prelude.f`, `errors.f`, `string.f`, `span.f`,
  `memory.f`, `num-types.f`, `num-arithmetic.f`, `image-lifecycle.f` and the
  `src/` files its boot prefix loads: their words resolve with no require and
  a `require` is a no-op, and `tools/check.f` answers `E-ENGINE-PROVIDED` for
  one (rebuild `bin/hb` to check a change to it). Write it
  anyway: a file states its dependencies.
- Multi-file packages reopen `package NAME` per file; reopening shares scope and
  loads nothing. An engine package is sealed: reopening one is refused. Run tools
  as `bin/hb --load src/lib/a.f tool.f -- args`; `SCRIPT-ARGV$` starts after `--`.

forth.md: **Files**, **Packages**.

## 8 Tests

Follow the testing policy in [AGENTS.md](../AGENTS.md). Exercise a complete
feature through its real load/build path and retain a repeatable artifact.
See `test/aot-chain-capture-suite.f` for a producer, saved image, and consumer
flow. Use `require src/lib/test.f` for assertions and keep fixtures package-scoped.

`T=` / `T<>` scalars, `T$=` strings, `TTRUE` / `TFALSE` flags, `TTHROWSQ` /
`TTHROWS` throw codes. `TEST-EVAL:N` / `FLAG` / `RC` take one cell, a flag or
the throw code out of a source text. Register it as a `SUITE name … ;SUITE`
entry in `test/gate-stdlib-cases.f`; `bin/hb --load test/run.f` runs every
suite.

forth.md: **Testing**, **Verification before committing**.

## 9 Comments

- A `\` comment is one or two lines and says only what the code cannot: units,
  an ownership or ordering invariant, a hardware encoding, a checker boundary,
  why a non-obvious choice is required. Meaning lives in names and factoring;
  design belongs in `docs/`.
- It states what is true now; a claim that needs proof names the test, the
  refusal and its code, or the measured behaviour that proves it.
- No history: no "used to", "no longer", earlier designs, what was tried,
  dates, dot, commit or review ids, agent or person names. A failure behind a
  rule is a present fact: "a stage without X dies `E-UNDEFINED` at Y".
- No restating the code, no design essays, no arguments for a design. A fact
  lives in one comment or one doc section; others point to it. Cite a word,
  not a line number.
- Any edit that touches a comment, a conflict merge or a moved definition
  included, brings it to these rules; a change that makes a comment false
  corrects it in the same change. A review reports a breach as a finding.

## 10 Open forth.md at this heading when …

| heading | open it when |
|---|---|
| Checked code and primitive boundaries | `TRUST`, a new `PRIM:`, what `evaluate-closed` leaves open |
| Naming | a collision, reserved names |
| Packages | reopening, include vs require |
| Importing … with `using` | ambiguity, scope end, the limit |
| Structures And Enums | `NEWTYPE`/`SUMTYPE`/`PRODUCT`/`ENUM`, `construct`, `MATCH`, `CAST:`, `LINEAR:` |
| Words & factoring | word size, argument limits, splits |
| Files | one concern per file, script argv |
| Stack comments | the token list, real types |
| Stack effects | the effect grammar, the return-stack clause, types, widening, `DEFTYPE`/`DEFLINEAR`, pointers, escape hatches |
| Checker & type model | loop frames, higher-order effects, `defer` |
| Owners and views | C2 owners, `read-view`/`mut-view`, scope binders |
| Errors | the `ENGINE-ERROR` ABI, `die` divergence |
| Integer arithmetic | the wrapping contract, `MIN-N` |
| Engine limits … | the body-text, REPL-line, `[:` and control-flow nesting, name, effect-depth, input-row and data-space ceilings |
| Constants | hex versus decimal, where codes live |
| Testing | assertions, a suite example, runner and property rules |
| Diagnosing a checker miss | find the layer that is wrong |
| Verification before committing | where the gates are |
| Comments & hygiene | debug prints, a refused definition's report |
| Habu Native Tooling Gotchas | debugger, xref, `DYNAMIC-BUFFER`, alignment, `CELL-BODY`, absent words, escapes |
| Native Forth Gotchas … | compile-only words, `case`, `parse-name` |
