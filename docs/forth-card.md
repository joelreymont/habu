# Habu in one card

Read this instead of [forth.md](forth.md). Every refusal below was measured
on `bin/hb`; every other claim condenses the forth.md
heading named at the end of its section; section 10 says which heading to open
when this runs out.

## 1 Names and packages

Our words UPPER-CASE, built-ins as-is (`dup`, `?do`). Hyphens, never
underscores, in word *and* file names. Predicates end `?`, conversions are
`>X`, fetch/store `X@` / `X!`, scope pairs are `FOO … ;FOO`. `$hex` for
machine-adjacent numbers. Every module is a package, named for its subject as
it is commonly called or by a short form of its words (`NUMFMT`, OOXML's
`numFmt`), never by initials coined for the package or an operation on it:
`JSON:READ-NEXT`, not `JR:NEXT`; `JSON:WRITE-OBJECT-START`, not
`JSON-WRITE:OBJECT-START`. A subject's reading and writing words share it.
Names use standard programming terms (error, not refusal), and helpers with
one meaning are one word. A library for a protocol names things in that
protocol's words (MCP's tool result and content blocks, not answers), stored
records included.

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
  global also owns is refused (§ 3) — rename the public. A file loaded under a
  `using` resolves through it. Close a using opened before `package` after
  `;package`: a `;using` inside the package that would close it is
  `ENGINE-ERROR:USING-OUTER`. A load file is a using scope too: a `;using` in
  an included file or `evaluate` buffer that would close its includer's using
  is refused the same way.
- `package B` in package A's private section, in the file or string A is
  open in and under no `using` A opened, opens the **part** `A:B`; a part
  holds no parts. Inside A and its parts `B:X` in a word or a family type,
  `using B` and `: B:Q` name the part first, then a top-level B; outside A
  nothing reaches it, `A:B:X` included, and a top-level `package B` is another
  package. A `DEFLINEAR` or `VALUE-RECORD` name `B:X` is an exact global
  spelling no part shadows: it is refused while top-level B or a part named B
  declares the family X (`checker: bad or duplicate signature type`, or
  `checker: bad or duplicate value-record type` for `VALUE-RECORD`, rc 70), and
  that family is refused while the name exists (for `NEWTYPE x`, `bad newtype
  declaration 'x': reserved name`, rc 70). `;package` of a part returns to A's
  private section. `package` anywhere else inside a package is refused:
  `E-PKG-CONTEXT habu: <loc>: package`, rc 75.
- `[package] ( -- ptr u8 n )`: the open package's name in lower case (`hb`, a
  part's `hb:b`), empty outside one; a literal in a body, pushed at top level.
- `EXPORT NAME` in a public section re-exports an existing word under its own
  tail: same xt, same effect, no body.
- The package keyword `seal-package` seals the open package: a later
  `package` of it, or a definition or `EXPORT` into it, is
  `E-SEALED-PACKAGE`, rc 84, and `tools/check.f` refuses the same. Outside a
  package: `E-PKG-CONTEXT habu: <loc>: seal-package`, rc 75; in a body or after `'`, as
  `public` there, `E-UNDEFINED`. A multi-file owner seals after its last file.
  A build seals every package its engine files made before program source.
- At top level `host` and `target` are the section words
  ([xcomp.md](xcomp.md#host-and-target-words)). Where a word named `HOST` or
  `TARGET` is in scope the bare token is refused, `E-SECTION-WORD`, rc 70:
  rename the word or call it qualified; a body or `'` names the word.
- A wordlist is a no-duplicate set, case-insensitively: a second `: R` is
  `E-DUPLICATE-DEFINITION`; `undefine R` first to replace one. A `does>`
  definer `R` also claims `R;does` there, and so does `EXPORT R`. The new `R`
  is checked and called as itself even when it spells an engine word: the
  checker's rules for `@`, `!`, `create` and the like belong to the engine's
  own word, which a redefinition is not.
- A call binds a word the engine holds. A name only the checker knows, a row
  recorded or scanned without a definition, is `E-UNDEFINED` in a body, except
  that generated definitions inside one checker replay may call rows recorded
  earlier in it. A definition (`:`, `create`, …), a new `package` or a
  `wordlist` made while a checker replay is open dies where it is made, naming
  it, and so does an `undefine` run there, before it retires anything:
  `ENGINE-ERROR:OVERLAY-OPEN`.
- Compiler keywords (`I`, `DO`, `IF`, …) cannot be definition names:
  `E-RESERVED-DEFINITION`. Nor can the target predicates `HB-TARGET-LINUX?`,
  `HB-TARGET-MACOS?`, `HB-TARGET-LINUX-X86-64?`, `HB-TARGET-LINUX-KERNEL?` and
  `HB-TARGET-WASM?`, as the name after `:`, `+:`, `TRUSTED:`, `KERNEL:`,
  `create`, `variable`, `constant`, `defer` or `LAYOUT-BUFFER`: `tools/check.f`
  refuses them, not `--load`. A number-shaped name (`: 42`, an overflowed `: 99999999999999999999`
  too) is refused by `--load`, by every definer: rc 70.

forth.md: **Naming**, **Packages**, **Importing … with `using`**, **Rules
learned by refusal**.

## 2 Effects, locals, quotations

Every definition carries `( before -- after )` and the checker reads it as the
signature. Type tokens only (`n`, `u8`, `bool`, `xt`, `ptr a`, `ptr u8`, `idx`,
`len`, `fd`, `rc`), never role prose like `( got want -- )`.
`xt` is a ticked engine primitive's token (`['] type`), which checked code
cannot execute: `E-EXEC-OPAQUE-XT`.
`width<t>` is one cell holding the bytes `t` takes in storage; `width>n` reads
them, `width<u8>` is not `width<u32>` or `width<n>`, and no storage, field or
cast holds or yields one.
`[type] t` in a body makes one for a closed type `ARENA` stores; it refuses an
open, linear, `xt`, `u32` or quotation type (`E-BAD-STORAGE`).

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
- A type var named only behind `ptr`, or in a family argument the family's
  width does not read, takes any stored type: `: SAME ( ptr t -- ptr t ) ;`
  takes `ptr turn`. Fetching, storing or stepping through it, or passing it to
  a `defer`, `RECURSE` or any primitive but a distance and `PTR>N`, makes it
  one cell: a record is `E-MISMATCH` at the call.
- Concrete integers widen when lossless (`u8 → u16 → u32 → cell/i64`). Generic
  `n` accepts structural integer stack cells in either direction: `n` can pass
  to `u32`, while `i64` needs an explicit conversion. In a definition's body a
  number literal passes into a fixed-width input or a declared output only when
  it fits: `65` into `u8`, never `300`. Pointer pointees stay exact; roles
  (`idx`, `len`, `fd`) never widen. Booleans are real `bool`s: `true`, `false` or a
  comparison, never a raw `0`/`-1`; a `bool` never passes as `n`, so convert
  it with `bool>n` (-1 or 0).
- `nullable<a>` (`src/lib/adt/nullable.f`) is one cell, an address or zero:
  `NULLABLE:SOME ( ptr a -- nullable<a> )`, `NULLABLE:NONE`, and `MATCH
  nullable` with `none` and `some` arms reaches the address. Zero-filled
  storage and a null `some` read as `none`.
- A `FUNCTION:` result declared `nullable<u8>` is `none` for NULL.
- Locals `{: a b:ptr :}` bind left to right from the deepest item, so
  `1 2 {: a:n b:n :}` gives `a`=1. A local binds **once**: a per-turn value
  lives on the stack or in a cell. Names are at most 16 bytes, 64 per
  definition, block-scoped, bindable after a closed early-exit guard. A local
  hides the word of its spelling, the loop index included:
  `{: i:n :} 3 0 ?do i . loop` prints the local three times.
- Code is well-factored and its lines are short, about 60 columns at most,
  because people read it: a word does one thing in about five lines, and a repeat or
  a dense expression becomes a named word.
- A `{: … :}` group that binds at entry goes on its own line after the name
  and stack effect, indented three spaces as the body.
- Bind multi-cell values whole: `{: p :}` or `{: r:res<n,n> :}` (arity checked).
  Destructure only to compute; pass the whole local between words.
- Quotations `[: … ;]` are xts. The token `[ in -- out ]` works as
  a parameter, a `TYPED-VARIABLE` or `TYPED-BUFFER` element, and a
  `STRUCTURE FIELD` value. A derived field accessor returns `ptr [ in -- out ]`.
  `: A ( n [ n -- n ] -- n ) execute ;` certifies and `2 [: 1 + ;] A` runs. A
  quotation may read the locals of the word it is written in while that word
  runs: `{: v :} [: v RENDER ;] [: v CLOSE ;] finally`. It may not outlive
  them (`E-LOCAL-ESCAPE`) and binds no group of its own.

forth.md: **Stack comments**, **Stack effects**, **Words & factoring**,
**Structures And Enums**.

## 3 What the checker refuses, and what it admits

Refused; code from `tools/check.f --json-errors`.
Top-level code is checked too: each token is judged against the live stack's
cells as the same token in a colon body. A checked step runs, as does one
deferred to the run; a refused or uncheckable one prints the body's diagnostic
before the word runs, and `--load` exits 70 (`evaluate-closed` throws 70).
`set-tier`'s row is global, so a program's first line may choose the tier
before any `require`.

| you write | diagnostic |
|---|---|
| `evaluate` in a checked body (top level is fine) | `E-UNSAFE` — use `evaluate-closed` |
| `variable V  : F ( -- ) V @ execute ;` | `E-EXEC-OPAQUE-XT` |
| an `if` arm or loop body that changes depth | `E-MISMATCH` at `then`/`loop`/`repeat` |
| a local declared inside `[: … ;]` | `E-BAD-LOCAL-SHAPE` |
| a quotation that reads a local and is stored, installed with `is`, passed to a word that keeps it or to `RECURSE` where the signature declares a quotation, handed to a task, returned, or held past the local's block | `E-LOCAL-ESCAPE`, `a quotation that reads a local would outlive it` |
| a word with no declared effect whose effect would hold a `forall<…>` callback outside a callback input, as `: W [: WITH-SCOPE ;] ;` where `WITH-SCOPE ( forall<p,[ a -- ]> -- )` | `E-BINDER-PLACE`, `a callback binder would sit outside …` — call the scope operation directly |
| `--` or `\|` in a locals group: `{: a -- b :}`, `{: a \| :}` | `E-BAD-LOCAL-SHAPE` at the token, `a locals group names only locals; …` — the signature declares the outputs, and Habu has no uninitialised locals |
| a `[:` past the quotation nesting ceiling | `E-UNCHECKABLE`; `--load` exits, `hb: quotation nesting full at <cap> levels` |
| a definition whose effect, a returned quotation's row included, is deeper than the ceiling | `E-UNCHECKABLE`, `effect too deep to record (depth <depth>, at most <cap>)` |
| a definition taking more input cells than the ceiling, declared or inferred | `E-UNCHECKABLE`, `input row too wide to record (<cells> cells, at most <cap>)` |
| a body nesting more than 32 control frames (each `if`, `begin`, `do`, `case`, `MATCH`, `of` arm and `[:` takes one): 33 nested `dup IF` | `E-UNCHECKABLE`, `control flow nested too deep to check (depth <depth>, at most <cap>)` |
| `: DOWN dup 0 > IF 1 - RECURSE THEN ;`: `recurse` in a definition with no signature | `E-UNCHECKABLE`, `recursion needs a declared stack effect` |
| `: MK ( n -- ) create , does> ( -- n ) @ dup 0 > IF RECURSE THEN ;`: `recurse` in a `does>` clause | `E-UNCHECKABLE`, `recurse in a does> clause would call the defining word` |
| a `trust`, `TRUSTED:` or `defer` row past either ceiling | `E-BAD-STORED-SIGNATURE`, `fix_signature_size`, the same reasons |
| a local's type nested past the depth ceiling: `{: x:[[[… :}` | `E-BAD-SIGNATURE`, `fix_signature_size`, the depth reason |
| more than 32 `forall<…>` binders nested in one signature | `E-BAD-SIGNATURE`, `fix_signature_size`, `more than 32 nested binders at '<token>'`; `E-BAD-STORED-SIGNATURE` for a stored row |
| a return-stack cell in a signature or quotation type: `( n \| -- \| n )`, `( R n \| S -- R )`, `[ n -- \| U -- U n ]`; only `\| U -- U` (one row variable, both sides) is admitted | `E-BAD-SIGNATURE`, `fix_return_stack`, at both tiers; `E-BAD-STORED-SIGNATURE` for `TRUSTED:`, `defer`, `trust`, `CAST:` — park a value with `>r … r>` inside one definition |
| a quotation literal that pushes a cell its caller pops: `[: >r ;] execute r>` | `E-REJECTED` at `;]`, `fix_return_stack` |
| a body or quotation literal that reads or pops a return cell it did not push: `: X r@ ;`, `: X ( -- ) r> drop ;`, `>r [: r> ;] execute` | `E-REJECTED` at the read, `fix_return_stack`, at both tiers |
| a family, variant or field name over the name cap | `E-BAD-DECLARATION`, `name longer than <cap> bytes` |
| `package` with a name over the name cap | `E-STATEMENT-THROW` (`E-PACKAGE-NAME-CAP`) |
| `: PKG:X ( -- ) ;`, `variable PKG:V` or `package PKG` where `PKG` is sealed, by its `seal-package` or as an engine package, and the open top-level package has no part `PKG`: a definition into, or the reopening of, a sealed package | `E-SEALED-PACKAGE` at the name |
| a non-preserving `[: G ;] catch` | `E-MISMATCH` at `catch`, with the rows |
| `( -- n ) [: 1 ;] ONCE` with `ONCE ( R [ -- S ] -- S ) execute`: a callable that names its stack, or keeps it where `[ -- S ]` is declared, passed, stored, installed with `is` or joined | `E-MISMATCH`, `callable row escape: …; held row A would escape into B`; declare `[ R -- S ]` |
| a read of what a throw left; `i`/`leave` outside a loop; `exit` in a loop, no `unloop` | `E-STALE-READ`; `E-REJECTED`, `fix_control_flow`, `no loop is open here`, `exit inside a loop needs unloop first` |
| any token after a call to a word whose body ends in `die` or `throw`, in the same branch: `: STOP ( -- ) 5 throw ; : AFTER ( -- n ) STOP 0 ;` | `E-DEAD-CODE` |
| a definer (`:`, `DEFTYPE`, `create`, `package`, …) or a parsing word (`char`, `'`, a field word) with nothing after it | `E-MISSING-NAME` from `tools/check.f` |
| `defer D`, `: MK ( n -- ) create , does> @ ;`: a definer that declares an effect with no signature after its name, or one never closed | `E-MISSING-SIGNATURE` at the definer, at `does>` for a clause; `--load` exits 76, `E-MISSING-SIGNATURE habu: <file>:<line>: 'D' has no closed ( -- ) signature` |
| a `\` or `( … )` comment in a `STRUCTURE`/`ENUM` body | rc 70 at load; `E-BAD-DECLARATION` from `tools/check.f` |
| `ENUM state DERIVE eq closed open ;ENUM` in a package's private section | `habu: bad enum declaration 'state': derive requires a public family at 'eq'`, rc 70; test a private family's values with an exhaustive `MATCH` |
| `16 DYNAMIC-STORAGE:RECORD-NEW`, or any call or tick of `DYNAMIC-STORAGE:RECORD-NEW` or `RECORD-RELEASE`, directly or through a re-export, at top level too, outside a `HEAP`'s generated NAME-NEW and NAME-RELEASE | `E-ARENA-CALL-TOP-LEVEL` for a top-level token in the source, else `E-ARENA-CALL` |
| `: field-project ( a n -- ptr n ) drop drop V ;`, by any definer, in any package or letter case | `E-RESERVED-DEFINITION habu: <file>:<line>: reserved word cannot be a definition name: field-project`, rc 70 |
| `4 TYPED-BUFFER B no-such-type`: a type, name or literal count `TYPED-*`, `*LAYOUT-BUFFER`, `DYNAMIC-BUFFER`, `ARENA` or `HEAP` refuses, or a name or type not on the definer's line | `E-BAD-STORAGE` |
| `( -- ptr a )` for a `variable` | `E-NONPARAMETRIC-EFFECT` |
| a structure pointer passed where a callback's context is `ptr a` (a pointer to cells): `: RUN ( ptr a [ ptr a -- ] -- ) execute ;`, `: GO ( ptr box -- ) [: CB ;] RUN ;` | `E-MISMATCH` at `RUN`; declare a pass-through context `a` |
| `( R [ R -- S ] -- S ) drop`, `( R n -- ) drop`, `( R -- S ) 1`: a body that unifies two declared rows, a row with the unnamed row, or binds a row other than its own return row to fixed cells | `E-NONPARAMETRIC-EFFECT`, naming the rows — run the quotation on every path, or declare the row the body leaves |
| a multi-cell value at the prompt | `E-LAYOUT-VALUE habu: interpret-mode layout value: NAME` |
| `depth` or `.s` over a value held in more than one cell, or a layout value whose width is open there: `3 OPTION:SOME depth`, `( option<a> -- ) depth` | `E-REJECTED`, `count_values`, ``cannot count `option<n>`: it is one value held in 2 cells``, or ``cannot count `option<a>`: it is one value, and how many cells it holds is not known here``; take the value apart first (`MATCH` needs concrete arguments, `E-MATCH-OPEN-ARGS`), or count another way |
| a bare `using` import a global also names, in a body, at top level or after `'` | `E-USING-SHADOW-GLOBAL`; at top level or after `'` `--load` exits with `ENGINE-ERROR:USING-SHADOW-GLOBAL` |
| a top-level word, or `'` of one, nothing defined before it, `1.` or `$GG` among them | `E-UNDEFINED-TOP-LEVEL` at the token, before anything runs |
| `' F execute` or `['] F execute` with F's inputs missing; `0 0 catch` or `0 execute`, a number where an xt belongs | `E-INPUT-UNDERFLOW` at `execute`; `E-MISMATCH` at `catch`, `execute` |
| outside its package, at top level or in a body, a call of a primitive whose one row is its package's private one: `0 0 0 xref-retarget` | `E-UNDEFINED`, as a call to an unknown word is; the word never runs; `'` of it at top level is admitted |
| outside its package, in a body, a call of a private primitive that also keeps a global trusted-only row: `0 0 reloc-maps-clear` | `E-CAP-TRUSTED` |
| `: USE-INF RG-ONE ;  : T3 USE-INF USE-INF SAME-ID ;` with `RG-ONE ( -- fresh-region-a )`, `SAME-ID ( x x -- )`: two calls of an inferred producer are two regions | `E-RIGID-REGION-MISMATCH` |
| `: K ( mut-view<p,q,fresh-region-b,u8> -- mut-view<p,q,fresh-region-c,u8> ) ;`: a fresh name the body supplies holds an input's region, or one region under two such names (forth.md **Rigid host-allocation identity domains**) | `E-RIGID-REGION-MISMATCH` |
| `[: RG-ONE ;] dup execute swap execute SAME-ID`, or `[: RG-ONE ;] RUN` with `RUN ( [ -- x ] -- x )`: a fresh region a quotation's inputs do not carry is minted by each execution of the outermost quotation holding it at the top of its outputs or in a callback input's rows, and shared by the quotations that execution returns or hands on, at a direct call, a tick and an inferred effect alike; no callee or callback whose effect types one, generic or fresh, takes it | `E-RIGID-REGION-MISMATCH` |
| a word defined before the check hook with no external row (a `PRIM:` axiom, or a `TRUSTED:` declaration after the checker) in a checked body | `E-CAP-TRUSTED`; a top-level step that reads one is refused the same way, so its row goes in `src/core/cell-effects.f` |
| a checker scan, `generates:`, a type registration or another checker store write in a native compile callback | `E-COMP-STATE`, catchable, before anything changes |

Checked C2 views carry owner and loan lifetimes: shared `read-view<p,q,T>` and
exclusive `mut-view<p,q,a,T>`. They occupy two cells but form one logical value.
`WITH-READ` and `WITH-MUT-LOAN` lend a child view and restore the original
parent on return or throw. A child cannot escape its callback. forth.md
**Owners and views** builds an owner in a checked source example.
`C2-MEM:ALLOC-DISPOSE` allocates from the owner it is given and registers a
callback that consumes the new unique view at that owner's close, including
when an inner owner is active.
`PUBLISH` consumes a mutable view to make it shared. Ordinary raw pointers
remain lifetime-free.

`C2-MEM:WITH-INIT` initializes a record in a byte view, `WITH-RECORDS` and
`WITH-RECORD` own a table of records and lend one element, and `WITH-FIELD
name` borrows one initialized field, each under a fresh scope nested in the
parent's that cannot escape its callback. The comments above each word in
`src/lib/c2-memory.f` state what it admits and refuses.

A caught result becomes readable on the success arm of core `code 0=` (or the
false arm of core `code 0<>`) only when `code` is that catch's own status.
Another zero or a shadowed predicate proves nothing. The value remains marked
as replaced for a later outer catch; quotations retain that mark on their
explicit outputs. A loop's back edge or `leave` carries each cell's mark and
proof as the loop entered with them; entered on the success arm, a caught value
may also come back through a word, which drops its proof (catch-stale-suite.f
A4).

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
| `0>` | `0 >` |
| `0>=` | `0 >=` |
| `2*` | `2 *` |
| `hex` | `$ff` literals; print with `SB-RESET n FMT:SB-HEX SB$ type`, `src/lib/fmt.f` |
| `.r` | none; `FMT:INT$`, `space` up to the width, then `type` |
| `value`, `to` | a `variable`: `5 V !`, `V @` |
| `cmove` | `BYTE-COPY ( src dst n -- )`, `src/core/bytes.f` |
| `bye` | none; a file ends at its last line, and `s" why" n die` exits with status n |

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
anything reaching `INCLUDE-EVALUATE`) makes names
`tools/check.f` leaves to its run, which type-checks their uses there, unless
the source declares them: uses of a `FUNCTION:` word and of a `generates:`
row's word (§ 4) are checked before the run, `--verify-only` included. A
failure the check pins before a use it leaves to the run decides that
definition. At top level any other such name opens that stretch. The renderer
reads none of the tokens after it, so they are checked. A reached renderer no
row or group declares is `W-CHECK-DEFERRED` at its own token, so a `deferred`
verdict always locates its gap.

After a reached call that may render or register nominal types, verification
defers a later use of a missing type to the run; a check no run follows reports
it as `W-CHECK-DEFERRED` at that type. It consumes the declaration without
publishing a type, layout, signature or generated words. Known syntax,
visibility, arity and count errors still refuse. Defining a provider, taking its
tick or reading an uncalled body does not create this uncertainty; ordinary
loading keeps its existing rules.

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
only while start < limit, signed, so `0 0`, `-1 0` and `$8000000000000000 0` run zero times
and `u 0 ?do` runs max(u,0); `?do … +loop` skips only equal bounds, so
`0 10 ?do … -1 +loop` counts down eleven turns. `evaluate-closed ( ptr u8 n -- )`
evaluates source in a body: the text's `depth` starts at 0, a token reaching
below it throws a stack underflow and a text that leaves cells throws
`E-EVAL-RESIDUE`, the caller's cells intact either way. An xt the text runs
cannot reach them: `' F execute` with F's input under the floor is
`E-INPUT-UNDERFLOW` at `execute`. A text that ends inside a definition it
opened is refused as every source is, `E-VS-UNTERMINATED-DEFINITION` (`source
ended inside definition: NAME`), and the definition is rolled back (forth.md **Checked code and primitive boundaries** states what `evaluate-closed` checks).

forth.md: **Checker & type model**, **Native Forth Gotchas …**.

## 4 Storage definers

Every form loaded and its accessor effect certified.

Storage for typed elements states the type (`n TYPED-BUFFER TB t`), never a
width, cell count or byte count worked out by hand: a type's width can change.

| form | for | accessor |
|---|---|---|
| `variable V` | a raw scalar, role or atom cell | `V @` / `V !` |
| `SCRATCH-VARIABLE V` | a `variable` whose value a build leaves out | `( -- ptr a )` |
| `create B 256 allot` | static dictionary storage | `B`, pointee bound by the use (`B 4 type`, `B @`) |
| `n BUFFER: B` | a fixed zeroed byte row | `( -- ptr u8 )`; bytes are never a `TYPED-BUFFER` element |
| `n SCRATCH-BUFFER: B` | a `BUFFER:` whose bytes a build leaves out | `( -- ptr u8 )` |
| `PTR-VARIABLE P` | a global slot holding an address | `( -- ptr ptr a )`; callers declare the pointee: `: F ( -- ptr u8 ) P @ ;` |
| `TYPED-VARIABLE V t` | one typed cell or record | `( -- ptr t )` |
| `TYPED-VARIABLE V ptr t` | a cell holding an address of `t` | `( -- ptr ptr t )` |
| `n TYPED-BUFFER TB t` | a fixed array of `t` | `( n -- ptr t )` |
| `n PTR-U8-TABLE TT` | a fixed table of byte pointers | `( -- ptr ptr u8 )`, indexed with `ptr-field` |
| `n PERSISTED-PTR-U8-TABLE TT` | a `PTR-U8-TABLE` whose cells keep their addresses across a save | `( -- ptr ptr u8 )`, indexed with `ptr-field` |
| `DYNAMIC-BUFFER DB t` | a growable mapped array, `u8` a byte row | `( n -- ptr t )` plus `DB-RESERVE` / `DB-RELEASE`; growth moves it — keep indices, reacquire ptrs, release before an image save |
| `ARENA AR t` | records of `t` in an arena the caller holds, never moved | inside a package: `AR-NEW ( ptr ARENA:arena -- ptr t )` a zeroed record, `AR-NEW-DISPOSE ( ptr ARENA:arena [ ptr t -- ] -- ptr t )` with an action run once at reset or release; `ARENA:NEW` or `ARENA:WITH` makes the arena, `ARENA:ALLOC` and `ARENA:ALLOC-DISPOSE` (with an action) answer raw bytes in it, `ARENA:RESET` or `ARENA:RELEASE` frees its records at once, and a pointer kept across either is stale; tasks sharing one arena serialise every call |
| `HEAP H t` | one record with its own lifetime, never moved | `H-NEW ( -- ptr t )` a zero-filled record on its own mapping, `H-RELEASE ( ptr t -- )` unmaps it; a pointer kept across it is stale, and a second RELEASE can unmap a later mapping; nothing registers a record, so release it before an image save |
| `n LAYOUT-BUFFER LB fam` | capacity for a declared family | `( n -- ptr fam )` |
| `STRUCTURE p FIELD x n … ;STRUCTURE` | a by-value record | `P:MAKE` / `P:UNMAKE`; under `package PKG` the tail is `PKG-P:MAKE`, hyphens doubled in both names (`package A-B`, `STRUCTURE c-d` gives `A--B-C--D:MAKE`); `STRUCTURE p OPAQUE …` keeps `PKG:p` public and makes the pair PKG's private `P-MAKE` / `P-UNMAKE` |

A build tracks a compile-time read of storage as a read of the record whose
address it derives from, within that record's extent: `L 8 +` past
`create L 8 allot` reads the next record, whose edit a warm build misses.

A word a definer's `create` makes has `-- ptr a` from the moment it is made, at
either tier, in a package or under `evaluate`, until a `does>` the definer then
reaches replaces it with the clause effect; a branch that `exit`s before
`does>` leaves `-- ptr a`.

Lay a created body's data right after its `create`, before the next definition
or row: a definition's `;` may lay its facts text at `here`, so data laid after
it no longer follows the body.

A definer that writes its word as text (`ERROR:CODE`) states what it makes with
`generates: D ( effect )` after D's definition, so `tools/check.f` checks that
word's uses before the run; what no row states is left to the run (§ 3). A
deferred D, or one no `parse-name` reaches through the words it calls (not
through a `defer` or an executed xt), is `E-GENERATES-ROW`.
`FUNCTION:` needs no row.

A definer that builds a word from words it found writes no text:
`name$ s" ( n -- n )" define-word` opens the word, `postpone W` appends the W
found where the definer was compiled, which no package the definer later runs
in can shadow, `body-append` appends text that binds where it runs, and
`postpone ;` ends the word.

`PTR-U8-TABLE-VARIABLE` is a cell holding a `PTR-U8-TABLE`'s base
(`( -- ptr ptr ptr u8 )`). An image save writes `PTR-VARIABLE`, `PTR-U8-TABLE` and
`PTR-U8-TABLE-VARIABLE` cells as null; their `PERSISTED-` siblings keep their
addresses, and so do the pointer fields of typed storage. A pointer into memory
the build mapped is refused at save, naming the cell. Scratch storage belongs to
the process that fills it: a build leaves it out and the program reads it as
zero, and it is declared where it is made, as `create R n SCRATCH-ALLOT` for a
definer's own scratch body, with `here` cell-aligned. Runtime-sized buffers come
from `src/lib/memory.f` (`MEM:ALLOC-BYTES`). `ptr-field` takes a **cell** index, not
a byte offset.

forth.md: **Structures And Enums**, **Habu Native Tooling Gotchas**, **Rules
learned by refusal**.

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
`create T ' dup ,` is `E-MISMATCH` at `,`. `0 set-check` opens no unchecked
window: a `:` definition or a top-level token inside
`check@ 0 set-check … set-check` is checked as outside it, and the data and
return-stack cells it leaves keep their types. Admitted at top level:
`5 V !`, `1 V +!`, `here PV !` with the pointer kept across a `:` definition or
a `require`, and `' X HK !` for `TYPED-VARIABLE HK [ -- ]`.

An integer or an `xt` becomes an address or a quotation only through a private
`CAST:` (`CAST: >BYTES ( n -- ptr u8 )` in a package's private section, else
`E-CAST-MINT`); `X NULL-PTR -` is the address-to-integer distance for any
pointer `X`, with no view: a distance against `NULL-PTR` or `data-base` binds
neither pointee, so `: D ( ptr a -- n ) NULL-PTR - ;` keeps `a` free.
A cast may take an `xt` (`CAST: XT>N ( xt -- n )`) but never yields one
(`E-CAST-CLASS`), and no storage holds one (`E-BAD-STORAGE`, `xt in a stored
type`).
A layout's fields count as the bare type: a cast into a layout whose `FIELD`
holds a pointer or a quotation is the same mint. A cast into another package's
cell family or `OPAQUE` structure is the writer's claim, which the checker does
not prove.

In checked code a `DEFLINEAR` token is minted and erased the same way, by the
package that declared the type: `LINEAR: MINT ( ptr n -- PKG:tok )` and `LINEAR: ERASE
( PKG:tok -- ptr n )` in its private section. Under `public` that is
`E-LINEAR-SCOPE`, as is a qualified `LINEAR:` name in the private section;
in any other package or for a top-level `DEFLINEAR`
`E-LINEAR-OWNER`, and a row that is not one token and one non-linear con (or
pointer to one) `E-LINEAR-PAYLOAD`. `CAST:` refuses a side that may hold a
linear value (`E-CAST-LINEAR`); a quotation whose rows end in different tails
(`[ R -- S ]`) counts as one, except in a cast's input outside a view's element,
where it is any quotation; where the output mints, an input row ending in a row
variable keeps that variable and its cell count in place (`E-CAST-CLASS`). A
type variable only where a type is stored is admitted (`CAST: >ELEM ( ptr u8 --
ptr t )` mints a pointer, so only in a private section); a type bound to it
later that may hold a linear value is `E-CAST-LINEAR`, an atom or a width
`E-CAST-CLASS`, and `is` refuses a callable over it into a cell over any type
(`E-CAST-LINEAR`). Both identity declarers preserve the stack below their
operand: `LINEAR: MINT ( R n -- R PKG:tok )` is valid, but different tails are
`E-CAST-ARITY`.
No storage is declared with a type that may hold a linear value (`E-BAD-STORAGE`,
`stored type may hold a linear value`): a token or a type variable, or a
quotation over a token, a type variable or different tails, at any depth
(`TYPED-VARIABLE HK [ -- PKG:tok ]`, `ptr a`, `[ ptr t ptr t -- ]`,
`[ R -- S ]`). A cell's type is believed at every read, so such a cell would
hand any package the token; a token stays on the stack and a `defer` names
such a quotation.

forth.md: **Structures And Enums**; the rule is forth.md **Raw storage never
holds an address**.

## 6 Errors

- External applications and separately distributed libraries allocate codes in
  `-999999..-100000`, inclusive; Habu's runtime, compiler, bundled libraries,
  tools and tests allocate outside it. Loaded dependencies need nonoverlapping
  blocks: package names do not distinguish thrown numbers. The application
  integrator resolves collisions; the checker does not enforce this convention.
- Library codes are named constants in `src/lib/error.f`; checker throws also use
  positive codes above 255 in their owning source. Codes 0..255 serve as
  process exit statuses and may be shared. Every other code, negative or above
  255, has one `E-` name across the tree; a file that needs it under its own
  name reads the owner's constant (`E-OWNER constant E-LOCAL`). A library owns
  one inclusive block of about a hundred bounded by its own `E-X-FIRST` /
  `E-X-LAST` (arrays, filesystem, strings, …) and
  reserves that whole range whether or not every code is minted;
  `tools/error-code-lint.f` reports a file minting inside another's.
- A code is declared with its text: `-9701 s" the provider refused the key"
  ERROR:CODE E-KEY` defines the constant `E-KEY` and keeps its name, qualified
  by the open package (`MODEL:E-KEY`), and the text in the program's data.
  `ERROR:NAME$` and `ERROR:TEXT$` ( n -- ptr u8 n ) answer them, empty for an
  undeclared code. `src/lib/error.f` states the refusals (CODE, CHECK).
- A block with codes in a package (`JSON:E-READ-SOURCE`) is minted, bounds and
  all, in the file that throws its codes and that every other file throwing
  them requires (`src/lib/json/read.f`, which `src/lib/json/copy.f` requires;
  both reopen package JSON, which `src/lib/json.f` loads), and
  `src/lib/error.f` says where.
  The engine bakes `src/lib/error.f` and every other file of a package it
  bakes, and its build seals the package: loaded source that opens one is
  `E-SEALED-PACKAGE`, rc 84.
- A fallible word `throw`s a named code, never an out-of-band flag.
- A failure caused outside the program (a provider, the network, the OS) is
  logged once, with its text, by the library that saw it, just before it
  throws: `LOG:ERROR s" model" LOG:LINE … LOG:TEXT+ … LOG:;LINE`
  (`src/lib/log.f`), the tag its package name in lower case. `LOG:CODE+`
  appends a code with its declared name and text.
- `throw` is catchable and belongs to the checker's exception edge. `die`
  (`ptr u8 n n --`, a real message and exit code) ends the process and is
  no-return (§ 3). `throw` for recoverable and interactive paths, `die` for
  build makers and CLI boundaries.
- Exit 79 (`$4F`) is compilation from a task: a definition, `create`, `allot`,
  `,`, `evaluate`, a load or a package keyword run on a task's, an exposed
  region's or a callback's thread. The kernel's guard writes
  `E-TASK-COMPILE habu: <primitive> from a task lr <lr>` on fd 2;
  `evaluate`, `evaluate-closed`, a load, `create`, `,`, `c,`, `align` and
  `allot` reach it first, so a definition or keyword in a text is refused
  before the text is read. Calling `INTERPRETER:EVAL-FRAMED`,
  `INTERPRETER:EVAL-FRAMED-RANGE` or `INTERPRETER:REPL` on a task's thread
  writes `E-TASK-COMPILE habu: source read on a task's thread; only the main
  thread compiles`. The main thread compiles while tasks run; `cp!` and
  `ndict!` exit 79 on it too while any task is live, writing
  `E-TASK-COMPILE habu: <primitive> while a task is live lr <lr>`. `NLEASE`'s
  checks throw `NLEASE:E-TASK` on a task's thread.
- `0 throw` throws `ENGINE-ERROR:THROW-ZERO` (109), never the ANS no-op:
  `[: 0 throw ;] catch` answers 109.
- A read or write that faults in Habu code throws `ENGINE-ERROR:MEMORY-FAULT`
  (110) on the faulting thread, after the line `E-MEMORY-FAULT habu: memory
  fault at <address> pc <pc>` on fd 2: `catch` answers 110, a task ends with
  110, an uncaught one exits 110. A fault in C code, a jump out of Habu code
  and a fault while one is delivered still dump and die by the signal.
- Arithmetic is modular and never refuses, with one exception: `/`, `mod` and
  `/mod` throw catchable `E-DIV-ZERO` on a zero divisor;
  `$8000000000000000 -1 /` wraps to `$8000000000000000`.
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
- A file picks its target's file at its top level, `HB-TARGET-MACOS? [if]
  require src/lib/secret/macos.f [then]`, never from inside a word: a build
  refuses a load a word runs (`include: a word loaded or provided a file
  during a build`). The verifier follows a `[if]` only after a target predicate, or one
  and `0=`; `[if]` and `[then]` are reserved. A file that builds for some
  targets only states them on its first code line, before its requires:
  `HB-TARGET-MACOS? HB-TARGET-LINUX-KERNEL? or ?TARGET`. On another target
  the load stops there, `E-TARGET habu: <path>:<line>: the file does not
  build for <target>` (the seam's `HB-TARGET$`), throw 74. A file that
  requires it states nothing.
- A loaded file is a closed program: its top level starts at `depth` 0, a
  token reaching its loader's cells throws a stack underflow, a file that ends
  with cells on the stack is `E-EVAL-RESIDUE`, and one that ends inside a
  definition it opened is refused there, as every source is. A value crosses a load
  only as a word the file defines. `here` is cell-aligned when a loaded file
  starts and when a `require` or `include` that ran a file returns: data never
  spans a load.
- The engine provides `src/lib/prelude.f`, `error.f`, `string.f`, `span.f`,
  `memory.f`, `num-types.f`, `num-arithmetic.f` and the
  `src/` files its boot prefix loads: their words resolve with no require and
  a `require` is a no-op, and `tools/check.f` answers `E-ENGINE-PROVIDED` for
  one (rebuild `bin/hb` to check a change to it). Write it
  anyway: a file states its dependencies.
- Multi-file packages reopen `package NAME` per file; reopening shares scope and
  loads nothing. Run tools as `bin/hb --load src/lib/a.f tool.f -- args`;
  `SCRIPT-ARGV$` starts after `--`.

forth.md: **Files**, **Packages**.

## 8 Tests

What to test: [test/AGENTS.md](../test/AGENTS.md) "Writing tests". Use
`require src/lib/test.f` for assertions and keep fixtures package-scoped.

`T=` / `T<>` scalars, `T$=` strings, `TTRUE` / `TFALSE` flags, `TTHROWSQ` /
`TTHROWS` throw codes. `TEST-EVAL:N` / `FLAG` / `RC` take one cell, a flag or
the throw code out of a source text. Register it as a `SUITE name … ;SUITE`
entry in `test/gate-stdlib-cases.f`; `bin/hb --load test/run.f` runs every
suite.

forth.md: **Testing**, **Verification before committing**.

## 9 Comments

- A `\` comment is one or two lines and says only what the code does not
  show: units, an ownership or ordering invariant, a hardware encoding, a
  checker boundary, why a non-obvious choice is required. One that says what
  the code says is deleted. Design belongs in `docs/`.
- A complicated algorithm is described once, in its file's header.
- It states what is true now. No history: no "used to", "no longer", earlier
  designs, what was tried, dates, dot, commit or review ids, agent or person
  names. A failure behind a rule is a present fact: "a stage without X dies
  `E-UNDEFINED` at Y".
- A fact lives in one comment, on the code that owns it. No other comment
  repeats it or points at another file's word for it: nothing checks the
  name.
- An edit that touches a comment, a conflict merge or a moved definition
  included, brings it to these rules; a change that makes a comment false
  corrects it in the same change.

## 10 Open forth.md at this heading when …

| heading | open it when |
|---|---|
| Checked code and primitive boundaries | `TRUST`, a new `PRIM:`, what `evaluate-closed` checks |
| Naming | a collision, reserved names |
| Packages | reopening, include vs require, search roots and `SOURCE-ROOT` |
| Importing … with `using` | ambiguity, scope end, the limit |
| Structures And Enums | `NEWTYPE`/`SUMTYPE`/`PRODUCT`/`ENUM`, `construct`, `MATCH`, `CAST:`, `LINEAR:`, `FSM:DEFINE` |
| Words & factoring | word size, argument limits, splits |
| Files | one concern per file, script argv |
| Stack comments | the token list, real types |
| Stack effects | the effect grammar, the return-stack clause, types, widening, `DEFTYPE`/`DEFLINEAR`, pointers, escape hatches |
| Checker & type model | loop frames, higher-order effects, `defer` |
| Owners and views | C2 owners, `read-view`/`mut-view`, scope binders |
| Errors | the `ENGINE-ERROR` ABI, `die` divergence |
| Integer arithmetic | the wrapping contract, the signed bounds |
| Engine limits … | the REPL-line, `[:` and control-flow nesting, effect-depth, input-row and data-space ceilings; a definition's text and name have none |
| Constants | hex versus decimal, where codes live |
| Testing | assertions, a suite example, runner and property rules |
| Diagnosing a checker miss | find the layer that is wrong |
| Verification before committing | where the gates are |
| Comments & hygiene | debug prints, a refused definition's report |
| Habu Native Tooling Gotchas | debugger, xref, `DYNAMIC-BUFFER`, `ARENA`, `HEAP`, alignment, absent words, escapes |
| Native Forth Gotchas … | compile-only words, `case`, `parse-name` |
