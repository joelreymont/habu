# habu — Checked Forth: Implementation Plan

A complete checked Forth, SELF-HOSTED: bin/hb compiles its own source (0.2 s,
type-checking itself as it goes) and reproduces itself byte-for-byte — see
tools/build.sh. No-binary recovery installs a trusted native seed with
tools/seed.sh and immediately rebuilds current source. Checked code is ordinary Forth that fails to
compile unless its body's inferred stack effect unifies with its declared effect. Background notes (a chat session, not authoritative on
scope): `~/Downloads/forth_checked_stack_session_history.md`.

## Goal & decisions (locked)

A **complete** checker with **full row-polymorphic unification** — type vars +
first-class row vars over **both data and return stacks**, let-polymorphic
generalization, quotations/combinators/iterators, and **all of Forth**.
Constructs whose effect is not statically inferable (`CREATE … DOES>`,
`POSTPONE`, `EVALUATE`, host primitives, FFI) are admitted via **trusted effect
annotations**: the body is not inferred, the declared effect is trusted, and
every call site is checked against it. **Real self-host**: the checker's own
sources are written in the checkable subset (with trusted annotations where they
metaprogram) and re-checked by the checker.

| Area           | Choice                                                          |
| -------------- | -------------------------------------------------------------- |
| Seed           | Trusted native `hb` seed via `tools/seed.sh`, then `tools/build.sh` fixpoint |
| Inference      | HM-style: type vars + row vars, mutually-recursive occurs, union-find subst, **let-generalization on store** |
| Effects        | **Four rows** per effect: data-in, data-out, return-in, return-out |
| Higher order   | `xt`≡`quot<E>` unified; quotations, combinators, iterators     |
| Escape hatch   | **Trusted annotations** for un-inferable / host / metaprogramming words |
| Host           | Self-hosted in Forth; override `:`; words UPPER-CASE, hyphens (`docs/forth.md`) |

## The type system

- **Types** `τ`: type var `α`; concrete code (`i64 u8 u32 cell bool char str
  addr`); `ptr<τ>`; quotation `quot<E>` (carries a full effect; `xt` is the same
  type — a ticked word becomes `quot<E>` from its DB effect). `str` = a string
  body (`c-addr u` pair handled as one logical value); `addr` = raw address.
- **Stacks (rows)** `S`: a row var `ρ`, or a push `S , τ`.
- **Effect** `E = ( Din Rin -- Dout Rout )` — four stacks. Surface notation
  writes the data part `( in -- out )` and an optional `| rin -- rout` return
  clause; words that don't touch the return stack share one fresh `ρr` across
  `Rin`/`Rout`.

Examples: `DUP ( ρ a -- ρ a a )`; `+ ( ρ i64 i64 -- ρ i64 )`;
`@ ( ρ ptr<a> -- ρ a )`; `>R ( ρ a | σ -- ρ | σ a )`;
`EXECUTE ( ρ q | σ -- ρ' | σ' )` where `q = quot<( ρ | σ -- ρ' | σ' )>` (the
quotation's four rows are threaded, **not** assumed return-pure);
`DIP ( ρ a q | σ -- ρ'' a | σ' )` with `q = quot<( ρ | σ -- ρ'' | σ' )>`.

## Unification (the core)

Three mutually-recursive unifiers over a union-find substitution (separate id
spaces for type vars and row vars), all occurs-checked:

- **UNIFY-TYPE(τ1,τ2)** — resolve both; equal → done; var vs τ → occurs then
  bind; `con/con` equal else `E-MISMATCH`; `ptr/ptr` → inners; `quot/quot` →
  UNIFY-EFFECT; else `E-MISMATCH`.
- **UNIFY-ROW(S1,S2)** — resolve both; equal → done; `ρ` vs `S` → row-occurs then
  bind `ρ:=S` (this is where a row binds to `rest ++ elems`); `push/push` →
  UNIFY-TYPE tops then UNIFY-ROW rests.
- **UNIFY-EFFECT(E1,E2)** — UNIFY-ROW each of the four stacks.

**Occurs is one mutually-recursive walk** (`OCCURS-TYPE`/`OCCURS-ROW`) descending
`ptr→inner`, `quot→effect→all four rows`, `push→(rest, top-type)`: a type var
must not occur in a `ptr`/`quot` it binds to; a row var must not occur in any
stack/effect — **including inside a quotation's rows**. A self-applying quotation
must be rejected with `E-OCCURS`, never loop. The native checker implements the
deep quotation-aware walk; keep omega-quotation regressions in the gate.

**Schemes are canonical signature strings** (persistent; the per-check arena is
wiped each definition, so terms can't be stored). This makes the two polymorphism
operations reuse the parser + renderer:

- **INST = `PARSE-SIG`** on the scheme string: the parser allocates **fresh** type
  and row vars by name as it builds the effect in the per-check arena, so every
  call site / quotation use gets an independent instantiation — polymorphism for
  free.
- **GENERALIZE = `RENDER-EFFECT`**: after a definition's final unify, render the
  resolved effect to canonical text (free vars named `a b…`/`R S…` by first
  appearance) and store that string in the DB. Two differently-typed calls then
  re-parse to fresh vars and both check.

So no cross-arena copy and no separate scheme record: the DB holds strings,
`RENDER-EFFECT` (in `sig.fs`, shared with diagnostics) is the printer, `PARSE-SIG`
is the reader/instantiator.

## Checking pipeline

Checker state is the **full current effect** `( In | Rin -- Dcur | Rcur )`
(four rows). To apply a token whose fresh effect is `( a | ar -- b | br )`:
`UNIFY-ROW(Dcur,a)`, `UNIFY-ROW(Rcur,ar)`, then `Dcur:=b`, `Rcur:=br`. Underflow
is not special — a too-short application binds the row tail; a real underflow
surfaces when the inferred effect is unified with the **required explicit
declared effect** (a checked `:` must declare its effect).

Token classification order: control word → structured handler; parsing/quoting
word (`' ['] S" ." C" CHAR [CHAR] LITERAL` …) → its parsing rule; number →
literal `( ρ -- ρ i64 )`; defining word (`CONSTANT VARIABLE CREATE …`) → defining
rule; effect-DB word → apply (INST then compose); FORBIDDEN-without-annotation →
`E-UNSAFE`; defined-but-uncharted → `E-UNCHECKED`; else `E-UNKNOWN`.

- **Control** — `IF/ELSE/THEN`: pop `bool`; snapshot the four-row effect; check
  THEN from the snapshot → `E1`; check ELSE (empty = identity on all four rows)
  → `E2`; UNIFY-EFFECT(E1,E2) (`E-BRANCH`); continue unified. `BEGIN…UNTIL`,
  `BEGIN…WHILE…REPEAT`, `BEGIN…AGAIN`, `DO/?DO…LOOP/+LOOP`: snapshot at the loop
  top; thread the per-construct equations (`WHILE`/`UNTIL` consume a top `bool`;
  `?DO/DO` consume `( limit index )`; `+LOOP` consumes an increment); the
  back-edge must `UNIFY-EFFECT` the snapshot with the post-iteration effect over
  **both** data and return rows — net growth on either is a row-occurs failure
  reported `E-LOOP`. `I J` push the loop index `( ρ -- ρ i64 )`; `LEAVE UNLOOP`
  handled; `EXIT` unifies the current effect with the declared output; `RECURSE`
  uses the current definition's **declared** effect.
- **Locals** — `{ a:u8 b -- }` / gforth `{: a b :}`: pop named inputs (a typed
  name asserts its type), open a scope where each name use pushes its type; scope
  ends at `;`.
- **Quotations & higher order** — `[: … ;]` checks its body to a generalized
  effect `Eq` and pushes `quot<Eq>` `( ρ -- ρ quot<Eq> )`. `' NAME`/`['] NAME`
  push `quot<E>` built from NAME's DB scheme (xt≡quot). `EXECUTE` and combinators
  (`DIP KEEP BI TRI`) consume `quot<…>` inputs with their four rows threaded.
  **Iterators** over a collection: an array iterator
  `EACH ( ρ ptr<a> u quot<( σ a -- σ )> -- ρ )`, `MAP`, `FOLD` with explicit
  element-type effects (closed signatures in the primitive table); a counted
  combinator `TIMES ( ρ u quot<( σ -- σ )> -- ρ )`. Each combinator/quotation use
  `INST`s the quotation effect fresh (so `BI`'s two quotations are independent).
  Native call sites are checked today; `DIP/KEEP/BI/TRI/TIMES/EACH/MAP/FOLD`
  live in `src/core/combinators.f` with `TRUST`ed public schemes. Native iterator
  schemes use parametric `ptr<a>` and preserve the element type across `@`/`!`.
- **Return stack** — `>R R> R@ 2>R 2R> 2R@` move types between the data and
  return rows; checked by the same four-row composition.

## Trusted annotations & defining words

- **Trusted annotation** — `TRUSTED: NAME ( eff ) … ;` records `eff` as NAME's
  DB scheme **without inferring the body**; call sites are checked normally.
  `s" name" s" eff" TRUST` annotates an already-defined/native word. This is the
  escape hatch for metaprogramming, host primitives, and FFI.
- **`CONSTANT` / `VARIABLE`** — defining words: `n CONSTANT NAME` charts
  `NAME : ( ρ -- ρ i64 )`; `VARIABLE NAME` charts `NAME : ( ρ -- ρ ptr<cell> )`.
- **`CREATE … DOES>`** — `TRUSTED: NAME ( definer-eff ) CREATES
  ( created-eff ) … ;` records `definer-eff` for the defining word and records
  `created-eff` for every word produced by runtime `CREATE` while that definer
  runs. A `DOES>` body inside a trusted definer requires `CREATES`; without it,
  definition fails. With `created-eff = ( in -- out )`, the native checker verifies
  the `DOES>` body as `( in ptr a -- out )`, because the created word pushes its
  data-field pointer before entering the body. Use typed pointer stepping
  (`cell+`, `count`, typed memory words), not raw integer `+`.
- **`POSTPONE IMMEDIATE COMPILE, EVALUATE`** — compiler-manipulating; only
  admitted inside a `TRUSTED:` word, else `E-UNSAFE`.

## Signature & surface syntax

**Case-insensitive**, like Forth: word and type-name matching ignores case
(`DUP`=`dup`, `I64`=`i64`; via `find-name`/`search-wordlist`/`CI=`). The sole
case-meaningful element is the **single-letter signature var** — lower-case = type
var, upper-case = row var — unambiguous because type names are ≥2 chars.

**Grammar** `( in -- out )` (+ optional `| rin -- rout`): type names
`i64 u8 u32 cell bool char str addr`; `ptr< τ >`; quotation `[ in -- out ]` →
`quot<…>`; type vars `a b c …` (normalized by first appearance); row vars
`R S …` (one leading row var implicit if omitted). One parser builds normalized
schemes; serves primitives, `TRUSTED:`, and user `:` (user sigs may be
polymorphic). A checked `:` **must** carry an effect.

**Override `:`** — save native colon as `NATIVE:`. New `:`: (1) `RE-EMIT?`
reentrancy → delegate to `NATIVE:`; (2) `PARSE-NAME` the name; (3) parse the
effect ourselves; (4) **capture the structured body**: a `parse-name` loop that
(a) tracks nesting of `[: … ;]`, `{ … }`/`{: … :}`, and control words to find the
matching top-level `;`, and (b) **consumes the delimited span of every parsing
word** — native today consumes `(  \  S"  C"  ."  CHAR  [CHAR]` spans; the
gforth-tier/full-Forth capture target also includes `S\"` and `.(` — via
`parse`/`>in` so an embedded `;` inside a string/comment never terminates
capture (a pure `parse-name` loop is provably wrong here). Copy each token's bytes out
immediately (a `parse-name` address dies at the next parse); (5) run the checker
inside `CATCH`; (6) on success set `RE-EMIT?`, re-`EVALUATE` `: NAME body ;`,
generalize + record the effect; (7) on failure format the diagnostic, define
nothing. (Verified in 0.7.9: native `:` non-immediate, `RECURSE` survives
re-EVALUATE, nesting-aware capture, quotations/locals round-trip. Avoid the name
`NT` — a gforth built-in.)

## Primitive table (closed checklist)

Authored through the signature parser at startup. Must include: `DUP DROP SWAP
OVER ROT -ROT NIP TUCK 2DUP 2DROP 2SWAP 2OVER`; `+ - * / MOD /MOD AND OR XOR
INVERT LSHIFT RSHIFT NEGATE ABS MIN MAX` (`i64`); `0= 0< = <> < > <= >=`
(`= : ( ρ a a -- ρ bool )`, comparisons `( ρ i64 i64 -- ρ bool )`); `@ ! +!
c@ c!` and pointer arithmetic `CELL+ CELLS CHAR+ CHARS` and `+`/`-` overloads for
`( ρ ptr<a> i64 -- ρ ptr<a> )`; `HERE ALLOT , C,`; `>R R> R@ 2>R 2R> 2R@`;
`I J` (in loop scope); `EXECUTE DIP KEEP BI TRI`, `EACH MAP FOLD TIMES`;
`CHAR [CHAR]` (`char`), `S" C" ." TYPE COUNT`; `. U. EMIT CR SPACE DEPTH`. Comparisons
yield `bool`; `IF/WHILE/UNTIL` require `bool` (a flag-producing word must yield
`bool`). Naked `?DUP` is value-dependent and remains deliberately untypeable;
use the checkable `?DUP-IF` idiom instead (see `docs/effects.md`).
Time primitives are `EPOCH-SECONDS ( -- n )` for UTC Unix seconds and `MONO-NS
( -- n )` for monotonic benchmarking. Shared checked UTC date formatting and
parsing lives in `tools/date.f` (`PARSE-YMD`, `FORMAT-YMD`,
`FORMAT-EPOCH-UTC`) for lints, benchmark run IDs, and JSONL metadata.

## Diagnostics (first-class — the LLM repair interface)

Canonical template (milestone tests assert it verbatim):
```
In WORD:
  declared: ( <eff> )
  at token: <tok>
  expected: <stack>
  actual:   <stack>
  repair_class: <machine-readable class>
  <one-line reason>   (E-CODE)
```
Stacks render bottom→top with the row var as `R`/`S`, type vars `a b …` by first
appearance, quotations `[ in -- out ]`, `ptr<…>`. Resolve deeply before printing.

## Error model

THROW codes (`config.fs`): `E-UNDERFLOW E-MISMATCH E-ARITY E-UNKNOWN E-UNSAFE
E-UNCHECKED E-OCCURS E-BADTYPE E-DEPTH E-TOOMANYVARS E-ARENA E-BRANCH E-LOOP
E-QUOT E-LOCAL E-RECURSE`. A global diagnostic record `{ word, token, expected,
actual, detail }` is filled before each THROW (storage in `diag-state.fs`, loaded
before its writers `unify`/`checker` and reader `diag`).

## Interface contract (pinned — parallel agents target this, not each other)

Every cross-file word, with its stack effect. Forward-declared in **`forward.fs`**
(loaded right after `config`) via `defer`, filled by the owning file.

- Type terms (`types.fs`): `MK-CON ( code -- t )` `MK-VAR ( id -- t )`
  `MK-PTR ( inner -- t )` `MK-QUOT ( eff -- t )` `TERM>TAG ( t -- tag )`
  `TERM>PAYLOAD ( t -- n )` `TYCON? ( t -- f )` `TYVAR? ( t -- f )`
  `RESOLVE-TYPE ( t -- t' )`; `TV@ TV! TV-RESET TV-ALLOC ( nv -- base )`.
- Stack terms (`rows.fs`): `MK-ROW ( id -- s )` `MK-PUSH ( rest top -- s )`
  `SROW? SPUSH?` `STACK-REST ( s -- s' )` `STACK-TOP ( s -- τ )`
  `RESOLVE-ROW ( s -- s' )`; `RV@ RV! RV-RESET RV-ALLOC ( nr -- base )`.
- Mutually-recursive seams (`forward.fs` `defer`, filled in `unify.fs`):
  `OCCURS-TYPE ( id t -- f )` `OCCURS-ROW ( id s -- f )`.
- Effects (`effects-repr.fs`): `MK-EFFECT ( din dout rin rout -- e )`
  `EFF>DIN EFF>DOUT EFF>RIN EFF>ROUT ( e -- s )`.
- Unify (`unify.fs`): `UNIFY-TYPE ( t t -- )` `UNIFY-ROW ( s s -- )`
  `UNIFY-EFFECT ( e e -- )`.
- Render (`render.fs`): `RENDER-EFFECT ( eff -- c-addr u )` (=GENERALIZE:
  canonical text, vars named by first appearance; shared with diag).
- Sig parse (`sigparse.fs`): `PARSE-SIG ( c-addr u -- eff )` (=INST: fresh vars
  by name per call).
- DB (`db.fs`): `CHART ( eff c-addr u -- )` (renders + stores the scheme string
  under a name) and `EFFECT-OF ( c-addr u -- scheme$a scheme$u | 0 )`.
- Primitives (`prims.fs`): authors the primitive effect table via PARSE-SIG+CHART.
- Diag state (`diag-state.fs`): `DIAG! ( word$ tok$ exp act detail -- )` plus
  field getters; `CUR-WORD! CUR-TOKEN!`.
- Checker hooks (`forward.fs` `defer`, filled by `control`/`locals`/`quots`):
  `CHECK-CONTROL ( c-addr u -- f )` `CHECK-LOCAL ( c-addr u -- f )`
  `CHECK-QUOT ( c-addr u -- f )` (each: handle the token if it owns it, return
  flag; never edit `checker.fs`; `IS` from their own file). Checker exposes
  `DCUR RCUR` (current rows) and `APPLY-EFFECT ( scheme -- )` for them to use.

## File layout & load order

The NATIVE toolchain the self-hosted engine compiles lives in
`src/{core,arch/arm64,habu,os/macos}`; its built-in checker
(`src/core/checker.f`) covers the engine's full compile surface (control flow,
return row, typed locals, quotations + typed execute, trust) — the toolchain
self-checks clean (see `STATUS.md`). The native sig grammar has named row vars,
quotation sub-sigs, and scheme-string recording of quot-bearing sigs
(combinator call sites check against them). Native now has distinct concrete types
(n = generic int, subsumes the widths), the `| rin -- rout` return clause, and
nested quotations and native parametric `ptr a`. Bracketed quotation signature
syntax is data-stack-only today; actual quote terms still thread return rows
through `execute`. See CODEGEN-PLAN.md for the engine side.

```
habu/  AGENTS.md LESSONS.md PLAN.md README.md .gitignore  docs/forth.md
  src/core/  checker render util sha256 combinators
  src/arch/arm64/  asm icode mnem disasm
  src/habu/  habu1 habu2 jit regalloc rt crash prof repl snap stage2
  src/os/macos/  sys env macho sign2
  test/ run.sh hb-suite.f prop-test.f proc-pty.f
```
Each file is one concern (no file bundles unrelated responsibilities — see
`docs/forth.md`). Load order: `config forward arena types rows effects-repr
diag-state unify render sigparse db prims diag checker control locals quots
defining capture colon`. `forward` pins every seam; `types`→`rows` (a push stores
a type top); `unify` fills the occurs seams; `render`∥`sigparse` (independent),
`db` needs `render`, `prims` needs `sigparse`+`db`; `checker` exposes
`DCUR`/`RCUR`/`APPLY-EFFECT` and the hook `defer`s, which
`control/locals/quots/defining` `IS` from their own files; `capture` is a
standalone parsing-aware lexer used by `colon`.

## Milestones (build order — all in final scope)

1. **Done** — Gforth 0.7.9 built.
2. **config + forward** — constants/tags/codes; `defer` all seams (occurs, hooks)
   with pinned effects. Tests: constants present; defers callable (stubbed).
3. **arena** — bounds-checked heap. Tests: alloc/reset/overflow→`E-ARENA`.
4. **types** — type terms (`T-CON/VAR/PTR/QUOT`), type-var store, `RESOLVE-TYPE`,
   shallow `OCCURS-TYPE` calling the deep seam. (Rewrites the stub.) Tests: term
   round-trip, resolve chains, shallow occurs.
5. **rows** — stack terms, row-var store, `RESOLVE-ROW`, shallow `OCCURS-ROW`
   seam. Tests: build/resolve stacks, shallow occurs. (Serial after types.)
6. **effects-repr ∥ diag-state** — effect nodes + readers; diagnostic record API.
7. **unify** — three unifiers, **deep mutually-recursive occurs** (fills seams),
   `INST`, `GENERALIZE`. Tests: row binds to rest++elems; quot/quot; **ω-quotation
   → `E-OCCURS`, no loop**; generalize→two diff-typed instantiations; NIP≡SWAP DROP.
8. **render ∥ sigparse → db → prims** — `RENDER-EFFECT` (canonical text) and
   `PARSE-SIG` (fresh vars by name) are independent; `db` (`CHART`/`EFFECT-OF`)
   needs render; `prims` (the primitive table) needs sigparse+db. Tests: parse
   round-trip incl. higher-order + return-clause; parse→render→parse structural
   equality; DB lookup; closed primitive checklist present.
9. **checker (core)** — four-row composition, literals, word application,
   classification, FORBIDDEN, `CHECK-DEF`, final unify + `GENERALIZE`-on-store.
   Tests: `DUP *`, `SQUARE`, `HYP2`; **two diff-typed calls of a poly def**
   (generalization regression); real underflow → fail; mismatch; `>R R>` round
   trip (return-row regression).
10. **diag** — canonical formatter + var/row naming. Tests: exact text per code.
11. **capture → colon** — `capture` is the parsing-aware structured body lexer
    (nesting of `[: ;]`/`{ }`/control; consume `( \ S" ." …` spans), tested
    standalone; `colon` is the `:` override using it: re-emit,
    `CATCH`. Tests: defs (incl. `IF`, `[: ;]`, locals, `S"`) compile **and run**;
    bad→`E-ARITY`/underflow rejected and not defined.
12. **control** — `IF/ELSE/THEN`, all loops, `I J LEAVE UNLOOP EXIT RECURSE`.
    Tests: branch balance, loop stability (data+return), recursion.
13. **locals** — typed `{ … }`/`{: :}` scope. Tests: typed assert, name pushes,
    scope ends at `;`.
14. **quots + combinators + iterators** — `[: ;]`, `' ['] (xt≡quot)`, `EXECUTE
    DIP KEEP BI TRI EACH MAP FOLD TIMES`. Tests: quotation typed+run; combinator
    compose; `BI` two-different-quot-types; iterator element typing; mismatch
    rejected.
15. **defining + trusted** — `TRUSTED:`, `CONSTANT VARIABLE CREATE…DOES>`,
    `POSTPONE…` only inside `TRUSTED:`. Tests: trusted charts effect; constant/
    variable charted; created word typed; bare `POSTPONE`→`E-UNSAFE`.
16. **self-host + polish** — annotate the checker's own sources where they
    metaprogram and **re-check them with the checker**; `test/all.fs`
    failure-counting runner; `examples.fs`; `README`.

## Test strategy

Vendored Hayes `T{ … -> … }T`. Pass cases compile and run with the expected
runtime effect; fail cases `CATCH` the **specific** code and match a diagnostic
substring. `test/all.fs` owns the exit code (a failure counter `(bye)`ing
nonzero) — `gforth -e bye` exits 0 on a failed `T{}T`. Per `docs/forth.md`.

## Known hard parts

- The occurs/resolve mutual recursion through quotations is the core risk —
  build and test it (ω rejection) before any higher-order checking.
- Generalization-on-store + per-check id reuse: copy-out-and-renumber is
  mandatory; the two-diff-typed-call test guards it.
- `CREATE…DOES>` / `POSTPONE` are not inferable — they exist only behind
  `TRUSTED:`; self-host leans on this for the checker's own metaprogramming.
- Four-row composition must thread the return stack uniformly, including through
  `EXECUTE` of a quotation that itself uses `>R`.
