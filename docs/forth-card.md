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
  global also owns is `E-USING-SHADOW-GLOBAL` (rc 67) — rename the public.
- `EXPORT NAME` in a public section re-exports an existing word under its own
  tail: same xt, same effect, no body.
- A wordlist is a no-duplicate set, case-insensitively: a second `: R` is
  `E-DUPLICATE-DEFINITION` (rc 78); `undefine R` first to replace one.
- Compiler keywords (`I`, `DO`, `IF`, …) cannot be definition names:
  `E-RESERVED-DEFINITION`. A number-shaped name (`: 42`) is refused by
  `tools/check.f` (`E-NUMERIC-DEFINITION`), not by `--load`.

forth.md: **Naming**, **Packages**, **Importing … with `using`**.

## 2 Effects, locals, quotations

Every definition carries `( before -- after )` and the checker reads it as the
signature. Type tokens only (`n`, `u8`, `bool`, `xt`, `ptr a`, `ptr u8`, `idx`,
`len`, `fd`, `rc`), never role prose like `( got want -- )`.

- A string is `ptr u8 n`, a cell address `ptr a`, `n` only a genuine scalar.
  `ptr a` needs a body that keeps the pointee parametric; read the cell as a
  number and declare `ptr n`, or it is `E-NONPARAMETRIC-EFFECT`. `ptr u8` is a
  byte span — `c@`/`c!`; cell `@` on it is `E-MISMATCH`.
- Integers widen when lossless (`u8 → u16 → u32 → n`); roles (`idx`, `len`,
  `fd`) never widen. Booleans are real `bool`s: `0 0=`, never a raw `0`/`-1`.
- Locals `{: a b:ptr :}` bind left to right from the deepest item, so
  `1 2 {: a:n b:n :}` gives `a`=1. A local binds **once**: a per-turn value
  lives on the stack or in a cell. Names are at most 16 bytes, 64 per
  definition, block-scoped, bindable after a closed early-exit guard.
- **A `{: p:ptr :}` local admits cell `@` and `!`**. The restriction is on
  the **declared** pointee:
  `: F ( ptr a -- n ) {: p:ptr :} p @ ;` is `E-NONPARAMETRIC-EFFECT` because `a`
  is specialised, while `( ptr n -- n )` certifies.
- Bind multi-cell values whole: `{: p :}` or `{: r:res<n,n> :}` (arity checked).
  Destructure only to compute; pass the whole local between words.
- Quotations `[: … ;]` are xts, not closures. The token `[ in -- out ]` works as
  a parameter, a `TYPED-VARIABLE` or a `TYPED-BUFFER` element, never a
  `FIELD` (`E-TDECL-SYNTAX`):
  `: A ( n [ n -- n ] -- n ) execute ;` certifies and `2 [: 1 + ;] A` runs. A
  quotation may not touch an enclosing local (`E-BAD-LOCAL-SHAPE`, rc 75) —
  pass the value on the stack or through a cell.

forth.md: **Stack comments**, **Words & factoring**.

## 3 What the checker refuses, and what it admits

Refused; every row measured, code from `tools/check.f --json-errors`.

| you write | diagnostic |
|---|---|
| `evaluate` in a checked body (top level is fine) | `E-UNSAFE` |
| `variable V  : F ( n -- n ) V ! V @ @ ;` | `E-RAW-CELL-PTR` — § 5 |
| `variable V  : F ( -- ) V @ execute ;` | `E-EXEC-OPAQUE-XT` |
| `@` on a `ptr u8` | `E-MISMATCH` — use `c@` |
| an `if` arm or loop body that changes depth | `E-MISMATCH` at `then`/`repeat` |
| a local read or declared inside `[: … ;]` | `E-BAD-LOCAL-SHAPE`, rc 75 |
| a second `[:` while one is open | **rc 75, bare `[:` on stderr, no code** |
| a non-preserving `[: G ;] catch`; `i`/`leave` outside a loop; `exit` in a loop without `unloop` | `E-REJECTED` |
| `exit` after a word ending in `die` | `E-DEAD-CODE` |
| `: I ( -- ) ;` | `E-RESERVED-DEFINITION` |
| `\` comment in a `STRUCTURE`/`ENUM` body | `E-BAD-DECLARATION`, rc 67 |
| `( -- ptr a )` for a `variable` | `E-NONPARAMETRIC-EFFECT` |
| a multi-cell value at the prompt | `hb: interpret-mode layout value: NAME` |
| a bare `using` import a global also names | `E-USING-SHADOW-GLOBAL`, rc 67 |
| a duplicate tail in one wordlist | `E-DUPLICATE-DEFINITION`, rc 78 |

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

Admitted and measured, the ones worth doubting: `tuck`, `+!`, `unloop exit`,
`>r r@ r> 2>r 2r>`, `RECURSE`, `['] W catch`, `finally`, `defer W ( n -- n )`
plus `[: IMPL ;] is W`, `parse-name`, `MATCH … ;MATCH`, `undefine`, and
`true false 0<> fdup` with no require. An `endcase` default arm producing a
value must leave the selector on top (`30 swap endcase`); `0 0 do … loop` runs
once, `0 0 ?do … loop` zero times.

forth.md: **Checker & type model**, **Native Forth Gotchas …**.

## 4 Storage definers

Every form loaded and its accessor effect certified.

| form | for | accessor |
|---|---|---|
| `variable V` | a raw scalar, role or atom cell | `V @` / `V !`; `( -- ptr a )` is not declarable |
| `create B 256 allot` | static dictionary storage | `B`, pointee bound by the use (`B 4 type`, `B @`) |
| `PTR-VARIABLE P` | a global slot holding an address | `( -- ptr ptr a )`; callers declare the pointee: `: F ( -- ptr u8 ) P @ ;` |
| `TYPED-VARIABLE V t` | one typed cell or record | `( -- ptr t )` |
| `TYPED-VARIABLE V ptr t` | a cell holding an address of `t` | `( -- ptr ptr t )` |
| `n TYPED-BUFFER TB t` | a fixed array of `t` | `( n -- ptr t )` |
| `n PTR-U8-TABLE TT` | a fixed table of byte pointers | `( -- ptr ptr u8 )`, indexed with `ptr-field` |
| `DYNAMIC-BUFFER DB t` | a growable mapped array | `( n -- ptr t )` plus `DB-RESERVE` / `DB-RELEASE`; growth moves it — keep indices, reacquire pointers, release before an image save |
| `n LAYOUT-BUFFER LB fam` | capacity for a declared family | `( n -- ptr fam )` |
| `STRUCTURE p 0 FIELD x n … ;STRUCTURE` | a by-value record, at most 32 cells | `P:MAKE` / `P:UNMAKE`; under `package PKG` the tail is `PKG-P:MAKE`, hyphens doubled |

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
variable V                                \ E-RAW-CELL-PTR at the fetch,
: PEEK ( n -- n ) V ! V @ @ ;             \ repair class declare_pointer_cell

PTR-VARIABLE V                            \ the declared twin certifies
: PEEK ( -- ptr u8 ) V @ ;
```

The declared forms are the `PTR-*` and `TYPED-*` rows of § 4. Such a cell takes
the address it was declared for and refuses the address *of* one.

forth.md: **Structures And Enums**; the rule and its open hole are
`docs/effects.md` "Raw storage never holds an address".

## 6 Errors

- Codes are named constants in `lib/errors.f`. A library owns one inclusive
  block of about a hundred bounded by its own `E-X-FIRST` / `E-X-LAST` (arrays
  `-2000`, filesystem `-2100`, strings `-2200`, …) and reserves that whole
  range whether or not every code is minted;
  `tools/error-code-lint.f` reports a file minting inside another's.
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
  path resolves against the `--load` entry's directory, then the working
  directory, so `require lib/…` names the tree root. Every file requires its
  **own** dependencies.
- The engine provides `lib/prelude.f`, `errors.f`, `string.f`, `memory.f`,
  `num-types.f`, `num-arithmetic.f`, `image-lifecycle.f` and every `src/` file:
  their words resolve with no require and a `require` is a no-op. Write it
  anyway: a file states its dependencies.
- Multi-file packages reopen `package NAME` per file; reopening shares scope and
  loads nothing. Run tools as `bin/hb --load lib/a.f tool.f -- args`;
  `SCRIPT-ARGV$` starts after `--`.

forth.md: **Files**, **Packages**.

## 8 Tests

`require lib/test.f`, keep the fixture in its own package, assert, report.

```forth
require lib/test.f
package CARD-TEST
: MAIN ( -- ) T-RESET  2 2 + 4 T=  [: BOOM ;] -2000 TTHROWSQ  T-REPORT ;
MAIN
;package
```

`T=` / `T<>` scalars, `T$=` strings, `TTRUE` / `TFALSE` flags, `TTHROWSQ` /
`TTHROWS` throw codes. Register it as a `SUITE name … ;SUITE` entry in
`test/gate-stdlib-cases.f`; `bin/hb --load test/run.f` runs every suite.

forth.md: **Testing**, **Verification before committing**.

## 9 Open forth.md at this heading when …

| heading | open it when |
|---|---|
| Checked code and primitive boundaries | `TRUST`, a new `PRIM:` |
| Naming | a collision, reserved names |
| Packages | reopening, include vs require |
| Importing … with `using` | ambiguity, scope end, the 16 limit |
| Structures And Enums | `NEWTYPE`/`SUMTYPE`/`PRODUCT`/`ENUM`, `CAST:` |
| Words & factoring | word size, argument limits, splits |
| Files | one concern per file, script argv |
| Stack comments | the token list, `DEFTYPE`, `DEFLINEAR` |
| Checker & type model | loop frames, higher-order effects, `defer` |
| Errors | the `ENGINE-ERROR` ABI, `die` divergence |
| Integer arithmetic | the wrapping contract, `MIN-N` |
| Engine limits … | 8000-byte body, 255-byte line, 28 `begin` |
| Constants | hex versus decimal, `src/config.fs` |
| Testing | groups, hooks, runner rules |
| Diagnosing a checker miss | find the layer that is wrong |
| Verification before committing | what to rebuild and run |
| Comments & hygiene | comment style, debug prints |
| Habu Native Tooling Gotchas | debugger, spawn and env, snapshots |
| Native Forth Gotchas … | compile-only words, `case`, `parse-name` |
| ptr locals and cell access | never; see § 2 |
