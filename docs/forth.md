# Forth Standards (habu)

How we write Forth in this repo. **BLOCKING** — code that violates these is wrong,
not a matter of taste. Target is the native `bin/hb` engine.

Durable Forth language guidance belongs here, not in `LESSONS.md`. Lessons may
record the incident that taught a rule, but the reusable rule itself lives in this
file.

## Naming

- **Our words UPPER-CASE; built-in Forth words as-is.** Words we define
  (`RESOLVE`, `MK-CON`, `APPLY-EFFECT`) are UPPER-CASE; core Forth words stay
  lower-case (`and`, `cells`, `allot`, `: ;`, `?do`). Never upper-case a built-in.
- **Hyphens, never underscores — in word names *and* file names.** `T-CON`,
  `TV-RESET`, `MAX-TV` — not `T_CON`. Source files too: `camera-tracker.f`,
  `latency-xcorr.f`, `timestamp-metrics.f` — not `camera_tracker.f`. Underscores
  are not idiomatic Forth, even when the file ports an underscore-named source from
  another language; name the Habu file in Habu style.
- **Conventional suffixes/prefixes**: predicates end `?` (`TYVAR?`); conversions
  `>X` (`TERM>TAG`, `S>NUMBER?`); fetch/store `X@` / `X!` (`TV@` / `TV!`);
  allocate/reset `X-ALLOC` / `X-RESET`.
- **Scope pairs are `FOO` … `;FOO`** (decision 2026-07-04). Every word pair that
  opens and closes a scope/region uses the opener's name with a `;` prefix as
  the closer: `STRUCTURE … ;STRUCTURE`, `package … ;package`. New scope words
  follow this from birth (type families, kernels, suites). `BEGIN-`/`END-` and
  `X`/`END-X` pairs are legacy shapes being renamed (dots
  `habu-convention-scope-pairs`, `habu-core-structures-dsl`). The bare `;`
  (definition closer) and non-scope `;`-words are unaffected — only paired
  scope delimiters follow the rule.
- Short names per the global naming rules: abbreviate common terms (`buf`, `ctx`,
  `idx`, `nv`, `ki`, `ko`); single letters are fine in tight scope only when
  they remain readable. Locals are lexical and local-first: a declared local
  named `i`, `count`, or `dup` must resolve to that local inside its scope.
  Prefer clearer names (`idx`, `len`, `value`) when they improve readability,
  but do not encode global dictionary collision workarounds into local names.
- **Check for collisions with built-ins** before naming — Forth dictionaries are
  case-insensitive here, so `CON?`/`VAR?` clash with existing words. Prefix to
  disambiguate (`TYCON?`, `TYVAR?`). When in doubt, `' NAME` in a REPL: if it
  resolves, the name is taken.
- **Do not shadow native primitive names.** Later dictionary entries can replace
  primitive signatures and codegen hooks; `shadow-lint` gates this class of bug.
- **Do not define parser/control reserved words.** `I`, `J`, `DO`, `LOOP`,
  `+LOOP`, `LEAVE`, `UNLOOP`, `IF`, `THEN`, `BEGIN`, `REPEAT`, `TRUST`,
  `CASE`, `OF`, `ENDOF`, `ENDCASE`, `TRUSTED:`, `PACKAGE`, `PUBLIC`,
  `PRIVATE`, `UNDEFINE`, and the other
  compiler-dispatch/lifecycle tokens are not ordinary global names even though the dictionary is
  case-insensitive. A generated converter that strips prefixes must run
  `tools/reserved-name-lint.f` after naturalization so `CC-I` becomes `IX`, not
  bare `I`; `CC-J` becomes `JX`, not bare `J`. `tools/check.f` runs this lint
  before spawning the checker child, so reserved-name failures must report
  `E-RESERVED-DEFINITION` with file/line/token instead of a silent rc 70. This
  rule applies to published definition names (`:`, `TRUSTED:`, `KERNEL:`,
  `create`, `variable`, `constant`); lexical locals such as `{: i:n :}` remain
  legal and resolve local-first inside their definition.
- **Prefer hex for numeric literals.** Use `$...` for byte values, masks,
  addresses, offsets, syscall/exit constants, instruction encodings, and other
  machine-adjacent numbers. Decimal is acceptable for small counts and ordinary
  human quantities where base 10 is clearer.
- **Use wordlist namespaces for global collisions.** Qualified names use one
  colon: `HB:COUNT`, `PTX:COUNT`, `MAKI:COUNT`. The qualifier names a wordlist
  namespace; the dictionary record stores the tail (`COUNT`) in that wordlist.
  Maki is the worked adoption: each maki file is wrapped in a `package MAKI` block
  (see Packages below), so its words live in the `MAKI` wordlist and a bare reference
  does not resolve from habu core — enforcing the one-way maki↔habu seam at the
  *dictionary* level, complementing `tools/maki-dep-lint.f`. External callers use
  `MAKI:WORD`; maki-internal cross-file calls reopen `package MAKI` and use bare names.
  Match qualifier case to the word vocabulary: project-defined namespaces and
  words are uppercase (`HB:COUNT`); lowercase built-in namespaces keep lowercase
  qualifiers and lowercase names (`forth:count`). Do not mix cases across the
  qualifier and word (`hb:COUNT`, `HB:count`); write matching-case forms such
  as all-project uppercase (`HB:COUNT`) or lowercase built-in vocabulary
  (`forth:count`). Use `hb:count` only for an intentionally lowercase
  vocabulary.
  Qualification is only a token with exactly one non-edge colon; names that
  start or end with `:` are ordinary Forth words. Do not fake namespaces with
  raw global prefixes when the runtime supports a real wordlist-qualified name.
- **Use packages for module namespaces.** New library, tool, test-support, and
  subsystem code belongs in `package NAME` unless it is a documented core
  language/prelude file. `package NAME` opens NAME's private wordlist, `public`
  switches definitions to NAME's exported wordlist, `private` switches back to
  internal definitions, and `end-package` restores the previous current
  wordlist. The exported API is the words defined in the `public` section and
  called as `NAME:WORD`; private helpers are visible only while the package is
  open, including reopened package blocks. Do not fake a namespace by prefixing
  every public word (`TASK-KILL`, `TASK-DONE?`) when the package can export the
  real interface (`TASK:KILL`, `TASK:DONE?`). Keep qualifier and word case
  matched as above (`HB:COUNT` or `hb:count`, not `hb:COUNT`).

### Packages

Packages are real wordlist namespaces for file/module scope and are the default
shape for new modules. Use the lowercase keywords because they are language
words; keep package names and project-defined words uppercase unless the package
intentionally belongs to a lowercase vocabulary. Define implementation helpers
before `public` or after `private`; define the public interface only in the
`public` section.

```forth
package HB

: HELPER ( n -- n )
   2 * ;

public

: COUNT ( -- n )
   5 HELPER ;

private

: INTERNAL ( -- n )
   COUNT 1 + ;

end-package
```

- `package NAME` consumes the next token, rejects a missing name, rejects names
  containing `:`, rejects nesting, opens NAME's private wordlist, and saves the
  caller's current wordlist. Definitions are private by default.
- `public` is valid only inside a package and switches new definitions to the
  package public/export wordlist. Public words are called from outside as
  `NAME:WORD`; unqualified global lookup must not find them.
- The public section is the module boundary. Export short domain words there:
  `TASK:KILL`, `TASK:DONE?`, `PTX:BROADCAST`, `MAP:GET`. Avoid repeating the
  package name in the public tail unless the domain spelling itself requires it.
  Prefix-style global APIs are legacy debt, not a pattern for new code.
- `private` is valid only inside a package and switches new definitions back to
  the package private wordlist. Private words are visible by unqualified name
  only while that package is open; `NAME:PRIVATE-WORD` must not resolve.
- `end-package` is valid only inside a package. It restores the saved current
  wordlist and clears both runtime and checker package scope.
- Reopening the same package with `package NAME` resumes the same public
  wordlist and the same private wordlist; it does not create a new module scope.
  Later package blocks loaded after earlier ones can call earlier private
  helpers, call earlier public words unqualified while the package is open, and
  add more public exports. Load order is still the dependency order: reopening a
  package does not load any source file by itself.
- Multi-file packages are split by reopening the package in each file. If the
  loader already supplies every package file in dependency order, do not add an
  include just to repeat that fact: `bin/hb --load app/core.f app/api.f` is
  sufficient, and `tools/check.f --source-list app/core.f app/api.f` is the
  checker-only form. Use include only when a source file or entry file should
  own loading its dependencies.
- The purpose of include/require is source composition, not namespace sharing.
  User entry files, tool entry files, and test files own their setup with
  `require` for dependencies and `include` only for deliberately repeated
  source composition. Callers should not have to know that `A.f` must be loaded
  before `B.f` just to run `B-test.f`. If many files need a small primitive
  helper, factor that helper into a narrow `src/core/*.f` prelude file loaded
  before stdlib/tool sources instead of depending on a broad library order such
  as `lib/string.f` before `lib/ffi.f`.
- Use include when a file should be self-sufficient or when a top-level entry
  file should assemble a package from submodules:

```forth
\ app/core.f
package APP

: HELPER ( -- n )
   9 ;

public

: CORE ( -- n )
   HELPER ;

end-package
```

```forth
\ app/api.f
include app/core.f

package APP

public

: RUN ( -- n )
   CORE ;

end-package
```

  `include path/to/file.f` parses the next whitespace-delimited filename and
  loads that source immediately every time. `s" path/to/file.f" included` is the
  lower-level string form. `require path/to/file.f` and `s" path/to/file.f"
  required` are include-once forms keyed by the exact path string in the current
  image; use them for normal dependencies so a shared setup phase and a test
  entry can both name the same support file without duplicate definitions. The
  native engine marks its baked prefix files as `provided` before user/test
  source runs, so `require src/core/sha256.f` skips the prefix-owned copy instead
  of reloading it. Snapshot images preserve the `require` registry because it
  describes which modules are already compiled into the live dictionary. Do not
  include a file merely so two files can see the same private helpers; reopening
  the package provides that shared package scope after both files have been
  loaded. Test suites load self-contained test/tool entry files plus any script
  args only; the test file requires its setup and owns its assertions. Gate
  source lists stay for explicit cross-file integration subjects and generated
  build-stage source, not for ordinary unit-test dependency plumbing.
- A package public or private wordlist is a no-duplicate set. Publishing a word
  whose folded tail already exists in the active target wordlist is an error,
  including across reopened package blocks and across `:`, `create`, `variable`,
  `constant`, and `TRUSTED:` publishing paths. This is case-insensitive:
  `RESET` and `reset` are the same tail.
- Redefinition is explicit only. To replace a word, write `undefine NAME` first;
  this retires the exact active wordlist entry and clears checker-side
  signature, defer-target, and control metadata. A later definition may then
  reuse the same name. Silent last-definition-wins shadowing is always an error.
- Shadowing an outer/global/built-in word from inside a package remains legal
  because it publishes into a different wordlist. The same tail may also appear
  in different packages (`APP:RESET` and `MK:RESET`); only duplicates in the
  same package public/private wordlist are rejected.
- While a package is open, unqualified lookup tries the package private wordlist,
  then the package public wordlist, then the saved/global lookup path. This lets
  public words call private helpers without qualification and lets later private
  helpers call earlier public words.
- Qualified names use the existing single-colon wordlist syntax. The qualifier
  selects the package public wordlist and the dictionary record stores the tail,
  so `HB:COUNT` resolves public `COUNT` in package `HB`.
- **Qualify only across package boundaries.** `NAME:WORD` is for callers *outside*
  `NAME`. A file that belongs to package `NAME` reopens it with `package NAME` and
  calls `NAME`'s words by their bare names — writing `NAME:WORD` inside `NAME`'s own
  files is redundant noise. A call into a *different* package either qualifies
  (`OTHER:WORD`) or reopens that package. Structure a subsystem as a small set of
  internal module packages plus one public-interface package the outside world
  qualifies against; the internal packages call each other across boundaries, the
  public package composes them, and only truly external code writes the qualifier.
- Package scope is mirrored into the checker. Certified definitions recorded
  inside `private` are visible only to later checked code in the same open
  package; certified definitions recorded inside `public` are visible as
  `NAME:WORD`. The checker must reject duplicate certified definitions in the
  same active package wordlist before runtime.
- Every package feature must have native gate coverage for runtime lookup,
  checker certification, private isolation, public export, reopen behavior,
  case-insensitive lookup, and fail-closed misuse (`public`/`private`/
  `end-package` outside a package, nested packages, missing package names, and
  qualified package names).

### Structures And Enums

Structures are a checked Forth layout DSL. Use the SwiftForth-style stack
protocol: `BEGIN-STRUCTURE NAME` opens a structure and creates `NAME` as the
final byte-size word; field words thread the offset; `END-STRUCTURE` seals the
size. Do not define raw offset constants by hand when this DSL fits.

```forth
BEGIN-STRUCTURE POINT
   CELL +FIELD POINT.X
   CELL +FIELD POINT.Y
   PTR-FIELD: POINT.NAME
   CFIELD: POINT.FLAGS
END-STRUCTURE
```

- `CELL` is the machine cell byte size. Prefer it to raw `$8` in field layouts.
- `+FIELD` has defining-time effect `( ptr a n n -- ptr a n )` and creates a
  field accessor with runtime effect `( ptr a -- ptr a )`; use `@`/`!` for cell
  fields.
- `PTR-FIELD:` has defining-time effect `( ptr a n -- ptr a n )` and creates a
  pointer-valued cell accessor with runtime effect `( ptr a -- ptr ptr a )`; use
  it instead of `+FIELD` when a field stores a typed pointer, then use normal
  `@`/`!`.
- `PTR-VARIABLE` creates a pointer-valued cell with runtime effect
  `( -- ptr ptr a )`; use it instead of `variable` plus `0 ptr-field` wrappers
  for global pointer slots.
- `CFIELD:` has defining-time effect `( ptr a n -- ptr a n )` and creates a
  field accessor with runtime effect `( ptr a -- ptr u8 )`; use `c@`/`c!` for
  byte fields. A byte field followed by cell `@` must reject under the checker.
- `BEGIN-STRUCTURE` rejects nesting and field words reject use outside an active
  structure. Add a gate test when introducing a new field-defining word.
- Keep field names qualified by the structure (`POINT.X`, `POINT.FLAGS`) so the
  dictionary and xref output communicate ownership.
- The structure definers load before `checker.f` so checker-internal records can
  use the same DSL. Their checker effects live in
  `src/core/structures-effects.f`; add a `TRUSTED.md` row and a dictionary gate
  test for any new field definer.

Value records are by-value stack records, not pointer layouts. Use
`VALUE-RECORD name field type ... END-VALUE-RECORD` when a signature should
carry a fixed group of stack cells as one nominal value:

```forth
VALUE-RECORD point x n y n END-VALUE-RECORD
: >POINT ( n n -- point ) ;
: POINT> ( point -- n n ) ;
: POINT-DUP ( point -- point point ) over over ;
: POINT-X ( point -- n ) drop ;
: POINT-Y ( point -- n ) nip ;
: POINT-X! ( n point -- point ) swap drop ;
: POINT-Y! ( point n -- point ) >r drop r> ;
```

The checker expands `point` to hidden field tokens (`field<point,x,n>`,
`field<point,y,n>`). Declared outputs may construct or destructure the record at
zero runtime cost, but hidden fields keep same-shape records distinct:
`( point -- rect )` rejects even when both records contain two `n` fields.
Accessors, updaters, copies, constructors, and destructors are ordinary checked
words over the expanded stack cells; no runtime header or heap object is created.
Fields may use any signature type, including type variables and parametric types:
`VALUE-RECORD box value a END-VALUE-RECORD` is a polymorphic one-field record.

Enums are checked defining words built on `create ... does>`. Use them for named
integer/status families instead of hand-maintained numeric drift:

```forth
0 ENUM E-OK
  ENUM E-OPEN
  ENUM4 E-RANGE
drop
```

`ENUM` defines the next name as the current value and returns `value + 1`;
`ENUM4` returns `value + 4`. Definitions publish through the active wordlist, so
package scope, duplicate-definition rejection, and case-insensitive lookup apply
exactly as for `:`, `create`, `variable`, and `constant`.

SwiftForth-style relocatable linked-list words (`@REL`, `!REL`, `,REL`,
`>LINK`, `<LINK`, `CALLS`) are not part of Habu's checked surface. They encode
dictionary-relative pointer arithmetic and executable list traversal, which is
the wrong abstraction for Habu snapshots and the checker. Use structures for
typed node layout, arrays/maps for runtime collections, `case/of/endof/endcase`
for selector dispatch, and checked execution vectors for late binding. Any
future list DSL must expose typed node/link effects and forbid raw relative
address arithmetic at the public boundary.

## Words & factoring

- **Strictly typed Habu, everywhere you can (BLOCKING).** The default is not
  "checked when convenient" — it is checked/typed, and unchecked is only a named,
  tested boundary the checker genuinely cannot express (see *Unchecked code is a
  named boundary*). Think in small, typed words: factor aggressively, give each a
  real `( in -- out )` effect, and compose them into **nice-reading checked DSLs**
  that read as the domain, not as stack plumbing. The three detailed rules below —
  typed by default, small factored words, DSL-first vocabulary — are this one
  principle expanded; if you reach for a giant word, deep juggling, or a raw `s"`
  blob, stop and build the typed words first.
- **Default new public/library Forth to checked typed definitions.** If the
  checker can express the layer, write an explicit typed effect and let `hb`
  verify it, e.g. `: SQUARE ( i64 -- i64 ) dup * ;`.
- **Keep control flow and multi-step computation out of argument lists.** The
  checker *does* accept `if/else` (and comparisons like `f> 0=`) producing a value
  mid-arg-list — e.g. `s" k" 1 0 > if 5 else 6 then 2.0 L-OF` type-checks and runs,
  so this is a readability/safety rule, not a checker limit. Prefer extracting the
  value into a named word with a real effect (`: DET-MINRATE ( -- r ) ... ;`) or a
  pre-bound local, then pass the simple word. Dense arg lists that splice `if/else`,
  comparisons, and several `@`/`F@` reads are where a wrong cell *type* (a `bool`
  from `0 >` where the callee wants `n`) hides — it surfaces at a *later* call as a
  confusing "expected n actual bool", not at the offending column. One value per
  concept; let each line read as the field it emits.
- **Check every source you can — byte emitters included.** Raw byte/layout
  emitters, ELF/Mach-O writers, tooling, tests, and build helpers are checkable
  unless a specific primitive boundary proves otherwise. Verify with the owning
  `bin/hb --load ...` or `tools/check.f --source-list` path before claiming the
  checker cannot express a layer.
- **Unchecked code is a named boundary, not a habit.** Use `0 set-check`,
  raw emitter words, or `TRUST` only for layers the checker cannot express:
  metaprogramming, source-string generators, primitive emitters, snapshot/build
  drivers, and similarly low-level support. Keep the boundary obvious in the
  file and add focused tests for the contract it asserts.
- **Do not stub real facts with `TRUSTED:`.** A predicate, target selector, or
  runtime fact named with `?` must execute a real body that pushes a boolean.
  `TRUSTED:` may assert nominal identity casts or primitive boundaries, but it is
  not a signature-only forward declaration for words owned by another file.
- **Factor reusable helpers back into checked Forth.** If an unchecked harness or
  tool grows a helper that can be typed, move that helper to checked code instead
  of letting unchecked scaffolding become the library surface.
- **Try the checked factor before adding trust.** If existing primitive effects
  can express the operation, define a small typed word and use it from callers;
  add a primitive model or `TRUSTED:` shim only after proving the checker cannot
  certify that helper.
- **Trust the uncheckable operation, not the dispatcher.** For indirect emitter
  callbacks, keep the row/dispatch word checked and isolate only the raw
  `execute` in a tiny `TRUSTED:` shim. Do not convert a whole factored dispatcher
  to trusted just because one leaf operation is higher-order.
- **Build checked task vocabulary before fighting syntax.** If a test, tool, or
  benchmark needs structured rows, JSON/TSV fragments, generated source,
  diagnostics, packets, or repeated assertions, factor domain words or a focused
  checked DSL first. Giant `s"` literals, fragile escaping, and private byte
  emitters are bugs unless they are the tested boundary of that DSL.
- **Readable DSLs execute the body they name.** Prefer forms such as
  `[: ITEM ;] NAME-FILES` or `TEST:SUITE name ... TEST:END-SUITE` over generic
  list wrappers. A generic `execute` layer needs higher-order effects the checker
  may not model; direct row/body words keep the effect visible.
- **Classification tables beat token ladders.** When a word becomes a long
  `dup`/`over` chain over token classes, move the classes into row/table data and
  factor named transition helpers. The tests should describe the table policy,
  not reconstruct a branch ladder.
- **Small, single-purpose words**, aim ≤ 5 lines. A word should read top-to-bottom
  without you tracking more than a few stack items.
- **Multi-pass words must be split into named passes.** A word that scans input,
  validates rows, mutates aggregate state, and renders output in one body is not
  reviewable even if its definition-line effect is correct. Factor cursor
  movement, row classification, validation, state updates, and render emission
  into named checked words with their own stack effects, then compose them with
  a small orchestration word.
- **No dense single-line control words.** A one-line definition is only for a
  trivial straight-line wrapper. Any word with `IF`, `BEGIN`, `WHILE`, `REPEAT`,
  `UNTIL`, `case`, locals, multiple stack transitions, or more than one semantic
  step must be split across lines. Add line stack effects only where they clarify
  non-obvious stack motion; if formatting the word makes it look noisy, factor it.
- **Raw compiler/emitter code is not exempt.** Unchecked words, register-level
  emitters, and bootstrap/compiler helpers still need exact stack-effect comments
  and small factored helper words. If review requires reconstructing stack state
  from surrounding code or register side effects, factor first; do not rely on
  hand-tracked effects.
- **Factor when the stack gets unreadable.** If you reach for `ROT -ROT PICK
  ROLL`, stop and either factor a helper or use locals. Deep juggling is exactly
  what this project forbids in *user* code — hold our own code to it.
- **Locals `{: a:type b:type :}`** are encouraged where they remove juggling.
  They bind inputs only; do not put `-- outputs` inside the locals form. Keep
  the effect in the stack comment. New locals are typed by default when the
  concrete checker type is known; a bare local name is allowed only when the
  entry stack effect intentionally preserves richer role detail that the local
  annotation cannot express, or when the missing typed capability is documented.
  New diff-introduced bare locals must be made explicit with
  `typed-local-lint: allow-bare-local` so review sees the exception.
- **Local type annotations can erase role detail.** A local such as `a:ptr`
  records only a pointer cell; it does not preserve `ptr u8`. If the body uses
  byte operations such as `c@`/`c!`, keep the detailed type in the stack effect
  and bind an untyped local name, or factor a helper whose entry effect carries
  `( ptr u8 ... -- ... )`.
- **Name same-type numeric slots before reordering them.** Effects such as
  `( cap used add -- )` are all `n`; a stray `swap` type-checks but changes the
  meaning. Bind names at the helper entry or factor role-specific helpers before
  doing capacity, offset, or decoder arithmetic.
- **Locals are block-scoped.** A `{:` group may appear on any live path, including
  inside `if`/`else`, `case` arms, and loop bodies. The names are visible until
  that block arm closes, then the checker and compiler restore the prior local
  scope and frame depth. A branch-local name must not be referenced after
  `then`/`endof`/`endcase`/`loop`/`repeat`; bind before the control word if the
  value must survive the join.
- **Dead code still cannot bind locals.** A later `{:` group is valid after a
  closed early-exit guard such as `dup 0 < if exit then`, because the fall-through
  path is live. A group immediately after an unconditional `exit`, `leave`,
  `throw`, `die`, or `again` remains a checker error.
- **Do not build deep locals stacks.** Habu locals are reliable for shallow
  factoring; nested helper calls from loop/callback bodies should use stack-based
  leaf helpers or explicitly separate scratch variables so inner helpers cannot
  clobber caller indexes.

## Files

- **One concern per file.** Do not bundle unrelated responsibilities: parser,
  renderer, DB, data table, and driver code belong in separate files. Split at
  responsibility boundaries so review stays focused and files can be built in
  parallel.
- **Reusable helpers belong in libraries, not pasted drivers.** Run multi-file
  tools as `hb --load lib/a.f lib/b.f tool.f -- args...`; the native loader
  appends the source files before `--`, `SCRIPT-ARGV$` starts after it, and fd 0
  remains available as tool data even when stdin is non-tty. Shared behavior
  still lives in one owned source file.
- **Keep physical source lines short.** Long `--load` builders and check source
  appenders must be factored into helper words or split across lines. Do not let
  load-list lines approach the interpreter input buffer; source truncation can
  surface later as unrelated top-level words.
- **Keep script argv explicit.** `hb tool.f arg...` preserves single-script
  compatibility and treats `arg...` as script arguments. Use `--load` only when
  the command line contains more than one source file.

## Stack comments

- **Every definition** carries `( before -- after )`.
- **Multi-line definitions use line effects where they clarify.** The
  definition line carries the word effect `( before -- after )`. Add trailing
  `\ ( before -- after )` comments only on body lines where the stack state is
  not obvious and the comment improves review. Do not add empty/no-op stack
  comments; if many line comments are needed, factor the word.
- **Checked definitions use type tokens only.** The checker reads the stack
  comment as the signature, so write `( n n -- )`, `( bool -- )`, or
  `( ptr u8 n -- )`, not arbitrary role names such as `( got want -- )`. Standard
  nominal role tokens such as `idx`, `len`, `count`, `fd`, `rc`, `reg`, `label`,
  `va`, `symidx`, `asm`, `img`, and `snap` are real checker types; informal
  names still belong in locals (`{: got want :}`), helper names, or nearby prose.
- **Use real types, not reflexive `n`.** A string is `ptr u8 n`; a dereferenced
  cell address is `ptr a`; a pointer-valued cell should preserve its nested
  pointer role. `n` is only for genuine scalar cells.
- **Same-cell values need nominal roles.** Values with the same runtime
  representation but different contracts (`reg`, `label`, `va`, `symidx`, `fd`,
  `count`, `asm`, `img`, `snap`) get distinct type tokens and negative checker
  fixtures. A raw `( n n -- )` signature hides swaps the checker should reject.
- **Declare new nominal roles explicitly.** Use top-level `DEFTYPE name` before a
  signature mentions a project-specific same-cell role. Unknown type tokens
  remain errors, so misspellings do not become fresh types.
- **Use `DEFLINEAR` for owner/lifetime tokens.** A linear token is nominal and
  noncopyable: generic `dup`/`over`/`2dup`, `drop`, `@`, `!`, and by-value record
  duplication reject when they would duplicate, discard, load, or store it. Only
  words whose own effect explicitly mentions the linear type may create or
  consume it, so allocation/free boundaries stay audited.
- **Raw role casts are not validators.** Cast words such as `>LEN`, `>IDX`,
  `>COUNT`, `>OFF`, `>ASM`, `>IMG`, and `>SNAP` are trusted identity boundaries.
  Public libraries should expose checked constructors and role-specific helpers
  so length/count/offset/phase swaps fail under `CHECK!`.
- **Unchecked/prose-only comments may name roles** when no checker hook consumes
  the comment, but keep the type shape obvious.
- Add inline `( … )` at non-obvious points inside a longer word so the reader can
  re-anchor mid-definition.
- Use standard notation: `x` cell, `n`/`u` signed/unsigned, `d` double, `c-addr u`
  string, `xt` execution token, `nt` name token, `f`/`bool` flag, `?` for
  maybe-present.

## Checker & type model

- **`CHECK!` is the user contract.** Inference (`CHECK`) proves internal
  consistency; user builds verify the body against the declared `( in -- out )`
  and make rejection fatal. Tests for bad programs must assert build rejection,
  not just runtime failure.
- **Every `TRUST` has a same-change audit row.** Add or update the matching
  `TRUSTED.md` row with effect, reason, and focused tests. Adding lines above a
  trust site drifts the manifest; rerun `trust-lint` and fix exact line numbers
  before commit.
- **Typed booleans are real `bool` values.** Produce true/false with typed
  producers such as `0 0=` and `0 0= 0=` or domain helpers. Do not store raw
  `0`/`-1` into a `ptr bool` cell, and do not compare bools with numeric `=`.
- **Structural integers widen, roles do not.** The checker models structural
  integer tokens with width/sign metadata: `u8 -> u16 -> u32 -> n/cell/i64`
  widening is implicit when lossless, but narrowing and same-width sign changes
  require an explicit conversion. Nominal roles (`idx`, `len`, `fd`, `rc`, `pid`,
  `asm`, `img`, `snap`, etc.) never widen to each other or to bare integers.
- **Pointer-valued cells use cell-indexed `ptr-field`.** When a typed DATA cell
  or record field stores a pointer, compute the cell slot with `ptr-field` so
  `@`/`!` preserve nested pointer types. The index is a cell slot, not a byte
  offset. Raw fixed-header byte offsets need an explicit trusted boundary or a
  modeled byte-offset primitive.
- **Byte pointers are not cell pointers.** `ptr u8` is a byte span and must use
  `c@`/`c!` for byte access. Cell `@`/`!` over a concrete `ptr u8` is a checker
  error; if a cell stores a byte pointer, model the address as `ptr ptr u8`
  through `ptr-field` and then use `@`/`!` on that cell address.
- **Raw state cells still need typed public effects.** Variables used from
  checked code need explicit `TRUST` rows such as `-- ptr n`, `-- ptr bool`, or
  `-- ptr ptr u8`. Boolean state cells are `ptr bool`, and string-pointer state
  should remain `ptr ptr u8` plus a separate length cell.
- **Path-sensitive control is a checker invariant.** `LEAVE`, `EXIT`, `throw`,
  `die`, and `again` must fold or kill paths according to their declared control
  effect. Divergent path arities are soundness bugs; after a dead path, only
  structural closers (`else`, `then`, `loop`, `+loop`, `repeat`, `again`, `;]`)
  may appear.
- **`RECURSE` uses the declared effect.** Recursive calls apply a fresh copy of
  the current definition's declared signature; keep the raw declared signature
  stable after `CHECK!` so rendered/mutated terms cannot corrupt the scheme.
- **Quotations are xts, not closures.** `[: ... ;]` may not read surrounding
  locals until real closures exist. The checker and compiler must reject local
  references while a quotation is open.
- **Checked `catch` is quotation catch.** Consume success outputs inside the
  quotation (`[: WORD drop ;] catch`) and preserve the exact throw code as data at
  an explicit recovery boundary. Do not widen checked code to arbitrary xt catch.
- **Higher-order signatures publish themselves.** If a checked word with
  quotation effects (`DIP`, `KEEP`, row callbacks) passes `CHECK!`, let it render
  into public signatures; do not keep a `TRUST` row just to pin its scheme.
- **Default new higher-order words to CHECKED — function-passing is a checked
  capability.** The checker verifies a quotation parameter (`[ a a -- bool ]`,
  `[ a -- a ]`, …) executed through a call chain AND inside a `?do`/`begin` loop:
  bind the quotation as an ordinary local, thread it to helpers, and `execute` it.
  A comparator heapsort, map, fold, and filter all check this way. Do NOT reach
  for `0 set-check`/`TRUST` for function-passing by precedent — the older
  `src/core/combinators.f` (MAP/FOLD/EACH) is an unchecked boundary that predates
  this and is *not* a model to copy. Only drop to an unchecked boundary after a
  minimal reproducer proves the checker rejects the specific higher-order shape,
  and file a checker-capability gap (per the Checker-Miss RCA below).
- **Execution vectors are typed `defer` words, not raw xt cells.**
  `defer ACTION ( in -- out )` declares the vector's public effect, and checked
  code installs an implementation with a typed quotation:
  `: INIT ( -- ) [: IMPL ;] is ACTION ;`. The checker must prove the quotation
  effect exactly matches the deferred word's declared effect. Do not write
  `variable ACTION`/`@ execute` dispatch tables or `['] IMPL is ACTION`; raw xt
  storage loses the effect. Calling an unset deferred word fails closed with the
  execution-vector error. If native engine state needs one fixed callback cell,
  store a checked vector bridge there once (`[: ACTION ;] CELL !`) and change the
  implementation only through `[: IMPL ;] is ACTION`; do not store raw
  implementations into the engine cell. `@EXECUTE` is not a general replacement
  for `defer` until its zero no-op behavior has a checked stack-effect model.
- **New type tokens need a checker-only bootstrap stage.** Old `bin/hb` rejects
  unknown stack-comment tokens before checked source can use them. Add parser,
  renderer, and `CC-*` checker support, refresh the native binary, then use the
  role in `TRUST` rows and checked definitions.
- **Phase tokens must reach the side effect they order.** `asm`, `img`, and
  `snap` phase cells should flow through the final sign/write/header operation,
  not just an early wrapper, so callers cannot skip required build stages.
- **Seal the implicit row under declared inputs.** Row polymorphism must not let a
  body borrow below declared inputs. A stack-preserving trusted effect such as
  `img -- img` or `fd -- fd` must not satisfy final output by binding an implicit
  base row that hides underflow.

## Errors

- **Fallible words `throw` a named code** (defined in `src/config.fs`,
  e.g. `E-MISMATCH`); they never fail silently or return an out-of-band flag in
  place of an error.
- **`catch` only at explicit recovery boundaries**: REPL/CLI wrappers, test
  assertions, and stack-preserving outcome adapters that return the exact throw
  code as data. No `… catch drop`, broad `catch 2drop`, or other masking.
- `unreachable`-style `abort"` only for proven-impossible states, with a message.
- **Interactive/REPL support recovers; builders may exit.** Recoverable
  interactive failures should `throw` into REPL recovery (`?`, rollback, reread).
  Use process exits such as `die` for build-time makers and CLI boundaries where
  terminating the process is the contract.
- **`throw` and `die` are different control effects.** `throw` is catchable and
  belongs to the checker exception edge; `die` terminates the process and belongs
  to no-return metadata. Do not add dummy output values after `throw` to balance
  a branch. If the checker cannot accept a real throw guard, fix the exception
  model or track that capability gap.
- **`die` consumes a real message and code.** Its effect is
  `( ptr u8 n n -- )`: pass an actual byte string and exit code, not `0 0` as a
  fake string. Model process exits as no-return control flow only at certified
  wrappers.

## Constants

- **Named constants, no magic numbers.** Limits and codes live in `src/config.fs`.
  A literal in code is only acceptable for true primitives of the encoding
  (e.g. the `3`/`7` of the 3-bit tag, and even those get a comment).
- **Default to `$hex` literals.** Bit masks, instruction encodings, ASCII codes,
  memory/struct/byte offsets, and field strides are always hex (`$FF and`,
  `$D10043FF`, `$200`, `$40`). Only genuine small decimal *counts* stay decimal:
  loop bounds, arities, shift amounts, and register indices. When in doubt,
  prefer hex. Cryptographic/format constants should follow the conventional hex
  spelling from the spec, not decimal transcriptions. The standalone parses
  `$hex` (case-insensitive, optional leading `-`).

## Testing (BLOCKING)

- **Every word is exercised by a checked assertion** as it's written — happy path
  plus each error/edge. Use the typed comparators in `lib/test/`: `T=` / `T<>` for
  scalars, `T$=` for strings, `TTRUE` / `TFALSE` for flags, `TTHROWS` for error
  codes, and `SNAP=` (two equal-shape quotations) for multi-value stack snapshots.
  A word without a test is unfinished.
- Tests live in the native gate: `test/engine-suite.f`, focused `tools/*-test.f`
  fixtures, and source-specific checks wired through `test/run.f`.
- Test orchestration uses `lib/test.f`. The framework vocabulary is test
  suite / test group / test; old gate/row wording is legacy only. Project
  adapters provide setup/teardown, argv/env policy, filters, and process
  execution; test files require their own dependencies. Test groups are named
  and either parallel or sequential, and reports print the group/test name,
  pass/fail state, and timing.
- Test-suite runners must not keep suite iteration state on the return stack
  while executing a test. Tests may use `catch`/`throw` for negative assertions;
  runner loops need explicit index/count cells so a caught throw inside one test
  cannot truncate the remaining suite.
- Put fixture helpers in a private package instead of global stems. A test file
  may define a private package, install package-local helpers into `TEST:*`
  hooks, define groups/tests, run once, assert counters, and close the package:

```forth
require lib/test.f

package FEATURE-TEST

variable RUN-N

: RUNNER ( ptr u8 n -- )
   2drop
   1 RUN-N +! ;

: INSTALL ( -- )
   [: RUNNER ;] TEST:RUNNER! ;

T-RESET
INSTALL
TEST:RESET

TEST:GROUP-SEQUENTIAL smoke
TEST:SUITE sample
   feature-test.f -- arg
TEST:END-SUITE
TEST:END-GROUP

TEST:RUN
RUN-N @ 1 T=
T-REPORT

end-package
```

  `lib/test.f` is the public framework interface: `T*` words are assertions,
  `TEST:SETUP!`/`TEST:TEARDOWN!`/`TEST:DRAIN!`/`TEST:ARGS-BEGIN!`/
  `TEST:ARG+!`/`TEST:SELECT?!`/`TEST:RUNNER!`/`TEST:STDIN-RUNNER!` install
  typed hooks, and `TEST:GROUP-PARALLEL`, `TEST:GROUP-SEQUENTIAL`,
  `TEST:END-GROUP`, `TEST:SUITE`, `TEST:SUITE-STDIN`, `TEST:END-SUITE`, and
  `TEST:RUN` define and execute the suite. Do not publish helper globals like
  `FOO-TEST-SETUP-N`; package scope is the namespace.
- Assert the **specific** outcome: inside checked definitions use
  `[: WORD ;] TTHROWSQ` or another stack-preserving quotation `catch` and check
  the exact THROW code; top-level scripts that cannot push quotations may use
  `' WORD TTHROWS`. For diagnostics, capture text and match a substring.
- Run focused fixtures during dev with their owning `tools/*-test.f`, then run
  the full native gate command shown in `docs/bootstrap.md`.
- **False-reject claims need execution proof.** Count a checker limitation only
  after running an unchecked copy and proving the measured stack behavior matches
  the declared effect. Generator bugs become rejections, not false certifications.
- **Signature-token changes need direct smoke probes.** Before rebuilding around a
  new token, test the atom parser/type mapper directly (`ATOM-TOK?`, `TOK-TYPE`,
  renderer output) so prefix/length mistakes fail small.

### Checker-Miss RCA

- Treat the phrase "why didn't the checker catch this?" and equivalent wording
  as an immediate trigger, even in meta-discussion about process. Before any
  tool call or visible text, ask: **What static invariant should have made this
  impossible before runtime, and where should the compiler/checker enforce it?**
  The first visible line of the response, progress update, note, dot, or
  investigation is `Static invariant:` followed by that invariant and boundary.
  If the invariant is not known yet, say so on that same line and reduce the
  case until it is known. If you have already started with runtime symptoms,
  stop and restart from this line. Questions about strengthening this rule,
  quoted examples, and process reviews are still live triggers.
- Do not put runtime symptoms first. The checker/compiler/primitive model owns
  the investigation until the exact checked path proves that the invariant is
  outside its contract. Guards, runtime repairs, documentation edits, and
  library edits come only after the static owner and negative regression are
  identified.
- Use this template before editing runtime/library code:
  `Static invariant:` the pre-runtime fact that should be impossible to violate;
  `Owner:` checker semantics, compiler/codegen model, primitive/boundary effect,
  or typed capability gap; `Path proof:` exact command proving the source path is
  fail-closed or the harness gap to fix first; `Reproducer:` minimal checked
  source that should reject; `Compiler fix:` checker/compiler/primitive metadata
  change or capability dot; `Regression:` negative test added with the fix.
- Prove the exact command path is fail-closed before touching runtime/library
  code. If bad checked source can run on that path, fix the harness/tooling path
  first.
- Classify the miss as wrong primitive/boundary effect, checker semantics,
  codegen/runtime mismatch, or same-type semantic-role gap. Add a minimal checked
  reproducer and a negative regression for that class.
- The runtime/library repair is incomplete until the checker/compiler/primitive
  model rejects the bad program. If the checker cannot yet express the invariant,
  create a detailed dot for the missing checker capability and keep only a named,
  tested boundary until that capability lands.
- Treat the compiler/checker as the owner until evidence excludes it. The normal
  fix sequence is: add the minimal negative checked regression, make it fail for
  the right reason, update checker semantics, compiler metadata, primitive
  effects, or boundary typing, then repair any downstream code.

## Comments & hygiene

- `\` line comments, terse. No restating what the code obviously does.
- Remove scratch/debug prints before commit.
- If a definition fails to compile in raw engine mode, Habu reports the undefined
  word on stderr and then may spill the rest of that definition through the
  interpreter. The native `tools/check.f` runner with `--json-errors
  --all-errors` wraps matched undefined tokens in schema-1 JSON diagnostics.

## Habu Native Tooling Gotchas

- **Use the native debugger stack before print-marker probes.** Runtime and
  codegen RCA starts with `docs/debugging.md`: `.s`, `BPW+` watch cells, REPL
  `step`, compiled-word breakpoints (`BP+`, `BP*`, `BPN`), `tools/jitdump.f`,
  and `tools/imgdump.f`. Extend those tools when they cannot expose the needed
  state; do not hide a missing debug surface behind ad hoc prints.
- **Semantic xref is an in-image responsibility.** Dictionary ownership,
  word-reference, and call/reference RCA should use Forth words in the live
  image (`XREF`/`SEE`/`USES`/`USED-BY` or their current equivalents), with any
  CLI as a thin wrapper. Use source search only to locate files or as a temporary
  fallback after verifying the native word is missing; then add a dot for the
  missing Forth capability.
- **Boundary spawns must attribute failures.** Gate/test/tool boundaries that
  spawn `hb` or another child use outcome capture for expected timeouts and
  failures, not throw-only capture that collapses into a shell rc. The failure
  report must include the suite/case label, phase, executable and argv/load list,
  outcome kind/code, named rc when known, capture bytes/capacity, and captured
  stdout/stderr. Use throw-on-timeout capture only inside a focused unit test
  whose assertion is the named throw itself.
- **Large native tool bundles are supported.** Do not split tools merely to dodge
  DATA pressure. `create ... allot` is for dictionary-sized static storage; large
  runtime-sized buffers use `lib/memory.f` (`MEM-ALLOC-BYTES` or
  `MEM-ALLOC-64K-BUFFERS`) so composition scales with OS-backed mappings rather
  than `DATA-SIZE`. Tools may keep as many 64K buffers and live spans as their
  workload needs, either as one contiguous `MEM-ALLOC-64K-BUFFERS` span or as
  many independent spans. The only accepted limits are cell-size overflow checks
  and an explicit OS allocation failure. If ordinary composition still hits
  capacity, fix the shared memory model and add a regression for the composed
  load.
- **Missing convenience words are not bugs in the standard.** Habu currently lacks
  words such as `pick`, `within`, and `0<>`; use variables, explicit increments,
  or explicit comparisons (`0 = 0=` for nonzero).
- **Trust is audited, not permanent.** `TRUST` records asserted effects so callers
  can be checked, but audit rows must stay current and stale dates must fail lint.
- **Typed pointer fields use cell indexes.** When a variable or record cell
  stores a pointer, construct a `ptr ptr x` field with `ptr-field`, then use
  normal `@`/`!`. Do not multiply indexes by cell size before `ptr-field`; use a
  named trusted boundary for raw byte-offset header cells.
- **Keep `TRUSTED:` bodies syntax-simple.** Do not use locals inside a trusted
  body. Factor checked helper words for real work, then keep the trusted body to
  the minimal operation that the checker cannot express.
- **Checked tool libraries restore checking.** A shared lint/check/tool library
  must not leave callers in unchecked mode. Declare and test any boundary
  locally, then reinstall `CHECK!` immediately after the raw declarations.
- **Generated unchecked spans are split at the first checkable file.** When a
  build tool emits `0 set-check`, prove the shortest source span empirically,
  reinstall the hook as soon as the next file checks, and pin the cut with a
  source-shape regression.
- **Generated checker preludes must rebind the existing hook.** If generated
  source emits `0 set-check` for an audited unchecked span after loading
  `src/core/check-hook.f`, it must reinstall that existing `HOOK` immediately
  afterward with `' HOOK set-check`. Do not define a second hook name in baked
  tty/stdin bundles; explicit duplicate-definition enforcement makes that fail
  closed on startup. Snapshot/AOT stages that install a different hook keep that
  hook local to the stage and must not leak a duplicate REPL hook into `bin/hb`.
- **Bootstrap/fixpoint temp roots are explicit script args.** Stage2/fixpoint
  sources must not depend on stale seed envp capture. Pass the temp root after
  `--`, keep all generated paths under that root, and let the build driver own
  path construction.
- **Use SwiftForth-style escaped literals for readable snapshots.** `S\"`,
  `C\"`, and `.\"` accept C-style escapes (`\\`, `\"`/`\q`, `\n`, `\r`,
  `\t`, `\xNN`, `\z`, etc.). Use them for direct JSON/source expected strings
  when a literal is clearer than a builder. When code generates syntax from
  fields, keep using checked byte/field helpers or `lib/json-write.f`.
- **Generated fixtures use unique test-owned names.** Strict duplicate rejection
  is a feature, so fixture generators must publish names with a tool/test prefix
  and a unique suffix (`CAE-CAP-OK-0`, `GDX-AE-BAD1`, etc.). Do not reuse baked
  generic names such as `OK`, `BAD`, `FOLD`, `RESET`, or a repeated generated
  stem unless the fixture is specifically testing duplicate rejection.
- **Source-use guards match tokens, not substrings.** Required-word checks and
  boundary scans must lex whole tokens and skip comments/strings; substring
  matches create false positives (`FOO` matching `FOO-BAR`) and hide policy bugs.
- **Preflight unchecked native emitters.** Raw image/primitive emitters still
  need checked shape tests before `BUILD-IMAGE`: no mid-control locals, no second
  locals groups, no hand-balanced descriptor math. Use named scratch cells and
  small helpers, then gate the forbidden source shapes in `tools/build-fixpoint.f`
  so bad emitters fail before a snapshot or `bin/hb` candidate is written.
- **Fixed DATA header cells need a layout audit.** Before adding a new native
  runtime cell in `src/habu/layout.f`, check the reserved JIT/runtime ranges:
  virtual stack tags/values (`VTAG-OFF`, `VVAL-OFF`), snapshot stack
  (`SNAPSTK-OFF`), body buffer, return stack, locals table, register tables,
  breakpoints, and snapshot cells. A cell inside a scratch range will be
  overwritten by ordinary compiled source; add a focused regression for the
  exact overlap class.
- **Snapshot builders retire the baked tail instead of replaying core sources.**
  When a snapshot entry needs to replace a baked tail word such as `SNAP-OUT`,
  use the explicit definition-lifecycle path (`undefine NAME` for one word,
  `HIDE-DEFS-FROM` only for refresh tail truncation) and append the actual
  snapshot entry file. Do not replay already-baked core, target, or image files
  just to mask duplicate definitions; that hides stale process state and makes
  strict duplicate checks look like the problem.
- **Snapshot builders reset process-local pointers.** Restored DATA cells are
  persistent, but mmap-backed image/include pointers and cursors (`MBUF-A`, `MP`,
  `MLEN@`/`MLEN!`, `INCLUDE-BUFS-A`, include depth/read/path cells, etc.) are valid only
  in the process that created them. Clear those transient cells in a named reset
  word before `BUILD-SNAP-HDR` or fresh image emission; never rely on source
  replay or variable redefinition to zero them.
- **Emitter punctuation is semantic.** Words such as `BL,`, `LBL,`, `ADR,`, and
  `ZBYTES,` are distinct from punctuation-less names; source-shape regressions
  should assert exact emitted tokens. Emitter stack comments describe the
  host/build-time stack (`( -- )`, `( n -- )`); document emitted runtime effects
  in nearby prose or in the generated word's own contract.

## Native Forth Gotchas That Shape How We Write Code

(Build/environment findings are in `../LESSONS.md`; these are the ones that affect
*coding*.)

- **Case-insensitive** dictionary -> name-collision risk (see Naming).
- **`[']` is compile-only.** In interpreted tests, use `'` (tick) to get an xt,
  e.g. `' WORD catch`.
- **Control words and ticks are compile-only** — `if`/`else`/`then`,
  `begin`/`while`/`repeat`, `[']`, `i`, `?do`, and `;` must live inside a
  `:` definition, never at the top level.
- **A `begin <cond> while <body> repeat` condition may only *add* a flag.** The
  stack below the flag at `while` must equal the stack at `begin`; a condition
  that net-produces carry values (e.g. `a u NEXT-TOKEN` leaving a token span
  under the flag) is rejected at `repeat`. Establish loop-carried values *before*
  `begin` (they thread through unchanged), or move the production into the body —
  a peek-only flag condition plus an extract-in-body step.
- **A no-`else` `if` must be stack-neutral.** If the true branch changes stack
  depth the merge at `then` fails (`expected: … actual:`). Bind the consumed
  value into a local *before* the `if` so both paths balance, or add `else drop`.
- **Malformed control syntax is a rejection, not `uncheckable`.** Orphan closers,
  unterminated control frames, `i`/`j` outside enough loops, and `leave` outside a
  loop must make `CHECK!` return `0`; `uncheckable` is reserved for modeled-word
  gaps, not parser/control imbalance.
- **`case/of/endof/endcase` is standard selector control flow.** Write the
  selector before `case`, each key before `of`, each matched arm before `endof`,
  and optional default code before `endcase`. `of` compares the key with the
  preserved selector; matched arms consume the selector, while the fall-through
  default path keeps it until `endcase` drops it. Therefore a default arm that
  produces a value must leave the selector on top for `endcase` to remove, e.g.
  `30 swap endcase`. The checker requires integer keys/selectors and unifies
  every live arm plus the default to one data/return-stack effect.
- **A local may be bound after a closed early-exit guard.** The checker tracks the
  live fall-through path, so `dup 0 < if exit then {: x:n :}` is valid. A local
  after an unconditional dead path is still rejected.
- **`parse-name` returns a transient `( c-addr u )`** that the next
  `s"`/`."`/`refill` invalidates — `move` the bytes into your own buffer
  immediately; never hold the pointer across another parsing word.
- **`s>number? ( c-addr u -- d flag )` returns a double** — narrow with `d>s`.
- **`s" "` is empty, not a one-space string.** The parser consumes the delimiter
  after `s"`, so generated-source builders that need a literal space should emit
  byte `32` or use an existing `*-SP` byte helper.
- **"is it a defined word?"** → `find-name ( c-addr u -- nt|0 )`, not `find`.
- **`catch` preserves the pre-call args** under the throw code: `nv ' WORD catch`
  on a throw leaves `( nv code )` — `nip`/adjust in tests accordingly.
- Run tests through the owning gate script so assertion failures control the
  process exit code.
- **Fallible value-returning scanners should validate first.** Put range/schema
  checks that can `throw` in a `--` helper, then make the value-returning word's
  remaining path structurally return its declared outputs. A final throw-only
  fallback in a word declared as `-- value...` can confuse path-effect merging.
