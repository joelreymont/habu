# Forth Standards (habu)

Start with the [short card](forth-card.md); use this guide for full language
rules. Build and test procedures live in [bootstrap.md](bootstrap.md) and
[gate.md](gate.md).

## Checked code and primitive boundaries

- Write ordinary Forth as checked definitions: tools, tests, emitters
  (ELF/Mach-O writers included), build drivers and helpers, source generators,
  cleanup and dispatch code.
- Never add `TRUST`, `TRUSTED:` or an unchecked span to bypass a missing checker
  model. Reduce the case and fix the declaration, checker or primitive interface
  at its owner, still rejecting invalid programs.
- `PRIM:`/`PPRIM:` axioms are for genuine engine, syscall and FFI operations,
  with checked callers. **Primitive effects are assumptions, not proofs**: each
  axiom describes the primitive's actual behaviour, with focused coverage
  through its real call path. Renaming a Forth algorithm or unchecked wrapper,
  or asserting its effect, does not make it a primitive.
- A `PPRIM:` row closed with `CLOSE-PRIVATE` instead of `PPRIM;` interns the
  axiom into the OWNER package's private wordlist: only a body compiled inside
  that package resolves the name; callers are still checked. A primitive may be
  that row alone: `src/core/internal-mark.f` classifies a global record no
  top-level row types by the owner-private row that reaches it, so the owner's
  checked callers compile at both tiers and an outside checked caller is
  refused by name. A checked `[']` of it, by its own name or an `EXPORT`
  alias's, is admitted and refused exactly where that call is. The seed
  primitive `set-tier` is such a row alone: package `TIER` (`src/lib/tier.f`) owns
  it and `TIER:SELECT` is how any other scope selects a tier. Top-level source
  is not checked: there `'` yields the xt and a call runs the primitive. An
  engine primitive (`src/habu/prims.f`) that a `TRUSTED:` body outside the
  owner calls keeps its global trusted-only row beside the private one, as
  `addrmap-set` and `ffi-call-bounded` do: tier 1 builds that caller's call
  window from a global row and refuses the caller without one
  (`E-HIR-UNMODELED`). A row for a word the owner defines itself types only
  that word: it does not keep a global record of the same name out of
  `DNAME-INT`. `test/prim-owner-scope.f` pins the matrix.
- An internal engine primitive (`DNAME-INT`: the prompt, `'`, `search-wl`
  and a checked `[']` refuse it, and only a `TRUSTED:` body compiles a call
  to it) that a package row in `src/habu/prims.f` types is its owner's:
  `ENGINE-PRIMS:DNAME` stamps `DNAME-OWNED` beside `DNAME-INT`
  (`src/habu/layout.f`). The bit opens only the call, and the call is the
  checker's decision through the rows: the owner's checked callers compile at
  both tiers, a checked caller elsewhere gets the global trusted-only row's
  `E-CAP-TRUSTED`, and a checked `[']` stays refused at both tiers, the
  owner's included. Unchecked code (`0 set-check`) compiles the call at both
  tiers, where no checker decides: tier 1 reports the global row's
  `E-CAP-TRUSTED`, as it reports every unjudged verdict, and compiles it.
  `namespace-record` and `package-scope!` are `CHECKER-OVERLAY`'s; so are
  `replay-open`, `replay-close`, `replay-widn!`, `replay-record`,
  `record-wid!` and `replay-private`, which no global row types: elsewhere a
  checked caller is `E-UNDEFINED`, and a `TRUSTED:` body binds one at tier 0
  only (tier 1 cannot compile it). `native-unit-publish` is `NPUB`'s and
  `unit-compile-run` is `SOURCE-UNIT`'s, whose public `GUARDED` is how other
  code runs one source under a token guard; both keep their global
  trusted-only rows, so a checked caller elsewhere is `E-CAP-TRUSTED`.
  `source-unit-run`'s `SOURCE-ROOT` row admits its owner's checked bodies
  (`test/closed-unit-suite.f`, on the whitebox engine, since the product
  seals `SOURCE-ROOT`); outside `SOURCE-ROOT` a checked caller gets the
  global trusted-only row's `E-CAP-TRUSTED` at both tiers, and a `TRUSTED:`
  body or unchecked tier-0 code calls it.
  `test/owner-access.f` and `test/prim-owner-scope.f` pin the rule.
- Never assert that arbitrary `evaluate` preserves the stack; use typed
  quotations for known callbacks. A checked word evaluates source with
  `evaluate-closed ( ptr u8 n -- )`: the text runs on a guarded data stack of
  its own, must leave nothing and must close every definition it opens
  (measured under **Rules learned by refusal**). The loader and every
  source-generating definer evaluate through it, so a loaded file
  (**Packages**) and a generated declaration are closed programs too. Three
  cases stay open:
  - A text that runs `0 set-check` leaves every later definition unchecked,
    inside the text and after it.
  - A text's top-level code is unchecked, so what it computes is untyped: with
    `variable V` and `S$ ( -- ptr u8 n )`, `s" S$ drop V !" evaluate-closed`
    stores an address in V that a checked `( -- n )` word then reads as `n`.
    Only the text's definitions are certified, each against its own signature.
  - `seed-ndict!` lowers the dictionary below the seal floor and clears it
    (the build rewinds). Top-level code calls it, text a checked word
    evaluates included; no checked body names it, and legacy `TRUSTED:`
    bodies still do. The window build's reset is such text: its checked
    `LOGICAL-RESET` `included`s `src/habu/window-rewind.f`, whose top level
    runs `0 set-check`, `0 set-top-check` and the rewind, as a build
    includes `src/habu/prefix-rewind.f`.
- Existing TRUST forms are legacy awaiting removal, tracked in [Campaign
  C2](../.dots/habu-campaign-c2-mem-c3d7662b/habu-campaign-c2-mem-c3d7662b.md);
  mentions below describe legacy syntax only.

## Naming

- **Our words UPPER-CASE, built-ins as-is**: `RESOLVE`, `MK-CON`,
  `APPLY-EFFECT`; `and`, `cells`, `allot`, `: ;`, `?do`. Never upper-case a
  built-in.
- **Scope pairs are `FOO … ;FOO`**: a new pair that opens and closes a scope
  closes with the opener's name behind `;`, as `SUMTYPE … ;SUMTYPE`, `PRODUCT`,
  `ENUM`, `VARIANT`, `MATCH`, `package … ;package` (keyword case follows the
  opener). `VALUE-RECORD … END-VALUE-RECORD` and Forth-2012 `BEGIN-STRUCTURE …
  END-STRUCTURE` are sanctioned exceptions, not templates. Never coin `END-FOO`,
  `FOO-END` or `ENDFOO`. ANS control words (`begin … until`, `case … endcase`,
  `do … loop`, `of … endof`), the bare `;` and non-scope `;`-words are
  unaffected.
- **Hyphens, never underscores**, in word and file names (`T-CON`,
  `camera-tracker.f`), ports of underscore-named sources included.
- **Conventional affixes**: predicates end `?` (`TYVAR?`); conversions `>X`
  (`TERM>TAG`); fetch/store `X@`/`X!` (`TV@`/`TV!`); allocate/reset
  `X-ALLOC`/`X-RESET`.
- **Short names**: `buf`, `ctx`, `idx`, `nv`, `ki`, `ko`; single letters only in
  a tight, readable scope; `idx`, `len`, `value` where clearer.
- **Locals are lexical and local-first.** A local named `i`, `count` or `dup`
  resolves to the local in its scope; never encode collision workarounds into
  local names. A reference binds a local only in its DECLARED spelling, byte for
  byte, while word lookup stays case-insensitive: `{: text :}` reads `text` as
  the local and `TEXT` as the word, and the local hides the word `TEXT` from the
  definition it is declared in (`address` hides `ADDRESS`). `{: i:n :} 0 3 0 ?do
  i + loop` answers three turns of the LOCAL; without the declaration, the loop
  index. A local binds from its group's closer to the end of its scope; names
  differing only in case are two locals; repeated declarations of one spelling
  resolve to the latest live binding; mentions before the closer resolve in the
  preceding scope. Checker, JIT and tier 1 agree
  (`test/compiler/native-local-case.f`). A local named `;]` is `E-NELAB-LOCAL`;
  a group that writes `[:` is `E-NELAB-QUOT`.
- **Check for collisions with built-ins**: the dictionary is case-insensitive,
  so `CON?`/`VAR?` clash; prefix (`TYCON?`, `TYVAR?`). If `' NAME` resolves in a
  REPL, the name is taken.
- **Never shadow a native primitive name**; `shadow-lint` gates this.
- **Never define a parser or control reserved word** as a published name (by
  `:`, `TRUSTED:`, `KERNEL:`, `create`, `variable`, `constant`, `defer`): `I`,
  `J`, `DO`, `LOOP`, `+LOOP`, `LEAVE`, `UNLOOP`, `IF`, `THEN`, `BEGIN`,
  `REPEAT`, `TRUST`, `CASE`, `OF`, `ENDOF`, `ENDCASE`, `TRUSTED:`, `PACKAGE`,
  `PUBLIC`, `PRIVATE`, `UNDEFINE` and the other compiler-dispatch and lifecycle
  tokens. The loaders `include`, `included`, `require`, `required` and
  `provided` are reserved because `tools/check.f`'s discovery reads their
  spelling lexically. The target predicates `HB-TARGET-LINUX?`,
  `HB-TARGET-MACOS?` and `HB-TARGET-LINUX-X86-64?` are reserved too:
  `tools/check.f` reads their spelling lexically to skip the arms of an `if` the
  engine's target never runs, and a source's own word of that spelling could
  answer otherwise. Lexical locals such as `{: i:n :}` stay legal. A generated
  converter that strips prefixes runs `tools/reserved-name-lint.f` after
  naturalization (`CC-I` becomes `IX`, `CC-J` becomes `JX`); `tools/check.f`
  runs that lint before spawning the checker child and reports
  `E-RESERVED-DEFINITION` with file, line and token instead of a silent rc 70.
- **Never define a number-shaped word.** Numeric parsing precedes dictionary
  lookup: `: 42 ( -- n ) 7 ; 42 .` prints 42, not 7. `tools/check.f` refuses
  such definitions as `E-NUMERIC-DEFINITION`, including dot-digit tails such
  as `U.0`. Dot-letter printers (`.U`, `.INT`, `F.N`) and names such as
  `1STNZ`, `0<>`, `2DUP` remain legal. Interpretation, compilation,
  `evaluate` and the checker use one literal grammar: integer `-?d+ | -?$h+`;
  float `-?d*.d+`, exactly one dot with a digit after it. Thus `.5`, `-.5`
  and `.0` are floats; `5.` and `..5` are words. A decimal shape whose
  integer magnitude, fractional numerator or power-of-ten scale exceeds its
  signed-cell accumulator remains claimed and rejects before dictionary lookup;
  `99999999999999999999` is `E-UNDEFINED-TOP-LEVEL` at top level.
- **Engine-provided sources are exempt from the reserved-word rule.** They
  implement the language, and several define the reserved words themselves:
  `TRUST` and `CHECK-DOES!` (`src/core/checker.f`), the loaders
  (`src/core/include.f`), `NEWTYPE`, `SUMTYPE`, `PRODUCT`, `ENUM`,
  `LAYOUT-BUFFER` and `undefine`. The lint guards what `tools/check.f` reads,
  and check.f loads and checks nothing from such a source (given only those, it
  refuses each, `E-ENGINE-PROVIDED`, rc 64); rebuilding `bin/hb` checks it. So
  their other words of a reserved spelling, such as `DECL-EVENT:FIELD` and
  `NFAM:VARIANT`, stand too. The number-shaped rule has no exemption: such a
  word is unreachable in any source. Each target's engine provides only its own
  `src/os/<target>/target.f`; on another host check.f does not take a foreign
  target's copy (it refuses it, as it redefines the host's predicates), so a
  target.f is checked by building its target's engine. A `src/` file no engine
  provides, such as `src/habu/primitive-registry.f`, is checked and keeps both
  rules.
- **Namespaces are wordlists.** A qualified name has exactly one non-edge colon
  (`HB:COUNT`, `PTX:COUNT`, `MAKI:COUNT`): the qualifier names the wordlist, the
  record stores the tail. Qualifier case matches the vocabulary: project words
  uppercase (`HB:COUNT`), built-in vocabularies lowercase (`forth:count`), never
  mixed (`hb:COUNT`, `HB:count`); `hb:count` only for an intentionally lowercase
  vocabulary. Names starting or ending with `:` are ordinary words.

### Packages

**Modules are packages**, wordlist namespaces at file/module scope: new library,
tool, test-support and subsystem code lives in `package NAME` unless it is a
documented core prelude file. Keywords are lowercase language words; package
names and project words are uppercase unless the package is a lowercase
vocabulary. Helpers go before `public` or after `private`. The `public` section
is the module boundary: the real interface as short domain words (`TASK:KILL`,
`TASK:DONE?`, `PTX:BROADCAST`, `MAP:GET`), without the package name in the tail
unless the domain spelling requires it. Never fake a namespace with global
prefixes (`TASK-KILL`); prefix-style global APIs are legacy debt. Maki is the
worked adoption.

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

;package
```

- `package NAME` consumes the next token, rejects a missing name, a name
  containing `:` and nesting, opens NAME's private wordlist and saves the
  caller's current wordlist; definitions are private by default. `;package`,
  valid only inside a package, restores the saved wordlist and clears runtime
  and checker package scope.
- `public` and `private`, valid only inside a package, switch new definitions to
  the export wordlist and back. Outside code calls a public word as `NAME:WORD`;
  unqualified global lookup never finds it. A private word is visible
  unqualified only while its package is open; `NAME:PRIVATE-WORD` never
  resolves. While a package is open, unqualified lookup tries the private
  wordlist, then the public wordlist, then the saved global path.
- The checker mirrors package scope: certified `private` definitions are visible
  only to later checked code in the same open package, `public` ones as
  `NAME:WORD`; duplicate certified definitions in one active wordlist are
  rejected before runtime.
- Reopening `package NAME` resumes the same public and private wordlists; it
  creates no scope and loads no file. Later blocks call earlier private helpers
  and public words unqualified and add exports. Load order is still dependency
  order.
- A package the engine bakes is sealed when the native build captures it
  (`src/core/internal-mark.f` `SEAL-PACKAGES`): on the product engine `package
  NAME` exits 84 with the package name on stderr, and a definition into either
  of its wordlists exits 84 naming the word (`hb: cannot publish into protected
  word: NAME:X`). `tools/check.f` refuses each at the name it reports,
  `E-STATEMENT-THROW` with throw code 84. Use its public words qualified or
  through `using`. Only the whitebox image keeps engine packages open; an
  application image (a `--repl` snapshot, `APP-IMAGE:SAVE`) keeps its own
  packages reopenable (`test/package-seal.f`, `test/checker-surface.f`).
- **Qualify only across package boundaries.** In `NAME`'s own files reopen the
  package and use bare names; `NAME:WORD` there is noise. A call into another
  package qualifies (`OTHER:WORD`) or reopens it. A subsystem is a few internal
  module packages plus one public-interface package; only truly external code
  writes the qualifier.
- A multi-file package reopens per file. When the loader lists every file in
  order (`bin/hb --load app/core.f app/api.f`; `tools/check.f --source-list
  app/core.f app/api.f` for the checker), add no include; include only when a
  file should own loading its dependencies:

```forth
\ app/core.f
package APP

: HELPER ( -- n )
   9 ;

public

: CORE ( -- n )
   HELPER ;

;package
```

```forth
\ app/api.f
include app/core.f

package APP

public

: RUN ( -- n )
   CORE ;

;package
```

- `include path.f` (`s" path.f" included`) loads the file every time; `require
  path.f` (`s" path.f" required`) loads once, keyed by canonical absolute
  pathname, so absolute, relative, `.`/`..` and symlink spellings are one
  identity. Both compose source and share no namespace. Use `require` for
  dependencies; entry, tool and test files own their setup with it, and a test
  file owns its assertions. A small helper many files need becomes a narrow
  `src/core/*.f` prelude loaded before stdlib and tools, not a broad library
  order. Never include a file so two files share private helpers; reopening the
  package does that. Gate source lists are for cross-file integration subjects
  and generated build-stage source, not unit-test dependency plumbing.
- A loaded file is a closed program: `include`, `require` and `--load`
  evaluate it through the loader's `INCLUDE-EVALUATE`, which is
  `evaluate-closed`. Its top level starts at `depth` 0, a token reaching its
  includer's cells throws 70, and a file that ends with cells on the stack is
  refused `E-EVAL-RESIDUE` (`--load` of a file ending `1 2` exits
  `hb: uncaught throw code -3804`, rc 67), so a value crosses a load only as a
  word the file defines. A file that ends inside a definition it opened is
  refused at its end, as every source is (**Rules learned by refusal**):
  `--load` of a file ending `: D ( -- n ) 42` exits
  `hb: source ended inside definition: D at <path>:<line>`, rc 74, so no
  definition spans a load. Fix a file that leaves cells or a definition; never
  loosen the loader.
  `hb prog.f` and a program on stdin are not loaded files and keep their
  top-level stack. `test/closed-source-suite.f` pins the boundary.
- A named `--load` entry's canonical directory is the primary source root.
  Relative dependencies search that root, then the invocation working
  directory, then the engine's source root, each root once.
  `SOURCE-ROOT:ENGINE$ ( -- ptr u8 n )` is the working directory when it is a
  Habu tree (it holds the engine's first boot file, `src/core/util.f`), else
  the tree two levels above the engine's executable (SwiftForth's `%`, found
  through symlinks) when that is one, else the working directory. So an
  installed `hb` runs from any directory, and inside a checkout the checkout
  is the root. No variable or word sets it. The working directory is searched
  before the engine root, so while the root is the executable's tree, a
  working directory that is not a tree must not hold copies of engine files:
  its `src/lib/errors.f` is an application file, and loading it beside the
  engine's is a duplicate definition, exit 78. With no tree at either place
  the working directory is the root, so its `src/lib/errors.f` is the engine's
  row and is never loaded. A relative `--load` entry is relative to the
  working directory alone; a dependency keeps the root that resolved it for
  its own loads; absolute paths bypass the search; the process working
  directory never changes. So `/work/app/main.f` can require `src/model.f`,
  and that file `src/math.f`.
- At the top level of a stdin session or of a program file run as
  `bin/hb file.f`, SwiftForth's path words, public in package `SOURCE-ROOT`
  (`SOURCE-ROOT:CD`, or `CD` under `using SOURCE-ROOT`), move the current
  path, the first root a relative path searches: `CD <dir>` sets it, relative
  to the current path when relative, and a bare `CD` prints it; `PUSHPATH`
  saves it, up to 8 deep, and `POPPATH` restores it. A missing directory, a
  file, a path over the loader's cap, a full or empty stack and a word inside
  a loaded file (`--load` or `require`) are refused by name, exit 74. They
  never change the engine root or the process working directory, and an
  image cannot be saved with a path pushed. A file scopes a root with
  `SOURCE-ROOT:WITH`.
  `SOURCE-ROOT:WITH ( ptr u8 n [ -- ] -- )` scopes an explicit root, restoring
  the caller's on return or throw; a root that does not resolve to a
  searchable directory, and an image save inside the scope, are refused by
  name, exit 74. Fixtures resolve against
  `SOURCE-ROOT:CURRENT$ ( -- ptr u8 n )`, never a script argument. Nested loads
  keep each parent's source bytes alive until it returns, releasing them on
  return or throw; there is no fixed nesting count. Each load reads its file
  into a frame sized to that file, so there is no fixed file size either: only
  a failed mapping refuses a file, exit 74. Discovery's record of the loads a
  file makes grows the same way, with no fixed count. Discovery, checker
  dependency collection and content closures use the same canonical paths and
  owner roots.
- The engine marks its baked prefix files `provided` before user source runs, so
  `require src/core/sha256.f` skips the prefix-owned copy; `provided` is honored
  before a missing file is opened. Frozen facts keep names relative to the
  engine's source root and match only that root's own copy, so the engine runs
  from another checkout or without its compiled sources, and an application's
  file named like an engine file stays the application's. A build records each
  boot file by that relative name, so one outside the engine root is refused by
  name, exit 74 (bootstrap.md, "Refresh `bin/hb`"); snapshots keep these facts
  and clear process-local roots and resolver scratch. Relocated reloading of a
  deleted application source tree is not promised.
- A package wordlist is a case-insensitive no-duplicate set (`RESET` and `reset`
  are one tail) across reopened blocks and across `:`, `create`, `variable`,
  `constant` and `TRUSTED:`; silent last-definition-wins shadowing is always an
  error. Redefine explicitly: `undefine NAME` retires the active entry and
  clears checker signature, defer-target and control metadata, then the name may
  be reused. Shadowing an outer, global or built-in word from inside a package
  is legal (a different wordlist), and one tail may live in several packages
  (`APP:RESET`, `MK:RESET`). A `does>` definer `MK` also publishes its clause
  as `MK;does` in its own wordlist, so a live `MK;does` there refuses `MK` at
  `does>` and a later `MK;does` is refused, rc 78 either way
  (test/does-clause-record-child.f).
- A **public** definition whose tail a **private** word of the same package owns
  is the forwarder pattern (`src/lib/task.f` publishes `: PREPARE ( ptr n -- )
  PREPARE ;` over its private `PREPARE`): legal, but both effects must move the
  same number of CELLS, because the bare tail binds the private word and a
  definition's contract is read from that binding
  (`src/compiler/native/compiler.f` KEEP-ARITY asks `NDICT:SPELL-ARITY` with the
  bare name). A mismatch is refused where written, `E-SHADOWED-ARITY` (checker
  7145, rc 70; `tools/check.f` refuses the definition as for a using refusal
  below), naming the package, the tail and both widths, its packet placed at
  the public definition's name; the native build's `-8303 E-NELAB-ARITY` stays
  as the backstop. The rule judges only a colon
  definition with a DECLARED signature; a public word made by a storage definer
  (`constant`, `variable`, `create`) is judged by its definer's row, so a
  private and a public `SHARED` constant in one package stay legal. Same cells
  through different types are accepted (`( ptr u8 n -- n )` against `( n n -- n
  )`), and so is a public definition made BEFORE the private one: it binds its
  own name when its contract is read.
- `EXPORT NAME` inside an open package re-exports an EXISTING word into the
  current section under its own tail: same xt, same checked effect (a fresh
  alpha-equivalent scheme copy), defer and control flags and immediate/wide bits
  carried, no forwarding body, zero runtime cost. `EXPORT EVAL:RUN` in `package
  MAKI public` publishes `MAKI:RUN`; a bare `EXPORT HELPER` in the public
  section promotes the private `HELPER`. Refused: an undefined source, a private
  word behind a CLOSED package, a source qualified into a sealed system package,
  a primitive, a duplicate tail in the target section and a failed
  declaration's row outside its own diagnostic run (E-EXPORT-UNDEFINED). An
  exported `does>` definer `MK` brings its clause along as `MK;does`, so a live
  `MK;does` in the target section refuses it, rc 78
  (test/does-clause-record-child.f).
  Re-exporting a generated constructor under a second name is allowed; adding
  tails INTO a generated constructor package is not. AOT tree-shake keeps one
  body; alias rows roll back with checker scope frames. At TOP LEVEL `EXPORT
  name…` is the hb-build `--repl` export directive: the build strips it and a
  plain load consumes the name as a no-op.
- Every package feature has native gate coverage: runtime lookup, checker
  certification, private isolation, public export, reopen, case-insensitive
  lookup and fail-closed misuse (`public`/`private`/`;package` outside a
  package, nesting, missing names, qualified package names).
- **A process can be sealed to the packages a harness admits.** After
  `POLICY:SEAL` (`src/lib/policy.f`) the source the engine reads may name only its
  own definitions, the public words of the packages given to `POLICY:ALLOW`,
  and twelve keywords; any other token is `hb: not in vocabulary: <token> at
  <path>:<line>`, exit code 107. [policy.md](policy.md) has the vocabulary and
  the refusals.

#### Importing a package's public words with `using`

`using NAME` makes package `NAME`'s **public** wordlist visible to bare lookup
in the current scope without opening `NAME`. `require` loads source; it imports
nothing and does not justify repeating `NAME:WORD`. Every new or changed
consumer that calls two or more public words of one required package MUST import
it once, `using NAME … ;using`, and call them bare; scratch files, reproducers,
performance scripts and generated files included. Untouched legacy consumers
stay explicit debt until owner-scoped migration. PREFER `NAME:WORD`, unchanged
and always available, for a one-off call or to escape a collision.

- `using NAME` consumes the next token and rejects a missing name, a name with
  `:` and an unknown package; it is valid at top level and inside an open
  package. Only the public wordlist joins the search; definitions still target
  the current scope's wordlist. A required file may open `NAME`; when the
  require returns, the consumer is back in its original scope. A file loaded or
  a buffer evaluated while a `using` is open resolves through it, at top level
  and in definitions.
- The scope ends at the matching `;using`, at the enclosing `;package` for a
  `using` opened inside a package, or at the end of the load file, whichever
  comes first; consumer files close explicitly with `;using`. `;using` closes
  the most recent `using`; one with none open is an error. At most `USE-MAX`
  (16) concurrent usings; a further one is rejected.
- Inside a package `;using` closes only a `using` the package opened; close one
  opened before `package` after `;package`. A `;using` inside the package that
  would close an outer one is refused by name (`ENGINE-ERROR:USING-OUTER`, rc
  104; the source verifier's `E-USING-OUTER`, 7146).
- A load file is a using scope the same way: an included file or an
  `evaluate`d buffer closes only the usings it opens. A `;using` in it that
  would close one its includer opened is refused by name with the same codes
  (`ENGINE-ERROR:USING-OUTER`, rc 104; `E-USING-OUTER`, 7146, for a using the
  source verifier's replay inherited). Closed, it came back open when the
  buffer ended, and a `using` the buffer opened next took its slot: after a
  buffer `;using using UB` under `using UA`, the includer resolved `UB`'s
  words where `UA`'s had been.
- Lookup for a bare tail: open-package scope (private, then own public) FIRST,
  then the global wordlist, then each used public wordlist. The open-package
  scope silently wins over a used public. A tail in MORE THAN ONE used public
  wordlist is `E-USING-AMBIGUOUS`; the same package named twice is not
  ambiguous. `using` never silently changes an existing binding: it is the sole
  resolver of an otherwise-unresolved name, or a hard error.
- In a definition `tools/check.f` refuses an ambiguous tail at the reference
  site (checker 7144), naming each used package it resolves in
  (`used_packages`, docs/repair-diagnostics.md; `ga:TOK`, `gb:TOK` in the line
  `--all-errors` prints without `--json-errors`); qualify the one meant or
  rename the collision. It refuses that definition, a definer when the tail
  is in its `does>` clause, as an `E-UNDEFINED` does and exits 70: plainly
  the check stops there, and under `--all-errors` or `--verify-only` the
  definition keeps its declared effect for later callers and the check goes
  on to report every later refusal. The same holds for the shadow below
  (checker 7141) and for `E-SHADOWED-ARITY` (`REFUSE-DEF`, `DEF-REFUSED` and
  `DEF-STOPPED`, `src/core/checker.f`), on which `bin/hb --load` ends, rc 70;
  a refusal outside a definition keeps its own status under `bin/hb --load`,
  and `tools/check.f` refuses the top-level token by the same rule, rc 70
  (the top-level rule below). The engine
  refuses an ambiguous tail earlier, before the checker: at top level, and in a
  definition under `bin/hb --load`, it prints
  `hb: ambiguous bare word resolves in multiple used packages: TOK at FILE:LINE`
  and exits 94.
- A bare tail resolving to a GLOBAL while a used package ALSO exports it is
  `E-USING-SHADOW-GLOBAL` (checker 7141) at the reference site, naming both
  candidates (`global TOK`, `PKG:TOK`) with arities. So a package whose public
  tails are ordinary verbs cannot be imported: `using TCP4` refuses at the first
  bare `READ`, `WRITE` or `CLOSE`. Qualify the package word (always certifies)
  or rename the collision. The checker enforces this in every checked body (rc
  70). The interpreter enforces it at top level and for `'`, by name
  (`ENGINE-ERROR:USING-SHADOW-GLOBAL`, rc 105, a throw inside `evaluate`);
  without it `using PS` then a top-level `SHW` ran the global. Only the bodies
  nothing certifies, `TRUSTED:` and `0 set-check` definitions, keep
  global-first, as the explicit unchecked boundary.
- The colliding global need not be one the checker knows: every engine-prefix
  colon word without signature or axiom, and every `0 set-check` definition,
  counts. The reference site asks the ENGINE's wordlists (`search-wl`, the
  engine's own scan and case fold) before a used public may bind. The
  open-package leg is decided the same way: a word in the open package's
  wordlist wins over a used public even when the checker never recorded it, and
  lacking a signature the reference is then `E-UNDEFINED`, not certified against
  the used public.
- Resolution happens at certify time: a call compiled inside a `using` scope
  keeps resolving after `;using`, and AOT/baked images carry no using-state. The
  engine's wordlists alone decide which scope claims a tail; the checker's
  symbol table answers only what a word's effect is. A checked body reading a
  used public certifies; a used private or an ambiguous tail is rejected before
  runtime; certification and execution name the same word. A global that appears
  AFTER a reference was certified does not change what it runs; the next
  reference to that tail is refused.
- `using` state is file-local: snapshotted per eval frame and REPL line and
  rolled back with the package scope, so a `using` left open in an included
  file, or aborted by a throw, never leaks to the caller. A package an included
  file leaves open keeps none of that file's usings: its using floor drops to
  the restored depth, so the includer's `;package` reopens none of them and its
  own `;using` inside the package closes. A file that closes its includer's
  package ends the usings opened in that package, and the includer gets back
  the depth that `;package` restored, not the one the file entered at: a using
  the file opened after it ends with the file.
- **A package word shadows the same-named global or primitive, and nothing
  reaches past it.** Inside `package APP` a bare `open` is `APP:OPEN`; in
  a checked body under `using DOC` a bare `close` is refused against
  `DOC:CLOSE`. There is no qualifier for the global "" wordlist: reach the
  operation through a differently named word (`OPEN-APPEND-FD`, the primitive's
  sibling `close-rc`) or rename the package word. Operator spellings too: once a
  package defines `@` or `+`, a bare `@` or `+` in its later bodies is the
  package word to the checker, including a replay, and both compilers, whatever
  its operands; a body compiled before the definition keeps the engine word
  (`test/reopen-binding.f`, `test/replay-binding.f`).

### Structures And Enums

New declarations use `NEWTYPE` for an opaque nominal cell, `STRUCTURE` for a
record with named fields and `ENUM` for alternatives with or without payloads.
Each registers a whole family at top level and cannot be called inside a checked
definition. Family headers name their type parameters, such as `box<a>` or
`result<a,b>`; a bare name has no parameters. Parameters bind payload types in
their written order. Replace the old integer counts with named parameters:

| Old header | Current header |
|---|---|
| `NEWTYPE rownum 0` | `NEWTYPE rownum` |
| `SUMTYPE result 0` | `SUMTYPE result` |
| `PRODUCT row 0` | `PRODUCT row` |
| `STRUCTURE box 1` | `STRUCTURE box<a>` |
| `ENUM result 2` | `ENUM result<a,b>` |

Compact `ENUM` has no payload fields or parameters. Legacy `SUMTYPE`, `PRODUCT`,
`VALUE-RECORD`,
low-level `BEGIN-STRUCTURE`/`END-STRUCTURE` definers and counter enums (`ENUM+`,
`ENUM4+`) still have executable sites; they are migration debt, forbidden in new
code.

```forth
package EXAMPLE
public
NEWTYPE index

STRUCTURE point
   FIELD x n
   FIELD y n
;STRUCTURE

ENUM message
   VARIANT quit ;VARIANT
   VARIANT move FIELD x n FIELD y n ;VARIANT
;ENUM

ENUM color red green blue ;ENUM
;package
```

A multi-cell record is one logical value and lives inside checked definitions.
The interpreter prompt refuses words with multi-cell inputs or outputs (`hb:
interpret-mode layout value`), including a word whose `does>` clause effect is
multi-cell (`64 SPAN-BUFFER: PBUF`, then a bare `PBUF`); so do `evaluate` and
interpret-mode tick, and bare `dup`, `drop`, `swap` at the prompt move single
cells. To compute with a record at the REPL, call a word whose public effect is
single-cell; `2 3 DEMO:AT DEMO:FIRST` at the prompt is refused at `DEMO:AT`:

```forth
package DEMO
public
STRUCTURE point FIELD x n FIELD y n ;STRUCTURE
: AT ( n n -- point ) DEMO-POINT:MAKE ;
: FIRST ( point -- n ) DEMO-POINT:UNMAKE drop ;
: FIRST-X ( -- n ) 2 3 AT FIRST ;
;package
DEMO:FIRST-X .                      \ prints 2
```

Inside a checked body a record binds to a local, untyped or annotated with the
family it holds:

```forth
: F ( pt -- n )
   {: p :}
   p PT:UNMAKE drop ;

: ID ( pt -- pt )
   {: p:pt :}
   p ;
```

The local holds the WHOLE value, whatever its cell count, and every reference
reloads every cell, so a named record survives a return, an `if` arm and a loop
body intact. Keep a domain value in one named local while passing it between
words. Project or unmake it when the fields supply the computation;
unchanged-value transport uses the whole local, as `CBIND:BIND` does for its
target and numeric policy.

An annotation is read by the SIGNATURE type grammar: family arguments (`{:
r:res<n,n> :}`), nested families (`{: o:opt<opt<n>> :}`), the definition's own
declared type variables (under `( opt<a> -- opt<a> )`, `{: o:opt<a> :} o`
certifies and keeps the quantifier) and the same arity check. The one spelling
only a local has is `{: p:ptr :}`: an annotation is a single token, so the bare
`ptr` means an INFERRED pointee, and `{: p:ptr n :}` is two locals, not a
pointee. The annotation is asserted, not decoration: a wrong family, a scalar
spelling (`{: p:n :}` is `E-MISMATCH`, expected: n actual: @pt.tag), a wrong
family argument and a bare tail of a family of arity > 0 are all refused. See
[the multi-cell type
rules](type-system.md#5-families-records-alternatives-and-generics).

- `NEWTYPE name` registers an opaque nominal cell family (`TK-CELL`), with no
  closer. `NEWTYPE name<a,b>` declares two type parameters. `SUMTYPE`,
  `PRODUCT`, `STRUCTURE`, and full `ENUM` use the same header syntax. The count
  stays internal; payload types bind the listed names by declaration order.
  Names are distinct letters from `a b c d e g h i j k l m o p q s t u v w x y z`;
  `f`, `n`, and `r` always name concrete scalar types. Parameters may be unused.
  An empty list (`<>`), duplicate name, or malformed list rejects.
- Family identity is the exact `(package, tail)` pair: a package family may
  share a tail with a global family or another package's, even at different
  arities. A qualified token resolves only its package row; a bare token
  resolves the open package's own row (private or public), then the global row,
  then the sole eligible public row of another package; two eligible non-lexical
  package-public rows are an error. So adding `MEM:span` cannot change what a
  top-level `span` means. Same-package duplicates, reserved grammar names and
  foreign private rows reject.
- `STRUCTURE name[<params>] [ header… ] FIELD f type … ;STRUCTURE` declares a
  single-shape record with named fields in declaration order, deepest field
  first on the stack. A structure with fields generates sealed `MAKE`/`UNMAKE`:
  `PKG-FAMILY:MAKE` in the constructor namespace for a public package family,
  `FAMILY-MAKE` in the declaring package's private wordlist for a private one.
  The header `OPAQUE` keeps a public family's type nameable from every package
  and gives its generated words — the pair and every `DERIVE` member — the
  private placement: `STRUCTURE box OPAQUE FIELD x n ;STRUCTURE` in `package
  OP` defines `BOX-MAKE` / `BOX-UNMAKE` as OP's private words, reserves no
  `OP-BOX:` namespace, and another package constructs a `box` only through the
  words OP defines over them. A fieldless structure is an opaque cell family
  with no generated constructor or destructor.
- Full `ENUM name[<params>] [ header… ] VARIANT v FIELD f type … ;VARIANT … ;ENUM`
  declares named alternatives with named payload fields; a variant may have
  none. Its tag is its declaration order; generated constructors feed an
  exhaustive `MATCH … ;MATCH`.
- Compact `ENUM name [ header… ] v0 v1 … ;ENUM` declares payloadless variants
  from bare names, with implicit arity zero; never mix it with `VARIANT` blocks.
- `POLICY`, `DERIVE` and `OPAQUE` headers precede the first field or variant: after the
  family header;
  repeating a feature rejects; `OPAQUE` is `STRUCTURE`-only, once, and needs a
  package and a field. `DERIVE addr` on a structure with fields
  generates typed field-address accessors plus `AT`, `BYTES` and `CELLS`: a
  global public `point` with `FIELD x n` gains `POINT:X ( ptr point -- ptr n )`,
  read and written with the field type's ordinary checked operations
  ([type-families.md](type-families.md) has layout policies and the generated
  storage interface). `DERIVE eq`/`DERIVE hash` on a public arity-0 family
  generate those operations; compare an enum with its family's derived `EQ`,
  never raw `=`.
- **A package family's generated words are `PKG-FAMILY:tail` with every hyphen
  in the family name doubled**: an `ENUM read-result` in `package TCP4`
  constructs its `data` variant through `TCP4-READ--RESULT:data`; a `STRUCTURE
  captured` in `package PCAP` unmakes through `PCAP-CAPTURED:UNMAKE`.
  `SIGNAL-RESULT:signal` and `SIGNAL:signal` are both `E-UNDEFINED`. Stack
  effects and `MATCH` name the family `PKG:family` (`( -- TCP4:read-result )`,
  `MATCH TCP4:read-result`); a body inside the owning package writes the bare
  family name in `MATCH`.
- **An OPEN family instance is placeable when the width cannot read the open
  argument.** `STRUCTURE span<a> FIELD base ptr a FIELD len n ;STRUCTURE` is two
  cells for every argument, so `: SKIP ( span<t> n -- span<t> ) …` compiles at
  tier 1 exactly like the `span<u8>` row. A family whose parameter IS a payload
  cell (`FIELD it a`) has a width its argument decides: a row carrying an open
  instance of THAT below another value is `E-NELAB-BUNDLE` (-8519, `ncomp:
  cannot compile NAME`) until the argument is concrete. No width is guessed: the
  registry answers which argument slots the width reads (`src/core/type-family.f
  TFAM-WIDTH-SLOT?`) and the checker expands an instance into its cells only
  when no open slot is one of them (`src/core/checker.f LAYOUT-WIDTH-OPEN?`). A
  `create … does>` clause's rows follow the same rule, with a definition's own
  value boundaries (`DOES-IN-SLOT` / `DOES-OUT-SLOT` on the owner ABI): a clause
  yielding a multi-cell value compiles at tier 1 and the created word runs (`n
  SPAN-BUFFER: NAME`, src/lib/span.f); only a clause row carrying an instance whose
  width reads an open argument is `E-NELAB-BUNDLE`, at the definer.
- `CAST: NAME ( source -- destination )` declares a checked retype: a reader
  keyword with no body and no `;`, publishing `NAME` as an identity whose call
  sites emit nothing. A conversion that can refuse is a checked word that
  throws, then the cast. The checker's rule, in refusal order
  (`src/core/checker.f` `CHECKER-DEFCAST`):
  - Every family named is declared, visible and applied to its declared
    number of arguments (`E-CAST-FAM`, 7131): `( n -- box )` for a
    one-parameter `box` is refused by name, not by a later death.
  - Any other signature fault, bad syntax or a bare `ptr`, is refused as a
    definition with that signature is: the bad-stored-signature diagnostic,
    then the compile-reject rc 70. The first fault names the class, so
    `( ptr -- box )` is this reject, not `E-CAST-FAM`. An output scope or
    region variable no input supplies, when it is the only fault, is left to
    the scope rule: it is the shape of a view pack.
  - A scope or region variable, a quantifier-bound variable, a scope, or a
    read view, mutable view or loan field anywhere in either row would erase
    or introduce a scope dependency (`E-CAST-SCOPE`, 7151). This is asked
    before the arity rule, which would read a view's two cells as two terms.
  - The exception is a view representation cast: it retypes one view,
    `read-view<p,q,T>` or `mut-view<p,q,a,T>`, as the two cells that represent
    it, `ptr u8 n` (base and length), or back, over the same tail. Producing a
    view is C2-MEM's: the pack `( ptr u8 n -- V )`, and the mut-view unpack
    `( mut-view<p,q,a,T> -- ptr u8 n )`, which takes the raw address of an
    exclusive view, are declared only in package C2-MEM's private section.
    Any package may declare the read-view unpack `( read-view<p,q,T> -- ptr
    u8 n )` in its private section: the projection erases the type and can
    neither widen bounds nor forge a scope. A pack's scope and region
    variables are free in its input and, like any callee's, instantiate
    fresh at each call. The cast takes an unqualified name: a qualified name
    selects that package's public wordlist and would publish the boundary.
    That, and every other cast with a view, public or private, is
    `E-CAST-SCOPE`.
  - Each side is one term over the same stack tail (`E-CAST-ARITY`, 7129);
    a layout value wider than a cell is one term per cell. The emitted identity
    preserves the tail, so `( R n -- R u8 )` is valid and `( R n -- S u8 )`
    is refused.
  - A cast term is one machine cell: a con, a width-1 family, a pointer, or a
    quotation. An atom, or a pointer to one, on either side, and an atom in an
    introduction position of the destination, are `E-CAST-CLASS` (7130).
  - A type variable or a linear type anywhere in either term, behind a
    pointer, in any quotation row or in a layout's members, is
    `E-CAST-LINEAR` (7137): a `PRODUCT` field `ptr lease` or a `STRUCTURE`
    field `[ -- lease ]` carries the lease through the cast on either side.
  - A scalar-cell family, including a parametric `NEWTYPE` instance, in an
    introduction position belongs to its declaring package (`E-CAST-OWNER`,
    7135): `CAST: >SLOT ( n -- slot )` outside `package DOC` is refused.
  - A pointer or a quotation in an introduction position is a class mint and
    is declared only in a package's private section (`E-CAST-MINT`, 7147).

  The introduction positions are where the destination hands out a value: the
  term itself, a pointer's pointee (a read), a layout family's arguments and
  its members (every field and variant payload, instantiated over those
  arguments: a field projection or a `MATCH` arm) and a quotation's produced
  rows (a call), recursively. A layout's fields carry its rules down: after
  `STRUCTURE pfbox FIELD p ptr u8 ;STRUCTURE`, `CAST: >PFBOX ( n -- pfbox )`
  is `E-CAST-MINT` outside a private section, and a layout whose field holds
  another package's family is `E-CAST-OWNER` outside that family's package,
  the layout's own package included. A layout that points at its own family
  is read once per instance. A quotation's consumed rows flip the direction,
  so `( n -- [ extent-a -- ] )` introduces no atom and
  `( n -- [ [ slot -- ] -- ] )` introduces a `slot`. A cell family's arguments
  are phantom and introduce nothing. Owner and private section are read from
  the engine's live namespace record and actual definition wordlist; mutable
  `CHECKER-PACKAGE-*` mirror state is not authority. Projections out
  (`fam -- n`, `ptr t -- n`, `[ … ] -- n`, `box<ptr u8> -- n`, `pfbox -- n`)
  need neither the owner nor a private section: store the projected identity
  and resolve it back through the owner's public words.
- `LINEAR: NAME ( payload -- PKG:tok )` mints a `DEFLINEAR` token and
  `LINEAR: NAME ( PKG:tok -- payload )` erases one: the reader-keyword shape of
  `CAST:`, an identity at its call sites, and the one checked way to make a
  token from a payload or erase one back into it. It is parsed and refused as
  a cast is first (`E-CAST-FAM`, the bad-signature reject, `E-CAST-ARITY`),
  then by its own rule (`src/core/checker.f` `LINEAR-CERTIFY`):
  - One side is a linear type and the other its payload, a non-linear con or a
    pointer chain ending at one (`E-LINEAR-PAYLOAD`, 7195): two tokens, no
    token, a type variable, a family, a quotation or an atom is refused.
  - The current package declared the linear type (`E-LINEAR-OWNER`, 7196).
    `DEFLINEAR` records the package it runs in, and one declared at top level
    has no owner, so nothing mints it.
  - The row has an unqualified name in that package's private section
    (`E-LINEAR-SCOPE`, 7197). A qualified name would publish into a public
    wordlist even while the declaring package is private.
  - The input and output stack tails agree (`E-CAST-ARITY`, 7129), because
    the emitted identity changes only its explicit payload or token cell.

  After `LINEAR: MINT ( ptr n -- PKG:tok )` and `LINEAR: ERASE ( PKG:tok --
  ptr n )`, the owner's checked words compose the two, `: PEEK ( PKG:tok --
  PKG:tok n ) ERASE dup @ swap MINT swap ;`, and its public words hand the
  token out and take it back; a caller holds it under the linear rules.
  `CAST:` still refuses a linear term, in the owner too (`E-CAST-LINEAR`).
  test/linear-suite.f runs the engine, the source pre-pass, tools/check.f and
  a reopened application image.
- Type, field and variant names are lowercase; generated and project words are
  uppercase.
- **Raw storage never holds an address.** A `variable`, `create` or `constant`
  cell, and any cell a `create … does>` definer makes, holds scalars, roles and
  atoms. Storing a pointer into one, fetching one out, or `ptr-field` over one
  is `E-RAW-CELL-PTR` (repair class `declare_pointer_cell`): `variable V : PEEK
  ( n -- n ) V ! V @ @ ;` does not certify. The declared forms: `PTR-VARIABLE`
  (effect `( -- ptr ptr a )`, in place of `variable` plus `0 ptr-field`),
  `PERSISTED-PTR-VARIABLE`, `TYPED-VARIABLE NAME ptr t`, `TYPED-BUFFER NAME ptr
  t`. See [effects.md](effects.md) "Raw storage never holds an address" for the
  open hole.
- **Nor an execution token.** The same cell, and either base address, refuses a
  quotation: `variable ZQW : ZQWQ ( -- ptr [ -- n ] ) ZQW ;` and `( -- ptr [ --
  n ] ) data-base 8 +` are `E-RAW-CELL-PTR` with the reason "an undeclared cell
  cannot hold an execution token / a quotation" and repair class
  `declare_xt_cell`. The declared forms: `TYPED-VARIABLE NAME [ in -- out ]`, a
  `TYPED-BUFFER`/`DYNAMIC-BUFFER` of `[ in -- out ]`, `defer`/`is`, and `xt!`,
  which declares the cell it writes. The null comparison on a declared code cell
  (`HK NULL-PTR =`) is refused; read such a cell's emptiness through a
  number-typed accessor of the same address.
- **A definer that only wants a type writes an EMPTY `does>` clause**: on a
  fresh word it is elided at both tiers, so a read costs one load; `0 ptr-field`
  in the clause is a body and pays a call, a branch and a frame per read.
- **A definer may replace another definer's `does>` clause.** Calling the inner
  definer creates the word; a nonempty outer clause then replaces its behavior
  and declared effect. Minimum input depth and the interpret-mode wide-value
  guard follow the replacement effect, including zero inputs or a scalar result.
  An empty replacement removes the earlier clause and restores the created
  word's original body.
- **`does>` patches the word the last `create`, `variable` or `constant`
  made, only while that word lives.** A forget, an `undefine`, a failed
  `evaluate` or a failed REPL line that retires it leaves `does>` nothing to
  patch, even when an earlier created word survives, and `does>` is then
  refused with `hb: does> has no created word` (rc 70, a catchable throw
  inside `evaluate`). Measured in `test/code-reclaim.f`:

  ```forth
  create K 7 ,
  : M ( -- ) ;
  create G 5 ,
  s" M" FORGET-DEFS-FROM
  : B ( -- ) does> ( -- n ) @ ;
  B
  ```

  A native engine boots with no created word, so a `does>` before any
  `create` is refused the same way. An image restored from a `--repl` build
  keeps its build's last created word instead, and its first `does>` patches
  that word (`tools/hb-build-repl-twin-test.f`).
- **A `STRUCTURE` or `ENUM` body is parsed by its definer.** A `\` comment
  inside the body is refused with `E-BAD-DECLARATION`; put comments above the
  opener, including comments explaining the header or fields.
- **A `FIELD` holds a value, not a body.** A payload is a type expression
  (letter param, concrete cell type, `ptr T`, closed arity-0 family, or a
  quotation `[ in -- out ]`). A quotation field is one execution-token cell;
  `MAKE`/`UNMAKE`, whole-record storage and a `DERIVE addr` accessor preserve
  its exact effect, so `FIELD handler [ request response -- ]` yields
  `ptr [ request response -- ]` and `@ execute` checks the call. A whole record
  stored in a `TYPED-VARIABLE` keeps its quotation callable after image restore.
- SwiftForth-style relocatable list words (`@REL`, `!REL`, `,REL`, `>LINK`,
  `<LINK`, `CALLS`) are outside the checked surface. Use structures for node
  layout, arrays and maps for collections, `case/of/endof/endcase` for dispatch
  and checked execution vectors for late binding.

## Words & factoring

Write checked, typed words with current `( in -- out )` effects. Keep related
one-line definitions together; separate multiline definitions with two blank
lines and place each word's comment directly above it. A readable word is
usually about five lines. A trivial straight-line wrapper may fit on one line;
`if`, loops, `case`, locals and multi-step stack work belong on separate lines.
Factor raw compiler and emitter code to the same standard.

- Build domain words or a checked DSL for structured rows, JSON/TSV, generated
  source, diagnostics, packets and repeated assertions. A named DSL construct
  executes the body it names (`[: ITEM ;] NAME-FILES`, `TEST:SUITE name …
  TEST:;SUITE`); keep helper and dispatch effects checked with typed
  quotations. Large `s"` literals and escaping logic belong at a tested
  boundary, not scattered through consumers.
- Guard a dependent read, index, owner or bounds operation with control flow
  before doing it. `and`/`or` combine already evaluated values, so combine
  predicates only when each is safe alone. Compute one argument per concept:
  extract `if/else`, comparisons and repeated `@`/`F@` from a dense call
  into a named word or local. A bool substituted for `n` may otherwise fail
  only at the later call.
- Prefer a classification table and named transitions to long `dup`/`over`
  token ladders. Split multi-pass work into named cursor, classification,
  validation, update and rendering words, then one orchestration word. `ROT`,
  `-ROT`, `PICK` and `ROLL` chains call for factoring or locals.
- About five or six input values is a useful upper bound for a word, counting
  a typed pointer as one value and its length as another. Pass each stage's
  result onward; derive metadata from its owner. A record should represent a
  domain value, not a bag of unrelated context. Apply the same judgment to
  allocation callbacks and low-level helpers.
- Locals `{: a:type b:type :}` bind inputs only, left to right from the
  deepest item: `1 2 {: a:n b:n :}` gives `a`=1 and `b`=2. Keep outputs in
  the stack effect. Annotate a known concrete type; use a bare local when
  the entry effect carries richer detail than the annotation can express.
  An entry group goes on its own line after the name and effect, indented
  three spaces with the body:

  ```forth
  : CLAMP ( n n n -- n )
     {: v:n lo:n hi:n :}
     v lo max hi min ;
  ```

  New and changed definitions follow this layout. A local name is at most
  16 bytes and a definition binds at most 64 locals. The compiler refuses
  either before storing anything (rc 70); the checker reports
  `E-LOCAL-NAME-TOO-LONG` or `E-TOO-MANY-LOCALS` with the limit.
- A local binds once. Keep a loop's changing cursor on the stack or in a cell:
  `begin {: cursor:n :} … cursor' again` pushes the next value before the
  back edge. Use `RECURSE` for a cursor only when depth is bounded; a
  peer-controlled count can overflow the task stack. Keep locals shallow so
  nested helpers cannot clobber caller indexes.
- A `ptr` annotation does not fit a byte span. Under `( ptr u8 n -- n )`,
  `{: p:ptr :}` is refused at `:}` (`expected: ptr n actual: ptr u8 n`).
  Bind a bare local or factor a helper retaining the detailed effect. Give
  same-type numeric slots names before reordering them: `( cap used add -- )`
  can type-check a wrong `swap`.
- Locals are block-scoped and may bind on a live path inside an `if` arm,
  `case` arm or loop. They die at its closer; bind before a join if the name
  must survive it. A group after a closed early-exit guard is live; directly
  after unconditional `exit`, `leave`, `throw`, `die` or `again` it is dead
  code and is refused.

## Files

- **One concern per file**: parser, renderer, DB, data table and driver are
  separate files, split at responsibility boundaries.
- **Reusable helpers live in libraries**; shared behaviour lives in one owned
  file. Run multi-file tools as `hb --load src/lib/a.f src/lib/b.f tool.f -- args…`:
  sources before `--`, `SCRIPT-ARGV$` after it, fd 0 still tool data when stdin
  is not a tty. Use `--load` only with more than one source file.
- **A leading `#!` line is a comment** in a program `hb` runs from a file or
  standard input, in every file `--load`, `include` and `require` load, and in
  the subject `tools/check.f` checks. A `#!` anywhere else is an ordinary token.
- **Keep physical lines short.** Factor long `--load` builders and check-source
  appenders; a line near the interpreter input buffer truncates and surfaces
  later as unrelated top-level words.
- **Script argv is explicit.** `hb tool.f arg…` treats `arg…` as script
  arguments; read them only after `SCRIPT-ARGC`, since `SCRIPT-ARGV$` for a
  missing argument faults today instead of throwing.

## Stack comments

- **The stack effect is the contract; prose is not.** Every definition carries a
  current `( before -- after )`; body lines carry a trailing `\ ( before --
  after )` only where the stack state is not obvious. No empty stack comments;
  many line comments mean factor.
- **Checked definitions use type tokens only**: `( n n -- )`, `( bool -- )`, `(
  ptr u8 n -- )`, never role prose such as `( got want -- )`. Nominal roles such
  as `idx`, `len`, `count`, `fd`, `rc`, `reg`, `label`, `va`, `symidx`, `asm`,
  `img` and `snap` are real types; same-cell values need them, with negative
  fixtures, because `( n n -- )` hides swaps. Informal names go in locals (`{:
  got want :}`), helper names or prose.
- **Real types, not reflexive `n`.** A string is `ptr u8 n`; a dereferenced cell
  address is `ptr a`; a pointer-valued cell keeps its nested pointer role; `n`
  is a genuine scalar. `ptr a` is only for a body that keeps the pointee
  parametric, since a declared effect must stay parametric over its quantifier:
  reading the cell as a number makes it `ptr n`, and so does returning a raw
  cell view of allocated bytes (`( -- ptr a ) 64 MEM:BYTES-ALLOC-LEN
  MEM:ALLOC-BYTES drop CELL-VIEW` is `E-NONPARAMETRIC-EFFECT`; declare `( -- ptr
  n )`). A `{: p:ptr :}` local admits cell `@`/`!`: `F` below certifies and
  answers the stored cell, while `G`, reading a parametric pointee as a cell, is
  `E-NONPARAMETRIC-EFFECT`. A word over "some cell buffer base" therefore
  declares the concrete pointee (`ptr n`), not `ptr a`; concrete per-buffer
  words sharing scalar cursor helpers remain a fine shape, not a forced one.

  ```forth
  : F ( ptr n -- n )
     {: p:ptr :}
     p @ ;

  : G ( ptr a -- n )
     {: p :}
     p @ ;
  ```
- **Reserved names cover constants and variants.** `MATCH` and the other compile
  keywords name words, not constants, so `MATCH` cannot name a package constant
  even under `public`; a `VARIANT` named by a reserved or taken word fails "name
  is reserved or already taken" (`VARIANT x`). Pick names that collide with
  nothing visible (`horizontal`, `vertical`).
- **Declare application nominals with `DEFTYPE`** (`require
  src/lib/type/deftype.f`): top-level `DEFTYPE NAME` mints a package-scoped type
  with converters `>NAME ( n -- name )` and `NAME>N ( name -- n )`, the only
  crossing; the type tail is the lowercase fold (`SERIAL` → `serial`, so `(
  serial -- n )`); `DEFTYPE SERIAL` in `CAMERA` and in `FRAME` are distinct;
  unknown tokens stay errors. Substrate: `docs/value-nominal-substrate.md`.
- **`DEFLINEAR` for owner and lifetime tokens.** A linear token is nominal and
  noncopyable: `dup`/`over`/`2dup`, `drop`, `@`, `!` and by-value record
  duplication reject when they would duplicate, discard, load or store it; only
  words whose effect names the linear type create or consume it, and in checked
  code only the declaring package's private `LINEAR:` rows make one from a
  payload or erase one (**Structures And Enums**); a `TRUSTED:` word whose
  effect names the type is asserted, not checked.
- **Raw role casts are not validators.** `>LEN`, `>IDX`, `>COUNT`, `>OFF`,
  `>ASM`, `>IMG`, `>SNAP` are trusted identity boundaries; libraries expose
  checked constructors and role helpers so swaps fail under `CHECK!`.
- Unchecked prose-only comments may name roles when no hook consumes them; keep
  the shape obvious. Add inline `( … )` at non-obvious points in a longer word.
  Standard notation: `x` cell, `n`/`u` signed/unsigned, `d` double, `c-addr u`
  string, `xt`, `nt`, `f`/`bool` flag, `?` maybe-present.

## Checker & type model

- **C2 views carry checked lifetimes and access authority.** The
  [ownership model](ownership-model.md) defines shared `read-view<p,q,T>` and
  exclusive `mut-view<p,q,a,T>` values, lexical loans, initialized fields and
  task-local cleanup. A view occupies two cells and travels as one value through
  stack permutations. Ordinary `ptr T` and `SPAN` remain lifetime-free.
- **Prefer owner-scoped bump allocation when practical.** Many small objects
  sharing a lifetime belong in chunks released together. Allocate a chunk with
  `capacity MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES`; keep its original base,
  returned `NUM:alloc-byte-len` and used byte offset in the owner. These raw
  mappings are caller-owned; the owner supplies their cleanup.
  For cell-bearing objects, compute padding to the next `CELL` boundary.
  Check padding against the remaining extent, then the object's size against
  the remainder after padding, before advancing the cursor. Return
  the chunk base plus that aligned offset. When it does not fit, allocate
  another chunk; never resize or move a chunk whose pointers have escaped.
  Pointers stay valid until the owner closes. On close or failure cleanup,
  release each original `(base, allocation length)` exactly once through
  `MEM:RELEASE-BYTES`, not an interior pointer or the used length. Individual
  allocations remain appropriate for independent lifetimes or when bulk
  ownership would retain significant dead storage.
- **`CHECK!` is the user contract.** `CHECK` proves internal consistency; user
  builds verify the body against the declared effect and make rejection fatal.
  Tests for bad programs assert build rejection, not runtime failure.
- **Typed booleans are `bool`**: produce them with `0 0=`, `0 0= 0=` or domain
  helpers; never store raw `0`/`-1` in a `ptr bool` cell or compare bools with
  `=` (`bool bool` is refused).
- **Quotations are xts, not closures.** `[: … ;]` cannot read surrounding
  locals: declaring or referencing a local inside a quotation is
  `E-BAD-LOCAL-SHAPE`, checker and compiler reject local references while any
  quotation is open, including in an enclosing quotation after an inner `;]`,
  and on the JIT tier a `{:` group inside `[: ;]` is refused. A body that needs
  locals inside a quotation becomes a named private word. A value a `catch`,
  `finally` or locked body needs travels through storage it can address; where
  it differs per task, that storage is the task's own slot (a `TASK:+USER` cell
  or a typed-buffer row indexed by the task) that the quotation reads for the
  running task. Quotations nest: each `[:` inside another opens its own body,
  with its own inputs, calls and `exit`, and the enclosing body resumes after
  its `;]`; at most 32 are open at once (**Engine limits ordinary source
  reaches**).
- **A handle over caller-owned storage is a linear token its package mints
  and erases, or a checked `ptr PKG:type`.** The token is a `DEFLINEAR` type
  with `LINEAR:` rows in the owner's private section (**Structures And
  Enums**), never a `TRUSTED:` mint, state and consume trio: `CAST:` refuses a
  linear operand, behind a pointer or in a quotation row too (7137
  `E-CAST-LINEAR`). Without a lifetime to prove, a public `STRUCTURE` plus a
  `TYPED-VARIABLE` or `TYPED-BUFFER` in the caller trades it for a runtime
  refusal off the definer's zero image (`src/lib/json-write.f`).
- **Concrete integers widen, roles do not.** `u8 -> u16 -> u32 -> cell/i64`
  widens implicitly when lossless; concrete narrowing and same-width sign
  changes need an explicit conversion. Generic `n` accepts structural integer
  stack cells in either direction: an `n` input can pass to a `u32` parameter,
  while an `i64` input cannot. This generic rule does not relax pointer
  pointees. Nominal roles (`idx`, `len`, `fd`, `rc`, `pid`, `asm`, `img`,
  `snap`, …) never widen to each other or to bare integers.
- **Pointer-valued cells use cell-indexed `ptr-field`**: `ptr-field` builds a
  `ptr ptr x` field whose index is a cell slot, not a byte offset, so `@`/`!`
  keep nested pointer types; never multiply by cell size. Raw byte offsets need
  a checked view or a modeled byte-offset primitive; byte-offset header access
  goes through checked views with explicit alignment and bounds; a missing
  primitive model is fixed, never cast around. The base must be a declared cell:
  `ptr-field` over raw storage is refused at the token.
- **Byte pointers are not cell pointers.** `ptr u8` is a byte span read with
  `c@`/`c!`; cell `@`/`!` over a concrete `ptr u8` is a checker error. A cell
  that stores a byte pointer is `ptr ptr u8` through `ptr-field`, then `@`/`!`.
- **State cells need typed public effects**: `-- ptr n`, `-- ptr bool`, `-- ptr
  ptr u8` plus a separate length cell for strings; never TRUST rows.
- **Path-sensitive control is a checker invariant.** `LEAVE`, `EXIT`, `throw`,
  `die` and `again` fold or kill paths per their control effect; divergent path
  arities are soundness bugs; after a dead path only structural closers (`else`,
  `then`, `loop`, `+loop`, `repeat`, `again`, `;]`) may follow.
- **Discharge counted loops before `exit`.** Each `do`/`?do` opens a frame;
  `unloop` removes the nearest; an `exit` needs one `unloop` per active loop in
  that definition or quotation; live branches agree on the remaining frames; a
  back edge or `leave` still owns its frame. `i`/`j` read the nearest and next
  frames of the current quotation or definition, never an enclosing quotation's;
  loop frames are separate from the typed return stack. A `do` whose every body
  path returns or throws has no normal continuation; `?do` keeps its zero-trip
  exit; `leave` is the explicit exit. `do` always takes its first turn. `?do`
  tests its bounds by its closer's rule before that turn: `?do … loop` enters
  only while start < limit, signed, so `-1 0 ?do` and
  `$8000000000000000 0 ?do` skip as `0 0 ?do` does and a count at or below
  zero takes no turn; `?do … +loop`
  skips only equal bounds, since its step may count down to a limit below the
  start. `+loop` adds its step wrapping and ends only when the index crosses
  between limit-1 and limit in the step's direction (Forth 2012 6.1.0140):
  equal bounds run once with a negative step and a whole cycle with a positive
  one, unlike `loop`. The checker models frames, not trip counts, so the entry
  rule changes no effect.
- **`RECURSE` uses the declared effect**, a fresh copy per call; keep the raw
  declared signature stable after `CHECK!`.
- **Checked `catch` is quotation catch**: `[: WORD drop ;] catch`, consuming
  success outputs inside and keeping the exact code as data at an explicit
  recovery boundary; no arbitrary-xt catch. The quotation is stack-preserving,
  because `catch` unifies the live stack with its inputs AND outputs: `( n -- n
  )` is accepted, `( n -- )` rejected. A value the caught code needs travels on
  the data stack through the quotation and back on every branch; no staging
  variable. A nominal handle cannot cross `catch` as a quotation's result; a
  one-slot `TYPED-BUFFER` holds it.

  ```forth
  : W ( n -- n )
     [: dup USE … ;] catch
     {: rc:n :}
     CLEANUP rc 0 <> if rc throw then ;
  ```
- **`finally`** is `( R [ R -- S ] [ -- ] -- S )`: body, then cleanup on return
  or catchable throw; cleanup takes and leaves nothing; a body error rethrows
  after cleanup, a cleanup error supersedes it; `die` skips cleanup. Implicit
  tails such as `[ -- ]` enforce their windows through wrappers and typed
  storage; name rows explicitly for generic callbacks (`[ R -- S ]`).
  A zero-initialized typed quotation may be fetched or dropped. Calling it
  through `execute`, `catch`, `finally`, or `run-in-stack` exits 86 with
  `hb: unset quotation` on stderr; `catch` cannot recover that fatal error.
- **Higher-order signatures publish themselves** once `CHECK!` passes (`DIP`,
  `KEEP`, row callbacks); no TRUST row to pin a scheme.
- **Function passing is checked.** A quotation parameter (`[ a a -- bool ]`, `[
  a -- a ]`) is verified through a call chain AND inside a `?do`/`begin` loop:
  bind it as a local, thread it, `execute` it; heapsort, map, fold and filter
  all check. `src/core/combinators.f` (MAP/FOLD/EACH) is an unchecked boundary,
  not a model.
- **Execution vectors are typed `defer` words.** `defer ACTION ( in -- out )`
  declares the effect; `: INIT ( -- ) [: IMPL ;] is ACTION ;` installs it, the
  checker proving the quotation matches exactly. No `variable`/`@ execute`
  tables, no `['] IMPL is ACTION`. An unset deferred word fails closed with the
  execution-vector error. A fixed engine callback cell stores one checked bridge
  (`[: ACTION ;] CELL !`) and changes only through `[: IMPL ;] is ACTION`.
  `@EXECUTE` is no replacement until its zero no-op has a checked model.
- **New type tokens need a checker-only bootstrap stage**: old `bin/hb` rejects
  unknown stack-comment tokens, so add parser, renderer and `CC-*` support,
  refresh the native binary, then use the token in axioms and definitions.
- **Phase tokens reach the side effect they order**: `asm`, `img`, `snap` flow
  through the final sign/write/header operation, not an early wrapper.
- **Seal the implicit row under declared inputs**: a stack-preserving trusted
  effect (`img -- img`, `fd -- fd`) must not satisfy output by binding an
  implicit base row that hides underflow.

## Integer arithmetic

Cells are two's-complement 64-bit. The signed bounds are
`$8000000000000000` and `$7FFFFFFFFFFFFFFF`. `+`, `-` and `*` wrap
(`$7FFFFFFFFFFFFFFF 1 +` is `$8000000000000000`) and never refuse. Division
is the one partial operation; its two boundary cases are contracts every
backend answers alike.

- **A zero divisor throws `E-DIV-ZERO`.** `/`, `mod` and `/mod`, the only
  dividing primitives, test the divisor and throw (`src/lib/errors.f`;
  `src/habu/prims.f` re-registers the code for the emitters, which compile
  before `src/lib/`). It is a catchable throw, not an exit: `[: a b / drop ;] catch
  E-DIV-ZERO = if … then`. A structurally positive divisor says so in the type:
  `src/lib/num-arithmetic.f`'s `positive-divisor` role makes the refusal
  unreachable. **The contract holds at every tier**: the interpreted primitive,
  a word compiled with `1 set-tier` and an AOT-built executable all throw the
  same code.
- **`$8000000000000000 -1 /` is `$8000000000000000`, and
  `$8000000000000000 -1 mod` is `0`.** The quotient `2^63` has no signed cell,
  so it wraps like `+`, `-`, `*`. A backend whose divide traps on this quotient
  (x86_64 `idiv`) tests for the `-1` divisor and answers quotient
  `$8000000000000000` and remainder `0` without executing it.
  `test/prim-parity.f` pins both contracts.
- **`.` prints every cell, including `$8000000000000000`**, and `FMT:SB-INT`
  prints the same digits.

## Errors

- **External error codes use `-999999..-100000`, inclusive.** Applications and
  separately distributed libraries allocate their own codes in this range.
  Habu's runtime, compiler, bundled libraries, tools and tests allocate outside
  it. Existing Habu codes, Forth-specified codes and process exit statuses keep
  their meanings.
- **Different errors in one process need different numeric codes.** Package
  names qualify constants, not thrown values. External packages choose
  nonoverlapping blocks within the external range; the application integrator
  resolves collisions among dependencies. Applications that never run together
  may reuse blocks. Re-export an existing error through its owner's constant.
  This is an allocation convention: the checker does not enforce it, and Habu's
  error-code lint scans this repository, not external dependency trees.
- Engine process failures use only the sealed `ENGINE-ERROR` package ABI:
  `SEAL-VIOLATION` 83, `SEAL-PACKAGE` 84, `BAD-TAG` 85, `CALLABLE-ABI` 86,
  `CATCH-STACK` 87, `CODE-CERT` 88, and `OVERLAY-OPEN` 108 for a definition,
  a new package, a wordlist or an `undefine` while a checker overlay is open.
  No global `E-*` aliases; native and no-binary recovery consume the same
  qualified names and values.
- **Fallible words `throw` a named code** (`src/config.fs`, e.g. `E-MISMATCH`),
  never a silent failure or an out-of-band flag.
- **`catch` only at explicit recovery boundaries**: REPL/CLI wrappers, test
  assertions, stack-preserving outcome adapters returning the exact code. No `…
  catch drop`, no `catch 2drop`, no masking.
- `abort"` only for proven-impossible states, with a message.
- **Interactive support recovers; builders may exit.** Recoverable interactive
  failures `throw` into REPL recovery (`?`, rollback, reread); `die` is for
  build-time makers and CLI boundaries where exiting is the contract.
- **`throw` and `die` are different control effects**: `throw` is catchable, the
  checker's exception edge; `die` terminates, no-return metadata. Never add
  dummy outputs after `throw` to balance a branch; fix the exception model or
  track the gap.
- **`die` consumes a real message and code**, `( ptr u8 n n -- )`, never `0 0`
  as a fake string; model exits as no-return only at certified wrappers.
- **`die` writes its message as one line**: the span and then exactly one
  newline, so a message never carries its own `\n`; an empty span writes
  nothing, which is what a site passes after ending its own report with `cr`.
- **A word that never returns ends its branch.** After a call whose body ends in
  `die` or `throw`, the checker refuses any further token in the same branch,
  `exit` or a balancing value alike, as `E-DEAD-CODE`:

  ```forth
  ARG s" child" TOK= if CHILD-MAIN exit then
  \ habu: in dispatch: at 'exit' after 'CHILD-MAIN'
  \ hook: non-certified definition: dispatch at 'exit'
  ```

  Write `if CHILD-MAIN then`: the rest of the definition runs only on paths that
  can return.

## Engine limits ordinary source reaches

These ceilings are reachable from plain Habu rather than a runaway; each refuses
by name with the count it saw and the ceiling, and none truncates.

- **A definition's captured source text: `BODYBUF-CAP`, 8000 bytes**
  (`src/habu/layout.f`): every token of a body (name, stack comment, words,
  string literals with closing quote) plus one separator each. Nine 900-byte
  `s"` arms in one `case` reach it. Past it: `hb: definition body text full at
  8000 bytes: <name> needs <count>`, rc 71, catchable inside `evaluate`. Repair:
  move the long literals into words of their own. The same constant bounds the
  source verifier's body buffer (`E-VS-BODY-CAP`) and the native compiler's unit
  text (`E-NCOMP-TEXT`), and it is the only bound on a definition's name, on
  both tiers.
- **One REPL line: 255 bytes** (`src/habu/repl.f` `LLINE-MAX`). A longer line is
  refused, `hb: repl line over 255 bytes: <length> typed`, and read again, never
  truncated, evaluated or saved to history. Load long definitions from a file.
- **`begin` nesting in one definition: `JIT-SNAP:FRAMES`, 28**
  (`src/habu/layout.f`), the JIT's value-stack snapshot frames per definition.
  Past it: `hb: BEGIN nesting full at 28 frames: <name> needs <depth>`, rc 75.
  Factor the inner loops into their own words.
- **Quotation nesting in one definition: `JIT-QUOT:LEVELS`, 32**
  (`src/habu/layout.f`), the most `[:` bodies open at once, which is the
  checker's control-frame capacity. Past it:
  `hb: quotation nesting full at 32 levels: <name> needs 33`, rc 75, catchable
  inside `evaluate`; `tools/check.f` refuses the same source first,
  `E-UNCHECKABLE`. Factor the inner quotations into named words. The files
  `tools/bootstrap.sh` compiles allow one: `hb-stage0` holds a single open
  quotation (`J-QUOT`, `bootstrap/cg/forth.fs`), so a nested `[:` there prints
  `[:` and exits 75 in the Gforth recovery build while `bin/hb` accepts it.
- **`do` and `?do` nesting in one definition: `LV-LEVELS`, 16**
  (`src/habu/layout.f`), the JIT's per-level `leave` chain and `?do` entry-test
  records. Past it: `hb: control-flow nesting too deep: do` (or `?do`), rc 70.
  Factor the inner loops into their own words.
- **A family, variant or field name: `TF-NAME-MAX`, 255 bytes**
  (`src/core/type-family.f`), the longest package name, so every constructor
  and member spelling derived from it fits. `SUMTYPE`, `PRODUCT`, `NEWTYPE`,
  `STRUCTURE` and `ENUM` refuse a longer one with `E-TDECL-CAP` (7118):
  `tools/check.f` reports `E-BAD-DECLARATION` with the name as its token and
  the reason `name longer than 255 bytes`, rc 70; `--load` prints `habu: bad
  <kind> declaration '<family>': name longer than 255 bytes at '<name>'` and
  exits 70. Shorten the name.
- **A package name: `CHECKER-PACKAGE-CAP` less one, 255 bytes**
  (`src/core/checker.f`). `package`, and `using` of a namespace a qualified
  definition made, throw `E-PACKAGE-NAME-CAP` (7154) out of the statement for a
  longer one: `tools/check.f` reports `E-STATEMENT-THROW` with that throw code
  at the name, rc 70; `--load` exits 67 with `hb: uncaught throw code 7154`. A
  type qualified by a longer name names no package, `E-UNKNOWN-SIGNATURE-TYPE`.
  Shorten the name.
- **A definition's effect: `EFFECT-DEPTH-MAX`, 4096 levels deep**
  (`src/core/checker.f`): a row of 4096 cells, where a cell's type adds the
  levels it nests, so a returned quotation's row counts from the cell that
  holds it. Recording an effect and instantiating it for a caller recurse once
  a level, and half the walk's depth budget and of the 8192-cell data stack is
  left for what the stacks already hold. Past it the definition is
  uncheckable: `E-UNCHECKABLE` with the reason `effect too deep to record
  (depth <depth>, at most 4096)`, rc 70 from `tools/check.f` and from `--load`,
  which adds `hook: non-certified definition: <name>`. A `trust` row that deep
  is refused as a bad one: `E-BAD-STORED-SIGNATURE` under `fix_signature_size`,
  with the same reason, as `tools/check.f` refuses a `defer` or `TRUSTED:` row
  that deep; `--load` stops such a definer first at its 8000-byte text (`hb:
  definition body text full`, rc 71). Keep bulk values in a buffer, not on the
  stack.
- **A definition's input row: `EFFECT-MIN-IN-MAX`, 255 cells**
  (`src/core/checker.f`), the most a call must provide that a published
  word's record holds, in eight bits. Declared or inferred, a wider input row
  makes the definition uncheckable: `E-UNCHECKABLE` with the reason `input row
  too wide to record (<cells> cells, at most 255)`, rc 70 from `tools/check.f`
  and from `--load`. A `RECURSE` in a body declared that wide is refused for
  the same reason, and `tools/check.f` refuses rather than defers a body
  calling a word only the run defines. Nothing is recorded for it, so callers
  find no effect; under `--all-errors` a refused definition with such a
  declaration keeps no record of it either. A stored signature that wide, a
  `trust` row's, a `TRUSTED:` definition's or a `defer`'s, is refused as a bad
  one: `E-BAD-STORED-SIGNATURE` under `fix_signature_size`, with the same
  reason. Pass bulk values in a buffer, not on the stack.
- **Data space: `DATA-SIZE - PROF-CNT-BYTES`**, 33,030,088 bytes on
  linux-aarch64 (`src/os/linux/layout-constants.f`, `src/habu/layout.f`;
  `DATA-SIZE` is per host). `allot`, `align`, `,`, `c,`, `create`/`variable`/
  `defer` and the interpret-mode string literals that keep their text (all but
  `."`) advance the DP through `DP-CHECK` (`src/habu/habu1.f`). Past it:
  `hb: data space out of
  range: DP <dp> of <cap> bytes`, rc 76, catchable inside `evaluate`, both
  numbers offsets from `data-base`. The refusal names no definition; it reports
  the line it came from (` at <path>:<line>`, added when a source file is open).
  Repair: hold bulk data in `MEM:ALLOC-BYTES` or a `DYNAMIC-BUFFER`, which map
  their own pages, not in the dictionary. While a task is live every one of
  these sinks exits `$4F` before it writes, as do `evaluate` and
  `evaluate-closed` ([threads.md](threads.md)): the definers name their token
  on stderr, the rest print nothing.
  Tier 1 keeps every string literal body and trap message in DATA too
  (`src/compiler/native/string.f`): when the store's last segment cannot take
  a body it opens another, 512 KB of bodies and 8192 rows, at `here`, so data
  space is the only bound on their count and total size. A rewind of DATA - a
  failed `evaluate` or REPL line - stops above the newest segment
  (`DATA-FLOOR-CELL`), and equal bodies share an address within one segment.

## Constants

- **Named constants, no magic numbers.** Limits and codes live in
  `src/config.fs`; a literal is acceptable only for a true primitive of the
  encoding (the `3`/`7` of the 3-bit tag), with a comment.
- **Default to `$hex`**: byte values, masks, addresses, memory, struct and byte
  offsets, field strides, syscall and exit constants, instruction encodings,
  ASCII codes (`$FF and`, `$D10043FF`, `$200`, `$40`). Decimal only for genuine
  small counts (loop bounds, arities, shift amounts, register indices) and
  ordinary human quantities. Crypto and format constants follow the spec's hex
  spelling. The standalone parses `$hex` case-insensitively with an optional
  leading `-`.

## Testing

- Exercise changed behaviour through checked assertions, meaningful errors and
  edges included, through public entry points rather than a test per trivial
  helper. `T=`/`T<>` for scalars, `T$=` for strings, `TTRUE`/`TFALSE` for flags
  (not `T=`), `TTHROWS` for codes; several results compared top down, one
  assertion each. A regression distinguishes the defect it prevents.
- Tests live in the native gate (`test/engine-suite.f`, focused `tools/*-test.f`
  fixtures and source-specific checks in `test/run.f`). `src/lib/test.f`
  supplies suites, groups, hooks, filters and reports; test files require their
  dependencies.
- Runners keep no suite iteration state on the return stack while a test runs:
  tests may `catch`/`throw`, so loops use explicit index/count cells that a
  caught throw cannot truncate.
- Fixture helpers live in a private package; install them through `TEST:*`
  hooks rather than global stems.

```forth
require src/lib/test.f

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

TEST:GROUP SEQ smoke
TEST:SUITE sample
   feature-test.f -- arg
TEST:;SUITE
TEST:;GROUP

TEST:RUN
CELL-N @ 1 T=
T-REPORT

;package
```

  `src/lib/test.f` exposes `T*` assertions; `TEST:SETUP!`,
  `TEST:TEARDOWN!`, `TEST:DRAIN!`, `TEST:ARGS-BEGIN!`, `TEST:ARG+!`,
  `TEST:RUNNER!`, `TEST:STDIN-RUNNER!` install typed hooks; `TEST:GROUP SEQ|PARA
  name` opens a group (the mode token is mandatory, before the name);
  `TEST:;GROUP`, `TEST:SUITE`, `TEST:SUITE-STDIN`, `TEST:;SUITE` and `TEST:RUN`
  define and run. No helper globals such as `FOO-TEST-SETUP-N`.
- Assert the specific outcome: inside checked definitions `[: WORD ;] TTHROWSQ`
  (it runs a `( -- )` quotation) or another stack-preserving `catch` with the
  exact code; top-level scripts that cannot push quotations use `' WORD
  TTHROWS`; diagnostics are captured and matched by substring.
- Run focused fixtures through their owning `tools/*-test.f` so assertion
  failures set the exit code.
- Property generators use `src/lib/property.f`: `PROP:SEED!` makes a run
  reproducible, and `PROP:RND%` mixes the generator's high bits before bounding
  a draw. Small bounds do not inherit the raw generator's short low-bit cycles:
  bound 2 produces repeats as well as alternations. The fixed-seed regression
  checks that each occupies 40–60% of 4096 transitions; this is a coverage
  guarantee for that sequence, not a claim of independent or cryptographic
  randomness.
- A claimed false reject needs a reduced checked refusal and a matching
  unchecked execution of the declared effect. Probe new signature tokens with
  `ATOM-TOK?`, `TOK-TYPE` and renderer output before rebuilding.

### Diagnosing a checker miss

Reduce the failing checked program, identify the violated contract, decide
whether the defect is in a declaration, the checker, generated code or runtime,
fix that layer, and add a regression through the actual load path. A property
outside the type system's contract is stated as such and checked at the right
runtime or analysis layer. No template or task record is required.

## Verification before committing

The required gates live in [gate.md](gate.md). Timing samples require
1-minute load below 4 with no competing engine; a measurement script refuses
if the host never quiets. For compiler, runtime or broad library changes,
rebuild the engine and run the full native suite:

```sh
bin/hb --load tools/build-fixpoint-refresh.f -- install --force
bin/hb --load test/run.f
```

`test/run.f` reports `suites: ran N of N`, the red set and a nonzero exit if
any suite fails. Documentation-only edits need no rebuild.

## Comments & hygiene

- `\` line comments, terse, in one or two lines: only what the code cannot state
  (units, ownership or ordering invariants, a hardware encoding, a checker
  boundary, why a non-obvious choice is required). Meaning lives in names and
  factoring; design belongs in `docs/`.
- A comment states what is true now. When a claim needs proof, name the fact
  that proves it: the test, the refusal and its code, the measured behaviour.
- No history: no "used to", "no longer", "was", earlier designs, what was tried,
  dates, dot, commit or review ids, agent or person names. A failure that
  motivates a rule is stated as a present fact: "a stage without X dies
  `E-UNDEFINED` at Y", never "when this was tried it died".
- No restating the code, no design essays, no arguments for or against a
  design. A fact lives in one comment or one doc section; others point to it.
- A change that makes a comment false corrects the comment in the same change.
  Remove scratch and debug prints before commit.
- A definition that fails to compile in raw engine mode reports the undefined
  word on stderr and may spill the rest of the definition through the
  interpreter. `tools/check.f --all-errors` and `--verify-only` report every
  refused definition, one with an undefined word included, as a schema-1 JSON
  diagnostic under `--json-errors` and always under `--verify-only`, and check
  each later definition against a refused one's declared effect
  ([repair-diagnostics.md](repair-diagnostics.md)). A duplicate definition
  ends `--all-errors`, as it ends the load; `--verify-only` reports it with the
  same record and goes on past it. Without them check.f stops at the first
  refusal. Under `--json-errors` or `--verify-only` its prose goes to stdout
  and its stderr carries packets, one for a require closure it cannot follow
  among them, at the form that stops the walk; under `--json-errors` packets
  alone, as it says on stdout, too, a throw that ends the check uncaught.
  A definition with several faults is reported for the one the load
  refuses it for, at that fault's token: the first body word the load cannot
  compile (undefined, called or ticked, a local it cannot bind, a construct or
  match it cannot read, a control-flow word with no structure or another's
  open, a name under `using` that a tick or the engine's lookup refuses), then
  a structure still open where the definition ends, at its `;` or `does>`, else
  the first name a used public shadows or makes ambiguous beside a global,
  else a signature that does not parse, else the first failing check. A word
  the engine holds that no checker row types, as one defined without a
  declaration while checking was off (`0 set-check`), is no word the load
  cannot compile: its tick is admitted, and its call is a failing check,
  `E-UNDEFINED`. A word after a terminating word, or inside a match nested
  past the checker's control-frame depth, is not resolved, though the load
  names it if undefined.

## Habu Native Tooling Gotchas

- **Native debugging**: [debugging.md](debugging.md) covers `.s`, `BPW+`,
  `step`, compiled-word breakpoints (`BP+`, `BP*`, `BPN`), `tools/jitdump.f`
  and `tools/imgdump.f`; `using DEBUG` opens the baked debugger.
- **Semantic xref is in-image**: ownership, references and call RCA use
  `XREF`/`SEE`/`USES`/`USED-BY` in the live image with CLIs as thin wrappers;
  source search where it answers, native inspection where runtime state matters.
- **Boundary spawns attribute failures.** Outcome capture for expected
  timeouts and failures reports suite/case, phase, executable and argv/load
  list, outcome kind/code, named rc when known, capture bytes/capacity and
  stdout/stderr. Throw-on-timeout capture is for a test of that throw.
- **`DYNAMIC-BUFFER NAME Type` for growing typed tables**: the stored types of
  `TYPED-BUFFER`, closed non-linear layouts included, **and `u8`** — the
  growable byte row. It is the one definer that takes `u8`, the only sub-cell
  type with accessors of its own: `DYNAMIC-BUFFER BYTES u8` loads and `0 BYTES
  c@` reads byte 0 (test/dynamic-buffer.f), while `DYNAMIC-BUFFER X u16`
  is refused. `count NAME-RESERVE` allocates at least that many elements and
  keeps contents; `index NAME` answers `ptr Type`, rejecting negative and
  beyond-capacity indices; a smaller reserve keeps the allocation; growth may
  move it, so retain indices and reacquire pointers. A growing reserve copies
  the whole old capacity, so stale cells beyond the old count survive and the
  accessor bounds by capacity, not the requested count: a column whose zero is a
  default is cleared over the newly exposed range by the reserving word.
  `NAME-RELEASE` frees the mapping and is safe to repeat. Mappings are
  transient: release before saving an image; image-retained values use
  dictionary storage. With `u8`, `index NAME` is `( n -- ptr u8 )` at byte
  `index`, read and written with `c@`/`c!`, never cell `@`; `count NAME-RESERVE`
  counts bytes, and since every capacity is a whole number of cells, the bound
  is the request rounded up to a cell.
- **Runtime-sized buffers use OS mappings.** `create … allot` consumes
  dictionary DATA; `MEM-ALLOC-BYTES` and `MEM-ALLOC-64K-BUFFERS` from
  `src/lib/memory.f` permit contiguous or separate 64K buffers and spans,
  bounded by cell-size overflow and OS allocation failure.
- **`create … allot`, `BUFFER:` and `TYPED-BUFFER` allot zeroed space on a
  cell-rounded address.** `create`, hence `variable`, rounds the data pointer to
  a cell before publishing, and `align` does on request. So a `create … allot`
  block and `MEM-ALLOC-BYTES` memory are both cell-aligned for a cell-typed
  reader such as `src/lib/json-read.f` `INIT`, and a row a foreign call reads as an
  aligned C object needs no alignment word of its own (src/lib/net/curl.f's fd_sets
  and out-parameter cells). `allot`, `c,` and `,` never realign: storage carved
  after byte-sized data is misaligned unless `align` precedes it, and such a
  reader refuses it (`E-STORAGE`).
- **Missing convenience words are not bugs in the standard.** Core lacks `pick`,
  `within`, `s>number?`, `move`, `fill`, `erase`, `bl` and `*/`; each is
  `E-UNDEFINED` in a checked body (`: W ( -- ) bl ;`). Use cells, explicit
  increments and comparisons, and the checked byte and string helpers. `0<>`,
  `true`, `false`, `fdup`, `fover`, `fdrop`, `f<=` and `f>=` come from
  `src/lib/prelude.f`, which the engine already provides: writing `require
  src/lib/prelude.f` documents the dependency but is not load-bearing. Never
  re-derive `0 0=` / `0 0= 0=` by hand.
- **Legacy checker preludes rebind the existing hook**: a prelude that disables
  checking after `src/core/check-hook.f` restores it at once with `' HOOK
  set-check`. A migration constraint, not permission for new spans. No second
  hook name in baked tty/stdin bundles (duplicate enforcement fails closed at
  startup); a snapshot or AOT stage keeps a different hook local and leaks no
  duplicate REPL hook into `bin/hb`.
- **Escaped literals for readable snapshots**: `s"` reads no escapes; `S\"`,
  `C\"`, `.\"` accept C-style escapes (`\\`, `\"`/`\q`, `\n`, `\r`, `\t`,
  `\xNN`, `\z`, …) for direct JSON/source expected strings. `S\"` needs its
  delimiter space (`s\"\n"` is one undefined token) and reads `\u` as its own
  escape, so a fixture holding JSON writes `\\uXXXX`. Generated syntax from
  fields uses checked byte/field helpers or `src/lib/json-write.f`.
- **`ESC-DECODE ( ptr u8 n ptr u8 -- n bool )` decodes an escaped payload** by
  the engine's escape table: it reads the n source bytes, writes at most n bytes
  at the destination, which the caller provides, and answers the count written
  and whether every escape was valid. The payload `A\x42C` writes `ABC` and
  answers 3 and true; `A\yB` stops at the bad escape, `A` written, and answers 1
  and false. Code that reads an escaped literal's bytes, as the dependency walk
  does, decodes through it rather than a table of its own.
- **Fixed DATA header cells need a layout audit** against the reserved ranges:
  `VTAG-OFF`, `VVAL-OFF`, `JIT-SNAP:STK-OFF`, `JIT-QUOT:STK-OFF`,
  `BODYBUF-OFF`, `LOCNAMES`, `BPTAB-OFF` and the `SNAP-RELOC` call and address
  maps in `src/habu/layout.f`, and the register tables
  `REGALLOC-ABI:VRTAB-OFF`/`VRITAB-OFF` in `src/habu/regalloc-abi.f`. A cell
  inside a scratch range is overwritten by compiled source. The return and
  loop stacks are guarded mappings (`STACK-ABI`), not header ranges. Give the
  cell a row in `src/habu/data-claims.f`, whose `CLAIMS-ASSERT` refuses an
  overlapping pair at build and names both, and add a regression for the exact
  overlap class.
- **Snapshot builders retire the baked tail and append their entry file.**
  Use `undefine NAME` for one word; use `HIDE-DEFS-FROM` only to truncate a
  refresh tail. Never replay baked core, target or image sources to hide
  duplicates. Mmap-backed image/include pointers and cursors (`MBUF-A`, `MP`,
  `MLEN@`/`MLEN!`, `INCLUDE-BUFS-A` and include depth/read/path cells) belong
  to their creating process. Clear them in a named reset before
  `BUILD-SNAP-HDR` or fresh image emission, not by replay or redefinition.
- **Emitter punctuation is semantic** (`BL,`, `LBL,`, `ADR,`, `ZBYTES,` differ
  from the bare names); source-shape regressions assert exact tokens; emitter
  stack comments describe the host stack (`( -- )`, `( n -- )`), emitted runtime
  effects live in prose or the generated word's contract.
- **Argv-only spawns start with an empty environment.** `PROC-SPAWN-ARGV-IO` and
  `PROC-RUN-ARGV-IO-RC` pass no variables (`/usr/bin/env` prints nothing); to
  inherit, `PROC-ARGV-ENV-RESET … PROC-ENV-INHERIT-MISSING` and spawn through
  the `*-ARGV-ENV-*` words. `PROC-CMD` inherits by default;
  `PROC-CMD:ENV-HERMETIC` turns it off.
- **`IMAGE-LIFECYCLE:PREPARE` keeps a hook registered when it throws**: the
  entry goes only after the callback returns normally, so a release that refuses
  must not clear its own registered flag before rethrowing, or the next
  `REGISTER`/`PREPARE` adds a second entry. Follow `src/lib/net/udp4.f`
  `REGISTER-CLEANUP`: set a private flag only after `IMAGE-LIFECYCLE:REGISTER`
  completes.

## Native Forth Gotchas That Shape How We Write Code

- **Control words and ticks are compile-only**: `if`/`else`/`then`,
  `begin`/`while`/`repeat`, `[']`, `i`, `?do` and `;` live inside a `:`
  definition, never at top level; interpreted tests use `'` (`' WORD catch`).
  Both ticks resolve the name as a bare word does (the open scope, the globals,
  then the used publics), and a miss is `E-UNDEFINED: NAME`: rc 70 at top
  level, a catchable 70 under `evaluate`. A tick is no presence probe: require
  the file that defines the word first (`' NO-SUCH` measured both ways,
  test/outer-interpret.f TICK-UNDEFINED).
- **A `begin <cond> while <body> repeat` condition may only add a flag.** The
  stack under the flag at `while` equals the stack at `begin`; a condition that
  net-produces carry values (`a u NEXT-TOKEN` leaving a span under the flag) is
  rejected at `repeat`. Establish loop-carried values before `begin`, or move
  the production into the body behind a peek-only flag.
- **A no-`else` `if` is stack-neutral**: a true branch that changes depth fails
  the merge at `then` (`expected: … actual:`); bind the consumed value in a
  local before the `if`, or add `else drop`.
- **Malformed control syntax is a rejection, not `uncheckable`**: orphan
  closers, unterminated frames, `i`/`j` outside enough loops and `leave` outside
  a loop make `CHECK!` return `0`; `uncheckable` is for modeled-word gaps.
- **`case/of/endof/endcase`**: selector before `case`, key before `of`, arm
  before `endof`, default before `endcase`. `of` compares with the preserved
  selector; matched arms consume it; the default runs with it still on the stack
  until `endcase` drops it, so a value-producing default leaves the selector on
  top (`30 swap endcase`). Keys and selectors are integers; every live arm and
  the default unify to one data/return-stack effect.
- **`parse-name` answers a transient `( c-addr u )`** that the next
  `s"`/`."`/`refill` invalidates: copy the bytes into your own buffer at once
  with the checked byte helpers (there is no `move`); never hold the pointer
  across another parsing word.
- **`s" "` is empty**, not one space; emit byte `32` or a `*-SP` helper for a
  literal space.
- **Query a wordlist with `s" WORD" get-current search-wl`**: the xt, or zero.
  Built native images provide no `find-name`, `defined` or `[defined]`.
- **A `SORT:SORT!` comparator receives raw cells**; nominal pointer views are
  re-cast on both arguments inside it.
- **Emitted primitive leafness follows emitted control flow**: `FPRIM-L` only
  when the whole body emits no `BL` or `BLR`, else `FPRIM` so the frame
  preserves the caller return address in `x30`.
- **Fallible value-returning scanners validate first**: range and schema checks
  that `throw` go in a `--` helper, and the value-returning word's remaining
  path structurally returns its outputs; a final throw-only fallback in a `--
  value…` word confuses path-effect merging.

## Rules learned by refusal

These edge cases name the refusal at the boundary. The general rules are in the
sections above.

- **`OPAQUE` belongs in a package's field-bearing `STRUCTURE` header.** After
  the first field, it is `header clause after the first field` (7107); twice,
  `a second OPAQUE clause in one declaration` (7206); without fields,
  `opaque requires a family with fields` (7107); outside a package,
  `OPAQUE requires a family declared in a package` (7207). An `ENUM` rejects
  it as `unexpected token in enum declaration` (7107). A rejected declaration
  rolls back.
- **A checker overlay admits rows, never live namespace changes.** Between
  `CHECKER-SCOPE-START-NEUTRAL` and `CHECKER-SCOPE-DONE`, a definer (`:`,
  `create`, `variable`, `constant`, `defer`, `TRUSTED:`, `CAST:`), `EXPORT`, a
  new `package`, or `wordlist` throws `ENGINE-ERROR:OVERLAY-OPEN` (rc 108) at
  its name, before the dictionary changes (`hb: definition while a checker
  replay is open: NAME`). `undefine` throws the same code before retiring
  anything (`hb: undefine while a checker replay is open: NAME`). Reopening
  an existing package is allowed. The
  throw is catchable inside `evaluate`; the overlay still closes.
- **A definition and a `(` comment close in the source that opened them.**
  Files, stdin and `evaluate` strings refuse an open definition at their end
  (`hb: source ended inside definition: NAME`) or an open comment (`hb: source
  ended inside a ( comment`), rc 74. The first includes its stack comment,
  locals and quotations; the definition rolls back on a caught `evaluate`.
  An immediate word may evaluate text while a definition is open, but a `;`
  from that text may not close the outer definition (`hb: source closed a
  definition it did not open: NAME`, rc 74). Completed tokens in nested
  evaluations stay compiled in the outer definition after a caught refusal.
  The open-definition diagnostic locates the name (or `def-open` caller),
  the open-comment diagnostic its `(`, and the foreign-close diagnostic its
  `;`; stdin has no line location.
- **`C2-MEM:WITH-INIT` preserves two bounds.** Its callback can declare
  `forall<i inside [l,T],[ ... ]>`: fresh `i` is inside the byte view's
  lifetime `l` and every scope dependency of the initial record `T`. It
  accepts a live unique byte view and a complete, non-unique, non-stale
  fixed-cell product. A short capacity, incompatible field schema, open,
  unique or stale record, raw or shared authority, incompatible callback, or
  escape of `i` refuses. On return it clears the initialized extent and
  restores the original byte view and full bound; throw and task halt close
  the scope.
- **`C2-MEM:WITH-RECORDS` gives a table one owning scope.** It takes a unique
  byte view, nonnegative count and committed, non-owning `DERIVE init` seed.
  The product of count and committed record size must fit the original bound;
  zero count is valid. Under `forall<i inside [l,T],...>`, its callback gets
  opaque linear `records<p,i,a,T>`. `C2-MEM:WITH-RECORD` checks a zero-based
  index and lends `mut-view<p,j,a,init<i,T>>` under `forall<j inside i,...>`;
  `j` cannot escape. The element is neither reconstructed nor cleared on
  return. Closing the table clears the complete initialized extent once and
  restores the original bound.
- **`C2-MEM:WITH-FIELD name` borrows an initialized field.** The name must
  belong to the receiver's committed instantiated schema. A live unique
  `mut-view<p,i,a,init<i,T>>` lends a child with field type `F`, fresh scope
  `j` inside `i`, unchanged init scope `i`, and region `field(a,name)`. On
  return it restores the exact parent without copying a record; throw and
  task halt close the child loan.
- **A refused storage declaration defines nothing.** `LAYOUT-BUFFER`,
  `DEFER-LAYOUT-BUFFER`, `TYPED-BUFFER`, `TYPED-VARIABLE` and
  `DYNAMIC-BUFFER` require a valid storable type and name, plus a valid count
  where the definer takes one. Unknown,
  malformed or missing types; missing names; names with more than one `:` or
  in sealed packages; and an invalid literal count are `E-BAD-STORAGE` at the
  offending token (rc 70 from `tools/check.f`). The name is on the definer's
  line, the type starts on the name's line; a following-line token belongs to
  the next statement. A computed count is bounded by the definer at run time
  with catchable `E-LAYOUT-BUFFER` (7121). For example,
  `4 TYPED-BUFFER B no-such-type` prints
  `habu: in B: unknown type 'no-such-type'` and exits 70.
- **Fixed `TYPED-BUFFER` elements occupy whole cells.** `2 TYPED-BUFFER TB
  u8` is refused (`type this definer cannot store 'u8'`, rc 70). Use `n
  BUFFER: B` for fixed bytes, yielding `( -- ptr u8 )`. `DYNAMIC-BUFFER` alone
  stores `u8`, with byte indexing and `c@`/`c!`; `u16` is refused. `create …
  allot`, `BUFFER:` and `TYPED-BUFFER` allocate zeroed, cell-aligned storage;
  `allot`, `c,` and `,` do not realign following storage (see **Habu Native
  Tooling Gotchas**).
- **A fixed buffer's count may be a literal, `( -- n )` word or expression.**
  The source pre-pass (`CHECKER-LBUF:COUNT-OK?`) reads a literal without
  executing it: `0 TYPED-BUFFER B n` is `E-BAD-STORAGE` (`count outside the
  buffer's extent`), while `$40 TYPED-BUFFER B n` checks. A single nonliteral
  count token must resolve to a
  no-input word producing one `n`-compatible cell; an unknown, ambiguous or
  shadowed name, `variable`, or `( -- bool )` word is `E-BAD-STORAGE` (`count
  resolves to no ( -- n ) word`). Expressions such as `N 2 * TYPED-BUFFER B
  n` are evaluated by the load; `HIR:OPCODES` is a valid engine count word.
  A `( -- n )` word whose value is zero passes
  pre-verification but the definer throws 7121. The same count rule applies
  to `LAYOUT-BUFFER`.
- **Parsing operands are data.** Top-level `'` and `char`, and body `[']` and
  `[char]`, consume the next blank-delimited token, even across lines, without
  treating its spelling as a comment, quote, definition or reserved name.
  Thus `char : constant COLON` and `: F ( -- n ) [char] ( ;` are valid. A
  lexical local takes precedence byte-for-byte over a keyword of the same
  spelling; `{: char :} char` reads the local and consumes no operand. A local
  declared in a block dies at its closer; an enclosing local in a quotation
  still binds and is refused there (`E-BAD-LOCAL-SHAPE`). A dictionary word
  that parses, such as `require` or `SEE`, is resolved by scope and is not
  inferred to be a keyword from its spelling. The source stages share
  `SOURCE:PARSING-KEYWORD?` and `LINT-LEX:OPERAND` for raw operands;
  `SOURCE:BLOCK-OPENER?` bounds locals.
- **Definer names are raw `parse-name` operands.** `package (` names `(` and
  `: \ ( -- n ) 1 ;` names `\`; they do not open a comment or line
  comment. The type definers refuse `(` or `\` as names (`E-BAD-NOMINAL-TYPE`
  for `DEFLINEAR` and `VALUE-RECORD`). A definer or parsing word without its
  operand is `E-MISSING-NAME` at the keyword in every `tools/check.f` mode,
  including `--verify-only` (`VERIFY:E-MISSING-NAME` in pre-verification).
  The language server publishes the result through `CHECK:VERIFY-OUT$`.
- **A `create … does>` clause declares its created words' effects.** The
  source pre-pass learns a scanned clause; for a resident definer, the checker
  latches the created-word effect at publication. A straight-line wrapper
  that calls exactly one definer and has no control frame, quotation, `exit`,
  `leave` or `recurse` inherits it (`CODEGEN:BUFFER` around `BUFFER-E` is a
  definer). A `TRUSTED:` body's clause is a declaration even though its body
  is asserted; `TASK:MIN-STACK TASK:TASK T1` gives T1 its declared effect.
  The same rule applies to a
  replacement clause (see **Structures And Enums**). A definer's `create`
  gives each word it makes `-- ptr a` the moment it is made, at either tier,
  at top level, in a package or under `evaluate`; a `does>` the definer then
  reaches replaces it with the clause effect, and a branch that `exit`s
  before `does>` leaves `-- ptr a`.
- **Renderers can define words absent from source text.** A word whose body
  reaches `INCLUDE-EVALUATE`, `evaluate` or `evaluate-closed` marks the
  wordlist of its top-level call. There, a later unresolved name that may be
  its product is checked by the subsequent `bin/hb --load` run; in
  `--verify-only` it is `W-CHECK-DEFERRED` at the use and the verdict is
  `deferred`. The load rejects a misspelled `trust` product as
  `E-TRUST-UNRESOLVED`. The mark covers only later uses in that section; an earlier use,
  other package, or inaccessible private product still refuses. A `trust` row
  for an unknown product defers only in the wordlist where it lands, not a
  wordlist reached through lookup. A `TRUSTED:` body carries bound calls'
  renderer facts, and a deferred body retains the facts of calls it binds.
  Calls through a `defer` or an executed xt expose no callee facts. A name
  already known to the checker resolves before any renderer mark, even if the
  generated product shadows it.
  A reached renderer without a read declaration group or `generates:` row is
  itself `W-CHECK-DEFERRED`, even without a later product use. A declared
  primary product closes that gap; other generated names remain deferred at
  their uses. Closing a read group with `;FUNCTION` is not a gap; closing one
  whose opener was not read is. `EXPORT` preserves an engine-provided
  definer's coverage declaration; an export of a source-scanned definer
  remains deferred.
- **A text-generating definer can declare its product with `generates:`.** At
  top level, after the definer, `generates: D ( effect )` assigns every word
  D makes that effect, as `does>` does. `FUNCTION:` instead supplies its
  declaration group, treating an `i32` result as `n`. A `generates:` claim
  does not define the made word; the load judges its actual effect and any
  other products. An undefined D, duplicate declaration (including a clause
  or inherited definer effect), or invalid effect is `E-GENERATES-ROW` (7153;
  check rc 70). A used-scope conflict is `E-USING-SHADOW-GLOBAL` (7141) or
  `E-USING-AMBIGUOUS` (7144). The name is one blank-delimited token; the
  signature opens at the first `(` after blanks, attached to a token or not,
  and closes at the first `)` byte; a line feed inside it stays in its token.
  It is at most 256 bytes (`GENR-SIG-CAP`: load rc 76, pre-pass rc 74).
  `CMD:COMMAND` uses this row for its primary generated word. `undefine D`
  retires the row; `EXPORT` preserves the created-word effect.
  `--verify-only` reports an undeclared renderer product at its use as
  deferred; `VERIFY:DEFERRED?` exposes that state. A declared product is
  checked against its row.
- **Top-level source checking follows source order without executing it.** A
  token outside a known declaration is resolved as the loader would resolve
  it. Numbers use the engine's parser; `-3`, `$FF` and `1.5` are literals,
  while `1.`, `$GG` and an over-bound decimal are
  `E-UNDEFINED-TOP-LEVEL` at their tokens. Other missing names and
  compile-only words (`if`, `;`, `does>`, `[char]`, `[']`, `[:`, `is`, `.(`)
  get that code before execution. A malformed qualified name is
  `E-BAD-QUALIFIED-TOP-LEVEL`; ambiguous and global-shadowing `using` names
  get their body diagnostics, check rc 70 (load rc 94 and 105). Top-level
  `PKG:tail` while PKG is open may find global `tail` if its public section
  lacks one; the selected word's own facts govern subsequent parsing and
  definitions. An uncertain resident immediate or loader may leave dependent
  source deferred rather than publish an uncertain definition.
- **Unknown top-level parsers stop discovery unless declared.** A word whose
  body reaches `parse-name` or `create`, or a `defer`, may consume or define
  following tokens. In `--verify-only`, it is `W-CHECK-DEFERRED`; later text
  in the file or included file is not scanned because operands can cross
  lines. A reached renderer marks its wordlist under the rule above. A source
  row after a word's definition can bound
  its operands: `parses: W n` consumes `n` raw tokens;
  `parses-through: W n ( E1 E2 )` consumes those and through the first exact
  listed terminator. `n` is nonnegative; parentheses and terminators are
  standalone tokens, terminators are case-sensitive and `)` cannot be one.
  The row is a trusted source-prepass claim, never a runtime
  parser definition. Bad target, no source-reading behavior, bad count or
  malformed list is `E-PARSES-ROW` (7186; load rc 67, verify rc 70). Later
  rows replace earlier ones; `undefine`/redefinition drops them; `EXPORT`
  copies them, but a calling wrapper does not inherit them. Ordinary loading
  checks a row without retaining it. A declared call is still
  `W-CHECK-DEFERRED`; scanning resumes after its raw operands. EOF inside
  those operands leaves the call deferred. A `defer` and a primitive remain
  undeclared. A resident or
  cached dependency needs a row read by this check; discovery uses a row by
  spelling only when its dependency closure has one declaration and no
  retirement. A bare name counts declarations across all packages; a qualified
  name counts only its package. Otherwise discovery reads operands as source.
  A checked use must
  materialize a cold captured binding before a row can bound it. Calls through
  xts and defers remain opaque; a plain `create` wrapper without `does>` has
  no created-word effect in checked bodies; the `does>` clause's own parsing
  behavior gives its products no parser facts.
- **`names: W` marks literal references W resolves.** After W's definition,
  this row states that W looks up its top `ptr u8 n` input as a name. A
  directly preceding unescaped string literal in a body or at top level is
  then a use of the word spelled by its bytes. A bare name resolves globally;
  `PKG:TAIL` selects that package's public word, and only a word already
  visible at the literal binds. Computed strings, strings passed through a
  caller and escaped literals bind nothing. `EXPORT` copies the claim and
  redefinition drops it; a calling wrapper needs its own `names:` row. An
  absent target or one without a declared trailing
  string input is `E-NAMES-ROW` (7208; load rc 67, verify rc 70). The claim
  is not checked against W's body. `names: XREF-FIND` and
  `names: XREF-FIND-INDEX` are engine declarations.
- **A `TRUSTED:` body may return loose cells as a declared family value.**
  Payload cells sit below the tag. Native elaboration groups them by its
  signature; a checked body must build
  the family value explicitly (`E-NELAB-JOIN`, -8503 for loose cells). A
  wrong cell count is `E-NELAB-ARITY` either way, and consumption still checks
  its tag (`hb: bad layout tag`, rc 85).
- **Binding follows the engine's selected word and its own metadata.** After
  `undefine dup  : dup ( n -- n ) 100 + ;`, `5 dup` answers 105 at either
  tier. Intrinsic typing belongs to the original engine word, not the new
  spelling; the same applies to package shadows and `EXPORT` aliases. A
  pending definition binds its own name before competing used publics. A
  checker-only row with no engine word is `E-UNDEFINED` in compiled code;
  neutral verifier scopes publish codeless records temporarily so candidates
  can refer to rows recorded earlier in the scope;
  `VERIFY:CANDIDATE-IN-SCOPE` reports that scoped result. A generated
  `DER:HASH` may therefore call `DER:UNMAKE` before either is published
  (see **Naming**).
- **Native widths have separate ceilings.** A 63-cell identity compiles in
  AOT; the same 64-cell definition currently refuses `E-A64RAV-DKEEP`
  (-8611). The IR signature list holds 64 cells; a 65th staged input or
  output is `E-IR-TYPE-ARITY` (-6688), with one staged value per record cell.
  Register pressure is separate: a 25-cell sum compiles on Darwin ARM64 with
  its 24-register pool by storing each entry value after loading it. A
  34-cell nested record roundtrip likewise compiles and runs.
- **Literal and output punctuation matter.** `s"` does not interpret escapes;
  `S\"` does and needs its delimiter space (see **Habu Native Tooling
  Gotchas**). Native `.` prints a newline: `11 . 22 . cr` emits
  `11\n22\n\n`; use digit emitters for inline numbers.
- **Package `private` is visibility, not sealing.** Another file may reopen
  the package and call its private words until `get-current prot-wid-add` or
  the native capture seals it. A sealed reopen exits 84. Multi-file owners
  seal only after their last file (see **Packages**).
- **A cast that introduces a pointer or xt requires a private package row.**
  `CAST: >BYTES ( n -- ptr u8 )` and `CAST: >OP ( n -- [ n -- n ] )`
  are `E-CAST-MINT` (7147) at top level or under `public`. A type-variable
  pointee is `E-CAST-LINEAR` (7137), and a layout's fields carry the mint
  and foreign-owner rules recursively: a `pfbox` whose field is `ptr u8`
  exposes that pointer through `PFBOX:UNMAKE` (see **Structures And Enums**). A
  `LINEAR:` token conversion similarly requires the declaring package's
  private section; bad payload, owner or scope is respectively
  `E-LINEAR-PAYLOAD` (7195), `E-LINEAR-OWNER` (7196) or `E-LINEAR-SCOPE`
  (7197). A checked body cannot declare one; outside callers use the owner's
  public words.
- **Nominal declaration placement and inference matter.** A `DEFTYPE` handed
  out by a definer's `does>` body must be public, so a reader can resolve the
  stored signature; a private one is `E-BAD-STORED-SIGNATURE`. Constructing
  only `RESULT:OK` leaves the error type variable of `result<a,b>` free;
  that variable cannot unify with a nominal enum or family. Use a result
  family whose error alternatives are nullary, such as
  `SUMTYPE foo-result<a>`. Spell an arity-one family with its argument in
  an effect (`family<PKG:t>`) and bare in `MATCH family`; the other form
  gives `wrong arity for type family`.
- **Atom prefixes and declaration keywords reserve family names.** An atom
  prefix such as `layout-` claims the whole lowercase `layout-*` namespace;
  a conflicting enum variant throws 7110. A grammar keyword such as
  `policy` and a value-record name already registered as `vr` similarly
  cannot name a family (`STRUCTURE vr` throws 7110).
- **`0 set-check` also disables compile preflight.** Use a `PRIM: … PRIM;`
  axiom for a genuine missing primitive model so later definitions remain
  checked. A `defer` before `: TRUST` in `src/core/checker.f` consumes one
  pre-trust slot; overflowing `PD-CAP` exits 72 at boot. Place new selectors
  after `: TRUST` or use an existing hook.
- **A sealed from-source prefix needs external rows for pre-hook words.** A
  word defined before the check hook is internal without a `PRIM:` axiom or a
  `TRUSTED:` declaration after `src/core/checker.f` claiming it. A checked
  or top-level use is `E-UNDEFINED` or `hb: internal engine word: NAME`
  (rc 70) before the checker runs. `PATH-CAP`, `E-PATH-RANGE` and
  `SCOPE-FIND-AMBIGUOUS` have rows; a pre-hook colon word such as
  `SCHEMA-REG:SCHEMA-CON` needs one. A `TRUSTED:` or data word before the
  hook gains no such declaration. A constant without one may be read at
  top level into a file-owned constant (`REG-PROT-CAP constant MY-CAP`). An
  unsealed prefix can bind a signed colon word by its declaration.
- **A signature compiled with the hook empty is a row without authority.**
  This applies to cold prefix and `0 set-check` definitions at either tier.
  An unsealed checker can use the declared effect to check callers, rejecting
  a missing input as `E-INPUT-UNDERFLOW`; sealed images
  (`CHECKER-EFFECT-AUTHORITY:SEAL`) refuse it `E-CAP-TRUSTED`, while a
  pre-hook internal name is refused by the engine first. `TRUSTED:` asserts
  an authoritative row. Before the checker claims a cold prefix's source,
  signed definitions are logged for that claim; a qualified definition there
  terminates with `checker: qualified definition before the source claim`,
  rc 76. A signature
  missing, unparseable or containing a scope scheme records nothing. On tier
  1 a refused scan leaves ordinary compilation using the declaration and
  reports the reason; a native build requires the scan and refuses before
  publication. A failed native compile retracts only the pending definition's
  rows, preserving prior rows, including an earlier `CHECK!` under the same
  name. If no row supplies arity it refuses with the hook reject status
  (rc 70); if another owner's row supplies arity but the body underflows it,
  it is `E-NELAB-UNDER` (-8304).
- **Native compile callbacks cannot change the checker store.** From scan
  completion through the last publication callback (`NBACK:OBSERVE!` and
  `NPUB:WITH-UNIT` included), scans (`CHECK!`,
  `CHECK-CANDIDATE!`) and writes (`generates:`, `CHECKER-DEFLINEAR`,
  `CHECKER-DEFRECORD`, `CHECKER-DEFFAMILY`, `TYPE-FIELD-OWNER` phases,
  extent marks) throw catchable
  `E-NCOMP-STATE` (-8570) before mutation. The window closes on every exit,
  including throws, so a refused definition cannot leave an orphaned type or
  row. This applies to certified and hookless compilation.
- **`catch` requires a callable with a known effect.** Literal quotations,
  callable ticks, quotation parameters already on the stack and fetched
  typed quotations are admitted. A callback whose effect does not preserve
  the stack needs a preserving wrapper, such as `[: INV ;] catch`.
  The quotation literal must preserve stack depth (see **Checker & type
  model**). `catch` restores both stacks' depths on throw, never their
  contents. Read the returned code; keep handles needed for cleanup in
  accessible storage or on proven-intact input cells.

  ```forth
  : INV ( ptr u8 n [ ptr u8 n -- ] -- ptr u8 n [ ptr u8 n -- ] )
     {: a u q :}
     a u q execute a u q ;
  ```

- **A caught literal leaves possibly overwritten inputs stale.** In its
  fixed input window, a cell keeps its type only if every throw path leaves
  that exact term in place. Other cells become `stale<t>`;
  `E-STALE-READ` (`keep_value_before_catch`) rejects `@`, `c@`, arithmetic,
  typed locals, a declared output and other typed reads. A stale value may be
  moved, dropped (unless linear), or bound to an untyped local. The exact
  catch code proves normal output in the success arm of core `0=` (or false
  arm of core `0<>`); an unrelated zero, shadowed predicate or another
  catch's code proves nothing. A later catch still treats physically replaced
  values as replaced. Live loop back edges agree on proof and replacement
  status. Quotation output carries replacement marks, but private success
  proofs do not escape its boundary; polymorphic effects transport types,
  not a particular catch's status.
- **Callee throw evidence follows the callable term.** A callee records which
  of its top 20 declared input cells every throw path leaves untouched;
  deeper inputs count as not intact. A drop, swap, overwrite, or local bind
  and pushback loses evidence. A called word that throws is assumed to
  overwrite its declared inputs unless its own body proves otherwise. The
  literal and `['] W` carry W's evidence, including through a local bound to
  that same tick. A fresh declared quotation parameter, typed `xt<effect>`
  cell or `defer` has no such edge and leaves the window typed. A tick of a
  never-returning callee still fits against its declared output row and can
  be `E-NELAB-QUOT` at `[']` in native elaboration.
- **Linear and multi-cell values respect the stale boundary.** A linear
  handle cannot cross a quotation-literal catch: reading it is
  `E-STALE-READ`, and dropping it is a linear refusal. A typed variable or
  buffer cannot store that linear type. Open it inside the caught body;
  use `['] WORD catch` only when WORD's own throw paths preserve the handle;
  or consume it before rethrowing. A multi-cell value is one logical stale
  value (`stale<option<pt>>`): one `drop` removes it, an untyped local can
  transport it, and a typed use or `MATCH` is `E-STALE-READ`.
- **`evaluate-closed ( ptr u8 n -- )` isolates a checked caller's stack.**
  A checked body calling `evaluate` is `E-UNSAFE`. Text under
  `evaluate-closed` starts at depth 0, cannot reach caller cells even through
  an xt, `catch` or `finally`, and must leave no cells or open definition.
  Underflow is catchable 70; residue is `E-EVAL-RESIDUE` (-3804, uncaught
  rc 67); an unfinished definition is rc 74. Data-stack overflow exits 102.
  With a task live, evaluation exits `$4F` before reading text. The three
  open cases are under **Checked code and primitive boundaries**.
- **`TEST-EVAL` returns a measured value from text.** From
  `src/lib/test.f`, `TEST-EVAL:N ( ptr u8 n -- n )` takes exactly one cell,
  `TEST-EVAL:FLAG ( ptr u8 n -- bool )` applies `0<>` to it, and
  `TEST-EVAL:RC ( ptr u8 n -- n )` returns the `evaluate-closed` throw code
  or zero. An empty N text underflows (70), `1 2` is
  `E-EVAL-RESIDUE`, `drop 1` underflows without touching caller cells, and
  an unfinished definition is rc 74. N can run inside N's evaluated text.
  The value type remains checked at the
  caller: declaring bool for N's numeric result is `E-MISMATCH`.
- **Return-stack cells never cross a signature or quotation boundary.** The
  only admitted `|` notation is the same single row variable on both sides;
  cell entries, a bar on only one side, and such entries in quotation types
  are `E-BAD-SIGNATURE` (or `E-BAD-STORED-SIGNATURE` for `TRUSTED:`, `defer`,
  `trust` and `CAST:`), repair `fix_return_stack`, rc 70. A literal that
  pushes a cell for its caller to pop, or pops one the caller pushed, is
  `E-REJECTED` at `;]`; an unsigned definition's `r@` or `r>` on an input
  it did not push is rejected at the token. Use `>r … r>` within one
  definition (see **Checker & type model**).

## Spans: a pointer that carries its reach

When a word writes into a buffer or indexes one, pass a `SPAN:span<u8>` rather
than a `ptr u8` and a separate length: `src/lib/span.f` bounds-checks every access
against the reach the span carries. Take the span from a producer — `n
SPAN-BUFFER: NAME`, `n SPAN-CELLS: NAME`, `MEM:ALLOC-SPAN` — or narrow one you
were handed with `SPAN:SKIP` / `SPAN:TAKE` / `SPAN:SUB`, which can never widen
it. `SPAN:MAKE`, the one place an address and a number become a reach, is
admitted in `src/` except `src/examples/`; `tools/lint/bare-copy-lint.f` reports it
(and bare `BYTE-COPY`) anywhere else. A read-only source stays the `( ptr u8 n
)` string idiom. The reach is counted in bytes whatever the element type;
`docs/type-system.md` § 11 has the measurement.
