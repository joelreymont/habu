# Forth Standards (habu)

Workers: read [docs/forth-card.md](forth-card.md) first; this file is the
reference.

How we write Forth for the native `bin/hb` engine. Durable language guidance
lives here; build, test and environment rules live in
[bootstrap.md](bootstrap.md) and [gate.md](gate.md).

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
  that package resolves the name; callers are still checked. For now such a prim
  also keeps its global `PRIM-TRUSTED-ONLY!` row: `src/core/internal-mark.f`
  classifies a record by its BARE name, so an owner-private-only primitive is
  sealed `DNAME-INT` with no checked caller. `test/prim-owner-scope.f` pins the
  matrix.
- Never assert that arbitrary `evaluate` preserves the stack; use typed
  quotations for known callbacks. A checked word evaluates source with
  `evaluate-closed ( ptr u8 n -- )`: the text runs on a guarded data stack of
  its own, must leave nothing and must close every definition it opens
  (measured under **Rules learned by refusal**). The loader and every
  source-generating definer evaluate through it, so a loaded file
  (**Packages**) and a generated declaration are closed programs too. Two cases
  stay open:
  - A text that runs `0 set-check` leaves every later definition unchecked,
    inside the text and after it.
  - A text's top-level code is unchecked, so what it computes is untyped: with
    `variable V` and `S$ ( -- ptr u8 n )`, `s" S$ drop V !" evaluate-closed`
    stores an address in V that a checked `( -- n )` word then reads as `n`.
    Only the text's definitions are certified, each against its own signature.
- Existing TRUST forms are legacy awaiting removal, tracked in [minimal PRIM
  migration](../.dots/habu-trusted-dies-prim-4fd12d60/habu-finish-minimal-prim-c00c6a93.md);
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
  `:`, `TRUSTED:`, `KERNEL:`, `create`, `variable`, `constant`): `I`, `J`, `DO`,
  `LOOP`, `+LOOP`, `LEAVE`, `UNLOOP`, `IF`, `THEN`, `BEGIN`, `REPEAT`, `TRUST`,
  `CASE`, `OF`, `ENDOF`, `ENDCASE`, `TRUSTED:`, `PACKAGE`, `PUBLIC`, `PRIVATE`,
  `UNDEFINE` and the other compiler-dispatch and lifecycle tokens. Lexical
  locals such as `{: i:n :}` stay legal. A generated converter that strips
  prefixes runs `tools/reserved-name-lint.f` after naturalization (`CC-I`
  becomes `IX`, `CC-J` becomes `JX`); `tools/check.f` runs that lint before
  spawning the checker child and reports `E-RESERVED-DEFINITION` with file, line
  and token instead of a silent rc 70.
- **Never define a number-shaped word.** hb parses numeric literals BEFORE
  dictionary lookup (`test/gate-dictionary-lib.f` GD-LITERAL-FIRST), so a word
  named like a literal (`42`, `.0`, `1.5`, `-.5`, `$FF`) loads but is
  unreachable: after `: 42 ( -- n ) 7 ;`, `42 .` prints 42. Only
  `tools/check.f`, through `tools/reserved-name-lint.f`, refuses such names
  (`E-NUMERIC-DEFINITION`). The lint also refuses dot-digit tails (`U.0`).
  Dot-letter printers (`.U`, `.INT`, `F.N`) and digit-leading names that cannot
  parse as a number (`1STNZ`, `0<>`, `2DUP`) are legal. One literal grammar
  serves interpret, colon-compile, `evaluate` and the checker: int `-?d+ |
  -?$h+`; float `-?d*.d+`, exactly one dot and at least one digit after it, so
  `.5`, `-.5` and `.0` are floats while `5.` and `..5` are words. A shaped
  decimal is admitted only while its integer magnitude, fractional numerator and
  power-of-ten scale fit their signed-cell accumulators; an over-bound shape
  stays claimed and is rejected before lookup, never a callable name. The
  checker's claim (`LITERAL-TOK?`/`ALLDIG?`/`FLODIG?`, `src/core/checker.f`)
  mirrors the engine parser (`EMIT-NUM`, `src/habu/habu1.f`) token for token;
  GD-LITERAL-FLOAT-FIRST pins the matrix, including that the checker rejects a
  call to a number-shaped word.
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
  word: NAME:X`). Use its public words qualified or through `using`. Only the
  whitebox image keeps engine packages open; an application image (a `--repl`
  snapshot, `APP-IMAGE:SAVE`) keeps its own packages reopenable
  (`test/package-seal.f`, `test/checker-surface.f`).
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
  refused `E-EVAL-UNFINISHED` at its end (`--load` of a file ending
  `: D ( -- n ) 42` exits `hb: closed text ended inside a definition: D at
  <path>:<line>` and `hb: uncaught throw code -3805`, rc 67), so no definition
  spans a load. Fix a file that leaves cells or a definition; never loosen the
  loader.
  `hb prog.f` and a program on stdin are not loaded files and keep their
  top-level stack. `test/closed-source-suite.f` pins the boundary.
- A named `--load` entry's canonical directory is the primary source root.
  Relative dependencies search that root, then the invocation working directory;
  a dependency keeps the root that resolved it for its own loads; absolute paths
  bypass the search; the process working directory never changes. So
  `/work/app/main.f` can require `src/model.f`, and that file `src/math.f`.
  `SOURCE-ROOT:WITH ( ptr u8 n [ -- ] -- )` scopes an explicit root, restoring
  the caller's on return or throw; fixtures resolve against
  `SOURCE-ROOT:CURRENT$ ( -- ptr u8 n )`, never a script argument. Nested loads
  keep each parent's source bytes alive until it returns, releasing them on
  return or throw; there is no fixed nesting count. Discovery, checker
  dependency collection and content closures use the same canonical paths and
  owner roots.
- The engine marks its baked prefix files `provided` before user source runs, so
  `require src/core/sha256.f` skips the prefix-owned copy; `provided` is honored
  before a missing file is opened. Frozen facts keep root-relative names, so the
  engine runs from another checkout or without its compiled sources; snapshots
  keep these facts and clear process-local roots and resolver scratch. Relocated
  reloading of a deleted application source tree is not promised.
- A package wordlist is a case-insensitive no-duplicate set (`RESET` and `reset`
  are one tail) across reopened blocks and across `:`, `create`, `variable`,
  `constant` and `TRUSTED:`; silent last-definition-wins shadowing is always an
  error. Redefine explicitly: `undefine NAME` retires the active entry and
  clears checker signature, defer-target and control metadata, then the name may
  be reused. Shadowing an outer, global or built-in word from inside a package
  is legal (a different wordlist), and one tail may live in several packages
  (`APP:RESET`, `MK:RESET`).
- A **public** definition whose tail a **private** word of the same package owns
  is the forwarder pattern (`lib/task.f` publishes `: PREPARE ( ptr n -- )
  PREPARE ;` over its private `PREPARE`): legal, but both effects must move the
  same number of CELLS, because the bare tail binds the private word and a
  definition's contract is read from that binding
  (`src/compiler/native/compiler.f` KEEP-ARITY asks `NDICT:SPELL-ARITY` with the
  bare name). A mismatch is refused where written, `E-SHADOWED-ARITY` (checker
  7145, rc 67), naming the package, the tail and both widths; the native build's
  `-8303 E-NELAB-ARITY` stays as the backstop. The rule judges only a colon
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
  a primitive and a duplicate tail in the target section. Re-exporting a
  generated constructor under a second name is allowed; adding tails INTO a
  generated constructor package is not. AOT tree-shake keeps one body; alias
  rows roll back with checker scope frames. At TOP LEVEL `EXPORT name…` is the
  hb-build `--repl` export directive: the build strips it and a plain load
  consumes the name as a no-op.
- Every package feature has native gate coverage: runtime lookup, checker
  certification, private isolation, public export, reopen, case-insensitive
  lookup and fail-closed misuse (`public`/`private`/`;package` outside a
  package, nesting, missing names, qualified package names).

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
- A bare tail resolving to a GLOBAL while a used package ALSO exports it is
  `E-USING-SHADOW-GLOBAL` (checker 7141) at the reference site, naming both
  candidates (`global TOK`, `PKG:TOK`) with arities. So a package whose public
  tails are ordinary verbs cannot be imported: `using TCP4` refuses at the first
  bare `READ`, `WRITE` or `CLOSE`. Qualify the package word (always certifies)
  or rename the collision. The checker enforces this in every checked body (rc
  67). The interpreter enforces it at top level and for `'`, by name
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
  reaches past it.** Inside `package TENDER` a bare `open` is `TENDER:OPEN`; in
  a checked body under `using DOC` a bare `close` is refused against
  `DOC:CLOSE`. There is no qualifier for the global "" wordlist: reach the
  operation through a differently named word (`OPEN-APPEND-FD`, the primitive's
  sibling `close-rc`) or rename the package word. Operator spellings too: once a
  package defines `@` or `+`, a bare `@` or `+` in its later bodies is the
  package word to the checker and both compilers, whatever its operands; a body
  compiled before the definition keeps the engine word
  (`test/reopen-binding.f`).

### Structures And Enums

New declarations use `NEWTYPE` for an opaque nominal cell, `STRUCTURE` for a
record with named fields and `ENUM` for alternatives with or without payloads.
Each registers a whole family at top level and cannot be called inside a checked
definition. `STRUCTURE` and full `ENUM` require an arity; compact `ENUM` omits
it and has no payload fields. Legacy `SUMTYPE`, `PRODUCT`, `VALUE-RECORD`,
low-level `BEGIN-STRUCTURE`/`END-STRUCTURE` definers and counter enums (`ENUM+`,
`ENUM4+`) still have executable sites; they are migration debt, forbidden in new
code.

```forth
package EXAMPLE
public
NEWTYPE index 0

STRUCTURE point 0
   FIELD x n
   FIELD y n
;STRUCTURE

ENUM message 0
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
STRUCTURE point 0 FIELD x n FIELD y n ;STRUCTURE
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
spelling (`{: p:n :}` is `E-MISMATCH`, expected: n actual: @pt.tag<>), a wrong
family argument and a bare tail of a family of arity > 0 are all refused. See
[the multi-cell type
rules](type-system.md#5-families-records-alternatives-and-generics).

- `NEWTYPE name arity` registers a nominal cell family (`TK-CELL`), no closer:
  arity `0` is an opaque scalar newtype (`lib/num-types.f`), arity `N` binds
  positional params.
- Family identity is the exact `(package, tail)` pair: a package family may
  share a tail with a global family or another package's, even at different
  arities. A qualified token resolves only its package row; a bare token
  resolves the open package's own row (private or public), then the global row,
  then the sole eligible public row of another package; two eligible non-lexical
  package-public rows are an error. So adding `MEM:span` cannot change what a
  top-level `span` means. Same-package duplicates, reserved grammar names and
  foreign private rows reject.
- `STRUCTURE name arity [ header… ] FIELD f type … ;STRUCTURE` declares a
  single-shape record with named fields in declaration order, deepest field
  first on the stack. A structure with fields generates sealed `MAKE`/`UNMAKE`:
  `PKG-FAMILY:MAKE` in the constructor namespace for a public package family,
  `FAMILY-MAKE` in the declaring package's private wordlist for a private one. A
  fieldless structure is an opaque cell family with no generated constructor or
  destructor.
- Full `ENUM name arity [ header… ] VARIANT v FIELD f type … ;VARIANT … ;ENUM`
  declares named alternatives with named payload fields; a variant may have
  none. Its tag is its declaration order; generated constructors feed an
  exhaustive `MATCH … ;MATCH`.
- Compact `ENUM name [ header… ] v0 v1 … ;ENUM` declares payloadless variants
  from bare names, with implicit arity zero; never mix it with `VARIANT` blocks.
- `POLICY` and `DERIVE` headers precede the first field or variant: after the
  name on compact `ENUM`, after the arity on `STRUCTURE` or full `ENUM`;
  repeating a feature rejects. `DERIVE addr` on a structure with fields
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
  argument.** `STRUCTURE span 1 FIELD base ptr a FIELD len n ;STRUCTURE` is two
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
  SPAN-BUFFER: NAME`, lib/span.f); only a clause row carrying an instance whose
  width reads an open argument is `E-NELAB-BUNDLE`, at the definer.
- `CAST: NAME ( source -- destination )` declares a checked retype: a reader
  keyword with no body and no `;`, publishing `NAME` as an identity whose call
  sites emit nothing. A conversion that can refuse is a checked word that
  throws, then the cast. The checker's rule, in refusal order
  (`src/core/checker.f` `CAST-CERTIFY`):
  - Every family named is declared, visible and applied to its declared
    number of arguments (`E-CAST-FAM`, 7131): `( n -- box )` for a
    one-parameter `box` is refused by name, not by a later death.
  - Any other signature fault, bad syntax or a bare `ptr`, is refused as a
    definition with that signature is: the bad-stored-signature diagnostic,
    then the compile-reject rc 70. The first fault names the class, so
    `( ptr -- box )` is this reject, not `E-CAST-FAM`.
  - Each side is one term (`E-CAST-ARITY`, 7129); a layout value wider than a
    cell is one term per cell.
  - A scope or region variable, a quantifier-bound variable, a scope, or a
    read view, mutable view or loan field anywhere in either term would erase
    or introduce a scope dependency (`E-CAST-SCOPE`, 7151).
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
  `STRUCTURE pfbox 0 FIELD p ptr u8 ;STRUCTURE`, `CAST: >PFBOX ( n -- pfbox )`
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

- Separate multiline definitions with two blank lines; related one-liners may
  stay together; a word's comment sits directly above it, after the blank lines.
- **Write checked, typed Habu**, the default for new public and library Forth:
  small typed words with real `( in -- out )` effects (`: SQUARE ( i64 -- i64 )
  dup * ;`), composed into checked DSLs that read as the domain, not as stack
  plumbing.
- **Build checked task vocabulary before fighting syntax.** Structured rows,
  JSON/TSV, generated source, diagnostics, packets, repeated assertions: factor
  domain words or a checked DSL first. Giant `s"` literals, fragile escaping and
  private byte emitters are bugs unless they are that DSL's tested boundary.
- **Readable DSLs execute the body they name** (`[: ITEM ;] NAME-FILES`,
  `TEST:SUITE name … TEST:;SUITE`), not generic `execute` wrappers.
- **Keep helpers and dispatch checked** with typed quotation effects.
- **Guard dependent operations with control flow.** `and`/`or` combine values
  already evaluated; establish shape, owner and bounds with `if … exit then`
  before indexing or reading a dependent field. Combine predicates only when
  each is safe alone.
- **Keep control flow and multi-step computation out of argument lists.** The
  checker accepts `s" k" 1 0 > if 5 else 6 then 2.0 L-OF`; extract the value
  into a named word (`: DET-MINRATE ( -- r ) … ;`) or a local, one value per
  concept. Dense lists that splice `if/else`, comparisons and several `@`/`F@`
  reads hide a wrong cell type (a `bool` from `0 >` where `n` is wanted) that
  surfaces at a later call as "expected n actual bool".
- **Predicates and selectors execute real bodies.** A declared effect states no
  runtime fact and defines no word owned by another file.
- **Classification tables beat token ladders**: a long `dup`/`over` chain over
  token classes becomes row data plus named transition helpers; tests describe
  the table policy.
- **Small words**, about five lines, readable top to bottom with a few stack
  items in mind.
- **Manageable argument lists**: about five or six inputs, a guideline, counting
  values not tokens (`ptr u8` is one pointer; pointer plus length is two). Split
  preparation, allocation, traversal and consumption; pass each stage's result
  instead of every earlier argument; derive metadata from its owner; prefer
  records that model domain values over context bags, globals or the return
  stack. Allocation callbacks and low-level helpers get the same judgement.
- **Split multi-pass words into named passes**: cursor movement, classification,
  validation, state update and rendering as checked words with their own effects
  plus a small orchestration word.
- **No dense one-line control words.** One line is for a trivial straight-line
  wrapper; anything with `IF`, `BEGIN`, `WHILE`, `REPEAT`, `UNTIL`, `case`,
  locals, several stack transitions or more than one step spans lines. Line
  effects only where they clarify; a noisy word is factored.
- **Raw compiler and emitter code is not exempt**: exact effects and small
  helpers; if review needs reconstructed stack state, factor first.
- **Factor when the stack gets unreadable**: `ROT -ROT PICK ROLL` means a helper
  or locals.
- **Locals `{: a:type b:type :}`** remove juggling. They bind inputs only, never
  `-- outputs`; the effect stays in the stack comment. Binding is left to right
  from the deepest item: `1 2 {: a:n b:n :}` gives `a`=1, `b`=2. Type new locals
  when the concrete type is known; a bare name only where the entry effect keeps
  richer role detail the annotation cannot express, or the typed capability is
  documented missing.
- **An entry locals group goes on its own line.** A `{: … :}` group that binds
  at entry goes on the line after the one holding the name and stack effect,
  indented as the body: three spaces, the indentation every body line in `lib/`
  and `tools/` uses (`lib/fs.f:640-642`). The body starts on the next line. The
  rule governs new and changed definitions; existing files are not reformatted
  for it.

  ```forth
  : CLAMP ( n n n -- n )
     {: v:n lo:n hi:n :}
     v lo max hi min ;
  ```
- **A local name is at most 16 bytes and a definition binds at most 64.** Past
  either the compiler refuses before storing anything, exit 70, naming the token
  (`hb: local name over 16 bytes: <token>`, `hb: more than 64 locals in one
  definition: <token>`); when the checker sees it first (tier 1, the check tool)
  it reports `E-LOCAL-NAME-TOO-LONG` or `E-TOO-MANY-LOCALS` with width and
  limit. Shorten or factor.
- **A local is bound once.** A value that changes per loop turn lives on the
  stack or in a cell: `begin {: cursor:n :} … cursor' again` binds at the top
  and pushes the next value before the back edge, so the stack at `again`
  matches `begin`. `RECURSE` for a cursor only where a limit bounds the depth; a
  peer-controlled depth, such as bytes arriving on a connection, overflows the
  task's stack.
- **A `ptr` annotation does not fit a byte span.** Under `( ptr u8 n -- n )` the
  group `{: p:ptr :}` is refused at `:}` (expected: ptr n actual: ptr u8 n).
  Keep the detailed type in the effect and bind a bare local for a body that
  uses `c@`/`c!`, or factor a helper whose entry carries `( ptr u8 … -- … )`.
- **Name same-type numeric slots before reordering them.** In `( cap used add --
  )` a stray `swap` type-checks; bind names at entry or factor role-specific
  helpers before capacity, offset or decoder arithmetic.
- **Locals are block-scoped.** A `{:` group may appear on any live path, inside
  `if`/`else`, `case` arms and loop bodies; the names die when that arm closes
  and the prior scope and frame depth return. Never reference a branch-local
  after `then`/`endof`/`endcase`/`loop`/`repeat`; bind before the control word
  to survive the join.
- **Dead code cannot bind locals.** A group after a closed early-exit guard
  (`dup 0 < if exit then {: x:n :}`) is valid, the fall-through is live; a group
  right after an unconditional `exit`, `leave`, `throw`, `die` or `again` is a
  checker error.
- **No deep locals stacks.** Locals are for shallow factoring; nested helper
  calls from loop or callback bodies use stack leaf helpers or separate scratch
  cells so inner helpers cannot clobber caller indexes.

## Files

- **One concern per file**: parser, renderer, DB, data table and driver are
  separate files, split at responsibility boundaries.
- **Reusable helpers live in libraries**; shared behaviour lives in one owned
  file. Run multi-file tools as `hb --load lib/a.f lib/b.f tool.f -- args…`:
  sources before `--`, `SCRIPT-ARGV$` after it, fd 0 still tool data when stdin
  is not a tty. Use `--load` only with more than one source file.
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
  lib/type/deftype.f`): top-level `DEFTYPE NAME` mints a package-scoped type
  with converters `>NAME ( n -- name )` and `NAME>N ( name -- n )`, the only
  crossing; the type tail is the lowercase fold (`SERIAL` → `serial`, so `(
  serial -- n )`); `DEFTYPE SERIAL` in `CAMERA` and in `FRAME` are distinct;
  unknown tokens stay errors. Substrate: `docs/value-nominal-substrate.md`.
- **`DEFLINEAR` for owner and lifetime tokens.** A linear token is nominal and
  noncopyable: `dup`/`over`/`2dup`, `drop`, `@`, `!` and by-value record
  duplication reject when they would duplicate, discard, load or store it; only
  words whose effect names the linear type create or consume it.
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
- **`CHECK!` is the user contract.** `CHECK` proves internal consistency; user
  builds verify the body against the declared effect and make rejection fatal.
  Tests for bad programs assert build rejection, not runtime failure.
- **Typed booleans are `bool`**: produce them with `0 0=`, `0 0= 0=` or domain
  helpers; never store raw `0`/`-1` in a `ptr bool` cell or compare bools with
  `=` (`bool bool` is refused).
- **Quotations are xts, not closures.** `[: … ;]` cannot read surrounding
  locals: declaring or referencing a local inside a quotation is
  `E-BAD-LOCAL-SHAPE`, checker and compiler reject local references while a
  quotation is open, and on the JIT tier a `{:` group inside `[: ;]` is refused.
  A body that needs locals inside a quotation becomes a named private word. A
  value a `catch`, `finally` or locked body needs travels through storage it can
  address; where it differs per task, that storage is the task's own slot (a
  `TASK:+USER` cell or a typed-buffer row indexed by the task) that the
  quotation reads for the running task. Only one `[:` is open at a time: a
  second refuses `hb: a quotation may not open inside a quotation: <name>`, rc
  75, catchable inside `evaluate`; sequential quotations in one definition are
  fine.
- **A handle over caller-owned storage is a public `STRUCTURE` plus a
  `TYPED-VARIABLE` or `TYPED-BUFFER` in the caller**, a checked `ptr PKG:type`.
  No `TRUSTED:` mint, state and consume leaves: `CAST:` refuses a linear
  operand, behind a pointer or in a quotation row too (7137 `E-CAST-LINEAR`),
  so a linear token over caller storage is not expressible; trade the type-level
  lifetime for a runtime refusal off the definer's zero image
  (`lib/json-write.f`).
- **Structural integers widen, roles do not.** `u8 -> u16 -> u32 -> n/cell/i64`
  widens implicitly when lossless; narrowing and same-width sign changes need an
  explicit conversion; nominal roles (`idx`, `len`, `fd`, `rc`, `pid`, `asm`,
  `img`, `snap`, …) never widen to each other or to bare integers.
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
  exit; `leave` is the explicit exit. `+loop` adds its step wrapping and ends
  only when the index crosses between limit-1 and limit in the step's direction
  (Forth 2012 6.1.0140): equal bounds run once with a negative step and a whole
  cycle with a positive one, unlike `loop`.
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

Cells are two's-complement 64-bit. `+`, `-` and `*` wrap (`MAX-N 1 +` is
`MIN-N`) and never refuse. Division is the one partial operation; its two
boundary cases are contracts every backend answers alike.

- **A zero divisor throws `E-DIV-ZERO`.** `/`, `mod` and `/mod`, the only
  dividing primitives, test the divisor and throw (`lib/errors.f`;
  `src/habu/prims.f` re-registers the code for the emitters, which compile
  before `lib/`). It is a catchable throw, not an exit: `[: a b / drop ;] catch
  E-DIV-ZERO = if … then`. A structurally positive divisor says so in the type:
  `lib/num-arithmetic.f`'s `positive-divisor` role makes the refusal
  unreachable. **The contract holds at every tier**: the interpreted primitive,
  a word compiled with `1 set-tier` and an AOT-built executable all throw the
  same code.
- **`MIN-N -1 /` is `MIN-N` and `MIN-N -1 mod` is `0`.** The quotient `2^63` has
  no cell, so it wraps like `+`, `-`, `*`. A backend whose divide traps on this
  quotient (x86_64 `idiv`) tests for the `-1` divisor and answers `(MIN-N, 0)`
  without executing it. `test/prim-parity.f` pins both contracts.
- **`.` prints every cell, `MIN-N` included**, and `FMT:SB-INT` prints the same
  digits.

## Errors

- Engine process failures use only the sealed `ENGINE-ERROR` package ABI:
  `SEAL-VIOLATION` 83, `SEAL-PACKAGE` 84, `BAD-TAG` 85, `CALLABLE-ABI` 86,
  `CATCH-STACK` 87, `CODE-CERT` 88. No global `E-*` aliases; native and
  no-binary recovery consume the same qualified names and values.
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
  `die`, the checker refuses an `exit` in the same `if`:

  ```forth
  ARG s" child" TOK= if CHILD-MAIN exit then
  \ habu: in dispatch: at 'exit' after 'CHILD-MAIN'
  \ hook: non-certified definition: dispatch at 'exit'
  ```

  Write `if CHILD-MAIN then`: the rest of the definition runs only on paths that
  can return.

## Engine limits ordinary source reaches

Four ceilings are reachable from plain Habu rather than a runaway; each refuses
by name with the count it saw and the ceiling, and none truncates.

- **A definition's captured source text: `BODYBUF-CAP`, 8000 bytes**
  (`src/habu/layout.f`): every token of a body (name, stack comment, words,
  string literals with closing quote) plus one separator each. Nine 900-byte
  `s"` arms in one `case` reach it. Past it: `hb: definition body text full at
  8000 bytes: <name> needs <count>`, rc 71, catchable inside `evaluate`. Repair:
  move the long literals into words of their own. The same constant bounds the
  source verifier's body buffer (`E-VS-BODY-CAP`) and the native compiler's unit
  text (`E-NCOMP-TEXT`).
- **One REPL line: 255 bytes** (`src/habu/repl.f` `LLINE-MAX`). A longer line is
  refused, `hb: repl line over 255 bytes: <length> typed`, and read again, never
  truncated, evaluated or saved to history. Load long definitions from a file.
- **`begin` nesting in one definition: `JIT-SNAP:FRAMES`, 28**
  (`src/habu/layout.f`), the JIT's value-stack snapshot frames per definition.
  Past it: `hb: BEGIN nesting full at 28 frames: <name> needs <depth>`, rc 75.
  Factor the inner loops into their own words.
- **Data space: `DATA-SIZE - PROF-CNT-BYTES`**, 33,030,080 bytes on
  linux-aarch64 (`src/os/linux/layout.f`, `src/habu/layout.f`; `DATA-SIZE` is
  per host). `allot`, `align`, `,`, `c,`, `create`/`variable`/`defer` and the
  interpret-mode string literals that keep their text (all but `."`) advance the
  DP through `DP-CHECK` (`src/habu/habu1.f`). Past it: `hb: data space out of
  range: DP <dp> of <cap> bytes`, rc 76, catchable inside `evaluate`, both
  numbers offsets from `data-base`. The refusal names no definition; it reports
  the line it came from (` at <path>:<line>`, added when a source file is open).
  Repair: hold bulk data in `MEM:ALLOC-BYTES` or a `DYNAMIC-BUFFER`, which map
  their own pages, not in the dictionary. While a task is live every one of
  these sinks exits `$4F` before it writes, as do `evaluate` and
  `evaluate-closed` ([threads.md](threads.md)): the definers name their token
  on stderr, the rest print nothing.

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
- Tests live in the native gate: `test/engine-suite.f`, focused `tools/*-test.f`
  fixtures, and source-specific checks wired through `test/run.f`.
- Orchestration uses `lib/test.f`: suites, groups and tests (gate/row wording is
  legacy). Adapters provide setup/teardown, argv/env policy, filters and process
  execution; test files require their own dependencies; groups are named,
  parallel or sequential; reports print group/test name, state and timing.
- Runners keep no suite iteration state on the return stack while a test runs:
  tests may `catch`/`throw`, so loops use explicit index/count cells that a
  caught throw cannot truncate.
- Fixture helpers live in a private package, not global stems: define the
  package, install helpers into `TEST:*` hooks, define groups and tests, run
  once, assert counters, close the package.

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

  `lib/test.f` is the interface: `T*` assertions; `TEST:SETUP!`,
  `TEST:TEARDOWN!`, `TEST:DRAIN!`, `TEST:ARGS-BEGIN!`, `TEST:ARG+!`,
  `TEST:RUNNER!`, `TEST:STDIN-RUNNER!` install typed hooks; `TEST:GROUP SEQ|PARA
  name` opens a group (the mode token is mandatory, before the name);
  `TEST:;GROUP`, `TEST:SUITE`, `TEST:SUITE-STDIN`, `TEST:;SUITE` and `TEST:RUN`
  define and run. No helper globals such as `FOO-TEST-SETUP-N`.
- Assert the specific outcome: inside checked definitions `[: WORD ;] TTHROWSQ`
  (it runs a `( -- )` quotation) or another stack-preserving `catch` with the
  exact code; top-level scripts that cannot push quotations use `' WORD
  TTHROWS`; diagnostics are captured and matched by substring.
- Run focused fixtures with their owning `tools/*-test.f`, and the full native
  suite when the impact warrants it (below), always through the owning gate
  script so assertion failures set the exit code.
- Property generators use `lib/property.f`: `PROP:SEED!` makes a run
  reproducible, and `PROP:RND%` mixes the generator's high bits before bounding
  a draw. Small bounds do not inherit the raw generator's short low-bit cycles:
  bound 2 produces repeats as well as alternations. The fixed-seed regression
  checks that each occupies 40–60% of 4096 transitions; this is a coverage
  guarantee for that sequence, not a claim of independent or cryptographic
  randomness.
- **False-reject claims need execution proof**: prove the gap with the owning
  `bin/hb --load` or `tools/check.f --source-list`, then run an unchecked copy
  and show the measured stack behaviour matches the declared effect before
  counting a checker limitation. Generator bugs become rejections, not
  certifications.
- **Signature-token changes need direct smoke probes** (`ATOM-TOK?`, `TOK-TYPE`,
  renderer output) before rebuilding around a new token.

### Diagnosing a checker miss

Reduce the failing checked program, identify the violated contract, decide
whether the defect is in a declaration, the checker, generated code or runtime,
fix that layer, and add a regression through the actual load path. A property
outside the type system's contract is stated as such and checked at the right
runtime or analysis layer. No template or task record is required.

## Verification before committing

- **A timing counts only on a quiet box**: 1-minute load under 4 with no
  competing engine; a measurement script refuses when the box never quiets
  rather than degrade the number.

Run focused tests for changed behaviour. For compiler, runtime or broad library
changes, rebuild the engine and run the full suite:

```sh
bin/hb --load tools/build-fixpoint-refresh.f -- install --force
bin/hb --load test/run.f
```

`test/run.f` runs every registered suite, draining the pool between groups
without stopping at a red, and ends with `suites: ran N of N`, the red set with
exit codes, and a nonzero exit if any suite is red. Run lints when their inputs
change. Documentation-only edits and file moves need no rebuild. Loom's model
tests belong to its repo. Report failed or unrun checks plainly, never as a
passing suite.

## Comments & hygiene

- `\` line comments, terse, in one or two lines: only what the code cannot state
  (units, ownership or ordering invariants, a hardware encoding, a checker
  boundary). No restating the code, no design essays, rationale or refutation
  history: that belongs in documentation; meaning lives in names and factoring.
  Remove scratch and debug prints before commit.
- A definition that fails to compile in raw engine mode reports the undefined
  word on stderr and may spill the rest of the definition through the
  interpreter; `tools/check.f --json-errors --all-errors` wraps matched
  undefined tokens in schema-1 JSON diagnostics.

## Habu Native Tooling Gotchas

- **Use the native debugger before print probes**: `docs/debugging.md`, `.s`,
  `BPW+` watch cells, REPL `step`, compiled-word breakpoints (`BP+`, `BP*`,
  `BPN`), `tools/jitdump.f`, `tools/imgdump.f`. Extend them rather than hide a
  missing surface behind prints.
- **Semantic xref is in-image**: ownership, references and call RCA use
  `XREF`/`SEE`/`USES`/`USED-BY` in the live image with CLIs as thin wrappers;
  source search where it answers, native inspection where runtime state matters.
- **Boundary spawns attribute failures.** A gate, test or tool spawning `hb` or
  another child uses outcome capture for expected timeouts and failures, never
  throw-only capture that collapses to a shell rc; the report carries suite/case
  label, phase, executable and argv/load list, outcome kind/code, named rc when
  known, capture bytes/capacity and captured stdout/stderr. Throw-on-timeout
  capture belongs only in a unit test asserting that throw.
- **`DYNAMIC-BUFFER NAME Type` for growing typed tables**: the stored types of
  `TYPED-BUFFER`, closed non-linear layouts included, **and `u8`** — the
  growable byte row. It is the one definer that takes `u8`, the only sub-cell
  type with accessors of its own: `DYNAMIC-BUFFER BYTES u8` loads and `0 BYTES
  c@` reads byte 0 (test/dynamic-buffer.f), while `DYNAMIC-BUFFER X u16`
  throws 7121. `count NAME-RESERVE` allocates at least that many elements and
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
- **Large tool bundles are supported.** Never split tools to dodge DATA
  pressure. `create … allot` is dictionary-sized static storage; runtime-sized
  buffers use `lib/memory.f` (`MEM-ALLOC-BYTES`, `MEM-ALLOC-64K-BUFFERS`),
  scaling with OS mappings rather than `DATA-SIZE`. Tools keep as many 64K
  buffers and spans as needed, one contiguous `MEM-ALLOC-64K-BUFFERS` span or
  many; the only limits are cell-size overflow checks and explicit OS allocation
  failure. If composition still hits capacity, fix the shared memory model and
  add a regression for the composed load.
- **`create … allot`, `BUFFER:` and `TYPED-BUFFER` allot zeroed space on a
  cell-rounded address.** `create`, hence `variable`, rounds the data pointer to
  a cell before publishing, and `align` does on request. So a `create … allot`
  block and `MEM-ALLOC-BYTES` memory are both cell-aligned for a cell-typed
  reader such as `lib/json-read.f` `INIT`, and a row a foreign call reads as an
  aligned C object needs no alignment word of its own (lib/net/curl.f's fd_sets
  and out-parameter cells). `allot`, `c,` and `,` never realign: storage carved
  after byte-sized data is misaligned unless `align` precedes it, and such a
  reader refuses it (`E-STORAGE`).
- **Missing convenience words are not bugs in the standard.** Core lacks `pick`,
  `within`, `s>number?`, `move`, `fill`, `erase`, `bl` and `*/`; each is
  `E-UNDEFINED` in a checked body (`: W ( -- ) bl ;`). Use cells, explicit
  increments and comparisons, and the checked byte and string helpers. `0<>`,
  `true`, `false`, `fdup`, `fover`, `fdrop`, `f<=` and `f>=` come from
  `lib/prelude.f`, which the engine already provides: writing `require
  lib/prelude.f` documents the dependency but is not load-bearing. Never
  re-derive `0 0=` / `0 0= 0=` by hand.
- **Tool libraries keep checking enabled** for themselves and their callers;
  when removing a legacy unchecked span, keep hook restoration until it is gone.
- **Pre-checker bootstrap stays minimal**: install checking as soon as the
  checker exists; never extend the startup exception to generated application
  code or load ordinary Forth unchecked.
- **Legacy checker preludes rebind the existing hook**: a prelude that disables
  checking after `src/core/check-hook.f` restores it at once with `' HOOK
  set-check`. A migration constraint, not permission for new spans. No second
  hook name in baked tty/stdin bundles (duplicate enforcement fails closed at
  startup); a snapshot or AOT stage keeps a different hook local and leaks no
  duplicate REPL hook into `bin/hb`.
- **Bootstrap and fixpoint temp roots are explicit script args** after `--`; no
  stale seed envp capture; generated paths stay under that root; the build
  driver owns path construction.
- **Escaped literals for readable snapshots**: `s"` reads no escapes; `S\"`,
  `C\"`, `.\"` accept C-style escapes (`\\`, `\"`/`\q`, `\n`, `\r`, `\t`,
  `\xNN`, `\z`, …) for direct JSON/source expected strings. `S\"` needs its
  delimiter space (`s\"\n"` is one undefined token) and reads `\u` as its own
  escape, so a fixture holding JSON writes `\\uXXXX`. Generated syntax from
  fields uses checked byte/field helpers or `lib/json-write.f`.
- **Generated fixtures use unique test-owned names** (`CAE-CAP-OK-0`,
  `GDX-AE-BAD1`); never baked generic names (`OK`, `BAD`, `FOLD`, `RESET`) or a
  repeated stem unless testing duplicate rejection.
- **Source-use guards match tokens**, lexing whole tokens and skipping comments
  and strings; substring matches (`FOO` in `FOO-BAR`) hide policy bugs.
- **Check native emitters before building an image**: checked algorithms,
  emitted code validated by focused tests; pre-checker emitters keep their
  source-shape checks until converted, which justify no new unchecked bodies.
- **Fixed DATA header cells need a layout audit** against the reserved ranges in
  `src/habu/layout.f` (`VTAG-OFF`, `VVAL-OFF`, `SNAPSTK-OFF`, body buffer,
  return stack, locals table, register tables, breakpoints, snapshot cells); a
  cell inside a scratch range is overwritten by compiled source. Add a
  regression for the exact overlap class.
- **Snapshot builders retire the baked tail** (`undefine NAME` for one word,
  `HIDE-DEFS-FROM` only for refresh tail truncation) and append the snapshot
  entry file; they never replay baked core, target or image files to mask
  duplicates.
- **Snapshot builders reset process-local pointers**: mmap-backed image and
  include pointers and cursors (`MBUF-A`, `MP`, `MLEN@`/`MLEN!`,
  `INCLUDE-BUFS-A`, include depth/read/path cells) are valid only in the
  creating process; clear them in a named reset word before `BUILD-SNAP-HDR` or
  fresh image emission, never by source replay or redefinition.
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
  `REGISTER`/`PREPARE` adds a second entry. Follow `lib/net/udp4.f`
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

Each of these was measured on the engine; the fact that proved it is beside
the rule.

- **Initialized scoped storage has two bounds.** A callback for
  `C2-MEM:WITH-INIT` may declare `forall<i inside [l,T],[ ... ]>`; its fresh
  scope `i` is inside the byte view's lifetime `l` and every scope dependency
  of the initial record `T`. The operation accepts a live unique byte view and
  a complete, non-unique, non-stale fixed-cell product. It clears the initialized
  extent before returning the original byte view and its full bound.
  `test/c2-init-program.f` measures the two-cell and nested record paths,
  clear-before-return, restored bound, short capacity, callback throw and task
  halt. `test/c2-init-refusals.f` rejects raw/shared authority, open, unique or
  stale records, a schema whose field widths disagree despite the same total,
  an incompatible callback and escape of `i`.
- **Initialized record tables use one owning scope.**
  `C2-MEM:WITH-RECORDS` accepts a unique byte view, a nonnegative count, and a
  committed non-owning `DERIVE init` record seed. Its callback receives an
  opaque linear `records<p,i,a,T>` under `forall<i inside [l,T],...>`.
  The count times the committed record size must fit the original byte bound;
  count zero is valid. `C2-MEM:WITH-RECORD` checks a zero-based index and loans
  an element as `mut-view<p,j,a,init<i,T>>` under `forall<j inside i,...>`.
  Its callback cannot return `j`, and the table returns without reconstructing
  or clearing that element. Closing the table clears the complete initialized
  extent once and restores the original byte bound. `test/c2-records-e2e.f`
  exercises source, native tier one, saved-image, cleanup and refusal paths.
- **An initialized field loan keeps its owner's init lifetime.**
  `C2-MEM:WITH-FIELD name` takes a live unique
  `mut-view<p,i,a,init<i,T>>` and a callback. `name` is resolved only against
  the receiver's committed instantiated schema. The callback's fresh child
  scope `j` is inside `i`; its view has field type `F`, unchanged init scope
  `i`, and structural region `field(a,name)`. On normal return the operation
  restores the exact original parent without copying the field into a new
  record; the runtime closes the child loan on throw or task halt.
  `test/c2-field-loan-e2e.f` exercises the nested nonzero-offset Pair, source
  and native lowering, a saved explicit callback scheme, refusals and cleanup.
- **A `TYPED-BUFFER` element is a storage type, never bare `u8`.**
  `2 TYPED-BUFFER TB u8` throws `E-LAYOUT-BUFFER` (7121) from
  `STORAGE-VALIDATE`; a byte row is `n BUFFER: B` (lib/string.f),
  `( -- ptr u8 )`. A fixed element is a whole allotted cell, and a stored `u8`
  would mint a `ptr u8` that cell `@` cannot read. **`DYNAMIC-BUFFER` is the
  one definer that does take `u8`**, because it allots no element storage and
  scales the index by the element's own width: `DYNAMIC-BUFFER BYTES u8` loads
  and `0 BYTES c@` reads byte 0 (test/dynamic-buffer.f), while
  `DYNAMIC-BUFFER X u16` still throws 7121 — `u8` is the only sub-cell type
  with accessors of its own.
  `create … allot`, `BUFFER:` and `TYPED-BUFFER` all allot
  zeroed space on a cell-rounded address (measured: after `create A 1 allot
  create B`, `B FFI:>CELL 7 and` is 0 and the bytes read back zero), so a row
  a foreign call reads as an aligned C object needs no alignment word of its
  own (lib/net/curl.f's fd_sets and out-parameter cells).
- **A `TYPED-BUFFER` or `LAYOUT-BUFFER` count is a decimal literal or a word
  of effect `( -- n )`.** The source pre-verifier reads the count as TEXT and
  never runs it (`verify-source.f` `RECORD-TYPED-BUFFER` hands the previous
  token to the checker's `CHECKER-LBUF:CERTIFY`). A decimal literal certifies
  by its value, positive and within `LBUF-COUNT-MAX` for the element width.
  Any other token certifies by its effect: it must resolve as a body would
  resolve it (bare, package-qualified or engine-held) to a word that takes no
  input and leaves one cell `n` accepts. A `constant`, a computed constant
  (`6 constant OPS  2 constant KEYS  OPS KEYS + constant VOCAB` then
  `VOCAB TYPED-BUFFER ROWS n`), an engine constant such as `HIR:OPCODES` and a
  colon `( -- n )` all pass `tools/check.f`. The load bounds the value
  (`src/core/layout-buffer.f` `LBUF-EXTENT?`): `0 constant Z  Z TYPED-BUFFER R n`
  passes pre-verification, and its load throws `E-LAYOUT-BUFFER` (7121, rc
  67). Measured refusals, 7121 from the pre-verifier (rc 67): an unknown name,
  a `variable` (it leaves an address), a `( -- bool )` word, a word with an
  input (`4 CAP TYPED-BUFFER` for `CAP ( n -- n )`), a name the scope refuses
  (a `using` public that shadows a global, or one two used packages export),
  `$40`, a literal `0`, and an inline expression: `Q-MAX Q-SEM-N * TYPED-BUFFER
  Q-SEMS TASK:sem` (lib/queue.f:42) is refused on its `*`.
- **A `create … does>` definer teaches the checker what its words are, whether
  or not its text was read.** A definer the source pre-verifier READ is learned
  from the clause text (`verify-source.f` `DEFINER-EFFECT`). A RESIDENT one —
  compiled in the checking process, its text never scanned — is known because
  the checker latches the created-word effect when it certifies the clause at
  the definer's `;` and stores it against the definer's symbol (`checker.f`
  `DOES-EFF-LATCH!`, the NORETS entry's CREATES cell); `NG-BUFFER`
  (lib/type/deftype.f:61) certifies through it. A straight-line wrapper read
  from source inherits the same row from the text. A wrapper of a RESIDENT
  definer is learned by the checker's own body walk (`checker.f`
  `WRAPN`/`WRAPC`/ `WRAPBENT`, taken by the same publish tail): a body that
  calls exactly one definer and never bends — no control frame, quotation,
  `exit`, `leave` or `recurse` — creates what that definer creates, so
  `CODEGEN:BUFFER` around `BUFFER-E` is a definer too, and `tools/check.f
  lib/process-env-test.f` certifies `PROC-ENV-DIAG` (lib/process-env.f:93).
  `TRUSTED:` changes neither answer: a trusted body is asserted, not checked,
  but its `does>` clause is still the declaration both paths record — the
  scanner reads the clause out of the trusted body (`verify-source.f`
  `SCAN-TRUSTED-BODY`) and the engine takes the latch at the trusted publication
  (`checker.f` `TRUST-DECL`). With a `TRUSTED: TD ( n -- ) create , does> ( --
  ptr n ) ;` in a required module, `5 MOD:TD W  : G ( -- n ) W ;` is the
  `E-MISMATCH` the effect deserves, not `E-UNDEFINED`, and `TASK:MIN-STACK
  TASK:TASK T1  : F ( -- ptr n ) T1 ;` certifies.
- **A `TRUSTED:` body may answer a family value from loose cells; a checked body
  groups its own result.** The native elaborator takes the declared row as the
  grouping of the cells the body leaves (`elaborate.f` `TRUSTED-FRAME-RESHAPE`):
  `TRUSTED: GWN-MAKE ( -- gwfn ) 7 1 ;` builds stripped and `MATCH`es as `gns`
  carrying 7 (test/gate-aot-positive-lib.f `TRUSTED-ROW`); the payload sits
  below and the tag on top. A checked body keeps the strict return check: two
  loose cells under a row declaring one two-cell value throw `E-NELAB-JOIN`
  (-8503, test/compiler/native-elaborate.f `RGLUE`). A count that disagrees is
  `E-NELAB-ARITY` either way, and a forged tag is still refused where the value
  is consumed (`hb: bad layout tag`, rc 85).
- **A name binds one word at every tier, and that word's own facts judge it.**
  The checker, the JIT and tier 1 ask the engine's one lookup (`scope-find`:
  the open package, the global wordlist, then the used publics), so a word
  redefined after `undefine`, a package word spelled like an engine word and
  an `EXPORT` alias are each checked and called as the word they are. The
  rules that type an engine word by what it does (`@`, `!`, `?dup`,
  `record-at`, `create`, `variable`, …) follow the identity the engine
  registered on that word's symbol (its `INTRINSIC` id, or `CTL-CORE-OP` for
  the stack shuffles and zero tests), which a redefinition does not carry, and
  the control facts a definition earns are recorded on its own symbol. After
  `undefine dup  : dup ( n -- n ) 100 + ;`, `: T ( -- n ) 5 dup ;` leaves
  `105` at both tiers (test/undefine-binding.f), and `package P : DIE ( ptr u8
  n -- ) 3 die ;  : T ( -- n ) s" x" DIE 0 ;` is `E-DEAD-CODE`
  (test/checker-dead-path-suite.f).
- **A name the engine holds no word for binds nowhere in compiled code.** The
  checker binds a token to the word the lookup finds, to the definition it
  recorded last and the engine has not yet published, or to a keyword it types
  by axiom (`>r`, `r@`). A name only the checker knows - a row `CHECK!` alone
  recorded, a qualified `TRUST` row with no word behind it, a word the source
  pre-verifier registered, a `CHECKER-EXPORT` alias - is unresolvable, bare or
  qualified, as the compiler finds it `E-UNDEFINED`. It binds on the certify
  path, which replays over the checker's records: `VERIFY:CANDIDATE-IN-SCOPE`
  (`src/habu/verify-source.f`) answers what that path says of a candidate. A
  candidate scope checks rows that publish together, so each row binds for the
  scope's later rows until it closes: a `STRUCTURE` deriving `hash` checks a
  `HASH` that calls its `UNMAKE` before either is published. Measured on
  `bin/hb`: after `s" SPANX ( -- n ) 1" CHECK!`, `: C ( -- n ) SPANX ;` is
  `E-UNDEFINED` and the candidate `C ( -- n ) SPANX` answers 1 live, as
  `s" C ( -- n ) SPANX" CHECK!` does, and -1 through `VERIFY:CANDIDATE-IN-SCOPE`
  (test/engine-suite.f, test/pointer-storage-test.f);
  `STRUCTURE der 0 DERIVE eq hash FIELD x n ;STRUCTURE` declares and its
  `DER:HASH` runs (test/structure-certify-suite.f).
- **Native width depends on how the cells are used.** A 63-cell identity
  compiles and runs in native AOT; the same 64-cell definition currently
  refuses with `E-A64RAV-DKEEP` (-8611). At tier 1 on Darwin ARM64, consuming
  all 25 entry cells in a sum currently refuses with `E-A64RA-POOL` (-8446);
  the 24-cell sum passes. Forwarding cells does not require that same
  simultaneous register set. The IR signature list itself holds at most
  64 cells: its sixty-fifth staged input or output rejects with
  `E-IR-TYPE-ARITY` (-6688, `test/compiler/ir-type.f`). A record uses the same
  list, with one staged value per cell. A checked 34-cell nested record
  roundtrip and the consumed-entry boundary are exercised by
  `test/compiler/native-generated-constructor.f`.
- **`s"` reads no escapes; `S\"` does.** `S\"` needs its delimiter space
  (`s\"\n"` is one undefined token) and reads `\u` as its own escape, so a
  fixture holding JSON writes `\\uXXXX`.
- **`.` ends the line.** The native `.` is newline-terminated, not
  space-terminated: `11 . 22 . cr` emits `11\n22\n\n`, so an assertion for two
  dotted numbers on one line never matches; digit emitters (`GT-U-TYPE`,
  `TS-N.`) build inline text.
- **`private` is a convention until the package seals itself, or until the
  capture does.** Any file may reopen `package NAME private` and call its
  internals. The protection idiom at the foot of a substrate file —
  `get-current prot-wid-add` — seals the wordlists; after it a second file that
  reopens the package dies at load with the package name as its whole message
  (exit 84). The native build seals every package the engine bakes the same
  way when it captures the image (**Packages**). Two files that belong
  together are two packages with a one-way dependency, or one package that only
  the last file seals.
- **An integer becomes an address or an execution token only through a private
  `CAST:`.** `CAST: >BYTES ( n -- ptr u8 )`, `CAST: >OP ( n -- [ n -- n ] )`
  and, for `STRUCTURE box 1 FIELD value a ;STRUCTURE`, `( n -- box<ptr u8> )`
  certify in a package's private section and are `E-CAST-MINT` (7147) at top
  level or under `public`, in the source pre-pass as well: the cast asserts
  what no check can see, so only code inside its package reaches it. Take the
  integer from a typed source, an address from the distance
  `B NULL-PTR BYTE-VIEW -` and an execution token from `search-wl`. A
  type-variable pointee is `E-CAST-LINEAR` (7137): `( n -- ptr a )` would
  forge a pointer to any nominal. The rule reads a layout's own `FIELD` and
  variant payload types as it reads its arguments, through nested layouts: after
  `STRUCTURE pfbox 0 FIELD p ptr u8 ;STRUCTURE`, `CAST: >PFBOX ( n -- pfbox )`
  is `E-CAST-MINT` at top level, since `PFBOX:UNMAKE` would hand out a
  `ptr u8`, and certifies in a private section. A layout whose field holds
  another package's family is `E-CAST-OWNER` outside that family's package.
  test/cast-suite.f runs the round trips; test/cast-negative-suite.f pins the
  refusals.
- **A `DEFTYPE` a defining word hands out sits in the public section.** A
  `does>` body is checked code and may publish a nominal handle directly, but
  the child's stored signature names the type, and a private one does not
  resolve for a reader: the definition is refused as it is made (`checker: bad
  stored signature`), even when only the package uses the converters.
- **A nominal error needs its own result family.** Constructing `RESULT:OK` in
  the ok-only path leaves the err variable of `result<a,b>` free, and a free
  variable unifies with a structural type but not with a nominal ENUM or
  TYPEFAMILY. Declare `SUMTYPE foo-result 1` whose ok variant carries the
  payload and whose errors are nullary variants (the `numeric-result` idiom in
  `lib/num-arithmetic.f`).
- **An arity-1 SUMTYPE is spelled with its argument in a signature and bare in a
  `MATCH` selector.** `family<PKG:t>` in the effect (`wrong arity for type
  family` otherwise), `MATCH family` at the arm; never both together.
- **A checker atom prefix reserves the whole lowercase `prefix-*` namespace.** A
  `layout-` prefix makes an ENUM variant spelled `layout-conflict` throw 7110;
  sweep with `rg '\bprefix-'` before choosing one. Declaration-grammar keywords
  are reserved family names too (`ENUM policy` throws 7110).
- **`0 set-check` also disarms the compile preflight.** A program that opens
  with it runs with neither gate. Declare the primitive instead, in the axiom
  form the engine's own primitives use (`PRIM: name PE-… PRIM;`), and the rest
  of the program still compiles checked.
- **A `defer` in `src/core/checker.f` before `: TRUST` takes a pre-trust pending
  slot.** `src/habu/layout.f PD-CAP` bounds the table; overflow dies at boot
  with exit 72 (`C-PD-DIE-FULL`). `test/pre-trust-defer.f` checks it. Add a selector to an
  existing hook instead (`SHADOW-DIAG-XT ( n -- )` carries two diagnostics), or
  place the defer after `: TRUST`.
- **A pre-hook word needs an axiom row in a checked body on a from-source
  engine.** `src/core/cell-effects.f` supplies rows for `PATH-CAP`,
  `E-PATH-RANGE` and `SCOPE-FIND-AMBIGUOUS`. A constant without a row can be read at top level into a
  file-owned constant (`REG-PROT-CAP constant MY-CAP`); `test/cold-naming-test.f`
  checks the refusal and the accepted forms.
- **`MATCH` and the other compile keywords name words, not constants**, even
  inside a package; a `case` default runs with the selector still on the
  stack. Two flags are not compared with `=` (`bool bool` is refused): a test
  asserts a flag with `TTRUE`/`TFALSE`, not `T=`. (Measured in Tender.)
- **A quotation sees no locals and must be stack-preserving under `catch`.**
  A value a `catch`, `finally` or locked body needs travels through storage it
  can address; after `catch` the restored cells are not the handles that were
  pushed, and a nominal handle cannot cross `catch` as a quotation's result —
  a one-slot `TYPED-BUFFER` holds it. `TTHROWSQ` runs a `( -- )` quotation.
  (Measured in Tender.)
- **A quotation-typed LOCAL cannot be caught; the same quotation on the stack
  can.** This is refused — `hook: non-certified definition: f at 'catch'`, rc 70
  — because the thing being caught has to be a literal quotation:

  ```forth
  : F ( ptr u8 n [ ptr u8 n -- ] -- )
     {: a u q :}
     a u q catch … ;
  ```

  What is admitted is a preserving word that takes the callback as an ORDINARY
  STACK ARGUMENT, with the literal quotation around it: `INV` under `[: INV ;]
  catch` answers the callback's thrown code.

  ```forth
  : INV ( ptr u8 n [ ptr u8 n -- ] -- ptr u8 n [ ptr u8 n -- ] )
     {: a u q :}
     a u q execute a u q ;
  ```

  So a combinator that must catch its callback needs no cell to park it in
  (lib/fs.f's walk), and `drop` on the quotation-typed value afterwards is
  admitted.
- **`catch` restores the DEPTH of both stacks and never their contents.** On a
  throw `nv ' WORD catch` leaves `( x code )` where `x` is whatever the callee
  left in that cell: keep every handle to release in your own locals and read
  only the code. The checker names that read only for a caught quotation
  LITERAL: a cell in its window — its declared fixed input prefix, on either
  stack — keeps its input type only when every throw path of that body provably
  left it untouched. Every other window cell is `stale<t>` after the catch, and
  reading one is `E-STALE-READ` (repair class `keep_value_before_catch`), named
  on the token that reads it. `test/catch-stale-suite.f` pins the rows.
  - A stale cell may be moved, dropped or bound to an untyped local; `@`, `c@`,
    arithmetic, a typed local and the definition's own declared output refuse an
    unguarded read. The exact code returned by that `catch` proves its normal
    output on the success arm of core `0=` (or the false arm of core `0<>`), so
    that arm may read the corresponding value; a different zero, a shadowed
    predicate, or another catch's code proves nothing. The successful value
    stays marked as physically replaced: if a later throw passes it through an
    enclosing catch, that catch cannot treat it as intact. A live loop back edge
    must carry the same status proof and replacement mark as its entry, or the
    loop is refused. Quotation application carries replacement marks on its
    explicit outputs, including a zero-input quotation's internal result, but no
    private success proof escapes its boundary. A declared polymorphic effect
    transports types, not the identity of a particular catch status.
  - The evidence is the identity of the row's term at the throw edge, not
    unification: a quotation literal infers its window on a fresh row, so until
    the catch site fits it the window cells are unbound variables, and `( n -- n
    n ) [: drop 5 -99 throw ;] catch` is refused too: the value the body left is
    a different term.
  - A CALLEE carries its own evidence: the checker records per definition which
    declared inputs every throw path of its body left where they were, so `( ptr
    u8 -- ptr u8 n ) [: WMAYBE ;] catch` over `: WMAYBE ( ptr u8 -- ptr u8 ) dup
    c@ 0= IF E-CS-BOOM throw THEN ;` keeps the address typed, while the same
    catch of a body that drops it, swaps it, binds it to a local or overwrites
    it on one arm still stales it. A called word that throws counts as
    overwriting every input it declared unless its own body proved otherwise: `(
    ptr u8 -- ptr u8 n ) [: W ;] catch` is refused for a `W ( ptr u8 -- )` that
    drops, swaps or rebuilds the cell on any throw path — one such arm clears
    the evidence for all of them. The evidence covers at most the top 20
    declared inputs, a deeper one being reported not intact. A callee that BINDS
    its inputs to locals and pushes them back restores the cells but records
    nothing.
  - The edge is not part of a quotation's TYPE, so it travels on a TERM: a
    literal's, and the one `['] W` pushes, which takes W's own edge with W's own
    evidence. `['] WMAYBE catch` keeps the address typed while `['] WSWAPT
    catch`, R1's body as a callee, stales what it swapped, per declared input,
    in W's own orientation. A tick bound to a local in the same body is that
    same term and keeps the edge.
  - Outside the rule is the route whose term is a fresh instance of a DECLARED
    type: `catch` of a quotation parameter, of a typed `xt<effect>` cell or of a
    `defer` keeps the window typed even when the target throws. A tick of a
    callee that never RETURNS takes the edge but not the dead flag (the native
    elaborator cannot reconcile a body with no result row with a tick's routine
    ABI: `E-NELAB-QUOT`, named on the `[']`), so that catch is still the
    fit-check against W's declared output row.
- **A LINEAR handle cannot cross a quotation-literal `catch` at all.** Reading
  it afterwards is `E-STALE-READ` (`expected: XML:reader actual:
  stale<XML:reader>`) and dropping it is the linear refusal — a linear cell may
  not be dropped, with or without a catch. `TYPED-VARIABLE V XML:reader` and `1
  TYPED-BUFFER V XML:reader` both throw **7121** at the declaration, so the
  one-slot `TYPED-BUFFER` route does not apply to a linear nominal. The two
  shapes that work: open the handle INSIDE the caught body (`lib/xml-test.f
  BAD`), or, where the handle must SURVIVE the caught failure, name the body and
  call `['] WORD catch` (`lib/byte-edit-test.f`, `lib/xml-test.f`
  `CAPACITY-AND-STATE`, `lib/json-read-test.f JRT-CATCH-BAD`), which survives
  exactly while WORD's own throw paths leave the handle where they found it, the
  evidence the tick carries. A MULTICELL bundle is stale as one value, not as
  the W hidden cells that carry it: an `option<pt>` window comes back as ONE
  `stale<option<pt>>` — one `drop` removes it, `nip`/`swap` move it, an untyped
  local holds it and gives it back stale, and every typed use (a word input, a
  typed local, a `MATCH`) is `E-STALE-READ` naming the logical type
  (`test/catch-stale-suite.f CS-SECTION-BUNDLES`, `test/compiler/native-catch.f
  CATCH-STALE-DROP`).
- **A checked word evaluates source with `evaluate-closed`, never
  `evaluate`.** A body naming `evaluate` is `E-UNSAFE`: its effect is the
  text's. `evaluate-closed` runs the text on a guarded data stack of its own
  and refuses whatever the text leaves, so its row is
  `( ptr u8 n -- )` and `: X ( ptr u8 n -- ) evaluate-closed ;` certifies.
  Measured (`test/compiler/native-eval.f`): `depth` in a text starts at 0;
  `7 [: s" drop" evaluate-closed ;] catch` answers 70
  (`hb: interpret stack underdepth: drop`) with the 7 still below it;
  `s" 1 2" evaluate-closed` throws `E-EVAL-RESIDUE` (-3804; uncaught,
  `hb: uncaught throw code -3804`, rc 67), and an inner text's residue reaches
  the outer text's caller as -3804; a definition the checker refuses throws 70;
  a text that ends inside a definition it opened, `s" : D ( -- n ) 42"
  evaluate-closed`, throws `E-EVAL-UNFINISHED` (-3805) after
  `hb: closed text ended inside a definition: D at <path>:<line>` (uncaught,
  rc 67), D is rolled back and the caller's next token is interpreted, and an
  inner text's unfinished definition reaches the outer text's caller as -3805;
  an xt the text runs cannot reach the caller's cells either: with
  `W ( n n -- n ) +`, `5 [: s" 1 ' W execute" evaluate-closed ;] catch`
  answers 70 (`hb: interpret stack underdepth: execute`) with the 5 intact
  and W unfinished, as do `' W CLEAN finally`, a quotation xt and a reach in
  an inner text, and `1 ' W catch` inside the text receives the 70 itself,
  because one cell under the text's floor is its stack's guard page and the
  engine throws that fault; at plain top level `1 ' W execute` is the same
  line and rc 70, not a crash; a data-stack overflow in a text exits
  `hb: stack bounds exceeded (data)`, rc 102, as outside one, and so does a
  jump under the floor ([debugging.md](debugging.md)); with a task live it
  exits `$4F` before reading the text and prints nothing, as `evaluate` does.
  The open cases are under **Checked code and primitive boundaries**.
- **A test takes a value out of a text with `TEST-EVAL`, never an `evaluate`
  wrapper.** `lib/test.f` loads it: `TEST-EVAL:N ( ptr u8 n -- n )` evaluates a
  text that must leave exactly one cell, `TEST-EVAL:FLAG ( ptr u8 n -- bool )`
  reads that cell with `0<>`, and `TEST-EVAL:RC ( ptr u8 n -- n )` is the
  text's `evaluate-closed` throw code, 0 when it loaded. N runs the text with
  plain `evaluate` at the top level of a constant closed text, so the closed
  floor sits under the text and N's store takes its one cell. Measured
  (`lib/test/eval-test.f`): an empty text throws 70
  (`hb: interpret stack underdepth: TEST-EVAL:N!`), `1 2` throws
  `E-EVAL-RESIDUE`, `drop 1` throws 70 with the caller's cells intact, N runs
  inside the text of N, `: W ( -- bool ) s" 1" TEST-EVAL:N ;` is refused
  (`expected: bool actual: n`), and a text that ends inside a definition it
  opened throws `E-EVAL-UNFINISHED` at the end of N's own closed text. N keeps
  `evaluate-closed`'s open cases.

## Spans: a pointer that carries its reach

When a word writes into a buffer or indexes one, pass a `SPAN:span<u8>` rather
than a `ptr u8` and a separate length: `lib/span.f` bounds-checks every access
against the reach the span carries. Take the span from a producer — `n
SPAN-BUFFER: NAME`, `n SPAN-CELLS: NAME`, `MEM:ALLOC-SPAN` — or narrow one you
were handed with `SPAN:SKIP` / `SPAN:TAKE` / `SPAN:SUB`, which can never widen
it. `SPAN:MAKE`, the one place an address and a number become a reach, is
admitted in `lib/` and `src/` only; `tools/lint/bare-copy-lint.f` reports it
(and bare `BYTE-COPY`) anywhere else. A read-only source stays the `( ptr u8 n
)` string idiom. The reach is counted in bytes whatever the element type;
`docs/type-system.md` § 11 has the measurement.
