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
- A `PPRIM:` row closed with `CLOSE-PRIVATE` instead of `PPRIM;` belongs to its
  package: only that package's checked bodies call the primitive, at both
  tiers, and a checked caller anywhere else is refused by name. A checked `[']`
  of it is admitted and refused exactly where that call is. At top level a call
  of it is refused as in a body (`E-CAP-TRUSTED`) and `'` yields the xt.
- Never assert that arbitrary `evaluate` preserves the stack; use typed
  quotations for known callbacks. A checked word evaluates source with
  `evaluate-closed ( ptr u8 n -- )`: the text runs on a guarded data stack of
  its own, must leave nothing and must close every definition it opens
  (measured under **Rules learned by refusal**). The loader and every
  source-generating definer evaluate through it, so a loaded file
  (**Packages**) and a generated declaration are closed programs too. A text's
  top-level code is typed as a body is: with `variable V` and
  `S$ ( -- ptr u8 n )`, `s" S$ drop V !" evaluate-closed` is `E-RAW-CELL-PTR`
  at `!` and throws 70 before V changes. One case stays open: a text that runs
  `0 set-check` leaves every later definition and token unchecked, inside the
  text and after it.
- Existing TRUST forms are legacy awaiting removal; mentions below describe
  legacy syntax only.

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
  preceding scope. A local named `;]` is `E-NELAB-LOCAL`; a group that writes
  `[:` is `E-NELAB-QUOT`.
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
  `provided` are reserved because `tools/check.f` reads their spelling
  lexically, and so are the target predicates `HB-TARGET-LINUX?`,
  `HB-TARGET-MACOS?` and `HB-TARGET-LINUX-X86-64?`, whose spelling it reads to
  skip the arms of an `if` the engine's target never runs. Lexical locals such
  as `{: i:n :}` stay legal. `tools/check.f` reports `E-RESERVED-DEFINITION`
  with file, line and token.
- **Never define a number-shaped word.** Numeric parsing precedes dictionary
  lookup: `: 42 ( -- n ) 7 ; 42 .` prints 42, not 7. `tools/check.f` refuses
  such definitions as `E-NUMERIC-DEFINITION`, including dot-digit tails such
  as `U.0`. Dot-letter printers (`.U`, `.INT`, `F.N`) and names such as
  `1STNZ`, `0<>`, `2DUP` remain legal. Interpretation, compilation,
  `evaluate` and the checker use one literal grammar: integer `-?d+ | -?$h+`;
  float `-?d*.d+`, exactly one dot with a digit after it. Thus `.5`, `-.5`
  and `.0` are floats; `5.` and `..5` are words. A decimal literal too large
  for a cell is still a number and is refused before dictionary lookup:
  `99999999999999999999` is `E-UNDEFINED-TOP-LEVEL` at top level. There is
  no `BASE`, `HEX` or `DECIMAL`: write `$ff` for hex, and `.` prints decimal.
- **Engine sources are exempt from the reserved-word rule**: they implement the
  language and define the reserved words themselves. `tools/check.f` does not
  check a file the engine provides (`E-ENGINE-PROVIDED`); rebuilding `bin/hb`
  checks it.
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
prefixes (`TASK-KILL`); prefix-style global APIs are legacy debt.

**A package is named for its subject**, the format, device or domain its words
act on (`JSON`, `ZIP`, `TASK`), never for an operation, direction or role on
that subject: no `-READ`, `-WRITE`, `-IN`, `-OUT`, `-READER` or `-WRITER`
package. The operation belongs in the word, verb first: `JSON:WRITE-OBJECT-START`,
not `JSON-WRITE:OBJECT-START`. One subject's reading and writing words share its
package; a large one spreads across files that each reopen it. A test package is
`<SUBJECT>-TEST`.

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
- A package the engine bakes is sealed: on `bin/hb`, reopening it or defining
  into either of its wordlists is refused naming the package or word, and
  `tools/check.f` refuses it as `E-STATEMENT-THROW`. Use its public words
  qualified or through `using`. An application image keeps its own packages
  reopenable.
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
  evaluate it with `evaluate-closed`. Its top level starts at `depth` 0, a
  token reaching its includer's cells throws 70, and a file that ends with
  cells on the stack is refused `E-EVAL-RESIDUE`, so a value crosses a load
  only as a word the file defines. A file that ends inside a definition it
  opened is refused at its end, as every source is (**Rules learned by
  refusal**): `hb: source ended inside definition: D at <path>:<line>`, so no
  definition spans a load. Fix a file that leaves cells or a definition; never
  loosen the loader. `hb prog.f` and a program on stdin are not loaded files
  and keep their top-level stack.
- **Source roots.** Relative dependencies search the root of the file that
  requires them, then the engine's source root, then the invocation working
  directory, each root once. A named `--load` entry's root is its directory; a
  program run as `hb prog.f` has its own canonical directory, also when started
  through a symlink of another name, and is recorded as loaded, so its require
  of itself is a no-op; a stdin session has the working directory. The
  engine's source root is the working directory when it is a
  Habu tree, else the tree above the engine's executable (found through
  symlinks) when that is one, else the working directory; so an installed `hb`
  runs from any directory, and inside a checkout the checkout is the root. No
  variable or word sets it. Habu's libraries always load Habu's files and an
  application's files load its own: the working directory answers only what
  neither root holds, such as a tree-relative require in an application entry
  below its tree, run from the tree. That entry's tree-relative require of a
  path Habu also has loads Habu's file, so an entry below its tree requires
  the tree's files relative to itself (`../src/model.f`). A relative `--load`
  entry or program file is relative to the working directory alone; a dependency keeps the root that resolved it for
  its own loads; absolute paths bypass the search; the process working
  directory never changes. So `/work/app/main.f` can require `src/model.f`,
  and that file `src/math.f`. Nested loads, file sizes and the loads a file
  makes have no fixed limit.
- At the top level of a stdin session or of a program file run as
  `bin/hb file.f`, SwiftForth's path words, public in package `SOURCE-ROOT`
  (`SOURCE-ROOT:CD`, or `CD` under `using SOURCE-ROOT`), move the current
  path, the first root a relative path searches: `CD <dir>` sets it, relative
  to the current path when relative, and a bare `CD` prints it; `PUSHPATH`
  saves it and `POPPATH` restores it. Inside a loaded file (`--load` or
  `require`) they are refused by name. They never change the engine root or
  the process working directory, and an image cannot be saved with a path
  pushed. A file scopes a root with
  `SOURCE-ROOT:WITH ( ptr u8 n [ -- ] -- )`, which restores the caller's on
  return or throw; a root that does not resolve to a searchable directory, and
  an image save inside the scope, are refused by name. Fixtures resolve
  against `SOURCE-ROOT:CURRENT$ ( -- ptr u8 n )`, never a script argument.
- The files the engine bakes are already loaded: `require` of one, such as
  `require src/core/sha256.f`, is a no-op, and an application's file named like
  an engine file outside the engine's source root stays the application's.
- A package wordlist is a case-insensitive no-duplicate set (`RESET` and `reset`
  are one tail) across reopened blocks and across `:`, `create`, `variable`,
  `constant` and `TRUSTED:`; silent last-definition-wins shadowing is always an
  error. Redefine explicitly: `undefine NAME` retires the active entry and
  clears checker signature, defer-target and control metadata, then the name may
  be reused. Shadowing an outer, global or built-in word from inside a package
  is legal (a different wordlist), and one tail may live in several packages
  (`APP:RESET`, `MK:RESET`). A `does>` definer `MK` also publishes its clause
  as `MK;does` in its own wordlist, so a live `MK;does` there refuses `MK` at
  `does>` and a later `MK;does` is refused.
- A **public** definition whose tail a **private** word of the same package owns
  is the forwarder pattern (`: PREPARE ( ptr n -- ) PREPARE ;` publishing a
  private `PREPARE`): legal, but both effects must move the same number of
  CELLS, because the bare tail binds the private word and a definition's
  contract is read from that binding. A mismatch is refused where written,
  `E-SHADOWED-ARITY`, naming the package, the tail and both widths. The rule
  judges only a colon definition with a DECLARED signature; a public word made
  by a storage definer (`constant`, `variable`, `create`) is judged by its
  definer's row, so a private and a public `SHARED` constant in one package
  stay legal. Same cells through different types are accepted (`( ptr u8 n --
  n )` against `( n n -- n )`), and so is a public definition made BEFORE the
  private one: it binds its own name when its contract is read.
- `EXPORT NAME` inside an open package re-exports an EXISTING word into the
  current section under its own tail: same xt, same checked effect, defer and
  control flags and immediate bits carried, no forwarding body, zero runtime
  cost. `EXPORT EVAL:RUN` in `package MAKI public` publishes `MAKI:RUN`; a bare
  `EXPORT HELPER` in the public section promotes the private `HELPER`.
  Refused: an undefined source, a private word behind a CLOSED package, a
  source qualified into a sealed system package, a primitive and a duplicate
  tail in the target section (`E-EXPORT-UNDEFINED`). An exported `does>`
  definer `MK` brings its clause along as `MK;does`. Re-exporting a generated
  constructor under a second name is allowed; adding tails INTO a generated
  constructor package is not. At TOP LEVEL `EXPORT name…` is the hb-build
  `--repl` export directive: the build strips it and a plain load consumes the
  name as a no-op.
- **A process can be sealed to the packages a harness admits**:
  `src/lib/vocabulary.f` states the vocabulary and the refusals.

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
  the most recent `using`; one with none open is an error. At most 16 usings
  are open at once; a further one is rejected.
- Inside a package `;using` closes only a `using` the package opened; close one
  opened before `package` after `;package`. A `;using` inside the package that
  would close an outer one is refused by name (`ENGINE-ERROR:USING-OUTER`;
  `E-USING-OUTER` from `tools/check.f`).
- A load file is a using scope the same way: an included file or an
  `evaluate`d buffer closes only the usings it opens. A `;using` in it that
  would close one its includer opened is refused by name the same way.
- Lookup for a bare tail: open-package scope (private, then own public) FIRST,
  then the global wordlist, then each used public wordlist. The open-package
  scope silently wins over a used public. A tail in MORE THAN ONE used public
  wordlist is `E-USING-AMBIGUOUS`; the same package named twice is not
  ambiguous. `using` never silently changes an existing binding: it is the sole
  resolver of an otherwise-unresolved name, or a hard error.
- An ambiguous tail is refused at the reference site, naming each used package
  it resolves in; qualify the one meant or rename the collision. Under
  `tools/check.f --all-errors` or `--verify-only` the refused definition keeps
  its declared effect for later callers and the check goes on to report every
  later refusal; the same holds for the shadow below and `E-SHADOWED-ARITY`.
- A bare tail resolving to a GLOBAL while a used package ALSO exports it is
  `E-USING-SHADOW-GLOBAL` at the reference site, naming both candidates
  (`global TOK`, `PKG:TOK`) with arities. So a package whose public tails are
  ordinary verbs cannot be imported: `using TCP4` refuses at the first bare
  `READ`, `WRITE` or `CLOSE`. Qualify the package word (always certifies) or
  rename the collision. The checker enforces this in every checked body, and
  the interpreter at top level and for `'` (`ENGINE-ERROR:USING-SHADOW-GLOBAL`,
  a throw inside `evaluate`). Only the bodies nothing certifies, `TRUSTED:` and
  `0 set-check` definitions, keep global-first, as the explicit unchecked
  boundary.
- The colliding global need not be one the checker knows: an unsigned engine
  word or a `0 set-check` definition counts. Likewise a word in the open
  package's wordlist wins over a used public even when the checker never
  recorded it, and lacking a signature the reference is then `E-UNDEFINED`.
- Resolution happens at certify time: a call compiled inside a `using` scope
  keeps resolving after `;using`, and saved images carry no using-state. A
  checked body reading a used public certifies; a used private or an ambiguous
  tail is rejected before runtime; certification and execution name the same
  word. A global that appears AFTER a reference was certified does not change
  what it runs; the next reference to that tail is refused.
- `using` state is file-local: a `using` left open in an included file, or
  aborted by a throw, never leaks to the caller, and a package an included file
  leaves open keeps none of that file's usings.
- **A package word shadows the same-named global or primitive, and nothing
  reaches past it.** Inside `package APP` a bare `open` is `APP:OPEN`; in
  a checked body under `using DOC` a bare `close` is refused against
  `DOC:CLOSE`. There is no qualifier for the global "" wordlist: reach the
  operation through a differently named word (`OPEN-APPEND-FD`, the primitive's
  sibling `close-rc`) or rename the package word. Operator spellings too: once a
  package defines `@` or `+`, a bare `@` or `+` in its later bodies is the
  package word, whatever its operands; a body compiled before the definition
  keeps the engine word.

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

The grammar; every type, field and variant name is lowercase:

```text
structure-decl  = STRUCTURE type-header header-clause* field* ;STRUCTURE
enum-decl       = ENUM type-header (full-enum | compact-enum) ;ENUM
full-enum       = header-clause* variant-block+
compact-enum    = header-clause* compact-variant+
header-clause   = POLICY policy-name | DERIVE derive-name+ | OPAQUE
field           = FIELD field-name type-expr
variant-block   = VARIANT variant-name field* ;VARIANT
compact-variant = variant-name
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
unchanged-value transport uses the whole local.

An annotation is read by the SIGNATURE type grammar: family arguments (`{:
r:res<n,n> :}`), nested families (`{: o:opt<opt<n>> :}`), the definition's own
declared type variables (under `( opt<a> -- opt<a> )`, `{: o:opt<a> :} o`
certifies and keeps the quantifier) and the same arity check. The one spelling
only a local has is `{: p:ptr :}`: an annotation is a single token, so the bare
`ptr` means an INFERRED pointee, and `{: p:ptr n :}` is two locals, not a
pointee. The annotation is asserted, not decoration: a wrong family, a scalar
spelling (`{: p:n :}` is `E-MISMATCH`, expected: n actual: @pt.tag), a wrong
family argument and a bare tail of a family of arity > 0 are all refused. The
multi-cell type rationale is `docs/design.md` "Types".

- `NEWTYPE name` registers an opaque nominal cell family, with no
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
  (`docs/design.md` "Families and layout policies" gives the layout
  policies). `DERIVE eq`/`DERIVE hash` on a public arity-0 family
  generate those operations; compare an enum with its family's derived `EQ`,
  never raw `=`. A public `STRUCTURE`'s `MAKE`/`UNMAKE` let any package
  rebuild it with any field values; an owner that validates its fields
  declares `OPAQUE`.
- `POLICY` takes `stack-cell-tag` (the default) or `packed-tag`;
  `niche-null` and `boxed` are `E-TDECL-POLICY` ("layout policy not yet
  supported"). A payload naming the family being declared (inline, through
  `ptr` or bare) is `E-TDECL-RECURSIVE` under either accepted policy, and a
  payload naming a family not yet declared is `E-TDECL-PAYLOAD`, which also
  keeps mutual recursion out.
- **`construct family variant` builds a variant inside the owning package.**
  Its two operands are read before locals and dictionary lookup, so they never
  resolve as either, and fold case like other body tokens. The family must
  belong to the active package (top level owns the global one), public or
  private, and be a sum or enum; a `PKG:family` operand and any other
  package's family are refused (`hb: construct: unknown family:
  SHAPE:result`), so other packages use the generated words. The effect is the
  generated constructor's. At run time a constructor leaves its payload cells,
  pushes `M-p` zero cells (`M` the family's widest payload, `p` this
  variant's), then the tag.
- **`MATCH family` is the only eliminator.** Each variant is named in exactly
  one `OF … ENDOF` arm, which starts with that variant's payload on the stack;
  there is no default arm (`E-MATCH-NONEXHAUSTIVE`, `bad match: missing
  variants: blue`, with a `missing_variants` JSON field), and the outputs of
  the arms that continue must unify (`E-MATCH-BRANCH-JOIN`). The scrutinee's
  whole bundle must be on top of the data stack (`E-MATCH-SCRUTINEE`); a value
  whose family arguments are still open is `E-MATCH-OPEN-ARGS`; and `MATCH` in
  a `[: ;]` quotation is `E-MATCH-QUOTATION` ("quotation rows cannot carry a
  scrutinee"), because a quotation's row is inferred forward. The family
  resolves as in a signature (own package, then the unique public family,
  `PKG:tail` accepted): whatever is nameable can be matched, unlike
  `construct`. Every compiled `MATCH` ends in a die a well-typed value never
  reaches: a tag forged through a `TRUSTED:` constructor writes `hb: bad
  <family> tag` and exits (`ENGINE-ERROR:BAD-TAG`).
  This program prints 5, 4 and 3:

  ```forth
  package SHAPE
  public
  ENUM result<a,b>
     VARIANT ok  FIELD value a ;VARIANT
     VARIANT err FIELD error b ;VARIANT
  ;ENUM
  : CHECK ( n -- result<n,n> )
     dup 0< IF SHAPE-RESULT:err ELSE construct result ok THEN ;
  : >CODE ( result<n,n> -- n )
     MATCH result
        ok OF ENDOF
        err OF negate ENDOF
     ;MATCH ;
  ;package
  : SHOW ( n -- ) SHAPE:CHECK SHAPE:>CODE . ;
  : VIA ( -- SHAPE:result<n,n> ) 3 SHAPE-RESULT:ok ;
  : T ( -- ) VIA SHAPE:>CODE . ;
  5 SHOW -4 SHOW T
  ```
- The other match and construct codes: `E-MATCH-UNKNOWN-FAMILY`,
  `E-MATCH-FAMILY-KIND`, `E-MATCH-FAMILY-MISMATCH`, `E-MATCH-UNKNOWN-VARIANT`,
  `E-MATCH-DUPLICATE-VARIANT`, `E-MATCH-MISSING-OF`, `E-MATCH-STRAY`,
  `E-MATCH-UNTERMINATED`, `E-MATCH-DEPTH`, `E-CONSTRUCT-UNKNOWN-FAMILY`,
  `E-CONSTRUCT-FAMILY-KIND`, `E-CONSTRUCT-UNKNOWN-VARIANT`,
  `E-CONSTRUCT-UNTERMINATED`. Each carries a repair class.
- **A linear payload makes the sum linear**, transitively through nesting:
  `lopt<lq2<ltok,n>>` is one linear unit.
  Constructing consumes the payload once; copying or dropping the sum, or
  losing the payload (`LOPT:NONE` where it was live), rejects. `MATCH`
  consumes the sum and gives the arm the payload once, which the arm must
  consume: re-mint it or call its destructor.
- **A package family's generated words are `PKG-FAMILY:tail` with every hyphen
  in the package and family names doubled**: an `ENUM read-result` in `package TCP4`
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
  instance of THAT below another value is `E-NELAB-BUNDLE` until the argument is concrete; no width is guessed. A
  `create … does>` clause's rows follow the same rule: a clause yielding a
  multi-cell value compiles at tier 1 and the created word runs (`n
  SPAN-BUFFER: NAME`); only a clause row carrying an instance whose
  width reads an open argument is `E-NELAB-BUNDLE`, at the definer.
- `CAST: NAME ( source -- destination )` declares a checked retype: a reader
  keyword with no body and no `;`, publishing `NAME` as an identity whose call
  sites emit nothing. A conversion that can refuse is a checked word that
  throws, then the cast. The checker's rule, in refusal order:
  - Every family named is declared, visible and applied to its declared
    number of arguments (`E-CAST-FAM`): `( n -- box )` for a
    one-parameter `box` is refused by name, not by a later death.
  - Any other signature fault, bad syntax or a bare `ptr`, is refused as a
    definition with that signature is: the bad-stored-signature diagnostic. The first fault names the class, so
    `( ptr -- box )` is this reject, not `E-CAST-FAM`. An output scope or
    region variable no input supplies, when it is the only fault, is left to
    the scope rule: it is the shape of a view pack.
  - A scope or region variable, a quantifier-bound variable, a scope, or a
    read view, mutable view or loan field anywhere in either row would erase
    or introduce a scope dependency (`E-CAST-SCOPE`).
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
  - Each side is one term over the same stack tail (`E-CAST-ARITY`);
    a layout value wider than a cell is one term per cell. The emitted identity
    preserves the tail, so `( R n -- R u8 )` is valid and `( R n -- S u8 )`
    is refused.
  - A cast term is one machine cell: a con, a width-1 family, a pointer, or a
    quotation. An atom, or a pointer to one, on either side, and an atom in an
    introduction position of the destination, are `E-CAST-CLASS`.
  - A type variable or a linear type anywhere in either term, behind a
    pointer, in any quotation row or in a layout's members, is
    `E-CAST-LINEAR`: a `PRODUCT` field `ptr lease` or a `STRUCTURE`
    field `[ -- lease ]` carries the lease through the cast on either side.
  - A scalar-cell family, including a parametric `NEWTYPE` instance, in an
    introduction position belongs to its declaring package (`E-CAST-OWNER`): `CAST: >SLOT ( n -- slot )` outside `package DOC` is refused.
  - A pointer or a quotation in an introduction position is a class mint and
    is declared only in a package's private section (`E-CAST-MINT`).

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
  are phantom and introduce nothing. Projections out
  (`fam -- n`, `ptr t -- n`, `[ … ] -- n`, `box<ptr u8> -- n`, `pfbox -- n`)
  need neither the owner nor a private section: store the projected identity
  and resolve it back through the owner's public words.
- `LINEAR: NAME ( payload -- PKG:tok )` mints a `DEFLINEAR` token and
  `LINEAR: NAME ( PKG:tok -- payload )` erases one: the reader-keyword shape of
  `CAST:`, an identity at its call sites, and the one checked way to make a
  token from a payload or erase one back into it. It is parsed and refused as
  a cast is first (`E-CAST-FAM`, the bad-signature reject, `E-CAST-ARITY`),
  then by its own rule:
  - One side is a linear type and the other its payload, a non-linear con or a
    pointer chain ending at one (`E-LINEAR-PAYLOAD`): two tokens, no
    token, a type variable, a family, a quotation or an atom is refused.
  - The current package declared the linear type (`E-LINEAR-OWNER`).
    `DEFLINEAR` records the package it runs in, and one declared at top level
    has no owner, so nothing mints it.
  - The row has an unqualified name in that package's private section
    (`E-LINEAR-SCOPE`). A qualified name would publish into a public
    wordlist even while the declaring package is private.
  - The input and output stack tails agree (`E-CAST-ARITY`), because
    the emitted identity changes only its explicit payload or token cell.

  After `LINEAR: MINT ( ptr n -- PKG:tok )` and `LINEAR: ERASE ( PKG:tok --
  ptr n )`, the owner's checked words compose the two, `: PEEK ( PKG:tok --
  PKG:tok n ) ERASE dup @ swap MINT swap ;`, and its public words hand the
  token out and take it back; a caller holds it under the linear rules.
  `CAST:` still refuses a linear term, in the owner too (`E-CAST-LINEAR`).
- Type, field and variant names are lowercase; generated and project words are
  uppercase.
- **Raw storage holds neither an address nor an execution token**: a
  `variable`, `create` or `constant` cell refuses both (`E-RAW-CELL-PTR`); see
  **Raw storage never holds an address** and **An undeclared cell holds no
  execution token** for the declared forms.
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
  refused with `hb: does> has no created word` (a catchable throw inside
  `evaluate`):

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
  that word.
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
  validation, update and rendering words, then one orchestration word. `ROT`
  and `-ROT` chains call for factoring or locals. `PICK`, `ROLL`, `?DUP` and
  `WITHIN` do not exist (`E-UNDEFINED`): name the value with a local.
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
  either before storing anything; the checker reports
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
- **Data never spans a load.** The loader aligns `here` to a cell when a loaded
  file starts and when it ends, so a file's data bytes depend only on its own
  source and its stored object installs whatever earlier files allotted. A live
  task freezes data, so the loader does not align then.
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
- **Declare application nominals with `DEFTYPE`, owner and lifetime tokens
  with `DEFLINEAR`.** Spelling, scope, converters and the linear rules are in
  **Declaring your own scalar types** below.
- **Raw role casts are not validators.** `>LEN`, `>IDX`, `>COUNT`, `>OFF`,
  `>ASM`, `>IMG`, `>SNAP` are trusted identity boundaries; libraries expose
  checked constructors and role helpers so swaps fail under `CHECK!`.
- Unchecked prose-only comments may name roles when no hook consumes them; keep
  the shape obvious. Add inline `( … )` at non-obvious points in a longer word.
  Standard notation: `x` cell, `n`/`u` signed/unsigned, `d` double, `c-addr u`
  string, `xt`, `nt`, `f`/`bool` flag, `?` maybe-present.

## Stack effects

A checked definition declares its effect in the `( … )` immediately after the
name: `: SQUARE ( i64 -- i64 ) DUP * ;`. The checker infers the body's effect
and unifies it with the declaration; a mismatch refuses the definition.

### Grammar

```
sig      = side '--' side
side     = stack ( '|' stack )?
stack    = rowvar? type*
type     = conname | role | linear-name | family | tyvar
         | 'ptr' type
         | '[' quot-sig ']'
quot-sig = stack '--' stack ( '|' stack '--' stack )?
family   = ( package ':' )? tail ( '<' type ( ',' type )* '>' )?
conname  = n f r i64 u8 u16 u32 cell char bool addr str f32
role     = idx len count off fd rc pid ms ns tok reg label va symidx asm img snap
linear-name = the exact token given to `DEFLINEAR`
tyvar    = a..z, except the reserved `n`, `f`, `r`
rowvar   = A..Z          (same letter → same row var; leading position only)
```

Some primitive and combinator effects:

```
DUP    ( R a -- R a a )
+      ( R n n -- R n )            \ also ( R ptr a n -- R ptr a ) and ( R n ptr a -- R ptr a )
<      ( R n n -- R bool )         \ comparisons yield bool
@      ( R ptr a -- R a )
EXECUTE( R [ R -- S ] -- S )
DIP    ( R a [ R -- S ] -- S a )
KEEP   ( R a [ R a -- S ] -- S a )
BI     ( R a [ R a -- R b ] [ R b a -- R b c ] -- R b c )
TIMES  ( R i64 [ R -- R ] -- R )
EACH   ( R ptr a i64 [ R a -- R ] -- R )
FOLD   ( R ptr a i64 b [ R b a -- R b ] -- R b )
```

The arithmetic and comparison axioms are declared over `n`, so a diagnostic
prints `n`. `>R`/`R>` have no row: the checker models the return-stack transfer
inside one definition. `WITHIN`, `?DUP`, `PICK`, `ROLL` and `EFFECT-OF` are not
defined (`E-UNDEFINED`). Write the shuffle with `DUP`/`OVER`/`SWAP`/`ROT` or locals, and branch
on an explicit comparison instead of `?DUP`.

### The return-stack clause

A definition reads only the return-stack cells it pushed with `>r` and leaves
the return stack as it found it, so no signature declares a return-stack cell.
A `|` clause may name one row variable, the same on both sides: the word leaves
its caller's return stack alone, which is what a signature with no bar says.
Any other clause is refused at the declaration, at both tiers, before the body
is checked: `habu: in NAME: return-stack effect in signature '<sig>'; after '|'
write one row variable, the same on both sides, or no '|'` (`E-BAD-SIGNATURE`,
repair class `fix_return_stack`). A stored signature — a `TRUSTED:`
definition's, a `defer`'s, a `trust` row's or a `CAST:` — is refused as
`E-BAD-STORED-SIGNATURE` with the same repair class. To keep a value across
other work, park it with `>r` and take it back with `r>` in the same
definition.

The clause is written in two places, and they are not the same shape.

**Top level — the bar goes inside each side.** One side is
`data-stack ( '|' return-stack )?`, and the top-level `--` separates the two
sides:

```
( Din | U -- Dout | U )
( R n | U -- S n | U )          \ certifies: neutral
( n | -- | n )                  \ refused: a cell on the output side
( | n -- n | )                  \ refused: a cell on the input side
```

A bar on one side only, `( R n | S -- R )`, pairs the written row with a fresh
one for the missing end and is refused like any other clause that moves the
return stack.

**Quotations — the bar separates two full `in -- out` pairs.** Inside `[ … ]`
the type is `in -- out`, then optionally `| rin -- rout`, under the same rule:

```
[ in -- out ]                   \ return-stack neutral
[ in -- out | U -- U ]          \ the same, spelled out
( R n [ R n -- S n | U -- U ] | U -- S n | U )    \ certifies
( n [ n -- | U -- U n ] | U -- n | U )            \ refused
```

**Do not write the quotation shape at the top level.** `( R a -- R | S -- S a )`
reads `Din = R a`, `--`, `Dout = R`, `Rout = S`, and nothing may follow the
output side, so the trailing `-- S a` is a syntax reject at that token
(`fix_signature_syntax`). The top-level shape inside a quotation,
`[ R a | S -- R | S a ]`, is the same hard reject, at the `|` in a definition
and as `E-BAD-STORED-SIGNATURE` in a stored signature.

### Rows, variables, and lexing

- A **row var** (`R`, `S`, …) at the front of a stack stands for the rest of
  the stack below. It is recognised only in leading position; an upper-case
  token anywhere else is looked up as a type name and rejects with `unknown
  type 'R' in signature`. Row vars and type vars are separate namespaces (case
  decides), both scoped to one signature. Stacks with no leading row var share
  one implicit data row and one implicit return row.
- The implicit data row is sealed for the body: callees may preserve it but
  may not bind it by consuming below the declared inputs. That call is refused
  at its own token with the cells it needed and the cells the declaration left
  (`E-INPUT-UNDERFLOW`, `supply_missing_input`); a call that is short *and*
  mistyped reports the type mismatch instead.
- The implicit return row is sealed the same way, with or without a signature:
  `r>`, `r@`, `2r>` and `2r@` reach only cells this definition pushed, even
  when a later `>r` restores the balance, so `: X r@ ;` and `: X r> ;` are
  refused (`fix_return_stack`) at both tiers. A quotation literal's return row
  is sealed at its `;]`: `[: >r ;] execute r>` and `>r [: r> ;] execute` are
  refused there.
- A **type var** (`a`, `b`, …) is a fresh polymorphic type; one letter in one
  signature is one type. `n`, `f` and `r` are concrete types, not type vars.
- Type, role and family names are lower case and case-sensitive: `I64` and
  `IDX` reject as bad signatures.
- `<`, `>` and `,` are single-token delimiters, so `pkg:result<pkg:index>` and
  `pkg:result < pkg:index >` parse identically. Do not nest `( )`: the inner
  `)` closes the comment.

### Types

- `n` is the unconstrained integer, interchangeable with every integer type in
  both directions; `cell` is the machine word; `i64`, `u8`, `u16`, `u32`,
  `char` are sized integers; `addr` is a raw address in a sign class of its own,
  used only for a value checked code never dereferences.
- `bool` is a flag; comparisons return it. `f` is an alias for `bool`, not a
  type variable, and renders as `bool`.
- `r` is a 64-bit float; `f32` is a 32-bit float and does not unify or widen
  with `r`.
- `str` is declarable, but no primitive produces or consumes it; a string is
  `ptr u8 n`.
- The roles are nominal one-cell types, distinct from each other and
  from every integer type.
- `ptr τ` is a typed pointer; `@`/`!` move `τ`. `[ in -- out ]` is a quotation
  or execution token carrying its effect (prose writes `xt<E>`; the parser does
  not). `pkg:tail<arg,…>` is a declared family (`NEWTYPE`, `ENUM`, `STRUCTURE`,
  `SUMTYPE`, `PRODUCT`, `LAYOUT-BUFFER`, `DEFTYPE`); a `DEFLINEAR` token is a
  linear cell type.

An unknown type token is never interned: it rejects with `unknown type 'tok' in
signature` (`E-UNKNOWN-SIGNATURE-TYPE`). The other signature codes are
`E-BARE-PTR-SIGNATURE` (`ptr` with no element), `E-WRONG-ARITY` (a family with
the wrong argument count) and `E-BAD-SIGNATURE` (a missing or misplaced `--`,
`|` or `]`).

### Integer widening

Widening applies only to integer-class types, only
at the top level of a stack cell, and only in an input or coercion position —
never inside a `ptr` pointee, never between roles, never between `bool` and an
integer (`bool>n` converts a `bool` to -1 or 0), never between `r` and `f32`.

`n` unifies with every integer type unconditionally. For the others, `got`
flows into `want` when `width(got) <= width(want)` and one of: either sign
class is generic (`cell`); the sign classes are equal; or `got` is unsigned,
`want` is signed and `got` is strictly narrower. `addr` has its own sign class,
so `n` and `cell` reach it but `u8`, `u16`, `u32`, `char` and `i64` do not, and
an address cannot drift into `i64`. `Y` = the row type is accepted where the
column type is declared:

```
got \ want   n  i64  u8  u16  u32  cell  char  addr
n            Y   Y   Y    Y    Y    Y     Y     Y
i64          Y   Y   .    .    .    Y     .     .
u8           Y   Y   Y    Y    Y    Y     Y     .
u16          Y   Y   .    Y    Y    Y     .     .
u32          Y   Y   .    .    Y    Y     .     .
cell         Y   Y   .    .    .    Y     .     Y
char         Y   Y   Y    Y    Y    Y     Y     .
addr         Y   .   .    .    .    Y     .     Y
```

Nothing else widens: every role, family, linear type, `str`, `r` and `f32`
unifies only with itself. Roles keep same-representation values apart (indexes
and lengths, descriptors and return codes, milliseconds and nanoseconds,
registers and labels, image-build phases). Enter and leave a role only through
an explicit converter or an audited boundary effect; unchecked native emitters
still publish roles in their `TRUST` effects so checked callers reject swaps.

### Declaring your own scalar types

`DEFTYPE` and `DEFLINEAR` differ in substrate, scope, spelling and diagnostics.

**`DEFTYPE NAME` — a package-scoped value nominal.** `src/lib/type/deftype.f`
must be required first. The type is a package-scoped arity-0
family, the substrate of `NEWTYPE`:

- The signature spells the lower-case fold: `DEFTYPE FRAME-IDX` is `frame-idx`
  inside the package and `PKG:frame-idx` outside it. `DEFTYPE SERIAL` in
  `CAMERA` and in `FRAME` are unrelated types.
- A mismatch names the type as source spells it: `: F ( n -- frame-idx ) ;`
  rejects with `expected: frame-idx actual: n`.
- It derives the converters `>NAME ( n -- name )` and `NAME>N ( name -- n )`,
  no-op casts and the only crossing. They follow package visibility: declared
  without `public`, `PKG:>NAME` is `E-UNDEFINED` outside.
- The name cannot be a built-in type, a role, an atom prefix, a one-letter type
  variable or `ptr`: `DEFTYPE IDX`, `DEFTYPE A` and `DEFTYPE PTR` reject with
  `bad newtype declaration '…': reserved name`. A family the
  scope already has is `duplicate family`; a package may shadow a global
  or another package's family. `tools/check.f` reports each refusal as
  `E-BAD-NOMINAL-TYPE`.

**`DEFLINEAR name` — a global linear cell type.** It is top-level
only and rejected inside a checked body.

- The signature spells the token exactly as declared, case-sensitively;
  declare it lower case, or as `PKG:tail` (`DEFLINEAR LIN-OWN:tok`). A mismatch
  renders the bare name.
- It is global. A second declaration, a name already in the type table, or a
  name a signature reads as something else is `checker: bad or duplicate
  signature type`: a one-letter type or row variable; `--`, `|`,
  `[`, `]` or `field`; a `VALUE-RECORD` name; a name with an atom prefix
  (`space-`, `extent-`, `mask-`, `block-`, `geom-`, `parity-`, `align-`,
  `fresh-region-`, `fresh-extent-`, `fresh-gen-`, `fresh-mask-`); a family tail
  any package declares; and a token holding `<`, `>`, `,`, `)`, `(`, `"` or
  `\` (source names the type again, where those bytes open a comment, string
  or line comment for `tools/check.f`). `CELL` is legal beside `cell`.
- No converters are derived. `DEFLINEAR` records the package it runs in; only
  that package's private `LINEAR:` rows mint and erase the token
  (**Structures And Enums**). One declared at top level has no owner and no
  mint.

A linear type is used exactly once. The checker enforces **conservation**: at
every step whose declared effect does not itself name a linear type, the number
of live linear values on the combined data and return stacks may not change.
So `dup`, `over`, `tuck`, `2dup`, `drop`, `nip`, `@`, `!`, `c@`, `c!` and a
by-value record copy reject on a linear, through control flow (both `IF` arms
and stack-neutral loop bodies agree) and through quotation application
(`[: dup ;] execute`). A move — `( own -- own )`, `swap`, `rot`, `>r`/`r>` — is
fine. A word that acquires a frame and forgets to release it fails its output
balance; releasing twice underflows.

Conservation is backed by a kind discipline: a type variable that binds a linear con must occur
equally on the input and output sides of the whole applied effect, quotation
rows included, so `[: FREE ;] KEEP` and `BI`/`TRI` fan-outs reject; and a
variable copied or dropped while still polymorphic is tainted, so
`[: dup FREE FREE ;] execute` rejects once `FREE` binds it to the linear.
Non-linear polymorphic code is unaffected. A linear value cannot be bound to a
local (`E-LINEAR-LOCAL`); keep it on the stack and factor the consumer.

`VALUE-RECORD name field type … END-VALUE-RECORD` is the legacy by-value
record: its token expands to touchable `field<rec,name,t>` cells that ordinary
primitives destructure, so `( n n -- point )` certifies with an empty body.
New records use `STRUCTURE`.

### Pointer types and arithmetic

`ptr τ` records the element addressed. Arithmetic is pointee-polymorphic: `+`,
`-`, `1+`, `1-`, `cell+` and `char+` step a `ptr τ` by an integer and keep `τ`;
`n + ptr a` is `ptr a`; `ptr a - ptr a` is the distance `n`. Adding two
pointers or scaling one is refused.

A **byte view** re-reads a pointer as bytes: `: NAME ( ptr a -- ptr u8 ) ;`
certifies, because the input pointee stays generalized. A `ptr u8` uses
`c@`/`c!`; cell `@`/`!` on it is refused.

A pointee is **invariant**: widening never applies inside a `ptr`, so
`ptr u8` never satisfies `ptr cell` or `ptr u32`, at any depth. A byte pointer
cannot be laundered into a cell pointer and cell-loaded.

#### Raw storage never holds an address

Strict pointee unification fences only a pointer that stays a pointer; an
integer could become one by passing through memory, so an undeclared cell is
fenced too.

`variable`, `create`, `constant`, `here` and every `create … does>` definer
publish **raw storage**: `-- ptr a` whose pointee is sealed RAW and
re-freshened at every mention. A RAW cell admits a scalar, a role and an atom;
it refuses a pointer in either direction (`E-RAW-CELL-PTR`,
`declare_pointer_cell`), a nominal family or layout value, and an execution
token (below). `variable V : PEEK ( n -- n ) V ! V @ @ ;` is refused at the
fetch, `V ! V @ !` at the store, `$1234 constant K : F ( -- n ) K @ ;` with no
store in sight, and `: G ( ptr a -- ) V ! ;`. `create BUF 256 allot BUF 4 type`
certifies: the pointee binds `u8`.

`ptr-field`'s row `( ptr a n -- ptr ptr b )` cannot carry the refusal, so its
base is checked first: a base whose pointee is still RAW is refused at the
`ptr-field` token (`V ! V 0 ptr-field @ @`).

The seal is applied where the cell is published, so it holds on every load
path: `: N>ID ( n -- CAD-KIND:region ) V ! V @ ;` rejects with
`expected: cad-kind:region actual: a`.

**A cell that holds an address is declared.** `PTR-VARIABLE`,
`PERSISTED-PTR-VARIABLE`, `TYPED-VARIABLE NAME ptr t`, `TYPED-BUFFER NAME ptr
t`, and the `create … does>` definers of `src/core/pointer-storage.f` that spell
their pointee. A declared cell takes the address it was declared for and
refuses an address of one: `PTR-VARIABLE X : F ( -- ptr u8 ) X @ ;` certifies
while `( -- ptr ptr u8 )` does not.

Records that mix a pointer field with scalars use `STRUCTURE … DERIVE addr`
(**Structures And Enums**). Two launders remain open and certify today:
`ptr-field`'s free result pointee reached through a parameter
(`: F ( ptr a -- ptr ptr u8 ) 0 ptr-field ;` over a `create`d cell, or
`NULL-PTR swap ptr-field @ @`), and a scalar store through a view of a
declared pointer cell (`V BYTE-VIEW CELL-VIEW !`). The reach is not closed
either: `ptr a n + -- ptr a` admits any offset, so a bounded reach needs a span
(**Spans: a pointer that carries its reach**).

#### An undeclared cell holds no execution token

Admissibility is about provenance, not what the store instruction carries. A
quotation met by a RAW cell or by either base address is refused:
`E-RAW-CELL-PTR`, reason "an undeclared cell cannot hold an execution token / a
quotation", repair class `declare_xt_cell`. It is judged at the accessor's own
declaration, so `variable ZQW : ZQWQ ( -- ptr [ -- n ] ) ZQW ;` dies before any
`execute`. The store
`[: 1 ;] V !` is refused at the same binding.

**A cell that holds code is declared**: `TYPED-VARIABLE NAME [ in -- out ]`, a
`TYPED-BUFFER` or `DYNAMIC-BUFFER` of `[ in -- out ]`, and `defer`/`is`. Each
mints the quotation type at the storage word: `HK @` recovers `xt<E>`, a store
`[: W ;] HK !` or `['] W HK !` (and `['] W idx HKB !` for a buffer slot)
fit-checks `W`'s effect against `E`, and `HK @ execute` fit-checks the row. A
tick whose sink is a scalar (`['] W +`) is refused.

**`xt!` is the sanctioned mint** for a persisted cell whose address is computed
at run time from a table base and a row index (`' W DECLARATIONS <off> + xt!`):
the window opens on the `xt!` token and closes with it. An accessor that hands
out a code cell at a fixed offset declares the retype once as a
package-private `CAST:`; a borrower stores through the owner's certified store.

A declared code cell may not be compared with or subtracted from the null
(`HK NULL-PTR =` is refused); read its emptiness through a number-typed
accessor of the same address.

**Executing an xt of unknown provenance is refused (`E-EXEC-OPAQUE-XT`,
`fix_opaque_execute`).** An xt fetched from an untyped cell is a bare type
variable whose effect was erased at the store, and `'` of a primitive or of a
word with no stack effect pushes an xt of no known effect, so `execute` and
`catch` of either reject and point at the typed routes: a quotation parameter,
a `defer` bound with `is`, or a typed code cell.

#### A base address addresses no declared element

`data-base` (the start of the task's DATA region) and `NULL-PTR` are not the
address of anything. Both are fenced, and both refuse with
`E-RAW-CELL-PTR`, `declare_pointer_cell`, each with its own reason.

- **The null** is fenced in value position and permissive inside a pointee:
  a value read through the null is never a nominal or a pointer, while
  `ptr thing NULL-PTR =`, `NULL-PTR X !` into a declared pointer cell (also
  `ptr ptr n`) and `NULL-PTR 0=` certify.
- **The DATA base** is fenced at every pointee depth: binding its pointee, at
  any depth, to a nominal, a pointer or a linear con is refused
  (`( -- ptr ptr ptr n ) data-base 8 +`).
- **An offset null is a DATA-base pointer.** A null stepped by an integer
  (`+`, `-` with `n`, `1+`, `1-`, `cell+`, `char+`) is fenced as the DATA base:
  `: F ( -- ptr ptr n ) NULL-PTR OFF + ;` is refused. Stepping any other
  pointer changes nothing, so `( ptr thing -- thing ) 8 + @` still certifies.
- **A distance binds no pointee.** `X NULL-PTR -` and `X data-base -` match the
  base's pointee alone, so `: D ( ptr a -- n ) NULL-PTR - ;`
  certifies; so does `V BYTE-VIEW NULL-PTR BYTE-VIEW -`.

`data-base OFF + @` still answers the number, xt or role the cell holds; a DATA
cell that holds an address is reached with `ptr-field`; `data-base BYTE-VIEW`
erases the fence.

**The wrapper is refused where it is written.** A declared quantifier the body
restricts to a base address is `E-NONPARAMETRIC-EFFECT` ("restricted to a base
address -- the DATA region, or a pointer computed off NULL-PTR", or "restricted
to the null address"): `: W ( -- ptr a ) data-base 8 + ;` would let its
caller choose the element type of engine DATA. Name the pointee the
cell holds (`ptr n`, `ptr u8`). A quantifier restricted through an input-only
position is excused for the null only (`: RELEASE ( ptr ptr a -- ) … NULL-PTR
cb ! ;`); one named on both sides is still published (`: LEAK ( ptr a -- ptr a )
drop data-base 8 + ;` is refused). The DATA base has no such excuse:
`: STASH ( ptr ptr a -- ) data-base 8 + swap ! ;` and `: FEED ( [ ptr a -- ] --
) data-base 8 + swap execute ;` are refused. To read one DATA offset at two
types, write two accessors, each naming what it reaches.

#### Reading code bytes

A code address reaches checked code as a number. Reading the bytes there goes
through `CODE-BYTES:AT ( n n -- ptr u8 n )` alone, which refuses a span not
wholly inside the image's code (`hb: span outside the code region`). A site that
only carries a code address keeps it a number.

### Declared polymorphic effects stay parametric

A quantifier in a declared effect promises the word works for every type
there. After the body checks, the checker rejects two
shapes with `E-NONPARAMETRIC-EFFECT` (`fix_parametric_effect`):

1. **Specialization to a sealed family.** A body that forces a quantifier to an
   arity-0 nominal or a layout family is refused, naming it: `: F ( a -- a )
   EFF-ID ;` with `EFF-ID ( region -- region )`. Scalar widening (`a := n`) stays
   legal, so `( ptr a -- n ) @` is untouched.
2. **Quantifier aliasing.** A body that unifies two declared quantifiers is
   refused, naming both: `: F ( a b -- a ) MERGE ;` with `MERGE ( g g -- g )`.

The pass makes no bindings, so a rejected signature never persists.

### Exceptional control flow

The signature describes the normal effect. The checker also tracks a separate
exceptional edge for catchable `throw` paths:

- `die` is process no-return; a wrapper whose every path reaches `die` is
  recorded as non-returning.
- `throw` consumes the code, records an exceptional edge and kills only the
  current normal path. A word may have a normal effect and throw paths; a word
  whose every path throws kills its caller's normal path, and the edge stays
  catchable.
- `catch` consumes a quotation that preserves both stacks, then pushes a throw
  code (`0` on normal completion). Its exceptional path restores the depth of
  both stacks, not their contents, so a cell of the quotation's declared input
  window that a throw path may have overwritten comes back as `stale<t>`: it may
  be moved, dropped or bound to an untyped local; reading it is `E-STALE-READ`
  (`keep_value_before_catch`). A cell every throw path left untouched keeps its
  type (**Rules learned by refusal**).

A guard may therefore throw directly, with no dummy value for the join:

```forth
: REQUIRE-NONEMPTY ( len -- ) LEN>N 0 <= if E-A-EMPTY throw then ;
: HEAD ( ptr i64 len -- i64 ) REQUIRE-NONEMPTY @ ;
```

`<=` is declared over `n` and `len` is a role, so the guard crosses through
`LEN>N`; `dup 0 <=` rejects with `expected: a n n actual: len len n`. Tokens
after a terminated path (`throw`, `die`, `exit`, `leave`, an unconditional back
edge) are refused unless they are structural closers (`else`, `then`, `loop`,
`+loop`, `repeat`, `again`, `;]`).

### Escape hatches

- `TRUSTED: NAME ( eff ) body ;` records `eff` without checking the body, then
  compiles it. `s" name" s" eff" TRUST` charts an already-defined word's effect
  so the checker can use it as a leaf. A body that reaches a word the checker
  cannot model (`evaluate` and other metaprogramming) is `E-UNCHECKABLE`;
  checked build paths treat that as a refusal unless the call sits behind a
  named, tested `TRUSTED:` boundary.
- Roles are entered and left through no-op conversions: `>IDX IDX>N`, `>LEN
  LEN>N`, `>COUNT COUNT>N`, `>OFF OFF>N`, `>FD FD>N`, `>RC RC>N`, `>PID PID>N`,
  `>MS MS>N`, `>NS NS>N`, `>TOK TOK>N`, `>REG REG>N`, `>LABEL LABEL>N`, `>VA
  VA>N`, `>SYMIDX SYMIDX>N` (`src/core/roles.f`). Library boundaries prefer
  checked refinement constructors that reject negative, overflowing or
  out-of-capacity input: `A-LEN`, `A-IDX`, `A-COUNT`, `STR-LEN`, `STR-OFF`,
  `STR-COUNT`, `M-LEN`, `M-OFF`.
- Phase roles `asm`, `img`, `snap` are ordering cells whose payload is
  ignored: each phase consumes the role the previous one produced, so the
  checker refuses a build step run out of order.
- A trusted defining word is `TRUSTED: NAME ( definer-eff ) create … does> (
  created-eff ) body ;`; the created effect must follow `does>` immediately.
  For a checked `CREATE … DOES>` with created effect `( in -- out )`, the
  `DOES>` body is checked as `( in ptr a -- out )`; step through the data field
  with `cell+`, not raw `+`.
- `0 set-check` disables the check hook for the rest of the load, so `:` is the
  plain native colon; `check@ 0 set-check … set-check` resumes the saved hook;
  `LOWER-CERT-HOOK:INSTALL` (`src/core/check-hook.f`) installs the checker
  afresh. There is no `CHECKING-ON?`.

### Rigid host-allocation identity domains

`ptr T` and ordinary type vars cannot tell two equal-sized allocations, or a
recreated owner, apart, so a stale index could regain authority over reused
storage. Three checker domains name these rigidly:

- **host region**, which allocation: an output spelled `fresh-region-*` mints a
  fresh region per call, shared across that call's outputs; an inferred effect
  records its body's identities as template slots, so callers mint per call.
- **extent**, the bounds identity: `fresh-extent-*`.
- **mutation generation**: `fresh-gen-*`. A mutation that invalidates
  outstanding indices yields a new generation, so an old index no longer
  matches.

Each domain has a private per-check counter that never reuses an id and throws
`E-RIGID-EXHAUST` at its limit rather than wrapping. Ids are qualified by
domain, so equal numbers in two domains do not unify. Mismatches
are distinct: `E-RIGID-REGION-MISMATCH` (`fix_host_region`),
`E-RIGID-EXTENT-MISMATCH` (`fix_host_extent`), `E-RIGID-STALE-GENERATION`
(`fix_stale_generation`), `E-RIGID-DOMAIN-CONFUSION` (`fix_rigid_domain`). No
runtime counter or address becomes persistent authority.

### Checked assertions

Assertions run inside a checked test word; consume and compare the top result
first, and the word's effect ensures every result is used:

```forth
require src/lib/test.f

package PAIR-TEST
: PAIR ( -- n n ) 3 4 ;
: RUN ( -- )
   T-RESET
   PAIR 4 T= 3 T=
   T-REPORT ;
RUN
;package
```

### Replay versus reconstruction

A live compile sees the records that exist at that moment, so a package word
declared later cannot shadow the global a body named. A verifier that rereads
recorded source in an engine already holding it (the build's certify step,
`--all-errors`) binds each body token the same way: to the newest record before
the definition, package first, used publics included, so a used public exported
later is neither a candidate nor a shadow. One limit: an `undefine`d spelling
defined again binds its earlier text against every later record too.

## Checker & type model

- **C2 views carry checked lifetimes and access authority.** **Owners and
  views** below defines shared `read-view<p,q,T>` and
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
  linear operand, behind a pointer or in a quotation row too
  (`E-CAST-LINEAR`). Without a lifetime to prove, a public `STRUCTURE` plus a
  `TYPED-VARIABLE` or `TYPED-BUFFER` in the caller trades it for a runtime
  refusal off the definer's zero image.
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
  after cleanup, a cleanup error supersedes it; `die` skips cleanup. A
  `TASK:HALT` observed at `TASK:PAUSE` unwinds through `finally` the same way; a
  `catch` between the `PAUSE` and the task's root never resumes. A halt observed
  inside a cleanup cuts that cleanup short and keeps the body's error. `JOIN`
  then answers a cleanup's error, else the body's error in flight, else ok with
  a result the body stored before the halt, else `E-TASK-NO-RESULT`.
  `TASK:HALT-UNWIND` ( n -- ) serves a pending halt where `PAUSE` would,
  carrying n as the task's error, so a scope that ends under a halt keeps its
  own error. Implicit
  tails such as `[ -- ]` enforce their windows through wrappers and typed
  storage; name rows explicitly for generic callbacks (`[ R -- S ]`).
  A zero-initialized typed quotation may be fetched or dropped. Calling it
  through `execute`, `catch`, `finally`, or `run-in-stack` exits with
  `hb: unset quotation` on stderr; `catch` cannot recover that fatal error.
- **Higher-order signatures publish themselves** once `CHECK!` passes (`DIP`,
  `KEEP`, row callbacks); no TRUST row to pin a scheme.
- **Function passing is checked.** A quotation parameter (`[ a a -- bool ]`, `[
  a -- a ]`) is verified through a call chain AND inside a `?do`/`begin` loop:
  bind it as a local, thread it, `execute` it; heapsort, map, fold and filter
  all check.
- **Execution vectors are typed `defer` words.** `defer ACTION ( in -- out )`
  declares the effect; `: INIT ( -- ) [: IMPL ;] is ACTION ;` installs it, the
  checker proving the quotation matches exactly. No `variable`/`@ execute`
  tables, no `['] IMPL is ACTION`. An unset deferred word fails closed with the
  execution-vector error. A fixed engine callback cell stores one checked bridge
  (`[: ACTION ;] CELL !`) and changes only through `[: IMPL ;] is ACTION`.
  `@EXECUTE` is no replacement until its zero no-op has a checked model.
- **Phase tokens reach the side effect they order**: `asm`, `img`, `snap` flow
  through the final sign/write/header operation, not an early wrapper.
- **Seal the implicit row under declared inputs**: a stack-preserving trusted
  effect (`img -- img`, `fd -- fd`) must not satisfy output by binding an
  implicit base row that hides underflow.

## Owners and views

A C2 view is a checked borrow: `read-view<p,q,T>` is shared read access and
`mut-view<p,q,a,T>` exclusive access to elements of type `T`, where `p` is the
owner scope, `q` the loan ceiling (a root view has `q = p`) and `a` the
allocation provenance. Bounds, scope and authority are part of the type, not a
runtime address heuristic; an aggregate carries the union of its fields' scope
dependencies, nested generic and sum payloads included.

### The public owner recipe

Load `src/lib/c2-owner.f` and `src/lib/c2-bytes.f`. `C2-MEM:OWNER-SIZE` is the
byte length of an owner header; `C2-MEM:WITH-MUT` owns those bytes for one
callback. Inside it, pass `C2-MEM:SEED-OWNER` to `C2-MEM:WITH-INIT`; its
callback receives a unique `owner-state` view, `C2-MEM:BIND` turns that view
into the linear owner handle, and `C2-MEM:UNBIND` returns it before the
callback ends. This certifies and runs:

```forth
require src/lib/c2-owner.f
require src/lib/c2-bytes.f
require src/lib/memory.f

package C2-EXAMPLE
private
: ADD ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> )
   16 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC
   s" hi" C2-BYTES:COPY$ drop
   C2-MEM:PUBLISH drop ;
: OWNED ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND ADD C2-MEM:UNBIND ;
: ROOT ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: OWNED ;] C2-MEM:WITH-INIT ;
public
: RUN ( -- )
   C2-MEM:OWNER-SIZE [: ROOT ;] C2-MEM:WITH-MUT ;
;package
```

- `C2-MEM:ALLOC` appends zeroed bytes to the **named** owner, even while an
  inner owner is active, so a later allocation typed for `p` stays owned by `p`
  after the inner scope ends.
- `C2-MEM:ALLOC-DISPOSE` ties an allocation to a foreign acquisition: its
  checked `[ mut-view<p,p,a,u8> -- ]` disposer consumes the unique view and
  completes the foreign release before reporting an error. The owner runs it
  once at close; a failed allocation runs none.
- `C2-BYTES` gives bounded copy, slice and length operations on live byte
  views. `XML-C2:WITH-READER` (`src/lib/xml/c2.f`) takes a shared source view
  and unique cursor storage; `XML-C2:NAME$` keeps the source lifetime after the
  cursor callback ends.
- At most 32 owner or initialization frames are live in one task: the 33rd
  open throws `E-C2-CAPACITY` before acquisition, and capacity returns
  as frames close. Capturing an image while an owner or loan is live throws
  `E-C2-CAPTURE`. These are runtime boundaries; escape is refused by
  the checker.

### View and scope rules

A scoped owner combinator gives its callback a fresh rigid scope. Ordinary
calls may return views that depend on still-active input scopes; the
combinator's own result may hold only values independent of the scope it
closes. Independently owned scalar or document results may escape.

| Operation | Rule |
| --- | --- |
| Copy shared read view | Allowed while the owner scope is active. |
| Copy exclusive view | Refused; a move or a bounded reborrow transfers access. |
| Read through shared view | Allowed within bounds; writes are refused. |
| Reborrow exclusive view | The parent is suspended for the lexical child loan and restored after it; no parent use or overlapping sibling loan meanwhile. |
| Shared loan from exclusive access | Mutation is suspended for the whole shared-loan scope, copies included. |
| Return a view from an ordinary callee | Allowed; the effect keeps the scope dependency. |
| Export a view as its owner combinator's result | Refused, through nested fields, generic wrappers and quotation payloads; handing a control handle back for cleanup is not export. |
| Store into a typed scoped record | Allowed only if the value cannot outlive the borrowed owner; the record keeps the field's dependency. |
| Store into globals, task slots or raw cells | Refused for a scoped value. |
| Install into defer/XT state, enqueue or spawn a task | Refused if it carries a scoped dependency out of its lexical scope. |
| Close a scoped owner explicitly | Refused; only leaving its combinator ends the scope, after dependent values and loans end. |
| End an exclusive loan or initialized reader scope | Ends its child loans, consumes its authority and invalidates its fields; an enclosing source owner stays open. |

A unique view is consumed by a scope end or an end-loan operation; it is never
duplicated or turned into a raw shared pointer. Exclusive views and mutable
scoped aggregates are stack-only: a local refuses them, wrappers holding
exclusive authority included. Shared read views may be locals and keep their
dependencies. Local lookup never reborrows. Task transport needs separately
owned data.

### Binders

A child reborrow introduces a fresh ceiling `q` inside its parent's and keeps
both dependencies; ending `q` invalidates every child copy before parent access
resumes. Exclusive authority also keeps its allocation provenance: two values
of one nominal type are no evidence of disjoint storage. Rigid region
identities and declared separate fields supply that evidence; unknown overlap
rejects. Scope, region, extent and generation are distinct domains.

The callback binder `forall<…>` appears only in the callback input of an
authenticated scope operation (`XML-C2:WITH-READER`'s effect shows one). The
operation checks the callback with a fresh rigid scope atom and closes it at
return or unwind; the rows around the binder cannot acquire the atom, through
stored fields, quotation payloads or the return stack. Ordinary helpers
quantify over scopes their inputs supply and cannot return one no input or
live binder supplies. No ordinary declaration, `fresh-region-*` included, mints
scope authority: an annotation alone is not authentication, and a higher-order
wrapper must keep the scope operation and its whole callback contract.

## Integer arithmetic

Cells are two's-complement 64-bit. The signed bounds are
`$8000000000000000` and `$7FFFFFFFFFFFFFFF`. `+`, `-` and `*` wrap
(`$7FFFFFFFFFFFFFFF 1 +` is `$8000000000000000`) and never refuse. Division
is the one partial operation; its two boundary cases are contracts every
backend answers alike.

- **A zero divisor throws `E-DIV-ZERO`.** `/`, `mod` and `/mod`, the only
  dividing primitives, test the divisor and throw. It is a catchable throw,
  not an exit: `[: a b / drop ;] catch E-DIV-ZERO = if … then`. A structurally
  positive divisor says so in the type: the `positive-divisor` role
  (`src/lib/num-arithmetic.f`) makes the refusal unreachable. **The contract
  holds at every tier**: the interpreted primitive, a word compiled with
  `1 set-tier` and an AOT-built executable all throw the same code.
- **`$8000000000000000 -1 /` is `$8000000000000000`, and
  `$8000000000000000 -1 mod` is `0`.** The quotient `2^63` has no signed cell,
  so it wraps like `+`, `-`, `*`.
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
- Engine process failures use only the sealed `ENGINE-ERROR` package, whose
  names and values are `src/core/engine-error.f`. No global `E-*` aliases.
- **Fallible words `throw` a named code** (`src/lib/errors.f` or the owning
  module's package, e.g. `E-FS-PATH`), never a silent failure or an
  out-of-band flag.
- **`catch` only at explicit recovery boundaries**: REPL/CLI wrappers, test
  assertions, stack-preserving outcome adapters returning the exact code. No `…
  catch drop`, no `catch 2drop`, no masking.
- A proven-impossible state throws its owning module's named state code
  (`E-QUEUE-STATE`, `E-IR-TYPE-STATE`). Habu has no `abort"`.
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

These ceilings are reachable from plain Habu rather than a runaway. Each
refuses by name with the count it saw and the ceiling, and none truncates; the
ceiling is a named constant beside the code that enforces it.

- **A definition's captured source text**: every token of a body (name, stack
  comment, words, string literals with closing quote) plus one separator each.
  Past it: `hb: definition body text full at <cap> bytes: <name> needs
  <count>`, catchable inside `evaluate`. Move long literals into words of their
  own. It is the only bound on a definition's name.
- **One REPL line.** A longer line is refused, `hb: repl line over <cap> bytes:
  <length> typed`, and read again, never truncated, evaluated or saved to
  history. Load long definitions from a file.
- **Quotation nesting in one definition**, the most `[:` bodies open at once:
  `hb: quotation nesting full at <cap> levels: <name> needs <depth>`, catchable
  inside `evaluate`; `tools/check.f` refuses the same source first,
  `E-UNCHECKABLE`. Factor the inner quotations into named words. The files
  `tools/bootstrap.sh` compiles allow one open quotation: a nested `[:` there
  prints `[:` and exits in the Gforth recovery build while `bin/hb` accepts it.
- **Control-flow nesting in one definition** (`begin`, `if`, `while`, `do`,
  `?do` and the other open structures): `hb: control-flow nesting too deep:
  <word>`. Checked code meets the checker first: 33 nested `begin`s are refused
  `hook: non-certified definition: <name> at 'begin'`. Factor the inner loops
  into their own words.
- **A family, variant, field or package name**: `SUMTYPE`, `PRODUCT`,
  `NEWTYPE`, `STRUCTURE` and `ENUM` refuse a longer one, `tools/check.f`
  reporting `E-BAD-DECLARATION` with the name as its token and the reason
  `name longer than <cap> bytes`; `package` and `using` throw
  `E-PACKAGE-NAME-CAP` out of the statement, reported as `E-STATEMENT-THROW`.
  A type qualified by a longer name names no package,
  `E-UNKNOWN-SIGNATURE-TYPE`. Shorten the name.
- **A definition's effect depth**: a row of cells, where a cell's type adds the
  levels it nests, so a returned quotation's row counts from the cell that
  holds it. Past it the definition is uncheckable: `E-UNCHECKABLE`, `effect too
  deep to record (depth <depth>, at most <cap>)`. A `trust`, `defer` or
  `TRUSTED:` row that deep is `E-BAD-STORED-SIGNATURE` under
  `fix_signature_size`, with the same reason. Keep bulk values in a buffer, not
  on the stack.
- **A definition's input row**: declared or inferred, a wider row makes the
  definition uncheckable: `E-UNCHECKABLE`, `input row too wide to record
  (<cells> cells, at most <cap>)`. A `RECURSE` in a body declared that wide is
  refused for the same reason. Nothing is recorded for it, so callers find no
  effect. A stored signature that wide is `E-BAD-STORED-SIGNATURE` under
  `fix_signature_size`. Pass bulk values in a buffer, not on the stack.
- **Data space**: `allot`, `align`, `,`, `c,`, `create`/`variable`/`defer` and
  the interpret-mode string literals that keep their text (all but `."`)
  advance the dictionary pointer. Past the DATA reservation: `hb: data space out
  of range: DP <dp> of <cap> bytes`, catchable inside `evaluate`, both numbers
  offsets from `data-base`, with ` at <path>:<line>` when a source file is
  open. Hold bulk data in `MEM:ALLOC-BYTES` or a `DYNAMIC-BUFFER`, which map
  their own pages, not in the dictionary. While a task is live every one of
  these sinks exits before it writes, as do `evaluate` and `evaluate-closed`
  (the `src/lib/task.f` header).

## Constants

- **Named constants, no magic numbers.** Limits and codes are named
  constants in the module that owns them, stdlib throw codes in
  `src/lib/errors.f`; a literal is acceptable only for a true primitive of the
  encoding (the `3`/`7` of the 3-bit tag), with a comment.
- **Default to `$hex`**: byte values, masks, addresses, memory, struct and byte
  offsets, field strides, syscall and exit constants, instruction encodings,
  ASCII codes (`$FF and`, `$D10043FF`, `$200`, `$40`). Decimal only for genuine
  small counts (loop bounds, arities, shift amounts, register indices) and
  ordinary human quantities. Crypto and format constants follow the spec's hex
  spelling. `$hex` parses case-insensitively with an optional leading `-`.

## Testing

- Exercise changed behaviour through checked assertions, meaningful errors and
  edges included, through public entry points rather than a test per trivial
  helper. `T=`/`T<>` for scalars, `T$=` for strings, `TTRUE`/`TFALSE` for flags
  (not `T=`), `TTHROWS` for codes; several results compared top down, one
  assertion each. A regression distinguishes the defect it prevents.
- `src/lib/test.f` is the test framework; a test file requires its own
  dependencies. A suite file installs its hooks, declares groups and rows and
  runs them once; the `src/lib/test/suite.f` header states the hook set, the
  group modes and the row grammar.
- Runners keep no suite iteration state on the return stack while a test runs:
  tests may `catch`/`throw`, so loops use explicit index/count cells that a
  caught throw cannot truncate.
- Fixture helpers live in a private package; install them through `TEST:*`
  hooks rather than global stems such as `FOO-TEST-SETUP-N`.

```forth
require src/lib/test.f

package FEATURE-TEST

variable RUN-N

: RUNNER ( ptr u8 n -- bool )
   2drop
   1 RUN-N +!
   true ;

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
RUN-N @ 1 T=
T-REPORT

;package
```

- Assert the specific outcome: inside checked definitions `[: WORD ;] TTHROWSQ`
  (it runs a `( -- )` quotation) or another stack-preserving `catch` with the
  exact code; top-level scripts that cannot push quotations use `' WORD
  TTHROWS`; diagnostics are captured and matched by substring.
- Run a focused fixture through its owning test tool so assertion failures set
  the exit code.
- Property generators use `src/lib/property.f`: `PROP:SEED!` makes a run
  reproducible, and `PROP:RND%` bounds a draw without inheriting the raw
  generator's short low-bit cycles. The generator is deterministic, not
  independent or cryptographic randomness.
- A claimed false reject needs a reduced checked refusal and a matching
  unchecked execution of the declared effect.

### Diagnosing a checker miss

Reduce the failing checked program, identify the violated contract, decide
whether the defect is in a declaration, the checker, generated code or runtime,
fix that layer, and add a regression through the actual load path. A property
outside the type system's contract is stated as such and checked at the right
runtime or analysis layer. No template or task record is required.

## Verification before committing

The required gates and the full native suite live in [gate.md](gate.md).
Compiler, runtime or broad library changes rebuild the engine before the suite;
documentation-only edits need no rebuild.

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
  Cite a word, not a line number.
- A change that makes a comment false corrects the comment in the same change.
  Remove scratch and debug prints before commit.
- A definition that fails to compile in raw engine mode reports the undefined
  word on stderr and may spill the rest of the definition through the
  interpreter. `tools/check.f --all-errors` and `--verify-only` report every
  refused definition, each for the one fault the load refuses it for; the
  `tools/diag-code.f` header states the diagnostic contract.

## Habu Native Tooling Gotchas

- **Native debugging**: [debugging.md](debugging.md) covers the stack display,
  breakpoints and the dump tools; `using DEBUG` opens the baked debugger.
- **Semantic xref is in-image**: ownership, references and call RCA use
  `XREF`/`SEE`/`USES`/`USED-BY` in the live image, with CLIs as thin wrappers;
  source search where it answers, native inspection where runtime state matters.
- **`DYNAMIC-BUFFER NAME Type` for growing typed tables**: the stored types of
  `TYPED-BUFFER`, closed non-linear layouts included, **and `u8`**, the
  growable byte row; it is the one definer that takes `u8` (`DYNAMIC-BUFFER X
  u16` is refused). `count NAME-RESERVE` allocates at least that many elements
  and keeps contents; `index NAME` answers `ptr Type`, rejecting negative and
  beyond-capacity indices; a smaller reserve keeps the allocation; growth may
  move it, so retain indices and reacquire pointers. A growing reserve copies
  the whole old capacity, so stale cells beyond the old count survive and the
  accessor bounds by capacity, not the requested count: a column whose zero is a
  default is cleared over the newly exposed range by the reserving word.
  `NAME-RELEASE` frees the mapping and is safe to repeat. Mappings are
  transient: release before saving an image; image-retained values use
  dictionary storage. With `u8`, `index NAME` is `( n -- ptr u8 )` at byte
  `index`, read and written with `c@`/`c!`, never cell `@`; `count NAME-RESERVE`
  counts bytes, and the capacity is the request rounded up to a cell.
- **Runtime-sized buffers use OS mappings.** `create … allot` consumes
  dictionary DATA; `src/lib/memory.f` allocates contiguous or separate 64K
  buffers and spans, bounded by cell-size overflow and OS allocation failure.
- **`create … allot`, `BUFFER:` and `TYPED-BUFFER` allot zeroed space on a
  cell-rounded address.** `create`, hence `variable`, rounds the data pointer to
  a cell before publishing, and `align` does on request. So a `create … allot`
  block and `src/lib/memory.f` memory are cell-aligned for a cell-typed reader,
  and a row a foreign call reads as an aligned C object needs no alignment word
  of its own. `allot`, `c,` and `,` never realign: storage carved after
  byte-sized data is misaligned unless `align` precedes it, and a cell-typed
  reader refuses it.
- **`CELL-BODY` names complete cells in a created body.** After `create` and
  the final `,` or `allot`, `n CELL-BODY` records exactly `n` cells in that
  live word's body. The body must be cell-aligned, `n` must be positive and
  within cell-size multiplication, and `here` must equal its end; otherwise
  the declaration throws `E-LAYOUT-BUFFER`. In a checked definer such as `:
  GRID: ( n -- ) create , 2 , 2 CELL-BODY does> ( -- SPAN:span<u8> ) cell+
  byte-view CELL SPAN:MAKE ;`, the declaration stays with the created
  allocation when `does>` changes the callable result. Image capture then
  scans only those complete cells instead of byte-shifted windows through
  them. `CELL-BODY` does not declare their values or authorize a pointer:
  an aligned cell containing a build-time mapping address remains refused in
  a stripped image, and undeclared raw bytes retain the conservative scan.
- **Missing convenience words are not bugs in the standard.** Core lacks `pick`,
  `within`, `s>number?`, `move`, `fill`, `erase`, `bl` and `*/`; each is
  `E-UNDEFINED` in a checked body (`: W ( -- ) bl ;`). Use cells, explicit
  increments and comparisons, and the checked byte and string helpers. `0<>`,
  `true`, `false`, `fdup`, `fover`, `fdrop`, `f<=` and `f>=` come from
  `src/lib/prelude.f`, which the engine already provides: writing `require
  src/lib/prelude.f` documents the dependency but is not load-bearing. Never
  re-derive `0 0=` / `0 0= 0=` by hand.
- **Code that suspends checking restores the existing hook**: after `0
  set-check` it re-arms the same hook at once with `' HOOK set-check`. That is
  no permission for new unchecked spans, and a second hook name in a baked
  bundle fails closed at startup.
- **Escaped literals for readable snapshots**: `s"` reads no escapes; `S\"`,
  `C\"`, `.\"` accept C-style escapes (`\\`, `\"`/`\q`, `\n`, `\r`, `\t`,
  `\xNN`, `\z`, …) for direct JSON/source expected strings. `S\"` needs its
  delimiter space (`s\"\n"` is one undefined token) and reads `\u` as its own
  escape, so a fixture holding JSON writes `\\uXXXX`. Generated syntax from
  fields uses checked byte/field helpers or `src/lib/json-write.f`.
- **Emitter punctuation is semantic**: an assembler word ending in `,` emits,
  and differs from the bare name; source-shape regressions assert exact tokens;
  emitter stack comments describe the host stack (`( -- )`, `( n -- )`), and
  emitted runtime effects live in prose or the generated word's contract.

## Native Forth Gotchas That Shape How We Write Code

- **Control words and ticks are compile-only**: `if`/`else`/`then`,
  `begin`/`while`/`repeat`, `[']`, `i`, `?do` and `;` live inside a `:`
  definition, never at top level; interpreted tests use `'` (`' WORD catch`).
  Both ticks resolve the name as a bare word does (the open scope, the globals,
  then the used publics), and a miss is `E-UNDEFINED: NAME`, catchable under
  `evaluate`. A tick is no presence probe: require the file that defines the
  word first.
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
  a loop reject the definition; `uncheckable` is for modeled-word gaps.
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
- **Fallible value-returning scanners validate first**: range and schema checks
  that `throw` go in a `--` helper, and the value-returning word's remaining
  path structurally returns its outputs; a final throw-only fallback in a `--
  value…` word confuses path-effect merging.

## Rules learned by refusal

These edge cases name the refusal at the boundary. The general rules are in the
sections above.

- **A ticked engine primitive is an opaque xt.** A primitive's effect is its
  `EPRIM:` row at a call; no row rides its xt, so `['] dup execute` and
  `['] NULL-PTR execute` are `E-EXEC-OPAQUE-XT`, and `['] NULL-PTR is W`
  is refused at `is`. A quotation that calls the primitive carries its row:
  `[: NULL-PTR ;] execute`.

- **`OPAQUE` belongs in a package's field-bearing `STRUCTURE` header.** After
  the first field, it is `header clause after the first field`; twice, `a
  second OPAQUE clause in one declaration`; without fields, `opaque requires a
  family with fields`; outside a package, `OPAQUE requires a family declared
  in a package`. An `ENUM` rejects it as `unexpected token in enum
  declaration`. A rejected declaration rolls back.
- **A checker replay admits rows, never live namespace changes.** While one is
  open, a definer (`:`, `create`, `variable`, `constant`, `defer`, `TRUSTED:`,
  `CAST:`), `EXPORT`, a new `package` or `wordlist` throws
  `ENGINE-ERROR:OVERLAY-OPEN` at its name before the dictionary changes (`hb:
  definition while a checker replay is open: NAME`), and `undefine` throws it
  before retiring anything (`hb: undefine while a checker replay is open:
  NAME`). Reopening an existing package is allowed. The throw is catchable
  inside `evaluate`; the replay still closes.
- **A definition and a `(` comment close in the source that opened them.**
  Files, stdin and `evaluate` strings refuse an open definition at their end
  (`hb: source ended inside definition: NAME`) or an open comment (`hb: source
  ended inside a ( comment`). The first includes its stack comment, locals and
  quotations; the definition rolls back on a caught `evaluate`. An immediate
  word may evaluate text while a definition is open, but a `;` from that text
  may not close the outer definition (`hb: source closed a definition it did
  not open: NAME`). Completed tokens in nested evaluations stay compiled in the
  outer definition after a caught refusal. The open-definition diagnostic
  locates the name (or `def-open` caller), the open-comment diagnostic its `(`,
  and the foreign-close diagnostic its `;`; stdin has no line location.
- **A fresh identity is minted per call, declared or inferred.** With
  `defer RG-ONE ( -- fresh-region-a )`, `: USE-INF RG-ONE ;` records its
  region as a template slot, so `: T3 USE-INF USE-INF SAME-ID ;` (where
  `SAME-ID` is `( x x -- )`) compares two allocations:
  `E-RIGID-REGION-MISMATCH`, as two calls of `RG-ONE` are.
- **A refused storage declaration defines nothing.** `LAYOUT-BUFFER`,
  `DEFER-LAYOUT-BUFFER`, `TYPED-BUFFER`, `TYPED-VARIABLE` and
  `DYNAMIC-BUFFER` require a valid storable type and name, plus a valid count
  where the definer takes one. Unknown, malformed or missing types; missing
  names; names with more than one `:` or in sealed packages; and an invalid
  literal count are `E-BAD-STORAGE` at the offending token. The name is on the
  definer's line, the type starts on the name's line; a following-line token
  belongs to the next statement. A computed count is bounded by the definer at
  run time with catchable `E-LAYOUT-BUFFER`. For example, `4 TYPED-BUFFER B
  no-such-type` prints `habu: in B: unknown type 'no-such-type'`.
- **Fixed `TYPED-BUFFER` elements occupy whole cells.** `2 TYPED-BUFFER TB
  u8` is refused (`type this definer cannot store 'u8'`). Use `n BUFFER: B`
  for fixed bytes, yielding `( -- ptr u8 )`. `DYNAMIC-BUFFER` alone stores
  `u8`, with byte indexing and `c@`/`c!`; `u16` is refused. `create … allot`,
  `BUFFER:` and `TYPED-BUFFER` allocate zeroed, cell-aligned storage; `allot`,
  `c,` and `,` do not realign following storage (see **Habu Native Tooling
  Gotchas**).
- **A fixed buffer's count may be a literal, `( -- n )` word or expression.**
  The source pre-pass reads a literal without executing it: `0 TYPED-BUFFER B
  n` is `E-BAD-STORAGE` (`count outside the buffer's extent`), while `$40
  TYPED-BUFFER B n` checks. A single nonliteral count token must resolve to a
  no-input word producing one `n`-compatible cell; an unknown, ambiguous or
  shadowed name, `variable`, or `( -- bool )` word is `E-BAD-STORAGE` (`count
  resolves to no ( -- n ) word`). Expressions such as `N 2 * TYPED-BUFFER B n`
  are evaluated by the load. A `( -- n )` word whose value is zero passes
  pre-verification, but the definer throws `E-LAYOUT-BUFFER`. The same count
  rule applies to `LAYOUT-BUFFER`.
- **Parsing operands are data.** Top-level `'` and `char`, and body `[']` and
  `[char]`, consume the next blank-delimited token, even across lines, without
  treating its spelling as a comment, quote, definition or reserved name.
  Thus `char : constant COLON` and `: F ( -- n ) [char] ( ;` are valid. A
  lexical local takes precedence byte-for-byte over a keyword of the same
  spelling; `{: char :} char` reads the local and consumes no operand. A local
  declared in a block dies at its closer; an enclosing local in a quotation
  still binds and is refused there (`E-BAD-LOCAL-SHAPE`). A dictionary word
  that parses, such as `require` or `SEE`, is resolved by scope and is not
  inferred to be a keyword from its spelling.
- **Definer names are raw `parse-name` operands.** `package (` names `(` and
  `: \ ( -- n ) 1 ;` names `\`; they do not open a comment or line
  comment. The type definers refuse `(` or `\` as names (`E-BAD-NOMINAL-TYPE`
  for `DEFLINEAR` and `VALUE-RECORD`). A definer or parsing word without its
  operand is `E-MISSING-NAME` at the keyword in every `tools/check.f` mode,
  `--verify-only` included.
- **A `create … does>` clause declares its created words' effects.** The
  source pre-pass learns a scanned clause; for a resident definer, the checker
  latches the created-word effect at publication. A straight-line wrapper
  that calls exactly one definer and has no control frame, quotation, `exit`,
  `leave` or `recurse` inherits it. A `TRUSTED:` body's clause is a
  declaration even though its body is asserted. The same rule applies to a
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
  `E-TRUST-UNRESOLVED`. The mark covers only later uses in that section; an
  earlier use, other package, or inaccessible private product still refuses.
  A `trust` row for an unknown product defers only in the wordlist where it
  lands, not a wordlist reached through lookup. A `TRUSTED:` body carries
  bound calls' renderer facts, and a deferred body retains the facts of calls
  it binds. Calls through a `defer` or an executed xt expose no callee facts.
  A name already known to the checker resolves before any renderer mark, even
  if the generated product shadows it. A reached renderer without a read
  declaration group or `generates:` row is itself `W-CHECK-DEFERRED`, even
  without a later product use. A declared primary product closes that gap;
  other generated names remain deferred at their uses. Closing a read group
  with `;FUNCTION` is not a gap; closing one whose opener was not read is.
  `EXPORT` preserves an engine-provided definer's coverage declaration; an
  export of a source-scanned definer remains deferred.
- **A text-generating definer can declare its product with `generates:`.** At
  top level, after the definer, `generates: D ( effect )` assigns every word
  D makes that effect, as `does>` does. `FUNCTION:` instead supplies its
  declaration group, treating an `i32` result as `n`. A `generates:` claim
  does not define the made word; the load judges its actual effect and any
  other products. An undefined D, duplicate declaration (including a clause
  or inherited definer effect), or invalid effect is `E-GENERATES-ROW`. A
  used-scope conflict is `E-USING-SHADOW-GLOBAL` or `E-USING-AMBIGUOUS`. The
  name is one blank-delimited token; the signature opens at the first `(`
  after blanks, attached to a token or not, and closes at the first `)` byte;
  a line feed inside it stays in its token. The signature has a fixed byte
  cap. `undefine D` retires the row; `EXPORT` preserves the created-word
  effect. `--verify-only` reports an undeclared renderer product at its use as
  deferred. A declared product is checked against its row.
- **Top-level source checking follows source order without executing it.** A
  token outside a known declaration is resolved as the loader would resolve
  it. Numbers use the engine's parser; `-3`, `$FF` and `1.5` are literals,
  while `1.`, `$GG` and an over-bound decimal are
  `E-UNDEFINED-TOP-LEVEL` at their tokens. Other missing names and
  compile-only words (`if`, `;`, `does>`, `[char]`, `[']`, `[:`, `is`, `.(`)
  get that code before execution. A malformed qualified name is
  `E-BAD-QUALIFIED-TOP-LEVEL`; ambiguous and global-shadowing `using` names
  get their body diagnostics. Top-level `PKG:tail` while PKG is open may find
  global `tail` if its public section lacks one; the selected word's own
  facts govern subsequent parsing and definitions. An uncertain resident
  immediate or loader may leave dependent source deferred rather than publish
  an uncertain definition.
- **Unknown top-level parsers stop discovery unless declared.** A word whose
  body reaches `parse-name` or `create`, or a `defer`, may consume or define
  following tokens. In `--verify-only`, it is `W-CHECK-DEFERRED`; later text
  in the file or included file is not scanned because operands can cross
  lines. A reached renderer marks its wordlist under the rule above. A source
  row after a word's definition can bound its operands: `parses: W n`
  consumes `n` raw tokens; `parses-through: W n ( E1 E2 )` consumes those and
  through the first exact listed terminator. `n` is nonnegative; parentheses
  and terminators are standalone tokens, terminators are case-sensitive and
  `)` cannot be one. The row is a trusted source-prepass claim, never a
  runtime parser definition. Bad target, no source-reading behavior, bad count
  or malformed list is `E-PARSES-ROW`. Later rows replace earlier ones;
  `undefine`/redefinition drops them; `EXPORT` copies them, but a calling
  wrapper does not inherit them. Ordinary loading checks a row without
  retaining it. A declared call is still `W-CHECK-DEFERRED`; scanning resumes
  after its raw operands. EOF inside those operands leaves the call deferred.
  A `defer` and a primitive remain undeclared. A resident or cached
  dependency needs a row read by this check; discovery uses a row by spelling
  only when its dependency closure has one declaration and no retirement. A
  bare name counts declarations across all packages; a qualified name counts
  only its package. Otherwise discovery reads operands as source. A checked
  use must materialize a cold captured binding before a row can bound it.
  Calls through xts and defers remain opaque; a plain `create` wrapper without
  `does>` has no created-word effect in checked bodies; the `does>` clause's
  own parsing behavior gives its products no parser facts.
- **`names: W` marks literal references W resolves.** After W's definition,
  this row states that W looks up its top `ptr u8 n` input as a name. A
  directly preceding unescaped string literal in a body or at top level is
  then a use of the word spelled by its bytes. A bare name resolves globally;
  `PKG:TAIL` selects that package's public word, and only a word already
  visible at the literal binds. Computed strings, strings passed through a
  caller and escaped literals bind nothing. `EXPORT` copies the claim and
  redefinition drops it; a calling wrapper needs its own `names:` row. An
  absent target or one without a declared trailing string input is
  `E-NAMES-ROW`. The claim is not checked against W's body.
- **A `TRUSTED:` body may return loose cells as a declared family value.**
  Payload cells sit below the tag. Native elaboration groups them by its
  signature; a checked body must build the family value explicitly
  (`E-NELAB-JOIN` for loose cells). A wrong cell count is `E-NELAB-ARITY`
  either way, and consumption still checks its tag (`hb: bad layout tag`).
- **Binding follows the engine's selected word and its own metadata.** After
  `undefine dup  : dup ( n -- n ) 100 + ;`, `5 dup` answers 105 at either
  tier. Intrinsic typing belongs to the original engine word, not the new
  spelling; the same applies to package shadows and `EXPORT` aliases. A
  pending definition binds its own name before competing used publics. A
  checker-only row with no engine word is `E-UNDEFINED` in compiled code,
  except that generated definitions in one checker scope may refer to rows
  recorded earlier in that scope (see **Naming**).
- **Native compilation bounds a definition's width.** A wide enough
  definition is refused: by the ARM64 backend as `E-A64RAV-DKEEP`, and past
  the IR signature list's cap as `E-IR-TYPE-ARITY`. Register pressure is no
  limit: entry values beyond the register pool are stored.
- **Literal and output punctuation matter.** `s"` does not interpret escapes;
  `S\"` does and needs its delimiter space (see **Habu Native Tooling
  Gotchas**). Native `.` prints a newline: `11 . 22 . cr` emits
  `11\n22\n\n`; use digit emitters for inline numbers.
- **Package `private` is visibility, not sealing.** Another file may reopen
  the package and call its private words until `get-current prot-wid-add` or
  the native capture seals it; a sealed reopen is refused. Multi-file owners
  seal only after their last file (see **Packages**).
- **A cast that introduces a pointer or xt requires a private package row.**
  `CAST: >BYTES ( n -- ptr u8 )` and `CAST: >OP ( n -- [ n -- n ] )` are
  `E-CAST-MINT` at top level or under `public`. A type-variable pointee is
  `E-CAST-LINEAR`, and a layout's fields carry the mint and foreign-owner rules
  recursively: a `pfbox` whose field is `ptr u8` exposes that pointer through
  `PFBOX:UNMAKE` (see **Structures And Enums**). A `LINEAR:` token conversion
  similarly requires the declaring package's private section; bad payload,
  owner or scope is respectively `E-LINEAR-PAYLOAD`, `E-LINEAR-OWNER` or
  `E-LINEAR-SCOPE`. A checked body cannot declare one; outside callers use the
  owner's public words.
- **Nominal declaration placement and inference matter.** A `DEFTYPE` handed
  out by a definer's `does>` body must be public, so a reader can resolve the
  stored signature; a private one is `E-BAD-STORED-SIGNATURE`. Constructing
  only `RESULT:OK` leaves the error type variable of `result<a,b>` free;
  that variable cannot unify with a nominal enum or family. Use a result
  family whose error alternatives are nullary, such as
  `ENUM foo-result<a>` with a payload-bearing success variant and nullary
  error variants. Spell an arity-one family with its argument in
  an effect (`family<PKG:t>`) and bare in `MATCH family`; the other form
  gives `wrong arity for type family`.
- **Atom prefixes and declaration keywords reserve family names.** An atom
  prefix such as `layout-` claims the whole lowercase `layout-*` namespace,
  so a conflicting enum variant is refused. A grammar keyword such as
  `policy` and a value-record name already registered as `vr` similarly
  cannot name a family (`STRUCTURE vr` is refused).
- **`0 set-check` suspends the armed hook with its compile preflight.** It
  empties both cells; `set-check` of that same hook resumes both, so
  `check@ 0 set-check … set-check` restores checking whole. Arming any
  other hook discards the suspension and starts without a preflight until
  `set-preflight` installs one; an armed checker without one refuses the
  first source-defined immediate with `hb: compile preflight hook missing`.
  `0 set-check` while already off keeps the suspension, so windows nest.
  `discard-check` drops a suspended hook so no later `set-check` resumes it. Use
  a `PRIM: … PRIM;` axiom for a genuine missing primitive model so later
  definitions remain checked.
- **A constructor-generating declaration needs checking on.** `SUMTYPE`,
  `PRODUCT`, `STRUCTURE` and `ENUM` generate constructor words the checker
  certifies, so with the hook suspended each is refused through its
  declaration packet before anything registers: `habu: bad sumtype declaration
  'name': checking is suspended; declare it where checking is on at 'name'`
  (`E-TDECL-UNCHECKED`), and a multi-error load counts it, skips to the
  terminator and continues. `NEWTYPE` and `DEFTYPE` generate no constructor
  and are admitted; a declaration after `check@ 0 set-check … set-check` is
  ordinary.
- **A sealed from-source prefix needs external rows for pre-hook words.** A
  word defined before the check hook is internal without a `PRIM:` axiom or a
  `TRUSTED:` declaration after the checker claiming it. A checked or
  top-level use is `E-UNDEFINED` or `hb: internal engine word: NAME` before
  the checker runs. A `TRUSTED:` or data word before the hook gains no such
  declaration. A constant without one may be read at top level into a
  file-owned constant. An unsealed prefix can bind a signed colon word by its
  declaration.
- **A signature compiled with the hook empty is a row without authority.**
  This applies to cold prefix and `0 set-check` definitions at either tier.
  An unsealed checker can use the declared effect to check callers, rejecting
  a missing input as `E-INPUT-UNDERFLOW`; sealed images refuse it
  `E-CAP-TRUSTED`, while a pre-hook internal name is refused by the engine
  first. `TRUSTED:` asserts an authoritative row. Before the checker claims a
  cold prefix's source, signed definitions are logged for that claim; a
  qualified definition there terminates with `checker: qualified definition
  before the source claim`. A signature missing, unparseable or containing a
  scope scheme records nothing. On tier 1 a refused scan leaves ordinary
  compilation using the declaration and reports the reason; a native build
  requires the scan and refuses before publication. A failed native compile
  retracts only the pending definition's rows, preserving prior rows. If no
  row supplies arity it refuses with the hook's reject status; if another
  owner's row supplies arity but the body underflows it, it is
  `E-NELAB-UNDER`.
- **Native compile callbacks cannot change the checker store.** From scan
  completion through the last publication callback, a checker scan or a
  checker write (`generates:`, a linear, record or family declaration, a
  field owner, an extent mark) throws catchable `E-NCOMP-STATE` before
  mutation. The window closes on every exit, including throws, so a refused
  definition cannot leave an orphaned type or row. This applies to certified
  and hookless compilation.
- **A file object refuses an effect below its file it cannot replay.** A
  file that leaves the compiler tier, the executable-build depth or saved tier,
  or the optimizer dispatch changed is `native-file: refused ...
  reason=kernel-cell-changed cell=tier` (`build-depth`, `build-tier`,
  `dispatch`); `0 set-tier` alone refuses, `0 set-tier` then `1 set-tier`
  stores. Only records the running binary held when the process first opened
  the definition cache are seeded. A record an earlier top-level load in the
  same process made has no key, so a later load that reads it is ordered
  (`resolution-key-unknown`, `binding-key-unknown`; the object refuses
  `ordered-dependency`), and a file loaded inside such a package is
  `scope-key-unknown word=<package>`; one loaded while a wordlist no file of
  the load made is current (an earlier top-level load's, or the session's) is
  `scope-wordlist-unknown`. A dependency the file's own load
  `require`s is tracked as usual. `test/native-file-frame-e2e.f` measures each line.
- **A file's top-level run is admitted by its closure or by its trace.** A run
  of the file's own word or of an earlier file's walks the closure it reaches:
  a definition whose host reason is SAFE, MEMORY, INDIRECT, FIXED or REBIND (a
  compiled `is`, whose stored xt is a declared cell the walk follows), an
  unchecked one with a checker contract, a representation cast (a `CAST:`
  whose output holds no pointer or quotation), a primitive with a REACH class
  and the unset-defer sentinel are admitted; a seed word with no class is
  `reason=run-reaches-kernel-word word=W` (`run-reaches-kernel-data` for seed
  data), a definition with another reason `run-host-cast`, `-unchecked` (no
  contract), `-foreign` or `-unknown`. A run that
  reaches an unchecked body leaves stores no checker saw, so the object judges
  the file's undeclared DATA cells as it does for a file that ran with
  checking off: a code or mapped address there refuses the file. `die` is classed as a
  process exit: a run that ends the process leaves no result to store, so a
  closure reaching it is admitted. Under the file's own word the file refuses
  by that name; under an earlier file's word the frame is ordered and the
  object refuses `ordered-dependency`. A seed word the top level runs itself is
  admitted only by the trace it leaves before the next token: source read past
  its token, a changed record count, HERE, wordlist count, kernel cell or
  checker unit fact count, a child file it loaded, text it wrote to the
  output device, or a change to the open FFI declaration (`FUNCTION:`, a
  clause). That text is part of the file's result: the object carries it and
  a hit writes it again in load order. `;FUNCTION` folds the declaration into
  the definition and FFI-FN row the object carries; a segment that opens or
  closes with a declaration open is `ffi-declaration-open`. Otherwise it is
  `run-unaccounted word=W` and the object refuses `ordered-dependency`, so the
  file compiles on every build: `s" HOME" GETENV`, `data-base`, `' W execute`,
  a top-level `WRITE-ALL` and `PROCESS-SYMBOLS` each refuse. A run that
  declares CWORK storage or writes a CWORK default, and loaded no child file,
  is `run-writes-cwork word=W` whatever else it left: compiler working state
  is never carried, so the file compiles on every build
  (`test/native-file-cwork-e2e.f`). A nested run that
  a caught throw abandoned refuses `run-abandoned word=W`: the catch restores
  HERE and the source position, so the run's trace cannot be judged.
  `test/native-file-frame-e2e.f` measures each line.
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
  of its top declared input cells, up to a fixed depth, every throw path
  leaves untouched; deeper inputs count as not intact. A drop, swap,
  overwrite, or local bind and pushback loses evidence. A called word that
  throws is assumed to overwrite its declared inputs unless its own body
  proves otherwise. The literal and `['] W` carry W's evidence, including
  through a local bound to that same tick. A fresh declared quotation
  parameter, typed `xt<effect>` cell or `defer` has no such edge and leaves
  the window typed. A tick of a never-returning callee still fits against its
  declared output row and can be `E-NELAB-QUOT` at `[']` in native
  elaboration.
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
  Underflow is catchable; residue is `E-EVAL-RESIDUE`; an unfinished
  definition is refused as at the end of any source. Data-stack overflow
  exits, and with a task live evaluation exits before reading text. The open
  cases are under **Checked code and primitive boundaries**.
- **`TEST-EVAL` returns a measured value from text.** From
  `src/lib/test.f`, `TEST-EVAL:N ( ptr u8 n -- n )` takes exactly one cell,
  `TEST-EVAL:FLAG ( ptr u8 n -- bool )` exactly one flag, and
  `TEST-EVAL:RC ( ptr u8 n -- n )` returns the `evaluate-closed` throw code
  or zero. An empty N text underflows, `1 2` is `E-EVAL-RESIDUE`, `drop 1`
  underflows without touching caller cells, and an unfinished definition is
  refused. N can run inside N's evaluated text. The value type remains
  checked at the caller: declaring bool for N's numeric result is
  `E-MISMATCH`.
- **Return-stack cells never cross a signature or quotation boundary.** The
  only admitted `|` notation is the same single row variable on both sides;
  cell entries, a bar on only one side, and such entries in quotation types
  are `E-BAD-SIGNATURE` (or `E-BAD-STORED-SIGNATURE` for `TRUSTED:`, `defer`,
  `trust` and `CAST:`), repair `fix_return_stack`. A literal that pushes a
  cell for its caller to pop, or pops one the caller pushed, is `E-REJECTED`
  at `;]`; an unsigned definition's `r@` or `r>` on an input it did not push
  is rejected at the token. Use `>r … r>` within one definition (see
  **Checker & type model**).

## Spans: a pointer that carries its reach

When a word writes into a buffer or indexes one, pass a `SPAN:span<u8>` rather
than a `ptr u8` and a separate length: `src/lib/span.f` bounds-checks every access
against the reach the span carries. Take the span from a producer — `n
SPAN-BUFFER: NAME`, `n SPAN-CELLS: NAME`, `MEM:ALLOC-SPAN` — or narrow one you
were handed with `SPAN:SKIP` / `SPAN:TAKE` / `SPAN:SUB`, which can never widen
it. `SPAN:MAKE`, the one place an address and a number become a reach, belongs
to library source; a lint reports it, and bare `BYTE-COPY`, in a consumer
(the `tools/lint/bare-copy-lint.f` header states where). A read-only source stays the `( ptr u8 n
)` string idiom. The reach is counted in bytes whatever the element type.
