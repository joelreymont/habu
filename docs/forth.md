# Forth Standards (habu)

Workers: read [docs/forth-card.md](forth-card.md) first; this file is the reference.

How we write Forth in this repo, for the native `bin/hb` engine. Durable
language guidance lives here, with the fact that proved each rule beside it;
build and test rules live in [bootstrap.md](bootstrap.md) and [gate.md](gate.md).

## Checked code and primitive boundaries

- Write ordinary Forth as checked definitions: tools, tests, emitters, build
  drivers, source generators, cleanup and dispatch code included.
- Never add `TRUST`, `TRUSTED:` or an unchecked span to bypass a missing checker
  model. Fix the declaration, checker or primitive interface at its owner.
- `PRIM:`/`PPRIM:` axioms are for genuine engine, syscall and FFI operations,
  with checked callers. A Forth algorithm or unchecked wrapper does not become a
  primitive by renaming it or asserting its effect.
- A `PPRIM:` row closed with `CLOSE-PRIVATE` instead of `PPRIM;` interns the
  axiom into the OWNER package's private wordlist, so only a body compiled
  inside that package resolves the name: a capability primitive bound to the one
  package entitled to it, callers still checked. Such a prim keeps its global
  `PRIM-TRUSTED-ONLY!` row beside the private one for now:
  `src/core/internal-mark.f` classifies a record by its BARE name, so a
  primitive with only an owner-private row is sealed `DNAME-INT` and has no
  checked caller at all. `test/prim-owner-scope.f` pins the matrix.
- Model dynamic source evaluation honestly: never assert that arbitrary
  `evaluate` preserves the stack. Use typed quotations for known callbacks.
- Existing TRUST forms are legacy awaiting removal, tracked in
  [minimal PRIM migration](../.dots/habu-trusted-dies-prim-4fd12d60/habu-finish-minimal-prim-c00c6a93.md);
  mentions below describe legacy syntax only.

## Naming

- **Our words UPPER-CASE, built-ins as-is.** `RESOLVE`, `MK-CON`, `APPLY-EFFECT`;
  `and`, `cells`, `allot`, `: ;`, `?do`. Never upper-case a built-in.
- **Scope pairs are `FOO … ;FOO`** (decision 2026-07-04). Every new pair that
  opens and closes a scope closes with the opener's name behind `;`, as the
  shipped DSL does: `SUMTYPE … ;SUMTYPE`, `PRODUCT`, `ENUM`, `VARIANT`, `MATCH`,
  `package … ;package` (keyword case follows the opener). The shipped
  `VALUE-RECORD … END-VALUE-RECORD` and the Forth-2012
  `BEGIN-STRUCTURE … END-STRUCTURE` are the sanctioned exceptions, not
  templates. Never coin `END-FOO`, `FOO-END` or `ENDFOO`. ANS control words
  (`begin … until`, `case … endcase`, `do … loop`, `of … endof`), the bare `;`
  and non-scope `;`-words are unaffected.
- **Hyphens, never underscores**, in word names and file names: `T-CON`,
  `camera-tracker.f`. A port of an underscore-named source is named in Habu
  style.
- **Conventional affixes**: predicates end `?` (`TYVAR?`); conversions `>X`
  (`TERM>TAG`); fetch/store `X@`/`X!` (`TV@`/`TV!`); allocate/reset
  `X-ALLOC`/`X-RESET`.
- **Short names**: `buf`, `ctx`, `idx`, `nv`, `ki`, `ko`; single letters only in
  a tight, readable scope; `idx`, `len`, `value` where clearer.
- **Locals are lexical and local-first.** A declared local named `i`, `count`
  or `dup` resolves to the local inside its scope; never encode dictionary
  collision workarounds into local names. A local shadows a visible word only
  in the spelling it was DECLARED in: a reference binds the local when it
  matches byte for byte, while word lookup stays case-insensitive, so a body
  that declares `text` reads the local as `text` and the word as `TEXT`.
  Measured: `{: i:n :} 0 3 0 ?do i + loop` answers three turns of the LOCAL;
  without the declaration it answers the loop index. A local binds from its
  group's closer to the end of its scope; two names differing only in case are
  two locals; repeated declarations of one spelling resolve to the latest live
  binding; mentions before the closer resolve in the preceding scope. Declaring
  or referencing a local inside a quotation is `E-BAD-LOCAL-SHAPE`; quotations
  capture no enclosing local. Because the native chain finds quotation spans
  before it knows a body's locals, a local named `;]` is `E-NELAB-LOCAL` and a
  group that writes `[:` is `E-NELAB-QUOT`.
- **Check for collisions with built-ins**: the dictionary is case-insensitive,
  so `CON?`/`VAR?` clash; prefix (`TYCON?`, `TYVAR?`). If `' NAME` resolves in a
  REPL, the name is taken.
- **Never shadow a native primitive name**: later entries replace primitive
  signatures and codegen hooks. `shadow-lint` gates this.
- **Never define a parser or control reserved word** as a published definition
  name (`:`, `TRUSTED:`, `KERNEL:`, `create`, `variable`, `constant`): `I`, `J`,
  `DO`, `LOOP`, `+LOOP`, `LEAVE`, `UNLOOP`, `IF`, `THEN`, `BEGIN`, `REPEAT`,
  `TRUST`, `CASE`, `OF`, `ENDOF`, `ENDCASE`, `TRUSTED:`, `PACKAGE`, `PUBLIC`,
  `PRIVATE`, `UNDEFINE` and the other compiler-dispatch and lifecycle tokens.
  Lexical locals such as `{: i:n :}` stay legal. A generated converter that
  strips prefixes runs `tools/reserved-name-lint.f` after naturalization, so
  `CC-I` becomes `IX` and `CC-J` becomes `JX`; `tools/check.f` runs that lint
  before spawning the checker child and reports `E-RESERVED-DEFINITION` with
  file, line and token instead of a silent rc 70.
- **Never define a number-shaped word.** hb parses numeric literals BEFORE
  dictionary lookup (`test/gate-dictionary-lib.f` GD-LITERAL-FIRST), so a
  definition named like a literal (`42`, `.0`, `1.5`, `-.5`, `$FF`) loads but is
  unreachable: every call site gets the number. Probe: `: 42 ( -- n ) 7 ;` then
  `42 .` prints 42. Only `tools/check.f`, through `tools/reserved-name-lint.f`,
  refuses such names (`E-NUMERIC-DEFINITION`); the engine will once
  `habu-refuse-a-number` lands. The lint also refuses dot-digit tails (`U.0`):
  one inserted space turns the tail into a float literal and generators misread
  it (the `lib/fmt.f` `.0`/`U.0` incident, since renamed `.INT`/`.U`).
  Dot-letter printers (`.U`, `.INT`, `F.N`) and digit-leading names that cannot
  parse as a number (`1STNZ`, `0<>`, `2DUP`) are legal. The literal grammar is
  ONE grammar in every context, interpret, colon-compile, `evaluate` and the
  checker: int `-?d+ | -?$h+`; float `-?d*.d+`, exactly one dot and at least
  one digit after it, so `.5`, `-.5` and `.0` are floats while `5.` and `..5`
  are words. A shaped decimal is admitted only while its integer magnitude,
  fractional numerator and power-of-ten scale fit their signed-cell
  accumulators; an over-bound shape stays claimed and is rejected before
  lookup, never a callable name. The checker's claim
  (`LITERAL-TOK?`/`ALLDIG?`/`FLODIG?`, `src/core/checker.f`) mirrors the engine
  parser (`EMIT-NUM`, `src/habu/habu1.f`) token for token;
  GD-LITERAL-FLOAT-FIRST pins the matrix, including that a call to a
  number-shaped word is rejected by the checker rather than certified against
  an effect the runtime never executes.
- **Prefer `$hex`** for byte values, masks, addresses, offsets, syscall and exit
  constants and instruction encodings; decimal for small counts and ordinary
  human quantities.
- **Namespaces are wordlists.** A qualified name has exactly one non-edge colon:
  `HB:COUNT`, `PTX:COUNT`, `MAKI:COUNT`; the qualifier names the wordlist and
  the record stores the tail. Match qualifier case to the vocabulary: project
  words uppercase (`HB:COUNT`), built-in vocabularies lowercase
  (`forth:count`), never mixed (`hb:COUNT`, `HB:count`); `hb:count` only for an
  intentionally lowercase vocabulary. Names starting or ending with `:` are
  ordinary words. Never fake a namespace with global prefixes.
- **Modules are packages.** New library, tool, test-support and subsystem code
  lives in `package NAME` unless it is a documented core prelude file. Export
  the real interface (`TASK:KILL`, `TASK:DONE?`), not prefixed globals
  (`TASK-KILL`). Maki is the worked adoption: each file is a `package MAKI`
  block, outsiders write `MAKI:WORD`, maki files reopen the package and use bare
  names.

### Packages

Packages are wordlist namespaces at file/module scope and the default shape for
new modules. Keywords are lowercase language words; package names and project
words are uppercase unless the package is a lowercase vocabulary. Helpers go
before `public` or after `private`; the interface goes in `public`.

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
  caller's current wordlist. Definitions are private by default.
- `public` and `private`, valid only inside a package, switch new definitions to
  the export wordlist and back. Public words are called from outside as
  `NAME:WORD` and are not found by unqualified global lookup; private words are
  visible unqualified only while the package is open, and `NAME:PRIVATE-WORD`
  never resolves.
- The public section is the module boundary: short domain words (`TASK:KILL`,
  `TASK:DONE?`, `PTX:BROADCAST`, `MAP:GET`), without the package name in the
  tail unless the domain spelling requires it. Prefix-style global APIs are
  legacy debt.
- `;package`, valid only inside a package, restores the saved wordlist and
  clears runtime and checker package scope.
- Reopening `package NAME` resumes the same public and private wordlists; it
  creates no new scope and loads no file. Later blocks call earlier private
  helpers and earlier public words unqualified and add exports. Load order is
  still dependency order.
- Multi-file packages reopen the package per file. When the loader already lists
  every file in order (`bin/hb --load app/core.f app/api.f`;
  `tools/check.f --source-list app/core.f app/api.f` for the checker), add no
  include to repeat it. Use include only when a source or entry file should own
  loading its dependencies:

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

- `include path.f` (`s" path.f" included`) loads the file every time;
  `require path.f` (`s" path.f" required`) loads once, keyed by canonical
  absolute pathname, so absolute, relative, `.`/`..` and symlink spellings are
  one identity. Use `require` for dependencies so setup and test entries can
  name one support file without duplicate definitions. Include and require
  compose source; they share no namespace. Entry, tool and test files own their
  setup with `require`: a test file requires its setup and owns its assertions,
  and no caller needs to know that `A.f` precedes `B.f` to run `B-test.f`. A
  small helper many files need becomes a narrow `src/core/*.f` prelude loaded
  before stdlib and tools, not a broad library order. Never include a file so
  two files share private helpers; reopening the package does that. Gate
  source lists are for cross-file integration subjects and generated
  build-stage source, not unit-test dependency plumbing.
- A named `--load` entry's canonical directory is the primary source root.
  Relative dependencies search that root, then the invocation working
  directory; a dependency keeps the root that resolved it for its own loads;
  absolute paths bypass the search; the process working directory never
  changes. So `/work/app/main.f` can require `src/model.f`, and that file
  `src/math.f`; Habu libraries found through the working-directory fallback
  keep that root. `SOURCE-ROOT:WITH ( ptr u8 n [ -- ] -- )` scopes an explicit
  root and restores the caller's on return or throw; fixtures resolve against
  `SOURCE-ROOT:CURRENT$ ( -- ptr u8 n )`, never a script argument. Nested loads
  keep each parent's source bytes alive until it returns and release them on
  return or throw; there is no fixed nesting count. Discovery, checker
  dependency collection and content closures use the same canonical paths and
  owner roots.
- The native engine marks its baked prefix files `provided` before user source
  runs, so `require src/core/sha256.f` skips the prefix-owned copy; frozen facts
  keep root-relative names, so the engine runs from another checkout or without
  its compiled sources; `provided` is honored before a missing file is opened;
  snapshots keep these facts and clear process-local roots and resolver scratch.
  Relocated reloading of a deleted application source tree is not promised.
- A package wordlist is a no-duplicate set, case-insensitive (`RESET` and
  `reset` are one tail), across reopened blocks and across `:`, `create`,
  `variable`, `constant` and `TRUSTED:`. Redefinition is explicit: `undefine
  NAME` retires the active entry and clears checker signature, defer-target and
  control metadata, then the name may be reused. Silent last-definition-wins
  shadowing is always an error. Shadowing an outer, global or built-in word from
  inside a package is legal (a different wordlist), and one tail may live in
  several packages (`APP:RESET`, `MK:RESET`).
- While a package is open, unqualified lookup tries the private wordlist, then
  the public wordlist, then the saved global path.
- A **public** definition whose tail a **private** word of the same package
  already owns is the forwarder pattern (`lib/task.f` publishes
  `: PREPARE ( ptr n -- ) PREPARE ;` over its private `PREPARE`) and stays
  legal, but the two effects must move the same number of CELLS. The bare tail
  binds the private word by the order above, and a definition's own contract is
  read from that binding (`src/compiler/native/compiler.f` KEEP-ARITY asks
  `NDICT:SPELL-ARITY` with the bare name), so a public word declaring a
  different arity would be compiled against the private word's. The public
  definition is refused where it is written, `E-SHADOWED-ARITY` (checker 7145,
  rc 67), naming the package, the tail and both widths; until that rule the pair
  passed the source run and the native build refused it minutes later with
  `-8303 E-NELAB-ARITY`, which stays as the backstop. The rule judges a colon
  definition with a DECLARED signature, which is the only record the native
  compiler elaborates a body for: a public word made by a storage definer
  (`constant`, `variable`, `create`) has no body and is judged by its definer's
  own row instead, so a private and a public `SHARED` constant in one package
  stay legal. Same cells through different types is accepted
  (`( ptr u8 n -- n )` against `( n n -- n )`), and so is the reverse order — a
  public definition made BEFORE the private one binds its own name at the moment
  its contract is read.
- `EXPORT NAME` inside an open package re-exports an EXISTING word into the
  current section under its own tail: same xt, same checked effect (a fresh
  alpha-equivalent scheme copy), defer and control flags and immediate/wide bits
  carried, no forwarding body, zero runtime cost. `EXPORT EVAL:RUN` in
  `package MAKI public` publishes `MAKI:RUN`; a bare `EXPORT HELPER` in the
  public section promotes the private `HELPER`. Refused: an undefined source, a
  private word behind a CLOSED package (qualified lookup is public-only), a
  source qualified into a sealed system package, a primitive (prims may be
  overloaded; an alias would narrow the effect) and a duplicate tail in the
  target section. Re-exporting a generated constructor under a second name is
  allowed; adding tails INTO a generated constructor package is not. AOT
  tree-shake keeps one body; alias rows roll back with checker scope frames. At
  TOP LEVEL `EXPORT name…` is the hb-build `--repl` export directive: the build
  strips it and a plain load consumes the name as a no-op.
- **Qualify only across package boundaries.** Inside `NAME`'s own files reopen
  the package and use bare names; `NAME:WORD` there is noise. A call into
  another package qualifies (`OTHER:WORD`) or reopens it. A subsystem is a few
  internal module packages plus one public-interface package; only truly
  external code writes the qualifier.
- Package scope is mirrored into the checker: certified definitions in
  `private` are visible only to later checked code in the same open package,
  those in `public` as `NAME:WORD`, and duplicate certified definitions in one
  active wordlist are rejected before runtime.
- Every package feature has native gate coverage: runtime lookup, checker
  certification, private isolation, public export, reopen, case-insensitive
  lookup and fail-closed misuse (`public`/`private`/`;package` outside a
  package, nesting, missing names, qualified package names).

#### Importing a package's public words with `using`

`using NAME` makes package `NAME`'s **public** wordlist visible to bare lookup
in the current scope without opening `NAME`. `require` loads source; it imports
nothing and does not justify repeating `NAME:WORD`. Every new or changed
consumer that calls two or more public words of one required package MUST
import it once, `using NAME … ;using`, and call them bare; scratch files,
reproducers, performance scripts and generated files included. Untouched legacy
consumers stay explicit debt until owner-scoped migration. PREFER `NAME:WORD`
for a one-off call or to escape a collision.

- `using NAME` consumes the next token and rejects a missing name, a name with
  `:` and an unknown package. It is valid at top level and inside an open
  package. Only the public wordlist joins the search; privates stay invisible;
  definitions still target the current scope's wordlist. A required file may
  open `NAME`; when the require returns, the consumer is back in its original
  scope.
- The scope ends at the matching `;using` (the `FOO … ;FOO` convention), at the
  enclosing `;package` for a `using` opened inside a package, or at the end of
  the load file, whichever comes first. `;using` closes the most recent `using`;
  one with no open `using` is an error. At most `USE-MAX` (16) concurrent
  usings; a further one is rejected. Consumer files close explicitly with
  `;using`.
- A package whose public tails are ordinary verbs cannot be imported:
  `using TCP4` refuses at the first bare `READ`, `WRITE` or `CLOSE`
  (`E-USING-SHADOW-GLOBAL`) because the global exists. Qualify such a package;
  uniquely named public surfaces are the fix, for packages as for words.
- Lookup for a bare tail: open-package scope (private, then own public) FIRST,
  then the global wordlist, then each used public wordlist. A tail in the
  open-package scope silently wins over a used public (inner scope wins;
  defining your own tail while a package is open is deliberate shadowing). A
  tail found in MORE THAN ONE used public wordlist is `E-USING-AMBIGUOUS`; the
  same package named twice is not ambiguous. `using` never silently changes an
  existing binding: it is the sole resolver of an otherwise-unresolved name, or
  a hard error.
- A bare tail resolving to a GLOBAL while a used package ALSO exports it is
  `E-USING-SHADOW-GLOBAL` (checker 7141) at the reference site. Without it the
  global-first order made the import silently dead: the reference bound the
  global's effect, and when the effects coincided it certified the wrong word
  with no diagnostic (the data-loader incident, dot
  `habu-err-on-global-e62f806c`: a kernel `LOAD` over a loader's public
  `LOAD`). The diagnostic names both candidates (`global TOK`, `PKG:TOK`) with
  arities. To mean the package word, qualify it (always certifies); there is no
  qualifier for the global "" wordlist, so RENAME the collision. The checker
  enforces this in every checked body; the engine's raw interpret and
  `0 set-check` keep global-first as the explicit unchecked boundary.
- The colliding global need not be one the checker knows: every engine-prefix
  colon word without signature or axiom, and every `0 set-check` definition, is
  such a global. The reference site asks the ENGINE's wordlists (`search-wl`,
  the engine's own scan and case fold) before a used public may bind (dot
  `habu-reject-a-bare-1f43a9a6`: a package public `FRESH` bound the checker's
  internal global `FRESH`, exit 0, wrong values). The open-package leg is
  decided the same way: a word in the open package's wordlist wins over a used
  public even when the checker never recorded it, and having no signature the
  reference is then `E-UNDEFINED`, not certified against the used public.
- Qualified `NAME:WORD` is unchanged and always available.
- Resolution happens at certify time: a call compiled inside a `using` scope
  keeps resolving after `;using`, and AOT/baked images carry no using-state.
  The engine's wordlists are the one authority on which scope claims a tail;
  the checker's symbol table answers only what a word's effect is. A checked
  body reading a used public certifies; a used private or an ambiguous tail is
  rejected before runtime; certification and execution name the same word. A
  global that appears AFTER a reference was certified does not change what it
  runs; the next reference to that tail is refused.
- `using` state is file-local: snapshotted per eval frame and REPL line and
  rolled back with the package scope, so a `using` left open in an included
  file, or aborted by a throw, never leaks to the caller.
- **A package word shadows the same-named global or primitive, and nothing
  reaches past it.** Inside `package TENDER` a bare `open` is `TENDER:OPEN`; in
  a checked body under `using DOC` a bare `close` is refused against
  `DOC:CLOSE`. There is no root-vocabulary qualifier: reach the operation
  through a differently named word (`OPEN-APPEND-FD`, the primitive's sibling
  `close-rc`) or rename the package word.

### Structures And Enums

New declarations use `NEWTYPE` for an opaque nominal cell, `STRUCTURE` for a
record with named fields, and `ENUM` for alternatives with or without payloads.
These landed forms register a whole family at top level; they cannot be called
inside a checked definition. `STRUCTURE` and full `ENUM` require an arity;
compact `ENUM` omits it and has no payload fields.

Legacy `SUMTYPE`, `PRODUCT`, `VALUE-RECORD`, low-level
`BEGIN-STRUCTURE`/`END-STRUCTURE` definers and counter enums (`ENUM+`, `ENUM4+`)
still have executable sites in the repository. They are migration debt,
forbidden in new code; use the declarations below.

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
The interpreter prompt refuses words with multi-cell inputs or outputs
(`hb: interpret-mode layout value`), including a word created by a `does>`
clause whose effect is multi-cell (`64 SPAN-BUFFER: PBUF`, then a bare `PBUF`):
a clause effect is published like any other effect. `evaluate` and
interpret-mode tick do the same, and bare `dup`, `drop`, `swap` at the prompt
move single cells. To
compute with a record at the REPL, define a word whose public effect is
single-cell and call it:

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

`2 3 DEMO:AT DEMO:FIRST` at the prompt is refused at `DEMO:AT`. Inside a
checked body a record binds to a local — untyped
(`: F ( pt -- n ) {: p :} p PT:UNMAKE drop ;`) or annotated with the family it
holds (`: ID ( pt -- pt ) {: p:pt :} p ;`). The local holds the WHOLE value,
whatever its cell count, and every reference reloads every cell, so a named
record survives a return, an `if` arm and a loop body intact. Keep a domain value
in one named local while passing it between words. Project or unmake it when
the fields supply the computation; unchanged-value transport uses the whole
local, as `CBIND:BIND` does for its target and numeric policy.

An annotation is read by the SIGNATURE type grammar: family arguments
(`{: r:res<n,n> :}`),
nested families (`{: o:opt<opt<n>> :}`), the definition's own declared type
variables (`( opt<a> -- opt<a> ) {: o:opt<a> :} o` certifies and keeps the
quantifier) and the same arity check. The one spelling only a local has is
`{: p:ptr :}`: an annotation is a single token, so the bare `ptr` means an
INFERRED pointee, and `{: p:ptr n :}` is two locals, not a pointee. The
annotation is asserted, not decoration: a wrong family, a scalar spelling
(`{: p:n :}` is `E-MISMATCH`, expected: n actual: @pt.tag<>), a wrong family
argument and a bare tail of a family of arity > 0 are all refused. See
[the multi-cell type rules](type-system.md#5-families-records-alternatives-and-generics).

- `NEWTYPE name arity` registers a nominal cell family (`TK-CELL`), no closer:
  arity `0` is an opaque scalar newtype (`lib/num-types.f`), arity `N` binds
  positional params.
- Family identity is the exact `(package, tail)` pair. A package family may
  share a tail with a global family or another package's, even at different
  arities. A qualified token resolves only its package row; a bare token
  resolves the open package's own row first (private or public), then the
  global row, then the sole eligible public row from other packages; two
  eligible non-lexical package-public rows are an error. So adding `MEM:span`
  cannot change what a top-level `span` means. Same-package duplicates,
  reserved grammar names and foreign private rows reject.
- `STRUCTURE name arity [ header… ] FIELD f type … ;STRUCTURE` declares a
  single-shape record with named fields in declaration order, deepest field
  first on the stack. A structure with fields generates sealed `MAKE`/`UNMAKE`:
  `PKG-FAMILY:MAKE` in the constructor namespace for a public package family,
  `FAMILY-MAKE` in the declaring package's private wordlist for a private one
  (a qualified name can never land in a private wordlist). A fieldless structure
  is an opaque cell family with no
  generated constructor or destructor.
- Full `ENUM name arity [ header… ] VARIANT v FIELD f type … ;VARIANT … ;ENUM`
  declares named alternatives with named payload fields. A variant may have no
  fields. Its tag is its declaration order; generated constructors feed an
  exhaustive `MATCH … ;MATCH`.
- Compact `ENUM name [ header… ] v0 v1 … ;ENUM` declares payloadless variants
  from bare names, with implicit arity zero. Do not mix compact names with full
  `VARIANT` blocks. `POLICY` and `DERIVE` headers precede the first field or
  variant, after the arity when one is present.
- `DERIVE addr` on a structure with fields generates typed field-address
  accessors plus `AT`, `BYTES` and `CELLS`. For example, a global public `point`
  with `FIELD x n` gains `POINT:X ( ptr point -- ptr n )`. Read or write through
  it with the field type's ordinary checked operations. See
  [type-families.md](type-families.md) for
  layout policies and the generated storage interface.
- **A package family's generated words are `PKG-FAMILY:tail` with every hyphen
  in the family name doubled**: an `ENUM read-result` in `package TCP4`
  constructs its `data` variant through `TCP4-READ--RESULT:data`; a
  `STRUCTURE captured` in `package PCAP` unmakes through
  `PCAP-CAPTURED:UNMAKE`. `SIGNAL-RESULT:signal`
  and `SIGNAL:signal` are both `E-UNDEFINED`. Stack effects and `MATCH` name
  the family `PKG:family` (`( -- TCP4:read-result )`, `MATCH TCP4:read-result`);
  a body inside the owning package writes the bare family name in `MATCH`.
- **An OPEN family instance is placeable when the width cannot read the open
  argument.** `STRUCTURE span 1 FIELD base ptr a FIELD len n ;STRUCTURE` is two
  cells for every argument, so `: SKIP ( span<t> n -- span<t> ) …` compiles at
  tier 1 exactly like the `span<u8>` row. A family whose parameter IS a payload
  cell (`FIELD it a`) has a width its argument decides: a row carrying an open
  instance of THAT below another value is `E-NELAB-BUNDLE` (-8519, `ncomp:
  cannot compile NAME`) until the argument is concrete. No width is ever
  guessed — the registry answers which argument slots the width reads
  (`src/core/type-family.f TFAM-WIDTH-SLOT?`) and the checker expands an
  instance into its cells only when no open slot is one of them
  (`src/core/checker.f LAYOUT-WIDTH-OPEN?`). A `create … does>` clause carries
  the same rule: its rows are rows, with the value boundaries of a definition's
  own (`DOES-IN-SLOT` / `DOES-OUT-SLOT` on the owner ABI), so a clause yielding
  a multi-cell value compiles at tier 1 and the created word runs
  (`n SPAN-BUFFER: NAME`, lib/span.f). Only a clause row carrying an instance
  whose width reads an open argument has no per-cell boundary to place, and
  that one is `E-NELAB-BUNDLE` at the definer.
- `DERIVE eq`/`DERIVE hash` on a public arity-0 family generate those
  operations. Clauses follow the name on compact `ENUM` and the arity on
  `STRUCTURE` or full `ENUM`; repeating a feature rejects.
- `CAST: NAME ( source -- destination )` declares a checked retype: a reader
  keyword with no body and no `;`, publishing `NAME` as an identity whose call
  sites emit nothing. A conversion that can refuse is a checked word that
  throws, then the cast. A resolved scalar-cell family destination, including a
  parametric `NEWTYPE` instance, may be introduced only while the engine's live
  namespace record and actual definition wordlist identify the declaring
  package; mutable `CHECKER-PACKAGE-*` mirror state is not authority.
  Projection casts from such a family are unrestricted.
- Type, field and variant names are lowercase; generated and project words are
  uppercase.
- **Raw storage never holds an address.** A `variable`, `create` or `constant`
  cell, and any cell a `create … does>` definer makes, holds scalars, roles and
  atoms. Storing a pointer into one, fetching one out, or `ptr-field` over one
  is `E-RAW-CELL-PTR` (repair class `declare_pointer_cell`):
  `variable V : PEEK ( n -- n ) V ! V @ @ ;` does not certify. The declared
  forms: `PTR-VARIABLE` (effect `( -- ptr ptr a )`, in place of `variable` plus
  `0 ptr-field`), `PERSISTED-PTR-VARIABLE`, `TYPED-VARIABLE NAME ptr t`,
  `TYPED-BUFFER NAME ptr t`. See [effects.md](effects.md) "Raw storage never
  holds an address" for the open hole.
- **Nor an execution token.** The same cell, and either base address, refuses a
  quotation: `variable ZQW : ZQWQ ( -- ptr [ -- n ] ) ZQW ;` and
  `( -- ptr [ -- n ] ) data-base 8 +` are `E-RAW-CELL-PTR` with the reason "an
  undeclared cell cannot hold an execution token / a quotation" and repair class
  `declare_xt_cell`; before the rule they certified and the fetched value was
  executed at whatever integer the cell held. The declared forms:
  `TYPED-VARIABLE NAME [ in -- out ]`, a `TYPED-BUFFER`/`DYNAMIC-BUFFER` of
  `[ in -- out ]`, `defer`/`is`, and `xt!`, which declares the cell it writes.
  The cost is the null comparison on a declared code cell (`HK NULL-PTR =`),
  which is refused now; read such a cell's emptiness through a number-typed
  accessor of the same address.
- **A definer that only wants a type writes an EMPTY `does>` clause.** `does>`
  runs after the created word pushes its address; on a fresh word it is elided at
  both tiers, so a read costs the one load a bare cell costs. Spelling
  `0 ptr-field` in the clause is a body and pays a call, a branch and a frame
  per read.
- **A definer may replace another definer's `does>` clause.** Calling the inner
  definer creates the word; a nonempty outer clause then replaces its behavior and
  declared effect. Minimum input depth and the interpret-mode wide-value guard
  follow the replacement effect, including zero inputs or a scalar result.
  An empty replacement removes the earlier clause and restores the created
  word's original body.
- **A `STRUCTURE` or `ENUM` body is parsed by its definer.** A `\` comment
  inside the body is refused with `E-BAD-DECLARATION`; put comments above the
  opener, including comments explaining the header or fields.
- **A `FIELD` holds a value, not a body.** A payload is a type token (letter
  param, concrete cell type, `ptr T`, closed arity-0 family); a quotation type
  (`[ a -- b ]`) is `E-TDECL-SYNTAX` (7109). A record that describes a
  behaviour keeps its data fields and stores the quotation beside it in a
  `TYPED-BUFFER NAME [ a -- b ]` indexed the same way, written and read together
  by the owning words; such accessors take an index.
- SwiftForth-style relocatable list words (`@REL`, `!REL`, `,REL`, `>LINK`,
  `<LINK`, `CALLS`) are outside the checked surface: dictionary-relative pointer
  arithmetic and executable traversal are wrong for snapshots and the checker.
  Use structures for node layout, arrays and maps for collections,
  `case/of/endof/endcase` for dispatch and checked execution vectors for late
  binding. A future list DSL exposes typed node/link effects and forbids raw
  relative arithmetic at its boundary.

## Words & factoring

- Separate multiline definitions with two blank lines; related one-liners may
  stay together; a word's comment sits directly above it, after the blank lines.
- **Write checked, typed Habu**: small typed words with real `( in -- out )`
  effects, composed into checked DSLs that read as the domain, not as stack
  plumbing. A giant word, deep juggling or a raw `s"` blob means build the typed
  words first.
- **Default new public and library Forth to checked typed definitions**
  (`: SQUARE ( i64 -- i64 ) dup * ;`).
- **Guard dependent operations with control flow.** `and`/`or` combine values
  already evaluated; establish shape, owner and bounds with `if … exit then`
  before indexing or reading a dependent field. Combine predicates only when
  each is safe alone.
- **A local is bound once.** A value that changes per loop turn lives on the
  stack or in a cell: `begin {: cursor:n :} … cursor' again` binds at the top
  and pushes the next value before the back edge, so the stack at `again`
  matches `begin`. `RECURSE` for a cursor only where a limit bounds the depth; a
  peer-controlled depth, such as bytes arriving on a connection, overflows the
  task's stack.
- **Keep control flow and multi-step computation out of argument lists.** The
  checker accepts `s" k" 1 0 > if 5 else 6 then 2.0 L-OF`; the rule is
  readability and safety. Extract the value into a named word
  (`: DET-MINRATE ( -- r ) … ;`) or a local. Dense lists that splice `if/else`,
  comparisons and several `@`/`F@` reads hide a wrong cell type (a `bool` from
  `0 >` where `n` is wanted) that surfaces at a later call as
  "expected n actual bool". One value per concept.
- **Check every source you can, byte emitters included**: ELF/Mach-O writers,
  tooling, tests, build helpers. Prove a checker gap with the owning
  `bin/hb --load` or `tools/check.f --source-list` before claiming one.
- **Predicates and selectors execute real bodies.** A declared effect states no
  runtime fact and defines no word owned by another file.
- **Keep helpers and dispatch checked** with typed quotation effects; repair a
  primitive interface rather than add a `TRUSTED:` shim or unchecked caller.
- **Build checked task vocabulary before fighting syntax.** Structured rows,
  JSON/TSV, generated source, diagnostics, packets, repeated assertions: factor
  domain words or a checked DSL first. Giant `s"` literals, fragile escaping and
  private byte emitters are bugs unless they are that DSL's tested boundary.
- **Readable DSLs execute the body they name** (`[: ITEM ;] NAME-FILES`,
  `TEST:SUITE name … TEST:;SUITE`), not generic `execute` wrappers whose
  higher-order effects the checker may not model.
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
- **Split multi-pass words into named passes**: cursor movement,
  classification, validation, state update and rendering as checked words with
  their own effects plus a small orchestration word.
- **No dense one-line control words.** One line is for a trivial straight-line
  wrapper; anything with `IF`, `BEGIN`, `WHILE`, `REPEAT`, `UNTIL`, `case`,
  locals, several stack transitions or more than one step spans lines. Line
  effects only where they clarify; a noisy word is factored.
- **Raw compiler and emitter code is not exempt**: exact effects and small
  helpers; if review needs reconstructed stack state, factor first.
- **Factor when the stack gets unreadable**: `ROT -ROT PICK ROLL` means a helper
  or locals; we hold our own code to what we forbid user code.
- **Locals `{: a:type b:type :}`** remove juggling. They bind inputs only, never
  `-- outputs`; the effect stays in the stack comment. Binding is left to right
  from the deepest item: `1 2 {: a:n b:n :}` gives `a`=1, `b`=2. Type new locals
  when the concrete type is known; a bare name only where the entry effect keeps
  richer role detail the annotation cannot express, or the typed capability is
  documented missing.
- **A local name is at most 16 bytes and a definition binds at most 64.** Past
  either the compiler refuses before storing anything, exit 70, naming the token
  (`hb: local name over 16 bytes: <token>`,
  `hb: more than 64 locals in one definition: <token>`); when the checker sees
  it first (tier 1, the check tool) it reports `E-LOCAL-NAME-TOO-LONG` or
  `E-TOO-MANY-LOCALS` with width and limit. Shorten or factor.
- **A `ptr` annotation does not fit a byte span.**
  `( ptr u8 n -- n ) {: p:ptr :}` is refused at `:}`
  (expected: ptr n actual: ptr u8 n). Keep the detailed type in the effect and
  bind a bare local for a body that uses `c@`/`c!`, or factor a helper whose
  entry carries `( ptr u8 … -- … )`.
- **Name same-type numeric slots before reordering them.** In
  `( cap used add -- )` a stray `swap` type-checks; bind names at entry or
  factor role-specific helpers before capacity, offset or decoder arithmetic.
- **Locals are block-scoped.** A `{:` group may appear on any live path, inside
  `if`/`else`, `case` arms and loop bodies; the names die when that arm closes
  and the prior scope and frame depth return. Never reference a branch-local
  after `then`/`endof`/`endcase`/`loop`/`repeat`; bind before the control word
  to survive the join.
- **Dead code cannot bind locals.** A group after a closed early-exit guard
  (`dup 0 < if exit then`) is valid, the fall-through is live; a group right
  after an unconditional `exit`, `leave`, `throw`, `die` or `again` is a checker
  error.
- **No deep locals stacks.** Locals are for shallow factoring; nested helper
  calls from loop or callback bodies use stack leaf helpers or separate scratch
  cells so inner helpers cannot clobber caller indexes.

## Files

- **One concern per file**: parser, renderer, DB, data table and driver are
  separate files, split at responsibility boundaries.
- **Reusable helpers live in libraries.** Run multi-file tools as
  `hb --load lib/a.f lib/b.f tool.f -- args…`: sources before `--`,
  `SCRIPT-ARGV$` after it, fd 0 still tool data when stdin is not a tty. Shared
  behaviour lives in one owned file.
- **Keep physical lines short.** Factor long `--load` builders and check-source
  appenders; a line near the interpreter input buffer truncates and surfaces
  later as unrelated top-level words.
- **Script argv is explicit.** `hb tool.f arg…` treats `arg…` as script
  arguments; read them only after `SCRIPT-ARGC`, since `SCRIPT-ARGV$` for a
  missing argument faults today instead of throwing (dotted). Use `--load` only
  with more than one source file.

## Stack comments

- **The stack effect is the contract; prose is not.** Keep every
  `( before -- after )` current. Comment only what the code cannot state (units,
  ownership or ordering invariants, a hardware encoding, a checker boundary), in
  one or two lines. No design essays, rationale or refutation history: that
  belongs in documentation. Meaning lives in names and factoring.
- **Every definition** carries `( before -- after )`; body lines carry a
  trailing `\ ( before -- after )` only where the stack state is not obvious. No
  empty stack comments; many line comments mean factor.
- **Checked definitions use type tokens only**: `( n n -- )`, `( bool -- )`,
  `( ptr u8 n -- )`, never role prose such as `( got want -- )`. Nominal roles
  such as `idx`, `len`, `count`, `fd`, `rc`, `reg`, `label`, `va`, `symidx`,
  `asm`, `img` and `snap` are real types; informal names go in locals
  (`{: got want :}`), helper names or prose.
- **Real types, not reflexive `n`.** A string is `ptr u8 n`; a dereferenced cell
  address is `ptr a`; a pointer-valued cell keeps its nested pointer role; `n`
  is a genuine scalar. `ptr a` is only for a body that keeps the pointee
  parametric: reading the cell as a number makes it `ptr n`, and so does
  returning a raw cell view of allocated bytes
  (`( -- ptr a ) 64 MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop CELL-VIEW` is
  `E-NONPARAMETRIC-EFFECT`; declare `( -- ptr n )`).
- **Compare an enum with its family's derived `EQ`**, never raw `=`.
- **A `CAST:` mints a foreign nominal only in its owner**:
  `CAST: >SLOT ( n -- slot )` outside `package DOC` is `E-CAST-OWNER` (7135);
  projecting out (`slot -- n`) works anywhere. Store the projected identity and
  resolve it back through the owner's public words.
- **Reserved names cover constants and variants.** `MATCH` cannot name a package
  constant even under `public`; a `VARIANT` named by a reserved or taken word
  fails "name is reserved or already taken" (`VARIANT x`). Pick names that
  collide with nothing visible (`horizontal`, `vertical`).
- **Same-cell values need nominal roles** (`reg`, `label`, `va`, `symidx`, `fd`,
  `count`, `asm`, `img`, `snap`) with negative fixtures; `( n n -- )` hides
  swaps.
- **Declare application nominals with `DEFTYPE`** (`require lib/type/deftype.f`):
  top-level `DEFTYPE NAME` mints a package-scoped type with converters
  `>NAME ( n -- name )` and `NAME>N ( name -- n )`, the only crossing; the type
  tail is the lowercase fold (`SERIAL` → `serial`, so `( serial -- n )`);
  `DEFTYPE SERIAL` in `CAMERA` and in `FRAME` are distinct; unknown tokens stay
  errors, so a misspelling never mints a type. Substrate and the rejected global
  table: `docs/value-nominal-substrate.md`.
- **`DEFLINEAR` for owner and lifetime tokens.** A linear token is nominal and
  noncopyable: `dup`/`over`/`2dup`, `drop`, `@`, `!` and by-value record
  duplication reject when they would duplicate, discard, load or store it; only
  words whose effect names the linear type create or consume it, so allocation
  and free boundaries stay audited.
- **Raw role casts are not validators.** `>LEN`, `>IDX`, `>COUNT`, `>OFF`,
  `>ASM`, `>IMG`, `>SNAP` are trusted identity boundaries; libraries expose
  checked constructors and role helpers so swaps fail under `CHECK!`.
- Unchecked prose-only comments may name roles when no hook consumes them; keep
  the shape obvious. Add inline `( … )` at non-obvious points in a longer word.
  Standard notation: `x` cell, `n`/`u` signed/unsigned, `d` double, `c-addr u`
  string, `xt`, `nt`, `f`/`bool` flag, `?` maybe-present.

## Checker & type model

- **`CHECK!` is the user contract.** `CHECK` proves internal consistency; user
  builds verify the body against the declared effect and make rejection fatal.
  Tests for bad programs assert build rejection, not runtime failure.
- **Fix missing models at their owner.** A rejection never authorizes TRUST;
  reduce the case and repair the primitive effect, declaration or checker while
  still rejecting invalid programs.
- **Typed booleans are `bool`**: produce them with `0 0=`, `0 0= 0=` or domain
  helpers; never store raw `0`/`-1` in a `ptr bool` cell or compare bools with
  `=`.
- **A quotation cannot read a local.** A value a `catch`, `finally` or locked
  body needs travels through storage; where it differs per task, that storage
  is the task's own slot (a `TASK:+USER` cell or a typed-buffer row indexed by
  the task) that the quotation reads for the running task.
- **A handle over caller-owned storage is a public `STRUCTURE` plus a
  `TYPED-VARIABLE` or `TYPED-BUFFER` in the caller**, a checked `ptr PKG:type`.
  No `TRUSTED:` mint, state and consume leaves: `CAST:` refuses a pointer
  operand (7130 `E-CAST-CLASS`) and a linear one (7137 `E-CAST-LINEAR`), so a
  linear token over caller storage is not expressible; trade the type-level
  lifetime for a runtime refusal off the definer's zero image
  (`lib/json-write.f`). Until a checker-owned linear mint exists, dot
  `habu-mint-and-erase-72e83e7a`; the bullet goes when it lands.
- **Structural integers widen, roles do not.** `u8 -> u16 -> u32 -> n/cell/i64`
  widens implicitly when lossless; narrowing and same-width sign changes need an
  explicit conversion; nominal roles (`idx`, `len`, `fd`, `rc`, `pid`, `asm`,
  `img`, `snap`, …) never widen to each other or to bare integers.
- **Pointer-valued cells use cell-indexed `ptr-field`**: the index is a cell
  slot, not a byte offset, so `@`/`!` keep nested pointer types; raw byte
  offsets need a checked view or a modeled byte-offset primitive; the base must
  be a declared cell, since `ptr-field` over raw storage is refused at the
  token.
- **Byte pointers are not cell pointers.** `ptr u8` is a byte span read with
  `c@`/`c!`; cell `@`/`!` over a concrete `ptr u8` is a checker error. A cell
  that stores a byte pointer is `ptr ptr u8` through `ptr-field`, then `@`/`!`.
- **State cells need typed public effects**: `-- ptr n`, `-- ptr bool`,
  `-- ptr ptr u8` plus a separate length cell for strings; never TRUST rows.
- **Path-sensitive control is a checker invariant.** `LEAVE`, `EXIT`, `throw`,
  `die` and `again` fold or kill paths per their control effect; divergent path
  arities are soundness bugs; after a dead path only structural closers
  (`else`, `then`, `loop`, `+loop`, `repeat`, `again`, `;]`) may follow.
- **Discharge counted loops before `exit`.** Each `do`/`?do` opens a frame;
  `unloop` removes the nearest; an `exit` needs one `unloop` per active loop in
  that definition or quotation; live branches agree on the remaining frames; a
  back edge or `leave` still owns its frame. `i`/`j` read the nearest and next
  frames of the current quotation or definition, never an enclosing
  quotation's; loop frames are separate from the typed return stack. A `do`
  whose every body path returns or throws has no normal continuation; `?do`
  keeps its zero-trip exit; `leave` is the explicit exit. `+loop` adds its step
  wrapping and ends only when the index crosses between limit-1 and limit in the
  step's direction (Forth 2012 6.1.0140): equal bounds run once with a negative
  step and a whole cycle with a positive one, unlike `loop`.
- **`RECURSE` uses the declared effect**, a fresh copy per call; keep the raw
  declared signature stable after `CHECK!`.
- **Quotations are xts, not closures.** `[: … ;]` cannot read surrounding
  locals; on the JIT tier a `{:` group inside `[: ;]` is refused; until nested
  quotations land only one `[:` is open at a time, and a second one refuses
  `hb: a quotation may not open inside a quotation: <name>`, rc 75, catchable
  inside `evaluate` — sequential quotations in one definition are fine; a body
  that needs locals inside a quotation becomes a named private word. Checker and
  compiler reject local references while a quotation is open.
- **Checked `catch` is quotation catch**: `[: WORD drop ;] catch`, consuming
  success outputs inside and keeping the exact code as data at an explicit
  recovery boundary; no arbitrary-xt catch. The quotation is stack-preserving,
  because `catch` unifies the live stack with its inputs AND outputs:
  `( n -- n )` is accepted, `( n -- )` rejected. A value the caught code needs
  travels on the data stack through the quotation and back on every branch:
  `: W ( n -- n ) [: dup USE … ;] catch {: rc:n :} CLEANUP rc 0 <> IF rc throw THEN ;`.
  No staging variable.
- **`finally`** is `( R [ R -- S ] [ -- ] -- S )`: body, then cleanup on return
  or catchable throw; cleanup takes and leaves nothing; a body error rethrows
  after cleanup, a cleanup error supersedes it; `die` skips cleanup. Implicit
  tails such as `[ -- ]` enforce their windows through wrappers and typed
  storage; name rows explicitly for generic callbacks (`[ R -- S ]`).
- **Higher-order signatures publish themselves** once `CHECK!` passes (`DIP`,
  `KEEP`, row callbacks); no TRUST row to pin a scheme.
- **Function passing is checked.** A quotation parameter (`[ a a -- bool ]`,
  `[ a -- a ]`) is verified through a call chain AND inside a `?do`/`begin`
  loop: bind it as a local, thread it, `execute` it; heapsort, map, fold and
  filter all check. `src/core/combinators.f` (MAP/FOLD/EACH) is an unchecked
  boundary that predates this, not a model. A rejected valid shape is reduced
  and fixed in the checker, callers still checked.
- **Execution vectors are typed `defer` words.** `defer ACTION ( in -- out )`
  declares the effect; `: INIT ( -- ) [: IMPL ;] is ACTION ;` installs it, the
  checker proving the quotation matches exactly. No `variable`/`@ execute`
  tables, no `['] IMPL is ACTION`: raw xt storage loses the effect. An unset
  deferred word fails closed with the execution-vector error. A fixed engine
  callback cell stores one checked bridge (`[: ACTION ;] CELL !`) and changes
  only through `[: IMPL ;] is ACTION`. `@EXECUTE` is no replacement until its
  zero no-op has a checked model.
- **New type tokens need a checker-only bootstrap stage**: old `bin/hb` rejects
  unknown stack-comment tokens, so add parser, renderer and `CC-*` support,
  refresh the native binary, then use the token in axioms and definitions.
- **Phase tokens reach the side effect they order**: `asm`, `img`, `snap` flow
  through the final sign/write/header operation, not an early wrapper, so no
  caller skips a stage.
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
  before `lib/`). It is a catchable throw, not an exit, because the divisor came
  from the program's own arithmetic: `[: a b / drop ;] catch E-DIV-ZERO = if … then`.
  The guard is the cold side of a compare-and-branch already there to stop
  arm64's `sdiv` answering zero. A structurally positive divisor says so in the
  type: `lib/num-arithmetic.f`'s `positive-divisor` role makes the refusal
  unreachable. **The contract holds at every tier**: the interpreted primitive,
  a word compiled with `1 set-tier`, and an AOT-built executable all throw the
  same code, so a program may catch it wherever it runs.
- **`MIN-N -1 /` is `MIN-N` and `MIN-N -1 mod` is `0`.** The quotient `2^63` has
  no cell, so it wraps like `+`, `-`, `*`; a second refusal would cost every
  division a compare, and no caller in the tree can reach it (every divisor is a
  positive literal, a positive named constant, a count or a
  `positive-divisor`). A backend whose divide traps on this quotient (x86_64
  `idiv`) tests for the `-1` divisor and answers `(MIN-N, 0)` without executing
  it. `test/prim-parity.f` pins both contracts.
- **`.` prints every cell, `MIN-N` included**: the signed printer negates and
  divides unsigned, since `MIN-N` negates to itself and a signed divide there
  wrote bytes below `'0'`; `FMT:SB-INT` reaches the same value through a
  canonical digit table.

## Errors

- Engine process failures use only the sealed `ENGINE-ERROR` package ABI:
  `SEAL-VIOLATION` 83, `SEAL-PACKAGE` 84, `BAD-TAG` 85, `CALLABLE-ABI` 86,
  `CATCH-STACK` 87, `CODE-CERT` 88. No global `E-*` aliases; native and
  no-binary recovery consume the same qualified names and values.
- **Fallible words `throw` a named code** (`src/config.fs`, e.g. `E-MISMATCH`),
  never a silent failure or an out-of-band flag.
- **`catch` only at explicit recovery boundaries**: REPL/CLI wrappers, test
  assertions, stack-preserving outcome adapters returning the exact code. No
  `… catch drop`, no `catch 2drop`, no masking.
- `abort"` only for proven-impossible states, with a message.
- **Interactive support recovers; builders may exit.** Recoverable interactive
  failures `throw` into REPL recovery (`?`, rollback, reread); `die` is for
  build-time makers and CLI boundaries where exiting is the contract.
- **`throw` and `die` are different control effects**: `throw` is catchable,
  the checker's exception edge; `die` terminates, no-return metadata. Never add
  dummy outputs after `throw` to balance a branch; fix the exception model or
  track the gap.
- **`die` consumes a real message and code**, `( ptr u8 n n -- )`, never `0 0`
  as a fake string; model exits as no-return only at certified wrappers.
- **A word that never returns ends its branch.** After a call whose body ends in
  `die`, the checker refuses an `exit` in the same `if`:

  ```forth
  ARG s" child" TOK= if CHILD-MAIN exit then
  \ habu: in dispatch: at 'exit' after 'CHILD-MAIN'
  \ hook: non-certified definition: dispatch at 'exit'
  ```

  Write `if CHILD-MAIN then`: the rest of the definition runs only on paths
  that can return.

## Engine limits ordinary source reaches

Three ceilings are reachable from plain Habu rather than a runaway; each refuses
by name with the count it saw and the ceiling, and none truncates.

- **A definition's captured source text: `BODYBUF-CAP`, 8000 bytes**
  (`src/habu/layout.f`): every token of a body (name, stack comment, words,
  string literals with closing quote) plus one separator each, because the
  check hook certifies the text that was compiled, not a summary. Nine 900-byte
  `s"` arms in one `case` reach it. Past it:
  `hb: definition body text full at 8000 bytes: <name> needs <count>`, rc 71,
  catchable inside `evaluate`. Repair: move the long literals into words of
  their own. The same constant bounds the source verifier's body buffer
  (`E-VS-BODY-CAP`) and the native compiler's unit text (`E-NCOMP-TEXT`).
- **One REPL line: 255 bytes** (`src/habu/repl.f` `LLINE-MAX`; the line lives in
  256 bytes and a history slot spends its first byte on the length). A longer
  line is refused, `hb: repl line over 255 bytes: <length> typed`, and read
  again, never truncated, evaluated or saved to history. Load long definitions
  from a file.
- **`begin` nesting in one definition: `JIT-SNAP:FRAMES`, 28**
  (`src/habu/layout.f`), the JIT's value-stack snapshot frames per definition.
  Past it: `hb: BEGIN nesting full at 28 frames: <name> needs <depth>`, rc 75.
  Factor the inner loops into their own words.

## Constants

- **Named constants, no magic numbers.** Limits and codes live in
  `src/config.fs`; a literal is acceptable only for a true primitive of the
  encoding (the `3`/`7` of the 3-bit tag), with a comment.
- **Default to `$hex`**: masks, instruction encodings, ASCII codes, memory,
  struct and byte offsets, field strides (`$FF and`, `$D10043FF`, `$200`,
  `$40`). Only genuine small counts stay decimal: loop bounds, arities, shift
  amounts, register indices. Crypto and format constants follow the spec's hex
  spelling. The standalone parses `$hex` case-insensitively with an optional
  leading `-`.

## Testing

- Exercise changed behaviour through checked assertions, meaningful errors and
  edges included, through public entry points rather than a test per trivial
  helper. `T=`/`T<>` for scalars, `T$=` for strings, `TTRUE`/`TFALSE` for
  flags, `TTHROWS` for codes; several results compared top down, one assertion
  each. A regression distinguishes the defect it prevents.
- Tests live in the native gate: `test/engine-suite.f`, focused
  `tools/*-test.f` fixtures, and source-specific checks wired through
  `test/run.f`.
- Orchestration uses `lib/test.f`: suites, groups and tests (gate/row wording is
  legacy). Adapters provide setup/teardown, argv/env policy, filters and
  process execution; test files require their own dependencies; groups are
  named, parallel or sequential; reports print group/test name, state and
  timing.
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
  or another stack-preserving `catch` with the exact code; top-level scripts
  that cannot push quotations use `' WORD TTHROWS`; diagnostics are captured and
  matched by substring.
- Run focused fixtures with their owning `tools/*-test.f`, and the full native
  suite when the impact warrants it (below).
- Property generators use `lib/property.f`: `PROP:SEED!` makes a run
  reproducible, and `PROP:RND%` mixes the generator's high bits before bounding
  a draw. Small bounds do not inherit the raw generator's short low-bit cycles:
  bound 2 produces repeats as well as alternations. The fixed-seed regression
  checks that each occupies 40–60% of 4096 transitions; this is a coverage
  guarantee for that sequence, not a claim of independent or cryptographic
  randomness.
- **False-reject claims need execution proof**: run an unchecked copy and show
  the measured stack behaviour matches the declared effect before counting a
  checker limitation. Generator bugs become rejections, not certifications.
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

- `\` line comments, terse; no restating the code. Remove scratch and debug
  prints before commit.
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
  source search where it answers, native inspection where runtime state
  matters.
- **Boundary spawns attribute failures.** A gate, test or tool spawning `hb` or
  another child uses outcome capture for expected timeouts and failures, never
  throw-only capture that collapses to a shell rc; the report carries suite/case
  label, phase, executable and argv/load list, outcome kind/code, named rc when
  known, capture bytes/capacity and captured stdout/stderr. Throw-on-timeout
  capture belongs only in a unit test asserting that throw.
- **`DYNAMIC-BUFFER NAME Type` for growing typed tables**: same stored types as
  `TYPED-BUFFER`, closed non-linear layouts included. `count NAME-RESERVE`
  allocates at least that many elements and keeps contents; `index NAME`
  answers `ptr Type`, rejecting negative and beyond-capacity indices; a smaller
  reserve keeps the allocation; growth may move it, so retain indices and
  reacquire pointers; a growing reserve copies the whole old capacity, so stale
  cells beyond the old count survive and the accessor bounds by capacity, not
  the requested count: a column whose zero is a default is cleared over the
  newly exposed range by the reserving word. `NAME-RELEASE` frees the mapping
  and is safe to repeat. Mappings are transient: release before saving an
  image; image-retained values use dictionary storage.
- **Large tool bundles are supported.** Never split tools to dodge DATA
  pressure. `create … allot` is dictionary-sized static storage; runtime-sized
  buffers use `lib/memory.f` (`MEM-ALLOC-BYTES`, `MEM-ALLOC-64K-BUFFERS`),
  scaling with OS mappings rather than `DATA-SIZE`. `create`, hence `variable`,
  rounds the data pointer to a cell before publishing, and `align` on request,
  so a `create … allot` block and `MEM-ALLOC-BYTES` memory are both
  cell-aligned for a cell-typed reader such as `lib/json-read.f` `INIT`;
  `allot`, `c,` and `,` never realign, so storage carved after byte-sized data
  is misaligned unless `align` precedes it, and such a reader refuses it
  (`E-STORAGE`). Tools keep as many 64K buffers and spans as needed, one
  contiguous `MEM-ALLOC-64K-BUFFERS` span or many; the only limits are
  cell-size overflow checks and explicit OS allocation failure. If composition
  still hits capacity, fix the shared memory model and add a regression for the
  composed load.
- **Missing convenience words are not bugs in the standard.** Core lacks
  `pick`, `within`, `s>number?`, `move`, `fill`, `erase`, `bl` and `*/`; each
  is `E-UNDEFINED` in a checked body (`: W ( -- ) bl ;`). Use cells, explicit
  increments and comparisons, and the checked byte and string helpers. `0<>`,
  `true`, `false`, `fdup`, `fover`, `fdrop`, `f<=` and `f>=` come from
  `lib/prelude.f`, which the engine already provides: writing
  `require lib/prelude.f` documents the dependency but is not load-bearing.
  Never re-derive `0 0=` / `0 0= 0=` by hand.
- **Primitive effects are assumptions, not proofs**: each genuine primitive's
  axiom describes its actual behaviour with focused coverage through its real
  call path; ordinary Forth stays checked.
- **Typed pointer fields use cell indexes**: `ptr-field` builds a `ptr ptr x`
  field, then `@`/`!`; never multiply by cell size; byte-offset header access
  goes through checked views with explicit alignment and bounds; a missing
  primitive model is fixed, never cast around.
- **Tool libraries keep checking enabled** for themselves and their callers;
  when removing a legacy unchecked span, keep hook restoration until it is gone.
- **Pre-checker bootstrap stays minimal**: install checking as soon as the
  checker exists; never extend the startup exception to generated application
  code or load ordinary Forth unchecked.
- **Legacy checker preludes rebind the existing hook**: a prelude that disables
  checking after `src/core/check-hook.f` restores it at once with
  `' HOOK set-check`. A migration constraint, not permission for new spans. No
  second hook name in baked tty/stdin bundles (duplicate enforcement fails
  closed at startup); a snapshot or AOT stage keeps a different hook local and
  leaks no duplicate REPL hook into `bin/hb`.
- **Bootstrap and fixpoint temp roots are explicit script args** after `--`; no
  stale seed envp capture; generated paths stay under that root; the build
  driver owns path construction.
- **Escaped literals for readable snapshots**: `S\"`, `C\"`, `.\"` accept
  C-style escapes (`\\`, `\"`/`\q`, `\n`, `\r`, `\t`, `\xNN`, `\z`, …) for
  direct JSON/source expected strings; generated syntax from fields uses checked
  byte/field helpers or `lib/json-write.f`.
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
  duplicates, which hides stale process state.
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
  entry goes only after the callback returns normally, so a release that
  refuses must not clear its own registered flag before rethrowing, or the next
  `REGISTER`/`PREPARE` adds a second entry. Follow `lib/net/udp4.f`
  `REGISTER-CLEANUP`: set a private flag only after `IMAGE-LIFECYCLE:REGISTER`
  completes, so a throwing path leaves flag and registration consistent.

## Native Forth Gotchas That Shape How We Write Code

Build and environment rules are in [bootstrap.md](bootstrap.md) and
[gate.md](gate.md); these affect coding.

- **Case-insensitive dictionary**: collision risk (see Naming).
- **`[']` is compile-only**; interpreted tests use `'` (`' WORD catch`).
- **Control words and ticks are compile-only**: `if`/`else`/`then`,
  `begin`/`while`/`repeat`, `[']`, `i`, `?do` and `;` live inside a `:`
  definition, never at top level.
- **A `begin <cond> while <body> repeat` condition may only add a flag.** The
  stack under the flag at `while` equals the stack at `begin`; a condition that
  net-produces carry values (`a u NEXT-TOKEN` leaving a span under the flag) is
  rejected at `repeat`. Establish loop-carried values before `begin`, or move
  the production into the body behind a peek-only flag.
- **A no-`else` `if` is stack-neutral**: a true branch that changes depth fails
  the merge at `then` (`expected: … actual:`); bind the consumed value in a
  local before the `if`, or add `else drop`.
- **Malformed control syntax is a rejection, not `uncheckable`**: orphan
  closers, unterminated frames, `i`/`j` outside enough loops and `leave`
  outside a loop make `CHECK!` return `0`; `uncheckable` is for modeled-word
  gaps.
- **`case/of/endof/endcase`**: selector before `case`, key before `of`, arm
  before `endof`, default before `endcase`. `of` compares with the preserved
  selector; matched arms consume it; the default keeps it until `endcase` drops
  it, so a value-producing default leaves the selector on top (`30 swap endcase`).
  Keys and selectors are integers; every live arm and the default unify to one
  data/return-stack effect.
- **A local may follow a closed early-exit guard**
  (`dup 0 < if exit then {: x:n :}`); one after an unconditional dead path is
  rejected.
- **`parse-name` answers a transient `( c-addr u )`** that the next
  `s"`/`."`/`refill` invalidates: copy the bytes into your own buffer at once
  with the checked byte helpers (there is no `move`); never hold the pointer
  across another parsing word.
- **No `s>number?`, `move`, `fill`, `erase`, `bl` or `*/`** in checked bodies;
  see Missing convenience words above.
- **`s" "` is empty**, not one space: the parser consumes the delimiter after
  `s"`; emit byte `32` or a `*-SP` helper for a literal space.
- **Query a wordlist with `s" WORD" get-current search-wl`**: the xt, or zero.
  Built native images provide no `find-name`, `defined` or `[defined]`.
- **`catch` restores the stack depth, not the values.** On a throw
  `nv ' WORD catch` leaves `( x code )` where `x` is whatever the callee left in
  that cell; keep every handle to release in your own locals and read only the
  code (`state doc 0 [: WORK ;] catch CLOSE` closed a garbage handle).
- **A `SORT:SORT!` comparator receives raw cells**; nominal pointer views are
  re-cast on both arguments inside it.
- **Emitted primitive leafness follows emitted control flow**: `FPRIM-L` only
  when the whole body emits no `BL` or `BLR`, else `FPRIM` so the frame
  preserves the caller return address in `x30`.
- Run tests through the owning gate script so assertion failures set the exit
  code.
- **Fallible value-returning scanners validate first**: range and schema checks
  that `throw` go in a `--` helper, and the value-returning word's remaining
  path structurally returns its outputs; a final throw-only fallback in a
  `-- value…` word confuses path-effect merging.

## Rules learned by refusal

Each of these was measured on the engine; the fact that proved it is beside
the rule.

- **A local binds in the spelling it was declared in; word lookup stays
  case-insensitive.** `{: text :}` reads `text` as the local and `TEXT` as the
  word, and the same local hides the word `TEXT` from the definition it is
  declared in (`address` hides `ADDRESS`). The three resolvers — the checker,
  the JIT and tier 1 — agree; `test/compiler/native-local-case.f` pins it.
- **A signature list holds at most 32 cells.** A word whose inputs (or
  outputs) stage more than 32 cells - several records passed by value add up
  fast - certifies at tier 0 and dies at native elaboration with
  `E-IR-TYPE-ARITY` (-6688): measured, `( n ×32 -- n )` runs under
  `test/compiler/aot-mode.f` and `( n ×33 -- n )` throws. Pass a record by
  reference (`ptr fam`, a handle) when a signature grows past that.
- **`s"` reads no escapes; `S\"` does.** `S\"` needs its delimiter space
  (`s\"\n"` is one undefined token) and reads `\u` as its own escape, so a
  fixture holding JSON writes `\\uXXXX`.
- **`.` ends the line.** The native `.` is newline-terminated, not
  space-terminated: `11 . 22 . cr` emits `11\n22\n\n`, so an assertion for two
  dotted numbers on one line never matches; digit emitters (`GT-U-TYPE`,
  `TS-N.`) build inline text.
- **`private` is a convention until the package seals itself.** Any file may
  reopen `package NAME private` and call its internals. The protection idiom at
  the foot of a substrate file — `get-current prot-wid-add` — seals the
  wordlists; after it a second file that reopens the package dies at load with
  the package name as its whole message (exit 84). Two files that belong
  together are two packages with a one-way dependency, or one package that
  only the last file seals.
- **A `DEFTYPE` a defining word hands out sits in the public section.** A
  `does>` body is checked code and may publish a nominal handle directly, but
  the child's stored signature names the type, and a private one does not
  resolve for a reader: the definition is refused as it is made
  (`checker: bad stored signature`), even when only the package uses the
  converters.
- **A nominal error needs its own result family.** Constructing `RESULT:OK` in
  the ok-only path leaves the err variable of `result<a,b>` free, and a free
  variable unifies with a structural type but not with a nominal ENUM or
  TYPEFAMILY. Declare `SUMTYPE foo-result 1` whose ok variant carries the
  payload and whose errors are nullary variants (the `numeric-result` idiom in
  `lib/num-arithmetic.f`).
- **An arity-1 SUMTYPE is spelled with its argument in a signature and bare in
  a `MATCH` selector.** `family<PKG:t>` in the effect (`wrong arity for type
  family` otherwise), `MATCH family` at the arm; never both together.
- **A checker atom prefix reserves the whole lowercase `prefix-*` namespace.**
  A `layout-` prefix makes an ENUM variant spelled `layout-conflict` throw 7110
  far from the cause; sweep with `rg '\bprefix-'` before choosing one.
  Declaration-grammar keywords are reserved family names too (`ENUM policy`
  throws 7110).
- **`0 set-check` also disarms the compile preflight.** A program that opens
  with it to get past one uncertified primitive is measured with neither gate.
  Declare the primitive instead, in the axiom form the engine's own primitives
  use (`PRIM: name PE-… PRIM;`), and the rest of the program still compiles
  checked.
- **A `defer` in `src/core/checker.f` before `: TRUST` takes a pre-trust
  pending slot, and the table holds 48** (`src/habu/layout.f PD-CAP`); the
  file holds 47 today. The 49th dies at boot with exit 72 (`C-PD-DIE-FULL`),
  and `test/pre-trust-defer.f` appends one to prove it — a lane that added one
  more defer went red there. Add a selector to an existing hook instead
  (`SHADOW-DIAG-XT ( n -- )` carries two diagnostics), or place the defer after
  `: TRUST`.
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

## Spans: a pointer that carries its reach

When a word writes into a buffer or indexes one, pass a `SPAN:span<u8>` rather
than a `ptr u8` and a separate length: `lib/span.f` bounds-checks every access
against the reach the span carries, so the capacity test is the type's job and
not a line you can forget. Take the span from a producer — `n SPAN-BUFFER: NAME`,
`n SPAN-CELLS: NAME`, `MEM:ALLOC-SPAN` — or narrow one you were handed with
`SPAN:SKIP` / `SPAN:TAKE` / `SPAN:SUB`, which can never widen it. `SPAN:MAKE` is
the one place an address and a number become a reach, so it is admitted in `lib/`
and `src/` only and `tools/lint/bare-copy-lint.f` reports it (and bare
`BYTE-COPY`) anywhere else. A read-only source stays the `( ptr u8 n )` string
idiom. The reach is counted in bytes whatever the element type;
`docs/type-system.md` § 11 has the measurement that settles why.

## ptr locals and cell access

A `{: p:ptr :}` local admits cell `@`/`!`: `: F ( ptr n -- n ) {: p:ptr :} p @ ;`
certifies and answers the stored cell. What does not certify is reading a
parametric pointee as a cell: `: G ( ptr a -- n ) {: p :} p @ ;` is
`E-NONPARAMETRIC-EFFECT`, because a declared effect must stay parametric over
its quantifier. A word over "some cell buffer base" therefore declares the
concrete pointee (`ptr n`), not `ptr a`; concrete per-buffer words sharing
scalar cursor helpers remain a fine shape, not a forced one.
