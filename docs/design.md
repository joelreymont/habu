# Why checked Habu is shaped as it is

Each section states a decision and the fact that forced it. The language rules
themselves are in [forth.md](forth.md) and the card; the code that enforces a
rule is named where the reason depends on it.

## Writing and changing Habu

- Tools, test harnesses and build drivers are checked Habu programs run by
  `bin/hb`. A shell or Python file may only invoke `bin/hb`, with no logic,
  parsing or policy. The one exception is the device-peer scripts under `test/`
  that stand in for a foreign machine (serial, XMODEM, UDP, the embedded
  assembler hosts, and `test/wasm/run.mjs` and `test/wasm/data-peer.mjs`, which
  run a Wasm module under bun). Any other non-Habu file under `tools/` or
  `test/` is a defect to convert. Tools that tests and builds run are built
  into `hb`, so running one rebuilds no image.
- Every module has a real package; public effects keep meaningful types, not
  `n` for everything.
- A container states the type it holds and the language supplies that type's
  width: a program never writes a width or a byte count for a typed element,
  since a type's width can change. Make typed containers only.
- A library takes its limits from the protocol it speaks, not from a preset.
- Use an unchecked or foreign boundary only after checking that the language
  cannot express the operation normally, and keep it explicit, small and tested.
- Keep changes focused, and remove obsolete machinery when the language or
  library already expresses the requirement.
- When a decision lacks evidence because a capability does not exist yet,
  build it in a branch and measure.

## Machinery being replaced

The machinery below is being replaced, each item by what follows its colon.
Never extend or patch it: a defect it shows becomes an acceptance case of the
replacing dot, and a change that needs it joins the replacing design.
Something one machine, tier or build path needs and another does not is a split
to remove, not a row to add.

- arm64's machine-code interpreter, definers and startup in `habu1.f` and
  `habu2.f`: one typed interpreter on every machine.
- Building a target by loading it into the running hb and capturing, cutting
  or rewinding its memory: a cross-build that writes the target as data.
- The per-definition build cache and its source observers: the file linker.
- A linker per target: one linker for every target.
- Writing no object for a cross-build, and compiling for the host a
  definition no build may run: every build writes objects, and a definition
  its source puts in a target section is compiled for the target alone
  ([xcomp.md](xcomp.md#host-and-target-words)).
- Tier 1 and the JIT as compilers: T0 on every target, reusing what they
  already implement; tier 1 changes only to fix a defect on a path it still
  owns.
- `TRUSTED:`, engine-only marks and owner rows: checked forms and private
  sections.
- Compatibility with older hosts, formats or schemas: hard cutover.
- A hook: the owner records data or makes a plain call.
- fork without exec: fork only to exec.

## Types

**The stack comment is the signature.** Every Forth word already states its
interface in `( in -- out )`. Habu makes that comment a type and proves the body
against it when the file loads; a body that disagrees stops the load with a
message naming the word and the token. No new syntax is needed, and a
wrong interface becomes a load failure instead of runtime stack corruption.

**Effect tokens are types, not names.** `( got expected -- bool )` declares two
type variables called `got` and `expected`, not two named inputs. Names belong
to locals (`{: got :}`). Keeping the two apart lets the effect grammar stay a
type grammar with no positional naming.

**The rest of the stack is an anonymous row.** A body may not reach below its
declared inputs. That makes the declared effect the whole composition contract:
a caller can check a call from the callee's signature alone.

**A type variable stays parametric.** A body declared `( a -- a )` must work
for every `a`; specialising `a` to one type is `E-NONPARAMETRIC-EFFECT`, and so
is collapsing two declared variables into one. Without this rule an ordinary
`:` definition could produce a value of a protected type from a variable, and
the trusted boundaries that are meant to be the only places such values are
made would stop being visible.

**`bool` is not `n`; roles are not `n`.** A comparison produces `bool`; a raw
`0` where `bool` is declared is refused. `idx`, `len`, `fd`, `rc`, `pid` and the
other built-in roles never silently become each other or `n`. A file
descriptor handed where a length belongs is then a load-time error, which is
the cheapest defence the checker offers.

**Integers widen only safely.** `n` flows into and out of every sized integer.
Between sized types, a narrower one widens into a wider one of the same sign,
and an unsigned one into a strictly wider signed one; narrowing or a change of
sign needs an explicit conversion. A quotation parameter reads the lattice the
other way for its inputs, because whoever executes the quotation supplies them:
`[ i64 -- ]` satisfies a `[ u8 -- ]` parameter and `[ u8 -- ]` does not satisfy
`[ i64 -- ]`.

**A pointer always says what it points at.** `ptr` consumes the next type
token (`ptr u8`, `ptr a`, `ptr ptr u8`); a bare `ptr` is malformed. The element
type is checked at call boundaries, so `ptr n` handed to a `ptr u8` parameter is
refused. The element type decides which fetch and store words apply, so it has
to be part of the type.

**Nominals: one that proves validation and one that only separates.** A
`NEWTYPE` derives nothing: its only constructors are what the owning package
writes, so holding a value proves the package's validation ran. A `DEFTYPE`
derives the converter pair `>NAME` and `NAME>N`, so anyone may cross the
boundary and every crossing is visible in source. A validating package
publishes no inverse to `n`; each consumer that needs the raw cell keeps its
own private `CAST:`. A public inverse would let any caller launder one role
into another through `n`.

**A nominal field is a proof token.** Generated constructors are always
public, so a field whose type only the package can construct is the one way a
package controls who builds its records. A public `UNMAKE` followed by `MAKE`
can still keep the token and replace the other fields; forth.md records that
limit.

**Linear values are consumed exactly once.** A `DEFLINEAR` value is neither
copied nor dropped on any path, so leaks and double releases of mapped files,
buffers and transactions become load-time errors. Linearity propagates: a
family whose payload is linear is one linear unit, and a `MATCH` arm must
consume or re-wrap the payload it re-introduces. A linear value may not be
bound to a local (`E-LINEAR-LOCAL`), because a local can be referenced twice;
the checker refuses even a local referenced once rather than counting
references.

**Disposal cannot half-fail.** A disposal word returns nothing, and a failing
release ends the process. A disposal word with a result could be ignored or half-handled and
leave a caller believing memory was returned. A disposer that runs in a
sequence of disposals may return a `result` instead, so one failure does not
unwind past owners not yet disposed; it must still free its own record before
the call that can fail.

**A quotation reads locals only while they live.** `[: … ;]` is an execution
token of one cell. One that reads locals carries a record in the stack frame
of the body it is written in, the word's or an enclosing quotation's, and the
checker refuses any use that would let it outlive those locals or that body.
There is then no heap environment to own, and only plain
execution tokens reach storage or an image.

**`catch` takes a stack-preserving quotation.** Its inputs must equal its
outputs, so the stack has one shape whether the body returned or threw, and a
value crosses the boundary by being passed in and returned. Values restored by
an unwind are typed `stale<T>`, because restoring depth does not restore the
original contents of those cells.

**Function-valued state is a typed `defer`.** `defer ACTION ( in -- out )`
declares the vector's effect and `is` checks the installed quotation against
it exactly. A raw execution token fetched from untyped memory has lost its
effect, and executing one is refused (see "Typed top level").

**Escape hatches are loud and few.** `TRUSTED:` asserts an effect without
checking the body; `PRIM:` and `PPRIM:` rows give an engine primitive its
effect; a private `CAST:` changes a type at one named point. Each is a site a reader and the checked-boundary lint
can find. A trusted site's comment states why the effect cannot be inferred,
and a focused test pins its behaviour through the production path. Before
adding trust, try a checked factoring: a single primitive row usually turns a
trusted call site, or a whole unchecked span, into checked code.

**Packages can be sealed.** Not resolving a private name from outside proves
only that the name is not visible; a later `package NAME` could reopen the
package and republish a private word, for example one that turns an `mmap`
result into a typed pointer. A sealed package refuses the reopen, and a
build seals every package its engine files made before it reads program source,
so neither a program compiled in a build nor source loaded on the engine
reopens one.

## Families and layout policies

**Every declared type is one row in one registry.** `STRUCTURE`, `ENUM`,
`NEWTYPE`, the older `SUMTYPE` and `PRODUCT`, and the built-in parametric
types (`span`, `matrix`, `tile`) all register a type family in
`src/core/type-family.f`: name, owning package, arity, fields or variants, and
opted-in derivations. Parametric type names were once a parser whitelist, so a
user could not declare a generic type at all. One registry gives every
declaring word the same scoping, unification, snapshot persistence and
transactional rollback. "Family" because one declaration stands for many
concrete types: `option<a>` is declared once and `option<n>` and
`option<NUM:index>` are distinct members.

**Rows key on (package, lowercase tail).** Two packages may each declare
`index` without aliasing. Type tails are lowercase and executable words upper
case, so a type and a word never collide by spelling.

**An algebraic type has four layers.** Each family defines a logical type
(`result<a,b>`), a physical layout (payload slots and a tag), generated
introduction forms (`RESULT:OK`, `RESULT:ERR`) and a checked elimination form
(`MATCH … ;MATCH`, which must cover every variant). A `result` that is only a
parsed type expression, or only a quotation combinator, lets a caller skip the
failure case; generated constructors and an exhaustive `MATCH` do not. The same
four layers serve `result`, `option`, enums, records and layout-bearing
evidence types.

**A multi-cell value is not one checker cell.** Stack values are cells. A
logical value that occupies several cells is modelled as its physical cells,
and the default layout is `slot0 … slot(M-1) tag`, where `M` is the widest
variant's payload in cells and the tag comes last. That layout needs no heap,
no collector, no fat pointer and no runtime metadata; dispatch reads the tag
directly, and values stay in registers. Hidden physical field terms carry the
resolved family id, so a same-tail `result` from another package cannot unify
with this one. Two consequences:

- A non-linear multi-cell value binds whole to a local, and each reference
  reloads every cell.
- A word that returns a multi-cell value cannot run at the interpreter prompt
  (`E-LAYOUT-VALUE ... interpret-mode layout value`), because the interpreter
  would move one physical cell of the bundle without knowing it.

**Layout policy is bound per family, once, at declaration.** `POLICY <name>`
follows the arity; a missing clause means `stack-cell-tag`. A per-use-site
policy would give one logical type several stack shapes.

- `stack-cell-tag` is the universal layout.
- `packed-tag` keeps the stack layout identical and adds a memory descriptor
  (size, alignment, tag width narrowed to the variant count) for arrays and
  buffers of the type. Payloads stay cell-wide, so the descriptor needs no
  offset table.
- `niche-null` and `boxed` change the stack shape to one cell. A policy that
  changes the stack shape must not be selectable before its constructor,
  `MATCH`, stack-operation and invalid-tag lowering exists; `boxed` has none
  and is refused with `layout policy not yet supported`.
- `niche-null` is one shape: an empty variant, then one holding a single `ptr`
  field. The cell is the pointer, zero for the empty variant, so every cell
  value is a variant and no tag can be invalid. With a plain `ptr` a null
  `some` reads as `none`: the family means "an address or none", and
  `nullable<a>` is it.
- Any other name is `unknown layout policy`. Every refusal rolls back the
  family row and its layout state.

**Construction never starts from a raw tag.** There is no public construction
from a tag value and no unchecked enum cast; a tag outside the declared range
is a runtime invalid-tag failure, not a new variant.

**Visibility picks the spelling, not whether words are generated.** A public
family publishes `PKG-FAMILY:MAKE` in a reserved constructor namespace; a
private family publishes `FAMILY-MAKE` in its package's private wordlist. Most
memory records are package-private, so a public-only generator would serve
almost nothing. The private spelling has no colon because it cannot: a
qualified definition name is routed to the named namespace's public wordlist,
and the checker records a colon name as a public symbol. A
private sum or enum publishes no words at all; it constructs through
checker-owned tokens keyed by package, family and variant.

**Generated words are protected by name.** A declaration protects no wordlist.
The checker refuses with `E-CTOR-PROTECTED` an `undefine` of a generated word
and, for a family's constructor namespace, reopening it or defining a new word
into it. A variant name is not a generated member, so a package may still
define a word that happens to share it.

## Type substrate

The checker has three places a type name can live: built-in CT roles (a
global, flat table: `idx`, `len`, `fd` and the rest), atoms (prefix-named
phantom tags such as `extent-…` and `space-…`, compared by identity), and
type families. Two decisions placed user-declared nominals on families.

**Phantom extents are families.** An extent is a type argument with no runtime
value, as in an index typed by the dimension it ranges over. A product of
extents needs a structured, ordered, nesting argument list: swapping the
factors must be refused and splitting a product must be a real re-typing.
Families already carry ordered, nested, identity-unified arguments, package
scoping, persistence and rollback, so an arity-2 family represents the product
with no new unification kernel. Roles have no argument slots, so a product
cannot be expressed on them without rebuilding families inside the role table.
Atoms have no registry and no typo protection: any undeclared `extent-…` name
is silently a valid distinct atom. Without the product requirement atoms would
have sufficed, which is the condition that would reopen the choice.

**Value nominals are arity-0 families with generated converters.** `DEFTYPE`
(`src/lib/type/deftype.f`) mints a package-scoped arity-0 cell family and
derives `>NAME` and `NAME>N`. The role table it once used was global: a second
same-named declaration in another package died. Families key on package, so
`DEFTYPE SERIAL` in two packages yields two types with no engine change, and
the strictness against `n` and against other nominals is the same. The built-in
roles stay in the CT table, where a global name is what they are.

## Registry protection

**The hole.** Registry control cells are `variable`/`create` records; a bare
store into one corrupts a registry past its public API.

**Privacy closes direct routes.** A registry cell belongs in its package's
private section, where nothing outside the package names it: a bare use or `'`
is E-UNDEFINED at top level and in a checked body alike. Reads and writes go
through the package's certified words.

**The checker closes the laundered route.** `variable V  ' W V !  : F V @
execute ;` would certify as a pure `( -- )` while the real xt ran, letting
checked source write a private cell or run a definer. Executing or catching an
xt fetched from untyped memory is refused. The fix is provenance-blind: it
closes the whole class of laundered calls at check time.

Neither route is a boundary against a program that computes addresses; both
keep checked code from reaching a registry by name.

**A guarded memory band was rejected.** Moving the cells into a write-trapped
band would require a guard-bypass writer for legitimate declaration-time
updates. Because the registry is Forth-level, that writer must be a
Forth-callable word with no effect, which is exactly what the laundered route
reached; the band would only trade one route for another. An engine band is
safe for a different reason: its writers are inline machine code inside
compiler primitives, never standalone words. Protection of a
Forth-level registry is therefore privacy plus the checker's refusal of opaque
execution.

## Typed top level

**The problem.** The native depth floor, checked before a certified word's
body runs, sees how many cells are present, not what they are. `' W execute` below `W`'s arity read below the stack base; `0 0 catch`
branched to address 0; `s" abc" +` added a pointer to its length; `here V !`
stored an address in a raw cell; and an immediate named in a checked body ran
at compile time, leaving an empty body under a non-empty certificate.

**The interpret row types top-level code.** The checker keeps a row for each
evaluator stack base (a closed evaluation has a floor of its own) and receives
one event per top-level token from an engine hook (`src/core/top-row.f`). Per
token, not per line: top-level tokens resolve as they execute (a line defines a
word a later line calls, and definers consume tokens), so checking a whole line
first would need a second parser that diverged from execution order. Each
token is judged as the same token in a colon body, storage included: a refusal
prints the body's diagnostic and throws the checker's rc before the word runs,
so the stack and every cell are untouched and the failure stays catchable by
the REPL and `evaluate`.

**Live depth is the anchor.** Each event loads the saved row at the live depth.
Cells the row does not know are unknown, and a typed consumer refuses them; the
row's bottom is the live floor, so a step that reaches below it is refused as a
body reaching under its declared inputs is. A word with no stored effect is
refused at top level as in a body.

**No escape hatch.** Nothing suspends the row: after `0 set-check` every
top-level token is still checked, and a cell pushed meanwhile keeps its type.

**An xt carries its effect.** `['] W` yields `xt<effect(W)>`, a quotation type
built from `W`'s certified effect, so `execute`, `catch` and `is` check it by
the same unification as a quotation literal; named-word xts and quotations are
one kind of value. The effect comes from the store the checker already
persists, so the dictionary needs no new metadata. A code cell that must hold
an xt is declared with its effect (`TYPED-VARIABLE NAME [ in -- out ]`), and
an xt from untyped memory is refused at `execute`.

**Immediates in checked bodies.** An immediate runs at compile time on the
interpret stack, so applying its declared effect to the body would certify a
step the body does not contain. Until compile-time expansion is modelled, a
signature-carrying immediate in a checked body is refused; modelled parsing
immediates such as `[char]` pass.

## Ownership

**Outcome.** The checker refuses a view used after its owner ends, overlapping
mutable access, and cleanup through stale exceptional state, while immutable
package bytes stay shareable, several parser cursors may run over one buffer,
and results can be returned independently owned. Checked borrowing must not
depend on a caller honouring a lifetime comment.

**Raw pointers stay lifetime-free.** `ptr T` says what it points at, not which
allocation, for how long or how far. Existing code and foreign and storage
boundaries keep it. Views are a separate surface that does not make raw-pointer
code memory-safe, and no ordinary checked operation turns a view into a raw
pointer (a view on either side of `CAST:` is `E-CAST-SCOPE`). Nonlexical
lifetime inference, borrowed sharing across tasks and mandatory migration of
raw pointers are outside the design.

**A view is two cells with scope parameters.** `read-view` and `mut-view`
carry an address, a bound and scope identities; `mut-view` adds a rigid
allocation region. Scopes are generative: each `WITH-READ` or `WITH-MUT`
callback gets fresh ones, also through repeated calls and saved schemes, so a
view cannot be carried out of the callback that scoped it. Every mutable view
counts as one linear unit, inside records and sums too. No ordinary
constructor, destructor or raw address accessor exists. An allocation identity
alone never establishes a live scope; `SPAN` checks bounds and narrowing, not
lifetime. forth.md "Owners and views" has the rules for writing owner code.

**Cleanup state lives outside the stack.** The owner scope keeps authoritative
cleanup state outside cells `catch` may restore as stale. A callback that
throws ends its loans and disposes its owner exactly once. `finally`
precedence holds: successful cleanup keeps the body's failure, a cleanup
failure supersedes it. A catch outside the scope cannot recover a view into
the disposed owner from restored cells, and a caught failure inside the scope
does not recreate consumed authority. Scope end discharges loans held in stale
cells without reading them; those cells may only be discarded.

**Owner frames belong to the task.** Frames form a chain in the current task's
storage, rooted in a private slot reached through `data-base`; the main thread
uses the same slot without a TCB. One static `TASK:AT-EXIT` callback per task
drains remaining frames innermost first. A halted task now unwinds through
each scope's cleanup, so the drain has no reachable case left; dot
`habu-retire-c2-invoke-a608fd55` retires it with `c2-invoke`. The exit
registry is shared and fixed, so ownership uses one row per task, not per
scope, and an exhausted registration fails before any owner is acquired. The
process-wide byte scope stack is not reused, because it nests per image, not
per task.

**No half-done transfer.** A frame is registered pending before allocation
and records each acquisition with a step that can neither throw nor yield; it
then moves pending, live, closing, closed, and a pending frame disposes only
what it recorded. Closing invalidates loans before disposal. A cooperative
halt is deferred across acquisition and close through the task's own deferral
depth, which the halt consults, so the task library has no dependency on the owner
runtime. A disposer finishes releasing before it reports an error, and a drain
continues through every remaining frame before reporting its first failure.
Otherwise an interrupted disposer could run twice or lose the authority to
release what remains.

## Models taken from other Forths

SwiftForth, polyFORTH and VFX Forth have no counterpart to the checker,
nominal and algebraic types, packages, typed foreign calls, image capture or
the deterministic gate. What Habu takes from them are library and kernel
services, under Habu's package prefixes and typed handles but with the
products' word names.

**Tasks.** The runner catches the worker xt, records an uncaught throw in the
task control block and ends only that task, as SwiftForth's
trampoline does; a worker throw must not end the process. Blocking waits use
semaphores (`SEMAPHORE`, `WAIT-SEM`, `SIGNAL-SEM`) and the task's own `WAIT`,
which `WAKE` ends, and `WAIT-MS`, which it ends early, instead of polling.
These names leave the products' (SwiftForth's `STOP`, VFX's `WAIT`/`SIGNAL`)
for the standard terms: a task's own wait is wait/wake, as Linux's futex and
Zig's `std.Thread.Futex` name it, and a word that takes a semaphore says so.
`JOIN` returns a `result` and `AT-EXIT` registers cleanup, after VFX's exit
code and task-exit hook. Messages are VFX's one-cell mailbox
(`SEND-MESSAGE`, `GET-MESSAGE`). VFX keeps its embedded task API on pthreads
so one program text runs hosted and on a target; Habu takes the same rule. A
target kernel is polyFORTH's round robin, where the `STATUS` cell is both
scheduler link and wake flag and every blocking I/O word yields; it exposes the
hosted words that make sense on a target, and the hosted-only words (`KILL`,
`JOIN`, `AT-EXIT`) are refused by the target's checker rows rather than
emulated. No target tasker exists in the tree yet.

**Devices.** `EMIT`, `KEY`, `TYPE` and `ACCEPT` reach the current task's
input and output device through a small table of operations
(`src/lib/genio-core.f`), as VFX's generic I/O devices do. A socket or serial port
can then become the REPL's console without changes to the REPL, which keeps a
running server or microcontroller inspectable and correctable from a distance.
A stream device's `ACCEPT` is standard Forth's: it fills the caller's buffer
and leaves the rest of a longer line for the next `ACCEPT`, and `READ-LINE`
answers a line of any length.
The device record stays small enough to be the per-task vector table SwiftX
uses on a microcontroller.

**Sockets and HTTP.** TCP takes SwiftForth's verbs (`BIND`, `LISTEN`,
`ACCEPT`, `CONNECT`, read, write, close) in the typed style of `UDP4`, with
result types carrying `errno`. The HTTP client is `libcurl` through the FFI, as
in VFX; the server is a small layer over `TCP4`. TLS for a server terminates at
a proxy.

**Databases.** SQL through the FFI came first: Postgres through libpq
(`src/lib/pg.f`), with a nominal connection handle, results as `DEFLINEAR`
tokens that live in scopes (`PG:WITH-RESULT` clears its result on every path),
result types for every fallible call and never a raw `n` for an error. The vocabulary is SQL-shaped because the schema language is SQL. A
connection and its results belong to the task that created them. polyFORTH's
fixed-record kit (typed fields, ordered indexes over a file or flash range) is
the model for targets, opened when a target program first needs persistent
data. The two do not share one field vocabulary: SQL columns and fixed fields
are different things.
