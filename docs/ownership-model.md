# C2: scoped ownership and borrowed views

Status: the checked C2 core is implemented in this source tree. Native owner
allocation and loans, initialized storage, task-local cleanup and capture
refusal have source and saved-image E2Es. Qualification and publication of a
release require an exact source/engine pair; this page does not claim that the
current public engine includes C2. Downstream package and document adaptation
is separate work.

## Outcome and boundary

Make the checker reject a view used after its owner ends, overlapping mutable
access, and cleanup through stale exceptional state. Preserve the ability to
share immutable package bytes, keep multiple independent parser cursors, and
return independently owned results. Checked borrowing must not rely on a
caller remembering an advisory lifetime comment.

Keep ordinary `ptr T` lifetime-free for existing code and explicit foreign or
storage boundaries. C2 introduces a checked view surface; it does not claim
that legacy raw-pointer code becomes memory-safe. A checked view must not be
convertible to an untracked raw pointer through an ordinary checked operation.
General nonlexical lifetime inference, cross-task borrowed sharing and a
mandatory migration of every raw pointer are outside the first implementation.

## What already exists

- Internal `read-view<p,q,T>` values carry owner and loan scope parameters and
  occupy two cells, an address and a bound. Checked helpers preserve these
  dependencies through records, sums and quotations. Storage and cast erasure
  refuse scoped values, including through generic helpers and captured effects.
  `C2-MEM:WITH-MUT`, `WITH-READ` and `WITH-MUT-LOAN` attach generative scopes to
  their callbacks. `C2-MEM:ALLOC` appends zeroed storage to an explicit owner;
  `ALLOC-DISPOSE` appends it with a supplied checked disposer. That disposer
  consumes the new unique view after ordinary loans end and before Habu releases
  the allocation. It runs once on owner close, even if an inner owner was active
  when the allocation was made. Allocation failure before registration invokes
  no disposer. A disposer must finish foreign release before throwing; cleanup
  continues with the remaining allocations and preserves the existing error
  precedence. `PUBLISH` consumes exclusive authority to produce a shared view.
  Ordinary constructors and raw address accessors are unavailable. The `read<P,L,T>`
  notation below names the semantic type.
- `DEFLINEAR` enforces single consumption, including nested family payloads.
  Linear locals are deliberately refused. This does not track borrowed access.
- `SPAN` checks bounds and narrowing, not ownership or lifetime.
- Rigid region, extent and mutation-generation identities already exist in
  `src/core/checker.f` and `docs/effects.md` under “Rigid host-allocation identity
  domains”. Reuse their domain-qualified identity machinery; add a distinct
  scope domain, lexical liveness and access authority. An allocation identity
  alone never establishes a live scope.
- `MEM:WITH-BYTES` and checked `finally` already provide scoped cleanup.
  `WITH-BYTES` yields raw pointers and uses a process-wide scope stack, so it
  does not establish checked borrowing or independent task scopes.
- `THROW-EDGE` intersects intact-value evidence across every throw edge;
  `RSCATCH` marks uncertain restored values `stale<T>`. The old first-throw-only
  diagnosis in `56884608` is obsolete. Stack-depth restoration does not restore
  the original values in those cells.

The standing pointer decision in `docs/type-system.md` remains true for raw
pointers. C2 narrows its advisory-borrow policy by adding an enforceable view
kind. The roadmap's sequence requiring general region-typed pointers is
replaced by the bounded sequence below.

## Public owner recipe

Load `lib/c2-owner.f` and `lib/c2-bytes.f`. `C2-MEM:OWNER-SIZE` supplies the
byte length for an owner header. `C2-MEM:WITH-MUT` owns those bytes for one
callback. Inside it, pass `C2-MEM:SEED-OWNER` to `C2-MEM:WITH-INIT`; the
initialized callback receives a unique `owner-state` view. `C2-MEM:BIND`
turns that view into the linear owner handle, and `C2-MEM:UNBIND` returns it
before the callback ends. For example:

```forth
require lib/c2-owner.f
require lib/c2-bytes.f
require lib/memory.f

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

`C2-MEM:ALLOC` appends zeroed bytes to the **named owner**, even while an
inner owner is active. Use `C2-MEM:ALLOC-DISPOSE` when that allocation is tied
to a foreign acquisition: pass a checked `[ mut-view<p,p,a,u8> -- ]` disposer
that consumes the unique view and completes foreign release before reporting
an error. The owner invokes it once at close; a failed allocation invokes no
disposer. `test/c2-owner-dispose.f` exercises normal return, throw, nested
owners, task halt and disposal errors. `C2-BYTES` provides bounded copy, slice
and length operations on live byte views. `XML-C2:WITH-READER` in
`lib/xml/c2.f` accepts a shared source view and unique cursor storage;
`XML-C2:NAME$` retains the source lifetime after the cursor callback ends.

At most 32 C2 owner or initialization frames are live in one task. The 33rd
open throws `E-C2-CAPACITY` (`-9360`) before acquisition; after frames close,
capacity is reusable. Capturing an image while an owner or loan is live throws
`E-C2-CAPTURE` (`-9364`); capture after close is permitted. These are runtime
boundaries, alongside the checker's refusal to let a scoped view escape.

## The consumer the design must express

Tender's `src/opc.f` exposes two distinct operations. `PART-SOURCE` returns
cached immutable bytes valid until package `CLOSE`. For the read-only archives
opened by OPC, `READ` allocates a fresh mutable copy for writers. Adding another
cached part does not relocate or invalidate published part bytes. This is not
a claim about every `ZIP:READ`: `lib/zip.f` returns the replacement buffer itself
for a replaced member of an editable archive. The first safe package surface
accepts read-only archives only; an editable archive cannot acquire that type.

`lib/xml/state.f` stores the source pointer in a reader's context. Tender's
`test/opc.f` keeps two readers over the same package alive simultaneously.
`src/docx/tree.f` retains source slices in typed tree nodes, closes the XML
cursor, then returns the tree while the package remains open. The resulting
DOC model copies emitted text into its own storage and can outlive the package.

There are therefore different authorities: the package-owned source bytes,
mutable parser cursor/storage, package cache metadata, temporary tree storage,
and the final document. Closing a parser ends its cursor authority, not the
package source lifetime. A projection must retain the lifetime of the actual
storage it refers to: source slices depend on the package; cursor scratch
views depend on the cursor. Do not label every XML result with one lifetime.

A blanket ban on storing or returning any borrow cannot express this flow.
The rule is instead: **a borrow may travel or be stored only where its owner
still outlives every possible use.**

## View and scope rules

The semantic notation below describes the implemented type/effect contract;
the accepted Habu spelling is `read-view`, `mut-view` and `forall<...>` as used
in the source examples. A view carries element type, bounds, owner scope, loan
ceiling
and read or exclusive access authority. An aggregate carries the union of the
scope dependencies of its fields, including nested generic and sum payloads.
These dependencies are part of its checked type, not a comment or runtime
address heuristic.

A scoped owner combinator introduces a fresh rigid scope for its callback.
The checker maintains the lexical parent relation and which scopes are active.
Ordinary calls may return views that depend on their still-active input scopes.
The callback hands designated control handles back to its combinator, which
consumes them at scope end. Its external result may contain only values
independent of the scope being closed. Independently owned scalar or document
results may escape.
A generic function cannot manufacture a live owner scope merely by naming a
fresh identity in its declared effect; allocating/opening and closing authority
belongs to the scoped owner operation.

| Operation | Rule |
| --- | --- |
| Copy shared read view | Allowed while the owner scope is active. |
| Copy exclusive view | Refused; moving or a bounded reborrow transfers access. |
| Read through shared view | Allowed within bounds; writes are refused. |
| Reborrow exclusive view | Parent is suspended for the lexical child loan; restored after that loan ends. No parent use or overlapping sibling loan meanwhile. |
| Produce a shared loan from exclusive access | Suspend mutation for the complete shared-loan scope, including all copied views. |
| Return a view from an ordinary callee | Allowed with the original scope dependency preserved in its effect. |
| Export a view as the result of its owner combinator | Refused, including nested fields, generic wrappers and hidden quotation payloads; handing a designated control handle back for scope cleanup does not export it. |
| Store into a typed scoped record | Allowed only if the initialized value's accessible lifetime cannot outlive the borrowed owner; the containing value retains the field's scope dependency. |
| Store into globals, task slots or untracked raw cells | Refused for values with a scoped dependency. |
| Install into defer/XT state, enqueue or spawn a task | Refused if this transports a scoped dependency outside its active lexical scope. |
| Explicitly close a scoped owner | Refused. Only leaving its owner combinator ends the scope, after dependent values and loans have ended. |
| End an exclusive loan or initialized reader scope | End its child loans, consume its designated authority and invalidate its fields; this does not close an enclosing source owner. |

Unique views are consumed by a scope end or explicit end-loan operation; they
cannot be duplicated or silently converted into a shared raw pointer. The
first implementation need not support splitting an exclusive view into dynamic
disjoint subranges. Synchronous calls may move/reborrow a view; task transport
requires separately owned data in this first version.

Exclusive views and mutable scoped aggregate handles remain stack-only in this
slice. Ordinary locals refuse them, including wrappers that contain exclusive
authority. Shared read views may be locals while retaining their dependencies.
There is no implicit reborrow on local lookup. The downstream adaptation
must refactor the DOCX tree builder, XLSX mutable context and mutable `READ`
tests to thread unique
handles through checked effects and factored words. This is an explicit part of
the consumer work; unchanged raw-pointer local code is not the acceptance claim.
General linear locals are a separate language feature, not an escape hatch.

Owner scope and mutation authority are not interchangeable. Immutable published
OPC source storage and mutable cursor/cache bookkeeping must have distinct
capabilities. Updating cache metadata must not require a mutable loan of all
published source bytes. Conversely, resizing or replacing actual source storage
must not retain a previously issued view's authority.

## Binders, effects and the package surface

`read<P,L,T>` and `mut<P,L,T>` name the owner scope `P`, access ceiling `L`,
and element type `T`; a root view has `L = P`. A child reborrow introduces a
fresh `L` inside its parent's ceiling and retains both dependencies. Ending `L`
invalidates every child copy before parent access resumes, even while `P` lives.
Exclusive authority also retains allocation/projection provenance: two values
of the same nominal type are not evidence of disjoint storage. Existing rigid
region identities and declared separate fields supply that evidence; unknown
overlap rejects. Scope, region, extent and generation are distinct domains.

Use a higher-rank callback binder, written semantically `forall P. [ ... ]`,
only in the callback input position of an authenticated scope operation. The
combinator checks that callback with a fresh rigid scope atom and closes the
atom at return or unwind. Its surrounding input/output rows are fixed outside
the binder and cannot acquire `P`, including through reachable stored fields,
quotation payloads or the return stack. Designated control outputs are consumed
inside the combinator and are not part of its external result row. Reborrow
callbacks use the same binder
rule for `L`, without creating allocation ownership. Ordinary helpers quantify
over scopes supplied by their inputs and preserve their dependencies; they
cannot return a scope not supplied by an input or an enclosing live binder.
An ordinary declaration, including `fresh-region-*`, cannot mint scope authority.
Only the checked scope operation couples binder introduction to the runtime
frame or loan transition. Higher-order wrappers must preserve that operation
and its complete callback contract; an annotation alone is not authentication.

The following semantic effects fix the consumer roles. `R` and `S` are ambient
rows fixed outside a binder. `ctl<P>` is move-only access to package bookkeeping,
not authority to mutate published source bytes or dispose the package.

| Operation | Semantic effect / constraint |
| --- | --- |
| `WITH-PACKAGE` | `R path, (forall P. [ R ctl<P> -- S ctl<P> ]) -> S`; return of the control handle lets the scope end normally; cleanup authority stays in its frame on every path. `S` is independent of `P`. |
| `PART-SOURCE` | `ctl<P>, name -> ctl<P>, read<P,P,u8>`; preserves previously published immutable parts. |
| `READ` | `ctl<P>, name -> ctl<P>, mut<P,P,u8>` with a fresh allocation provenance; only the read-only package contract licenses this unique copy. |
| `WITH-READER` | Consume `mut<B,L,reader-storage>` and `read<P,Q,u8>`; introduce initialized-reader scope `I` inside `B`, `L`, `P` and `Q`. A `forall I` callback takes `R reader<I,P,Q>` and returns `S reader<I,P,Q>`. The combinator returns `S mut<B,L,reader-storage>` after invalidating the reader. `S` is independent of `I` but may retain `P` and `Q`. |
| Reader helper return | `reader<I,P,Q> -> reader<I,P,Q>, ...`; a helper may return the input reader within the established `I`. It cannot invent or let `I` escape. Initialization happens inside `WITH-READER` before its callback. |
| `NEXT` | `reader<I,P,Q> -> reader<I,P,Q>, kind`; unique cursor access is threaded. |
| `NAME$` / source projection | `reader<I,P,Q> -> reader<I,P,Q>, read<P,Q,u8>`; copies the stored source view's dependencies, not a loan of reader storage. |
| `OPEN-PACKAGE` | `ctl<P> -> ctl<P>, DOC:document`; the document owns copies and has no dependency on `P`. |

The proposed package adaptation replaces manual `OPEN`/`CLOSE` pairs with
`WITH-PACKAGE`.
Non-nested overlapping package lifetimes become nested scopes; the first slice
may retain an outer package until the inner scope finishes. Runtime stale-handle
tests remain tests of the raw API; safe use-after-scope programs must reject.
The safe XML surface likewise replaces manual `INIT`/`CLOSE` with `WITH-READER`;
source-reader helpers take the scoped callback rather than returning a newly
introduced reader scope. Ending that callback closes the cursor before the
caller uses its returned tree. Copied source slices retain `P` and `Q`, so they
survive this close; any projection into cursor storage also depends on `I` and
cannot survive. No early-close operation on the safe scoped handle is needed.

Package bookkeeping is a typed scoped record instantiated for `P` when the
package is opened. Its cache fields may hold `read<P,P,u8>` because the cache
itself cannot outlive `P`. `ctl<P>` gives exclusive access to these metadata
fields; every helper threads it, and no shared package handle writes the cache.
It is separate from authority over the immutable allocations described by those
fields. A miss allocates independent bytes, initializes them uniquely, then
irreversibly publishes a shared view for `P` and caches that view through a
typed field. No mutable alias to published bytes survives. A hit copies the
stored read view. Adding entries cannot move published bytes. The owner frame
alone owns disposal of all allocations. This uses the same unique-view and
typed-field rules as other records, not a general interior-mutation exemption.

Every later allocation or publication typed for `P` must register through the
owner handle for `P`, never the currently innermost scope. The frame records its
package node at entry; that node's allocation lists own subsequent cache misses
and mutable copies, as ZIP's node-owned buffer lists already do. The task's
chain head selects frames for scope entry and cleanup, not the owner of bytes
requested through an arbitrary control handle. Acquisition into an existing
owner uses the same pre-registration and nonthrowing/non-yielding transfer rule
as scope entry. Thus a cache miss on an outer `ctl<P>` inside an inner package
scope remains owned by `P` after the inner scope ends.

The candidate tree follows the same separation: mutable node metadata belongs
to a unique tree control handle; source names keep `P`/`Q`; copied text lives in
separate immutable allocations owned by the tree scope. Do not keep a shared
view into the mutable tree arena while continuing to mutate it. Node links are
owner-qualified identifiers, not duplicated exclusive views; access requires
the matching tree control handle and a bounded projection loan. This permits
graph links without dynamic disjoint-range splitting. It is part of the
candidate adaptation, not a claim that the existing raw chunk allocator checks.
The tree owner scope encloses the reader callback, so the tree can return from
reader cleanup to that enclosing scope without escaping its own storage owner.

## Typed storage and checker authority

### Implementation interface

The implementation spells exclusive views `mut-view<P,L,A,T>`.
`P` and `L` are scope identities; `A` is a separate rigid allocation or
projection region. The representation remains two cells, address and bound.
Every mutable view contributes exactly one linear unit, including inside a
product or sum. Its element type `T` retains its scope dependencies but does
not contribute owned payloads to that count. No ordinary constructor,
destructor or raw address accessor is exposed.

Binders carry their domain and scope bounds in the existing effect graph.
Scope binders introduce fresh scope atoms; region binders introduce fresh
rigid regions. Both are generative through repeated calls and saved schemes.
A prefix of binders describes one quotation/XT, for example
`forall<p,forall-region<a,[... ]>>`. A child scope binder may name a parent
ceiling, as in `forall<l inside q,[... ]>`. Bound roots are existing type
terms: a scope contributes itself, and a record contributes its scope
dependencies. Region binders have no lifetime bounds.

The following internal operations complement the consumer effects above.
All surrounding rows are fixed outside the binders, and designated callback
control outputs are consumed inside the operation.

| Operation | Internal effect |
| --- | --- |
| `WITH-MUT` | `R size, forall P A. [ R mut<P,P,A,u8> -- S mut<P,P,A,u8> ] -> S`; introduces a fresh scope and allocation region backed by a registered allocation. |
| `WITH-READ` | `R read<P,Q,T>, forall L inside Q. [ R read<P,L,T> -- S read<P,L,T> ] -> S read<P,Q,T>`; shared parent copies remain usable. |
| `WITH-MUT-LOAN` | `R mut<P,Q,A,T>, forall L inside Q. [ R mut<P,L,A,T> -- S mut<P,L,A,T> ] -> S mut<P,Q,A,T>`. |
| Shared loan from a mutable parent | `R mut<P,Q,A,T>, forall L inside Q. [ R read<P,L,T> -- S read<P,L,T> ] -> S mut<P,Q,A,T>`. |

Each loan consumes its parent for the callback and restores the original
address and bound from its own runtime frame. It does not reconstruct the
parent from the returned child. A mutable parent's authority is unavailable
throughout either kind of loan. Whole-view loans preserve `A`.

Initialized storage has the reserved element wrapper `init<I,T>`, with `T`'s
physical layout and a fixed accessible lifetime `I`. It has no ordinary
construction, destruction or cast surface. The initialization boundary is:

```text
R mut<B,L,A,u8> T,
  forall I inside [L,T].
    [ R mut<B,I,A,init<I,T>> -- S mut<B,I,A,init<I,T>> ]
-> S mut<B,L,A,u8>
```

Admission requires an existing unique checked byte capability and a complete,
non-unique initial value. Size and alignment come from the committed
instantiated record schema. Initialize before the callback, then invalidate
and clear initialized fields before restoring the original byte capability.
The first implementation supports statically fixed record layouts; an
unresolved or unsupported layout is refused.

For `H = mut<B,L,A,init<I,Record>>`, generated `FIELD@` has effect `H -> H F`
and `FIELD!` has effect `H F -> H`, where `F`, its offset and width come from
the committed field record. A store requires `I` to be inside every scope
dependency of `F`, exact invariant schema matching, and a non-stale,
non-unique value. Reborrowing changes `L`, never `I`. A copied source field
retains its original owner and ceiling; it does not acquire `I` merely because
the reader stored it.

A bounded field loan carries structural provenance
`field(A, committed-field)` and preserves `init<I,F>`. It consumes the whole
parent for its callback and restores it afterward. Simultaneous sibling
field loans and dynamic range splitting are outside this first slice.

Record schemas and by-value construction, mutable-view accounting, binder
support and task-local cleanup can be implemented independently. Unique
admission combines those leaves; initialized storage and field access then
build on that boundary. No public safe admission is available until storage,
escape, cleanup and capture fences are complete.

A borrowed field is legal in a scoped record whose accessible lifetime is no
longer than the field owner's lifetime. An outer object cannot acquire an inner
scope dependency through a store: attaching a dependency to a temporary local
alias would not constrain other aliases of that outer object. The initialized
value's scope and field schema are checked before the write. A newly created
inner scoped aggregate may hold outer views and retains their dependencies.

The physical backing allocation may live longer than that initialized value.
For example, reader initialization exclusively lends caller-owned storage to a
scoped reader and suspends every other access to that storage. Reader close
invalidates its borrow-bearing state and field aliases before restoring the
original storage authority. Admission requires an existing unique C2 storage
capability from an enclosing checked owner or loan, never a raw pointer plus a
length. The checker cannot revoke copies of a legacy raw pointer. Raw caller
storage must remain on the raw API or be created through the checked ownership
surface before lending; no runtime address scan claims to make it unique. This
permits reusable caller storage without letting it retain an inner source view
after the typed reader ends.

Mutable locations are invariant in both element type and scope parameters.
A store, generic instantiation, `ptr-field`, `CELL-VIEW`, integer cast, field
projection, `MAKE`/`UNMAKE`, or unknown effect must not erase a view dependency
or turn read authority into write authority. Refuse an unmodeled crossing;
do not treat an unrecognized pointer as an unscoped view.

Library code may need a small private representation boundary to access the
underlying machine address. That boundary must be owned, explicit and tested;
it cannot publish the address, a pointer cell, or a scope-free accessor that
lets checked callers bypass the view rules. Ordinary raw APIs remain available
only for inputs which already belong to the raw surface.
Converted owners must not also publish a raw alias to the same borrowed storage
as a compatibility escape.

Use existing type-family and effect machinery to carry scopes through records,
sums, quotations and separate compilation. Persist the same abstract scope
binders and dependencies through source reconstruction, AOT capture/restore
and the checker payload. A restored effect must grant exactly the authority of
its source effect. Do not serialize host addresses or runtime allocation order
as checker identity. Diagnostics must identify the escaping owner or conflicting
loan and the operation which caused the refusal.

Persisting an abstract effect is different from capturing a live loan. The first
implementation refuses image capture while any live C2 owner or loan scope
would be included in the image. Capture admission must cover every included
task's runtime scope state, as well as statically visible scoped values. It
must not restore lifetime or cleanup authority merely from a saved address.
Ordinary compiled code and generic effects using views remain capturable when
no live scoped authority is being captured.

Relevant existing dots are `7399c340` (pointee invariance), `e04f7b3e` (raw-cell
laundering), `ef8fca8e` (record stride), and `3bd40ca9` (layout walk ownership).
Reproduce the crossing used by this view implementation before making one a
prerequisite. Do not make the entire record/span migration a gate for C2.

## Cleanup, exceptions and tasks

The owner scope holds authoritative cleanup state outside cells that `catch`
may restore as stale. A thrown callback ends its loans and disposes the owner
exactly once. On return/throw, existing `finally` precedence applies: successful
cleanup preserves the body's failure; a cleanup failure supersedes it. Task-exit
cleanup keeps the first failure already recorded by the body or exit chain.
A catch outside the scope cannot recover a valid view into
the disposed owner from restored stack cells. A caught failure inside a live
scope does not recreate consumed owner or loan authority.
Scope end discharges its C2 loan obligations, including those represented by
stale cells, without reading those cells. Such cells remain unusable and may
only be discarded; they cannot demand a second disposal or restore access.
This does not weaken conservation for ordinary `DEFLINEAR` owners outside C2.

Runtime owner frames form a chain in the current task's storage, rooted in a
private typed user slot reached through `data-base`. Main-thread scopes use the
same current-region slot without requiring a TCB; the main thread has none.
The task-exit hook and frame capacity must be ready before acquisition. Each
entry registers one pending frame before allocation, then records acquired
resources with a nonthrowing, non-yielding transfer before any fallible work.
Normal `finally` invokes the frame's close operation and removes it. A single
static `[ -- ]` callback registered through `TASK:AT-EXIT` drains the current
task's remaining frames innermost first. It captures no per-scope values. This
drain is necessary: `TASK:PAUSE` ends a halted thread without unwinding `finally`.
The private runtime anchor is cleanup authority, not a public task slot into
which checked code may store a borrowed value.

The current exit registry has 128 image-lifetime task/quotation rows shared with
other services. C2 uses one per participating task, not one per scope; exhausted
registration must fail before acquiring an owner. Increasing that registry's
capacity is not required for the two-task proof.

Frames transition pending -> live -> closing -> closed; a pending frame disposes
only acquisitions it recorded. Closing invalidates loans before disposal and
retains the frame until disposal finishes. TASK owns a per-task deferral depth
consulted by `PAUSE`; the C2 runtime brackets its critical intervals through
that interface, without a TASK dependency on C2. Cooperative halt is deferred
across the acquisition/registration transfer and while a close/drain is in progress;
a pause during that interval yields without recursively ending the task. A
pending halt is serviced after normal scope cleanup; the halt drain itself
finishes before task completion is published. This prevents an interrupted
disposer from being invoked twice or losing its remaining release authority.

Every registered disposer must finish disposal before reporting a catchable
error; a partially releasing operation needs a total adapter or the existing
fatal invariant policy. A task drain records errors and continues closing all
remaining frames before reporting its first cleanup failure to the exit chain.
Borrow values add no second disposer. MEM's fatal unmap policy is unchanged.
The existing process-wide `WITH-BYTES` stack is not reused for C2 scopes.

`6218899c` (generic owner scope) is a useful integration point after these rules
are expressible. Reconcile `56884608` and `9812a28c` against the current
stale-value model rather than implementing their old exceptional-stack recipes.
The first implementation rejects storage of a stale value into any typed
borrow-bearing field; it cannot recover authority through such a store.

## Implementation and remaining use

The checker, typed storage, loans, task-local owner runtime and `C2-MEM` /
`XML-C2` adapters are implemented in this source tree. The registered E2Es
exercise fresh source consumers, rooted and ordinary native images, and saved
images. A public release still requires qualification of its exact source and
engine together. The separate downstream work is the OPC/DOCX/XLSX ownership
flow described above; migration follows a qualified release.

General linear locals (`4a2f5db4`), owner-only product constructors
(`d967fc03`/`65ddd22f`), and arbitrary lifetime-parametric raw pointers are not
blanket prerequisites. Reuse existing owner mint/consume support where adequate;
`0a19f45d`/`72e83e7a` belong in the slice only if the chosen public owner surface
requires them. Do not reserve the old proposed construction flag bit 4: it is
already used by `DRV-ADDR`. The old `527e05ca` child is absent; the JSON writer
aliasing task `e454cd08` and scoped-memory task `129ad1c0` are already closed.

## Acceptance and release discipline

Use focused real-load tests while implementing. Required new negative behavior
includes owner-scope escape through nested aggregates and generic calls,
outer-record/global/raw-cell storage, read-to-write conversion, duplicate or
conflicting unique loans, explicit scoped-owner close, exclusive locals,
raw-storage admission, scope forgery from a declaration or rigid region,
projection with an ended owner or child-loan scope, stale exceptional values,
deferred capture and task transport. Positive
cases include independent readers sharing copied read views, nested different
owners, lexical reborrow ending
before parent reuse, typed inner storage borrowing outer bytes, and callee
returns preserving an active source scope.
Binders must remain generative across two invocations, recursive calls and
image replay; a callback restricted to one pre-existing scope is not a valid
`forall` callback. Source projections can outlive reader close, while projections
into reader storage and child loans cannot outlive their own ceiling.

The consumer E2E must preserve two readers over one package, stable published
source after another cache entry is added, independent mutable `READ` copies,
tree source after cursor close, and an independently owned document after
package close. With nested packages, a cache miss through the outer handle
inside the inner callback must remain readable after the inner package closes
and be disposed only with the outer package. Thread mutable tree/XLSX state
through real checked helpers;
reject the old exclusive-local shape rather than silently weakening uniqueness.
Refuse the corresponding escaped tree/view program at check
time. Exercise failure during allocation, parsing and cleanup, with independent
concurrent scopes, main-thread scopes and a cooperative halt inside nested
scopes or during cleanup. Verify one disposal per acquisition, continued drain
after a disposal error, and the specified error precedence. Refuse using an
editable/replaced ZIP member as the fresh mutable-copy package surface.
Retain a parsed/filled document and repeatable readback as
artifacts. Add no mirrored storage-layout assertions.

Run the native suite when integrating the shared checker/runtime semantics;
run focused checks between local changes. Qualify capture/restore of the new
effects through native images. Generation convergence is needed only where
changed compilation or capture can affect successive output. Gforth recovery
is a separate audit when its mirror/seed path changes, not a per-change C2 gate.
Downstream proof and migration have their own acceptance checks. No Maki build
or unrelated application migration is a C2 core acceptance prerequisite.
