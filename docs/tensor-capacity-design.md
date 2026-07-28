# Tensor capacity and provenance design

Design for dot `habu-design-typed-mutable-76654024`. Design only — no
implementation may start until both orchestrators accept this document and the
implementation leaves are minted from its migration table.

## Problem

Three independent proofs established that public compute words built on raw
`ptr a n` arguments certify wrong programs:

1. A raw in-place `GELU!` shape was destruction-rejected twice: a probe proved
   `n = -1` executes and mutates cell zero, and an oversized count has no
   capacity witness (rejected commit `1122785b`).
2. The right-transposed matmul takes six same-typed extents; a caller can
   transpose dimensions and still certify.
3. `T-ADD!` and `ROW-ADD` had identical checked signatures with opposite
   argument roles; a swapped call certifies and runs, silently accumulating
   into the wrong buffer.

The root defect sits one level down. `TENSOR:TV-NEW-HOST`
(`maki/tensor-value.f:372`) records a raw base pointer and a *claimed* shape;
it records no allocation capacity and no provenance. Every later bounds law —
`VIEW-BOUNDS-CK` (`tensor-value.f:186`) proving view footprints, the
"proven at construction" justification for unchecked element access
(`tensor-value.f:209`) — is proof relative to that unwitnessed claim.

Load-bearing facts from the survey of the landed tree:

- `TV-NEW-HOST`/`TV-NEW` have **no production callers**, and public
  `TV-DATA@` is consumed only by tests and TENSOR's own `TV-LINEAR`. The
  eager compute path (executor and every `maki/` kernel) runs on bare
  `ptr + n` and bypasses package TENSOR entirely. The raw public seams can be
  hardened or deleted without a production migration cascade, and migrating
  the mutators onto `tensor` is first traffic, not a refactor.
- The capacity witness already exists at the grant site and is discarded:
  `MEM:ALLOC-BYTES ( CAD-NUM:alloc-byte-len -- ptr u8 CAD-NUM:alloc-byte-len )`
  returns the typed extent beside the pointer, and `MEM:RELEASE-BYTES`
  requires it back (`lib/memory.f:185,200`).
- `lib/byte-buffer.f` (package `BUF`) is the landed precedent for
  capacity-as-ownership with fail-closed liveness.
- Three buffer authorities coexist unreconciled: the TENSOR descriptor table
  (types and views, no capacity), the executor node arena (real capacity, no
  types), and the extent registry's baked constants (types, no pointer, no
  capacity).
- Latent defects to correct, not hide: `TV-AT@`/`TV-AT!` perform no per-call
  index bound against the recorded extents, and `TV-NEW-VIEW` writes
  `TV-VFIELDS!` twice.

## Candidate comparison (mandated)

**A — extend the existing TENSOR authority.** One storage identity record
inside package TENSOR; tensors and views reference it; mutators take tensors.

**B — a new typed mutable span value.** A second value type owning
pointer-plus-extent, package-owned indexed access.

Authority test: B would own pointer-plus-extent for the same memory the tensor
record already describes — every tensor-backed buffer would carry two
independent descriptions of its extent, a fourth buffer authority on a tree
with three unreconciled ones. Duplicate authority by construction. B is
admissible only if A cannot express the invariant.

A expresses it, with one correction to the naive version: **a caller-supplied
capacity column is not a witness** — the pointer and the length arrive as two
values with nothing binding them, so the same caller lies twice. The witness
must be created where the grant happens: the only mint sites for storage are
owner adapters at the allocation seams, so pointer and capacity are bound by
construction and never re-associated by a caller.

**Verdict: candidate A**, in the storage-record form below. Candidate B is
rejected as duplicate authority.

## The frozen extension

### Storage identity record

A private parallel-column table in package TENSOR. The owner value is
**linear**, following the landed `DEFLINEAR` pattern the weight store already
uses for its owners:

```
DEFLINEAR storage            \ linear owner value; the checker rejects
                             \ dropping it and consuming it twice
```

The linear `storage` value wraps the table slot (`gen * TS-CAP + idx`).
Because it is linear, **leaks and double release are checker-enforced, not
runtime-detected**: a program that fails to release a storage, or releases it
twice, does not certify. Owner transfer is linear move, which the checker
already models. Tensor records reference the slot by index and generation as
plain (copyable) data — only the owner value is linear, so tensors and views
stay cheap.

Columns (all private): `TS-BASE-AT` (raw base, family-less sibling like
`TV-DATA` today), `TS-CAPB-AT` (capacity), `TS-SPACE-AT`
(`CAD-KIND:address-space`), `TS-PROV-AT` (provenance enum), `TS-GEN-AT`
(generation), `TS-LIVE-AT` (liveness flag).

The stored capacity type is exactly **`CAD-NUM:alloc-byte-len`** — the one
nominal type `MEM:ALLOC-BYTES` returns and `MEM:RELEASE-BYTES` demands back.
No conversion is stored and none is erased. The fit law needs one new checked
comparison in package CAD-NUM:

```
: FITS-ALLOC? ( n CAD-NUM:alloc-byte-len -- bool )
```

which compares a computed raw byte count against the witnessed capacity
without erasing or converting the role. Capacity is bytes, not elements:
bytes are what the allocator grants and demands back; element math would bake
in dtype width and break silently when a non-cell dtype gains real storage.

```
ENUM tensor-prov  prov-owned  prov-mapped  ;ENUM   \ compact
```

`prov-owned`: granted by `MEM:ALLOC-BYTES` through `TS-ALLOC`. `prov-mapped`:
admitted weight-store spans (later leaf, below). There is deliberately no
"static fixture" arm and no constructor from a bare pointer: tests allocate
through `TS-ALLOC` like production. If a concrete fixture genuinely cannot
allocate, that need is brought back to this design, not solved with a side
door.

### Mint sites — the only ones

```
: TS-ALLOC   ( CAD-NUM:alloc-byte-len -- storage )
: TS-RELEASE ( storage -- )
```

`TS-ALLOC` calls `MEM:ALLOC-BYTES` itself and records base, capacity, host
space, `prov-owned`, generation, live — the caller never touches the pointer
before the record exists, so there is nothing to lie about; and because the
`CAD-NUM:alloc-byte-len` argument is only a size request (any caller can mint
one), the design claims nothing from it beyond the number: the pointer, the
binding, and the provenance all come from the allocation performed inside the
mint. `TS-RELEASE` consumes the linear owner, calls `MEM:RELEASE-BYTES` with
the recorded witness (the typed release the allocator already demands),
clears liveness, and bumps the generation, which invalidates every
outstanding tensor reference to that slot. Double release is statically
impossible (the linear value is gone); a forgotten release fails
certification (the linear value is unconsumed).

The executor arena needs no separate mint: the arena becomes one `TS-ALLOC`
storage, and node buffers become views into it, bounds-proven by the existing
view law against a now-witnessed capacity. Arena reset releases and re-mints,
so stale node tensors from a previous plan fail closed by generation.

A `prov-mapped` adapter at weight-store admission (where `SLOT!` already
proves `byte-off`/`byte-len` fit inside the mapping) is a named later leaf:
its generation is invalidated when the mapping closes. The GPT-2 critical
path does not depend on it — the tensor intake contract copies weights into
model-owned buffers, which are ordinary `TS-ALLOC` storage.

### Tensor construction and access

```
: TV-TENSOR ( storage CAD-KIND:rows CAD-KIND:cols dtype layout -- storage tensor )
```

replaces `TV-NEW-HOST`/`TV-NEW` (safe: no production callers; tests migrate
with the leaf). The linear owner threads through unconsumed — the weight
store's landed convention for operations on a linear value — and the tensor
receives only the copyable slot reference. `TV-TENSOR` computes the footprint
from shape and dtype width via `CAD-NUM:FITS-ALLOC?` and throws `E-TV-CAP`
unless footprint ≤ recorded capacity. The tensor record drops its raw
`TV-DATA` column and instead references the storage slot; `TV-DESC`
(bufferless) is unchanged.

Every data access — element words, materialize, the kernel seam — resolves
the storage handle first: wrong generation or dead storage throws
`E-TV-STALE`. This is **runtime use-after-release/use-after-reset detection,
not static lifetime safety**; the static proof (borrowing, regions) remains
the separate checker capability the advisory-span files already name, and
this design must not be described as providing it.

Corrective changes folded into the core leaves, named as such:

- `TV-AT@ ( tensor n n -- r )` / `TV-AT! ( r tensor n n -- )` gain the
  missing per-call index bound against recorded rows/cols (`E-TV-IDX`).
- `TV-NEW-VIEW`'s double `TV-VFIELDS!` write is reduced to one write path.
- Public `TV-DATA@` is withdrawn from the public surface (only tests and
  `TV-LINEAR` consume it); raw base resolution becomes package-private, and
  `TV-LINEAR` goes through the private seam.

View constructors keep their signatures and their `VIEW-BOUNDS-CK` law; the
reference extent is now backed by the storage witness, so the existing
"proven at construction" justification becomes true. Views inherit storage
(and therefore provenance and generation) through the existing storage-ref
chain.

### Kernel seam

Public compute mutators take `tensor` handles, never raw pointer/count pairs,
and never a caller-supplied loop bound. The public word validates every input
and output footprint — shapes, dtype, contiguity class, storage liveness —
**before the first write**, then derives base and extent package-internally
and feeds the existing private element loops, which stay private exactly as
the package rules require. Shape mismatches throw named codes at the seam.
No public word accepting a caller-supplied length is added anywhere.

### What stays out of scope, named honestly

- **Destination/source role confusion between two same-typed tensors** is not
  fixed here: `ADD! ( tensor tensor -- )` still swaps when shapes agree. This
  is the write-effect checker gap (fail-closed-before-write, mutable-operand
  roles), minted as its own checker-capability dot at acceptance. Until it
  lands, destination-first ordering plus witnessed shape checks are the
  named, insufficient mitigation.
- **Static lifetime/borrow proof** — separate checker capability; generation
  checks are runtime detection only, as stated above.
- **The `TENSOR:` name collision** (package qualifier vs the extent-registry
  definer, `maki/extent-tensor.f:180`) and the extent registry's fold-in are
  follow-on migration debt with their own dot: after this design TENSOR owns
  pointer and capacity, and the extent registry stays purely type-level until
  its collision is retired.

## Migration order

Ordered leaves, each below 30 minutes of implementation, minted as dots only
after joint acceptance. Later leaves depend on earlier ones.

| # | Leaf | Owned result |
|---|------|--------------|
| 0a | Storage record and mint sites | Linear `storage` owner (`DEFLINEAR`), columns, `tensor-prov`, `TS-ALLOC`/`TS-RELEASE`, `CAD-NUM:FITS-ALLOC?`, generation/liveness law, `E-TV-CAP`/`E-TV-STALE`; `TV-TENSOR` replacing `TV-NEW-HOST`/`TV-NEW` with the owner threading through; tests migrated to `TS-ALLOC`. Test matrix: fit positive/negative, role-swap rejections (raw `n` and `byte-len` rejected where `alloc-byte-len` is demanded), dropped owner fails certification, double release fails certification, release-then-access through a stale tensor throws `E-TV-STALE`, generation reuse cannot resurrect. |
| 0b | Element access bounds | `TV-AT@`/`TV-AT!` index bound (`E-TV-IDX`), negative and overflow probes on roots and views. |
| 0c | View write path | Single `TV-VFIELDS!` write in `TV-NEW-VIEW`; behavior-pinning view tests unchanged. |
| 0d | Public surface hardening | `TV-DATA@` withdrawn from public; `TV-LINEAR` through the private seam; runtime word-list assertions prove the withdrawal (bare, qualified public, qualified private). |
| 1 | `MAKI:GELU! ( tensor -- )` | The twice-rejected in-place GELU leaf (`habu-add-in-place-f0c95650`) unblocks, tensor-native from birth — first consumer, no legacy callers. |
| 2 | Executor storage | Arena as one owned storage with node buffers as views; `EX-BIND` retyped to take a tensor whose shape is fit-checked against the declared slot shape — retiring the length-less `( ptr a MIR:input-slot -- )` seam. |
| 3 | Matmul family | `MATMUL`, `MATMUL-DX`, `MATMUL-DW`, `MATMUL-RIGHT-T`, `MATMUL-LEFT-T` take tensors; dimensions come from witnessed shapes, killing the six-same-typed-extents transposition defect. |
| 4 | Embedding | Gather/scatter surface takes tensors; composes with the integer-token cutover (`habu-unify-token-embedding-fb629f63`); the proven out-of-bounds gather (id 99 against a 3-row table) becomes structurally impossible — the row bound is the witnessed extent. |
| 5 | `MAKI:ADD!` | Retyped `( tensor tensor -- )` atop the landed pointer version; callers hold tensors after leaves 2–4. Residual dst/src gap deferred to the write-effect dot, explicitly. |
| 6 | LayerNorm | `LN-FWD`/`LN-BWD`/affine forms and executor row drivers take tensors; the four-buffer `LN-AFFINE-BWD` mutation surface becomes fully witnessed. |
| 7 | Weight-store admission | `prov-mapped` storage minted where `SLOT!` proves the span; generation invalidated at mapping close; zero-copy weight tensors. Not on the GPT-2 critical path (intake copies into owned storage); scheduled after decode works. |

## What the checker enforces, and what remains runtime-thrown

Checker-enforced (static, fail-closed):

- Storage cannot be minted from a bare pointer: no such constructor exists;
  allocation happens inside the mint, so the size argument is only a request
  and proves nothing the design relies on.
- Storage cannot leak and cannot be released twice: the owner is a linear
  value; an unconsumed or doubly consumed owner fails certification.
- A kernel cannot be called with raw pointer/count arguments: its effect
  demands `tensor`.
- Mapped bytes cannot become storage outside the admission adapter: no other
  constructor exists.

Runtime-thrown (named codes):

- `E-TV-CAP` — footprint exceeds witnessed capacity at construction.
- `E-TV-STALE` — access through a handle whose storage was released or
  re-minted (use-after-release/reset detection, not static lifetime proof).
- `E-TV-IDX` — element index outside recorded extents.
- Existing `E-MK-DIM` view-footprint law, now against a witnessed extent.
- Kernel-seam shape/dtype/contiguity mismatches.

Not enforced by this design (tracked, not normalized):

- Mutable-operand roles — the write-effect checker capability, its own dot.
- Static lifetime/borrowing for mapped spans — the existing region-capability
  dots.
