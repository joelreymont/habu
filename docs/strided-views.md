# Strided tensor views (layout v2)

Design-only. Decides how maki gains stored view descriptors — offset plus
strides over existing storage — completing the slot the layout column reserved
("v1: contiguity order only; strides arrive with cad-1",
`maki/tensor-value.f:54`). Produced under dot `habu-strided-tensor-views-75d8864e`
after Joel's 2026-07-20 question "do we have view semantics on tensors like in
pytorch?". Every current-behavior claim carries a `file:line`. No code lands
here; the SV-N contract items at the end are what implementation lanes bind to.

## 0. What exists today (the substrate)

- **Tensor descriptor is contiguous-only.** `TV-DESC ( rows cols dtype layout
  address-space -- tensor )` (`maki/tensor-value.f:255`); `layout` is the
  2-value enum row/col contiguity order (`maki/tensor-value.f:64`) — no offset
  cell, no stride cells.
- **The graph is single-assignment.** Every op output owns an arena slot
  (`maki/executor.f:110` `EX-NODE-PTR`); nothing mutates a node buffer after its
  op writes it, and the save-vs-recompute machinery (`maki/saved.f`) relies on
  exactly that run-immutability.
- **Access-pattern freedom already exists at three seams**, none of them stored
  descriptors: equation index position (a factor enters transposed by how its
  indices are written — no transposed copy; `maki/spec.f` generated accessors),
  window binding (`maki/executor.f:505` `EX-BIND` binds a caller slice — the
  batch loop binds T×C windows of the B*T buffer), and batched free-extent
  loops (generated batch loops around the GEMM word, `maki/spec.f:537`).
- **The scatter-add adjoint pattern is landed** (GATHER's backward), and the
  aliasing hazard class exists only in the host facade (`maki/mha.f`
  alias-unsafety, recorded in `habu-own-multi-head-c863298a`).

## 1. Decision: views are the general case of the tensor value

One representation, not a new node kind: the tensor value record grows three
things — a **storage reference**, an **offset** (elements), and a **stride
pair** (elements per row step, per col step). A contiguous tensor is the
degenerate view: offset 0, strides (cols, 1) for row-major. This is what the
v1 comment reserved; the layout enum survives as the *classification* (derived:
contiguous-row / contiguous-col / strided) rather than the representation.

Rejected alternative — a VIEW op node producing a descriptor value: it puts
aliasing into the graph topology, makes every consumer pattern-match two value
kinds, and buys nothing the record extension doesn't.

## 2. The immutability law (the PyTorch bug class stays out)

**Views are read descriptors over run-immutable storage.** There is no
write-through-view operation and none may be added: the only writer of any
storage remains the op that owns it as output, exactly the SSA property the
executor has today. Consequences, each load-bearing:

- "An in-place op mutated the base a saved view aliased" is unrepresentable —
  in-place ops do not exist inside the graph.
- A view never invalidates a saved tensor; `saved.f`'s decisions are unaffected.
- Two views of one storage never race; order of reads is immaterial.

The write-side counterpart (building a big buffer by writing through window
views — e.g. a KV cache appending per step) is NOT a view: it is the owning
op/loop writing its own output storage at computed offsets, host-side, before
the graph reads views of it. The KV consumer contract (SV-5) keeps this
distinction explicit.

## 3. Legality and typing

- **Construction is checked, fail-closed, named.** A view constructor proves
  `offset + (rows-1)*rstride + (cols-1)*cstride < storage-elements` (and
  non-negative strides in v2; negative strides deferred until a consumer needs
  reversal) at construction — a named throw, red-first, nothing constructed on
  failure.
- **The checker types a view as a tensor of its extents** plus provenance to
  the storage extent — the extent-role machinery is the natural host (a view's
  extents are ordinary extent roles; its storage provenance is an extent the
  bounds proof consumed). A view is indistinguishable to consumers from a
  materialized tensor of the same extents, BY DESIGN, except for the
  contiguity classification.
- **Ops declare their layout demands.** An op that requires contiguous input
  (a fused kernel, a device transfer without stride support) rejects a strided
  view with a named error whose remedy is the explicit COPY op — the
  `.contiguous()` trap made loud and explicit instead of a silent copy. No op
  silently materializes.

## 4. Autograd

The adjoint of a view read is a **scatter-add of the view's adjoint into the
storage's adjoint at the same (offset, strides)** — the GATHER-backward pattern
generalized. Multiple views of one storage accumulate naturally (sum of
scatter-adds), which is exactly the correct calculus for reads that fan out.
Head-split views (SV-6) make this concrete: dQKV accumulates H per-head
scatter-adds. The derivation must gradcheck against central FD per consumer, and
a batch-isolation-style proof (perturbing storage outside the view leaves the
view's adjoint contribution zero) pins the bounds.

## 5. Execution

- **Contiguous fast path unchanged**: classification says contiguous → today's
  code paths byte-for-byte (the regression bar: contiguous models produce
  bit-identical results and no measurable slowdown).
- **Strided reads at the accessor seam**: the generated accessors and op read
  paths gain the (base + offset + i*rstride + j*cstride) form where an op
  declares stride support. Initial stride-aware set = the consumers' needs
  (equation feeds, attention reads); everything else demands contiguity and
  fails closed per §3.
- **Device**: TMA box descriptors take base + strides natively — a view maps
  directly onto the transfer descriptor with no device-side materialization;
  the PROMOTE leg already carries extent magnitudes and grows the stride pair.

## 6. Interaction with plan/cache identity

Kernel and schedule choice can depend on layout: the sched-key gains the
contiguity classification (and stride signature where a kernel specializes on
it), so a strided-input plan never cache-hits a contiguous-input plan. Same
rule the affine-LayerNorm form landing followed for cache keys.

## Contract items

- **SV-1 (representation)**: tensor-value record grows storage-ref + offset +
  stride pair; contiguous = degenerate view; layout enum becomes derived
  classification. All existing suites bit-identical (the degenerate path is
  the old path).
- **SV-2 (constructors)**: checked WINDOW (contiguous row slice — the EX-BIND
  generalization), TRANSPOSE-VIEW (stride swap), HEAD-SPLIT (SV-6's form), and
  the general VIEW (explicit offset+strides) — each bounds-proved at
  construction, fail-closed named, red-first.
- **SV-3 (immutability law)**: no write-through-view op exists; a lint-level or
  checker-level guard makes adding one loud (the review bar: any future
  write-view proposal reopens this doc's §2).
- **SV-4 (autograd)**: scatter-add adjoint for view reads, gradchecked; fan-out
  accumulation proven; out-of-view perturbation proof.
- **SV-5 (KV-cache consumer)**: incremental generation keeps a host-owned
  ring/append buffer written by the sampling loop (owner-writes, per §2), and
  the per-step forward binds WINDOW views of it — replacing the full-context
  re-forward. Dot `habu-autoregressive-gen-sampling-7f0df415` (landed) named
  this as its follow-up.
- **SV-6 (head-split consumer)**: multi-head attention reads H per-head views
  of one QKV buffer — head h = offset h*hd, row stride H*hd, col stride 1 —
  with the fused-QKV projection writing once and H equation feeds reading
  views; dQKV accumulates H scatter-adds. Owned by
  `habu-complete-trainable-multi-39e26b3d`, which decides whether views or
  per-head equations win on evidence.
- **SV-7 (device consumer)**: strided transfers map views onto TMA box
  descriptors; owned by the GB10 batched-attention plan node
  (`habu-gb10-batched-attention-3055d565`, BTC-6).
- **SV-8 (plan identity)**: sched-key carries the layout classification +
  stride signature where kernels specialize.

## Sequencing

SV-1..4 are the library core and land together (one lane, all-or-nothing like
the derive-from-model stage 3 — the degenerate-view rewrite touches the same
tensor-value record). SV-5..7 land inside their owning consumer dots, each
proving its views against this contract. SV-8 rides whichever consumer first
specializes a kernel on stride. The whole program starts after multi-head
attention (SV-6's owner) fixes its evidence-based choice — this doc exists so
that choice is made against a settled design instead of ad hoc.
