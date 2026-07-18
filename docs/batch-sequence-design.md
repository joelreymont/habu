# Batch/sequence (B,T,C) design over the 2D model IR

Design-only. Decides the convention by which maki expresses nanoGPT's
`(batch, time, channel)` tensors on a model IR that is 2D `rows x cols` today,
so that causal self-attention masks **block-diagonally per sequence** and never
contracts across the batch. Produced under dot
`habu-batch-sequence-tensor-006f25a1` (epic `habu-epic-nanogpt-in-b239aa8d`).
Every current-behavior claim carries a `file:line`. No code lands here; the
follow-up sub-dot drafts at the end are what the orchestrator mints.

## 0. What is 2D today (the substrate)

- **Tensor descriptor is 2D.** `TV-DESC ( rows cols dtype layout address-space
  -- tensor )` (`maki/tensor-value.f:255`); shape arithmetic is the two nominal
  families `CAD-KIND:rows` / `CAD-KIND:cols` (`maki/tensor.f:82` `SHAPE`). There
  is no third extent family and no stride cell anywhere — layout is a **2-value
  enum** `row`/`col` (`maki/tensor-value.f:64`), i.e. C-contiguous vs
  column-major, not a general stride vector.
- **Model-IR node/slot descriptors are 2D.** A node stores `MI-ROWS-AT` /
  `MI-COLS-AT` columns (`maki/model-ir.f:112-113`); inputs store
  `MI-IS-ROWS-AT` / `MI-IS-COLS-AT` (`maki/model-ir.f:125-126`);
  `MIR-INPUT+ ( rows cols dtype layout -- slot )` (`maki/model-ir.f:311`) and
  `MIR-OP+ ( rows cols dtype layout attr mat -- node )` (`maki/model-ir.f:361`)
  take exactly two extents. Table caps: 128 nodes / 64 input slots
  (`maki/model-ir.f:100,102`).
- **`MODEL:` signatures parse `name:RxC` only.** `PARSE-SHAPE` splits on a single
  `x` into `rows cols` (`maki/cad.f:461-471`); there is no `RxCxB` form. The DSL
  threads **one running value** and drains parameter operands FIFO
  (`maki/cad.f:285` `CAP-EMIT-PARAMS`; `:557` `CAP-TOKEN`).
- **Every op is 2D-broadcast-typed.** `SHP-LEGAL?` (`maki/cad.f:338`) encodes the
  broadcast classes `1xC` row (`SHP-ROW-OK?` `:325`), `1x1` scalar
  (`SHP-SCALE-OK?` `:331`), and same-shape; the executor's broadcast read
  `EX-BC@` (`maki/executor.f:145`) is a 2-axis `(row,col)` read.
- **The whole executor/autograd stack is a `MATCH` over one 2D op enum.**
  32 op-kinds (`maki/op-kind.f:20-62`), each with a 2D host reference
  (`maki/executor.f:343` `EX-NODE`) and a 2D adjoint (`maki/backward.f:332`
  `BW-STEP`). The arena is a flat 32768-cell float pool
  (`maki/executor.f:75` `EX-ARENA-CELLS $8000`).

### Where B*T-as-rows is already correct, and where it is not

Every GPT op EXCEPT token-mixing is **row-independent** and already runs with the
batch folded into rows:

- The committed MLP golden is `MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 ... ) LINEAR
  GELU LINEAR ;` (`maki/from-scratch-model.f:126`) — the `8` is a batch of 8
  rows; LINEAR/GELU/bias/residual/LayerNorm/RMSNorm/softmax-row all act per row
  (`maki/executor.f:200` `EX-ROW-FWD`, `:170` `EX-EW2`), so folding `B*T` into
  rows changes **nothing** for embeddings, LayerNorm, MLP, residual, LM-head, or
  cross-entropy. Their adjoints are equally row-local (`maki/backward.f:203`
  `BW-STEP-COPY`, `:220` `BW-STEP-UNARY`, `:226` `BW-STEP-SOFTMAX`).
- **Only self-attention mixes tokens**, and it must mix ONLY within one sequence
  and causally. The committed attention capture is a **single sequence, no
  batch, no mask**: `MODEL: ADAM-ATTN ( q:4x3 kt:3x4 s:1x1 v:4x3 -- o ) MATMUL
  SCALE SOFTMAX-ROW MATMUL ;` (`maki/adam-train.f:224`), i.e. `L=4, d=3`. Fold a
  batch into those rows and `MATMUL(Q[B*T x d], Kt[d x B*T]) = S[B*T x B*T]`
  contracts **every token against every other token across all sequences** — the
  exact cross-sequence leak the dot forbids. The host golden `MM-NT`
  (`maki/attention.f:18`) and unmasked `SM-FWD` (`maki/softmax.f:26`) have no
  notion of a sequence boundary.

**Conclusion that frames the whole decision:** the "(B,T,C)" question is *not*
global. It is localized to the attention score-matmul, its mask, its row-softmax,
and the value-matmul. Any option that imposes a third dimension on the ~28
row-local ops pays a stack-wide tax for zero correctness benefit there; any option
that leaves attention as a plain 2D matmul over `B*T` rows is silently wrong.

## 1. Decision space

### Option A — sequence-as-rows + block-diagonal causal mask (2D IR preserved)

Keep `rows = B*T`, one `(B*T)x(B*T)` score matrix, and a mask tensor/witness that
sets cross-sequence and future `(t' > t)` positions to `-inf` before
`SOFTMAX-ROW`. No IR shape change; the sequence boundary is witnessed by the mask
contents.

### Option B — a true third dimension in tensor-value / model-ir / SPEC:

Add a third extent family (`CAD-KIND:depth` for `B`, or a `(B,T,C)` triple) to
`SHAPE`/`TV-DESC` (`maki/tensor.f:82`, `maki/tensor-value.f:255`), a third node/
slot column (`maki/model-ir.f:112-113,125-126`), `RxCxB` parsing
(`maki/cad.f:461`), and a 3-axis variant of every broadcast/op/adjoint MATCH arm.
Cross-sequence contraction becomes a **type error** (batch extent must match and
is never contracted).

### Option C — executor-level batching (2D IR per sequence, batch loop outside)

The IR stays 2D and describes **one sequence** (`T x C`). A host loop runs the
forward+backward `B` times, binding a different sequence slice each iteration
(`maki/executor.f:413` `EX-BIND`) and **accumulating** parameter gradients across
iterations (the pattern `SC-GRAD-AT` / `ADAM-UPD` at `maki/adam-train.f:250-253`
already reads one grad node per slot; accumulation adds a running sum). Attention
is per-sequence-correct because each IR invocation *is* one sequence.

### Option D — batch/seq/head as SPEC: extent roles, lowered to a *segment
attribute* on the token-mixing ops only (2D memory)

Memory stays 2D `rows = B*T`, row-major, **B outermost** so sequence `b` is the
contiguous row block `[b*T, b*T+T)`. The batch/sequence/head structure is carried
as **extent roles** in the SPEC:/candidate-B surface (`#B`, `#T`, `#H`;
`docs/golden-syntax.md:56-71`) and lowered onto a **segment/causal attribute**
(the same attrs cell that already carries slice ranges, `maki/model-ir.f:116`
`MI-ATTR`, read at `maki/cad.f:667` `RB-SLICE`) on exactly the four token-mixing
ops. A segmented attention op contracts block-diagonally per `T`-block and
causally within it; it never materializes a `(B*T)x(B*T)` score. The ~28
row-local ops are untouched. This is the option the code makes viable that the
dot's list under-specifies: it is *not* "raw 2D + attribute" (which has no static
guard) and *not* "full 3D bytes" — the role lives in the **type**, the bytes stay
2D.

### Option E — reshape-to-blocks + batched-matmul primitive (variant of D)

Instead of a new attention op, add a batched-matmul op-kind that reads the batch
count from attrs and loops the existing `MATMUL` reference (`maki/matmul.f:22`)
per block, plus the existing causal-mask dot for the within-sequence part. This
is D with the "segment" made a matmul attribute rather than an attention-sublayer
attribute — smaller new surface, but it needs the mask + softmax to also learn
the block stride, so it fragments the segment fact across three ops instead of
one.

## 2. Evaluation

Criteria abbreviations: **MASK** = per-sequence causal correctness; **SPEC** =
`SPEC:` grammar / planner dataflow impact; **AG** = autograd walker
(`maki/backward.f`); **EXE** = executor binding + memory; **PROMOTE** = shape
obligations; **GB10** = kernel strides / TMA / tiling; **LOOP** = nanoGPT
training loop (`get_batch`, loader); **COST** = blast radius / files.

### Option A — block-diagonal mask over B*T rows

- **MASK**: correct *if and only if* the mask is exactly right; the boundary is
  witnessed only by mask **data**, so a wrong mask is a silent numeric bug, not a
  checker reject — the weakest possible witness. `SM-FWD` is unmasked today
  (`maki/softmax.f:26`), so this needs the masked-softmax dot
  `habu-causal-attention-mask-1ced9cbd` anyway.
- **SPEC**: the score is a dense `(B*T)x(B*T)` GEMM; the grammar sees a plain
  contraction with a mask elementwise op — no batch index at all, so the planner
  cannot recover the block structure to tile it.
- **AG**: adjoint is the dense transposed matmuls (`maki/backward.f:240`) plus a
  mask-multiply adjoint; works, but differentiates through the wasted
  off-diagonal blocks.
- **EXE / memory**: `(B*T)^2` score buffer. GPT-2-small `B=12,T=1024` →
  `(12288)^2 = 1.5e8` cells vs the correct `12*1024*1024 = 1.26e7` — **12x**
  waste, and it overflows the 32768-cell arena (`maki/executor.f:75`) by four
  orders of magnitude even at toy scale.
- **PROMOTE**: the mask is a first-class `B*T x B*T` tensor obligation.
- **GB10**: worst case — a dense score with a sparse mask defeats flash-attention
  tiling; the tiler must special-case the block-diagonal it was never told about.
- **LOOP**: `get_batch` must also emit the block-diagonal mask per step.
- **COST**: one mask op + softmax masking. Small code, but **structurally the
  wrong shape** — rejected on O(n^2) waste and the data-only boundary witness.

### Option B — true third dimension

- **MASK**: **strongest static guarantee** — a batch extent that must match and is
  never contracted makes cross-sequence contraction a *type error*, the most
  "Habu" answer (make the bad state unrepresentable).
- **SPEC**: clean — `O[b i n] = A[b i k] B[b n k] * +Σk`, `b` a free extent. But
  this is the *same information* extent roles carry (Option D) without touching
  the runtime representation.
- **AG**: every `BW-*` emitter grows a third extent: `REF-ROWS`/`REF-COLS`
  (`maki/backward.f:86-89`), `BW-OP2`/`BW-OP3`/`BW-MM`/`BW-TR`/`BW-RS`/`BW-SL`
  (`:120-162`) all re-plumbed; the `TRANSPOSE-SHAPE`/`ROWS+` algebra
  (`maki/tensor.f:222,164`) needs 3-axis forms.
- **EXE**: `EX-BC@` broadcast (`maki/executor.f:145`) becomes 3-axis; **every one
  of the 32 op arms** in `EX-U-EL`/`EX-EW2-EL`/`EX-ROW-FWD-1`/`EX-ROW-BWD-1`/
  `EX-NODE` (`maki/executor.f:119,150,180,206,343`) grows a depth loop.
- **PROMOTE / GB10**: gives the planner true 3D strides directly — genuinely nice
  for GB10, but see COST.
- **LOOP**: `get_batch` emits a native `(B,T,C)` tensor.
- **COST**: **maximal blast radius** — `tensor.f`, `tensor-value.f`, `model-ir.f`
  (three columns + `MIR-INPUT+`/`MIR-OP+`/`MIR-SHAPE!`/every `RB-*` re-prop
  `maki/cad.f:634-716`), `cad.f` (`PARSE-SHAPE`, `SHP-LEGAL?`, all move re-prop),
  `backward.f`, `executor.f`, plus `golden-syntax`/`SPEC:`. Touches ~every file
  in maki's model stack and taxes the 28 row-local ops that gain nothing.
  Phase-0 explicitly says flagship code waits for the type-family surface
  (`docs/nanogpt-inventory.md:124-137`); a bespoke 3rd extent duplicates the
  extent-role machinery Foundation A is already building.

### Option C — executor-level batch loop

- **MASK**: correct by construction — one sequence per invocation, so attention
  never sees another sequence; causal masking reduces to the within-sequence dot
  `habu-causal-attention-mask-1ced9cbd`, no cross-sequence concern at all.
- **SPEC**: **invisible to the planner** — the planner sees a single `T x C`
  graph per step and never learns `B`. This is the flagship split (see §3): fine
  for training, a dead end for the Triton-reimpl planner, which needs the batch
  extent to map the grid and amortize launches.
- **AG**: no walker change; gradients accumulate across the host loop (extend the
  `SC-GRAD-AT`/`ADAM-UPD` read at `maki/adam-train.f:250` with a running sum
  buffer). Weight-tying grad-accumulation (`habu-weight-tying-wte-ab4145da`)
  composes naturally — it is the same accumulate-across-slots pattern.
- **EXE / memory**: smallest footprint — arena holds one sequence
  (`T*C`), reused `B` times. `B` forward+backward passes per step (`B`x slower,
  but correct). No cross-batch statistic exists in GPT (no BatchNorm), so
  per-sequence independence is legitimate.
- **PROMOTE / GB10**: nothing to promote for the batch — the planner is not told,
  so GB10 cannot batch the kernel. **This is the disqualifier for the north-star
  (PROMOTE to GB10, `docs/compute-campaign.md:80`).**
- **LOOP**: cleanest — `get_batch` yields `B` windows, the loop binds each.
- **COST**: minimal, trainer-only. But it does not *decide the convention*; it
  postpones it. Correct as an interim training harness, wrong as the answer.

### Option D — extent roles + segment attribute on token-mixing ops (2D memory) — RECOMMENDED

- **MASK**: cross-sequence contraction is **unrepresentable** once the segment op
  is the only token-mixer: it contracts strictly within a `T`-block, and the
  causal constraint `j <= i` lives inside that block. With extent roles
  (`idx<#T>` vs `idx<#(B*T)>`, `docs/golden-syntax.md:59-71`) a plain matmul over
  `#(B*T)` rows cannot be fed to a contraction expecting `#T` — a **checker
  reject**, restoring Option B's static guarantee without Option B's bytes.
- **SPEC**: batch/seq/head become extent roles; the GGEMM schematic
  (`docs/golden-syntax.md:81`) grows a free (non-contracted) index:
  `S[b h i j] = Q[b h i k] K[b h j k] * +Σk`, `+Σ` still only over `k`. The
  planner derivation gains a **batched contraction** = the same contraction
  replicated over the free extent — exactly what a batched HMMA/tcgen05 GEMM and
  flash-attention want (`docs/compute-campaign.md:39-45`). Aligned with the
  Triton-reimpl planner's `idxctx`/extent needs (`docs/tma-gather.md:20-27`).
- **AG**: the adjoint of a segmented/batched matmul is the segmented/batched
  transposed matmul — the **same rule** as `BW-STEP-MATMUL`
  (`maki/backward.f:240`) replicated per segment. Softmax-row adjoint
  (`maki/backward.f:226`) is untouched: softmax stays per row (one query's
  distribution over its `<=t` in-sequence keys). Masked positions are
  structurally absent, so their grads are zero with **no new adjoint**. Net: one
  new adjoint (the segment attention / batched matmul), zero change to the 28
  row-local adjoints.
- **EXE / memory**: 2D arena unchanged; the segment op loops `B` blocks
  internally over row offsets `b*T` (reusing `MATMUL` `maki/matmul.f:22` and
  `MM-NT` `maki/attention.f:18`). Score memory is `B` independent `T x T` blocks,
  never the `(B*T)^2` dense matrix.
- **PROMOTE**: extent roles give the planner `#B`,`#T`,`#H` to derive strides and
  TMA box dims; **B outermost** ⇒ its stride is `T*C`, the slowest-varying — the
  kernel-shape-correct choice (a batch dim that is not the slowest stride is a
  kernel-shape decision, `docs/tma-gather.md:70-72,90-92`).
- **GB10**: batch/head is the outermost grid dim; each `(b,h)` attention is a
  `T x hd` TMA-movable tile. For GPT-2 `T=1024`, `T x T x 4B = 4 MB` exceeds the
  99 KB SMEM budget (`docs/tma-gather.md:90-92`), so the score must be
  flash-tiled — the megafusion the campaign already plans
  (`docs/compute-campaign.md:39-45`); the segment/extent-role model feeds that
  tiler the `T` extent directly.
- **LOOP**: `get_batch` writes `B` windows into the contiguous `B*T x C` buffer;
  the segment attribute tells attention `T`.
- **COST**: attention subsystem + extent roles only — NOT the whole stack. Rides
  Foundation A / candidate B (`habu-foundation-a1-declarable-98aebe7b`,
  `habu-extent-typed-tensor-bde435dc`), which the epic already sequences
  (`docs/nanogpt-inventory.md:129-137`).

### Option E — batched-matmul primitive + existing mask/softmax

- Same memory story as D. **MASK/SPEC/AG**: weaker — the segment fact is split
  across three ops (batched-matmul, mask, softmax) that must each carry the block
  stride, so a mismatch between them is representable (softmax over the wrong
  block size type-checks). D concentrates the fact in one attention op/spec, so
  the block structure is decided once. **COST**: slightly smaller than D
  (no new sublayer op) but re-fragments what D unifies; and it still needs the
  extent roles to make the block size a checked type rather than a raw attr.
  Viable fallback if the SPEC: sublayer op slips; inferior as the primary answer.

## 3. Where the two flagship needs pull apart (stated, not averaged)

- **nanoGPT training** wants the smallest correct thing that runs on the 32768-
  cell host arena and gradchecks. It does not care about extent roles or planner
  visibility. Its happiest path is **Option C** (host batch loop + grad
  accumulation): correct, cheap, and it de-risks the numerics before `SPEC:`
  lands (the epic's stated order, `docs/nanogpt-inventory.md:138-147`). The host
  arena *cannot* hold GPT-2-scale `(B,T,C)` regardless (Option B buys nothing
  here).
- **The Triton-reimpl / GB10 planner** wants the batch/sequence/head structure
  **visible as extent roles** so it can derive batched-contraction dataflow, TMA
  boxes, and flash tiling (`docs/tma-gather.md:20-45`, `docs/compute-campaign.md:39-45`).
  Option C is a **dead end** for it — the batch never enters the planner. Option
  B over-serves it (true 3D) at a stack-wide cost.

These genuinely pull apart on one question: **must the batch/sequence structure
be visible to the planner, or can it be an outer host loop?** Training says "host
loop is fine"; the planner says "must be a visible extent". Option D is the only
choice that serves both from **one** decision: the *same* extent-role facts that
the planner reads to lower a batched kernel are the facts the host segment op
loops over — and, in the interim before the segment op and `SPEC:` land, the host
batch loop (Option C) runs underneath the same 2D `B*T`-row layout D mandates, so
C is not thrown away but becomes D's training-time execution strategy.

## 4. Recommendation

**Adopt Option D. Concretely, the convention is:**

1. **Memory layout — 2D, batch outermost.** Tensors stay
   `(rows = B*T, cols = C)`, `layout row` / C-contiguous
   (`maki/tensor-value.f:64`). **B is the slowest-varying (outermost) axis**:
   sequence `b` occupies the contiguous row block `[b*T, b*T+T)`. No third extent
   family, no stride cell, no change to `TV-DESC`/`MIR-INPUT+`/the 28 row-local
   ops. This is exactly the layout `SCRATCH-MLP` already uses
   (`maki/from-scratch-model.f:126`) and is provably identity-correct for every
   per-token op.

2. **Stride order.** Row-major with strides `(T*C, C, 1)` for `(b, t, c)`. Batch
   stride `T*C` is the largest, satisfying the "batch = slowest-varying" kernel
   rule (`docs/tma-gather.md:70-72`). Head split for MHA is a `C = H*hd` column
   partition, so head stride is `hd` within a row — heads are contiguous column
   sub-ranges, the natural SLICE/RESHAPE target (`maki/cad.f:665` `RB-SLICE`,
   `:655` `RB-RESHAPE`).

3. **Sequence boundary witnessed to the checker/planner as an EXTENT ROLE, not
   data.** Add nominal extent roles `#B` (batch), `#T` (sequence/time), `#H`
   (head) in the candidate-B surface (`docs/golden-syntax.md:56-71`), riding
   Foundation A (`habu-foundation-a1-declarable-98aebe7b`). A row index is
   typed `idx<#B*#T>`; the segment op re-types it as `idx<#B>` (free) x `idx<#T>`
   (in-block). A contraction accepts only the in-block `#T`/`#k` extents, so
   contracting across `#B` is a **role mismatch = checker reject** — the boundary
   is a type, provable before runtime. This is the answer to "why didn't the
   checker catch the cross-sequence leak": once extent roles exist, it *is*
   caught statically; until they do, only the segment op's internal loop enforces
   it and the gap is tracked by sub-dot BTC-5 below.

4. **Mask representation — structural, not a dense tensor.** Causal masking is the
   in-block upper-triangular constraint `j <= i` applied inside the segment op /
   masked-softmax (`habu-causal-attention-mask-1ced9cbd`); the cross-sequence
   block-diagonal is enforced by the segment op contracting only within `[b*T,
   b*T+T)`. **No `(B*T)x(B*T)` mask tensor is ever materialized** — the mask is a
   pair of structural facts (block width `T`, triangular within block) carried in
   the attrs cell (`maki/model-ir.f:116`), not float data.

5. **Interim execution — Option C underneath D's layout.** Until the segment op +
   `SPEC:` land, nanoGPT training runs the 2D IR once per sequence in a host batch
   loop with gradient accumulation (sub-dot BTC-3), on the same `B*T`-row buffer.
   This unblocks the numeric goldens (mask, MHA composition) immediately without
   committing the planner to anything, and is replaced — not rewritten — by the
   segment op when it lands.

### Trade-offs costed

- **We give up** Option B's property that cross-sequence contraction is a type
  error *at the tensor level*. **We recover it** at the extent-role level (point
  3), which is strictly cheaper (rides Foundation A, which the epic builds
  anyway) — but the recovery is **contingent on Foundation A + candidate B
  landing**. Between now and then, the guarantee is enforced only by the segment
  op's construction, an honest gap tracked by BTC-5.
- **We add** one new op-kind (segment/causal attention or batched-matmul) and one
  new adjoint — real, gradcheckable surface, not free.
- **We keep** the 28 row-local ops, the arena, and the 2D descriptor untouched —
  the stack-wide tax of Option B is avoided.
- **We accept** `B`x host passes per step in the interim (Option C), paid down
  when the segment op batches the loop internally.

### The two strongest counterarguments to this recommendation

1. **The static guarantee is deferred and contingent.** The recommendation's
   headline safety property — cross-sequence contraction is impossible — is only
   a *checker* fact once Foundation A + extent-typed accessors land
   (`habu-foundation-a1-declarable-98aebe7b`, `habu-extent-typed-tensor-bde435dc`).
   Until then it is merely a *construction* property of one op, exactly the
   "runtime guard instead of a checked invariant" the project forbids
   normalizing. If the SPEC: chain slips, D degrades to Option E/C with no
   type-level boundary, i.e. the same silent-leak exposure as Option A minus the
   dense mask. Option B would have the guarantee **today**, standalone, at the
   cost of blast radius.

2. **A segment attribute on a 2D tensor is a second, weaker encoding of
   dimensionality.** With `B*T` folded into rows, *nothing in the tensor type*
   stops a plain `MATMUL` node (`maki/op-kind.f:29`) from being emitted over the
   full `B*T` rows and contracting across sequences — the bad program is
   representable and only avoided by authors choosing the segment op. A true 3rd
   dimension (Option B) makes the batch extent a first-class thing the matmul
   *must* not contract, which is the stronger, more uniform invariant and the
   more idiomatic Habu answer ("make the bad state unrepresentable"). The
   rebuttal — that extent roles restore this — is real but itself depends on the
   Foundation A churn window and adds a role-mismatch rule to the contraction
   checker that does not exist yet (BTC-2/BTC-5).

## 5. Follow-up implementation sub-dot drafts

Drafts only; the orchestrator mints them under `-P habu-epic-nanogpt-in-b239aa8d`.
Titles are ≤50-char imperative where used as subjects.

---

**BTC-1 — Segment/causal self-attention op-kind + host reference + adjoint**

Add a token-mixing op that contracts block-diagonally per sequence and causally
within a block, so `(B,T,C)` attention runs on the 2D `B*T`-row layout without a
`(B*T)x(B*T)` score. Convention per `docs/batch-sequence-design.md` §4: rows =
`B*T`, B outermost (block `[b*T, b*T+T)`), block width `T` and the triangular
constraint carried in the attrs cell (`maki/model-ir.f:116`, read like
`maki/cad.f:667` `RB-SLICE`). New op-kind in the enum (`maki/op-kind.f:20-62`)
with `OPKIND>N` wire code; host reference composing the existing `MM-NT`
(`maki/attention.f:18`), scaled masked `SM-FWD` (`maki/softmax.f:26` +
`habu-causal-attention-mask-1ced9cbd`), and `MATMUL` (`maki/matmul.f:22`) per
block over row offsets; executor dispatch arm (`maki/executor.f:343`). Adjoint =
the per-block transposed-matmul rule of `BW-STEP-MATMUL` (`maki/backward.f:240`)
plus the unchanged per-row `BW-STEP-SOFTMAX` (`maki/backward.f:226`); masked
positions carry zero grad structurally (no new adjoint for the mask). Gradcheck
against a from-scratch multi-sequence reference. Dep: causal-mask dot
`habu-causal-attention-mask-1ced9cbd`; composes with MHA dot
`habu-multi-head-self-a1e0692f`. Fail-closed: a non-segment `MATMUL` over `B*T`
rows must NOT be silently used for attention (BTC-5 makes it a checker reject).

---

**BTC-2 — Batch/sequence/head extent roles (#B, #T, #H) in the SPEC:/candidate-B
surface**

Introduce nominal extent roles `#B`, `#T`, `#H` as declarable integer types
(candidate B, `docs/golden-syntax.md:56-71`), so a row index is typed
`idx<#B*#T>` and the segment op (BTC-1) re-types it as free `idx<#B>` x in-block
`idx<#T>`. Extend the GGEMM schematic (`docs/golden-syntax.md:81`) with a free
(non-contracted) batch index: `S[b h i j] = Q[b h i k] K[b h j k] * +Σk`,
`+Σ` only over `k`. Emits (1) the checked candidate-B accessor bodies, (2) the
planner dataflow with the batched-contraction free extent
(`docs/tma-gather.md:29-45`), (3) the PROMOTE stride/TMA-box obligations with B
outermost (`docs/tma-gather.md:70-72,90-92`). Dep (hard): Foundation A
`habu-foundation-a1-declarable-98aebe7b`; extent-typed accessors
`habu-extent-typed-tensor-bde435dc`; SPEC: word `habu-spec-word-generating-0729fbea`.
This is the capability that turns BTC-5's guarantee from construction-only into a
checker fact.

---

**BTC-3 — Host batch-loop + gradient accumulation trainer (interim execution)**

Unblock nanoGPT training on the 32768-cell host arena (`maki/executor.f:75`)
before the segment op/SPEC land: run the 2D IR once per sequence in a host loop,
binding each sequence's `T x C` slice via `EX-BIND` (`maki/executor.f:413`) and
**accumulating** parameter gradients across iterations. Extend the single-pass
grad read `SC-GRAD-AT` / `ADAM-UPD` (`maki/adam-train.f:250-253`,
`maki/from-scratch-train.f:80`) with a per-slot running-sum buffer, applying Adam
once per step from the accumulated grads. Same `B*T`-row layout as the final
design (§4), so this is D's interim execution strategy, not a throwaway. Composes
with weight-tying grad-accumulation `habu-weight-tying-wte-ab4145da` (same
accumulate-across-slots pattern). No IR/checker change. Fail-closed: accumulation
must zero the running buffer at step start (an un-zeroed buffer is a silent
grad-leak across steps — add a focused test).

---

**BTC-4 — get_batch windowed loader into the B*T-row contiguous buffer**

Shape `get_batch` output to the recommended layout: `B` sequence windows of
length `T` written as the contiguous row block layout `(rows = B*T, cols = C after
embedding)`, B outermost. Coordinates with the char tokenizer/loader dot
`habu-tiny-shakespeare-char-125d9684`: the loader emits `(B,T)` int token windows
and `(B,T)` shifted targets; this dot places them into the `B*T`-row embedding
input (gather `EMB-GATHER` `maki/embedding.f:23`) and the cross-entropy target
vector (`habu-cross-entropy-loss-93356943`). Emits the segment attribute value
`T` for BTC-1. Dep: `habu-tiny-shakespeare-char-125d9684`. Fail-closed: a batch
whose `B*T` exceeds the row cap / arena must throw, not truncate.

---

**BTC-5 — Negative regression: cross-sequence contraction is a checker reject**

The soundness closer. Once BTC-2's extent roles land, add a minimal **checked
negative fixture** proving that a plain `MATMUL` over `idx<#B*#T>` rows fed where
a within-sequence `idx<#T>` contraction is required is a **load-time checker
reject** (exit 70), not a runtime error — the answer to "why didn't the checker
catch the cross-sequence leak". Mirror the fail-closed discipline of the existing
`SHP-LEGAL?` param-shape rejects (`maki/cad.f:338,373`). Until BTC-2 lands, this
dot holds the documented gap: the boundary is enforced only by BTC-1's
construction, and this negative test is the acceptance criterion that the gap is
closed. Dep: BTC-2. This dot must not be closed by a runtime guard — only by the
checker rejecting the reduced bad program.

---

**BTC-6 — GB10 batched-attention tiling plan node (planner/PROMOTE side)**

Consume BTC-2's `#B`/`#T`/`#H` extents in the movement/schedule planner to lower
a batched attention: batch/head as the outermost grid extent, each `(b,h)` a
`T x hd` TMA-movable tile, flash-tiled when `T x T` exceeds the 99 KB SMEM budget
(`docs/tma-gather.md:90-92`, `docs/compute-campaign.md:39-45`). Records the chosen
lowering + evidence per the §7.4 store like the existing MOVE plan node
(`docs/tma-gather.md:29-45`). Dep: BTC-2; sequences after the sm_121a process row
(`docs/tma-gather.md:83-95`) and the attention megafusion
(`docs/compute-campaign.md`). North-star only — out of the numeric-golden critical
path, but the reason B-outermost is fixed now (§4.2).

## 6. Open questions

1. **Head dimension as a 4th axis.** MHA is `(B, nh, T, hd)`; this design treats
   heads as a `C = H*hd` column partition (§4.2) handled by SLICE/RESHAPE
   (`maki/cad.f:655,665`). Is a column-partition head split sufficient for the
   planner to tile per-head, or does the head need its own extent role `#H`
   distinct from a column slice (BTC-2 assumes the latter)? Resolve with the MHA
   composition dot `habu-multi-head-self-a1e0692f`.
2. **Variable `T` across a batch (ragged sequences).** GPT-2 pretraining uses a
   fixed `T` (packed windows), so this design assumes uniform `T`. Ragged `T`
   would make the segment op's block width per-sequence data, not a single attr —
   a `MOVE`/ragged-GEMM schedule axis already anticipated
   (`docs/tma-gather.md:96-105`). Out of scope for GPT-2-small; flag if a ragged
   loader is ever wanted.
3. **Does the segment fact belong on one attention op (D) or fragmented across
   batched-matmul + mask + softmax (E)?** §2 recommends D; if the `SPEC:`
   sublayer op slips, is E's three-op fragmentation an acceptable interim, or is
   the host loop (C) preferable to keep the block fact from being splittable?
4. **Arena scale for multi-sequence host goldens.** Even Option C holds one
   `T x C` sequence; a modest `T=128, C=768` is `98304` cells > the 32768-cell
   arena (`maki/executor.f:75`). Does the host golden need a larger arena, or do
   goldens run at toy `T`/`C` and PROMOTE handles real scale? Likely toy-scale
   goldens, but confirm against the gradcheck harness before BTC-3 sizing.
