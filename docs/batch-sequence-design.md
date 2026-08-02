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

The 32 op-kinds (`maki/op-kind.f:20-62`) fall into three classes under a `B*T`
row fold. The classes matter because two *different* correctness arguments carry
them, and only the token-mixing contraction is unsafe.

**Class (i) — truly row-independent ops (fold = identity).** Each acts per row
(or per element / per row over `C`), so which row is which token of which
sequence is irrelevant.
- Elementwise: `add mul scale bias residual-add cast relu gelu silu`
  (`maki/executor.f:139` `EX-U`, `:170` `EX-EW2`) and rotary `rope`
  (`maki/executor.f:326` `EX-ROPE-FWD`, per row over column pairs).
- Row-reduce forward: `layernorm rmsnorm softmax-row`
  (`maki/executor.f:200` `EX-ROW-FWD`, per row over `C`).
- Their backward duals: `relu-bwd gelu-bwd silu-bwd gelu-bwd2` (elementwise,
  `EX-EW2`), `layernorm-bwd rmsnorm-bwd softmax-row-bwd`
  (`maki/executor.f:227` `EX-ROW-BWD`), `rope-bwd`; `BW-STEP-COPY`
  (`maki/backward.f:203`), `BW-STEP-UNARY` (`:220`), `BW-STEP-SOFTMAX` (`:226`)
  are all row-local.
- Proof it already works: `MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 ... ) LINEAR GELU
  LINEAR ;` (`maki/from-scratch-model.f:126`) — the `8` is a batch of 8 rows and
  the MLP is per-row-correct today.

**Class (ii) — contractions + movement whose row-axis meaning interacts with the
fold.** These read the row axis *as a logical dimension*, so the fold changes
what they compute:
- `linear` and `matmul` where operand 1 is a **parameter** weight (`C x N`)
  project each row independently — per-row-correct under the fold (this is the
  MLP proof above; `maki/executor.f:241` `EX-LINEAR`, `:235` `EX-MATMUL`). But
  `matmul` of **two `B*T`-row activations** contracts the row axis itself:
  `MODEL: ADAM-ATTN ( q:4x3 kt:3x4 s:1x1 v:4x3 -- o ) MATMUL SCALE SOFTMAX-ROW
  MATMUL ;` (`maki/adam-train.f:224`, `L=4 d=3`) folded to a batch makes
  `MATMUL(Q[B*T x d], Kt[d x B*T]) = S[B*T x B*T]` contract **every token
  against every other token across all sequences** — the cross-sequence leak.
  `MM-NT` (`maki/attention.f:18`) and unmasked `SM-FWD` (`maki/softmax.f:26`)
  have no boundary notion. This is the ONLY genuinely unsafe use.
- `transpose` (`maki/executor.f:261`, `maki/cad.f:662` `RB-TRANSPOSE`) swaps the
  row axis into columns — meaningful only inside the attention block (`Kᵀ`, the
  matmul adjoints `maki/backward.f:240`), where it is per-block.
- `slice` (`maki/cad.f:665` `RB-SLICE`, `maki/executor.f:266`) is a row window
  `[r0,r1)` — can cut across a sequence boundary if `r0/r1` are not `T`-aligned
  (used for `wpe[0..T]` and the concat adjoint); `concat` (`maki/cad.f:670`
  `RB-CONCAT`) appends rows. Both are row-axis ops, fold-aware.
- `gather` forward (`maki/executor.f:278`, `maki/embedding.f:23` `EMB-GATHER`)
  is per-output-row-independent — each gathered row is one token's table lookup,
  so it is fold-safe; its adjoint is class (iii).
- `reshape` (`maki/executor.f:256`, `maki/cad.f:655` `RB-RESHAPE`) is
  **element-order-preserving** — it reinterprets `RxC` keeping the flat order,
  so it cannot cleanly extract a sub-axis out of the fold (see F2 / Open Q1).

**Class (iii) — shared-parameter reduction adjoints (cross-row, but CORRECT).**
These deliberately reduce *across* the `B*T` fold, and that is right because the
reduction axis is exactly the shared-parameter broadcast axis — a different
argument than row-independence:
- `rowsum-bwd` (`maki/executor.f:286`) sums the cotangent over all rows → `1xC`:
  the bias is one `1xC` parameter broadcast to every `B*T` row
  (`maki/cad.f:340` `SHP-ROW-OK?`), so `dBias = Σ` over all `B*T` rows is the
  correct gradient, not a leak.
- `fullsum-dot-bwd` (`maki/executor.f:291`) full-reduces `Σ(ct ⊙ x)` → `1x1`:
  the scalar `scale` is shared over every element, so its grad sums the whole
  fold.
- `scatter-add` (`maki/executor.f:301`, `maki/embedding.f:41`
  `EMB-SCATTER-ADD`) accumulates cotangent rows into the shared vocab table at
  the gathered ids; every position across all `B*T` that used a table row sums
  into that row's grad — correct because the table row is shared across
  sequences. (`pad-scatter`, the slice adjoint, is a movement scatter, class-(ii)
  adjacent.)

**Conclusion that frames the whole decision:** the "(B,T,C)" question is *not*
global. Only class (ii)'s activation-activation `matmul` (attention) is unsafe
under the fold; class (i) is identity and class (iii) is a *correct* cross-row
reduction. Any option that imposes a third dimension on the class-(i)/(iii) ops
pays a stack-wide tax for zero correctness benefit there; any option that leaves
attention as a plain 2D `matmul` over `B*T` rows is silently wrong.

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
iterations (the grad read `SC-GRAD-AT` (`maki/from-scratch-train.f:80`) already
reads one grad node per slot and the Adam apply `ADAM-UPD`/`ATN-APPLY`
(`maki/adam-train.f:250-253`) consumes it; accumulation adds a running sum). Attention
is per-sequence-correct because each IR invocation *is* one sequence.

### Option D — batch/seq/head as SPEC: extent roles, lowered to a *segment
attribute* on the token-mixing ops only (2D memory)

Memory stays 2D `rows = B*T`, row-major, **B outermost** so sequence `b` is the
contiguous row block `[b*T, b*T+T)`. The batch/sequence/head structure is carried
as **extent roles** in the SPEC:/candidate-B surface (`#B`, `#T`, `#H`;
`docs/golden-syntax.md:56-71`) and lowered onto a **segment/causal attribute**
(the same attrs cell that already carries slice ranges, `maki/model-ir.f:116`
`MI-ATTR`, read at `maki/cad.f:665` `RB-SLICE`) on exactly the four token-mixing
ops. A segmented attention op contracts block-diagonally per `T`-block and
causally within it; it never materializes a `(B*T)x(B*T)` score. The class-(i)
and class-(iii) ops (§0) are untouched. This is the option the code makes viable that the
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
- **EXE / memory**: `(B*T)^2` score buffer. At GPT-2-small `B=12,T=1024` →
  `(12288)^2 = 1.5e8` cells vs the correct `12*1024*1024 = 1.26e7` — **12x**
  waste, and it exceeds the 32768-cell arena (`maki/executor.f:75`) by
  `1.5e8/32768 ≈ 4.6e3` ⇒ ~3.7 orders of magnitude (and that is GPT-2-small
  scale, not toy).
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
  in maki's model stack and taxes the class-(i)/(iii) ops (§0) that gain nothing.
  Flagship code waits for the type-family surface; a bespoke 3rd extent
  duplicates the extent-role machinery Foundation A is already building.

### Option C — executor-level batch loop

- **MASK**: correct by construction — one sequence per invocation, so attention
  never sees another sequence; causal masking reduces to the within-sequence dot
  `habu-causal-attention-mask-1ced9cbd`, no cross-sequence concern at all.
- **SPEC**: **invisible to the planner** — the planner sees a single `T x C`
  graph per step and never learns `B`. This is the flagship split (see §3): fine
  for training, a dead end for the Triton-reimpl planner, which needs the batch
  extent to map the grid and amortize launches.
- **AG**: no walker change; gradients accumulate across the host loop (extend the
  per-slot grad read `SC-GRAD-AT` (`maki/from-scratch-train.f:80`) and the Adam
  apply `ADAM-UPD`/`ATN-APPLY` (`maki/adam-train.f:250-253`) with a running sum
  buffer). Weight-tying grad-accumulation (`habu-weight-tying-wte-ab4145da`)
  composes naturally — it is the same accumulate-across-slots pattern.
- **EXE / memory**: smallest footprint — arena holds one sequence
  (`T*C`), reused `B` times. `B` forward+backward passes per step (`B`x slower,
  but correct). No cross-batch statistic exists in GPT (no BatchNorm), so
  per-sequence independence is legitimate.
- **PROMOTE / GB10**: nothing to promote for the batch — the planner is not told,
  so GB10 cannot batch the kernel. **This is the disqualifier for the north-star**
  (the epic goal dot's mission: "then PROMOTE through the compute campaign to
  train on GB10", `.dots/habu-epic-nanogpt-in-b239aa8d.md` "North star (2)").
- **LOOP**: cleanest — `get_batch` yields `B` windows, the loop binds each.
- **COST**: minimal, trainer-only. But it does not *decide the convention*; it
  postpones it. Correct as an interim training harness, wrong as the answer.

### Option D — extent roles + segment attribute on token-mixing ops (2D memory) — RECOMMENDED

- **MASK**: cross-sequence contraction is **unrepresentable** once the segment op
  is the only token-mixer: it contracts strictly within a `T`-block, and the
  causal constraint `j <= i` lives inside that block. With extent roles
  (`idx<#T>` vs the folded row role, `docs/golden-syntax.md:59-71`) a plain matmul
  over the folded rows cannot be fed to a contraction expecting `#T` — a **checker
  reject**, restoring Option B's static guarantee without Option B's bytes. This
  demands **factoring** the folded row role into free `#B` x in-block `#T`, which
  A1's flat nominal roles cannot express — it needs a new extent-role
  product/factorization capability (sub-dot BTC-7), priced honestly in §4.
- **SPEC**: batch/seq/head become extent roles; the GGEMM schematic
  (`docs/golden-syntax.md:81`) grows a free (non-contracted) index:
  `S[b h i j] = Q[b h i k] K[b h j k] * +Σk`, `+Σ` still only over `k`. The
  planner derivation gains a **batched contraction** = the same contraction
  replicated over the free extent — exactly what a batched HMMA/tcgen05 GEMM and
  flash-attention want (`docs/compute-campaign.md:39-45`). Aligned with the
  Triton-reimpl planner's `idxctx`/extent needs (`docs/tma-gather.md:17-19`).
- **AG**: the adjoint of a segmented/batched matmul is the segmented/batched
  transposed matmul — the **same rule** as `BW-STEP-MATMUL`
  (`maki/backward.f:240`) replicated per segment. Softmax-row adjoint
  (`maki/backward.f:226`) is untouched: softmax stays per row (one query's
  distribution over its `<=t` in-sequence keys). Masked positions are
  structurally absent, so their grads are zero with **no new adjoint**. Net: one
  new adjoint (the segment attention / batched matmul), zero change to the
  class-(i)/(iii) adjoints (§0).
- **EXE / memory**: 2D arena unchanged; the segment op loops `B` blocks
  internally over row offsets `b*T` (reusing `MATMUL` `maki/matmul.f:22` and
  `MM-NT` `maki/attention.f:18`). Score memory is `B` independent `T x T` blocks,
  never the `(B*T)^2` dense matrix.
- **PROMOTE**: extent roles give the planner `#B`,`#T`,`#H` to derive strides and
  TMA box dims. **B outermost** ⇒ its logical stride is `T*C`, the largest — a
  derivation (see §4.2), not a cited rule; the external constraint the planner must
  respect is the SMEM box budget (`docs/tma-gather.md:90-92`), a size limit.
- **GB10**: batch/head is the outermost grid dim; each `(b,h)` attention is a
  `T x hd` TMA-movable tile. For GPT-2 `T=1024`, `T x T x 4B = 4 MB` exceeds the
  99 KB SMEM budget (`docs/tma-gather.md:90-92`), so the score must be
  flash-tiled — the megafusion the campaign already plans
  (`docs/compute-campaign.md:39-45`); the segment/extent-role model feeds that
  tiler the `T` extent directly.
- **LOOP**: `get_batch` writes `B` windows into the contiguous `B*T x C` buffer;
  the segment attribute tells attention `T`.
- **COST**: attention subsystem + extent roles — NOT the whole stack — but *not
  free*. It rides Foundation A / candidate B
  (`habu-foundation-a1-declarable-98aebe7b`, `habu-extent-typed-tensor-bde435dc`),
  which the epic sequences anyway, AND it
  adds **new checker capability beyond A1**: A1 delivers only *flat* nominal roles
  with same/other/generic-int unification
  (`.dots/habu-foundation-a1-declarable-98aebe7b.md`, "flat" per
  `docs/golden-syntax.md:68`), so factoring a folded row role into `#B` x `#T`
  needs an extent-role **product/factorization** type former + contraction rule
  that no dot yet covers — priced as sub-dot BTC-7. So the honest bill is: one new
  op-kind + adjoint (BTC-1), the extent roles (BTC-2, rides A1), AND the
  factorization capability (BTC-7, genuinely new checker work).

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
  lands (the epic's stated order). The host
  arena *cannot* hold GPT-2-scale `(B,T,C)` regardless (Option B buys nothing
  here).
- **The Triton-reimpl / GB10 planner** wants the batch/sequence/head structure
  **visible as extent roles** so it can derive batched-contraction dataflow, TMA
  boxes, and flash tiling (`docs/tma-gather.md:29-45`, `docs/compute-campaign.md:39-45`).
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
   family, no stride cell, no change to `TV-DESC`/`MIR-INPUT+`/the class-(i)/(iii)
   ops (§0). This is exactly the layout `SCRATCH-MLP` already uses
   (`maki/from-scratch-model.f:126`) and is provably identity-correct for every
   per-token op.

2. **Stride order.** Row-major logical strides `(T*C, C, 1)` for `(b, t, c)` over
   the 2D `B*T x C` buffer, so batch stride `T*C` is the largest. This is a
   derivation from B-outermost, **not a cited rule** — no stride-ordering rule
   exists in `docs/tma-gather.md` (its `:70-72` are TMA *alignment* legality). The
   arithmetic: a batch axis placed slowest-varying gets the outermost kernel-grid
   stride, which is the layout a batched GB10 kernel wants; the only load-bearing
   *external* constraint is the SMEM box budget (`docs/tma-gather.md:90-92`), a
   size limit, not an ordering. **Head split for MHA is deferred to Open Question 1**
   (see F2 / §0): `reshape` is element-order-preserving and `slice` is a row
   window, so neither extracts a `C = H*hd` head partition as contiguous columns —
   heads need `#H` as its own extent role (BTC-2) or a dedicated column-view
   primitive, decided there.

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
  3) — but NOT for free and NOT merely by "riding Foundation A". A1 delivers only
  flat roles (`docs/golden-syntax.md:68`,
  `.dots/habu-foundation-a1-declarable-98aebe7b.md`); the recovery needs an
  extent-role **product/factorization** capability (split a folded role into free
  `#B` x in-block `#T`, make `#T`/`#k` contractable and `#B` not) that no dot
  covers — new checker work, drafted as BTC-7. So the recovery is smaller than
  Option B's stack-wide 3D retrofit, but it is a real new checker feature beyond
  A1, and it is **contingent on A1 + candidate B + BTC-7 landing**. Between now and
  then the guarantee is enforced only by the segment op's construction, an honest
  gap tracked by BTC-5.
- **We add** one new op-kind (segment/causal attention or batched-matmul) and one
  new adjoint (BTC-1) — real, gradcheckable surface — plus the BTC-7 checker
  capability; none of it is free.
- **We keep** the class-(i)/(iii) ops (§0), the arena, and the 2D descriptor untouched —
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
   more idiomatic Habu answer ("make the bad state unrepresentable"). The rebuttal
   — that extent roles restore this — is real but hangs on a capability that **does
   not exist and is not yet dotted beyond this design**: A1's roles are *flat*
   (`docs/golden-syntax.md:68`), so restoring the guarantee requires an extent-role
   **product/factorization** type former (split the folded row role into free `#B`
   x in-block `#T`) plus the contraction rule that `#T`/`#k` are contractable and
   `#B` is not — the missing capability is factorization, not merely a
   role-mismatch rule, and it is drafted here as BTC-7 (hard dep of BTC-2/BTC-5),
   riding the Foundation A churn window but genuinely additional to it.

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
`maki/cad.f:665` `RB-SLICE`). New op-kind in the enum (`maki/op-kind.f:20-62`)
with `OPKIND>N` wire code; host reference composing the existing `MM-NT`
(`maki/attention.f:18`), scaled masked `SM-FWD` (`maki/softmax.f:26` +
`habu-causal-attention-mask-1ced9cbd`), and `MATMUL` (`maki/matmul.f:22`) per
block over row offsets; executor dispatch arm (`maki/executor.f:343`). Adjoint =
the per-block transposed-matmul rule of `BW-STEP-MATMUL` (`maki/backward.f:240`)
plus the unchanged per-row `BW-STEP-SOFTMAX` (`maki/backward.f:226`); masked
positions carry zero grad structurally (no new adjoint for the mask). **Acceptance
is a multi-sequence numeric gradcheck, in construction terms**: a reference that
runs `B>1` sequences and confirms the segment op reproduces `B` independent
per-sequence attentions (zero cross-sequence coupling) with matching forward + VJP
against the single-sequence `ATTN-FWD`/`ATTN-BWD` (`maki/attention.f:37,71`) run
per block. BTC-1 lands BEFORE any extent-role machinery, so it CANNOT itself make
a non-segment `MATMUL` over `B*T` rows a checker reject — that static guarantee is
BTC-5's job on the BTC-2/BTC-7 substrate; BTC-1 only guarantees the segment op,
when used, is per-sequence-correct. Dep: causal-mask dot
`habu-causal-attention-mask-1ced9cbd`; composes with MHA dot
`habu-multi-head-self-a1e0692f`.

---

**BTC-2 — Batch/sequence/head extent roles (#B, #T, #H) in the SPEC:/candidate-B
surface**

Introduce nominal extent roles `#B`, `#T`, `#H` as declarable integer types
(candidate B, `docs/golden-syntax.md:56-71`). Extend the GGEMM schematic
(`docs/golden-syntax.md:81`) with a free (non-contracted) batch index:
`S[b h i j] = Q[b h i k] K[b h j k] * +Σk`, `+Σ` only over `k`. Emits (1) the
checked candidate-B accessor bodies, (2) the planner dataflow with the
batched-contraction free extent (`docs/tma-gather.md:29-45`), (3) the PROMOTE
TMA-box obligations bounded by the SMEM budget (`docs/tma-gather.md:90-92`); the
B-outermost stride order is a derivation (§4.2), not a cited rule.
NOTE: declaring the flat roles is A1's remit, but typing a folded row index as
`#B*#T` and letting the segment op (BTC-1) split it into free `#B` x in-block `#T`
requires the extent-role **product/factorization** capability of **BTC-7** — A1's
flat roles cannot do it. Dep (hard): Foundation A
`habu-foundation-a1-declarable-98aebe7b`; extent-typed accessors
`habu-extent-typed-tensor-bde435dc`; SPEC: word
`habu-spec-word-generating-0729fbea`; **BTC-7 (extent-role product/factorization)**.
Together with BTC-7 this is what turns BTC-5's guarantee from construction-only
into a checker fact.

---

**BTC-3 — Host batch-loop + gradient accumulation trainer (interim execution)**

Unblock nanoGPT training on the 32768-cell host arena (`maki/executor.f:75`)
before the segment op/SPEC land: run the 2D IR once per sequence in a host loop,
binding each sequence's `T x C` slice via `EX-BIND` (`maki/executor.f:413`) and
**accumulating** parameter gradients across iterations. Extend the single-pass
grad read `SC-GRAD-AT` (`maki/from-scratch-train.f:80`) and Adam apply
`ADAM-UPD`/`ATN-APPLY` (`maki/adam-train.f:250-253`) with a per-slot running-sum
buffer, applying Adam once per step from the accumulated grads. Same `B*T`-row layout as the final
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
whose `B*T` (or node count) exceeds the model-IR node cap `MIR-CAP` = 128
(`maki/model-ir.f:100`) or whose node buffers exceed the executor arena
`EX-ARENA-CELLS` = 32768 (`maki/executor.f:73,75`) must throw, not truncate.

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
closed. Dep: BTC-2 (which itself hard-depends on BTC-7's factorization
capability). This dot must not be closed by a runtime guard — only by the checker
rejecting the reduced bad program.

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

---

**BTC-7 — Extent-role product/factorization capability**

The checker capability the recommendation's static guarantee depends on, and which
Foundation A1 does NOT provide. A1 declares only *flat* nominal integer roles with
same-role / other-role / generic-int unification
(`.dots/habu-foundation-a1-declarable-98aebe7b.md`; flat per
`docs/golden-syntax.md:68`); it has no way to say a role is the *product* of two
roles or to *split* one back into its factors. Deliver:
(a) a **type former** `#B*#T` (a product / composite extent role) over declared
flat roles, so a folded row index can be typed `idx<#B*#T>`;
(b) the **factorization / re-typing** rule that the segment op (BTC-1) uses to
split `idx<#B*#T>` into a free outer `idx<#B>` and an in-block `idx<#T>` (arithmetic
identity `#B*#T = rows`), plus the inverse join;
(c) the **contraction rule** in the checker's unification: a contraction (`+Σ`)
accepts an in-block / inner extent (`#T`, `#k`) and REJECTS a free extent (`#B`),
so a `matmul` / segment-attention over `#B*#T` rows that tries to sum the `#B`
factor is a type error (this is the rule that makes the cross-sequence leak
unrepresentable);
(d) where it lives: the checker unification / role registry extended past A1's
flat-role handling (coordinate with `habu-split-checker-f-837bc1a4`'s churn window)
and the candidate-B signature surface (`habu-extent-typed-tensor-bde435dc`) so
accessor and loop-induction sigs can carry a factored role;
(e) **negative fixtures** per rule: a product mistyped against a mismatched factor;
a split whose factors' product ≠ the source extent; and a contraction over a free
`#B` factor (the cross-sequence leak) rejected at load time (exit 70).
Dep (hard): Foundation A1 `habu-foundation-a1-declarable-98aebe7b` (flat roles
first). **Hard prerequisite of BTC-2 and BTC-5** — without it the extent-role
guarantee is not expressible and the design falls back to BTC-1's
construction-only enforcement plus Option C's host loop. Priced in §2 (Option D
COST) and §4 as new checker work *beyond* A1, not a ride on it.

## 6. Open questions

1. **Head dimension.** MHA is `(B, nh, T, hd)`, and this design has **no
   mechanism** to extract the head partition from a `C = H*hd` column layout:
   `reshape` is element-order-preserving (`maki/cad.f:655` `RB-RESHAPE`,
   `maki/executor.f:256`) and there is **no column-slice op** — `slice` is a ROW
   window (`maki/cad.f:665` `RB-SLICE`) — so a naive reshape yields
   **row-interleaved** heads, not clean per-head column views. The design therefore
   assumes `#H` is its **own extent role** (BTC-2/BTC-7), a free/parallel axis the
   segment op and planner iterate like `#B`, not a column slice. If a column-view
   route is ever preferred instead, it needs a **dedicated column-slice / head-view
   movement primitive** (new op-kind + adjoint) with its own dot. Resolve the
   head-axis representation with the MHA composition dot
   `habu-multi-head-self-a1e0692f`.
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
