# nanoGPT-in-Habu inventory

Inventory of maki's model stack against Karpathy nanoGPT (GPT-2-small class),
with the SPEC-grammar requirement list and a build order. Epic
`habu-epic-nanogpt-in-b239aa8d`; produced by `habu-inventory-maki-model-413fc38c`.
Read-only survey — every claim carries `file:line` evidence.

## How maki expresses a model today (the substrate the gaps sit on)

- **Two authoring layers.** (1) *Buffer goldens*: plain checked words over
  contiguous float cell buffers (`T-GET`/`T-SET`, `maki/array.f`) — the CPU
  references, numerically gradcheckable. (2) *The `MODEL:` DSL* (`maki/cad.f`):
  a package-scoped colon definer that captures a dataflow into the shared model
  IR (`maki/model-ir.f`), which `maki/backward.f` differentiates and
  `maki/executor.f` runs.
- **`MODEL:` shape.** One line: `MODEL: NAME ( x:RxC w:RxC ... -- y ) OP OP ... ;`
  (`cad.f:605`). **Single running value**: input 0 is the running tensor; each
  op drains its parameter operands FIFO from pending named refs then declared
  inputs (`cad.f:284` `CAP-EMIT-PARAMS`). `>V NAME` names the current value and
  keeps it running (`cad.f:296`); a bare `NAME` is a pending reference and
  `NAME^T` a transposed reference (`cad.f:555` `CAP-TOKEN`). **Consequence**:
  residual skips ARE expressible (`>V X` … `X RESIDUAL-ADD`, proven in
  `plan-vocab-test.f:80`), but an *internal* contraction of two intermediates
  both derived from the running value (Q@K^T) is NOT — stated at
  `adam-train.f:15`, worked around by passing Q, Kt, V as separate inputs.
- **2D only.** Shapes are `RxC` (`cad.f:461` `PARSE-SHAPE`, `maki/tensor.f` 2D
  shape). There is no batch / 3rd dimension.
- **Op set** (one enum, `maki/op-kind.f:20-40`; `MODEL:`-parseable subset in
  `cad.f:175` `OP-LOOKUP`): `ADD MUL SCALE BIAS RELU GELU SILU LAYERNORM
  RMSNORM SOFTMAX-ROW MATMUL LINEAR RESIDUAL-ADD CAST ROPE RESHAPE:RxC
  TRANSPOSE SLICE:R0..R1 CONCAT GATHER`. Backward ops (`op-kind.f:42-61`) are
  synthesized by `backward.f`, never parseable.
- **Loss is external, not an IR node.** Trainers run the forward slice, read the
  output, and write seed cotangents into the backward seed slot
  (`from-scratch-train.f:73` `SC-LOSS-SEED` → `BW-SEED-SLOT`); the loss itself
  (NLL/MSE) is computed host-side, not captured. Cross-entropy is not wired.

## Inventory table

State: **EXISTS** (golden-only / device-lowered / autograd-covered noted) ·
**PARTIAL** (what is missing) · **ABSENT**.

| nanoGPT piece | State | Evidence | Notes / gap |
|---|---|---|---|
| Token embedding (gather) | **EXISTS** (golden + op + adjoint) | `embedding.f:23` `EMB-GATHER`, `:41` `EMB-SCATTER-ADD`; `op-kind.f:40` `OP-GATHER`; `cad.f:195` GATHER in `OP-LOOKUP`; adjoint `OP-SCATTER-ADD` `op-kind.f:57` | Buffer golden + a `MODEL:`-parseable GATHER op with a scatter-add VJP. Gradcheck `embedding-test.f`. |
| Positional embedding (learned wpe) | **PARTIAL** | ROPE (rotary) only: `rope.f`, `op-kind.f:34` | Learned absolute wpe table + the token+pos ADD composition are missing → `habu-learned-positional-embedding-43b259e2`. |
| LayerNorm | **PARTIAL** (no affine) | `layernorm.f:32` `LN-FWD`, `:54` `LN-BWD`; `op-kind.f:26` `OP-LAYERNORM` (unary); adjoint `op-kind.f:45` | golden and op are **no-affine**; GPT-2 needs gamma/beta + their grads → `habu-affine-layernorm-gamma-d19a57e0`. |
| Multi-head causal self-attention | **PARTIAL / ABSENT** | golden single-head/no-mask/no-batch `attention.f:37` `ATTN-FWD`, `:71` `ATTN-BWD`; DSL cannot express internal Q@K^T `adam-train.f:15` | No multi-head, no causal mask, no in-DSL QKV projection. Composition dot `habu-multi-head-self-a1e0692f`; mask dot below. |
| Causal mask | **ABSENT** | `softmax.f` `SM-FWD` is unmasked; no mask op in `op-kind.f` | → `habu-causal-attention-mask-1ced9cbd`. |
| Softmax | **EXISTS** (golden + op + adjoint) | `softmax.f:26` `SM-FWD`, `:42` `SM-BWD`; `op-kind.f:28` `OP-SOFTMAX-ROW`; adjoint `:47` | Numerically stable (max-subtract). Row-wise, unmasked. |
| MLP with GELU | **EXISTS** (autograd-covered) | `mlp.f:16` `MLP-FWD` (LINEAR GELU LINEAR); `gelu.f:19` `GELU-F`, `:49` `GELU-BWD`; ops `op-kind.f:25,30` | Full forward+backward+SGD; `MODEL: … LINEAR GELU LINEAR` proven (`from-scratch-model.f:126`). GELU is tanh-approx. |
| Linear / MATMUL | **EXISTS** (autograd-covered) | `linear.f:24` `LINEAR`, `:42` `LINEAR-BWD`; `matmul.f:22` `MATMUL`, `:38` `MATMUL-DX`, `:57` `MATMUL-DW`; ops `op-kind.f:29,30` | matmul adjoints are transposed matmuls (`backward.f:240`). |
| Residual connections | **EXISTS** (op + adjoint + DSL) | `op-kind.f:31` `OP-RESIDUAL-ADD`; skip via `>V`/named ref `plan-vocab-test.f:80` | Adjoint is copy. |
| Final LayerNorm | **PARTIAL** | same as LayerNorm | Same affine gap. |
| LM head | **EXISTS** (as LINEAR/MATMUL) | `linear.f`, `matmul.f` | Just a Linear to vocab; no separate work beyond composition. |
| Weight tying (wte↔lm_head) | **ABSENT** | executor binds each slot to a distinct buffer `executor.f` `EX-BIND` | Shared buffer + gradient accumulation across two slots → `habu-weight-tying-wte-ab4145da`. |
| Cross-entropy loss | **PARTIAL** (golden only, not wired, one-hot) | `celoss.f:16` `CE`, `:20` `SOFTMAX-CE-BWD` (=y−t) | one-hot + pre-softmax only; NOT in `loss-tensor.f`; NOT seeded by any trainer. Needs logits+int-target log-softmax CE + seed wiring → `habu-cross-entropy-loss-93356943`. |
| Adam | **EXISTS** (golden + tensor) | `optim.f:37` `ADAM`, bias-correction `adam-train.f:57`; tensor `optim-tensor.f:21` `TT-ADAM!` | Host-runnable, gradcheck-driven trainers `adam-train.f`. |
| AdamW (decoupled decay) | **PARTIAL** | `optim.f:22` `WEIGHT-DECAY` scalar exists but `TT-ADAM!` / `ADAM-UPD` (`adam-train.f:67`) apply NO decay | decoupled decay + param-group policy → `habu-adamw-decoupled-weight-d322fe1f`. |
| Cosine LR + warmup | **ABSENT** | lr is a constant `from-scratch-train.f:49` `SC-LR`, `adam-train.f:86` `AMT-LR` | → `habu-cosine-lr-schedule-77c2d0f2`. |
| Gradient clipping (global norm) | **ABSENT** | no clip word in maki | → `habu-global-norm-gradient-c164eb61`. |
| Dropout | **ABSENT** | none | pretrain default 0.0 → low priority `habu-dropout-op-train-fe0ad08d`. |
| Weight init | **PARTIAL** (uniform only) | LCG uniform `from-scratch-model.f:109` `SC-SMALL`, `adam-train.f:191` `ATN-FILL`; no normal RNG | needs normal(0,0.02), residual-scaled init, LN 1/0, bias 0 → `habu-nanogpt-weight-init-b2fc5b4f`. |
| Gradient checkpointing (activation remat) | **EXISTS** | `checkpoint.f` `CK-FWD`/`CK-BWD`, bit-identical remat | Present; distinct from training-resume checkpointing. |
| Checkpoint save/load (resume) | **ABSENT / unverified** | `store.f`, `golden-artifact.f` are eval artifacts, not param+optimizer state | persist params + Adam m/v + step → `habu-training-state-checkpoint-3907d0d4`. |
| Char tokenizer (tiny-shakespeare v0) | **ABSENT** | all data is synthetic LCG `from-scratch-model.f:102` `SC-GEN-DATA` | vocab/encode/decode + get_batch windows → `habu-tiny-shakespeare-char-125d9684`. |
| Batch dimension (B,T,C) | **ABSENT / structural** | IR is 2D `cad.f:461`, `tensor.f` 2D | B*T-as-rows breaks per-sequence causal attention → `habu-batch-sequence-tensor-006f25a1`. |

## Gap list (summary, all minted as sub-dots)

Model math: affine LayerNorm; causal mask; multi-head self-attention
composition; positional embedding + embed-sum; cross-entropy over logits+int
targets; weight tying. Training: AdamW decoupled decay; cosine LR + warmup;
global-norm grad clip; nanoGPT weight-init + Gaussian RNG; dropout;
training-state checkpoint. Data/shape: char tokenizer + loader; batch/sequence
(B,T,C) dimension; whole GPT-2 block + model composition.

## SPEC-grammar requirements (docs/golden-syntax.md candidate C)

The `SPEC:` word (`habu-spec-word-generating-0729fbea`) must express these forms,
drawn from the GPT-2-small op set. Notation follows the doc's worked example
`SPEC: GGEMM O[m n] = A[ ix[m] k ] B[n k] * +Σk ;`.

**Contractions the grammar must express (`* +Σ<extent>`):**
- Plain GEMM `O[m n] = A[m k] B[k n] * +Σk` — Linear / LM head (`matmul.f:22`).
- Transposed-operand GEMMs — the matmul adjoints `dX = ct @ Wᵀ`, `dW = Xᵀ @ ct`
  (`backward.f:240`, `matmul.f:38,57`) and attention `S = Q @ Kᵀ`
  (`attention.f:14` `MM-NT`, `:49` `MM-TN`). Needs a transpose/index-swap on an
  operand inside the contraction.
- Two contractions bridged by a row op — attention `softmax(Q@Kᵀ·s)@V`
  (`adam-train.f:224` `MODEL: ADAM-ATTN`): the grammar must compose
  contraction → row-reduction op → contraction in one spec.

**Gather / indexing forms:**
- Row gather by an index tensor `A[ ix[m], k ]` (the doc's example; embedding
  `E[ids[i], :]`, `embedding.f:23`). The index tensor's *element* type is the
  row-space it selects into (`idx<#M'>`), the fact the device `uniqidxctx`
  wants witnessed (golden-syntax.md:69).
- Positional slice `wpe[0..T, :]` — a static row range (`SLICE:R0..R1`,
  `cad.f:510`).

**Reductions (row-wise and full):**
- Row max + row sum for stable softmax (`softmax.f:12,17`) — kept as a plain
  named op (`SOFTMAX-ROW`), not open-coded in the spec.
- Row mean + row variance for LayerNorm (`layernorm.f:15,18`) — plain op.
- Vocab-axis log-sum-exp for cross-entropy (`celoss.f`) — plain op.
- Bias grad = column/row sum `Σr ct[r,c]` (`op-kind.f:54` `OP-ROWSUM-BWD`);
  scale grad = full-reduce dot `Σ (ct ⊙ x)` (`op-kind.f:55` `OP-FULLSUM-DOT-BWD`).

**Broadcasts (the shape classes `SHP-LEGAL?` already encodes, `cad.f:338`):**
- Row broadcast `1×C` over rows — bias add (`cad.f:340` `SHP-ROW-OK?`).
- Scalar `1×1` broadcast — scale (`cad.f:341` `SHP-SCALE-OK?`).
- Same-shape elementwise — residual/add/mul (`cad.f:342-344`).

**Ops that STAY plain named words (the SPEC escape hatch, golden-syntax.md:98):**
softmax internals (max/exp/÷, `softmax.f`), GELU (tanh approx, `gelu.f`),
LayerNorm internals (mean/var/√/eps, `layernorm.f`), and all transcendentals
(`fmath.f` FEXP/TANH-F/FLN). `SPEC:` references these by name as ops; it does
not open-code their numerics. It expresses the *dataflow* (contractions,
gathers, index/reduction extents, broadcasts) and derives (1) the checked
candidate-B golden, (2) the planner dataflow, (3) the PROMOTE shape
obligations.

## Proposed build order (Phase 0 type-family-first)

The epic makes the **SPEC: chain critical path**; it already has dots — do NOT
re-mint:

0. **Foundation A1** — declarable nominal integer types
   (`habu-foundation-a1-declarable-98aebe7b`). Prerequisite: extent roles are
   checker types, not special cases. Checker split
   (`habu-split-checker-f-837bc1a4`) rides its churn window.
1. **Extent-typed accessors** `TENSOR:/ITENSOR:/EXTENT:`
   (`habu-extent-typed-tensor-bde435dc`) — candidate B; needs A1.
2. **`SPEC:` word** (`habu-spec-word-generating-0729fbea`) — candidate C,
   generating B; the default golden surface. Needs 1.

Model work (this inventory's sub-dots), sequenced against that chain:

3. **Numeric goldens that need no SPEC** — prototype now in the current idiom
   (epic: de-risk numerics first, rewrite as SPEC: before composition):
   affine LayerNorm, causal mask, cross-entropy (logits+int), positional
   embedding, Gaussian-RNG weight init, char tokenizer + loader.
4. **Training scaffolding** (independent of SPEC): AdamW decoupled decay,
   cosine LR + warmup, global-norm clip, training-state checkpoint.
5. **Batch/sequence (B,T,C) design** (`habu-batch-sequence-tensor-006f25a1`) —
   decide the convention before attention composition; blocks the block dots.
6. **Compositions (SPEC:-authored, hard-blocked on step 2):**
   multi-head self-attention sublayer → GPT-2 block + full model → weight
   tying.
7. **PROMOTE** the composed CPU goldens through the compute campaign to train
   on GB10 (epic north star; out of inventory scope).

## Minted sub-dots (all `-P habu-epic-nanogpt-in-b239aa8d`)

Wave A (independent goldens / scaffolding, startable pre-SPEC):
- `habu-affine-layernorm-gamma-d19a57e0` — Affine LayerNorm (γ/β) fwd+bwd+op
- `habu-cross-entropy-loss-93356943` — CE over logits+int targets + seed wiring
- `habu-learned-positional-embedding-43b259e2` — wpe + token+pos embed compose
- `habu-causal-attention-mask-1ced9cbd` — Causal mask (masked softmax) + adjoint
- `habu-adamw-decoupled-weight-d322fe1f` — AdamW decoupled weight decay
- `habu-cosine-lr-schedule-77c2d0f2` — Cosine LR + linear warmup
- `habu-global-norm-gradient-c164eb61` — Global-norm gradient clipping
- `habu-nanogpt-weight-init-b2fc5b4f` — nanoGPT init policy + Gaussian RNG
- `habu-dropout-op-train-fe0ad08d` — Dropout op + VJP (low priority; pretrain 0.0)
- `habu-tiny-shakespeare-char-125d9684` — Char tokenizer + text data loader (v0)
- `habu-training-state-checkpoint-3907d0d4` — Training-state checkpoint save/load
- `habu-batch-sequence-tensor-006f25a1` — Batch/sequence (B,T,C) design over 2D IR

Wave B (compositions, dependency-linked):
- `habu-multi-head-self-a1e0692f` — MHA sublayer composition (after `SPEC:`)
- `habu-gpt-2-block-a9039501` — GPT-2 block + full model (after MHA)
- `habu-weight-tying-wte-ab4145da` — Weight tying + grad accumulation (after model)

SPEC critical-path prerequisites (pre-existing, referenced not minted):
`habu-foundation-a1-declarable-98aebe7b`,
`habu-extent-typed-tensor-bde435dc`, `habu-spec-word-generating-0729fbea`,
`habu-split-checker-f-837bc1a4`.
