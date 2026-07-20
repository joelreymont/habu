# nanoGPT-in-Habu inventory

Inventory of maki's model stack against Karpathy nanoGPT (GPT-2-small class).
Epic `habu-epic-nanogpt-in-b239aa8d`; this refresh is
`habu-refresh-nanogpt-inventory-40906c61`. Read-only survey — every capability
claim carries `file:line` evidence in the real maki sources, and every owner
link is checked against live dot status by `tools/nanogpt-inventory-lint.f`
(see the owner manifest at the end). Last rebuilt from code + dot status
2026-07-20.

## Why six dimensions, not one label

A single present/absent label hid incomplete work behind old gaps: the model
math has largely landed as **host CPU goldens with autograd**, but almost none
of it is **batched (B>1)**, **device-lowered to the GB10**, or **measured** —
and several closed feature dots were narrowed by destruction review to a much
smaller milestone than their close reason first claimed. Each row is therefore
graded on six independent dimensions, and every incomplete one links to its
exact live owner:

- **P — prototype golden.** A checked host reference word over float buffers
  (`T-GET`/`T-SET`, `maki/array.f`) with a finite-difference gradcheck.
- **H — host production.** Reachable in the trainable substrate: a `MODEL:` DSL
  op (`maki/cad.f` → `maki/model-ir.f`) or a tensor-loss/trainer surface, not
  just a standalone golden.
- **A — trainable AD.** VJP/adjoint wired and finite-difference gradchecked
  through `maki/backward.f`.
- **B — batched semantics.** Correct for B>1 under the landed 2D `B*T`-row
  convention (`docs/batch-sequence-design.md`), with per-sequence isolation.
- **D — device lowering.** Lowered to GB10 PTX (`maki/lower/*.f` or
  `tools/ptx/*-cg.f`), or explicitly fail-closed.
- **M — measured performance.** A device timing / roofline row
  (`tools/ptx/perf-rows.tsv`).

Legend: **y** complete · **~** partial · **–** absent · **n** not applicable.

## Substrate: how maki expresses a model today

- **Two authoring layers.** (1) *Buffer goldens*: checked words over contiguous
  float cell buffers (`maki/array.f`), numerically gradcheckable. (2) The
  `MODEL:` DSL (`maki/cad.f`) capturing a dataflow into the shared model IR
  (`maki/model-ir.f`), differentiated by `maki/backward.f`, run by
  `maki/executor.f`.
- **The SPEC: einsum chain landed.** Internal contractions the single-running-
  value `MODEL:` DSL cannot root (Q@Kᵀ) are now expressible as declared einsums
  whose transposed, batched adjoints derive at declaration
  (`maki/attn-eq.f` `A-SCORES`/`A-CTX`, `maki/spec.f`). The `MODEL:` DSL's own
  single-running-value / four-pending-reference limits (`maki/cad.f`,
  `CAP-PEND-CAP=4`) remain real; whether to retire `MODEL:` once SPEC: carries
  attention is `habu-decide-model-retirement-bd76a741`.
- **Batch convention decided.** 2D `B*T` rows, B-outermost, extent roles
  `#B`/`#T`/`#H`, with a block-diagonal segment/causal attention op and an
  interim host batch loop (`docs/batch-sequence-design.md`,
  `habu-batch-sequence-tensor-006f25a1`). No 3rd tensor dimension exists; B>1
  correctness is still being pushed through the composition
  (`habu-gb10-batched-attention-3055d565`, `habu-give-the-block-3bdedf46`).
- **Loss is a composition, not an IR node.** Trainers run the forward slice and
  write seed cotangents into `BW-SEED-SLOT`; cross-entropy is composed from
  `maki/loss-tensor.f` `TT-XENT`/`TT-XENT-SEED`, not a fused op.

## Inventory

`P·H·A·B·D·M` = prototype golden · host production · trainable AD · batched ·
device · measured.

| nanoGPT piece | P·H·A·B·D·M | Evidence (owner module) | Live owners of incomplete dimensions |
|---|---|---|---|
| Token embedding (gather) | y·y·y·y·~·n | `maki/embedding.f:36` `EMB-GATHER`, `:54` `EMB-SCATTER-ADD`; `op-kind.f:40` GATHER, adjoint `:57` | device/host id bounds `habu-validate-embedding-access-2cf51e2c`; packaging `habu-own-embedding-pkg-9e22b2b0` |
| Learned positional embedding (wpe) | y·~·y·–·–·n | `maki/embedding.f:71` `WPE-SLICE`, `:76` pad-scatter adj, `:80` `TOKPOS-EMBED`; `MODEL:` GATHER ADD | B>1 shared MaxT×C table `habu-complete-batched-pos-99332bf6`; duplicate primitives `habu-remove-positional-buf-ceaf46d0`; packaging `habu-own-embedding-pkg-9e22b2b0` |
| Affine LayerNorm | y·y·y·y·~·~ | `maki/layernorm.f:73` `LN-AFFINE-FWD`, `:84` `LN-AFFINE-BWD`; `OP-LAYERNORM` payload form (plain\|affine) | device backward + execution bounded by the pre-existing `E-PTXTC-ARCH` harness gap (no dedicated owner — see loose ends); perf `habu-bind-performance-evidence-e454f629` |
| LayerNorm (no-affine) | y·y·y·y·~·n | `maki/layernorm.f:35` `LN-FWD`, `:57` `LN-BWD`; `op-kind.f:26` `OP-LAYERNORM` | GPT-2 final + block LN use the affine form above |
| RMSNorm (in-tree, not GPT-2-small) | y·y·y·y·y·~ | `maki/rmsnorm.f:29` `RMS-FWD`, `:46` `RMS-BWD`; `op-kind.f:27,24`; device `tools/ptx/rmsnorm-cg.f` | perf `habu-bind-performance-evidence-e454f629` |
| RoPE (in-tree; GPT-2 uses learned wpe) | y·y·y·y·y·~ | `maki/rope.f:15` `ROPE-PAIR`, `:19` `ROPE-BWD`; `op-kind.f:34,26`; device `tools/ptx/rope-cg.f` | perf `habu-bind-performance-evidence-e454f629` |
| Softmax (row) | y·y·y·y·~·n | `maki/softmax.f` `SM-FWD`/`SM-BWD`; `op-kind.f:28` `OP-SOFTMAX-ROW`, adjoint `:25` | device row-reduce path partial (`maki/lower/red.f`) |
| Causal mask (masked softmax) | y·y·y·y·–·n | `maki/causal.f:44` `SM-FWD-CAUSAL`, `:51` `SM-BWD-CAUSAL`, `:58` `CAUSAL-SOFTMAX-ROWS` (`habu-causal-attention-mask-1ced9cbd`) | device `habu-gb10-batched-attention-3055d565` |
| Segment/causal attention op | y·y·y·y·–·– | `maki/segment.f:74` `SEG-ATTN-FWD`, `:103` `SEG-ATTN-BWD`; `op-kind.f:32` `OP-SEG-ATTN`, adjoint `:33` (`habu-segment-causal-attention-5fbe00e1`) | device + perf `habu-gb10-batched-attention-3055d565` |
| Multi-head self-attention sublayer | y·~·y·y·–·– | `maki/mha.f` fused-QKV `MHA-FWD`/`MHA-BWD` over `maki/attn-eq.f` batched einsums (header boundary notes) | alias-safety/packaging `habu-own-multi-head-c863298a`; device `habu-gb10-batched-attention-3055d565`; block compose `habu-compose-batched-mha-d3166a09` |
| MLP with GELU | y·y·y·y·y·~ | `maki/mlp.f:16` `MLP-FWD`; `maki/gelu.f` `GELU-F`/`GELU-BWD`; device `maki/lower/model-mlp-device-test.f` | perf `habu-bind-performance-evidence-e454f629` (GELU is tanh-approx) |
| Linear / MATMUL / LM head | y·y·y·y·y·y | `maki/linear.f:24` `LINEAR`; `maki/matmul.f:22` `MATMUL` + transposed adjoints; device `maki/lower/mm.f`, `tools/ptx/perf-rows.tsv` MM-* | complete (device timing is orin-nx roofline; GB10 is the campaign target) |
| Residual connections | y·y·y·y·y·n | `op-kind.f:31` `OP-RESIDUAL-ADD` (copy adjoint); skip via `>V`/named ref | complete |
| Dropout | y·y·y·y·n·n | `maki/dropout.f` inverted mask+scale, reseeded backward; `op-kind.f:36` `OP-DROPOUT`, `:37` bwd | pretrain default 0.0; `habu-dropout-op-train-fe0ad08d` (closed) |
| Cross-entropy loss (logits+int) | y·y·~·y·–·– | `maki/loss-tensor.f:80` `TT-XENT`, `:90` `TT-XENT-SEED`; one-hot golden `maki/celoss.f:16` | trainer state `habu-own-cross-entropy-c78644e3`; double-forward `habu-fuse-cross-entropy-9e625f93` |
| Weight tying (wte↔lm_head) | y·y·y·y·–·n | tied inside the block composition (`habu-compose-tied-wte-f276dc6a`, consuming `habu-weight-tying-wte-ab4145da`'s mirror + summed-grad machinery): one stored (V,C) parameter, head reads its transpose mirror asserted bit-identical each step, tied grad = both contributions, gradchecked, 12-step training locked (`gptblock-attn-test.f` (F)) | — |
| GPT-2 block / full-model composition | y·~·y·–·–·– | `maki/examples/nanogpt/gptblock-attn-test.f` `GBLK` `MODEL:` + `GBR-FWD` internal golden (B=1, single-head via `maki/attn-eq.f`, host oracle); tied wte/LM-head with external torch f64 golden grounding the tied trace (`habu-external-deterministic-golden-b7693e44`, closed) | real owner `habu-gpt-2-composition-a90e901e`; B extent `habu-give-the-block-3bdedf46`; batched MHA `habu-compose-batched-mha-d3166a09`; device `habu-block-device-lowering-9f9270bb`; Nx `habu-train-n-block-25fb2316` |
| Batch / sequence (B,T,C) | y·y·y·~·–·– | design `docs/batch-sequence-design.md`; `habu-get-batch-loader-542f6f22`, `habu-host-batch-loop-66773b33`, `habu-extent-roles-b-df9d232f`, `habu-extent-role-product-8e364885` (all closed) | device batched attention `habu-gb10-batched-attention-3055d565`; block B extent `habu-give-the-block-3bdedf46` |
| Adam / AdamW | y·y·y·y·n·n | `maki/optim-tensor.f:33` `TT-ADAM!`, `:42` `TT-ADAMW!` decoupled decay (`habu-adamw-decoupled-weight-d322fe1f`) | optimizer-state packaging `habu-own-adam-optimizer-e542f1c8` |
| Cosine LR + warmup | y·y·n·n·n·n | `LR-SCHED` (`maki/examples/nanogpt/adam-train.f`), degree-12 Maclaurin cosine, max err 6.32e-9 (`habu-cosine-lr-schedule-77c2d0f2`) | complete (host scheduling scaffold) |
| Global-norm gradient clip | y·y·n·n·n·n | `GRAD-CLIP-COEF` before optimizer apply, `AMT-CLIP!` arming (`habu-global-norm-gradient-c164eb61`) | complete (Adam-MLP trainer by design) |
| Weight init (Gaussian) | y·~·n·n·n·n | `SC-GAUSS` polar Marsaglia over the LCG + `INIT-FILL` role policy (`habu-nanogpt-weight-init-b2fc5b4f`) | wiring into block init is `habu-gpt-2-composition-a90e901e`; LCG dedup `habu-factor-maki-random-f3dce839` |
| Training-state checkpoint (resume) | y·y·n·n·n·n | `maki/train-state.f` FNV-1a-64 codec, params+moments+step, resume bit-identical (`habu-training-state-checkpoint-3907d0d4`); atomicity `habu-make-store-replay-7cd1f6d7` | complete |
| Gradient checkpointing (remat) | y·y·y·y·–·n | `maki/checkpoint.f` `CK-FWD`/`CK-BWD` bit-identical remat | complete (distinct from resume checkpointing) |
| Char tokenizer + data loader | y·y·n·y·n·n | `maki/examples/nanogpt/tokenizer.f` `TOK-BUILD`; `maki/examples/nanogpt/data-loader.f` `LOAD-CORPUS`; get_batch reuses `maki/examples/nanogpt/batch-loader.f` `BL-LOAD` (`habu-tiny-shakespeare-char-125d9684`, bounds `habu-bound-tokenizer-api-111a9a88`, corpus `habu-make-corpus-load-d6ce6c05`) | state/packaging `habu-own-tokenizer-state-d5db1943` |
| Byte-level BPE tokenizer | y·y·n·y·n·n | `maki/examples/nanogpt/bpe.f` byte-level BPE; real-vocab/tiktoken parity `habu-bpe-real-vocab-c973932a` (closed) | full 50k merges `habu-bpe-full-50k-a598ba57`; unicode pretokenization `habu-bpe-unicode-pre-e6e7f34f` |
| Autoregressive generation / sampling | y·y·n·n·n·n | `maki/examples/nanogpt/generate.f` `GEN-ARGMAX`/`GEN-TEMP!`/`GEN-TOPK!`/`GEN-SAMPLE`/`GEN-NEXT` (landed, archived) | complete (inference path) |

## Closed prototype claims narrowed by destruction review (dated notes)

Each dot below is **closed**, but its close reason was narrowed to a smaller
milestone than first stated; the remaining work has a live owner. These rows
exist so a closed feature dot is never read as full completion.

- **2026-07-19 Affine LayerNorm.** `habu-affine-layernorm-gamma-d19a57e0`
  closed "fully landed," but review found unary/affine shared one opkind with
  input-count inference (0/2/4-input states representable) and a backward
  `MUL` where the 1×C γ broadcast needs `BCAST-MUL`. Corrected under
  `habu-make-affine-layernorm-ddb6d70d` (explicit `lnform` payload,
  `BCAST-MUL`, forward device kernel) — itself now closed; device **execution**
  still bounded by the pre-existing `E-PTXTC-ARCH` harness gap.
- **2026-07-19 MHA sublayer.** `habu-multi-head-self-a1e0692f`'s "merged"
  close reason was a fixed-shape **forward-only oracle**. Trainable semantics
  (adjoints, per-(b,h) batched einsums, fused QKV) landed under
  `habu-complete-trainable-multi-39e26b3d` and `habu-fuse-multi-head-83294c30`;
  package/alias-safety remains `habu-own-multi-head-c863298a`.
- **2026-07-19 Positional embedding.** `habu-learned-positional-embedding-43b259e2`
  proved only **B=1** buffer behaviour; the GPT-2 shared MaxT×C table with
  gradient accumulation across batches is `habu-complete-batched-pos-99332bf6`.
- **2026-07-19 Cross-entropy.** `habu-cross-entropy-loss-93356943` landed the
  stable logsumexp CE + seed, but the trainer test's "end-to-end gradient"
  differentiates a different objective; the real composed weight/bias gradient
  proof and target validation landed via `habu-validate-cross-entropy-4b176d46` (closed, 0c0afa1a).
- **2026-07-20 GPT-2 block.** `habu-gpt-2-block-a9039501` trains a **B=1,
  untied, single-head** host oracle and dies at the MIR node cap past ~1 block.
  The real GPT-2-small composition (B>1, tied wte, batched MHA, Nx, device,
  external golden) is `habu-gpt-2-composition-a90e901e` and its sub-dots.

## Historical decisions (dated notes)

- **2026-07-19 SPEC: chain landed.** The einsum surface that expresses internal
  contractions and derives batched transposed adjoints is in place; attention
  scores/context are authored as SPEC: einsums (`maki/attn-eq.f`). The
  `MODEL:` retirement decision is deferred to
  `habu-decide-model-retirement-bd76a741`.
- **2026-07-18 Batch design (Option D).** 2D `B*T` rows won over a 3rd tensor
  dimension; the block-diagonal segment/causal op avoids materializing a
  `(B*T)×(B*T)` score. Recorded in `docs/batch-sequence-design.md`.

## Loose ends (no dedicated live owner yet — proposed, not minted)

- **Affine/plain LayerNorm device backward + execution.** `LN-BWD`/`ROWSUM-BWD`
  are not device-lowered and forward execution is blocked by the pre-existing
  `E-PTXTC-ARCH` harness gap. No open dot owns this specifically; propose a
  device-LN-completion dot rather than folding it silently into
  `habu-bind-performance-evidence-e454f629`.
- **SGD / attention trainers under LR schedule + grad clip.** The cosine LR and
  global-norm clip are wired into the Adam-MLP trainer only; extending them to
  the SGD and attention trainers is unowned.

## Owner manifest (checked by tools/nanogpt-inventory-lint.f)

Machine-readable owner list. `open`/`closed` name a dot whose live status must
match (archive or frontmatter `status:`); `module` names an owner source file
that must exist. The lint reds on an unknown id/path, a duplicate, a
status-mismatch (a closed dot cited as a live owner, or the reverse), a prose
`habu-…` id missing from this list, or a manifest row never referenced above.

```owners
open   habu-epic-nanogpt-in-b239aa8d
closed habu-refresh-nanogpt-inventory-40906c61
open   habu-gpt-2-composition-a90e901e
closed habu-compose-tied-wte-f276dc6a
open   habu-give-the-block-3bdedf46
open   habu-compose-batched-mha-d3166a09
closed habu-external-deterministic-golden-b7693e44
open   habu-block-device-lowering-9f9270bb
open   habu-train-n-block-25fb2316
open   habu-gb10-batched-attention-3055d565
open   habu-complete-batched-pos-99332bf6
open   habu-own-multi-head-c863298a
open   habu-own-cross-entropy-c78644e3
open   habu-own-adam-optimizer-e542f1c8
open   habu-own-embedding-pkg-9e22b2b0
open   habu-own-tokenizer-state-d5db1943
closed habu-validate-cross-entropy-4b176d46
open   habu-validate-embedding-access-2cf51e2c
open   habu-factor-maki-random-f3dce839
open   habu-remove-positional-buf-ceaf46d0
open   habu-fuse-cross-entropy-9e625f93
open   habu-bind-performance-evidence-e454f629
open   habu-decide-model-retirement-bd76a741
open   habu-bpe-full-50k-a598ba57
open   habu-bpe-unicode-pre-e6e7f34f
closed habu-affine-layernorm-gamma-d19a57e0
closed habu-make-affine-layernorm-ddb6d70d
closed habu-learned-positional-embedding-43b259e2
closed habu-multi-head-self-a1e0692f
closed habu-complete-trainable-multi-39e26b3d
closed habu-fuse-multi-head-83294c30
closed habu-causal-attention-mask-1ced9cbd
closed habu-segment-causal-attention-5fbe00e1
closed habu-cross-entropy-loss-93356943
closed habu-weight-tying-wte-ab4145da
closed habu-batch-sequence-tensor-006f25a1
closed habu-nanogpt-weight-init-b2fc5b4f
closed habu-adamw-decoupled-weight-d322fe1f
closed habu-cosine-lr-schedule-77c2d0f2
closed habu-global-norm-gradient-c164eb61
closed habu-training-state-checkpoint-3907d0d4
closed habu-tiny-shakespeare-char-125d9684
closed habu-dropout-op-train-fe0ad08d
closed habu-gpt-2-block-a9039501
closed habu-bpe-real-vocab-c973932a
closed habu-make-store-replay-7cd1f6d7
closed habu-bound-tokenizer-api-111a9a88
closed habu-make-corpus-load-d6ce6c05
closed habu-get-batch-loader-542f6f22
closed habu-host-batch-loop-66773b33
closed habu-extent-roles-b-df9d232f
closed habu-extent-role-product-8e364885
module maki/embedding.f
module maki/layernorm.f
module maki/rmsnorm.f
module maki/rope.f
module maki/softmax.f
module maki/causal.f
module maki/segment.f
module maki/attn-eq.f
module maki/mha.f
module maki/mlp.f
module maki/gelu.f
module maki/linear.f
module maki/matmul.f
module maki/celoss.f
module maki/loss-tensor.f
module maki/dropout.f
module maki/optim-tensor.f
module maki/train-state.f
module maki/checkpoint.f
module maki/examples/nanogpt/tokenizer.f
module maki/examples/nanogpt/data-loader.f
module maki/examples/nanogpt/batch-loader.f
module maki/examples/nanogpt/bpe.f
module maki/examples/nanogpt/generate.f
module maki/examples/nanogpt/gptblock-attn-test.f
```
