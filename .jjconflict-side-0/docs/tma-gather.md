# Gathered GEMM and TMA: movement-plan design

What maki and the PTX backend are missing to express an indexed (gathered)
GEMM and lower it through Blackwell's TMA engine — with the stride-legality
class of `docs/case-tma-stride.md` made unrepresentable. Extends the §8.1
compute-bound campaign (docs/archive/cad-plan.md) from Orin/sm_87 to a second process
target; nothing here disturbs the sm_87 sequencing.

The one-sentence position: **gather is a movement plan, not syntax.** The
model layer states the dataflow (`O[m,:] = Σk A[ix[m],k]·B[k,:]`); which
engine moves the bytes — predicated per-lane loads, pointer-increment tables,
`cp.async`, or TMA — is a planner decision under design rules, exactly like
routing under DRC in the EDA analogy.

## What already exists (build on, do not duplicate)

- `idxctx` / `uniqidxctx` (`docs/ptx-sketch.md`): the indexed-memory context —
  gather as a *typed context* over an index span I and data span D, with the
  uniqueness witness split out. This is the frontend of the whole design; it
  needs no new syntax.
- `align-16` marker atoms in the sig grammar; spans carry base + extent.
  Alignment evidence is already a type-level fact.
- `cp.async.cg` double-buffered staging (§8.1 step 2B, landed) and the
  schedule family's `stages` parameter — the pipelining shell TMA slots into.
- Non-affine indexing via pointer-increment lookup tables is already named as
  our gather/scatter pattern (`docs/triton.md` §7): that is the sm_87 gather
  lowering and stays the portable baseline.

## Missing piece 1 — gather as a first-class movement op in the planner

A `MOVE` node in the memory plan: (dst tile, src span or idxctx, staging
choice). Today the emitters hard-wire the movement per kernel; the planner
must own the choice per (target, dtype, shape, layout) key:

1. Predicated per-lane loads (any arch) — exists.
2. Pointer-increment lookup table (any arch; the docs/triton.md §7 pattern) —
   design exists, emitter does not.
3. `cp.async` staged (sm_80+) — exists for dense; not wired to `idxctx`.
4. TMA dense (`cp.async.bulk.tensor`, sm_90+) and TMA gather4
   (`tensormap` gather, sm_100+ family incl. consumer Blackwell) — new.

The plan records which lowering was chosen and why (roofline + legality), and
the §7.4 content-keyed store caches it with the tuning evidence.

## Missing piece 2 — TMA emitter surface (new PTX, arch-gated)

New emitter words, gated like every arch feature (sm_87 has none of this):

- Host-side descriptor build for statically-known geometry (the common case:
  strides are plan-time facts), passed via `.param` — mirrors
  `cuTensorMapEncodeTiled`.
- Device-side descriptor patching (`tensormap.replace.tile.*`) only where
  geometry is genuinely runtime — and then the legality facts below must be
  runtime-witnessed (an audited TRUST row or an emitted check), never assumed.
- `cp.async.bulk.tensor.{1,2}d.shared::cluster.global` loads + the mbarrier
  completion protocol; gather4 variant for `idxctx`.
- SMEM staging: TMA writes land in SMEM boxes; the existing `stages`
  double/triple buffering shell carries over, with mbarrier arrive/expect-tx
  replacing the `cp.async.commit/wait` group protocol.

The mbarrier protocol is a collective protocol in the checker's sense and gets
the same treatment as the existing collective lowering: legal orderings
enforced, divergence rejected.

## Missing piece 3 — TMA legality as a design-rule family (the case payoff)

Checker rules keyed to the *chosen lowering*, not global:

- TMA lowering demands: base `align-16`, every global stride ≡ 0 mod 16 B,
  box dims ≤ 256, dtype in the engine's supported set, SMEM box fits the
  target's per-stage budget.
- Evidence sources, in order of preference: (a) plan-time arithmetic — the
  planner *derives* stride alignment from dtype × padded extent (it owns
  allocation, so it can simply make the fact true by padding); (b) declared
  span marks checked at PROMOTE; (c) for runtime geometry, a witness or
  refusal. No silent path: an `idxctx` whose facts don't support TMA lowers
  through 1–3 instead, and the plan says so.
- The `docs/case-tma-stride.md` golden (gathered GEMM, bf16, K=511) becomes a
  permanent regression: exact under every lowering the planner may choose,
  plus a negative fixture proving the checker refuses a hand-forced TMA plan
  on a misaligned span.

## Missing piece 4 — GB10 process target (sm_121a)

Second real target row next to sm_87 (arch gates already anticipate this):

- `sm_121a`: 48 SMs, 99 KB SMEM/SM (101376 B opt-in), unified memory,
  bf16/fp16/fp8 yes, TMA yes (incl. gather4), tcgen05 **no** — block-scaled
  MMA lowers to upcast + HMMA, accumulator in SMEM. The smem-budget numbers
  are load-bearing: the triton_kernels #8182 failure came from assuming
  datacenter (228 KB) geometry on this part; our process row must carry the
  measured budget so schedule enumeration never proposes an infeasible stage.
- Toolchain: system CUDA 13 `ptxas` on the DGX Spark (`spark` host) knows
  `sm_121a` natively; bench harness (`tools/ptx/bench.f`) unchanged.

## Missing piece 5 — schedule-family fit for gathered/ragged GEMM

Two new plan choices, enumerable and tunable like `bm/bn/bk/warps/stages`:

- Ragged-K handling: `masked-tail` (predicated last tile) vs `pad-stride`
  (planner pads allocation to aligned stride; enables TMA, costs bytes).
  On unified-memory targets the byte cost is small and measurable.
- Gather locality: table-increment vs TMA-gather4 crossover is empirical —
  a tuner axis, decided by measurement under §7.4 keys, not by folklore.

## Dependency order

1. Process row for sm_121a + arch gates (small, unblocks everything).
2. `MOVE` plan node with lowerings 1–3 unified behind it (no TMA yet; pure
   refactor of existing emitters onto the plan node — sm_87 output must stay
   byte-identical, proven by the gate).
3. TMA legality rule family + negative fixtures (checker work; can land
   before the emitter — rules first, engine second is the CAD way).
4. TMA dense emitter + mbarrier protocol; then gather4 for `idxctx`.
5. Gathered-GEMM golden + K=511 case regression wired into GOLDEN.
6. Schedule-family axes (ragged-K, gather crossover) + tuner integration.

Steps 1–3 are useful on their own even if 4 slips: the legality family
already catches hand-written descriptor plans, and the plan node cleans up
existing movement code. The golden-authoring syntax question this raises —
how the model layer states dataflow like `A[ix[m],k]` pleasantly — is a
separate concern: `docs/golden-syntax.md`.
