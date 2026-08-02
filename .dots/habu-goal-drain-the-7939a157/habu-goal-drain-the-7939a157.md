---
title: "GOAL: drain the ENTIRE tracker to the gates (no epic carve-out)"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T13:33:26.102998+02:00\""
---

Completion is defined over the reconciled current tracker, not a frozen list
of historical task names. On each wave, resolve every live dot and dependency
from the current tree. Every open or active result must either land, pass its
owning gates, and close, or remain explicitly deferred behind a currently
verified device, user, or external-state condition with its re-entry condition
recorded. Closed, missing, superseded, and already-landed tasks are never part
of the must-land set.

The goal completes only after two consecutive reconciliations in which
`dot ready` contains no actionable work, no new ungated dot was minted, every
deferred survivor still satisfies its stated exclusion, and the tracker has no
dangling blocker or stale claim. Work discovered during execution is reconciled
into the current tracker rather than appended to an obsolete campaign queue.
The per-wave report records the ready-set delta, landed commit for each closed
result, and the current deferred set. This goal closes last.

## Mission pivot 2026-07-18 (Joel): NORTH STAR = two flagship projects
(1) Triton re-implementation from first principles (the docs/ptx.md thesis + docs/compute-campaign.md + docs/tma-gather.md, now targeting GB10/sm_121a on spark alongside Orin) and (2) Karpathy's nanoGPT rebuilt in Habu end-to-end (CPU goldens -> PROMOTE -> device training on GB10). ALL other work waits unless it is a critical fix, blocks the north star, or Joel flags it important. RESTRICTIONS REMOVED per Joel: the net-open-count convergence rule and pre-existing-first dispatch order no longer bind — critical fixes and new important work may be minted and dispatched mid-stream. (Worker-count cap REMOVED by Joel 2026-07-18 — time is of the essence: launch as many workers as necessary. Merge safety comes from OWNERSHIP PARTITIONING instead: every worker owns a declared disjoint file set; same-file work serializes into one lane regardless of worker supply; the orchestrator integrates serially with the full gate as referee.) E2 decisions now mission-relevant: Triton-baseline (benchmark referee) and fp16 policy (nanoGPT training).

## Sequencing correction 2026-07-18 (Joel): SPEC: is LOAD-BEARING
The candidate-C SPEC: surface (docs/golden-syntax.md, gathered-GEMM/#10927 example) is load-bearing for both flagships, not a nicety: the demo is nanoGPT authored as SPEC: lines, and SPEC: doubles as the planner's dataflow input for the Triton reimpl. Therefore Lane C = Foundation A1 (habu-foundation-a1-declarable-98aebe7b) -> TENSOR: (habu-extent-typed-tensor-bde435dc) -> SPEC: (habu-spec-word-generating-0729fbea) is CRITICAL PATH, dispatched on the checker lane FIRST (all claims released at maki-agent wind-down; fields hardening lane follows unless critical). The nanoGPT inventory dot must additionally extract the SPEC grammar requirements from GPT-2's op set (contractions, gather indexing, reductions; non-linear ops remain words) so A1/SPEC: scope is driven by the real model, not speculation.

## Phase 0 mandate 2026-07-18 (Joel): type-family code lands FIRST
Flagship lanes are AUTHORED in type-family-enabled code, so Phase 0 (checker lane, serialized) precedes all flagship authoring: (1) the unified type-DSL hard cutover specified by `docs/type-families.md` §2, (2) fields hardening lane (habu-fields-add-shared children; protect -> rollback-canonical -> visibility/provenance -> factor -> harden -> validators), (3) Foundation A1 -> TENSOR: -> SPEC:. Lane A/B design-only artifacts (process-row numbers, nanoGPT inventory/gap list, SPEC grammar extraction) may proceed in parallel — no new flagship CODE in old idiom.
## Codegen verdict protocol (Joel: "our code generator absolutely sucks — prove me wrong")
Measured, not argued, on GB10: (a) mem-bound kernels vs measured DRAM ceiling; (b) fp32 blocked GEMM vs CUDA-core fp32 roofline; (c) cuobjdump SASS audit of ptxas output on our PTX (reg pressure, scheduling, what ptxas fixed for us). Known-right: zero native opt passes (only bootstrap/cg/opt.fs 240-line gforth peephole); per-family hand-shaped emitters cap SPEC: generality — habu-ptx-opt-layer-325b9507 is the named fix. Known-mitigating: DRAM parity on Orin mem-bound; the 4.3x GEMM gap is TF32-tensor-core vs our fp32-CUDA-core (MMA unlanded), not proven codegen waste. If kernels sit near roof for their instruction mix: adequate, MMA/TMA are the gap. If far under: Joel is right, IR/opt layer joins Phase 0 on the critical path.
