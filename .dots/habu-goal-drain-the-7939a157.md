---
title: "GOAL: drain the ENTIRE tracker to the gates (no epic carve-out)"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T13:33:26.102998+02:00\""
---

COMPLETION = a wave in which dot ready contains ONLY dots in the three exclusion classes AND no new ungated dot was minted, TWICE consecutively. EXCLUSION CLASSES (externally verifiable, re-checked every wave): (E1) device-gated - needs the Orin/zed (PTX M1/M5/M7/M9, m1d-cuda-driver, rca-culaunchkernel, make-ptx-device, committed-device-correctness, small-model-end, tensor-core-mma, re-express-tiled, automatic-op-fusion device legs, maki-tensor-array device verify, autograd-end-to device parity legs, maki-onnx-import-to-PTX device goldens, paper device rows; if the Orin returns these ENTER the queue); (E2) user-gated - needs a human act (live eval rounds BEYOND the already-approved subagent round; any credential/purchase); (E3) claimed by the live tfam agent (currently owner-seal-persist-1f23e205, tfam-2b-sealed-1b77662c; if released they ENTER the queue). EVERYTHING ELSE lands on master via claim->worker->review->window with full owning gates, including: typed-top campaign IN ORDER (p5 70dc94e4 FIRST - HIGH soundness; then hook 2b2e88aa, tracker 82cf8b84, xt 096a8f1b, compile-guard aad4acc7, snapshot daa8989a, tier-2 589c550f), unsafety sym-set endpoint 1c537c1f (recreated; original d12bc784 was never committed) (incl. defer/is probe), nominal-storage-raw a3430ef2 (unclaimed - claim it; coordinates the typed-storage value side), type-habu epic tail (fold-compile-keyword, single-pass-checking, unfreeze-checker-prop, multi-err-checking, self-check-checker, staged-fixpoint-src, builder-trust-rows, primitive-effect-axiom, ddc-cross-check, typed-depth-introspection, linear-once-resource, retire-0-set-check, checker-capability-ptr, typed-defining-words, typed-dictionary-record), habu-native-kernel host workflow, eval-real-gen's approved live subagent round (v1.1 tokens landed for exactly this), V2 design dots NOT claimed by tfam (artifact-typestate 6ee556f8, finite-effects 18bb1b35 etc. - design deliverables count as landing for design-type dots), and every dot minted during execution. Work discovered = work queued - no next-campaign deferrals except into E1/E2/E3 with the gate condition recorded. Per-wave report: ready-set delta + landing-commit table. Goal closes LAST with the full table.

## Session note 2026-07-13 (paused by user: "merge finished work, then stop")
User narrowed scope mid-session from full drain to "merge current agents' work
then stop; do not launch new agents." A parallel wave of 7 workers + reviews ran;
the API session limit then killed the survivors mid-flight. Disposition:
- MERGED to master (reviewed green): staged-fixpoint 0b5fc6e6 (+subsumed
  self-check-checker e10ce327), typed-async-DAG ae7c9bd5, unsafety-sym-set
  1c537c1f, top-row-hook 2b2e88aa. Design R7 artifact-typestate 6ee556f8 landed
  earlier with 6 implementation sub-dots minted.
- PARKED, claims released, NOT merged (see each dot's Parked note): p5 fitting-
  arity 70dc94e4 (reproducer only, HIGH), eval-real-gen dab1b6cd (BLOCK), kernel-
  bench 548b0d4c (BLOCK), typestate-stage a0eb43a2 (not started).
- Follow-ups discovered this session are listed in the session scratch
  pending-dots.md (suite-membership single-source, hunk-aware diff lints, perf
  waiver ratchet, eval device goldens + judge-image tripwire, misplaced epic
  self-file for 9549fdd8). Minted this session: execute-of-stored-xt soundness
  gap 5923c543 (HIGH, from the symset review).
Goal remains active for a future session to resume the drain.

DRAIN PLAN v2 (distilled 2026-07-17 after user review; the loop was
fanning out - 369 open vs 343 at day start despite ~25 closures):
RULE 1 - queued != dispatched. "Work discovered = work queued" still
mints the dot, but dispatch priority is PRE-EXISTING tracker dots; a
fresh residual is dispatched only when it BLOCKS a pre-existing chain.
Residuals that neither block nor shrink the tracker get folded into an
existing dot instead of minted where possible.
RULE 2 - lane budget 2 concurrent workers max (3+ caused LESSONS merge
churn and the master-divergence race); checker lanes exclusive.
RULE 3 - convergence check at every landing: net open count must fall;
a landing that would mint more than it closes folds its residuals.
PRIORITY ORDER:
(a) migration mass first - maki-migrate(8) libs-migrate(5)
    tests-migrate(4) delete-legacy(3) switchover-wave(3): mechanical
    host-glue retirement, burns count fastest;
(b) type-system program - v2-r3(7) v2-types(7) type-dsl(6) +
    checker-capability(3), serialized on the checker lane;
(c) V2 persistence/autonomous remainder - evidence-applicability next
    after proof-obligation lands, then differential/machine-action/
    capability chain;
(d) device dots queue against the Spark (being configured by another
    agent - coordinate via habu-v2-dgx-spark-e88559f6); zed reserved
    for deployment-facing legs;
(e) E2 user-gated parked for Joel: fp16 policy, CAP vocabulary,
    onnxruntime reference, Triton-baseline decision.
Epics close last as containers; this goal closes last with the landing
table. In-flight check against this standard: oblig = (c), keep;
leg2c = elaboration with a built-in net<=-1 STOP - honor its outcome,
no successor legs either way.

WIND-DOWN 2026-07-18 (maki orchestrator session, per Joel): this lane STOPS
after the nomstore landing; all habu work consolidates under ONE Claude Code
(the type-families successor line). Handoff state:
- Master green at the nomstore landing (this commit); fixpoint 2cb3b0cc
  (8MB region / 32k dict / narrow LPROT engine); full battery green.
- No live claims from this session; all .jj-ws/fable-* workspaces retired.
- Closed today (9): habu-v2-checked-async-8d460576,
  habu-lprot-narrow-protection-03cc8d7f, habu-v2-differential-runner-13359019,
  habu-switchover-wave-b-08482d5b (program complete, 5 batches),
  habu-v2-types-finite design phase (umbrella stays open),
  habu-v2-competitive-evidence-5d07d471, habu-persist-typed-cevid-6f08452c,
  habu-v2-differential-suite-2d896ced (stale-active reconciled),
  habu-checker-capability-layout-9b8540bd (5 slices; slice 6 = optional p3
  habu-layout-slice-6-4bb1e4fb), habu-nominal-storage-migrate-47ee0f93.
- Next-up queue per DRAIN PLAN v2 (recorded above): seal-owners chain
  (habu-seal-owners-syntax-63051652 gates the first R8 leaf
  habu-seal-cad-effect-49cac404); wave E trust discharges (owns the raw
  SPLIT-NEXT kernel note); Foundation A1 + TMA campaigns (successor's own
  line); device dots queue on Spark (habu-v2-dgx-spark-e88559f6, being
  configured by another agent); USER-GATED parked: fp16/bf16
  (habu-user-gated-fp16-58c1b84d), CAP vocabulary
  (habu-user-gated-cap-edccf572), onnxruntime reference, Triton-baseline.
- Standing gates and rules: DRAIN PLAN v2 rules above; claim protocol,
  battery composition, and the day's incident lessons live in LESSONS.md and
  docs/. Two master-red incidents today came from cross-agent landings
  (stale-status archive exemption; dangling dedupe blocker edge) - both
  fixed forward here; consolidation removes that class.
The goal dot stays ACTIVE for the consolidated instance; it closes LAST with
the landing table per the original directive.

## Takeover 2026-07-18 (Fable, dgx-spark session)
Per Joel: maki agent wound down; this session (and its successors on spark) now owns ALL Habu planning. Standing state: Drain Plan v2 rules ADOPTED (lane budget 2 confirmed by today's own experience — 4 parallel waves caused LESSONS merge churn + master divergence; convergence rule net<=-1 per landing; pre-existing dots dispatch first). Spark is NOW a configured Habu host (bin/hb at linux baseline 147648, full gate correctness-green; perf-verdict inadmissible pending GB10 cal fix in flight) — E1 device-gated dots whose legs run on GB10/sm_121a ENTER the queue once the cal/profile fix lands; zed stays reserved for deployment legs. In flight from this session: infra (cal root-cause + dgx-spark profile + DDC schedule + ABI pin), lib-prune (json-read, task.f), size-attribution, maki-dirs — merging serially onto master as they green. Campaign folds into priority (b): fields lane -> Foundation A1 (habu-foundation-a1-declarable-98aebe7b) -> checker split + generic registry in its window -> TENSOR: -> SPEC: (default surface) -> TMA/GB10 (docs/tma-gather.md). Size campaign (docs/size-campaign.md) runs on the engine lane. E2 user-gated queue for Joel now FIVE: fp16 policy, CAP vocabulary, onnxruntime reference, Triton-baseline decision, render/report Zig-analyzer alive-or-dead.

## Mission pivot 2026-07-18 (Joel): NORTH STAR = two flagship projects
(1) Triton re-implementation from first principles (the docs/ptx.md thesis + docs/compute-campaign.md + docs/tma-gather.md, now targeting GB10/sm_121a on spark alongside Orin) and (2) Karpathy's nanoGPT rebuilt in Habu end-to-end (CPU goldens -> PROMOTE -> device training on GB10). ALL other work waits unless it is a critical fix, blocks the north star, or Joel flags it important. RESTRICTIONS REMOVED per Joel: the net-open-count convergence rule and pre-existing-first dispatch order no longer bind — critical fixes and new important work may be minted and dispatched mid-stream. (Worker-count cap REMOVED by Joel 2026-07-18 — time is of the essence: launch as many workers as necessary. Merge safety comes from OWNERSHIP PARTITIONING instead: every worker owns a declared disjoint file set; same-file work serializes into one lane regardless of worker supply; the orchestrator integrates serially with the full gate as referee.) E2 decisions now mission-relevant: Triton-baseline (benchmark referee) and fp16 policy (nanoGPT training).

## Sequencing correction 2026-07-18 (Joel): SPEC: is LOAD-BEARING
The candidate-C SPEC: surface (docs/golden-syntax.md, gathered-GEMM/#10927 example) is load-bearing for both flagships, not a nicety: the demo is nanoGPT authored as SPEC: lines, and SPEC: doubles as the planner's dataflow input for the Triton reimpl. Therefore Lane C = Foundation A1 (habu-foundation-a1-declarable-98aebe7b) -> TENSOR: (habu-extent-typed-tensor-bde435dc) -> SPEC: (habu-spec-word-generating-0729fbea) is CRITICAL PATH, dispatched on the checker lane FIRST (all claims released at maki-agent wind-down; fields hardening lane follows unless critical). The nanoGPT inventory dot must additionally extract the SPEC grammar requirements from GPT-2's op set (contractions, gather indexing, reductions; non-linear ops remain words) so A1/SPEC: scope is driven by the real model, not speculation.

## Phase 0 mandate 2026-07-18 (Joel): type-family code lands FIRST
Flagship lanes are AUTHORED in type-family-enabled code, so Phase 0 (checker lane, serialized) precedes all flagship authoring: (1) unified type-DSL cutover waves (census-type-dsl-cutover.md), (2) fields hardening lane (habu-fields-add-shared children; protect -> rollback-canonical -> visibility/provenance -> factor -> harden -> validators), (3) Foundation A1 -> TENSOR: -> SPEC:. Lane A/B design-only artifacts (process-row numbers, nanoGPT inventory/gap list, SPEC grammar extraction) may proceed in parallel — no new flagship CODE in old idiom.
## Codegen verdict protocol (Joel: "our code generator absolutely sucks — prove me wrong")
Measured, not argued, on GB10: (a) mem-bound kernels vs measured DRAM ceiling; (b) fp32 blocked GEMM vs CUDA-core fp32 roofline; (c) cuobjdump SASS audit of ptxas output on our PTX (reg pressure, scheduling, what ptxas fixed for us). Known-right: zero native opt passes (only bootstrap/cg/opt.fs 240-line gforth peephole); per-family hand-shaped emitters cap SPEC: generality — habu-ptx-opt-layer-325b9507 is the named fix. Known-mitigating: DRAM parity on Orin mem-bound; the 4.3x GEMM gap is TF32-tensor-core vs our fp32-CUDA-core (MMA unlanded), not proven codegen waste. If kernels sit near roof for their instruction mix: adequate, MMA/TMA are the gap. If far under: Joel is right, IR/opt layer joins Phase 0 on the critical path.
