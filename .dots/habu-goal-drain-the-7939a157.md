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
