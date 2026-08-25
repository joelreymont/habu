---
title: "Paper: Habu checked kernel target + Model CAD"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:31:55.700347+02:00"
---

User-approved direction 2026-07-04 (pending final go to write). In-repo paper at docs/paper/ (markdown first; LaTeX only if submitting). SCOPE NOW (earned claims only): (1) the checked concatenative kernel target - row-polymorphic stack effects type fused tile compositions; author-time error-class shift measured vs real Triton 3.5.1 on the same Orin (docs/eval-triton.md error battery + bandwidth parity at the streaming roof); (2) Model CAD - MODEL: capture -> typed-legality fusion with exact byte accounting -> generated per-region kernels (EW/reduction/matmul/movement) verified device-vs-host (LOWER-GOLDEN) under per-class tolerances -> gate-licensed promoted artifacts with evidence rows; (3) the from-scratch training flagship: the same IR gradchecks and trains (verified-gradient); (4) self-hosted <128KB checked-Forth engine, Moore/OKAD lineage (README/positioning framing). EXPLICITLY DEFERRED (placeholder section wired to CAD-PLAN 8.1 milestones): compute-bound performance vs Triton (GEMM baseline, MMA, cp.async, cad-6 tune, end-to-end model latency). Structure sketch: intro/positioning (vs Triton paper taxonomy - see docs/triton.md), language + checker, the CAD pipeline, verification methodology (gates as claims), measured results (only eval-triton.md numbers), limitations (honest: v1 caps, no perf claims), related work. Rules: every quantitative claim must name its running gate/test; NVIDIA model workloads citable, the private application project NEVER named; venue target workshop-register first (MAPL/ARRAY-class), full systems paper after 8.1 numbers land. Writing is orchestrator-led (like docs/triton.md), review by user.

DECISION 2026-07-04 (user): do NOT write yet. The paper waits until actual
performance results are in - blocked on the CAD-PLAN 8.1 sequence (on-device
PROFILE/roofline, measured GEMM-vs-Triton baseline, cp.async stages,
tensor-core MMA, cad-6-tune, end-to-end model latency). Revisit when those
numbers exist; scope above stands.

REQUIREMENT (user, 2026-07-04): the paper is incomplete without PROOF that
each invention actually does good - an effectiveness/ablation matrix, every
row a committed reproducible experiment, not narrative:
- Checker (typed effects): author-time error battery vs Triton (extend
  eval-triton battery: k candidates x task classes; bugs caught static vs
  runtime) + authoring pass@k (eval harness exists).
- Fusion planner + byte accounting: (a) predicted-vs-measured traffic (the
  byte model is falsifiable against measured GB/s - roofline denominator);
  (b) fusion ON/OFF latency ablation per model (same kernels, regions split).
- Device-vs-host GOLDEN: seeded-fault injection - wrong index / dropped mask
  / transposed operand / stale kernel - each class caught by the gate;
  uniform-vs-nonuniform golden sensitivity (sum-launch precedent).
- Sentinels: dropped-copy-back injection caught (vs silent pass without).
- Verified gradient: wrong-adjoint detection fixture (exists) + from-scratch
  convergence gate as the end-to-end proof.
- Persistent content-keyed tuning: time-to-first-correct-inference vs
  Triton JIT autotune warmup on the same device; replay determinism across
  process restarts.
- EXPLAIN packets: measurable via the agent-repair loop (eval-repair
  repair-rounds / tokens-to-green with vs without packets).
- Schedule machinery: tuned-vs-closed-form-default deltas per family
  (cad-6 output).
Each row lands as a gate/tool in-tree so reviewers can rerun it; rows that
already exist are cited by path, missing ones become dots when the paper
unblocks.

ABLATION ROWS LANDED 2026-07-05 (fable eecca5cb): golden fault injection (4
corruption classes each caught V-FAIL on Orin), sentinel dropped-copy-back
(E-PTX-READBACK), fusion ON/OFF toggle + measured traffic deltas (FFN +25.3%,
MIX +35.6%, slice-gelu +40% unfused). Full 16-row matrix with citations:
docs/ablation.md. Pending rows tracked there: on-device traffic/latency
(bench harness), persistent-tune warmup (cad-6), EXPLAIN A/B arm,
tuned-vs-default deltas. (The interim ablation work-dot was lost to the
minted-on-orphaned-commit gotcha; this entry is the durable record.)

EXPLAIN A/B ARM LANDED 2026-07-06: row 7 (with-vs-without-packet) is now
implemented-here. maki/eval-repair-ab-test.f measures both arms over 4 seeded
authoring-error fixtures (fix_type, add_producer, two-bug, remove_producer),
scored by the same checker and converging on the same green kernel so only the
repair PATH differs. ON = the rich EXPLAIN packet (repair_class + offending token
+ expected/actual rows + suggestion, per docs/repair-diagnostics.md); OFF = the
minimal status-quo signal (verdict + raw code only), which forces one
plausible-but-wrong repair per error. Measured aggregate: rounds 5 (ON) vs 10
(OFF) = 2.0x; tokens-to-green 255 (ON) vs 383 (OFF) = +50.2%. The repair-loop
metric engine was factored into maki/eval-repair-loop.f (shared by eval-repair.f
and the A/B test). Numbers + per-fixture table: docs/ablation.md § EXPLAIN packet
A/B. Rerun: bin/hb --load maki/test.f. Still pending on this row family:
on-device latency arm (bench harness), persistent-tune warmup (cad-6),
tuned-vs-default deltas.
