---
title: Automatic AGGRESSIVE fusion (the beat-Triton lever)
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T16:38:43.909898+02:00"
blocks:
---

RECONCILED 2026-07-15 against the landed lowering campaign (the parent
habu-automatic-op-fusion closed with the 2.07x fused-vs-ablated device win).
Already LANDED of the original three items: matmul->EW EPILOGUE fusion
(FP-CAP MATMUL->EW; lower-mm relu/gelu epilogues + LMM-BIAS for LINEAR - the
'matmul->bias->relu one epilogue pass' verify item exists today); reduction
fusion in both directions EW<->ROW-REDUCE (FP-CAP; lower-red fuses EW
prologue+epilogue around reductions, softmax max+sum budget backend-proven);
whole-graph greedy maximal-region selection (FP-BUILD, lowerable-by-
construction via the split legality/capability matrices); staged-transpose
movement folding into EW consumers.

REMAINING SCOPE, four separable legs:
(a) LAYERNORM ONE-KERNEL PROOF (dispatchable, zed): layernorm = mean+var
reductions + normalize + affine. ROW-REDUCE->ROW-REDUCE and ROW-REDUCE->EW are
already emittable, so FP-BUILD may ALREADY plan layernorm as one region - prove
it: fixture the op chain, assert the single-region plan, device-golden it, and
bench fused vs FP-FUSE-OFF! ablated (expect ~3x round-trip reduction) with
orin-nx-25w rows per the fusion-bench-device-test.f pattern. If the planner
splits it, pin exactly which pair breaks and why.
(b) EW PROLOGUE INTO MATMUL: legal (FP-BASE-FUSE? EW->MATMUL) but the backend
cannot emit it - the documented FP-CAP gap 'lower-mm cannot pre-transform A/B
(E-LMM-PROLOGUE)'. The fix is a lower-mm capability (fenced: sol's region
territory + makipools remainder) - flip the FP-CAP bit + regression when it
lands. Coordinate before dispatch; do not edit lower-mm under the fence.
(c) SOFTMAX+MATMUL (attention fusion): needs the attention region class first
- owned by habu-attention-region-class-0de99a25 (sol-gated). Fold this leg
into that dot's acceptance when it dispatches.
(d) TRITON COMPARISON ROWS (user-gated): the strict-GB/s-win claim needs
hand-fused Triton baselines for the same multi-op workloads. No Triton
toolchain is installed on zed and installing one is a user decision; when
baselines exist, import them via the typed BENCH surface (eval-triton.f
precedent, policy-comparable rows only). Until then the fused-vs-ablated rows
(landed + leg a) carry the bandwidth evidence.

Files (leg a): a maki layernorm device test following
maki/fusion-bench-device-test.f, tools/ptx/perf-rows.tsv, MODEL-CAD-V2-PLAN.md
evidence note. Verify: plan-shape assertions everywhere, on-device golden +
ablation rows, corruption probe, maki/test.f. Ownership: maki lowering
evidence; legs b/c ride their owning lanes; leg d awaits user-provided Triton
baselines.
