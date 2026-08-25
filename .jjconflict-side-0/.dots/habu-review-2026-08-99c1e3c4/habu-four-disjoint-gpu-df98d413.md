---
title: four disjoint GPU stacks and a toy-shape CAD lowering
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.992850+02:00"
---

Problem: maki/lower/launch.f:80 LLA-NCAP 4096 (16 KB buffers), lower/mm.f:106 LMM-KCAP 256, lower/red.f:86 cols<=256 - GPT-2 124M (768x2304) cannot pass LMM-CHECK-DIMS (mm.f:271-275), so maki/infer/gpt2-model.f has its own layout (60-93, 320-335), launch boilerplate (489-564, seven copies), context lifecycle, and requires none of maki/lower, fusion-plan.f, cad.f; a third stack maki/gpu.f (GN 4 'demo') + gpu-train.f with GPU:SETUP/LAUNCH/SGD at 0 non-test consumers; a fourth maki/infer/kv-cache.f (1340 lines) with exactly one consumer, its own 1320-line device test, unused by gpt2-model.f. Acceptance: a ruling recorded here, then: one launch/ownership layer (GPU:session/GPU:buffer), gpu.f/gpu-train.f deleted, kv-cache.f wired into gpt2-model.f or deleted, the seven launch bodies replaced by one table-driven launcher with param sizes derived from the cg's param list. Files: maki/gpu*.f, maki/infer/gpt2-model.f, kv-cache.f, maki/lower/launch.f. Verify: maki/test.f; GPT-2 device tests target-blocked here. Depends: none. Ownership: maki device layer. Claim: unassigned.
