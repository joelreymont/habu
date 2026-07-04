---
title: "CAD: matmul prologue fusion - plan vs backend"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T19:46:36.822648+02:00"
---

Slice-3 finding: FP-BASE-FUSE? (maki/fusion-plan.f) fuses an EW producer INTO a contraction consumer (GELU MATMUL = one region, classmix EW|MATMUL), but no lowering backend can emit a pre-contraction transform: maki/lower-mm.f v1 fails closed E-LMM-PROLOGUE, so a planned prologue region is currently UNLOWERABLE - it would block slice-5 OPTIMIZE wiring on any model with an activation before a matmul. Decide + implement ONE of: (a) tighten the legality matrix so EW->contraction does not fuse until the backend lowers it (plan matches backend capability; simplest, but loses the CAD-PLAN 8.1 prologue-dequant lever's planning); (b) teach lower-mm.f prologue support (apply the EW chain per-element during the A/B K-loop loads - correct but costs recompute per K-iteration for A-side prologues; the traffic model must account for it); (c) a backend-capability gate in FP-JOIN? consulted at plan time (the plan asks what the lowering can emit - the architecturally-correct long-term shape per CAD-PLAN 5.2). Evidence fixture: MODEL: x GELU MATMUL. Blocks: slice-5 OPTIMIZE wiring completeness.
