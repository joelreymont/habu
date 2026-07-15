---
title: Attention region class for whole-model lowering
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T16:47:37.608613+02:00"
---

Residual from habu-small-model-end-f7cc1b39 (landed e6626893): LOWER-MODEL-RUN dispatches only matmul/row-reduce/elementwise/movement region classes (LLA-REGION-*), so a one-head attention block cannot lower through the whole-model device path even though the fused ATTN kernel exists (lib/ptx/cg-attention.f) and is device-gold'd standalone (tools/ptx/device-gold.f). Fix: an attention region class in the fusion planner (maki/fusion-plan.f - check sol's claim state) + an LLA attention launch shape (maki/lower-launch.f - same) + the whole-model golden extended to an attention+MLP model (the small-model dot's stretch shape). Acceptance: one-head attention + MLP lowers end-to-end and matches host under composed tolerance on the Orin; corruption probe. Files: maki/fusion-plan.f, lower-launch.f (SOL TERRITORY - coordinate/wait), maki/lower-model-device.f + a new device test. Ownership: maki lowering (gated on sol's region lane).
