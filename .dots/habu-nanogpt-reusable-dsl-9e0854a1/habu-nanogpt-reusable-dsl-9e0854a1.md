---
title: nanoGPT reusable DSL epic
status: open
priority: 1
issue-type: task
created-at: "2026-07-01T22:29:15.318096+02:00"
---

File: PLAN.md:3; cause: reviewed plan now defines reusable PTX/Maki DSLs plus a tiny causal GPT capstone but no dot owns final integration; fix: integrate leaf dots only after generic runtime, planner, kernel construction, fusion, GEMM/MMA, attention, autograd, Maki lowering, and AdamW are green; deps: enforced by child/blocks edges during reconciliation; verification: PLAN.md coverage matrix, dot-dep-lint, Maki CPU gate, Orin device/capstone gate, PTX slices, full native gate.
