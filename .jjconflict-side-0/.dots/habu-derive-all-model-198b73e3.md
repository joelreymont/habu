---
title: Derive all model consumer capacities from one plan
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:01:19.347083+02:00"
---

maki/model-ir.f admits far more nodes and input slots than fusion, lowering, checkpoint, executor, and golden consumers can represent. A 129-node fusion case fails late with an unrelated status and a 66-slot golden case can index beyond its 64-slot table. Replace scattered fixed capacities with one validated model-capacity plan derived from the accepted model and threaded into every consumer. Each consumer allocates or reserves from that plan, or one shared hard limit rejects at model construction; no downstream table may impose a smaller hidden ceiling. Checked size arithmetic must reject overflow and cap+1 before indexing or allocation. Add a machine-derived consumer census so a new model-indexed table must declare which plan dimension bounds it. Tests cover every current consumer at zero, exact cap, cap+1, mismatched plan, large overflow, and a GPT-2 124M-shaped graph; canaries prove no out-of-bounds access and errors name the limiting dimension. Raising constants independently is forbidden. Files: model capacity schema, model IR, fusion/lowerers/checkpoint/executor/golden consumers and focused tests. Verify Maki core/db/eval, target model build, typed-local/package/host/dot lints, performance and memory budgets, and full native gate.
