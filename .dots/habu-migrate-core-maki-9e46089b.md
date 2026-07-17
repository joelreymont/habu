---
title: Migrate core Maki count callers
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T14:14:22.915692+02:00\""
---

Full context: migrate the frozen B5 MIR count callers in core/planner ownership: maki/backward-test.f, maki/backward.f, maki/cad.f, maki/checkpoint.f, maki/executor.f, maki/fusion-mout-test.f, maki/fusion-plan.f, maki/mem-plan.f, maki/saved.f, and maki/traffic.f plus associated focused tests. Replace all four old count accessors used in each owned file with MIR packaged typed counts; no sched-key/lowerer/golden/ONNX files. Acceptance: focused suites and Maki remain exact; no count/index erasure; per-file census empty. Depends on MIR count APIs.

Claim: agent=mircount workspace=.jj-ws/fable-mircount (tri-dot lane; per-dot disjoint file ownership per the dots themselves)
