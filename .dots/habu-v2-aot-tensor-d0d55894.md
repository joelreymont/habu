---
title: V2 AOT tensor interop
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:14:25.658313+02:00"
---

Problem: MODEL-CAD-V2-PLAN.md:1502-1512 requires deployment without compilation/tuning and external zero-copy tensors. Fix: define the first stable AOT package/runtime ABI plus DLPack-compatible tensor ownership/layout validation for one promoted inference artifact. Acceptance: warm start performs no compile/tune; incompatible schema/target rejects typed; compatible tensor is consumed without copy; ownership misuse rejects. Files: maki/artifact-store.f, maki/promotion.f, maki/executor.f, docs/repl-loop.md. Verify: package roundtrip, ABI mutation negatives, external tensor pointer identity test.
