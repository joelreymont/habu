---
title: V2 stream allocator
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:14:25.244385+02:00"
---

Problem: MODEL-CAD-V2-PLAN.md:1419-1436 requires stream-ordered allocation and zero-allocation steady state; current buffers do not derive reuse from async lifetimes. Fix: implement lifetime intervals over the typed async DAG, reuse non-overlapping buffers, and expose allocation/reuse evidence. Acceptance: overlapping lifetimes never alias; repeated replay allocates zero bytes after warmup; peak bytes do not exceed the plan. Files: maki/mem-plan.f, maki/executor.f, maki/store.f. Verify: adversarial lifetime fixtures, allocation counters, repeated device replay.
