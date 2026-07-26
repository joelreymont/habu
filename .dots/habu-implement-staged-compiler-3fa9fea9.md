---
title: Implement staged compiler IR
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:52:55.238657+02:00"
---

Full context: docs/compiler-ir-design.md and PLAN.md define the scope-locked replacement of native direct AArch64 emission and GPU string-first PTX emission with immutable validated staged IRs. Required result: complete the baseline, shared substrate, native Waves 2-8, GPU Waves A-E, proof synchronization, cutovers, and retirement gates using only `NEWTYPE`, `ENUM`, and `STRUCTURE`, with no silent fallback or edits to the independent Spark vLLM work. Acceptance: all child dots close; native and GPU completion criteria in design section 21 pass; old production emitters are retired only after verified cutover.
