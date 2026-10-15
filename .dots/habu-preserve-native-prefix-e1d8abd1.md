---
title: Preserve native prefix type registry base
status: active
priority: 2
issue-type: task
created-at: "2026-10-15T09:36:28.669001+03:00"
---

Direct source-host compiler preload and native build on reviewed931c3edb passes source typing and compiles the first nine FILE units through src/core/cell-body.f, then exits76: tfam: a seeded type registry does not start where its capture did (REG-AOT-BASE-BAD). No native product or map is emitted. Locate exact unit, expected/live registry base and owner transition before changing code; the possible relation to preloaded compiler types is not established. Preserve type identity, existing seed import refusal and real FILE reuse. Fix the responsible prefix/type-family boundary, obtain independent review and verify the original native build, warm/cold behavior and required convergence. Separate from pre-hook numeric effects and artifact-refresh pointer refusal.

The recovery host retains a REPL signature payload with family base 55; its replacement source registry starts at 42. Reviewed fix 86b69801 retires that payload only after successful transfer to a different checker owner, then resets the replacement checker's seed state. Zero-owner, same-owner and failed-transfer paths preserve their seed. Clearing it earlier would leave the retained checker with a ready latch and a null payload. The original native build now completes all 116 FILE units and emits a product and map; warm replay and combined-source convergence remain required before closure.
