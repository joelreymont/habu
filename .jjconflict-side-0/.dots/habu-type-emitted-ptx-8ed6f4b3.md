---
title: Type emitted PTX loops
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T11:02:28.572037+02:00"
---

Why: checked kernel code cannot express a runtime PTX loop whose induction, stride, and bound are tied to a tensor extent, so GPT-2 target primitives retain raw generated loops. Result: add one checked PTX loop context that binds an extent-typed runtime bound, proves positive stride and no u32/u64 wrap, preserves accumulator phantoms, and rejects wrong extents or non-neutral loop state. Owner: shared lib/ptx loop vocabulary and checker support only. Acceptance: migrate the GPT-2 attention dim/token loops and tensor runtime-K/strided-row loops without changing emitted formulas; wrap, wrong-extent, reordered-state, and accumulator-forgery negatives reject; focused PTX, GPT-2 device, Maki, and ptx-stdlib gates pass. Forbidden: runtime framework, model-specific type family, manifest, registry, ABI version, compatibility, or product API.
