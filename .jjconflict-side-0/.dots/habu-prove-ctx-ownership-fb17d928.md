---
title: Prove context ownership in canonicalization
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T01:39:18.787766+02:00"
---

Full context: gap 1 from agent ircanon 2026-07-30 (commit 00d0d5a1, src/compiler/ir/canon.f). IR-CANON:CANON allocates from the presented context but reads frozen views owned by the MODULE's context, and no public reader exposes a view's owning context, so a frozen module from a different live context is canonicalized silently instead of refused. Structural fix: an owner projection on IR-ARENA:view (or on the frozen module surface - decide which is the single authority), then an E-IR-CANON-OWNER refusal arm in CANON with a hostile fixture (module frozen in context A, canonicalized under live context B, must refuse by name) and a mutation proof. Same ownership discipline IR-BUILD:FREEZE just gained (commit 869e56f5).
