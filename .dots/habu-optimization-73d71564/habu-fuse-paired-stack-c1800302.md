---
title: Fuse paired stack writeback
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-28T18:11:16.417235+02:00\""
---

Exact owned-code census found 4449 plain STP x19,0 pairs followed by pointer adjustment:17796 gross bytes. Fuse only already-eligible GPR64 data-stack store pairs with the next actual planned move into STP post-index. Preserve full pair source origin, call barriers, exact accesses and final pointer; consume only the move slot, never an enclosing call. Reuse existing planner with shared measure/write decision, no IR/schema/ABI expansion. Add genuine selected-mode stack/guard E2E before code, measure full B1/B2 product cost, independent review and required native qualification. This is a bounded contribution toward additional1MB, not a promised ceiling.
