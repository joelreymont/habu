---
title: Reject overflowing arena append ranges before mutation
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-13T14:51:13.068869+03:00\""
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: active; owner: Sol arena lane, .jj-ws/cedar-arena-range.

Own src/compiler/ir/arena.f APPEND-SPAN and its tests. Validate from<=source-count then k<=source-count-from before growth/copy, preserving LIVE/OWN and zero-length-at-end. Rejected appends leave destination count/content unchanged. Test largest signed inputs, one-past/end/empty and valid bulk clones through real arena tests; no invalid-memory probe is needed.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
