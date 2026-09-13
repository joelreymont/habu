---
title: Reject overflowing arena append ranges before mutation
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-13T14:51:13.068869+03:00\\\"\""
closed-at: "2026-09-13T16:25:50.475734+03:00"
close-reason: Integrated4d34fa11 after separate Astra review plus80b4cf59 strengthening overflow-before-growth check. Fresh rebuilt0c602d1c passes test/compiler/ir-arena.f and full315-suite compiler-ir-arena entry. Largest signed start, source extent/end/empty, ownership and unchanged count/content/scratch checks pass. Other named full-suite failures remain in campaign; no invalid access needed to validate fixed implementation.
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: active; owner: Sol arena lane, .jj-ws/cedar-arena-range.

Own src/compiler/ir/arena.f APPEND-SPAN and its tests. Validate from<=source-count then k<=source-count-from before growth/copy, preserving LIVE/OWN and zero-length-at-end. Rejected appends leave destination count/content unchanged. Test largest signed inputs, one-past/end/empty and valid bulk clones through real arena tests; no invalid-memory probe is needed.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Integrated4d34fa11 after independent Astra review, with80b4cf59 strengthening
the largest-start case to require rejection before destination growth. Fresh
private engine0c602d1c194c, built from the combined source using host28e11361,
passes `test/compiler/ir-arena.f`, including overflow, empty/exact-end spans,
unchanged destination contents/count/scratch and live cross-context source rules.
The full native suite is running in `.jj-ws/cedar-correctness-verify`.
