---
title: "Give word tables their interner and one binding gate"
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T18:11:06.105604+03:00"
blocks:
  - habu-idx-ir-sym-a35dd84d
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own native/hir-word.f and constructor call sites only. Carry owning interner, uniform ROW-SPELL and LOOKUP intrinsic gate for LINK-CLONE/LINK-NONE. Remove SESS-KEY/POOL/ROWS and split writer BVOCAB?/reader ROW-BOUND? after migration. Preserve definition-local memo because shadowing changes during load. Verify native-hir/native-word-binding including OWN-GATE-CASE/PLAIN-CASE, session/ordinary clones, private/ambiguous/shadowed names. No second session model; dict.f lookup is2b13e978.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
