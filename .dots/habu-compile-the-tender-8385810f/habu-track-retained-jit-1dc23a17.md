---
title: "Finish tier dispatch, checker owners and retained-code provenance"
status: open
priority: 1
issue-type: task
created-at: "\"2026-09-11T16:07:57.854352+03:00\""
blocks:
  - habu-keep-a-row-f2c4f3d4
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own rowan-tier91fff115 dispatch/provenance in habu2.f, checker owner-record declarations and src/compiler/native owner/tape/front-end consumers; exclude call-row/payload fixes, build routing, prefix tables and messages. Reuse pending stack with definition-tier latch. Reject JIT compiler entry/tier0 requests during AOT before emission; NCOMP absence/failure preserves original error without fallback. Provenance covers hidden/re-exposed spans, aliases, DOES, stored quotations/direct calls; record count alone fails. Verify tier parity/owner isolation, span bounds, AOT saves and retained-JIT negatives. Native execution and emitting REPL support remain legal.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
