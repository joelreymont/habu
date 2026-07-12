---
title: V2 atomic transaction commit
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:26.960293+02:00"
blocks:
  - habu-v2-txn-journal-d0bc644f
---

Implement the smallest crash-safe commit slice from MODEL-CAD-V2-PLAN.md:1832-1849 over the V2 object store: validate head/base, complete read set, capabilities, budget, digests, and obligation closure; write objects and commit marker atomically; recover old or complete new revision only. Add failpoints before every durability boundary. Acceptance: injected crashes never expose partial revision, idempotent retry returns original result, stale head returns typed conflict, and deterministic replay yields equal revision digest. Depends on transaction journal and V2 persistent object-store owner.
