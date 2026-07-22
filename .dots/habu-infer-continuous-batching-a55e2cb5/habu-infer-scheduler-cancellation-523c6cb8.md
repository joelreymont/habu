---
title: "Infer scheduler: cancellation and failure"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.828511+02:00"
blocks:
  - habu-infer-scheduler-token-3490da89
  - habu-infer-kv-atomic-c402952e
---

Why this exists:
cancellation or model failure may arrive in waiting, prefill, or decode and must release state exactly once.

Required result:
define per-state cleanup transitions through the owned engine, KV, and snapshot APIs.

Done when:
every state and injected failure returns all queue, page, reservation, snapshot, and workspace owners; repeated/stale cancellation rejects.

Expected touch points: new maki/infer/request-cleanup.f, focused test.
Smallest check: focused state-by-state cleanup and syscall trace.
Prerequisites: token-boundary assembly and atomic KV cancellation.
Owned result: scheduler cancellation/failure cleanup only.
Claim: unassigned.
