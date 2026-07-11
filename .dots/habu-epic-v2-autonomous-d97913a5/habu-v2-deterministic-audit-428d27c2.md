---
title: V2 deterministic audit replay
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:27.630504+02:00"
blocks:
  - habu-v2-txn-journal-d0bc644f
---

Implement append-only canonical events for action requests/results, transaction commits, verifier runs, evidence decisions, promotion, activation, and rollback. Replay from an empty store must reproduce revision/artifact/evidence/state digests without invoking the LLM. Acceptance: event omission/reorder/tamper rejects, nondeterministic action is marked and must carry captured output evidence, and replay is byte-stable across fresh processes.
