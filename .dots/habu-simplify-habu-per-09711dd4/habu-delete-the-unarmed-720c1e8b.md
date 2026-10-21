---
title: Delete the unarmed tree shaker
status: closed
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.757356+03:00"
closed-at: "2026-10-21T20:03:41.004767+03:00"
close-reason: "landed in 3bdffeeafa17 (lane dfa997d26bd9): src/habu/treeshake.f and every KEEP?/FP-KEEP? gate gone, kept branches emitted directly, span tests kept in test/engine-span-end.f, docs corrected; independent review 2026-10-21 ACCEPT; master full native suite and two-generation build pass (Carl's combined lane retired)"
---

Problem: The shaker was never armed in production; its KEEP? gates always kept every primitive. The reachability scan was test-only and had an unchecked buffer write. Delete the shaker and load entries, emit the kept branches directly, retain the checker span tests, and correct affected docs and pending design notes.

Claim: mark-tree-cleanup workspace from 2e2a1f98, authorized by Carl. Keep active until Carl's combined native qualification and integration. The owner of that qualification closes this dot.
