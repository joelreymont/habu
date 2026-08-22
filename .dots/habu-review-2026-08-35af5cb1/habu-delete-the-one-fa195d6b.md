---
title: delete the one-instance transaction coordinator
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.854675+02:00"
---

Problem: src/core/declaration-transaction.f (383 lines, 'reusable ordered transaction coordinator') has exactly one instance, GENERATED-DECL-OWNER (generated-declaration.f:566-623) with a fixed set of five participants (:580); GENERATED-DECL (625-663) re-exports the same eight words again; the telemetry words (LAST-FAILURE-PHASE, LAST-FAILURE-PARTICIPANT, LAST-CLEANUP-PARTICIPANT, POISONED?, COUNT, SEALED?) have 1-3 consumers each, all tests; registration entry points are undefined right after boot (generated-declaration-protection.f:173-185). Acceptance: one RUN word calling the five snapshot/prepare/commit/rollback/release sequences in source order with a single poison flag; the file and its test go; behaviour of a failing declaration unchanged (existing rollback suites green). Files: src/core/declaration-transaction.f, generated-declaration.f, generated-declaration-protection.f. Verify: declaration and rollback suites. Depends: none. Ownership: declaration transaction. Claim: unassigned.
