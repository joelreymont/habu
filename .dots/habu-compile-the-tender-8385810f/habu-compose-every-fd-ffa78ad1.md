---
title: "Emit complete newline-terminated engine diagnostics"
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T11:09:21.780935+03:00"
blocks:
  - habu-size-the-snapshot-1ca5db10
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own remaining habu2.f fd2 payload emission and diagnostic tests; capacity leaf owns its message; stage2f64be7c owns both EM-AOT-PATCH-SITES name/site messages including complete newline emission. Compose actual payload plus newline before one BYTES, so alignment padding cannot insert NULs/eat newline. Reuse byte emitter without general messaging framework. Verify all migrated payload lengths/newline/no NUL, representative actual stderr and byte identity. Preserve exit codes/bounds. About20 split runs identified.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
