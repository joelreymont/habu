---
title: Emit complete newline-terminated engine diagnostics
status: closed
priority: 2
issue-type: task
created-at: "\"2026-09-13T11:09:21.780935+03:00\""
closed-at: "2026-09-16T14:34:48.371006+03:00"
close-reason: "superseded by habu-campaign-c4-diagnostics-3b6de147: Residue: engine stderr payloads are still emitted in pieces, so a diagnostic can lose its newline or gain padding NULs, and E-BAD-LOCAL-SHAPE prints a bare name."
blocks:
  - habu-size-the-snapshot-1ca5db10
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own remaining habu2.f fd2 payload emission and diagnostic tests; capacity leaf owns its message; stage2f64be7c owns both EM-AOT-PATCH-SITES name/site messages including complete newline emission. Compose actual payload plus newline before one BYTES, so alignment padding cannot insert NULs/eat newline. Reuse byte emitter without general messaging framework. Verify all migrated payload lengths/newline/no NUL, representative actual stderr and byte identity. Preserve exit codes/bounds. About20 split runs identified.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Concrete diagnostic acceptance from Rowan's Maki lane (2026-09-13): a local referenced inside a quotation currently prints only its name without a newline and exits75. Preserve rejection, but report the existing E-BAD-LOCAL-SHAPE name, offending local and available source location in one complete diagnostic. Reproduce through a real --load file; verify stderr text/newline and exit code, and keep ordinary valid locals working. Use the existing local-shape diagnostic owner; no new diagnostic framework. The BB reproducer captures `a` inside `[: a alen type ;]` after binding `{ : a:ptr alen:n : }` (remove spaces inside the local delimiters when reproducing).
