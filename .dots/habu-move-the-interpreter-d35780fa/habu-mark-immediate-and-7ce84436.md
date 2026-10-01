---
title: "Mark immediate and declare cast: in the Habu loop"
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T14:29:58.194989+03:00"
---

Problem: at interpret level `immediate` (`C-IMMEDIATE`, `src/habu/habu2.f`) and `cast:` (`C-CAST`) are still the assembly interpreter's, so the Habu loop stops at either. Neither has a writer row: `immediate` sets `DNAME-IMM` on the record just published, under the record protection (`LPROTREC`), and `cast:` is the `CHECKER-DEFCAST` registration (`src/core/checker.f`) plus the identity word it publishes. Split from habu-close-definitions-with-8ace78d8 (I5e), which landed `;` and the `def-close` row and found both undesigned.
Acceptance: `immediate` and `cast: NAME ( in -- out )` in the Habu loop leave the dictionary, flags and checker state the engine's dispatch leaves, including `cast:`'s missing-name refusal (`C-CAST-DIE-NO-NAME`); each new row follows `def-close`: a `prims.f` row with an ARM64 body in `habu2.f` DEFWRITE and an x86 body in `src/habu/kernel-x64.f`; cases beside `test/outer-interpret.f` compare the two loops.
Files: `src/habu/definers.f`, `src/habu/outer.f`, `src/habu/prims.f`, `src/habu/habu2.f`, `src/habu/kernel-x64.f`, `test/outer-interpret.f`, `test/engine-writers.f`, `test/x86-64-kernel-definition.f`, `docs/x86-64.md`.
Verify: spark: `test/outer-interpret.f`, `test/engine-writers.f`; rebuild, chain, gate; ThinkPad: `test/x86-64-kernel-definition.f` image statuses.
Depends: none open. Needs a design before dispatch.
Route: Alder (shared: src/habu/definers.f, src/habu/outer.f, src/habu/prims.f, src/habu/habu2.f).
Ownership: krait (Intel lane).
Claim: unassigned.
