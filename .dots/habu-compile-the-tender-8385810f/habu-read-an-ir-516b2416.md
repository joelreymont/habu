---
title: "Measure and finish pass-scoped IR reader reuse"
status: open
priority: 2
issue-type: task
created-at: "\"2026-09-11T16:38:06.256061+03:00\""
blocks:
  - habu-build-the-compiler-c348eab0
  - habu-attr-and-remove-2b13e978
  - habu-make-spill-rewrite-ca192310
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Reopen only residual measured pass-level scope;43e272e2 checked readers/per-field migration remains complete. Own NFROZEN cursor bindings and reader-taking IR dialect operations, excluding symbol mutation/hir-word constructors. Measure opens per input arena/pass after selfbuild; if material retain checked readers and rebind on module change, with view wrappers and owner/generation/state/bounds checks. If immaterial close with evidence and no new API. Test stale/retired/reused context/input switching and opens count plus total-time pair. No unchecked pinned pointer: prior no-validation ceiling only5.5% on one word.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
