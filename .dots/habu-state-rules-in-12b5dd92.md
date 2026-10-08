---
title: State rules in comments, not history
status: open
priority: 2
issue-type: task
created-at: "2026-10-08T11:00:31.008190+02:00"
---

Problem: source comments break the comment policy in docs/forth.md (Comments). Audit of src, lib, tools and test (110,360 comment lines in 1,687 files): history narrative ~820 lines (e.g. tools/bootstrap.sh:285-310, tools/native-build-core.f:44-60, src/core/checker.f:10260-10283); dates, dot ids and names ~975; essays of 8+ lines 2,651 blocks / 46,777 lines (checker.f 3,916, habu2.f 2,004, layout.f 1,267; tools/lsp-test-lib.f:1-429 is one header); restating the code 240+; the same fact in several places 429 groups; stale names or paths ~357. Report and the 171 package slices: ~/.cache/tmp/heron-arm64/evidence/comment-audit/report.md and data/slices.tsv.
Ruling (Joel, 2026-10-08): rule only, cut history; the policy in docs/forth.md governs.
Placement: history, dates, ids and arguments for or against a design are deleted, not moved. Design text that a reader of the code still needs goes to the docs page for its subject, once; the comment keeps the rule and points there. A subject with no page gets one named for it: docs/checker.md (checker internals), docs/aot-capture.md, docs/lsp.md. Text describing machinery an open dot deletes (PRIM:/PPRIM: rows, marks, the checker's own symbol table) is deleted, not moved.
Acceptance: every comment states the current rule and, where needed, the fact that proves it; no history, dates, dot ids, names, essays, restated code or duplicated facts remain; no code change.
Files: src, lib, tools, test (*.f), tools/bootstrap.sh, the docs pages above. One worker per slice, run by package.
Verify: the suite on the built hb (comments only); rg for the history markers returns nothing.
Depends: none. Ownership: unassigned. Claim: unassigned.
